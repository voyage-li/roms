#!/usr/bin/env python3
"""Lower explicitly selected independent ROMS row loops to Kokkos.

Input is CPP-cleaned Fortran, so physics selection has already happened.
This is a deliberately restricted, fail-closed source generator, not a
Fortran compiler. Unknown statements/calls are errors. All arithmetic is
read from the original loops; fallback retains that original source.
Sequential launches preserve dependencies between row phases, and k stays
sequential inside a row whenever it carries a vertical recurrence.
Generated f90/cpp are build artifacts, not maintained copies of the math.
"""
import argparse
import re
from pathlib import Path

TARGETS = {
    'rhs3d': ['rhs3d_tile'],
    'set_depth': ['set_depth_tile', 'set_depth0_tile'],
    'set_massflux': ['set_massflux_tile'],
    'rho_eos': ['rho_eos_tile'],
    'bulk_flux': ['bulk_flux_tile'],
    't3dmix': ['t3dmix2_geo_tile'],
    'uv_var_change': ['uv_c2a_grid_tile'],
    'pre_step3d': ['pre_step3d_tile'],
    'step3d_t': ['step3d_t_tile'],
    'diag': ['diag_tile'],
    'wvelocity': ['wvelocity_tile'],
    'set_vbc': ['set_vbc_tile'],
    'set_avg': ['set_avg_tile'],
    'set_diags': ['set_diags_tile'],
    'lmd_swfrac': ['lmd_swfrac_tile'],
    'lmd_vmix': ['lmd_vmix_tile', 'lmd_finish_tile'],
    'lmd_skpp': ['lmd_skpp_tile'],
    'lmd_bkpp': ['lmd_bkpp_tile'],
    'analytical': ['ana_vmix_tile', 'ana_smflux_tile', 'ana_stflux_tile',
                   'ana_btflux_tile', 'ana_srflux_tile', 'ana_bsflux_tile',
                   'ana_ssflux_tile', 'ana_winds_tile', 'ana_tair_tile',
                   'ana_humid_tile', 'ana_pair_tile', 'ana_cloud_tile',
                   'ana_rain_tile'],
}
HOST_ARRAYS = set(('n nt lm mm dt hc iic ntfirst r0 s0 t0 tcoef scoef tdays el xl '
                   'akt_bak akv_bak blk_zq blk_zt blk_zw ewperiodic nsperiodic '
                   'tnudg').split())
FIELD_ARRAYS = {'scalars': {'sc_r','sc_w','cs_r','cs_w'},
                'clima': {'m3nudgcof','uclm','vclm','tnudgcof','tclm'}}
FIELD_MODULES={name:'mod_'+name for name in ('grid','ocean','mixing','forces','diags')}
FIELD_MODULES['average']='mod_average'
FIELD_CACHE={}
GLOBAL_TYPES=None
GLOBAL_ARRAYS={'lmd_mu1','lmd_mu2','lmd_r1'}
INTRINSICS = {'sqrt':'std::sqrt', 'abs':'std::abs', 'exp':'std::exp',
              'log':'std::log', 'log10':'std::log10', 'atan':'std::atan',
              'tanh':'std::tanh', 'sin':'std::sin', 'cos':'std::cos',
              'min':'roms_loops::fmin', 'max':'roms_loops::fmax',
              'sign':'roms_loops::sign', 'int':'int', 'real':'double',
              'mod':'std::fmod'}


def split(s):
    result, start, depth = [], 0, 0
    for i, c in enumerate(s):
        if c == '(': depth += 1
        if c == ')': depth -= 1
        if c == ',' and depth == 0:
            result.append(s[start:i].strip()); start = i+1
    result.append(s[start:].strip())
    return result


def statements(text):
    """Keep source spans so replacement never changes unselected code."""
    result, buf, start = [], '', 0
    pos = 0
    for line in text.splitlines(True):
        end = pos+len(line)
        raw = line.split('!', 1)[0].strip()
        if raw:
            if not buf: start = pos
            buf += raw.lstrip('&').rstrip('&').strip()+' '
            if not raw.endswith('&'):
                result.append((buf.strip().lower(), start, end)); buf = ''
        pos = end
    if buf: raise ValueError('unterminated continuation')
    return result


def paren_end(s, start):
    level = 0
    for i in range(start, len(s)):
        if s[i] == '(': level += 1
        if s[i] == ')':
            level -= 1
            if level == 0: return i
    raise ValueError('unmatched parenthesis: '+s)


def power_expr(s):
    # Right-associative Fortran exponentiation, with unary sign outside.
    while '**' in s:
        p = s.rfind('**')
        l = p-1
        while l >= 0 and s[l].isspace(): l -= 1
        if s[l] == ')':
            depth, q = 1, l-1
            while depth:
                if s[q] == ')': depth += 1
                if s[q] == '(': depth -= 1
                q -= 1
            l = q+1
            q = l-1
            while q >= 0 and (s[q].isalnum() or s[q] in '_:'): q -= 1
            l = q+1
        else:
            while l >= 0 and (s[l].isalnum() or s[l] in '._'): l -= 1
            l += 1
        r = p+2
        while s[r].isspace(): r += 1
        if s[r] in '+-': r += 1
        if s[r] == '(':
            r = paren_end(s, r)+1
        else:
            m = re.match(r'(?:\d+(?:\.\d*)?(?:[eE][+-]?\d+)?|[a-zA-Z_]\w*(?:::\w+)*)',s[r:])
            if not m: raise ValueError('power exponent: '+s[p:])
            r += len(m[0])
            if r < len(s) and s[r] == '(': r = paren_end(s,r)+1
        s = s[:l]+'roms_loops::power('+s[l:p].strip()+','+s[p+2:r].strip()+')'+s[r:]
    return s


class Kernel:
    def __init__(self, name, block, declarations, parallel='j'):
        self.name, self.block, self.declarations = name, block, declarations
        self.parallel=parallel
        self.arrays, self.scalars, self.locals = {}, {}, {}
        self.scalar_types={}
        self.assigned = set()
        self.seen_write = set()
        for stmt,_,_ in block:
            d = re.match(r'(?:\w+\s*:\s*)?do\s+(\w+)\s*=',stmt)
            a = re.match(r'(\w+)\s*=(?!=)',stmt)
            if d: self.assigned.add(d[1])
            if a: self.assigned.add(a[1])
        for n in self.assigned:
            self.locals[n] = declarations.get(n, {}).get('type', 'double')
        self.local_init = set()
        self.code = []
        self.loop_stack = []
        self.if_stack = []

    def scalar(self, value, logical=False):
        key = re.sub(r'\s+','',value)
        if key not in self.scalars:
            self.scalars[key] = (value, logical)
            n=re.match(r'\w+',key)
            declaration=self.declarations.get(n[0],{}) if n else {}
            self.scalar_types[key]='int' if logical else declaration.get('type',global_scalar_type(n[0]) if n else 'double')
        return 's'+str(list(self.scalars).index(key))

    def array(self, name):
        if name not in self.arrays:
            if name not in self.declarations:
                raise ValueError('undeclared array '+name)
            dec = self.declarations[name]
            if dec.get('parameter',False):
                raise ValueError('parameter arrays need an explicit interface: '+name)
            if not dec['dims']: raise ValueError('scalar used as array '+name)
            if dec.get('logical',False):
                raise ValueError('logical arrays need an explicit ABI: '+name)
            if dec['type']=='int' and dec['dummy'] and len(dec['dims'])!=2:
                raise ValueError('shared integer arrays currently require rank two: '+name)
            # Row-only automatic scratch has no j axis. It must not be
            # shared between CPEs. Full tile scratch remains shared across
            # synchronous launches, just as in the original Fortran.
            private = not dec['dummy'] and not dec.get('allocatable',False) and not any('j' in b for b in dec['dims'])
            self.arrays[name] = dict(dec, private=private)
        return name

    def expr(self, s):
        s = s.strip()
        s = re.sub(r'\b(\d+(?:\.\d*)?(?:[ed][+-]?\d+)?)_r8\b',r'\1',s)
        s = re.sub(r'(?<=\d)d(?=[+-]?\d)', 'e', s)
        # Scalar derived-type fields (advection and domain flags) are
        # evaluated by Fortran at the call site, not reinterpreted in C++.
        pat = r'\b\w+\([^()]*\)\s*%\s*\w+(?:\([^()]*\))?'
        def field(m):
            t=m[0]
            if t.startswith(('domain(', 'hadvection(', 'vadvection(')):
                if any(re.search(r'\b'+n+r'\b',t) for n in self.assigned):
                    flag=re.fullmatch(r'(hadvection|vadvection)\(itrc,\s*ng\)\s*%\s*(\w+)',t)
                    if not flag: raise ValueError('row-varying derived flag: '+t)
                    name='kk_flag_'+flag[1]+'_'+flag[2]
                    self.declarations[name]=dict(type='double',dims=['nt(ng)'],dummy=True,logical=False,flag_expr=t)
                    return name+'(itrc)'
                return self.scalar(t, True)
            component=re.match(r'(\w+\([^()]*\)\s*%\s*(\w+))\s*\((.*)\)$',t)
            if component:
                base=component[1]; owner=base.split('(',1)[0]
                name='field_'+owner+'_'+component[2]
                if component[2] not in registered_fields(owner):
                    raise ValueError('unregistered component array: '+base)
                self.declarations[name]=dict(type='double',dims=[':']*len(split(component[3])),dummy=True,logical=False,fexpr=base)
                return name+'('+component[3]+')'
            return self.scalar(t)
        s = re.sub(pat, field, s)
        # Replace calls from the inside out, leaving ordinary arithmetic
        # parenthesis untouched. Function arguments are translated recursively.
        out, pos = '', 0
        call = re.compile(r'\b([a-z_]\w*)\s*\(')
        while True:
            m = call.search(s,pos)
            if not m: out += s[pos:]; break
            a=s.index('(',m.start()); b=paren_end(s,a)
            out += s[pos:m.start()]
            n=m[1]; args=s[a+1:b]
            if n in GLOBAL_ARRAYS:
                self.declarations[n]=dict(type='double',dims=['9'],dummy=True,logical=False)
            if n in self.declarations and self.declarations[n]['dims']:
                out += self.array(n)+'('+','.join(self.expr(x) for x in split(args))+')'
            elif n in INTRINSICS:
                vals=split(args)
                if n=='real' and len(vals)==2: vals=vals[:1]
                out += INTRINSICS[n]+'('+','.join(self.expr(x) for x in vals)+')'
            elif n in ('bulk_psiu','bulk_psit'):
                out += n+'('+','.join(self.expr(x) for x in split(args))+')'
            elif n in HOST_ARRAYS and not any(re.search(r'\b'+v+r'\b',args) for v in self.assigned):
                out += self.scalar(n+'('+args+')',n in ('ewperiodic','nsperiodic'))
            else:
                raise ValueError('unsupported call: '+n+'('+args+')')
            pos=b+1
        s=out
        for f,c in {'.true.':'true','.false.':'false','.not.':'!', '.and.':'&&','.or.':'||', '.eq.':'==','.ne.':'!=','.gt.':'>','.ge.':'>=','.lt.':'<','.le.':'<='}.items():
            s=s.replace(f,c)
        # Token replacement does not inspect generated names/functions.
        def token(m):
            n=m[0]
            if n in ('true','false','std','roms_loops','power','double','int') or n in self.arrays or n in INTRINSICS.values() or re.fullmatch(r's\d+',n): return n
            if m.start()>0 and (s[m.start()-1].isalnum() or s[m.start()-1]==':'): return n
            if s[m.end():].lstrip().startswith('('): return n
            if n in self.locals:
                if n not in self.seen_write:
                    self.local_init.add(n)
                return n
            return self.scalar(n, self.declarations.get(n,{}).get('logical',False) or n.startswith(('ewperiodic','nsperiodic')))
        s=re.sub(r'\b[a-z_]\w*\b',token,s)
        for f,c in {'.true.':'true','.false.':'false','.not.':'!', '.and.':'&&','.or.':'||', '.eq.':'==','.ne.':'!=','.gt.':'>','.ge.':'>=','.lt.':'<','.le.':'<='}.items():
            s=s.replace(f,c)
        return power_expr(s)

    def translate(self):
        for stmt,_,_ in self.block:
            stmt=re.sub(r'^\w+\s*:\s*(?=(?:do|if)\b)','',stmt)
            d=re.match(r'do\s+(\w+)\s*=\s*(.*)',stmt)
            if d:
                n=d[1]; terms=split(d[2]); low,high=terms[:2]; step=terms[2] if len(terms)>2 else '1'
                if n==self.parallel:
                    # The selected outer loop is represented by the policy.
                    self.code.append('/* independent '+n+' */ {')
                else:
                    cmp='>=' if step.strip().startswith('-') else '<='
                    self.code.append('for ('+n+'='+self.expr(low)+'; '+n+cmp+self.expr(high)+'; '+n+'+='+self.expr(step)+') {')
                self.seen_write.add(n); self.loop_stack.append(n)
            elif stmt.startswith('end do') or stmt=='enddo':
                self.loop_stack.pop(); self.code.append('}')
            elif stmt.startswith('else if') or stmt.startswith('elseif'):
                flow=self.if_stack[-1]
                flow['branches'].append(self.seen_write.copy())
                self.seen_write=flow['entry'].copy()
                a=stmt.index('('); b=paren_end(stmt,a)
                self.code.append('} else if ('+self.expr(stmt[a+1:b])+') {')
            elif re.match(r'else\b',stmt):
                flow=self.if_stack[-1]
                flow['branches'].append(self.seen_write.copy())
                flow['else']=True
                self.seen_write=flow['entry'].copy()
                self.code.append('} else {')
            elif stmt.startswith(('end if','endif')):
                flow=self.if_stack.pop()
                flow['branches'].append(self.seen_write.copy())
                if not flow['else']: flow['branches'].append(flow['entry'])
                self.seen_write=set.intersection(*flow['branches'])
                self.code.append('}')
            elif re.match(r'if\s*\(',stmt):
                a=stmt.index('('); b=paren_end(stmt,a)
                cond=self.expr(stmt[a+1:b]); tail=stmt[b+1:].strip()
                if tail=='then':
                    self.if_stack.append(dict(entry=self.seen_write.copy(),branches=[],**{'else':False}))
                    self.code.append('if ('+cond+') {')
                else:
                    entry=self.seen_write.copy()
                    self.code.append('if ('+cond+') '+self.assignment(tail))
                    self.seen_write=entry
            elif '=' in stmt and not stmt.startswith(('call ','allocate ','deallocate ')):
                self.code.append(self.assignment(stmt))
            elif stmt=='cycle': self.code.append('continue;')
            else: raise ValueError('unsupported statement: '+stmt)
        if self.loop_stack: raise ValueError('unclosed loop')
        if self.if_stack: raise ValueError('unclosed conditional')
        if self.local_init:
            raise ValueError('possible loop-carried scalar(s): '+', '.join(sorted(self.local_init)))
        return '\n'.join(self.code)

    def assignment(self, stmt):
        a,b=stmt.split('=',1)
        lhs=a.strip() if a.strip() in self.locals else self.expr(a)
        rhs=self.expr(b)
        if re.fullmatch(r'\w+',a.strip()): self.seen_write.add(a.strip())
        statement=lhs+' = '+rhs+';'
        # uv_C2A_grid's original serial loop repeats these two boundary
        # fills for every j. Give each fill to the row that produces its
        # source, avoiding cross-row reads/writes inside a parallel launch.
        if self.name.startswith('kk_uv_var_change_'):
            if re.match(r'urho\(istr-1,\s*j\)',a.strip()):
                statement='if (i=='+self.expr('istr')+') '+statement
            if re.match(r'urho\(iend\+1,\s*j\)',a.strip()):
                statement='if (i=='+self.expr('iend')+') '+statement
            if re.match(r'vrho\(i,\s*jstr-1\)',a.strip()):
                statement='if (j=='+self.expr('jstr')+') '+statement
            if re.match(r'vrho\(i,\s*jend\+1\)',a.strip()):
                statement='if (j=='+self.expr('jend')+') '+statement
        return statement


def declarations(stmts):
    dec={}
    for s,_,_ in stmts:
        m=re.match(r'(real\(r8\)|integer|logical)(.*?)::\s*(.*)',s)
        if not m: continue
        typ='double' if m[1].startswith('real') else 'int'
        attr=m[2]; dims=re.search(r'dimension\s*\(',attr)
        common=split(attr[attr.index('(',dims.start())+1:paren_end(attr,attr.index('(',dims.start()))]) if dims else []
        for var in split(m[3]):
            v=re.match(r'(\w+)\s*(?:\((.*?)\))?(?:\s*=.*)?$',var)
            # Nested dimension expressions need the balanced form.
            n=re.match(r'\w+',var)
            if not n: raise ValueError('declaration '+var)
            name=n[0]; rest=var[n.end():].lstrip(); shape=common
            if rest.startswith('('): shape=split(rest[1:paren_end(rest,0)])
            dec[name]=dict(type=typ,dims=shape,dummy='intent' in attr,logical=typ=='int' and m[1]=='logical',allocatable='allocatable' in attr,parameter='parameter' in attr)
    return dec


def global_scalar_type(name):
    global GLOBAL_TYPES
    if GLOBAL_TYPES is None:
        GLOBAL_TYPES={}
        directory=Path(__file__).resolve().parents[2]/'Modules'
        for module in ('mod_param','mod_scalars','mod_ncparam','mod_eoscoef'):
            p=directory/(module+'.F')
            for n,d in declarations(statements(p.read_text())).items():
                GLOBAL_TYPES[n]=d['type']
    return GLOBAL_TYPES.get(name,'double')


def registered_fields(owner):
    if owner in FIELD_ARRAYS: return FIELD_ARRAYS[owner]
    if owner not in FIELD_MODULES: return set()
    if owner not in FIELD_CACHE:
        path=Path(__file__).resolve().parents[2]/'Modules'/(FIELD_MODULES[owner]+'.F')
        dec=declarations(statements(path.read_text()))
        FIELD_CACHE[owner]={n for n,d in dec.items() if d['type']=='double' and d['dims']}
    return FIELD_CACHE[owner]


def wrap_fline(s):
    # Emit safe free-form continuation, including long coefficient expressions.
    lines=[]
    while len(s)>110:
        choices=[m.end() for m in re.finditer(r'[, +*/]',s[:100])]
        cut=choices[-1] if choices else 95
        lines.append(s[:cut]+' &'); s='     & '+s[cut:]
    return '\n'.join(lines+[s])


def emit_kernel(k, body):
    axis=k.parallel
    loop=re.match(r'(?:\w+\s*:\s*)?do\s+'+axis+r'\s*=\s*(.*)',k.block[0][0])
    lower,upper=split(loop[1])[:2]
    low=k.expr(lower); high=k.expr(upper)
    f=['      kk_a%p=C_NULL_PTR', '      kk_a%b=0', '      kk_a%s=0.0_C_DOUBLE']
    label=getattr(k,'label',k.name[3:].rsplit('_',1)[0])
    if label.endswith('_tile'): label=label[:-5]
    cpp=['extern "C" void '+k.name+'(const KkLoopArgs* input) {', 'roms::initialize();', 'ROMS_KK_TIMER("extra '+label+'");', 'KkLoopArgs args=*input;']
    for slot,(n,d) in enumerate(k.arrays.items()):
        rank=len(d['dims'])
        original=d.get('fexpr',n)
        if 'flag_expr' in d:
            flag=re.sub(r'\bitrc\b','kk_it',d['flag_expr'])
            f += ['      DO kk_it=1,NT(ng)',f'      {n}(kk_it)=REAL(MERGE(1,0,{flag}),C_DOUBLE)','      END DO']
        dtype=d['type']
        if dtype not in ('double','int'): raise ValueError('unsupported array kind '+n)
        if d.get('allocatable',False): f += [f'      IF (ALLOCATED({original})) THEN']
        for q in range(rank):
            f += [f'      kk_a%b({10*slot+2*q+1})=LBOUND({original},{q+1})',f'      kk_a%b({10*slot+2*q+2})=UBOUND({original},{q+1})']
        if d['private']:
            cpp += [f'std::size_t count{slot}=1;']
            for q in range(rank): cpp += [f'count{slot}*=args.b[{10*slot+2*q+1}]-args.b[{10*slot+2*q}]+1;']
            cpp += [dtype+'* buffer'+str(slot)+'=nullptr;', '#ifndef __sw_slave__', f'static thread_local std::vector<{dtype}> scratch{slot};',f'scratch{slot}.resize(count{slot} * (int({high})-int({low})+1));',f'buffer{slot}=scratch{slot}.data();','#endif']
        else:
            f += [f'      kk_a%p({slot+1})=C_LOC({original})']
        if d.get('allocatable',False): f += ['      END IF']
    # Expression translation above may add bound scalars.
    seeds={n:k.scalar(n) for n in sorted(k.local_init)}
    for i,(value,logical) in enumerate(k.scalars.values()):
        value=f'MERGE(1,0,{value})' if logical else value
        f += [wrap_fline(f'      kk_a%s({i+1})=REAL({value},C_DOUBLE)')]
        key=list(k.scalars)[i]
        scalar_type=k.scalar_types[key]
        cpp += [f'const {scalar_type} s{i}=args.s[{i}];']
    # Scratch sizing needs scalar bounds; move allocation after captures.
    is_capture=lambda x: bool(re.match(r'const (double|int) s\d+=',x))
    scalar_lines=[x for x in cpp if is_capture(x)]
    cpp=[x for x in cpp if not is_capture(x)]
    cpp[4:4]=scalar_lines
    for slot,(n,d) in enumerate(k.arrays.items()):
        ptr=f'buffer{slot}' if d['private'] else f'args.p[{slot}]'
        view='base_'+n if d['private'] else n
        cpp += [f'roms_loops::Array<{len(d["dims"])},{d["type"]}> {view}({ptr},args.b+{10*slot});']
    cpp += ['using policy=Kokkos::RangePolicy<roms::exec_space,Kokkos::IndexType<int>>;', f'Kokkos::parallel_for("{k.name}",policy(int({low}),int({high})+1),KOKKOS_LAMBDA(int {axis}) {{']
    for slot,(n,d) in enumerate(k.arrays.items()):
        if d['private']:
            cpp += [f'auto {n}=base_{n};', f'{n}.p+=count{slot}*({axis}-int({low}));']
    for n,t in sorted(k.locals.items()):
        if n!=axis: cpp += [t+' '+n+('='+seeds[n] if n in seeds else '')+';']
    cpp += [body, '});', '}']
    if len(k.arrays)>128 or any(len(d['dims'])>5 for d in k.arrays.values()):
        raise ValueError('array metadata exceeds ABI capacity')
    if len(k.scalars)>256: raise ValueError('scalar metadata exceeds ABI capacity')
    f += [f'      CALL {k.name}(kk_a)']
    return '\n'.join(f)+'\n', '\n'.join(cpp)+'\n'


def prepare_diag(text):
    """Parallel row statistics, then deterministic Fortran reduction.

    Max-C ties retain the first (j,k,i) encountered by the original loop.
    The horizontal energy/volume sums and MPI collectives are untouched.
    """
    st=statements(text)
    a=next(i for i,x in enumerate(st) if x[0].startswith('subroutine diag_tile '))
    row=next(i for i in range(a,len(st)) if st[i][0]=='do j=jstr,jend')
    depth=1
    for b in range(row+1,len(st)):
        if re.match(r'(?:\w+\s*:\s*)?do\s+',st[b][0]): depth+=1
        if st[b][0].startswith(('end do','enddo')): depth-=1
        if depth==0: break
    names=['my_max_c','my_max_cu','my_max_cv','my_max_cw','my_max_ci','my_max_cj','my_max_ck','my_maxspeed','my_maxrho']
    initial=''.join('      '+n+'='+('-spval' if n=='my_maxrho' else '0')+'\n' for n in names)
    outputs=''.join(f'      kk_diag({i+1},j)={n}\n' for i,n in enumerate(names))
    fold=initial+'      DO j=Jstr,Jend\n      IF (kk_diag(1,j).gt.my_max_C) THEN\n'
    fold+=''.join(f'      {n}=kk_diag({i+1},j)\n' for i,n in enumerate(names[:7]))
    fold+='      END IF\n      my_maxspeed=MAX(my_maxspeed,kk_diag(8,j))\n      my_maxrho=MAX(my_maxrho,kk_diag(9,j))\n      END DO\n'
    text=text[:st[b][1]]+outputs+text[st[b][1]:st[b][2]]+fold+text[st[b][2]:]
    text=text[:st[row][2]]+initial+text[st[row][2]:]
    # Automatic array bounds use imported scratch bounds, not the tile
    # locals which are only assigned later by set_bounds.h.
    p=st[a][2]
    # Declaration must follow the USE statements, so attach to ke2d.
    needle=re.search(r'^\s*real\(r8\).*:: ke2d\b.*$',text,re.M|re.I)
    text=text[:needle.start()]+'\n      real(r8), TARGET :: kk_diag(9,JminS:JmaxS)\n'+text[needle.start():]
    return text


def prune_sources(block):
    """Point-source metadata may not even be allocated when disabled.
    Keep source-enabled rows in Fortran rather than eagerly touching that
    metadata while assembling arguments for a source-free C++ launch.
    """
    out=[]; guards=[]; i=0
    while i<len(block):
        s=block[i][0]
        m=re.fullmatch(r'if\s*\(\s*(l[uw]src\(ng\))\s*\)\s*then',s)
        if not m: out.append(block[i]); i+=1; continue
        guards.append('.not.'+m[1]); depth=1; i+=1
        while i<len(block):
            s=block[i][0]
            if re.match(r'if\s*\(.*\)\s*then$',s): depth+=1
            if s.startswith(('end if','endif')): depth-=1
            if depth==1 and s.startswith('else'): raise ValueError('source branch has ELSE')
            i+=1
            if depth==0: break
    return out,guards


def check_scalar_liveout(stmts, spans):
    """Reject a lowering that would hide a scalar needed by host code.
    Loop counters and row temporaries normally die at the phase boundary;
    model arrays, not those scalars, carry the state between row phases.
    """
    dirty=set(); at={a:(b,k) for a,b,k in spans}; i=0
    while i<len(stmts):
        if i in at:
            b,k=at[i]
            inputs=' '.join(v for v,_ in k.scalars.values())
            live=dirty & set(re.findall(r'\b\w+\b',inputs))
            if live: raise ValueError('scalar live-out into kernel: '+', '.join(sorted(live)))
            dirty |= k.assigned
            i=b+1; continue
        s=stmts[i][0]
        ass=re.match(r'(\w+)\s*=(?!=)',s)
        loop=re.match(r'(?:\w+\s*:\s*)?do\s+(\w+)\s*=\s*(.*)',s)
        reads=s.split('=',1)[1] if ass else loop[2] if loop else s
        if '::' in s or s.startswith(('use ','end ')): reads=''
        reads=re.sub(r"'(?:[^']|'')*'|\"(?:[^\"]|\"\")*\"",'',reads)
        live=dirty & set(re.findall(r'\b\w+\b',reads))
        if live: raise ValueError('scalar live-out into Fortran: '+', '.join(sorted(live)))
        if ass: dirty.discard(ass[1])
        if loop: dirty.discard(loop[1])
        i+=1


def process(text, stem):
    if stem=='diag': text=prepare_diag(text)
    cpp=['// Generated from preprocessed ROMS Fortran by row_codegen.py.', '#include "cppdefs.h"', '#include "kokkos_loops.hpp"']
    replacements=[]
    allstmts=statements(text)
    if stem=='bulk_flux':
        for helper in ('bulk_psiu','bulk_psit'):
            a=next((i for i,x in enumerate(allstmts) if x[0].startswith('function '+helper+' ')),None)
            if a is None: continue
            b=next(i for i in range(a+1,len(allstmts)) if allstmts[i][0].startswith('end function '+helper))
            h=allstmts[a:b]
            begin=next(i for i,x in enumerate(h) if x[0].startswith('if '))
            body=[x for x in h[begin:] if x[0]!='return']
            k=Kernel(helper,body,declarations(h))
            k.locals.update(zol='double',pi='double',r3='double')
            k.seen_write.update(('zol','pi','r3'))
            code=k.translate()
            if k.scalars: raise ValueError('uncaptured helper constant '+str(k.scalars))
            decl='\n'.join(t+' '+n+';' for n,t in k.locals.items() if n not in ('zol','pi','r3'))
            parameter=next(re.search(r'\br3\s*=\s*(.*)',x[0])[1] for x in h
                           if 'parameter' in x[0] and re.search(r'\br3\s*=',x[0]))
            r3=k.expr(parameter)
            cpp += ['KOKKOS_INLINE_FUNCTION double '+helper+'(double zol,double pi) {', 'const double r3='+r3+';',decl,code,'return '+helper+';','}']
            # Rename only the result variable, not the function itself.
            cpp[-4]=cpp[-4].replace(helper,'result')
            cpp[-3]=cpp[-3].replace(helper,'result')
            cpp[-2]='return result;'
    kernel_count=0
    for routine in TARGETS.get(stem,[]):
        start=next((i for i,x in enumerate(allstmts) if x[0].startswith('subroutine '+routine+' ' ) or x[0].startswith('subroutine '+routine+'(')),None)
        if start is None: continue
        end=next(i for i in range(start+1,len(allstmts)) if allstmts[i][0].startswith('end subroutine '+routine))
        stmts=allstmts[start:end+1]; dec=declarations(stmts)
        kernels=[]; spans=[]; pos=0
        for i,(s,a,b) in enumerate(stmts):
            if i<pos or not re.match(r'(?:\w+\s*:\s*)?do\s+j\s*=',s): continue
            if stem=='diag' and kernels: continue
            # Keep the finish routine's lateral boundary fills on the host.
            if routine=='lmd_finish_tile' and kernels: continue
            depth=1; finish=i
            for z in range(i+1,len(stmts)):
                ss=stmts[z][0]
                if re.match(r'(?:\w+\s*:\s*)?do\s+',ss): depth+=1
                if ss.startswith(('end do','enddo')): depth-=1
                if depth==0: finish=z; break
            if finish==i: raise ValueError('unterminated row '+s)
            original_block=stmts[i:finish+1]
            block,guards=prune_sources(original_block)
            k=Kernel('kk_'+stem+'_'+str(kernel_count),block,dec)
            k.label=routine
            kernel_count+=1
            try:
                body=k.translate()
                f,c=emit_kernel(k,body)
            except ValueError as e:
                raise ValueError(routine+' row '+str(len(kernels))+': '+str(e)) from e
            cpp.append(c); kernels.append(k); spans.append((i,finish,k))
            if stem in ('pre_step3d','step3d_t'):
                guards += ['(nnew.ge.1)', '(nnew.le.2)']
            guards += ['kk_row_contiguous('+d.get('fexpr',n)+')'
                       for n,d in k.arrays.items()
                       if not d['private'] and not d.get('allocatable',False)]
            if guards:
                original=text[a:stmts[finish][2]]
                condition=wrap_fline('      IF ('+' .and. '.join(guards)+') THEN')
                f=condition+'\n'+f+'      ELSE\n      CALL kk_loop_warn("'+stem+'")\n'+original+'      END IF\n'
            replacements.append((a,stmts[finish][2], f))
            pos=finish+1
        if not kernels: continue
        check_scalar_liveout(stmts,spans)
        replacements.append((stmts[0][2],stmts[0][2],'      USE kokkos_loops_mod\n'))
        # Before the first declaration, after the USE/implicit section.
        firstdecl=next(x for x in stmts if re.match(r'(integer|real\(r8\)|logical|character)\b',x[0]))
        interface=['      TYPE(kk_loop_args) :: kk_a']
        flags={n for k in kernels for n,d in k.arrays.items() if 'flag_expr' in d}
        if flags:
            synthetic=['      integer :: kk_it']
            synthetic += ['      real(C_DOUBLE), TARGET :: '+n+'(NT(ng))' for n in sorted(flags)]
            lastdecl=max(x[2] for x in stmts if '::' in x[0] and
                         re.match(r'(integer|real\(|logical|type\b|character)',x[0]))
            replacements.append((lastdecl,lastdecl,'\n'.join(synthetic)+'\n'))
        interface += ['      INTERFACE']
        for k in kernels:
            interface += [f'        SUBROUTINE {k.name}(args) BIND(C)', '          IMPORT :: kk_loop_args', '          TYPE(kk_loop_args), INTENT(IN) :: args',f'        END SUBROUTINE {k.name}']
        interface += ['      END INTERFACE']
        replacements.append((firstdecl[1],firstdecl[1],'\n'.join(interface)+'\n'))
        # C_LOC requires TARGET even for assumed-shape dummies. The caller
        # passes contiguous ROMS arrays; no temporary is introduced here.
        used={n for k in kernels for n,d in k.arrays.items() if not d['private']}
        for s,a,b in stmts:
            if '::' not in s or not s.startswith(('real(r8)','integer')): continue
            if any(re.search(r'\b'+n+r'\b',s.split('::',1)[1]) for n in used):
                raw=text[a:b]
                attrs=s.split('::',1)[0]
                if 'target' not in attrs and 'pointer' not in attrs: raw=raw.replace('::',', TARGET ::',1)
                replacements.append((a,b,raw))
    for a,b,r in sorted(replacements,reverse=True): text=text[:a]+r+text[b:]
    return text,'\n'.join(cpp)+'\n'


def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('source',type=Path);p.add_argument('fortran',type=Path);p.add_argument('cpp',type=Path)
    a=p.parse_args()
    text,cpp=process(a.source.read_text(),a.fortran.stem)
    a.fortran.write_text(text); a.cpp.write_text(cpp)

if __name__=='__main__': main()
