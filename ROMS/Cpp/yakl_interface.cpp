#include "YAKL.h"
#include <bits/stdc++.h>
#include "globaldefs.h"

#define dsign(a, b) ((b) >= 0.0 ? fabs(a) : -fabs(a))

typedef double real;
typedef yakl::Array<real, 1, yakl::memHost, yakl::styleFortran> realHost1d;
typedef yakl::Array<real, 2, yakl::memHost, yakl::styleFortran> realHost2d;
typedef yakl::Array<real, 3, yakl::memHost, yakl::styleFortran> realHost3d;
typedef yakl::Array<real, 1, yakl::memDevice, yakl::styleFortran> real1d;
typedef yakl::Array<real, 2, yakl::memDevice, yakl::styleFortran> real2d;
typedef yakl::Array<real, 3, yakl::memDevice, yakl::styleFortran> real3d;

int Istr_g, IstrB_g, IstrP_g, IstrR_g, IstrT_g, IstrM_g, IstrU_g;
int Iend_g, IendB_g, IendP_g, IendR_g, IendT_g;
int Jstr_g, JstrB_g, JstrP_g, JstrR_g, JstrT_g, JstrM_g, JstrV_g;
int Jend_g, JendB_g, JendP_g, JendR_g, JendT_g;
int Istrm3_g, Istrm2_g, Istrm1_g, IstrUm2_g, IstrUm1_g;
int Iendp1_g, Iendp2_g, Iendp2i_g, Iendp3_g;
int Jstrm3_g, Jstrm2_g, Jstrm1_g, JstrVm2_g, JstrVm1_g;
int Jendp1_g, Jendp2_g, Jendp2i_g, Jendp3_g;

int ng_g, tile_g;
int LBi_g, UBi_g, LBj_g, UBj_g, UBk_g;
int IminS_g, ImaxS_g, JminS_g, JmaxS_g;
int krhs_g, kstp_g, knew_g;
#ifdef SOLVE3D
int nstp_g, nnew_g;
#endif

extern "C" void pass_const(
    int &Istr_f, int &IstrB_f, int &IstrP_f, int &IstrR_f, int &IstrT_f, int &IstrM_f, int &IstrU_f,
    int &Iend_f, int &IendB_f, int &IendP_f, int &IendR_f, int &IendT_f,
    int &Jstr_f, int &JstrB_f, int &JstrP_f, int &JstrR_f, int &JstrT_f, int &JstrM_f, int &JstrV_f,
    int &Jend_f, int &JendB_f, int &JendP_f, int &JendR_f, int &JendT_f,
    int &Istrm3_f, int &Istrm2_f, int &Istrm1_f, int &IstrUm2_f, int &IstrUm1_f,
    int &Iendp1_f, int &Iendp2_f, int &Iendp2i_f, int &Iendp3_f,
    int &Jstrm3_f, int &Jstrm2_f, int &Jstrm1_f, int &JstrVm2_f, int &JstrVm1_f,
    int &Jendp1_f, int &Jendp2_f, int &Jendp2i_f, int &Jendp3_f,
    int &ng_f, int &tile_f,
    int &LBi_f, int &UBi_f, int &LBj_f, int &UBj_f, int &UBk_f,
    int &IminS_f, int &ImaxS_f, int &JminS_f, int &JmaxS_f,
    int &krhs_f, int &kstp_f, int &knew_f
#ifdef SOLVE3D
    ,
    int nstp_f, int nnew_f
#endif
)
{
    Istr_g = Istr_f;
    IstrB_g = IstrB_f;
    IstrP_g = IstrP_f;
    IstrR_g = IstrR_f;
    IstrT_g = IstrT_f;
    IstrM_g = IstrM_f;
    IstrU_g = IstrU_f;
    Iend_g = Iend_f;
    IendB_g = IendB_f;
    IendP_g = IendP_f;
    IendR_g = IendR_f;
    IendT_g = IendT_f;
    Jstr_g = Jstr_f;
    JstrB_g = JstrB_f;
    JstrP_g = JstrP_f;
    JstrR_g = JstrR_f;
    JstrT_g = JstrT_f;
    JstrM_g = JstrM_f;
    JstrV_g = JstrV_f;
    Jend_g = Jend_f;
    JendB_g = JendB_f;
    JendP_g = JendP_f;
    JendR_g = JendR_f;
    JendT_g = JendT_f;
    Istrm3_g = Istrm3_f;
    Istrm2_g = Istrm2_f;
    Istrm1_g = Istrm1_f;
    IstrUm2_g = IstrUm2_f;
    IstrUm1_g = IstrUm1_f;
    Iendp1_g = Iendp1_f;
    Iendp2_g = Iendp2_f;
    Iendp2i_g = Iendp2i_f;
    Iendp3_g = Iendp3_f;
    Jstrm3_g = Jstrm3_f;
    Jstrm2_g = Jstrm2_f;
    Jstrm1_g = Jstrm1_f;
    JstrVm2_g = JstrVm2_f;
    JstrVm1_g = JstrVm1_f;
    Jendp1_g = Jendp1_f;
    Jendp2_g = Jendp2_f;
    Jendp2i_g = Jendp2i_f;
    Jendp3_g = Jendp3_f;
    ng_g = ng_f;
    tile_g = tile_f;
    LBi_g = LBi_f;
    UBi_g = UBi_f;
    LBj_g = LBj_f;
    UBj_g = UBj_f;
    UBk_g = UBk_f;
    IminS_g = IminS_f;
    ImaxS_g = ImaxS_f;
    JminS_g = JminS_f;
    JmaxS_g = JmaxS_f;
    krhs_g = krhs_f;
    kstp_g = kstp_f;
    knew_g = knew_f;
#ifdef SOLVE3D
    nstp_g = nstp_f;
    nnew_g = nnew_f;
#endif
}

extern "C" void step_loop1(
    real *Drhs_f, real *zeta_f,
    int &zetax, int &zetay, int &zetaz,
    real *h_f, int &hx, int &hy,
    real *on_u_f, int &on_ux, int &on_uy,
    real *DUon_f, real *ubar_f,
    int &ubarx, int &ubary, int &ubarz,
#ifdef WEC
#ifdef WET_DRY
    real *umask_wet_f,
    int &umask_wetx, int &umask_wety,
#endif
    real *DUson_f, real *ubar_stokes_f,
    int &ubar_stokesx, int &ubar_stokesy,
#endif
    real *om_v_f, int &om_vx, int &om_vy,
    real *DVom_f, real *vbar_f,
    int &vbarx, int &vbary, int &vbarz
#ifdef WEC
#ifdef WET_DRT
    ,
    real *vmask_wet_f,
    int &vmask_wetx, int &vmask_wety,
#endif
    ,
    real *DVsom_f, real *vbar_stokes_f,
    int &vbar_stokesx, int &vbar_stokesy
#endif
)
{
#if defined DISTRIBUTE && !defined NESTING
    int IminS = IminS_g;
    int ImaxS = ImaxS_g;
    int JminS = JminS_g;
    int JmaxS = JmaxS_g;
    int LBi = LBi_g;
    int LBj = LBj_g;
    int krhs = krhs_g;
    int JstrV = JstrV_g;
    int Jendp2 = Jendp2_g;
    int IstrU = IstrU_g;
    int Iendp2 = Iendp2_g;
    // std::cout << IminS << " " << ImaxS << " " << JminS << " " << JmaxS << " " << LBi << " " << LBj << " " << krhs << " " << JstrV << " " << Jendp2 << " " << IstrU << " " << Iendp2 << std::endl;

    realHost2d Drhs_h("Drhs_h", Drhs_f, {IminS, ImaxS}, {JminS, JmaxS});
    realHost3d zeta_h("zeta_h", zeta_f, {LBi, LBi + zetax - 1}, {LBj, LBj + zetay - 1}, zetaz);
    realHost2d h_h("h_h", h_f, {LBi, LBi + hx - 1}, {LBj, LBj + hy - 1});

    realHost2d on_u_h("on_u_h", on_u_f, {LBi, LBi + on_ux - 1}, {LBj, LBj + on_uy - 1});
    realHost2d DUon_h("DUon_h", DUon_f, {IminS, ImaxS}, {JminS, JmaxS});
    realHost3d ubar_h("ubar_h", ubar_f, {LBi, LBi + ubarx - 1}, {LBj, LBj + ubary - 1}, ubarz);
#ifdef WEC
#ifdef WET_DRY
    realHost2d umask_wet_h("umask_wet_h", u_mask_wet_f, {LBi, LBi + umask_wetx - 1}, {LBj, LBj + umask_wety - 1});
#endif
    realHost2d DUson_h("DUson_h", DUson_f, {IminS, ImaxS}, {JminS, JmaxS});
    realHost2d ubar_stokes_h("ubar_stokes_h", ubar_stokes_f, {LBi, LBi + ubar_stokesx - 1}, {LBj, LBj + ubar_stokesy - 1});
#endif
    realHost2d om_v_h("om_v_h", om_v_f, {LBi, LBi + om_vx - 1}, {LBj, LBj + om_vy - 1});
    realHost2d DVom_h("DVom_h", DVom_f, {IminS, ImaxS}, {JminS, JmaxS});
    realHost3d vbar_h("vbar_h", vbar_f, {LBi, LBi + vbarx - 1}, {LBj, LBj + vbary - 1}, vbarz);
#ifdef WEC
#ifdef WET_DRY
    realHost2d vmask_wet_h("vmask_wet_h", v_mask_wet_f, {LBi, LBi + vmask_wetx - 1}, {LBj, LBj + vmask_wety - 1});
#endif
    realHost2d DVsom_h("DVsom_h", DVsom_f, {IminS, ImaxS}, {JminS, JmaxS});
    realHost2d vbar_stokes_h("vbar_stokes_h", vbar_stokes_f, {LBi, LBi + vbar_stokesx - 1}, {LBj, LBj + vbar_stokesy - 1});
#endif
    // begin
    real2d Drhs_d("Drhs_d", {IminS, ImaxS}, {JminS, JmaxS});
    real3d zeta_d("zeta_d", {LBi, LBi + zetax - 1}, {LBj, LBj + zetay - 1}, zetaz);
    real2d h_d("h_d", {LBi, LBi + hx - 1}, {LBj, LBj + hy - 1});

    real2d on_u_d("on_u_d", {LBi, LBi + on_ux - 1}, {LBj, LBj + on_uy - 1});
    real2d DUon_d("DUon_d", {IminS, ImaxS}, {JminS, JmaxS});
    real3d ubar_d("ubar_d", {LBi, LBi + ubarx - 1}, {LBj, LBj + ubary - 1}, ubarz);
    // end
#ifdef WEC
#ifdef WET_DRY
    real2d umask_wet_d("umask_wet_d", {LBi, LBi + umask_wetx - 1}, {LBj, LBj + umask_wety - 1});
#endif
    real2d DUson_d("DUson_d", {IminS, ImaxS}, {JminS, JmaxS});
    real2d ubar_stokes_d("ubar_stokes_d", {LBi, LBi + ubar_stokesx - 1}, {LBj, LBj + ubar_stokesy - 1});
#endif
    real2d om_v_d("om_v_d", {LBi, LBi + om_vx - 1}, {LBj, LBj + om_vy - 1});
    real2d DVom_d("DVom_d", {IminS, ImaxS}, {JminS, JmaxS});
    real3d vbar_d("vbar_d", {LBi, LBi + vbarx - 1}, {LBj, LBj + vbary - 1}, vbarz);
#ifdef WEC
#ifdef WET_DRY
    real2d vmask_wet_d("vmask_wet_d", {LBi, LBi + vmask_wetx - 1}, {LBj, LBj + vmask_wety - 1});
#endif
    real2d DVsom_d("DVsom_d", {IminS, ImaxS}, {JminS, JmaxS});
    real2d vbar_stokes_d("vbar_stokes_d", {LBi, LBi + vbar_stokesx - 1}, {LBj, LBj + vbar_stokesy - 1});
#endif

    Drhs_h.deep_copy_to(Drhs_d);
    zeta_h.deep_copy_to(zeta_d);
    h_h.deep_copy_to(h_d);

    on_u_h.deep_copy_to(on_u_d);
    DUon_h.deep_copy_to(DUon_d);
    ubar_h.deep_copy_to(ubar_d);
#ifdef WEC
#ifdef WET_DRY
    umask_wet_h.deep_copy_to(umask_wet_d);
#endif
    DUson_h.deep_copy_to(DUson_d);
    ubar_stokes_h.deep_copy_to(ubar_stokes_d);
#endif
    om_v_h.deep_copy_to(om_v_d);
    DVom_h.deep_copy_to(DVom_d);
    vbar_h.deep_copy_to(vbar_d);
#ifdef WEC
#ifdef WET_DRY
    vmask_wet_h.deep_copy_to(vmask_wet_d);
#endif
    DVsom_h.deep_copy_to(DVsom_d);
    vbar_stokes_h.deep_copy_to(vbar_stokes_d);
#endif
    // real cff, cff(1), cff(2), cff(3), cff(4), cff(5), cff(6), cff(7);
    yakl::SArray<real, 1, 8> cff;

    yakl::fortran::parallel_for(
        "step_loop1_1",
        yakl::fortran::Bounds<2>({JstrV - 2, Jendp2}, {IstrU - 2, Iendp2}),
        YAKL_LAMBDA(int j, int i) {
            Drhs_d(i, j) = zeta_d(i, j, krhs) + h_d(i, j);
        });
    yakl::fortran::parallel_for(
        "step_loop1_2",
        yakl::fortran::Bounds<2>({JstrV - 2, Jendp2}, {IstrU - 1, Iendp2}),
        YAKL_LAMBDA(int j, int i) {
            cff(0) = 0.5 * on_u_d(i, j);
            cff(1) = cff(0) * (Drhs_d(i, j) + Drhs_d(i - 1, j));
            DUon_d(i, j) = ubar_d(i, j, krhs) * cff(1);
#ifdef WEC
#ifdef WET_DRY
            cff(5) = fabs(fabs(umask_wet(i, j)) - 1.0);
            cff(6) = 0.5 + dsign(0.5, ubar_stokes_d(i, j)) * umask_wet_d(i, j);
            cff(7) = 0.5 * umask_wet_d(i, j) * cff(5) + cff(6) * (1.0 - cff(5));
            cff(1) = cff(1) * cff(7);
#endif
            DUSon_d(i, j) = ubar_stokes_d(i, j) * cff(1);
            DUon_d(i, j) = DUon_d(i, j) + DUSon_d(i, j);
#endif
        });
    yakl::fortran::parallel_for(
        "step_loop1_3",
        yakl::fortran::Bounds<2>({JstrV - 1, Jendp2}, {IstrU - 2, Iendp2}),
        YAKL_LAMBDA(int j, int i) {
            cff(0) = 0.5 * om_v_d(i, j);
            cff(1) = cff(0) * (Drhs_d(i, j) + Drhs_d(i, j - 1));
            DVom_d(i, j) = vbar_d(i, j, krhs) * cff(1);
#ifdef WEC
#ifdef WET_DRY
            cff(5) = fabs(fabs(vmask_wet(i, j)) - 1.0);
            cff(6) = 0.5 + dsign(0.5, vbar_stokes_d(i, j)) * vmask_wet_d(i, j);
            cff(7) = 0.5 * vmask_wet_d(i, j) * cff(5) + cff(6) * (1.0 - cff(5));
            cff(1) = cff(1) * cff(7);
#endif
            DVsom_d(i, j) = vbar_stokes_d(i, j) * cff(1);
            DVom_d(i, j) = DVom_d(i, j) + DVsom_d(i, j);
#endif
        });

    Drhs_d.deep_copy_to(Drhs_h);
    zeta_d.deep_copy_to(zeta_h);
    h_d.deep_copy_to(h_h);

    on_u_d.deep_copy_to(on_u_h);
    DUon_d.deep_copy_to(DUon_h);
    ubar_d.deep_copy_to(ubar_h);
#ifdef WEC
#ifdef WET_DRY
    umask_wet_d.deep_copy_to(umask_wet_h);
#endif
    DUson_d.deep_copy_to(DUson_h);
    ubar_stokes_d.deep_copy_to(ubar_stokes_h);
#endif
    om_v_d.deep_copy_to(om_v_h);
    DVom_d.deep_copy_to(DVom_h);
    vbar_d.deep_copy_to(vbar_h);
#ifdef WEC
#ifdef WET_DRY
    vmask_wet_d.deep_copy_to(vmask_wet_h);
#endif
    DVsom_d.deep_copy_to(DVsom_h);
    vbar_stokes_d.deep_copy_to(vbar_stokes_h);
#endif
    yakl::fence();
#else
    int IminS = IminS_g;
    int ImaxS = ImaxS_g;
    int JminS = JminS_g;
    int JmaxS = JmaxS_g;
    int LBi = LBi_g;
    int LBj = LBj_g;
    int krhs = krhs_g;
    int JstrVm2 = JstrVm2_g;
    int Jendp2 = Jendp2_g;
    int IstrUm2 = IstrUm2_g;
    int Iendp2 = Iendp2_g;

    realHost2d Drhs_h("Drhs_h", Drhs_f, {IminS, ImaxS}, {JminS, JmaxS});
    realHost3d zeta_h("zeta_h", zeta_f, {LBi, LBi + zetax - 1}, {LBj, LBj + zetay - 1}, zetaz);
    realHost2d h_h("h_h", h_f, {LBi, LBi + hx - 1}, {LBj, LBj + hy - 1});

    realHost2d on_u_h("on_u_h", on_u_f, {LBi, LBi + on_ux - 1}, {LBj, LBj + on_uy - 1});
    realHost2d DUon_h("DUon_h", DUon_f, {IminS, ImaxS}, {JminS, JmaxS});
    realHost3d ubar_h("ubar_h", ubar_f, {LBi, LBi + ubarx - 1}, {LBj, LBj + ubary - 1}, ubarz);
#ifdef WEC
#ifdef WET_DRY
    realHost2d umask_wet_h("umask_wet_h", u_mask_wet_f, {LBi, LBi + umask_wetx - 1}, {LBj, LBj + umask_wety - 1});
#endif
    realHost2d DUson_h("DUson_h", DUson_f, {IminS, ImaxS}, {JminS, JmaxS});
    realHost2d ubar_stokes_h("ubar_stokes_h", ubar_stokes_f, {LBi, LBi + ubar_stokesx - 1}, {LBj, LBj + ubar_stokesy - 1});
#endif
    realHost2d om_v_h("om_v_h", om_v_f, {LBi, LBi + om_vx - 1}, {LBj, LBj + om_vy - 1});
    realHost2d DVom_h("DVom_h", DVom_f, {IminS, ImaxS}, {JminS, JmaxS});
    realHost3d vbar_h("vbar_h", vbar_f, {LBi, LBi + vbarx - 1}, {LBj, LBj + vbary - 1}, vbarz);
#ifdef WEC
#ifdef WET_DRY
    realHost2d vmask_wet_h("vmask_wet_h", v_mask_wet_f, {LBi, LBi + vmask_wetx - 1}, {LBj, LBj + vmask_wety - 1});
#endif
    realHost2d DVsom_h("DVsom_h", DVsom_f, {IminS, ImaxS}, {JminS, JmaxS});
    realHost2d vbar_stokes_h("vbar_stokes_h", vbar_stokes_f, {LBi, LBi + vbar_stokesx - 1}, {LBj, LBj + vbar_stokesy - 1});
#endif

    real2d Drhs_d("Drhs_d", {IminS, ImaxS}, {JminS, JmaxS});
    real3d zeta_d("zeta_d", {LBi, LBi + zetax - 1}, {LBj, LBj + zetay - 1}, zetaz);
    real2d h_d("h_d", {LBi, LBi + hx - 1}, {LBj, LBj + hy - 1});

    real2d on_u_d("on_u_d", {LBi, LBi + on_ux - 1}, {LBj, LBj + on_uy - 1});
    real2d DUon_d("DUon_d", {IminS, ImaxS}, {JminS, JmaxS});
    real3d ubar_d("ubar_d", {LBi, LBi + ubarx - 1}, {LBj, LBj + ubary - 1}, ubarz);
#ifdef WEC
#ifdef WET_DRY
    real2d umask_wet_d("umask_wet_d", {LBi, LBi + umask_wetx - 1}, {LBj, LBj + umask_wety - 1});
#endif
    real2d DUson_d("DUson_d", {IminS, ImaxS}, {JminS, JmaxS});
    real2d ubar_stokes_d("ubar_stokes_d", {LBi, LBi + ubar_stokesx - 1}, {LBj, LBj + ubar_stokesy - 1});
#endif
    real2d om_v_d("om_v_d", {LBi, LBi + om_vx - 1}, {LBj, LBj + om_vy - 1});
    real2d DVom_d("DVom_d", {IminS, ImaxS}, {JminS, JmaxS});
    real3d vbar_d("vbar_d", {LBi, LBi + vbarx - 1}, {LBj, LBj + vbary - 1}, vbarz);
#ifdef WEC
#ifdef WET_DRY
    real2d vmask_wet_d("vmask_wet_d", {LBi, LBi + vmask_wetx - 1}, {LBj, LBj + vmask_wety - 1});
#endif
    real2d DVsom_d("DVsom_d", {IminS, ImaxS}, {JminS, JmaxS});
    real2d vbar_stokes_d("vbar_stokes_d", {LBi, LBi + vbar_stokesx - 1}, {LBj, LBj + vbar_stokesy - 1});
#endif

    Drhs_h.deep_copy_to(Drhs_d);
    zeta_h.deep_copy_to(zeta_d);
    h_h.deep_copy_to(h_d);

    on_u_h.deep_copy_to(on_u_d);
    DUon_h.deep_copy_to(DUon_d);
    ubar_h.deep_copy_to(ubar_d);
#ifdef WEC
#ifdef WET_DRY
    umask_wet_h.deep_copy_to(umask_wet_d);
#endif
    DUson_h.deep_copy_to(DUson_d);
    ubar_stokes_h.deep_copy_to(ubar_stokes_d);
#endif
    om_v_h.deep_copy_to(om_v_d);
    DVom_h.deep_copy_to(DVom_d);
    vbar_h.deep_copy_to(vbar_d);
#ifdef WEC
#ifdef WET_DRY
    vmask_wet_h.deep_copy_to(vmask_wet_d);
#endif
    DVsom_h.deep_copy_to(DVsom_d);
    vbar_stokes_h.deep_copy_to(vbar_stokes_d);
#endif
    // real cff, cff(1), cff(2), cff(3), cff(4), cff(5), cff(6), cff(7);
    yakl::SArray<real, 1, 8> cff;

    yakl::fortran::parallel_for(
        "step_loop1_1",
        yakl::fortran::Bounds<2>({JstrVm2 - 1, Jendp2}, {IstrUm2 - 1, Iendp2}),
        YAKL_LAMBDA(int j, int i) {
            Drhs_d(i, j) = zeta_d(i, j, krhs) + h_d(i, j);
        });
    yakl::fortran::parallel_for(
        "step_loop1_2",
        yakl::fortran::Bounds<2>({JstrVm2 - 1, Jendp2}, {IstrUm2, Iendp2}),
        YAKL_LAMBDA(int j, int i) {
            cff(0) = 0.5 * on_u_d(i, j);
            cff(1) = cff(0) * (Drhs_d(i, j) + Drhs_d(i - 1, j));
            DUon_d(i, j) = ubar_d(i, j, krhs) * cff(1);
#ifdef WEC
#ifdef WET_DRY
            cff(5) = fabs(fabs(umask_wet(i, j)) - 1.0);
            cff(6) = 0.5 + dsign(0.5, ubar_stokes_d(i, j)) * umask_wet_d(i, j);
            cff(7) = 0.5 * umask_wet_d(i, j) * cff(5) + cff(6) * (1.0 - cff(5));
            cff(1) = cff(1) * cff(7);
#endif
            DUSon_d(i, j) = ubar_stokes_d(i, j) * cff(1);
            DUon_d(i, j) = DUon_d(i, j) + DUSon_d(i, j);
#endif
        });
    yakl::fortran::parallel_for(
        "step_loop1_3",
        yakl::fortran::Bounds<2>({JstrVm2, Jendp2}, {IstrUm2 - 1, Iendp2}),
        YAKL_LAMBDA(int j, int i) {
            cff(0) = 0.5 * om_v_d(i, j);
            cff(1) = cff(0) * (Drhs_d(i, j) + Drhs_d(i, j - 1));
            DVom_d(i, j) = vbar_d(i, j, krhs) * cff(1);
#ifdef WEC
#ifdef WET_DRY
            cff(5) = fabs(fabs(vmask_wet(i, j)) - 1.0);
            cff(6) = 0.5 + dsign(0.5, vbar_stokes_d(i, j)) * vmask_wet_d(i, j);
            cff(7) = 0.5 * vmask_wet_d(i, j) * cff(5) + cff(6) * (1.0 - cff(5));
            cff(1) = cff(1) * cff(7);
#endif
            DVsom_d(i, j) = vbar_stokes_d(i, j) * cff(1);
            DVom_d(i, j) = DVom_d(i, j) + DVsom_d(i, j);
#endif
        });

    Drhs_d.deep_copy_to(Drhs_h);
    // zeta_d.deep_copy_to(zeta_h);
    // h_d.deep_copy_to(h_h);

    // on_u_d.deep_copy_to(on_u_h);
    DUon_d.deep_copy_to(DUon_h);
    // ubar_d.deep_copy_to(ubar_h);
#ifdef WEC
#ifdef WET_DRY
    // umask_wet_d.deep_copy_to(umask_wet_h);
#endif
    DUson_d.deep_copy_to(DUson_h);
    // ubar_stokes_d.deep_copy_to(ubar_stokes_h);
#endif
    // om_v_d.deep_copy_to(om_v_h);
    DVom_d.deep_copy_to(DVom_h);
    // vbar_d.deep_copy_to(vbar_h);
#ifdef WEC
#ifdef WET_DRY
    // vmask_wet_d.deep_copy_to(vmask_wet_h);
#endif
    DVsom_d.deep_copy_to(DVsom_h);
    // vbar_stokes_d.deep_copy_to(vbar_stokes_h);
#endif
    yakl::fence();
#endif
}

extern "C" void step_loop2(
    real &cff2,
    real *Zt_avg1_f, int &Zt_avg1x, int &Zt_avg1y,
    real *DU_avg1_f, int &DU_avg1x, int &DU_avg1y,
    real *DU_avg2_f, int &DU_avg2x, int &DU_avg2y,
    real *DUon_f,
    real *DV_avg1_f, int &DV_avg1x, int &DV_avg1y,
    real *DV_avg2_f, int &DV_avg2x, int &DV_avg2y,
    real *DVom_f)
{
    int IminS = IminS_g;
    int ImaxS = ImaxS_g;
    int JminS = JminS_g;
    int JmaxS = JmaxS_g;
    int LBi = LBi_g;
    int LBj = LBj_g;
    int JstrR = JstrR_g;
    int JendR = JendR_g;
    int IstrR = IstrR_g;
    int IendR = IendR_g;
    int Istr = Istr_g;
    int Jstr = Jstr_g;

    realHost2d Zt_avg1_h("Zt_avg1_h", Zt_avg1_f, {LBi, LBi + Zt_avg1x - 1}, {LBj, LBj + Zt_avg1y - 1});
    realHost2d DU_avg1_h("DU_avg1_h", DU_avg1_f, {LBi, LBi + DU_avg1x - 1}, {LBj, LBj + DU_avg1y - 1});
    realHost2d DU_avg2_h("DU_avg2_h", DU_avg2_f, {LBi, LBi + DU_avg2x - 1}, {LBj, LBj + DU_avg2y - 1});
    realHost2d DUon_h("DUon_h", DUon_f, {IminS, ImaxS}, {JminS, JmaxS});
    realHost2d DV_avg1_h("DV_avg1_h", DV_avg1_f, {LBi, LBi + DV_avg1x - 1}, {LBj, LBj + DV_avg1y - 1});
    realHost2d DV_avg2_h("DV_avg2_h", DV_avg2_f, {LBi, LBi + DV_avg2x - 1}, {LBj, LBj + DV_avg2y - 1});
    realHost2d DVom_h("DVom_h", DVom_f, {IminS, ImaxS}, {JminS, JmaxS});

    real2d Zt_avg1_d("Zt_avg1_d", {LBi, LBi + Zt_avg1x - 1}, {LBj, LBj + Zt_avg1y - 1});
    real2d DU_avg1_d("DU_avg1_d", {LBi, LBi + DU_avg1x - 1}, {LBj, LBj + DU_avg1y - 1});
    real2d DU_avg2_d("DU_avg2_d", {LBi, LBi + DU_avg2x - 1}, {LBj, LBj + DU_avg2y - 1});
    real2d DUon_d("DUon_d", {IminS, ImaxS}, {JminS, JmaxS});
    real2d DV_avg1_d("DV_avg1_d", {LBi, LBi + DV_avg1x - 1}, {LBj, LBj + DV_avg1y - 1});
    real2d DV_avg2_d("DV_avg2_d", {LBi, LBi + DV_avg2x - 1}, {LBj, LBj + DV_avg2y - 1});
    real2d DVom_d("DVom_d", {IminS, ImaxS}, {JminS, JmaxS});

    Zt_avg1_h.deep_copy_to(Zt_avg1_d);
    DU_avg1_h.deep_copy_to(DU_avg1_d);
    DU_avg2_h.deep_copy_to(DU_avg2_d);
    DUon_h.deep_copy_to(DUon_d);
    DV_avg1_h.deep_copy_to(DV_avg1_d);
    DV_avg2_h.deep_copy_to(DV_avg2_d);
    DVom_h.deep_copy_to(DVom_d);

    yakl::fortran::parallel_for(
        "step2_loop2_1",
        yakl::fortran::Bounds<2>({JstrR, JendR}, {IstrR, IendR}),
        YAKL_LAMBDA(int j, int i) {
            Zt_avg1_d(i, j) = 0.0;
        });
    yakl::fortran::parallel_for(
        "step2_loop2_2",
        yakl::fortran::Bounds<2>({JstrR, JendR}, {Istr, IendR}),
        YAKL_LAMBDA(int j, int i) {
            DU_avg1_d(i, j) = 0.0;
            DU_avg2_d(i, j) = cff2 * DUon_d(i, j);
        });
    yakl::fortran::parallel_for(
        "step2_loop2_3",
        yakl::fortran::Bounds<2>({Jstr, JendR}, {IstrR, IendR}),
        YAKL_LAMBDA(int j, int i) {
            DV_avg1_d(i, j) = 0.0;
            DV_avg2_d(i, j) = cff2 * DVom_d(i, j);
        });

    Zt_avg1_d.deep_copy_to(Zt_avg1_h);
    DU_avg1_d.deep_copy_to(DU_avg1_h);
    DU_avg2_d.deep_copy_to(DU_avg2_h);
    DV_avg1_d.deep_copy_to(DV_avg1_h);
    DV_avg2_d.deep_copy_to(DV_avg2_h);

    yakl::fence();
}