  �"  :   k820309    �          2021.11.1   ���f                                                                                                          
       /staff/liyuanhang/work/2024OCT/roms_8/roms/project/upwelling/CBuild_romsM/f90/mod_sources.f90 MOD_SOURCES          @       �                                          
                                                                   
                                                                                                                                                                                                                                                                                                    1#         @                                                               #MOD_PARALLEL!ALLOCATE_SOURCES%MPIFCMB5    #MOD_PARALLEL!ALLOCATE_SOURCES%MPIFCMB9 	   #MOD_PARALLEL!ALLOCATE_SOURCES%MPIPRIV1    #MOD_PARALLEL!ALLOCATE_SOURCES%MPIPRIV2    #MOD_PARALLEL!ALLOCATE_SOURCES%MPIPRIVC    #NG                                                                                                                                                                 #ALLOCATE_SOURCES%MPIFCMB5%MPI_UNWEIGHTED              �             �                                                                                               	                          #ALLOCATE_SOURCES%MPIFCMB9%MPI_WEIGHTS_EMPTY 
             �             �                          
                                                                                               #ALLOCATE_SOURCES%MPIPRIV1%MPI_BOTTOM    #ALLOCATE_SOURCES%MPIPRIV1%MPI_IN_PLACE    #ALLOCATE_SOURCES%MPIPRIV1%MPI_STATUS_IGNORE              �             �                                                        �             �                                                       �             �                                                          p          p            p                                                                                                             #ALLOCATE_SOURCES%MPIPRIV2%MPI_STATUSES_IGNORE    #ALLOCATE_SOURCES%MPIPRIV2%MPI_ERRCODES_IGNORE              �             �                                                           p          p          p            p          p                                            �             �                                                          p          p            p                                                                                                             #ALLOCATE_SOURCES%MPIPRIVC%MPI_ARGVS_NULL    #ALLOCATE_SOURCES%MPIPRIVC%MPI_ARGV_NULL    -          �             �                                                           p          p          p            p          p                                  -          �             �                                                          p          p            p                                             @                                                    #         @                                                                #NG                                                                                       @                                        '                    #ISRC    #JSRC    #DSRC    #FSRC    #QBAR    #QSHAPE    #QSRC    #TSRC     #XSRC !   #YSRC "   #QBARG #   #TSRCG $               �                                                                                 &                                                       �                                                   H                             &                                                       �                                                  �                 
            &                                                       �                                                  �                 
            &                                                       �                                                                   
            &                                                       �                                                  h                
            &                   &                                                       �                                                  �                
            &                   &                                                       �                                                   (                
            &                   &                   &                                                       �                                      !            �             	   
            &                                                       �                                      "            �             
   
            &                                                       �                                      #            0                
            &                   &                                                       �                                      $            �                
            &                   &                   &                   &                                                    @ @                                        %                                    &                                           #T_SOURCES             @ @                                        &                                   &                                                    @ @                                        '                                   &                                           #         @                                            (                   #MOD_PARALLEL!CHECK_SOURCES%MPIFCMB5 )   #MOD_PARALLEL!CHECK_SOURCES%MPIFCMB9 +   #MOD_PARALLEL!CHECK_SOURCES%MPIPRIV1 -   #MOD_PARALLEL!CHECK_SOURCES%MPIPRIV2 1   #MOD_PARALLEL!CHECK_SOURCES%MPIPRIVC 4   #NG 7   #NCNAME 8   #NPSRC 9                                                                                                                             )                          #CHECK_SOURCES%MPIFCMB5%MPI_UNWEIGHTED *             �             �                          *                                                                     +                          #CHECK_SOURCES%MPIFCMB9%MPI_WEIGHTS_EMPTY ,             �             �                          ,                                                                     -                          #CHECK_SOURCES%MPIPRIV1%MPI_BOTTOM .   #CHECK_SOURCES%MPIPRIV1%MPI_IN_PLACE /   #CHECK_SOURCES%MPIPRIV1%MPI_STATUS_IGNORE 0             �             �                          .                              �             �                          /                             �             �                          0                                p          p            p                                                                                   1                          #CHECK_SOURCES%MPIPRIV2%MPI_STATUSES_IGNORE 2   #CHECK_SOURCES%MPIPRIV2%MPI_ERRCODES_IGNORE 3             �             �                          2                                 p          p          p            p          p                                            �             �                          3                                p          p            p                                                                                   4                          #CHECK_SOURCES%MPIPRIVC%MPI_ARGVS_NULL 5   #CHECK_SOURCES%MPIPRIVC%MPI_ARGV_NULL 6   -          �             �                          5                                 p          p          p            p          p                                  -          �             �                          6                                p          p            p                                            
  @                                        7                     
  @                                      8                    1           
                                           9              �   r      fn#fn      H   J   MOD_KINDS    Z  H   J   MOD_PARAM    �  x       R8+MOD_KINDS !     H       NGRIDS+MOD_PARAM    b  y       INLM+MOD_PARAM !   �  �      ALLOCATE_SOURCES M   a  �   �  MOD_PARALLEL!ALLOCATE_SOURCES%MPIFCMB5+MOD_PARALLEL=MPIFCMB5 F   �  P     ALLOCATE_SOURCES%MPIFCMB5%MPI_UNWEIGHTED+MOD_PARALLEL M   7  �   �  MOD_PARALLEL!ALLOCATE_SOURCES%MPIFCMB9+MOD_PARALLEL=MPIFCMB9 I   �  P     ALLOCATE_SOURCES%MPIFCMB9%MPI_WEIGHTS_EMPTY+MOD_PARALLEL M     �   �  MOD_PARALLEL!ALLOCATE_SOURCES%MPIPRIV1+MOD_PARALLEL=MPIPRIV1 B   �  P     ALLOCATE_SOURCES%MPIPRIV1%MPI_BOTTOM+MOD_PARALLEL D   ?  P     ALLOCATE_SOURCES%MPIPRIV1%MPI_IN_PLACE+MOD_PARALLEL I   �  �     ALLOCATE_SOURCES%MPIPRIV1%MPI_STATUS_IGNORE+MOD_PARALLEL M   ;  �   �  MOD_PARALLEL!ALLOCATE_SOURCES%MPIPRIV2+MOD_PARALLEL=MPIPRIV2 K   �  �     ALLOCATE_SOURCES%MPIPRIV2%MPI_STATUSES_IGNORE+MOD_PARALLEL K   �	  �     ALLOCATE_SOURCES%MPIPRIV2%MPI_ERRCODES_IGNORE+MOD_PARALLEL M   q
  �   �  MOD_PARALLEL!ALLOCATE_SOURCES%MPIPRIVC+MOD_PARALLEL=MPIPRIVC F   $  �     ALLOCATE_SOURCES%MPIPRIVC%MPI_ARGVS_NULL+MOD_PARALLEL E   �  �     ALLOCATE_SOURCES%MPIPRIVC%MPI_ARGV_NULL+MOD_PARALLEL $   �  H   a   ALLOCATE_SOURCES%NG #   �  X       DEALLOCATE_SOURCES &   <  H   a   DEALLOCATE_SOURCES%NG    �  �       T_SOURCES    X  �   a   T_SOURCES%ISRC    �  �   a   T_SOURCES%JSRC    �  �   a   T_SOURCES%DSRC    ,  �   a   T_SOURCES%FSRC    �  �   a   T_SOURCES%QBAR !   d  �   a   T_SOURCES%QSHAPE      �   a   T_SOURCES%QSRC    �  �   a   T_SOURCES%TSRC    �  �   a   T_SOURCES%XSRC    4  �   a   T_SOURCES%YSRC     �  �   a   T_SOURCES%QBARG     �  �   a   T_SOURCES%TSRCG    h  �       SOURCES      �       MSRC    �  �       NSRC    3  �      CHECK_SOURCES J   �  �   �  MOD_PARALLEL!CHECK_SOURCES%MPIFCMB5+MOD_PARALLEL=MPIFCMB5 C   ;  P     CHECK_SOURCES%MPIFCMB5%MPI_UNWEIGHTED+MOD_PARALLEL J   �  �   �  MOD_PARALLEL!CHECK_SOURCES%MPIFCMB9+MOD_PARALLEL=MPIFCMB9 F     P     CHECK_SOURCES%MPIFCMB9%MPI_WEIGHTS_EMPTY+MOD_PARALLEL J   a  �   �  MOD_PARALLEL!CHECK_SOURCES%MPIPRIV1+MOD_PARALLEL=MPIPRIV1 ?   7  P     CHECK_SOURCES%MPIPRIV1%MPI_BOTTOM+MOD_PARALLEL A   �  P     CHECK_SOURCES%MPIPRIV1%MPI_IN_PLACE+MOD_PARALLEL F   �  �     CHECK_SOURCES%MPIPRIV1%MPI_STATUS_IGNORE+MOD_PARALLEL J   �  �   �  MOD_PARALLEL!CHECK_SOURCES%MPIPRIV2+MOD_PARALLEL=MPIPRIV2 H   ;  �     CHECK_SOURCES%MPIPRIV2%MPI_STATUSES_IGNORE+MOD_PARALLEL H     �     CHECK_SOURCES%MPIPRIV2%MPI_ERRCODES_IGNORE+MOD_PARALLEL J   �  �   �  MOD_PARALLEL!CHECK_SOURCES%MPIPRIVC+MOD_PARALLEL=MPIPRIVC C   `   �     CHECK_SOURCES%MPIPRIVC%MPI_ARGVS_NULL+MOD_PARALLEL B   ,!  �     CHECK_SOURCES%MPIPRIVC%MPI_ARGV_NULL+MOD_PARALLEL !   �!  H   a   CHECK_SOURCES%NG %    "  T   a   CHECK_SOURCES%NCNAME $   t"  H   a   CHECK_SOURCES%NPSRC 