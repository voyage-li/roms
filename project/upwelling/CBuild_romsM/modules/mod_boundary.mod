  <0  C   k820309    �          2021.11.1   ���f                                                                                                          
       /staff/liyuanhang/work/2024OCT/roms_8/roms/project/upwelling/CBuild_romsM/f90/mod_boundary.f90 MOD_BOUNDARY          @       �                                          
                                                                                                                      #         @                                                                #NG                                                                                                                                                                                          
                                                      #         @                                                                #NG                                                                                                                                                                                                      
                                                      #         @                                                                #NG    #TILE 	   #MODEL 
                                                                                                                                                                                                     
                                                                
                                           	                     
                                           
                              @                                        '                    #WEST    #EAST    #SOUTH    #NORTH                �                                                                                 &                                                       �                                                   H                             &                                                       �                                                   �                             &                                                       �                                                   �                             &                                                    @ @                                                                            &                                           #T_APPLY                       @                                        '�             0      #ZETA_WEST    #ZETAG_WEST    #ZETA_EAST    #ZETAG_EAST    #ZETA_SOUTH    #ZETAG_SOUTH    #ZETA_NORTH    #ZETAG_NORTH    #UBAR_WEST    #VBAR_WEST    #UBARG_WEST    #VBARG_WEST    #UBAR_EAST    #VBAR_EAST    #UBARG_EAST     #VBARG_EAST !   #UBAR_SOUTH "   #VBAR_SOUTH #   #UBARG_SOUTH $   #VBARG_SOUTH %   #UBAR_NORTH &   #VBAR_NORTH '   #UBARG_NORTH (   #VBARG_NORTH )   #U_WEST *   #V_WEST +   #UG_WEST ,   #VG_WEST -   #U_EAST .   #V_EAST /   #UG_EAST 0   #VG_EAST 1   #U_SOUTH 2   #V_SOUTH 3   #UG_SOUTH 4   #VG_SOUTH 5   #U_NORTH 6   #V_NORTH 7   #UG_NORTH 8   #VG_NORTH 9   #T_WEST :   #TG_WEST ;   #T_EAST <   #TG_EAST =   #T_SOUTH >   #TG_SOUTH ?   #T_NORTH @   #TG_NORTH A               �                                                                    
            &                                                       �                                                  H                 
            &                   &                                                       �                                                  �                 
            &                                                       �                                                  �                 
            &                   &                                                       �                                                  P                
            &                                                       �                                                  �                
            &                   &                                                       �                                                  �                
            &                                                       �                                                  @                
            &                   &                                                       �                                                  �             	   
            &                                                       �                                                  �             
   
            &                                                       �                                                  0                
            &                   &                                                       �                                                  �                
            &                   &                                                       �                                                  �                
            &                                                       �                                                  8                
            &                                                       �                                                   �                
            &                   &                                                       �                                      !            �                
            &                   &                                                       �                                      "            @                
            &                                                       �                                      #            �                
            &                                                       �                                      $            �                
            &                   &                                                       �                                      %            0                
            &                   &                                                       �                                      &            �                
            &                                                       �                                      '            �                
            &                                                       �                                      (                             
            &                   &                                                       �                                      )            �                
            &                   &                                                       �                                      *            �                
            &                   &                                                       �                                      +            @                
            &                   &                                                       �                                      ,            �                
            &                   &                   &                                                       �                                      -            	                
            &                   &                   &                                                       �                                      .            �	                
            &                   &                                                       �                                      /            �	                
            &                   &                                                       �                                      0            P
                
            &                   &                   &                                                       �                                      1            �
                 
            &                   &                   &                                                       �                                      2            @             !   
            &                   &                                                       �                                      3            �             "   
            &                   &                                                       �                                      4                          #   
            &                   &                   &                                                       �                                      5            x             $   
            &                   &                   &                                                       �                                      6            �             %   
            &                   &                                                       �                                      7            P             &   
            &                   &                                                       �                                      8            �             '   
            &                   &                   &                                                       �                                      9            (             (   
            &                   &                   &                                                       �                                      :            �             )   
            &                   &                   &                                                       �                                      ;                         *   
            &                   &                   &                   &                                                       �                                      <            �             +   
            &                   &                   &                                                       �                                      =                          ,   
            &                   &                   &                   &                                                       �                                      >            �             -   
            &                   &                   &                                                       �                                      ?            (             .   
            &                   &                   &                   &                                                       �                                      @            �             /   
            &                   &                   &                                                       �                                      A            0             0   
            &                   &                   &                   &                                                    @ @                                        B            �                       &                                           #T_BOUNDARY       �   t      fn#fn      H   J   MOD_KINDS    \  x       R8+MOD_KINDS "   �        ALLOCATE_BOUNDARY %   �  H   a   ALLOCATE_BOUNDARY%NG $            DEALLOCATE_BOUNDARY '   0  H   a   DEALLOCATE_BOUNDARY%NG $   x  %      INITIALIZE_BOUNDARY '   �  H   a   INITIALIZE_BOUNDARY%NG )   �  H   a   INITIALIZE_BOUNDARY%TILE *   -  H   a   INITIALIZE_BOUNDARY%MODEL    u  �       T_APPLY    �  �   a   T_APPLY%WEST    �  �   a   T_APPLY%EAST    /  �   a   T_APPLY%SOUTH    �  �   a   T_APPLY%NORTH    g	  �       LBC_APPLY    
        T_BOUNDARY %     �   a   T_BOUNDARY%ZETA_WEST &   �  �   a   T_BOUNDARY%ZETAG_WEST %   h  �   a   T_BOUNDARY%ZETA_EAST &     �   a   T_BOUNDARY%ZETAG_EAST &   �  �   a   T_BOUNDARY%ZETA_SOUTH '   T  �   a   T_BOUNDARY%ZETAG_SOUTH &     �   a   T_BOUNDARY%ZETA_NORTH '   �  �   a   T_BOUNDARY%ZETAG_NORTH %   X  �   a   T_BOUNDARY%UBAR_WEST %   �  �   a   T_BOUNDARY%VBAR_WEST &   �  �   a   T_BOUNDARY%UBARG_WEST &   D  �   a   T_BOUNDARY%VBARG_WEST %   �  �   a   T_BOUNDARY%UBAR_EAST %   �  �   a   T_BOUNDARY%VBAR_EAST &   0  �   a   T_BOUNDARY%UBARG_EAST &   �  �   a   T_BOUNDARY%VBARG_EAST &   �  �   a   T_BOUNDARY%UBAR_SOUTH &   4  �   a   T_BOUNDARY%VBAR_SOUTH '   �  �   a   T_BOUNDARY%UBARG_SOUTH '   �  �   a   T_BOUNDARY%VBARG_SOUTH &   8  �   a   T_BOUNDARY%UBAR_NORTH &   �  �   a   T_BOUNDARY%VBAR_NORTH '   p  �   a   T_BOUNDARY%UBARG_NORTH '   $  �   a   T_BOUNDARY%VBARG_NORTH "   �  �   a   T_BOUNDARY%U_WEST "   �  �   a   T_BOUNDARY%V_WEST #   @  �   a   T_BOUNDARY%UG_WEST #     �   a   T_BOUNDARY%VG_WEST "   �  �   a   T_BOUNDARY%U_EAST "   �   �   a   T_BOUNDARY%V_EAST #   @!  �   a   T_BOUNDARY%UG_EAST #   "  �   a   T_BOUNDARY%VG_EAST #   �"  �   a   T_BOUNDARY%U_SOUTH #   �#  �   a   T_BOUNDARY%V_SOUTH $   @$  �   a   T_BOUNDARY%UG_SOUTH $   %  �   a   T_BOUNDARY%VG_SOUTH #   �%  �   a   T_BOUNDARY%U_NORTH #   �&  �   a   T_BOUNDARY%V_NORTH $   @'  �   a   T_BOUNDARY%UG_NORTH $   (  �   a   T_BOUNDARY%VG_NORTH "   �(  �   a   T_BOUNDARY%T_WEST #   �)  �   a   T_BOUNDARY%TG_WEST "   �*  �   a   T_BOUNDARY%T_EAST #   T+  �   a   T_BOUNDARY%TG_EAST #   8,  �   a   T_BOUNDARY%T_SOUTH $   -  �   a   T_BOUNDARY%TG_SOUTH #   �-  �   a   T_BOUNDARY%T_NORTH $   �.  �   a   T_BOUNDARY%TG_NORTH    �/  �       BOUNDARY 