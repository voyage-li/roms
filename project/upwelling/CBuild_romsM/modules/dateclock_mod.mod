    K   k820309    �          2021.11.1   ���f                                                                                                          
       /staff/liyuanhang/work/2024OCT/roms_8/roms/project/upwelling/CBuild_romsM/f90/dateclock.f90 DATECLOCK_MOD          @       �                                          
                                                                                                                      #         @                                                                #CURRENTTIME    #YY_I    #YD_I    #MM_I    #DD_I    #H_I 	   #M_I 
   #S_I    #YD_DP    #DD_DP    #H_DP    #M_DP    #S_DP                              
                                               
                F @                                                              F @                                                              F @                                                              F @                                                              F @                                        	                      F @                                        
                      F @                                                              F @                                            
                 F @                                            
                 F @                                            
                 F @                                            
                 F @                                            
       #         @                                                                #DATENUMBER    #YEAR    #MONTH    #DAY    #HOUR    #MINUTES    #SECONDS              D                                                            
     p          p            p                                    
                                                                
                                                                
                                                                
 @                                                             
 @                                                             
 @                                            
      #         @                                                                #DATENUMBER    #ISDAYUNITS    #DATESTRING              
  @                                            
                
  @                                                             D                                                             1 #         @                                                             
   #DATENUMBER    #ISDAYUNITS    #YEAR     #MONTH !   #DAY "   #HOUR #   #MINUTES $   #SECONDS %   #F_MINUTES &   #F_HOUR '             
  @                                            
                
                                                                D                                                                 D                                          !                      D                                          "                      D                                          #                      D                                          $                      D @                                       %     
                 D @                                       &     
                 D @                                       '     
       #         @                                            (                    #MONTH )   #DAY *   #YEAR +   #CODE ,             
                                           )                     
                                           *                     
                                           +                     D                                          ,            #         @                                            -                    #DATE_STR .             D @                                      .                     1 #         @                                            /                    #R_TIME 0                               
  @                                       0     
      #         @                                            1                    #YEAR 2   #MONTH 3   #DAY 4   #HOUR 5   #MINUTES 6   #SECONDS 7   #CLOCKTIME 8                                
  @                                        2                     
  @                                        3                     
  @                                        4                     
  @                                        5                     
  @                                        6                     
  @                                       7     
                D                                         8     
       #         @                                            9                    #MYTIME :   #DATE_STRING ;             
                                          :     
                D                                         ;                            #         @                                            <                    #MYTIME =   #DATE_STRING >             
                                          =     
                D                                         >                            #         @                                            ?                    #USTRING @   #YEAR A   #MONTH B   #DAY C   #HOUR D   #MINUTES E   #SECONDS F             
  @                                      @                    1           D                                          A                      D                                          B                      D                                          C                      D                                          D                      D                                          E                      D                                         F     
       %         @                                         G                           #YEAR H   #MONTH I   #DAY J             
  @                                        H                     
                                           I                     
                                           J              �   r      fn#fn      H   J   MOD_KINDS    Z  x       DP+MOD_KINDS    �  �       CALDATE $   �  H   a   CALDATE%CURRENTTIME      H   a   CALDATE%YY_I    J  H   a   CALDATE%YD_I    �  H   a   CALDATE%MM_I    �  H   a   CALDATE%DD_I    "  H   a   CALDATE%H_I    j  H   a   CALDATE%M_I    �  H   a   CALDATE%S_I    �  H   a   CALDATE%YD_DP    B  H   a   CALDATE%DD_DP    �  H   a   CALDATE%H_DP    �  H   a   CALDATE%M_DP      H   a   CALDATE%S_DP    b  �       DATENUM #     �   a   DATENUM%DATENUMBER    �  H   a   DATENUM%YEAR    �  H   a   DATENUM%MONTH    0  H   a   DATENUM%DAY    x  H   a   DATENUM%HOUR     �  H   a   DATENUM%MINUTES     	  H   a   DATENUM%SECONDS    P	  �       DATESTR #   �	  H   a   DATESTR%DATENUMBER #   
  H   a   DATESTR%ISDAYUNITS #   `
  T   a   DATESTR%DATESTRING    �
  �       DATEVEC #   �  H   a   DATEVEC%DATENUMBER #   �  H   a   DATEVEC%ISDAYUNITS      H   a   DATEVEC%YEAR    Y  H   a   DATEVEC%MONTH    �  H   a   DATEVEC%DAY    �  H   a   DATEVEC%HOUR     1  H   a   DATEVEC%MINUTES     y  H   a   DATEVEC%SECONDS "   �  H   a   DATEVEC%F_MINUTES    	  H   a   DATEVEC%F_HOUR    Q  x       DAY_CODE    �  H   a   DAY_CODE%MONTH      H   a   DAY_CODE%DAY    Y  H   a   DAY_CODE%YEAR    �  H   a   DAY_CODE%CODE    �  ^       GET_DATE "   G  T   a   GET_DATE%DATE_STR    �  n       REF_CLOCK !   	  H   a   REF_CLOCK%R_TIME    Q  �       ROMS_CLOCK       H   a   ROMS_CLOCK%YEAR !   M  H   a   ROMS_CLOCK%MONTH    �  H   a   ROMS_CLOCK%DAY     �  H   a   ROMS_CLOCK%HOUR #   %  H   a   ROMS_CLOCK%MINUTES #   m  H   a   ROMS_CLOCK%SECONDS %   �  H   a   ROMS_CLOCK%CLOCKTIME    �  m       TIME_ISO8601 $   j  H   a   TIME_ISO8601%MYTIME )   �  X   a   TIME_ISO8601%DATE_STRING    
  m       TIME_STRING #   w  H   a   TIME_STRING%MYTIME (   �  X   a   TIME_STRING%DATE_STRING      �       TIME_UNITS #   �  T   a   TIME_UNITS%USTRING     
  H   a   TIME_UNITS%YEAR !   R  H   a   TIME_UNITS%MONTH    �  H   a   TIME_UNITS%DAY     �  H   a   TIME_UNITS%HOUR #   *  H   a   TIME_UNITS%MINUTES #   r  H   a   TIME_UNITS%SECONDS    �  v       YEARDAY    0  H   a   YEARDAY%YEAR    x  H   a   YEARDAY%MONTH    �  H   a   YEARDAY%DAY 