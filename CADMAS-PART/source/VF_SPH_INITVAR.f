      SUBROUTINE VF_SPH_INITVAR()
C     用于初始化 SPH Coupling 相关的变量 init variables   add by LK
      USE mod_sph, ONLY: GREIS,GREIE,GREJS,GREJE,GREKS,GREKE,
     &                    BREIS,BREIE,BREJS,BREJE,BREKS,BREKE,
     &                    BRE,GRE,CNUMGRE,CNUMBRE,
     &                    CADM_PN,SPH_PN,LB_CADM,LB_SPH
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)
      GREIS=0
      GREIE=0
      GREJS=0
      GREJE=0
      GREKS=0
      GREKE=0
      BREIS=0
      BREIE=0
      BREJS=0
      BREJE=0
      BREKS=0
      BREKE=0
      CNUMGRE=0
      CNUMBRE=0
      BRE=0
      GRE=0
      CADM_PN=0
      SPH_PN=0
      LB_CADM=0
      LB_SPH=0
      
      END SUBROUTINE