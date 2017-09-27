      MODULE mod_sph
        IMPLICIT NONE
        PRIVATE
        PUBLIC :: GREIS,GREIE,GREJS,GREJE,GREKS,GREKE
        PUBLIC :: BREIS,BREIE,BREJS,BREJE,BREKS,BREKE
        PUBLIC :: BRE,GRE
        PUBLIC :: CNUMGRE,CNUMBRE
        PUBLIC :: NB_SPH,CADM_PN,SPH_PN
        PUBLIC :: LB_CADM,IB_CADM,LB_SPH,IB_SPH
        PUBLIC :: DELETE_ARRAY_CWSPH


        INTEGER GREIS,GREIE,GREJS,GREJE,GREKS,GREKE
        INTEGER BREIS,BREIE,BREJS,BREJE,BREKS,BREKE
        INTEGER BRE,GRE
        INTEGER CNUMGRE,CNUMBRE
        INTEGER NB_SPH,CADM_PN,SPH_PN
        INTEGER LB_CADM,LB_SPH
        INTEGER,ALLOCATABLE :: IB_CADM(:),IB_SPH(:)
!!!!!!   GREIS, BREIS系列分别用于纪录ghost region 和 buffer region 的范围,目前只考虑在X方向上划分,Y和Z方向不划分
!!!!!!   BRE,GRE分别用于纪录当前计算区域是否包含ghost region 和  buffer region, 0：不包含   1 : 包含
!!!!!!   CNUMGRE 用于记录各个进程中包含的位于ghost region区域内的单元数，(不计入通讯层单元和dummy cell)
!!!!!!   CNUMBRE 用于纪录各个进程中包含的位于buffer region ...
!!!!!!   CNUMGRE 和 CNUMBRE 可以在各个进程中累加，检查数目是否正确

!!!!!!   NB_SPH  用以记录当前进程是否参与 和SPH模型的信息交换
!!!!!!   CADM_PN,SPH_PN 分别记录参与CADMAS&SPH信息交换的CADMAS进程数以及SPH进程数
!!!!!!   IB_CADM(),LB_CADM用于记录当前进程在comm_mg_isph通讯子中的进程标号(CADMAS 部分)
!!!!!!   IB_SPH(),LB_SPH用于记录当前进程在comm_mg_isph通讯子中的进程标号(SPH 部分)

      CONTAINS
        SUBROUTINE DELETE_ARRAY_CWSPH
!!!!!!  释放分配的数组空间 delete array used in coupling with SPH
          INTEGER IERR
          DEALLOCATE( IB_CADM,STAT=IERR )
          DEALLOCATE( IB_SPH,STAT=IERR )

        END SUBROUTINE

      END MODULE