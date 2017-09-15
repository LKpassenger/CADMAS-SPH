      MODULE mod_sph
        IMPLICIT NONE
        PRIVATE
        PUBLIC :: GREIS,GREIE,GREJS,GREJE,GREKS,GREKE
        PUBLIC :: BREIS,BREIE,BREJS,BREJE,BREKS,BREKE
        PUBLIC :: BRE,GRE
        PUBLIC :: CNUMGRE,CNUMBRE


        INTEGER GREIS,GREIE,GREJS,GREJE,GREKS,GREKE
        INTEGER BREIS,BREIE,BREJS,BREJE,BREKS,BREKE
        INTEGER BRE,GRE
        INTEGER CNUMGRE,CNUMBRE
!!!!!!   GREIS, BREIS系列分别用于纪录ghost region 和 buffer region 的范围,目前只考虑在X方向上划分,Y和Z方向不划分
!!!!!!   BRE,GRE分别用于纪录当前计算区域是否包含ghost region 和  buffer region, 0：不包含   1 : 包含
!!!!!!   CNUMGRE 用于记录各个进程中包含的位于ghost region区域内的单元数，(不计入通讯层单元和dummy cell)
!!!!!!   CNUMBRE 用于纪录各个进程中包含的位于buffer region ...
!!!!!!   CNUMGRE 和 CNUMBRE 可以在各个进程中累加，检查数目是否正确









      END MODULE 