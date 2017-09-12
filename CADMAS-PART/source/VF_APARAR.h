C-*- mode:fortran; -*-
      COMMON /VF_APARAR/ GLXMIN,GLXMAX,GLYMIN,GLYMAX

CD=== 概要 ===========================================================

CDT   VF_APARAR.h:並列化関連:実数 Parallelization related: Real number
C     并行计算相关,X Y方向上网格坐标范围

C==== 内容 ===========================================================

CD    GLXMIN           : CNS : R*8 : 全体のx方向格子座標の最小値 Minimum value of the overall x direction lattice coordinate
CD    GLXMAX           : CNS : R*8 : 全体のx方向格子座標の最大値
CD    GLYMIN           : CNS : R*8 : 全体のy方向格子座標の最小値
CD    GLYMAX           : CNS : R*8 : 全体のy方向格子座標の最大値
