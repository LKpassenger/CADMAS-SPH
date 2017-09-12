C-*- mode:fortran; -*-
      COMMON /VF_ATIMER/ TEND,TNOW,
     &                   DTNOW,DTCNST,DTINIT,DTMIN,DTMAX,DTSAFE

CD=== 概要 ===========================================================

CDT   VF_ATIMER.h:時間制御関連(解析時刻および時間刻み幅等):実数 时间步相关

C==== 内容 ===========================================================

CD    TEND   : CNS : R*8 : 解析終了時刻  终了时刻
CD    TNOW   : TRN : R*8 : 解析時刻     解析时刻
CD    DTNOW  : TRN : R*8 : 時間刻み幅   步长
CD    DTCNST : CNS : R*8 : 時間刻み幅の一定値 Constant value of time step width  默认值1.0D-3
CD    DTINIT : CNS : R*8 : 時間刻み幅の初期値 步长最初值  默认值1.0D-6
CD    DTMIN  : CNS : R*8 : 時間刻み幅の最小値 步长最小值
CD    DTMAX  : CNS : R*8 : 時間刻み幅の最大値 步长最大值
CD    DTSAFE : CNS : R*8 : 時間刻み幅の安全率 步长安全系数
