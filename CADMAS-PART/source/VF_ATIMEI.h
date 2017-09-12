C-*- mode:fortran; -*-
      COMMON /VF_ATIMEI/ NEND,NNOW,IDTTYP,LOOPS

CD=== 概要 ===========================================================

CDT   VF_ATIMEI.h:時間制御関連(解析時刻および時間刻み幅等):整数 Time control related (analysis time and time step width etc)
C     时间步相关

C==== 内容 ===========================================================

CD    NEND   : CNS : I*4 : 解析終了ステップ End of analysis step
CD    NNOW   : TRN : I*4 : 解析ステップ  analysis step
CD    IDTTYP : CNS : I*4 : 時間刻み幅の計算方法 Calculation method of time step width
CD                         = 0:一定 constant
CD                         !=0:自動 
CD    LOOPS  : CNS : I*4 : 流速・圧力計算のサブループ回数 Number of sub-loops for flow velocity / pressure calculation
CD                         =1 :通常計算 
CD                         >1 :サブループ有り With sub-loop
