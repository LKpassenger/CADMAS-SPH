C-*- mode:fortran; -*-
      PARAMETER (ICPUIN=  0, ICPUST=  1, ICPUEN=  2, ICPUOU=  3,
     &           KCP0AL= 22,
     &           KCP1PR=  1, KCP1CL=  2,
     &           KCP2FL=  3, KCP2VL=  4, KCP2TT=  5, KCP2SS=  6,
     &           KCP2KE=  7, KCP2FF=  8,
     &           KCPVFL=  9, KCPVGN= 10, KCPVEL= 11, KCPVPC= 12,
     &           KCPVPS= 13, KCPVMD= 14,
     &           KCPFFL= 15, KCPFEL= 16, KCPFMD= 17, KCPFNF= 18,
     &           KCP9PL= 19, KCP9M1= 20, KCP9NF= 21             )
      COMMON /VF_ACPUTR/ CPUS(KCP0AL), CPUW(KCP0AL)

CD=== 概要 ===========================================================

CDT   VF_ACPUTR.h:CPU時間の計測関連(CPU時間等):パラメータと実数 CPU time measurement related (CPU time etc.): Parameter and real number
C     定义了一些CPU计时用的常量及变量，用于统计CPU计算时间
C==== 内容 ===========================================================

CD    ICPUIN : PRM : I*4 : タイマー処理フラグ(初期化) Timer processing flag (initialization)
CD    ICPUST : PRM : I*4 : タイマー処理フラグ(スタート) Timer processing flag (start)
CD    ICPUEN : PRM : I*4 : タイマー処理フラグ(止めて,合計をとる) Timer processing flag (stop, take a total)
CD    ICPUOU : PRM : I*4 : タイマー処理フラグ(CPU時間を出力する) Timer processing flag (CPU time output)
CD    KCP0AL : PRM : I*4 : タイマー種別フラグ(全体,種別フラグ数を兼ねる) Timer type flag (doubles as the total number flag)
CD    KCP1PR : PRM : I*4 : タイマー種別フラグ(前処理）Timer type flag (preprocessing)
CD    KCP1CL : PRM : I*4 : タイマー種別フラグ(計算部）Timer type flag (calculation unit)
CD    KCP2FL : PRM : I*4 : タイマー種別フラグ(計算部/ファイル入出力）Timer type flag (calculation unit / file input / output)
CD    KCP2VL : PRM : I*4 : タイマー種別フラグ(計算部/流速・圧力）Timer type flag (calculation unit / flow velocity / pressure)
CD    KCP2TT : PRM : I*4 : タイマー種別フラグ(計算部/温度）Timer type flag (calculation part / temperature)
CD    KCP2SS : PRM : I*4 : タイマー種別フラグ(計算部/スカラー）Timer type flag (calculation unit / scalar)
CD    KCP2KE : PRM : I*4 : タイマー種別フラグ(計算部/k-εモデル）Timer type flag (calculation unit / k - ε model)
CD    KCP2FF : PRM : I*4 : タイマー種別フラグ(計算部/VOF関数F）imer type flag (calculation unit / VOF function F)
CD    KCPVFL : PRM : I*4 : タイマー種別フラグ(流速・圧力関連）Timer type flag (flow rate / pressure related)
CD    KCPVGN : PRM : I*4 : ...
CD    KCPVEL : PRM : I*4 : ...
CD    KCPVPC : PRM : I*4 : ...
CD    KCPVPS : PRM : I*4 : ...
CD    KCPVMD : PRM : I*4 : ...
CD    KCPFFL : PRM : I*4 : タイマー種別フラグ(VOF関数F関連）Timer type flag (related to VOF function F)
CD    KCPFEL : PRM : I*4 : ...
CD    KCPFMD : PRM : I*4 : ...
CD    KCPFNF : PRM : I*4 : ...
CD    KCP9PL : PRM : I*4 : タイマー種別フラグ(ルーチン/VF_P****）Timer type flag (routine / VF_P ****) 运行VF_P部分
CD    KCP9M1 : PRM : I*4 : タイマー種別フラグ(ルーチン/VF_M1BCGS）Timer type flag (routine / VF_M1BCGS)
CD    KCP9NF : PRM : I*4 : タイマー種別フラグ(ルーチン/VF_FDROPF）Timer type flag (routine / VF_FDROPF)
CD    CPUS(KCP0AL) : TRN : R*8 : CPU時間の合計             Total CPU time，数组
CD    CPUW(KCP0AL) : TRN : R*8 : タイマーが起動された時間  Time when timer started 数组，用于记录计时起始时间
