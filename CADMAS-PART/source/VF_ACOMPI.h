C-*- mode:fortran; -*-
      COMMON /VF_ACOMPI/ ICGTYP,ICGMAX,ICGITR,ISCMVP,ISCMFF,
     &                   ISCMK,ISCMT,ISCMC(MAXNC),IBSUW0

CD=== 概要 ===========================================================

CDT   VF_ACOMPI.h:数値解法関連(各種パラメータおよび反復回数等):整数 设定了一些控制计算格式的整形变量 COMpute Parameter Integer
C===== Numerical solution related (various parameters and number of iterations etc.): integer

C==== 内容 ===========================================================

CD    ICGTYP : CNS : I*4 : 連立1次方程式の解法の前処理の種類 Types of preprocessing for solving simultaneous linear equations
CD                         = 0:不完全LU分解(ILU)
CD                        !=0:修正不完全LU分解(MILU) --默认值
CD    ICGMAX : CNS : I*4 : 連立1次方程式の解法の最大反復回数 默认500
CD    ICGITR : TRN : I*4 : 連立1次方程式の解法の反復回数
CD    ISCMVP : CNS : I*4 : 流速の対流項の差分スキーム Difference scheme of flow velocity convection term
CD                         = 0:DONOR 默认值
CD                         !=0:????? CAKIY
CD    ISCMFF : CNS : I*4 : VOF関数Fの対流項の差分スキーム Difference scheme of convection term of VOF function F
CD                         = 0:ドナー・アクセプタ法 donor-acceptor method 默认值
CD                         !=0:界面の傾きを考慮した方法  Method considering the inclination of the interface
CD    ISCMK  : CNS : I*4 : k-εの移流項の差分スキーム  Difference scheme of advection term of k - ε
CD                         = 0:DONOR 默认值
CD                         !=0:?????
CD    ISCMT  : CNS : I*4 : 温度の移流項の差分スキーム Difference scheme of advection term of temperature
CD                         = 0:DONOR
CD                         !=0:?????
CD    ISCMC(MAXNC) : CNS : I*4 : 濃度の移流項の差分スキーム Difference scheme of advection term of concentration
CD                         = 0:DONOR
CD                         !=0:?????
CD    IBSUW0 : CNS : I*4 : 表面セルの流速計算法 Flow velocity calculation method of surface cell
CD                         =0 :流体側から補外できる場合は線形補外 linear extrapolation if extrapolable from fluid side 默认值，线性外插
CD                         !=0:勾配ゼロ Gradient zero
