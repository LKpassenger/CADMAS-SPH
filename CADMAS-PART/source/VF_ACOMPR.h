C-*- mode:fortran; -*-
      COMMON /VF_ACOMPR/ CGPARA,CGEPSA,CGEPSR,CGDIV ,CGBNRM,CGXNRM,
     &                   FEPS  ,FLOWER,FUPPER,FSUM  ,FCUT  ,PLOWER,
     &                   SCMVP ,SCMK  ,SCMT  ,SCMC(MAXNC)

CD=== 概要 ===========================================================

CDT   VF_ACOMPR.h:数値解法関連(各種パラメータおよび反復回数等):実数 设定了一些控制计算格式的实型变量 COMpute Parameter Real
C===== Numerical solution related (various parameters and number of iterations etc.): integer

C==== 内容 ===========================================================

CD    CGPARA : CNS : R*8 : 連立1次方程式の解法のMILU用パラメータ Parameters for MILU of solving simultaneous linear equations
CD    CGEPSA : CNS : R*8 : 連立1次方程式の解法の収束判定値(絶対誤差)
CD    CGEPSR : CNS : R*8 : 連立1次方程式の解法の収束判定値(相対誤差)
CD    CGDIV  : CNS : R*8 : 連立1次方程式の解法の発散判定値 Divergence judgment value of solving method of simultaneous linear equation
CD    CGBNRM : TRN : R*8 : 連立1次方程式の解法の右辺のノルム norm on the right side of simultaneous linear equation solving
CD    CGXNRM : TRN : R*8 : 連立1次方程式の解法の残差のノルム norm of residuals of solving simultaneous linear equations
CD    FEPS   : CNS : R*8 : VOF関数Fのゼロ判定値 Zero judgment value of VOF function F 默认值1.0D-6
CD    FLOWER : CNS : R*8 : VOF関数Fの下限値  lower limit of function F 1.0D-6
CD    FUPPER : CNS : R*8 : VOF関数Fの上限値  upper limit of function F 1.-1.0D-6
CD    FSUM   : TRN : R*8 : VOF関数Fの空間積分値 Spatial integral value of VOF function F
CD    FCUT   : TRN : R*8 : VOF関数Fのカットオフ値 Cutoff value of VOF function F
CD    PLOWER : CNS : R*8 : ポーラス値の下限値 Lower limit of the porous value 1.0D-4
CD    SCMVP  : CNS : R*8 : DONORスキームのパラメータ(流速用) Parameter of DONOR scheme (for flow rate)
CD                         =0.0:2次中心
CD                         =1.0:1次風上  默认
CD    SCMK   : CNS : R*8 : DONORスキームのパラメータ(k-ε用) Parameter of DONOR scheme (for k - ε))
CD                         =0.0:2次中心
CD                         =1.0:1次風上  默认
CD    SCMT   : CNS : R*8 : DONORスキームのパラメータ(温度用) Parameter of DONOR scheme (for temperature)
CD                         =0.0:2次中心
CD                         =1.0:1次風上
CD    SCMC(MAXNC) : CNS : R*8 : DONORスキームのパラメータ(濃度用) DONOR scheme parameters (for concentration)
CD                         =0.0:2次中心
CD                         =1.0:1次風上
