      SUBROUTINE VF_ZSETI1(IA,IVAL,N)

CD=== 概要 ===========================================================

CDT   VF_ZSETI1:整数の1次元配列に一定値を代入する 初始化整形1维数组，但不限于初始化一维数组，公用函数

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

CD    -- 引数 --
CD    IA(N) : OUT : I*4 : 1次元配列 需要初始化的数组
CD    IVAL  : IN  : I*4 : 一定値    初始值
CD    N     : IN  : I*4 : 配列IAのサイズ  维度的大小
      DIMENSION IA(N)  ! 声明实参

C==== 実行 ===========================================================

CD    -- 配列の全要素に代入 --
      DO 100 I=1,N
        IA(I)=IVAL
 100  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
