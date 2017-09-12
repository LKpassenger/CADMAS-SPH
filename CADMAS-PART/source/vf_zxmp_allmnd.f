      SUBROUTINE VF_ZXMP_ALLMND(DSEND,DRECV,IERR)

CD=== 概要 ===========================================================

CDT   VF_ZXMP_ALLMND:MPIマスク/データの最小値を求める/実数 Find the minimum value of MPI mask / data / real number

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'mpif.h'
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    DSEND : IN  : R*8 : 送信するデータ 发送地址
CD    DRECV : OUT : R*8 : データの最小値 接收地址
CD    IERR  : OUT : I*4 : 完了コード      

C==== 実行 ===========================================================

C     -- MPIルーチン --
      CALL MPI_ALLREDUCE(DSEND,DRECV,1,MPI_DOUBLE_PRECISION,MPI_MIN,   ! 利用全局归约计算最小值
     &                   MGCOMM,IERR)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
