      SUBROUTINE VF_ZXMP_GATHERI(ISEND,ISIZE,IRECV,NDIM,IERR)

CD=== 概要 ===========================================================

CDT   VF_ZXMP_GATHERI: 固定長の整数型データを0番PEに集める

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'mpif.h'
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    ISEND : IN  : R*8 : 送信するデータ
CD    IRECV : OUT : R*8 : データの最小値
CD    IERR  : OUT : I*4 : 完了コード

      INTEGER ISIZE
      INTEGER ISEND(ISIZE)
      INTEGER IRECV(NDIM)

C==== 実行 ===========================================================

C     -- MPIルーチン --
      CALL MPI_GATHER(ISEND,ISIZE,MPI_INTEGER,   ! 收集至各个分区局部标号为0的进程中
     &                IRECV,ISIZE,MPI_INTEGER,
     &                0,MGCOMM,IERR)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
