      SUBROUTINE VF_ZXMG_ABORT(IERR)

CD=== 概要 ===========================================================

CDT   VF_ZXMG_ABORT:MPIマスク/異常終了する 

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE  'mpif.h'

CD    -- 引数 --
CD    IERR : OUT : I*4 : 完了コード

C==== 実行 ===========================================================

C     -- MPIルーチン --
      IERR2=77
      CALL MPI_ABORT(MPI_COMM_WORLD,IERR2,IERR) !调用MPI_ABORT()终止MPI执行环境

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
