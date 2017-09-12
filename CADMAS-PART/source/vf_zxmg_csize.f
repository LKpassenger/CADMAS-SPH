      SUBROUTINE VF_ZXMG_CSIZE(MGPROC,IERR)

CD=== 概要 ==========================================================
CDT   VF_ZXMG_CSIZE:MPIマスク/プロセス数を得る 返回在通讯子comm_model中的总进程数目

C==== 宣言 ===========================================================

C     -- 大域型 --
      use mod_comm,only: comm_model
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE  'mpif.h'

CD    -- 引数 --
CD    MGPROC : OUT : I*4 : プロセス数
CD    IERR   : OUT : I*4 : 完了コード

C==== 実行 ===========================================================

C     -- MPIルーチン --
      CALL MPI_COMM_SIZE(comm_model,MGPROC,IERR) !返回comm_model通讯子中的总进程数，设定至MGPROC

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
