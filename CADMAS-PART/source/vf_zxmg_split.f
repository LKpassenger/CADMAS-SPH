      SUBROUTINE VF_ZXMG_SPLIT(ICOL,IKEY,ICOMM,IERR)

CD=== 概要 ===========================================================

CDT   VF_ZXMG_SPLIT:MPIマスク/COMM_WORLDを分割する

C==== 宣言 ===========================================================

C     -- 大域型 --
      use mod_comm,only: comm_model
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'mpif.h'

CD    -- 引数 --
CD    ICOL  : IN  : I*4 : カラー  color值
CD    IKEY  : IN  : I*4 : キー    
CD    ICOMM : OUT : I*4 : コミュニケータ 新分割的communicator
CD    IERR  : OUT : I*4 : 完了コード

C==== 実行 ===========================================================

C     -- MPIルーチン --
      CALL MPI_COMM_SPLIT(comm_model,ICOL,IKEY,ICOMM,IERR)  !对comm_model这个通讯子根据所属分区进行分割，形成互不相交的多个子集，记录在ICOMM中

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
