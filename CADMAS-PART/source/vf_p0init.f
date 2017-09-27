      SUBROUTINE VF_P0INIT()

CD=== 概要 ===========================================================

CDT   VF_P0INIT:並列環境を初期化する  初始化并行环境

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APARAR.h'

C==== 実行 ===========================================================

C     -- VF_APARAI.hを初期化する -- 初始化 VF_APARAI.h中的变量为0
      MGPROC=0   ! 初始化
      MGRANK=0
      MGCOMM=0
      MGARAN=0
      DO 100 I=1,MAXPRO  !MAXPRO=400 程序中限制的最大线程数
        MGNAME(I)=' '
 100  CONTINUE
      CALL VF_ZSETI1(MGNLEN,0,MAXPRO) ! 初始化为0
      CALL VF_ZSETI1(MGNPIN,0,MAXPRO)
      CALL VF_ZSETI1(MGPARE,0,MAXPRO)
      CALL VF_ZSETI1(MGAREA,0,MAXPRO)
      CALL VF_ZSETI1(MGSPH,0,MAXPRO) ! add by LK
      MGPRNK=0
      CALL VF_ZSETI1(MGPINF,0,9)
      MGCNUM=0
      CALL VF_ZSETI1(MGCRNK,0,  MAXPRO)
      CALL VF_ZSETI1(MGCINF,0,9*MAXPRO) ! 初始化为0
      CALL VF_ZSETI1(MGCPOS,0,6*MAXPRO)
      NPROCS=0
      NUMNPI=0
      NUMNPJ=0
      MYRANK=0
      MYRI  =0
      MYRJ  =0
      NUMI0 =0
      NUMJ0 =0
      MYIS  =0
      MYIE  =0
      MYJS  =0
      MYJE  =0
      MYMIS =0
      MYMIE =0
      MYMJS =0
      MYMJE =0
      MYGIS =0
      MYGIE =0
      MYGJS =0
      MYGJE =0
      NUMBUF=0
      CALL VF_ZSETI1(IPROCS,0,MAXNPI+1)
      CALL VF_ZSETI1(JPROCS,0,MAXNPJ+1)

C     -- VF_APARAR.hを初期化する -- 初始化 VF_APARAR.h中的变量为0.0
      GLXMIN=0.0D0
      GLXMAX=0.0D0
      GLYMIN=0.0D0
      GLYMAX=0.0D0

C     -- 初期化する --
      CALL VF_ZXMG_INIT(IERR)

C     -- プロセス数を得る --
      CALL VF_ZXMG_CSIZE(MGPROC,IERR)  ! Get number of processe,设定至VF_APARAI.h中的MGPROC

C     -- 自分のランクを得る --
      CALL VF_ZXMG_CRANK(MGRANK,IERR) ! Get rank of processe in communicator,设定至VF_APARAI.h中的MGRANK

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
