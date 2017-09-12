      SUBROUTINE VF_P0BCSI(IBUF,N,IROOT)

CD=== 概要 ===========================================================

CDT   VF_P0BCSI:irootからその他へ送信する/整数 利用MPI_BCAST()进行进程间消息通讯（有些
C                                              操作是在单一进程中进行的，需要将消息共享至其他进程）

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACPUTR.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    IBUF(N) : I/O : I*4 : 転送するデータ
CD    N       : IN  : I*4 : データの要素数
CD    IROOT   : IN  : I*4 : 送信元のランク
      DIMENSION IBUF(N)

C==== 実行 ===========================================================

      CALL VF_A2CPUT(0,ICPUST,KCP9PL) !记录这段运行所用时间

C     -- irootからその他へ送信する -- 
      IF (MGPROC.NE.1) CALL VF_ZXMG_BCASTI(IBUF,N,IROOT,IERR)

C     -- 実行文の終了 --
      CALL VF_A2CPUT(0,ICPUEN,KCP9PL)
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
