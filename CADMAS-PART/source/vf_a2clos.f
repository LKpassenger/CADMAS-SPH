      SUBROUTINE VF_A2CLOS()

CD=== 概要 ===========================================================

CDT   VF_A2CLOS:ファイルをクローズする 用于在各种异常或运行结束时关闭文件

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'

C==== 実行 ===========================================================

CD    -- オープンされているファイルをクローズする -- close各种文件
      IF (IENFIL.NE.0) CLOSE(IENFIL)  ! 对文件的使用规则是如果I/O号为0，则表示没有打开使用，如果文件被打开使用，则会赋值一个非0的I/O号
      IF (IINFIL.NE.0) CLOSE(IINFIL)
      IF (IMTFIL.NE.0) CLOSE(IMTFIL)
      IF (IMTFIL2.NE.0) CLOSE(IMTFIL2)
      IF (IREFIL.NE.0) CLOSE(IREFIL)
      IF (IPRFIL.NE.0) CLOSE(IPRFIL)
      IF (ILPFIL.NE.0) CLOSE(ILPFIL)
      IF (IGRFIL.NE.0) CLOSE(IGRFIL)
      IF (IRSFIL.NE.0) CLOSE(IRSFIL)
      IF (ITRFIL.NE.0) CLOSE(ITRFIL)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
