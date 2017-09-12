      SUBROUTINE VF_PMGINP()

CD=== 概要 ===========================================================

CDT   VF_PMGINP:マルチグリッド環境ファイルを読み込む          读入env文件,并对进程进行划分，指定哪些负责父分区，哪些负责子分区
C                                                             针对Coupling 模型，考虑从data.env文件中读入新指标，用于指定哪些分区参与 与SPH的coupling
C                                                             哪些不参与，并设定至每个进程

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_ASTOCI.h'

C==== 実行 ===========================================================

CD    -- マルチグリッド環境ファイルの読み込み --
      IENFIL=0  

      IF (MGRANK.EQ.0) THEN  !只在MGRANK=0的线程中执行
        WRITE(*,9510)
        OPEN(MFILEN,ERR=9010,FILE='data.env',
     &         STATUS='OLD',FORM='FORMATTED' ) !环境文件data.env i/o号为10，既定存在，格式化文件,文件名必须为data.env
        IENFIL=MFILEN !设定COMMON变量
        READ(IENFIL,*,END=9020,ERR=9020) MGARAN !读入.env第一行的分区的个数
        IF (MGARAN.GT.MAXPRO) GOTO 9030 ! MAXPRO=400
        IF (MGARAN.LE.0     ) GOTO 9040
        N=0
        DO 120 I=1,MGARAN
          READ(IENFIL,*,END=9020,ERR=9020) MGNAME(I),MGNPIN(I),MGPARE(I) !依次读入每个分区的名称、分配给该区的进程数、父分区的编号
          MGNLEN(I)=0
          DO 100 L=MAXCHR,1,-1  ! 
            IF (MGNAME(I)(L:L).NE.' ') THEN
              MGNLEN(I)=L !设定每个分区名字的长度
              GOTO 110
            ENDIF
 100      CONTINUE
 110      CONTINUE
          IF (MGNLEN(I).LE.0     ) GOTO 9040   !检查是否有不适当的名称存在
          IF (MGNPIN(I).LE.0     ) GOTO 9040   !...是否有不适当的进程数
CCC IC-MG COUPLING    IF (MGPARE(I).LT.0    ) GOTO 9040
          IF (MGPARE(I).GT.MGARAN) GOTO 9040    !...是否有不适当的父分区编号
          IF (MGPARE(I).EQ.I     ) GOTO 9040
          N=N+MGNPIN(I) !统计.env文件中各个分区的总进程数
 120    CONTINUE

        CLOSE(IENFIL)  !完成.env文件读入
        IENFIL=0  !将IENFIL重置为0表示该文件未被使用打开
        IF (N.GT.MAXPRO) GOTO 9030
        IF (N.NE.MGPROC) GOTO 9050  ! 与MPI配置进行一致性检验
      ENDIF
C----单一进程执行部分结束-------------------------------
C----以下在每个进程中都执行-----------------------------

CD    -- 環境データのパッシング --Passing environmental data由于前文只是在一个进程中读入.env文件，所以要将读入的数据通讯至别的进程中
      CALL VF_P0BCSI(MGARAN,     1,0) 
      CALL VF_P0BCSI(MGNLEN,MGARAN,0)
      CALL VF_P0BCSI(MGNPIN,MGARAN,0)
      CALL VF_P0BCSI(MGPARE,MGARAN,0)
      DO 200 I=1,MGARAN
        CALL VF_P0BCSC(MGNAME(I),MGNLEN(I),0)
 200  CONTINUE

CD    -- 親子関係のループチェック -- 检查分区间父-子关系，从0 1 2 3 4.....这样排列不会有问题
      DO 320 I=1,MGARAN
        N=MGPARE(I)
cmod 20131023        IF (N.LT.0) NB_SC = -N
cmod 20160527        IF (N.LT.0.AND.N.GT.-10) NB_SC = -N
        IF (N.GT.0) THEN
          DO 300 L=1,MGARAN-1
            N=MGPARE(N)
            IF (N.LE.0) GOTO 310
 300      CONTINUE
 310      CONTINUE
          IF (N.GT.0) GOTO 9040
        ENDIF
 320  CONTINUE

CD    -- 領域名の重複チェック -- 检查分区的名字是否有重复
      DO 410 I=1,MGARAN-1
        DO 400 L=I+1,MGARAN
          IF (MGNAME(I).EQ.MGNAME(L)) GOTO 9040
 400    CONTINUE
 410  CONTINUE

CD    -- 領域毎のデータをプロセス毎に展開する -- 
      N=MGPROC
      NPROCS=0
      MYRANK=-1
      DO 510 I=MGARAN,1,-1
        DO 500 L=1,MGNPIN(I) ! MGNPIN(I)指分配给某个分区的线程数
          MGAREA(N)=I ! 为每个进程设置信息
          MGNAME(N)=MGNAME(I) !当前进程计算的分区名称
          MGNLEN(N)=MGNLEN(I)
          MGNPIN(N)=MGNPIN(I) 
          MGPARE(N)=MGPARE(I) !父分区的编号
          IF (N.EQ.MGRANK+1) THEN
            NPROCS=MGNPIN(N)  !设定每一个进程的NPROCS与MYRANK
            MYRANK=NPROCS-L
          ENDIF
          N=N-1
 500    CONTINUE
 510  CONTINUE
      IF (NPROCS.LE.0) GOTO 9060
      IF (MYRANK.LT.0) GOTO 9060
cadd 20160527
c      write(100+mgrank,81)
c   81 format('mgarea  mgname  mgpare  mgnpin  myrank/nprocs')
c      do n=1,mgproc
c         if(n==1) then
c            write(100+mgrank,'(i6,2x,a6,2x,i6,2x,i6,2x,i6,a1,i2)') 
c     $         mgarea(n),mgname(n)(1:mgnlen(n)),mgpare(n),mgnpin(n),
c     $         myrank,'/',nprocs
c         else
c            write(100+mgrank,'(i6,2x,a6,2x,i6,2x,i6)') 
c     $         mgarea(n),mgname(n)(1:mgnlen(n)),mgpare(n),mgnpin(n)
c         endif
c      enddo
c      write(100+mgrank,*) ''
      IF(MGPARE(MGRANK+1).LT.0.AND.MGPARE(MGRANK+1).GT.-10)  ! 通过这里，将CADMAS中有些进程的NB_SC设置为非0
     $   NB_SC = -MGPARE(MGRANK+1)
C
CD    -- 領域毎のコミュニケータ作成 -- Create a communicator for each area
      MGCOMM=0
      CALL VF_ZXMG_SPLIT(MGAREA(MGRANK+1),MYRANK,MGCOMM,IERR)  ! 属于相同分区的线程被分配到了一组，形成新通讯子MGCOMM
      CALL VF_ZXMP_CSIZE(NP,IERR) ! 返回当前进程在MGCOMM中对应的通讯子进程SIZE
      CALL VF_ZXMP_CRANK(MY,IERR) ! 返回当前进程在MGCOMM中的标号MY
      IF (NPROCS.NE.NP) GOTO 9060 ! 相当于CHEAK
      IF (MYRANK.NE.MY) GOTO 9060

C     -- 実行文の終了 --
      GOTO 9999

C==== ファイル関連エラー処理 =========================================error信息处理

 9010 CONTINUE
      WRITE(*,9520) 'VF_PMGINP','CAN NOT OPEN (data.env).'
      WRITE(*,9530)
      CALL VF_A2CLOS()
      CALL VF_P0ENDA()
      STOP
      GOTO 9999

 9020 CONTINUE
      WRITE(*,9520) 'VF_PMGINP','I/O ERROR.'
      WRITE(*,9530)
      CALL VF_A2CLOS()
      CALL VF_P0ENDA()
      STOP
      GOTO 9999

 9030 CONTINUE
      WRITE(*,9520) 'VF_PMGINP','AREA IS FULL.'
      WRITE(*,9530)
      CALL VF_A2CLOS()
      CALL VF_P0ENDA()
      STOP
      GOTO 9999

 9040 CONTINUE
      WRITE(*,9520) 'VF_PMGINP','INVALID VALUE.'
      WRITE(*,9530)
      CALL VF_A2CLOS()
      CALL VF_P0ENDA()
      STOP
      GOTO 9999

 9050 CONTINUE
      WRITE(*,9520) 'VF_PMGINP','MGPROC <> SUM(PE).'
      WRITE(*,9530)
      CALL VF_A2CLOS()
      CALL VF_P0ENDA()
      STOP
      GOTO 9999

 9060 CONTINUE
      WRITE(*,9520) 'VF_PMGINP','P.G ERROR.'
      WRITE(*,9530)
      CALL VF_A2CLOS()
      CALL VF_P0ENDA()
      STOP
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT(/' ','>> FILE-ENV : IN : ALL')
 9520 FORMAT(/' ','>>>>> ERROR. [',A,'] : ',A)
 9530 FORMAT(/' ','##### ABNORMAL END. #########################'/)

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
