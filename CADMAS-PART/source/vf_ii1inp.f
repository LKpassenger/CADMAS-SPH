      SUBROUTINE VF_II1INP(XX,YY,ZZ,CM0,CD0,GGV,GGX,GGY,GGZ,
C----------------------------------------------------------2011.04 start
     &                     XPF,YPF,ZPF,IPF,JPF,KPF,
C----------------------------------------------------------2011.04 end
     &                     BCU,BCV,BCW,BCP,BCF,BCVI,
     &                     BCK,BCE,BCT,BCTI,BCC,BCCI,
     &                     NF,INDX,INDY,INDZ,INDB,
     &                     INDBK,INDBE,INDBT,INDBC,
     &                     DBUF,IBUF,XXWK,YYWK,ZZWK)

CD=== 概要 ===========================================================

CDT   VF_II1INP:入力ファイルを読み込む
CD      (1)1行の最大文字数はMAXCHR
CD      (2)1行の最大単語数はMAXWDS
CD      (3)区切りは1つ以上の空白   关键词之间的分解符是空格
CD      (4)空白行はスキップ
CD      (5)「#」以降はコメント     每一行第一个非空格字符为#,则改行表示注释
CD      (6)文法上有効な最後の単語以降は無視
CD      (7)ダブ等の特殊文字は判定していない(注意)

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --！  注意这些是形参
CD    XX(MAXG1,NUMI)   : I/O : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : I/O : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : I/O : R*8 : z方向格子座標等
CD    CM0(@FOR-3D@)    : I/O : R*8 : 慣性力係数
CD    CD0(@FOR-3D@)    : I/O : R*8 : 抵抗係数
CD    GGV(@FOR-3D@)    : I/O : R*8 : 空隙率
CD    GGX(@FOR-3D@)    : I/O : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)    : I/O : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)    : I/O : R*8 : z方向面積透過率
C----------------------------------------------------------2011.04 start
CD    XPF(NUMI)        : OUT : R*8   :x方向の親格子に対する補間係数
CD    YPF(NUMJ)        : OUT : R*8   :y方向の親格子に対する補間係数
CD    ZPF(NUMK)        : OUT : R*8   :z方向の親格子に対する補間係数
CD    IPF(MGPINF(1))   : OUT : I*4   :x方向の親格子1に対する格子の数
CD    JPF(MGPINF(2))   : OUT : I*4   :y方向の親格子1に対する格子の数
CD    KPF(MGPINF(3))   : OUT : I*4   :z方向の親格子1に対する格子の数
C----------------------------------------------------------2011.04 end
CD    BCU(NUMB)        : I/O : R*8 : x方向流速の境界値
CD    BCV(NUMB)        : I/O : R*8 : y方向流速の境界値
CD    BCW(NUMB)        : I/O : R*8 : z方向流速の境界値
CD    BCP(NUMB)        : I/O : R*8 : 圧力の境界値
CD    BCF(NUMB)        : I/O : R*8 : VOF関数Fの境界値
CD    BCVI(NUMB)       : I/O : R*8 : 流速の境界条件(壁面の粗さ)
CD    BCK(NUMB)        : I/O : R*8 : 乱流エネルギの境界値
CD    BCE(NUMB)        : I/O : R*8 : 乱流エネルギ散逸の境界値
CD    BCT(NUMB)        : I/O : R*8 : 温度の境界値
CD    BCTI(2,NUMB)     : I/O : R*8 : 温度の境界条件
CD    BCC(NUMB,LEQC)    : I/O : R*8 : 濃度の境界値
CD    BCCI(2,NUMB,LEQC) : I/O : R*8 : 濃度の境界条件
CD    NF(@FOR-3D@)     : I/O : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : I/O : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : I/O : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)   : I/O : I*4 : z面の状態を示すインデックス
CD    INDB(MAXB1,NUMB) : I/O : I*4 : 境界面のインデックス
CD    INDBK(NUMB)      : I/O : I*4 : 乱流エネルギの境界条件
CD    INDBE(NUMB)      : I/O : I*4 : 乱流エネルギ散逸の境界条件
CD    INDBT(NUMB)      : I/O : I*4 : 温度の境界条件
CD    INDBC(NUMB,LEQC) : I/O : I*4 : 濃度の境界条件
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    IBUF(NUMBUF*MAXBUF) : OUT :I*4 : 並列用のバッファ
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)  ! VF_ANUMBI.h中的变量在VF_A2DFLI()中被初始化为0
      DIMENSION CM0 (NUMI,NUMJ,NUMK),CD0 (NUMI,NUMJ,NUMK)
      DIMENSION GGV (NUMI,NUMJ,NUMK),GGX (NUMI,NUMJ,NUMK)
      DIMENSION GGY (NUMI,NUMJ,NUMK),GGZ (NUMI,NUMJ,NUMK)
C----------------------------------------------------------2011.04 start
      DIMENSION XPF(NUMI),YPF(NUMJ),ZPF(NUMK)
      DIMENSION IPF(0:MGPINF(1)),JPF(0:MGPINF(2)),KPF(0:MGPINF(3))
C----------------------------------------------------------2011.04 end
      DIMENSION BCU(NUMB),BCV(NUMB),BCW(NUMB),BCP(NUMB),BCF(NUMB)
      DIMENSION BCVI(NUMB),BCK(NUMB),BCE(NUMB),BCT(NUMB),BCTI(2,NUMB)
      DIMENSION BCC(NUMB,LEQC),BCCI(2,NUMB,LEQC)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION INDB(MAXB1,NUMB),INDBK(NUMB),INDBE(NUMB)
      DIMENSION INDBT(NUMB),INDBC(NUMB,LEQC)
      DIMENSION DBUF(NUMBUF*MAXBUF),IBUF(NUMBUF*MAXBUF)
      DIMENSION XXWK(NUMI0),YYWK(NUMJ0),ZZWK(NUMK)

CD    -- 局所変数 --
CD    TEXT       : C*(MAXCHR) : 入力した文字列
CD    IS(MAXWDS) : I*4        : n番目の単語の開始位置
CD    IE(MAXWDS) : I*4        : n番目の単語の終了位置
      CHARACTER*5 TEXTP
      CHARACTER*(MAXCHR) TEXT  ! MAXCHR=256
      DIMENSION IS(MAXWDS),IE(MAXWDS) ! MAXWDS=128
      INTEGER MODEOBS ! 地形データ(OBST)を外部ファイルから読み込む(=1)か否か(=0) 是否从外部文件（非.in文件）读入地形数据（obstacle部分） ，否=0,是=1
      DATA MODEOBS /0/
      INTEGER MODEPRS ! 地形データ(POROUS)を外部ファイルから読み込む(=1)か否か(=0) 是否从外部文件（非.in文件）读入地形数据（Porous部分） ，否=0
      DATA MODEPRS /0/
      INTEGER MODEORS ! 各PEの担当領域の地形データの入出力を行うか(=1)か否か(=0) Whether or not input / output of terrain data of the responsible area of each PE
      DATA MODEORS /0/

C==== 実行 ===========================================================

CD    -- 読み込みレベルの設定 --
C     * LEVEL=<0:格子数を読み込む(格子数を決定する)  LEVEL<=0,只从文件中获得网格节点数目
C     * LEVEL= 1:格子座標と障害物データを読み込む(境界面数を決定する) LEVEL=1 读入有关坐标信息
C     * LEVEL>=2:その他のデータを読み込む                             LEVEL=2 读入其他信息
      LEVEL=0  !  
      IF (NUMI0.GT.0) LEVEL=1 !NUMI0在VF_POINIT中初始化为0，当以LEVEL=0模式运行VF_II1INP()后会GET到节点数量后，NUMI0会被设定为非零，之后LEVEL被设定
      IF (NUMBX.GT.0) LEVEL=2 !NUMBX在VF_A2DFLT中初始化为0,第二次调用后NUMBX不再为0
      WRITE(ILPFIL,9510) '## INPUT-LEVEL=',LEVEL

CD    -- NFにデフォルト値を設定 -- 设定NF的初始值，但只针对dummy cell的NF值
      IF (LEVEL.EQ.1) CALL VF_CNFDFL(NF)  

CD    -- 境界面のインデックスを設定 -- LEVEL=2时才执行，设定INDB()
      IF (LEVEL.EQ.2) CALL VF_CINDB(NF,INDX,INDY,INDZ,INDB)

CD    -- ローカルな格子数のクリア --局部变量
      NI=0
      NJ=0
      NK=0

CD    -- 入力ファイルのオープン --
      IEOF  =0
      IINFIL=0
      IF (MYRANK.EQ.0.OR.INMODE.EQ.1) THEN  ! INMODE作为常量定义为0，故每个父-子分区只在MYRANK=0进程中读入
        OPEN(MFILIN,ERR=9010,
     &       FILE=MGNAME(MGRANK+1)(1:MGNLEN(MGRANK+1))//'.in',  ! 打开对应的.in文件，I/O号11
     &       STATUS='OLD',FORM='FORMATTED'  )
c        IINFIL=MFILIN
      ENDIF
      IINFIL=MFILIN  

CD    -- ファイルの読み込み --
C     ** 前判定反復:入力ファイルが終了するまで **
 100  CONTINUE
CCC        IF (IEOF.NE.0) GOTO 5010
        IF (IEOF.NE.0) THEN  !!  到达文件结尾
C         *** OBST外部入力の場合のファイル切替処理 ***
          IF (LEVEL.EQ.1.AND.MODEOBS.EQ.1 ) THEN  
            IF(IINFIL.EQ.MFILIN) THEN ! 1回目のEOFでファイルを切り替え
              IINFIL=MFILOB
              IEOF=0
            ELSEIF(IINFIL.EQ.MFILOB) THEN  ! 2回目のEOFでループ抜け
              IINFIL=MFILIN
              GOTO 5010
            ELSE
              CALL VF_A2ERR('VF_II1INP','UNEXPECTED ERROR 1.')
            ENDIF
C
C         *** POROUS外部入力の場合のファイル切替処理 ***
          ELSEIF (LEVEL.GE.2 .AND.MODEPRS.EQ.1 ) THEN
            IF(IINFIL.EQ.MFILIN) THEN ! 1回目のEOFでファイルを切り替え
              IINFIL=MFILPS
              IEOF=0
            ELSEIF(IINFIL.EQ.MFILPS) THEN  ! 2回目のEOFでループ抜け
              IINFIL=MFILIN
              GOTO 5010
            ELSE
              CALL VF_A2ERR('VF_II1INP','UNEXPECTED ERROR 2.')
            ENDIF
C
          ELSE
            GOTO 5010
          ENDIF
        ENDIF

CD      -- 1行を読み込み単語に分解する --  读入一行并对内容进行分解
        CALL VF_ZGETLN(IS,IE,MAXWDS,NWD,IINFIL,IEOF,TEXT)
        IF (NWD.LE.0) GOTO 5000  ! 若读入空行或者注释行

CD      -- PARALLELを解釈(LEVEL=<0) --
        IF     (TEXT(IS(1):IE(1)).EQ.'PARALLEL') THEN   ! 处理MPI计算区域划分信息
          IF (LEVEL.LE.0) THEN
            WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD) ! 输出至.list
            CALL VF_IIPARA(IS,IE,NWD,TEXT)
          ENDIF

CD      -- EQUATIONを解釈(LEVEL=<0) --
        ELSEIF (TEXT(IS(1):IE(1)).EQ.'EQUATION') THEN   !处理EQUATION部分
          IF (LEVEL.LE.0) THEN
            WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
            CALL VF_IIEQUA(IS,IE,NWD,TEXT)
          ENDIF

CD      -- GRIDを解釈(LEVEL=<0で格子数、LEVEL=1で座標値) -- 处理网格节点GRID部分
        ELSEIF (TEXT(IS(1):IE(1)).EQ.'GRID') THEN
          IF (LEVEL.LE.0) WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD) 
          IF (NWD.LT.2) CALL VF_A2ERR('VF_II1INP','SYNTAX ERROR.')
C         * x方向格子数を入力
          IF     (TEXT(IS(2):IE(2)).EQ.'X') THEN
            CALL VF_IIGRID(1,NI,NUMI,XX,IEOF,LEVEL,IS,IE,NWD,TEXT,  ! 直至读完GRID X 部分才返回调用
     &                     NUMI0,XXWK)
C         * y方向格子数を入力
          ELSEIF (TEXT(IS(2):IE(2)).EQ.'Y') THEN
            CALL VF_IIGRID(2,NJ,NUMJ,YY,IEOF,LEVEL,IS,IE,NWD,TEXT,
     &                     NUMJ0,YYWK)
C         * z方向格子数を入力
          ELSEIF (TEXT(IS(2):IE(2)).EQ.'Z') THEN
            CALL VF_IIGRID(3,NK,NUMK,ZZ,IEOF,LEVEL,IS,IE,NWD,TEXT, !Z方向略有不同，将表示读入的结点数设定至NUMK处
     &                     NUMK ,ZZWK)
C         * 解釈できない単語によるエラー
          ELSE
            CALL VF_A2ERR('VF_II1INP','UNKNOWN WORD.')
          ENDIF

CD      -- OBSTを解釈(LEVEL=1) --
        ELSEIF (TEXT(IS(1):IE(1)).EQ.'OBST') THEN  ! 处理障碍物OBST部分
          IF(TEXT(IS(2):IE(2)).EQ.'FILE') THEN  ! 如果为OBST FILE 命令
            IF (LEVEL.LE.0) THEN
              IF(MODEOBS.EQ.0) THEN
                WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
                IF (MYRANK.EQ.0.OR.INMODE.EQ.1) THEN
                  OPEN(MFILOB,ERR=9020,FILE=TEXT(IS(3):IE(3)),
     &                 STATUS='OLD',FORM='FORMATTED'  )
                ENDIF
                MODEOBS=1
              ELSE
                CALL VF_A2ERR('VF_II1INP',
     $                        'MULTIPLE DEFINITION OF OBST FILE.')
              ENDIF
            ENDIF
          ELSE   ! 如果为OBST 命令
            IF (LEVEL.EQ.1) THEN ! LEVEL=1时执行
              IF (MODEOBS.NE.1)
     $            WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
              CALL VF_IIOBST(NF,IS,IE,NWD,TEXT)
            ENDIF
          ENDIF

CD      -- POROUSを解釈(LEVEL>=2) --处理孔隙率POROUS部分
        ELSEIF (TEXT(IS(1):IE(1)).EQ.'POROUS') THEN
          IF(TEXT(IS(2):IE(2)).EQ.'FILE') THEN ! 如果为POROUS FILE 命令
            IF (LEVEL.LE.0) THEN
              IF(MODEPRS.EQ.0) THEN
                WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
                IF (MYRANK.EQ.0.OR.INMODE.EQ.1) THEN
                  OPEN(MFILPS,ERR=9030,FILE=TEXT(IS(3):IE(3)),
     &                 STATUS='OLD',FORM='FORMATTED'  )
                ENDIF
                MODEPRS=1
              ELSE
                CALL VF_A2ERR('VF_II1INP',
     $                        'MULTIPLE DEFINITION OF OBST FILE.')
              ENDIF
            ENDIF
          ELSE  ! 如果为POROUS命令
            IF (LEVEL.GE.2) THEN  !LEVEL=0，1时不运行
              IF (MODEPRS.NE.1)
     $            WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
              CALL VF_IIPORO(CM0,CD0,GGV,GGX,GGY,GGZ,
     &                       NF,INDX,INDY,INDZ,IS,IE,NWD,TEXT)
            ENDIF
          ENDIF

C在此读入SPH相关命令
        ELSEIF(TEXT(IS(1):IE(1)).EQ.'SPH') THEN
C         在这里处理SPH相关命令
          IF(LEVEL.EQ.1) THEN
            IF(NWD.LT.6) CALL VF_A2ERR('VF_II1INP','SYNTAX ERROR IN SPH 
     &                                               COMMAND')    ! 需保证至少有六个单词
            CALL VF_IISPH(IS,IE,NWD,TEXT)! 在这里调用VF_IISPH()处理具体的命令         
          ENDIF

CD      -- B.C.を解釈(LEVEL>=2) --
        ELSEIF (TEXT(IS(1):IE(1)).EQ.'B.C.') THEN
          IF (LEVEL.GE.2) THEN ! LEVEL=2时才执行
            WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
            IF (NWD.LT.2) CALL VF_A2ERR('VF_II1INP','SYNTAX ERROR.')
CD          * デフォルトを入力
            IF     (TEXT(IS(2):IE(2)).EQ.'D') THEN  !设定默认边界条件
              CALL VF_IIBOUN(BCU,BCV,BCW,BCP,BCF,BCVI,
     &                       BCK,BCE,BCT,BCTI,BCC,BCCI,
     &                       -1,INDX,INDB,INDBK,INDBE,INDBT,INDBC,
     &                       IS,IE,NWD,TEXT)
              CALL VF_IIBOUN(BCU,BCV,BCW,BCP,BCF,BCVI,
     &                       BCK,BCE,BCT,BCTI,BCC,BCCI,
     &                       -2,INDY,INDB,INDBK,INDBE,INDBT,INDBC,
     &                       IS,IE,NWD,TEXT)
              CALL VF_IIBOUN(BCU,BCV,BCW,BCP,BCF,BCVI,
     &                       BCK,BCE,BCT,BCTI,BCC,BCCI,
     &                       -3,INDZ,INDB,INDBK,INDBE,INDBT,INDBC,
     &                       IS,IE,NWD,TEXT)
CD          * x面を入力
            ELSEIF (TEXT(IS(2):IE(2)).EQ.'X') THEN
              CALL VF_IIBOUN(BCU,BCV,BCW,BCP,BCF,BCVI,
     &                       BCK,BCE,BCT,BCTI,BCC,BCCI,
     &                       +1,INDX,INDB,INDBK,INDBE,INDBT,INDBC,
     &                       IS,IE,NWD,TEXT)
CD          * y面を入力
            ELSEIF (TEXT(IS(2):IE(2)).EQ.'Y') THEN
              CALL VF_IIBOUN(BCU,BCV,BCW,BCP,BCF,BCVI,
     &                       BCK,BCE,BCT,BCTI,BCC,BCCI,
     &                       +2,INDY,INDB,INDBK,INDBE,INDBT,INDBC,
     &                       IS,IE,NWD,TEXT)
CD          * z面を入力
            ELSEIF (TEXT(IS(2):IE(2)).EQ.'Z') THEN
              CALL VF_IIBOUN(BCU,BCV,BCW,BCP,BCF,BCVI,
     &                       BCK,BCE,BCT,BCTI,BCC,BCCI,
     &                       +3,INDZ,INDB,INDBK,INDBE,INDBT,INDBC,
     &                       IS,IE,NWD,TEXT)
CD          * 解釈できない単語によるエラー
            ELSE
              CALL VF_A2ERR('VF_II1INP','UNKNOWN WORD.')
            ENDIF
          ENDIF

CD      -- TIMEを解釈(LEVEL>=2) --
        ELSEIF (TEXT(IS(1):IE(1)).EQ.'TIME') THEN
          IF (LEVEL.GE.2) THEN  ! LEVEL=2模式下读入并设定
            WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
            CALL VF_IITIME(IS,IE,NWD,TEXT)
          ENDIF

CD      -- MATEを解釈(LEVEL>=2) --
        ELSEIF (TEXT(IS(1):IE(1)).EQ.'MATE') THEN
          IF (LEVEL.GE.2) THEN
            WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
            CALL VF_IIMATE(IS,IE,NWD,TEXT)
          ENDIF

CD      -- MODELを解釈(LEVEL>=2) --
        ELSEIF (TEXT(IS(1):IE(1)).EQ.'MODEL') THEN
          IF (LEVEL.GE.2) THEN
            WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
            CALL VF_IIMDL(IS,IE,NWD,TEXT)
          ENDIF

CD      -- COMPを解釈(LEVEL>=2) --
        ELSEIF (TEXT(IS(1):IE(1)).EQ.'COMP') THEN
          IF (LEVEL.GE.2) THEN
            WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
            CALL VF_IICOMP(IS,IE,NWD,TEXT)
          ENDIF

CD      -- FILEを解釈(LEVEL>=2) --
        ELSEIF (TEXT(IS(1):IE(1)).EQ.'FILE') THEN
C
CD      -- FILE RESとFILE OBSはLEVEL=<0で処理する --FILE RES与FILE OBS在LEVEL<=0时处理
          IF( TEXT(IS(2):IE(2)).EQ.'RES' ) THEN  ! FILE RES  指定从何时重新计算
            IF (LEVEL.LE.0) THEN
              WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
              IF (NWD.LT.3) CALL VF_A2ERR('VF_II1INP','SYNTAX ERROR.')
C
              IF( TEXT(IS(3):IE(3)).EQ.'AUTO' ) THEN
                IF (MYRANK.EQ.0) THEN
                  IRETYP=-1
                  OPEN(MFILAR,ERR=310,
     &               FILE=MGNAME(MGRANK+1)(1:MGNLEN(MGRANK+1))//'.ars',
     &               STATUS='OLD',FORM='FORMATTED'  )
                  READ(MFILAR,*,ERR=310) II,TT
                  CLOSE(MFILAR)
CCC                  IF( II.GE.0 ) IRETYP=II
                  IRETYP=II
  310             CONTINUE
                ENDIF
                IF (NPROCS.NE.1) THEN
                  CALL VF_P1BCSI(IRETYP,1,0)
                ENDIF
                IF (IRETYP.EQ.-999)
     &          CALL VF_A2ERR('VF_II1INP','CALCULATION END.')
              ELSE
                CALL VF_ZSTOI(IRETYP,TEXT(IS(3):IE(3)))
                IF (IRETYP.LT.0)
     &          CALL VF_A2ERR('VF_II1INP','INVALID VALUE.')
              ENDIF
            ENDIF
C
C         高速化のために、各PEの担当領域の地形データの入出力を行うか否か
          ELSEIF( TEXT(IS(2):IE(2)).EQ.'OBS' ) THEN  ! FILE OBS 指定如何在重启动时读入地形文件
            IF (LEVEL.LE.0) THEN
              WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
              IF ( TEXT(IS(3):IE(3)).EQ.'ON' ) THEN
                MODEORS=1
              ELSEIF ( TEXT(IS(3):IE(3)).EQ.'OFF' ) THEN
                MODEORS=0
              ELSE
                CALL VF_A2ERR('VF_II1INP','UNKNOWN VALUE.')
              ENDIF
            ENDIF
          ELSE
            IF (LEVEL.GE.2) THEN
              WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
              CALL VF_IIFILE(IS,IE,NWD,TEXT) ! LEVEL=2时读入并解释FILE命令
            ENDIF
          ENDIF

CAKIY   @@@@@@@@@@@@@@@@@@@@@@
CD      -- OBST.TBLを解釈 --
CD      -- POROUS.TBLを解釈 --
CAKIY   @@@@@@@@@@@@@@@@@@@@@@

CD      -- OPTIONを解釈(LEVEL>=2) --
        ELSEIF (TEXT(IS(1):IE(1)).EQ.'OPTION') THEN
          IF (LEVEL.GE.2) THEN  ! LEVEL=2模式下才执行
            WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
            CALL VF_IIOPT(IS,IE,NWD,TEXT,IINFIL,IEOF)
          ENDIF
C         * LEVELが2ではないときは、パラメタ個数分だけ飛ばす
          IF (LEVEL.LT.2 .AND. TEXT(IS(2):IE(2)).EQ.'DRAG-DF') THEN
            CALL VF_ZSTOI(IDRGN,TEXT(IS(4):IE(4)))
            DO 200 II=1,IDRGN
              CALL VF_ZGETLN(IS,IE,MAXWDS,NWD,IINFIL,IEOF,TEXT)
 200        CONTINUE
          ENDIF

CD      -- DEBUGを解釈:デバッグ用(LEVEL>=2) --
        ELSEIF (TEXT(IS(1):IE(1)).EQ.'DEBUG') THEN
          IF (LEVEL.GE.2) THEN
            WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
            CALL VF_IIDBG(IS,IE,NWD,TEXT)
          ENDIF

CD      -- 解釈できないヘッダーによるエラー -- 出现无预定义的关键词， 抛出异常
        ELSE
          WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
          CALL VF_A2ERR('VF_II1INP','UNKNOWN HEADER.')
        ENDIF

C     ** 反復終了 **
 5000   GOTO 100
 5010 CONTINUE

CD    -- 入力ファイルのクローズ -- close 文件
      IF (MYRANK.EQ.0.OR.INMODE.EQ.1) THEN
        CLOSE(IINFIL)
C        IINFIL=0
        IF(MODEOBS.EQ.1.AND.LEVEL.EQ.1) CLOSE(MFILOB)
        IF(MODEPRS.EQ.1.AND.LEVEL.GE.2) CLOSE(MFILPS)
      ENDIF
      IINFIL=0

CD    -- 必須項目のチェック --
      IF (NI.LE.0) CALL VF_A2ERR('VF_II1INP','NOT FOUND (GRID X).')
      IF (NJ.LE.0) CALL VF_A2ERR('VF_II1INP','NOT FOUND (GRID Y).')
      IF (NK.LE.0) CALL VF_A2ERR('VF_II1INP','NOT FOUND (GRID Z).')

CD    -- 並列制御データのチェックと設定 --Checking and setting of parallel control data LEVEL=0时运行，
      IF (LEVEL.LE.0) CALL VF_CPARA() ! LEVEL=0 时才运行

CD    -- リスタート時に地形データの読み込みを高速化するための処理  Process for speeding up reading of terrain data at restart
      IF( MODEORS.EQ.1 ) THEN  !MODEORS被初始化为0 
CD      *** 初回計算時 ***
        IF( IRETYP.LT.0 ) THEN
          IF( LEVEL.EQ.1 ) THEN
            WRITE(TEXTP,'(I5.5)') MYRANK
            OPEN(MFILOR,FORM='UNFORMATTED',STATUS='UNKNOWN',ERR=9040,
     &         FILE=MGNAME(MGRANK+1)(1:MGNLEN(MGRANK+1))//'.obs'//TEXTP)
C           NFを出力する
            WRITE(MFILOR) NF
C
          ELSEIF( LEVEL.GE.2 ) THEN
C           GGV,GGX,GGY,GGZ,CM0,CD0を出力する
            WRITE(MFILOR) GGV
            WRITE(MFILOR) GGX
            WRITE(MFILOR) GGY
            WRITE(MFILOR) GGZ
            WRITE(MFILOR) CM0
            WRITE(MFILOR) CD0
            CLOSE(MFILOR)
          ENDIF
C
CD      *** リスタート計算時 ***  restart calculation
        ELSE
          IF( LEVEL.EQ.1 ) THEN
            WRITE(TEXTP,'(I5.5)') MYRANK
            OPEN(MFILOR,FORM='UNFORMATTED',STATUS='OLD',ERR=9040,
     &         FILE=MGNAME(MGRANK+1)(1:MGNLEN(MGRANK+1))//'.obs'//TEXTP)
C           NFを読み込み
            READ(MFILOR,ERR=9050) NF
C
          ELSEIF( LEVEL.GE.2 ) THEN
C           GGV,GGX,GGY,GGZ,CM0,CD0を読み込み
            READ(MFILOR,ERR=9050) GGV
            READ(MFILOR,ERR=9050) GGX
            READ(MFILOR,ERR=9050) GGY
            READ(MFILOR,ERR=9050) GGZ
            READ(MFILOR,ERR=9050) CM0
            READ(MFILOR,ERR=9050) CD0
            CLOSE(MFILOR)
          ENDIF
        ENDIF
      ENDIF

CD    -- マルチグリッド環境の親子関係のチェックと設定 --Checking and setting parent-child relationship of multi-grid environment -
C----------------------------------------------------------2011.04 start
C     IF (LEVEL.EQ.1) CALL VF_PMGSET(XX,YY,ZZ,NF)
cmod20160803(s)
c      IF (LEVEL.EQ.1)
c     &  CALL VF_PMGSET(XX,YY,ZZ,GGV,GGX,GGY,GGZ,XPF,YPF,ZPF,NF)
      IF (LEVEL.GE.1)  ! LEVEL=1，=2是均执行
     &  CALL VF_PMGSET(XX,YY,ZZ,GGV,GGX,GGY,GGZ,XPF,YPF,ZPF,NF
     &                ,XXWK,YYWK,ZZWK,dbuf,ibuf,level)
cmod20160803(e)
      IF (LEVEL.EQ.2) CALL VF_PMGST2(XPF,YPF,ZPF,IPF,JPF,KPF)  ! 计算当前进程作为子进程的IPF()等
C----------------------------------------------------------2011.04 end

CD    -- STOCから地形データを受信する --Receive terrain data from STOC  每个进程都调用，XXWK等记录的是CADMAS每一个父-子分区的坐标，而不是每个进程的
      IF (LEVEL.EQ.1) CALL VF_STOC_AREA(XXWK,YYWK,ZZWK)
      IF (LEVEL.EQ.1) CALL VF_STOC_OBST(NF,IBUF)
      IF (LEVEL.EQ.2) CALL VF_STOC_PORS(GGV,GGX,GGY,GGZ,
     &                                  NF,INDX,INDY,INDZ,DBUF)

CD    -- 面の状態を示すインデックスを設定 -- Set the index indicating the state of the surface
      IF (LEVEL.EQ.1) CALL VF_CINDX(NF,INDX,INDY,INDZ) ! VF_CINDX（）在执行时设定了NUMB

CD    -- マルチグリッド環境の境界条件の強制設定 -- Forced setting of boundary condition of multigrid environment
      IF (LEVEL.GE.2) CALL VF_PMGBC0(BCU,BCV,BCW,BCP,BCF,  ! 设置父—子分区 交界面的边界类型
     &                               INDX,INDY,INDB)

C     -- 実行文の終了 --
      GOTO 9999

C==== ファイル関連エラー処理 =========================================

 9010 CONTINUE
      CALL VF_A2ERR('VF_II1INP','CAN NOT OPEN (*****.in).')
      GOTO 9999

 9020 CONTINUE
      CALL VF_A2ERR('VF_II1INP',
     &              'CAN NOT OPEN OBST FILE ([OBST FILE ...]).')
      GOTO 9999

 9030 CONTINUE
      CALL VF_A2ERR('VF_II1INP',
     &              'CAN NOT OPEN POROUS FILE ([POROUS FILE ...]).')
      GOTO 9999

 9040 CONTINUE
      CALL VF_A2ERR('VF_II1INP','CAN NOT OPEN OBS FILE([FILE OBS ON]).')
      GOTO 9999

 9050 CONTINUE
      CALL VF_A2ERR('VF_II1INP','READ ERROR OBS FILE([FILE OBS ON]).')
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT(/' ',A,100(I2:))
 9520 FORMAT( ' ',100('[',A,']':))

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
