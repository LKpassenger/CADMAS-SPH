      SUBROUTINE VF_IIGRID(ISW,NL,NUML,XYZ,IEOF,LEVEL,IS,IE,NWD,TEXT,
     &                     NUMWK,XYZWK)

CD=== 概要 ===========================================================

CDT   VF_IIGRID:ある方向の格子座標データ(GRID)を読み込み、解釈する 用于解释.in文件中的GRID参数部分
CD      (1)1行目はVF_II1INPより受け取る The first line is received from VF_II1INP
CD      (2)「END」を読み込むまで、反復する Iterate until "END" is read

C==== 宣言 ===========================================================
C==========针对于SPH 的coupling 模型，该函数不需要修改 ----LK

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APARAR.h'
      INCLUDE 'VF_ASTOCI.h'
      INCLUDE 'VF_ASTOCR.h'

CD    -- 引数 --
CD    ISW             : IN  : I*4   : 方向(X:=1,Y:=2,Z:=3)
CD    NL              : I/O : I*4   : 今回のレベルで決めた格子数+1
CD    NUML            : I/O : I*4   : 前回のレベルで決めた格子数+1
CD    XYZ(MAXG1,NUML) : I/O : R*8   : ある方向の格子座標等
CD    IEOF            : I/O : I*4   : = 0:EOFを読み込んでいない
CD                                    !=0:EOFを読み込んだ
CD    LEVEL           : IN  : I*4   : 入力レベル
CD                                    =<0:格子数のみを決定
CD                                    = 1:座標値を読み込む
CD                                    >=2:読み飛ばす
CD    IS(MAXWDS)      : I/O : I*4   : n番目の単語の開始位置
CD    IE(MAXWDS)      : I/O : I*4   : n番目の単語の終了位置
CD    NWD             : I/O : I*4   : 単語の数
CD    TEXT            : I/O : C*(*) : 入力した文字列
      DIMENSION XYZ(MAXG1,NUML)
      DIMENSION IS(MAXWDS),IE(MAXWDS)
      DIMENSION XYZWK(NUMWK)
      CHARACTER*(MAXCHR) TEXT

C==== 実行 ===========================================================

CD    -- 初期設定 --
      IF (NL.NE.0) CALL VF_A2ERR('VF_IIGRID','SECOND TIMES.')
      NL  =1
      IEND=0 !局部变量，用于指示是否读到END关键词，=0，表示未读到
      IP  =3
      X1  =0.0D0
      X2  =0.0D0
C     エラーチェッカーをだますため
      XOLD=0.0D0 ! 指上一个读到的值
      IF     (ISW.EQ.1) THEN
        NUML0=NUMI0 ! 最初NUMI0，MYGIS，MYGIE在VF_P0INIT中均被初始化为0，第一次CALL vf_ii1inp()后NUMI0等不再为0
        LG0=MYGIS
        LG2=MYGIE
      ELSEIF (ISW.EQ.2) THEN
        NUML0=NUMJ0
        LG0=MYGJS
        LG2=MYGJE
      ELSE
        NUML0=NUML
        LG0=1
        LG2=NUML0
      ENDIF
      LG1=LG0
      IF (LG1.EQ.1) LG1=2

CD    -- ENDがくるまで座標値を読む --Read coordinate values 直至关键词END
C     ** 中判定反復 **
 100  CONTINUE

CD      -- 単語を解釈 --
        DO 200 I=IP,NWD
          IF (TEXT(IS(I):IE(I)).EQ.'END') THEN  ! 表示结束的END需要单独放在一行中
            IF (I.NE.NWD) CALL VF_A2ERR('VF_IIGRID','SYNTAX ERROR.')
            IEND=I  !   设置IEND用于结束某一方向节点坐标的读取
            GOTO 210
          ELSE
            NL=NL+1
            IF ((LEVEL.GE.1) .AND. (NL.GT.NUML0)) ! LEVEL=1时
     &                CALL VF_A2ERR('VF_IIGRID','FILE WAS CHANGED.')
            CALL VF_ZSTOR(XNOW,TEXT(IS(I):IE(I)))  ! 解读当前行储存至局部变量XNOW中
            IF (NL.GE.3) THEN
              IF (XNOW-XOLD.LT.ZEROG)  ! 检查前后读入的两个坐标值是否重合
     &                  CALL VF_A2ERR('VF_IIGRID','INVALID VALUE.')
            ENDIF
            IF (LEVEL.EQ.1) THEN  ! LEVEL=1时
              IF     (LG1.LE.NL .AND. NL.LE.LG2) THEN ! 判断读入的节点是否属于当前进程负责的范围 LG1-1<=NL-1<=LG2-1
                XYZ(1,NL-LG0+1)=XNOW
              ELSEIF (NL.EQ.LG2+1              ) THEN
C               * 通信をしないためにとっておく Keep it for not communicating
                XBUF=XNOW ! 若读到的坐标为作用范围结束点的坐标，应放置在XBUF中，不同进程不同
              ENDIF
cmod20160721              IF (LB_CADMAS.EQ.1) XYZWK(NL-1)=XNOW
              XYZWK(NL-1)=XNOW  ! 记录读到的数值至XXWK,YYWK,ZZWK
               
            ENDIF
            XOLD=XNOW  
            IF (NL.EQ.2) X1=XNOW
            X2=XNOW
          ENDIF
 200    CONTINUE
 210    CONTINUE

CD      -- ENDを読み込んだら抜ける --  I can get out even if I read END
        IF (IEND.NE.0) GOTO 300
        IF (IEOF.NE.0) CALL VF_A2ERR('VF_IIGRID','NOT FOUND (END).')

CD      -- 次の1行を読み込み単語に分解する --
        CALL VF_ZGETLN(IS,IE,MAXWDS,NWD,IINFIL,IEOF,TEXT) ! 调用VF_ZGETLN（）读取LINBUF（）的下一行
        IF (NWD.GT.0 .AND. LEVEL.LE.0)
     &               WRITE(ILPFIL,9510) (TEXT(IS(I):IE(I)),I=1,NWD)
        IP=1

CD    ** 反復終了 **
        GOTO 100
 300  CONTINUE  ! 正常情况下循环执行直至找到END关键词后再跳出

CD    -- 格子数のチェックと全体の範囲 --
      IF (NL.LT.3) CALL VF_A2ERR('VF_IIGRID','2 > NUMBER OF GRID.')
      IF ((LEVEL.GE.1) .AND. (NL.NE.NUML0)) 
     &               CALL VF_A2ERR('VF_IIGRID','FILE WAS CHANGED.')
      IF     (ISW.EQ.1) THEN  ! X方向
        NUMI0 =NL   ! VF_APARAI.h中的NUMI0，全局范围内
        GLXMIN=X1   ! GLXMIN:读入的节点坐标最小值，全局范围内
        GLXMAX=X2   ! GLXMAX:读入的节点坐标最大值，全局范围内
      ELSEIF (ISW.EQ.2) THEN
        NUMJ0 =NL   ! 同X方向
        GLYMIN=X1   
        GLYMAX=X2   
      ELSE
        NUML  =NL  ! Z方向略有不同，将表示读入的结点数设定至形参,原因可能是Z方向上不能进行MPI分区
      ENDIF

CD    -- 格子関連データの設定 --
      IF (LEVEL.EQ.1) CALL VF_CGRID(ISW,XYZ,XBUF,NUML)

C     -- 実行文の終了 --
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT(' ','& ',100('[',A,']':))

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
