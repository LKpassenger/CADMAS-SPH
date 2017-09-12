      SUBROUTINE VF_PMGBC0(BCU,BCV,BCW,BCP,BCF,INDX,INDY,INDB)

CD=== 概要 ===========================================================

CDT   VF_PMGBC0:マルチグリッド環境の境界条件を強制設定する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    BCU(NUMB)        : I/O : R*8 : x方向流速の境界値
CD    BCV(NUMB)        : I/O : R*8 : y方向流速の境界値
CD    BCW(NUMB)        : I/O : R*8 : z方向流速の境界値
CD    BCP(NUMB)        : I/O : R*8 : 圧力の境界値
CD    BCF(NUMB)        : I/O : R*8 : VOF関数Fの境界値
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDB(MAXB1,NUMB) : I/O : I*4 : 境界面のインデックス
      DIMENSION BCU(NUMB),BCV(NUMB),BCW(NUMB),BCP(NUMB),BCF(NUMB)
      DIMENSION INDX(NUMI,NUMJ,NUMK),INDY(NUMI,NUMJ,NUMK)
      DIMENSION INDB(MAXB1,NUMB)

C==== 実行 ===========================================================

CD    -- 子との通信部分に設定する --当前进程作为父进程，设置从子分区接收边界信息的交接面的边界类型
      DO 300 IC=1,MGCNUM   !  当前进程的各个子进程循环，如果当前进程并无子进程，则MGCNUM=0，不进行该循环
        IS=MGCPOS(1,IC)   ! 子进程的范围
        JS=MGCPOS(2,IC)
        KS=MGCPOS(3,IC)
        IE=MGCPOS(4,IC)
        JE=MGCPOS(5,IC)
        KE=MGCPOS(6,IC)
cadd20160729(s)
c ..... ローカルインデックスに変換
        IS=IS-MYGIS+1
        JS=JS-MYGJS+1
        IE=IE-MYGIS+1
        JE=JE-MYGJS+1
        ISG=MYIS  ! 当前进程的范围
        IEG=MYIE
        JSG=MYJS
        JEG=MYJE
cadd20160729(e)
        IF (MGCINF(4,IC).EQ.0) IS=IS+1
        IF (MGCINF(5,IC).EQ.0) JS=JS+1
        IF (MGCINF(7,IC).EQ.0) IE=IE-1
        IF (MGCINF(8,IC).EQ.0) JE=JE-1

        IF (MGCINF(4,IC).EQ.0) THEN
          DO 110 K=KS,KE
            DO 100 J=JS,JE
cmod20160729(s)
              I=IS
              IF( ISG.LE.I.AND.I.LE.IEG .AND.       
     $            JSG.LE.J.AND.J.LE.JEG ) THEN    !  判断当前进程是否包含子->父方向的边界面
                IF (INDX(IS,J,K).GE.1) THEN  ! 单元表面作为物理边界面使用
                    L=INDX(IS,J,K) ! 为当前交接面设置边界条件
                    INDB(3,L)=3  ! 给定流速  这是这个子过程的关键，相当于设定这些交界面的边界类型为给定值，具体值应该来自于子进程，这里先设定为0.0
                    INDB(4,L)=1  ! 给定F
                    BCU(L)=0.0D0 ! 具体的流速、压强、F边界条件值，先设定为0.0，之后会再设定
                    BCV(L)=0.0D0
                    BCW(L)=0.0D0
                    BCP(L)=0.0D0
                    BCF(L)=0.0D0
                ENDIF
              ENDIF
cmod20160729(e)
 100        CONTINUE
 110      CONTINUE
        ENDIF

        IF (MGCINF(7,IC).EQ.0) THEN
          DO 160 K=KS,KE
            DO 150 J=JS,JE
cmod20160729(s)
              I=IE+1
              IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $            JSG.LE.J.AND.J.LE.JEG ) THEN
              IF (INDX(IE+1,J,K).GE.1) THEN
                L=INDX(IE+1,J,K)
                INDB(3,L)=3  ! 同上
                INDB(4,L)=1
                BCU(L)=0.0D0
                BCV(L)=0.0D0
                BCW(L)=0.0D0
                BCP(L)=0.0D0
                BCF(L)=0.0D0
              ENDIF
              ENDIF
cmod20160729(e)
 150        CONTINUE
 160      CONTINUE
        ENDIF

        IF (MGCINF(5,IC).EQ.0) THEN
          DO 210 K=KS,KE
            DO 200 I=IS,IE
cmod20160729(s)
              J=JS
              IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $            JSG.LE.J.AND.J.LE.JEG ) THEN
              IF (INDY(I,JS,K).GE.1) THEN
                L=INDY(I,JS,K)
                INDB(3,L)=3
                INDB(4,L)=1
                BCU(L)=0.0D0
                BCV(L)=0.0D0
                BCW(L)=0.0D0
                BCP(L)=0.0D0
                BCF(L)=0.0D0
              ENDIF
              ENDIF
cmod20160729(e)
 200        CONTINUE
 210      CONTINUE
        ENDIF

        IF (MGCINF(8,IC).EQ.0) THEN
          DO 260 K=KS,KE
            DO 250 I=IS,IE
cmod20160729(s)
              J=JE+1
              IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $            JSG.LE.J.AND.J.LE.JEG ) THEN
              IF (INDY(I,JE+1,K).GE.1) THEN
                L=INDY(I,JE+1,K)
                INDB(3,L)=3
                INDB(4,L)=1
                BCU(L)=0.0D0
                BCV(L)=0.0D0
                BCW(L)=0.0D0
                BCP(L)=0.0D0
                BCF(L)=0.0D0
              ENDIF
              ENDIF
cmod20160729(e)
 250        CONTINUE
 260      CONTINUE
        ENDIF
 300  CONTINUE

CD    -- 親との通信部分に設定する --当前进程作为子进程，设置从父分区接收边界信息的交接面的边界类型
      IF (MGPRNK.GE.0) THEN   ! 若当前进程在CADMAS部分无父进程，则MGPRNK保持为-1，跳过该块
        IS=2
        JS=2
        IE=NUMI-1
        JE=NUMJ-1

        IF (MGPINF(4).EQ.0) THEN
          DO 410 K=2,NUMK-1
            DO 400 J=JS,JE
              IF (INDX(IS,J,K).GE.1) THEN
                L=INDX(IS,J,K)
                INDB(3,L)=3
                INDB(4,L)=1
                BCU(L)=0.0D0
                BCV(L)=0.0D0
                BCW(L)=0.0D0
                BCP(L)=0.0D0
                BCF(L)=0.0D0
              ENDIF
 400        CONTINUE
 410      CONTINUE
        ENDIF

        IF (MGPINF(7).EQ.0) THEN
          DO 460 K=2,NUMK-1
            DO 450 J=JS,JE
              IF (INDX(IE+1,J,K).GE.1) THEN
                L=INDX(IE+1,J,K)
                INDB(3,L)=3
                INDB(4,L)=1
                BCU(L)=0.0D0
                BCV(L)=0.0D0
                BCW(L)=0.0D0
                BCP(L)=0.0D0
                BCF(L)=0.0D0
              ENDIF
 450        CONTINUE
 460      CONTINUE
        ENDIF

        IF (MGPINF(5).EQ.0) THEN
          DO 510 K=2,NUMK-1
            DO 500 I=IS,IE
              IF (INDY(I,JS,K).GE.1) THEN
                L=INDY(I,JS,K)
                INDB(3,L)=3
                INDB(4,L)=1
                BCU(L)=0.0D0
                BCV(L)=0.0D0
                BCW(L)=0.0D0
                BCP(L)=0.0D0
                BCF(L)=0.0D0
              ENDIF
 500        CONTINUE
 510      CONTINUE
        ENDIF

        IF (MGPINF(8).EQ.0) THEN
          DO 560 K=2,NUMK-1
            DO 550 I=IS,IE
              IF (INDY(I,JE+1,K).GE.1) THEN
                L=INDY(I,JE+1,K)
                INDB(3,L)=3
                INDB(4,L)=1
                BCU(L)=0.0D0
                BCV(L)=0.0D0
                BCW(L)=0.0D0
                BCP(L)=0.0D0
                BCF(L)=0.0D0
              ENDIF
 550        CONTINUE
 560      CONTINUE
        ENDIF
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
