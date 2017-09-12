      SUBROUTINE VF_PMGC2P(XX,YY,ZZ,UU,VV,WW,FF,
C----------------------------------------------------------2011.04 start
     &                     GGV,GGX,GGY,XPF,YPF,ZPF,IPF,JPF,KPF,
C----------------------------------------------------------2011.04 end
     &                     BCU,BCV,BCW,BCF,DBUF,NF,INDX,INDY,INDB)

CD=== 概要 ===========================================================

CDT   VF_PMGC2P:マルチグリッド環境の子の情報を親へ転送する
C        Transfer the child information of the multigrid environment to the parent
C         子分区 向 父分区 传递父分区需要的边界信息

C==== 宣言 ===========================================================

C     -- 大域型 --
      use mod_apara,only: set_c2p,ncountrsum,nrecv,rbuf
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)   : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)     : I/O : R*8 : x方向流速
CD    VV(@FOR-3D@)     : I/O : R*8 : y方向流速
CD    WW(@FOR-3D@)     : IN  : R*8 : z方向流速
CD    FF(@FOR-3D@)     : IN  : R*8 : VOF関数F
C----------------------------------------------------------2011.04 start
CD    XPF(NUMI)        : IN  : R*8 : x方向の親格子に対する補間係数
CD    YPF(NUMJ)        : IN  : R*8 : y方向の親格子に対する補間係数
CD    ZPF(NUMK)        : IN  : R*8 : z方向の親格子に対する補間係数
CD    IPF(MGPINF(1))   : IN  : I*4 : x方向の親格子1に対する格子の数
CD    JPF(MGPINF(2))   : IN  : I*4 : y方向の親格子1に対する格子の数
CD    KPF(MGPINF(3))   : IN  : I*4 : z方向の親格子1に対する格子の数
C----------------------------------------------------------2011.04 end
CD    BCU(NUMB)        : I/O : R*8 : x方向流速の境界値
CD    BCV(NUMB)        : I/O : R*8 : y方向流速の境界値
CD    BCW(NUMB)        : I/O : R*8 : z方向流速の境界値
CD    BCF(NUMB)        : I/O : R*8 : VOF関数Fの境界値
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDB(MAXB1,NUMB) : IN  : I*4 : 境界面のインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK),FF  (NUMI,NUMJ,NUMK)
C----------------------------------------------------------2011.04 start
      DIMENSION GGV(NUMI,NUMJ,NUMK)
      DIMENSION GGX(NUMI,NUMJ,NUMK),GGY(NUMI,NUMJ,NUMK)
      DIMENSION XPF(NUMI),YPF(NUMJ),ZPF(NUMK)
      DIMENSION IPF(0:MGPINF(1)),JPF(0:MGPINF(2)),KPF(0:MGPINF(3))
C----------------------------------------------------------2011.04 end
      DIMENSION BCU (NUMB),BCV(NUMB),BCW(NUMB),BCF(NUMB)
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDB(MAXB1,NUMB)

C==== 実行 ===========================================================

CD    C@ CAKIY ID,JD,KD
C----------------------------------------------------------2011.04 start
C     ID=3
C     JD=3
C     KD=1
      MGNV = 4  !!!!!  这里是  4   
C----------------------------------------------------------2011.04 end

CD    -- 子の値を親に転送する --
      IF (MGPRNK.GE.0) THEN  ! 若当前进程存在父进程
C----------------------------------------------------------2011.04 start
C       DO 290 LL=1,4
C----------------------------------------------------------2011.04 end
          IS=1
          JS=1
          KS=1
          IE=MGPINF(1)
          JE=MGPINF(2)
          KE=MGPINF(3)
          IF (MGPINF(4).EQ.0) IS=IS+1
          IF (MGPINF(5).EQ.0) JS=JS+1
          IF (MGPINF(7).EQ.0) IE=IE-1
          IF (MGPINF(8).EQ.0) JE=JE-1

          NN=0
          IF (MGPINF(4).EQ.0) THEN
            DO 130 KK=KS,KE
              DO 120 JJ=JS,JE
C----------------------------------------------------------2011.04 start
C               NN=NN+1
C               VAL=0.0D0
C               K0=1+KD*(KK-1)
C               J0=1+JD*(JJ-1)
C               I0=2+ID*(IS-1)
C               DO 110 KL=1,KD
C                 K=K0+KL
C                 DO 100 JL=1,JD
C                   J=J0+JL
C                   IF (LL.EQ.1) THEN
C                     VAL=VAL+0.5D0*(FF(I0-1,J,K)+FF(I0,J,K))
C                   ENDIF
C                   IF (LL.EQ.2) THEN
C                     VAL=VAL+UU(I0,J,K)
C                   ENDIF
C                   IF (LL.EQ.3) THEN
C                     VAL=VAL+0.25D0*(VV(I0-1,J  ,K)+VV(I0,J  ,K)+
C     &                                VV(I0-1,J+1,K)+VV(I0,J+1,K))
C                   ENDIF
C                   IF (LL.EQ.4) THEN
C                     VAL=VAL+0.25D0*(WW(I0-1,J,K  )+WW(I0,J,K  )+
C    &                                WW(I0-1,J,K+1)+WW(I0,J,K+1))
C                   ENDIF
C100              CONTINUE
C110            CONTINUE
C               DBUF(NN)=VAL/DBLE(JD*KD)
                CALL VF_PMGC2P_CF(DBUF(NN+1),
     &                            XX,YY,ZZ,GGV,GGX,GGY,UU,VV,WW,FF,
     &                            XPF,YPF,ZPF,IPF,JPF,KPF,
     &                            BCU,BCV,BCW,BCF,NF,INDX,INDY,INDB,
     &                            IS,JJ,KK,-1,MGNV)
                NN = NN + MGNV
C----------------------------------------------------------2011.04 end
 120          CONTINUE
 130        CONTINUE
          ENDIF

          IF (MGPINF(7).EQ.0) THEN
            DO 180 KK=KS,KE
              DO 170 JJ=JS,JE
C----------------------------------------------------------2011.04 start
C               NN=NN+1
C               VAL=0.0D0
C               K0=1+KD*(KK-1)
C               J0=1+JD*(JJ-1)
C               I0=  ID*(IE+1)
C               DO 160 KL=1,KD
C                 K=K0+KL
C                 DO 150 JL=1,JD
C                   J=J0+JL
C                   IF (LL.EQ.1) THEN
C                     VAL=VAL+0.5D0*(FF(I0-1,J,K)+FF(I0,J,K))
C                   ENDIF
C                   IF (LL.EQ.2) THEN
C                     VAL=VAL+UU(I0,J,K)
C                   ENDIF
C                   IF (LL.EQ.3) THEN
C                     VAL=VAL+0.25D0*(VV(I0-1,J  ,K)+VV(I0,J  ,K)+
C    &                                VV(I0-1,J+1,K)+VV(I0,J+1,K))
C                   ENDIF
C                   IF (LL.EQ.4) THEN
C                     VAL=VAL+0.25D0*(WW(I0-1,J,K  )+WW(I0,J,K  )+
C    &                                WW(I0-1,J,K+1)+WW(I0,J,K+1))
C                   ENDIF
C150              CONTINUE
C160            CONTINUE
C               DBUF(NN)=VAL/DBLE(JD*KD)
                CALL VF_PMGC2P_CF(DBUF(NN+1),
     &                            XX,YY,ZZ,GGV,GGX,GGY,UU,VV,WW,FF,
     &                            XPF,YPF,ZPF,IPF,JPF,KPF,
     &                            BCU,BCV,BCW,BCF,NF,INDX,INDY,INDB,
     &                            IE+1,JJ,KK,+1,MGNV)
                NN = NN + MGNV
C----------------------------------------------------------2011.04 end
 170          CONTINUE
 180        CONTINUE
          ENDIF

          IF (MGPINF(5).EQ.0) THEN
            DO 230 KK=KS,KE
              DO 220 II=IS,IE
C----------------------------------------------------------2011.04 start
C               NN=NN+1
C               VAL=0.0D0
C               K0=1+KD*(KK-1)
C               J0=2+JD*(JS-1)
C               I0=1+ID*(II-1)
C               DO 210 KL=1,KD
C                 K=K0+KL
C                 DO 200 IL=1,ID
C                   I=I0+IL
C                   IF (LL.EQ.1) THEN
C                     VAL=VAL+0.5D0*(FF(I,J0-1,K)+FF(I,J0,K))
C                   ENDIF
C                   IF (LL.EQ.2) THEN
C                     VAL=VAL+0.25D0*(UU(I  ,J0-1,K)+UU(I  ,J0,K)+
C    &                                UU(I+1,J0-1,K)+UU(I+1,J0,K))
C                   ENDIF
C                   IF (LL.EQ.3) THEN
C                     VAL=VAL+VV(I,J0,K)
C                   ENDIF
C                   IF (LL.EQ.4) THEN
C                     VAL=VAL+0.25D0*(WW(I,J0-1,K  )+WW(I,J0,K  )+
C    &                                WW(I,J0-1,K+1)+WW(I,J0,K+1))
C                   ENDIF
C200              CONTINUE
C210            CONTINUE
C               DBUF(NN)=VAL/DBLE(ID*KD)
                CALL VF_PMGC2P_CF(DBUF(NN+1),
     &                            XX,YY,ZZ,GGV,GGX,GGY,UU,VV,WW,FF,
     &                            XPF,YPF,ZPF,IPF,JPF,KPF,
     &                            BCU,BCV,BCW,BCF,NF,INDX,INDY,INDB,
     &                            II,JS,KK,-2,MGNV)
                NN = NN + MGNV
C----------------------------------------------------------2011.04 end
 220          CONTINUE
 230        CONTINUE
          ENDIF

          IF (MGPINF(8).EQ.0) THEN
            DO 280 KK=KS,KE
              DO 270 II=IS,IE
C----------------------------------------------------------2011.04 start
C               NN=NN+1
C               VAL=0.0D0
C               K0=1+KD*(KK-1)
C               J0=  JD*(JE+1)
C               I0=1+ID*(II-1)
C               DO 260 KL=1,KD
C                 K=K0+KL
C                 DO 250 IL=1,ID
C                   I=I0+IL
C                   IF (LL.EQ.1) THEN
C                     VAL=VAL+0.5D0*(FF(I,J0-1,K)+FF(I,J0,K))
C                   ENDIF
C                   IF (LL.EQ.2) THEN
C                     VAL=VAL+0.25D0*(UU(I  ,J0-1,K)+UU(I  ,J0,K)+
C    &                                UU(I+1,J0-1,K)+UU(I+1,J0,K))
C                   ENDIF
C                   IF (LL.EQ.3) THEN
C                     VAL=VAL+VV(I,J0,K)
C                   ENDIF
C                   IF (LL.EQ.4) THEN
C                     VAL=VAL+0.25D0*(WW(I,J0-1,K  )+WW(I,J0,K  )+
C    &                                WW(I,J0-1,K+1)+WW(I,J0,K+1))
C                   ENDIF
C250              CONTINUE
C260            CONTINUE
C               DBUF(NN)=VAL/DBLE(ID*KD)
                CALL VF_PMGC2P_CF(DBUF(NN+1),
     &                            XX,YY,ZZ,GGV,GGX,GGY,UU,VV,WW,FF,
     &                            XPF,YPF,ZPF,IPF,JPF,KPF,
     &                            BCU,BCV,BCW,BCF,NF,INDX,INDY,INDB,
     &                            II,JE+1,KK,+2,MGNV)
                NN = NN + MGNV
C----------------------------------------------------------2011.04 end
 270          CONTINUE
 280        CONTINUE
          ENDIF
c          write(100+mgrank,*) 'c2p(child):',nn,MGPRNK
          CALL VF_ZXMG_ISENDD(DBUF,NN,MGPRNK,IREQ,IERR)  !!!! 每个子进程都将统计的DBUF()发送至 MYRANK=0 的父进程中
          CALL VF_ZXMG_WAIT(IREQ,IERR)
C----------------------------------------------------------2011.04 start
C290    CONTINUE
C----------------------------------------------------------2011.04 end
      ENDIF

c      call vf_zxmp_barri(ierr)
cadd20160729(s)  用于父进程 接收 子进程传递进来的 边界条件， 并进行设定
      if( MGCNUM.gt.0 ) then
         if( myrank.eq.0 ) then
            NN=1
            DO 610 IC=1,MGCNUM
c               write(100+mgrank,*) 'c2p:',nn,ic,nrecv(ic),MGCRNK(IC)
               CALL VF_ZXMG_IRECVD(RBUF(NN),NRECV(IC),MGCRNK(IC),  !!! 接收至RBUF（） 中
     $                             IREQ,IERR)
               CALL VF_ZXMG_WAIT(IREQ,IERR)
cc               write(1000+mgrank,*) rbuf(nn:nn+nrecv(ic)-1)
               NN=NN+NRECV(IC)
  610       CONTINUE
         endif
c
         CALL SET_C2P(RBUF,DBUF,MYRANK,NPROCS)  !!! 重新排序后 再 发送回 至 当前进程的DBUF()中
      endif                                     !!! 这样当前进程的DBUF() 包含了 子分区提供给当前进程的边界条件
cadd20160729(e)
CD    -- 子の値を親が受信する --  下边根据接收的DBUF() 进行设定
      DO 600 IC=1,MGCNUM
C----------------------------------------------------------2011.04 start
C       DO 590 LL=1,4
C----------------------------------------------------------2011.04 end
          IS=MGCPOS(1,IC)
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
          ISG=MYIS
          IEG=MYIE
          JSG=MYJS
          JEG=MYJE
          IF (MGCINF(4,IC).EQ.0) IS=IS+1
          IF (MGCINF(5,IC).EQ.0) JS=JS+1
          IF (MGCINF(7,IC).EQ.0) IE=IE-1
          IF (MGCINF(8,IC).EQ.0) JE=JE-1
          NN2=0
          IF (MGCINF(4,IC).EQ.0) NN2=NN2+(JE-JS+1)*(KE-KS+1)
          IF (MGCINF(7,IC).EQ.0) NN2=NN2+(JE-JS+1)*(KE-KS+1)
          IF (MGCINF(5,IC).EQ.0) NN2=NN2+(IE-IS+1)*(KE-KS+1)
          IF (MGCINF(8,IC).EQ.0) NN2=NN2+(IE-IS+1)*(KE-KS+1)
C----------------------------------------------------------2011.04 start
          NN2 = MGNV*NN2
C----------------------------------------------------------2011.04 start
c          CALL VF_ZXMG_IRECVD(DBUF,NN,MGCRNK(IC),IREQ,IERR)
c          CALL VF_ZXMG_WAIT(IREQ,IERR)
          if( ic.eq.1 ) NN=0
c          write(1000+mgrank,*) dbuf(nn+1:nn+nn2)
cadd20160729(s)
          IF (MGCINF(4,IC).EQ.0) THEN
            DO 310 K=KS,KE
              DO 300 J=JS,JE
cmod20160729(s)
                I=IS
                IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $              JSG.LE.J.AND.J.LE.JEG ) THEN
C----------------------------------------------------------2011.04 start
C               NN=NN+1
C               VAL=DBUF(NN)
C----------------------------------------------------------2011.04 end
                IF (INDX(IS,J,K).GE.1) THEN
                  L=INDX(IS,J,K)
C----------------------------------------------------------2011.04 start
C                 IF (LL.EQ.1) BCF(L)=VAL
C                 IF (LL.EQ.2) THEN
C                   BCU(L)=VAL
C                   UU(IS,J,K)=VAL
C                 ENDIF
C                 IF (LL.EQ.3) BCV(L)=VAL
C                 IF (LL.EQ.4) BCW(L)=VAL
                  BCF(L)=DBUF(NN+1)    !!!!!!!! 设定边界条件值BCF(),BCU()等
                  BCU(L)=DBUF(NN+2)
                  BCV(L)=DBUF(NN+3)
                  BCW(L)=DBUF(NN+4)
                  UU(IS,J,K)=BCU(L)
                  NN = NN + MGNV
C----------------------------------------------------------2011.04 end
                ENDIF
              ENDIF
cmod20160729(e)
 300          CONTINUE
 310        CONTINUE
          ENDIF

          IF (MGCINF(7,IC).EQ.0) THEN
            DO 360 K=KS,KE
              DO 350 J=JS,JE
cmod20160729(s)
                I=IE+1
                IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $              JSG.LE.J.AND.J.LE.JEG ) THEN
C----------------------------------------------------------2011.04 start
C               NN=NN+1
C               VAL=DBUF(NN)
C----------------------------------------------------------2011.04 end
                IF (INDX(IE+1,J,K).GE.1) THEN
                  L=INDX(IE+1,J,K)
C----------------------------------------------------------2011.04 start
C                 IF (LL.EQ.1) BCF(L)=VAL
C                 IF (LL.EQ.2) THEN
C                   BCU(L)=VAL
C                   UU(IE+1,J,K)=VAL
C                 ENDIF
C                 IF (LL.EQ.3) BCV(L)=VAL
C                 IF (LL.EQ.4) BCW(L)=VAL
                  BCF(L)=DBUF(NN+1)
                  BCU(L)=DBUF(NN+2)
                  BCV(L)=DBUF(NN+3)
                  BCW(L)=DBUF(NN+4)
                  UU(IE+1,J,K)=BCU(L)
                  NN = NN + MGNV
C----------------------------------------------------------2011.04 end
                ENDIF
              ENDIF
cmod20160729(e)
 350          CONTINUE
 360        CONTINUE
          ENDIF

          IF (MGCINF(5,IC).EQ.0) THEN
            DO 410 K=KS,KE
              DO 400 I=IS,IE
cmod20160729(s)
                J=JS
                IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $              JSG.LE.J.AND.J.LE.JEG ) THEN
C----------------------------------------------------------2011.04 start
C               NN=NN+1
C               VAL=DBUF(NN)
C----------------------------------------------------------2011.04 end
                IF (INDY(I,JS,K).GE.1) THEN
                  L=INDY(I,JS,K)
C----------------------------------------------------------2011.04 start
C                 IF (LL.EQ.1) BCF(L)=VAL
C                 IF (LL.EQ.2) BCU(L)=VAL
C                 IF (LL.EQ.3) THEN
C                   BCV(L)=VAL
C                   VV(I,JS,K)=VAL
C                 ENDIF
C                 IF (LL.EQ.4) BCW(L)=VAL
                  BCF(L)=DBUF(NN+1)
                  BCU(L)=DBUF(NN+2)
                  BCV(L)=DBUF(NN+3)
                  BCW(L)=DBUF(NN+4)
                  VV(I,JS,K)=BCV(L)
                  NN = NN + MGNV
C----------------------------------------------------------2011.04 end
                ENDIF
              ENDIF
cmod20160729(e)
 400          CONTINUE
 410        CONTINUE
          ENDIF

          IF (MGCINF(8,IC).EQ.0) THEN
            DO 460 K=KS,KE
              DO 450 I=IS,IE
cmod20160729(s)
                J=JE+1
                IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $              JSG.LE.J.AND.J.LE.JEG ) THEN
C----------------------------------------------------------2011.04 start
C               NN=NN+1
C               VAL=DBUF(NN)
C----------------------------------------------------------2011.04 end
                IF (INDY(I,JE+1,K).GE.1) THEN
                  L=INDY(I,JE+1,K)
C----------------------------------------------------------2011.04 start
C                 IF (LL.EQ.1) BCF(L)=VAL
C                 IF (LL.EQ.2) BCU(L)=VAL
C                 IF (LL.EQ.3) THEN
C                   BCV(L)=VAL
C                   VV(I,JE+1,K)=VAL
C                 ENDIF
C                 IF (LL.EQ.4) BCW(L)=VAL
                  BCF(L)=DBUF(NN+1)
                  BCU(L)=DBUF(NN+2)
                  BCV(L)=DBUF(NN+3)
                  BCW(L)=DBUF(NN+4)
                  VV(I,JE+1,K)=BCV(L)
                  NN = NN + MGNV
C----------------------------------------------------------2011.04 end
                ENDIF
              ENDIF
cmod20160729(e)
 450          CONTINUE
 460        CONTINUE
c            write(2000+mgrank,*) bcu
c            write(2100+mgrank,*) bcv
c            write(2200+mgrank,*) bcw
c            write(2300+mgrank,*) bcf
          ENDIF
C----------------------------------------------------------2011.04 start
C590    CONTINUE
C----------------------------------------------------------2011.04 end
 600  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
