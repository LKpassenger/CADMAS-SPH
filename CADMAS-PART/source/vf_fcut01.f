      SUBROUTINE VF_FCUT01(XX,YY,ZZ,FF,GGV,DBUF,NF)

CD=== 概要 ===========================================================

CDT   VF_FCUT01: VOF関数Fのカットオフ(0.0=<F=<1.0)と空間積分の計算
C               Cutoff of VOF function F (0.0 = <F = <1.0) and calculation of spatial integral
C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPR.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)   : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    FF(@FOR-3D@)     : I/O : R*8 : VOF関数F
CD    GGV(@FOR-3D@)    : IN  : R*8 : 空隙率
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION FF(NUMI,NUMJ,NUMK),GGV(NUMI,NUMJ,NUMK)
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION NF(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- F値のカットオフと空間積分 --
      FSUM=0.0D0
      FCUT=0.0D0
      DO 220 K=2,NUMK-1
        DO 210 J=MYJS,MYJE
          DO 200 I=MYIS,MYIE
            IF (NF(I,J,K).NE.-1) THEN
              V=XX(2,I)*YY(2,J)*ZZ(2,K)*GGV(I,J,K)  ! 相当于体积乘以孔隙率，得到的是水的体积
              IF     (FF(I,J,K).GT.FUPPER) THEN ! 默认情况下 FUPPER=1.0D0-FEPS
                FCUT=FCUT+(FF(I,J,K)-1.0D0)*V  ! 累加
                FF(I,J,K)=1.0D0  ! 超过FUPPER的被设置为1.0
              ELSEIF (FF(I,J,K).LT.FLOWER) THEN
                FCUT=FCUT+(FF(I,J,K)-0.0D0)*V
                FF(I,J,K)=0.0D0  ! 低于FLOWER的被设置为0.0
              ENDIF
              FSUM=FSUM+FF(I,J,K)*V ! 累加,相当于记录总的水的体积
            ENDIF
 200      CONTINUE
 210    CONTINUE
 220  CONTINUE
      W=FSUM
      CALL VF_P1SUMD(W,FSUM) ! 同父-子分区各个进程的FSUM归约求和
      W=FCUT
      CALL VF_P1SUMD(W,FCUT) !

      CALL VF_P3SRD2(FF,DBUF,0)  !!!!!!! 由于对一些单元的FF()进行了修正，故调用VF_P3SRD2()重新更新MPI通讯层的FF()属性

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
