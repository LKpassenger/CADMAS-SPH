C----------------------------------------------------------2011.04 start
C     SUBROUTINE VF_PMGSET(XX,YY,ZZ,NF)
cmod20160721(s)
c      SUBROUTINE VF_PMGSET(XX,YY,ZZ,GGV,GGX,GGY,GGZ,XPF,YPF,ZPF,NF)
      SUBROUTINE VF_PMGSET(XX,YY,ZZ,GGV,GGX,GGY,GGZ,XPF,YPF,ZPF,NF
     &                    ,XXWK,YYWK,ZZWK,dbuf,ibuf,level)
cmod20160721(e)
C----------------------------------------------------------2011.04 end

CD=== 概要 ===========================================================

CDT   VF_PMGSET:マルチグリッド環境の親子関係をチェックし設定する 
C     Check and set the parent-child relationship of the multigrid environment
C==== 宣言 ===========================================================

cadd20160727(s)
      use mod_apara,only: 
     $   MAKE_GGV_LIST,DELETE_GGV_LIST,SET_GGV_F,SET_GGV_UVW,
     $   MAKE_P2C_LIST,MAKE_C2P_LIST
cadd20160727(e)
C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)   : I/O : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : I/O : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : I/O : R*8 : z方向格子座標等
C----------------------------------------------------------2011.04 start
CD    GGV(@FOR-3D@)    : I/O : R*8 : 空隙率
CD    GGX(@FOR-3D@)    : I/O : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)    : I/O : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)    : I/O : R*8 : z方向面積透過率
CD    XPF(NUMI)        : O   : R*8 : 親の格子におけるx方向の補間係数  Interpolation coefficient in the x direction in the parent lattice
CD    YPF(NUMJ)        : O   : R*8 : 親の格子におけるy方向の補間係数
CD    ZPF(NUMK)        : O   : R*8 : 親の格子におけるz方向の補間係数
C----------------------------------------------------------2011.04 end
CD    NF(@FOR-3D@)     : I/O : I*4 : セルの状態を示すインデックス
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ  Buffer for parallel  ,MAXBUF在VF_A0PRM.h中被定义为常量
CD    IBUF(NUMBUF*MAXBUF) : OUT :I*4 : 並列用のバッファ  
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
C----------------------------------------------------------2011.04 start
      DIMENSION GGV (NUMI,NUMJ,NUMK),GGX (NUMI,NUMJ,NUMK)
      DIMENSION GGY (NUMI,NUMJ,NUMK),GGZ (NUMI,NUMJ,NUMK)
      DIMENSION XPF(NUMI),YPF(NUMJ),ZPF(NUMK)
C----------------------------------------------------------2011.04 end
      DIMENSION NF(NUMI,NUMJ,NUMK)
cadd20160721(s)
      DIMENSION XXWK(NUMI0),YYWK(NUMJ0),ZZWK(NUMK)
      DIMENSION DBUF(NUMBUF*MAXBUF),IBUF(NUMBUF*MAXBUF)  ! MAXBUF在VF_A0PRM.h中被定义为常量
      INTEGER,ALLOCATABLE:: JSW(:)  ! 局部变量
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: 
     $                                           GWRKF,GWRKU,GWRKV,GWRKW
      INTEGER NNF,NNU,NNV,NNW
cadd20160721(e)

CD    -- 局所変数 --
C----------------------------------------------------------2011.04 start
C     DIMENSION DDD(6),III(9)
      DIMENSION III(9)
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: XWRK,YWRK,ZWRK
cmod20160803(s)
      INTEGER,ALLOCATABLE:: nfwrk(:,:,:)
C----------------------------------------------------------2011.04 end
      ALLOCATE (nfwrk(NUMI,NUMJ,NUMK))
      nfwrk(:,:,:)=1  ! 全体初始化为1
cmod20160803(e)

C==== 実行 ===========================================================

CD    -- 自分の解析範囲 --
      XMS=XX(1,MYIS  )  ! MYIS属于VF_APARAI.h中的common块，并在VF_CPARA()中被设定为MYMIS+1
      XME=XX(1,MYIE+1)  ! 在VF_CPARA()中被设定为NUMI-MYMIE
      YMS=YY(1,MYJS  )
      YME=YY(1,MYJE+1)
      ZMS=ZZ(1,2     )
      ZME=ZZ(1,NUMK  )
cmod20160721(s)
      CALL VF_ZXMP_ALLMND(XMS,XMS1,IERR) ! 计算MGCOMM中各个进程组子集负责的计算分区的XMS的最小值，若存在多个父-子关系，则各个分区之间不互相影响
      CALL VF_ZXMP_ALLMND(YMS,YMS1,IERR)
      CALL VF_ZXMP_ALLMXD(XME,XME1,IERR) !计算最大值
      CALL VF_ZXMP_ALLMXD(YME,YME1,IERR)
      IF(MYRANK.NE.0) THEN  ! 只在MGCOMM中各个进程组子集中第一个进程中保存正确的值,影响第229行的执行
         XMS1=0.D0
         YMS1=0.D0
         XME1=0.D0
         YME1=0.D0
      ENDIF
      ALLOCATE(JSW(0:MGPROC-1),STAT=IERR)
      JSW(:)=0 ! 全体初始化为0
cmod20160721(e)
c      write(100+mgrank,'(a13,i5,i5)')     ' numi ,numj =',numi,numj
c      write(100+mgrank,'(a13,i5,i5)')     ' numi0,numj0=',numi0,numj0
c      write(100+mgrank,'(a13,f6.1,f6.1)') ' xms  ,xme  =',xms,xme
c      write(100+mgrank,'(a13,f6.1,f6.1)') ' yms  ,yme  =',yms,yme
c      write(100+mgrank,'(a13,f6.1,f6.1)') ' xms1 ,xme1 =',xms1,xme1
c      write(100+mgrank,'(a13,f6.1,f6.1)') ' yms1 ,yme1 =',yms1,yme1
c      write(100+mgrank,*) ''

CD    -- 親子関係をチェックし設定する --   这一部分是为当前进程设定与其他进程间的父-子关系
      MGPRNK=-1  ! 首先设定表示当前进程的父进程编号的MGPRNK=-1，无父进程的进程，MGPRNK会保持-1
      MGCNUM=0
      DO 140 IC=0,MGPROC-1
C       * 自分が子として親候補に情報を送り、結果をもらう send information to a parent candidate as a child and receive a result
        IF (MGPARE(IC+1).NE.0) THEN  ! 相当于跳过分配给最外层父分区的进程,若IC分区为STOC模型的一个子分区，也满足这个判断条件，但随后145行的判断条件不满足
          IF (MGRANK.EQ.IC) THEN     ! 即在这里并不向STOC进程发送作为子计算的信息
C----------------------------------------------------------2011.04 start
C           DDD(1)=XMS
C           DDD(2)=XME
C           DDD(3)=YMS
C           DDD(4)=YME
C           DDD(5)=ZMS
C           DDD(6)=ZME
C----------------------------------------------------------2011.04 end
            III(1)=MYIE-MYIS+1  ! 当前进程负责的范围在X,Y,Z方向上的单元数，不包括dummy cell与通讯层cell
            III(2)=MYJE-MYJS+1
            III(3)=NUMK-2
            III(4)=1   ! 除Z方向外先初始化为不通信
            III(5)=1
            III(6)=0
            III(7)=1
            III(8)=1
            III(9)=0
            IF (MYMIS.EQ.1) III(4)=0  ! 对于负责自身计算分区内 边缘处的进程，其X,Y方向上应设定为通信，这是由于其一层肯定存在父分区，因此一定存在父—子之间的信息交换
            IF (MYMJS.EQ.1) III(5)=0
            IF (MYMIE.EQ.1) III(7)=0
            IF (MYMJE.EQ.1) III(8)=0
C----------------------------------------------------------2011.04 start
            DO 10 I=MYIS,MYIE+1  ! XPF[]在主程序中被初始化为0.0
              XPF(I) = XX(1,I)  ! 记录了各个进程负责的区域的范围
 10         CONTINUE
            DO 11 J=MYJS,MYJE+1
              YPF(J) = YY(1,J)
 11         CONTINUE
            DO 12 K=2,NUMK
              ZPF(K) = ZZ(1,K)
 12         CONTINUE
C----------------------------------------------------------2011.04 end
C           * 親候補をさがして Find a parent candidate
            DO 100 IP=0,MGPROC-1  
              IF (MGPARE(IC+1).EQ.MGAREA(IP+1)) THEN   ! 为当前进程搜寻其父进程IP，只限在CADMAS进程部分搜寻，至于与STOC耦合的情况，并不在这部分执行。
C----------------------------------------------------------2011.04 start
C               CALL VF_ZXMG_ISENDD(DDD,6,IP,IREQ,IERR)
C               CALL VF_ZXMG_WAIT(IREQ,IERR)
C----------------------------------------------------------2011.04 end
                CALL VF_ZXMG_ISENDI(III,9,IP,IREQ,IERR) ! 从当前进程发送III至其父进程IP
                CALL VF_ZXMG_WAIT(IREQ,IERR)
C----------------------------------------------------------2011.04 start
                CALL VF_ZXMG_ISENDD(XPF(MYIS),III(1)+1,IP,IREQ,IERR) ! 从当前进程发送XPF[]至其父进程IP,注意从XPF(MYIS)元素开始发送
                CALL VF_ZXMG_WAIT(IREQ,IERR)
                CALL VF_ZXMG_ISENDD(YPF(MYJS),III(2)+1,IP,IREQ,IERR)
                CALL VF_ZXMG_WAIT(IREQ,IERR)
                CALL VF_ZXMG_ISENDD(ZPF(2),III(3)+1,IP,IREQ,IERR)
                CALL VF_ZXMG_WAIT(IREQ,IERR)
C----------------------------------------------------------2011.04 end
cmod20160726(s)
c                CALL VF_ZXMG_IRECVI(ISW,1,IP,IREQ,IERR)
                CALL VF_ZXMG_IRECVI(JSW(IP),1,IP,IREQ,IERR)  ! 从父进程IP中接收一个元素放置在JSW(IP处)
                CALL VF_ZXMG_WAIT(IREQ,IERR)
c                IF (ISW.NE.0) THEN
                IF (JSW(IP).NE.0) THEN  
cmod20160726(e)
                  IF (MGPRNK.GE.0)
     &              CALL VF_A2ERR('VF_PMGSET','SECOND TIMES (MGPRNK).')
                  MGPRNK=IP  !若当前进程有父进程，MGPRNK只会被赋值一次，记录的是其父分区局部标号为0的进程在comm_model中的标号,这是由于第  87 行的作用
                  CALL VF_ZXMG_IRECVI(MGPINF,9,IP,IREQ,IERR)
                  CALL VF_ZXMG_WAIT(IREQ,IERR)
C----------------------------------------------------------2011.04 start
                  CALL VF_ZXMG_IRECVD(XPF(MYIS),III(1)+1,IP,IREQ,IERR)
                  CALL VF_ZXMG_WAIT(IREQ,IERR)
                  CALL VF_ZXMG_IRECVD(YPF(MYJS),III(2)+1,IP,IREQ,IERR)
                  CALL VF_ZXMG_WAIT(IREQ,IERR)
                  CALL VF_ZXMG_IRECVD(ZPF(2),III(3)+1,IP,IREQ,IERR)
                  CALL VF_ZXMG_WAIT(IREQ,IERR)

cmod20160726(s)
c                  CALL VF_PMGGGT(GGV,GGX,GGY,GGZ,XPF,YPF,ZPF,NF,IP)
cmod20160726(e)
C----------------------------------------------------------2011.04 start
                ENDIF
              ENDIF
 100        CONTINUE
CDEBUG            IF (MGPRNK.LT.0)
CDEBUG     &              CALL VF_A2ERR('VF_PMGSET','NOT FOUND (MGPRNK).')
          ENDIF
C         * 自分が親の候補なら調べて、相手に結果を送る
          IF (MGPARE(IC+1).EQ.MGAREA(MGRANK+1)) THEN  ! If you are a candidate for your parent, check it and send the result to the other party
C----------------------------------------------------------2011.04 start 若IC进程负责的范围属于当前进程所在分区的子分区，即为当前进程寻找子进程
C           CALL VF_ZXMG_IRECVD(DDD,6,IC,IREQ,IERR)
C           CALL VF_ZXMG_WAIT(IREQ,IERR)
C----------------------------------------------------------2011.04 end
c      write(100+mgrank,*) 'vf_pmgset:irecvi level=',level
            CALL VF_ZXMG_IRECVI(III,9,IC,IREQ,IERR)
            CALL VF_ZXMG_WAIT(IREQ,IERR) ! 如果IREQ所指的操作已经完成，MPI_Wait将结束等待状态,用于等待上一步操作完成再继续执行
C----------------------------------------------------------2011.04 start
            NXC = III(1)  ! 局部变量
            NYC = III(2)
            NZC = III(3)
            ALLOCATE(XWRK(0:NXC),YWRK(0:NYC),ZWRK(0:NZC))
c      write(100+mgrank,*) 'vf_pmgset:irecvd level=',level
            CALL VF_ZXMG_IRECVD(XWRK,NXC+1,IC,IREQ,IERR)
            CALL VF_ZXMG_WAIT(IREQ,IERR)
            CALL VF_ZXMG_IRECVD(YWRK,NYC+1,IC,IREQ,IERR)
            CALL VF_ZXMG_WAIT(IREQ,IERR)
            CALL VF_ZXMG_IRECVD(ZWRK,NZC+1,IC,IREQ,IERR)
            CALL VF_ZXMG_WAIT(IREQ,IERR)
C----------------------------------------------------------2011.04 end
C----------------------------------------------------------2011.04 start
C           XCS=DDD(1)
C           XCE=DDD(2)
C           YCS=DDD(3)
C           YCE=DDD(4)
C           ZCS=DDD(5)
C           ZCE=DDD(6)
            XCS=XWRK(0) ! XCS,XCE记录的是某一个子进程的负责区域的坐标范围
            XCE=XWRK(NXC)
            YCS=YWRK(0)
            YCE=YWRK(NYC)
            ZCS=ZWRK(0)
            ZCE=ZWRK(NZC)
C----------------------------------------------------------2011.04 end
            ISW=0
C           * 子が自分に含まれるなら、自分が親   If a child is included in yourself
cmod20160721(s)
            IF ( XMS1.LE.XCS .AND. XCE.LE.XME1 .AND.   ! 若当前进程的某个子进程负责的范围被完全包含在当前进程所在的分区内
     &           YMS1.LE.YCS .AND. YCE.LE.YME1 .AND.   ! 只有各个分区中局部标号为0的进程运行这个IF块
     &           ZMS .LE.ZCS .AND. ZCE.LE.ZME ) THEN   ! 这里也包含了相等的情况
CDEBUG            IF (XMS.LE.XCS .AND. XCE.LE.XME .AND. 
CDEBUG     &          YMS.LE.YCS .AND. YCE.LE.YME .AND. 
CDEBUG     &          ZMS.LE.ZCS .AND. ZCE.LE.ZME      ) THEN
cmod20160721(e)
              ISW=1
              MGCNUM=MGCNUM+1
              MGCRNK(  MGCNUM)=IC
              MGCINF(1,MGCNUM)=III(1)
              MGCINF(2,MGCNUM)=III(2)
              MGCINF(3,MGCNUM)=III(3)
              MGCINF(4,MGCNUM)=III(4)
              MGCINF(5,MGCNUM)=III(5)
              MGCINF(6,MGCNUM)=III(6)
              MGCINF(7,MGCNUM)=III(7)
              MGCINF(8,MGCNUM)=III(8)
              MGCINF(9,MGCNUM)=III(9)
C             * イコール判定で座標値を比較(CAKIYC@@)
C----------------------------------------------------------2011.04 start
C             IS=0
C             IE=0
C             DO 110 I=MYIS,MYIE+1
C               IF (XX(1,I).EQ.XCS) IS=I
C               IF (XX(1,I).EQ.XCE) IE=I-1
C110          CONTINUE
C             JS=0
C             JE=0
C             DO 120 J=MYJS,MYJE+1
C               IF (YY(1,J).EQ.YCS) JS=J
C               IF (YY(1,J).EQ.YCE) JE=J-1
C120          CONTINUE
C             KS=0
C             KE=0
C             DO 130 K=2,NUMK
C               IF (ZZ(1,K).EQ.ZCS) KS=K
C               IF (ZZ(1,K).EQ.ZCE) KE=K-1
C130          CONTINUE
cmod20160721(s)
c              CALL VF_PMGSTS(XWRK,XX,IS,IE,NXC,NUMI,MYIS,MYIE)
c              CALL VF_PMGSTS(YWRK,YY,JS,JE,NYC,NUMJ,MYJS,MYJE)
c              CALL VF_PMGSTS(ZWRK,ZZ,KS,KE,NZC,NUMK,2,NUMK-1) 
c      write(100+mgrank,*) 'vf_pmgset:pmgsts level=',level
              CALL VF_PMGSTS(XWRK,XXWK,IS,IE,NXC,NUMI0,2,NUMI0-1)  ! IS,IE表示X方向上子进程负责的区域在父分区网格体系下的范围（以节点编号表示）
              CALL VF_PMGSTS(YWRK,YYWK,JS,JE,NYC,NUMJ0,2,NUMJ0-1)
              CALL VF_PMGSTS(ZWRK,ZZWK,KS,KE,NZC,NUMK,2,NUMK-1)
c             write(100+mgrank,*) 'is,ie,isize=',is,ie,IE-IS+1
c             write(100+mgrank,*) 'js,je,jsize=',js,je,JE-JS+1
c             write(100+mgrank,*) 'ks,ke,ksize=',ks,ke,KE-KS+1
cmod20160721(s)
C----------------------------------------------------------2011.04 end
              MGCPOS(1,MGCNUM)=IS  ! 将前边调用VF_PMGSTS()计算得到的IS,IE等设定至MGCPOS()中
              MGCPOS(2,MGCNUM)=JS
              MGCPOS(3,MGCNUM)=KS
              MGCPOS(4,MGCNUM)=IE
              MGCPOS(5,MGCNUM)=JE
              MGCPOS(6,MGCNUM)=KE
              III(1)=IE-IS+1
              III(2)=JE-JS+1
              III(3)=KE-KS+1
              III(4)=0
              III(5)=0
              III(6)=0
              III(7)=0
              III(8)=0
              III(9)=0
              IF (MGCINF(4,MGCNUM).NE.0) III(4)=1
              IF (MGCINF(5,MGCNUM).NE.0) III(5)=1
              IF (MGCINF(7,MGCNUM).NE.0) III(7)=1
              IF (MGCINF(8,MGCNUM).NE.0) III(8)=1
cmod20160721(s)
c              IF (XMS.EQ.XCS) III(4)=1
c              IF (YMS.EQ.YCS) III(5)=1
              IF (XMS1.EQ.XCS) III(4)=1  ! 处理父—子分区计算范围在六个方向上重合的情况，这种情况下
              IF (YMS1.EQ.YCS) III(5)=1  ! 对应方向上父-子分区不存在信息交换
              IF (ZMS.EQ.ZCS) III(6)=1
c              IF (XME.EQ.XCE) III(7)=1
c              IF (YME.EQ.YCE) III(8)=1
              IF (XME1.EQ.XCE) III(7)=1
              IF (YME1.EQ.YCE) III(8)=1
              IF (ZME.EQ.ZCE) III(9)=1
cmod20160721(e)
              MGCINF(4,MGCNUM)=III(4)
              MGCINF(5,MGCNUM)=III(5)
              MGCINF(6,MGCNUM)=III(6)
              MGCINF(7,MGCNUM)=III(7)
              MGCINF(8,MGCNUM)=III(8)
              MGCINF(9,MGCNUM)=III(9)
c             write(100+mgrank,*) 'iii(4:9)=',iii(4:9)
C             * 親側でセル数が２より小さい場合は両側境界のみ 
              IF (III(1).LT.2) THEN  ! 限制了一些情况 When the number of cells on the parent side is smaller than 2, only the boundary on both sides
                IF (III(4).NE.1 .OR. III(7).NE.1)
     &              CALL VF_A2ERR('VF_PMGSET','INVALID VALUE(POS:X).')
              ENDIF
              IF (III(2).LT.2) THEN
                IF (III(5).NE.1 .OR. III(8).NE.1)
     &              CALL VF_A2ERR('VF_PMGSET','INVALID VALUE(POS:Y).')
              ENDIF
              IF (III(3).LT.2) THEN
                IF (III(6).NE.1 .OR. III(9).NE.1)
     &              CALL VF_A2ERR('VF_PMGSET','INVALID VALUE(POS:Z).')
              ENDIF
C             * 親境界に接するか２セル離れてなければならない  It must touch the parent boundary or be separated by two cells
cmod20160721(s)
c              IF (III(4).NE.1 .AND. (IS-MYIS).LE.1)
              IF (III(4).NE.1 .AND. (IS-2   ).LE.1) ! 同样限制了一写情况
     &              CALL VF_A2ERR('VF_PMGSET','INVALID VALUE(POS:X-).')
c              IF (III(5).NE.1 .AND. (JS-MYJS).LE.1)
              IF (III(5).NE.1 .AND. (JS-2   ).LE.1)
     &              CALL VF_A2ERR('VF_PMGSET','INVALID VALUE(POS:Y-).')
              IF (III(6).NE.1 .AND. (KS-2   ).LE.1)
     &              CALL VF_A2ERR('VF_PMGSET','INVALID VALUE(POS:Z-).')
c              IF (III(7).NE.1 .AND. (MYIE-IE).LE.1)
              IF (III(7).NE.1 .AND. (NUMI0-IE).LE.2)
     &              CALL VF_A2ERR('VF_PMGSET','INVALID VALUE(POS:X+).')
c              IF (III(8).NE.1 .AND. (MYJE-JE).LE.1)
              IF (III(8).NE.1 .AND. (NUMJ0-JE).LE.2)
     &              CALL VF_A2ERR('VF_PMGSET','INVALID VALUE(POS:Y+).')
              IF (III(9).NE.1 .AND. (NUMK-KE).LE.2)
     &              CALL VF_A2ERR('VF_PMGSET','INVALID VALUE(POS:Z+).')
cmod20160721(e)
            ENDIF
c            write(100+mgrank,*) 'isw=',isw
            CALL VF_ZXMG_ISENDI(ISW,1,IC,IREQ,IERR)
            CALL VF_ZXMG_WAIT(IREQ,IERR)
            IF (ISW.NE.0) THEN
              CALL VF_ZXMG_ISENDI(III,9,IC,IREQ,IERR)
              CALL VF_ZXMG_WAIT(IREQ,IERR)
C----------------------------------------------------------2011.04 start
              CALL VF_ZXMG_ISENDD(XWRK,NXC+1,IC,IREQ,IERR)
              CALL VF_ZXMG_WAIT(IREQ,IERR)
              CALL VF_ZXMG_ISENDD(YWRK,NYC+1,IC,IREQ,IERR)
              CALL VF_ZXMG_WAIT(IREQ,IERR)
              CALL VF_ZXMG_ISENDD(ZWRK,NZC+1,IC,IREQ,IERR)
              CALL VF_ZXMG_WAIT(IREQ,IERR)

cmod20160726(s)
c              CALL VF_PMGGPT(GGV,GGX,GGY,GGZ,NF,IC,IS,JS,KS,IE,JE,KE)
cmod20160726(e)
C----------------------------------------------------------2011.04 end
            ENDIF
C----------------------------------------------------------2011.04 start
            DEALLOCATE(XWRK,YWRK,ZWRK)
C----------------------------------------------------------2011.04 end
          ENDIF
        ENDIF
 140  CONTINUE

c      write(100+mgrank,*) 'mgcnum = ',mgcnum
c      call VF_ZXMG_ABORT(IERR)
cadd20160726(s)
      CALL VF_ZXMP_BCASTI(ISW,1,0,IERR)  ! 广播至MGCOMM中同进程组子集中的其他进程中
      CALL VF_ZXMP_BCASTI(MGCNUM,1,0,IERR)
      ISIZE=6*MAX(MGCNUM,1)
      CALL VF_ZXMP_BCASTI(MGCPOS,ISIZE,0,IERR)
      ISIZE=9*MAX(MGCNUM,1)
      CALL VF_ZXMP_BCASTI(MGCINF,ISIZE,0,IERR)
C
      IF( MGCNUM.GT.0 ) THEN  ! 若当前进程存在子进程
CDEBUGC ...... 子供の全体領域の範囲
CDEBUG         ISCA=HUGE(1)
CDEBUG         JSCA=HUGE(1)
CDEBUG         IECA=0
CDEBUG         JECA=0
CDEBUG         KSCA=2
CDEBUG         KECA=NUMK-1
CDEBUG         DO N=1,MGCNUM
CDEBUG            ISCA=MIN(ISCA,MGCPOS(1,N))
CDEBUG            JSCA=MIN(JSCA,MGCPOS(2,N))
CDEBUG            IECA=MAX(IECA,MGCPOS(4,N))
CDEBUG            JECA=MAX(JECA,MGCPOS(5,N))
CDEBUG         ENDDO
CDEBUGC        ローカルインデックスに変換
CDEBUG         ISC=ISCA-MYGIS+1
CDEBUG         JSC=JSCA-MYGJS+1
CDEBUG         KSC=KSCA
CDEBUG         IEC=IECA-MYGIS+1
CDEBUG         JEC=JECA-MYGJS+1
CDEBUG         KEC=KECA
CDEBUG         write(100+mgrank,*) 'isc=',isc
CDEBUG         write(100+mgrank,*) 'iec=',iec
CDEBUG         write(100+mgrank,*) 'jsc=',jsc
CDEBUG         write(100+mgrank,*) 'jec=',jec
CDEBUGC
CDEBUGC ...... 自分の領域の範囲(ローカルインデックス)
CDEBUG         ISG=MYIS
CDEBUG         IEG=MYIE
CDEBUG         JSG=MYJS
CDEBUG         JEG=MYJE
CDEBUG         KSG=2
CDEBUG         KEG=NUMK-1
CDEBUG         CALL SET_REGION(ISC,JSC,KSC,IEC,JEC,KEC,
CDEBUG     $                   ISG,JSG,KSG,IEG,JEG,KEG,MYRANK)
CDEBUGC
C ...... 自分の領域の範囲(グローバルインデックス)
         ISG=MYGIS+MYMIS  ! ISG,IEG等表示当前进程(作为父进程)负责的范围(用节点编号表示),不计dummy cell与通讯层cell
         JSG=MYGJS+MYMJS  
         IEG=MYGIE-MYMIE
         JEG=MYGJE-MYMJE
         CALL MAKE_GGV_LIST(MGCPOS,MGCNUM,MYRANK,NPROCS,
     $                      ISG,JSG,IEG,JEG)
C
         IF( MYRANK.EQ.0 ) THEN  ! 只在MYRANK=0的父进程中执行
            NGWRKF = 0   ! 局部变量
            NGWRKU = 0
            NGWRKV = 0
            NGWRKW = 0
            DO N=1,MGCNUM
               IS=MGCPOS(1,N)
               JS=MGCPOS(2,N)
               KS=MGCPOS(3,N)
               IE=MGCPOS(4,N)
               JE=MGCPOS(5,N)
               KE=MGCPOS(6,N)
               NX = IE - IS + 1
               NY = JE - JS + 1
               NZ = KE - KS + 1  
               NGWRKF = NGWRKF + (NX*NY - MAX((NX-2)*(NY-2),0))*NZ *2   ! 只在MYRANK=0的进程中累加，其他进程中设定为1
               NGWRKU = NGWRKU + ((NX+1)*NY - MAX((NX-3)*(NY-2),0))*NZ
               NGWRKV = NGWRKV + (NX*(NY+1) - MAX((NX-2)*(NY-3),0))*NZ
               NGWRKW = NGWRKW + (NX*NY - MAX((NX-2)*(NY-2),0))*(NZ+1)
            ENDDO
         ELSE      ! 其他父进程设定为1
            NGWRKF=1
            NGWRKU=1
            NGWRKV=1
            NGWRKW=1
         ENDIF

         ALLOCATE(GWRKF(NGWRKF),GWRKU(NGWRKU),       ! 对于MYRANK!=0的进程,GWRKF()只被分配了一个大小
     $            GWRKV(NGWRKV),GWRKW(NGWRKW),STAT=IERR)

         CALL SET_GGV_F(GWRKF,NGWRKF,ggv,nf,numi,numj,numk,   ! 设定父--->子进程中需传递的GGV 与 NF 数据，每个父进程都调用
     $                  myrank,nprocs,MYGIS,MYGJS)
c         write(100+mgrank,*) 'gather u start'
c      call vf_zxmp_barri(ierr)
         CALL set_ggv_uvw(GWRKU,NGWRKU,GGX,numi,numj,numk,
     $                    1,myrank,nprocs,MYGIS,MYGJS)
c         write(100+mgrank,*) 'gather v start'
c      call vf_zxmp_barri(ierr)
         CALL set_ggv_uvw(GWRKV,NGWRKV,GGY,numi,numj,numk,
     $                    2,myrank,nprocs,MYGIS,MYGJS)
c         write(100+mgrank,*) 'gather w start'
c      call vf_zxmp_barri(ierr)
         CALL set_ggv_uvw(GWRKW,NGWRKW,GGZ,numi,numj,numk,
     $                    3,myrank,nprocs,MYGIS,MYGJS)
         CALL delete_ggv_list(MYRANK,NPROCS)  ! 每个进程都调用
c     check
c         if( myrank==0 ) then
c            write(100+mgrank,*) 'min gwrkf=',minval(gwrkf)
c            write(100+mgrank,*) 'min gwrku=',minval(gwrku)
c            write(100+mgrank,*) 'min gwrkv=',minval(gwrkv)
c            write(100+mgrank,*) 'min gwrkw=',minval(gwrkw)
c         endif
      ENDIF
C
      M=0
      NNF=0
      NNU=0
      NNV=0
      NNW=0
C
      DO 150 IC=0,MGPROC-1  ! comm_model中所有进程循环
C       * 自分が子として親候補に情報を送り、結果をもらう  I send information to a parent candidate as a child and receive a result
        IF (MGPARE(IC+1).NE.0) THEN  ! 如果进程IC有父进程，若IC为STOC模型的一个子分区，也满足这个判断条件，但此时JSW()全部为0，影响498行的执行
          IF (MGRANK.EQ.IC) THEN
C           * 親候補をさがして  Find a parent candidate
            DO 160 IP=0,MGPROC-1
              IF (JSW(IP).NE.0) THEN ! 找到当前进程的父分区MYRANK=0的进程IP
                 if( LEVEL.eq.1 )
     $           CALL VF_PMGGGT(GGV,GGX,GGY,GGZ,XPF,YPF,ZPF,NF,IP)    ! 每一次调用将接受并设定一个子进程的GGV GGX GGY GGZ 
                 if( level.eq.2 )
     $           CALL VF_PMGGGT(GGV,GGX,GGY,GGZ,XPF,YPF,ZPF,nfwrk,IP) ! nfwrk为当前子过程中的局部变量,当以LEVEL=2模式调用时，由于单元的孔隙属性
              ENDIF                                                  ! 在第三次调用时才读入并设置，故LEVEL=2模式下应重新将父网格的GGV等信息
 160        CONTINUE                                                 ! 传递给子分区
          ENDIF
C         * 自分が親の候補なら調べて、相手に結果を送る
          IF (MGPARE(IC+1).EQ.MGAREA(MGRANK+1)) THEN  ! 判断当前进程是不是IC的父进程
            IF (ISW.NE.0.and.MYRANK.eq.0) THEN  ! 只在MYRANK=0中储存了要向子分区传递的信息
               M=M+1  
               IS=MGCPOS(1,M)  
               JS=MGCPOS(2,M)
               KS=MGCPOS(3,M)
               IE=MGCPOS(4,M)
               JE=MGCPOS(5,M)
               KE=MGCPOS(6,M)
C
               CALL VF_PMGGPT(GWRKF,GWRKU,GWRKV,GWRKW,NNF,NNU,NNV,NNW,  !依次找到当前进程的各个子进程，并执行VF_PMGGPT()
     $               IC,IS,JS,KS,IE,JE,KE,NGWRKF,NGWRKU,NGWRKV,NGWRKW)   ! 每一次调用将一次发送GGV GGX GGY GGZ
            ENDIF
          ENDIF
        ENDIF
 150  CONTINUE
c
      IF( MGCNUM.GT.0 ) THEN
         DEALLOCATE(GWRKF,GWRKU,GWRKV,GWRKW,STAT=IERR)
      ENDIF
cadd20160726(e)

CD    -- 子の領域を障害物セルにする(Z方向は完全に抜く) 设定父分区中和子分区重叠的部分的NF()，这里必须要在LEVEL=1模式下运行
cmod20160803(s)                                               ! 因为子分区向父分区传递交界面信息的单元表面也会被定义为边界
      if(level.eq.1)then
cmod20160803(e)
      DO 230 IC=1,MGCNUM   ! 若当前进程存在子进程，则运行循环块
        IS=MGCPOS(1,IC)
        JS=MGCPOS(2,IC)
        IE=MGCPOS(4,IC)
        JE=MGCPOS(5,IC)
cadd20160729(s)
c ..... ローカルインデックスに変換  Convert to local index
        IS=IS-MYGIS+1
        JS=JS-MYGJS+1
        IE=IE-MYGIS+1
        JE=JE-MYGJS+1
        ISG=MYIS
        IEG=MYIE
        JSG=MYJS
        JEG=MYJE
cadd20160729(e)
C
c ..... 接続する場合は、1層分内側から障害物にする
        IF (MGCINF(4,IC).EQ.0) IS=IS+1
        IF (MGCINF(5,IC).EQ.0) JS=JS+1
        IF (MGCINF(7,IC).EQ.0) IE=IE-1
        IF (MGCINF(8,IC).EQ.0) JE=JE-1
c        write(mgrank+100,*) 'nf set: is,ie=',is,ie
c        write(mgrank+100,*) 'nf set: js,je=',js,je
        DO 220 K=1,NUMK     !!!!!! Z 方向上包括dummy cell 层都被设定
          DO 210 J=JS,JE
            DO 200 I=IS,IE
cmod20160729(s)
              IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $            JSG.LE.J.AND.J.LE.JEG ) THEN
              NF(I,J,K)=-1
              ENDIF
cmod20160729(e)
 200        CONTINUE
 210      CONTINUE
 220    CONTINUE
 230  CONTINUE
cmod20160803(s)
      endif
c
      deallocate(nfwrk)
cmod20160803(e)

C     -- 実行文の終了 --
      GOTO 9999   

C==== 終了 ===========================================================

 9999 CONTINUE
cmod20160803(s)
      if(level.eq.2)then   !LEVEL=2模式下才执行
         if( MGCNUM.GT.0 ) then  ! 存在子进程时才运行,这一部分相当于计算父进程需要发送的以及从子进程接收的边界面信息
            ISG=MYGIS+MYMIS  !相当于父进程的范围，不包括MPI通讯层单元以及dummy单元
            JSG=MYGJS+MYMJS
            IEG=MYGIE-MYMIE
            JEG=MYGJE-MYMJE
C
            MGNV=9
            call make_p2c_list(MGCPOS,MGCINF,MGCNUM,MYRANK,NPROCS,
     $                         ISG,JSG,IEG,JEG,MGNV)
c
            MGNV=4 !!! 与上边不同
            call make_c2p_list(MGCPOS,MGCINF,MGCNUM,MYRANK,NPROCS,
     $                         ISG,JSG,IEG,JEG,MGNV)
         endif
      endif
cmod20160803(e)
c      write(100+mgrank,*) 'exit vf_pmgset. level=',level
cmod20170426(s)
      IF( MGPRNK.GE.0 ) THEN  ! 如果当前进程有父进程，为什么只在有父进程的进程中执行？ 应该是由于子分区根据父分区重新设定一些GGV,NF等，故需重新更新MPI通讯层，而父分区不需要根据子分区设定这类变量
         IF(LEVEL.EQ.1)THEN
            CALL VF_P3SRI2(NF,IBUF,0)  ! IBUF()在主程序中被定义并被分配空间，之所以在LEVEL=1时不对孔隙介质属性GGV等进行操作，是由于LEVEL=2时才读入各个单元的孔隙率
         ELSEIF(LEVEL.EQ.2)THEN  
            CALL VF_P3SRD2(GGV,DBUF,0)  ! 由于在LEVEL=2的模式下孔隙率有关参数才被从.IN文件中读入，所以在LEVEL=2的模式下，需要设定各进程需要的通讯层信息
            CALL VF_P3SRD2(GGX,DBUF,1)
            CALL VF_P3SRD2(GGY,DBUF,2)
            CALL VF_P3SRD2(GGZ,DBUF,3)
         ENDIF
      ENDIF
cmod20170426(e)



      RETURN
      END
