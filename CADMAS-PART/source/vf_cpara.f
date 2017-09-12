      SUBROUTINE VF_CPARA()

CD=== 概要 ===========================================================

CDT   VF_CPARA:並列制御データをチェックし設定する  Check and set parallel control data

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

C==== 実行 ===========================================================

CD    -- 端の値を設定 --
      NUMNPI=NUMNPI+1   ! 此时NUMNPI=X方向被划分的块数
      NUMNPJ=NUMNPJ+1
c      write(100+mgrank,*) 'numnpi=',NUMNPI
c      write(100+mgrank,*) 'numnpj=',NUMNPJ
c      write(100+mgrank,*) ''
      IPROCS(0     )=0  ! 设定IPROCS()的首尾元素
      JPROCS(0     )=0
      IPROCS(NUMNPI)=NUMI0-2
      JPROCS(NUMNPJ)=NUMJ0-2

CD    -- 値をシフトしチェック -- shift value and check
      DO 100 I=0,NUMNPI   ! shift value by +1,相当于得到MPI各个分区范围的结束节点号
        IPROCS(I)=IPROCS(I)+1
 100  CONTINUE
      DO 110 J=0,NUMNPJ
        JPROCS(J)=JPROCS(J)+1
 110  CONTINUE

      IF (NUMNPI.GT.1) THEN
        DO 120 I=1,NUMNPI
          IF (IPROCS(I)-IPROCS(I-1).LT.2)  ! CHECK 每一个MPI分区应至少分配两个网格
     &                      CALL VF_A2ERR('VF_CPARA','2 > IC2-IC1.')
 120    CONTINUE
      ENDIF
      IF (NUMNPJ.GT.1) THEN
        DO 130 J=1,NUMNPJ
          IF (JPROCS(J)-JPROCS(J-1).LT.2)
     &                      CALL VF_A2ERR('VF_CPARA','2 > JC2-JC1.')
 130    CONTINUE
      ENDIF

c      do i=0,numnpi
c         write(100+mgrank,*) 'iprocs(',i,')=',iprocs(i)
c      enddo
c      do j=0,numnpj
c         write(100+mgrank,*) 'jprocs(',j,')=',jprocs(j)
c      enddo
c      write(100+mgrank,*) ''

CD    -- プロセス数との整合性をチェック --
      IF (NPROCS.NE.NUMNPI*NUMNPJ) ! 检查与当前分区被分配的进程数之间的一致性
     &                CALL VF_A2ERR('VF_CPARA','NPROCS <> NPI*NPJ.')

CD    -- プロセスの位置 -- 为每个进程分配其负责的区域，用MYRI,MYRJ指定，X方向优先，再Y方向
      L=0
      DO 210 J=1,NUMNPJ
        DO 200 I=1,NUMNPI
          IF (L.EQ.MYRANK) THEN
            MYRI=I
            MYRJ=J
          ENDIF
          L=L+1
 200    CONTINUE
 210  CONTINUE

CD    -- プロセス単位の格子数等の設定 -- Setting number of grids etc. per process
      MYGIS=IPROCS(MYRI-1)+1  ! MYGIS表示X方向上
      MYGIE=IPROCS(MYRI  )
      MYGJS=JPROCS(MYRJ-1)+1
      MYGJE=JPROCS(MYRJ  )
      NUMI =MYGIE-MYGIS+1 ! 到此得到的NUMI,NUMJ为各个进程在X,Y方向上负责的网格数目
      NUMJ =MYGJE-MYGJS+1
      MYMIS=2  ! 用于考虑不同进程间通信用
      MYMIE=2
      MYMJS=2
      MYMJE=2
      IF (MYGIS.EQ.2      ) MYMIS=1
      IF (MYGIE.EQ.NUMI0-1) MYMIE=1
      IF (MYGJS.EQ.2      ) MYMJS=1
      IF (MYGJE.EQ.NUMJ0-1) MYMJE=1
      NUMI =NUMI+MYMIS+MYMIE  ! 最终的NUMI记录了考虑MPI不同分区间通信时在X方向该进程负责的网格单元数目
      NUMJ =NUMJ+MYMJS+MYMJE  ! Y方向
      MYGIS=MYGIS-MYMIS       ! 考虑了MPI不同分区间通信的情况，对之前的MYGIS进行调整，在X方向上向外侧扩大两个网格的范围（边界处特殊）
      MYGIE=MYGIE+MYMIE
      MYGJS=MYGJS-MYMJS
      MYGJE=MYGJE+MYMJE
      MYIS =MYMIS+1
      MYIE =NUMI-MYMIE
      MYJS =MYMJS+1
      MYJE =NUMJ-MYMJE

c      write(100+mgrank,*) 'NUMI       :',NUMI
c      write(100+mgrank,*) 'NUMJ       :',NUMJ
c      write(100+mgrank,*) 'MYGIS,MYGIE:',MYGIS,MYGIE
c      write(100+mgrank,*) 'MYGJS,MYGJE:',MYGJS,MYGJE
c      write(100+mgrank,*) 'MYIS ,MYIE :',MYIS,MYIE
c      write(100+mgrank,*) 'MYJS ,MYJE :',MYJS,MYJE
c      write(100+mgrank,*) 'MYMIS,MYMIE:',MYMIS,MYMIE
c      write(100+mgrank,*) 'MYMJS,MYMJE:',MYMJS,MYMJE
c      write(100+mgrank,*) ''

CD    -- バッファ用データの数(1本分)の設定 --
C     NUMBUF=0
C     IF (NUMNPI.GE.2) THEN
C       IF (NUMBUF.LT.NUMJ) NUMBUF=NUMJ
C     ENDIF
C     IF (NUMNPJ.GE.2) THEN
C       IF (NUMBUF.LT.NUMI) NUMBUF=NUMI
C     ENDIF
C     IF (NUMBUF.EQ.0) THEN
C       NUMBUF=1
C     ELSE
C       NUMBUF=NUMBUF*NUMK
C     ENDIF
      IF (NUMI.GT.NUMJ) THEN
        NUMBUF=NUMI*NUMK
      ELSE
        NUMBUF=NUMJ*NUMK
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
