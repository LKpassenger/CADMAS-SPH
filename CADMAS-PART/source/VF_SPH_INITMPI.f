      SUBROUTINE VF_SPH_INITMPI(IERR)
C     用以初始化CADMAS与SPH模型的MPI通讯环境  add by LK
      USE mod_sph, ONLY: NB_SPH,CADM_PN,SPH_PN,
     &                    IB_CADM,LB_CADM,IB_SPH,LB_SPH
      USE mod_comm, ONLY: nrank_all,comm_work_mg_isph,comm_mg_isph

      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'mpif.h'
C      INCLUDE 'VF_A0PRM.h'
C      INCLUDE 'VF_AFILEI.h' !!! 测试用，向list文件输出信息检测信息
C      INCLUDE 'VF_APARAI.h' !!! 测试用，输出MGRANK以及MYRANK,记得删除

      INTEGER IERR

C=====临时变量======
      INTEGER isize,irank,iflg1,iflg2
      INTEGER iwork0,m,n
      INTEGER,ALLOCATABLE :: iwork(:)


      CALL MPI_COMM_SPLIT(comm_work_mg_isph,NB_SPH,nrank_all,
     &                    comm_mg_isph,IERR)

C      WRITE(ILPFIL,"(' ','MGRANK=',I3,'   MYRANK=',I3,'   NB_SPH=',I3)")
C     &                 MGRANK,MYRANK,NB_SPH     !!! 测试用，记得注释掉
C      IF( NB_SPH.EQ.0) THEN    !!! 测试用,记得注释掉
C        CALL MPI_COMM_SIZE(comm_mg_isph,isize,IERR)
C        WRITE(ILPFIL,"(' ','ISIZE=',I3)") isize
C      ENDIF

      IF( NB_SPH.EQ.0 ) RETURN  ! 不参与CADMAS&SPH信息交换的进程跳出
      
      CALL MPI_COMM_SIZE(comm_mg_isph,isize,IERR)   !!! comm_mg_isph通讯子中的进程总数以及进程标号
      CALL MPI_COMM_RANK(comm_mg_isph,irank,IERR)
C      WRITE(ILPFIL,"(' ','ISIZE=',I3,'   IRANK in comm_mg_sph=',I3)")
C     &                 isize,irank
      iflg1=0   ! CADMAS部分的进程iflg1设定为0,SPH部分的进程iflg1设定为1
      CALL MPI_ALLREDUCE(iflg1,iflg2,1,MPI_INTEGER,
     &                   MPI_SUM,comm_mg_isph,IERR)    !!! 对iflg1进行归约
      SPH_PN  =iflg2
      CADM_PN =isize-SPH_PN
C      WRITE(ILPFIL,"(' ','SIZE OF CADMAS PART=',I3,
C     &      '   SIZE OF CADMAS PART=',I3)")   CADM_PN,SPH_PN   !!! 测试用,记得注释掉

      ALLOCATE(IB_CADM(CADM_PN))
      ALLOCATE(iwork(isize))
      CALL VF_ZSETI1(IB_CADM,-1,CADM_PN)
      CALL VF_ZSETI1(iwork,0,isize)
C=====IB_CADM(),LB_CADM设定
      iwork0=irank  !!! SPH部分的进程iwork0应设定为小于零的值
      CALL MPI_ALLGATHER(iwork0,1,MPI_INTEGER,
     $                   iwork,1,MPI_INTEGER,comm_mg_isph,IERR)
      m = 0
      LB_CADM = 0
      DO n=1,isize
         IF( iwork(n).GE.0 ) THEN
            m = m + 1
            IB_CADM(m) = iwork(n)  !设定IB_CADMAS(M)
            IF( irank.EQ.iwork(n) ) LB_CADM = m
         ENDIF
      ENDDO
      IF( CADM_PN.NE.m ) CALL VF_A2ERR('VF_SPH_INIT',
     &                   'Procs Num of CADMAS_PART is not coincident')
C      WRITE(ILPFIL,"(' ','IB_CADM(:):   ',20(I3,I3,/,14X))")
C     &       (n,IB_CADM(n),n=1,CADM_PN)   !!! 测试用，记得注释掉
C      WRITE(ILPFIL,"(' ','LB_CADM=',I3)") LB_CADM

C=====IB_SPH(),LB_SPH设定
      ALLOCATE(IB_SPH(SPH_PN))
      CALL VF_ZSETI1(IB_SPH,-1,SPH_PN )      !!!初始化为-1
      iwork0=-1   !!! SPH部分的进程iwork0应设定为其进程编号 in comm_mg_isph
      CALL VF_ZSETI1(iwork,0,isize )
      CALL MPI_ALLGATHER(iwork0,1,MPI_INTEGER,
     &                   iwork,1,MPI_INTEGER,comm_mg_isph,IERR)
      m = 0
      LB_SPH = 0
      DO n=1,isize
         IF( iwork(n).GE.0 ) THEN  ! IWORK0 = -1,跳过
            m = m + 1
            IB_SPH(m) = iwork(n)   
            IF( irank.EQ.iwork(n) ) LB_SPH = m !!! CADMAS部分的进程在执行时，LB_SPH始终保持为0
         ENDIF                                 !!! IB_SPH()记录的是正确值
      ENDDO
      IF( SPH_PN.NE.m ) CALL VF_A2ERR('VF_SPH_INITMPI',
     &                   'Procs Num of SPH_PART is not coincident')
C      WRITE(ILPFIL,"(' ','IB_SPH(:):   ',20(I3,I3,/,14X))")
C     &       (n,IB_SPH(n),n=1,SPH_PN)   !!! 测试用，记得注释掉
C      WRITE(ILPFIL,"(' ','LB_SPH=',I3)") LB_SPH

      RETURN
      END SUBROUTINE