      SUBROUTINE VF_SPH_INITMPI(IERR)
C     ���Գ�ʼ��CADMAS��SPHģ�͵�MPIͨѶ����  add by LK
      USE mod_sph, ONLY: NB_SPH,CADM_PN,SPH_PN,
     &                    IB_CADM,LB_CADM,IB_SPH,LB_SPH
      USE mod_comm, ONLY: nrank_all,comm_work_mg_isph,comm_mg_isph

      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'mpif.h'
C      INCLUDE 'VF_A0PRM.h'
C      INCLUDE 'VF_AFILEI.h' !!! �����ã���list�ļ������Ϣ�����Ϣ
C      INCLUDE 'VF_APARAI.h' !!! �����ã����MGRANK�Լ�MYRANK,�ǵ�ɾ��

      INTEGER IERR

C=====��ʱ����======
      INTEGER isize,irank,iflg1,iflg2
      INTEGER iwork0,m,n
      INTEGER,ALLOCATABLE :: iwork(:)


      CALL MPI_COMM_SPLIT(comm_work_mg_isph,NB_SPH,nrank_all,
     &                    comm_mg_isph,IERR)

C      WRITE(ILPFIL,"(' ','MGRANK=',I3,'   MYRANK=',I3,'   NB_SPH=',I3)")
C     &                 MGRANK,MYRANK,NB_SPH     !!! �����ã��ǵ�ע�͵�
C      IF( NB_SPH.EQ.0) THEN    !!! ������,�ǵ�ע�͵�
C        CALL MPI_COMM_SIZE(comm_mg_isph,isize,IERR)
C        WRITE(ILPFIL,"(' ','ISIZE=',I3)") isize
C      ENDIF

      IF( NB_SPH.EQ.0 ) RETURN  ! ������CADMAS&SPH��Ϣ�����Ľ�������
      
      CALL MPI_COMM_SIZE(comm_mg_isph,isize,IERR)   !!! comm_mg_isphͨѶ���еĽ��������Լ����̱��
      CALL MPI_COMM_RANK(comm_mg_isph,irank,IERR)
C      WRITE(ILPFIL,"(' ','ISIZE=',I3,'   IRANK in comm_mg_sph=',I3)")
C     &                 isize,irank
      iflg1=0   ! CADMAS���ֵĽ���iflg1�趨Ϊ0,SPH���ֵĽ���iflg1�趨Ϊ1
      CALL MPI_ALLREDUCE(iflg1,iflg2,1,MPI_INTEGER,
     &                   MPI_SUM,comm_mg_isph,IERR)    !!! ��iflg1���й�Լ
      SPH_PN  =iflg2
      CADM_PN =isize-SPH_PN
C      WRITE(ILPFIL,"(' ','SIZE OF CADMAS PART=',I3,
C     &      '   SIZE OF CADMAS PART=',I3)")   CADM_PN,SPH_PN   !!! ������,�ǵ�ע�͵�

      ALLOCATE(IB_CADM(CADM_PN))
      ALLOCATE(iwork(isize))
      CALL VF_ZSETI1(IB_CADM,-1,CADM_PN)
      CALL VF_ZSETI1(iwork,0,isize)
C=====IB_CADM(),LB_CADM�趨
      iwork0=irank  !!! SPH���ֵĽ���iwork0Ӧ�趨ΪС�����ֵ
      CALL MPI_ALLGATHER(iwork0,1,MPI_INTEGER,
     $                   iwork,1,MPI_INTEGER,comm_mg_isph,IERR)
      m = 0
      LB_CADM = 0
      DO n=1,isize
         IF( iwork(n).GE.0 ) THEN
            m = m + 1
            IB_CADM(m) = iwork(n)  !�趨IB_CADMAS(M)
            IF( irank.EQ.iwork(n) ) LB_CADM = m
         ENDIF
      ENDDO
      IF( CADM_PN.NE.m ) CALL VF_A2ERR('VF_SPH_INIT',
     &                   'Procs Num of CADMAS_PART is not coincident')
C      WRITE(ILPFIL,"(' ','IB_CADM(:):   ',20(I3,I3,/,14X))")
C     &       (n,IB_CADM(n),n=1,CADM_PN)   !!! �����ã��ǵ�ע�͵�
C      WRITE(ILPFIL,"(' ','LB_CADM=',I3)") LB_CADM

C=====IB_SPH(),LB_SPH�趨
      ALLOCATE(IB_SPH(SPH_PN))
      CALL VF_ZSETI1(IB_SPH,-1,SPH_PN )      !!!��ʼ��Ϊ-1
      iwork0=-1   !!! SPH���ֵĽ���iwork0Ӧ�趨Ϊ����̱�� in comm_mg_isph
      CALL VF_ZSETI1(iwork,0,isize )
      CALL MPI_ALLGATHER(iwork0,1,MPI_INTEGER,
     &                   iwork,1,MPI_INTEGER,comm_mg_isph,IERR)
      m = 0
      LB_SPH = 0
      DO n=1,isize
         IF( iwork(n).GE.0 ) THEN  ! IWORK0 = -1,����
            m = m + 1
            IB_SPH(m) = iwork(n)   
            IF( irank.EQ.iwork(n) ) LB_SPH = m !!! CADMAS���ֵĽ�����ִ��ʱ��LB_SPHʼ�ձ���Ϊ0
         ENDIF                                 !!! IB_SPH()��¼������ȷֵ
      ENDDO
      IF( SPH_PN.NE.m ) CALL VF_A2ERR('VF_SPH_INITMPI',
     &                   'Procs Num of SPH_PART is not coincident')
C      WRITE(ILPFIL,"(' ','IB_SPH(:):   ',20(I3,I3,/,14X))")
C     &       (n,IB_SPH(n),n=1,SPH_PN)   !!! �����ã��ǵ�ע�͵�
C      WRITE(ILPFIL,"(' ','LB_SPH=',I3)") LB_SPH

      RETURN
      END SUBROUTINE