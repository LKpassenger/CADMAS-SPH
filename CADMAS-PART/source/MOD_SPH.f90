      MODULE mod_sph
        IMPLICIT NONE
        PRIVATE
        PUBLIC :: GREIS,GREIE,GREJS,GREJE,GREKS,GREKE
        PUBLIC :: BREIS,BREIE,BREJS,BREJE,BREKS,BREKE
        PUBLIC :: BRE,GRE
        PUBLIC :: CNUMGRE,CNUMBRE
        PUBLIC :: NB_SPH,CADM_PN,SPH_PN
        PUBLIC :: LB_CADM,IB_CADM,LB_SPH,IB_SPH
        PUBLIC :: DELETE_ARRAY_CWSPH


        INTEGER GREIS,GREIE,GREJS,GREJE,GREKS,GREKE
        INTEGER BREIS,BREIE,BREJS,BREJE,BREKS,BREKE
        INTEGER BRE,GRE
        INTEGER CNUMGRE,CNUMBRE
        INTEGER NB_SPH,CADM_PN,SPH_PN
        INTEGER LB_CADM,LB_SPH
        INTEGER,ALLOCATABLE :: IB_CADM(:),IB_SPH(:)
!!!!!!   GREIS, BREISϵ�зֱ����ڼ�¼ghost region �� buffer region �ķ�Χ,Ŀǰֻ������X�����ϻ���,Y��Z���򲻻���
!!!!!!   BRE,GRE�ֱ����ڼ�¼��ǰ���������Ƿ����ghost region ��  buffer region, 0��������   1 : ����
!!!!!!   CNUMGRE ���ڼ�¼���������а�����λ��ghost region�����ڵĵ�Ԫ����(������ͨѶ�㵥Ԫ��dummy cell)
!!!!!!   CNUMBRE ���ڼ�¼���������а�����λ��buffer region ...
!!!!!!   CNUMGRE �� CNUMBRE �����ڸ����������ۼӣ������Ŀ�Ƿ���ȷ

!!!!!!   NB_SPH  ���Լ�¼��ǰ�����Ƿ���� ��SPHģ�͵���Ϣ����
!!!!!!   CADM_PN,SPH_PN �ֱ��¼����CADMAS&SPH��Ϣ������CADMAS�������Լ�SPH������
!!!!!!   IB_CADM(),LB_CADM���ڼ�¼��ǰ������comm_mg_isphͨѶ���еĽ��̱��(CADMAS ����)
!!!!!!   IB_SPH(),LB_SPH���ڼ�¼��ǰ������comm_mg_isphͨѶ���еĽ��̱��(SPH ����)

      CONTAINS
        SUBROUTINE DELETE_ARRAY_CWSPH
!!!!!!  �ͷŷ��������ռ� delete array used in coupling with SPH
          INTEGER IERR
          DEALLOCATE( IB_CADM,STAT=IERR )
          DEALLOCATE( IB_SPH,STAT=IERR )

        END SUBROUTINE

      END MODULE