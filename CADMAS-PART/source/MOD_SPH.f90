      MODULE mod_sph
        IMPLICIT NONE
        PRIVATE
        PUBLIC :: GREIS,GREIE,GREJS,GREJE,GREKS,GREKE
        PUBLIC :: BREIS,BREIE,BREJS,BREJE,BREKS,BREKE
        PUBLIC :: BRE,GRE
        PUBLIC :: CNUMGRE,CNUMBRE


        INTEGER GREIS,GREIE,GREJS,GREJE,GREKS,GREKE
        INTEGER BREIS,BREIE,BREJS,BREJE,BREKS,BREKE
        INTEGER BRE,GRE
        INTEGER CNUMGRE,CNUMBRE
!!!!!!   GREIS, BREISϵ�зֱ����ڼ�¼ghost region �� buffer region �ķ�Χ,Ŀǰֻ������X�����ϻ���,Y��Z���򲻻���
!!!!!!   BRE,GRE�ֱ����ڼ�¼��ǰ���������Ƿ����ghost region ��  buffer region, 0��������   1 : ����
!!!!!!   CNUMGRE ���ڼ�¼���������а�����λ��ghost region�����ڵĵ�Ԫ����(������ͨѶ�㵥Ԫ��dummy cell)
!!!!!!   CNUMBRE ���ڼ�¼���������а�����λ��buffer region ...
!!!!!!   CNUMGRE �� CNUMBRE �����ڸ����������ۼӣ������Ŀ�Ƿ���ȷ









      END MODULE 