      SUBROUTINE VF_IISPH(IS,IE,NWD,TEXT)
      USE mod_sph, ONLY: GREIS,GREIE,GREJS,GREJE,GREKS,GREKE,
     &                    BREIS,BREIE,BREJS,BREJE,BREKS,BREKE,
     &                    BRE,GRE,
     &                    CNUMGRE,CNUMBRE
      IMPLICIT INTEGER(I-N), DOUBLE PRECISION(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_AFILEI.h'   !!!����ÿ�������Ƿ�����ȷ����SPH������жϸ��Խ����Ƿ����ghost region �Լ� buffer region

      DIMENSION IS(MAXWDS),IE(MAXWDS)
      CHARACTER*(MAXCHR) TEXT
!!!!!!   IS(MAXWDS) : IN  : I*4   : ��N�����ʿ�ʼ��λ��
!!!!!!   IE(MAXWDS) : IN  : I*4   : ��N�����ʽ�����λ��
!!!!!!   NWD        : IN  : I*4   : һ�������е��ʵĸ���
!!!!!!   TEXT       : IN  : C*(*) : �����һ������
      

!=====ʵ��==========================================================================
      IF (TEXT(IS(2):IE(2)).EQ.'REGION') THEN
        IF (TEXT(IS(3):IE(3)).EQ.'GHOST') THEN
          !!��¼������GHOST REGION����ķ�Χ
            CALL VF_ZSTOI(GREIS,TEXT(IS(4):IE(4)))
            CALL VF_ZSTOI(GREIE,TEXT(IS(5):IE(5)))
            CALL VF_ZSTOI(GREJS,TEXT(IS(6):IE(6)))
            CALL VF_ZSTOI(GREJE,TEXT(IS(7):IE(7)))
            CALL VF_ZSTOI(GREKS,TEXT(IS(8):IE(8)))
            CALL VF_ZSTOI(GREKE,TEXT(IS(9):IE(9)))
            GREIS=GREIS+1
            GREIE=GREIE
            GREJS=GREJS+1
            GREJE=GREJE 
            GREKS=GREKS+1
            GREKE=GREKE
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'BUFFER') THEN
          !!��¼������BUFFER REGION����ķ�Χ
            CALL VF_ZSTOI(BREIS,TEXT(IS(4):IE(4)))
            CALL VF_ZSTOI(BREIE,TEXT(IS(5):IE(5)))
            CALL VF_ZSTOI(BREJS,TEXT(IS(6):IE(6)))
            CALL VF_ZSTOI(BREJE,TEXT(IS(7):IE(7)))
            CALL VF_ZSTOI(BREKS,TEXT(IS(8):IE(8)))
            CALL VF_ZSTOI(BREKE,TEXT(IS(9):IE(9)))
            BREIS=BREIS+1    !!! �Զ���ֵ���е���
            BREIE=BREIE
            BREJS=BREJS+1
            BREJE=BREJE
            BREKS=BREKS+1
            BREKE=BREKE
        ELSE 
          CALL VF_A2ERR('VF_IISPH','SYNTAX ERROR IN SPH COMMAND,
     &                  UNIDENTIFIED REGION SETTING')
        ENDIF 

      ELSE
        CALL VF_A2ERR('VF_IISPH','SYNTAX ERROR IN SPH COMMAND,
     &                ONLY "REGION" IS AVAILABLE BY NOW')
      ENDIF 

!!!!!!----�жϵ�ǰ���̸���ķ�Χ�Ƿ����ghost region �Լ� buffer region
      IF (TEXT(IS(3):IE(3)).EQ.'GHOST') THEN
        J1=MYGJS+MYMJS
        J2=MYGJE-MYMJE
        I1=MYGIS+MYMIS
        I2=MYGIE-MYMIE
        CNUMGRE=0   ! ��ʼ��������
        DO K=GREKS,GREKE
          IF (2.LE.K.AND.K.LE.NUMK-1) THEN
            DO J=GREJS,GREJE
              IF (J1.LE.J.AND.J.LE.J2) THEN
                DO I=GREIS,GREIE
                  IF (I1.LE.I.AND.I.LE.I2) THEN
                    CNUMGRE=CNUMGRE+1   
                  ENDIF
                ENDDO
              ENDIF
            ENDDO
          ENDIF
        ENDDO
        GRE=0
        IF (CNUMGRE.GT.0) GRE=1
      ENDIF

      IF (TEXT(IS(3):IE(3)).EQ.'BUFFER') THEN
        J1=MYGJS+MYMJS
        J2=MYGJE-MYMJE
        I1=MYGIS+MYMIS
        I2=MYGIE-MYMIE
        CNUMBRE=0    ! ��ʼ��
        DO K=BREKS,BREKE
          IF (2.LE.K.AND.K.LE.NUMK-1) THEN
            DO J=BREJS,BREJE
              IF (J1.LE.J.AND.J.LE.J2) THEN
                DO I=BREIS,BREIE
                  IF (I1.LE.I.AND.I.LE.I2) THEN
                  CNUMBRE=CNUMBRE+1
                  ENDIF
                ENDDO
              ENDIF
            ENDDO
          ENDIF
        ENDDO
        BRE=0
        IF (CNUMBRE.GT.0) BRE=1
      ENDIF

!!!!!!������
C      WRITE (ILPFIL,"(' ','GHOST: ','X:',2I5,' Y:',2I5,' Z:',2I5)") 
C     &       GREIS,GREIE,GREJS,GREJE,GREKS,GREKE
C      WRITE (ILPFIL,"(' ','CNUMGRE=',I5,'   GRE=',I2)") CNUMGRE,GRE
C
C     WRITE (ILPFIL,"(' ','BUFFER: ','X:',2I5,' Y:',2I5,' Z:',2I5)") 
C     &       BREIS,BREIE,BREJS,BREJE,BREKS,BREKE
C     WRITE (ILPFIL,"(' ','CNUMBRE=',I5,'   BRE=',I2)") CNUMBRE,BRE
            
      END SUBROUTINE