      SUBROUTINE VF_IISPH(IS,IE,NWD,TEXT)
      USE mod_sph, ONLY: GREIS,GREIE,GREJS,GREJE,GREKS,GREKE,
     &                    BREIS,BREIE,BREJS,BREJE,BREKS,BREKE,
     &                    BRE,GRE
     &                    CNUMGRE
      IMPLICIT INTEGER(I-N), DOUBLE PRECISION(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_ANUMBI.h'

      DIMENSION IS(MAXWDS),IE(MAXWDS)
      CHARACTER*(MAXCHR) TEXT
!!!!!!   IS(MAXWDS) : IN  : I*4   : 第N个单词开始的位置
!!!!!!   IE(MAXWDS) : IN  : I*4   : 第N个单词结束的位置
!!!!!!   NWD        : IN  : I*4   : 一行命令中单词的个数
!!!!!!   TEXT       : IN  : C*(*) : 读入的一行命令
      

!=====实现==========================================================================
      IF (TEXT(IS(2):IE(2)).EQ.'REGION') THEN
        IF (TEXT(IS(3):IE(3)).EQ.'GHOST') THEN
          !!记录给定的GHOST REGION区域的范围
          IF (TEXT(IS(4):IE(4)).EQ.'X') THEN
            CALL VF_ZSTOI(GREIS,TEXT(IS(5):IE(5)))
            CALL VF_ZSTOI(GREIE,TEXT(IS(6):IE(6)))
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'Y') THEN
            CALL VF_ZSTOI(GREJS,TEXT(IS(5):IE(5)))
            CALL VF_ZSTOI(GREJE,TEXT(IS(6):IE(6)))
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'Z') THEN 
            CALL VF_ZSTOI(GREKS,TEXT(IS(5):IE(5)))
            CALL VF_ZSTOI(GREKE,TEXT(IS(6):IE(6))) 
          ELSE 
            CALL VF_A2ERR('VF_IISPH','SYNTAX ERROR IN SPH COMMAND,UNIDENTIFIED REGION SETTING') 
          ENDIF

        ELSEIF (TEXT(IS(3):IE(3)).EQ.'BUFFER') THEN
          !!记录给定的BUFFER REGION区域的范围
          IF (TEXT(IS(4):IE(4)).EQ.'X') THEN
            CALL VF_ZSTOI(BREIS,TEXT(IS(5):IE(5)))
            CALL VF_ZSTOI(BREIE,TEXT(IS(6):IE(6)))
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'Y') THEN
            CALL VF_ZSTOI(BREJS,TEXT(IS(5):IE(5)))
            CALL VF_ZSTOI(BREJE,TEXT(IS(6):IE(6)))
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'Z') THEN 
            CALL VF_ZSTOI(BREKS,TEXT(IS(5):IE(5)))
            CALL VF_ZSTOI(BREKE,TEXT(IS(6):IE(6))) 
          ELSE 
            CALL VF_A2ERR('VF_IISPH','SYNTAX ERROR IN SPH COMMAND,UNIDENTIFIED REGION SETTING') 
          ENDIF

        ELSE 
          CALL VF_A2ERR('VF_IISPH','SYNTAX ERROR IN SPH COMMAND,UNIDENTIFIED REGION SETTING')
        ENDIF 

      ELSE
        CALL VF_A2ERR('VF_IISPH','SYNTAX ERROR IN SPH COMMAND,ONLY "REGION" IS AVAILABLE BY NOW')
      ENDIF 
!!!!!!读入完成，对读入值进行调整
      BREIS=BREIS+1
      BREIE=BREIE
      BREJS=2
      BREJE=NUMJ0-1
      BREKS=2
      BREKE=NUMK-1
!!!!!!----------------
      GREIS=GREIS+1
      GREIE=GREIE
      GREJS=2
      GREJE=NUMJ0-1
      GREKS=2
      GREKE=NUMK-1

!!!!!!----判断当前进程负责的范围是否包含ghost region 以及 buffer region
      J1=MYGJS+MYMJS
      J2=MYGJE-MYMJE
      I1=MYGIS+MYMIS
      I2=MYGIE-MYMIE
      CNUMGRE=0   ! 初始化在这里
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

      CNUMBRE=0    ! 初始化
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
    
      GRE=0
      BRE=0
      IF (CNUMGRE.GT.0) GRE=1
      IF (CNUMBRE.GT.0) BRE=1
            
      END SUBROUTINE        