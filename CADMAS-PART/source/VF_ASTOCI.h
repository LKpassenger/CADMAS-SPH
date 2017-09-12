      INTEGER MAX_CADMAS
      INTEGER MAX_NIST,MAX_NJST,MAX_NKST

      PARAMETER ( MAX_STOC   = 1024 )
      PARAMETER ( MAX_CADMAS = 1024 )
      PARAMETER ( MAX_NIST = 2000 )
      PARAMETER ( MAX_NJST = 2000 )
      PARAMETER ( MAX_NKST = 500 )

      INTEGER NB_STOC,LB_STOC,IB_STOC(MAX_STOC)
      INTEGER NB_CADMAS,LB_CADMAS,IB_CADMAS(MAX_CADMAS)
      INTEGER NB_SC,ITAGSC
      INTEGER NIST,NJST,NKST,IWST,IEST,JSST,JNST
      INTEGER MIST(MAX_NIST+1),MJST(MAX_NJST+1),MKST(MAX_NKST+1)
      INTEGER IIST(4,MAX_STOC),JJST(4,MAX_STOC)

      COMMON /VF_ASTOCI/NB_STOC,LB_STOC,IB_STOC
     &                 ,NB_CADMAS,LB_CADMAS,IB_CADMAS,NB_SC,ITAGSC
     &                 ,NIST,NJST,NKST,IWST,IEST,JSST,JNST
     &                 ,MIST,MJST,MKST,IIST,JJST

CD=== 概要 ===========================================================

CDT   VF_ASTOCI.h:CADMAS-STOC連成関連: 整数 CADMAS-STOC coupling 相关

C==== 内容 ===========================================================

CD    -- STOCとの通信に関する変数 --  Variable on communication with STOC
CD    NB_STOC          : CNS : I*4 : CADMASと接続しているSTOC-ICのPE数        Number of PEs of STOC-IC connected to CADMAS
CD    LB_STOC          : CNS : I*4 : CADMASと接続しているSTOCのPEの順番付け   Ordering PE of STOC connected to CADMAS   1,2,3.....
CD                                   値は1〜NB_STOC(CADMASのPE及びCADMASと接続しないSTOCのPEでは0)
CD                                   IB_STOC参照時に使用  The value is 1 to NB_STOC (0 for PE of STOC that does not connect with PE and CADMAS of CADMAS)
CD    IB_STOC          : CNS : I*4 : CADMASと接続しているSTOCのPEの、comm_ic_mgにおけるSTOC-ICのランクRank of STOC-IC in comm_ic_mg of PE of STOC connected to CADMAS
CD       (NB_STOC)
CD    NB_CADMAS        : CNS : I*4 : STOC-ICと接続しているCADMASのPE数
CD    LB_CADMAS        : CNS : I*4 : STOCと接続しているCADMASのPEの順番付け
CD                                   値は1〜NB_CADMAS(STOCのPE及びSTOCと接続しないCADMASのPEでは0)
CD                                   IB_CADMASやIB_SC参照時に使用
CD    IB_CADMAS        : CNS : I*4 : STOCと接続しているCADMASのPEの、comm_ic_mgにおけるSTOC-ICのランク
CD       (NB_CADMAS)
CD    NB_SC            : CNS : I*4 : STOC-CADMAS接続に参加しているか  指示进程是否参加STOC-CADMAS的交换
CD                                      (=0:参加していない、>0:参加している） 0：否

CD    -- STOCとCADMASのメッシュの対応関係に関する変数 -- Variables on correspondence between STOC and CADMAS meshes
CD    NIST             : CNS : I*4 : 自領域をSTOC側のメッシュで分割した場合のX方向分割数  Number of divisions in the X direction when the own area is divided by the mesh on the STOC side
CD    NJST             : CNS : I*4 : 自領域をSTOC側のメッシュで分割した場合のY方向分割数
CD    NKST             : CNS : I*4 : 自領域をSTOC側のメッシュで分割した場合のZ方向分割数
CD    MIST(NIST+1)     : CNS : I*4 : STOCのI番目の格子線位置が、CADMASの格子点インデックスのいくつに相当するか(X方向) How the Ith lattice line position of STOC corresponds to the lattice point index of CADMAS (X direction)
CD    MJST(NJST+1)     : CNS : I*4 : STOCのJ番目の格子線位置が、CADMASの格子点インデックスのいくつに相当するか(Y方向)
CD    MKST(NKST+1)     : CNS : I*4 : STOCのK番目の格子線位置が、CADMASの格子点インデックスのいくつに相当するか(Z方向)
CD    IWST             : CNS : I*4 : 西側境界でSTOCと接続するか否か(=1：接続する、=0：接続しない)  X-侧是否与STOC区域接壤
CD    IEST             : CNS : I*4 : 東側境界でSTOCと接続するか否か(=1：接続する、=0：接続しない)
CD    JSST             : CNS : I*4 : 南側境界でSTOCと接続するか否か(=1：接続する、=0：接続しない)
CD    JNST             : CNS : I*4 : 北側境界でSTOCと接続するか否か(=1：接続する、=0：接続しない)
