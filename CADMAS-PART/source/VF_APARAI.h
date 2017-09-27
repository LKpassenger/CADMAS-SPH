C-*- mode:fortran; -*-
      CHARACTER*(MAXCHR) MGNAME
      COMMON /VF_APARAI/ MGPROC,MGRANK,MGCOMM,
     &                   MGARAN,
     &                   MGNAME(MAXPRO),MGNLEN(MAXPRO),
     &                   MGNPIN(MAXPRO),MGPARE(MAXPRO),
     &                   MGAREA(MAXPRO),MGSPH(MAXPRO),
     &                   MGPRNK,MGPINF(9),
     &                   MGCNUM,MGCRNK(MAXPRO),MGCINF(9,MAXPRO),
     &                   MGCPOS(6,MAXPRO),
     &                   NPROCS,NUMNPI,NUMNPJ,
     &                   MYRANK,MYRI  ,MYRJ  ,
     &                   NUMI0 ,NUMJ0 ,
     &                   MYIS  ,MYIE  ,MYJS  ,MYJE  ,
     &                   MYMIS ,MYMIE ,MYMJS ,MYMJE ,
     &                   MYGIS ,MYGIE ,MYGJS ,MYGJE ,
     &                   NUMBUF,
     &                   IPROCS(0:MAXNPI),JPROCS(0:MAXNPJ),
     &                   FC_RANK,MA_RANK

CD=== 概要 ===========================================================

CDT   VF_APARAI.h:並列化関連:整数 并行计算相关,有一部分没有弄清楚概念  Multi Grid

C==== 内容 ===========================================================

CD    -- マルチグリッド全体に関する変数 -- Variables on the whole multi grid
CD    MGPROC           : CNS : I*4 : 全体のプロセス数 The total number of processes--指 comm_model这个通讯子中的进程总数 Multi Grid PROCess
CD    MGRANK           : CNS : I*4 : 全体の中の自分のランク rank in the whole----每个进程在comm_model这个通讯子中的标号
CD    MGCOMM           : CNS : I*4 : 領域毎のコミュニケータ Communicator per area 每个不同的分区都会对应一个communicator，这个分区指整个计算区根据父-子关系被换分成不同的分区，而非MPI计算分区
CD    MGARAN           : CNS : I*4 : 領域数 Number of Regions，env文件中指定的将整个计算区域分割的区域数
CD    MGNAME(MAXPRO)   : CNS : C** : 領域名 Region name 每一个进程执行的分区名称，以下数组同理
CD    MGNLEN(MAXPRO)   : CNS : I*4 : 領域名の文字数 Number of characters of Region name
CD    MGNPIN(MAXPRO)   : CNS : I*4 : 領域毎のプロセス数  Number of processes per region
CD    MGPARE(MAXPRO)   : CNS : I*4 : 親の領域番号 Parent Region number
CD    MGAREA(MAXPRO)   : CNS : I*4 : 領域番号 Region number 每个进程所属的分区号 1 2 3 4 5 .....
CD    MGSPH(MAXPRO)    : CNS : I*4 : 用以记录进程是否参与到与SPH模型coupling  0代表不参与  1代表参与  add by LK
CD    MGPRNK           : CNS : I*4 : 全体の中の親のランク Parent's rank in the whole
CD                                   < 0:親無し No parent
CD                                   >=0:親のランク parent's rank
CD    MGPINF(9)        : CNS : I*4 : 親の情報 Parent's information
CD                                   (1):自分に接するx方向セル数 Number of x directional cells in contact 
CD                                   (2):自分に接するy方向セル数
CD                                   (3):自分に接するz方向セル数
CD                                   親との通信 Communication with parent
CD                                   (4):x-:しない(=1)、する(=0) x -: 不通信 (= 1), 通信 (= 0)
CD                                   (5):y-:しない(=1)、する(=0)
CD                                   (6):z-:しない(=1)、する(=0)
CD                                   (7):x+:しない(=1)、する(=0)
CD                                   (8):y+:しない(=1)、する(=0)
CD                                   (9):z+:しない(=1)、する(=0)
CD    MGCNUM           : CNS : I*4 : 子供の数 Number of children 记录当前进程的子进程的数目
CD    MGCRNK(MAXPRO)   : CNS : I*4 : 全体の中の子のランク Children rank in the whole
CD    MGCINF(9,MAXPRO) : CNS : I*4 : 子の情報 Child information，第二维度每个位置记录了当前进程的一个子进程的相关信息
CD                                   (1,*):x方向セル数,仮想含まず  Number of cells in x direction, virtual inclusion，dummy cell与通讯层cell不计入
CD                                   (2,*):y方向セル数,仮想含まず
CD                                   (3,*):z方向セル数,仮想含まず
CD                                   子との通信 Communication with child
CD                                   (4,*):x-:しない(=1)、する(=0) x -: 不通信 (= 1), 通信 (= 0)
CD                                   (5,*):y-:しない(=1)、する(=0)
CD                                   (6,*):z-:しない(=1)、する(=0)
CD                                   (7,*):x+:しない(=1)、する(=0)
CD                                   (8,*):y+:しない(=1)、する(=0)
CD                                   (9,*):z+:しない(=1)、する(=0)
CD    MGCPOS(6,MAXPRO) : CNS : I*4 : 自分の中の子の位置 Child's position
CD                                   (1,*):x方向セル番号(開始) x direction cell number (start)
CD                                   (2,*):y方向セル番号(開始)
CD                                   (3,*):z方向セル番号(開始)
CD                                   (4,*):x方向セル番号(終了)
CD                                   (5,*):y方向セル番号(終了)
CD                                   (6,*):z方向セル番号(終了)

CD    -- 各グリッドに関する変数 -- Variables on each grid ，grid指什么？
CD    NPROCS           : CNS : I*4 : プロセス数 Number of processes 对每个进程都储存了其所属的父-子结构中的分区被分配了几个进程
CD    NUMNPI           : CNS : I*4 : x方向プロセス数  x direction process number
CD    NUMNPJ           : CNS : I*4 : y方向プロセス数  Y direction process number
CD    MYRANK           : CNS : I*4 : 自分のランク My rank 指当前执行的进程在同一分区中的局部编号，从0开始
CD    MYRI             : CNS : I*4 : 自分のx方向ランク My x direction rank 当前进程负责X方向的第几块区域
CD    MYRJ             : CNS : I*4 : 自分のy方向ランク My y direction rank ............Y...............
CD    NUMI0            : CNS : I*4 : 全体のx方向格子数+1 the total number of lattices in x direction + 1
CD    NUMJ0            : CNS : I*4 : 全体のy方向格子数+1

CD    MYIS             : CNS : I*4 : x方向セル番号(開始,仮想含まず,局所)  x direction cell number (start, virtual not included, local)
CD    MYIE             : CNS : I*4 : x方向セル番号(終了,仮想含まず,局所)  x direction cell number (end, virtual not included, local)

CD    MYJS             : CNS : I*4 : y方向セル番号(開始,仮想含まず,局所)
CD    MYJE             : CNS : I*4 : y方向セル番号(終了,仮想含まず,局所)

CD    MYMIS            : CNS : I*4 : x方向セル番号(開始,仮想の厚み) x direction cell number (start, virtual thickness)
CD    MYMIE            : CNS : I*4 : x方向セル番号(終了,仮想の厚み) x direction cell number (end, virtual thickness)

CD    MYMJS            : CNS : I*4 : y方向セル番号(開始,仮想の厚み)
CD    MYMJE            : CNS : I*4 : y方向セル番号(終了,仮想の厚み)

CD    MYGIS            : CNS : I*4 : x方向セル番号(開始,仮想含む,大域) x direction cell number (start, virtual inclusive, global)记录当前进程负责的MPI分区X方向上的网格节点号
CD    MYGIE            : CNS : I*4 : x方向セル番号(終了,仮想含む,大域) x direction cell number (end, virtual inclusive, global)

CD    MYGJS            : CNS : I*4 : y方向セル番号(開始,仮想含む,大域)
CD    MYGJE            : CNS : I*4 : y方向セル番号(終了,仮想含む,大域)
CD    NUMBUF           : CNS : I*4 : バッファ用データの数(1本分) Number of data for buffer (for one)
CD    IPROCS(0:MAXNPI) : CNS : I*4 : x方向の終了セル番号  End cell number in the x direction  记录X方向上的每个MPI分区负责的范围（实际是响应的网格节点编号)
CD    JPROCS(0:MAXNPJ) : CNS : I*4 : y方向の終了セル番号 End cell number in the y direction

CD    -- 他モジュールとの連成に関する変数 -- Variables on linkage with other modules -
CD    FC_RANK          : CNS : I*4 : comm_modelにおける2FCモデルのランク rank of 2FC model in comm_model
CD    MA_RANK          : CNS : I*4 : comm_mlicdsmg2fc_mltにおけるマルチエージェントモデルのランク rank of multi agent model in comm_mlicdsmg2fc_mlt
