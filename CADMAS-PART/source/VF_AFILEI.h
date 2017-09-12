C-*- mode:fortran; -*-
      COMMON /VF_AFILEI/ IENFIL,IINFIL,IMTFIL,IMTFIL2,IREFIL,IPRFIL,
     &                   ILPFIL,IGRFIL,IRSFIL,ITRFIL,IMMFIL,
     &                   ILPTYP,ILPTRN(3),ILPARA(3),ILPON(21),
     &                   IGRTYP,IGRTRN(3),IGRARA(6),IGRVOR,
     &                   IRSTYP,IRSTRN(3),IRETYP,
     &                   ITRTYP,ITRTRN(3),ITRNUM,ITRPRM(MAXTR1,MAXTR),
     &                   IMMTYP,IMMTRN(3),
     &                   IPRNT,IPRNB,IPRNP,IPRARA(6,MAXPRB),IPRIT,
     &                   MTBTYP ,MTBTT ,MTBZZ ,MTBNOW ,
     &                   MTBTYP2,MTBTT2,MTBZZ2,MTBNOW2

CD=== 概要 ===========================================================

CDT   VF_AFILEI.h:ファイル関連(ファイル番号および出力制御等):整数  File related (file number, output control, etc.): integer
C     定义了一些文件的I/O号，及一些控制输出内容的变量
C==== 内容 ===========================================================

CD    IENFIL    : CNS : I*4 : マルチグリッド環境ファイルのファイル番号 File number of multigrid environment file
CD    IINFIL    : CNS : I*4 : 入力ファイルのファイル番号(0=未open) File number of the input file (0 = not open)
CD    IMTFIL    : CNS : I*4 : マトリクスデータファイル-1のファイル番号 File number of matrix data file - 1
CD    IMTFIL2   : CNS : I*4 : マトリクスデータファイル-2のファイル番号  File number of matrix data file - 2
CD    IREFIL    : CNS : I*4 : リスタートファイルのファイル番号 File number of the restart file
CD    IPRFIL    : CNS : I*4 : 時間依存型空隙率ファイルのファイル番号 File number of the time dependent porosity file
CD    ILPFIL    : CNS : I*4 : リストファイルのファイル番号 File number of the list file
CD    IGRFIL    : CNS : I*4 : 図化ファイルのファイル番号 File number of the figure file
CD    IRSFIL    : CNS : I*4 : 詳細ファイルのファイル番号 File number of detailed file
CD    ITRFIL    : CNS : I*4 : 時系列ファイルのファイル番号 File number of time series file
CD    IMMFIL    : CNS : I*4 : マルチエージェントファイルのファイル番号 File number of multi agent file
CD    ILPTYP    : CNS : I*4 : リストファイルの出力方法 Output method of list file
CD                            =0:出力しない Do not output
CD                            =1:ステップ間隔 Step interval
CD                            =2:解析時間間隔 analysis time interval
CD    ILPTRN(3) : CNS : I*4 : リストファイルの出力ステップ情報 List file output step information 以下变量控制list file的输出内容
CD                            (1):出力開始ステップ Output start step
CD                            (2):出力終了ステップ Output end step
CD                            (3):出力ステップ間隔 Output step interval 
CD    ILPARA(3) : CNS : I*4 : リストファイルの出力断面情報 Output section information of the list file
CD                            (1):出力断面 Output section
CD                               =1:xy断面 , =2:xz断面 , =3:yz断面
CD                               =4:yx断面 , =5:zx断面 , =6:zy断面
CD                            (2):出力断面のセル番号 cell number of output section
CD                            (3):出力断面の格子番号  lattice number of output section
CD    ILPON(21) : CNS : I*4 : リストファイルへの出力/非出力
CD                            ( 1):障害物
CD                               =0:出力しない , !=0:出力する   =0：No output， !=0:output
CD                            ( 2):境界条件に関するインデックス Index on boundary conditions
CD                               =0:出力しない , !=0:出力する
CD                            ( 3):慣性力係数 Inertial force coefficient
CD                               =0:出力しない , !=0:出力する
CD                            ( 4):抵抗係数 resistance coefficient
CD                               =0:出力しない , !=0:出力する
CD                            ( 5):ポーラス値 Porous value
CD                               =0:出力しない , !=0:出力する
CD                            ( 6):GLV,GLX,GLZ
CD                               =0:出力しない , !=0:出力する
CD                            ( 7):NF
CD                               =0:出力しない , !=0:出力する
CD                            ( 8):VOF関数F
CD                               =0:出力しない , !=0:出力する
CD                            ( 9):流速
CD                               =0:出力しない , !=0:出力する
CD                            (10):圧力
CD                               =0:出力しない , !=0:出力する
CD                            (11):分子動粘性係数と渦動粘性係数の和  两个粘性系数之和
CD                               =0:出力しない , !=0:出力する
CD                            (12):境界値 boundary value
CD                               =0:出力しない , !=0:出力する
CD                            (13):温度
CD                               =0:出力しない , !=0:出力する
CD                            (14):熱伝導率 Thermal conductivity
CD                               =0:出力しない , !=0:出力する
CD                            (15):濃度
CD                               =0:出力しない , !=0:出力する
CD                            (16):拡散係数 diffusion coefficient
CD                               =0:出力しない , !=0:出力する
CD                            (17):乱流エネルギ  turbulent energy
CD                               =0:出力しない , !=0:出力する
CD                            (18):乱流エネルギ散逸 Turbulent energy dissipation
CD                               =0:出力しない , !=0:出力する
CD                            (19):渦動粘性係数 Vortical viscosity coefficient
CD                               =0:出力しない , !=0:出力する
CD                            (20):空気圧の計算用インデックス Index for calculation of air pressure
CD                               =0:出力しない , !=0:出力する
CD                            (21):空気圧の計算用の圧力 Pressure for calculation of air pressure
CD                               =0:出力しない , !=0:出力する
CD    IGRTYP    : CNS : I*4 : 図化ファイルの出力方法 Output method of graphic file
CD                            =0:出力しない Do not output
CD                            =1:ステップ間隔 Step interval
CD                            =2:解析時間間隔  analysis time interval
CD    IGRTRN(3) : CNS : I*4 : 図化ファイルの出力ステップ情報 Step of outputting the graphic file
CD                            (1):出力開始ステップ Output start step
CD                            (2):出力終了ステップ Output end step
CD                            (3):出力ステップ間隔 Output step interval
CD    IGRARA(6) : CNS : I*4 : リストファイルの出力断面情報 Output section information of the list file
CD                            (1):始点のx方向セル番号 x direction cell number of the starting point
CD                            (2):始点のy方向セル番号  
CD                            (3):始点のz方向セル番号
CD                            (4):終点のx方向セル番号
CD                            (5):終点のy方向セル番号
CD                            (6):終点のz方向セル番号
CD    IGRVOR    : CNS : I*4 : 図化ファイルへ渦度を出力するか vorticity output to a graphic file
CD                            = 0:出力しない Do not output
CD                            !=0:出力する Output
CD    IRSTYP    : CNS : I*4 : 詳細ファイルの出力方法 Output method of detailed file
CD                            =0:出力しない Do not output
CD                            =1:ステップ間隔 Step interval
CD                            =2:解析時間間隔 analysis time interval
CD    IRSTRN(3) : CNS : I*4 : 詳細ファイルの出力ステップ情報 Detailed file output step information
CD                            (1):出力開始ステップ Output start step
CD                            (2):出力終了ステップ Output end step
CD                            (3):出力ステップ間隔 Output step interval
CD    IRETYP    : CNS : I*4 : リスタートの方法 Restart method
CD                            < 0:リスタートしない Do not restart
CD                            >=0:リスタートステップ Restart step
CD    ITRTYP    : CNS : I*4 : 時系列ファイルの出力方法 Output method of time series file
CD                            =0:出力しない Do not output
CD                            =1:ステップ間隔 Step interval
CD                            =2:解析時間間隔 analysis time interval
CD    ITRTRN(3) : CNS : I*4 : 時系列ファイルの出力ステップ情報 Time series file output step information
CD                            (1):出力開始ステップ
CD                            (2):出力終了ステップ
CD                            (3):出力ステップ間隔
CD    ITRNUM    : CNS : I*4 : 時系列ファイルへの出力対象データ数 Number of data to be output to time series file
CD    ITRPRM(MAXTR1,MAXTR)   ITRPRM(8,10000),在include 该头文件前需include 'VF_A0PRM.h',里边定义了MAXTR1，MAXTR
CD              : CNS : I*4 : 時系列ファイルのパラメータ Parameters of time series file
CD                            (1,*):値の算出方法  计算方法
CD                                  = 0:初期水面からの水位変動 Water level fluctuation from initial water level
CD                                  = 1:指定点の値 value of specified point
CD                                  = 2:指定領域の最小値 Minimum value of specified area
CD                                  = 3:指定領域の最大値 Maximum value of specified area
CD                                  = 4:指定領域の体積平均値 Volume average value of specified area
CD                                  = 5:指定領域の体積積分値 Volume integral value of specified area
CD                                  =11:障害物への波力 Wave power to obstacles
CD                            (2,*):物理量の種別 Type of physical quantity
CD                                  < 0:濃度(成分番号×(-1)) Concentration (component number × (-1))
CD                                  = 0:初期水面からの水位変動 Water level fluctuation from initial water level
CD                                  = 1:x方向流速 x direction flow velocity
CD                                  = 2:y方向流速
CD                                  = 3:z方向流速
CD                                  = 4:圧力
CD                                  = 5:F値
CD                                  = 6:乱流エネルギ turbulent energy
CD                                  = 7:乱流エネルギ散逸  Turbulent energy dissipation
CD                                  = 8:温度
CD                                  =11:障害物へのxの負方向の波力 Wave force in the negative direction of x to an obstacle
CD                                  =12:障害物へのxの正方向の波力 the positive wave force of x to an obstacle
CD                                  =13:障害物へのyの負方向の波力
CD                                  =14:障害物へのyの正方向の波力
CD                                  =15:障害物へのzの負方向の波力
CD                                  =16:障害物へのzの正方向の波力
CD                                  =17:渦度(x方向成分) vorticity (x direction component)
CD                                  =18:渦度(y方向成分)
CD                                  =19:渦度(z方向成分)
CD                            (3,*):x方向格子またはセル番号(I1) x direction lattice or cell number (I 1)
CD                                  =-1:x座標最小位置の造波境界の解 Solve the wave boundary at the x coordinate minimal position
CD                                  =-2:x座標最大位置の造波境界の解
CD                                  =-3:y座標最小位置の造波境界の解
CD                                  =-4:y座標最大位置の造波境界の解
CD                                  =-5:造波ソースの解 Solve the wave source
CD                            (4,*):y方向格子またはセル番号(J1) y direction lattice or cell number (J 1)
CD                            (5,*):z方向格子またはセル番号(K1) 
CD                            (6,*):x方向格子またはセル番号(I2) x direction lattice or cell number (I 2)
CD                            (7,*):y方向格子またはセル番号(J2)
CD                            (8,*):z方向格子またはセル番号(K2)
CD    IMMTYP    : CNS : I*4 : マルチエージェントファイルの出力方法 Multiagent file output method
CD                            =0:出力しない 不输出
CD                            =1:ステップ間隔
CD                            =2:解析時間間隔
CD    IMMTRN(3) : CNS : I*4 : マルチエージェントの出力ステップ情報 Multiagent output step information
CD                            (1):出力開始ステップ
CD                            (2):出力終了ステップ
CD                            (3):出力ステップ間隔
CD    IPRNT     : CNS : I*4 : 空隙率の時間方向のデータ数 number of data of the porosity in the time direction
CD                            =0:空隙率ファイルを読み込まない Do not read porosity file
CD                            =1:時間依存データでは無い It is not time dependent data
CD                            >1:時間依存データ Time dependent data
CD    IPRNB     : CNS : I*4 : 時間依存型空隙率の空間ブロックの最大数 Maximum number of spatial blocks of time dependent porosity
CD    IPRNP     : CNS : I*4 : 時間依存型空隙率の設定セル数 setting Cell number of Time-dependent porosity 
CD    IPRARA(6,MAXPRB)  IPRARA(6,10)
CD              : CNS : I*4 : 時間依存型空隙率の空間ブロック情報 Time-dependent porosity spatial block information
CD                            (1,*):始点のx方向セル番号  x direction cell number of start point
CD                            (2,*):始点のy方向セル番号
CD                            (3,*):始点のz方向セル番号
CD                            (4,*):終点のx方向セル番号
CD                            (5,*):終点のy方向セル番号
CD                            (6,*):終点のz方向セル番号
CD    IPRIT     : TRN : I*4 : 現在の読み込んでいる時刻ブロック Current loading time block
CD    MTBTYP    : CNS : I*4 : マトリクスデータのタイプ-1 Matrix data type -1
CD                            =0:使用しない Not used
CD                            =1:水位と流速指定 Water level and Flow velocity specification
CD                            =2:流速指定 Flow velocity specification
CD                            =3:水位指定 Water level specification
CD    MTBTT     : CNS : I*4 : マトリクスデータの位相方向のデータ数 Number of data in the phase direction of the matrix data
CD    MTBZZ     : CNS : I*4 : マトリクスデータの水深方向のデータ数 Number of data in the depth direction of the matrix data
CD    MTBNOW    : TRN : I*4 : マトリクスデータの位相方向の現在位置 Current position in the phase direction of the matrix data
CD    MTBTYP2   : CNS : I*4 : マトリクスデータのタイプ-2   Matrix data type 2
CD                            =0:使用しない
CD                            =1:水位と流速指定
CD                            =2:流速指定
CD                            =3:水位指定
CD    MTBTT2    : CNS : I*4 : マトリクスデータの位相方向のデータ数 同上
CD    MTBZZ2    : CNS : I*4 : マトリクスデータの水深方向のデータ数
CD    MTBNOW2   : TRN : I*4 : マトリクスデータの位相方向の現在位置
