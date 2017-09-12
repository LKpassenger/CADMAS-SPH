C-*- mode:fortran; -*-
      COMMON /VF_APHYSI/ IBCTYP(4,4),IDAMP(4),IDROP,ISCTYP(2),
     &                   NPVCB,IPVCBC(MAXPVC),IDRGN

CD=== 概要 ===========================================================

CDT   VF_APHYSI.h:物理事象関連(物理量および物理モデル等):整数 物理量相关：X,Y方向上的边界属性

C==== 内容 ===========================================================

CD    IBCTYP(4,4) : CNS : I*4 : 特殊境界に関する情報  Information on special boundary 第二个维度代表造波方向
CD                              (*,1):x座標最小位置の境界  boundary of x coordinate minimum position
CD                              (*,2):x座標最大位置の境界  boundary of x coordinate maximum position
CD                              (*,3):y座標最小位置の境界
CD                              (*,4):y座標最大位置の境界
CD                              (1,*):特殊境界の種別 Type of special boundary
CD                                    =0:無し None
CD                                    =1:法線方向への造波境界 Wave boundary in the normal direction
CD                                    =2:法線方向への開境界 Open boundary in the normal direction
CD                              (2,*):特殊境界の種別の詳細 Details of type of special boundary
CD                                    造波境界の場合 wave boundary case
CD                                    =-4:マトリクスデータ-2 (BCのみ) Matrix data - 2 
CD                                    =-3:マトリクスデータ-1  Matrix data -1
CD                                    =-2:Stokes波(第5次近似解) Stokes wave
CD                                    =-1:Cnoidal波(第3次近似解) Cnoidal wave
CD                                    = 0:Stokes波またはCnoidal波 Stokes wave or Cnoidal wave
CD                                    > 0:流れ関数法Bとその次数 Flow function method B and its order
CD                                    開境界の場合 open boundary case
CD                                    = 0:放射境界(微小振幅波の波速) Radiation boundary (wave velocity of small amplitude wave)
CD                              (3,*):特殊境界の始点セル番号 Start cell number of special boundary
CD                              (4,*):特殊境界の終点セル番号 End  cell number of special boundary
CD    IDAMP(4)    : CNS : I*4 : 減衰領域の設定フラグ Setting flag of damping area
CD                              (1):x座標最小位置近傍 x coordinate minimum position附近
CD                              (2):x座標最大位置近傍
CD                              (3):y座標最小位置近傍
CD                              (4):y座標最大位置近傍
CD                                  = -1:使用しない  Not used
CD                                  >= 0:使用する,かつ,減衰関数の次数 Used and the order of the attenuation function
CD    IDROP  : CNS : I*4 : 水滴の自由落下処理(TimerDoor法) Free drop treatment of water drops 
CD                         =0:処理を行わない Do not process
CD                         =1:処理を行う process
CD    ISCTYP(2)   : CNS : I*4 : 造波ソースに関する情報 Information on the wave source
CD                              (1):造波ソースの種別  Type of wave source
CD                                  =0:無し None
CD                                  >0:x方向への造波ソース(ABS=I) Waveform source in the x direction (ABS = I)
CD                                  <0:y方向への造波ソース(ABS=J) Wave source in the y direction (ABS = J)
CD                              (2):造波関数 wave generation function
CD                                  =-3:マトリクスデータ Matrix data
CD                                  =-2:Stokes波(第5次近似解)
CD                                  =-1:Cnoidal波(第3次近似解)
CD                                  = 0:Stokes波またはCnoidal波 Stokes wave or Cnoidal wave
CD                                  > 0:流れ関数法Bとその次数 Flow function method B and its order
CD    NPVCB          : TRN : I*4 : 気泡の数(:空気圧計算用) Number of bubbles (for calculating air pressure)
CD    IPVCBC(MAXPVC) : TRN : R*8 : 空気圧計算用のワーク
CD    IDRGN  : CNS : I*4 : Dupuit-Forheimer式による抵抗力 Resistance by Dupuit-Forheimer equation
CD                         =0:抵抗力は既存の処理 Resistance is an existing process
CD                         >0:Dupuit-Forheimer式(粒経の入力数) Dupuit-Forheimer equation (number of inputs of grain size)
