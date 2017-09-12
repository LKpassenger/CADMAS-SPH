C-*- mode:fortran; -*-
      COMMON /VF_ANUMBI/ NUMI,NUMJ,NUMK,NUMB,NUMS,LEQK,LEQT,LEQC,NUMIJ
     &                  ,NUMBX

CD=== 概要 ===========================================================

CDT   VF_ANUMBI.h:データ数関連(配列サイズおよび格子数等):整数 Data number related (array size and number of grids etc.)
C     设定一些网格相关信息及一些控制开关，例如是否计算k-ε ....

C==== 内容 ===========================================================

CD    NUMI  : CNS : I*4 : x方向格子数+1(x方向セル数+2) 记录了考虑MPI不同分区间通信时在X方向该进程负责的网格单元数目
CD    NUMJ  : CNS : I*4 : y方向格子数+1(y方向セル数+2)
CD    NUMK  : CNS : I*4 : z方向格子数+1(z方向セル数+2)
CD    NUMB  : CNS : I*4 : 境界面の数 Number of boundary surfaces  指的是作为一定物理边界条件处理的单元表面
CD    NUMS  : TRN : I*4 : 表面セルの数 Number of surface cells
CD    LEQK  : CNS : I*4 : k-εを計算するかしないか calculate k - εmodel  默认值为0，即不计算紊动模型
CD                        = 0:計算しない Do not calculate
CD                        !=0:計算する Calculate
CD    LEQT  : CNS : I*4 : 温度を計算するかしないか calculate temperature?
CD                        = 0:計算しない
CD                        !=0:計算する
CD    LEQC  : CNS : I*4 : 濃度を計算するかしないか calculate the concentration?
CD                        = 0:計算しない
CD                        >=1:成分数
CD    NUMIJ : CNS : I*4 : 造波ソース用水平方向格子数+1 Horizontal grid number for wave generating source + 1
CD    NUMBX : CNS : I*4 : =MAX(NUMB,1) 
