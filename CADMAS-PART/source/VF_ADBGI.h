C-*- mode:fortran; -*-
      COMMON /VF_ADBGI/ IDBGF(6),IDBGTD(6)

CD=== 概要 ===========================================================

CDT   VF_ADBGI.h:デバッグ関連:整数  DEBUG相关

C==== 内容 ===========================================================

CD    IDBGF(6)  : CNS : I*4 : 矩形ボックスへのF値指定データ
CD                            (1):始点のx方向セル番号  网格单元编号 
CD                            (2):始点のy方向セル番号
CD                            (3):始点のz方向セル番号
CD                            (4):終点のx方向セル番号
CD                            (5):終点のy方向セル番号
CD                            (6):終点のz方向セル番号
CD    IDBGTD(6) : CNS : I*4 : 矩形ボックスへのTD用速度指定データ
CD                            (1):始点のx方向セル番号
CD                            (2):始点のy方向セル番号
CD                            (3):始点のz方向セル番号
CD                            (4):終点のx方向セル番号
CD                            (5):終点のy方向セル番号
CD                            (6):終点のz方向セル番号
