C-*- mode:fortran; -*-
      COMMON /VF_AFILER/ RLPTRN(4),RGRTRN(4),RRSTRN(4),
     &                   RTRTRN(4),RTRVAL(MAXTR),RMMTRN(4),
     &                   PRTOLD,PRTNOW,
     &                   DMTBT0,DMTBT02,ETIME

CD=== 概要 ===========================================================

CDT   VF_AFILER.h:ファイル関連(出力時間間隔等):実数 File related (output time interval etc): Real number

C==== 内容 ===========================================================

CD    RLPTRN(4) : TRN : R*8 : リストファイルの出力時間情報 Output time information of the list file
CD                            (1):出力開始時刻 Output start time
CD                            (2):出力終了時刻 Output end time
CD                            (3):出力時間間隔 Output time interval
CD                            (4):次の出力時刻 Next output time
CD    RGRTRN(4) : TRN : R*8 : 図化ファイルの出力時間情報 Output time information of the figure file
CD                            (1):出力開始時刻
CD                            (2):出力終了時刻
CD                            (3):出力時間間隔
CD                            (4):次の出力時刻
CD    RRSTRN(4) : TRN : R*8 : 詳細ファイルの出力時間情報  output time information of Detailed file
CD                            (1):出力開始時刻
CD                            (2):出力終了時刻
CD                            (3):出力時間間隔
CD                            (4):次の出力時刻
CD    RTRTRN(4) : TRN : R*8 : 時系列ファイルの出力時間情報 Output time information of the time series file
CD                            (1):出力開始時刻
CD                            (2):出力終了時刻
CD                            (3):出力時間間隔
CD                            (4):次の出力時刻
CD    RTRVAL(MAXTR)
CD              : TRN : R*8 : 時系列ファイルへ出力する計算値  Calculated value output to time series file
CD    RMMTRN(4) : TRN : R*8 : マルチエージェントファイルの出力時間情報 Multi-agent file output time information
CD                            (1):出力開始時刻
CD                            (2):出力終了時刻
CD                            (3):出力時間間隔
CD                            (4):次の出力時刻
CD    PRTOLD    : TRN : R*8 : 前の空隙率ブロックの時刻 Time of the previous porosity block
CD    PRTNOW    : TRN : R*8 : 現在の空隙率ブロックの時刻 Time of the current porosity block
CD    DMTBT0    : CNS : R*8 : マトリクスデータ-1の初期無次元位相 initial non-dimensional phase of matrix data - 1
CD    DMTBT02   : CNS : R*8 : マトリクスデータ-2の初期無次元位相  initial non-dimensional phase of matrix data - 2
CD    ETIME     : CNS : R*8 : 自動リスタート制御時の終了までの経過時間(s) Elapsed time until completion of automatic restart control (s)
