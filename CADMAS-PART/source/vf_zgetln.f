      SUBROUTINE VF_ZGETLN(IS,IE,MWD,NWD,IFIL,IEOF,TEXT)

CD=== 概要 ===========================================================

CDT   VF_ZGETLN:テキストデータを1行を読み込み、単語に分解する
CD      (1)区切りは1つ以上の空白
CD      (2)「#」以降はコメント
CD      (3)n番目の単語は TEXT(IS(n):IE(n)) の形式で参照する
CD      (4)空白行とコメント行はNWD=0
CD      (5)ダブ等の特殊文字は判定していない(注意)

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    IS(MWD) : OUT : I*4   : n番目の単語の開始位置   第n个关键词在当前行中开始的位置
CD    IE(MWD) : OUT : I*4   : n番目の単語の終了位置   第n个关键词在当前行中结束的位置
CD    MWD     : IN  : I*4   : 単語の最大数  字符的最大数目
CD    NWD     : OUT : I*4   : 単語の数      当前行关键词的数目
CD    IFIL    : IN  : I*4   : ファイル番号
CD    IEOF    : OUT : I*4   : = 0:EOFを読み込んでいない 未到达EOF，注意为返回值，影响调用程序
CD                            !=0:EOFを読み込んだ       到达EOF
CD    TEXT    : OUT : C*(*) : 入力した文字列   读入的信息
      DIMENSION IS(MWD),IE(MWD)  
      CHARACTER*(*) TEXT
C
      CHARACTER*(MAXCHR) LINBUF(MAXLIN)    !  MAXLIN= 50，LINBUF()相当于一个解释每一行内容前的用于储存的缓冲区，局部变量
      SAVE LINBUF
      INTEGER LINCNT,LINEOF  ! 初始化为0  LINCNT：用于解读LINBUF()时计数用 LINEOF用以只是是否处理至文件尾
      DATA LINCNT,LINEOF /2*0/  !   LINCNT与LINEOF均具有save属性
C
C     LINBUF: 入力データMAXLIN行分を読み貯めておく配列  Array that reads the input data MAXLIN rows
C     LINCNT: 前回の呼び出しで読み込んだLINBUFの要素番号
C             1<=LINCNT<MAXLINでMAXLINに達したら0リセット
C             =0のときLINBUFを読み込む
C     LINEOF: EOFに達したときのLINBUFの要素番号。EOFに達していないときは0

C==== 実行 ===========================================================

CD    -- 初期設定 --
      NWD =0
      IEOF=0
      TEXT=' '
      NC  =LEN(TEXT)   ! TEXT对应的实参被声明为长度为256

CD    -- MAXLIN行を読み込む --
      IF (LINCNT.EQ.0) THEN
        LINBUF(:)=' '   ! 整个数组初始化为" "
        IF (MYRANK.EQ.0.OR.INMODE.EQ.1) THEN   ! 只在标号为0进程中读入
          DO N=1,MAXLIN  ! 一次最多从.in文件中读入50行存放至LINBUF()
            READ(IFIL,9510,END=100,ERR=9010) LINBUF(N)   ! 将.in中每一行的内容以字符的形式存至LINBUF()中，等待解释
          ENDDO
          LINEOF=0
          GOTO 110
 100      CONTINUE
          LINEOF=N   ! 处理读至文件末尾的情况
 110      CONTINUE
        ENDIF

        IF (INMODE.EQ.0.AND.NPROCS.NE.1) THEN
          CALL VF_P1BCSC(LINBUF,NC*MAXLIN,0)  ! 将从.in文件中读到的信息广播至其他进程中!!!!!!!!!!!这样每个进程中都可以处理.in文件中的信息了
          CALL VF_P1BCSI(LINEOF,        1,0)
        ENDIF
      ENDIF
C
      LINCNT=LINCNT+1
      TEXT=LINBUF(LINCNT)
      IF (LINCNT.EQ.LINEOF) IEOF=1
      IF (LINCNT.EQ.MAXLIN) LINCNT=0

CD    -- 空白行とコメント行はスキップ -- 识别处理空白行和注释行
      ION=0 ! ION=0表示读入的某一行为空行，非0值表示第一个非空格字符出现的位置
      ICM=0 ! ICM=0表示当前行并无注释字符"#"
      DO 200 I=1,NC
        IF (ION.EQ.0 .AND. TEXT(I:I).NE.' ') ION=I  !若存在非" "字符，则ION=第一个非" "字符的位置
        IF (ICM.EQ.0 .AND. TEXT(I:I).EQ.'#') ICM=I  !若存在"#"字符，ICM=第一个"#"出现的位置，大于0
 200  CONTINUE
      IF (ION.EQ.0  ) GOTO 9000 ! 若当前行为空行或者注释行，则直接RETURN,然后分析下一行
      IF (ION.EQ.ICM) GOTO 9000

CD    -- 有効行を単語に分解 --
      IF (ICM.EQ.0) ICM=NC+1
      TEXT=TEXT(ION:ICM-1) ! 取有效部分，不包含前边的空格，不包含后边出现的注释
      CALL VF_ZSTOWS(IS,IE,MWD,NWD,TEXT(1:ICM-ION))

C     -- 実行文の終了 --
 9000 CONTINUE
      GOTO 9999

C==== ファイル関連エラー処理 =========================================

 9010 CONTINUE
      CALL VF_A2ERR('VF_ZGETLN','I/O ERROR(text data).')
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT(A)

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
