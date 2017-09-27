!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! 使用方法
!!!
!!! 各モジュールへの組込み時は"my_group="と"my_model="の2行だけ書き換える 
!!! When incorporating into each module, rewrite only two lines "my_group =" and "my_model ="
!!!
!!! メインルーチンの最初のところでinit_mpmdをCALLする
!!! CALL init_mpmd at the beginning of the main routine
!!!
!!! init_mpmd以降では、STOC内(CADMAS内)ではmpi_comm_worldの代わりにcom_*を使用し、
!!!    use mod_comm,only: comm_*
!!! を追加する。
!!! Since init_mpmd, com_ * is used instead of mpi_comm_world in STOC (in CADMAS),use mod_comm, only: comm_ *
!!! mpi_comm_worldを使用するのは、mpi_abortのみ。 mpi_comm_world is used only for mpi_abort.
!!!
!!!
!!! その他の大域変数nsize_all,nrank_all,l_group,l_modelは使わなくてもよい。
!!!Other global variables nsize_all, nrank_all, l_group, l_model need not be used.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module mod_comm
  implicit none
!
! Common Definition
! 考虑CADMAS 与 SPH coupling 时，应为my_group,my_model 添加 代表SPH的变量值，这样才能分裂正确的通讯子comm_group,comm_model
! 除此之外，还应 分割 整合CADMAS和SPH进程的 通讯子，用以两个模型之间的信息交换
  integer,parameter:: &                            ! グループ番号の定義 define group number
       &              l_stoc=0, l_cadmas=1, l_sph=2  ! 添加了表示SPH模型的group类别-----LK
  integer,parameter:: &                            ! モデル番号の定義 define model number
       &              l_stoc_ml   = 0, l_stoc_ic   = 1, l_stoc_ds = 2 &
       &             ,l_stoc_dm   = 3, l_stoc_oil  = 4                &
       &             ,l_cadmas_mg =10, l_cadmas_2fc=11, l_paridem =12 &
       &             ,l_mlt_agent =13, l_str       =14                &
       &             ,l_sph_isph =20    ! 添加了表示ISPH模型的model类别-----LK
  character(6),parameter::  c_group(0:2)=(/ &      ! グループ名 define group name add by LK
       &             'stoc  ', 'cadmas','sph'   /)
  character(10),parameter:: c_model(0:20)=(/ &     ! モデル名  define model name  add by LK
       &             'ml        ', 'ic        ', 'ds        ' &
       &            ,'dm        ', 'oil       ', '#5#       ' &
       &            ,'#6#       ', '#7#       ', '#8#       ' &
       &            ,'#9#       ', 'mg        ', '2fc       ' &
       &            ,'paridem   ', 'mlt_agent ', 'str       ' &
       &            ,'#15#      ', '#16#      ', '#17#      ' &
       &            ,'#18#      ', '#19#      ', 'sph       ' &
       &                                   /)
!
! Definition for each model
  integer,parameter:: my_group=l_cadmas !  =1
  integer,parameter:: my_model=l_cadmas_mg  !  =10
!
! Global variables
  integer:: comm_group             ! グループ内の通信に用いるコミュニケータ Communicator used for communication within group
  integer:: comm_model             ! モデル内の通信に用いるコミュニケータ Communicator used for communication in the model
!                                  ! (例外1: STOC-ML,IC,DSは同じcomm_modelに属する) STOC-ML, IC, DS belong to the same comm_model
!                                  ! (例外2: CADMAS-SURF/MGと2FCは同じcomm_modelに属する) CADMAS-SURF / MG and 2FC belong to the same comm_model
!
!                                  ! モデル間の通信に用いるコミュニケータ群(値の設定は各モデル内で別途行う。
!                                  ! Communicator group to be used for communication between models (Value setting is done separately in each model.
!                                  ! 下記のテンポラリのコミュニケータを利用して生成する)Generate using the following temporary communicator
!                                  ! 1対1通信のみであれば、mpi_comm_worldやcomm_groupを用いてもよい。
!                                  ! If it is only one-to-one communication, mpi_comm_world and comm_group may be used.
  integer:: comm_mlicds_dm        ! STOC-ML,IC,DSとSTOC-DMの通信に用いるコミュニケータCommunicator used for communication between STOC-ML, IC, DS and STOC-DM
  integer:: comm_mlicds_oil       ! STOC-ML,IC,DSとSTOC-OILの通信に用いるコミュニケータ
  integer:: comm_ic_mg            ! STOC-ICとCADMAS-SURF/MGの通信に用いるコミュニケータ
  integer:: comm_mg_2fc           ! CADMAS-SURF/MGとCADMAS-SURF/2FCの通信に用いるコミュニケータ
  integer:: comm_2fc_dem          ! CADMAS-SURF/2FCとPARIDEMの通信に用いるコミュニケータ
  integer:: comm_2fc_str          ! CADMAS-SURF/2FCとSTRの通信に用いるコミュニケータ
  integer:: comm_mlicdsmg2fc_mlt  ! STOC-ML,IC,DSとCADMAS-SURF/MG,2FCとMLT_AGENTの通信に用いるコミュニケータ
  integer:: comm_mlicdsmg2fc      ! STOC-ML,IC,DSとCADMAS-SURF/MGと2FCの通信に用いるコミュニケータ Communicator used for communication between STOC-ML, IC, DS and CADMAS-SURF / MG and 2FC
  INTEGER   comm_mg_isph          ! 负责CADMAS-SPH的进程集合，通过对comm_work_mg_isph分割得到 add by LK
!
! テンポラリで設定するコミュニケータ(モデル間の通信に用いるコミュニケータを作成する前に、
!                                    COMM_SPLITに参加するモデルを絞ったコミュニケータを作成しておく)
  integer:: comm_work_mlicds_dm    ! STOC-ML,IC,DSとSTOC-DMを含むコミュニケータ
  integer:: comm_work_mlicds_oil   ! STOC-ML,IC,DSとSTOC-OILを含むコミュニケータ
  integer:: comm_work_ic_mg        ! STOC-ICとCADMAS-SURF/MGを含むコミュニケータ STOC-IC与CADMAS-SURF/MG coupling 通讯子
  integer:: comm_work_mg_2fc       ! CADMAS-SURF/MGとCADMAS-SURF/2FCを含むコミュニケータ
  integer:: comm_work_2fc_dem      ! CADMAS-SURF/2FCとPARIDEMを含むコミュニケータ
  integer:: comm_work_2fc_str      ! CADMAS-SURF/2FCとSTRを含むコミュニケータ
  integer:: comm_work_mlicdsmg2fc_mlt ! STOC-ML,IC,DSとCADMAS-SURF/MG,2FCとMLT_AGENTを含むコミュニケータ
  INTEGER comm_work_mg_isph        ! 包含CADMAS 以及 ISPH 进程在内的通讯子 add by LK
!
  integer:: nsize_all,nrank_all    ! mpi_comm_worldにおけるサイズとランク  Size and rank at mpi_comm_world
  integer:: nsize_grp,nrank_grp    ! com_groupにおけるサイズとランク Size and rank in com_group
!
  integer,allocatable:: l_group(:) ! PE番号とグループ番号の対応リストCorrespondence list of PE number and group number
  integer,allocatable:: l_model(:) ! PE番号とモデル番号の対応リスト Correspondence list of PE number and model number
!
!
contains
!
  subroutine init_mpmd
    include 'mpif.h'
!    use mpi
!    use ifport
    interface
       integer(4) function getcwd(dirname)
         character(len=*) dirname
       end function getcwd
!
       integer(4) function chdir(dirname)
         character(len=*) dirname
       end function chdir
    end interface
    integer:: l,m,n,imodel,icode=9876,ierr
    character(256):: dirname
!!!
!!! (1) 初期化とmpi_comm_worldのサイズ・ランクの取得 Initialization and obtaining size / rank of mpi_comm_world
!!!
    call mpi_init(ierr)  ! MPI初始化函数,初始化MPI执行环境
         if(ierr/=0) goto 901
    call mpi_comm_size(mpi_comm_world,nsize_all,ierr) ! mpi_comm_world在mpif.h中声明,返回nsize_all
         if(ierr/=0) goto 902
    call mpi_comm_rank(mpi_comm_world,nrank_all,ierr)  ! 返回当前进程在全局中的rank号,放在nrank_all
         if(ierr/=0) goto 903
!!!
!!! (2) グループリストとモデルリストの作成  Create group list and model list
!!!
    allocate(l_group(0:nsize_all-1),l_model(0:nsize_all-1),stat=ierr)
         if(ierr/=0) goto 904
    call mpi_allgather(my_group,1,mpi_integer,l_group,1,mpi_integer &    ! l_groupの作成,设定l_group数组,将每个进程中的my_group信息收集起来
         &            ,mpi_comm_world,ierr)                               ! 排列好赋值给每一个进程中的l_group()
         if(ierr/=0) goto 905
    call mpi_allgather(my_model,1,mpi_integer,l_model,1,mpi_integer &    ! l_modelの作成,设定l_model数组
         &            ,mpi_comm_world,ierr)
         if(ierr/=0) goto 905
!!!
!!! (3) コミュニケータの分割(mpi_comm_world -> comm_group -> comm_model) 
!!! Split communicator (mpi_comm_world -> comm_group -> comm_model)
    call mpi_comm_split(mpi_comm_world,my_group,nrank_all,comm_group &        ! comm_groupの作成 设定通讯子comm_goup
         &            ,ierr)
         if(ierr/=0) goto 906
    call mpi_comm_size(comm_group,nsize_grp,ierr)  !
         if(ierr/=0) goto 902
    call mpi_comm_rank(comm_group,nrank_grp,ierr)  !返回进程在comm_group中的rank号nrank_grp 
         if(ierr/=0) goto 903
!
    imodel=my_model
    if( imodel==l_cadmas_2fc ) then
       imodel=l_cadmas_mg
!      cadmas_mgとcadmas_2fcは同じcomm_modelに属する
    elseif( imodel==l_stoc_ic.or.imodel==l_stoc_ds ) then
       imodel=l_stoc_ml
!      stoc_mlとstoc_icとstoc_dsは同じcomm_modelに属する
    endif
    call mpi_comm_split(mpi_comm_world,imodel,nrank_all,comm_model &          ! comm_modelの作成 设定通讯子comm_model
         &            ,ierr)
         if(ierr/=0) goto 907
!
    imodel=my_model !imodel用于对全局进程 进行 分割，imodel用于给每个进程设定一个用于分组的color
    if( imodel==l_stoc_ic.or.imodel==l_stoc_ds.or. &
      & imodel==l_cadmas_mg.or.imodel==l_cadmas_2fc ) then
       imodel=l_stoc_ml ! l_stoc_ml   = 0
    endif
    call mpi_comm_split(mpi_comm_world,imodel,nrank_all,comm_mlicdsmg2fc &          ! comm_mlicdsmg2fcの作成 设定通讯子comm_mlicdsmg2fc
         &            ,ierr)
         if(ierr/=0) goto 912
!!!
!!! (4) テンポラリで設定するコミュニケータの分割
!!!
    imodel=my_model
    if( imodel==l_stoc_ml.or.imodel==l_stoc_ic.or. &
         &   imodel==l_stoc_ds.or.imodel==l_stoc_dm ) then
       imodel=0
    else
       imodel=1
    endif
    call mpi_comm_split(mpi_comm_world,imodel,nrank_all,comm_work_mlicds_dm  &! comm_work_mlicds_dmの作成 设定通讯子comm_work_mlicds_dm
         &            ,ierr)
         if(ierr/=0) goto 911
!
    imodel=my_model
    if( imodel==l_stoc_ml.or.imodel==l_stoc_ic.or. &
         &   imodel==l_stoc_ds.or.imodel==l_stoc_oil ) then
       imodel=0
    else
       imodel=1
    endif
    call mpi_comm_split(mpi_comm_world,imodel,nrank_all,comm_work_mlicds_oil &! comm_work_mlicds_oilの作成 设定通讯子comm_work_mlicds_oil
         &            ,ierr)
         if(ierr/=0) goto 911
!
    imodel=my_model
    if( imodel==l_stoc_ic.or.imodel==l_cadmas_mg ) then
       imodel=0
    else
       imodel=1
    endif
    call mpi_comm_split(mpi_comm_world,imodel,nrank_all,comm_work_ic_mg &     ! comm_work_ic_mgの作成 设定通讯子comm_work_ic_mg
         &            ,ierr)
         if(ierr/=0) goto 911
!
    imodel=my_model
    if( imodel==l_cadmas_mg.or.imodel==l_cadmas_2fc ) then
       imodel=0
    else
       imodel=1
    endif
    call mpi_comm_split(mpi_comm_world,imodel,nrank_all,comm_work_mg_2fc &    ! comm_work_mg_2fcの作成 设定通讯子comm_work_mg_2fc
         &            ,ierr)
         if(ierr/=0) goto 911
!
    imodel=my_model
    if( imodel==l_cadmas_2fc.or.imodel==l_paridem ) then
       imodel=0
    else
       imodel=1
    endif
    call mpi_comm_split(mpi_comm_world,imodel,nrank_all,comm_work_2fc_dem &   ! comm_work_2fc_demの作成 设定通讯子comm_work_2fc_dem
         &            ,ierr)
         if(ierr/=0) goto 911
!
    imodel=my_model
    if( imodel==l_cadmas_2fc.or.imodel==l_str ) then
       imodel=0
    else
       imodel=1
    endif
    call mpi_comm_split(mpi_comm_world,imodel,nrank_all,comm_work_2fc_str &   ! comm_work_2fc_strの作成 设定通讯子comm_work_2fc_str
         &            ,ierr)
         if(ierr/=0) goto 911
!
    imodel=my_model
    if( imodel==l_stoc_ml.or.imodel==l_stoc_ic.or.imodel==l_stoc_ds.or.imodel==l_cadmas_mg.or. &
         &              imodel==l_cadmas_2fc.or.imodel==l_mlt_agent ) then
       imodel=0
    else
       imodel=1
    endif
    call mpi_comm_split(mpi_comm_world,imodel,nrank_all,comm_work_mlicdsmg2fc_mlt &    ! comm_work_mlicdsmg2fc_mltの作成 设定通讯子comm_work_mlicdsmg2fc_mlt
         &            ,ierr)
         if(ierr/=0) goto 911
!
    imodel=my_model
    IF ( imodel==l_cadmas_mg.or.imodel==l_sph_isph ) THEN
      imodel=0
    ELSE
      imodel=1
    ENDIF
    CALL mpi_comm_split(mpi_comm_world,imodel,nrank_all,comm_work_mg_isph,ierr)     ! 设定通讯子comm_work_mg_isph
    IF(ierr/=0) GOTO 911

!!!
!!! (5) stocグループとcadmasグループの実行ディレクトリを分ける
!!! Divide the execution directory of stoc group and cadmas group 
!!!                                                                 
    l=l_group(0)
    m=0
    do n=1,nsize_all-1
       if( l/=l_group(n) ) m=1                  ! 別グループを発見した場合にm=1とする When another group is found, m = 1
    enddo                                       !取决于前边line 115生成的l_group()  若存在coupling模型，则m=1
!
 m=0 ! ディレクトリを分ける機能は一旦封印する 将m设置为0以下部分不再执行
    if( m==1 ) then                             ! STOCとCADMASの連成ありのときディレクトリを分ける Separate directories when STOC and CADMAS are coupled
!       if(nrank_all==0) ierr=getcwd(dirname)                            ! 起動時のディレクトリ名の取得 Get directory name at startup
            if(ierr/=0) goto 908
!
       call mpi_bcast(dirname,256,mpi_character,0,mpi_comm_world,ierr)
            if(ierr/=0) goto 909
!
       if( m==1.and.my_group==l_stoc ) then
          dirname=trim(dirname)//'/stoc'
       else if( m==1.and.my_group==l_cadmas ) then
          dirname=trim(dirname)//'/cadmas'
       endif
!
!       ierr=chdir(dirname)                                              ! ディレクトリの移動
            if(ierr/=0) goto 910
!       ierr=getcwd(dirname)                                             ! 実行ディレクトリ名の取得
            if(ierr/=0) goto 908
    endif
!!!
!!!  (5) デバッグ出力  Debug output
!!!
    if( nrank_all==0 ) then                                             ! mpmd参加リストの出力 Output of mpmd participation list
       write(*,10) nsize_all
       do n=0,nsize_all-1
          write(*,20) n, c_group(l_group(n)), trim(c_model(l_model(n)))
       enddo
       write(*,*) ''
!
       write(*,30) c_group(my_group),nsize_grp
       m=0
       do n=0,nsize_all-1
          if( l_group(n)==my_group ) then
             write(*,40) m, trim(c_model(l_model(n)))
             m=m+1
          endif
       enddo
       write(*,*) ''
!
       if( nsize_all-nsize_grp>0 ) then
       write(*,30) c_group(2-my_group),nsize_all-nsize_grp   !!!add by LK
       m=0
       do n=0,nsize_all-1
          if( l_group(n)==2-my_group ) then   !!!add by LK
             write(*,40) m, trim(c_model(l_model(n)))
             m=m+1
          endif
       enddo
       write(*,*) ''
       endif
    endif
10 format('### MPMD Information ( SIZE=',I4,' ) ###',/, &
        & 'Rank Group  Module')
20 format(i4,1x,a6,1x,a)
30 format('### MPMD Subgroup Information:  ( GROUP=',A6,' SIZE=',I4,' ) ###',/, &
        & 'Rank Module')
40 format(i4,1x,a)
!
    return
!
901 continue
    write(*,*) 'Error at mpi_comm_size(init_mpmd)'
    goto 999
902 continue
    write(*,*) 'Error at mpi_comm_size(init_mpmd)'
    goto 999
903 continue
    write(*,*) 'Error at mpi_comm_rank(init_mpmd)'
    goto 999
904 continue
    write(*,*) 'Error at allocate(init_mpmd)'
    goto 999
905 continue
    write(*,*) 'Error at mpi_allgather(init_mpmd)'
    goto 999
906 continue
    write(*,*) 'Error at mpi_comm_split(init_mpmd):comm_group'
    goto 999
907 continue
    write(*,*) 'Error at mpi_comm_split(init_mpmd):comm_model'
    goto 999
908 continue
    write(*,*) 'Error at getcwd(init_mpmd)'
    goto 999
909 continue
    write(*,*) 'Error at mpi_bcast(init_mpmd)'
    goto 999
910 continue
    write(*,*) 'Error at chdir(init_mpmd)'
    goto 999
911 continue
    write(*,*) 'Error at mpi_comm_split(init_mpmd):comm_work_*'
    goto 999
912 continue
    write(*,*) 'Error at mpi_comm_split(init_mpmd):comm_mlicdsmg2fc'
    goto 999
999 continue
    call mpi_abort(mpi_comm_world,icode,ierr)
  end subroutine init_mpmd
!
end module mod_comm
