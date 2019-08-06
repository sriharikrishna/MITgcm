module OAD_tape

#ifdef ALLOW_OPENAD_COMPRESSION
  use cpc 
#endif
  implicit none

  private :: increment , dtt, itt, ltt, stt, & 
       init, dump_tapestats, & 
       dt_grow, it_grow, lt_grow, st_grow, &
       push_d0, push_i0, push_d1, push_i1, & 
       pop_d0, pop_i0, pop_d1, pop_i1, & 
       push_d4, push_d6, & 
       pop_d4, pop_d6, &
       cp_tape_unit
    
  public :: &
       oad_dt, oad_dt_ptr, oad_dt_sz, oad_dt_grow, &
       oad_it, oad_it_ptr, oad_it_sz, oad_it_grow, &
       oad_lt, oad_lt_ptr, oad_lt_sz, oad_lt_grow, &
       oad_st, oad_st_ptr, oad_st_sz, oad_st_grow, &
       oad_chunk_size, &
       oad_tape_init, &
       oad_dump_tapestats, & 
       oad_tape_push, oad_tape_pop, &
       cp_write_tape, cp_read_tape, &
       cp_open_tape, cp_close_tape, &
       cp_read_tape_state, cp_write_tape_state
       
  double precision, dimension(:), allocatable :: oad_dt, dtt
  integer, dimension(:), allocatable :: oad_it, itt
  logical, dimension(:), allocatable :: oad_lt, ltt
  character(80), dimension(:), allocatable :: oad_st, stt
  integer :: oad_dt_ptr=0, oad_it_ptr=0
  integer :: oad_dt_sz=0, oad_it_sz=0
  integer :: oad_lt_ptr=0, oad_st_ptr=0
  integer :: oad_lt_sz=0, oad_st_sz=0
  integer :: increment
  integer :: oad_chunk_size
  integer :: cp_tape_unit

  interface oad_tape_init
    module procedure init
  end interface 

  interface oad_dump_tapestats
     module procedure dump_tapestats
  end interface

  interface oad_dt_grow
    module procedure dt_grow
  end interface

  interface oad_it_grow
    module procedure it_grow
  end interface

  interface oad_lt_grow
    module procedure lt_grow
  end interface

  interface oad_st_grow
    module procedure st_grow
  end interface

  interface oad_tape_push
     module procedure push_d0, push_i0
     module procedure push_d1, push_i1
     module procedure push_d4, push_d6
  end interface

  interface oad_tape_pop
     module procedure pop_d0, pop_i0
     module procedure pop_d1, pop_i1
     module procedure pop_d4, pop_d6
  end interface

!Logical Tape Store/Restore

  interface cp_open_tape
     module procedure open_tape_i
  end interface

  interface cp_close_tape
     module procedure close_tape_i
  end interface

  interface cp_read_tape
     module procedure read_tape_i
  end interface

  interface cp_write_tape
     module procedure write_tape_i
  end interface

!Tape Status Store
  interface cp_read_tape_state
     module procedure read_tape_state_i
  end interface

  interface cp_write_tape_state
     module procedure write_tape_state_i
  end interface
contains

  subroutine init
    implicit none
    integer :: initialSize=1048576
    increment=16777216
    oad_dt_ptr=1
    oad_dt_sz=initialSize
    oad_it_ptr=1
    oad_it_sz=initialSize
    oad_lt_ptr=1
    oad_lt_sz=initialSize
    oad_st_ptr=1
    oad_st_sz=initialSize 

    !DT
    if (allocated(oad_dt)) then 
       deallocate(oad_dt)
    end if
    allocate(oad_dt(oad_dt_sz))
    ! IT
    if (allocated(oad_it)) then 
       deallocate(oad_it)
    end if
    allocate(oad_it(oad_it_sz))
    ! LT
    if (allocated(oad_lt)) then 
       deallocate(oad_lt)
    end if
    allocate(oad_lt(oad_lt_sz))
    ! ST
    if (allocated(oad_st)) then 
       deallocate(oad_st)
    end if
    allocate(oad_st(oad_st_sz))

  end subroutine init

  subroutine dump_tapestats()
    write(*,'(3(A,I9))',ADVANCE='NO') & 
         ' TD:',oad_dt_ptr,' TI:',oad_it_ptr, ' TS:',oad_st_ptr
  end subroutine dump_tapestats

  subroutine dt_grow
    integer status
    print *, "OAD: DT+ ", oad_dt_sz
    allocate(dtt(oad_dt_sz),STAT=status)
    if (status .gt. 0 ) then
       print *,'OAD: allocation (1)failed with', status
       stop
    end if
    dtt=oad_dt
    deallocate(oad_dt)
    allocate(oad_dt(oad_dt_sz+increment),STAT=status)
    if (status .gt. 0 ) then
       print *,'OAD: allocation (2)failed with', status
       stop
    end if
    oad_dt(1:oad_dt_sz) = dtt
    deallocate(dtt)
    oad_dt_sz=oad_dt_sz+increment
  end subroutine dt_grow

  subroutine it_grow
    integer status
    print *, "OAD: IT+ ", oad_it_sz
    allocate(itt(oad_it_sz),STAT=status)
    if (status .gt. 0 ) then
       print *,'OAD: allocation (1)failed with', status
       stop
    end if
    itt=oad_it
    deallocate(oad_it)
    allocate(oad_it(oad_it_sz+increment),STAT=status)
    if (status .gt. 0 ) then
       print *,'OAD: allocation (2)failed with', status
       stop
    end if
    oad_it(1:oad_it_sz) = itt
    deallocate(itt)
    oad_it_sz=oad_it_sz+increment
  end subroutine it_grow

  subroutine lt_grow
    integer status
    print *, "OAD: LT+ ", oad_lt_sz
    allocate(ltt(oad_lt_sz),STAT=status)
    if (status .gt. 0 ) then
       print *,'OAD: allocation (1)failed wlth', status
       stop
    end if
    ltt=oad_lt
    deallocate(oad_lt)
    allocate(oad_lt(oad_lt_sz+increment),STAT=status)
    if (status .gt. 0 ) then
       print *,'OAD: allocation (2)failed wlth', status
       stop
    end if
    oad_lt(1:oad_lt_sz) = ltt
    deallocate(ltt)
    oad_lt_sz=oad_lt_sz+increment
  end subroutine lt_grow

  subroutine st_grow
    integer status
    print *, "OAD: ST+ ", oad_st_sz
    allocate(stt(oad_st_sz),STAT=status)
    if (status .gt. 0 ) then
       print *,'OAD: allocation (1)failed wsth', status
       stop
    end if
    stt=oad_st
    deallocate(oad_st)
    allocate(oad_st(oad_st_sz+increment),STAT=status)
    if (status .gt. 0 ) then
       print *,'OAD: allocation (2)failed wsth', status
       stop
    end if
    oad_st(1:oad_st_sz) = stt
    deallocate(stt)
    oad_st_sz=oad_st_sz+increment
  end subroutine st_grow

  subroutine push_d0(v)
    implicit none
    double precision :: v
    if(oad_dt_sz .lt. oad_dt_ptr+1) call oad_dt_grow()
    oad_dt(oad_dt_ptr)=v; oad_dt_ptr=oad_dt_ptr+1
  end subroutine push_d0

  subroutine push_i0(v)
    implicit none
    integer :: v
    if(oad_it_sz .lt. oad_it_ptr+1) call oad_it_grow()
    oad_it(oad_it_ptr)=v; oad_it_ptr=oad_it_ptr+1
  end subroutine push_i0

  subroutine push_d1(v)
    implicit none
    double precision :: v(:)
    integer :: chunk
    chunk=size(v,1)
    if(oad_dt_sz .lt. oad_dt_ptr+chunk) call oad_dt_grow()
    oad_dt(oad_dt_ptr:oad_dt_ptr+chunk-1)=v; oad_dt_ptr=oad_dt_ptr+chunk
  end subroutine push_d1

  subroutine push_i1(v)
    implicit none
    integer :: v(:)
    integer :: chunk
    chunk=size(v,1)
    if(oad_it_sz .lt. oad_it_ptr+chunk) call oad_it_grow()
    oad_it(oad_it_ptr:oad_it_ptr+chunk-1)=v; oad_it_ptr=oad_it_ptr+chunk
  end subroutine push_i1

  subroutine push_d4(v)
    implicit none
    double precision :: v(:,:,:,:)
    integer :: chunk(1), dims(4)
    dims=shape(v)
    chunk(1)=dims(1)*dims(2)*dims(3)*dims(4)
    do while (oad_dt_sz .lt. oad_dt_ptr+chunk(1)) 
       call oad_dt_grow()
    end do
    oad_dt(oad_dt_ptr:oad_dt_ptr+chunk(1)-1)=reshape(v,chunk) 
    oad_dt_ptr=oad_dt_ptr+chunk(1)
  end subroutine push_d4

  subroutine push_d6(v)
    implicit none
    double precision :: v(:,:,:,:,:,:)
    integer :: chunk(1), dims(6)
    dims=shape(v)
    chunk(1)=dims(1)*dims(2)*dims(3)*dims(4)*dims(5)*dims(6)
    do while (oad_dt_sz .lt. oad_dt_ptr+chunk(1)) 
       call oad_dt_grow()
    end do
    oad_dt(oad_dt_ptr:oad_dt_ptr+chunk(1)-1)=reshape(v,chunk) 
    oad_dt_ptr=oad_dt_ptr+chunk(1)
  end subroutine push_d6

  subroutine pop_d0(v)
    implicit none
    double precision :: v
    oad_dt_ptr=oad_dt_ptr-1
    v=oad_dt(oad_dt_ptr)
  end subroutine pop_d0

  subroutine pop_i0(v)
    implicit none
    integer :: v
    oad_it_ptr=oad_it_ptr-1
    v=oad_it(oad_it_ptr)
  end subroutine pop_i0

  subroutine pop_d1(v)
    implicit none
    double precision :: v(:)
    integer :: chunk
    chunk=size(v,1)
    oad_dt_ptr=oad_dt_ptr-chunk    
    v=oad_dt(oad_dt_ptr:oad_dt_ptr+chunk-1)
  end subroutine pop_d1
  
  subroutine pop_i1(v)
    implicit none
    integer :: v(:)
    integer :: chunk
    chunk=size(v,1)
    oad_it_ptr=oad_it_ptr-chunk
    v=oad_it(oad_it_ptr:oad_it_ptr+chunk-1)
  end subroutine pop_i1

  subroutine pop_d4(v)
    implicit none
    double precision :: v(:,:,:,:)
    integer :: chunk, dims(4)
    dims=shape(v)
    chunk=dims(1)*dims(2)*dims(3)*dims(4)
    oad_dt_ptr=oad_dt_ptr-chunk
    v=reshape(oad_dt(oad_dt_ptr:oad_dt_ptr+chunk-1),dims) 
  end subroutine pop_d4

  subroutine pop_d6(v)
    implicit none
    double precision :: v(:,:,:,:,:,:)
    integer :: chunk, dims(6)
    dims=shape(v)
    chunk=dims(1)*dims(2)*dims(3)*dims(4)*dims(5)*dims(6)
    oad_dt_ptr=oad_dt_ptr-chunk
    v=reshape(oad_dt(oad_dt_ptr:oad_dt_ptr+chunk-1),dims) 
  end subroutine pop_d6

  subroutine open_tape_i(fileno,tapetypestr,isread)
    implicit none
#ifdef ALLOW_USE_MPI
include "mpif.h"
#endif
    integer ::fileno
    integer ::rank
    integer ::mpirc
    logical ::isread
    character*128 ::fname ! file name
    character*128 ::tapetypestr 
    fname(1:128) = " "
#ifdef ALLOW_OPENAD_COMPRESSION
    if(isread.eqv..true.) then
      if(trim(tapetypestr).eq."integer") then
        call cp_rd_open(fileno,"oad_aux_i_tp")
      else if(trim(tapetypestr).eq."logical") then
        call cp_rd_open(fileno,"oad_aux_l_tp")
      else if(trim(tapetypestr).eq."double") then
        call cp_rd_open(fileno,"oad_aux_d_tp")
      else if(trim(tapetypestr).eq."string") then
        call cp_rd_open(fileno,"oad_aux_s_tp")
      else
        write(*,*) "Matching tapetypestr not found", trim(tapetypestr)
        stop
      end if
    else 
      if(trim(tapetypestr).eq."integer") then
        call cp_wr_open(fileno,"oad_aux_i_tp")
      else if(trim(tapetypestr).eq."logical") then
        call cp_wr_open(fileno,"oad_aux_l_tp")
      else if(trim(tapetypestr).eq."double") then
        call cp_wr_open(fileno,"oad_aux_d_tp")
      else if(trim(tapetypestr).eq."string") then
        call cp_wr_open(fileno,"oad_aux_s_tp")
      else
        write(*,*) "Matching tapetypestr not found", trim(tapetypestr)
        stop
      end if
    end if
#else
    rank=0
#ifdef ALLOW_USE_MPI
    call mpi_comm_rank(MPI_COMM_WORLD,rank, mpirc)
#endif
      if(trim(tapetypestr).eq."integer") then
        write(fname,'(A,I3.3,A,I3.3)') 'oad_aux_i_tp.',fileno,'.',rank
      else if(trim(tapetypestr).eq."logical") then
        write(fname,'(A,I3.3,A,I3.3)') 'oad_aux_l_tp.',fileno,'.',rank
      else if(trim(tapetypestr).eq."double") then
        write(fname,'(A,I3.3,A,I3.3)') 'oad_aux_d_tp.',fileno,'.',rank
      else if(trim(tapetypestr).eq."string") then
        write(fname,'(A,I3.3,A,I3.3)') 'oad_aux_s_tp.',fileno,'.',rank
      else
        write(*,*) "Matching tapetypestr not found", trim(tapetypestr)
        stop
      end if
#ifdef DEBUG_OPENAD_COMPRESS
    write(*,*) "open_tape_i fname ::",fname,"::",TRIM(fname)  
#endif
    open( UNIT=cp_tape_unit,FILE=TRIM(fname),FORM='unformatted',STATUS='UNKNOWN' )
#endif
  end subroutine 

  subroutine close_tape_i()
    implicit none
#ifdef ALLOW_OPENAD_COMPRESSION
    call cpc_close()
#else
    close( UNIT=cp_tape_unit)
#endif
  end subroutine

  subroutine write_tape_i(fileno,tapetypestr)
    implicit none
    integer fileno,s
    character*128 tapetypestr 
#ifdef DEBUG_OPENAD_COMPRESS
    write(*,*) 'DIVA Trying to open file ',trim(tapetypestr),'_tape', fileno
#endif
    call cp_open_tape(fileno,tapetypestr,.FALSE.)
#ifdef DEBUG_OPENAD_COMPRESS
    print *, 'DIVA Writing to file ',trim(tapetypestr),'_tape', fileno
#endif
    if(trim(tapetypestr).eq."integer") then
#ifdef ALLOW_OPENAD_COMPRESSION
      call CompressWrC_integer_1(oad_it(1:oad_it_ptr-1))
#else
      write (unit=cp_tape_unit) oad_it(1:oad_it_ptr-1)
#endif
    else if(trim(tapetypestr).eq."double") then
#ifdef ALLOW_OPENAD_COMPRESSION
      call CompressWrC_real_1(oad_dt(1:oad_dt_ptr-1))
#else
      write (unit=cp_tape_unit) oad_dt(1:oad_dt_ptr-1)
#endif
    else if(trim(tapetypestr).eq."logical") then
#ifdef ALLOW_OPENAD_COMPRESSION
      call CompressWrC_bool_1(oad_lt(1:oad_lt_ptr-1))
#else
      write (unit=cp_tape_unit) oad_lt(1:oad_lt_ptr-1)
#endif
    else if(trim(tapetypestr).eq."string") then
#ifdef ALLOW_OPENAD_COMPRESSION
      do s=1, oad_st_ptr-1
        call CompressWrC_string(oad_st(s))
      end do
#else
      write (unit=cp_tape_unit) oad_st(1:oad_st_ptr-1)
#endif
    else
      write(*,*) "Unable to find tape nameed ", trim(tapetypestr)
      stop
    end if
    call cp_close_tape
  end subroutine

  subroutine read_tape_i(fileno,tapetypestr)
    implicit none
#ifdef ALLOW_USE_MPI
include "mpif.h"
#endif
    integer fileno, mpirc, rank,s
    character*128 :: tapetypestr
#ifdef ALLOW_USE_MPI
    call mpi_comm_rank(MPI_COMM_WORLD,rank, mpirc)
#endif
    call cp_open_tape(fileno,tapetypestr,.TRUE.)
    if(trim(tapetypestr).eq."integer") then
#ifdef ALLOW_OPENAD_COMPRESSION
      call CompressRdC_integer_1(oad_it(1:oad_it_ptr-1))
#else
      read (unit=cp_tape_unit) oad_it(1:oad_it_ptr-1)
#endif
    else if(trim(tapetypestr).eq."double") then
#ifdef ALLOW_OPENAD_COMPRESSION
      call CompressRdC_real_1(oad_dt(1:oad_dt_ptr-1))
#else
      read (unit=cp_tape_unit) oad_dt(1:oad_dt_ptr-1)
#endif
    else if(trim(tapetypestr).eq."logical") then
#ifdef ALLOW_OPENAD_COMPRESSION
      call CompressWrC_bool_1(oad_lt(1:oad_lt_ptr-1))
#else
      read (unit=cp_tape_unit) oad_lt(1:oad_lt_ptr-1)
#endif
    else if(trim(tapetypestr).eq."string") then
#ifdef ALLOW_OPENAD_COMPRESSION
      do s=1, oad_st_ptr-1
        call CompressRdC_string(oad_st(s))
      end do
#else
      read (unit=cp_tape_unit) oad_st(1:oad_st_ptr-1)
#endif
    else
      write(*,*) "Unable to find tape nameed ", tapetypestr
      stop
    end if
    call cp_close_tape
  end subroutine

!Tape State
  subroutine write_tape_state_i(fileno)
    implicit none
#ifdef ALLOW_USE_MPI
include "mpif.h"
#endif
    integer fileno
    integer rank
    integer mpirc
    logical exst
    character*128 fname ! file name
#ifdef ALLOW_USE_MPI
    call mpi_comm_rank(MPI_COMM_WORLD,rank, mpirc)
#endif
    write(fname,'(A,I3.3,A,I3.3)') 'oad_aux_tape_state.',fileno,'.',rank
    exst =.false.
    inquire(file=fname,exist=exst)
    if (exst.eqv..true.) then
      print *, 'DIVA NOT Writing tape_state'
      return
    end if
    open( UNIT=77,FILE=TRIM(fname),FORM='formatted')
    write(unit=77,fmt=*) oad_dt_ptr
    write(unit=77,fmt=*) oad_dt_sz
    write(unit=77,fmt=*) oad_it_ptr
    write(unit=77,fmt=*) oad_it_sz
    write(unit=77,fmt=*) oad_lt_ptr
    write(unit=77,fmt=*) oad_lt_sz
    write(unit=77,fmt=*) oad_st_ptr
    write(unit=77,fmt=*) oad_st_sz
    close(unit=77)
    call cp_write_tape(fileno,"integer")
    call cp_write_tape(fileno,"double")
    call cp_write_tape(fileno,"logical")
    call cp_write_tape(fileno,"string")
  end subroutine

  subroutine read_tape_state_i(fileno) 
    implicit none
#ifdef ALLOW_USE_MPI
include "mpif.h"
#endif
    integer fileno
    integer rank
    integer mpirc
    logical exst
    character*128 fname ! file name

    rank=0
#ifdef ALLOW_USE_MPI
    call mpi_comm_rank(MPI_COMM_WORLD,rank, mpirc)
#endif
    write(fname,'(A,I3.3,A,I3.3)') 'oad_aux_tape_state.',fileno,'.',rank
    exst =.false.
    inquire(file=fname,exist=exst)
    if (exst.eqv..true.) then
      open( UNIT=77,FILE=TRIM(fname),FORM='formatted')
      read(unit=77,fmt=*) oad_dt_ptr
      read(unit=77,fmt=*) oad_dt_sz
      read(unit=77,fmt=*) oad_it_ptr
      read(unit=77,fmt=*) oad_it_sz
      read(unit=77,fmt=*) oad_lt_ptr
      read(unit=77,fmt=*) oad_lt_sz
      read(unit=77,fmt=*) oad_st_ptr
      read(unit=77,fmt=*) oad_st_sz
      close(unit=77)
      print *, 'DIVA found tape_state_file', fileno, rank, oad_dt_sz, oad_dt_ptr
    else
      print *, 'DIVA tape_state_file not found', fname
      stop 'DIVA State file not found'
    end if

    !DT
    if (allocated(oad_dt)) then
       deallocate(oad_dt)
    end if
    allocate(oad_dt(oad_dt_sz))
    ! IT
    if (allocated(oad_it)) then
       deallocate(oad_it)
    end if
    allocate(oad_it(oad_it_sz))
    ! LT
    if (allocated(oad_lt)) then
       deallocate(oad_lt)
    end if
    allocate(oad_lt(oad_lt_sz))
    ! ST
    if (allocated(oad_st)) then
       deallocate(oad_st)
    end if
    allocate(oad_st(oad_st_sz))

    if (exst.eqv..true.) then
      call cp_read_tape(fileno,"integer")
      call cp_read_tape(fileno,"double ")
      call cp_read_tape(fileno,"logical")
      call cp_read_tape(fileno,"string ")
    end if

  end subroutine 
end module
