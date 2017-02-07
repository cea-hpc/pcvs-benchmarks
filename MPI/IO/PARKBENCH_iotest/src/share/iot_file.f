!     $Id: iot_file.f,v 1.20 1997/11/24 09:42:27 oliver Exp $
!
!     IOT: MPI IO Test Suite
!
!     Copyright Fujitsu Ltd. 1997
!
!     Function:
!     File handling functions
!     Release 1.0
!
!     Description:
!     Functions to handle input of data from user file
!     and output to statistics file
!
!     Interface:
!     use iot_file
!
!     External components:
!     iot_defs, iot_string, iot_key
!-------------------------------------------------------------------


      module iot_file

      contains

!     get key, value strings from user input file
      subroutine get_test_strings(userfile, eoferr, ierr)
      use iot_defs
      use iot_string
      use iot_key
      implicit none

      integer :: userfile, ierr, eoferr
      integer,save :: cnt
      integer :: size, key, i
      character(IOT_MAX_LINE_LEN),save :: line
      character(IOT_MAX_STRING_LEN) :: value
      logical,save :: firstcall = .true.
      logical :: firstclass
!     delete all old value strings
      do i = 1, IOT_NUM_KEYS
         call deallocate_string(ios(i), ierr)
      end do
      if (firstcall) then
         call get_line(userfile, line, cnt, eoferr)
      end if
      firstclass = .true.
      do
!     exit loop if end of file is empty line
      if (cnt == 0) then
         exit
      end if
      call check_key(line, cnt, key, ierr)
      if (ierr /= IOT_SUCCESS) then
         write(fout, *) "ERROR: Invalid key on line:"
         write(fout, *) line(1:cnt)
         return
      end if
      call get_value(line, cnt, value, size, ierr)
      if (ierr /= IOT_SUCCESS) then
         write(fout, *) "ERROR: Invalid value on line:"
         write(fout, *) line(1:cnt)
         return
      end if
!     exit if found next classname
      if (key == IOT_KEY_CLASSNAME) then
         if (firstcall) then
            firstcall = .false.
            exit
         end if
         if (firstclass) then
            firstclass = .false.
         else
            exit
         end if
      end if
!     save key, value pair
      call allocate_string(ios(key), ierr)
      if (ierr /= IOT_SUCCESS) then
         write(fout, *) "ERROR: Repeated key on line:"
         write(fout, *) line(1:cnt)
      end if
      ios(key) = value(1:size)
!     exit loop if end of file
      if (eoferr /= IOT_SUCCESS) then
         exit
      end if
      call get_line(userfile, line, cnt, eoferr)
      end do
      return
      end subroutine

!-------------------------------------------------------------------
!     read a record using standard Fortran IO 
      subroutine get_line(unit, line, cnt, ierr)
      use iot_defs
      use iot_string
      implicit none

      integer :: unit, cnt, ierr
      character(*) :: line
      character(len=20) :: fmt, tmp
      integer :: ios

      write (tmp, *) IOT_MAX_STRING_LEN
      fmt = "(a"// tmp(1:len_trim(tmp)) //")"
      ierr = 0
      line(1:len(line)) = repeat(" ",len(line))
!     skip comment lines and blank lines
      do
      read (unit=unit,fmt=fmt,iostat=ios) line
      if (ios < 0) exit
      line = adjustl(line)
      cnt = len_trim(line)
      if ((cnt /= 0) .and. (index(line, "#") /= 1)) exit
      end do
      if (ios >= 0) then
         ierr = IOT_SUCCESS
         return
      else
         ierr = IOT_ERR_EOF
         line = adjustl(line)
         cnt = len_trim(line)
         if ((cnt /= 0) .and. (index(line, "#") == 1)) cnt = 0
         return
      end if
      end subroutine

!-------------------------------------------------------------------
!     check input line for valid key
      subroutine check_key(line, cnt, key, ierr)
      use iot_defs
      use iot_string
      use iot_key
      implicit none

      integer :: cnt, key, ierr
      character(*) :: line
      integer :: ptr, i
      ptr = index(line, " ") - 1
      ierr = IOT_ERR_KEY
      do i = 1, IOT_NUM_KEYS
         if (iokey(i) == line(1:ptr)) then
            ierr = IOT_SUCCESS
            key = i
            exit
         end if
      end do
      return
      end subroutine

!-------------------------------------------------------------------
!     get value string 
      subroutine get_value(line, cnt, value, size, ierr)
      use iot_defs
      use iot_string
      implicit none

      integer :: cnt, ierr, size, start
      character(*) :: line
      character(*) :: value
      ierr = IOT_SUCCESS
      start = index(line, " ") + 1
      value = line(start:cnt)
      value = adjustl(value)
      size = min(len_trim(value),cnt-start+1)
      return
      end subroutine

!-------------------------------------------------------------------
!     check test name
      subroutine check_testname(test, ierr)
      use iot_defs
      use iot_string
      use iot_key
      implicit none

      integer :: test, ierr
      integer :: i
      ierr = IOT_ERR_TEST
      do i = 1, IOT_NUM_TESTS
         if (iotestname(i) == ios(IOT_KEY_TESTNAME)) then
            ierr = IOT_SUCCESS
            test = i
            exit
         end if
      end do
      return
      end subroutine

!-------------------------------------------------------------------
      subroutine print_keys()
      use iot_defs
      use iot_string
      use iot_key
      implicit none

      integer :: i
      do i = 1, IOT_NUM_KEYS
         write(fout,*) i, ios(i)%s(1:len(ios(i)))
      end do
      return
      end subroutine

!-------------------------------------------------------------------
!     broadcast test information
      subroutine broadcast_data(comm, node, ierr)
      use iot_defs
      use iot_string
      use iot_key
      implicit none

      integer :: ierr, comm, node
      integer :: i, cnt
      character(len=IOT_MAX_BUF_LEN),pointer :: buf
      integer,dimension(IOT_NUM_KEYS+1) :: ptr
!     pack information on node 0
      if (node == 0) then
         cnt = 0
         ptr(1) = 1
         do i = 1, IOT_NUM_KEYS
            cnt = cnt+len(ios(i))
            ptr(i+1) = cnt+1
         end do
         allocate(buf,stat=ierr)
         if (ierr /= 0) then
            ierr = IOT_ERR_ALLOC
            call mpi_abort(comm, ierr, ierr)
         end if
         do i = 1, IOT_NUM_KEYS
            if (ptr(i+1)-ptr(i) .gt. 0) then
               buf(ptr(i):ptr(i+1)-1) = ios(i)
            end if
         end do
      end if

!     broadcast packed buffer 
      call mpi_bcast(cnt, 1, MPI_INTEGER, &
                     0, comm, ierr)
      if (node /= 0) then
         allocate(buf,stat=ierr)
         if (ierr /= 0) then
            ierr = IOT_ERR_ALLOC
            call mpi_abort(comm, ierr, ierr)
         end if
      end if
      call mpi_bcast(buf, cnt, MPI_CHARACTER, &
                     0, comm, ierr)
      call mpi_bcast(ptr, IOT_NUM_KEYS+1, MPI_INTEGER, &
                     0, comm, ierr)

!     unpack buffer 
      if (node /= 0) then
         do i = 1, IOT_NUM_KEYS
            call deallocate_string(ios(i), ierr)
            if (ptr(i+1)-ptr(i) .gt. 0) then
               call allocate_string(ios(i), ierr)
               if (ierr /= IOT_SUCCESS) then
                  write(fout,*) &
        "ERROR: Failed to allocate enough memory"
                  call mpi_abort(comm, ierr, ierr)
               end if
               ios(i) = buf(ptr(i):ptr(i+1)-1)
            end if
         end do
      end if
      deallocate(buf)
      ierr = IOT_SUCCESS
      return
      end subroutine

!-------------------------------------------------------------------
!     get data file name and file hints info structure
      subroutine get_file_info(datafilename, lname, info, ierr)
      use iot_defs
      use iot_string
      use iot_key
      implicit none

      character(*) :: datafilename
      integer :: lname, info, ierr
      integer :: num, i

!     create file name and file info structure
      lname = len(ios(IOT_KEY_FILENAME))
      datafilename(1:lname) = ios(IOT_KEY_FILENAME)

      call mpi_info_create(info, ierr)
      if (ierr /= MPI_SUCCESS) then
         write(*,*) "ERROR: Error in info create"
         return
      end if
      do i = IOT_FIRST_HINT, IOT_LAST_HINT
         if (ios(i)%inuse) then
            call mpi_info_set(info, iokey(i), &
              ios(i)%s(1:len(ios(i))), ierr)
            if (ierr /= MPI_SUCCESS) then
               write(*,*) "ERROR: Error in info add"
            end if
         end if
      end do

!     set to NULL while waiting for fix
!      info = MPI_INFO_NULL
!      ierr = IOT_SUCCESS

      return
      end subroutine

!-------------------------------------------------------------------
      subroutine init_stat(statsize, ierr)
      use iot_defs
      use iot_string
      use iot_key
      implicit none

      integer :: statsize, ierr
      integer :: ierra, ierrb, ierrc


!     allocate statistics arrays
      maxstatsize = statsize
      allocate(sval(maxstatsize),stat=ierra)
      allocate(skey(maxstatsize),stat=ierrb)
      allocate(stype(maxstatsize),stat=ierrc)
      if ((ierra /= 0).or.(ierrb /= 0).or.(ierrc /= 0)) then
         ierr = IOT_ERR_ALLOC
!         call mpi_abort(comm, ierr, ierr)
         stop
      end if


!     initialise pointer to empty stat array
      sidx = 1
!     find nodename
      call mpi_get_processor_name(nodename, lnodename, ierr)
!     set starting timestamp
      starttime = repeat(" ",len(starttime))
      call date_and_time(starttime(1:8), starttime(10:19), &
           starttime(21:25))
      ierr = IOT_SUCCESS
      return
      end subroutine

!-------------------------------------------------------------------
      subroutine finalize_stat(node, statfile, ierr)
      use iot_defs
      use iot_string
      use iot_key
      implicit none

      integer :: node, statfile, ierr
      character(len=IOT_FORMAT_LEN) :: linefmt
      character(IOT_MAX_LINE_LEN) :: line

      if (node == 0) then
!        set finishing timestamp
         finishtime = repeat(" ",len(finishtime))
         call date_and_time(finishtime(1:8), finishtime(10:19), &
            finishtime(21:25))

!        construct format specifiers
         write (linefmt,*) "(a",IOT_OFKEY_LEN,",a)"
!        blank line
         write(statfile,*) 
!        write comment separator to stat file
         write(line,"(a)") "## Test group finish " // repeat("#", 50)
         write(statfile,"(a)") line(1:70)
!        blank line
         write(statfile,*) 
         write(statfile, linefmt) &
            sharedkey(IOT_STAT_FINISH)//repeat(" ",IOT_OFKEY_LEN), &
            trim(finishtime)
!        blank line
         write(statfile,*) 
      end if

!     deallocate statistics arrays
!      deallocate(sval,skey,stype)
      ierr = IOT_SUCCESS
      return
      end subroutine

!-------------------------------------------------------------------
      subroutine save_stat(key, val, type, ierr)
      use iot_defs
      use iot_string
      use iot_key
      implicit none

      integer :: ierr, key, type
      real(kind=8) :: val
      if (sidx .gt. maxstatsize) then
         ierr = IOT_ERR_KEY
         return
      end if
      skey(sidx) = key
      stype(sidx) = type
      sval(sidx) = val
      sidx = sidx+1
      ierr = IOT_SUCCESS
      return
      end subroutine

!-------------------------------------------------------------------
!     write inpout keys to file using Fortran IO
      subroutine write_keys2(comm, node, nprocs, statfile, &
           datafilename, info, statkey, ierr)
      use iot_defs
      use iot_string
      use iot_key
      implicit none

      integer :: comm, node, nprocs, statfile, info, ierr
      character(len=*) :: datafilename
      character(len=*),dimension(*) :: statkey
      character(len=1),dimension(:),allocatable :: nodenames
      integer,dimension(:),allocatable :: dispnames, lengthnames
      integer :: i, j, cnt
      character(len=IOT_FORMAT_LEN) :: intfmt, realfmt
      character(len=IOT_FORMAT_LEN) :: charfmt, strfmt, keyfmt, linefmt
      character(IOT_MAX_LINE_LEN) :: line
      integer,dimension(:),pointer :: key
      real(kind=8),dimension(:),pointer :: val
      integer,dimension(:),pointer :: type
      real(kind=8) :: tick

      if (node == 0) then
!        construct format specifiers
         write (realfmt,*) "(e",IOT_OFVAL_LEN,".",IOT_OFVAL_DP,")"
         write (intfmt,*) "(i",IOT_OFVAL_LEN,")"
         write (charfmt,*) "(a",IOT_OFVAL_LEN,")"
         write (strfmt,*) "(",IOT_OFVAL_LEN,"a1)"
         write (keyfmt,*) "(a",IOT_OFKEY_LEN,")"
         write (linefmt,*) "(a",IOT_OFKEY_LEN,",a)"
      end if

!     gather process names to root
      if (node == 0) then
         allocate(dispnames(nprocs))
         allocate(lengthnames(nprocs))
      end if
      call mpi_gather(lnodename, 1, MPI_INTEGER, &
           lengthnames, 1, MPI_INTEGER, 0, comm, ierr)
      if (node == 0) then
         dispnames(1) = 0
         do i = 2, nprocs
            dispnames(i) = dispnames(i-1)+lengthnames(i-1)
         end do
         allocate(nodenames(dispnames(nprocs)+lengthnames(nprocs)))
      end if
      call mpi_gatherv(nodename, lnodename, MPI_CHARACTER, &
        nodenames, lengthnames, dispnames, MPI_CHARACTER, 0, comm, ierr)
      if (node == 0) then
!        write comment separator to stat file
         write(line,"(a)") "## General run information " // repeat("#", 50)
         write(statfile,"(a)") line(1:70)
!        blank line
         write(statfile,*) 
!        write process names to file
         do j = 0, nprocs-1
            if (lengthnames(j+1) <= IOT_OFVAL_LEN) then
               write(line(j*IOT_OFVAL_LEN+1:(j+1)*IOT_OFVAL_LEN), strfmt) &
                  (nodenames(i),i=dispnames(j+1)+1, &
                  dispnames(j+1)+lengthnames(j+1))
!              should not have IOT_OFVAL_LEN**+10** but BUGGY
               write(line(j*IOT_OFVAL_LEN+1+lengthnames(j+1): &
                  (j+1)*IOT_OFVAL_LEN+10), charfmt) &
                  repeat(" ",IOT_OFVAL_LEN-lengthnames(j+1))
            else
               write(line(j*IOT_OFVAL_LEN+1:(j+1)*IOT_OFVAL_LEN), strfmt) &
                  (nodenames(i),i=dispnames(j+1)+1, &
                  dispnames(j+1)+IOT_OFVAL_LEN)
            end if
         end do
         write (statfile, linefmt) &
            sharedkey(IOT_STAT_PROCNAME)//repeat(" ",IOT_OFKEY_LEN), &
            line(1:IOT_OFVAL_LEN*nprocs)
!        write tick resolution
         tick = mpi_wtick()
         write(line(1:IOT_OFVAL_LEN), realfmt) tick
         write(statfile, linefmt) &
              sharedkey(IOT_STAT_WTICK)//repeat(" ",IOT_OFKEY_LEN), &
              line(1:IOT_OFVAL_LEN)
!        write start and finish timestamps and nodenames
         write(statfile, linefmt) &
              sharedkey(IOT_STAT_START)//repeat(" ",IOT_OFKEY_LEN), &
              trim(starttime)

!        blank line
         write(statfile,*) 
         write(line,"(a)") "## Test input arguments " // repeat("#", 50)
         write(statfile,"(a)") line(1:70)
!        blank line
         write(statfile,*) 
!        write the number of processes here
         write(line(1:IOT_OFVAL_LEN), intfmt) nprocs
         write(statfile, linefmt) &
              sharedkey(IOT_STAT_NPROCS)//repeat(" ",IOT_OFKEY_LEN), &
              line(1:IOT_OFVAL_LEN)
!        write test input parameters to file
         do i = 1, IOT_NUM_KEYS
            if (len(ios(i)) /= 0) then
               write(statfile, linefmt) &
                    iokey(i)//repeat(" ",IOT_OFKEY_LEN), &
                    ios(i)%s(1:len(ios(i)))
            end if
         end do
      end if

!     deallocate space
      if (node == 0) then
         deallocate(nodenames)
         deallocate(lengthnames)
         deallocate(dispnames)
      end if

      ierr = IOT_SUCCESS
      return
      end subroutine

!-------------------------------------------------------------------
!     write statistics information to file using Fortran IO
      subroutine write_stat2(comm, node, nprocs, statfile, &
           datafilename, info, statkey, ierr)
      use iot_defs
      use iot_string
      use iot_key
      implicit none

      integer :: comm, node, nprocs, statfile, info, ierr
      character(len=*) :: datafilename
      character(len=*),dimension(*) :: statkey
      integer :: i, j, cnt
      character(len=IOT_FORMAT_LEN) :: intfmt, realfmt
      character(len=IOT_FORMAT_LEN) :: charfmt, strfmt, keyfmt, linefmt
      character(IOT_MAX_LINE_LEN) :: line
      integer,dimension(:),pointer :: key
      real(kind=8),dimension(:),pointer :: val
      integer,dimension(:),pointer :: type
      real(kind=8) :: tick
      integer :: ierra, ierrb, ierrc

      if (node == 0) then
!        construct format specifiers
         write (realfmt,*) "(e",IOT_OFVAL_LEN,".",IOT_OFVAL_DP,")"
         write (intfmt,*) "(i",IOT_OFVAL_LEN,")"
         write (charfmt,*) "(a",IOT_OFVAL_LEN,")"
         write (strfmt,*) "(",IOT_OFVAL_LEN,"a1)"
         write (keyfmt,*) "(a",IOT_OFKEY_LEN,")"
         write (linefmt,*) "(a",IOT_OFKEY_LEN,",a)"
      end if

      cnt = sidx-1
      sidx = 1

!     allocate space on root for output data from nodes
      if (node == 0) then
         allocate(key(cnt*nprocs),stat=ierra)
         allocate(val(cnt*nprocs),stat=ierrb)
         allocate(type(cnt*nprocs),stat=ierrc)
         if ((ierra /= 0).or.(ierrb /= 0).or.(ierrc /= 0)) then
            ierr = IOT_ERR_ALLOC
            call mpi_abort(comm, ierr, ierr)
         end if
      end if


!     gather output statistics structures to root
      call mpi_gather(skey, cnt, MPI_INTEGER, &
           key(cnt*node+1), cnt, MPI_INTEGER, 0, comm, ierr)
      call mpi_gather(sval, cnt, MPI_DOUBLE_PRECISION, &
           val(cnt*node+1), cnt, MPI_DOUBLE_PRECISION, 0, comm, ierr)
      call mpi_gather(stype, cnt, MPI_INTEGER, &
           type(cnt*node+1), cnt, MPI_INTEGER, 0, comm, ierr)

      if (node == 0) then
!        blank line
         write(statfile,*) 
!        write comment separator to stat file
         write(line,"(a)") "## Test output information " // repeat("#", 50)
         write(statfile,"(a)") line(1:70)
!        blank line
         write(statfile,*) 
!        write timings to file
         do i = 1, cnt
            if (type(i) == IOT_REAL) then
               do j = 0, nprocs-1
                  if (val(i+j*cnt) /= IOT_STAT_EMPTY) then
                     write(line(j*IOT_OFVAL_LEN+1:(j+1)*IOT_OFVAL_LEN), &
                          realfmt) val(i+j*cnt)
                  else
                     write(line(j*IOT_OFVAL_LEN+1:(j+1)*IOT_OFVAL_LEN), &
                          charfmt) repeat(" ",IOT_OFVAL_LEN)
                  end if
               end do
               write (statfile, linefmt) &
                    statkey(key(i))//repeat(" ",IOT_OFKEY_LEN), &
                    line(1:IOT_OFVAL_LEN*nprocs)
            else if (type(i) == IOT_INTEGER) then
               do j = 0, nprocs-1
                  if (val(i+j*cnt) /= IOT_STAT_EMPTY) then
                     write(line(j*IOT_OFVAL_LEN+1:(j+1)*IOT_OFVAL_LEN), &
                          intfmt) int(val(i+j*cnt),8)
                  else
                     write(line(j*IOT_OFVAL_LEN+1:(j+1)*IOT_OFVAL_LEN), &
                          charfmt) repeat(" ",IOT_OFVAL_LEN)
                  end if
               end do
               write (statfile, linefmt) &
                    statkey(key(i))//repeat(" ",IOT_OFKEY_LEN), &
                    line(1:IOT_OFVAL_LEN*nprocs)
            else if (type(i) == IOT_CHARACTER) then
            end if
         end do

!        deallocate space
         deallocate(key)
         deallocate(val)
         deallocate(type)
      end if

      ierr = IOT_SUCCESS
      return
      end subroutine

!-------------------------------------------------------------------
      subroutine time_stamp(node, statfile, ierr)
      use iot_defs
      use iot_string
      use iot_key
      implicit none

      integer :: node, statfile
      integer :: ierr
      character(len=IOT_MAX_DATE_AND_TIME_LEN) :: time
      character(len=IOT_FORMAT_LEN) :: linefmt
      character(IOT_MAX_LINE_LEN) :: line

      if (node == 0) then
!        construct format specifiers
         write (linefmt,*) "(a",IOT_OFKEY_LEN,",a)"
!        get time
         call date_and_time(time(1:8), time(10:19), time(21:25))
         time(9:9) = " "
         time(20:20) = " "
!        write comment separator to stat file
         write(line,"(a)") "## Time stamp " // repeat("#", 50)
         write(statfile,"(a)") line(1:70)
!        write time to stat file
         write(statfile, linefmt) &
            sharedkey(IOT_STAT_TIMESTAMP)//repeat(" ",IOT_OFKEY_LEN), &
            trim(time)
!        blank line
         write(statfile,*) 
      end if
      ierr = IOT_SUCCESS
      return
      end subroutine

!-------------------------------------------------------------------
!     write statistics information to file using Fortran IO
      subroutine write_stat(comm, node, nprocs, statfile, &
           datafilename, info, statkey, ierr)
      use iot_defs
      use iot_string
      use iot_key
      implicit none

      integer :: comm, node, nprocs, statfile, info, ierr
      character(len=*) :: datafilename
      character(len=*),dimension(*) :: statkey
      integer :: i, j, cnt
      character(len=IOT_FORMAT_LEN) :: intfmt, realfmt
      character(len=IOT_FORMAT_LEN) :: charfmt, strfmt, keyfmt, linefmt
      character(IOT_MAX_LINE_LEN) :: line
      integer,dimension(:),pointer :: key
      real(kind=8),dimension(:),pointer :: val
      integer,dimension(:),pointer :: type
      real(kind=8) :: tick
      integer :: ierra, ierrb, ierrc

      if (node == 0) then
!        construct format specifiers
         write (realfmt,*) "(e",IOT_OFVAL_LEN,".",IOT_OFVAL_DP,")"
         write (intfmt,*) "(i",IOT_OFVAL_LEN,")"
         write (charfmt,*) "(a",IOT_OFVAL_LEN,")"
         write (strfmt,*) "(",IOT_OFVAL_LEN,"a1)"
         write (keyfmt,*) "(a",IOT_OFKEY_LEN,")"
         write (linefmt,*) "(a",IOT_OFKEY_LEN,",a)"
      end if

      cnt = sidx-1

!     reset stat pointer 
      sidx = 1

!     allocate space on root for output data from nodes
      if (node == 0) then
         allocate(key(cnt*nprocs),stat=ierra)
         allocate(val(cnt*nprocs),stat=ierrb)
         allocate(type(cnt*nprocs),stat=ierrc)
         if ((ierra /= 0).or.(ierrb /= 0).or.(ierrc /= 0)) then
            ierr = IOT_ERR_ALLOC
            call mpi_abort(comm, ierr, ierr)
         end if
      end if

!     gather output statistics structures to root
      call mpi_gather(skey, cnt, MPI_INTEGER, &
           key(cnt*node+1), cnt, MPI_INTEGER, 0, comm, ierr)
      call mpi_gather(sval, cnt, MPI_DOUBLE_PRECISION, &
           val(cnt*node+1), cnt, MPI_DOUBLE_PRECISION, 0, comm, ierr)
      call mpi_gather(stype, cnt, MPI_INTEGER, &
           type(cnt*node+1), cnt, MPI_INTEGER, 0, comm, ierr)

      if (node == 0) then
!        blank line
         write(statfile,*) 
!        write comment separator to stat file
         write(line,"(a)") "## Test output information " // repeat("#", 50)
         write(statfile,"(a)") line(1:70)
!        blank line
         write(statfile,*) 
!        write timings to file
         do i = 1, cnt
            if (type(i) == IOT_REAL) then
               do j = 0, nprocs-1
                  if (val(i+j*cnt) /= IOT_STAT_EMPTY) then
                     write(line(j*IOT_OFVAL_LEN+1:(j+1)*IOT_OFVAL_LEN), &
                          realfmt) val(i+j*cnt)
                  else
                     write(line(j*IOT_OFVAL_LEN+1:(j+1)*IOT_OFVAL_LEN), &
                          charfmt) repeat(" ",IOT_OFVAL_LEN)
                  end if
               end do
               write (statfile, linefmt) &
                    statkey(key(i))//repeat(" ",IOT_OFKEY_LEN), &
                    line(1:IOT_OFVAL_LEN*nprocs)
            else if (type(i) == IOT_INTEGER) then
               do j = 0, nprocs-1
                  if (val(i+j*cnt) /= IOT_STAT_EMPTY) then
                     write(line(j*IOT_OFVAL_LEN+1:(j+1)*IOT_OFVAL_LEN), &
                          intfmt) int(val(i+j*cnt),8)
                  else
                     write(line(j*IOT_OFVAL_LEN+1:(j+1)*IOT_OFVAL_LEN), &
                          charfmt) repeat(" ",IOT_OFVAL_LEN)
                  end if
               end do
               write (statfile, linefmt) &
                    statkey(key(i))//repeat(" ",IOT_OFKEY_LEN), &
                    line(1:IOT_OFVAL_LEN*nprocs)
            else if (type(i) == IOT_CHARACTER) then
            end if
         end do
      end if

      ierr = IOT_SUCCESS
      return
      end subroutine

!-------------------------------------------------------------------
!     write input keys to file using Fortran IO
      subroutine write_keys(comm, node, nprocs, statfile, &
           datafilename, info, statkey, ierr)
      use iot_defs
      use iot_string
      use iot_key
      implicit none

      integer :: comm, node, nprocs, statfile, info, ierr
      character(len=*) :: datafilename
      character(len=*),dimension(*) :: statkey
      character(len=1),dimension(:),allocatable :: nodenames
      integer,dimension(:),allocatable :: dispnames, lengthnames
      integer :: i, j, cnt
      character(len=IOT_FORMAT_LEN) :: intfmt, realfmt
      character(len=IOT_FORMAT_LEN) :: charfmt, strfmt, keyfmt, linefmt
      character(IOT_MAX_LINE_LEN) :: line
      integer,dimension(:),pointer :: key
      real(kind=8),dimension(:),pointer :: val
      integer,dimension(:),pointer :: type
      real(kind=8) :: tick

      if (node == 0) then
!        construct format specifiers
         write (realfmt,*) "(e",IOT_OFVAL_LEN,".",IOT_OFVAL_DP,")"
         write (intfmt,*) "(i",IOT_OFVAL_LEN,")"
         write (charfmt,*) "(a",IOT_OFVAL_LEN,")"
         write (strfmt,*) "(",IOT_OFVAL_LEN,"a1)"
         write (keyfmt,*) "(a",IOT_OFKEY_LEN,")"
         write (linefmt,*) "(a",IOT_OFKEY_LEN,",a)"
      end if

!     find nodename
      call mpi_get_processor_name(nodename, lnodename, ierr)
!     gather process names to root
      if (node == 0) then
         allocate(dispnames(nprocs))
         allocate(lengthnames(nprocs))
      end if
      call mpi_gather(lnodename, 1, MPI_INTEGER, &
           lengthnames, 1, MPI_INTEGER, 0, comm, ierr)
      if (node == 0) then
         dispnames(1) = 0
         do i = 2, nprocs
            dispnames(i) = dispnames(i-1)+lengthnames(i-1)
         end do
         allocate(nodenames(dispnames(nprocs)+lengthnames(nprocs)))
      end if
      call mpi_gatherv(nodename, lnodename, MPI_CHARACTER, &
        nodenames, lengthnames, dispnames, MPI_CHARACTER, 0, comm, ierr)
      if (node == 0) then
!        write comment separator to stat file
         write(line,"(a)") "## General run information " // repeat("#", 50)
         write(statfile,"(a)") line(1:70)
!        blank line
         write(statfile,*) 
!        write process names to file
         do j = 0, nprocs-1
            if (lengthnames(j+1) <= IOT_OFVAL_LEN) then
               write(line(j*IOT_OFVAL_LEN+1:(j+1)*IOT_OFVAL_LEN), strfmt) &
                  (nodenames(i),i=dispnames(j+1)+1, &
                  dispnames(j+1)+lengthnames(j+1))
!              should not have IOT_OFVAL_LEN**+10** but BUGGY
               write(line(j*IOT_OFVAL_LEN+1+lengthnames(j+1): &
                  (j+1)*IOT_OFVAL_LEN+10), charfmt) &
                  repeat(" ",IOT_OFVAL_LEN-lengthnames(j+1))
            else
               write(line(j*IOT_OFVAL_LEN+1:(j+1)*IOT_OFVAL_LEN), strfmt) &
                  (nodenames(i),i=dispnames(j+1)+1, &
                  dispnames(j+1)+IOT_OFVAL_LEN)
            end if
         end do
         write (statfile, linefmt) &
            sharedkey(IOT_STAT_PROCNAME)//repeat(" ",IOT_OFKEY_LEN), &
            line(1:IOT_OFVAL_LEN*nprocs)
!        write tick resolution
         tick = mpi_wtick()
         write(line(1:IOT_OFVAL_LEN), realfmt) tick
         write(statfile, linefmt) &
              sharedkey(IOT_STAT_WTICK)//repeat(" ",IOT_OFKEY_LEN), &
              line(1:IOT_OFVAL_LEN)
!        blank line
         write(statfile,*) 
         write(line,"(a)") "## Test input arguments " // repeat("#", 50)
         write(statfile,"(a)") line(1:70)
!        blank line
         write(statfile,*) 
!        write the number of processes here
         write(line(1:IOT_OFVAL_LEN), intfmt) nprocs
         write(statfile, linefmt) &
              sharedkey(IOT_STAT_NPROCS)//repeat(" ",IOT_OFKEY_LEN), &
              line(1:IOT_OFVAL_LEN)
!        write test input parameters to file
         do i = 1, IOT_NUM_KEYS
            if (len(ios(i)) /= 0) then
               write(statfile, linefmt) &
                    iokey(i)//repeat(" ",IOT_OFKEY_LEN), &
                    ios(i)%s(1:len(ios(i)))
            end if
         end do
      end if

!     deallocate space
      if (node == 0) then
         deallocate(nodenames)
         deallocate(lengthnames)
         deallocate(dispnames)
      end if

      ierr = IOT_SUCCESS
      return
      end subroutine

!-------------------------------------------------------------------
      subroutine alloc_stat(statsize, ierr)
      use iot_defs
      use iot_string
      use iot_key
      implicit none

      integer :: statsize, ierr
      integer :: ierra, ierrb, ierrc

!     allocate statistics arrays
      maxstatsize = statsize
      allocate(sval(maxstatsize),stat=ierra)
      allocate(skey(maxstatsize),stat=ierrb)
      allocate(stype(maxstatsize),stat=ierrc)
      if ((ierra /= 0).or.(ierrb /= 0).or.(ierrc /= 0)) then
         ierr = IOT_ERR_ALLOC
         return
      end if
!     initialise pointer to empty stat array
      sidx = 1
      ierr = IOT_SUCCESS
      return
      end subroutine

!-------------------------------------------------------------------
      subroutine free_stat(ierr)
      use iot_defs
      use iot_string
      use iot_key
      implicit none

      integer :: ierr

!     deallocate statistics arrays
      deallocate(sval,skey,stype)
      ierr = IOT_SUCCESS
      return
      end subroutine

      end module iot_file




