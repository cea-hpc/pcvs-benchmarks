!     $Id: iot_lowlevel.f,v 1.6 1998/01/21 18:26:23 djl Exp $
!
!     IOT: MPI IO Test Suite
!
!     Copyright Fujitsu Ltd. 1997
!
!     Function:
!     iot_lowlevel
!     Release 1.0
!
!     Description:
!     Lowlevel test program for test: single, multiple
!
!     Interface:
!     
!
!     External components:
!     
!-------------------------------------------------------------------

!     General program wrapper for IO tests
!
!     By changing the individual tests called in the select statement
!     test programs can be compiled which can perform any combination of
!     individual tests from a single test to all the tests.
!
!     This may be useful if certain test codes don't compile on a
!     particular architecture. All the other tests can still be compiled
!     and run independently.

!     program naming convention:
!        iot_<TEST>
!     where:
!        <TEST> is individual test name or:
!            all: all tests
!            low: all low-level tests
!            kernel: all kernel tests
!            app: all application tests

      subroutine mpc_user_main
      use iotest
      implicit none

!     files
!     userfile: user input file containing data file names,
!               file hints and test parameters
!               read by root process
!     datafile: for reading/writing test data 
!               delete after successful run
!     statfile: for recording useful performance information
!               permanent; not deleted
!               written by root process
      character(len=*),parameter :: userfilename = &
         "../iotparams.in"
      character(len=IOT_MAX_STRING_LEN) :: statfilename
      character(len=IOT_MAX_STRING_LEN) :: datafilename
      integer :: userfile, statfile
      integer :: info
      integer :: test
      integer :: eof, ierr, errfile, uferr, sferr
      integer :: node, nprocs
      integer :: comm, lname, ltestname, lfilename

!     general initialization
      call mpi_init(ierr)
      comm = MPI_COMM_WORLD
      call mpi_comm_rank(comm, node, ierr)
      call mpi_comm_size(comm, nprocs, ierr)
      call iot_init(ierr)

      write(fout,*) "Node (MPI) ", node, "starting iot_lowlevel"

      
      if (node == 0) then

!       open user input file
         userfile=IOT_USERFILE_UNIT
         open(unit=userfile, file=userfilename, status="old", &
              iostat=uferr)
         if (uferr /= 0) then
            write (fout,*) "ERROR: Error opening test parameter file ", &
                 userfilename
            call mpi_abort(comm, ierr, ierr)
         end if

!       get statistics filename from user input file
         call get_test_strings(userfile, eof, ierr)
         if (ierr /= IOT_SUCCESS) then
            write (fout,*) "ERROR: Error reading statistics filename", &
              " from test parameter file"
            call mpi_abort(comm, ierr, ierr)
         end if
         statfilename = ios(IOT_KEY_TIMINGSFILENAME)
         lname = len_trim(statfilename)

!       open statistics output file
         statfile=IOT_STATFILE_UNIT
         open(unit=statfile, file=statfilename(1:lname), &
              status="replace", iostat=sferr)
         if (sferr /= 0) then
            write (fout,*) "ERROR: Error opening statistics output file ", &
                 statfilename(1:lname)
            call mpi_abort(comm, ierr, ierr)
         end if
      end if


!    repeat over user tests
      eof = IOT_SUCCESS
      do 

!       get test information
         if (node == 0) then
            if (eof == IOT_ERR_EOF) then
               errfile = eof
            else
               call get_test_strings(userfile, eof, errfile)
            end if
         end if
!       broadcast error condition
         call mpi_bcast(errfile, 1, MPI_INTEGER, 0, comm, ierr)
!       exit loop if no keyword strings
         if (errfile /= IOT_SUCCESS) exit

!       broadcast test information to all processes
         call broadcast_data(comm, node, ierr)

!       get test identifier
         call check_testname(test, ierr)
         ltestname = len(ios(IOT_KEY_TESTNAME))
!       cycle loop if invalid name given
         if (ierr /= IOT_SUCCESS) then
            if (node == 0) then
               write (fout,*) "ERROR: Invalid test program name ", &
                 ios(IOT_KEY_TESTNAME)%s(1:ltestname)
            end if
            cycle
         end if


!         write(6,*) "test = ", test


!       get filename and info structure
         call get_file_info(datafilename, lname, info, ierr)
         lfilename = len(ios(IOT_KEY_FILENAME))
!       cycle loop if invalid filename or info given
         if (ierr /= IOT_SUCCESS) then
            if (node == 0) then
               write (fout,*) "ERROR: Invalid data file name ", &
                 ios(IOT_KEY_FILENAME)%s(1:lfilename)
            end if
            cycle
         end if


         write (fout,*) "Node (MPI) ",node," starting ", &
           ios(IOT_KEY_TESTNAME)%s(1:ltestname)
         call mpi_barrier(comm, ierr)

!       pass control to specific test
         select case (test)
         case (IOT_TEST_SINUNIX)
            call iot_wrap_sinunix(comm, node, nprocs, &
              datafilename(1:lfilename), info, statfile, ierr)
         case (IOT_TEST_SINGLE)
            call iot_wrap_single(comm, node, nprocs, &
              datafilename(1:lfilename), info, statfile, ierr)
         case (IOT_TEST_MULTIPLE)
            call iot_wrap_multiple(comm, node, nprocs, &
              datafilename(1:lfilename), info, statfile, ierr)
         case default
            if (node == 0) then
               write(fout,*) "ERROR: Program not compiled with test ", &
                 ios(IOT_KEY_TESTNAME)%s(1:ltestname)
            end if
         end select

         if (ierr == IOT_SUCCESS) then
            write (fout,*) "Node (MPI) ",node," completing ", &
              ios(IOT_KEY_TESTNAME)%s(1:ltestname)
         else
            write (fout,*) "ERROR: node ",node,"error in test ", &
              ios(IOT_KEY_TESTNAME)%s(1:ltestname)
         end if

         call mpi_barrier(comm, ierr)

      end do ! user test loop

!    ensure all writes are completed before closing files
      call mpi_barrier(comm, ierr)

!    close user and statistics files
      if (node == 0) then
         close(unit=statfile)
         close(unit=userfile)
      end if

      write(fout,*) "Node (MPI) ", node, "finishing iot_lowlevel"

      call mpi_finalize(ierr)

      end subroutine 



