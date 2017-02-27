C     Checking function for one type
      subroutine check_type_size(type_test, my_size)
      integer :: typeSize, type_test, my_size
      typeSize = 0
      
      call MPI_TYPE_SIZE(type_test, typeSize, ierr)
      if(typeSize .ne. my_size) then
              print *,typeSize,'not equal to ',my_size,' by type = ',
     $                  type_test,' -> ABORT'
             call abort
      endif
      return
      end

C     Checking function for all types
      subroutine check_all_types_size(tabType, NBTYPES)
      integer :: NBTYPES
      integer :: tabType(NBTYPES),i, tabSize(NBTYPES)

      tabSize = (/ 1,2,4,8,4,8,16,1,2,4,8,4,8,16 /)

      do 1 i = 1, NBTYPES
        call check_type_size(tabType(i),tabSize(i))
1     continue
      return
      end

C     User Main
      subroutine mpc_user_main
      include 'mpif.h'

C     Declaration
      integer :: i, j, ierr, rank, typeSize, NBELEM, NBTYPES
C     Constants
      PARAMETER(NBELEM=16*8, NBTYPES=14)
      integer*1 buffer(NBELEM), STATUS(MPI_STATUS_SIZE)
      integer :: tabType(NBTYPES)
      
      tabType = (/ MPI_INTEGER1, MPI_INTEGER2, MPI_INTEGER4,MPI_INTEGER8
     $ ,MPI_REAL4, MPI_REAL8, MPI_REAL16, MPI_INTEGER1, MPI_INTEGER2 
     $ ,MPI_INTEGER4, MPI_INTEGER8, MPI_REAL4, MPI_REAL8 
     $ ,MPI_REAL16 /)
      
C     MPI Initialization
      call MPI_INIT(ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
      call check_all_types_size(tabType, NBTYPES)

      do 4 i = 1, NBTYPES
            if (rank == 0) then
C                 tab Initialization
                  do 5 j = 1, NBELEM
                        buffer(j) = 127
5                 continue
                  call MPI_SEND(buffer,4,tabType(i),1,1,MPI_COMM_WORLD
     $                 ,ierr)
            else
                  print *, 'Checking type ', i,'/14     
     $                    Type ID = ',tabType(i)
C                 tab Initialization
                  do 6 j = 1, NBELEM
                        buffer(j) = 0
6                 continue
                  
                  call MPI_RECV(buffer,4, tabType(i),0,1,MPI_COMM_WORLD,
     $                 STATUS,ierr)
                  call MPI_TYPE_SIZE(tabType(i), typeSize, ierr)
                  
                  do 7 j = 1,4*typeSize 
                        if(buffer(j) .ne. 127) then
                             print *,'Arrays doesn''t equals -> ABORT'
                              call abort
                        end if
7                 continue
                  
                  if(buffer((4*typeSize)+1) .ne. 0) then
                        print *,'Datas shouldn''t be here -> ABORT'
                        call abort
                  end if
            end if
4     continue
       
      call MPI_FINALIZE(ierr)
       
      if(rank == 0) then
            print *, 'ALL PASSED !!'
      end if
      end subroutine mpc_user_main
