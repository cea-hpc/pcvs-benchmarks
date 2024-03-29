        program main

        include 'mpif.h'

!          Variables globales
        integer privatize
        common /comon/ privatize

        integer ierror
        integer rank
        integer sizes
        integer irank
        integer i
        integer ith
        integer istart, istop
        integer nvals
        integer pid
        real value
        real valueRead

        character(len=21) tmp
        CHARACTER(len=21) master_pid
        character(len=128) filename
        integer omp_get_num_threads
        integer omp_get_thread_num
        call mpi_init(ierror)
        call mpi_comm_size(MPI_COMM_WORLD, sizes, ierror)
        call mpi_comm_rank(MPI_COMM_WORLD, rank, ierror)
        pid = getpid()
        call mpi_bcast(pid, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)
        nvals = 10000
        write(tmp, "(I4.4)"), rank
        write(master_pid, "(I8.8)"), pid
        filename = "file_unit_"//trim(master_pid)//"_"//trim(tmp)
        call mpi_barrier(MPI_COMM_WORLD, ierror)
        open(167, FILE=filename)
        call mpi_barrier(MPI_COMM_WORLD, ierror)
!$OMP PARALLEL
!         write(*,*), omp_get_thread_num(), " ", omp_get_num_threads()
!       Each thread one after the other in the right order
        do ith=0,omp_get_num_threads()
           if (omp_get_thread_num() .eq. ith) then
                istart = ith*(nvals/omp_get_num_threads()+1)
                istop = (ith+1)*(nvals/omp_get_num_threads())+ith
                write(*,*), 'Rank : ', rank, 'Thread : ', omp_get_thread_num(), " : ", istart, "=>", istop
                do i=istart, istop
                    write(167, *), sin(real(i)+real(rank))
                    call flush(167)
                end do
            end if
!$OMP BARRIER
        end do
!$OMP END PARALLEL
        call mpi_barrier(MPI_COMM_WORLD, ierror)
        close(167)
!       Verification
        if(rank .eq.0) then
            do irank=0,sizes-1
                write(*,*), "Checking for rank ", irank
                write(tmp, "(I4.4)"), irank
                filename = "file_unit_"//trim(master_pid)//"_"//trim(tmp)
                open(100, FILE=filename)
                do i=0, nvals
                    value = sin(real(i)+real(irank))
!                   Get rid of precision
                    write(tmp, *), value
                    read(tmp, *), value
                    read(100, *), valueRead

                    if ( value .ne. valueRead ) then
                        write(6,*) 'Something is wrong with units...'
                        write(6,*) 'Expected ', value, ' got ', valueRead, ' at index ', i, ' on file ', trim(filename)
                        call MPI_Abort(MPI_COMM_WORLD, 12, ierror)
                    end if
                end do
                close(100,  status='delete')
            end do
        end if

        call mpi_barrier(MPI_COMM_WORLD, ierror)
        call mpi_finalize(ierror)
        end
