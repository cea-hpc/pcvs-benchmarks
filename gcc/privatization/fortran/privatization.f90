!~         program main
		subroutine mpc_user_main
        
        include 'mpif.h'
        
!      	Variables globales
        integer privatize
        common /comon/ privatize

        integer ierror
        integer rank

        call mpi_init(ierror)
        call mpi_comm_size(MPI_COMM_WORLD, sizes, ierror)
        call mpi_comm_rank(MPI_COMM_WORLD, rank, ierror)

		privatize = rank;
		call mpi_barrier(MPI_COMM_WORLD, ierror)
		
		if(rank .ne. privatize) then
			write(6,*) 'PRIVATIZATION FAILURE'
			call abort()
		endif
        call mpi_barrier(MPI_COMM_WORLD, ierror)
        
        if(rank .eq. 0) then
             write(6,*) 'PRIVATIZATION SUCCESS'
        endif
        call mpi_finalize(ierror)
        end
