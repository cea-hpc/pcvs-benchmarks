!~ FAIRE APPEL A UNE FONCTION C !		
		subroutine mpc_user_main
        
        include 'mpif.h'
        
!      	Variables globales
        integer table
        save table
!~         common /comon/ table
        integer ierror
		integer rank
		
        !$hls node (table)
        
        call mpi_init(ierror)
        call mpi_comm_size(MPI_COMM_WORLD, sizes, ierror)
        call mpi_comm_rank(MPI_COMM_WORLD, rank, ierror)

        table = rank
        call mpi_barrier(MPI_COMM_WORLD, ierror)
		
		if(rank .ne. table) then
			write(6,*) 'HLS NODE FAILURE'
		endif
        
        if(rank .eq. 0) then
             write(6,*) 'HLS NODE SUCCESS'
        endif
		call mpi_finalize(ierror)
        end
