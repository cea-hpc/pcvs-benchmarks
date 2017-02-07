!~         program main
		subroutine mpc_user_main
        
        include 'mpif.h'
        
!      	Variables globales
        integer compteur_node
        save compteur_node

        integer ierror
        integer rank

        !$hls node(compteur_node)
        
        call mpi_init(ierror)
        call mpi_comm_size(MPI_COMM_WORLD, sizes, ierror)
        call mpi_comm_rank(MPI_COMM_WORLD, rank, ierror)

        !$hls barrier(compteur_node)

        !$hls single(compteur_node)
            compteur_node = compteur_node + 1 
        !$hls end single
        
        !$hls barrier(compteur_node)
        
        if(rank .eq. 0) then
!          node
            if(compteur_node .ne. 1) then
                write(6,*) 'HLS NODE SINGLE FAILURE'
                call abort() 
            else
                write(6,*) 'HLS NODE SINGLE SUCCESS'
            endif
        endif
		call mpi_finalize(ierror)
        end
