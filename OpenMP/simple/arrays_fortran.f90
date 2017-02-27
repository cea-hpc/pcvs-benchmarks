        
        Program test
        implicit none
        integer count
        parameter (count = 10)
        integer, dimension(count) :: ref, a
        integer i, k, tmp, rank
        integer :: OMP_GET_THREAD_NUM
        
        do k=1, count
            ref(k) = 55
            a(k) = 0.0
        end do

        do k=1, count
            !$OMP PARALLEL DO PRIVATE(tmp,rank) REDUCTION(+:a)
            do i=1, count
                rank = OMP_GET_THREAD_NUM()
                tmp = i
                a(k) = a(k) + tmp
                write (*,*) "a(", k, ",", rank, ") = ", a(k)
            end do
            !$OMP END PARALLEL DO
            write (*,*) "a(", k, ") = ", a(k)
        end do
        
        do k=1, count
            if (ref(k) .ne. a(k)) then
                write(*,*) "FAILED: a(", k, ")=", a(k)
                exit
            else
                write(*,*) "SUCCESS"
            end if
        end do
        end
