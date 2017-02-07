        
        Program test
        implicit none
        integer count
        parameter (count = 10)
        integer, dimension(2,count) :: ref, a
        integer i, k, tmp, rank
        integer :: OMP_GET_THREAD_NUM
        
        do k=1, count
            a(1,k) = 0.0
            a(2,k) = 0.0
            ref(1,k) = 55
            ref(2,k) =  55
        end do

        do k=1, count
            !$OMP PARALLEL DO PRIVATE(tmp,rank) REDUCTION(+:a)
            do i=1, count
                rank = OMP_GET_THREAD_NUM()
                tmp = i
                a(1,k) = a(1,k) + tmp
                a(2,k) = a(2,k) + tmp
                write (*,*) "a(1", k, ") = ", a(1,k)
                write (*,*) "a(2", k, ") = ", a(2,k)
            end do
            !$OMP END PARALLEL DO
            write (*,*) "a(1", k, ") = ", a(1,k)
            write (*,*) "a(2", k, ") = ", a(2,k)
        end do

        do k=1, count
            if (ref(1,k) .ne. a(1,k)) then
                write(*,*) "FAILED: a(1,", k, ")=", a(1,k)
                exit
            else
                write(*,*) "SUCCESS"
            end if
            if (ref(2,k) .ne. a(2,k)) then
                write(*,*) "FAILED: a(2,", k, ")=", a(2,k)
                exit
            else
                write(*,*) "SUCCESS"
            end if
        end do                                                  
        end
