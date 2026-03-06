!***********************************************************************
!                        Version 1:  05/92, PFN                        *
!                                                                      *
!   RTPLNK - Evaluates the fraction of energy emitted from a frequency *
!            group for all zones based on the Plankian integral.       *
!                                                                      *
!   Input:   x      - h*nu/kT                                          *
!                                                                      *
!   Output:  EFRAC  - fraction of energy emitted in a group            *
!                                                                      *
!   Relevent Formulae:                                                 *
!                                                                      *
!      efrac(j) =                                                      *
!                                                                      *
!         15.*pi**(-4)*integral(u**3/(exp(u)-1.),  0 .lt. u .lt. x)    *
!                                                                      *
!                   = sum (ck*x**k  ,  k=3,4,5,...)  near x = 0        *
!                                                                      *
!              f(i) = efrac(j) for x = xm + h*i                        *
!                                                                      *
!***********************************************************************
      subroutine rtplnk(len,x,EFRAC)

    use kind_mod
    use constant_mod

      implicit none

!  Arguments

      integer,    intent(in)    :: len

      real(adqt), intent(in)    :: x(len)

      real(adqt), intent(inout) :: efrac(len)

!  Local

      integer    :: j,it1,ione,i57
 
      real(adqt) :: x2,pl1,t1,t2,f1,f2,f3,c3,c4,c5,c7,c9,  &
                    xmin,xmax,qq,xm,h

      real(adqt) :: f(59)

      parameter (ione=1)
      parameter (i57=57)

      parameter (c3=0.51329911d-1)
      parameter (c4=0.19248717d-1)
      parameter (c5=0.25664956d-2)
      parameter (c7=0.30553519d-4)
      parameter (c9=0.5658059d-6)

      parameter (xmin=1.9d0)
      parameter (xmax=12.d0)
      parameter (qq=0.15398973d0)
      parameter (xm=1.62d0)
      parameter (h=0.18d0)

      data f  /.144005d0,.177286d0,.212769d0,.249946d0,.288322d0, &
               .327420d0,.366798d0,.406054d0,.444830d0,.482815d0, &
               .519747d0,.555410d0,.589629d0,.622277d0,.653255d0, &
               .682506d0,.709999d0,.735729d0,.759714d0,.781988d0, &
               .802601d0,.821615d0,.839099d0,.855130d0,.869788d0, &
               .883155d0,.895317d0,.906355d0,.916351d0,.925385d0, &
               .933533d0,.940868d0,.947460d0,.953372d0,.958668d0, &
               .963403d0,.967630d0,.971399d0,.974754d0,.977738d0, &
               .980387d0,.982737d0,.984818d0,.986660d0,.988288d0, &
               .989726d0,.990994d0,.992111d0,.993095d0,.993961d0, &
               .994721d0,.995389d0,.995975d0,.996489d0,.996939d0, &
               .997333d0,.997677d0,.997978d0,.998242d0/
 
!  For each energy boundary, calculate the Planck integral (0 to x)
!  using the appropriate method for the value of x.
 
!*********************************************
!* Quadratic Interpolation (xmin < x < xmax) *
!*********************************************
 
!  First determine interpolation points;  Keep interpolation points
!  within bounds

      do j=1,len

!*******************************
!*  Power Series  (x < xmin)   *
!*******************************

        if (x(j) <= xmin) then

          x2  = x(j)*x(j)

          efrac(j) = ((((c9*( x2 )-c7)*( x2 )+c5)*x(j)-c4)*x(j)+c3)* &
                       x(j)*( x2 )

!**********************************
!* Asymptotic Formula (x > xmax)  *
!**********************************

        elseif (x(j) >= xmax) then

          t1 = log(x(j)*(six + x(j)*(three + x(j))))

!         compute the fraction of energy, protecting against roundoff

          if (t1-x(j) > log(epsilon(one)/qq)) then
             efrac(j) = one - qq*exp(t1 - x(j))
          else
             efrac(j) = one
          endif

!*******************************
!*  Interpolation              *
!*******************************

        else

          t1  = (x(j) - xm)/h

          it1 = int( t1 )
          t2  = t1 - float( it1 )

          it1 = max(it1,ione)
          it1 = min(it1,i57)
          f1  = f(it1)
          f2  = f(it1+1)
          f3  = f(it1+2)

          efrac(j) = half*( t2 - one )*(f1*( t2 - two ) +  & 
                           f3*t2) - f2*( t2 - two)*t2

        endif

      enddo

 
      return
      end subroutine rtplnk


