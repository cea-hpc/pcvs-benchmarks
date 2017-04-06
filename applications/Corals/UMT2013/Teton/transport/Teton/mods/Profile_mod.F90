! Profile Module:  Contains data structures that describe source profiles 

module Profile_mod 

  use kind_mod

  private

! public interfaces

  public construct, destruct
                                                                                 
  type, public :: Profile 

     integer              :: NumTimes          ! number of profile times 
     integer              :: NumValues         ! number of profile values 
     integer              :: NumInterpValues   ! number of interpolated values

     real(adqt)           :: Multiplier        ! profile multiplier

     character(len=8)     :: Location          ! boundary or volume
     character(len=8)     :: Type              ! profile type
     character(len=8)     :: Shape             ! profile angular shape
     character(len=8)     :: Status            ! profile on/off flag

     real(adqt), pointer  :: Times(:)          ! profile times
     real(adqt), pointer  :: Values(:)         ! profile values
     real(adqt), pointer  :: InterpValues(:)   ! interpolated values

  end type Profile 

  type(Profile), pointer, public :: ProfID

  interface construct
    module procedure Profile_ctor
  end interface

  interface destruct
    module procedure Profile_dtor
  end interface

contains

!=======================================================================
! construct interface
!=======================================================================
                                                                                   
  subroutine Profile_ctor(self,            &
                          NumTimes,        &
                          NumValues,       &
                          NumInterpValues, &
                          Multiplier,      &
                          Location,        &
                          Type,            &
                          Shape,           &
                          Times,           &
                          Values)

    implicit none

!   Passed variables

    type(Profile), intent(inout)           :: self

    integer, intent(in)                    :: NumTimes         
    integer, intent(in)                    :: NumValues       
    integer, intent(in)                    :: NumInterpValues

    real(adqt), optional, intent(in)       :: Multiplier     

    character(len=8), intent(in)           :: Location      
    character(len=8), intent(in)           :: Type         
    character(len=8), optional, intent(in) :: Shape       

    real(adqt), intent(in)                 :: Times(NumTimes)         
    real(adqt), intent(in)                 :: Values(NumValues)       

!   Set Properties

    self % NumTimes        = NumTimes
    self % NumValues       = NumValues
    self % NumInterpValues = NumInterpValues

    if (present(Multiplier)) then
      self % Multiplier = Multiplier 
    else
      self % Multiplier = 1.0_adqt
    endif

    self % Location = Location
    self % Type     = Type

    if (present(Shape)) then
      self % Shape = Shape 
    else
      self % Shape = 'iso' 
    endif

    self % Status  = 'off'

    allocate( self % Times(self % NumTimes) )
    allocate( self % Values(self % NumValues) )
    allocate( self % InterpValues(self % NumInterpValues) )

    self % Times(:)  = Times(:)
    self % Values(:) = Values(:) 


    return

  end subroutine Profile_ctor
                                                      
!=======================================================================
! destruct interface
!=======================================================================
                                                                                    
  subroutine Profile_dtor(self)


    implicit none

!   Passed variables
                                                                                     
    type(Profile),  intent(inout) :: self

!   Local


    return

  end subroutine Profile_dtor

end module Profile_mod

