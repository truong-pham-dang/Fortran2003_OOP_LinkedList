!
! Module to demonstrate extending anyitem to handle a user-defined type.
!
module myitem_list_m
  use anylist_m
  implicit none
  type, extends(anyitem) :: myitem
  contains
    procedure :: print => myprint
  end type
  type rational
    integer :: numerator = 0
    integer :: denominator = 1
  end type
contains
  !
  ! Version of print that will handle type rational.
  !
  subroutine myprint(this)
    class(myitem), intent(in) :: this
    select type (v=>this%value)
    class is (rational)
      print *, 'rational =', v%numerator, '/', v%denominator
    class default
      call this%anyitem%print
    end select
  end subroutine
  function new_myitem(anything)
    class(*), intent(in) :: anything
    class(myitem), pointer :: new_myitem
    allocate (new_myitem)
    allocate (new_myitem%value, source=anything)
  end function
end module
!
