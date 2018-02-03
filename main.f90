module anylist_m
  !
  ! Module for a list type that can contain items with any scalar value.
  ! Values are copied into the list items.
  !
  ! A list item can be in at most one list at a time.
  !
  implicit none
  private
  public :: anylist, anyitem, newitem
  !
  ! type(anylist) is the list header type.
  !
  type anylist
    class(anyitem), pointer, private :: firstptr => null()
  contains
    procedure, non_overridable :: append
    procedure, non_overridable :: count_list
    procedure, non_overridable :: delete_list
    procedure, non_overridable :: first
    procedure, non_overridable :: last
    procedure, non_overridable :: prepend
    procedure, non_overridable :: print_list
  end type
  !
  ! type(anyitem) is the list item type.
  ! These are allocated by newitem.
  !
  type anyitem
    class(*), allocatable            :: value
    class(anyitem), pointer, private :: nextptr => null(), prevptr => null()
    class(anylist), pointer, private :: upptr => null()
  contains
    procedure, non_overridable :: change
    procedure, non_overridable :: delete
    procedure, non_overridable :: list
    procedure, non_overridable :: next
    procedure, non_overridable :: prev
    procedure                  :: print
    procedure, non_overridable :: remove
  end type

contains
  !
  ! Create a new (orphaned) list item.
  !
  function newitem(something)
    class(*), intent(in)    :: something
    class(anyitem), pointer :: newitem
    allocate (newitem)
    allocate (newitem%value, source=something)
    newitem%prevptr => newitem
  end function
  !
  ! Append an item to a list.
  !
  subroutine append(list, item)
    class(anylist), intent(inout), target :: list
    class(anyitem), target                :: item
    class(anyitem), pointer               :: last
    if (associated(item%upptr)) call remove(item)
    item%upptr => list
    if (associated(list%firstptr)) then
      last => list%firstptr%prevptr
      last%nextptr => item
      item%prevptr => last
      list%firstptr%prevptr => item
    else
      list%firstptr => item
      item%prevptr => item
    end if
  end subroutine
  !
  ! Count how many items there are in a list.
  !
  integer function count_list(list)
    class(anylist), intent(in) :: list
    class(anyitem), pointer :: p
    count_list = 0
    p => list%firstptr
    do
      if (.not.associated(p)) exit
      count_list = count_list + 1
      p => p%nextptr
    end do
  end function
  !
  ! Delete the contents of a list.
  !
  subroutine delete_list(list)
    class(anylist), intent(inout) :: list
    do
      if (.not.associated(list%firstptr)) exit
      call delete(list%firstptr)
    end do
  end subroutine
  !
  ! Return the first element of a list.
  !
  function first(list)
    class(anylist), intent(in) :: list
    class(anyitem), pointer :: first
    first => list%firstptr
  end function
  !
  ! Return the last element of a list
  !
  function last(list)
    class(anylist), intent(in) :: list
    class(anyitem), pointer :: last
    last => list%firstptr
    if (associated(last)) last => last%prevptr
  end function
  !
  ! Insert an item at the beginning of a list.
  !
  subroutine prepend(list, item)
    class(anylist), intent(inout), target :: list
    class(anyitem), target                :: item
    if (associated(item%upptr)) call remove(item)
    item%upptr => list
    if (associated(list%firstptr)) then
      item%prevptr => list%firstptr%prevptr
      item%nextptr => list%firstptr
      list%firstptr%prevptr => item
    else
      item%prevptr => item
    end if
    list%firstptr => item
  end subroutine
  !
  ! Print the items in a list.
  !
  subroutine print_list(list, show_item_numbers, show_empty_list)
    class(anylist), intent(in) :: list
    logical, intent(in), optional :: show_item_numbers, show_empty_list
    class(anyitem), pointer :: p
    integer i
    logical :: show_numbers
    if (present(show_item_numbers)) then
      show_numbers = show_item_numbers
    else
      show_numbers = .true.
    end if
    p => list%firstptr
    if (.not.associated(p)) then
      if (present(show_empty_list)) then
        if (show_empty_list) print *, 'List is empty.'
      else
        print *, 'List is empty.'
      end if
    else
      do i=1, huge(i)-1
        if (show_numbers) write (*, 1, advance='no') i
1       format(1x, 'Item ', i0, ':')
        call p%print
        p => p%nextptr
        if (.not.associated(p)) exit
      end do
    end if
  end subroutine
  !
  ! Change the value of an item.
  !
  subroutine change(item, newvalue)
    class(anyitem), intent(inout) :: item
    class(*), intent(in)          :: newvalue
    deallocate (item%value)
    allocate (item%value, source=newvalue)
  end subroutine
  !
  ! Delete an item: removes it from the list and deallocates it.
  !
  subroutine delete(item)
    class(anyitem), target  :: item
    class(anyitem), pointer :: temp
    temp => item
    call remove(item)
    deallocate (temp)
  end subroutine
  !
  ! Return the list that an item is a member of.  Null if an orphan.
  !
  function list(item)
    class(anyitem), intent(in) :: item
    class(anylist), pointer :: list
    list => item%upptr
  end function
  !
  ! Return the next item in the list.
  !
  function next(item)
    class(anyitem), intent(in) :: item
    class(anyitem), pointer :: next
    next => item%nextptr
  end function
  !
  ! Return the previous item in the list,
  ! or the last item if this one is the first.
  !
  function prev(item)
    class(anyitem), intent(in) :: item
    class(anyitem), pointer :: prev
    prev => item%prevptr
  end function
  !
  ! Print an item.  This is overridable.
  !
  subroutine print(this)
    class(anyitem), intent(in) :: this
    integer length
    select type (v=>this%value)
    type is (character(*))
      length = len(v)
      if (length>40) then
        print 1, length, v(:36)
1       format(1x, 'character(len=', i0, ') = "', a, '"...')
      else
        print *, 'character = "', v, '"'
      end if
    type is (complex)
      print *, 'complex', v
    type is (complex(kind(0d0)))
      print 2, kind(v), v
2     format(1x, 'complex(kind=', i0, ') = (', es23.16, ', ', es23.16, ')')
    type is (real(kind(0d0)))
      print 3, kind(v), v
3     format(1x, 'real(kind=', i0, ') = ', es23.16)
    type is (integer)
      print *, 'integer = ', v
    type is (real)
      print *, 'real = ', v
    type is (logical)
      print *, 'logical = ', v
    class default
      print *, 'unrecognised item type - cannot display value'
    end select
  end subroutine
  !
  ! Remove an item from a list (but keep it and its value).
  !
  subroutine remove(item)
    class(anyitem), intent(inout), target :: item
    class(anylist), pointer :: list
    list => item%upptr
    if (associated(list)) then
      if (associated(item%prevptr, item)) then
        ! Single item in list.
        nullify(list%firstptr)
      else if (.not.associated(item%nextptr)) then
        ! Last item in list.
        list%firstptr%prevptr => item%prevptr
        nullify(item%prevptr%nextptr)
      else if (associated(list%firstptr, item)) then
        ! First item in list.
        list%firstptr => item%nextptr         ! first = next.
        item%prevptr%prevptr => item%nextptr  ! last%prev = item%next.
        item%nextptr%prevptr => item%prevptr  ! next%prev = last.
      else
        item%prevptr%nextptr => item%nextptr  ! last%next = item%next.
        item%nextptr%prevptr => item%prevptr  ! next%prev = item%last.
      end if
      item%prevptr => item
    end if
    nullify(item%upptr)
  end subroutine
end module
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
! Demonstration program.
!
program demonstration
  use myitem_list_m
  implicit none
  type(anylist) :: list
  class(anyitem), pointer :: p
  !
  ! First demonstrate the most basic workings of a list.
  print *, 'The initial list has', list%count_list(), 'items.'
  call list%append(newitem(17))
  print *, 'The list now has', list%count_list(), 'items.'
  call list%append(newitem('world'))
  print *, 'The list now has', list%count_list(), 'items.'
  call list%prepend(newitem('hello'))
  print *, 'The list now has', list%count_list(), 'items.'
  call list%append(newitem(2.25))
  print *, 'The list now has', list%count_list(), 'items.'
  write (*, '(1x, a)', advance='no') 'The first element is: '
  p => list%first()
  call p%print
  write (*, '(1x, a)', advance='no') 'The last element is: '
  p => list%last()
  call p%print
  print *, 'After deleting the last element, the list contents are:'
  call p%delete
  call list%print_list
  !
  ! Now delete the old list and make a new one,
  ! with some values from myitem_list_m.
  !
  call list%delete_list
  call list%append(new_myitem('The next value is one third.'))
  call list%append(new_myitem(rational(1,3)))
  call list%append(new_myitem('Where did this number come from?'))
  call list%append(new_myitem(rational(173,13)))
  print *, 'The contents of our new list are:'
  call list%print_list
  !
  ! Now test some of the other procedures, just to prove they work.
  !
  p => list%first()
  p => p%prev()        ! Test prev(), this will be the last item.
  call p%remove        ! Remove the last item.
  call list%prepend(p) ! Put it back, at the beginning of the list.
  p => p%next()        ! Test next(), this will be the second item,
                       ! the one with the string "...third.".
  call p%change((0,1)) ! Replace it with a complex number.
  print *, 'Revised list contents:'
  call list%print_list
  call list%prepend(p) ! Move new item to top
  print *, 'Afer moving item 2 to top, list contents:'
  call list%print_list
end program
