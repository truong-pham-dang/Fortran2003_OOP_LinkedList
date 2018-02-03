!
! Demonstration program.
!
program demonstration
  use myitem_list_m
  implicit none
  type(anylist), target :: list
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
  call delete(p)
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
end program
