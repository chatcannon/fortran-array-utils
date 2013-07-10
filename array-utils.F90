! Template file for automatic generation of the extendable arrays module

! Copyright (C) 2013 Chris Kerr
! You may use this software under the terms of the GNU General Public License (GPL)
! version 3 or, at your option, any later version

module array_utils

interface resize
  module procedure resize_int_1
  module procedure resize_real_1
  module procedure resize_dble_1
  module procedure resize_int_2
  module procedure resize_real_2
  module procedure resize_dble_2
  module procedure resize_int_3
  module procedure resize_real_3
  module procedure resize_dble_3
end interface resize

interface extend
  module procedure extend_int_1
  module procedure extend_real_1
  module procedure extend_dble_1
  module procedure extend_int_2
  module procedure extend_real_2
  module procedure extend_dble_2
  module procedure extend_int_3
  module procedure extend_real_3
  module procedure extend_dble_3
end interface extend

interface insert
  module procedure insert_int_1
  module procedure insert_real_1
  module procedure insert_dble_1
  module procedure insert_int_2
  module procedure insert_real_2
  module procedure insert_dble_2
  module procedure insert_int_3
  module procedure insert_real_3
  module procedure insert_dble_3
end interface insert


interface insert_read
  module procedure insert_read_fd_int_1
  module procedure insert_read_str_int_1
  module procedure insert_read_fd_real_1
  module procedure insert_read_str_real_1
  module procedure insert_read_fd_dble_1
  module procedure insert_read_str_dble_1
  module procedure insert_read_fd_int_2
  module procedure insert_read_str_int_2
  module procedure insert_read_fd_real_2
  module procedure insert_read_str_real_2
  module procedure insert_read_fd_dble_2
  module procedure insert_read_str_dble_2
  module procedure insert_read_fd_int_3
  module procedure insert_read_str_int_3
  module procedure insert_read_fd_real_3
  module procedure insert_read_str_real_3
  module procedure insert_read_fd_dble_3
  module procedure insert_read_str_dble_3
end interface insert_read

interface insert_row
  module procedure insert_row_int_2
  module procedure insert_row_real_2
  module procedure insert_row_dble_2
  module procedure insert_row_int_3
  module procedure insert_row_real_3
  module procedure insert_row_dble_3
end interface insert_row

interface insert_read_row
  module procedure insert_read_row_fd_int_2
  module procedure insert_read_row_str_int_2
  module procedure insert_read_row_fd_real_2
  module procedure insert_read_row_str_real_2
  module procedure insert_read_row_fd_dble_2
  module procedure insert_read_row_str_dble_2
  module procedure insert_read_row_fd_int_3
  module procedure insert_read_row_str_int_3
  module procedure insert_read_row_fd_real_3
  module procedure insert_read_row_str_real_3
  module procedure insert_read_row_fd_dble_3
  module procedure insert_read_row_str_dble_3
end interface insert_read_row

contains


pure subroutine resize_int_1 (arr, d1, fill)
  implicit none
  INTEGER, allocatable, intent(inout) :: arr(:)
  integer, intent(in) :: d1
  INTEGER, intent(in), optional :: fill

  integer :: arrshape(1)
  integer :: d1_min
  INTEGER, allocatable :: tmp(:)

  if ((d1 == 0)) then
    if(allocated(arr)) deallocate(arr)
    return
  end if

  if (.not. allocated(arr)) then
    allocate(arr(d1))
    if (present(fill)) arr = fill
    return
  end if
  
  arrshape = shape(arr)
  d1_min = min(d1, arrshape(1))

  if ((d1 /= arrshape(1))) then
    allocate(tmp(d1))
    if (present(fill)) tmp = fill
    tmp(1:d1_min) = arr(1:d1_min)
    call move_alloc(tmp, arr)
  end if

end subroutine resize_int_1

pure subroutine extend_int_1 (arr, d1, fill)
  implicit none
  INTEGER, allocatable, intent(inout) :: arr(:)
  integer, intent(in) :: d1
  INTEGER, intent(in), optional :: fill

  integer :: arrshape(1)
  integer :: d1_max

  arrshape = shape(arr)
  d1_max = max(d1, arrshape(1))
  call resize_int_1 (arr, d1_max, fill)
end subroutine extend_int_1

pure subroutine insert_int_1 (arr, d1, item, fill)
  implicit none
  INTEGER, allocatable, intent(inout) :: arr(:)
  integer, intent(in) :: d1
  INTEGER, intent(in) :: item
  INTEGER, intent(in), optional :: fill

  call extend_int_1 (arr, d1, fill)

  arr(d1) = item

end subroutine insert_int_1

subroutine insert_read_fd_int_1 (arr, d1, fd, fill)
  implicit none
  INTEGER, allocatable, intent(inout) :: arr(:)
  integer, intent(in) :: d1
  integer, intent(in) :: fd
  INTEGER, intent(in), optional :: fill

  call extend_int_1 (arr, d1, fill)

  read (fd,*) arr(d1)

end subroutine insert_read_fd_int_1

pure subroutine insert_read_str_int_1 (arr, d1, str_to_read, fill)
  implicit none
  INTEGER, allocatable, intent(inout) :: arr(:)
  integer, intent(in) :: d1
  character(len=*), intent(in) :: str_to_read
  INTEGER, intent(in), optional :: fill

  call extend_int_1 (arr, d1, fill)

  read (str_to_read,*) arr(d1)

end subroutine insert_read_str_int_1



pure subroutine resize_real_1 (arr, d1, fill)
  implicit none
  REAL, allocatable, intent(inout) :: arr(:)
  integer, intent(in) :: d1
  REAL, intent(in), optional :: fill

  integer :: arrshape(1)
  integer :: d1_min
  REAL, allocatable :: tmp(:)

  if ((d1 == 0)) then
    if(allocated(arr)) deallocate(arr)
    return
  end if

  if (.not. allocated(arr)) then
    allocate(arr(d1))
    if (present(fill)) arr = fill
    return
  end if
  
  arrshape = shape(arr)
  d1_min = min(d1, arrshape(1))

  if ((d1 /= arrshape(1))) then
    allocate(tmp(d1))
    if (present(fill)) tmp = fill
    tmp(1:d1_min) = arr(1:d1_min)
    call move_alloc(tmp, arr)
  end if

end subroutine resize_real_1

pure subroutine extend_real_1 (arr, d1, fill)
  implicit none
  REAL, allocatable, intent(inout) :: arr(:)
  integer, intent(in) :: d1
  REAL, intent(in), optional :: fill

  integer :: arrshape(1)
  integer :: d1_max

  arrshape = shape(arr)
  d1_max = max(d1, arrshape(1))
  call resize_real_1 (arr, d1_max, fill)
end subroutine extend_real_1

pure subroutine insert_real_1 (arr, d1, item, fill)
  implicit none
  REAL, allocatable, intent(inout) :: arr(:)
  integer, intent(in) :: d1
  REAL, intent(in) :: item
  REAL, intent(in), optional :: fill

  call extend_real_1 (arr, d1, fill)

  arr(d1) = item

end subroutine insert_real_1

subroutine insert_read_fd_real_1 (arr, d1, fd, fill)
  implicit none
  REAL, allocatable, intent(inout) :: arr(:)
  integer, intent(in) :: d1
  integer, intent(in) :: fd
  REAL, intent(in), optional :: fill

  call extend_real_1 (arr, d1, fill)

  read (fd,*) arr(d1)

end subroutine insert_read_fd_real_1

pure subroutine insert_read_str_real_1 (arr, d1, str_to_read, fill)
  implicit none
  REAL, allocatable, intent(inout) :: arr(:)
  integer, intent(in) :: d1
  character(len=*), intent(in) :: str_to_read
  REAL, intent(in), optional :: fill

  call extend_real_1 (arr, d1, fill)

  read (str_to_read,*) arr(d1)

end subroutine insert_read_str_real_1



pure subroutine resize_dble_1 (arr, d1, fill)
  implicit none
  DOUBLE PRECISION, allocatable, intent(inout) :: arr(:)
  integer, intent(in) :: d1
  DOUBLE PRECISION, intent(in), optional :: fill

  integer :: arrshape(1)
  integer :: d1_min
  DOUBLE PRECISION, allocatable :: tmp(:)

  if ((d1 == 0)) then
    if(allocated(arr)) deallocate(arr)
    return
  end if

  if (.not. allocated(arr)) then
    allocate(arr(d1))
    if (present(fill)) arr = fill
    return
  end if
  
  arrshape = shape(arr)
  d1_min = min(d1, arrshape(1))

  if ((d1 /= arrshape(1))) then
    allocate(tmp(d1))
    if (present(fill)) tmp = fill
    tmp(1:d1_min) = arr(1:d1_min)
    call move_alloc(tmp, arr)
  end if

end subroutine resize_dble_1

pure subroutine extend_dble_1 (arr, d1, fill)
  implicit none
  DOUBLE PRECISION, allocatable, intent(inout) :: arr(:)
  integer, intent(in) :: d1
  DOUBLE PRECISION, intent(in), optional :: fill

  integer :: arrshape(1)
  integer :: d1_max

  arrshape = shape(arr)
  d1_max = max(d1, arrshape(1))
  call resize_dble_1 (arr, d1_max, fill)
end subroutine extend_dble_1

pure subroutine insert_dble_1 (arr, d1, item, fill)
  implicit none
  DOUBLE PRECISION, allocatable, intent(inout) :: arr(:)
  integer, intent(in) :: d1
  DOUBLE PRECISION, intent(in) :: item
  DOUBLE PRECISION, intent(in), optional :: fill

  call extend_dble_1 (arr, d1, fill)

  arr(d1) = item

end subroutine insert_dble_1

subroutine insert_read_fd_dble_1 (arr, d1, fd, fill)
  implicit none
  DOUBLE PRECISION, allocatable, intent(inout) :: arr(:)
  integer, intent(in) :: d1
  integer, intent(in) :: fd
  DOUBLE PRECISION, intent(in), optional :: fill

  call extend_dble_1 (arr, d1, fill)

  read (fd,*) arr(d1)

end subroutine insert_read_fd_dble_1

pure subroutine insert_read_str_dble_1 (arr, d1, str_to_read, fill)
  implicit none
  DOUBLE PRECISION, allocatable, intent(inout) :: arr(:)
  integer, intent(in) :: d1
  character(len=*), intent(in) :: str_to_read
  DOUBLE PRECISION, intent(in), optional :: fill

  call extend_dble_1 (arr, d1, fill)

  read (str_to_read,*) arr(d1)

end subroutine insert_read_str_dble_1



pure subroutine resize_int_2 (arr, d1, d2, fill)
  implicit none
  INTEGER, allocatable, intent(inout) :: arr(:,:)
  integer, intent(in) :: d1, d2
  INTEGER, intent(in), optional :: fill

  integer :: arrshape(2)
  integer :: d1_min, d2_min
  INTEGER, allocatable :: tmp(:,:)

  if ((d1 == 0) .and. (d2 == 0)) then
    if(allocated(arr)) deallocate(arr)
    return
  end if

  if (.not. allocated(arr)) then
    allocate(arr(d1, d2))
    if (present(fill)) arr = fill
    return
  end if
  
  arrshape = shape(arr)
  d1_min = min(d1, arrshape(1))
  d2_min = min(d2, arrshape(2))

  if ((d1 /= arrshape(1)) .or. (d2 /= arrshape(2))) then
    allocate(tmp(d1, d2))
    if (present(fill)) tmp = fill
    tmp(1:d1_min, 1:d2_min) = arr(1:d1_min, 1:d2_min)
    call move_alloc(tmp, arr)
  end if

end subroutine resize_int_2

pure subroutine extend_int_2 (arr, d1, d2, fill)
  implicit none
  INTEGER, allocatable, intent(inout) :: arr(:,:)
  integer, intent(in) :: d1, d2
  INTEGER, intent(in), optional :: fill

  integer :: arrshape(2)
  integer :: d1_max, d2_max

  arrshape = shape(arr)
  d1_max = max(d1, arrshape(1))
  d2_max = max(d2, arrshape(2))
  call resize_int_2 (arr, d1_max, d2_max, fill)
end subroutine extend_int_2

pure subroutine insert_int_2 (arr, d1, d2, item, fill)
  implicit none
  INTEGER, allocatable, intent(inout) :: arr(:,:)
  integer, intent(in) :: d1, d2
  INTEGER, intent(in) :: item
  INTEGER, intent(in), optional :: fill

  call extend_int_2 (arr, d1, d2, fill)

  arr(d1, d2) = item

end subroutine insert_int_2

subroutine insert_read_fd_int_2 (arr, d1, d2, fd, fill)
  implicit none
  INTEGER, allocatable, intent(inout) :: arr(:,:)
  integer, intent(in) :: d1, d2
  integer, intent(in) :: fd
  INTEGER, intent(in), optional :: fill

  call extend_int_2 (arr, d1, d2, fill)

  read (fd,*) arr(d1, d2)

end subroutine insert_read_fd_int_2

pure subroutine insert_read_str_int_2 (arr, d1, d2, str_to_read, fill)
  implicit none
  INTEGER, allocatable, intent(inout) :: arr(:,:)
  integer, intent(in) :: d1, d2
  character(len=*), intent(in) :: str_to_read
  INTEGER, intent(in), optional :: fill

  call extend_int_2 (arr, d1, d2, fill)

  read (str_to_read,*) arr(d1, d2)

end subroutine insert_read_str_int_2

pure subroutine insert_row_int_2 (arr, d2, item, fill)
  implicit none
  INTEGER, allocatable, intent(inout) :: arr(:,:)
  integer, intent(in) :: d2
  INTEGER, intent(in) :: item(:)
  INTEGER, intent(in), optional :: fill

  call extend_int_2 (arr, size(item), d2, fill)

  arr(:,d2) = item

end subroutine insert_row_int_2

subroutine insert_read_row_fd_int_2 (arr, d1, d2, fd, fill)
  implicit none
  INTEGER, allocatable, intent(inout) :: arr(:,:)
  integer, intent(in) :: d1, d2
  integer, intent(in) :: fd
  INTEGER, intent(in), optional :: fill

  call extend_int_2 (arr, d1, d2, fill)

  read(fd, *) arr(:,d2)

end subroutine insert_read_row_fd_int_2

pure subroutine insert_read_row_str_int_2 (arr, d1, d2, str_to_read, fill)
  implicit none
  INTEGER, allocatable, intent(inout) :: arr(:,:)
  integer, intent(in) :: d1, d2
  character(len=*), intent(in) :: str_to_read
  INTEGER, intent(in), optional :: fill

  call extend_int_2 (arr, d1, d2, fill)

  read(str_to_read, *) arr(:,d2)

end subroutine insert_read_row_str_int_2


pure subroutine resize_real_2 (arr, d1, d2, fill)
  implicit none
  REAL, allocatable, intent(inout) :: arr(:,:)
  integer, intent(in) :: d1, d2
  REAL, intent(in), optional :: fill

  integer :: arrshape(2)
  integer :: d1_min, d2_min
  REAL, allocatable :: tmp(:,:)

  if ((d1 == 0) .and. (d2 == 0)) then
    if(allocated(arr)) deallocate(arr)
    return
  end if

  if (.not. allocated(arr)) then
    allocate(arr(d1, d2))
    if (present(fill)) arr = fill
    return
  end if
  
  arrshape = shape(arr)
  d1_min = min(d1, arrshape(1))
  d2_min = min(d2, arrshape(2))

  if ((d1 /= arrshape(1)) .or. (d2 /= arrshape(2))) then
    allocate(tmp(d1, d2))
    if (present(fill)) tmp = fill
    tmp(1:d1_min, 1:d2_min) = arr(1:d1_min, 1:d2_min)
    call move_alloc(tmp, arr)
  end if

end subroutine resize_real_2

pure subroutine extend_real_2 (arr, d1, d2, fill)
  implicit none
  REAL, allocatable, intent(inout) :: arr(:,:)
  integer, intent(in) :: d1, d2
  REAL, intent(in), optional :: fill

  integer :: arrshape(2)
  integer :: d1_max, d2_max

  arrshape = shape(arr)
  d1_max = max(d1, arrshape(1))
  d2_max = max(d2, arrshape(2))
  call resize_real_2 (arr, d1_max, d2_max, fill)
end subroutine extend_real_2

pure subroutine insert_real_2 (arr, d1, d2, item, fill)
  implicit none
  REAL, allocatable, intent(inout) :: arr(:,:)
  integer, intent(in) :: d1, d2
  REAL, intent(in) :: item
  REAL, intent(in), optional :: fill

  call extend_real_2 (arr, d1, d2, fill)

  arr(d1, d2) = item

end subroutine insert_real_2

subroutine insert_read_fd_real_2 (arr, d1, d2, fd, fill)
  implicit none
  REAL, allocatable, intent(inout) :: arr(:,:)
  integer, intent(in) :: d1, d2
  integer, intent(in) :: fd
  REAL, intent(in), optional :: fill

  call extend_real_2 (arr, d1, d2, fill)

  read (fd,*) arr(d1, d2)

end subroutine insert_read_fd_real_2

pure subroutine insert_read_str_real_2 (arr, d1, d2, str_to_read, fill)
  implicit none
  REAL, allocatable, intent(inout) :: arr(:,:)
  integer, intent(in) :: d1, d2
  character(len=*), intent(in) :: str_to_read
  REAL, intent(in), optional :: fill

  call extend_real_2 (arr, d1, d2, fill)

  read (str_to_read,*) arr(d1, d2)

end subroutine insert_read_str_real_2

pure subroutine insert_row_real_2 (arr, d2, item, fill)
  implicit none
  REAL, allocatable, intent(inout) :: arr(:,:)
  integer, intent(in) :: d2
  REAL, intent(in) :: item(:)
  REAL, intent(in), optional :: fill

  call extend_real_2 (arr, size(item), d2, fill)

  arr(:,d2) = item

end subroutine insert_row_real_2

subroutine insert_read_row_fd_real_2 (arr, d1, d2, fd, fill)
  implicit none
  REAL, allocatable, intent(inout) :: arr(:,:)
  integer, intent(in) :: d1, d2
  integer, intent(in) :: fd
  REAL, intent(in), optional :: fill

  call extend_real_2 (arr, d1, d2, fill)

  read(fd, *) arr(:,d2)

end subroutine insert_read_row_fd_real_2

pure subroutine insert_read_row_str_real_2 (arr, d1, d2, str_to_read, fill)
  implicit none
  REAL, allocatable, intent(inout) :: arr(:,:)
  integer, intent(in) :: d1, d2
  character(len=*), intent(in) :: str_to_read
  REAL, intent(in), optional :: fill

  call extend_real_2 (arr, d1, d2, fill)

  read(str_to_read, *) arr(:,d2)

end subroutine insert_read_row_str_real_2


pure subroutine resize_dble_2 (arr, d1, d2, fill)
  implicit none
  DOUBLE PRECISION, allocatable, intent(inout) :: arr(:,:)
  integer, intent(in) :: d1, d2
  DOUBLE PRECISION, intent(in), optional :: fill

  integer :: arrshape(2)
  integer :: d1_min, d2_min
  DOUBLE PRECISION, allocatable :: tmp(:,:)

  if ((d1 == 0) .and. (d2 == 0)) then
    if(allocated(arr)) deallocate(arr)
    return
  end if

  if (.not. allocated(arr)) then
    allocate(arr(d1, d2))
    if (present(fill)) arr = fill
    return
  end if
  
  arrshape = shape(arr)
  d1_min = min(d1, arrshape(1))
  d2_min = min(d2, arrshape(2))

  if ((d1 /= arrshape(1)) .or. (d2 /= arrshape(2))) then
    allocate(tmp(d1, d2))
    if (present(fill)) tmp = fill
    tmp(1:d1_min, 1:d2_min) = arr(1:d1_min, 1:d2_min)
    call move_alloc(tmp, arr)
  end if

end subroutine resize_dble_2

pure subroutine extend_dble_2 (arr, d1, d2, fill)
  implicit none
  DOUBLE PRECISION, allocatable, intent(inout) :: arr(:,:)
  integer, intent(in) :: d1, d2
  DOUBLE PRECISION, intent(in), optional :: fill

  integer :: arrshape(2)
  integer :: d1_max, d2_max

  arrshape = shape(arr)
  d1_max = max(d1, arrshape(1))
  d2_max = max(d2, arrshape(2))
  call resize_dble_2 (arr, d1_max, d2_max, fill)
end subroutine extend_dble_2

pure subroutine insert_dble_2 (arr, d1, d2, item, fill)
  implicit none
  DOUBLE PRECISION, allocatable, intent(inout) :: arr(:,:)
  integer, intent(in) :: d1, d2
  DOUBLE PRECISION, intent(in) :: item
  DOUBLE PRECISION, intent(in), optional :: fill

  call extend_dble_2 (arr, d1, d2, fill)

  arr(d1, d2) = item

end subroutine insert_dble_2

subroutine insert_read_fd_dble_2 (arr, d1, d2, fd, fill)
  implicit none
  DOUBLE PRECISION, allocatable, intent(inout) :: arr(:,:)
  integer, intent(in) :: d1, d2
  integer, intent(in) :: fd
  DOUBLE PRECISION, intent(in), optional :: fill

  call extend_dble_2 (arr, d1, d2, fill)

  read (fd,*) arr(d1, d2)

end subroutine insert_read_fd_dble_2

pure subroutine insert_read_str_dble_2 (arr, d1, d2, str_to_read, fill)
  implicit none
  DOUBLE PRECISION, allocatable, intent(inout) :: arr(:,:)
  integer, intent(in) :: d1, d2
  character(len=*), intent(in) :: str_to_read
  DOUBLE PRECISION, intent(in), optional :: fill

  call extend_dble_2 (arr, d1, d2, fill)

  read (str_to_read,*) arr(d1, d2)

end subroutine insert_read_str_dble_2

pure subroutine insert_row_dble_2 (arr, d2, item, fill)
  implicit none
  DOUBLE PRECISION, allocatable, intent(inout) :: arr(:,:)
  integer, intent(in) :: d2
  DOUBLE PRECISION, intent(in) :: item(:)
  DOUBLE PRECISION, intent(in), optional :: fill

  call extend_dble_2 (arr, size(item), d2, fill)

  arr(:,d2) = item

end subroutine insert_row_dble_2

subroutine insert_read_row_fd_dble_2 (arr, d1, d2, fd, fill)
  implicit none
  DOUBLE PRECISION, allocatable, intent(inout) :: arr(:,:)
  integer, intent(in) :: d1, d2
  integer, intent(in) :: fd
  DOUBLE PRECISION, intent(in), optional :: fill

  call extend_dble_2 (arr, d1, d2, fill)

  read(fd, *) arr(:,d2)

end subroutine insert_read_row_fd_dble_2

pure subroutine insert_read_row_str_dble_2 (arr, d1, d2, str_to_read, fill)
  implicit none
  DOUBLE PRECISION, allocatable, intent(inout) :: arr(:,:)
  integer, intent(in) :: d1, d2
  character(len=*), intent(in) :: str_to_read
  DOUBLE PRECISION, intent(in), optional :: fill

  call extend_dble_2 (arr, d1, d2, fill)

  read(str_to_read, *) arr(:,d2)

end subroutine insert_read_row_str_dble_2


pure subroutine resize_int_3 (arr, d1, d2, d3, fill)
  implicit none
  INTEGER, allocatable, intent(inout) :: arr(:,:,:)
  integer, intent(in) :: d1, d2, d3
  INTEGER, intent(in), optional :: fill

  integer :: arrshape(3)
  integer :: d1_min, d2_min, d3_min
  INTEGER, allocatable :: tmp(:,:,:)

  if ((d1 == 0) .and. (d2 == 0) .and. (d3 == 0)) then
    if(allocated(arr)) deallocate(arr)
    return
  end if

  if (.not. allocated(arr)) then
    allocate(arr(d1, d2, d3))
    if (present(fill)) arr = fill
    return
  end if
  
  arrshape = shape(arr)
  d1_min = min(d1, arrshape(1))
  d2_min = min(d2, arrshape(2))
  d3_min = min(d3, arrshape(3))

  if ((d1 /= arrshape(1)) .or. (d2 /= arrshape(2)) .or. (d3 /= arrshape(3))) then
    allocate(tmp(d1, d2, d3))
    if (present(fill)) tmp = fill
    tmp(1:d1_min, 1:d2_min, 1:d3_min) = arr(1:d1_min, 1:d2_min, 1:d3_min)
    call move_alloc(tmp, arr)
  end if

end subroutine resize_int_3

pure subroutine extend_int_3 (arr, d1, d2, d3, fill)
  implicit none
  INTEGER, allocatable, intent(inout) :: arr(:,:,:)
  integer, intent(in) :: d1, d2, d3
  INTEGER, intent(in), optional :: fill

  integer :: arrshape(3)
  integer :: d1_max, d2_max, d3_max

  arrshape = shape(arr)
  d1_max = max(d1, arrshape(1))
  d2_max = max(d2, arrshape(2))
  d3_max = max(d3, arrshape(3))
  call resize_int_3 (arr, d1_max, d2_max, d3_max, fill)
end subroutine extend_int_3

pure subroutine insert_int_3 (arr, d1, d2, d3, item, fill)
  implicit none
  INTEGER, allocatable, intent(inout) :: arr(:,:,:)
  integer, intent(in) :: d1, d2, d3
  INTEGER, intent(in) :: item
  INTEGER, intent(in), optional :: fill

  call extend_int_3 (arr, d1, d2, d3, fill)

  arr(d1, d2, d3) = item

end subroutine insert_int_3

subroutine insert_read_fd_int_3 (arr, d1, d2, d3, fd, fill)
  implicit none
  INTEGER, allocatable, intent(inout) :: arr(:,:,:)
  integer, intent(in) :: d1, d2, d3
  integer, intent(in) :: fd
  INTEGER, intent(in), optional :: fill

  call extend_int_3 (arr, d1, d2, d3, fill)

  read (fd,*) arr(d1, d2, d3)

end subroutine insert_read_fd_int_3

pure subroutine insert_read_str_int_3 (arr, d1, d2, d3, str_to_read, fill)
  implicit none
  INTEGER, allocatable, intent(inout) :: arr(:,:,:)
  integer, intent(in) :: d1, d2, d3
  character(len=*), intent(in) :: str_to_read
  INTEGER, intent(in), optional :: fill

  call extend_int_3 (arr, d1, d2, d3, fill)

  read (str_to_read,*) arr(d1, d2, d3)

end subroutine insert_read_str_int_3

pure subroutine insert_row_int_3 (arr, d2, d3, item, fill)
  implicit none
  INTEGER, allocatable, intent(inout) :: arr(:,:,:)
  integer, intent(in) :: d2, d3
  INTEGER, intent(in) :: item(:)
  INTEGER, intent(in), optional :: fill

  call extend_int_3 (arr, size(item), d2, d3, fill)

  arr(:,d2, d3) = item

end subroutine insert_row_int_3

subroutine insert_read_row_fd_int_3 (arr, d1, d2, d3, fd, fill)
  implicit none
  INTEGER, allocatable, intent(inout) :: arr(:,:,:)
  integer, intent(in) :: d1, d2, d3
  integer, intent(in) :: fd
  INTEGER, intent(in), optional :: fill

  call extend_int_3 (arr, d1, d2, d3, fill)

  read(fd, *) arr(:,d2, d3)

end subroutine insert_read_row_fd_int_3

pure subroutine insert_read_row_str_int_3 (arr, d1, d2, d3, str_to_read, fill)
  implicit none
  INTEGER, allocatable, intent(inout) :: arr(:,:,:)
  integer, intent(in) :: d1, d2, d3
  character(len=*), intent(in) :: str_to_read
  INTEGER, intent(in), optional :: fill

  call extend_int_3 (arr, d1, d2, d3, fill)

  read(str_to_read, *) arr(:,d2, d3)

end subroutine insert_read_row_str_int_3


pure subroutine resize_real_3 (arr, d1, d2, d3, fill)
  implicit none
  REAL, allocatable, intent(inout) :: arr(:,:,:)
  integer, intent(in) :: d1, d2, d3
  REAL, intent(in), optional :: fill

  integer :: arrshape(3)
  integer :: d1_min, d2_min, d3_min
  REAL, allocatable :: tmp(:,:,:)

  if ((d1 == 0) .and. (d2 == 0) .and. (d3 == 0)) then
    if(allocated(arr)) deallocate(arr)
    return
  end if

  if (.not. allocated(arr)) then
    allocate(arr(d1, d2, d3))
    if (present(fill)) arr = fill
    return
  end if
  
  arrshape = shape(arr)
  d1_min = min(d1, arrshape(1))
  d2_min = min(d2, arrshape(2))
  d3_min = min(d3, arrshape(3))

  if ((d1 /= arrshape(1)) .or. (d2 /= arrshape(2)) .or. (d3 /= arrshape(3))) then
    allocate(tmp(d1, d2, d3))
    if (present(fill)) tmp = fill
    tmp(1:d1_min, 1:d2_min, 1:d3_min) = arr(1:d1_min, 1:d2_min, 1:d3_min)
    call move_alloc(tmp, arr)
  end if

end subroutine resize_real_3

pure subroutine extend_real_3 (arr, d1, d2, d3, fill)
  implicit none
  REAL, allocatable, intent(inout) :: arr(:,:,:)
  integer, intent(in) :: d1, d2, d3
  REAL, intent(in), optional :: fill

  integer :: arrshape(3)
  integer :: d1_max, d2_max, d3_max

  arrshape = shape(arr)
  d1_max = max(d1, arrshape(1))
  d2_max = max(d2, arrshape(2))
  d3_max = max(d3, arrshape(3))
  call resize_real_3 (arr, d1_max, d2_max, d3_max, fill)
end subroutine extend_real_3

pure subroutine insert_real_3 (arr, d1, d2, d3, item, fill)
  implicit none
  REAL, allocatable, intent(inout) :: arr(:,:,:)
  integer, intent(in) :: d1, d2, d3
  REAL, intent(in) :: item
  REAL, intent(in), optional :: fill

  call extend_real_3 (arr, d1, d2, d3, fill)

  arr(d1, d2, d3) = item

end subroutine insert_real_3

subroutine insert_read_fd_real_3 (arr, d1, d2, d3, fd, fill)
  implicit none
  REAL, allocatable, intent(inout) :: arr(:,:,:)
  integer, intent(in) :: d1, d2, d3
  integer, intent(in) :: fd
  REAL, intent(in), optional :: fill

  call extend_real_3 (arr, d1, d2, d3, fill)

  read (fd,*) arr(d1, d2, d3)

end subroutine insert_read_fd_real_3

pure subroutine insert_read_str_real_3 (arr, d1, d2, d3, str_to_read, fill)
  implicit none
  REAL, allocatable, intent(inout) :: arr(:,:,:)
  integer, intent(in) :: d1, d2, d3
  character(len=*), intent(in) :: str_to_read
  REAL, intent(in), optional :: fill

  call extend_real_3 (arr, d1, d2, d3, fill)

  read (str_to_read,*) arr(d1, d2, d3)

end subroutine insert_read_str_real_3

pure subroutine insert_row_real_3 (arr, d2, d3, item, fill)
  implicit none
  REAL, allocatable, intent(inout) :: arr(:,:,:)
  integer, intent(in) :: d2, d3
  REAL, intent(in) :: item(:)
  REAL, intent(in), optional :: fill

  call extend_real_3 (arr, size(item), d2, d3, fill)

  arr(:,d2, d3) = item

end subroutine insert_row_real_3

subroutine insert_read_row_fd_real_3 (arr, d1, d2, d3, fd, fill)
  implicit none
  REAL, allocatable, intent(inout) :: arr(:,:,:)
  integer, intent(in) :: d1, d2, d3
  integer, intent(in) :: fd
  REAL, intent(in), optional :: fill

  call extend_real_3 (arr, d1, d2, d3, fill)

  read(fd, *) arr(:,d2, d3)

end subroutine insert_read_row_fd_real_3

pure subroutine insert_read_row_str_real_3 (arr, d1, d2, d3, str_to_read, fill)
  implicit none
  REAL, allocatable, intent(inout) :: arr(:,:,:)
  integer, intent(in) :: d1, d2, d3
  character(len=*), intent(in) :: str_to_read
  REAL, intent(in), optional :: fill

  call extend_real_3 (arr, d1, d2, d3, fill)

  read(str_to_read, *) arr(:,d2, d3)

end subroutine insert_read_row_str_real_3


pure subroutine resize_dble_3 (arr, d1, d2, d3, fill)
  implicit none
  DOUBLE PRECISION, allocatable, intent(inout) :: arr(:,:,:)
  integer, intent(in) :: d1, d2, d3
  DOUBLE PRECISION, intent(in), optional :: fill

  integer :: arrshape(3)
  integer :: d1_min, d2_min, d3_min
  DOUBLE PRECISION, allocatable :: tmp(:,:,:)

  if ((d1 == 0) .and. (d2 == 0) .and. (d3 == 0)) then
    if(allocated(arr)) deallocate(arr)
    return
  end if

  if (.not. allocated(arr)) then
    allocate(arr(d1, d2, d3))
    if (present(fill)) arr = fill
    return
  end if
  
  arrshape = shape(arr)
  d1_min = min(d1, arrshape(1))
  d2_min = min(d2, arrshape(2))
  d3_min = min(d3, arrshape(3))

  if ((d1 /= arrshape(1)) .or. (d2 /= arrshape(2)) .or. (d3 /= arrshape(3))) then
    allocate(tmp(d1, d2, d3))
    if (present(fill)) tmp = fill
    tmp(1:d1_min, 1:d2_min, 1:d3_min) = arr(1:d1_min, 1:d2_min, 1:d3_min)
    call move_alloc(tmp, arr)
  end if

end subroutine resize_dble_3

pure subroutine extend_dble_3 (arr, d1, d2, d3, fill)
  implicit none
  DOUBLE PRECISION, allocatable, intent(inout) :: arr(:,:,:)
  integer, intent(in) :: d1, d2, d3
  DOUBLE PRECISION, intent(in), optional :: fill

  integer :: arrshape(3)
  integer :: d1_max, d2_max, d3_max

  arrshape = shape(arr)
  d1_max = max(d1, arrshape(1))
  d2_max = max(d2, arrshape(2))
  d3_max = max(d3, arrshape(3))
  call resize_dble_3 (arr, d1_max, d2_max, d3_max, fill)
end subroutine extend_dble_3

pure subroutine insert_dble_3 (arr, d1, d2, d3, item, fill)
  implicit none
  DOUBLE PRECISION, allocatable, intent(inout) :: arr(:,:,:)
  integer, intent(in) :: d1, d2, d3
  DOUBLE PRECISION, intent(in) :: item
  DOUBLE PRECISION, intent(in), optional :: fill

  call extend_dble_3 (arr, d1, d2, d3, fill)

  arr(d1, d2, d3) = item

end subroutine insert_dble_3

subroutine insert_read_fd_dble_3 (arr, d1, d2, d3, fd, fill)
  implicit none
  DOUBLE PRECISION, allocatable, intent(inout) :: arr(:,:,:)
  integer, intent(in) :: d1, d2, d3
  integer, intent(in) :: fd
  DOUBLE PRECISION, intent(in), optional :: fill

  call extend_dble_3 (arr, d1, d2, d3, fill)

  read (fd,*) arr(d1, d2, d3)

end subroutine insert_read_fd_dble_3

pure subroutine insert_read_str_dble_3 (arr, d1, d2, d3, str_to_read, fill)
  implicit none
  DOUBLE PRECISION, allocatable, intent(inout) :: arr(:,:,:)
  integer, intent(in) :: d1, d2, d3
  character(len=*), intent(in) :: str_to_read
  DOUBLE PRECISION, intent(in), optional :: fill

  call extend_dble_3 (arr, d1, d2, d3, fill)

  read (str_to_read,*) arr(d1, d2, d3)

end subroutine insert_read_str_dble_3

pure subroutine insert_row_dble_3 (arr, d2, d3, item, fill)
  implicit none
  DOUBLE PRECISION, allocatable, intent(inout) :: arr(:,:,:)
  integer, intent(in) :: d2, d3
  DOUBLE PRECISION, intent(in) :: item(:)
  DOUBLE PRECISION, intent(in), optional :: fill

  call extend_dble_3 (arr, size(item), d2, d3, fill)

  arr(:,d2, d3) = item

end subroutine insert_row_dble_3

subroutine insert_read_row_fd_dble_3 (arr, d1, d2, d3, fd, fill)
  implicit none
  DOUBLE PRECISION, allocatable, intent(inout) :: arr(:,:,:)
  integer, intent(in) :: d1, d2, d3
  integer, intent(in) :: fd
  DOUBLE PRECISION, intent(in), optional :: fill

  call extend_dble_3 (arr, d1, d2, d3, fill)

  read(fd, *) arr(:,d2, d3)

end subroutine insert_read_row_fd_dble_3

pure subroutine insert_read_row_str_dble_3 (arr, d1, d2, d3, str_to_read, fill)
  implicit none
  DOUBLE PRECISION, allocatable, intent(inout) :: arr(:,:,:)
  integer, intent(in) :: d1, d2, d3
  character(len=*), intent(in) :: str_to_read
  DOUBLE PRECISION, intent(in), optional :: fill

  call extend_dble_3 (arr, d1, d2, d3, fill)

  read(str_to_read, *) arr(:,d2, d3)

end subroutine insert_read_row_str_dble_3


end module array_utils