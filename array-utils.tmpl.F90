## Template file for automatic generation of the extendable arrays module

! Copyright (C) 2013 Chris Kerr
! 
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are
! met:
! 
!     * Redistributions of source code must retain the above copyright
!       notice, this list of conditions and the following disclaimer.
! 
!     * Redistributions in binary form must reproduce the above
!       copyright notice, this list of conditions and the following
!       disclaimer in the documentation and/or other materials provided
!       with the distribution.
! 
!     * Neither the name of the author nor the names of any
!       contributors may be used to endorse or promote products derived
!       from this software without specific prior written permission.
! 
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
! A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
! OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
! SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
! LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
! DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
! THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
! (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

module array_utils

#for $interface_name in ['resize', 'extend', 'insert']
interface $interface_name
#for N, longtype, shorttype, typeN, dNs, colons in $descriptor_tuples
  module procedure ${interface_name}_${typeN}
#end for
end interface $interface_name

#end for

interface insert_read
#for N, longtype, shorttype, typeN, dNs, colons in $descriptor_tuples
  module procedure insert_read_fd_${typeN}
  module procedure insert_read_str_${typeN}
#end for
end interface insert_read

interface insert_row
#for N, longtype, shorttype, typeN, dNs, colons in $descriptor_tuples
#if (N > 1)
  module procedure insert_row_${typeN}
#end if
#end for
end interface insert_row

interface insert_read_row
#for N, longtype, shorttype, typeN, dNs, colons in $descriptor_tuples
#if (N > 1)
  module procedure insert_read_row_fd_${typeN}
  module procedure insert_read_row_str_${typeN}
#end if
#end for
end interface insert_read_row

contains

#for N, longtype, shorttype, typeN, dNs, colons in $descriptor_tuples

pure subroutine resize_${typeN} (arr, ${', '.join(dNs)}, fill)
  implicit none
  ${longtype}, allocatable, intent(inout) :: arr(${','.join(colons)})
  integer, intent(in) :: ${', '.join(dNs)}
  ${longtype}, intent(in), optional :: fill

  integer :: arrshape(${N})
  integer :: ${', '.join([dN+'_min' for dN in dNs])}
  ${longtype}, allocatable :: tmp(${','.join(colons)})

  if (${' .and. '.join(['(%s == 0)' % dN for dN in dNs])}) then
    if(allocated(arr)) deallocate(arr)
    return
  end if

  if (.not. allocated(arr)) then
    allocate(arr(${', '.join(dNs)}))
    if (present(fill)) arr = fill
    return
  end if
  
  arrshape = shape(arr)
#for i,dN in enumerate(dNs)
  ${dN}_min = min(${dN}, arrshape(${i+1}))
#end for

  if (${' .or. '.join(['(%s /= arrshape(%d))' % (dN,i+1) for i,dN in enumerate(dNs)])}) then
    allocate(tmp(${', '.join(dNs)}))
    if (present(fill)) tmp = fill
    tmp(${', '.join(['1:%s_min' % dN for dN in dNs])}) = arr(${', '.join(['1:%s_min' % dN for dN in dNs])})
    call move_alloc(tmp, arr)
  end if

end subroutine resize_${typeN}

pure subroutine extend_${typeN} (arr, ${', '.join(dNs)}, fill)
  implicit none
  ${longtype}, allocatable, intent(inout) :: arr(${','.join(colons)})
  integer, intent(in) :: ${', '.join(dNs)}
  ${longtype}, intent(in), optional :: fill

  integer :: arrshape(${N})
  integer :: ${', '.join([dN+'_max' for dN in dNs])}

  arrshape = shape(arr)
#for i,dN in enumerate(dNs)
  ${dN}_max = max(${dN}, arrshape(${i+1}))
#end for
  call resize_${typeN} (arr, ${', '.join([dN+'_max' for dN in dNs])}, fill)
end subroutine extend_${typeN}

pure subroutine insert_${typeN} (arr, ${', '.join(dNs)}, item, fill)
  implicit none
  ${longtype}, allocatable, intent(inout) :: arr(${','.join(colons)})
  integer, intent(in) :: ${', '.join(dNs)}
  ${longtype}, intent(in) :: item
  ${longtype}, intent(in), optional :: fill

  call extend_${typeN} (arr, ${', '.join(dNs)}, fill)

  arr(${', '.join(dNs)}) = item

end subroutine insert_${typeN}

subroutine insert_read_fd_${typeN} (arr, ${', '.join(dNs)}, fd, fill)
  implicit none
  ${longtype}, allocatable, intent(inout) :: arr(${','.join(colons)})
  integer, intent(in) :: ${', '.join(dNs)}
  integer, intent(in) :: fd
  ${longtype}, intent(in), optional :: fill

  call extend_${typeN} (arr, ${', '.join(dNs)}, fill)

  read (fd,*) arr(${', '.join(dNs)})

end subroutine insert_read_fd_${typeN}

pure subroutine insert_read_str_${typeN} (arr, ${', '.join(dNs)}, str_to_read, fill)
  implicit none
  ${longtype}, allocatable, intent(inout) :: arr(${','.join(colons)})
  integer, intent(in) :: ${', '.join(dNs)}
  character(len=*), intent(in) :: str_to_read
  ${longtype}, intent(in), optional :: fill

  call extend_${typeN} (arr, ${', '.join(dNs)}, fill)

  read (str_to_read,*) arr(${', '.join(dNs)})

end subroutine insert_read_str_${typeN}

#if (N > 1)
pure subroutine insert_row_${typeN} (arr, ${', '.join(dNs[1:])}, item, fill)
  implicit none
  ${longtype}, allocatable, intent(inout) :: arr(${','.join(colons)})
  integer, intent(in) :: ${', '.join(dNs[1:])}
  ${longtype}, intent(in) :: item(:)
  ${longtype}, intent(in), optional :: fill

  call extend_${typeN} (arr, size(item), ${', '.join(dNs[1:])}, fill)

  arr(:,${', '.join(dNs[1:])}) = item

end subroutine insert_row_${typeN}

subroutine insert_read_row_fd_${typeN} (arr, ${', '.join(dNs)}, fd, fill)
  implicit none
  ${longtype}, allocatable, intent(inout) :: arr(${','.join(colons)})
  integer, intent(in) :: ${', '.join(dNs)}
  integer, intent(in) :: fd
  ${longtype}, intent(in), optional :: fill

  call extend_${typeN} (arr, ${', '.join(dNs)}, fill)

  read(fd, *) arr(:,${', '.join(dNs[1:])})

end subroutine insert_read_row_fd_${typeN}

pure subroutine insert_read_row_str_${typeN} (arr, ${', '.join(dNs)}, str_to_read, fill)
  implicit none
  ${longtype}, allocatable, intent(inout) :: arr(${','.join(colons)})
  integer, intent(in) :: ${', '.join(dNs)}
  character(len=*), intent(in) :: str_to_read
  ${longtype}, intent(in), optional :: fill

  call extend_${typeN} (arr, ${', '.join(dNs)}, fill)

  read(str_to_read, *) arr(:,${', '.join(dNs[1:])})

end subroutine insert_read_row_str_${typeN}
#end if

#end for

end module array_utils