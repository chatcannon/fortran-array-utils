! Template file for automatic generation of the extendable arrays module

! Copyright (C) 2013 Chris Kerr
! You may use this software under the terms of the GNU General Public License (GPL)
! version 3 or, at your option, any later version

module array_utils

#for $interface_name in ['resize', 'extend', 'insert']
interface $interface_name
  module procedure ${', '.join([interface_name+'_'+typeN for typeN in $all_typeN])}
end interface $interface_name

#end for

interface insert_row
  module procedure ${', '.join([interface_name+'_'+typeN 
                                for N, longtype, shorttype, typeN, dNs, colons in $descriptor_tuples
                                if (N > 1)])}
end interface insert_row

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
#end if

#end for

end module array_utils