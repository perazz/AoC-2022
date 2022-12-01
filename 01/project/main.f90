program aoc_2022_01
    use iso_fortran_env
    implicit none

    character(*), parameter :: fileName = 'input.txt'
    integer :: iunit,ios,meal
    integer, allocatable :: calories(:)

    open(newunit=iunit,file=fileName,form='formatted',iostat=ios)
    allocate(calories(1),source=0)
    do while (.not.is_iostat_end(ios))
        read(iunit,'(i10)',blank='ZERO',iostat=ios) meal
        if (meal==0) then
            calories = [calories,meal]
        else
            calories(size(calories)) = calories(size(calories))+meal
        end if
    end do
    close(iunit)

    print "(*(a,:,i0))", 'elf ',maxloc(calories),' of ',size(calories),' ate ',maxval(calories),' calories'

end program
