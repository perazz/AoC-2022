program aoc_06
    implicit none
    integer :: bytes,i,ascii(0:127) = 0
    character, allocatable :: stream(:)

    inquire(file='input.txt',size=bytes)
    allocate(stream(bytes))
    open(1,file='input.txt',access='stream')
    read(1) stream
    close(1)
    do i=1,bytes
       ascii(ichar(stream(i:i))) = ascii(ichar(stream(i:i)))+1
       if (i<4) cycle
       if (all(ascii(ichar(stream(i-3:i)))==1)) exit
       ascii(ichar(stream(i-3:i-3))) = ascii(ichar(stream(i-3:i-3)))-1
    end do
    print *, 'start=',i

end program
