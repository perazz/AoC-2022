1 integer, allocatable :: eat(:)
2 open(newunit=iunit,file='input.txt',form='formatted')
3 allocate(eat(1),source=0)
4 read(iunit,'(i10)',blank='ZERO',err=8,end=8) meal
5 if (meal) 6,6,7
6 eat = [eat,meal]; goto 4
7 eat(size(eat)) = eat(size(eat))+meal; goto 4
8 print "(*(a,:,i0))", 'elf ',maxloc(eat),' of ',size(eat),' ate ',maxval(eat),' calories'
9 itot3=0; do i=1,3; itot3=itot3+eat(maxloc(eat,1)); eat(maxloc(eat,1)) = 0; end do; print *, 'tot3=',itot3
10 end program
