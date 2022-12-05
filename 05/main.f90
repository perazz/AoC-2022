program aoc_05
    implicit none

    character :: crates(9,0:999)=''
    character(len=20) :: thisMove(6)
    integer :: i,ierr=0,level=0,moves=0,movement(3,999)=0,stack(9)

    call parse

    do i=1,moves
        call move(crates,movement(:,i))
    end do

    ! Print stack top
    print "(a,*(1x,i0))", 'final stacks: ',stack
    print *, (crates(i,stack(i)),i=1,9)

    contains

    subroutine parse

      ! Parse craters like [P] [B] [B] [P] [Q] [S] [L] [H] [B]
      open(1,file='input.txt')
      do while (ierr==0 .and. crates(1,level)/='1')
         read(1,"(9(1x,a1,2x))",iostat=ierr) crates(:,level+1)
         level = level+1
      end do
      level = level-1 ! Take out index level

      ! Parse movements
      read(1,*)
      do
         read(1,*,iostat=ierr) thisMove
         if (ierr/=0) exit
         moves=moves+1
         do i=1,3; read(thisMove(2*i),*) movement(i,moves); end do

         print *, movement(:,moves)
      end do
      close(1)

      ! Sort crates back to stacks
      crates(:,1:level+1) = crates(:,level:0:-1)
      stack = findloc(crates(:,1:level+1),' ',dim=2)-1

      print "(9(1x,a1,2x))",crates(:,1:level)
      print "(a,*(1x,i0))", 'initial stacks: ',stack

    end subroutine parse

    subroutine move(crates,this_move)
       character, intent(inout) :: crates(:,0:)
       integer, intent(in) :: this_move(3)
       integer :: j
       associate(count=>this_move(1),from=>this_move(2),to=>this_move(3))

       do j=1,count
           if (stack(from)<=0) cycle
           stack(to) = stack(to)+1
           crates(to  ,stack(to)  ) = crates(from,stack(from))
           crates(from,stack(from)) = ' '
           stack(from) = stack(from)-1
       end do

       print "('moved ',i0,' from ',i0,' to ',i0,':')",this_move
       print "(9(1x,a1,2x))",crates(:,1:maxval(stack,1))

       end associate
    end subroutine move

end program

