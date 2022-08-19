module bananamod
implicit none 

type, public :: banana 
    private
    real,allocatable :: x(:), y(:) 
    integer :: v 

    contains 
        private 
        procedure, pass, private :: inside_gate
        procedure, pass, private :: inside_gate_r 
        procedure, pass, private :: readBananaFile

        procedure, pass, public :: readfile => readBananaFile
        generic, public :: gate => inside_gate, inside_gate_r

        final :: destructor
end type


contains 


subroutine readBananaFile(this, fname)
class(banana) :: this
character (*),intent(in) :: fname
character (LEN=100) ::string
character (LEN=2) ::dummy
character (LEN=1) ::dummy1
integer :: i, m


open (unit=22,status='old',file=fname)
do i=1,3
  read(22,*)string !reading the first three lines of .ban file
end do

read (22,50)this%v !reading the fourth line of .ban file. noting down the number of vertices

allocate(this%x(this%v),this%y(this%v))

do i=1,this%v
    read(22,*)dummy1,dummy1,dummy,m,this%x(i),this%y(i)
    !write(*,*)this%x(i)/2.0,this%y(i)/2.0
end do
this%x = this%x/2.0
this%y = this%y/2.0

close(22)
50 format(2x,I3)
! close(23)
end subroutine


logical function inside_gate(this,cx,cy)
class(banana) :: this 
integer, intent(in) :: cx,cy
real :: Ca,Cb ! Coordinates
logical :: jj
integer :: i,j

Ca = 1.0*cx 
Cb = 1.0*cy 

inside_gate = .false.
j = this%v
jj = .false.

do i=1,this%v
    if (( (this%y(i) .ge. Cb) .neqv. (this%y(j) .ge. Cb) ) .and. &
    ( Ca .le. ((this%x(j)-this%x(i))*(Cb-this%y(i))/(this%y(j)-this%y(i))+this%x(i)))) then
        jj = .not. jj
    end if  
    j = i
end do

if (jj) inside_gate = .true.

return 
end function 

logical function inside_gate_r(this,cx,cy)
class(banana) :: this 
real, intent(in) :: cx,cy
real :: Ca,Cb ! Coordinates
logical :: jj
integer :: i,j

Ca = cx 
Cb = cy 

inside_gate_r = .false.
j = this%v
jj = .false.

do i=1,this%v
    if (( (this%y(i) .ge. Cb) .neqv. (this%y(j) .ge. Cb) ) .and. &
    ( Ca .le. ((this%x(j)-this%x(i))*(Cb-this%y(i))/(this%y(j)-this%y(i))+this%x(i)))) then
        jj = .not. jj
    end if  
    j = i
end do

if (jj) inside_gate_r = .true.

return 
end function 

subroutine destructor(this)
        type(banana) :: this
        if (ALLOCATED(this % x)) deallocate(this % x)
        if (ALLOCATED(this % y)) deallocate(this % y)
end subroutine

end module bananamod




!program test_read 
!use bananamod
!implicit none 
!
!type (banana) :: fold, t1t2, test
!
!call fold%readfile('fold129.6.ban')
!print*,fold%gate(3500,3500)
!
!call t1t2%readfile('t1t2_129.6.ban')
!print*,t1t2%gate(4400.,4400.) 
!
!call test%readfile('test.ban')
!print*,test%gate(1800,1800)
!end program 
