include 'bananamod.f90'
include 'variables.f90'

subroutine BSTART()
use bananamod
use variables 
! Reading banangate files
call t1alpha%readfile('bananagate_filename1')
call t1alpha%readfile('bananagate_filename2')
call t1alpha%readfile('bananagate_filename3') 
return
end subroutine

subroutine FSTART()
return
end subroutine


subroutine USERSUB()
use bananamod
use variables 
! Using t1alpha bananagate
if (t1alpha%gate(x1,y1)) then 
!   do something
endif 

! Using t2alpha bananagate
if (t2alpha%gate(x2,y2)) then 
!   do something
endif 

! Using t3alpha bananagate
if (t3alpha%gate(x3,y3)) then 
!   do something
endif  
return
end subroutine 


subroutine FSTOP()
return
end subroutine

subroutine BSTOP()
return
end subroutine

