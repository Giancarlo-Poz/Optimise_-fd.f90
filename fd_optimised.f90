program fd
implicit none
double precision, dimension (17,17) :: a, b
integer :: i, j, n
a = 0.0d0

! set the static boundary conditions
do i=1, 17
    a(i,1) = 1.0d0
    a(i,17) = 2.0d0
    a(1,i) = 3.0d0
end do
b = a

! run through the matrix solving for the stencil
do n=1, 10
    !run through the first n+1 rows
    do i=2, n+1
        do j=2, 16
            b(i,j) = a(i,j) / 2.0d0 + ( a(i+1,j) + a(i-1,j) + a(i,j+1) + a(i,j-1) ) / 8.0d0
        end do
    end do
    !continue with the first n+1 and last n+1 columns    
    do i=n+2, 16
        do j=2, n+1
            b(i,j) = a(i,j) / 2.0d0 + ( a(i+1,j) + a(i-1,j) + a(i,j+1) + a(i,j-1) ) / 8.0d0
        end do
        do j=17-n, 16
            b(i,j) = a(i,j) / 2.0d0 + ( a(i+1,j) + a(i-1,j) + a(i,j+1) + a(i,j-1) ) / 8.0d0
        end do
    end do
    
    a = b
end do

write(*,*) a(5,5), 0.0d0, a(12,12)
end program
