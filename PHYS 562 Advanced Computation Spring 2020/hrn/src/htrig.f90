
program htrig

    use numtype
    implicit none

    real(dp) :: coeff(0:100)
    real(dp) :: x, y, dx
    integer :: i, imax

    ! Horner scheme for sine function

    imax = 100

    coeff(0) = 0
    coeff(1) = 1
    coeff(2:imax:2) = 0

    do i = 3, imax-1, 2
        coeff(i) = -coeff(i-2) / (i*(i-1))
    end do

    x = -2*pi
    dx = pi/30

    do while (x .le. 2*pi)
        y = coeff(imax)
        do i = imax-1, 0, -1
        y = coeff(i) + x*y 
        end do
        write(1,*) x, y
        y = sin(x)
        write(2,*) x, y
        x = x+dx
    end do 

    ! ---------------------------------
    ! Horner scheme for cosine function

    coeff(0) = 1
    coeff(1:imax:2) = 0

    do i = 2, imax, 2
        coeff(i) = -coeff(i-2) / (i*(i-1))
    end do

    x = -2*pi
    dx = pi/50

    do while (x .le. 2*pi)
        y = coeff(imax)
        do i = imax-1, 0, -1
        y = coeff(i) + x*y 
        end do
        write(3,*) x, y
        y = cos(x)
        write(4,*) x, y
        x = x+dx
    end do 

end program htrig
