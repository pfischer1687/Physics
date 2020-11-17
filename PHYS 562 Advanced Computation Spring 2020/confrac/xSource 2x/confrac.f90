
program confrac

    use numtype
    implicit none

    real(dp) :: x, ww, fx, y
    integer :: nn, i

    x = 1._dp

    do nn = 2, 21
        ww = 0

        do i = nn, 1, -1

            ww = ((i*x)**2)/(2*i+1+ww)

        end do

        fx = x/(1._dp+ww)

        y = pi - (4._dp * fx)
        
        write(1,*) nn, y

    end do

end program confrac
