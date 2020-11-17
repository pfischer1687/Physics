
program confrac

    use numtype
    implicit none
    real(dp) :: x, y

    x = 1._dp

    y = 4._dp * arctan(x)

    print *, pi, y

    contains

        function arctan(x) result(fx)

            use numtype
            implicit none

            real(dp) :: x, ww, fx
            integer :: nn, i

            nn = 100
            ww = 0

            do i = nn, 1, -1

                ww = ((i*x)**2)/(2*i+1+ww)

            end do

            fx = x/(1._dp+ww)


        end function arctan

end program confrac
