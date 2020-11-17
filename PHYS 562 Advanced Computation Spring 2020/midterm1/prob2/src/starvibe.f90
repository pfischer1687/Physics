
module setup

    use numtype
    implicit none
    integer, parameter :: n_eq = 6

    real(dp), parameter :: m = 40.e30_dp, &
        r = 10.e3_dp, nu = 40.e3_dp, eps = 10.e-8_dp

end module setup

program starvibe

    use setup
    implicit none
    real(dp), dimension(n_eq) :: y
    real(dp), dimension(3) :: O
    real(dp) :: t, dt, tmax, omega, w, I0, &
        Ix, Iy, Iz
    integer :: i

    t = 0._dp
    dt = 0.01_dp
    tmax = 20._dp

    w = 2 * pi * nu
    omega = 100 * w
    I0 = 2._dp / 5 * m * r**2

    y(1) = 0._dp
    y(2) = 0._dp
    y(3) = 0._dp
    y(4) = I0 * omega / sqrt(3._dp) ! L_x
    y(5) = I0 * omega / sqrt(3._dp) ! L_y
    y(6) = I0 * omega / sqrt(3._dp) ! L_z

    do while ( t < tmax )

        Ix = I0 * ( 1 - eps / 2 * cos( w * t ) )
        Iy = Ix
        Iz = I0 * ( 1 + eps * cos( w * t ) )

        O(1) = y(4) / Ix 
        O(2) = y(5) / Iy 
        O(3) = y(6) / Iz

        do i = 1 , 3
            write(i,*) t, O(i)
        end do

        call rk4step( t, dt, y )

    end do

end program starvibe

subroutine rk4step(x, h, y)

    use setup
    implicit none
    real(dp), intent(inout) :: x
    real(dp), intent(in) :: h
    real(dp), intent(inout), dimension(n_eq) :: y
    real(dp), dimension(n_eq) :: k1, k2, k3, k4, dy

    k1 = kv (x, h, y)
    k2 = kv (x+h/2, h, y+k1/2)
    k3 = kv (x+h/2, h, y+k2/2)
    k4 = kv (x+h, h, y+k3)

    dy = (k1 + 2*k2 + 2*k3 + k4) / 6

    x = x + h 
    y = y + dy

    contains

        function kv (t, dt, y) result(k)

            use setup
            implicit none
            real(dp), intent(in) :: t
            real(dp), intent(in) :: dt
            real(dp), intent(in), dimension(n_eq) :: y
            real(dp), dimension(n_eq) :: f, k
            real(dp) :: I0, w, g

            I0 = 2._dp / 5 * m * r**2
			w = 2 * pi * nu
			g = eps * cos ( w * t )

	        f(1:3) = y(4:6)

			f(4) = - 3 / ( 2 * I0 ) * y(5) * y(6) * g / ( ( 1 + g ) * ( 1 - g / 2 ) )

			f(5) = 3 / ( 2 * I0 ) * y(4) * y(6) * g / ( ( 1 + g ) * ( 1 - g / 2 ) )

			f(6) = 0

            k(1:n_eq) = h*f(1:n_eq)

        end function kv

end subroutine rk4step
