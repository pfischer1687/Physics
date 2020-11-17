
module setup

    use numtype
    implicit none
    integer, parameter :: n_eq = 4

    real(dp), parameter :: g = 1._dp, l_1 = 2._dp, &
        m_1 = 3._dp, l_2 = 1._dp, m_2 = 1._dp

end module setup

program dubpen

    use setup
    implicit none
    real(dp), dimension(n_eq) :: y
    real(dp) :: t, dt, tmax, w2, y1, yl, yu, &
        x_1, x_2, y_1, y_2, E, Et, eps, y3
    real(dp), dimension(3) :: Ei
    real(dp), dimension(4) :: Ef

    t = 0._dp
    dt = 0.1_dp
    tmax = 600._dp

    ! Enter initial conditions
    y(1) = pi                  ! theta_1
    y(2) = 0._dp               ! omega_1
    y(3) = pi                  ! theta_2
    y(4) = 0.14142135465680267 ! omega_2

    ! Energy terms to be summed
    Ei(1) = 1._dp / 2 * m_2 * l_2**2 * y(4)**2
    Ei(2) = m_1 * g * ( l_2 + l_1 * ( 1 - cos( y(1) ) ) )
    Ei(3) = m_2 * g * ( l_1 * ( 1 - cos( y(1) ) ) + l_2 * ( 1 - cos( y(3) ) ) )

    ! Energy for intial conditions
    E = sum( Ei )

    ! w2 such that E = Ei + 0.01
    w2 = sqrt( 2 * ( 0.01 ) / ( m_2 * l_2**2 ) )

    print *, "E =", E, "w2 =", w2

    ! For plotting Poincare section
    eps = 0.2

    do while ( t < tmax )


        ! Plot angles vs. time
        write(1,*) t, y(1)
        write(2,*) t, y(3) 

        ! Plot positions y vs. x
        x_1 = l_1 * sin( y(1) )
        y_1 = l_2 + l_1 * ( 1 - cos( y(1) ) )
        x_2 = x_1 + l_2 * sin( y(3) )
        y_2 = y_1 - l_2 * cos( y(3) )
        write(3,*) x_1, y_1 
        write(4,*) x_2, y_2 

        ! Plot energy vs. time
        Ef(1) = 1._dp / 2 * ( m_1 + m_2 ) * l_1**2 * y(2)**2
        Ef(2) = 1._dp / 2 * m_2 * l_2**2 * y(4)**2
        Ef(3) = m_2 * l_1 * l_2 * y(2) * y(4) * cos( y(1) - y(3) )
        Ef(4) = m_1 * g * ( y_1 + y_2 )
        Et = sum( Ef )
        write(7,*) t, Et

        ! Plot Poincare Section
        y1 = y(1)
        y3 = y(3)
        if ( y1 >= pi ) then
            do while ( y1 >= pi )
                y1 = y1 - 2 * pi
            end do
        elseif ( y1 < - pi ) then
            do while ( y1 < - pi )
                y1 = y1 + 2 * pi
            end do
        end if
        if ( y3 >= pi ) then
            do while ( y3 >= pi )
                y3 = y3 - 2 * pi
            end do
        elseif ( y3 < - pi ) then
            do while ( y3 < - pi )
                y3 = y3 + 2 * pi
            end do
        end if
        yl = y1 - eps 
        yu = y1 + eps
        if ( yl < 0 .and. yu + eps > 0 .and. y(2) > 0 ) then
            write(8,*) y3, y(4)
            write(9,*) y(3), y(4) ! PS with unbounded theta_2
        end if

        call rk4step( t, dt, y )

    end do

end program dubpen

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
            real(dp), dimension(4) :: n_1 
            real(dp), dimension(3) :: n_2
            real(dp) :: coeff, d 

            f(1) = y(2)
            f(3) = y(4)

            ! Numerator terms for f(2)
            n_1(1) = - g * ( 2 * m_1 + m_2 ) * sin( y(1) )
            n_1(2) = - m_2 * g * sin ( y(1) - 2 * y(3) )
            coeff = 2 * sin ( y(1) - y(3) )
            n_1(3) = - coeff * m_2 * y(4)**2 * l_2
            n_1(4) = - coeff * m_2 * y(2)**2 * l_1 * cos ( y(1) - y(3) )

            ! Numerator terms for f(4)
            n_2(1) = y(2)**2 * l_1 * ( m_1 + m_2 )
            n_2(2) = g * ( m_1 + m_2 ) * cos ( y(1) )
            n_2(3) = y(4)**2 * l_2 * m_2 * cos( y(1) - y(3) )

            ! Denominator term
            d = 2 * m_1 + m_2 - m_2 * cos( 2 * ( y(1) - y(3) ) )

            f(2) = sum(n_1) / ( l_1 * d )
            f(4) = 2 * sin( y(1) - y(3) ) * sum(n_2) / ( l_2 * d )

            k(1:n_eq) = h*f(1:n_eq)

        end function kv

end subroutine rk4step
