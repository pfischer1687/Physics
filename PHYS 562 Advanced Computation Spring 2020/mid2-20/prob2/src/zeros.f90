
module setup

    use numtype
    implicit none
    integer, parameter :: nv = 1, lda = 1, nrhs = 1
    real(dp) :: x(lda), f(lda), deriv(lda, lda)

end module setup

program zeros

    use setup
    use chebyshev
    implicit none

    ! Newton-Raphson
    real(dp) :: dx(lda, nrhs), ff, res
    integer :: info, i, ipiv(lda), maxstep, j
    real(dp), parameter :: eps = 1.e-15_dp

    ! Chebyshev
    integer :: n, np, maxf
    real(dp) :: ya, yb, dxx, xx, ress, xxx(maxch)
    real(dp), external :: funcc, funccinv


    print *, '------------------'

    ! use Newton-Raphson to locate the zeros of 'func' for x \in [0, 5]
    print *, 'zeros for x \in [0, 5]:'

    do j = 0, 5

        x(1:nv) = (/ j - 0.1_dp /)

        maxstep = 50
        print '((9x,a),4x,2(9x,a))','x', 'f_1', '|f|'

        do i = 1, maxstep

            call func(ff)

            ! print '(f10.4,4x,2e12.3)',x(1:nv),f(1:nv),ff
            if (ff <= eps) exit

            dx(1:nv,1) = -f(1:nv)

            info = 0
            call dgesv(nv, nrhs, deriv, lda, ipiv, dx, lda, info)
            if (info /= 0) stop 'info /= 0'

            x(1:nv) = x(1:nv) + dx(1:nv,1)

        end do

        print '(f10.4,4x,2e12.3)',x(1:nv),f(1:nv),ff

    end do

    print *, '------------------'

    ! locate the poles of 'func' for x \in [0, 5]
    print *, 'poles for x \in [0, 5]'

    do j = 1, 5

        x(1:nv) = (/ j + 0.1_dp /)

        maxstep = 9492
        print '((9x,a),4x,(9x,a))','x', 'residue'

        do i = 1, maxstep

            call funcinv(ff)

            !print '(f10.4,4x,2e12.3)',x(1:nv),f(1:nv),ff
            if (ff <= eps) exit

            dx(1:nv,1) = -f(1:nv)

            info = 0
            call dgesv(nv, nrhs, deriv, lda, ipiv, dx, lda, info)
            if (info /= 0) stop 'info /= 0'

            x(1:nv) = x(1:nv) + dx(1:nv,1)

        end do

        ! residue
        res = pi * j**2

        print '(f10.4,8x,f12.3)',x(1:nv), res

    end do

    print *, '------------------'

    ! use Chebyshev to locate the zeros of 'funcc' for xx \in [0, 5]

    ya = 0
    yb = 5

    n = 9
    np = 50
    dxx = ( yb - ya ) / np

    call chebyex( funcc, n, cheb, ya, yb )

    call chebyzero( n, cheb, ya, yb, z0, iz0 )

    maxf = 10

    do i = 1, iz0

        xx = z0(i)
        call root_polish( funcc, xx, dxx, eps, maxf )
        if (xx < eps .or. xx > yb) cycle
        print *, "f(x) = 0", i, xx, funcc(xx) 

    end do

    print *, '------------------'

    ! use Chebyshev to locate the poles of 'func' for x \in [0, 5]

    ya = 0
    yb = 5

    n = 9
    np = 50
    dxx = ( yb - ya ) / np

    call chebyex( funccinv, n, cheb, ya, yb )

    call chebyzero( n, cheb, ya, yb, z0, iz0 )

    maxf = 10

    j = 0

    do i = 1, iz0

        xx = z0(i)
        call root_polish( funccinv, xx, dxx, eps, maxf )

        xxx(i) = xx
        if (i > 1) then
            if (abs(xx - xxx(i-1)) < eps) then
                j = j + 1
            cycle
            end if
        end if

        if (xx < eps .or. xx > yb) cycle

        ! residua
        call chebyex( funccinv, 1, cheb, xx-eps, xx+eps )

        ress = eps / cheb(1) 

        print *, "1/f(x) = 0", i-j, xx, funccinv(xx), ress

    end do

    print *, '------------------'

end program zeros

! Newton-Raphson
subroutine func(ff)

    use setup
    implicit none
    
    real(dp) :: ff

    f(1) = (x(1)*pi)**2 / tan( pi * x(1) )
    ff = norm2(f(1:nv))
    
    deriv(1, 1) = pi**2 * x(1) * (2 / tan( pi * x(1)) - pi * x(1) / sin( pi * x(1) )**2 )
   
end subroutine func

subroutine funcinv(ff)

    use setup
    implicit none
    
    real(dp) :: ff

    f(1) = tan( pi * x(1) ) / (x(1) * pi)**2
    ff = norm2(f(1:nv))
    
    deriv(1, 1) = pi * x(1) / cos( pi * x(1) )**2 - 2 * tan( pi * x(1) ) / (pi**2 * x(1)**3)
   
end subroutine funcinv

! Chebyshev
function funcc(x) result(y)

    use numtype
    implicit none
    real(dp) :: x, y

    y = (x * pi)**2 / tan(pi * x)

end function funcc

function funccinv(x) result(y)

    use numtype
    implicit none
    real(dp) :: x, y

    y = 1 / ( (x * pi)**2 / tan(pi * x) )

end function funccinv
