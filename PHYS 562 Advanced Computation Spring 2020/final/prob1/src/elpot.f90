! program elpot outputs the total charge Q
! and plots the electric potential \Phi and field E
! for a given charge distribution \rho

! enter the given \rho in 
! function rhointegrand in elpot.f90 and
! function rho in rk4step.f90

program elpot

    use setup
    use chebyshev
    implicit none
  
    real(dp), external :: diff
    real(dp) :: ya, yb, phiprime0, yx
    integer :: nch
    
    integer :: itype, npnts, ifail, n
    real(dp) :: aa, bb, cc, dd, res
    integer, parameter :: maxint = 300
    real(dp) :: weight(maxint), abscis(maxint)
    real(dp), external :: rhointegrand

    ! integrate \rho to find total charge Q (Gauss-Legendre)
    itype = 0
    aa = eps
    bb = 15._dp
    cc = 0._dp; dd = 0._dp
    npnts = 100

    call d01bcf(itype,aa,bb,cc,dd,npnts,weight, &
        abscis,ifail)

    if (ifail /= 0) stop 'ifail /= 0'
    res = 0
    do n = 1, npnts
        res = res + weight(n) * rhointegrand(abscis(n))
    end do
    print *, 'for r \in [eps, 15], total charge Q =', res 

    ! plot \Phi(r) and E(r) for r \in [eps, 15]
    nch = 10
    ya = eps
    yb = 15._dp 
    iw = 0
    
    phif = res    ! \Phi goes like Q/r so \phi goes like Q

    call chebyex(diff, nch, cheb, ya, yb)
    call chebyzero(nch, cheb, ya, yb, z0, iz0)

    ! print phiprime0 such that boundary values are met
    do iw = 1, iz0 
        phiprime0 = z0(iw) 
        yx = diff(phiprime0)
        print *, 'phiprime0 =', phiprime0, 'yx=', yx 
    end do

end program elpot

function diff(phiprime0) 

    use chebyshev, only : iz0
    use setup
    implicit none
    real(dp) :: r, dr, y(n_eq), diff, phiprime0, E

    r = eps
    dr = 0.0001_dp 
    y(1) = phi0
    y(2) = phiprime0

    do while (r <= 15) 
        if (iw /= 0) then

            ! plot \phi vs. r for r \in [eps, 15]
            write(iw, *) r, 0._dp, y(1)

            if (r > 0.1_dp)then    ! avoid singular behavior
                ! plot \Phi vs. r for r \in (0.1, 15]
                write(iw+iz0, *) r, 0._dp, y(1) / r
                ! plot E vs. r for r \in (0.1, 15]
                E = ( y(1) - r * y(2) ) / r**2
                write(iw+iz0+1, *) r, 0._dp, E
            end if
     
        end if 
        call rk4step(r, dr, y)
    end do

    diff = phif - y(1) 

end function diff

function rhointegrand(r)

    use numtype
    implicit none
    real(dp) :: r, rho, rhointegrand

    ! enter the given \rho
    rho = cos(r) * exp(-r) / (2._dp*pi)

    rhointegrand = rho * 4 * pi * r**2

end function rhointegrand
