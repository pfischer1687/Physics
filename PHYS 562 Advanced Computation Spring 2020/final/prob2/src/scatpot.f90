
module setupscatpot

	use numtype
	implicit none

	integer, parameter :: npmax = 50, npar = 2
	real(dp) :: xx(1:npmax), yy(1:npmax)
	integer :: icall, nsp, iprint, nspmin, nspmax
	
end module setupscatpot

program scatpot

    use setupscatpot
    implicit none
    real(dp), external :: chi2
    integer :: i, stat, itmin, itmax
    real(dp) :: xstart(1:npar), fstart, stepi, epsf

    integer :: pr
    pr = 0

    open(unit=2,file='scat.data')
    i = 1
    do
        read(unit=2,fmt='(f5.1,f11.8)', iostat=stat ) xx(i), yy(i)
        if ( stat /= 0 ) exit
        ! print '(i5,2x,f6.2,f10.3)',i,xx(i),yy(i)
        i = i + 1
    end do
    nsp = i-1
    close(2)
    nspmin = 1
    nspmax = nsp

    ! xstart is defined as (v0, x0)
    !  xstart(1:npar)  = (/ 0.2_dp, -0.1_dp /)
    xstart(1:npar) = (/ 2.7846_dp, 1.4162_dp /)
    icall = 0

    iprint = 7
    fstart = chi2(xstart)

    stepi = 0.05_dp
    epsf = 0.001_dp

    itmin = 20
    itmax = 200

    iprint = 0
    
    call downhill(npar,chi2,xstart,fstart,stepi,epsf,itmin,itmax)
 
    iprint = 17
    fstart = chi2(xstart)

    print *, 'v0, x0, chi2'
    print *, xstart(1:npar), fstart

    ! print wavefunction and potential graphs
    do pr = 1, nspmax, 10
        call sch03sc(xx(pr), yy(pr), xstart(1), xstart(2), pr)
    end do
    
end program scatpot

function chi2(par) result(s2)

    use setupscatpot
    implicit none
    real(dp) :: par(npar), x, v0, x0, s2, fi 
    integer :: i 

    real(dp) :: rr

    icall = icall + 1

    v0 = par(1); x0 = par(2)

    s2 = 0
    do i = nspmin, nspmax
        
        x = xx(i) ! x is defined as E
        call sch03sc(x, rr, v0, x0, 0)
        fi = rr 
        s2 = s2 + (yy(i) - fi)**2 / sqrt(yy(i) + 2._dp)

        if ( iprint /= 0 ) then
            ! plot scat.data R(E) vs. E
            write(unit=iprint, fmt='(3f15.5 )' ) x, 0._dp, yy(i)
            ! plot fit rr vs. x
            write(unit=iprint + 1, fmt='(3f15.5 )' ) x, 0._dp, fi 
        end if
    end do 
    s2 = s2 / abs(nspmax - nspmin)

    print '(i5,2x, 8f12.4 )', icall, par(1:npar), s2

end function chi2
