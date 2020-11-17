
subroutine sch03sc(energy, rr, v0, x0, pr)

    use setupsch03sc
    implicit none
    real(dp) :: x, tt
    complex(dp) :: psi(n_eq), aa, bb, k1, k2

    real(dp), intent(in) :: energy
    real(dp), intent(out) :: rr 
    real(dp), intent(in) :: v0, x0

    integer, intent(in) :: pr

    xmax = 10._dp ! 20._dp
    dstep = 0.001_dp ! 0.001_dp
    x = xmax
   
    ! must add 0*iic to make zqrt argument complex(8)
    k2 = zsqrt(2._dp * mass / hbar2 * ( energy - potential(x) ) + 0._dp * iic)
    psi(1) = exp(iic * k2 * x)
    psi(2) = iic * k2 * psi(1)

    if (pr > 0) then
        do while (x > - xmax) 
            write(19+2*pr, *) x, 0._dp, realpart( psi(1) )
            write(20+2*pr, *) x, imagpart( psi(1) )
            write(21+2*pr, *) x, potential(x)
            write(22+2*pr, *) x, energy
            call rk4step(x, - dstep, psi, energy, v0, x0)
        end do
    end if
  

    x = - xmax 

    k1 = zsqrt(2._dp * mass / hbar2 * &
        & ( energy - potential(x) ) + 0._dp * iic)

    aa = ( psi(1) + psi(2) / (iic * k1) ) / &
        & ( 2._dp * exp(iic * k1 * x) )
    bb = ( psi(1) - psi(2) / (iic * k1) ) / &
        & ( 2._dp * exp( - iic * k1 * x) )
    
    rr = abs(bb / aa)**2
    tt = realpart(k2 / k1) * abs(1 / aa)**2

    ! print *, v0, energy, k2, k1
    ! print *, rr, tt, rr + tt 

    contains 

        function potential(z) result(pot)

            use numtype
            implicit none
            real(dp) :: z, pot
        
            pot = v0 / 2._dp * &
                & ( 1._dp + tanh(z / x0) )
        
        end function potential

end subroutine sch03sc
