
module setupsch03sc

    use numtype
	implicit none

	integer, parameter :: n_eq = 2
	real(dp), parameter:: hbar = 1._dp, & 
	hbar2 = hbar**2, mass = 1._dp
    real(dp) :: xmax, dstep

end module setupsch03sc
