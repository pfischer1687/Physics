
module setup

	use numtype
	implicit none
	integer, parameter :: n_eq = 2
	real(dp), parameter :: eps = 1e-8_dp

	! theoretical boundary values for \phi
	real(dp), parameter:: phi0 = eps    ! \phi = r * \Phi, \Phi finite with distribution
	real(dp) :: phif    ! to be determined by total charge Q

	integer :: iw
	
end module setup
