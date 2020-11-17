
subroutine rk4step(x,h,y)  ! 4-th order Runge-Kutta step
	
	use setup, only : dp, n_eq
	implicit none
	real(dp), intent(inout) :: x
	real(dp), intent(in) :: h
	real(dp), dimension(n_eq), intent(inout) :: y 
	real(dp), dimension(n_eq) :: k1, k2, k3, k4, dy
	
	k1 = kv (x, h, y)
	k2 = kv (x+h/2, h, y+k1/2)
	k3 = kv (x+h/2,	h, y+k2/2)
	k4 = kv (x+h, h, y+k3)	

	dy = (k1 + 2*k2 + 2*k3 + k4)/6		! increment
	
	x = x + h                     		! update
	y = y + dy

    contains
    
        function kv (t,dt,y) result(k)  ! derivative

	        use setup, only : dp, n_eq, pi
	        implicit none
	        real(dp), intent(in) :: t, dt
	        real(dp), dimension(n_eq), intent(in) :: y
			real(dp), dimension(n_eq) :: f, k

			real(dp), external :: rho
	        
            f(1) = y(2)                    

			f(2) = -4._dp * pi * t * rho(t)

			k = dt * f
				
		end function kv

end subroutine rk4step

function rho(r)

	use numtype
	implicit none
	real(dp) :: r, rho 

	! enter the given \rho 
	rho = cos(r) * exp(-r) / (2._dp * pi)

end function rho
