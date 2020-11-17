
subroutine rk4step(x,h,y, energy, v0, x0)  ! 4-th order Runge-Kutta step
	
	use setupsch03sc, only : dp, n_eq
	implicit none
	real(dp), intent(inout) :: x
	real(dp), intent(in) :: h
	complex(dp), dimension(n_eq), intent(inout) :: y 
	complex(dp), dimension(n_eq) :: k1, k2, k3, k4, dy

	real(dp), intent(in) :: energy, x0, v0
	
	k1 = kv (x, h, y)
	k2 = kv (x+h/2, h, y+k1/2)
	k3 = kv (x+h/2,	h, y+k2/2)
	k4 = kv (x+h, h, y+k3)	

	dy = (k1 + 2*k2 + 2*k3 + k4)/6		! increment
	
	x = x + h                     		! update
	y = y + dy

    contains
    
        function kv (x, dx, y) result(k)  ! derivative

	        use setupsch03sc
	        implicit none
	        real(dp), intent(in) :: x, dx
	        complex(dp), dimension(n_eq), intent(in) :: y
	        complex(dp), dimension(n_eq) :: f, k
			
			f(1) = y(2) 
			
			f(2) = - 2 * mass / hbar2 * ( energy - &
				& potential(x) ) * y(1)
		
			k = dx * f 
	
		end function kv
		
		function potential(x)

			use numtype
			implicit none 
			real(dp) :: x, potential 

			potential = v0 / 2._dp * &
				& ( 1 + tanh(x / x0) )

		end function potential
	
end subroutine rk4step
