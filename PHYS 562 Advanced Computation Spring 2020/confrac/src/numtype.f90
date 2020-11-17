
module numtype
    save
    integer, parameter :: dp = selected_real_kind(15,307)
    real(dp), parameter :: pi = 4*atan(1._dp)
 
end module numtype