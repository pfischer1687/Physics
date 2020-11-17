
module numtype

    save
    integer, parameter :: dp = &
        & selected_real_kind(15,307)
    real(dp), parameter :: pi = 4*atan(1._dp)
    complex(dp), parameter :: z0 = (0._dp, 0._dp), &
        z1 = (1._dp, 0._dp), zi = (0._dp,1._dp)
    real(dp), parameter :: tiny = 1.e-30_dp

end module numtype
