
program hrmosc

    use numtype
    implicit none

    ! enter dimension 'ndim' of linear operator matrices 
    integer, parameter :: ndim = 20, lwork = 2 * ndim - 1 
    integer, external :: delta
    integer :: i, j, k, info, ipiv(ndim)
    real(dp), parameter :: hbar = 1._dp, m = 1._dp, & 
        w = 1._dp, Mass = 2._dp, & 
        Omega = sqrt(2._dp), eps = 1e-10_dp 
    real(dp), external :: det
    real(dp) :: eval(ndim), rwork(3 * ndim - 2), re, &
        im, sum
    complex(dp), dimension(ndim,ndim) :: a, adag, &
        X, P, H, evec, orth, iden, spec, Hspec, &
        idenspec, evecdag, E
    complex(dp) :: work(lwork), D(ndim), determinant(2)

    print *, '------------------------'

    ! consider lowering operator 'a' in a finite subset
    ! of the basis
    forall(i = 0 : ndim - 1, j = 0 : ndim - 1) &
        & a(i+1,j+1) = sqrt(1._dp * j) * delta(i, j-1)

    print *, 'lowering operator ''a'' ='
    do k = 1, ndim
        ! 'print' format must start with minimum 2*ndim
        print '(40f10.4)', a(k, 1:ndim)
    end do

    print *, '------------------------'

    ! determine raising operator 'adag'
    adag = transpose(conjg(a))

    print *, 'raising operator ''adag'' ='
    do k = 1, ndim
        print '(40f10.4)', adag(k, 1:ndim)
    end do

    print *, '------------------------'

    ! determine position operator 'X'
    X = sqrt( hbar / (2 * m * w) ) * (a + adag)

    print *, 'position operator ''X'' ='
    do k = 1, ndim
        print '(40f10.4)', X(k, 1:ndim)
    end do

    print *, '------------------------'

    ! determine momentum operator 'P'
    P = zi * sqrt(m * hbar * w / 2) * (adag - a)

    print *, 'momentum operator ''P'' ='
    do k = 1, ndim
        print '(40f10.4)', P(k, 1:ndim)
    end do

    print *, '------------------------'

    ! determine Hamiltonian operator 'H'
    H = matmul(P, P) / (2 * Mass) &
        & + 1._dp / 2 * Mass * Omega**2 &
        & * matmul(X, X)

    print *, 'Hamiltonian operator ''H'' ='
    do k = 1, ndim
        print '(40f10.4)', H(k, 1:ndim)
    end do

    print *, '------------------------'

    ! determine the eigenvalues and eigenvectors of 
    ! Hamiltonian operator 'H'
    info = 0 
    evec = H

    call zheev('v', 'u', ndim, evec, ndim, eval, &
        work, lwork, rwork, info)
        if(info /= 0) stop ' zheev info /= 0'

    print *, 'eigenvalue array ''eval'' ='
    do i= 1, ndim
        print '(f10.4)', eval(i)
    end do

    print *, '------------------------'

    print *, 'the eigenvectors are the columns of ''evec'' ='
    do i= 1, ndim
        print '(40f10.4)', evec(1:ndim,i)
    end do

    print *, 'evec(1:ndim,i) is the corresponding', &
        ' eigenvector to the eigenvalue eval(i)'

    print *, '------------------------'

    ! show that the eigenvectors are orthogonal
    print *, 'the eigenvectors are orthogonal if the', &
        ' following is the identity matrix:'

    orth = matmul(conjg( transpose(evec) ), evec)

    do i= 1, ndim
        print '(40f10.4)', orth(1:ndim, i)
    end do

        ! print whether or not any element of 
        ! 'orth' matrix differs from identity 
        ! matrix 'iden' by less than parameter 'eps' 
    do i = 1, ndim 
        do j = 1, ndim
            iden(i, j) = delta(i, j)
        end do
    end do

    do i = 1, ndim
        do j = 1, ndim
            re = realpart(orth(i,j)) - realpart(iden(i,j))
            im = imagpart(orth(i,j)) - imagpart(iden(i,j))
            sum = abs(re) + abs(im)

            if ( sum > eps) then
                print *, 'the eigenvectors are not orthogonal'
                go to 10
            end if
        end do
    end do
    10 if (i == ndim + 1) then
        print *, 'the eigenvectors are orthogonal'
    end if

    print *, '------------------------'

    ! build up the spectral representation of the unit
    ! matrix 'idenspec' as the sum of the outer products 
    ! of the eigenvectors of the Hamiltonian operator 'H'

    forall(i = 1:ndim, j = 1:ndim) &
        & spec(i, j) = evec(i, j) * eval(j) 

    evecdag = transpose(conjg(evec))

    do k=1,ndim
        do i=1,ndim
            do j=1,ndim
                idenspec(i,j) = idenspec(i,j) + &
                    & evecdag(i,k) * evec(k,j)
            end do
        end do
    end do

    print *, 'spectral representation of the unit matrix', &
        ' ''idenspec'' as the sum of the outer products of', & 
        ' the eigenvectors of Hamiltonian operator ''H'''
    do i= 1, ndim
        print '(40f10.4)', idenspec(1:ndim, i)
    end do

    do i = 1, ndim
        do j = 1, ndim
            re = realpart(idenspec(i,j)) - realpart(iden(i,j))
            im = imagpart(idenspec(i,j)) - imagpart(iden(i,j))
            sum = abs(re) + abs(im)

            if ( sum > eps) then
                print *, '''idenspec'' is not', &
                    ' the identity matrix'
                go to 30
            end if
        end do
    end do
    30 if (i == ndim + 1) then
        print *, '''idenspec'' is the identity matrix'
    end if

    print *, '------------------------'

    ! spectral representation 'Hspec' of the Hamiltonian 
    ! operator matrix 'H'
    
    Hspec = matmul(spec, conjg(transpose(evec)))

    print *, 'spectral representation ''Hspec'' of the', &
        ' Hamiltonian operator matrix ''H'''
    do i= 1, ndim
        print '(40f10.4)', Hspec(1:ndim, i)
    end do

    do i = 1, ndim
        do j = 1, ndim
            re = realpart(Hspec(i,j)) - realpart(H(i,j))
            im = imagpart(Hspec(i,j)) - imagpart(H(i,j))
            sum = abs(re) + abs(im)

            if ( sum > eps) then
                print *, 'H /= matmul(spec,', &
                    ' conjg(transpose(evec)))'
                go to 300
            end if
        end do
    end do
    300 if (i == ndim + 1) then
        print *, 'H = matmul(spec, conjg(transpose(evec)))'
    end if

    print *, '------------------------'

    ! construct the function D(E) = |E − H| and show that the 
    ! eigenvalues coincide with the zeros of the determinant
    do i = 1, ndim
        E = eval(i)*iden-H 
        call zgbdi(E, ndim, ndim, ndim-1, ndim-1, ipiv, determinant)
        D(i) = determinant(1)
        print '(a2, f10.4, a14, 2f11.4)', 'D(', eval(i), &
            ') = |E - H| = ', determinant(1)
    end do

    do i = 1, ndim
        sum = abs(realpart(D(i)))+abs(imagpart(D(i)))
        if ( sum > eps) then
            print *, 'the eigenvalues do not coincide', &
                ' with the zeros of the determinant'
            go to 90
        end if
    
    end do
    90 if (i == ndim + 1) then
        print *, 'the eigenvalues coincide with the zeros', &
        ' of the determinant'
    end if

    print *, '------------------------'

end program hrmosc

function delta(x,y)
    ! function 'delta' outputs the result 
    ! of the Dirac delta function of the inputs

    use numtype
    implicit none
    integer :: x, y, delta

    if (x == y) then
        delta = 1
    else
        delta = 0
    end if

end function delta 

function det(E, H, ndim, eps)
    ! The function 'det' calculates the determinant
    ! of E - H by multiplying the diagonals of 
    ! its LU factorization by -1**s, where s is
    ! the number of row intercganhes

    use numtype
    implicit none
    integer, external :: delta
    integer :: ndim, i, j, ipiv(ndim), info, s, ii
    real(dp) :: E, mag, eps, det, diag(ndim), diagi(ndim)
    complex(dp), dimension(ndim, ndim) :: H, mat, iden

    ! create identity matrix 'iden' for 
    ! 'E' = eigenvalue * 'iden'
    do i = 1, ndim 
        do j = 1, ndim
            iden(i, j) = delta(i, j)
        end do
    end do

    mat = E * iden - H 

    ! for 'H' diagonal, skip zgetrf
    do i = 1, ndim
        do j = 1, ndim
            mag = abs(realpart(mat(i, j))) + & 
                & abs(imagpart(mat(i, j)))

            if ( i /= j .and. mag > eps) then
                go to 100
            end if
        end do
    end do
    if (i == ndim + 1) then
        go to 110
    end if

    ! for 'H' not diagonal
    100 call zgetrf(ndim, ndim, mat, ndim, ipiv, info)
        ! if(info /= 0) stop ' zgetrf info /= 0'
        ! this can be ignored since we will not solve
        ! linear equations with this -- thusly no dividing
        ! by 0

    do i = 1, ndim
        mag = abs(realpart(mat(i, i))) + & 
                & abs(imagpart(mat(i, i)))
        if (mag < eps) then
            det = 0._dp
            return
        end if
    end do

    ! calculate magnitude of determinant 'D' of 'mat'
    110 det = 1._dp

        ! start by putting diagonal elements in order from 
        ! least to greatest so that the numbers don't blow up
        ! as ndim gets arbitrarily large
    do i = 1, ndim
        diag(i) = realpart(mat(i, i))
    end do

    diagi = diag
    do i = 1, ndim                         
        ii = minloc(diagi,dim=1)
        diag(i) = diagi(ii)
        diagi(ii) = huge(0._dp)
    end do

    do i = 1, ndim
        det = det * diag(i)
    end do

    ! sign of determinant
    s = 0
    do i = 1, ndim
        if (ipiv(i) == i) then
            s = s + 0
        else
            s = s + 1
        end if
    end do

    det = (-1._dp)**s * det

end function det

subroutine zgbdi(abd, lda, n, ml, mu, ipvt, det)
    ! the LINPACK subroutine zgbdi outputs the 
    ! complex determinant det(1) of a complex 
    ! matrix abd

    use numtype
    implicit none
    complex(dp) :: abd(lda, n), det(2)
    integer :: lda, n, ml, mu, ipvt(n)

    integer :: i, m
    complex(dp) :: zdum, zdumr, zdumi
    real(dp) :: ten, cabs1, dreal, dimag

      dreal(zdumr) = zdumr
      dimag(zdumi) = (0.0d0,-1.0d0)*zdumi
      cabs1(zdum) = dabs(dreal(zdum)) + dabs(dimag(zdum))
      m = ml + mu + 1
      det(1) = (1.0d0,0.0d0)
      det(2) = (0.0d0,0.0d0)
      ten = 10.0d0
      do 50 i = 1, n
         if (ipvt(i) .ne. i) det(1) = -det(1)
         det(1) = abd(m,i)*det(1)
         exit
         if (cabs1(det(1)) .eq. 0.0d0) go to 60
   10    if (cabs1(det(1)) .ge. 1.0d0) go to 20
            det(1) = dcmplx(ten,0.0d0)*det(1)
            det(2) = det(2) - (1.0d0,0.0d0)
         go to 10
   20    continue
   30    if (cabs1(det(1)) .lt. ten) go to 40
            det(1) = det(1)/dcmplx(ten,0.0d0)
            det(2) = det(2) + (1.0d0,0.0d0)
         go to 30
   40    continue
   50 continue
   60 continue
      return

end subroutine
