program test
  implicit none

  external func
  integer, parameter          :: N = 3
  integer, parameter          :: ldfjac = N
  double precision, parameter :: tol = 1.D-8
  integer                     :: info
  double precision            :: x(N), fvec(N), fjac(ldfjac,N)

  x(1:N) = (/1., 1., 1./)
  call hybrj1 ( func, N, x, fvec, fjac, ldfjac, tol, info )
  call r8vec_print ( N, x, '  X:' )
  call r8vec_print ( N, fvec, '  F(X):' )

  stop
end program test

subroutine func(N,x,fvec,fjac,ldfjac,iflag)
  use coeff
  implicit none

  integer,          intent(in)  :: N, ldfjac, iflag
  double precision, intent(in)  :: x(N)
  double precision, intent(out) :: fvec(N), fjac(ldfjac,N)

  if (iflag == 1) then
     fvec(1) = (5.*x(2) + 3.)*a
     fvec(2) = 4.*x(1)*x(1) - 2.*sin(x(2)*x(3))
     fvec(3) = x(2)*x(3) - 1.5
  else if (iflag == 2) then
     fjac(1,1) = 0.
     fjac(1,2) = 10.
     fjac(1,3) = 0.
     fjac(2,1) = 8.*x(1)
     fjac(2,2) = -2.*x(3)*cos(x(2)*x(3))
     fjac(2,3) = -2.*x(2)*cos(x(2)*x(3))
     fjac(3,1) = 0.
     fjac(3,2) = x(3)
     fjac(3,3) = x(2)
  endif
  return
end subroutine func
