program fcc_deform
  implicit none
  double precision, parameter :: FCCspacing       = 4.050d0
  integer,          parameter :: nCellsPerSide(3) = [/5,3,1/]
  integer,          parameter :: &
       N = 4*nCellsPerSide(1)*nCellsPerSide(2)*nCellsPerSide(3)+&
           2*nCellsPerSide(1)*nCellsPerSide(2)+&
           2*nCellsPerSide(1)*nCellsPerSide(3)+&
           2*nCellsPerSide(2)*nCellsPerSide(3)+&
           nCellsPerSide(1)+nCellsPerSide(2)+nCellsPerSide(3)+1
  integer,          parameter :: DIM = 3
  double precision            :: coords(N,DIM), forces(N,DIM), energy
  integer middleAtomID

  call create_FCC_configuration(FCCspacing, nCellsPerSide, .false., coords, MiddleAtomId)

!-------------------------------------------------------------------------------
!
! create_FCC_configuration subroutine
!
!  creates a cubic configuration of FCC atoms with lattice spacing `FCCspacing' and
!  `nCellsPerSide' cells along each direction.
!
!  With periodic==.true. this will result in a total number of atoms equal to
!  4*(nCellsPerSide)**3 + 6*(nCellsPerSide)**2 + 3*(nCellsPerSide) + 1
!
!  With periodic==.false. this will result in a total number of atoms equal to
!  4*(nCellsPerSide)**3
!
!  Returns the Id of the atom situated in the middle of the configuration
!  (this atom will have the most neighbors.)
!
!-------------------------------------------------------------------------------
subroutine create_FCC_configuration(FCCspacing, nCellsPerSide, periodic, coords, MiddleAtomId)
  implicit none

  !-- Transferred variables
  double precision, intent(in)  :: FCCspacing
  integer,          intent(in)  :: nCellsPerSide(3)
  logical,          intent(in)  :: periodic
  double precision, intent(out) :: coords(3,*)
  integer,          intent(out) :: MiddleAtomId
  !
  ! cluster setup variables
  !
  double precision FCCshifts(3,4)
  double precision latVec(3)
  integer a, i, j, k, m

  ! Create a cubic FCC cluster
  !
  FCCshifts(1,1) = 0.d0;           FCCshifts(2,1) = 0.d0;           FCCshifts(3,1) = 0.d0
  FCCshifts(1,2) = 0.5*FCCspacing; FCCshifts(2,2) = 0.5*FCCspacing; FCCshifts(3,2) = 0.d0
  FCCshifts(1,3) = 0.5*FCCspacing; FCCshifts(2,3) = 0.d0;           FCCshifts(3,3) = 0.5*FCCspacing
  FCCshifts(1,4) = 0.d0;           FCCshifts(2,4) = 0.5*FCCspacing; FCCshifts(3,4) = 0.5*FCCspacing

  MiddleAtomID = 1 ! Always put middle atom as #1
  a = 1            ! leave space for middle atom as atom #1
  do i=1,nCellsPerSide(1)
     latVec(1) = (i-1)*FCCspacing
     do j=1,nCellsPerSide(2)
        latVec(2) = (j-1)*FCCspacing
        do k=1,nCellsPerSide(3)
           latVec(3) = (k-1)*FCCspacing
           do m=1,4
              a = a+1
              coords(:,a) = latVec + FCCshifts(:,m)
              if ((i.eq.nCellsPerside(1)/2+1) .and. (j.eq.nCellsPerSide(2)/2+1) .and. &
                   (k.eq.nCellsPerSide(3)/2+1) .and. (m.eq.1)) then
                 coords(:,1) = latVec + FCCshifts(:,m) ! put middle atom as atom #1
                 a = a - 1
              endif
           enddo
        enddo
     enddo
  enddo

  if (.not. periodic) then
     ! Add in the remaining corner
     latVec(1) = nCellsPerSide(1)*FCCspacing
     latVec(2) = nCellsPerSide(2)*FCCspacing
     latVec(3) = nCellsPerSide(3)*FCCspacing
     a = a+1; coords(:,a) = latVec

     do j=1,nCellsPerSide(2)
        ! Add in the remaining edge
        latVec(1) = nCellsPerSide(1)*FCCspacing
        latVec(2) = (j-1)*FCCspacing
        latVec(3) = nCellsPerSide(3)*FCCspacing
        a = a+1; coords(:,a) = latVec
        do k=1,nCellsPerSide(3)
           ! Add in the remaining face
           ! pos-x face
           latVec(1) = nCellsPerSide(1)*FCCspacing
           latVec(2) = (j-1)*FCCspacing
           latVec(3) = (k-1)*FCCspacing
           a = a+1; coords(:,a) = latVec
           a = a+1; coords(:,a) = latVec + FCCshifts(:,4)
        enddo
     enddo

     do i=1,nCellsPerSide(1)
        ! Add in the remaining edge
        latVec(1) = (i-1)*FCCspacing
        latVec(2) = nCellsPerSide(2)*FCCspacing
        latVec(3) = nCellsPerSide(3)*FCCspacing
        a = a+1; coords(:,a) = latVec
        do k=1,nCellsPerSide(3)
           ! pos-y face
           latVec(1) = (i-1)*FCCspacing
           latVec(2) = nCellsPerSide(2)*FCCspacing
           latVec(3) = (k-1)*FCCspacing
           a = a+1; coords(:,a) = latVec
           a = a+1; coords(:,a) = latVec + FCCshifts(:,3)
        enddo
        do j=1,nCellsPerSide(2)
           ! pos-z face
           latVec(1) = (i-1)*FCCspacing
           latVec(2) = (j-1)*FCCspacing
           latVec(3) = nCellsPerSide(3)*FCCspacing
           a = a+1; coords(:,a) = latVec
           a = a+1; coords(:,a) = latVec + FCCshifts(:,2)
        enddo
     enddo
     ! Add in the remaining edge
     do k=1,nCellsPerSide(3)
        latVec(1) = nCellsPerSide(1)*FCCspacing
        latVec(2) = nCellsPerSide(2)*FCCspacing
        latVec(3) = (k-1)*FCCspacing
        a = a+1; coords(:,a) = latVec
     enddo
  endif

  return

end subroutine create_FCC_configuration
