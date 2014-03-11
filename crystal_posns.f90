program crystal_posns
implicit none

double precision A1(3),A2(3),A3(3),R(3)
integer NB,i,l1min,l1max,l2min,l2max,l3min,l3max,l1,l2,l3,lambda
double precision, allocatable :: Z(:,:)
character(len=80) filename

! Read input
write(6,'(a)')
write(6,'(a)') 'Program for generating crystal positions according to eqn (3.15) in MM'
write(6,'(a)')
write(6,'(a)') 'R(l1,l2,l3,lambda) = l1*A1 + l2*A2 + l3*A3 + Z(lambda)'
write(6,'(a)')
write(6,'(a)') 'INPUT'
write(6,'(a)')
write(6,'(a)', advance="no") 'A1() = '; read(5,*) A1(1:3)
write(6,'(a)', advance="no") 'A2() = '; read(5,*) A2(1:3)
write(6,'(a)', advance="no") 'A3() = '; read(5,*) A3(1:3)
write(6,'(a)')
write(6,'(a)', advance="no") 'NB = '; read(5,*) NB
if (NB<0) stop 'Error: Zero or negative number of basis atoms.'
write(6,'(a)') 'Note: Setting position of basis atom 0 to zero.'
allocate(Z(3,0:NB-1))
Z(:,0) = 0.d0
do i=1,NB-1
   write(6,'(a,i1,a)', advance="no") 'Z',i,'() = '; read(5,*) Z(1:3,i)
enddo
write(6,'(a)')
write(6,'(a)', advance="no") 'l1min, l1max = '; read(5,*) l1min,l1max
write(6,'(a)', advance="no") 'l2min, l2max = '; read(5,*) l2min,l2max
write(6,'(a)', advance="no") 'l3min, l3max = '; read(5,*) l3min,l3max
write(6,'(a)')
write(6,'(a)', advance="no") 'Output filename = '; read(5,*) filename
write(6,'(a)')
write(6,'(a)', advance="no") 'OUTPUT'
write(6,'(a)')
write(6,'(a)') 'The positions of the atoms will be sent to file '//trim(filename)
write(6,'(a)')
write(6,'(a)') 'For each atom, the distance of the atom from the origin and its'
write(6,'(a)') 'coordinates are output:'
write(6,'(a)')
write(6,'(a)') '|R| R(1) R(2) R(3)'
write(6,'(a)') '...'
write(6,'(a)')
write(6,'(a)') 'To obtain the atoms sorted by distance from the origin,'
write(6,'(a)') 'use the unix ''sort'' command:'
write(6,'(a)')
write(6,'(a)') '% sort ' // trim(filename)
write(6,'(a)')

! Open output file
open(10,file=trim(filename),status='unknown')

! Generate atom positions and print out
do l1 = l1min, l1max
   do l2 = l2min, l2max
      do l3 = l3min, l3max
         do lambda=0,NB-1
            R = l1*A1 + l2*A2 + l3*A3 + Z(:,lambda)
            write(10,'(f12.5,5x,3f12.5)') R(1:3)
         enddo
      enddo
   enddo
enddo

! Close output file
close(10)

! Free storage
deallocate(Z)

end program crystal_posns
