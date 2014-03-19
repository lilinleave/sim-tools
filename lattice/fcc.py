import pylab as pl

def create_FCC_configuration(FCCspacing, nCellsPerSide, periodic, coords, MiddleAtomId)
    FCCshifts = pl.array([[0.,0.,0.],[0.5*FCCspacing,0.5*FCCspacing,0.],\
                          [0.5*FCCspacing,0.,0.5*FCCspacing],\
                          [0.,0.5*FCCspacing,0.5*FCCspacing]],pl.double)
    MiddleAtomID = 0
    a = 0
    coords[a:a+3,:] = latVec + FCCshifts
    latVec = pl.empty(nCellsPerSide,pl.double)

nCellsPerside = [4,2,1]
N = 4*nCellsPerSide[0]*nCellsPerSide[1]*nCellsPerSide[2]+\
    2*nCellsPerSide[0]*nCellsPerSide[1]+\
    2*nCellsPerSide[0]*nCellsPerSide[2]+\
    2*nCellsPerSide[1]*nCellsPerSide[2]+\
    nCellsPerSide[0]+nCellsPerSide[1]+nCellsPerSide[2]+1
