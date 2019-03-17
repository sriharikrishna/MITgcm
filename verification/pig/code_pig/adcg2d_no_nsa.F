C $Header: /u/gcmpack/MITgcm_contrib/dgoldberg/depth_control_no_nsa/code/adcg2d_no_nsa.F,v 1.1 2017/12/07 23:21:12 dgoldberg Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"


CBOP
C     !ROUTINE: ADCG2D_PASSMAT
C     !INTERFACE:
      SUBROUTINE ADCG2D_NO_NSA(
     I                cg2d_b_v, ! Right hand side, forward
     U                cg2d_b_d, ! Right hand side, adjoint (output)
     I                cg2d_x_v, ! scaled eta,      forward 
     U                cg2d_x_d, ! scaled eta,      adjoint (input)
     U                ac2d_v, ac2d_d, as2d_v, as2d_d, aw2d_v, aw2d_d,
     O                firstResidual, 
     O                minResidualSq, 
     O                lastResidual,
     U                numIters, 
     U                nIterMin,
     I                myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CG2D_PASSMAT
C     | o Two-dimensional grid problem conjugate-gradient
C     |   inverter (with preconditioner).
C     *==========================================================*
C     | Con. grad is an iterative procedure for solving Ax = b.
C     | It requires the A be symmetric.
C     | This implementation assumes A is a five-diagonal
C     | matrix of the form that arises in the discrete
C     | representation of the del^2 operator in a
C     | two-dimensional space.
C     | Notes:
C     | ======
C     | This implementation can support shared-memory
C     | multi-threaded execution. In order to do this COMMON
C     | blocks are used for many of the arrays - even ones that
C     | are only used for intermedaite results. This design is
C     | OK if you want to all the threads to collaborate on
C     | solving the same problem. On the other hand if you want
C     | the threads to solve several different problems
C     | concurrently this implementation will not work.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CG2D.h"

c      double precision ac2d_ad(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
c      double precision as2d_ad(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
c      double precision aw2d_ad(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
c      common /cg2d_i_r_ad/ aw2d_ad, as2d_ad, ac2d_ad

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     cg2d_b    :: The source term or "right hand side" (output: normalised RHS)
C     cg2d_x    :: The solution (input: first guess)
C     firstResidual :: the initial residual before any iterations
C     minResidualSq :: the lowest residual reached (squared)
C     lastResidual  :: the actual residual reached
C     numIters  :: Inp: the maximum number of iterations allowed
C                  Out: the actual number of iterations used
C     nIterMin  :: Inp: decide to store (if >=0) or not (if <0) lowest res. sol.
C                  Out: iteration number corresponding to lowest residual
C     myThid    :: Thread on which I am working.
      _RL  cg2d_b_v(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cg2d_x_v(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
       _RS  aW2d_v  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
       _RS  aS2d_v  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
       _RS  aC2d_v  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cg2d_b_d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cg2d_x_d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
       _RS  aW2d_d  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
       _RS  aS2d_d  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
       _RS  aC2d_d  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)   
      _RL xtemp_v  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL btemp_d  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  firstResidual
      _RL  minResidualSq
      _RL  lastResidual
      _RL  tmp_tolerance
      INTEGER numIters
      INTEGER nIterMin
      INTEGER myThid
      

C     !LOCAL VARIABLES:
C     === Local variables ====
C     bi, bj     :: tile index in X and Y.
C     i, j, it2d :: Loop counters ( it2d counts CG iterations )
C     actualIts  :: actual CG iteration number
C     err_sq     :: Measure of the square of the residual of Ax - b.
C     eta_qrN    :: Used in computing search directions; suffix N and NM1
C     eta_qrNM1     denote current and previous iterations respectively.
C     cgBeta     :: coeff used to update conjugate direction vector "s".
C     alpha      :: coeff used to update solution & residual
C     sumRHS     :: Sum of right-hand-side. Sometimes this is a useful
C                   debugging/trouble shooting diagnostic. For neumann problems
C                   sumRHS needs to be ~0 or it converge at a non-zero residual.
C     cg2d_min   :: used to store solution corresponding to lowest residual.
C     msgBuf     :: Informational/error message buffer
      INTEGER bi, bj
      INTEGER i, j
      INTEGER numIterTemp, nIterMinTemp
      _RL    rhsMax
      _RL    rhsNorm
      _RL    recip_cg2dNorm
      
      recip_cg2dNorm = 1.0
      if (cg2dNorm .ne. 0. _d 0) then
       recip_cg2dNorm = 1./cg2dNorm
      endif
      
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          btemp_d (i,j,bi,bj) =
     &     cg2d_b_d (i,j,bi,bj)
          xtemp_v (i,j,bi,bj) =
     &     cg2d_x_v (i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      
      numIterTemp = numIters
      nIterMinTemp = nIterMin
      
#ifndef ALLOW_OPENAD
      CALL CG2D_NO_NSA(
     U                cg2d_b_v, cg2d_x_v,
     O                firstResidual, minResidualSq, lastResidual,
     U                numIterTemp, nIterMinTemp,
     I                myThid )
#endif

      _EXCH_XY_RL( cg2d_x_d, myThid )
      numIterTemp = numIters
      nIterMinTemp = nIterMin

      tmp_tolerance = cg2dtolerance
      cg2dtolerance = 1. _d -15

      
      CALL CG2D_NO_NSA(
     U                cg2d_x_d, cg2d_b_d, ac2d_v, as2d_v, aw2d_v,
     O                firstResidual, minResidualSq, lastResidual,
     U                numIterTemp, nIterMinTemp,
     I                myThid )

      cg2dtolerance = tmp_tolerance
      
      _EXCH_XY_RL( cg2d_b_d, myThid )
      _EXCH_XY_RL( cg2d_x_v, myThid )
      
      if (.true.) then
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx

            ac2d_d(i,j,bi,bj) =
     &       ac2d_d(i,j,bi,bj) -
     &       cg2d_b_d(i,j,bi,bj) *
     &       cg2d_x_v(i,j,bi,bj) * 
     &       recip_cg2dNorm 
     
            aw2d_d(i,j,bi,bj) =
     &       aw2d_d(i,j,bi,bj) -
     &       cg2d_b_d(i,j,bi,bj) *
     &       cg2d_x_v(i-1,j,bi,bj) * 
     &       recip_cg2dNorm 
     
            aw2d_d(i+1,j,bi,bj) =
     &       aw2d_d(i+1,j,bi,bj) -
     &       cg2d_b_d(i,j,bi,bj) *
     &       cg2d_x_v(i+1,j,bi,bj) * 
     &       recip_cg2dNorm 
            
            as2d_d(i,j,bi,bj) =
     &       as2d_d(i,j,bi,bj) -
     &       cg2d_b_d(i,j,bi,bj) *
     &       cg2d_x_v(i,j-1,bi,bj) * 
     &       recip_cg2dNorm 
      
            as2d_d(i,j+1,bi,bj) =
     &       as2d_d(i,j+1,bi,bj) -
     &       cg2d_b_d(i,j,bi,bj) *
     &       cg2d_x_v(i,j+1,bi,bj) * 
     &       recip_cg2dNorm 
            
         enddo
        enddo
       enddo
      enddo
      endif
      
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          if (i.lt.1.or.i.gt.sNx.or.
     &        j.lt.1.or.j.gt.sNy) then
     
           cg2d_b_d (i,j,bi,bj) = 0.0           
           ac2d_d(i,j,bi,bj)=0.0
           as2d_d(i,j,bi,bj)=0.0
           aw2d_d(i,j,bi,bj)=0.0

          endif
          
          cg2d_b_d (i,j,bi,bj) =
     &     cg2d_b_d (i,j,bi,bj) +
     &     btemp_d (i,j,bi,bj)
          
          cg2d_x_d (i,j,bi,bj) = 0.
          
          cg2d_x_v (i,j,bi,bj) =
     &     xtemp_v (i,j,bi,bj)
     
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
