      SUBROUTINE DFLUX(FLUX,SOL,KSTEP,KINC,TIME,NOEL,NPT,COORDS,
     1 JLTYP,TEMP,PRESS,SNAME)
C
      INCLUDE 'ABA_PARAM.INC'
C
      DIMENSION FLUX(2), TIME(2), COORDS(3)
      CHARACTER*80 SNAME


      double precision Q, ETA, R0, S, X, Y, Z
	  
	  double precision T, V, R, Z0, Zs, Q1, Q2, ry
	  
	  double precision X0
	  
	  Q = 2000
	  
	  ETA = 0.75
	  
	  R0 = 0.001
	  
	  S = 0.002
	  
	  V = 0.02
	  
	  T = TIME(1)
	  
	  X = COORDS(1)
	  
	  Y = COORDS(2)
	  
	  Z = COORDS(3)
	  
	  Z0 = 0.0
	  
	  X0 = 0.0
	  
	  Zs = V*T + Z0
	  
	  R =((X-X0)**2+(Z-Zs)**2)**0.5
	  
	  Q1 = (2*Q*ETA)/(S*(R0**2))
	  
	  ry = R0*(((Y+S)/S)**0.5)
	  
	  Q2 = exp(-3*((R/ry)**2))
	  
	  FLUX(1) = Q1*Q2
	  
	  FLUX(2) = 0.0


      RETURN
      END