      SUBROUTINE DFLUX(FLUX,SOL,KSTEP,KINC,TIME,NOEL,NPT,COORDS,
     1 JLTYP,TEMP,PRESS,SNAME)
C
      INCLUDE 'ABA_PARAM.INC'
C
      DIMENSION FLUX(2), TIME(2), COORDS(3)
      CHARACTER*80 SNAME


      double precision q, eta, pi, r0, s, x, y, z
	  
	  double precision t, v, r, z0, zs, q1, q2, q3
	  
	  double precision x0
	  
	  q=2000
	  
	  eta=0.75
	  
	  r0=0.001
	  
	  s=0.004
	  
	  v=0.0117
	  
	  t=time(1)
	  
	  x=coords(1)
	  
	  y=coords(2)
	  
	  z=coords(3)
	  
	  pi=3.141593
	  
	  z0=0.0
	  
	  x0=0.0
	  
	  zs=v*t+z0
	  
	  r=((x-x0)**2+(z-zs)**2)**0.5
	  
	  q1=(q*eta)/(pi*s*r0**2)
	  
	  q2=1.0-(abs(y))/s
	  
	  q3=exp(1.0-(r/r0)**2)
	  
	  flux(1)=q1*q2*q3
	  
	  flux(2)=0.0


      RETURN
      END