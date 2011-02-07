************************************************************************
************************************************************************
***** 8 node solid shell element                                   *****
***** with 24 DOFs,                                                *****
***** 8 strain collocation points, and                             *****
***** 9 internal strain parameters                                 *****
***** (Klinkel et al.)                                             *****
************************************************************************
************************************************************************
*
      SUBROUTINE uel(rhs,amatrx,energy,ndofel,props,nprops,coords,nnod,
     #u,ndload,jdltyp,adlmag,predef,npredf)

*
*      INCLUDE 'ABA_PARAM.INC'
      IMPLICIT NONE
*
      INTEGER ndofel,nprops,nnod,ndload,jdltyp(ndload),npredf
      DOUBLE PRECISION rhs(ndofel),amatrx(ndofel,ndofel),props(nprops),
     #energy,coords(3,nnod),u(ndofel),adlmag(ndload),
     #predef(1,npredf,nnod)
*
************************************************************************
***** user defined declarations
************************************************************************
*
      INTEGER neas, nipa
      DOUBLE PRECISION gamma2
      PARAMETER (neas   = 9,  ! # of EAS parameter
     #           nipa   = 2,
     #           gamma2 = 0.5773502691896258D0) ! SQRT(1/3)
      INTEGER i, j, k, l, m, n, i1, i2, i3
      DOUBLE PRECISION volu_, area_, volu_o, area_o, Jdet
      DOUBLE PRECISION lame(2), pcoord(nprops)
      DOUBLE PRECISION xi(nipa)
      DOUBLE PRECISION mf3(nnod,nnod), mf4(nnod,nnod),mf5(nnod,nnod),
     #  pf1(nnod,nnod), pf2(nnod,nnod), pf6(nnod,nnod)
      DOUBLE PRECISION df(nnod,3)
      DOUBLE PRECISION dfa(nnod,3), dfb(nnod,3), dfc(nnod,3),
     #  dfd(nnod,3), dfe(nnod,3), dff(nnod,3), dfg(nnod,3),
     #  dfh(nnod,3) 
      DOUBLE PRECISION EVEC(6), SVEC(6), EVEC1(6), ET(6)
      DOUBLE PRECISION E4(2), E5(2)
      DOUBLE PRECISION E3(4)
      DOUBLE PRECISION MTS(neas), FEAS(neas), alph(neas)
      DOUBLE PRECISION BTS(ndofel), FINT(ndofel)
      DOUBLE PRECISION M0(6,neas), M1(6,neas)
      DOUBLE PRECISION eye(3,3)
      DOUBLE PRECISION rRef(3,nnod), rDef(3,nnod)
      DOUBLE PRECISION rRefa(3,3), rDefa(3,3), rRefb(3,3),
     #  rDefb(3,3), rRefc(3,3), rDefc(3,3), rRefd(3,3),
     #  rDefd(3,3), rRef3(3,4), rDef3(3,4)
      DOUBLE PRECISION gRef(3,3), gDef(3,3), gRef1(3,3), gDef1(3,3)
      DOUBLE PRECISION Spf(nnod,nnod), GTO(3,3), T(6,6)
      DOUBLE PRECISION oRef(3,3), oRef_(3,3)
      DOUBLE PRECISION CMAT(6,6)
      DOUBLE PRECISION BMAT(6,ndofel), CB(6,ndofel), BMATS(6,ndofel),
     #    BMAT1(6,ndofel), BMAT2(6,ndofel), BMAT3(6,ndofel)
*      DOUBLE PRECISION MTC(6,neas)
      DOUBLE PRECISION MTCM(neas,neas), KEAS(neas,neas), 
     #    UEAS(neas,neas)
      DOUBLE PRECISION MTCB(neas,ndofel), KCPL(neas,ndofel),
     #  K_KCPL(neas,ndofel), KGEO(ndofel,ndofel)
      DOUBLE PRECISION BTCB(ndofel,ndofel), KMAT(ndofel,ndofel)
      DOUBLE PRECISION tDef(3,3), gg_(3,3), gRef_(3,3), gDef_(3,3)
      DOUBLE PRECISION ENORM, DOT, DDOT,DIRVEC(3,4),DLDIR(3),
     # sfa(4),DLNOD(3,4),DLNODI(3,4),DLOAD,TEMPCP(4), ENGAU,infla,AREA
*
************************************************************************
***** initial temperature
************************************************************************
*     
      predef(1,1,1:8)=predef(1,1,1:8)-predef(1,2,1:8)  
*
************************************************************************
***** properties
************************************************************************
*
*     material parameters
      lame(2) = props(1)/(1.D0+props(2))
      lame(1) = lame(2)*props(2)/(1.D0-props(2)*2.D0)
      lame(2) = lame(2)/2.D0
*
************************************************************************
***** IP coordinates and weights
************************************************************************
*
      xi(1)    =  -gamma2
      xi(2)    =  gamma2
*
************************************************************************
***** initializations
************************************************************************
*
      KMAT = 0.D0
      KEAS = 0.D0
      KCPL = 0.D0
      FINT = 0.D0
      FEAS = 0.D0
      KGEO = 0.D0
      Spf  = 0.D0
      mf3  = 0.D0
      mf4  = 0.D0
      mf5  = 0.D0
      eye  = 0.D0
      eye(1,1) = 1.D0
      eye(2,2) = 1.D0
      eye(3,3) = 1.D0
*
************************************************************************
***** base vectors in the current/reference configuration
************************************************************************
*
      rRef = coords
*
*     nodal coordinates
      DO n = 1,nnod
        rDef(:,n) = rRef(:,n) + u((/-2,-1,0/)+3*n)
      ENDDO
*
*     base vectors at selected points
      CALL deriv_shapef(0.D0,0.D0,0.D0,df)
      CALL KAB(3,3,8,1.D0,rRef,df,oRef)
      CALL area_volume(oRef,area_o,volu_o)
      CALL co2contra(3,oRef,oRef_)
*
*
*     A(-1,0,0)
      CALL deriv_shapef(-1.D0,0.D0,0.D0,dfa)
      CALL KAB(3,3,8,1.D0,rRef,dfa,rRefa)
      CALL KAB(3,3,8,1.D0,rDef,dfa,rDefa)
*     B(0,-1,0)
      CALL deriv_shapef(0.D0,-1.D0,0.D0,dfb)
      CALL KAB(3,3,8,1.D0,rRef,dfb,rRefb)
      CALL KAB(3,3,8,1.D0,rDef,dfb,rDefb)
*     C(1,0,0)
      CALL deriv_shapef(1.D0,0.D0,0.D0,dfc)
      CALL KAB(3,3,8,1.D0,rRef,dfc,rRefc)
      CALL KAB(3,3,8,1.D0,rDef,dfc,rDefc)
*     D(0,1,0)
      CALL deriv_shapef(0.D0,1.D0,0.D0,dfd)
      CALL KAB(3,3,8,1.D0,rRef,dfd,rRefd)
      CALL KAB(3,3,8,1.D0,rDef,dfd,rDefd)
*
*     E(-1,-1,0)
      CALL deriv_shapef(-1.D0,-1.D0,0.D0,dfe)
      CALL KAX(3,8,rRef,dfe(:,3),rRef3(:,1))
      CALL KAX(3,8,rDef,dfe(:,3),rDef3(:,1))
*     F(1,-1,0)
      CALL deriv_shapef(1.D0,-1.D0,0.D0,dff)
      CALL KAX(3,8,rRef,dff(:,3),rRef3(:,2))
      CALL KAX(3,8,rDef,dff(:,3),rDef3(:,2))
*     G(1,1,0)
      CALL deriv_shapef(1.D0,1.D0,0.D0,dfg)
      CALL KAX(3,8,rRef,dfg(:,3),rRef3(:,3))
      CALL KAX(3,8,rDef,dfg(:,3),rDef3(:,3))
*     H(-1,1,0)
      CALL deriv_shapef(-1.D0,1.D0,0.D0,dfh)
      CALL KAX(3,8,rRef,dfh(:,3),rRef3(:,4))
      CALL KAX(3,8,rDef,dfh(:,3),rDef3(:,4))
*
*
************************************************************************
***** transverse shear and normasl strains at ANS colocation points
************************************************************************
*
*     temperature at at coolocation points
      TEMPCP(1)=(PREDEF(1,1,1)+PREDEF(1,1,4)+PREDEF(1,1,5)+
     # PREDEF(1,1,8))/4.D0
      TEMPCP(2)=(PREDEF(1,1,1)+PREDEF(1,1,2)+PREDEF(1,1,5)+
     # PREDEF(1,1,6))/4.D0
      TEMPCP(3)=(PREDEF(1,1,2)+PREDEF(1,1,3)+PREDEF(1,1,6)+
     # PREDEF(1,1,7))/4.D0
      TEMPCP(4)=(PREDEF(1,1,3)+PREDEF(1,1,4)+PREDEF(1,1,7)+
     # PREDEF(1,1,8))/4.D0
      
*     transverse shear strains at coolocation points A,C,D,B
      E4(1) = DOT(rDefa(:,2),rDefa(:,3))-DOT(rRefa(:,2),rRefa(:,3))*
     # (1+2*props(3)*TEMPCP(1))
      E4(2) = DOT(rDefc(:,2),rDefc(:,3))-DOT(rRefc(:,2),rRefc(:,3))*
     # (1+2*props(3)*TEMPCP(2))
      E5(1) = DOT(rDefb(:,1),rDefb(:,3))-DOT(rRefb(:,1),rRefb(:,3))*
     # (1+2*props(3)*TEMPCP(3))
      E5(2) = DOT(rDefd(:,1),rDefd(:,3))-DOT(rRefd(:,1),rRefd(:,3))*
     # (1+2*props(3)*TEMPCP(4))
*
*     temperature
      TEMPCP(1)=(PREDEF(1,1,1)+PREDEF(1,1,5))/2.D0
      TEMPCP(2)=(PREDEF(1,1,2)+PREDEF(1,1,6))/2.D0
      TEMPCP(3)=(PREDEF(1,1,3)+PREDEF(1,1,7))/2.D0
      TEMPCP(4)=(PREDEF(1,1,4)+PREDEF(1,1,8))/2.D0
*
*     normal strains at coolocation points (= nodes)
      DO n = 1,4
      E3(n) = ( DOT(rDef3(:,n),rDef3(:,n)) -
     #          DOT(rRef3(:,n),rRef3(:,n)) )/2.D0 -
     #          DOT(rRef3(:,n),rRef3(:,n))*
     #          TEMPCP(n)*props(3)
      ENDDO
*
************************************************************************
***** START OF LOOP: area integration 1
*
      DO i1 = 1,nipa
        DO i2 = 1,nipa
           DO i3 = 1,nipa
*
*     Enhanced Asumed Strain (EAS) matices
      CALL eas_matrix(xi(i1),xi(i2),xi(i3),M0,neas)
*
*     Interpolated base vectors
      CALL deriv_shapef(xi(i1),xi(i2),xi(i3),df)
      CALL KAB(3,3,8,1.D0,rRef,df,gRef) !#G = rRef*df
      CALL KAB(3,3,8,1.D0,rDef,df,gDef) !#g = rDef*df
*
*     Green Lagrange strains
      CALL KATA(3,3,1.D0,gRef,gRef1)
      CALL KATA(3,3,1.D0,gDef,gDef1)
      CALL SYM2VEC((gDef1-gRef1)/2.D0,EVEC)
*
*     temperature	 
      CALL KTEMP(props(3),xi(i1),xi(i2),xi(i3),PREDEF(1,1,1:8),gRef,ET)
      EVEC = EVEC-ET
*      
*     assumed natural strain (ANS)
      CALL nstrain(E3,xi(i1),xi(i2),EVEC(3))
      CALL sstrain(E4,xi(i1),EVEC(4))
      CALL sstrain(E5,xi(i2),EVEC(5))
*
*     transformation from base O onto the natural base
      CALL area_volume(gRef,area_,volu_)
      Jdet = volu_o/volu_
      CALL KATB(3,3,3,gRef,oRef_,GTO)
      CALL trafo_matrix(GTO,T)
      CALL KAB(6,neas,6,Jdet,T,M0,M1)
*
*     material matrix (6*6)
      CALL mat_matrix(gRef,lame,CMAT)
*
*     stress vector (PK2 stress resultants)
      CALL KSX(6,CMAT,EVEC,SVEC)
*
*     enhanced stiffnenss matrix
      CALL KATSA(6,neas,M1,CMAT,MTCM)
*      CALL KATB(neas,neas,6,M1,MTC,MTCM)
      KEAS = KEAS + volu_*MTCM
*
*     enhanced forces (* -1)
      CALL KATX(6,neas,M1,SVEC,MTS)
      FEAS = FEAS - volu_*MTS
*
          ENDDO
        ENDDO
      ENDDO
*
***** END OF LOOP: area integration 1
************************************************************************
*
*     get EAS parameters from the equilibrium of the enhanced forces
      CALL KOLFAC(neas,KEAS,UEAS)
      CALL KSOLVE(neas,1,UEAS,FEAS,alph)
*
************************************************************************
***** START OF LOOP: area integration 2
*
      AREA=0.D0
      DO i1 = 1,nipa
        DO i2 = 1,nipa
           DO i3 = 1,nipa
*
*     Enhanced Asumed Strain (EAS) matices
      CALL eas_matrix(xi(i1),xi(i2),xi(i3),M0,neas)
*
*     Interpolated base vectors
      CALL deriv_shapef(xi(i1),xi(i2),xi(i3),df)
      CALL KAB(3,3,8,1.D0,rRef,df,gRef) !#G = rRef*df
      CALL KAB(3,3,8,1.D0,rDef,df,gDef) !#g = rDef*df
*
*     transformation from base O onto the natural base
      CALL area_volume(gRef,area_,volu_)
      Jdet = volu_o/volu_
      CALL KATB(3,3,3,gRef,oRef_,GTO)
      CALL trafo_matrix(GTO,T)
      CALL KAB(6,neas,6,Jdet,T,M0,M1)
*
*     Green Lagrange strains
      CALL KATA(3,3,1.D0,gRef,gRef1)
      CALL KATA(3,3,1.D0,gDef,gDef1)
      CALL SYM2VEC((gDef1-gRef1)/2.D0,EVEC)
*
*     temperature
      CALL KTEMP(props(3),xi(i1),xi(i2),xi(i3),PREDEF(1,1,1:8),gRef,ET)
      EVEC = EVEC-ET
*       
*     assumed natural strain (ANS)
      CALL nstrain(E3,xi(i1),xi(i2),EVEC(3))
      CALL sstrain(E4,xi(i1),EVEC(4))
      CALL sstrain(E5,xi(i2),EVEC(5))
*
      CALL KAX(6,neas,M1,alph,EVEC1)
      EVEC  = EVEC + EVEC1
*
*     material matrix (6*6)
      CALL mat_matrix(gRef,lame,CMAT)
*     stress vector (PK2 stress resultants)
      CALL KSX(6,CMAT,EVEC,SVEC)
*--------------------------------------------------------------------------------------------------
*     strain energy 
      CALL KABT(1,1,6,EVEC,SVEC,ENGAU)
*      print*,"energy",energy
      energy=energy+ENGAU*volu_*0.5D0
*      print*,"volu",volu_
*--------------------------------------------------------------------------------------------------
*
*     material stiffness matrix
      CALL B_matrix(nnod,gDef,df,BMAT)
      CALL B_bars(rDefa,rDefb,rDefc,rDefd,xi(i1),xi(i2),ndofel,BMATS)
      CALL B_barn(3,rDef3,xi(i1),xi(i2),ndofel,BMAT3)
      BMAT(3:5,:) = 0.D0
      BMAT = BMAT + BMAT3 + BMATS
      CALL KSA(6,ndofel,CMAT,BMAT,CB)
      CALL KATB(ndofel,ndofel,6,BMAT,CB,BTCB)
      KMAT = KMAT + volu_*BTCB
*
*     coupling stiffness matrix
      CALL KATB(neas,ndofel,6,M1,CB,MTCB)
      KCPL = KCPL + volu_*MTCB
*
*     internal force vector
      CALL KATX(6,ndofel,BMAT,SVEC,BTS)
      FINT = FINT + volu_*BTS
*
*     derivatives of the B matrix
      CALL KXXT(nnod,1.D0,df(:,1),pf1)
      CALL KXXT(nnod,1.D0,df(:,2),pf2)
      CALL KXYTSY(nnod,df(:,1),df(:,2),pf6)
*
      CALL deriv_BN(3,xi(i1),xi(i2),dfe,dff,dfg,dfh,mf3)
      CALL deriv_BS(2,3,xi(i1),dfa,dfc,mf4)
      CALL deriv_BS(1,3,xi(i2),dfb,dfd,mf5)
*
*     submatrices of the geometric stiffness matrix
      Spf = Spf + volu_*(SVEC(1)*pf1+SVEC(2)*pf2
     #          + SVEC(3)*mf3+SVEC(4)*mf4+SVEC(5)*mf5+SVEC(6)*pf6)
*      
             ENDDO
           ENDDO
      ENDDO
*
***** END OF LOOP: area integration 2
************************************************************************
***** distributed load 
      RHS=0.D0
      DO i3=1,ndload     
*      
      IF (JDLTYP(i3).EQ.1) THEN
        DLOAD=ADLMAG(i3)
        DLNOD = 0.D0
*       get direction vectors for distributed load 
        CALL KDLNODIR(coords,DIRVEC)
*
***** START OF LOOP: area integration 3
      DO i1 = 1,nipa
        DO i2 = 1,nipa
*         Interpolated base vectors
          CALL deriv_shapef(xi(i1),xi(i2),0.D0,df)
          CALL KAB(3,3,8,1.D0,rRef,df,gRef) !#G = rRef*df
*	  
          CALL KAB(3,3,8,1.D0,rRef,df,gRef) !#G = rRef*df
*         transformation from base O onto the natural base
          CALL area_volume(gRef,area_,volu_)
*  
*         direction at integration point
          CALL KDLIPDIR(xi(i1),xi(i2),DIRVEC,DLDIR,sfa)
          CALL KAB(3,4,1,1.D0,DLDIR,sfa,DLNODI)
          DLNOD=DLNOD+DLNODI*area_
        ENDDO
      ENDDO
***** END OF LOOP: area integration 3
*
      DLNOD=DLNOD*DLOAD/2.D0  
      RHS(1:3)=RHS(1:3)+DLNOD(:,1)
      RHS(4:6)=RHS(4:6)+DLNOD(:,2)
      RHS(7:9)=RHS(7:9)+DLNOD(:,3)
      RHS(10:12)=RHS(10:12)+DLNOD(:,4)
      RHS(13:24)=RHS(1:12)
*
      ELSE
      IF (JDLTYP(1).NE.0) THEN
        WRITE(6,1)
 1      FORMAT(//,30X,'***WARNING: FALSE DEFINITION OF DLOAD')
      ENDIF
      ENDIF
      ENDDO
************************************************************************
*     geometric stiffness matrix
      DO n = 1,nnod
        DO m = 1,nnod
            KGEO((/-2,-1,-0/)+3*n,(/-2,-1,0/)+3*m) = Spf(n,m)*eye
        ENDDO
      ENDDO
*
*     total stiffness matrix
      CALL KSOLVE(neas,ndofel,UEAS,KCPL,K_KCPL)
      CALL KATB(ndofel,ndofel,neas,KCPL,K_KCPL,AMATRX)
      AMATRX = KMAT + KGEO - AMATRX
* 
*     RHS
      RHS=RHS-FINT
*
*
* AREA
      DO i1 = 1,nipa
        DO i2 = 1,nipa
*         Interpolated base vectors
          CALL deriv_shapef(xi(i1),xi(i2),0.D0,df)
          CALL KAB(3,3,8,1.D0,rRef,df,gRef) !#G = rRef*df
*	  
          CALL KAB(3,3,8,1.D0,rRef,df,gRef) !#G = rRef*df
*         transformation from base O onto the natural base
          CALL area_volume(gRef,area_,volu_)
*  
          AREA=AREA+area_
        ENDDO
      ENDDO
*
*       infla=ABS(TIME(1)-1.D0)
*       IF ((jelem.EQ.102502).OR.(jelem.EQ.112002)) THEN
*	IF (infla.LT.1.D-6) THEN
*         IF (jelem.EQ.1) THEN
*           print*,"  "
*           print*,"total step time: ",TIME(2)
*         ENDIF           
*         print*,TIME(2),jelem,ENERGY(2),ENERGY(2)/AREA
*       ENDIF
*	IF (TIME(2).GE.4.68D0) THEN
*         IF (jelem.EQ.1) THEN
*           print*,"  "
*           print*,"total step time: ",TIME(2)
*         ENDIF           
*         print*,TIME(2),jelem,ENERGY(2),ENERGY(2)/AREA
*       ENDIF
*       ENDIF
*
      END SUBROUTINE uel
*
*************************************************************************
***** direction of distributed load in nodes (defined by nodes n,n+4)
************************************************************************
*
      SUBROUTINE KDLNODIR(coords,DIRVEC)
      IMPLICIT NONE
      INTEGER i
      DOUBLE PRECISION coords(3,8),DIRVEC(3,4),ENORM
      DIRVEC(:,1)=coords(:,5)-coords(:,1)
      DIRVEC(:,1)=DIRVEC(:,1)/ENORM(DIRVEC(:,1))
      DIRVEC(:,2)=coords(:,6)-coords(:,2)
      DIRVEC(:,2)=DIRVEC(:,2)/ENORM(DIRVEC(:,2))
      DIRVEC(:,3)=coords(:,7)-coords(:,3)
      DIRVEC(:,3)=DIRVEC(:,3)/ENORM(DIRVEC(:,3))
      DIRVEC(:,4)=coords(:,8)-coords(:,4)
      DIRVEC(:,4)=DIRVEC(:,4)/ENORM(DIRVEC(:,4))
      END
*
*************************************************************************
***** direction of distributed load in nodes (defined by cross product)
************************************************************************
*
!      SUBROUTINE KDLNODIR(coords,DIRVEC)
!      IMPLICIT NONE
!      INTEGER i
!      DOUBLE PRECISION coords(3,8),DIRVEC(3,4),ENORM,
!     # VEC12(3),VEC23(3),VEC34(3),VEC41(3)
!      VEC12=coords(:,2)-coords(:,1) 
!      VEC23=coords(:,3)-coords(:,2) 
!      VEC34=coords(:,4)-coords(:,3) 
!      VEC41=coords(:,1)-coords(:,4) 
!      CALL KROSS(VEC12,-VEC41,DIRVEC(:,1))
!      CALL KROSS(VEC23,-VEC12,DIRVEC(:,2))
!      CALL KROSS(VEC34,-VEC23,DIRVEC(:,3))
!      CALL KROSS(VEC41,-VEC34,DIRVEC(:,4))
!      DIRVEC(:,1)=DIRVEC(:,1)/ENORM(DIRVEC(:,1))
!      DIRVEC(:,2)=DIRVEC(:,2)/ENORM(DIRVEC(:,2))
!      DIRVEC(:,3)=DIRVEC(:,3)/ENORM(DIRVEC(:,3))
!      DIRVEC(:,4)=DIRVEC(:,4)/ENORM(DIRVEC(:,4))
!      print*,"coords"
!      print*,coords
!      print*,"DIRVEC"
!      print*,DIRVEC
!      END
*
*************************************************************************
***** direction of distributed load in integration point
************************************************************************
*
      SUBROUTINE KDLIPDIR(xi1,xi2,DIRVEC,DLDIR,sf)
      IMPLICIT NONE
      INTEGER i
      DOUBLE PRECISION DL,xi1,xi2,DIRVEC(3,4),DLVEC(3),sf(4),DLDIR(3),
     # ENORM
      sf(1) = (1-xi1)*(1-xi2)/4.D0
      sf(2) = (1+xi1)*(1-xi2)/4.D0
      sf(3) = (1+xi1)*(1+xi2)/4.D0
      sf(4) = (1-xi1)*(1+xi2)/4.D0
      CALL KAX(3,4,DIRVEC,sf,DLDIR)
      DLDIR=DLDIR/ENORM(DLDIR)
      END
*
*************************************************************************
***** temperature
************************************************************************
*
      SUBROUTINE KTEMP(alpha,xi1,xi2,xi3,T,g,EVEC)
      IMPLICIT NONE
      INTEGER i
      DOUBLE PRECISION alpha,xi1,xi2,xi3,T(8),EVEC(6),sf(8),TA,g(3,3),
     # DOT
      CALL shapef(xi1,xi2,xi3,sf)
      CALL KAX(1,8,T,sf,TA)
      TA=TA*alpha
      EVEC(1)=TA*DOT(g(:,1),g(:,1))
      EVEC(2)=TA*DOT(g(:,2),g(:,2))
      EVEC(3)=TA*DOT(g(:,3),g(:,3))
      TA=TA*2.D0
      EVEC(4)=TA*DOT(g(:,2),g(:,3))
      EVEC(5)=TA*DOT(g(:,1),g(:,3))
      EVEC(6)=TA*DOT(g(:,1),g(:,2))
*      Print*,"-",T
      END
*
************************************************************************
***** shape functions
************************************************************************
*
      SUBROUTINE shapef(xi1,xi2,xi3,sf)
      IMPLICIT NONE
      DOUBLE PRECISION  xi1, xi2, xi3, sf(8)
      sf(1) = (1.D0-xi1)*(1.D0-xi2)*(1.D0-xi3)/8.D0;
      sf(2) = (1.D0+xi1)*(1.D0-xi2)*(1.D0-xi3)/8.D0;
      sf(3) = (1.D0+xi1)*(1.D0+xi2)*(1.D0-xi3)/8.D0;
      sf(4) = (1.D0-xi1)*(1.D0+xi2)*(1.D0-xi3)/8.D0;
      sf(5) = (1.D0-xi1)*(1.D0-xi2)*(1.D0+xi3)/8.D0;
      sf(6) = (1.D0+xi1)*(1.D0-xi2)*(1.D0+xi3)/8.D0;
      sf(7) = (1.D0+xi1)*(1.D0+xi2)*(1.D0+xi3)/8.D0;
      sf(8) = (1.D0-xi1)*(1.D0+xi2)*(1.D0+xi3)/8.D0;
      END
*
*************************************************************************
***** store symmetric 3*3 matrix in packed format
************************************************************************
*
      SUBROUTINE SYM2VEC(S,P)
      IMPLICIT NONE
      DOUBLE PRECISION P(6), S(3,3)
      P(1) = S(1,1)
      P(2) = S(2,2)
      P(3) = S(3,3)
      P(4) = S(2,3)*2.D0
      P(5) = S(1,3)*2.D0
      P(6) = S(1,2)*2.D0
      END
*
************************************************************************
***** expand 6 vector to symmetric 3*3 matrix
************************************************************************
*
      SUBROUTINE VEC2SYM(P,S)
      IMPLICIT NONE
      DOUBLE PRECISION P(6), S(3,3)
      S(1,1) = P(1)
      S(1,2) = P(6)/2.D0
      S(1,3) = P(5)/2.D0
      S(2,1) = S(1,2) 
      S(2,2) = P(2)
      S(2,3) = P(4)/2.D0
      S(3,1) = S(1,3)
      S(3,2) = S(2,3)
      S(3,3) = P(3)
      END
*
************************************************************************
***** fill empty half of symmetric matrix
************************************************************************
*
      SUBROUTINE KFILL(UPLO,M,S)
      IMPLICIT NONE
      INTEGER M, I
      CHARACTER UPLO*1
      DOUBLE PRECISION S(M,M)
      IF ( (UPLO.EQ.'U').OR.(UPLO.EQ.'u') ) THEN
        DO I = 1,M-1
          S(I+1,1:I) = S(1:I,I+1)
        ENDDO
      ELSEIF ( (UPLO.EQ.'L').OR.(UPLO.EQ.'l') ) THEN
        DO I = 2,M
          S(I-1,I:M) = S(I:M,I-1)
        ENDDO
      ELSE
        S = 0.D0
      ENDIF
      END
*
************************************************************************
***** Euclidic norm of a 3 vector
************************************************************************
*
      DOUBLE PRECISION FUNCTION ENORM(X)
      IMPLICIT NONE
      DOUBLE PRECISION X(3)
      ENORM = SQRT(X(1)**2+X(2)**2+X(3)**2)
      END
*
************************************************************************
***** dot product of two 3 vectors
************************************************************************
*
      DOUBLE PRECISION FUNCTION DOT(X,Y)
      IMPLICIT NONE
      DOUBLE PRECISION X(3), Y(3)
      DOT = X(1)*Y(1) + X(2)*Y(2) + X(3)*Y(3)
      END
*
************************************************************************
***** cross product of two 3 vectors
************************************************************************
*
      SUBROUTINE KROSS(X,Y,Z)
      IMPLICIT NONE
      DOUBLE PRECISION X(3), Y(3), Z(3)
      Z(1) = X(2)*Y(3) - X(3)*Y(2)
      Z(2) = X(3)*Y(1) - X(1)*Y(3)
      Z(3) = X(1)*Y(2) - X(2)*Y(1)
      END
*
************************************************************************
***** dot product of two 4 vectors
************************************************************************
*
      SUBROUTINE DOT4(X,Y,A)
      IMPLICIT NONE
      DOUBLE PRECISION X(4), Y(4), A
      A = X(1)*Y(1) + X(2)*Y(2) + X(3)*Y(3) + X(4)*Y(4)
      END
*
************************************************************************
***** dot product of two 2 vectors
************************************************************************
*
      SUBROUTINE DOT2(X,Y,A)
      IMPLICIT NONE
      DOUBLE PRECISION X(2), Y(2), A
      A = X(1)*Y(1) + X(2)*Y(2)
      END
*
************************************************************************
***** area & volume
************************************************************************
*
      SUBROUTINE area_volume(BASVEC,AREA,VOLUME)
      IMPLICIT NONE
      DOUBLE PRECISION DOT, ENORM, AREA, VOLUME, NORVEC(3), BASVEC(3,3)
      CALL KROSS(BASVEC(:,1),BASVEC(:,2),NORVEC)
      AREA = ENORM(NORVEC)
      VOLUME = DOT(BASVEC(:,3),NORVEC)
      END
*
************************************************************************
***** y = S * x ( S = SYM(M) )
************************************************************************
*
      SUBROUTINE KSX(M,S,X,Y)
      IMPLICIT NONE
      INTEGER M
      DOUBLE PRECISION X(M), Y(M), S(M,M)
      CALL DSYMV('U',M,1.D0,S,M,X,1,0.D0,Y,1)
      END
*
************************************************************************
***** y = A * x ( A = MAT(M,N) )
************************************************************************
*
      SUBROUTINE KAX(M,N,A,X,Y)
      IMPLICIT NONE
      INTEGER M,N
      DOUBLE PRECISION X(N), Y(M), A(M,N)
      CALL DGEMV('N',M,N,1.D0,A,M,X,1,0.D0,Y,1)
      END
*
************************************************************************
***** y = A' * x ( A = MAT(M,N) )
************************************************************************
*
      SUBROUTINE KATX(M,N,A,X,Y)
      IMPLICIT NONE
      INTEGER M,N
      DOUBLE PRECISION X(M), Y(N), A(M,N)
      CALL DGEMV('T',M,N,1.D0,A,M,X,1,0.D0,Y,1)
      END
*
************************************************************************
***** S = alpha * x * x' ( S = SYM(M) )
************************************************************************
*
      SUBROUTINE KXXT(M,ALPHA,X,S)
      IMPLICIT NONE
      INTEGER M
      DOUBLE PRECISION ALPHA, X(M), S(M,M)
      S = 0.D0
      CALL DSYR('U',M,ALPHA,X,1,S,M)
      CALL KFILL('U',M,S)
      END
*
************************************************************************
***** A = x * y' ( A = MAT(M,N) )
************************************************************************
*
      SUBROUTINE KXYT(M,N,X,Y,A)
      IMPLICIT NONE
      INTEGER M, N
      DOUBLE PRECISION X(M), Y(N), A(M,N)
      A = 0.D0
      CALL DGER(M,N,1.D0,X,1,Y,1,A,M)
      END
*
************************************************************************
***** S = x * y' + y * x' ( S = SYM(M) )
************************************************************************
*
      SUBROUTINE KXYTSY(M,X,Y,S)
      IMPLICIT NONE
      INTEGER M
      DOUBLE PRECISION X(M), Y(M), S(M,M)
      S = 0.D0
      CALL DSYR2('U',M,1.D0,X,1,Y,1,S,M)
      CALL KFILL('U',M,S)
      END
*
************************************************************************
***** C = S * A ( S = SYM(M), A = MAT(M,N), C = MAT(M,N) )
************************************************************************
*
      SUBROUTINE KSA(M,N,S,A,C)
      IMPLICIT NONE
      INTEGER M, N
      DOUBLE PRECISION S(M,M), A(M,N), C(M,N)
      C = 0.D0
      CALL DSYMM('L','U',M,N,1.D0,S,M,A,M,0.D0,C,M)
      END
*
************************************************************************
***** S = alpha * A' * A ( A = MAT(M,K), S = SYM(K) )
************************************************************************
*
      SUBROUTINE KATA(M,K,ALPHA,A,S)
      IMPLICIT NONE
      INTEGER M, K
      DOUBLE PRECISION ALPHA, A(M,K), S(K,K)
      S = 0.D0
      CALL DSYRK('U','T',M,K,ALPHA,A,K,0.D0,S,K)
      CALL KFILL('U',K,S)
      END
*
************************************************************************
***** C = alpha * A * B ( A = MAT(M,K), B = MAT(K,N), C = MAT(M,N) )
************************************************************************
*
      SUBROUTINE KAB(M,N,K,ALPHA,A,B,C)
      IMPLICIT NONE
      INTEGER M, N, K
      DOUBLE PRECISION ALPHA,A(M,K), B(K,N), C(M,N)
      C = 0.D0
      CALL DGEMM('N','N',M,N,K,ALPHA,A,M,B,K,0.D0,C,M)
      END
*
************************************************************************
***** C = A' * B ( A = MAT(K,M), B = MAT(K,N), C = MAT(M,N) )
************************************************************************
*
      SUBROUTINE KATB(M,N,K,A,B,C)
      IMPLICIT NONE
      INTEGER M, N, K
      DOUBLE PRECISION A(K,M), B(K,N), C(M,N)
      C = 0.D0
      CALL DGEMM('T','N',M,N,K,1.D0,A,K,B,K,0.D0,C,M)
      END
*
************************************************************************
***** C = A * B' ( A = MAT(M,K), B = MAT(N,K), C = MAT(M,N) )
************************************************************************
*
      SUBROUTINE KABT(M,N,K,A,B,C)
      IMPLICIT NONE
      INTEGER M, N, K
      DOUBLE PRECISION A(M,K), B(N,K), C(M,N)
      C = 0.D0
      CALL DGEMM('N','T',M,N,K,1.D0,A,M,B,N,0.D0,C,M)
      END
*
************************************************************************
***** S = alpha * ( A' * B + B' * A ) ( A,B = MAT(K,M), S = SYM(M) )
************************************************************************
*
      SUBROUTINE KATBSY(M,K,ALPHA,A,B,S)
      IMPLICIT NONE
      INTEGER M, K
      DOUBLE PRECISION ALPHA, A(K,M), B(K,M), S(M,M)
      S = 0.D0
      CALL DSYR2K('U','T',M,K,ALPHA,A,K,B,K,0.D0,S,M)
      CALL KFILL('U',M,S)
      END
*
************************************************************************
***** C = A' * S * A ( A = MAT(M,N), S = SYM(M), C = SYM(N) )
************************************************************************
*
      SUBROUTINE KATSA(M,N,A,S,C)
      IMPLICIT NONE
      INTEGER M, N
      DOUBLE PRECISION A(M,N), S(M,M), B(M,N), C(N,N)
      CALL KSA(M,N,S,A,B)
      CALL KATB(N,N,M,A,B,C)
      END
*
************************************************************************
***** A = U' * U (Cholesky factorization)
************************************************************************
*
      SUBROUTINE KOLFAC(N,A,U)
      IMPLICIT NONE
      INTEGER N, INFO
      DOUBLE PRECISION A(N,N), U(N,N)
 900  FORMAT(A50,I3)
      U = A
      CALL DPOTRF('U',N,U,N,INFO)
      IF ( INFO.NE.0 ) THEN
        U = 0.D0
        WRITE(7,900)
     #    '--- UEL ERROR CALLING KOLFAC. MATRIX DIMENSION =', N
      ENDIF
      END
*
************************************************************************
***** solve A * X = B, based on Cholesky factorization A = U' * U
************************************************************************
*
      SUBROUTINE KSOLVE(N,NRHS,U,B,X)
      IMPLICIT NONE
      INTEGER N, NRHS, INFO
      DOUBLE PRECISION U(N,N), B(N,NRHS), X(N,NRHS)
 900  FORMAT(A50,I3)
      X = B
      CALL DPOTRS('U',N,NRHS,U,N,X,N,INFO)
      IF ( INFO.NE.0 ) THEN
        X = 0.D0
        WRITE(7,900)
     #    '--- UEL ERROR CALLING KSOLVE. MATRIX DIMENSION =', N
      ENDIF
      END
*
************************************************************************
***** contravariant base vectors
************************************************************************
*
      SUBROUTINE co2contra(n,a_cov,a_con)
      IMPLICIT NONE
      INTEGER n
      DOUBLE PRECISION a_cov(3,n), a_con(3,n), A(n,n), U(n,n), X(n,3)
      CALL KATA(3,n,1.D0,a_cov,A)
      CALL KOLFAC(n,A,U)
      CALL KSOLVE(n,3,U,TRANSPOSE(a_cov),X)
      a_con = TRANSPOSE(X)
      END
*
************************************************************************
***** in-plane base vectors
************************************************************************
*
      SUBROUTINE deriv_shapef(xi1,xi2,xi3,df)
      IMPLICIT NONE
      DOUBLE PRECISION  xi1, xi2, xi3, df(8,3)
      df = 0.D0
*     df1
      df(1,1) = -(1-xi2)*(1-xi3)/8.D0
      df(2,1) = +(1-xi2)*(1-xi3)/8.D0
      df(3,1) = +(1+xi2)*(1-xi3)/8.D0
      df(4,1) = -(1+xi2)*(1-xi3)/8.D0
      df(5,1) = -(1-xi2)*(1+xi3)/8.D0
      df(6,1) = +(1-xi2)*(1+xi3)/8.D0
      df(7,1) = +(1+xi2)*(1+xi3)/8.D0
      df(8,1) = -(1+xi2)*(1+xi3)/8.D0
*     df2
      df(1,2) = -(1-xi1)*(1-xi3)/8.D0
      df(2,2) = -(1+xi1)*(1-xi3)/8.D0
      df(3,2) = +(1+xi1)*(1-xi3)/8.D0
      df(4,2) = +(1-xi1)*(1-xi3)/8.D0
      df(5,2) = -(1-xi1)*(1+xi3)/8.D0
      df(6,2) = -(1+xi1)*(1+xi3)/8.D0
      df(7,2) = +(1+xi1)*(1+xi3)/8.D0
      df(8,2) = +(1-xi1)*(1+xi3)/8.D0
*     df3 
      df(1,3) = -(1-xi1)*(1-xi2)/8.D0
      df(2,3) = -(1+xi1)*(1-xi2)/8.D0
      df(3,3) = -(1+xi1)*(1+xi2)/8.D0
      df(4,3) = -(1-xi1)*(1+xi2)/8.D0
      df(5,3) = +(1-xi1)*(1-xi2)/8.D0
      df(6,3) = +(1+xi1)*(1-xi2)/8.D0
      df(7,3) = +(1+xi1)*(1+xi2)/8.D0
      df(8,3) = +(1-xi1)*(1+xi2)/8.D0
      END
*
************************************************************************
***** transformation from base O onto the natural base
************************************************************************
*
      SUBROUTINE trafo_matrix(GTO,T)
      IMPLICIT NONE
      DOUBLE PRECISION GTO(3,3), T(6,6)
*
      T(1,1) = GTO(1,1)**2
      T(2,1) = GTO(2,1)**2
      T(3,1) = GTO(3,1)**2
      T(1,2) = GTO(1,2)**2
      T(2,2) = GTO(2,2)**2
      T(3,2) = GTO(3,2)**2
      T(1,3) = GTO(1,3)**2
      T(2,3) = GTO(2,3)**2
      T(3,3) = GTO(3,3)**2
*
      T(1,4) = 2*GTO(1,2)*GTO(1,3)
      T(2,4) = 2*GTO(2,2)*GTO(2,3)
      T(3,4) = 2*GTO(3,2)*GTO(3,3)
      T(1,5) = 2*GTO(1,1)*GTO(1,3)
      T(2,5) = 2*GTO(2,1)*GTO(2,3)
      T(3,5) = 2*GTO(3,1)*GTO(3,3)
      T(1,6) = 2*GTO(1,1)*GTO(1,2)
      T(2,6) = 2*GTO(2,1)*GTO(2,2)
      T(3,6) = 2*GTO(3,1)*GTO(3,2)
*
      T(4,1) = GTO(2,1)*GTO(3,1)
      T(5,1) = GTO(1,1)*GTO(3,1)
      T(6,1) = GTO(1,1)*GTO(2,1)
      T(4,2) = GTO(2,2)*GTO(3,2)
      T(5,2) = GTO(1,2)*GTO(3,2)
      T(6,2) = GTO(1,2)*GTO(2,2)
      T(4,3) = GTO(2,3)*GTO(3,3)
      T(5,3) = GTO(1,3)*GTO(3,3)
      T(6,3) = GTO(1,3)*GTO(2,3)
*
      T(4,4) = GTO(2,2)*GTO(3,3) + GTO(2,3)*GTO(3,2)
      T(5,4) = GTO(1,2)*GTO(3,3) + GTO(1,3)*GTO(3,2)
      T(6,4) = GTO(1,2)*GTO(2,3) + GTO(1,3)*GTO(2,2)
      T(4,5) = GTO(2,1)*GTO(3,3) + GTO(2,3)*GTO(3,1)
      T(5,5) = GTO(1,1)*GTO(3,3) + GTO(1,3)*GTO(3,1)
      T(6,5) = GTO(1,1)*GTO(2,3) + GTO(1,3)*GTO(2,1)
      T(4,6) = GTO(2,1)*GTO(3,2) + GTO(2,2)*GTO(3,1)
      T(5,6) = GTO(1,1)*GTO(3,2) + GTO(1,2)*GTO(3,1)
      T(6,6) = GTO(1,1)*GTO(2,2) + GTO(1,2)*GTO(2,1)
      END
*
************************************************************************
***** EAS ansatz functions in the natural coordinate system
************************************************************************
*
      SUBROUTINE eas_matrix(xi1,xi2,xi3,M,neas)
      IMPLICIT NONE
      INTEGER neas
      DOUBLE PRECISION xi1, xi2, xi3, M(6,neas)
      M = 0.D0
      M(1,1) = xi1
      M(2,2) = xi2
      M(6,3) = xi1
      M(6,4) = xi2
      M(6,5) = xi1*xi2
      M(3,6) = xi3
      M(3,7) = xi1*xi3
      M(3,8) = xi2*xi3
      M(3,9) = xi1*xi2*xi3
      END
*
************************************************************************
***** Asumed Natural Strains ANS
************************************************************************      
*      
      SUBROUTINE nstrain(E3,xi1,xi2,EVEC3)
      IMPLICIT NONE
      DOUBLE PRECISION E3,xi1,xi2,sf(4),EVEC3
      sf(1) = (1-xi1)*(1-xi2)/4.D0
      sf(2) = (1+xi1)*(1-xi2)/4.D0
      sf(3) = (1+xi1)*(1+xi2)/4.D0
      sf(4) = (1-xi1)*(1+xi2)/4.D0
      CALL DOT4(E3,sf,EVEC3)
      END
*
      SUBROUTINE sstrain(E,xi1,EVEC)
      IMPLICIT NONE
      DOUBLE PRECISION E(2), xi1, sf(2), EVEC
      EVEC = 0.D0
      sf(1) = (1-xi1)/2.D0
      sf(2) = (1+xi1)/2.D0
      CALL DOT2(E,sf,EVEC)
      END
*
************************************************************************
***** strain displacement (B) matrices
************************************************************************      
*      
      SUBROUTINE B_matrix(nnod,gDef,df,BMAT)
      IMPLICIT NONE
      INTEGER n, nnod, j(3)
      DOUBLE PRECISION gDef(3,3), df(nnod,3),BMAT(6,24)
      DO n=1,nnod
           j=(/-2,-1,0/)+3*n
      BMAT(1,j) = df(n,1)*gDef(:,1)
      BMAT(2,j) = df(n,2)*gDef(:,2)
      BMAT(3,j) = df(n,3)*gDef(:,3)
      BMAT(4,j) = df(n,2)*gDef(:,3) + df(n,3)*gDef(:,2)
      BMAT(5,j) = df(n,1)*gDef(:,3) + df(n,3)*gDef(:,1)
      BMAT(6,j) = df(n,1)*gDef(:,2) + df(n,2)*gDef(:,1)
      ENDDO
      END
*
************************************************************************
***** derivatives of the B matrix
************************************************************************      
*
      SUBROUTINE deriv_BN(i,xi1,xi2,df1,df2,df3,df4,mf)
      IMPLICIT NONE
      INTEGER i
      DOUBLE PRECISION xi1, xi2, mf(8,8), sf(4)
      DOUBLE PRECISION pf1(8,8), pf2(8,8), pf3(8,8), pf4(8,8)
      DOUBLE PRECISION df1(8,3), df2(8,3), df3(8,3), df4(8,3)
      sf(1) = (1-xi1)*(1-xi2)/4.D0
      sf(2) = (1+xi1)*(1-xi2)/4.D0
      sf(3) = (1+xi1)*(1+xi2)/4.D0
      sf(4) = (1-xi1)*(1+xi2)/4.D0
      CALL KXYT(8,8,df1(:,i),df1(:,i),pf1)
      CALL KXYT(8,8,df2(:,i),df2(:,i),pf2)
      CALL KXYT(8,8,df3(:,i),df3(:,i),pf3)
      CALL KXYT(8,8,df4(:,i),df4(:,i),pf4)
      mf = sf(1)*pf1 + sf(2)*pf2 + sf(3)*pf3 + sf(4)*pf4
      END
*
*
      SUBROUTINE deriv_BS(j,k,xi1,df1,df2,mf)
      IMPLICIT NONE
      INTEGER j,k
      DOUBLE PRECISION xi1, mf(8,8), sf(2)
      DOUBLE PRECISION pf1(8,8), pf2(8,8), pf3(8,8), pf4(8,8)
      DOUBLE PRECISION df1(8,3), df2(8,3)
      sf(1) = (1-xi1)/2.D0
      sf(2) = (1+xi1)/2.D0
      CALL KXYT(8,8,df1(:,j),df1(:,k),pf1)
      CALL KXYT(8,8,df1(:,k),df1(:,j),pf2)
      CALL KXYT(8,8,df2(:,j),df2(:,k),pf3)
      CALL KXYT(8,8,df2(:,k),df2(:,j),pf4)
      mf = sf(1)*(pf1+pf2) + sf(2)*(pf3+pf4)
      END
*
************************************************************************
***** B bar (ANS)
************************************************************************
*
      SUBROUTINE B_barn(j,rDef,xi1,xi2,ndofel,BMATN)
      IMPLICIT NONE
      INTEGER j, n, k, ndofel, nm(3,4), np(3,4)
      DOUBLE PRECISION sf(4), rDef(3,4), xi1, xi2, BMATN(6,ndofel)
      sf(1) = (1-xi1)*(1-xi2)/8.D0
      sf(2) = (1+xi1)*(1-xi2)/8.D0
      sf(3) = (1+xi1)*(1+xi2)/8.D0
      sf(4) = (1-xi1)*(1+xi2)/8.D0
      BMATN = 0.D0
      nm = 0.D0
      np = 0.D0
      nm(1,:) = [1,4,8,5]
      np(1,:) = [2,3,7,6]
      nm(2,:) = [1,2,6,5]
      np(2,:) = [4,3,7,8]
      nm(3,:) = [1,2,3,4]
      np(3,:) = [5,6,7,8]
      DO n=1,4
        DO k = 1,3
          BMATN(j,k+3*(nm(j,n)-1)) = -sf(n)*rDef(k,n)
          BMATN(j,k+3*(np(j,n)-1)) =  sf(n)*rDef(k,n)
        ENDDO
      ENDDO
      END
*
************************************************************************
*
      SUBROUTINE B_bars(rDefa,rDefb,rDefc,rDefd,xi1,xi2,ndofel,BMATS)
      IMPLICIT NONE
      INTEGER ndofel
      DOUBLE PRECISION fa, fb, fc, fd
      DOUBLE PRECISION rDefa(3,4), rDefb(3,4), rDefc(3,4), rDefd(3,4),
     #                 xi1, xi2, BMATS(6,ndofel)
*     
      fa = (1-xi1)/8.D0
      fc = (1+xi1)/8.D0
      fb = (1-xi2)/8.D0
      fd = (1+xi2)/8.D0
*
      BMATS = 0.D0
      BMATS(4,1:3)   = fa*(-rDefa(:,3)-rDefa(:,2))
      BMATS(4,4:6)   = fc*(-rDefc(:,3)-rDefc(:,2))
      BMATS(4,7:9)   = fc*(+rDefc(:,3)-rDefc(:,2))
      BMATS(4,10:12) = fa*(+rDefa(:,3)-rDefa(:,2))
      BMATS(4,13:15) = fa*(-rDefa(:,3)+rDefa(:,2))
      BMATS(4,16:18) = fc*(-rDefc(:,3)+rDefc(:,2))
      BMATS(4,19:21) = fc*(+rDefc(:,3)+rDefc(:,2))
      BMATS(4,22:24) = fa*(+rDefa(:,3)+rDefa(:,2))
*
      BMATS(5,1:3)   = fb*(-rDefb(:,3)-rDefb(:,1))
      BMATS(5,4:6)   = fb*(+rDefb(:,3)-rDefb(:,1))
      BMATS(5,7:9)   = fd*(+rDefd(:,3)-rDefd(:,1))
      BMATS(5,10:12) = fd*(-rDefd(:,3)-rDefd(:,1))
      BMATS(5,13:15) = fb*(-rDefb(:,3)+rDefb(:,1))
      BMATS(5,16:18) = fb*(+rDefb(:,3)+rDefb(:,1))
      BMATS(5,19:21) = fd*(+rDefd(:,3)+rDefd(:,1))
      BMATS(5,22:24) = fd*(-rDefd(:,3)+rDefd(:,1))
      END
*
************************************************************************
***** material (C) matrices
************************************************************************
*
      SUBROUTINE mat_matrix(gRef,lame,CMAT)
      IMPLICIT NONE
      INTEGER i, j, k, l, ind(3,3)
      DOUBLE PRECISION lame(2),gRef(3,3),gRef_(3,3),gg_(3,3),CMAT(6,6)
*     initializations
      ind(1,1) = 1;  ind(1,2) = 6;  ind(1,3) = 5
      ind(2,1) = 6;  ind(2,2) = 2;  ind(2,3) = 4
      ind(3,1) = 5;  ind(3,2) = 4;  ind(3,3) = 3
      CMAT = 0.D0
*       base vectors and contravariant metric tensor of the shell body
        CALL co2contra(3,gRef,gRef_)
        CALL KATA(3,3,1.D0,gRef_,gg_)
*       6 x 6 material matrix
        DO i = 1,3 
          DO j = 1,i
            DO k = 1,3 
              DO l = 1,k
                CMAT(ind(i,j),ind(k,l)) = lame(1)*gg_(i,j)*gg_(k,l)+
     #                lame(2)*(gg_(i,k)*gg_(j,l)+gg_(i,l)*gg_(j,k))
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      CMAT(4,4) = CMAT(4,4)*5.D0/6.D0
      CMAT(5,5) = CMAT(5,5)*5.D0/6.D0
      END
*
************************************************************************
***** END OF FILE
************************************************************************
