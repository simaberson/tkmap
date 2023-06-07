C     --------------------------------------------------------------- 
      PROGRAM TKMAP
C 
C     Program plots dropwindsonde mission flight tracks, and 
C     tabulates turn and drop points.
C     --------------------------------------------------------------- 
C     Modified by Paul A. Leighton, Jul 2015, for corrections requested by 
C     Jason Dunion.  See "! pal" comment on lines added or amended.
C 
      PARAMETER (MPTS = 1000, MAXTRAX = 4)
C
      CHARACTER*1 TYPE(MPTS), ANS
      CHARACTER*8 TAKEOFF
      CHARACTER*12 FNAME
      CHARACTER*30 STRING, STMNAME, STRINGA, STRINGZ
      CHARACTER*50 LIBNAME, CURNAME
C
      DIMENSION DTYPE(MPTS),
     1          SPEED(MPTS)
      LOGICAL SKIP
C
C     Set LUs
C     ------------------------------------------------
      deg2rad=3.1415927/180.
      OPEN(1, FILE = '/dev/tty')  ! pal for print to screen.
      LUFI = 14              !FLIGHT TRACK FILE
C
      do IFILE=1,MAXTRAX
        FNAME = 'current .ftk'
        WRITE(FNAME(8:8),'(i1)')IFILE
C
C       Calculate track distance tables
C       -------------------------------
        CALL TRACKDIS(IFILE,FNAME)
      end do
      STOP '99'
C
      END 
C
C     ----------------------------------------------------
      SUBROUTINE TRACKDIS(IFILE,FNAME)
C     Computes great circle distance from files 
C     containing turns points of flight tracks. 
C     All latitude and longitude values must be positive. 
C     James Franklin   7/86, 2/89, 2/96
C     ----------------------------------------------------
C2345678911234567892123456789312345678941234567895123456789612345678971X34567890
      PARAMETER (MPTS = 100)
C
      DIMENSION RLAT(MPTS),RLON(MPTS),IPOS(10),DTYPE(MPTS),SPEED(MPTS)
      DIMENSION SRRAD(MPTS), SRAZM(MPTS)
C
      CHARACTER*1 TYPE(MPTS), FLAG(MPTS), TEE
      CHARACTER*7 FNAMEP
      CHARACTER*8 TAKEOFF,FNAMEHUR
      CHARACTER*10 FILEOUT,FILEOUTD
      CHARACTER*12 FNAME
      CHARACTER*30 STRING,NAME(10),STMNAME,STRINGA,STRINGZ
      CHARACTER*30 TTYP
      LOGICAL globalhawk, climb, ipset, climbdleg  ! pal used to not include inserted drops in turn file.
C 
 160  FORMAT(1X,I2,A1,4X,A16,12X,1X,f5.0,3X,f5.0,4X,I2,':',I2.2)
 161  FORMAT(1X,I2,A1,2X,2(I4,1X,I2.2,2X),12X,
     *      1X,f5.0,3X,f5.0,4X,I2,':',I2.2)
 162  FORMAT(1X,I2,A1,2X,2(I4,1X,I2.2,2X), 
     *       2X,I3,'/',i3.3,4X,f5.0,3X,f5.0,4X,I2,':',I2.2)
 171  FORMAT(1X,I2,A1,2X,2(I4,1X,I2.2,2X),13X,I2,':',I2.2)
 172  FORMAT(1X,I2,A1,2X,2(I4,1X,I2.2,2X),3X,I3,'/',I3.3,
     1          3X,I2,':',I2.2)
 500  FORMAT(2F8.3,1x,a1)
 501  FORMAT(2F8.3)
 901  FORMAT(1X,72("="),/,
     * 1x,'MISSION PLAN: ',a20,//,
     * 1X,'Prepared by the Hurricane Research Division',
     * 1x,'File: ',A,//,
     * 1X,'Aircraft: N',i2,'RF  Proposed takeoff: ',A8,/,
     * 1x,72("="),///
     * 1x,A30,/) 
 902  FORMAT(1X,
     *'===========================================================') 
 903  FORMAT
     * (1X,' #      LAT      LON     RAD/AZM     LEG    TOTAL     TIME'/
     *   '        (d m)    (d/m)    (nm/dg)     (nm)   (nm)     (h:mm)') 
 904  FORMAT(1X,
     *'-----------------------------------------------------------') 
 907  FORMAT(1X,'==========================================') 
 908  FORMAT(1X,' #      LAT      LON      RAD/AZM    TIME',/,
     *         '        (d m)    (d m)     (nm/dg)   (h:mm)')
 909  FORMAT(1X,'------------------------------------------')
 910  FORMAT(1X,'------------------------------------------')
C2345678911234567892123456789312345678941234567895123456789612345678971X34567890
C
      TEE='T'
      LUT = 8
      LUP = 9  ! pal
      LUFO = 7
      LUFI = 14 
      deg2rad=3.1415927/180.  ! pal
      globalhawk = .false.  ! pal
      climb = .false.  ! pal
      ipset = .true.  ! pal
      climbdleg = .true.  ! pal
      inserted = 0

C     Get track file name, reset variables
C     ------------------------------------
C2345678911234567892123456789312345678941234567895123456789612345678971X34567890
      OPEN(LUFI,FILE=FNAME,STATUS='OLD',ERR=9010) 
      WRITE(1, '(1X, A12)') FNAME
      WRITE(FILEOUT,'("turns",I1.1,".txt")') IFILE
      OPEN(LUFO,FILE=FILEOUT,STATUS='UNKNOWN',ERR=9011)
      WRITE(FILEOUTD,'("drops",I1.1,".txt")') IFILE
      OPEN(LUT,FILE=FILEOUTD,STATUS='UNKNOWN',ERR=9012)
      WRITE(FNAMEP,'("points",I1.1)') IFILE
      OPEN(LUP,FILE=FNAMEP,STATUS='UNKNOWN',ERR=9013)
      WRITE(FNAMEHUR,'("hurrloc",I1.1)') IFILE
      OPEN(UNIT=72,FILE=FNAMEHUR,STATUS='UNKNOWN',ERR=9014)
C
      DO L = 1,MPTS
        FLAG(L) = ' '
      END DO
C
C     Read header line
C     ----------------
      READ(LUFI,'(I2,1X,8A,1X,A20)') IAC,TAKEOFF,STMNAME
      WRITE(1, '(I2, 1X, 8A, 1X, A20)') iac, takeoff, stmname  ! pal
      IALT1 = 0
      IALT2 = 0
      IF (IAC.EQ.42 .OR. IAC.EQ.43) THEN
        globalhawk = .true.  ! pal boolean for non-included drops to turns.
        DO IJK=1,MPTS
          SPEED(IJK) = 330.  ! jpd modified to correct for faster ferry speeds
        END DO
         SPEED(1) = 330.
         IALT1 = 0
         IALT2 = 10
      ENDIF
      IF (IAC.EQ.49) THEN
         globalhawk = .true.  ! pal boolean for non-included drops to turns.
         DO IJK=1,MPTS
           SPEED(IJK) = 442.
         END DO
         IALT1 = 41
         IALT2 = 45
      ENDIF
c
c     50 is AFC130-J
c
      if (iac.eq.50)then
        globalhawk = .true.  ! pal boolean for non-included drops to turns.
        DO IJK=1,MPTS
          SPEED(IJK) = 290.
        END DO
        ialt1 = 18
        ialt2 = 25
      endif
c
c     51 is NASA DC-8
c
      if (iac.eq.51)then
        globalhawk = .true.  ! pal boolean for non-included drops to turns.
        DO IJK=1,MPTS
          SPEED(IJK) = 440.
        END DO
        ialt1 = 18
        ialt2 = 38
      endif
c
c     52 is Global Hawk
c
      if (iac.eq.52)then
        globalhawk = .true.  ! pal boolean for non-included drops to turns.
        DO IJK=1,MPTS
          SPEED(IJK) = 335.
        END DO
        ialt1 = 55
        ialt2 = 60
      endif
c
c     57 is AF WB-57
c
      if (iac.eq.57)then
        globalhawk = .true.  ! pal sets 57 to act same as globalhawk for inserts
        DO IJK=1,MPTS
          SPEED(IJK) = 300.
        END DO
        ialt1 = 40
        ialt2 = 60
      endif

      speed(2)=speed(1)
      I = 0                             
      DLEG = 0. 
      TOTAL = 0.
      TIME = 0.
      NS = 0 
C 
C2345678911234567892123456789312345678941234567895123456789612345678971X34567890
C 
C     Begin reading in input file 
C     --------------------------- 
      INSERT = 0
100   I = I+1 
      READ(LUFI,'(A1,1X,A)',END=800) TYPE(I), STRING 
      SRRAD(I) = -99.
      CALL UPPERCASE(TYPE(I))
      DO L = 1,20
        CALL UPPERCASE(STMNAME(L:L))
        CALL UPPERCASE(STRING(L:L))
      END DO
C
      FLAG(I) = ' '
      IF (TYPE(I).EQ.'E' .OR. TYPE(I).EQ.'F') FLAG(I)='E'
      IF (TYPE(I).EQ.'I') THEN
        READ(STRING,*) INSERT,ALT
        WRITE(1, '("I", I2, 1X, F7.1)') insert, alt  ! pal 
        do ijk=i,i+insert
          IF((IAC.EQ.42.OR.IAC.EQ.43.OR.IAC.EQ.57).AND.I.GT.3)
     1       CALL GETSPEED(IAC,ALT,SPEED(ijk))
        end do
        rlat(i) = insert
        rlon(i) = alt
        
        GOTO 100
      ENDIF
C
      IF (TYPE(I).EQ.'C') THEN
        READ(STRING,*) IALT1,IALT2
        I = I-1
        GOTO 100
      ENDIF
C
      IF (TYPE(I).EQ.'H') THEN
        read(string,*,err=120)rlat(i),rlon(i),isdir,isspd
        sdir=float(isdir)
        sspd=float(isspd)
        call uvcomp(sdir,sspd)
        stormu=-sdir
        stormv=-sspd
        OPEN(UNIT=72,FILE=FNAMEHUR)
        SLAT = RLAT(I)
        SLON = -RLON(I)
        write(72,*)slat,slon
        close(72)
        goto 100
      ENDIF
C
C     Position input is a name or stmrel position
C     -------------------------------------------
120   IF (TYPE(I).EQ.'S') THEN
         READ(STRING(1:20),*) RDIS,THETA,ALT
         WRITE(1, '("S", 3(F6.0, 1X))') rdis, theta, alt  ! pal
         IF((IAC.EQ.42.OR.IAC.EQ.43.OR.IAC.EQ.57).AND.I.GT.3)
     1      CALL GETSPEED(IAC,ALT,SPEED(I))
         RDIS = RDIS*111.1/60.
         CALL SRLATLON(SLAT,SLON,THETA,RDIS,XLAT,XLON)
         rlat(i)=xlat
         rlon(i)=-xlon
         rlat(i)=(xlat-slat)*60.  ! pal 
         rlon(i)=(xlon-slon)*60.*cos(deg2rad*0.5*(xlat+slat))  ! pal
         FLAG(I) = 'S'
         SRRAD(I) = RDIS*60./111.1
         SRAZM(I) = THETA
         insert = 0
         GOTO 100
      ENDIF
      IF (TYPE(I).EQ.'A'.OR.TYPE(I).EQ.'Z')THEN
         CALL CITY(0,STRING,RLAT(I),RLON(I))
         WRITE(1, '(A1, 1X, A30)') type(i), string  ! pal
         IF (RLAT(I).EQ.-99.) GOTO 900
         SRRAD(I) = -99.
         NS = NS+1
         IPOS(NS) = I
         IF(TYPE(I).EQ.'A') THEN
           STRINGA=STRING
           name(ns) = string  ! pal set first name
         ENDIF
         IF(TYPE(I).EQ.'Z')then
           stringz = string  ! pal from above but set name(ns) only at end
           IF(IAC.EQ.42.OR.IAC.EQ.43)SPEED(I)=330.
	   IF(IAC.EQ.57)SPEED(I)=300.  ! jpd modified
           IF (insert.GT.0.AND.(type(i-2).NE.'S')) THEN
             ipos(ns) = i + insert - 1
             name(ns) = string
             GOTO 130  ! pal insert final leg
           ENDIF
           name(ns) = string
           insert = 0
           GOTO 800
         ENDIF
         goto 100
      ELSE
         READ(STRING,*,ERR=120) RLAT(I),RLON(I),ALT
         WRITE(1, '(3(F8.2, 1X))') rlat(i), rlon(i), alt  ! pal
         IF((IAC.EQ.42.OR.IAC.EQ.43.OR.IAC.EQ.57).AND.I.GT.3)
     1     CALL GETSPEED(IAC,ALT,SPEED(I))
         IF(insert.eq.0) goto 100
      ENDIF
C
C
130   CONTINUE
      DTYPE(I) = 1.0
      IF (INSERT.GT.0) THEN
         K = I+INSERT-1
         RLAT(K) = RLAT(I)
         RLON(K) = RLON(I)
         TYPE(K) = TYPE(I)
         FLAG(K) = FLAG(I)
         DTYPE(K) = DTYPE(I)
         SRRAD(K) = SRRAD(I)
         SRAZM(K) = SRAZM(I)
         RLATINC = (RLAT(k) - RLAT(I-2))/FLOAT(INSERT+1)
         RLONINC = (RLON(k) - RLON(I-2))/FLOAT(INSERT+1)
         DO L = 1,INSERT
            J = I-2+L
            RLAT(J) = RLAT(I-2)+FLOAT(L)*RLATINC
            RLON(J) = RLON(I-2)+FLOAT(L)*RLONINC
            TYPE(J) = 'D'
            DTYPE(J) = 1.0
            SRRAD(J) = -99.
            flag(j) = 'I'  ! pal- all should be labelled 'I' not 'S' or 'D'
         END DO
         I = K
         INSERT = 0
      ENDIF
C
      IF (I.GT.100) STOP '100'
      IF (TYPE(I).NE.'Z') GOTO 100
      WRITE(1, '(A1, 1X, A30)') type(i), string  ! pal
C 
      NT = I
C 
C2345678911234567892123456789312345678941234567895123456789612345678971X34567890
C 
C     -----------------------------------------------------------
C     Done reading input file.
C     Compute track distance for all turn points and print tables
C     -----------------------------------------------------------
C 
 800  CONTINUE
      TTYP = 'TRACK DISTANCE TABLE'
      NT = I
      WRITE(LUFO,901) STMNAME,FNAME,IAC,TAKEOFF,TTYP
      WRITE(1, 901) stmname, fname, iac, takeoff, ttyp  ! pal
      WRITE(LUFO,902)
      WRITE(LUFO,903)
      WRITE(LUFO,904)
      TTYP = 'DROP LOCATIONS'
      WRITE(LUT,901) STMNAME,FNAME,IAC,TAKEOFF,TTYP
      WRITE(LUT,907)
      WRITE(LUT,908)
      WRITE(LUT,909)
C     WRITE(LUT,'(/)') 
C 
      LTURN = -1
      IC = 0
      LDROP = 0  !  jpd changed from “1” to “0” to set IP as waypoint #1: 6-10-2016
      TOTAL = 0.
      subtotal = 0.0  ! pal keeps track of legs of inserted drops.
      TIME = 0. 
      i = 0
      dleg52 = 0.0
  150 i = i + 1 
      subtotal = total
      IF (TYPE(I).EQ.'X' .OR. TYPE(I).EQ.'H') GOTO 150
      IF (type(i).EQ.'I') THEN  ! insert storm relative points count read
        insert = rlat(i)
        inserted = insert
        alt = rlon(i)
        GOTO 150
      ENDIF        
      IL = IC                      ! Current index
      IC = I                       ! Last index 
      IF (IL .EQ. 0) IL = IC
      LTURN = LTURN+1 
      if(type(i).eq.'S')then
        temptime=time
C       I have no idea why this is being done in a loop of 5 but it matters
C       for the results to be the same as before??? pal
        Do ijkl=1,5 
c         calculate storm location at time
          stormlat=slat+temptime*stormv/60.
          stormlon=slon+temptime*stormu/(60.*cos(deg2rad*stormlat))
c
c         calculate aircraft location in relation to storm
          temprlat=stormlat+rlat(i)/60.
          temprlon=-(stormlon+rlon(i)/(60.*cos(deg2rad*temprlat)))
          dleg=gcdistance(temprlat,temprlon,rlat(il),rlon(il))
          temptime=time+dleg/speed(i) 
        ENDDO  ! ijkl 1->5 loop
        rlat(ic)=temprlat
        rlon(ic)=temprlon
        dleg=gcdistance(rlat(ic),rlon(ic),rlat(il),rlon(il))
        total=total+dleg  
        time=time+dleg/speed(i)+0.0167
        subtotal = subtotal + dleg

        IF (insert.NE.0) THEN  ! pal insert storm relative points here
          dlegout = dleg / (insert + 1)
          time = time - dleg/speed(i)
          subtotal = subtotal - dleg
          newnt = nt + insert - 1
          dtype(i) = 1.0
          k = i + insert - 1
          DO j = newnt, k, -1  ! pal move points to leave space for inserts
            l = j - insert + 1
            rlat(j) = rlat(l)
            rlon(j) = rlon(l)
            type(j) = type(l)
            flag(j) = flag(l)
            dtype(j) = dtype(l)
            srrad(j) = srrad(l)
            srazm(j) = srazm(l)
            speed(j) = speed(l)
          ENDDO
          rlatinc = (rlat(k) - rlat(i-2))/FLOAT(insert + 1)
          rloninc = (rlon(k) - rlon(i-2))/FLOAT(insert + 1)
          temptime = time
          stormlat=slat+temptime*stormv/60.
          stormlon=slon+temptime*stormu/(60.*cos(deg2rad*stormlat))
          DO l = 1, insert
            j = i - 2 + l
            rlat(j) = rlat(i - 2) + FLOAT(l) * rlatinc
            rlon(j) = rlon(i - 2) + FLOAT(l) * rloninc
            xlat = rlat(j) ! pal comments out to test Aug 3
            xlon = -rlon(j)  ! pal comments out to test Aug 3
            temprlat=stormlat+rlat(j)/60.
            temprlon=-(stormlon+rlon(j)/(60.*cos(deg2rad*temprlat)))
            dlegtime=gcdistance(temprlat,temprlon,rlat(j-1),rlon(j-1))
            temptime=time+dlegtime/speed(j - 1)  ! pal change i to j - 1
            type(j) = 'D'
            dtype(j) = 1.0
            srrad(j) = -99.
            flag(j) = 'I'  ! pal- all should be labelled 'I' not 'S' or 'D'
            time = time + dlegout / speed(j)  ! pal 
            subtotal = subtotal + dlegout  ! pal ? total = total + dleg ???
            CALL DGMN(rlat(j), lad, lam)
            CALL DGMN(rlon(j), lod, lom)
            CALL DGMN(time, ith, itm) 
            ldrop = ldrop + 1
            if((lturn.eq.1 .OR. ldrop.eq.1) .AND. ipset) then  !  jpd changed from “2” to “1” to set IP as waypoint #1: 6-10-2016
              iph=ith
              ipm=itm
              ipset=.false.
            endif
            WRITE(LUP, 501) rlat(j), rlon(j)  ! output to points file
            IF (.NOT. globalhawk)
     +        WRITE(lufo, 161) lturn, flag(j), lad, lam, lod, lom,
     +          dlegout, subtotal, ith, itm
              WRITE(lut, 171) ldrop, flag(j), lad, lam, lod, lom,
     +          ith, itm 
c              lturn = lturn + 1  ! pal commented out to not count SR inserts as WPs
          ENDDO  ! pal inserts
          indexk = 21  ! jason testing 10 aug 2016
          dleg52 = 0
          total = subtotal + dlegout  ! pal 
          subtotal=0  ! pal inserts to test Aug 3
          time = time + dlegout / speed(j)  ! pal just added
          dlegout = 0  ! pal added for SR insert testing 10 aug 2016
          i = k
          ic = ic+insert-1  ! pal changes on 12 aug 2016
          insert = 0
          nt = newnt
          ipos(ns) = nt
        ENDIF  ! pal storm relative insertions
      elseif(type(i).ne.'I'.and.type(i).ne.'D')then   
        DLEG = GCDISTANCE(RLAT(IC),RLON(IC),RLAT(IL),RLON(IL))  
        TOTAL = TOTAL+DLEG
        time=time+dleg/speed(i)+0.0167 
        subtotal = subtotal + dleg
        IF ((type(i).EQ.'Z').AND.(insert.NE.0)) THEN    !  jpd start
          dlegout = dleg / (insert + 1)
          time = time - dleg/speed(i)
          subtotal = subtotal - dleg
          newnt = nt + insert - 1
          dtype(i) = 1.0
          k = i + insert - 1
          DO j = newnt, k, -1  ! pal move points to leave space for inserts
            l = j - insert + 1
            rlat(j) = rlat(l)
            rlon(j) = rlon(l)
            type(j) = type(l)
            flag(j) = flag(l)
            dtype(j) = dtype(l)
            srrad(j) = srrad(l)
            srazm(j) = srazm(l)
            speed(j) = speed(l)
          ENDDO
          rlatinc = (rlat(k) - rlat(i-2))/FLOAT(insert + 1)
          rloninc = (rlon(k) - rlon(i-2))/FLOAT(insert + 1)
          temptime = time
          stormlat=slat+temptime*stormv/60.
          stormlon=slon+temptime*stormu/(60.*cos(deg2rad*stormlat))
          DO l = 1, insert
            j = i - 2 + l
            rlat(j) = rlat(i - 2) + FLOAT(l) * rlatinc
            rlon(j) = rlon(i - 2) + FLOAT(l) * rloninc
            xlat = rlat(j)  ! pal comments out to test Aug 3
            xlon = -rlon(j)  ! pal comments out to test Aug 3
            temprlat=stormlat+rlat(j)/60.
            temprlon=-(stormlon+rlon(j)/(60.*cos(deg2rad*temprlat)))
            dlegtime=gcdistance(temprlat,temprlon,rlat(j-1),rlon(j-1))
            temptime=time+dlegtime/speed(j - 1)  ! pal change i to j - 1
            type(j) = 'D'
            dtype(j) = 1.0
            srrad(j) = -99.
            flag(j) = 'I'  ! pal- all should be labelled 'I' not 'S' or 'D'
            time = time + dlegout / speed(j)  ! pal 
            subtotal = subtotal + dlegout  ! pal ? total = total + dleg ???
            CALL DGMN(rlat(j), lad, lam)
            CALL DGMN(rlon(j), lod, lom)
            CALL DGMN(time, ith, itm) 
            ldrop = ldrop + 1
            if((lturn.eq.1 .OR. ldrop.eq.1) .AND. ipset) then  !  jpd changed from “2” to “1” to set IP as waypoint #1: 6-10-2016
              iph=ith
              ipm=itm
              ipset=.false.
            endif
            write(6,*)' 1, i, type(i)=',i,type(i)
            write(6,501) rlat(j), rlon(j)
            WRITE(LUP, 501, err=9013) rlat(j), rlon(j)  ! output to points file
            IF (.NOT. globalhawk)
     +        WRITE(lufo, 161) lturn, flag(j), lad, lam, lod, lom,
     +          dlegout, subtotal, ith, itm
              WRITE(lut, 171) ldrop, flag(j), lad, lam, lod, lom,
     +          ith, itm 
              lturn = lturn + 1
          ENDDO  ! pal inserts
          dleg52 = 0  ! pal added for testing 10 aug 2016
          total = subtotal + dlegout  ! pal 
          time = time + dlegout / speed(j)  ! pal just added
          i = k
          insert = 0
          nt = newnt
          ipos(ns) = nt
        ENDIF  ! pal storm relative insertions

      else
        IF (iac.EQ.52 .OR. iac.EQ.57 .OR. iac.EQ.51 .OR. iac.EQ.50 .OR. 
     +    iac.EQ.49 .OR. iac.EQ.43 .OR. iac.EQ.42) THEN 
	  lturn = lturn - 1  ! pal sets to exclude interpolated points as WPs
        ENDIF
	DLEG = GCDISTANCE(RLAT(IC),RLON(IC),RLAT(IL),RLON(IL)) 
        dleg52 = dleg52+dleg
        TOTAL = TOTAL+DLEG
        subtotal = subtotal + dleg
        time=time+dleg/speed(i)
      endif
      IF (iac.eq.52) THEN  ! jpd  modified to add 30-min to the GH track after take-off (GH climb-out maneuvers)
        IF ((type(i).eq.'A')) THEN  
          climb=.true.
        endif
        IF ((type(i).eq.'Z')) THEN  ! jpd  modified to add 30-min to the GH track just before landing (GH decent maneuvers)
          time=time+0.5
          dleg=dleg+(335*0.5)
         total= total+(335*0.5)
        endif               
      endif
      CALL DGMN(RLAT(I),LAD,LAM)
      CALL DGMN(RLON(I),LOD,LOM)
      CALL DGMN(TIME,ITH,ITM) 
      IX = 0
      DO 155 L = 1,NS
        IF (IPOS(L).EQ.I) IX = L
155   CONTINUE
C     write(6,*)' 2, i, type(i)=',i,'/',type(i),'/'
      write(6,501) rlat(i), rlon(i)
      if(type(i).ne.'T')write(LUP,501,err=9013)rlat(i),rlon(i)
      if(type(i).eq.'T')write(LUP,500,err=9013)rlat(i),rlon(i),TEE
      if((lturn.eq.1 .OR. ldrop.eq.1) .AND. ipset) then  !  jpd changed from “2” to “1” to set IP as waypoint #1
        iph=ith
        ipm=itm
        ipset=.false.
      endif
      IF (.NOT.((TYPE(I).EQ.'D').AND.globalhawk)) THEN   ! pal
        dlegout = subtotal
        IF (iac.EQ.52 .OR. iac.EQ.57 .OR. iac.EQ.51 .OR. iac.EQ.50 .OR. 
     +    iac.EQ.49 .OR. iac.EQ.43 .OR. iac.EQ.42) THEN  ! pal sets to exclude interpolated points as WPs
          dleg=dleg+dleg52  ! pal comment out Aug 3
          dleg52=0  ! pal comment out Aug 3
        ENDIF
        IF ((LTURN.EQ.1).AND.(IAC.EQ.52)) THEN ! jpd adds to account for climb out dx to track for WP 1
          dleg=dleg+(335*0.5) 
          total= total+(335*0.5) 
        ENDIF
        IF (IX.NE.0) THEN
          WRITE(LUFO,160) LTURN,FLAG(I),NAME(IX),DLEG,TOTAL,ITH,ITM
        ELSE
          IF (SRRAD(I).GE.0.) THEN
            WRITE(LUFO,162) LTURN,FLAG(I),LAD,LAM,LOD,LOM,
     *        INT(SRRAD(I)),INT(SRAZM(I)),DLEG, TOTAL,ITH,ITM
          ELSE
            IF (globalhawk) THEN
              dleg = dleg*(inserted+1)  ! pal adds to track inserted legs
              inserted = 0
            ENDIF
            WRITE(LUFO,161) LTURN,FLAG(I),LAD,LAM,LOD,LOM,DLEG,
     *        TOTAL,ITH,ITM
          ENDIF
        ENDIF
        subtotal = 0.0
      ENDIF  ! pal
      IF (iac.eq.52 .AND. climb) THEN  ! jpd  modified to add 30-min to the GH track after take-off (GH climb-out maneuvers)
        time=time+0.5
        climb=.false.
      ENDIF
      IF(type(i).eq.' '.or.type(i).eq.'D'.or.type(i).eq.'E'
     1  .OR.type(i).eq.'S'.or.type(i).eq.'I')then
        LDROP = LDROP+1 
        if((lturn.eq.1 .OR. ldrop.eq.1) .AND. ipset) then  !  jpd changed from “2” to “1” to set IP as waypoint #1
          iph=ith
          ipm=itm
          ipset=.false.
        endif
        IF (SRRAD(I).GE.0.) THEN
          isrrad=srrad(i)
          israzm=srazm(i)
          write(lut,172)LDROP,flag(i),lad,lam,lod,lom,
     *      isrrad,israzm,ith,itm 
        ELSE
          WRITE(LUT,171) LDROP,FLAG(I),LAD,LAM,LOD,LOM,ITH,ITM 
        ENDIF
      ENDIF 
      IF (i.LT.nt) GOTO 150  ! pal getting out early?
      WRITE(LUFO,904)
C 
C     Now print table for drop locations
C     ----------------------------------
      WRITE(LUT,910)
c
c     Now print MTS information in drops and turns files
      WRITE(LUFO,8011)STMNAME,FNAME,IAC,STRINGA,TAKEOFF,STRINGZ,
     1  IPH,IPM,ITH,ITM
 8011 FORMAT(//,1X,72("*"),/,
     1 ' MISSION PLAN: ',a20,/,
     2 ' Prepared by the Hurricane Research Division File: ',A,/,
     3 ' Aircraft: N',i2,'RF',/,
     4 ' Proposed takeoff: ',A8,' ',A8,/,
     5 ' Proposed recovery: ',A8,/,
     6 ' Time to IP: ',I2,':'I2,/,
     7 ' Mission Duration: ',I2,':'I2,/,
     8 1X,72("*"))
C 
C     All done, return
C     ----------------
      CLOSE(LUFI)
      CLOSE(LUFO)
      CLOSE(LUT)
      RETURN
C 
900   WRITE(LUT,*) ' OPEN OR READ OR CITY ERROR IN SUBROUTINE TRACK'
      STOP '00'
9010  STOP '10'
9011  STOP '11'
9012  STOP '12'
9013  STOP '13'
9014  STOP '14'
      END 
C 
C2345678911234567892123456789312345678941234567895123456789612345678971X34567890
      SUBROUTINE DGMN(DDEG,IDEG,MIN)
      IDEG = ABS(INT(DDEG)) 
      MIN = NINT((ABS(DDEG)-IDEG)*60.)
      IF (MIN.EQ.60) THEN 
        MIN = 0 
        IDEG = IDEG+1.
      ENDIF 
      IF (DDEG.LT.0.) IDEG = -IDEG
      RETURN
      END 
C    --------------------------------------------------------
C2345678911234567892123456789312345678941234567895123456789612345678971X34567890
C     SONDELIB
C
C     Collection of subroutines relating to soundings.
C     This files has three main sections:  
C
C         1) Routines relating to Ooyama's spline filtering.
C         2) Mathematical or meteorological routines
C         3) Routines for message decoding
C
C     Recent revision history:
C
C     12/17/99:  Modified dropsonde TEMP DROP to HSA format
C                conversion routine to allow for decoding of:
C                  1) surface significant wind data following
C                     the "21212" indicator
C                  2) extrapolated level data following both
C                     the "51515" indicator and doubtful
C                     temperature or height groups (SEF)
C
C     ----------------------------------------------------------------- 
      SUBROUTINE STMREL(DAYMN,DAYOB,GMTOB,XD,YD,NFD,DAYSTM,GMTSTM,
     *                  XSTM,YSTM,DAY0,GMT0)
C     ----------------------------------------------------------------- 
C 
C 
      DIMENSION DAYSTM(NFD),GMTSTM(NFD),XSTM(NFD),YSTM(NFD) 
      DIMENSION DTFIX(500) 
      LOGICAL MATCH 
C 
      MATCH=.FALSE. 
      NFIX = NFD
C 
C 
C     Find storm position at reference time 
C     ------------------------------------- 
C 
      DO 110 L=1,NFIX 
              IF (DAY0.EQ.DAYSTM(L).AND.GMT0.EQ.GMTSTM(L)) THEN 
                 XSTM0=XSTM(L)
                 YSTM0=YSTM(L)
                 MATCH=.TRUE. 
                 GOTO 120 
                 ENDIF
110           CONTINUE
120   IF (.NOT.MATCH) THEN
              WRITE(6,'("*** BAD REFERENCE TIME IN STMREL ***")') 
              STOP
              ENDIF 
C 
C 
200   CALL XTRTIME(DAYMN,000.0,IYMN,IMMN,IDMN,HRMN) 
      DO 210 N=1,NFIX 
        CALL XTRTIME(DAYSTM(N),GMTSTM(N),IYR,IMO,IDA,HR)
        CALL DIFTIME(IYMN,IMMN,IDMN,HRMN,IYR,IMO,IDA,HR,DTFIX(N))   
210     CONTINUE
C 
C 
      CALL XTRTIME(DAYOB,GMTOB,IYR,IMO,IDA,HR)
      CALL DIFTIME(IYMN,IMMN,IDMN,HRMN,IYR,IMO,IDA,HR,DTOB) 
      NFIX1=NFIX-1
C 
      DO 300 N=1,NFIX1
        NF=N
        IF (DTOB.GE.DTFIX(N) .AND. DTOB.LE.DTFIX(N+1)) GOTO 310 
300     CONTINUE
C 
      WRITE(6,301) DAYOB,GMTOB
301   FORMAT(' **** CANNOT INTERPOLATE FIXES FOR ',I6,1X,I4,' ****',/,
     *       ' **** PROGRAM ABORTING ****',//)
      STOP
C 
C 
310   DT=DTFIX(NF+1)-DTFIX(NF)
      IF (ABS(DT) .LT. 1.E-4) GOTO 910
      RAT = (DTOB-DTFIX(NF))/(DTFIX(NF+1)-DTFIX(NF))
      XSTOB = XSTM(NF)+(XSTM(NF+1)-XSTM(NF))*RAT
      YSTOB = YSTM(NF)+(YSTM(NF+1)-YSTM(NF))*RAT
      XD = XD - XSTOB + XSTM0 
      YD = YD - YSTOB + YSTM0       
      RETURN
C 
C 
C     Errors
C     ------
C 
910   WRITE(6,911) NF,DTFIX(NF),DTFIX(NF+1) 
911   FORMAT("**** FIX TIMES ARE EQUAL IN STMREL***",I5,2F15.5) 
      STOP
      END 
C 
C 
C 
C     --------------------------------------------
      SUBROUTINE XTRTIME(DATE,GMT,IYR,IMO,IDA,HR) 
C     --------------------------------------------
C 
      IYR=INT(DATE/10000.)
      IMO=INT((DATE-FLOAT(IYR)*10000.)/100.)
      IDA=INT(DATE-FLOAT(IYR)*10000.-IMO*100.)
      HR=FLOAT(NINT(GMT/100.-0.3))
      HR=HR+(GMT-HR*100.)/60. 
      RETURN
      END 
C 
C 
C 
C     --------------------------------------------------------
      SUBROUTINE DIFTIME(IYZ,IMZ,IDZ,HRZ,IYX,IMX,IDX,HRX,DHR) 
C     --------------------------------------------------------
C 
      DIMENSION MODA(12)
      DATA MODA/31,28,31,30,31,30,31,31,30,31,30,31/
C 
      DHR=0.0 
      IDZZ=IDZ
      IMZZ=IMZ
      HRZZ=HRZ
C 
      IF(IYZ.EQ.IYX) GOTO 100 
      DHR=24.-HRZZ+FLOAT((MODA(IMZZ)-IDZZ)*24+MAX0(IYX-(IYZ+1),0)*8760) 
      IMZZ=MOD(IMZZ+1,12) 
      IDZZ=1
      HRZZ=0.0
C 
      IF (IMZZ.EQ.1) GOTO 100 
      DO  I=IMZZ,12 
        DHR=DHR+FLOAT(MODA(I)*24) 
      END DO
      IMZZ=1
C 
100   IF (IMZZ.EQ.IMX) GOTO 200 
      DHR=DHR+FLOAT((MODA(IMZZ)-IDZZ)*24)+24.-HRZZ
      IMZZ1=IMZZ+1
C 
      IF (IMZZ1 .GE. IMX) GOTO 120
      IMX1=IMX-1
      DO I=IMZZ1,IMX1 
        DHR=DHR+FLOAT(MODA(I)*24) 
      END DO
C 
120   IMZZ=IMZZ1
      IDZZ=1
      HRZZ=0.0
C 
200   DHR=DHR+FLOAT((IDX-IDZZ-1)*24)+24.-HRZZ+HRX 
      RETURN
      END 
C 
C 
C 
C     --------------------------------------------------
      SUBROUTINE DAYCMPR(FLID,DATE,TODAY,TOMORROW)
C     --------------------------------------------------
C 
C 
      DIMENSION IMONTH(12)
      LOGICAL TODAY,TOMORROW
      DATA IMONTH/31,28,31,30,31,30,31,31,30,31,30,31/
C     
      TODAY = .FALSE. 
      TOMORROW = .FALSE.
C 
C     SEPERATE DATES INTO YEAR,MONTH,DAY
C     ----------------------------------
C 
      IYR = INT(FLID/10000.)
      IMN = INT(FLID/100.) - (IYR*100.) 
      IDY = FLID - (IYR*10000.) - (IMN*100.)
      JYR = INT(DATE/10000.)
      JMN = INT(DATE/100.) - (JYR*100.) 
      JDY = DATE - (JYR*10000.) - (JMN*100.)
      IF (MOD(IYR,4).EQ.0) IMONTH(2) = 29 
C 
C     FIND IF SAME DAY OR NEXT
C     ------------------------
C 
      IF (FLID.EQ.DATE) TODAY = .TRUE.
      IF (FLID+1.EQ.DATE) TOMORROW = .TRUE. 
C 
C     CHECK FOR MONTHLY OR YEARLY FLIP OVERS
C     --------------------------------------
C 
      IF (JDY.EQ.1.AND.IDY.EQ.IMONTH(IMN)) THEN 
        IF (JMN.EQ.IMN+1) TOMORROW = .TRUE. 
        IF (JMN.EQ.1.AND.IMN.EQ.12) THEN
          IF (JYR.EQ.IYR+1) TOMORROW = .TRUE. 
          ENDIF 
        ENDIF 
      RETURN
      END 
C
C
C
C     --------------------------------------------------------
      SUBROUTINE LOPASS(GIN,GOUT,NMAX,FRAC,NTRM)
C     Complete symmetric filter with multiplicative adjustment
C     --------------------------------------------------------
C 
C 
      PARAMETER(MTERMS=100) 
      DIMENSION GIN(NMAX),GOUT(NMAX),WT(MTERMS) 
      DATA PI/3.1415926535/ 
      IPRT=1
      NTRM1=NTRM+1
      NMAX1=NMAX-NTRM 
      OMCUT=PI*FRAC 
      WTO=FRAC
C 
C     Endpoints 
C     --------- 
C 
      GOUT(1)=GIN(1)
      GOUT(NMAX)=GIN(NMAX)
C 
C 
      DO 40 L=2,NMAX-1
              SUM=0.0 
              MTRM=NTRM 
              IF(L.LE.NTRM) MTRM=L-1
              IF(L.GE.NMAX+1-MTRM) MTRM=NMAX-L    !FIX 2/17/88
C 
C     Calculate Weights 
C     ----------------- 
C 
      WTO = FRAC
      WFSUM = 0.0 
      DO 20 N=1,MTRM
              FAC=FLOAT(N)*2.*PI/(2.*MTRM+1)
              WT(N)=SIN(FLOAT(N)*OMCUT)/(PI*FLOAT(N))*SIN(FAC)/FAC
              WFSUM=WFSUM+2.0*WT(N) 
20            CONTINUE
C 
      WFSUM=WFSUM+WTO 
      WTO=WTO/WFSUM 
C 
      DO N=1,MTRM
        WT(N)=WT(N)/WFSUM 
      END DO
      DO N=1,MTRM
        SUM=SUM+(GIN(L-N)+GIN(L+N))*WT(N) 
      END DO
              GOUT(L)=SUM+GIN(L)*WTO    
40            CONTINUE
      RETURN
      END 
C 
C 
C 
C     --------------------------------------------
      SUBROUTINE GAP(X,NT,IBGN,IEND,NGAPS,BAD)
C     --------------------------------------------
C 
      PARAMETER (MXGPS = 100) 
C 
      DIMENSION X(NT),IBGN(MXGPS),IEND(MXGPS) 
      LOGICAL LAST
C
      COMMON /PARAM/ LUT,LUFI,LUFX,LUFW,LUFP,LUFO,LUPR,
     *               NPLTFORM,NSNDTYPE,FALLRATE,DATARATE
C 
      IGAP=0
      LAST=.FALSE.
      NGAPS=0 
C 
      DO 5 L=1,MXGPS
         IBGN(L)=0
         IEND(L)=0
5        CONTINUE 
C 
      DO 10 L=1,NT
      IF (X(L).GT.BAD) THEN
              IF (LAST) GO TO 10
              IGAP=IGAP+1 
              IBGN(IGAP)=L
              LAST=.TRUE. 
              GO TO 10
      ELSE
              IF(.NOT.LAST) GO TO 10
              IEND(IGAP)=L-1
              LAST=.FALSE.
              GO TO 10
      ENDIF 
10    CONTINUE
C 
      IF(X(NT).GT.BAD) IEND(IGAP)=NT 
      NGAPS=IGAP-1
      IF (NGAPS.LT.0) NGAPS=0 
C
      IF (NGAPS.GT.MXGPS) THEN
         WRITE(LUT,'(/,
     *   " *** FATAL ERROR: TOO MANY GAPS IN SOUNDING ***")')
         STOP
         ENDIF
C
      RETURN
      END 
C
C
C
      FUNCTION GCDISTANCE(YY,XX,BB,AA)
      DOUBLE PRECISION X,Y,A,B
      PI = ACOS(-1.0)
      REARTHKM = 6378.163
      REARTHNM = REARTHKM*60./111.12
      IF (YY.EQ.BB .AND. XX.EQ.AA) THEN
              GCDISTANCE = 0.
              RETURN
              ENDIF
      Y=YY*2.0*PI/360.
      X=XX*2.0*PI/360.
      A=AA*2.0*PI/360.
      B=BB*2.0*PI/360.
      ANGLE=DACOS(DCOS(A-X)*DCOS(Y)*DCOS(B)+DSIN(Y)*DSIN(B))
      GCDISTANCE=ANGLE*REARTHNM
      RETURN
      END
C
C
C
C     ------------------------------------------------
      SUBROUTINE UPPERCASE(CH)
C
C     Converts to upper case.
C     ------------------------------------------------
C
      CHARACTER*1 CH
C
      I = ICHAR(CH)
      IF (I.LT.97 .OR. I.GT.122) RETURN
      IUPPER = I-32
      CH = CHAR(IUPPER)
      RETURN
      END
C
C
C
C     ------------------------------------------------
      SUBROUTINE LOWERCASE(CH)
C
C     Converts to lower case.
C     ------------------------------------------------
C
      CHARACTER*1 CH
C
      I = ICHAR(CH)
      IF (I.LT.65 .OR. I.GT.90) RETURN
      ILOWER = I+32
      CH = CHAR(ILOWER)
      RETURN
      END
C
C
C
C     --------------------------------------------
      FUNCTION AZIMUTH(VLAT,VLON)
C     --------------------------------------------
      RAD = 3.14156/180.
      AZIMUTH = ATAN2(VLON,VLAT)/RAD
      CALL DEG360(AZIMUTH)
      RETURN
      END
C
C
C
C     --------------------------------------------
      SUBROUTINE DEG360(ANGLE) 
C     --------------------------------------------
100   IF (ANGLE.GT.360.) THEN 
         ANGLE = ANGLE -360. 
         GO TO 100 
         ENDIF 
      IF (ANGLE.LT.0.) THEN 
         ANGLE = ANGLE + 360.
         GO TO 100 
         ENDIF 
      RETURN
      END 
C
C
C
C     --------------------------------------------------------
      SUBROUTINE SRLATLON(SLAT,SLON,THETA,R,YLAT,YLON)
C
C     Computes lat and lon of an r,theta position relative to
C     a target.  East longitude is positive, R in km.
C     --------------------------------------------------------
C
      DOUBLE PRECISION B, COLAT, THETARAD, COSA, A, SINB, DEG2RAD
      PARAMETER (PI = 3.141592654)
      PARAMETER (RE = 6371.23)
C
      GOTO 200
100   DEG2RAD = PI/180.
      COLAT = 90.-SLAT
      B = R/RE
      THETARAD = THETA*DEG2RAD
      COLAT = COLAT*DEG2RAD
      COSA = DCOS(B)*DCOS(COLAT) + DSIN(B)*DSIN(COLAT)*DCOS(THETARAD)
      A = DACOS(COSA)
      YLAT = 90.-(A/DEG2RAD)
      SINB = DSIN(THETARAD)*DSIN(B)/DSIN(A)
      B = DASIN(SINB)/DEG2RAD
      YLON = SLON+B
      RETURN
C
200   DX = R*COS((90.-THETA)*PI/180.)
      DY = R*SIN((90.-THETA)*PI/180.)
      SLATR = SLAT * PI/180.
      KMDEGLAT = 111.13209 - 0.56605 * cos(2.*SLATR) +
     *           0.00012*cos(4.0*SLATR)-
     *           0.000002*cos(6.*SLATR)
      KMDEGLON = 111.41513*cos(SLATR)-0.09455*cos(3.*SLATR)+
     *           0.00012*cos(5.*SLATR)
      YLAT = SLAT + DY/KMDEGLAT
      YLON = SLON + DX/KMDEGLON
C
      DIST = GCDISTANCE(SLAT,SLON,YLAT,YLON)
      DIST = 111.12*DIST/60.
      IF (DIST.EQ.0.) RETURN
      SCALE = R/DIST
      DX = DX*SCALE
      DY = DY*SCALE
      YLAT = SLAT + DY/KMDEGLAT
      YLON = SLON + DX/KMDEGLON
      RETURN
      END
C
C
C
C     --------------------------------------------------------
C     Section 3: Routines relating to message decoding.
C     --------------------------------------------------------
C
C
c     -------------------------------------------------
      subroutine drop(lu,iwx,iflag,iyrs,imns,idys,line)
c
c     decodes tempdrop message into spline format
c     -------------------------------------------------
c
      character*1 clat,clon
      character*2 header(12)
      character*30 blank
      character*70 line
      character*80 dropl(200)
      character*200 remark
      dimension prs(12)
      logical plev(12),knots,skpwind
c
      common /dropdata/idropl,dropl
c
      data header /'99','00','92','85','70','50','40','30',
     *             '25','20','15','10'/
      data prs /1070.,1000.,925.,850.,700.,500.,400.,300.,
     *          250.,200.,150.,100./
      data plev /12*.false./
      data blank /'                             '/
c
      idropl = 0
      ihhmm = 9999
      lvl=0
      sfcp = 9999.
      splat = -999.
      splon = -999.
c
c     ------------------------------------------------
c     if iflag=1 then we are already at the XXAA line
c     ------------------------------------------------
c
c     read the line with the mission number
c     -------------------------------------
 10   if (iflag.ne.1) then
         read(12,'(a)',end=99)
         if(line(1:30).eq.blank)goto 10
         endif
c
c     read the first data line
c     ------------------------
 40   if (iflag.ne.1) then
         read(12,'(a)',end=99)line
         if(line(1:30).eq.blank)goto 40
         endif
c
c
c     read the value of the day
c     check if the winds are in knots or m/s
c     --------------------------------------
      read (line(7:8),'(i2)') iday
      if (iday.gt.50) then
         iday=iday-50
         knots = .true.
         else
         knots = .false.
         endif
c
c     check for month, year flips
c     ---------------------------
      yy = iyrs
      mm = imns
      if (iday.lt.idys) then
         mm = imns+1
         if (mm.eq.13) then
            mm = 1
            yy = iyrs + 1.
            if (yy.eq.100.) yy = 00.
            endif
         endif
c
      yymmdd = yy * 10000. + float(mm) * 100. + float(iday)
c
c     read the value of the hour
c     --------------------------
      read (line(9:10),'(i2)') ihour
      igmt = ihour * 100.
c
c     set the value of the highest mandatory wind level reporting
c     -----------------------------------------------------------
      if(line(11:11).eq.'/')then
        maxlvl=1
      elseif(line(11:11).eq.'0')then
        maxlvl=2
      elseif(line(11:11).eq.'9')then
        maxlvl=3
      elseif(line(11:11).eq.'8')then
        maxlvl=4
      elseif(line(11:11).eq.'7')then
        maxlvl=5
      elseif(line(11:11).eq.'5')then
        maxlvl=6
      elseif(line(11:11).eq.'4')then
        maxlvl=7
      elseif(line(11:11).eq.'3')then
        maxlvl=8
      elseif(line(11:11).eq.'2')then
        maxlvl=10
      elseif(line(11:11).eq.'1')then
        maxlvl=12
      endif
c
c     read the latitude
c     -----------------
      read (line(15:17),'(f3.1)') alat
c
c     read the quadrant,longitude
c     ---------------------------
      read (line(19:23),'(i1,f4.1)') nquad,alon
c
c     Assign negative sign to east or south (HSA convention)
c     ------------------------------------------------------
      if (nquad.eq.1 .or. nquad.eq.3) alon = -alon
      if (nquad.eq.5 .or. nquad.eq.3) alat = -alat
c
c     go to column 31 to read the surface group
c     -----------------------------------------
      itag=31
c
c
c     Go on to next mandatory level
c     --------------------------------------------
200   do 205 l = 1,12
         plev(l)=.false.
205      continue
c
c     count the number of the mandatory level
c     ---------------------------------------
      lvl=lvl+1
c
c     check to see if 925 level is missing
c     ------------------------------------
      if (lvl.eq.3 .and. line(itag:itag+1).eq.'85') lvl=lvl+1
c
c     return point for trop and max wind levels
c     -----------------------------------------
210   press = -99.
      temp = -99.
      rh = -99.
      geopot = -99.
      wdir = -99.
      wspd = -99.
      skpwind = .false.
c
      if(line(itag:itag+1).eq.header(lvl))then
        plev(lvl)=.true.
        press=prs(lvl)
        call geo (line,itag,plev,geopot,sfcp)
        itag=itag+6
        call tagtst(itag,line)
        pressx = press
        if (press.eq.1070. .and. sfcp.le.1070.) pressx = sfcp
        call temdew (line,itag,pressx,temp,rh)
        if(lvl.le.maxlvl)then
          itag=itag+6
          call tagtst(itag,line)

c         check if sfc wind group is missing
c         ----------------------------------
          if (lvl.eq.1 .and. line(itag:itag+1) .eq. '00' .and.
     *        line(itag+6:itag+7).ne.'00') then
             skpwind = .true.
             else
             call wind (line,itag,wdir,wspd,*99)
             call dstouv (wdir,wspd,alat,alon,knots)
             skpwind = .false.
             endif
c
          endif
        if (temp .ne. -99. .or. rh .ne. -99. .or. geopot .ne. -99.
     1        .or. wdir .ne. -99. .or. wspd .ne. -99.) call out
     2        (lu,iwx,yymmdd,igmt,alat,alon,press,temp,rh,geopot,wdir,
     3         wspd,1)
        if (.not. skpwind) then
           itag=itag+6
           call tagtst(itag,line)
           endif
        go to 200
c
c
c     decode tropopause
c     -----------------
      elseif (line(itag:itag+1) .eq. '88') then
        if(line(itag+2:itag+4).eq.'999')then
          itag=itag+6
          call tagtst(itag,line)
          goto 210
          endif
        geopot = -99.
        read (line(itag+2:itag+4),'(f3.0)') press
        itag=itag+6
        call tagtst(itag,line)
        pressx = press
        call temdew (line,itag,pressx,temp,rh)
        itag=itag+6
        call tagtst(itag,line)
        call wind (line,itag,wdir,wspd,*99)
        call dstouv (wdir,wspd,alat,alon,knots)
        if (temp .ne. -99. .or. rh .ne. -99. .or. 
     1     wdir .ne. -99. .or. wspd .ne. -99.) call out (lu,
     2     iwx,yymmdd,igmt,alat,alon,press,temp,rh,geopot,wdir,
     3     wspd,7)
        itag = itag+6
        call tagtst(itag,line)
        goto 210
c
c     decode max wind level
c     ---------------------
      elseif (line(itag:itag+1) .eq. '77' .or.
     1  line(itag:itag+1) .eq. '66') then
        if(line(itag+2:itag+4).ne.'999')then
          read (line(itag+2:itag+4),'(f3.0)') press
          itag=itag+6
          call tagtst(itag,line)
          call wind (line,itag,wdir,wspd,*99)
          call dstouv (wdir,wspd,alat,alon,knots)
          if (temp .ne. -99. .or. rh .ne. -99. .or. geopot .ne. -99.
     1          .or. wdir .ne. -99. .or. wspd .ne. -99.) call out (lu,
     2          iwx,yymmdd,igmt,alat,alon,press,temp,rh,geopot,wdir,
     3          wspd,6)
        endif
c
      endif
c
c
c     end of part A decoding.  Now look for significant level data.
c     -------------------------------------------------------------
60    read(12,'(a)',end=99)line
c
c     check if the line has data or duplicates part B
c     decode splash location.
c     -----------------------------------------------
      if(line(1:30).eq.blank) goto 60
      if(line(1:5).eq.'31313') goto 60
      if(line(1:5).eq.'51515') goto 60
      if(line(1:5).eq.'61616') goto 60
      if(line(1:5).eq.'62626') then
         remark = ' '
         itag = 1
         ix = 0
62       ix = ix+1
         if (line(itag:itag).eq.'=') goto 63
         remark(ix:ix) = line(itag:itag)
         itag = itag+1
         if (itag.eq.66) then
            read(12,'(a)',end=99) line
            itag=1
            if (line(1:4).eq.'XXBB') goto 63
            endif
         goto 62
c
63       do 65 i=1,ix
            if (remark(i:i+3).eq.'SPL ') then
               read(remark(i+4:i+14),'(2i2,a1,i3,i2,a1)')
     *         ilat1,ilat2,clat,ilon1,ilon2,clon
               splat = float(ilat1)+float(ilat2)/100.
               if (clat.eq.'S') splat = -splat
               splon = float(ilon1)+float(ilon2)/100.
               if (clon.eq.'E') splon = -splon
               endif
65          continue
c
         do 66 i=1,ix
            if (remark(i:i+7).eq.'DLM WND ') then
               itagr = 9
               call wind (remark(i:i+20),itagr,wdir,wspd,*99)
               call dstouv (wdir,wspd,alat,alon,knots)
               read(remark(i+14:i+19),'(2i3)') ip1,ip2
               if (ip1.lt.ip2) ip1 = ip1+1000
               geopot = float(ip1+ip2)/2.0
               press = 1099.
               temp = -99.
               rh = -99.
               if (wdir .ne. -99. .or. wspd .ne. -99.) call out (lu,
     2          iwx,yymmdd,igmt,alat,alon,press,temp,rh,geopot,wdir,
     3          wspd,8)

               endif
66          continue
c
         if (line(1:4).ne.'XXBB') goto 60
         endif
c
c     check significant level data
c     ----------------------------
 75   if(line(1:4).eq.'XXBB')then
        itag=31
c
c     added check for case of no sigt/h data -- jlf 1/98
c     --------------------------------------------------
        call tagtst(itag,line)
        if(line(itag:itag+4).eq.'21212' .or.
     1     line(itag:itag+4).eq.'31313') goto 75
c
        ihead=-11
 70     ihead=ihead+11
        if(ihead.gt.99)ihead=11
        read(line(itag:itag+1),'(i2)')jhead
        if(jhead.eq.ihead)then
          if(line(itag+2:itag+4).eq.'///') then
             itag=itag+6
             call tagtst(itag,line)
             itag=itag+6          
             goto 71
             endif
          read(line(itag+2:itag+4),'(i3)')iprs
          if(iprs.lt.100)iprs=iprs+1000.
          press=iprs
          itag=itag+6
          call tagtst(itag,line)
          call temdew (line,itag,press,temp,rh)
          geopot = -99.
          wdir = -99.
          wspd = -99.
          if (temp .ne. -99. .or. rh .ne. -99. .or. geopot .ne. -99.
     1          .or. wdir .ne. -99. .or. wspd .ne. -99.) call out (lu,
     2          iwx,yymmdd,igmt,alat,alon,press,temp,rh,geopot,wdir,
     3          wspd,2)
          itag=itag+6
 71       call tagtst(itag,line)
c
          if(line(itag:itag+4).eq.'21212' .or.
     1       line(itag:itag+4).eq.'31313')goto 75
          goto 70
        endif
c
c     decode signficant wind levels--added surface decode--SEF 12/17/99
c     -----------------------------------------------------------------
      elseif(line(itag:itag+4).eq.'21212')then
        itag=itag+6
        ihead=-11
 30     ihead=ihead+11
        if(ihead.gt.99)ihead=11
        read(line(itag:itag+1),'(i2)',err=75)jhead
        if(jhead.eq.ihead)then
          if(line(itag+2:itag+4).eq.'///') then
             itag=itag+6
             call tagtst(itag,line)
             itag=itag+6
             goto 31
          endif
          read(line(itag+2:itag+4),'(i3)')iprs
          if(iprs.lt.100)iprs=iprs+1000.
          press=iprs
          itag=itag+6
          call tagtst(itag,line)
          call wind (line,itag,wdir,wspd,*99)
          call dstouv (wdir,wspd,alat,alon,knots)
          temp=-99.
          rh=-99.
          geopot=-99.
          if (temp .ne. -99. .or. rh .ne. -99. .or. geopot .ne. -99.
     1          .or. wdir .ne. -99. .or. wspd .ne. -99.) call out (lu,
     2          iwx,yymmdd,igmt,alat,alon,press,temp,rh,geopot,wdir,
     3          wspd,2)
          itag=itag+6
          call tagtst(itag,line)
 31       goto 30
        endif
        goto 75
c
      elseif(line(itag:itag+4).eq.'31313')then
        read (line(itag+13:itag+16),'(i4)') ihhmm
        itag = itag+19
        call tagtst(itag,line)
        goto 75
c
c     decode extrapolated levels in additional data groups
c     ----------------------------------------------------
      elseif(line(itag:itag+4).eq.'51515')then
        itag = itag+6
        call tagtst(itag,line)
500     if (line(itag:itag+4).eq.'10190') then
          itag = itag+6
          call tagtst(itag,line)
          do 505 l = 1,12
            plev(l)=.false.
505         continue
          press = -99.
          temp = -99.
          rh = -99.
          geopot = -99.
          wdir = -99.
          wspd = -99.
c
          do 510 l = 1,12
            if(line(itag:itag+1).eq.header(l))then
               plev(l)=.true.
               press=prs(l)
               call geo (line,itag,plev,geopot,sfcp)
               if (geopot .ne. -99.) call out(lu,iwx,yymmdd,igmt,
     *         alat,alon,press,temp,rh,geopot,wdir,wspd,3)
               endif
510         continue
          itag = itag+6
          call tagtst(itag,line)
          goto 500
c
c       added loop to re-check for extrapolated levels if doubtful
c       temperature or height groups first appear--SEF 12/17/99
c       ----------------------------------------------------------
	elseif(line(itag:itag+3) .eq. '1016') then
	  itag = itag + 12
	  call tagtst (itag,line)
	  goto 500
        endif
c
      elseif(line(1:4).eq.'NNNN')then
        call dropout(lu,ihhmm,splat,splon)
        return
c
      elseif(line(1:5).eq.'Sonde')then
        call dropout(lu,ihhmm,splat,splon)
        return
      endif
c
 99   call dropout(lu,ihhmm,splat,splon)
      return
      end
c
c
c
      subroutine uvcomp (dir,spd)
c 
c     this subroutine changes dir to u, and spd to v, where dir is
c     given in meteorological degrees.  The original values of dir
c     and spd are destroyed.
c
      degrad = atan(1.0) / 45.
      dirdg = 270.0 - dir
      if (dirdg .lt. 0.0) dirdg = dirdg + 360.
      dirrd = dirdg * degrad
      dir = spd * cos(dirrd)
      spd = spd * sin(dirrd)
      return
      end
c
c
c
      subroutine uvcomp2(dir,spd,u,v)
      degrad=atan(1.)/45.
      dirdg=270.-dir
      if(dirdg.lt.0.)then
        dirdg=dirdg+360.
      endif
      dirrd=dirdg*degrad
      u=spd*cos(dirrd)
      v=spd*sin(dirrd)
      return
      end
c
c
c
      subroutine temdew (line,lptr,press,temp,rh)
      character*70 line
c
c     extract the temperature
c
      temp = -99.
      rh = -99.
c
      if (line(lptr:lptr+2) .ne. '///') then
        read (line(lptr:lptr+2),'(f3.1)') atemp
        read (line(lptr+2:lptr+2),'(i1)') ifrac
        if (mod(ifrac,2) .eq. 0) then
          temp = atemp
        else
          temp = -atemp
        endif
      endif
c
c     extract the dewpoint depression
c
      if (line(lptr+3:lptr+4) .ne. '//') then
        read (line(lptr+3:lptr+4),'(i2)') idd
        if (idd .gt. 50) then
          dd = float (idd - 50)
        else
          dd = float (idd) / 10.
        endif
        dewpt = temp - dd
        call relhum (press,temp,dewpt,rh)
      endif
      return
      end
c
c
c
      subroutine relhum (press,temp,dewpt,rh)
      parameter (tkelvn = 273.16)
      parameter (em = 9.4051)
      parameter (e = 2353.)
c
c     compute the relative humidity using the vapor pressure vprs
c     and the saturation vapor pressure svprs
c
      vprs = 10**(em - e / (dewpt + tkelvn))
      svprs = 10**(em - e / (temp + tkelvn))
      fmixr = vprs / (press - vprs)
      smixr = svprs / (press - svprs)
      rh = 100. * fmixr / smixr
      if(rh.gt.100.)rh=100.
      return
      end
c
c
c
      subroutine geo (line,lptr,plev,geopot,sfcp)
      character*70 line
      logical plev
      dimension plev(12)
c
c     extract the geopential height (modifications by JLF 11/92)
c
      if (line(lptr+2:lptr+4) .ne. '///') then
        read (line(lptr+2:lptr+4),'(f3.0)') geopot 
c
        if (plev(1)) then                          ! Surface
          if (geopot .lt. 100.) geopot = geopot + 1000.
          sfcp = geopot
          endif
c
        if (plev(2)) then                          ! 1000 mb
          if (geopot .ge. 500.) geopot = -(geopot-500.)
          endif
c
        if (plev(3)) then
          if (sfcp.le.925..and.geopot.ge.500.) geopot=-(geopot-500.)
          endif
c
        if (plev(4)) then
          geopot = geopot+1000.
          if (sfcp.le.950..and.geopot.gt.1500.) geopot=geopot-1000.
          endif
c
        if (plev(5)) then                          ! 700 mb
          add = 2000.
          if (geopot .lt. 500.) add = 3000.
          if (sfcp.lt.960.) add = 2000.    
          geopot = geopot + add
          endif
c
        if (plev(6) .or. plev(7)) then             ! 500, 400 mb
          geopot = geopot * 10.
          endif
c
        if (plev(8) .or. plev(9) .or. plev(10)     ! >= 300 mb
     *      .or. plev(11) .or. plev(12)) then
          geopot = geopot * 10.
          if (geopot.lt.8500.) geopot = geopot + 10000.
          endif
c
      endif
      return
      end
c
c
c
      subroutine wind (line,lptr,wdir,wspd,*)
      character*70 line
c
c     extract the wind direction and speed
c
      if (line(lptr:lptr+4) .ne. '/////') then
        read (line(lptr:lptr+1),'(f2.0)') wdir
        read (line(lptr+2:lptr+4),'(f3.0)') wspd
      else
        wdir = -99.
        wspd = -99.
      endif
      return
      end
c
c
c
      subroutine dstouv (wdir,wspd,alat,alon,knots)
      logical knots
      real alat,alon,wdir,wspd
c
c     convert wind direction and speed to u, v (in m/s)
c
      if (wdir .ne. -99.) then
        wdir = wdir * 10.
        if(wspd.ge.500.0)then
          wspd=wspd-500.
          wdir=wdir+5
        endif
        if (knots) wspd = 0.514444 * wspd
        call uvcomp (wdir,wspd)
      endif
      return
      end
c
c
c
      subroutine tagtst(itag,line)
      character*70 line
c
c     check if the end of the line has been reached and the next line should 
c     be read
c  
c      if(itag.gt.47)then
        if(itag.lt.66)then
          do i=itag,itag+5
            if(line(i:i).ne.' ')return
          end do
        endif
        read(12,'(70a)',end=99)line
        itag=1
c     endif
 99   return
      end
c
c
c
      subroutine recco(lu,iyrs,imns,idys,line)
      character*1 quad
      character*30 blank
      character*70 line
      logical knots
      data blank /'                             '/
c
c
c     Read the day
c     ------------
      read(line(13:14),'(i2)') iday
      if(line(13:13).eq.' ') read (line(14:15),'(i2)') iday
      if (iday.gt.50) then
         iday=iday-50
         knots = .true.
         else
         knots = .false.
         endif
c
c     check for month, year flips
c     ---------------------------
      yy = iyrs
      mm = imns
      if (idys.gt.27.and.iday.eq.1) then
         mm = imns+1
         if (mm.eq.13) then
            mm = 1
            yy = iyrs + 1.
            if (yy.eq.100.) yy = 00.
            endif
         endif
c
      yymmdd = yy * 10000. + float(mm) * 100. + float(iday)
c
c
c     read the next line
c
 20   read(12,'(a)',end=99)line
c
c     check if the line has information
c
      if(line(1:30).eq.blank)goto 20
c
c     define the data type
c
      if(index(line,'AF').ne.0)iwx=6
      if(index(line,'NOAA').ne.0)iwx=3
c
c     read the data line
c
 10   read(12,'(a)',end=99)line
c
c     if line is NNNN return
c
      if(index(line,'NNNN').ne.0)return
c
c     check if the line has data
c
      if(line(1:30).eq.blank)goto 10
c
c     recco's begin with 97779 or 95559
c
      if (line(1:5) .eq. '97779'.or. line(1:5).eq.'95559') then
        read (line(7:8),'(i2)') ihour
        read (line(9:10),'(i2)') min
        quad = line(14:14)
        read (line(15:17),'(f3.1)') alat
        read (line(19:21),'(f3.1)') alon
        if (quad .eq. '1' .and. line(19:19) .ne. '9')alon = alon + 100.
        read (line(31:32),'(f2.0)') wdir
        read (line(33:35),'(f3.0)') wspd
        if(wspd.ge.500.0)then
          wspd=wspd-500.
          wdir=wdir+0.5
        endif
        call rtmdew (line,temp,dewpt,*99)
        call rpress (line,press,geopot)
        call relhum (press,temp,dewpt,rh)
        call rdstuv (wdir,wspd,alat,alon)
        igmt = float(ihour) * 100. + min
        call out (lu,iwx,yymmdd,igmt,alat,alon,press,
     1            temp,rh,geopot,wdir,wspd,4)
        go to 10
      endif
 99   return
      end
c
c
c
      subroutine rtmdew (line,temp,dwpt,*)
      character*70 line,line2
c
c     extract the temperature from the RECCO
c
      read (line(37:38),'(f2.0)',err=99) atemp
      if (atemp .lt. 50.) then
        temp = atemp
      else
        atemp = atemp - 50.
        temp = -atemp
        endif
c
c     if the dewpoint is missing, it may be in plain text on line 2
c
      if (line(39:40) .eq. '//') then
        read (12,'(a)',end=99) line2
        if (line2(1:3) .eq. 'DEW') then
          read (line2(11:15),'(f5.1)') dewpt
        else
          dewpt = 0.0
          endif
        dwpt = dewpt
        go to 20
        endif
c
c     otherwise, get the dewpoint from the main line
      read (line(39:40),'(f2.0)',err=99) dewpt
      if (dewpt .lt. 50.) then
        dwpt = dewpt
      else
        dewpt = dewpt - 50.
        dwpt = -dewpt
        endif
20    continue
      return
99    return 1
      end
c
c
c
      subroutine rpress (line,press,geopot)
      character*70 line
      integer prsind
      dimension sprs(7)
      data sprs /200.,850.,700.,500.,400.,300.,250./
c
c     extract the pressure and geopotential from the RECCO message
c
      read (line(44:44),'(i1)') prsind
      if (prsind .eq. 0) then
        read (line(45:47),'(f3.0)') geopot
        if (geopot .lt. 800.) geopot = geopot + 1000.
        press = 1070.
      elseif (prsind .eq. 9) then
        geopot = -99.
        read (line(25:27),'(f3.0)') tralt
        pralt = tralt * 10.
        press = 1013.25 * (1. - (pralt / 44331.)) ** (1. / 0.190263)
      elseif (prsind .ge. 1 .and. prsind .le. 7) then
        press = sprs(prsind)
        read (line(45:47),'(f3.0)') geopot
        if (prsind .gt. 3 .and. prsind .lt. 7) then
          geopot = geopot * 10.
        elseif (prsind .eq. 2) then
          if (geopot .lt. 800.) geopot = geopot + 1000.
        elseif (prsind .eq. 1 .or. prsind .eq. 7) then
          geopot = geopot * 10.
          if (geopot .lt. 800.) geopot = geopot + 1000.
        elseif (prsind.eq.3)then
          geopot=geopot+3000.
        endif
      else if (prsind .eq. 8) then
        read (line(25:27),'(f3.0)') tralt   ! true alt in decameters
        read (line(45:47),'(f3.0)') dvalue  ! d-value in decameters
        if (dvalue .gt. 500.) dvalue = -(dvalue - 500.)
        pralt = tralt * 10. - dvalue * 10.
        press = 1013.25 * (1. - (pralt / 44331.))
     1    ** (1. / 0.190263)
        geopot = pralt
        if (geopot .lt. 0.) geopot = geopot + 500.
      else
        press = 0.
        geopot = 0.
        endif
      return
      end
c
c
c
      subroutine rdstuv (wdir,wspd,alat,alon)
      real alat,alon,wdir,wspd
c
c     convert wind direction and speed to u, v for RECCOs
c
      wdir = wdir * 10.
      wspd = 0.514444 * wspd
      call uvcomp (wdir,wspd)
      return
      end
c
c
c
      subroutine vortex(lu,iyrs,imns,idys,line)
      character*4 itime1,itime2
      character*30 blank
      character*70 line
      logical knots
      dimension a(10,8)
      data blank /'                             '/
c
c
      i=0
      iwx=7
c
c
c     Read the day
c     ------------
      read(line(13:14),'(i2)') iday
      if(line(13:13).eq.' ') read (line(14:15),'(i2)') iday
      if (iday.gt.50) then
         iday=iday-50
         knots = .true.
         else
         knots = .false.
         endif
c
c     check for month, year flips
c     ---------------------------
      yy = iyrs
      mm = imns
      if (idys.gt.27.and.iday.eq.1) then
         mm = imns+1
         if (mm.eq.13) then
            mm = 1
            yy = iyrs + 1.
            if (yy.eq.100.) yy = 00.
            endif
         endif
c
      yymmdd = yy * 10000. + float(mm) * 100. + float(iday)
c
c
c
c     read the line with the mission number
c
 10   read(12,'(a)',end=99)line
c
c     check if the line has information
c
      if(line(1:30).eq.blank)goto 10
c
c     read the line 'SUPPLEMENTARY VORTEX DATA MESSAGE'
c
 20   read(12,'(a)',end=99)line
c
c     check if the line has the information
c
      if(line(1:30).eq.blank)goto 20
c
c     read the data line
c
3330  read(12,'(a)',end=99)line
c
c     check if line is data, remarks, or has no information at all
c
      if(index(line,'MF').ne.0.or.index(line,'REM').ne.0.or.
     1   line(1:30).eq.blank)goto 3330
c
c     if the line with the observation times has been read, write data
c
      if(index(line,'OB').ne.0)goto 90
c
c     count the number of the data point
c
      i=i+1
c
c     check for end of message
c
      if(index(line,'NNNN').ne.0)return
c
c     read the latitude
c
      read(line(3:5),'(i3)')ilat
c
c     read the longitude
c
      read(line(8:11),'(i4)')ilon
c
c     save the value of the latitude
c
      alat=ilat/10.
c
c     save the value of the longitude
c
      alon=ilon/10.
c
c     read the pressure level
c
      read(line(14:14),'(i1)',err=190)ihgt
c
c     save the value of the pressure
c
      if(ihgt.eq.5)press=400.
      if(ihgt.eq.4)press=500.
      if(ihgt.eq.3)press=700.
      if(ihgt.eq.2)press=850.
      if(ihgt.eq.1)press=1000.
      if(ihgt.eq.0)press=1070.
c
c     read the value of the geopotential
c
 190  read(line(15:17),'(i3)',err=191)ihgt
c
c     save the value of the geopotential
c
      geopot=ihgt
c
c     adjust the value of the geopotential
c
      if(press.eq.1070..and.geopot.lt.100)geopot=geopot+1000.
      if(press.eq.850.0)geopot=geopot+1000.
      if(press.eq.700.0)geopot=geopot+3000.
      if(press.eq.500.0)geopot=geopot+5000.
      if(press.eq.400.0)geopot=geopot+7000.
c
c     read the value of the temperature
c
 191  read(line(20:21),'(f2.0)',err=192)temp
c
c     correct the value of the temperature
c
      if(temp.ge.50.0)temp=50.0-temp
c
c     read the value of the dewpoint
c
 192  read(line(22:23),'(f2.0)',err=193)dewpt
c
c     correct the value of the dewpoint
c
      if(dewpt.ge.50.0)dewpt=50.0-dewpt
c
c     read the value of the wind direction
c
 193  read(line(25:26),'(i2)',err=194)iwdir
c
c     save the value of the wind direction
c
      wdir=iwdir
c
c     read the value of the wind speed
c
      read(line(27:29),'(i3)',err=194)iwspd
c
c     save the value of the wind speed
c
      wspd=iwspd
c
c     calculate the relative humidity
c
      call relhum (press,temp,dewpt,rh)
c
c     calculate the u and v components of the wind
c
      call rdstuv (wdir,wspd,alat,alon)
c
c     save values until time has been calculated
c
      a(i,1)=alat
      a(i,2)=alon
      a(i,3)=press
      a(i,4)=temp
      a(i,5)=rh
      a(i,6)=geopot
      a(i,7)=wdir
      a(i,8)=wspd
c
c     reinitialize values of the variables
c
 194  press=-99.
      temp=-99.
      rh=-99.
      geopot=-99.
      wdir=-99.
      wspd=-99.
c
c     continue loop
c
      goto 3330
c
c     read time of the first observation
c
 90   if(index(line,'SFC').ne.0)goto 3330
      idx1=index(line,'AT')
      if(idxchk.ne.2)then
        read(line(idx1+3:idx1+6),'(a)')itime1
        read(itime1(1:2),'(i2)')ihour1
        read(itime1(3:4),'(i2)')imin1
        jtime1=ihour1*60+imin1
      else
        read(line(idx1+3:idx1+6),'(a)')itime2
        read(itime2(1:2),'(i2)')ihour2
        read(itime2(3:4),'(i2)')imin2
        jtime2=ihour2*60+imin2
      endif
c
c     read time of the second observation
c
      if(idxchk.ne.2)then
        idx2=index(line(idx1+1:70),'AT')+idx1
        if(idx2.eq.0)then
          read(12,'(a)',end=99)line
          idxchk=2
          goto 90
        endif
        read(line(idx2+3:idx2+6),'(a)')itime2
        read(itime2(1:2),'(i2)')ihour2
        read(itime2(3:4),'(i2)')imin2
        jtime2=ihour2*60+imin2
      endif
      idxchk=1
c
c     calculate the times and write out the data
c
      do j=1,i
        itime=j*(jtime2-jtime1)/i+jtime1
        itime=itime/60*100+mod(itime,60)
        alat=a(j,1)
        alon=a(j,2)
        press=a(j,3)
        temp=a(j,4)
        rh=a(j,5)
        geopot=a(j,6)
        wdir=a(j,7)
        wspd=a(j,8)
        call out (lu,iwx,yymmdd,itime,alat,alon,press,temp,rh,geopot,
     1            wdir,wspd,5)
      end do
      i=0
      goto 3330
99    continue
      stop
      end
c
c
c
      subroutine out (lu,iwx,yymmdd,igmt,alat,alon,press,
     1                temp,rh,geopot,wdir,wspd,msgtype)
      character*4 tail
      character*80 dropl(200)
      real alat,alon,wdir,wspd
c
      common /output/nrecs
      common /dropdata/idropl,dropl

      tail='0000'
      if (msgtype.eq.1) tail = 'MANL'     ! DROP/Mandatory
      if (msgtype.eq.2) tail = 'SIGL'     ! DROP/Significant
      if (msgtype.eq.3) tail = 'ADDL'     ! DROP/Additional (51515)
      if (msgtype.eq.4) tail = 'RECO'     ! RECCO
      if (msgtype.eq.5) tail = 'SUPV'     ! SUPPL VTX
      if (msgtype.eq.6) tail = 'MWND'     ! DROP/Max wind
      if (msgtype.eq.7) tail = 'TROP'     ! DROP/Tropopause
      if (msgtype.eq.8) tail = 'DLMW'     ! DROP/DLM wind
      nrecs = nrecs+1
c
c     Write it to the output file.
c     If DROP, save line until we get exact time later
c     ------------------------------------------------
      if (msgtype.le.3 .or. msgtype.eq.6 .or. msgtype.eq.7 .or.
     1   msgtype.eq.8) then
         idropl = idropl+1
         write(dropl(idropl),510) iwx,yymmdd,igmt,alat,alon,press,
     1               temp,rh,geopot,wdir,wspd,tail
         else
         write (lu,510) iwx,yymmdd,igmt,alat,alon,press,temp,rh,geopot,
     1               wdir,wspd,tail
         endif
c
      return
510   format (i2,1x,f7.0,1x,i4,1x,2(f7.3,1x),3(f6.1,1x),
     1  f7.1,2(f6.1,1x),a4)
      end
c
c
c
      subroutine dropout(lu,itime,splat,splon)
      character*80 dropl(200),dropx,dropsh
      dimension press(200)
      logical sort
c
      common /dropdata/idropl,dropl
c
      sort = .true.
      do 100 i = 1,idropl
c
         read(dropl(i)(33:38),*) press(i)
         if (splat.ne.-999.) write(dropl(i)(17:31),'(f7.3,1x,f7.3)')
     *                       splat,splon
c
         if (itime.ge.2400) goto 100
c
         read(dropl(i)(12:15),'(i4)') nhr
         if (nhr.eq.0 .and. itime.gt.2300) then
            read(dropl(i)(8:9),*) iday
            iday = iday-1
            if (iday.eq.0) then
               read(dropl(i)(6:7),*) mth
               iday = 31
               mth = mth-1
               if (mth.eq.2) iday = 28
               if (mth.eq.4 .or. mth.eq.6 .or. mth.eq.9 .or.
     *             mth.eq.11) iday = 30
               if (mth.eq.0) then
                  mth =12
                  read(dropl(i)(4:5),*) iyr
                  iyr = iyr-1
                  write(dropl(i)(4:5),'(i2.2)') iyr
                  endif
               write(dropl(i)(6:7),'(i2.2)') mth
               endif
            write(dropl(i)(8:9),'(i2.2)') iday
            endif
c
         write(dropl(i)(12:15),'(i4)') itime
100      continue
c
c     sort by pressure
c     ----------------
      if (.not.sort) goto 300
      do 200 i=1,idropl-1
         do 210 j=i+1,idropl
            if (press(i).lt.press(j)) then
               dropx = dropl(j)
               pressx = press(j)
               do 220 k=j,i+1,-1
                  dropl(k) = dropl(k-1)
                  press(k) = press(k-1)
220               continue
               dropl(i) = dropx
               press(i) = pressx
               endif
210         continue
200      continue   
c
      u1 = -99.
      u2 = -99.
      v1 = -99.
      v2 = -99.
      bad = -99.
      psh = 8520.
      dropsh = dropl(1)
300   do 310 i=1,idropl
         read(dropl(i)(33:38),*) pr
         if (pr.eq.200.) then
            read(dropl(i)(62:66),*) u1
            read(dropl(i)(69:73),*) v1
            endif
         if (pr.eq.850.) then
            read(dropl(i)(62:66),*) u2
            read(dropl(i)(69:73),*) v2
            endif
         write(lu,'(a)') dropl(i)
310      continue
c
      if (u1.ne.-99. .and. u2.ne.-99. 
     *   .and. v1.ne.-99. .and. v2.ne.-99.) then
         ushear = u1-u2
         vshear = v1-v2
         write(dropsh(33:38),'(f6.1)') psh
         write(dropsh(40:60),'(f6.1,1x,f6.1,1x,f7.1)') bad,bad,bad
         write(dropsh(61:73),'(f6.1,1x,f6.1)') ushear,vshear
         dropsh(75:78) = 'WSHR'
         write(lu,'(a)') dropsh
         endif       
c
      return
      end
C
C
C
C     --------------------------------------
      SUBROUTINE CITY(IOPT,STRING,RLAT,RLON)
C     --------------------------------------
      PARAMETER (NC = 41)
      CHARACTER*30 STRING
      CHARACTER*30 NAMES(NC)
      DIMENSION XPOS(2,NC)
C
      DATA NAMES /'MACDILL                       ',
     *            'OPA LOCKA                     ',
     *            'TAMIAMI                       ',
     *            'MIAMI                         ',
     *            'FT LAUDERDALE                 ',
     *            'SAN JUAN                      ',
     *            'ST CROIX                      ',
     *            'BARBADOS                      ',
     *            'GRAND CAYMAN                  ',
     *            'BERMUDA                       ',
     *            'BOSTON                        ',
     *            'PROVIDENCE                    ',
     *            'ANDREWS                       ',
     *            'CHARLESTON                    ',
     *            'SAVANNAH                      ',
     *            'JACKSONVILLE                  ',
     *            'NEW ORLEANS                   ',
     *            'CORPUS CHRISTI                ',
     *            'HARLINGEN                     ',
     *            'KEESLER                       ',
     *            'HOMESTEAD                     ',
     *            'ROBINS                        ',
     *            'ELLINGTON                     ',
     *            'CURACAO                       ',
     *            'HONOLULU                      ',
     *            'HALIFAX                       ',
     *            'BRUNSWICK                     ',
     *            'PORTSMOUTH                    ',
     *            'SAN JOSE                      ',
     *            'SAN DIEGO                     ',
     *            'EDWARDS                       ',
     *            'ARMSTRONG                     ',
     *            'WALLOPS                       ',
     *            'LA PAZ                        ',
     *            'LIBERIA                       ',
     *            'ANCHORAGE                     ',
     *            'ARUBA                         ',
     *            'SAL                           ',
     *            'MOBILE                        ',
     *            'BORINQUEN                     ',
     *            'LAKELAND                      '/
C
      DATA XPOS  /27.85, 82.52,
     *            25.91, 80.28,
     *            25.65, 80.43,
     *            25.79, 80.29,
     *            26.07, 80.15,
     *            18.44, 66.00,
     *            17.70, 64.80,
     *            13.06, 59.49,
     *            19.29, 81.36,
     *            32.36, 64.68,
     *            42.37, 71.01,
     *            41.73, 71.43,
     *            38.81, 76.87,
     *            32.90, 80.04,
     *            32.13, 81.20,
     *            30.23, 81.68,
     *            29.99, 90.26,
     *            27.77, 97.50,
     *            26.22, 97.65,
     *            30.41, 88.92,
     *            25.49, 80.39,
     *            32.64, 83.59,
     *            29.61, 95.16,
     *            12.18, 68.97,
     *            21.30,157.90,
     *            44.88, 63.52,
     *            43.90, 69.93,
     *            43.08, 70.82,
     *             9.98, 84.22,
     *            32.85,117.12,
     *            34.92,117.87,
     *            34.92,117.87,
     *            37.93, 75.48,
     *            24.07,110.36,
     *            10.59, 85.54,
     *            61.18,150.00,
     *            12.50, 70.01,
     *            16.74, 22.95,
     *            30.63, 88.07,
     *            18.50, 67.13,
     *            27.99, 82.02/
C
      IF (IOPT.EQ.0) THEN
         DO 100 L = 1,NC
            IF (STRING.EQ.NAMES(L)) GOTO 200
100         CONTINUE
C
         RLAT = -99.
         RLON = -99.
         RETURN
C
200      RLAT = XPOS(1,L)
         RLON = XPOS(2,L)
         RETURN
         ENDIF
C
C
      IF (IOPT.GT.0 .AND. IOPT.LE.NC) THEN
         STRING = NAMES(IOPT)
         RETURN
         ENDIF
C
C
      IF (IOPT.GT.NC) THEN
         STRING = 'ERROR'
         RETURN
         ENDIF
C
      END


      subroutine getspeed(iac,alt,speedcal)
      if(iac.eq.42.or.iac.eq.43)then
        if(alt.ge.5000..and.alt.le.8000.)then
          speedcal=218.+(232.-218.)*(alt-5000.)/3000.
        elseif(alt.gt.8000..and.alt.le.10000.)then
          speedcal=232.+(242.-232.)*(alt-8000.)/2000.
        elseif(alt.gt.10000.and.alt.le.12000.)then
          speedcal=242.+(252.-242.)*(alt-10000.)/2000.
        elseif(alt.gt.12000.and.alt.le.20000.)then
          speedcal=252.+(300.-252.)*(alt-12000.)/8000.
        else
          speedcal=300.
        endif
      endif
      if(iac.eq.57)then
        if(alt.ge.60000)then
          speedcal=400.
        elseif(alt.le.40000)then
          speedcal=300.
        else
          speedcal=300.+100.*(alt-30000)/10000.
        endif
      endif
      return
      end
