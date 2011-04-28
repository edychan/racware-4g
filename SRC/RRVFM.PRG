* ===========================================================================
* add/update reservation record
*
* date: 05/01/91
* author: KST
*
* revision
* date : 09/25/92
* edc: new features for insurance replacement rentals
* date: 01/28/93
* edc: warn user about duplicate res.
* date: 01/29/93
* edc: add more fields for insurance replacement rentals
* date: 10/07/93
* edc: save old values for comparsion to stop from replacing rates.
* date: 11/03/93
* edc: change claim# to Reference # (recall at open RA)
* date: 12/04/93
* edc: take out resv grid
* date: 12/05/93
* edc: handle advance res deposit
* date: 12/21/93
* edc: f_rvstat, if empty(fresvstat) causes PROC Type Mismatch L+C
*      correction: if empty(fresvstat) return "UNKNOWN"
* date: 02/15/94
* edc: handle advance deposit
* date: 08/09/94
* edc: specialty vehicle res table
* date: 04/17/95
* edc: rrvfmup7, allow user update travel agent file info.
* date: 12/07/95: 
* edc: allow cc deposit for open res
* 07.13.98
* add availability table (raproj, raflts)
* 10.21.99
* year 2000
* 02.26.02: implement xtraday rate
* 07.26.02: Add book time (fbooktime)
* ------------------------------------------------
* 07.27.09: add email function. (not activate yet)
* 10.06.09: add FESTCHG (rares.dbf) for est. charge
* ===========================================================================
parameters xtyp
private yatc, ycustno, ycrpno, yrate, yclass, yloc, ycomm, yresvno, yunit
private i, y1, y2, yrdateout, yrdatein, yans, ydateout, yhtm

f_clrscn ("Reservation File")
f_popup ("Please Wait...")
set century on       && 10.21.99
set key 28 to rrvfmhlp
set key -1 to rrvfmget
set key -2 to rrvfmfs
set key -10 to f_listfs
set key -11 to f_listav
f_popback ()
if xtyp = "A"
   yans = "D"
   do while .t.
      if yans = "D"
         restore from (gmempath + "RARES") additive
         l_fcode = grate
         l_fdateout = date ()
         l_floc = gloc
         l_frloc = gloc
         l_fbookdate = date ()
         l_fid = gusrid
      endif      
      yfreesell = gfreesell
      ydateout = ctod (space (8))
      store space (13) to yrdateout, yrdatein
      store "~" to ycustno, ycrpno, yrate, yclass, yloc, yunit, yatc, ;
            yresvno
      f_use ("raloc")
      seek gloc
      l_fresvno = ""
      if l_fresvno <> strtran (str (swcalcxorb (gloc, 10) * 10000000 + ;
            fresvno, 10), " ", "0")
         * f_fupdate ("C")    && 03.12.01
         reclock ()
         if fresvno >= 9999999
            replace fresvno with 1
         else
            replace fresvno with fresvno + 1
         endif
         commit
         unlock
         l_fresvno = strtran (str (swcalcxorb (gloc, 10) * 10000000 + ;
               fresvno, 10), " ", "0")
      endif
      l_fresvstat = "O"
      l_fbooktime = substr (time(), 1, 5)
      f_screen (0, "RARES")
      do while .t.
         if .not. yfreesell
            setcolor (gredcolor)
            @ 24, 01 say "F3 Overwrite Free Sell"
         endif
         setcolor (gbluecolor)
         f_rd ()
         if empty (l_fresvno)
            ykeyin = f_confirm ("[E]dit  [V]oid", "EV")
         else
            ykeyin = f_confirm ("[C]onfirm  [E]dit  [V]oid", "CEV")
         endif
         do case
         case ykeyin = "C"
            * 01/28/93: warn user about duplicate res.
            f_use ("rares")
            set order to 2
            seek 'O'+upper(l_flname)
            yans = "Y"
            do while .not. eof() .and. upper(rares->flname) = upper(l_flname)
               if rares->fdateout = l_fdateout
                  yans = f_confirm ("Warning: Duplicate Reservation Found!" + ;
                  " Continue ?[Y/N]", "YNyn")
                  exit
               else
                 skip
               endif
            enddo
            set order to 1
            if yans $ "Nn"
               exit
            endif

            *
            append blank
            f_replace ()
            f_fupdate ("A")

            f_use ("rartm")
            if f_getrate (l_fcode, l_floc, l_fclass, l_fdateout)
               * f_fupdate ("C")
               reclock ()
               replace fresvcnt with fresvcnt + 1
               commit
               unlock
            endif
            use
            if .not. empty (l_fatc)
               f_use ("raagnt", 1)
               seek l_fatc
               if found ()
                  * f_fupdate ("C")
                  reclock ()
                  replace fres with fres + 1
                  replace factdt with date ()
                  commit
                  unlock
               endif
               use
            endif
            * handle advance deposit 12/05/93
            if l_fdepamt > 0
               f_use ("radtr")
               f_findblank ()
               replace floc with gloc, frano with 999999, frloc with gloc
               replace flname with l_flname, funit with l_fresvno
               replace frectype with "A"
               replace famt1 with l_fdepamt, fpaytyp1 with "CA"
               replace ftotal with l_fdepamt
               commit
               unlock
               use
            endif
            * update fleet summary
            if l_fdateout = date()
               f_use ("RAFLTS")
               seek l_fclass + l_floc
               reclock ()
               replace fres with fres + 1
               replace fnet with favail + fin + fdue - fres
               commit
               unlock
            endif
            * update availability table
            f_use ("RAPROJ")
            seek l_fclass + dtos(l_fdateout)
            if .not. eof ()
               y2 = fres + 1
               reclock ()
               replace fres with y2
               replace fnet with favail + fdue - y2
               commit
               unlock
               skip
            endif
            do while .not. eof () .and. fclass = l_fclass .and. fdate <= l_fdatein
               y2 = favail - 1
               reclock ()
               replace favail with y2
               replace fnet with y2 + fdue - fres
               commit
               unlock
               skip
            enddo
            * handle due back
            seek l_fclass + dtos(l_fdatein)
            if .not. eof ()
               y2 = fdue + 1
               reclock ()
               replace fdue with y2
               replace fnet with favail + y2 - fres
               commit
               unlock
            endif
            use 
            *
            exit
         case ykeyin = "E"
            f_screen (1, "RARES")
            loop
         case ykeyin = "V"
            if f_confirm ("Are You Sure To Void This Reservation? [Y/N]", ;
                  "YN") = "Y"
               exit
            else
               keyboard chr (18)
               loop
            endif
         endcase
      enddo
      yans = f_confirm ("Want to enter another one? [S]imilar/" + ;
            "[D]ifferent one/[N]o", "SDN")
      if yans = "N"
         exit
      endif
   enddo
else
   restore from (gmempath + "RARES") additive
   yfreesell = gfreesell
   f_use ("rares")
   if eof ()
      f_popup ("No Reservation File Found!!!. Press Any Key to Continue", .t.)
      close databases
      set key 28 to
      set key -2 to
      set key -1 to
      return
   endif
   private yarray [5], yptr, ycolor, yscn, ykey
   yarray [1] = " Confirmation No.  "
   yarray [2] = " Last Name         "
   yptr = f_pick_a (02, 05, "", "", YARRAY, 2, 1)
   if yptr = 0
      set key 28 to
      set key -2 to
      set key -1 to
      return
   endif
   ycolor = setcolor (gsubcolor)
   yscn = f_box (2, 21, 4, 58)
   do case
   case yptr = 1
      ykey = "l_fresvno"
      ytitle = "StatÄÄÄÄResv No.ÄÄÄLocationÄÄÄNameÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" ;
         + "Resv Dte"
      yexp = "f_rvstat (fresvstat) + [ ] + fresvno + [ ] + floc + [ ] + " + ;
         "f_truncate (trim (ffname) + [ ] + flname, 28) + [ ] + " + ;
         "dtoc (fdateout)"
      @ 3, 23 say "Confirmation No. ......"
      @ 3, 47 get l_fresvno picture "!!!!!!!!!!"
   case yptr = 2
      ykey = "l_fresvstat + upper (l_flname)"
      ytitle = "StatÄÄÄÄName"
      yexp = "f_rvstat (fresvstat) + [ ] + f_truncate (trim (flname) + [, ] " + ;
         "+ ffname, 28) + [ Pick Up Car At ] + floc + [ ] + dtoc (fdateout)"
      @ 3, 23 say "Last Name ........."
      @ 3, 43 get l_flname
   endcase
   if f_rd () = 27
      setcolor (ycolor)
      close databases
      set key 28 to
      set key -2 to
      set key -1 to
      return
   endif
   set order to (yptr)
   if yptr = 2
      yarray [1] = " Open             "
      yarray [2] = " Used (RA Closed) "
      yarray [3] = " Used (RA Open)   "
      yarray [4] = " No Show          "
      yarray [5] = " Cancel           "
      yptr = f_pick_a (04, 30, "", "", YARRAY, 5, 1)
      if yptr = 0
         set key 28 to
         set key -2 to
         set key -1 to
         return
      endif
      l_fresvstat = substr ("OUXNC", yptr, 1)
   endif
   f_restbox (yscn)
   setcolor (ycolor)
   set softseek on
   seek &ykey
   if eof ()
      go bottom
   endif
   set softseek off

   if .not. found () .or. yptr = 2
      if .not. f_pick_f (02, 77 - len (&yexp), "", ytitle, yexp)
         close databases
         set key 28 to
         set key -2 to
         set key -1 to
         return
      endif
   endif

   f_retrieve ()
   do rrvfmsm      && 10/07/93 (edc): save old values
   f_screen (0, "RARES")

   do while .t.
      * 04/28/94: edc show cancellation date correctly
      if l_fresvstat = "O"
         @ 17,45 say "Checkout Date....."
         @ 17,64 say "        "
         * 07.27.09: add email (using NetmailBot)
         ykeyin = f_confirm ("[U]pdate  [C]ancel  [E]mail  [R]es Deposit  " + ;
            "[N]ext  [P]rev  [Q]uit", "UCERNPQ")
      elseif l_fresvstat $ "X;U"
         @ 17,45 say "Checkout Date....."
         @ 17,64 say l_fdateout
         ykeyin = f_confirm ("[D]etail  [N]ext  " + ;
            "[P]revious  [Q]uit", "DNPQ")
      else
         if l_fresvstat = "C"
            @ 07,23 say  l_fdateout+l_fdays
            @ 17,45 say "Date Cancelled...."
            @ 17,64 say l_fdatein
         else
            @ 17,45 say "Checkout Date....."
            @ 17,64 say "        "
         endif
         ykeyin = f_confirm ("[R]esv Deposit  [D]etail  [N]ext  " + ;
            "[P]revious  [Q]uit", "RDNPQ")
      endif
      do case

      * 07.27.09: email open rez
      case ykeyin = "E"
         clear gets
         setcolor (gsubcolor)
         yscn = f_box (13, 14, 16, 63)
         yemail = rares->femail
         @ 14, 16 say "email ....."
         do while .t.
         @ 14, 28 get yemail picture replicate ("x", 30) ;
            valid f_valid (good_email (yemail), "Invalid email address ...")
         if f_rd () = 27
            exit
         endif
         ykeyin = f_confirm ("[C]onfirm   [E]dit   [I]gnore", "CEI")
         if ykeyin = "C"

            select rares
            reclock ()
            replace femail with yemail
            commit
            unlock

            * update gempath+"rezmsg.dbf"
            yfil = gempath + "rezmsg.dbf"
            if file (yfil)
               ydays = l_fdatein - l_fdateout
               do case
               case ydays > 4 .and. l_fwkchg > 0
                  yrate = [$] + str(l_fwkchg,6,2) + [ per week.]
               otherwise
                  yrate = [$] + str(l_fdlychg,6,2) + [ per day.]
               endcase
               
               yname = trim(l_ffname)+[ ]+trim(l_flname)
               * -- get est. charges
               l_fsurchg = 0
               l_fsurchg1 = 0
               l_ftaxtot = 0

               * 10.06.09: re calculate 
               l_festchg = rrvestchg ()     && add festchg to store est.
               * --

               select 0
               use &yfil
               reclock ()
               append blank
               replace femail with yemail, fresvno with l_fresvno
               replace fname with yname
               replace fcartype with l_fclass, frate with yrate
               replace fdateout with dtoc(l_fdateout)+" "+l_ftimeout
               replace fdatein with dtoc(l_fdatein)+" "+l_ftimein
               replace fsurchg with l_fsurchg, fsurchg1 with l_fsurchg1
               replace ftaxtot with l_ftaxtot, ftotal with l_festchg
               * replace ffile with if(l_floc='EGE',"ege.htm","jac.htm")
               replace floc with l_floc, frloc with l_frloc
               commit
               unlock
               use

               f_valid (.f., "Email is now in the message queue ...")
            else
               f_valid (.f., "Email is not setup ...")
            endif
            exit
         elseif ykeyin = "E"
            loop
         else
            exit
         endif
         enddo
         select rares
         f_restbox (yscn)
         setcolor (gbluecolor)

      * --

      * 02/15/94: handle res. deposit
      case ykeyin = "R"
         clear gets
         setcolor (gsubcolor)
         yscn = f_box (13, 14, 19, 63)
         ypaytyp = rares->fcctype
         yamt = 0.00
         yexp = rares->fccexp
         yccnum = rares->fccnum
         yauthcode = space (6)
         yauthstat = space (20)
         yrectype = __gccuncap
         yauthonly = .f.
         @ 14, 16 say "Deposit Type............. "
         @ 15, 16 say "CC Number ............... "
         @ 16, 16 say "Expiration............... "
         @ 17, 16 say "Amount .................. "
         @ 18, 16 say "Auth Code ............... "
         do while .t.
            ycode = 0
            @ 14, 42 get ypaytyp picture "!!!" ;
               valid f_valid (good_paytype (ypaytyp, @ycode), ;
               "Invalid Payment Type!!!")
            @ 15, 15 clear to 18, 62
            if f_rd () = 27
               exit
            endif
            if good_cctype (ypaytyp, @yauthonly)
               @ 15, 16 say "CC Number ............... "
               @ 16, 16 say "Exp ..................... "
               @ 17, 16 say "Amount .................. "
               @ 18, 16 say "Auth Code ............... "
               @ 15, 42 get yccnum picture replicate ("9", 20) ;
                  valid f_valid (good_card (yccnum), "Invalid Credit Card!!!")
               @ 16, 42 get yexp picture "99/99" ;
                  valid f_valid (f_expired (yexp), "Card Expired!!!")
               @ 17, 42 get yamt picture "99999.99" valid f_valid (yamt >=0)
               @ 18, 42 get yauthcode picture "!!!!!!"
            else
               @ 18, 16 say "Amount .................. "
               @ 18, 42 get yamt picture "99999.99" valid f_valid (yamt >=0)
            endif
            f_rd ()
            if empty(yauthcode) .and. ycode = 1
               yauthstat = space (30)
               if get_auth ("S", yccnum, yexp, yamt, @yauthcode, ;
                            @yauthstat) = 0
                 if f_valid (auth_ok (yauthstat, @yauthcode), ;
                            "Warning! Credit Card Authorization is rejected!")
                    @ 18, 42 say yauthcode
                    yrectype = 3
                 else
                    yrectype = 6
                 endif
               endif
            endif
            ykeyin = f_confirm ("[C]onfirm   [E]dit   [I]gnore", "CEI")
            if ykeyin = "C"
               select rares
               f_fupdate ("C")
               reclock ()
               replace fdepamt with fdepamt + yamt
               commit
               unlock
               if ycode = 1          
                  f_use ("RACRED", 1)
                  f_findblank ()
                  replace fauthonly with yauthonly, fauthamt with abs (yamt)
                  replace fauthcode with yauthcode, fauthstat with yauthstat
                  replace fauthdate with date (), fauthtime with time ()
                  replace fccexp with yexp, fccnum with yccnum
                  replace fcctype with ypaytyp, ffname with rares->ffname
                  replace flname with rares->flname, floc with rares->floc
                  replace frano with 999999, frectype with yrectype
                  replace frloc with rares->floc
                  if empty (yauthcode) 
                     replace ftranstyp with "F"
                  else
                     replace ftranstyp with "S"
                  endif
                  commit
                  unlock
                  * f_fupdate ("A")
                  use
               endif
               f_use ("RADTR")
               f_findblank ()
               replace floc with rares->floc, frano with 999999
               replace funit with rares->fresvno, frloc with rares->floc
               replace flname with rares->flname, fpaytyp1 with ypaytyp
               * replace foitem1 with "RES", fotot1 with yamt
               replace famt1 with yamt, frectype with "A"    && 11.16.99
               replace ftotal with yamt           
               commit
               unlock
               * f_fupdate ("A")
               use
               f_screen (0, "RARES")
               exit
            elseif ykeyin = "E"
               loop
            else
               exit
            endif
         enddo
         select rares
         f_restbox (yscn)
         setcolor (gbluecolor)

      case ykeyin = "D"
         ycolor = setcolor (gsubcolor)
         yscn = f_box (5, 4, 19, 75)
         
         @ 06, 6 say "Reference#............."             && 11/03/93 (edc)
         @ 07, 6 say "Last/First Name........"
         @ 08, 9 say "License/St/DOB/Exp.."
         @ 09, 9 say "Address............."
         @ 10, 9 say "City/St/Zip/Phone..."     
         @ 11, 9 say "Ins Co./Policy#....."
         @ 12, 9 say "Agent/Phone #......."
         @ 13, 6 say "Paying Co./Policy#....."
         @ 14, 9 say "Agent/Phone #......."
         @ 15, 9 say "Adjuster/Phone #...."
         @ 16, 9 say "Maximum amount/days."
         @ 17, 6 say "Auto Shop/Phone #......"
         setcolor (gsubget)
         @ 06, 30 say l_frefno
         @ 07, 30 say l_flname
         @ 07, 45 say l_ffname
         @ 08, 30 say l_flic
         @ 08, 50 say l_flicst
         @ 08, 54 say l_fdob
         @ 08, 65 say l_flicexp
         @ 09, 30 say l_faddr
         @ 10, 30 say l_fcity
         @ 10, 46 say l_fstate
         @ 10, 50 say l_fzip
         @ 10, 62 say l_fphone
         @ 11, 30 say l_fcinsur1
         @ 11, 52 say l_fcinsur2
         @ 11, 65 say l_fcexp1
         @ 12, 30 say l_fcagent
         @ 12, 62 say l_fcagentph
         @ 13, 30 say l_finsur1
         @ 13, 52 say l_finsur2
         @ 13, 65 say l_fexp1
         @ 14, 30 say l_finsagent
         @ 14, 62 say l_fagentph
         @ 15, 30 say l_fadjuster
         @ 15, 62 say l_fadjph
         @ 16, 30 say l_fmaxamt
         @ 16, 39 say l_fmaxday
         @ 17, 30 say l_fshop
         @ 17, 62 say l_fshopph
         f_popup ("Press Any Key to Continue...", .t.)
         f_restbox (yscn)
         setcolor (ycolor)
      case ykeyin = "U"
         do while .t.
            if .not. yfreesell
               setcolor (gredcolor)
               @ 24, 01 say "F3 Overwrite Free Sell"
            endif
            setcolor (gbluecolor)
            f_rd ()
            if empty (l_fresvno)
               ykey = f_confirm ("[E]dit  [I]gnore Changes", "EI")
            else
               ykey = f_confirm ("[C]onfirm  [E]dit  [I]gnore Changes", "CEI")
            endif
            do case
            case ykey = "C"
               f_use ("rartm")
               if f_getrate (rares->fcode, rares->floc, rares->fclass, ;
                     rares->fdateout)
                  * f_fupdate ("C")
                  reclock ()
                  replace fresvcnt with fresvcnt - 1
                  commit
                  unlock
               endif
               if f_getrate (l_fcode, l_floc, l_fclass, l_fdateout)
                  * f_fupdate ("C")
                  reclock ()
                  replace fresvcnt with fresvcnt + 1
                  commit
                  unlock
               endif
               * advance deposit  12/05/93
               if l_fdepamt <> rares->fdepamt
               f_use ("radtr")
               f_findblank ()
               replace floc with gloc, frano with 999999, frloc with gloc
               replace flname with l_flname, funit with l_fresvno
               replace frectype with "A"
               replace famt1 with l_fdepamt-rares->fdepamt, fpaytyp1 with "CA"
               replace ftotal with l_fdepamt-rares->fdepamt
               commit
               unlock
               use
               endif
               ********************************
               if l_fatc <> rares->fatc
                  if .not. empty (l_fatc)
                     f_use ("raagnt", 1)
                     seek l_fatc
                     if found ()
                        * f_fupdate ("C")
                        reclock ()
                        replace fres with fres + 1
                        replace factdt with date ()
                        commit
                        unlock
                     endif
                  endif
                  if .not. empty (rares->fatc)
                     f_use ("raagnt", 1)
                     seek rares->fatc
                     if found ()
                        * f_fupdate ("C")
                        reclock ()
                        replace fres with fres - 1
                        commit
                        unlock
                     endif
                  endif
               endif
               * 10.06.09: add est. charge (FESTCHG)
               l_festchg = rrvestchg ()
               * --
               f_use ("rares")
               f_fupdate ("C")
               f_replace ()
               exit
            case ykey = "E"
               f_screen (1, "RARES")
               loop
            case ykey = "I"
               if f_confirm ("Are You Sure to Ignore the Change? [Y/N]", ;
                     "YN") = "Y"
                  exit
               else
                  keyboard chr (18)
                  loop
               endif
               exit
            endcase
         enddo
         exit
      case ykeyin = "C"
         if f_confirm ("Are you sure? [Y/N]", "YN") = "Y"
            clear gets
            l_fdatein = date ()
            @ 17, 45 say "Date Cancelled...."
            @ 17, 64 get l_fdatein
            * 06/08/94 (edc) update remarks field
            @ 21, 23 get l_fremark1
            @ 22, 23 get l_fremark2
            if f_rd () = 27
               exit
            endif
            * update fleet summary
            if l_fdateout = date()
               f_use ("RAFLTS")
               seek l_fclass + l_floc
               reclock ()
               replace fres with fres - 1
               replace fnet with favail + fin + fdue - fres
               commit
               unlock
            endif
            * update availability table
            f_use ("raproj")
            seek rares->fclass + dtos(rares->fdateout)
            if .not. eof ()
               y2 = fres - 1
               reclock ()
               replace fres with y2
               replace fnet with favail + fdue - y2
               commit
               unlock
               skip
            endif
            do while .not. eof () .and. fclass = rares->fclass .and. ;
               fdate <= rares->fdatein
               y2 = favail + 1
               reclock ()
               replace favail with y2
               replace fnet with y2 + fdue - fres
               commit
               unlock
               skip
            enddo
            * handle due back
            seek rares->fclass + dtos(rares->fdatein)
            if .not. eof ()
               y2 = fdue - 1
               reclock ()
               replace fdue with y2
               replace fnet with favail + y2 - fres
               commit
               unlock
            endif
            use
            *
            select rares
            f_fupdate ("C")
            reclock ()
            replace fdatein with l_fdatein      && 04/06/94 (edc)
            replace fresvstat with "C"
            replace fremark1 with l_fremark1, fremark2 with l_fremark2
            commit
            unlock

            f_use ("rartm")
            if f_getrate (rares->fcode, rares->floc, rares->fclass, ;
                  rares->fdateout)
               * f_fupdate ("C")
               reclock ()
               replace fresvcnt with fresvcnt - 1
               commit
               unlock
            endif
            if .not. empty (rares->fatc)
               f_use ("raagnt", 1)
               seek rares->fatc
               if found ()
                  * f_fupdate ("C")
                  reclock ()
                  replace fres with fres - 1
                  commit
                  unlock
               endif
               use
            endif
            exit
         else
            f_screen (1, "RARES")
         endif
      case ykeyin = "N"
         clear gets
         skip 1
         if eof ()
            f_popup ("End of file. Press Any Key to Continue...", .t.)
            go bottom
         else
            f_retrieve ()
            do rrvfmsm      && 10/07/93 (edc): save old values
            f_screen (1, "RARES")
         endif
      case ykeyin = "P"
         clear gets
         skip -1
         if bof ()
            f_popup ("Top of file. Press Any Key to Continue...", .t.)
            go top
         else
            f_retrieve ()
            do rrvfmsm      && 10/07/93 (edc): save old values
            f_screen (1, "RARES")
         endif
      case ykeyin = "Q"
         clear gets
         exit
      endcase
   enddo
endif

close databases
set century off       && 10.21.99
set key 28 to
set key -1 to
set key -2 to

* ---------------------------
* check email: need to refine the logic
*
function good_email
parameters xemail

do case
case empty(xemail)
   return .f.
case at ("@", xemail) = 0
   return .f.
otherwise
   return .t.
endcase

******************************
procedure rrvfmsm
* save old values
ycustno = fcustno
ycrpno = fcrpno
yrate = fcode
yclass = fclass
yloc = floc
ydateout = fdateout
yatc = fatc
yresvno = l_fresvno
yunit = l_funit
yrdateout = dtos (fdateout) + ftimeout
yrdatein = dtos (fdatein) + ftimein

return

******************************
procedure rrvfmfs

if yfreesell
   return
endif
private yscn, ycolor, yusr, ypass, ysp, ychr
ycolor = setcolor (gsubcolor)
yscn = f_box (1, 24, 4, 78)
@ 2, 26 say "Enter Username With Ability Of Free Sell"
@ 3, 26 say "Password ..............................."
yusr = space (3)
do while .t.
   if .not. f_getfld (@yusr, 2, 67, "W/N", 3, "!!!")
      setcolor (ycolor)
      f_restbox (yscn)
      return
   endif
   f_use ("RAUSR")
   seek yusr
   if .not. f_valid (found (), "Invalid User Name!!!")
      loop
   endif
   f_use ("RAGROUP")
   for n = 0 to 9
      if str (n, 1) $ rausr->fgroup
         go (n + 1)
         if ffreesell
            yfreesell = .t.
            exit
         endif
      endif
   next
   if .not. f_valid (yfreesell, "User Does Not Have Free Sell Right!!!")
      loop
   endif
   use
   f_use ("rausr")

   set console off
   ypass = ""
   ysp = 67
   setcolor (gsubget)
   @ 3, ysp say space (10)
   do while len (ypass) < 11
      @ 3, ysp say ""
      wait to ychr
      if len (ychr) > 0
         @ 3, ysp say "X"
         ysp = ysp + 1
         ypass = ypass + upper (ychr)
      else
         exit
      endif
   enddo
   set console on
   if .not. f_valid (f_truncate (ypass, len (fpasswd)) = fpasswd, ;
         "Invalid Password, Please Retry ...")
      @ 3, 67 say space (10)
      loop
   endif
   f_use ("rausr")
   use
   exit
enddo
setcolor (ycolor)
f_restbox (yscn)


******************************
procedure rrvfmhlp

private yvar, yscn, ycolor, ylname, yfname
yvar = upper (readvar ())
ycolor = setcolor (gsubcolor)
if yvar $ "L_FCLASS;L_FCODE"
   if empty(l_fcode)
      do rrndr
   else
      f_use ("RARTM")
      set filter to (empty(fdatefrom) .and. l_fdateout <= fdateto) .or.  ;
                 (l_fdateout >= fdatefrom .and. l_fdateout <= fdateto) .or. ;
                 (empty(fdateto) .and. l_fdateout >= fdatefrom) .or. ;
                 (empty(fdatefrom) .and. empty(fdateto))
      go top
      seek l_fcode+l_floc
      if eof ()
         f_valid (.f., "Invalid Rate Code...")
      else
         set century off
         f_pick_f (02, 01, "", "ÄRateÄÄÄLocationÄÄÄClassÄÄFromÄÄÄ" + ;
                  "ÄÄÄToÄÄÄÄÄÄÄDlyÄÄÄÄÄWkdÄÄÄÄÄÄWkÄÄÄÄÄMth", ;
                  "fcode+[ ]+if(empty(floc),[ALL       ],floc)+[ ]+" + ;
                  "fclass+[ ]+dtoc(fdatefrom)+[ ]+dtoc(fdateto)+[ ]+" + ;
                  "str(fdlychg,8,2)+str(fwkdchg,8,2)+str(fwkchg,8,2)+" + ;
                  "str(fmthchg,8,2)", "rrndrd","fcode+floc","l_fcode+l_floc")
         set century on
      endif
      select rartm
      set filter to
      use
   endif
elseif yvar = "L_FCUSTNO"
   ylname = l_flname
   yscn = f_box (2, 23, 4, 54)
   @ 3, 25 say "Last Name ... "
   if .not. f_getfld (@ylname, 3, 39, "W/N")
      setcolor (ycolor)
      f_restbox (yscn)
      return
   else
      if empty (ylname)
         yfname = l_ffname
         @ 3, 25 say "First Name .... "
         if .not. f_getfld (@yfname, 3, 41, "W/N")
            setcolor (ycolor)
            f_restbox (yscn)
            return
         endif
      else
         yfname = ""
      endif
   endif
   if empty (yfname)
      f_use ("racust", 1)
   else
      f_use ("racust", 4)
      ylname = yfname
   endif
   set softseek on
   seek upper (ylname)
   if eof ()
      go bottom
   endif
   set softseek off
   if f_pick_f (2, 2, "", "", "f_truncate (trim (ffname) + [ ] + " + ;
         "flname, 28) + if (fhold, [ On Hold ], [ ------- ]) + [ Birthday ] + dtoc (fbirthdt)")
      if f_valid (.not. fhold, "Customer is on HOLD status!!!")
         l_fcustno = racust->fcustno
         keyboard chr (13)
      endif
   endif
   set filter to
   f_use ("rares")
   f_restbox (yscn)
elseif yvar = "L_FCRPNO"
   ylname = space (25)
   yscn = f_box (2, 23, 4, 65)
   @ 3, 25 say "Company Name  "
   if .not. f_getfld (@ylname, 3, 39, "W/N")
      setcolor (ycolor)
      f_restbox (yscn)
      return
   endif
   f_use ("racrp", 2)
   set softseek on
   seek upper (ylname)
   if eof ()
      go bottom
   endif
   set softseek off
   if f_pick_f (2, 2, "", "Company NameÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄCorporate No.ÄÄÄ" + ;
         "Phone No.", "fcompany + if (fhold, [ On Hold ], [ ------- ]) + fcrpno + [ ] + fphone")
      if f_valid (.not. fhold, "Corporate Account is on HOLD status!!!")
         l_fcrpno = racrp->fcrpno
         if .not. empty (racrp->frate)
            l_fcode = racrp->frate
         endif
         l_fdisc = racrp->fdisc
         keyboard chr (13)
      endif
   endif
   set filter to
   f_use ("rares")
   f_restbox (yscn)
elseif yvar = "L_FATC"
   f_use ("raagnt", 1)
   go top
   if f_valid (.not. eof (), "No Travel Agents File Found!!!")
      set softseek on
      seek l_fatc
      if eof ()
         go bottom
      endif
      set softseek off
      if f_pick_f (11, 30, "", "", "fatc + [ - ] + fcompany + [  Tel ] " ;
            + "+ fphone", "")
         l_fatc = raagnt->fatc
         keyboard chr (13)
      endif
   endif
elseif yvar = "L_FAGENT"
   f_use ("raagnt", 2)
   go top
   if f_valid (.not. eof (), "No Travel Agents File Found!!!")
      set softseek on
      seek upper (l_fagent)
      if eof ()
         go bottom
      endif
      set softseek off
      if f_pick_f (11, 30, "", "", "fcompany + [ - ] + fatc + [  Tel ] " ;
            + "+ fphone", "")
         l_fatc = raagnt->fatc
         keyboard chr (5)
      endif
   endif
elseif yvar = "L_FUNIT"    && 08/09/94: specialty vehicle res table
   f_use ("ravm",1)
   set filter to fresv
   go top
   if eof ()
      f_valid (.f., "No Help information for this Field ...")
   else
      do while .t.
         f_pick_f (03, 10, "", "Unit No.ÄÄÄClassÄYrÄColorÄÄÄÄÄÄModelÄÄÄÄÄÄÄ" ;
            + "DescÄÄÄÄÄÄÄÄMiles", "funit + [ ] + fclass + [ ] + fyear + " + ;
            "[ ]+fcolor+[ ]+fmodel+[ ]+substr(fdesc,1,15)+[ ]+str (fmileage, 6)",  ;
            "rrvgrid")
         if lastkey () = 27
            exit
         endif
      enddo
   endif
   select ravm
   set filter to 
   use
else
   f_valid (.f., "No Help information for this Field ...")
endif
setcolor (ycolor)
return

******************************
procedure rrvgrid
private ycolor, yscn, yord, yrecno, yrow

ycolor = setcolor (gsubcolor)
yscn = f_box (05, 15, 16, 70)

@ 06, 16 say "Res #     Check Out    Date/Time Out   Date/Time In  "
@ 15, 25 say "Press any key to continue"

if xtyp = "A"      && add res
   f_use ("rares",2)
else
   select rares
   yord = indexord ()
   yrecno = recno ()
   set order to 2
endif
l_funit = ravm->funit
l_fclass = ravm->fclass
set filter to funit = ravm->funit .and. fresvno <> l_fresvno .and.   ;
             ((fdateout >= l_fdateout .and. fdateout <= l_fdatein) .or.  ;
              (fdatein >= l_fdateout .and. fdatein <= l_fdatein) .or.   ;
              (fdateout <= l_fdateout .and. fdatein >= l_fdatein))
go top
if eof ()
   @ 08,16 say "OPEN "
else
   seek "O"
   yrow = 7
   setcolor (gsubget)
   do while .not. eof () .and. fresvstat = "O"
      @ yrow, 16 say fresvno+[ ]+floc+[  ]+dtoc(fdateout)+[ ]+ftimeout+[  ]+   ;
                     dtoc(fdatein)+[ ]+ftimein
      yrow = yrow + 1
      if yrow > 14
         exit
      endif
      skip
   enddo
endif
set filter to
f_getkey ()
setcolor (ycolor)
f_restbox (yscn)
if xtyp = "U"     && update res
   select rares
   set order to yord
   go yrecno
endif
select ravm

******************************
procedure rrvfmget
*
* revision:
* date: 09/25/92
* edc: new features for insurance replacement rentals
* date: 01/29/93
* edc: add more fields for ins

private yscn, ycolor, yptr, ydate1, ydate2, ydob, ylicexp
ycolor = setcolor (gsubcolor)
yscn = f_box (5, 4, 19, 75)

@ 06, 6 say "Reference#............."
@ 07, 6 say "Last/First Name........"
@ 08, 9 say "License/St/DOB/Exp.."
@ 09, 9 say "Address............."
@ 10, 9 say "City/St/Zip/Phone..."     
@ 11, 9 say "Company/Phone......."
@ 12, 9 say "Ins Co./Policy#....."
@ 13, 9 say "Agent/Phone #......."
@ 14, 6 say "Paying Co./Policy#....."
@ 15, 9 say "Agent/Phone #......."
@ 16, 9 say "Adjuster/Phone #...."

@ 17, 9 say "Maximum amount/days."
@ 18, 6 say "Auto Shop/Phone #......"
setcolor (gsubget)
set century off     && y2k
@ 06, 30 say l_frefno
@ 07, 30 say l_flname
@ 07, 45 say l_ffname
@ 08, 30 say l_flic
@ 08, 50 say l_flicst
@ 08, 54 say l_fdob
@ 08, 65 say l_flicexp
@ 09, 30 say l_faddr
@ 10, 30 say l_fcity
@ 10, 46 say l_fstate
@ 10, 50 say l_fzip
@ 10, 62 say l_fphone
@ 11, 30 say l_fcompany
@ 11, 57 say l_fcphone
@ 12, 30 say l_fcinsur1
@ 12, 52 say l_fcinsur2
@ 12, 65 say l_fcexp1
@ 13, 30 say l_fcagent
@ 13, 62 say l_fcagentph
@ 14, 30 say l_finsur1
@ 14, 52 say l_finsur2
@ 14, 65 say l_fexp1
@ 15, 30 say l_finsagent
@ 15, 62 say l_fagentph
@ 16, 30 say l_fadjuster
@ 16, 62 say l_fadjph
@ 17, 30 say l_fmaxamt pict "9999.99"
@ 17, 39 say l_fmaxday pict "9999"
@ 18, 30 say l_fshop
@ 18, 62 say l_fshopph
yptr = 1
ydate1 = dtoc(l_fcexp1)
ydate2 = dtoc(l_fexp1)
ydob = dtoc(l_fdob)
ylicexp = dtoc(l_flicexp)
do while .t.
   do case
   case yptr = 1
      f_getfld (@l_frefno, 6, 30, "W/N", 0, replicate ("X", 14), .t.)
   case yptr = 2
      f_getfld (@l_flname, 7, 30, "W/N", 0, replicate ("!", 14), .t.)
   case yptr = 3
      f_getfld (@l_ffname, 7, 45, "W/N", 0, replicate ("!", 12), .t.)
* 03.01.04: add new fields for res (PV)
   case yptr = 4
      f_getfld (@l_flic, 8, 30, "W/N", 0, replicate ("X", 19), .t.)
   case yptr = 5
      f_getfld (@l_flicst, 8, 50, "W/N", 0, replicate ("!", 2), .t.)
   case yptr = 6
      ydob = dtoc (l_fdob)
      f_getfld (@ydob, 8, 54, "W/N", 0, "99/99/99", .t.)
      l_fdob = ctod (ydob)
   case yptr = 7
      ylicexp = dtoc (l_flicexp)
      f_getfld (@ylicexp, 8, 65, "W/N", 0, "99/99/99", .t.)
      l_flicexp = ctod (ylicexp)
      * if .not. f_valid (f_expdate (l_flicexp),"Warning: Date Expired!")
      *    loop
      * endif
*
   case yptr = 8
      f_getfld (@l_faddr, 9, 30, "W/N", 0, replicate ("X", 25), .t.)
   case yptr = 9
      f_getfld (@l_fcity, 10, 30, "W/N", 0, replicate ("X", 15), .t.)
   case yptr = 10
      f_getfld (@l_fstate, 10, 46, "W/N", 0, replicate ("!", 2), .t.)
   case yptr = 11
      f_getfld (@l_fzip, 10, 50, "W/N", 0, "XXXXX-XXXX", .t.)
   case yptr = 12
      f_getfld (@l_fphone, 10, 62, "W/N", 0, "999-999-9999", .t.)
* 07.01.04: add new field 
   case yptr = 13
      f_getfld (@l_fcompany, 11, 30, "W/N", 0, replicate ("!", 25), .t.)
   case yptr = 14
      f_getfld (@l_fcphone, 11, 57, "W/N", 0, "999-999-9999", .t.)
*
   case yptr = 15
      f_getfld (@l_fcinsur1, 12, 30, "W/N", 0, replicate ("X", 20), .t.)
   case yptr = 16
      f_getfld (@l_fcinsur2, 12, 52, "W/N", 0, replicate ("X", 12), .t.)
   case yptr = 17
      f_getfld (@ydate1, 12, 65, "W/N", 0, "99/99/99", .t.)
      l_fcexp1 = ctod (ydate1)
      if .not. f_valid (f_expdate (l_fcexp1)  ;
         .or. (empty(l_fcinsur1).and.empty(l_fcexp1)),  ; 
               "Warning! Date Expired!")
         loop
      endif
   case yptr = 18
      f_getfld (@l_fcagent, 13, 30, "W/N", 0, replicate ("X", 30), .t.)
   case yptr = 19
      f_getfld (@l_fcagentph, 13, 62, "W/N", 0, "999-999-9999", .t.)
   case yptr = 20
      f_getfld (@l_finsur1, 14, 30, "W/N", 0, replicate ("X", 20), .t.)
   case yptr = 21
      f_getfld (@l_finsur2, 14, 52, "W/N", 0, replicate ("X", 12), .t.)
   case yptr = 22
      f_getfld (@ydate2, 14, 65, "W/N", 0, "99/99/99", .t.)
      l_fexp1 = ctod (ydate2)
      if .not. f_valid (f_expdate (l_fexp1) ;
         .or. (empty(l_finsur1).and.empty(l_fexp1)),  ; 
               "Warning! Date Expired!")
         loop
      endif
   case yptr = 23
      f_getfld (@l_finsagent, 15, 30, "W/N", 0, replicate ("X", 30), .t.)
   case yptr = 24
      f_getfld (@l_fagentph, 15, 62, "W/N", 0, "999-999-9999", .t.)
   case yptr = 25
      f_getfld (@l_fadjuster, 16, 30, "W/N", 0, replicate ("X", 30), .t.)
   case yptr = 26
      f_getfld (@l_fadjph, 16, 62, "W/N", 0, "999-999-9999", .t.)
   case yptr = 27
      f_getnum (@l_fmaxamt, 17, 30, "", "9999.99", .t.)
   case yptr = 28
      f_getnum (@l_fmaxday, 17, 39, "", "9999", .t.)
   case yptr = 29
      f_getfld (@l_fshop, 18, 30, "W/N", 0, replicate ("X", 25), .t.)
   case yptr = 30
      f_getfld (@l_fshopph, 18, 62, "W/N", 0, "999-999-9999", .t.)
   endcase
   ykey = lastkey ()
   if (ykey = 24 .or. ykey = 13) .and. yptr < 30
      yptr = yptr + 1
   elseif ykey = 5 .and. yptr > 1
      yptr = yptr - 1
   elseif ykey = 27 .or. ykey = 13 .or. ykey = 3 .or. ykey = 18
      exit
   endif
enddo
f_restbox (yscn)
setcolor (ycolor)
set century on     && y2k

******************************
function rrvfmup0

if f_valid (f_verify("raloc",1,l_floc),"Invalid Location...")
   l_frloc = l_floc
   return .t.
else
   return .f.
endif

******************************
function rrvfmup1

if f_valid (f_y2k (@l_fdateout) .and. l_fdays >= 0)
   l_fdatein = l_fdateout + l_fdays
   return .t.
else
   return .f.
endif


******************************
function rrvfmup2

if f_valid (f_y2k (@l_fdatein) .and. l_fdatein >= l_fdateout)
   if l_fdays <> (l_fdatein - l_fdateout)
      l_fdays = l_fdatein - l_fdateout
      *  keyboard chr (5) + chr (5)       && 12/21/93 (edc): do not go back
      @ 06, 23 say l_fdays pict "999"
   endif
   return .t.
else
   return .f.
endif


******************************
function rrvfmup3

if l_fcustno = ycustno .or. empty (l_fcustno)
   return .t.
endif
if f_valid (f_verify ("RACUST", 2, l_fcustno), "Invalid Customer Number!!!")
   if f_valid (.not. racust->fhold, "Customer is on HOLD Status!!!")
      ycustno = l_fcustno
      l_flname = racust->flname
      l_ffname = racust->ffname
      if .not. empty (racust->frate)
         l_fcode = racust->frate
      endif
      * 06.30.04
      l_flic = racust->flic
      l_flicst = racust->flicst
      l_fcompany = racust->fcompany    && new rares field
      l_fcphone = racust->fcphone      && new rares field
      *
      l_fdob = racust->fbirthdt
      l_flicexp = racust->fexpdt
      l_faddr = racust->faddr
      l_fcity = racust->fcity
      l_fstate = racust->fstate
      l_fzip = racust->fzip
      l_fphone = racust->fphone
      l_fdisc = racust->fdisc
      l_fccnum = racust->fccnum
      l_fcctype = racust->fcctype
      l_fccexp = racust->fccexp
      f_valid (empty (racust->frmk1), alltrim (racust->frmk1))
      f_valid (empty (racust->frmk2), alltrim (racust->frmk2))

      * --12.18.08
      l_finsur1 = racust->finsur1
      l_finsur2 = racust->finsur2
      l_finsdate = racust->finsdate
      l_finsver = racust->finsver

      setcolor (gblueget)
      @ 12,23 say l_flname
      @ 13,23 say l_ffname
      @ 14,23 say l_fphone
      @ 15,23 say l_fccnum
      @ 16,23 say l_fcctype
      @ 16,28 say l_fccexp
      @ 17,23 say l_finsur1
      @ 18,23 say l_finsur2
      @ 19,23 say l_finsdate
      @ 20,23 say l_finsver
      setcolor (gbluecolor)

      * --12.18.08

      return .t.
   else
      return .f.
   endif
else
   return .f.
endif


******************************
function rrvfmup4

if l_fcrpno = ycrpno .or. empty (l_fcrpno)
   return .t.
endif
f_use ("racrp", 1)
seek l_fcrpno
if f_valid (found ())
   if f_valid (.not. fhold, "Corporate account is on HOLD Status!!!")
      ycrpno = l_fcrpno
      if .not. empty (frate)
         l_fcode = frate
      endif
      l_fdisc = fdisc
      f_valid (empty (frmk1), alltrim (frmk1))
      f_valid (empty (frmk2), alltrim (frmk2))
      f_valid (empty (frmk3), alltrim (frmk3))
      return .t.
   else
      return .f.
   endif
else
   return .f.
endif


******************************
function rrvfmup5

private yflag, yord, yrecno

if lastkey () = 5 .or. empty (l_funit)
   return .t.
endif

*if empty (l_funit) .or. (l_funit = yunit .and. ;
*      (dtos (l_fdateout) + l_ftimeout) = yrdateout ;
*      .and. (dtos (l_fdatein) + l_ftimein = yrdatein))
*   return .t.
*endif

*if empty (l_funit) .or. l_funit = yunit
*   return .t.
*endif

f_use ("RAVM", 1)
seek l_funit
if .not. f_valid (found (), "Invalid Unit #!!!")
   use 
   return .f.
elseif .not. f_valid (fresv, "This unit CANNOT be reserved !")
   use
   return .f.
endif
* 08/09/94: specialty vehicle res table
if xtyp = "A"      && add res
   f_use ("rares",2)
else
   select rares
   yord = indexord ()
   yrecno = recno ()
   set order to 2
endif
seek "O"
yflag = .t.
do while .not. eof() .and. fresvstat = "O"
   if funit <> ravm->funit .or. fresvno = l_fresvno
      skip
      loop
   endif
   if l_fdateout > fdateout .and. l_fdateout < fdatein
      yflag = .f.
      exit
   elseif l_fdatein > fdateout .and. l_fdatein < fdatein
      yflag = .f.
      exit
   elseif l_fdateout <= fdateout .and. l_fdatein >= fdatein
      yflag = .f.
      exit
   elseif l_fdateout = fdateout .and. l_ftimeout >= ftimeout
      yflag = .f.
      exit
   elseif l_fdateout = fdatein .and. l_ftimeout <= ftimein
      yflag = .f.
      exit
   endif
   skip
enddo
l_fclass = if(yflag,ravm->fclass,l_fclass)
select ravm 
use
if xtyp = "U"     && update res
   select rares
   set order to yord
   go yrecno
endif
if yflag
   return .t.
else
   f_valid (.f., "This unit CANNOT be reserved due to schedule conflict !")
   return .f.
endif

*elseif (dtos (l_fdateout) + l_ftimeout) >= yrdateout ;
*      .or. (dtos (l_fdatein) + l_ftimein <= yrdatein)
*   if f_validres (l_funit, l_fdateout, l_ftimeout, l_fdatein, ;
*         l_ftimein, "R" + l_fresvno)
*      f_markvres (l_funit, l_fdateout, l_ftimeout, l_fdatein, ;
*         l_ftimein)
*      if l_funit <> yunit .or. empty (l_fclass)
*         l_fclass = ravm->fclass
*      endif
*      yunit = l_funit
*      yrdateout = dtos (l_fdateout) + l_ftimeout
*      yrdatein = dtos (l_fdatein) + l_ftimein
*      select ravm
*      use
*      return .t.
*   else
*      select ravm
*      use
*      return .f.
*   endif
*endif

******************************
function rrvfmup6
private yok, i, y1, ynet

if lastkey () = 5
   return .t.
endif

if .not. f_valid (.not. empty (l_fclass))
   return .f.
endif
if l_fcode = yrate .and. l_fclass = yclass .and. l_floc = yloc .and. ;
      l_fdateout = ydateout
   if .not. yfreesell
      keyboard replicate (chr (13), 12)
   endif
   return .t.
endif

* implement block rate
if block_rate ()
   return .f.
endif

f_use ("rartm")
if f_valid (f_getrate (l_fcode, l_floc, l_fclass, l_fdateout), ;
      "Rate Not Found!!!")
   if f_valid (rartm->fresvok = 0 .or. rartm->fresvcnt < rartm->fresvok * ;
         (1 + rartm->fresvovr / 100), "Reservation Exceeds Limit!!!")
      l_fdlychg = rartm->fdlychg
      l_fdlymlg = rartm->fdlymlg
      * l_fxdlychg = rartm->fxdlychg      && 02.26.02
      l_fwkdchg = rartm->fwkdchg
      l_fwkdmlg = rartm->fwkdmlg
      l_fwkdmin = rartm->fwkdmin
      l_fwkdmax = rartm->fwkdmax
      l_fwkchg = rartm->fwkchg
      l_fwkmlg = rartm->fwkmlg
      l_fmthchg = rartm->fmthchg
      l_fmthmlg = rartm->fmthmlg
      l_fhrchg = rartm->fhrchg
      l_fmlgchg = rartm->fmlgchg
      l_fcdw = rartm->fcdwchg
      setcolor (gblueget)
      @ 4, 56 say l_fdlychg picture [9999.99]
      @ 4, 64 say l_fdlymlg picture [9999]
      @ 5, 56 say l_fwkdchg picture [9999.99]
      @ 5, 64 say l_fwkdmlg picture [9999]
      @ 6, 56 say l_fwkchg  picture [9999.99]
      @ 6, 64 say l_fwkmlg  picture [9999]
      @ 7, 56 say l_fmthchg picture [9999.99]
      @ 7, 64 say l_fmthmlg picture [9999]
      @ 8, 56 say l_fhrchg  picture [9999.99]
      @ 9, 56 say l_fmlgchg picture [9999.99]
      @ 10, 60 say l_fwkdmin picture [99]
      @ 10, 68 say l_fwkdmax picture [99]
      setcolor (gbluecolor)
      yrate = l_fcode
      yclass = l_fclass
      yloc = l_floc
      ydateout = l_fdateout
      if .not. yfreesell
         keyboard replicate (chr (13), 12)
      endif

      yok = .t.
      * check availability
      if xtyp = "A"
         f_use ("RAPROJ")
         y1 = l_fdatein - l_fdateout
         y1 = if(y1 > 14, 14, y1)
         for i = 0 to y1
            seek l_fclass + dtos(l_fdateout+i)
            if .not. eof ()
               if fnet <= 0
                  yok = .f.
                  exit
               endif
            endif
         next i
         if .not. yok
            f_pick_f (07, 04, "",   ;
                      "ÄÄDateÄÄÄÄÄÄÄAvailÄÄÄDueÄÄÄHoldÄÄÄÄResÄÄÄNet",   ;
                      "[  ]+dtoc(fdate)+[ ]+str(favail,6)+[ ]+" +  ;
                      "str(fdue,6)+[ ]+str(fhold,6)+[ ]+str(fres,6)+[ ]+str(fnet,6)", ;
                      "X","fclass+dtos(fdate)","l_fclass")
            f_valid (.f., "Warning: "+l_fclass+[ ]+"Oversell...")
            if f_confirm ("[R]etype  [C]ontinue  ", "RC") = "C"
               yok = .t.
            else
               yclass = space(4)     && force check next time around
            endif
         endif
         use
      endif
      *
      return yok
   else
      keyboard chr (5)
      return .f.
   endif
else
   keyboard chr (5)
   return .f.
endif

******************************
function block_rate

yblock = .f.

f_use ("rartml")
seek l_floc+l_fcode+l_fclass

do while .not. eof () .and. rartml->floc=l_floc .and. rartml->fcode=l_fcode  ;
          .and. rartml->fclass = l_fclass .and. .not. yblock
   yblock = check_block ()
   skip
enddo

if yblock
   f_valid (.f.,"Rate is blocked for this period...")
   use
   return .t.
endif

* handle rate code for ALL locations
seek space(10)+l_fcode+l_fclass
do while .not. eof () .and. rartml->floc=space(10) .and. rartml->fcode=l_fcode  ;
          .and. rartml->fclass = l_fclass .and. .not. yblock
   yblock = check_block ()
   skip
enddo
use
if yblock
   f_valid (.f.,"Rate is blocked for this period...")
   return .t.
else
   return .f.
endif

*********************
function check_block

zblock = .f.
if .not. empty(rartml->fdtfrom1)
   if l_fdateout >= rartml->fdtfrom1 .and. l_fdateout <= rartml->fdtto1
       zblock = .t.
   endif
   *if l_fdatein >= rartml->fdtfrom1 .and. l_fdatein <= rartml->fdtto1
   *    zblock = .t.
   *endif
endif
if .not. empty(rartml->fdtfrom2)
   if l_fdateout >= rartml->fdtfrom2 .and. l_fdateout <= rartml->fdtto2
       zblock = .t.
   endif
   *if l_fdatein >= rartml->fdtfrom2 .and. l_fdatein <= rartml->fdtto2
   *    zblock = .t.
   *endif
endif
if .not. empty(rartml->fdtfrom3)
   if l_fdateout >= rartml->fdtfrom3 .and. l_fdateout <= rartml->fdtto3
       zblock = .t.
   endif
   *if l_fdatein >= rartml->fdtfrom3 .and. l_fdatein <= rartml->fdtto3
   *    zblock = .t.
   *endif
endif
return (zblock)

******************************
function rrvfmup7

if empty (l_fatc)              && .or. l_fatc = yatc
   return .t.
endif
f_use ("raagnt", 1)
seek l_fatc
if found ()
   if .not. (empty(raagnt->fcompany).or.empty(raagnt->faddr).or. ;
      raagnt->fstd=0)
      yatc = l_fatc
      l_fagent = raagnt->fcompany
      do rrvfmup8
      l_fcommpct = ycomm
      return .t.
   endif
endif
tone (500, 9)
if f_confirm ("Do You Want To Update Travel Agent File ? [Y/N] ", ;
      "YN") = "N"
   yatc = l_fatc
   return .t.
endif

set cursor on
private yscn, ycolor, yrcompany, ystcomm, ycmcomm, yspcomm, yptr
private yaddr, yaddr1, ycity, ycontact, ystate, yzip, yphone, yfax

ycolor = setcolor (gsubcolor)
yscn = f_box (09, 25, 22, 77)
@ 10, 27 say "ATC Number .. "
@ 11, 27 say "Company ..... "
@ 12, 27 say "Address ..... "
@ 14, 27 say "City ........ "
@ 15, 27 say "State/Zip ... "
@ 16, 27 say "Contact ..... "
@ 17, 27 say "Phone ....... "
@ 18, 27 say "Fax ......... "
@ 19, 30 say "Standard ........ "
@ 20, 30 say "Consortium ...... "
@ 21, 30 say "Special ......... "

if eof ()
   yrcompany = space(30)
   ystcomm = 0
   ycmcomm = 0
   yspcomm = 0
   store space (20) to ycontact
   store space (30) to yaddr, yaddr1
   store space (15) to ycity
   ystate = "  "
   yzip = "     -    "
   store "   -   -    " to yfax, yphone
else
   yrcompany = raagnt->fcompany
   ystcomm = raagnt->fstd
   ycmcomm = raagnt->fcsm
   yspcomm = raagnt->fspecial
   ycontact = raagnt->fcontact
   yaddr = raagnt->faddr
   yaddr1 = raagnt->faddr1
   ycity = raagnt->fcity
   ystate = raagnt->fstate
   yzip = raagnt->fzip
   yfax = raagnt->ffax
   yphone = raagnt->fphone
endif

@ 10, 41 say l_fatc
setcolor (gsubget)
@ 11, 41 say yrcompany
@ 12, 41 say yaddr
@ 13, 41 say yaddr1
@ 14, 41 say ycity
@ 15, 41 say ystate
@ 15, 44 say yzip
@ 16, 41 say ycontact
@ 17, 41 say yphone
@ 18, 41 say yfax
@ 19, 48 say ystcomm picture "99"
@ 20, 48 say ycmcomm picture "99"
@ 21, 48 say yspcomm picture "99"

yptr = 1
do while .t.
   do case
   case yptr = 1
      f_getfld (@yrcompany, 11, 41, "W/N", 0, replicate ("X", 30), .t.)
   case yptr = 2
      f_getfld (@yaddr, 12, 41, "W/N", 0, replicate ("X", 30), .t.)
   case yptr = 3
      f_getfld (@yaddr1, 13, 41, "W/N", 0, replicate ("X", 30), .t.)
   case yptr = 4
      f_getfld (@ycity, 14, 41, "W/N", 0, replicate ("X", 15), .t.)
   case yptr = 5
      f_getfld (@ystate, 15, 41, "W/N", 0, "!!", .t.)
   case yptr = 6
      f_getfld (@yzip, 15, 44, "W/N", 0, "XXXXX-XXXX", .t.)
   case yptr = 7
      f_getfld (@ycontact, 16, 41, "W/N", 0, replicate ("X", 20), .t.)
   case yptr = 8
      f_getfld (@yphone, 17, 41, "W/N", 0, "999-999-9999", .t.)
   case yptr = 9
      f_getfld (@yfax, 18, 41, "W/N", 0, "999-999-9999", .t.)
   case yptr = 10
      f_getnum (@ystcomm, 19, 48, "W/N", "99", .t.)
   case yptr = 11
      f_getnum (@ycmcomm, 20, 48, "W/N", "99", .t.)
   case yptr = 12
      f_getnum (@yspcomm, 21, 48, "W/N", "99", .t.)
   endcase
   ykey = lastkey ()
   if (ykey = 24 .or. ykey = 13) .and. yptr < 12
      yptr = yptr + 1
   elseif ykey = 5 .and. yptr > 1
      yptr = yptr - 1
   elseif ykey = 27 .or. ykey = 13 .or. ykey = 3 .or. ykey = 18
      exit
   endif
enddo
f_restbox (yscn)
setcolor (ycolor)

f_use ("raagnt")
seek l_fatc
if eof ()
   append blank
endif
reclock ()
replace fatc with l_fatc, fcompany with yrcompany
replace fstd with ystcomm, fcsm with ycmcomm, fspecial with yspcomm
replace faddr with yaddr, faddr1 with yaddr1, fcity with ycity, fcontact with ycontact
replace fstate with ystate, fzip with yzip, fmoddt with date ()
replace fphone with yphone, ffax with yfax
commit
unlock
* f_fupdate ("A")

yatc = l_fatc
l_fagent = raagnt->fcompany
do rrvfmup8
l_fcommpct = ycomm
return .t.


******************************
procedure rrvfmup8

private yarray [3], yptr
yarray [1] = "Standard Commission .... " + str (fstd, 2) + "%"
yarray [2] = "Consortium Commission .. " + str (fcsm, 2) + "%"
yarray [3] = "Special Commission ..... " + str (fspecial, 2) + "%"

yptr = f_pick_a (15, 40, "", "", YARRAY, 3, 1)
if yptr = 2
   ycomm = fcsm
elseif yptr = 3
   ycomm = fspecial
else
   ycomm = fstd
endif


******************************
function rrvfmup9

private yrecno, yfnd
if empty (l_fresvno)
   return f_valid (.f.)
endif
if yresvno = l_fresvno
   return .t.
endif
f_use ("rares", 1)
if xtyp = "A"
   seek l_fresvno
   *return f_valid (.not. found (), "Reservation Number Already Exist!!!")
   if eof ()
      return .t.
   else
      if f_confirm ("Warning: Duplicate Reservation # !" + ;
         " Continue ?[Y/N]", "YNyn") $ "Yy"
         return .t.
      else
         return .f.
      endif
   endif  
else
   yrecno = recno ()
   seek l_fresvno
   yfnd = .not. found () .or. yrecno = recno ()
   go yrecno
   return f_valid (yfnd, "Reservation Number Already Exist!!!")
endif


******************************
function rrvfmup10

if .not. f_valid (.not. empty (l_flname))
   return .f.
endif

if .not. yfreesell .and. lastkey () = 5
   keyboard replicate (chr (5), 12)
endif

* 10.06.09: add est. charge (FESTCHG)
l_festchg = rrvestchg ()

return .t.


******************************
function f_rvstat

parameter xstat

do case
case xstat = "O"
   return "OPEN   "
case xstat = "X"
   return "USED   "
case xstat = "U"
   return "CLOSED "
case xstat = "C"
   return "CANCEL "
case xstat = "N"
   return "NO SHOW"
otherwise
   return "UNKNOWN"
endcase

**********************
* 02.26.02: implement xtra day charge
function rrvfmup11

*ycolor = setcolor (gsubcolor)
*yscn = f_box (7, 50, 9, 70)
*@ 8, 52 say "Xtra Day... "
*f_getnum (@l_fxdlychg, 8, 63, "", "9999.99", .t.)
*setcolor (ycolor)
*f_restbox (yscn)

return .t.

* ==================================
* 07.29.09: est. rental charge
function rrvestchg

private y1, y2, y3, yto, yti, yrtot, yfdays, yfhr
private ymhr, ytotd, ytotw, ytotm, yrdays, yrwk, yrmlg, yrmlgd, yrmlgw, yrmlgm
private yydays

* initialize variables
l_fdly = 0
l_fwkd = 0
l_fwkdtot = 0
l_fdlytot = 0
l_fwktot = 0
l_fmthtot = 0
l_fhrtot = 0
* --
yto = val (substr (l_ftimeout, 1, 2)) * 60 + ;
      val (substr (l_ftimeout, 4, 2))
yti = val (substr (l_ftimein, 1 ,2)) * 60 + val (substr (l_ftimein, 4, 2))
ymins = (l_fdatein - l_fdateout) * 24 * 60 + yti - yto
yydays = int (ymins / 1440)
ymins = ymins - yydays * 1440

ydaychg = if(ymins<=0,yydays,yydays + 1)

l_fhr = int (ymins / 60)
ymins = ymins - l_fhr * 60
l_fhr = if(ymins>0, l_fhr+1, l_fhr)

l_frhr = l_fhr
l_fdly = yydays
l_fcdwdays = ydaychg     && use by surchg formaula
l_fpaidays = ydaychg

if (l_fhr * l_fhrchg) >= l_fdlychg .and. l_fdlychg > 0
   l_fdly = l_fdly + 1
   l_fhr = 0
endif

store 99999.99 to ytotd, ytotw, ytotm, yrtot

yfdays = l_fdly
yfhr = l_fhr              && save for week/month calc.

* estimate daily chg 01/07/95:(edc)
if l_fdlychg > 0.00
   ytotd = l_fdly * l_fdlychg + l_fhr * l_fhrchg 
   yrtot = ytotd
endif

yrdays = l_fdly
yrhr = l_fhr
* estimate weekly chg
l_fwk = 0
if l_fwkchg > 0.00     
   * --02.20.09     
      l_fwk = int (yfdays/7)
      l_fdly = yfdays - l_fwk * 7
      l_fhr = yfhr
      if l_fdly * l_fdlychg + l_fhr * l_fhrchg > l_fwkchg
         l_fwk = l_fwk + 1
         l_fdly = 0
         l_fhr = 0
      endif
      if l_fdly > 0 .and. l_fdlychg <= 0
         l_fwk = l_fwk + 1
         l_fdly = 0
         l_fhr = 0
      endif
      if l_fwk > 0
         ytotw = l_fwk * l_fwkchg + l_fdly * l_fdlychg +   ;
                 l_fhr * l_fhrchg 
         if ytotw > ytotd
            l_fwk = 0
            l_fdly = yrdays 
            l_fhr = yrhr
         else
            yrtot = ytotw
         endif
      else
         * restore old values
         l_fdly = yrdays 
         l_fhr = yrhr
      endif

   * --02.20.09
   l_fwktot = l_fwkchg * l_fwk
else
   l_fwktot = 0.00
endif


if l_fdlychg > 0.00
   l_fdlytot = l_fdlychg * l_fdly
elseif l_fwkchg > 0.00
   if l_fdly > 0      
      l_fwk = l_fwk + 1
      l_fwktot = l_fwktot + l_fwkchg
      l_fdly = 0
      l_fhr = 0 
   endif
endif
l_fhrtot = l_fhr * l_fhrchg
l_ftmetot = l_fdlytot + l_fwktot + l_fhrtot
l_fmlgtot = 0

* -- variables used by surcharge formula
l_fmlgtot = 0
l_fcdwtot = 0
l_fpaitot = 0
l_fdisctot = 0
l_fdmgtot = 0
l_ffueltot = 0
l_fcredtot = 0
l_fotot1 = 0
l_fotot2 = 0
l_fotot3 = 0
l_fotot4 = 0
l_fotot5 = 0
l_fotot6 = 0

l_fothtot1 = 0.00          && taxable
l_fothtot2 = 0.00          && non taxable
* -- Surcharge
l_fsurchg = 0
l_fsurchg1 = 0
if .not. empty(gsurchg1)     && 03.10.09: MUST calculate this first ...
   l_fsurchg1 = &gsurchg1
endif

* -- rely on surcharge formula:: set other charge 2 always equal to SUR1

* if l_floc = "JAC"            && 06.30.09: only JAC
*    l_fotot1 = l_fsurchg1        
* endif

* --

if .not. empty(gsurchg)      && 03.10.09: JAC surcharge formula includes surchg1 ...
   l_fsurchg = &gsurchg
endif

* --10.20.09: calculate discount if any
if l_fdisc > 0
   l_fdisctot = round (l_fdisc * (l_fdlytot + l_fwkdtot + l_fwktot + l_fmthtot + ;
                l_fhrtot + l_fmlgtot) / 100.00, 2)
endif
* --

ytot1 = l_ftmetot + l_fmlgtot - l_fdisctot ;
        + if (gsurtx, l_fsurchg, 0.00) + if (gsurtx1, l_fsurchg1, 0.00) 

l_ftaxtot = ytot1 * gtaxrate / 100.00

ytot2 = if (gsurtx, 0.00, l_fsurchg) + if (gsurtx1, 0.00, l_fsurchg1) 
                                                            
ytotal = round (ytot1 + ytot2 + l_ftaxtot, 2)

return ytotal
