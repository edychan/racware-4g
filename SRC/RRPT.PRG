* ===========================================================================
* report module
*
* date: 05/01/91
* author: edc
*
* revision
* 03/04/94 (edc): fix missing ra report.
* 07.31.98 (edc): year2000
*
* 04.22.07: new marketing tally report (PV)
* 09.24.07: new Monthy Customer Activity (interface with Excel) MGMT_10
* ---
* 07.21.11: group by car class for monthly vehicle tally report
* 07.21.11: gorup by car class/month for vehicle revenue report
* ===========================================================================
private yret, yfile, i, yfld, ytitle, ycond, yopt, yfilter

rest from (gmempath + "rarpt") additive
bliovlclr ()

if empty (gete ("RACCPRT"))
   l_cprt = chr (27) + chr (15)
else
   * l_cprt = alltrim (upper (gete ("RACCPRT")))
   l_cprt = alltrim (gete ("RACCPRT"))      && 03.13.08: PCL is case sensitive
   l_cprt = &l_cprt
endif
if empty (gete ("RACNPRT"))
   l_nprt = chr (18)
else
   * l_nprt = alltrim (upper (gete ("RACNPRT")))
   l_nprt = alltrim (gete ("RACNPRT"))      && 03.13.08
   l_nprt = &l_nprt
endif

do while .t.
   xret1 = f_pushmenu (xoption0)
   blimempak (-1)
   if xret1 = 0
      exit
   endif
   xoption1 = substr (goption [xret1], 4)
   f_use ("rarpt")
   set filter to
   do case
   case xret1 = 1
      f_clrscn ("RENTAL & OPERATION REPORT")
      yfilter = "#1"
   case xret1 = 2
      f_clrscn ("RESERVATION REPORT")
      yfilter = "#2"
   case xret1 = 3
      f_clrscn ("CREDIT CARD REPORT")
      yfilter = "#3"
   case xret1 = 4
      f_clrscn ("INVENTORY REPORT")
      yfilter = "#4"
   case xret1 = 5
      f_clrscn ("MARKETING & SALE REPORT")
      yfilter = "#5"
   case xret1 = 6
      private ychoice [10]
      do while .t.
         f_clrscn ("MANAGEMENT REPORT")
         l_formlen = 132
         l_page = 0
         l_date = dtoc (date ())
         l_time = time ()
         ychoice[1] = "Vehicle Utilization Report   "
         ychoice[2] = "Vehicle Tally Report         "
         ychoice[3] = "Vehicle Revenue Report       "
         ychoice[4] = "RA Freqency Report           "
         ychoice[5] = "Missing RA Report            "
         ychoice[6] = "Travel Agent Commission      "
         ychoice[7] = "Add'l Revenue Summary Report "
         ychoice[8] = "Rental Days Projection       "
         ychoice[9] = "Marketing Tally Report       "
         ychoice[10] = "Monthly Customer Activity   "
         yptr = 1
         yret = f_pick_a (02, 05, "", "", ychoice, 10, yptr)
         if yret = 0
            exit
         endif
         if yret = 1
            do mgmt_1  
         elseif yret = 2
            do mgmt_2
         elseif yret = 3
            do mgmt_3
         elseif yret = 4
            do mgmt_4
         elseif yret = 5
            do mgmt_5
         elseif yret = 6
            do mgmt_6
         elseif yret = 7
            do mgmt_7
         elseif yret = 8
            do mgmt_8
         elseif yret = 9
            do mgmt_9
         elseif yret = 10
            do mgmt_10
         endif
      enddo
      loop
   case xret1 = 7
      do rrptrr
      loop
   endcase
   seek yfilter
   if .not. eof()
      if f_pick_f (02, 3, "", "", "[ -> ]+ftitle+[ <- ]","","fname","yfilter")
         yrname = rarpt->fname
         ytitle = ""
         ycond = ""
         yret = f_rr (yrname, ytitle)
         if yret < 0
            tone(500, 9)
            f_popup ("Invalid Report Setup... Error: "+str(yret,3) ;
               + ". Press Any Key...",.t.)
         endif
      endif
   else
      tone (500, 9)
      f_popup ("File is Empty. Press Any Key to Continue...",.t.)
   endif
enddo

release all like l_*
close database

******************************
* help function for mgmt reports
function mgmt_hdr

parameters xdisp, xfeed
private i, yln, ylen, ycol, ydesc
if xdisp = "P"
   if xfeed
      eject
   else
      setprc (0, 0)
      @ 00, 01 say l_cprt
   endif
elseif xfeed
   @ yrow+1, 0 say ""
   setprc (0, 0)   
   @ 0, 0 say replicate ("Ä", l_formlen-1)
else 
   setprc (0,0)
endif
l_page = l_page + 1
@ 01, 01 say 'DATE: ' + l_date
@ 01, yc1 say gtitle
@ 01, yc3 say 'PAGE: ' + str (l_page, 3)
@ 02, 01 say 'TIME: ' + l_time
@ 02, yc2 say l_ftitle
@ 04, 01 say l_header
yln = 6

return (yln)

**********************************
function mgmt_end
parameter xrow

if ydisp = "P"
   @ xrow + 1, 0 say l_nprt
   eject
endif

set device to screen
set printer to
set console on
set print off
if ydisp = "D"
   yfil=gstnpath+"rrout.rpt"
   setcolor ("W/N")
   clear
   run racbrow &yfil
   setcolor (gbluecolor)
else
   f_valid (.f., "Process completed")
endif

*********************************
* return report destination for mgmt reports
function mgmt_option
private ypick

gemrpt = .f.      && disable email option for now...
* 06.21.11: add email report function
if gemrpt
   ypick = f_confirm ("[D]isplay on Screen  [P]rint  [F]ile  [E]mail  [Q]uit", "DPFEQ")    && add email
else
   ypick = f_confirm ("[D]isplay on Screen  [P]rint  [F]ile  [Q]uit", "DPFQ")    
endif
return ypick

***********************************
* return filename 
* pass by reference
function mgmt_getfil
parameter xfile
private yfile
yfile = space(40)
do while .t.
   @ 07, 00
   @ 07, 05 say "File Name: " get yfile 
   if f_rd() = 27
      return .f.
   else
      ykeyin = f_confirm ("Is this correct [Y/N/Q] ", "YNQ")
      if ykeyin = "Y"
         @ 07, 00
         exit
      elseif ykeyin = "Q"
         return .f.
      endif
   endif
enddo
xfile = yfile
return .t.

****************************************
* return email
* pass by reference
function mgmt_getem
parameter xemail, xfile
private yemail, yfile
yemail = f_truncate(gemail,40)
* -- add email function: 06.21.11
do while .t.
   @ 07, 00
   @ 07, 05 say "email: " get yemail pict replicate ("x", 40) valid ;
     f_valid (f_goodem (yemail, .f.), "Invalid email address ...")     && cannot enter NA as valid email 
   if f_rd() = 27
      return .f.
   else
      ykeyin = f_confirm ("Is this correct [Y/N/Q] ", "YNQ")
      if ykeyin = "Y"
         @ 07, 00
         yfile = gempath + "rr"+strtran(time(),":","")+".txt"
         exit
      elseif ykeyin = "Q"
         return .f.
      endif
   endif
enddo
xemail = yemail
xfile = yfile
return .t.

*********************************
* set file destination
function mgmt_dest 
parameter xoption, xfile

f_popup ("Creating Report ...")
if xoption $ "DEF"        && 06.21.11: add Email
   set device to print
   set printer to &xfile
else
   * set device to print
   set console off
   set print on
   set device to print
   set printer to lpt1
endif

**********************************
procedure mgmt_1
private y1, y2, yt, yrow, ycol, ystr, ydisp
private nloc, yloc [10], ytotal [10], yatot[10], yrtot[10]
private nclass, yclass[50]      && 01.22.09: increase from 25 to 50
private yfilename

f_box (02, 05, 04, 78, "You have selected")
@ 03, 07 say "Vehicle Utilization Report"

* 01.22.09 -- ydisp = f_confirm ("[D]isplay on Screen  [P]rint [Q]uit", "DPQ")
ydisp = f_confirm ("[D]isplay on Screen  [P]rint  [F]ile  [Q]uit", "DPFQ")
if ydisp ="Q"
   return
endif

* -- 01.22.09
yfilename = space(40)
do while ydisp = "F"
   @ 07, 03 say "File Name.."
   @ 07, 15 get yfilename
   if f_rd() = 27
      return 
   else
      ykeyin = f_confirm ("Is this correct [Y/N/Q] ", "YNQ")
      if ykeyin = "Y"
         exit
      elseif ykeyin = "Q"
         return 
      endif
   endif
enddo
* --

f_popup ("Please Wait...")
f_use ("ravm")
select 0
if .not. file (gstnpath + "mgmt_1.dbf")
   create (gstnpath + "stru")
   use (gstnpath + "stru") exclusive
   append blank
   replace field_name with "FLOC"
   replace field_type with "C"
   replace field_len with 10
   replace field_dec with 0
   append blank
   replace field_name with "FCLASS"
   replace field_type with "C"
   replace field_len with 4
   replace field_dec with 0
   append blank
   replace field_name with "FAVAIL"
   replace field_type with "N"
   replace field_len with 5
   replace field_dec with 0
   append blank
   replace field_name with "FRENTED"
   replace field_type with "N"
   replace field_len with 5
   replace field_dec with 0
   append blank
   replace field_name with "FTOTAL"
   replace field_type with "N"
   replace field_len with 5
   replace field_dec with 0
   use
   create (gstnpath + "mgmt_1") from (gstnpath + "stru")
   erase (gstnpath + "stru.dbf")
endif
use (gstnpath + "mgmt_1") exclusive alias mgmt_1
index on floc + fclass to (gstnpath + "mgmt_1")
set index to (gstnpath + "mgmt_1")
zap

nclass = 0
y1 = ""
afill (yatot, 0)
afill (yrtot, 0)
afill (ytotal, 0)
select ravm
go top
do while .not. eof ()

   if .not. (fclass $ y1)
      nclass = if (nclass >= 50, 50, nclass + 1)     && 01.22.09: increase to 50
      yclass [nclass] = fclass
      y1 = y1 + fclass + ";"
   endif

   select mgmt_1
   seek ravm->floc + ravm->fclass
   if eof ()
      append blank
      replace floc with ravm->floc, fclass with ravm->fclass
   endif
   if ravm->fstatus = "O"
      replace frented with frented + 1
   else
      replace favail with favail + 1
   endif
   replace ftotal with ftotal + 1
   select ravm
   skip

enddo
select ravm 
use
asort (yclass)

select mgmt_1
index on fclass to (gstnpath+"mgmt_1a.ntx")
total on fclass fields frented, favail, ftotal to (gstnpath+"mgmt_1t.dbf")
select 0
use (gstnpath+"mgmt_1t.dbf") excl alias mgmt_1t
 
select mgmt_1
set index to (gstnpath+"mgmt_1.ntx")
go top
nloc = 0
y2 = ""
do while .not. eof ()
   if .not. (floc $ y2)
      nloc = if (nloc > 10, 10, nloc + 1)
      yloc [nloc] = floc
      y2 = y2 + floc + "/"
   endif
   skip
enddo
asort (yloc)

l_ftitle = "Vehicle Utilization Report"
l_header = ""
for i = 1 to nloc
   l_header = l_header + yloc[i] + space(5)     && 01.22.09
next i
l_header = space(20) + trim(l_header) + space(6) + [TOTAL]
yc1 = max (20, int (l_formlen/2) - int (len (gtitle) / 2))
yc2 = max (20, int (l_formlen/2) - int (len (l_ftitle) / 2))
yc3 = l_formlen - 12

f_popup ("Creating Report ...")

if ydisp $ "DF"
   if empty(yfilename)
      yfil=gstnpath+"rrout.rpt"
   else
      yfil=yfilename
   endif
   set device to print
   set printer to &yfil
else
   set device to print
endif

yrow = mgmt_hdr (ydisp, .f.)
select mgmt_1
go top
for i = 1 to nclass
   yrow = if (int (i/9)*9 = i, mgmt_hdr (ydisp, .t.), yrow)
   @ yrow, 01 say "CLASS: "+yclass[i]
   yrow = yrow + 2

   select mgmt_1
   @ yrow, 03 say "UNIT RENTED"
   ystr = ""
   for j = 1 to nloc
      seek yloc[j] + yclass[i]
      ystr = if (eof (), ystr + str(0,7) + str (0,6) + [%],  ;
                 ystr + str(frented,7) + str (frented/ftotal*100,6) + [%])
      yrtot[j] = if (eof (), yrtot[j], yrtot[j] + frented)
   next j
   select mgmt_1t
   locate for fclass = yclass [i]
   ystr = if (eof (), ystr + str(0,7) + str(0,6) + [%],  ;
          ystr + str(frented,7) + str (frented/ftotal*100,6) + [%])
   @ yrow, 20 say ystr

   yrow = yrow + 1
   select mgmt_1
   @ yrow, 03 say "UNIT AVAILABLE"
   ystr = ""
   for j = 1 to nloc
      seek yloc[j] + yclass[i]
      ystr = if (eof (), ystr + str(0,7) + str (0,6) + [%],  ;
                 ystr + str(favail,7) + str (favail/ftotal*100,6) + [%])
      yatot[j] = if (eof (), yatot[j], yatot[j] + favail)
   next j
   select mgmt_1t
   locate for fclass = yclass [i]
   ystr = if(eof (), ystr + str(0,7) + str(0,6) + [%],  ;
          ystr + str(favail,7) + str (favail/ftotal*100,6) + [%])
   @ yrow, 20 say ystr

   yrow = yrow + 1
   select mgmt_1
   @ yrow, 03 say "TOTAL UNITS"
   ystr = ""
   for j = 1 to nloc
      seek yloc[j] + yclass[i]
      ystr = if (eof (), ystr + str(0,7) + space(7),  ;
             ystr + str(ftotal,7) + space (7))
      ytotal[j] = if (eof (), ytotal[j], ytotal[j] + ftotal)
   next j
   select mgmt_1t
   locate for fclass = yclass [i]
   ystr = if (eof (), ystr + str(0,7) + space (7),   ;
          ystr + str(ftotal,7) + space (7))
   @ yrow, 20 say ystr

   yrow = yrow + 2

next i

@ yrow, 01 say "REPORT SUMMARY"
yrow = yrow + 1
ystr = ""
yt1 = 0
yt2 = 0
@ yrow, 03 say "UNIT RENTED"
for i = 1 to nloc
   ystr = ystr + str(yrtot[i],7) + str(yrtot[i]/ytotal[i]*100,6) + [%]
   yt1 = yt1 + yrtot [i]
   yt2 = yt2 + ytotal [i]
next i
ystr = ystr + str(yt1,7) + str(yt1/yt2*100,6) + [%]
@ yrow, 20 say ystr
yrow = yrow + 1
ystr = ""
yt1 = 0
@ yrow, 03 say "UNIT AVAILABLE"
for i = 1 to nloc
   ystr = ystr + str(yatot[i],7) + str(yatot[i]/ytotal[i]*100,6) + [%]
   yt1 = yt1 + yatot [i]
next i
ystr = ystr + str(yt1,7) + str(yt1/yt2*100,6) + [%]
@ yrow, 20 say ystr
yrow = yrow + 1
ystr = ""
yt1 = 0
@ yrow, 03 say "TOTAL UNITS"
for i = 1 to nloc
   ystr = ystr + str(ytotal[i],7) + space (7)
next i
ystr = ystr + str(yt2,7) 
@ yrow, 20 say ystr

mgmt_end (yrow)

select mgmt_1
use
select mgmt_1t
use

*************************************
* Vehicle Tally Report
* --
* 07.21.11: group by car class
procedure mgmt_2
private ydate, ymo, yyr, ystart, yend, ydays, ystatus, yln, ylcnt, ydisp
private ylogic, y1, y2, y3, ytotal, ycars

f_box (02, 05, 04, 78, "You have selected")
@ 03, 07 say "Monthly Vehicle Tally Report"
ydate = substr(dtoc(date()),1,2)+[/] + substr(dtoc(date()),7,2)
ydisp = "D"
@ 07, 03 say "Month....." get ydate pict "99/99" 
if f_rd () = 27
  return
endif

ymo = substr (ydate,1,2)
yyr = substr (ydate,4,2)
ydisp = f_confirm ("[D]isplay on Screen  [P]rint [Q]uit", "DPQ")
if ydisp ="Q"
   return
endif

f_popup ("Please Wait...")

ystart = ctod(ymo+"/01/"+yyr) 
f_y2k (@ystart)

* 08/10/95: chg for current month calc.
if ymo = substr(dtoc(date()),1,2) .and. yyr = substr(dtoc(date()),7,2)    
   ydays = val(substr(dtoc(date()),4,2))
   yend = date()
else
   for i = 31 to 28 step -1
      if .not. empty(ctod(ymo+"/"+str(i,2)+"/"+yyr))
         exit
      endif
   next
   ydays = i
   yend = ctod(ymo+"/"+str(i,2)+"/"+yyr)
   f_y2k (@yend)
endif

if .not. file (gstnpath + "mgmt_2.dbf")
   create (gstnpath + "stru")
   use (gstnpath + "stru") exclusive
   append blank
   replace field_name with "FUNIT"
   replace field_type with "C"
   replace field_len with 10
   replace field_dec with 0
   append blank                      && 07.21.11: add car class
   replace field_name with "FCLASS"
   replace field_type with "C"
   replace field_len with 5
   replace field_dec with 0
   append blank
   replace field_name with "FSTATUS"
   replace field_type with "C"
   replace field_len with 31
   replace field_dec with 0
   use
   create (gstnpath + "mgmt_2") from (gstnpath + "stru")
   erase (gstnpath + "stru.dbf")
endif
use (gstnpath + "mgmt_2") exclusive alias mgmt_2
index on funit to (gstnpath + "mgmt_2")
zap

f_use ("ravm")
go top
do while .not. eof ()
   if .not.empty(ravm->fclass) .and. .not.empty(ravm->funit)
      select mgmt_2
      seek ravm->funit
      if eof () .and. ravm->fpurdt <= yend       && ignore new acq. vehicles
         append blank
         replace funit with ravm->funit, fclass with ravm->fclass, fstatus with replicate("0",31)
      endif
   endif
   select ravm
   skip
enddo
select ravm 
use

f_use ("raagrh",4)
select raagrh
set softseek on
seek (ystart-31)       && assume max. open ra = 31 days
set softseek off
do while fdatein <= yend .and. .not. eof ()
   y1 = 0
   y2 = 0
   if raagrh->fdatein <= yend 
      if raagrh->fdateout < ystart
         y1 = 1
         y2 = (raagrh->fdatein - ystart) + 1
      else
         y1 = day (raagrh->fdateout)
         y2 = (raagrh->fdatein - raagrh->fdateout) + 1
      endif
   elseif raagrh->fdateout <= yend
      y1 = day (raagrh->fdateout)
      y2 = (yend - raagrh->fdateout) + 1
   elseif raagrh->fdateout < ystart .and. raagrh->fdatein > yend
      y1 = 1
      y2 = ydays
   endif
   if y1 > 0
      select mgmt_2
      seek raagrh->funit
      if eof ()
         append blank
         replace funit with raagrh->funit, fclass with raagrh->fclass, fstatus with replicate("0",31)    && 07.21.11
      endif
      replace fstatus with stuff (fstatus, y1, y2, replicate ("1",y2))
   endif
   select raagrh
   skip
enddo
select raagrh 
use

f_use ("raagr")
set filter to frano <> 0
go top
do while .not. eof ()
   y1 = 0
   y2 = 0
   if raagr->fdatein <= yend 
      if raagr->fdateout < ystart
         y1 = 1
         y2 = (raagr->fdatein - ystart) + 1
      else
         y1 = day (raagr->fdateout)
         y2 = (raagr->fdatein - raagr->fdateout) + 1
      endif
   elseif raagr->fdateout <= yend
      y1 = day (raagr->fdateout)
      y2 = (yend - raagr->fdateout) + 1
   elseif raagr->fdateout < ystart .and. raagr->fdatein > yend
      y1 = 1
      y2 = ydays
   endif
   if y1 > 0
      select mgmt_2
      seek raagr->funit
      if eof ()
         append blank
         replace funit with raagr->funit, fstatus with replicate("0",31)
      endif
      replace fstatus with stuff (fstatus, y1, y2, replicate ("1",y2))
   endif
   select raagr
   skip
enddo
select raagr 
use

select mgmt_2
go top
if eof ()
   f_valid (.f., "Empty Selection...")
   use
   return
endif 

l_ftitle = "Monthly Vehicle Tally Report"+[   For: ]+ ymo + [/] + yyr
l_header = ""
for i = 1 to ydays
   l_header = l_header + str(i,4)
next
l_header = l_header+" TOTAL"
yc1 = max (20, int (l_formlen/2) - int (len (gtitle) / 2))
yc2 = max (20, int (l_formlen/2) - int (len (l_ftitle) / 2))
yc3 = l_formlen - 12

f_popup ("Creating Report ...")
if ydisp = "D"
   yfil=gstnpath+"rrout.rpt"
   set device to print
   set printer to &yfil
else
   set device to print
endif

yrow = mgmt_hdr (ydisp, .f.)

* --07.21.11: output by car class
private ystot [ydays]
afill (ystot, 0)
yscars = 0
ystotal = 0
* --
private ytot [ydays]
afill (ytot, 0)
ytotal = 0
select mgmt_2
index on fclass+funit to (gstnpath + "mgmt_2")    && group by car class
go top
ycars = 0
yclas = fclass
do while .t.    &&    .not. eof ()
   yrow = if (int (yrow/55)*55 = yrow, mgmt_hdr (ydisp, .t.), yrow)
   if yclas <> fclass .or. eof()        && output subtotal
      yrow = yrow + 2
	  @ yrow, 01 say "Class: "+yclas
	  yrow = yrow + 1
	  @ yrow, 01 say "Units: "+str(yscars,4)
	  yrow = yrow + 1
      ystr = ""
      for i = 1 to ydays 
         ystr = ystr + str(ystot[i],4) 
      next
      ystr = ystr + [ ] + str(ystotal,4)
      @ yrow, 01 say ystr
      yrow = yrow + 1
      ystr = " "
      for i = 1 to ydays 
         ystr = ystr + str(ystot[i]/yscars*100,3)+[%]
      next   
      ystr = ystr + [ ] + str(ystotal/(yscars*ydays)*100,3)+[%]
      @ yrow, 01 say ystr
      * -- zero out subtotal
      if eof ()
	     exit
      else
         yclas = fclass
	     yscars = 0
	     ystotal = 0
         afill (ystot, 0)
	  endif
   endif
   for i = 1 to ydays
      if substr(fstatus,i,1) = "1"
         ystot [i] = ystot [i] + 1     && car group subtotal
         ystotal = ystotal + 1
         ytot [i] = ytot [i] + 1       && grand total
         ytotal = ytotal + 1
      endif
   next 
   yscars = yscars + 1
   ycars = ycars + 1

   * ystr = funit+[  ]+ystr+[  ]+str(y1,3)+[  ]+str(y1/ydays*100,5,1)+[%]
   * @ yrow, 01 say ystr
   * yrow = yrow + 1
   skip
enddo
* output summary
l_header = ""
yrow = mgmt_hdr (ydisp, .t.)
yrow = yrow + 2
ystr = space(40) + "R E P O R T    S U M M A R Y"
@ yrow, 01 say ystr
yrow = yrow + 2
ystr = ""
for i = 1 to ydays
   ystr = ystr + str(i,4)
next
ystr = ystr+" TOTAL"
@ yrow, 01 say ystr
yrow = yrow + 2
ystr = ""
for i = 1 to ydays 
   ystr = ystr + str(ytot[i],4) 
next
ystr = ystr + [ ] + str(ytotal,4)
@ yrow, 01 say ystr
yrow = yrow + 1
ystr = " "
for i = 1 to ydays 
   ystr = ystr + str(ytot[i]/ycars*100,3)+[%]
next
ystr = ystr + [ ] + str(ytotal/(ycars*ydays)*100,3)+[%]
@ yrow, 01 say ystr

@ yrow+2, 01 say "TOTAL UNIT............... "+str(ycars,6)
@ yrow+4, 01 say "ACCUMULATED RENTAL DAYS.. "+str(ytotal,6)
@ yrow+6, 01 say "TOTAL RENTAL DAYS........ "+str(ycars*ydays,6)
@ yrow+8, 01 say "TOTAL UTILIZATION........ "+str(ytotal/(ycars*ydays)*100,6,2)+[%]
mgmt_end (yrow)

select mgmt_2
use

**********************************
* 06.29.11
* -- add file as a option
* -- add Detail/Summary
* -- stop use ravm->fclass, instead use raagrh->fclass
* 07.26.11: take out Detail option, group by vehicle class
procedure mgmt_3
private ystart, yend, yclass, yfnd, ystatus, yln, ylcnt, ydisp, ylogic

f_box (02, 05, 04, 78, "You have selected")
@ 03, 07 say "Vehicle Revenue Report"

* -- 06.22.11
l_filename = space(50)
yemail = space(40)
yrptname = "Vehicle Utilization Report"
ydisp = mgmt_option()
if ydisp ="Q"
   return
elseif ydisp = "F"
   if .not. mgmt_getfil (@l_filename)     && pass by reference
      return
   endif  
elseif ydisp = "E"
   if .not. mgmt_getem (@yemail, @l_filename)     && pass by reference
      return
   endif	  
endif
* --
yclass = space (4)
ystart = date()
yend = date()
ydetail = .t.

* @ 07, 3 say "Class....." get yclass pict "!!!!"
@ 08, 3 say "From......" get ystart pict "@D" valid f_y2k (@ystart)
@ 09, 3 say "To........" get yend pict "@D" valid ;
  f_valid (f_y2k (@yend) .and.  yend >= ystart)
* @ 10, 3 say "Detail...." get ydetail pict "Y"
if f_rd () = 27
  return
endif

f_popup ("Please Wait...")
if .not. file (gstnpath + "mgmt_3.dbf")
   create (gstnpath + "stru")
   use (gstnpath + "stru") exclusive
   append blank
   replace field_name with "FUNIT"
   replace field_type with "N"
   replace field_len with 5
   replace field_dec with 0
   append blank
   replace field_name with "FCLASS"
   replace field_type with "C"
   replace field_len with 4
   replace field_dec with 0
   append blank
   replace field_name with "FMODEL"
   replace field_type with "C"
   replace field_len with 13
   replace field_dec with 0
   append blank
   replace field_name with "FPRICE"
   replace field_type with "N"
   replace field_len with 10
   replace field_dec with 2
   append blank
   replace field_name with "FTNM"
   replace field_type with "N"
   replace field_len with 11
   replace field_dec with 2
   append blank
   replace field_name with "FCDW"
   replace field_type with "N"
   replace field_len with 11
   replace field_dec with 2
   append blank
   replace field_name with "FDAYS"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 0
   use
   create (gstnpath + "mgmt_3") from (gstnpath + "stru")
   erase (gstnpath + "stru.dbf")
endif
use (gstnpath + "mgmt_3") exclusive alias mgmt_3
zap
* index on funit+fclass to (gstnpath + "mgmt_3")
index on fclass to (gstnpath + "mgmt_3")      && group by car class

* 07.26.11: implement new report requirement => class,#units,T&M,CDW,#days,Revenue,Rev/Unit

* -- build # car class from ravm
f_use ("ravm")
go top
do while .not. eof ()
   if .not.empty(ravm->fclass) .and. .not.empty(ravm->funit)
      select mgmt_3
      seek ravm->fclass
      if eof () .and. ravm->fpurdt <= yend       && ignore new acq. vehicles
         append blank
         replace fclass with ravm->fclass, funit with 1
      else
         replace funit with funit + 1  	  
      endif
   endif
   select ravm
   skip
enddo
select ravm 
use

f_use ("raagrh",4)
select raagrh
set softseek on
seek ystart
set softseek off
do while fdatein <= yend .and. .not. eof ()
   if empty(raagrh->fclass)
      skip
	  loop
   endif	  
   select mgmt_3
   seek raagrh->fclass
   if eof()
      select mgmt_3 
      append blank
      replace fclass with raagrh->fclass    
      replace ftnm with raagrh->ftmetot+raagrh->fmlgtot, fcdw with raagrh->fcdwtot
      ydays = raagrh->fdatein-raagrh->fdateout
      replace fdays with if(ydays>0, ydays, 1)
   else
      ydays = raagrh->fdatein-raagrh->fdateout
      rlock ()
      replace ftnm with ftnm+raagrh->ftmetot+raagrh->fmlgtot
      replace fcdw with fcdw+raagrh->fcdwtot
      replace fdays with fdays+if(ydays>0, ydays, 1)
   endif
   select raagrh
   skip     
enddo   
select raagrh
use

select mgmt_3
go top
if eof ()
   f_valid (.f., "Empty Selection...")
   use
   return
else
   store 0 to sumunit, sumtnm, sumcdw, sumdays, sumrev, sumavg
   sum funit, ftnm, fcdw, ftnm+fcdw, fdays, (ftnm+fcdw)/funit to   ;
       sumunit, sumtnm, sumcdw, sumrev, sumdays, sumavg
endif 

l_ftitle = "Vehicle Revenue Report"+[ / ]+yclass+[ From: ]+dtoc(ystart)+  ;
           [ to: ]+dtoc(yend)
l_header = "Class    # Units         T & M     "+  ;
           "   CDW   # Days      Revenue    Rev/Unit"
yc1 = max (20, int (l_formlen/2) - int (len (gtitle) / 2))
yc2 = max (20, int (l_formlen/2) - int (len (l_ftitle) / 2))
yc3 = l_formlen - 12

* -- 06.22.11: Create report here
l_filename = if(empty(l_filename), gstnpath + "rrout.rpt", l_filename)
mgmt_dest (ydisp, l_filename)     && set print dest
* --
yrow = mgmt_hdr (ydisp, .f.)

if ydetail       && output details
* --
select mgmt_3
go top
do while .not. eof ()
   yrow = if (int (yrow/55)*55 = yrow, mgmt_hdr (ydisp, .t.), yrow)
   ystr = fclass+[   ]+str(funit,6)+space(7)+str(ftnm,11,2)+[  ]+  ;
          str(fcdw,10,2)+[  ]+str(fdays,5)+[   ]+str(ftnm+fcdw,11,2)+[    ]+   ;
          str((ftnm+fcdw)/funit,8,2)
   @ yrow, 01 say ystr
   yrow = yrow + 1
   skip
enddo
* --
endif
* output summary
yrow = yrow + 2
ystr = space(7)+str(sumunit,6)+space(7)+str(sumtnm,11,2)+[  ]+str(sumcdw,10,2)+  ;
      [  ]+str(sumdays,5)+[   ]+str(sumrev,11,2)+[    ]+str(sumrev/sumunit,8,2)
@ yrow,1 say ystr

mgmt_end (yrow)

select mgmt_3
use

**********************************
procedure mgmt_4

* RA frequency report
private ystart, yend, yloc, yfnd, ystatus, yln, ylcnt, ydisp
* ydow1 == total checkout for day of week
* ydow2 == total checkin for day of week
private i, i1, ylogic, y1, y2, y3, ydow1 [7], ydow2 [7], ydow [7]

afill (ydow1, 0)
afill (ydow2, 0)

f_box (02, 05, 04, 78, "You have selected")
@ 03, 07 say "RA Frequency Report"
yloc = gloc
ystart = date()
yend = date()
ydisp = "D"
yshow = "D"
@ 07, 3 say "Location............ " get yloc pict "!!!!!!!!!!" valid   ;
        f_valid (f_verify("raloc",1,yloc) .or. empty(yloc))
@ 08, 3 say "From................ " get ystart pict "@D"
@ 09, 3 say "To.................. " get yend pict "@D"
@ 10, 3 say "[S]ummary/[D]etail.. " get yshow pict "!" valid ;
        f_valid (yshow $ "S;D")
if f_rd () = 27
  return
endif

ydisp = f_confirm ("[D]isplay on Screen  [P]rint [Q]uit", "DPQ")
if ydisp ="Q"
   return
endif

f_popup ("Please Wait...")
if .not. file (gstnpath + "mgmt_4.dbf")
   create (gstnpath + "stru")
   use (gstnpath + "stru") exclusive
   append blank
   replace field_name with "FDATE"
   replace field_type with "D"
   replace field_len with 8
   replace field_dec with 0
   append blank
   replace field_name with "FTIME"
   replace field_type with "C"
   replace field_len with 5
   replace field_dec with 0
   append blank
   replace field_name with "FRANO"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 0
   append blank
   replace field_name with "FUNIT"
   replace field_type with "C"
   replace field_len with 10
   replace field_dec with 0
   append blank
   replace field_name with "FSTATUS"
   replace field_type with "C"
   replace field_len with 1
   replace field_dec with 0
   use
   create (gstnpath + "mgmt_4") from (gstnpath + "stru")
   erase (gstnpath + "stru.dbf")
endif
use (gstnpath + "mgmt_4") exclusive alias mgmt_4
zap

f_use ("raagrh",4)
select raagrh
set softseek on
seek (ystart-31)       && assume max. open ra = 31 days
set softseek off
ylogic = if(empty(yloc),".t.","raagrh->floc=yloc .or. raagrh->frloc=yloc")
do while fdatein <= yend .and. .not. eof ()
   if raagrh->fdateout >= ystart .and. raagrh->fdateout <= yend .and. &ylogic
      select mgmt_4
      append blank
      replace fdate with raagrh->fdateout, ftime with raagrh->ftimeout
      replace frano with raagrh->frano, funit with raagrh->funit
      replace fstatus with "O"
   endif
   if raagrh->fdatein >= ystart .and. raagrh->fdatein <= yend .and. &ylogic
      select mgmt_4
      append blank
      replace fdate with raagrh->fdatein, ftime with raagrh->ftimein
      replace frano with raagrh->frano, funit with raagrh->funit
      replace fstatus with "I"
   endif
   select raagrh
   skip
enddo
select raagrh 
use

f_use ("raagr")
set filter to frano <> 0
go top
ylogic = if(empty(yloc),".t.","raagr->floc=yloc")
do while .not. eof ()
   if raagr->fdateout >= ystart .and. raagr->fdateout <= yend .and. &ylogic
      select mgmt_4
      append blank
      replace fdate with raagr->fdateout, ftime with raagr->ftimeout
      replace frano with raagr->frano, funit with raagr->funit
      replace fstatus with "O"
   endif
   select raagr
   skip
enddo
select raagr 
use

select mgmt_4
go top
if eof ()
   f_valid (.f., "Empty Selection...")
   use
   return
else
   index on DTOS(FDATE)+FTIME to (gstnpath + "mgmt_4")
endif 

l_ftitle = "RA Frequency Report"+[   From: ]+dtoc(ystart)+  ;
           [ to: ]+dtoc(yend)
l_header = "DATE      TIME    RA#     UNIT#       ACTION   "
yc1 = max (20, int (l_formlen/2) - int (len (gtitle) / 2))
yc2 = max (20, int (l_formlen/2) - int (len (l_ftitle) / 2))
yc3 = l_formlen - 12

f_popup ("Creating Report ...")
if ydisp = "D"
   yfil=gstnpath+"rrout.rpt"
   set device to print
   set printer to &yfil
else
   set device to print
endif

yrow = mgmt_hdr (ydisp, .f.)

select mgmt_4
go top
ykey = fdate
y1 = 0
y2 = 0
y3 = 0
do while .not. eof ()
   if yshow = "D"
      yrow = if (int (yrow/55)*55 = yrow, mgmt_hdr (ydisp, .t.), yrow)
      ystr = dtoc(fdate)+[  ]+ftime+[   ]+str(frano,6)+[  ]+funit+[  ]+  ;
          if(fstatus="O","CHECK OUT","CHECK IN")
      @ yrow, 01 say ystr
      yrow = yrow + 1
   endif
   i1 = dow(fdate)
   if fstatus = "O"
      y1 = y1 + 1
      ydow1 [i1] = ydow1 [i1] + 1
   elseif fstatus = "I"
      y2 = y2 + 1
      ydow2 [i1] = ydow2 [i1] + 1
   endif
   y3 = y3 + 1
   skip
   if fdate <> ykey .and. yshow = "D"
      ykey = fdate
      @ yrow, 01 say "CHECK OUT = " + str(y1,4) + [   ] +  ;
                     "CHECK IN  = " + str(y2,4) + [   ] +  ;
                     "TOTAL     = " + str(y3,4)
      yrow = yrow + 2
      y1 = 0
      y2 = 0
      y3 = 0
   endif
enddo

ydow [1] = "Sunday:   "
ydow [2] = "Monday:   "
ydow [3] = "Tuesday:  "
ydow [4] = "Wednesday:"
ydow [5] = "Thursday: "
ydow [6] = "Friday:   "
ydow [7] = "Saturday: "

for i = 1 to 7
   yrow = if (int (yrow/55)*55 = yrow, mgmt_hdr (ydisp, .t.), yrow)
   @ yrow, 01 say "For " + ydow [i] + [   ] + ;
                  "CHECK OUT = " + str(ydow1[i],5) + [   ] +  ;
                  "CHECK IN  = " + str(ydow2[i],5) + [   ] +  ;
                  "TOTAL     = " + str(ydow1[i]+ydow2[i],5)
   yrow = yrow + 2
next
 
mgmt_end (yrow)

select mgmt_4
use

*************************
procedure mgmt_5
private yrano1, yrano2, yloc, yfnd, ystatus, yln, ylcnt, ydisp

f_box (02, 05, 04, 78, "You have selected")
@ 03, 07 say "Missing RA Report"
yloc = gloc
yrano1 = 0
yrano2 = 0

@ 07, 3 say "Location.." get yloc pict "!!!!!!!!!!" valid   ;
        f_valid (f_verify("raloc",1,yloc) .or. empty(yloc))
@ 08, 3 say "From RA # " get yrano1 pict "999999"
@ 09, 3 say "To RA #   " get yrano2 pict "999999"
if f_rd () = 27
   return
endif

ydisp = f_confirm ("[D]isplay on Screen  [P]rint [Q]uit", "DPQ")
if ydisp ="Q"
   return
endif

if empty (yloc)
   f_use ("raloc")
   private ylocary [reccount ()]
   go top
   nloc = 0
   do while .not. eof ()
      if floc $ gusrloc
         nloc = nloc + 1
         ylocary [nloc] = floc
      endif
      skip
   enddo
   use
endif

f_use ("RAAGR")
f_use ("RAAGRH")
yln = 0
ylcnt = 0
f_popup ("Creating Report ...")
if ydisp = "D"
   yfil=gstnpath+"rrout.rpt"
   set device to print
   set printer to &yfil
else
   set device to print
endif
for n = yrano1 to yrano2
   yfnd = .f.
   ystatus = "MISSING"
   select raagr
   if empty (yloc)
      for n1 = 1 to nloc
         seek ylocary [n1] + str (n, 6)
         if found ()
            ystatus = "OPEN RA"
            yfnd = .t.
            exit
         endif
      next
   else
      seek yloc + str (n, 6)
      if found ()
         * ystatus = "CLOSED "  
         ystatus = "OPEN RA"        && 03/04/94:EDC 
         yfnd = .t.
      endif
   endif
   if .not. yfnd
      select raagrh
      if empty (yloc)
         for n1 = 1 to nloc
            seek ylocary [n1] + str (n, 6)
            if found ()
               yfnd = .t.
               ystatus = "CLOSED "
               exit
            endif
         next
      else
         seek yloc + str (n, 6)
         if found ()
            yfnd = .t.
            ystatus = "CLOSED "
         endif
      endif
   endif
   *if .not. yfnd
      if yln > 56
         if ydisp = "D"
            @ yln + 1, 0 say replicate (chr (196), 78)
            @ yln + 2, 0 say ""
            setprc (0, 0)
         else
            eject
         endif
         yln = 0
      endif
      if yln = 0
         @ 1, 0 say "MISSING RA REPORT AS OF " + DTOC (DATE ())
         @ 2, 0 say "FOR LOCATION " + yloc + "   From " + str (yrano1, 6) + ;
            "   To " + str (yrano2, 6)
         yln = 4
      endif
      @ yln, ylcnt * 16 say f_truncate (ltrim (str (n, 6)), 7) + [-] +  ;
                            ystatus + [ ]
      if ylcnt = 4
         yln = yln + 1
         ylcnt = 0
      else
         ylcnt = ylcnt + 1
      endif
   *endif
next
if yln > 0 .and. ydisp = "P"
   eject
endif
select raagrh
use
select raagr
use
set device to screen
set printer to
set console on
set print off
if ydisp = "D"
   yfil=gstnpath+"rrout.rpt"
   setcolor ("W/N")
   clear
   run racbrow &yfil
   setcolor (gbluecolor)
endif

**********************************
procedure mgmt_6
private ystart, yend, ytnm, yfnd, ystatus, yln, ylcnt, ydisp, ylogic

f_box (02, 05, 04, 78, "You have selected")
@ 03, 07 say "Travel Agent Commission Report"
ystart = date()
yend = date()

@ 07, 3 say "From......" get ystart pict "@D" valid f_y2k (@ystart)
@ 08, 3 say "To........" get yend pict "@D" valid f_y2k (@yend)
if f_rd () = 27
  return
endif

ydisp = f_confirm ("[D]isplay on Screen  [P]rint [Q]uit", "DPQ")
if ydisp ="Q"
   return
endif

f_popup ("Please Wait...")
if .not. file (gstnpath + "mgmt_6.dbf")
   create (gstnpath + "stru")
   use (gstnpath + "stru") exclusive
   append blank
   replace field_name with "FATC"
   replace field_type with "C"
   replace field_len with 10
   replace field_dec with 0
   append blank
   replace field_name with "FCOMPANY"
   replace field_type with "C"
   replace field_len with 30
   replace field_dec with 0
   append blank
   replace field_name with "FLOC"
   replace field_type with "C"
   replace field_len with 10
   replace field_dec with 0
   append blank
   replace field_name with "FRANO"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 0
   append blank
   replace field_name with "FRESVNO"
   replace field_type with "C"
   replace field_len with 10
   replace field_dec with 0
   append blank
   replace field_name with "FLNAME"
   replace field_type with "C"
   replace field_len with 15
   replace field_dec with 0
   append blank
   replace field_name with "FDATEIN"
   replace field_type with "D"
   replace field_len with 8
   replace field_dec with 0
   append blank
   replace field_name with "FRATE"
   replace field_type with "C"
   replace field_len with 5
   replace field_dec with 0
   append blank
   replace field_name with "FDAYS"
   replace field_type with "N"
   replace field_len with 4
   replace field_dec with 0
   append blank
   replace field_name with "FTNM"
   replace field_type with "N"
   replace field_len with 8
   replace field_dec with 2
   append blank
   replace field_name with "FCOMMPCT"
   replace field_type with "N"
   replace field_len with 2
   replace field_dec with 0
   append blank
   replace field_name with "FCOMM"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 2
   use
   create (gstnpath + "mgmt_6") from (gstnpath + "stru")
   erase (gstnpath + "stru.dbf")
endif
use (gstnpath + "mgmt_6") exclusive alias mgmt_6
zap
index on fcompany+fatc+fresvno to (gstnpath + "mgmt_6")

f_use ("raagnt")
f_use ("rares")
set relation to fatc into raagnt
f_use ("raagrh",4)
set relation to fresvno into rares
select raagrh
set softseek on
seek ystart
set softseek off
do while fdatein <= yend .and. .not. eof ()
   if empty (raagrh->fresvno) .or. empty(rares->fatc)
      skip
      loop
   endif
   select mgmt_6
   ytnm = raagrh->ftmetot+raagrh->fmlgtot-raagrh->fdisctot
   append blank
   replace fatc with rares->fatc, fcompany with raagnt->fcompany
   replace floc with raagrh->floc
   replace frano with raagrh->frano, fresvno with raagrh->fresvno
   replace flname with raagrh->flname+[ ]+substr(raagrh->ffname,1,1)
   replace fdatein with raagrh->fdatein
   replace frate with raagrh->frate, fdays with raagrh->fdays
   replace ftnm with ytnm, fcommpct with rares->fcommpct
   replace fcomm with ytnm * rares->fcommpct / 100
   select raagrh
   skip
enddo
select rares
use
select raagrh
use

select mgmt_6
go top
if eof ()
   f_valid (.f., "Empty Selection...")
   use
   return
endif 

l_ftitle = "Travel Agent Commission Report"+[  From: ]+dtoc(ystart)+  ;
           [ to: ]+dtoc(yend)
l_header = "RENTAL AGREEMENT   RES #      RENTER               DATE  "+  ;
           "RATE  DAYS   T & M    COMMISSION"
yc1 = max (20, int (l_formlen/2) - int (len (gtitle) / 2))
yc2 = max (20, int (l_formlen/2) - int (len (l_ftitle) / 2))
yc3 = l_formlen - 12

f_popup ("Creating Report ...")
if ydisp = "D"
   yfil=gstnpath+"rrout.rpt"
   set device to print
   set printer to &yfil
else
   set device to print
endif

yrow = mgmt_hdr (ydisp, .f.)

select mgmt_6
go top
ykey = "~"
y1 = 0
y2 = 0
do while .not. eof ()
   yrow = if (yrow > 55, mgmt_hdr (ydisp, .t.), yrow)
   if fatc <> ykey
      yrow = if (yrow+7 > 55, mgmt_hdr (ydisp, .t.), yrow)
      ykey = fatc
      select raagnt
      seek ykey
      @ yrow, 01 say "AGENT: "+ ykey
      yrow = yrow + 1
      @ yrow, 08 say raagnt->fcompany+space(5)+raagnt->fphone
      yrow = yrow + 1
      @ yrow, 08 say raagnt->faddr
      yrow = yrow + 1
      if .not. empty (raagnt->faddr1)
         @ yrow, 08 say raagnt->faddr1
         yrow = yrow + 1
      endif
      @ yrow, 08 say raagnt->fcity+space(2)+raagnt->fstate+space(2)+raagnt->fzip
      yrow = yrow + 2
      select mgmt_6
   endif
   ystr = floc+[ ]+str(frano,6)+[ ]+fresvno+[ ]+flname+[ ]+  ;
          dtoc(fdatein)+[ ]+frate+[ ]+str(fdays,4)+[  ]+   ;
          str(ftnm,8,2)+[  ]+str(fcommpct,2)+[%  ]+str(fcomm,6,2)
   @ yrow, 01 say ystr
   yrow = yrow + 1
   y1 = y1 + 1
   y2 = y2 + fcomm
   skip
   if fatc <> ykey
      @ yrow, 01 say "# of RA: " + str(y1,4) + space(5) +  ;
                     "Commission = " + str(y2,8,2)
      yrow = yrow + 3
      y1 = 0
      y2 = 0
   endif
enddo

* output summary

mgmt_end (yrow)

select mgmt_6
use

***************************
* Add'l Revenue summary report
procedure mgmt_7
private yret, yfile, i, yfld, ytitle, ycond, yopt, yfilter
private yloc, ystart, yend, ycode, ytnm, yfnd, ystatus, yln, ylcnt, ydisp

f_box (02, 05, 04, 78, "You have selected")
@ 03, 07 say "Add'l Revenue Summary Report"
yloc = gloc
set century on
ystart = date()
yend = date()
@ 06, 03 say "Location....... " get yloc pict "!!!!!!!!!!"
@ 07, 03 say "From Date...... " get ystart pict "@D" 
@ 08, 03 say "To Date........ " get yend pict "@D" valid ;
  f_valid (yend >= ystart)       

f_rd ()
set century off
if lastkey () = 27
  return
endif

ydisp = f_confirm ("[D]isplay on Screen  [P]rint [Q]uit", "DPQ")
if ydisp ="Q"
   return
endif

f_popup ("Please Wait...")
* load paycode array
f_use ("rapaycd")
ip = reccount()
private ypcode [ip], ypdesc [ip], yptot [ip], yncnt [ip]
i = 1 
go top
do while .not. eof ()
   ypcode [i] = fpaycd
   ypdesc [i] = fitem
   yptot [i] = 0
   yncnt [i] = 0
   i = i + 1
   skip
enddo
ip = i - 1
select rapaycd
use
* accumulate total
private ydays, ynra, ycdw, ypai, ysli, yupg, ywu, yra1, yra2, yytot
private yncdw, ynpai, ynsli, ynupg, ynwu
store 0 to ydays, ynra, ycdw, ypai, ysli, yupg, ywu
store 0 to yncdw, ynpai, ynsli, ynupg, ynwu
f_use ("raagrh",4)
set softseek on
seek ystart
set softseek off
yra1 = 999999
yra2 = 0
do while fdatein <= yend .and. .not. eof ()
   * 
   if raagrh->floc <> yloc .or. raagrh->ftotal <= 0    && skip 0 revenue ra
      skip
      loop
   endif
   * find starting and ending ra
   if frano < yra1
      yra1 = frano
   endif
   if frano > yra2
      yra2 = frano
   endif
   * accumulate # days and ra
   ynra = ynra + 1
   ydays = ydays + if (frhr >= 3, fdays + 1, fdays + frhr / 3)
   * accumulate cdw, pai, sli, upg
   ycdw = ycdw + fcdwtot
   ypai = ypai + fpaitot
   * accumulate %
   yncdw = if(fcdwtot > 0, yncdw + 1, yncdw)
   ynpai = if(fpaitot > 0, ynpai + 1, ynpai)
   * check add'l charge
   for i = 1 to 4
      yt1 = "raagrh->foitem"+str(i,1)
      yt2 = "raagrh->fotot"+str(i,1)
      yitem = &yt1
      if .not. empty(yitem)
         for j = 1 to ip
            if yitem = ypcode [j]
               yptot [j] = yptot [j] + &yt2
               yncnt [j] = if(&yt2 > 0, yncnt [j] + 1, yncnt [j])
               exit
            endif
         next j 
      endif
   next i
   skip
enddo
select raagrh
use

l_ftitle = "Location Yield Report"+[  From: ]+dtoc(ystart)+  ;
           [ to: ]+dtoc(yend)
l_header = " "
yc1 = max (20, int (l_formlen/2) - int (len (gtitle) / 2))
yc2 = max (20, int (l_formlen/2) - int (len (l_ftitle) / 2))
yc3 = l_formlen - 12

f_popup ("Creating Report ...")
if ydisp = "D"
   yfil=gstnpath+"rrout.rpt"
   set device to print
   set printer to &yfil
else
   set device to print
endif

yrow = mgmt_hdr (ydisp, .f.)

* output line items
y1 = if(ynra > 0, ynra, 1)
@ yrow, 03 say "# RA............" + str(ynra,9)
yrow = yrow + 1
@ yrow, 03 say "# Days.........." + str(ydays,9,2)
yrow = yrow + 2
@ yrow, 03 say "CDW Total......." + str(ycdw,9,2) + space(5) + str(yncdw/y1*100,6,2)+[ %]
yrow = yrow + 1
@ yrow, 03 say "PAI Total......." + str(ypai,9,2) + space(5) + str(ynpai/y1*100,6,2)+[ %]
yrow = yrow + 2
yytot = ycdw + ypai + ysli + yupg
for i = 1 to ip
   @ yrow, 03 say ypdesc [i] + ": " + str(yptot [i],8,2) + space(5) + str(yncnt[i]/y1*100,6,2)+[ %]
   yytot = yytot + yptot [i]
   yrow = yrow + 1
next i
yrow = yrow + 2
@ yrow, 03 say "Location Total.." + str(yytot, 9,2)
yrow = yrow + 1
@ yrow, 03 say "Location Yield.." + str(yytot / ydays,9,2)

mgmt_end (yrow)
close all        && leave this in... (edc)

*************************************
* changes:
* 04.05.00: add on rent vehicle
*
procedure mgmt_8
private ydate, ymo, yyr, ystart, yend, ydays, ystatus, yln, ylcnt, ydisp
private yloc

f_box (02, 05, 04, 78, "You have selected")
@ 03, 07 say "Rental Days Projection Report"
set century on
ydisp = "D"
yloc = gloc
ystart = date()
yend = date()
@ 07, 03 say "Location..... " get yloc pict "!!!!!!!!!!" valid ;
     f_valid (yloc $ gusrloc .or. yloc='ALL')
@ 08, 03 say "From Date.... " get ystart
@ 09, 03 say "To Date...... " get yend
f_rd ()
set century off
if lastkey () = 27     && century get reset upon return
  return
endif

ydisp = f_confirm ("[D]isplay on Screen  [P]rint [Q]uit", "DPQ")
if ydisp ="Q"
   return
endif

f_popup ("Please Wait...")

f_y2k (@ystart)
f_y2k (@yend)

* if .not. file (gstnpath + "mgmt_8.dbf")
   create (gstnpath + "stru")
   use (gstnpath + "stru") exclusive
   append blank
   replace field_name with "FDATE"
   replace field_type with "D"
   replace field_len with 8
   replace field_dec with 0
   append blank
   replace field_name with "FCLASS"
   replace field_type with "C"
   replace field_len with 4
   replace field_dec with 0
   append blank
   replace field_name with "FRES"
   replace field_type with "N"
   replace field_len with 4
   replace field_dec with 0
   append blank
   replace field_name with "FRENTED"
   replace field_type with "N"
   replace field_len with 4
   replace field_dec with 0
   use
   create (gstnpath + "mgmt_8") from (gstnpath + "stru")
   erase (gstnpath + "stru.dbf")
* endif
use (gstnpath + "mgmt_8") exclusive alias mgmt_8
zap
index on dtos(fdate)+fclass to (gstnpath + "mgmt_8")

*
ystart = ystart - 30     && go back 30 days 
yend = yend + 1          && 06.29.00: fix last day
declare yvclass [20]
afill (yvclass, "")
yvnum = 0
f_use ("rares", 2)
seek "O"
do while fresvstat = "O" .and. .not. eof ()
   if rares->floc <> yloc .and. yloc <> "ALL"
      skip
      loop
   elseif empty(rares->fclass)
      skip
      loop
   endif
   if rares->fdateout >= ystart .and. rares->fdateout <= yend
      select mgmt_8
      locate for fclass = rares->fclass
      if eof ()
         yvnum = yvnum + 1
         yvnum = if(yvnum>20, 20, yvnum)
         yvclass [yvnum] = rares->fclass
         for i = 1 to yend - ystart + 1
           select mgmt_8
           append blank
           replace fdate with ystart + i - 1, fclass with rares->fclass
           commit
           unlock
         next
      endif
      y1 = if(rares->fdatein > yend, yend-rares->fdateout+1, rares->fdatein-rares->fdateout)    && optional: +1
      y1 = if(y1>0, y1, 1)
      for i = 1 to y1
         select mgmt_8
         y2 = rares->fdateout + i - 1
         seek dtos(y2)+rares->fclass
         if .not. eof ()
            replace fres with fres + 1
         endif
      next
   elseif rares->fdatein >= ystart .and. rares->fdatein <= yend
      select mgmt_8
      locate for fclass = rares->fclass
      if eof ()
         yvnum = yvnum + 1
         yvnum = if(yvnum>20, 20, yvnum)
         yvclass [yvnum] = rares->fclass
         for i = 1 to yend - ystart + 1
           select mgmt_8
           append blank
           replace fdate with ystart + i - 1, fclass with rares->fclass
           commit
           unlock
         next
      endif
      y1 = rares->fdatein - ystart            && optional: +1
      y1 = if(y1>0, y1, 1)
      for i = 1 to y1    
         select mgmt_8
         y2 = ystart + i - 1
         seek dtos(y2)+rares->fclass
         if .not. eof ()
            replace fres with fres + 1
         endif  
      next
   endif
   select rares
   skip
enddo
select rares
use

* process open ra's
f_use ("ravm")       && get the actual vehicle class for open ra's
f_use ("raagr")
go top
do while .not. eof ()
   if (raagr->floc <> yloc .and. yloc <> "ALL") .or. (raagr->frano=0)
      skip
      loop
   elseif empty(raagr->funit)
      skip
      loop
   endif
   if raagr->fdateout >= ystart .and. raagr->fdateout <= yend
      select ravm
      seek raagr->funit
      if eof ()
         select raagr
         skip
         loop
      endif
      select mgmt_8
      locate for fclass = ravm->fclass
      if eof ()
         yvnum = yvnum + 1
         yvnum = if(yvnum>20, 20, yvnum)
         yvclass [yvnum] = ravm->fclass
         for i = 1 to yend - ystart + 1
           select mgmt_8
           append blank
           replace fdate with ystart + i - 1, fclass with ravm->fclass
           commit
           unlock
         next
      endif
      y1 = if(raagr->fdatein > yend, yend-raagr->fdateout, raagr->fdatein-raagr->fdateout)    && optional: +1
      y1 = if(y1>0, y1, 1)
      for i = 1 to y1
         select mgmt_8
         y2 = raagr->fdateout + i - 1
         seek dtos(y2)+ravm->fclass
         if .not. eof ()
            replace frented with frented + 1
         endif  
      next
   elseif raagr->fdatein >= ystart .and. raagr->fdatein <= yend
      select ravm
      seek raagr->funit
      if eof ()
         select raagr
         skip
         loop
      endif
      select mgmt_8
      locate for fclass = ravm->fclass
      if eof ()
         yvnum = yvnum + 1
         yvnum = if(yvnum>20, 20, yvnum)
         yvclass [yvnum] = ravm->fclass
         for i = 1 to yend - ystart + 1
           select mgmt_8
           append blank
           replace fdate with ystart + i - 1, fclass with ravm->fclass
           commit
           unlock
         next
      endif
      y1 = raagr->fdatein - ystart            && optional: +1
      y1 = if(y1>0, y1, 1)
      for i = 1 to y1    
         select mgmt_8
         y2 = ystart + i - 1
         seek dtos(y2)+ravm->fclass
         if .not. eof ()
            replace frented with frented + 1
         endif  
      next
   endif
   select raagr
   skip
enddo
select ravm
use
select raagr
use

select mgmt_8
go top
if eof ()
   f_valid (.f., "Empty Selection...")
   use
   return
endif 

ystart = ystart + 30        && +30...
yend = yend - 1             && 06.29.00
l_ftitle = "Rental Days Projection Report"+[  ]+dtoc(ystart)+[ thru ]+dtoc(yend)
l_header = ""
asort (yvclass, 1, yvnum)
for i = 1 to yvnum
   l_header = l_header + yvclass[i] + space(3)
next i
l_header = space(12) + l_header + [   TOTAL]
yc1 = max (20, int (l_formlen/2) - int (len (gtitle) / 2))
yc2 = max (20, int (l_formlen/2) - int (len (l_ftitle) / 2))
yc3 = l_formlen - 12

f_popup ("Creating Report ...")
if ydisp = "D"
   yfil=gstnpath+"rrout.rpt"
   set device to print
   set printer to &yfil
else
   set device to print
endif

yrow = mgmt_hdr (ydisp, .f.)

select mgmt_8
y1 = ystart
seek dtos(y1)
do while .not. eof () .and. fdate <= yend
   yrow = if (yrow > 55, mgmt_hdr (ydisp, .t.), yrow)
   ystr1 = dtoc(fdate)+space(2)
   ystr2 = space(10)
   ystr3 = space(10)
   y2 = 0
   y3 = 0
   y4 = 0
   do while y1 = fdate .and. .not. eof ()
      ystr1 = ystr1 + str(fres,4) + space(3)
      y2 = y2 + fres
      ystr2 = ystr2 + str(frented,4) + space(3)
      y3 = y3 + frented
      ystr3 = ystr3 + str(fres+frented,4) + space(3)
      y4 = y4 + fres + frented
      skip
   enddo
   y1 = fdate
   ystr1 = ystr1 + space(3) + str(y2, 5)
   @ yrow, 01 say ystr1
   yrow = yrow + 1
   ystr2 = ystr2 + space(3) + str(y3, 5)
   @ yrow, 01 say ystr2
   yrow = yrow + 1
   ystr3 = ystr3 + space(3) + str(y4, 5)
   @ yrow, 01 say ystr3
   yrow = yrow + 2
enddo

* output summary
declare yvtot [yvnum], yytot [yvnum]
afill (yvtot, 0)
afill (yytot, 0)
select mgmt_8
y1 = ystart 
seek dtos(y1)
do while .not. eof () .and. fdate <= yend
   for i = 1 to yvnum
      if fclass = yvclass [i]
         yvtot [i] = yvtot [i] + fres
         yytot [i] = yytot [i] + frented
         exit
      endif
   next 
   skip
enddo
ygtot = 0
yctot = 0
ytotal = 0
for i = 1 to yvnum
   ygtot = ygtot + yvtot [i]
   yctot = yctot + yytot [i]
   ytotal = ytotal + yvtot [i] + yytot [i]
next
ystr1 = space(10)
ystr2 = space(10)
ystr3 = space(10)
for i = 1 to yvnum
   ystr1 = ystr1 + str(yvtot [i], 4) + space(3)
   ystr2 = ystr2 + str(yytot [i], 4) + space(3)
   ystr3 = ystr3 + str(yvtot [i] + yytot [i], 4) + space(3)
next
ystr1 = ystr1 + space(3) + str(ygtot, 5)
ystr2 = ystr2 + space(3) + str(yctot, 5)
ystr3 = ystr3 + space(3) + str(ytotal, 5)
yrow = yrow + 2
yrow = if (yrow > 55, mgmt_hdr (ydisp, .t.), yrow)
@ yrow, 01 say ystr1
yrow = yrow + 1
@ yrow, 01 say ystr2
yrow = yrow + 1
@ yrow, 01 say ystr3

mgmt_end (yrow)

select mgmt_8
use

********************
* Marketing Tally Report
* Format::=
* Acct #   Acct Name   Jan Feb Mar Apr May .... Dec
*

procedure mgmt_9

private yyr, ytype, ytally, yracnt
private yln, ylcnt, ydisp, yfilename

f_box (02, 05, 04, 78, "You have selected")
@ 03, 07 say "Marketing Tally Report"
ydisp = "D"
yfilename = space(40)
yyr = str(year(date()),4)
ytype = [1]
ytally = [1]
yracnt = 1

set century on
@ 07, 03 say "Year....... "
@ 08, 03 say "Acct Type..       [1] Corporate"
@ 09, 03 say "                  [2] Customer" 
@ 10, 03 say "Tally Type.       [1] # Rental "
@ 11, 03 say "                  [2] RA Total "
@ 12, 03 say "# Rentals.. "
@ 07, 15 get yyr pict "9999"
@ 08, 15 get ytype pict "9" valid f_valid (ytype $ [1;2],"Please enter 1 or 2")
@ 10, 15 get ytally pict "9" valid f_valid (ytally $ [1;2],"Please enter 1 or 2")
@ 12, 15 get yracnt pict "99" valid f_valid (yracnt > 0)
f_rd ()
set century off

if lastkey () = 27
  return
endif

ydisp = f_confirm ("[D]isplay on Screen  [P]rint  [F]ile  [Q]uit", "DPFQ")
if ydisp ="Q"
   return
endif

do while ydisp = "F"
   @ 14, 03 say "File Name.."
   @ 14, 15 get yfilename
   if f_rd() = 27
      return 
   else
      ykeyin = f_confirm ("Is this correct [Y/N/Q] ", "YNQ")
      if ykeyin = "Y"
         exit
      elseif ykeyin = "Q"
         return 
      endif
   endif
enddo

f_popup ("Please Wait...")


   create (gstnpath + "stru")
   use (gstnpath + "stru") exclusive
   append blank
   replace field_name with "FACCT"
   replace field_type with "C"
   replace field_len with 15
   replace field_dec with 0
   append blank
   replace field_name with "FNAME"
   replace field_type with "C"
   replace field_len with 20
   replace field_dec with 0
   append blank
   replace field_name with "FNOR"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 0
   append blank
   replace field_name with "F01"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 0
   append blank
   replace field_name with "F02"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 0
   append blank
   replace field_name with "F03"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 0
   append blank
   replace field_name with "F04"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 0
   append blank
   replace field_name with "F05"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 0
   append blank
   replace field_name with "F06"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 0
   append blank
   replace field_name with "F07"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 0
   append blank
   replace field_name with "F08"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 0
   append blank
   replace field_name with "F09"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 0
   append blank
   replace field_name with "F10"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 0
   append blank
   replace field_name with "F11"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 0
   append blank
   replace field_name with "F12"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 0
   use
   create (gstnpath + "mgmt_9") from (gstnpath + "stru")
   erase (gstnpath + "stru.dbf")

use (gstnpath + "mgmt_9") exclusive alias mgmt_9
index on facct to (gstnpath + "mgmt_9")

ystart = ctod("01/01/"+yyr)
yend = ctod("12/31/"+yyr)
if ytype = [1]
   ycond = "empty(fcrpno)"
   ykey = "raagrh->fcrpno"
   yname = "raagrh->fcrpnm"
else
   ycond = "empty(fcustno)"
   ykey = "raagrh->fcustno"
   yname = "raagrh->flname"
endif

if ytally = [2]
   ysumfld = "raagrh->ftmetot+raagrh->fmlgtot"
else
   ysumfld =""
endif
f_use ("raagrh",4)
set softseek on
seek ystart
set softseek off
do while fdatein <= yend .and. .not. eof ()
   * 
   if &ycond
      skip
      loop
   endif
   yp = "F"+strtran(str(month(raagrh->fdatein),2)," ","0")    && F01, F02... F12
   select mgmt_9
   seek &ykey
   if eof ()
      append blank
      replace facct with &ykey, fname with &yname
   endif
   replace fnor with fnor + 1       && number of rentals
   if empty(ysumfld)
      replace &yp with &yp + 1
   else
      replace &yp with &yp + &ysumfld
   endif
   select raagrh
   skip
enddo
select raagrh
use

l_ftitle = "Marketing Tally Report"+[  YEAR: ]+yyr+  ;
           [  ]+if(ytype=[1],[Corporate Acct],[Customer Acct])
l_header = "ACCT #          NAME "+space(20)+ ;
           "JAN    FEB    MAR    APR    MAY    JUN    "+  ;
           "JUL    AUG    SEP    OCT    NOV    DEC    TOTAL"
yc1 = max (20, int (l_formlen/2) - int (len (gtitle) / 2))
yc2 = max (20, int (l_formlen/2) - int (len (l_ftitle) / 2))
yc3 = l_formlen - 12

f_popup ("Creating Report ...")
if ydisp $ "DF"
   if empty(yfilename)
      yfil=gstnpath+"rrout.rpt"
   else
      yfil=yfilename
   endif
   set device to print
   set printer to &yfil
else
   set device to print
endif

yrow = mgmt_hdr (ydisp, .f.)

select mgmt_9
go top
do while .not. eof ()
   yrow = if (yrow > 55, mgmt_hdr (ydisp, .t.), yrow)
   if fnor < yracnt
      skip
      loop
   endif
   @ yrow, 01 say facct + " " + fname + " "+ ;
                  str(f01,6) + " " + ;
                  str(f02,6) + " " + ;
                  str(f03,6) + " " + ;
                  str(f04,6) + " " + ;
                  str(f05,6) + " " + ;
                  str(f06,6) + " " + ;
                  str(f07,6) + " " + ;
                  str(f08,6) + " " + ;
                  str(f09,6) + " " + ;
                  str(f10,6) + " " + ;
                  str(f11,6) + " " + ;
                  str(f12,6) + "   " + ;
                  str(f01+f02+f03+f04+f05+f06+f07+f08+f09+f10+f11+f12,6)
   yrow = yrow + 1
   skip
enddo

mgmt_end (yrow)

close all        

********************
* Monthly Customer Activity Report
*

procedure mgmt_10

private y1,y2, ytally, yracnt
private yln, ylcnt, ydisp, yfilename
private xyr, xmo

f_box (02, 05, 04, 78, "You have selected")
@ 03, 07 say "Monthly Customer Activity Report"

ydisp = "F"
yfilename = space(40)
ystart = date()
yend = date()                                                     
ytally = [1]
yracnt = 1

set century on
@ 07, 03 say "Start Date:"
@ 08, 03 say "End Date  :"
@ 10, 03 say "Tally Type:       [1] # Rental "
@ 11, 03 say "                  [2] RA Total "
@ 12, 03 say "# Rentals : "
@ 07, 15 get ystart pict "@D"
@ 08, 15 get yend pict "@D"
@ 10, 15 get ytally pict "9" valid f_valid (ytally $ [1;2],"Please enter 1 or 2")
@ 12, 15 get yracnt pict "999" valid f_valid (yracnt > 0)
f_rd ()
set century off
if lastkey () = 27     && century get reset upon return
  return
endif

ydisp = f_confirm ("[F]ile  [Q]uit", "FQ")
if ydisp ="Q"
   return
endif

do while ydisp = "F"
   @ 14, 03 say "File Name.."
   @ 14, 15 get yfilename
   if f_rd() = 27
      return 
   else
      ykeyin = f_confirm ("Is this correct [Y/N/Q] ", "YNQ")
      if ykeyin = "Y"
         exit
      elseif ykeyin = "Q"
         return 
      endif
   endif
enddo
yfilename = alltrim(yfilename)+".txt"

f_popup ("Please Wait...")


   create (gstnpath + "stru")
   use (gstnpath + "stru") exclusive
   append blank
   replace field_name with "FACCT"
   replace field_type with "C"
   replace field_len with 15
   replace field_dec with 0
   append blank
   replace field_name with "FFNAME"
   replace field_type with "C"
   replace field_len with 12
   replace field_dec with 0
   append blank
   replace field_name with "FLNAME"
   replace field_type with "C"
   replace field_len with 14
   replace field_dec with 0

   append blank
   replace field_name with "FEMP"
   replace field_type with "C"
   replace field_len with 3
   replace field_dec with 0
   append blank
   replace field_name with "FCKIN"
   replace field_type with "D"
   replace field_len with 8
   replace field_dec with 0
   append blank
   replace field_name with "FPHONE"
   replace field_type with "C"
   replace field_len with 12
   replace field_dec with 0

   append blank
   replace field_name with "FNOR"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 0
   append blank
   replace field_name with "F01"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 0
   append blank
   replace field_name with "F02"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 0
   append blank
   replace field_name with "F03"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 0
   append blank
   replace field_name with "F04"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 0
   append blank
   replace field_name with "F05"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 0
   append blank
   replace field_name with "F06"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 0
   append blank
   replace field_name with "F07"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 0
   append blank
   replace field_name with "F08"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 0
   append blank
   replace field_name with "F09"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 0
   append blank
   replace field_name with "F10"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 0
   append blank
   replace field_name with "F11"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 0
   append blank
   replace field_name with "F12"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 0
   use
   create (gstnpath + "mgmt_10") from (gstnpath + "stru")
   erase (gstnpath + "stru.dbf")

use (gstnpath + "mgmt_10") exclusive alias mgmt_10
index on facct to (gstnpath + "mgmt_10")

ycond = "empty(fcustno)"
ykey = "raagrh->fcustno"

if ytally = [2]
   ysumfld = "raagrh->ftmetot+raagrh->fmlgtot"
else
   ysumfld =""
endif

*
xyr = year(ystart)
xmo = month(ystart) - 1

f_use ("racust",2)
f_use ("raagrh",4)
set relation to fcustno into racust
set softseek on
seek ystart
set softseek off
do while fdatein <= yend .and. .not. eof ()
   * 
   if &ycond
      skip
      loop
   endif
   * example, start date = 09/01/2007
   * f01 = 09 - 8
   * f02 = 10 - 8
   * f05 = 01 - 8 + 12
   * f12 = 08 - 8 + 12
   ymo = month(raagrh->fdatein) - xmo
   ymo = if(ymo > 0, ymo, ymo+12)
   yp = "F"+strtran(str(ymo,2)," ","0")    && F01, F02... F12
   select mgmt_10
   seek &ykey
   if eof ()
      append blank
      replace facct with f_truncate(raagrh->fcustno,15)
      replace ffname with racust->ffname, flname with racust->flname
      replace femp with if(empty(racust->fid1),"*",racust->fid1)
      replace fphone with racust->fphone
   endif
   replace fnor with fnor + 1       && number of rentals
   if empty(ysumfld)
      replace &yp with &yp + 1
   else
      replace &yp with &yp + &ysumfld
   endif
   * last checkin date
   if fckin < raagrh->fdatein
      replace fckin with raagrh->fdatein
   endif
   select raagrh
   skip
enddo
select raagrh
use

f_popup ("Creating Report ...")
if ydisp $ "DF"
   if empty(yfilename)
      yfil=gstnpath+"rrout.rpt"
   else
      yfil=yfilename
   endif
   set device to print
   set printer to &yfil
else
   set device to print
endif

*
private yymo[12]
y1 = "_" + substr(str(xyr,4),3,2)
y2 = "_" + substr(str(xyr+1,4),3,2)
do case
case xmo = 0
   yymo[1] = "Jan"+y1
   yymo[2] = "Feb"+y1
   yymo[3] = "Mar"+y1
   yymo[4] = "Apr"+y1
   yymo[5] = "May"+y1
   yymo[6] = "Jun"+y1
   yymo[7] = "Jul"+y1
   yymo[8] = "Aug"+y1
   yymo[9] = "Sep"+y1
   yymo[10] = "Oct"+y1
   yymo[11] = "Nov"+y1
   yymo[12] = "Dec"+y1
case xmo = 1
   yymo[1] = "Feb"+y1
   yymo[2] = "Mar"+y1
   yymo[3] = "Apr"+y1
   yymo[4] = "May"+y1
   yymo[5] = "Jun"+y1
   yymo[6] = "Jul"+y1
   yymo[7] = "Aug"+y1
   yymo[8] = "Sep"+y1
   yymo[9] = "Oct"+y1
   yymo[10] = "Nov"+y1
   yymo[11] = "Dec"+y1
   yymo[12] = "Jan"+y2
case xmo = 2
   yymo[1] = "Mar"+y1
   yymo[2] = "Apr"+y1
   yymo[3] = "May"+y1
   yymo[4] = "Jun"+y1
   yymo[5] = "Jul"+y1
   yymo[6] = "Aug"+y1
   yymo[7] = "Sep"+y1
   yymo[8] = "Oct"+y1
   yymo[9] = "Nov"+y1
   yymo[10] = "Dec"+y1
   yymo[11] = "Jan"+y2
   yymo[12] = "Feb"+y2
case xmo = 3
   yymo[1] = "Apr"+y1
   yymo[2] = "May"+y1
   yymo[3] = "Jun"+y1
   yymo[4] = "Jul"+y1
   yymo[5] = "Aug"+y1
   yymo[6] = "Sep"+y1
   yymo[7] = "Oct"+y1
   yymo[8] = "Nov"+y1
   yymo[9] = "Dec"+y1
   yymo[10] = "Jan"+y2
   yymo[11] = "Feb"+y2
   yymo[12] = "Mar"+y2
case xmo = 4
   yymo[1] = "May"+y1
   yymo[2] = "Jun"+y1
   yymo[3] = "Jul"+y1
   yymo[4] = "Aug"+y1
   yymo[5] = "Sep"+y1
   yymo[6] = "Oct"+y1
   yymo[7] = "Nov"+y1
   yymo[8] = "Dec"+y1
   yymo[9] = "Jan"+y2
   yymo[10] = "Feb"+y2
   yymo[11] = "Mar"+y2
   yymo[12] = "Apr"+y2
case xmo = 5
   yymo[1] = "Jun"+y1
   yymo[2] = "Jul"+y1
   yymo[3] = "Aug"+y1
   yymo[4] = "Sep"+y1
   yymo[5] = "Oct"+y1
   yymo[6] = "Nov"+y1
   yymo[7] = "Dec"+y1
   yymo[8] = "Jan"+y2
   yymo[9] = "Feb"+y2
   yymo[10] = "Mar"+y2
   yymo[11] = "Apr"+y2
   yymo[12] = "May"+y2
case xmo = 6
   yymo[1] = "Jul"+y1
   yymo[2] = "Aug"+y1
   yymo[3] = "Sep"+y1
   yymo[4] = "Oct"+y1
   yymo[5] = "Nov"+y1
   yymo[6] = "Dec"+y1
   yymo[7] = "Jan"+y2
   yymo[8] = "Feb"+y2
   yymo[9] = "Mar"+y2
   yymo[10] = "Apr"+y2
   yymo[11] = "May"+y2
   yymo[12] = "Jun"+y2
case xmo = 7
   yymo[1] = "Aug"+y1
   yymo[2] = "Sep"+y1
   yymo[3] = "Oct"+y1
   yymo[4] = "Nov"+y1
   yymo[5] = "Dec"+y1
   yymo[6] = "Jan"+y2
   yymo[7] = "Feb"+y2
   yymo[8] = "Mar"+y2
   yymo[9] = "Apr"+y2
   yymo[10] = "May"+y2
   yymo[11] = "Jun"+y2
   yymo[12] = "Jul"+y2
case xmo = 8
   yymo[1] = "Sep"+y1
   yymo[2] = "Oct"+y1
   yymo[3] = "Nov"+y1
   yymo[4] = "Dec"+y1
   yymo[5] = "Jan"+y2
   yymo[6] = "Feb"+y2
   yymo[7] = "Mar"+y2
   yymo[8] = "Apr"+y2
   yymo[9] = "May"+y2
   yymo[10] = "Jun"+y2
   yymo[11] = "Jul"+y2
   yymo[12] = "Aug"+y2
case xmo = 9
   yymo[1] = "Oct"+y1
   yymo[2] = "Nov"+y1
   yymo[3] = "Dec"+y1
   yymo[4] = "Jan"+y2
   yymo[5] = "Feb"+y2
   yymo[6] = "Mar"+y2
   yymo[7] = "Apr"+y2
   yymo[8] = "May"+y2
   yymo[9] = "Jun"+y2
   yymo[10] = "Jul"+y2
   yymo[11] = "Aug"+y2
   yymo[12] = "Sep"+y2
case xmo = 10
   yymo[1] = "Nov"+y1
   yymo[2] = "Dec"+y1
   yymo[3] = "Jan"+y2
   yymo[4] = "Feb"+y2
   yymo[5] = "Mar"+y2
   yymo[6] = "Apr"+y2
   yymo[7] = "May"+y2
   yymo[8] = "Jun"+y2
   yymo[9] = "Jul"+y2
   yymo[10] = "Aug"+y2
   yymo[11] = "Sep"+y2
   yymo[12] = "Oct"+y2
case xmo = 11
   yymo[1] = "Dec"+y1
   yymo[2] = "Jan"+y2
   yymo[3] = "Feb"+y2
   yymo[4] = "Mar"+y2
   yymo[5] = "Apr"+y2
   yymo[6] = "May"+y2
   yymo[7] = "Jun"+y2
   yymo[8] = "Jul"+y2
   yymo[9] = "Aug"+y2
   yymo[10] = "Sep"+y2
   yymo[11] = "Oct"+y2
   yymo[12] = "Nov"+y2
endcase
l_header = "Customer"+space(5)+"Name"+space(11)+ ;
   "Acount-No      Emp  CheckIn   Phone #     #Rental" + space(2)+ ;
   yymo[1]+"  "+yymo[2]+"  "+yymo[3]+"  "+ ;
   yymo[4]+"  "+yymo[5]+"  "+yymo[6]+"  "+ ;
   yymo[7]+"  "+yymo[8]+"  "+yymo[9]+"  "+ ;
   yymo[10]+"  "+yymo[11]+"  "+yymo[12]+"   Total"

setprc (0,0)
@ 1, 1 say l_header
yrow = 2

select mgmt_10
go top
do while .not. eof ()
   if fnor < yracnt
      skip
      loop
   endif
   @ yrow, 01 say ffname + " " + flname+ " " + facct + " "+ ;
                  femp + " " + dtoc(fckin) + " " + fphone + " "+ ;
                  str(fnor,6) + "   " + ;
                  str(f01,6) + "  " + ;
                  str(f02,6) + "  " + ;
                  str(f03,6) + "  " + ;
                  str(f04,6) + "  " + ;
                  str(f05,6) + "  " + ;
                  str(f06,6) + "  " + ;
                  str(f07,6) + "  " + ;
                  str(f08,6) + "  " + ;
                  str(f09,6) + "  " + ;
                  str(f10,6) + "  " + ;
                  str(f11,6) + "  " + ;
                  str(f12,6) + "  " + ;
                  str(f01+f02+f03+f04+f05+f06+f07+f08+f09+f10+f11+f12,6)
   yrow = yrow + 1
   skip
enddo


set device to screen
set printer to
set console on
set print off

f_valid (.f., "Process completed")

close all

