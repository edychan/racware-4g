* ===========================================================================
* Reservation availability module
*
* 06.01.98
* edc
* 01.28.09: use raagr->freferal for [REWRITE] calculation
* ===========================================================================
do while .t.
   xret2 = f_pushmenu (xoption1)
   if xret2 = 0
      exit
   endif
   xoption2 = substr (goption [xret2], 4)
   do case
   case xret2 = 1
      do rrvra1
   case xret2 = 2
      do rrvra2
   case xret2 = 3       && fleet summary table
      do rrvra3
   case xret2 = 4       && fleet variance table
      do rrvra4
   endcase
enddo

***********************
procedure rrvra1
private n, ypjdays

f_clrscn ("SETUP RES AVAILABILITY")

f_box (03, 03, 05, 62)
*yans = [ ]
*@ 04, 05 say "Do you want to rebuild Res Availability table [Y/N]? "
*@ 04, 58 get yans pict "!" valid f_valid (yans $ "YN")
ypjdays = 0
@ 04, 05 say "Enter # of days to rebuild Res Availability table "
@ 04, 58 get ypjdays pict "99" valid f_valid (ypjdays >= 0.and.ypjdays <= 45)
if f_rd () = 27 .or. ypjdays = 0
   return
endif

* only 1 person can re-calculate
f_use ("raproj")
seek "----"
if eof ()
   append blank
   replace fclass with "----", favail with 1
   commit
   unlock
else
   if raproj->favail > 0
      f_valid (.f., "Rebuild is in progress...")
      close data
      return
   endif
   reclock ()
   replace favail with 1
   commit
   unlock
endif
use

* build vehicle class table
select 0
create (gstnpath + "stru")
use (gstnpath + "stru") exclusive
append blank
replace field_name with "FCLASS"
replace field_type with "C"
replace field_len with 4
replace field_dec with 0
append blank
replace field_name with "FAVAIL"
replace field_type with "N"
replace field_len with 4
replace field_dec with 0
append blank
replace field_name with "FHOLD"
replace field_type with "N"
replace field_len with 4
replace field_dec with 0
use
create (gstnpath + "ravehs") from (gstnpath + "stru")
erase (gstnpath + "stru.dbf")
use (gstnpath + "ravehs") exclusive alias ravehs
zap
index on fclass to (gstnpath + "ravehs")

f_use ("ravm")
go top
do while .not. eof ()
   if empty(ravm->fclass)
      skip
      loop
   endif
   select ravehs
   seek ravm->fclass
   if eof ()
      append blank
      replace fclass with ravm->fclass
      if ravm->fstatus $ "AIX"
         replace favail with 1
      elseif ravm->fstatus = "H"
         replace fhold with 1
      endif
   else
      if ravm->fstatus $ "AIX"
         replace favail with favail + 1
      elseif ravm->fstatus = "H"
         replace fhold with fhold + 1
      endif
   endif
   select ravm
   skip
enddo
select ravm 
use

f_popup ("Please Wait...")
select ravehs
private yclass [reccount ()]
go top
ynumclass = 0
setcolor (gbluecolor)
@ 1, 0 clear to 23, 79
do while .not. eof ()
   ynumclass = ynumclass + 1
   yclass [ynumclass] = fclass
   @ 2, 5 say "Number of vehicle classes = " + ltrim (str (ynumclass))
   skip
enddo

f_use ("raproj")

xdate = date()
for n = 0 to ypjdays
   select ravehs
   go top
   do while .not. eof ()
      select raproj
      seek ravehs->fclass + dtos(xdate + n) 
      if eof ()
         append blank
      endif
      reclock ()
      replace fdate with date () + n
      replace fclass with ravehs->fclass
      replace fres with 0, fdue with 0, fhold with ravehs->fhold
      replace fnet with 0
      if n = 0
         replace favail with ravehs->favail
      else
         replace favail with 0
      endif
      commit
      unlock
      select ravehs
      skip
   enddo
next
select ravehs
use

f_popback ()

* calculate # res & # due back from res
x = 0
f_use ("rares", 2)
seek "O"
ycond1='fdateout <= xdate + ypjdays'
ycond2='fdatein <= xdate + ypjdays' 
do while fresvstat = "O" .and. .not. eof ()
   x = x + 1
   @ 4, 5 say "Scanning " + ltrim (str (x)) + " reservations"
   if &ycond1
      select raproj
      seek rares->fclass + dtos (rares->fdateout) 
      if .not. eof ()
         reclock ()
         replace fres with fres + 1
         commit
         unlock
      endif
      select rares
   endif
   if &ycond2
      select raproj
      seek rares->fclass + dtos (rares->fdatein) 
      if .not. eof ()
         reclock ()
         replace fdue with fdue + 1
         commit
         unlock
      endif
      select rares
   endif
   skip
enddo
select rares
use

* calculate fleet variant
yfil = gdbfpath + "rafltv"
select 0
use &yfil alias rafltv
go top
do while .not. eof ()
   select raproj
   seek rafltv->fclass + dtos (rafltv->fdate)
   if .not. eof ()
      reclock ()
      replace favail with favail + rafltv->fvar
      commit
      unlock
   endif
   select rafltv
   skip
enddo
select rafltv
use

* calculate # due back from open ra
f_use ("raagr")
select raagr
* --01.28.09: set filter to frano > 0 .and. .not. empty (floc) ;
*      .and. upper (flocal1) <> "REWRITE"
set filter to frano > 0 .and. .not. empty (floc) ;
      .and. upper (freferal) <> "REWRITE"
go top
x = 0
do while .not. eof ()
   x = x + 1
   @ 6, 5 say "Scanning " + ltrim (str (x)) + " Open Contracts"
   if &ycond2
      select raproj
      seek raagr->fuclass + dtos (raagr->fdatein) 
      if .not. eof ()
         reclock ()
         replace fdue with fdue + 1
         commit
         unlock
      endif
   endif
   select raagr
   skip
enddo
select raagr
use

f_popup ("Accumulating Data", .f.)
for n = 1 to ynumclass
   ykey = yclass [n]
   select raproj
   seek ykey + dtos(xdate)
   * 1st record
   if .not. eof ()
      ynextnet = favail + fdue - fres
      reclock ()
      replace fnet with fnet + ynextnet
      commit
      unlock
      skip
      ynextnet = if(ynextnet>0, ynextnet, 0)
   else
      ynextnet = 0
   endif
   do while .not. eof () .and. fclass = ykey
      ytmp = favail
      reclock ()
      replace favail with favail + ynextnet
      replace fnet with ytmp + ynextnet + fdue - fres
      commit
      unlock
      ynextnet = favail + fdue - fres
      ynextnet = if(ynextnet > 0, ynextnet, 0)
      skip
   enddo
next 

* reset control
select raproj
seek "----"
if .not. eof ()
   reclock ()
   replace favail with 0
   commit
   unlock
endif

close data
f_valid (.f., "Process Completed...")

***********************
procedure rrvra2
private yclass, ydate
declare l_dbfld [7], l_dbcol [7], l_dbvalid [7], l_dbpict [7]

f_clrscn ("UPDATE RES AVAILABILITY")
f_box (02, 02, 05, 45)
yclass = space(4)
ydate = date()
@ 03, 04 say "Class...."
@ 04, 04 say "Date....."
@ 03, 14 get yclass pict "!!!!"
@ 04, 14 get ydate 
if f_rd () = 27
   return
endif

f_use ("raproj")  && set order to fclass+fdate
set softseek on
seek yclass+dtos(ydate)
set softseek off   
l_dbfld[1]="fclass"
l_dbfld[2]="fdate"
l_dbfld[3]="favail"
l_dbfld[4]="fhold"
l_dbfld[5]="fres"
l_dbfld[6]="fdue"
l_dbfld[7]="fnet"

l_dbcol[1]="Class"
l_dbcol[2]="Date"
l_dbcol[3]="Avail"
l_dbcol[4]="On Hold"
l_dbcol[5]="Res"
l_dbcol[6]="Due "
l_dbcol[7]="Net"

l_dbvalid[1]=".t."
l_dbvalid[2]=".t."
l_dbvalid[3]=".t."
l_dbvalid[4]=".t."
l_dbvalid[5]=".t."
l_dbvalid[6]=".t."
l_dbvalid[7]=".t."

l_dbpict[1]="!!!!"
l_dbpict[2]="@D"
l_dbpict[3]="99999"
l_dbpict[4]="99999"
l_dbpict[5]="99999"
l_dbpict[6]="99999"
l_dbpict[7]="99999"

f_edit (l_dbfld, l_dbcol, 0, .t., l_dbvalid, l_dbpict)
use


***********************
* fleet summary table
procedure rrvra3
private yfil, yans

f_clrscn ("UPDATE FLEET SUMMARY TABLE")

f_box (03, 03, 05, 60)
yans = [ ]
@ 04, 05 say "Do you want to rebuild fleet summary table [Y/N]? "
@ 04, 57 get yans pict "!" valid f_valid (yans $ "YN")
if f_rd () = 27
   return
endif

if yans = "Y"
   f_use ("raflts")
   * reset to zero
   go top
   do while .not. eof ()
      reclock ()
      replace favail with 0, fin with 0, fout with 0
      replace fdue with 0, fhold with 0, fres with 0
      replace fnet with 0
      commit
      unlock
      skip
   enddo
   f_popup ("Please Wait...")
   * calculate fleet summary
   f_use ("ravm")
   go top
   do while .not. eof ()
      if empty (ravm->fclass)
         skip
         loop
      endif 
      select raflts
      seek ravm->fclass+ravm->floc
      if eof ()
         append blank
         replace fclass with ravm->fclass, floc with ravm->floc
      endif
      reclock ()
      do case
         case ravm->fstatus = "A"
            replace favail with favail + 1
         case ravm->fstatus = "O"
            replace fout with fout + 1
         case ravm->fstatus = "H"
            replace fhold with fhold + 1
         case ravm->fstatus = "I"
            replace fin with fin + 1
      endcase
      commit
      unlock
      select ravm
      skip
   enddo
   select ravm
   use
   * calculate res 
   f_use ("rares", 3)    && date order
   ydate = date()
   seek ydate
   do while .not. eof () .and. fdateout = ydate
      if fresvstat = "O"
         select raflts
         seek rares->fclass + rares->floc
         if eof ()
            append blank
            replace fclass with rares->fclass, floc with rares->floc
            replace fres with fres + 1
            commit
            unlock
         else
            reclock ()
            replace fres with fres + 1
            commit
            unlock
         endif
      endif
      select rares
      skip
   enddo
   select rares
   use
   * calculate due back from open ra
   f_use ("raagr")
   select raagr
   * --01.28.09: set filter to frano > 0 .and. .not. empty (floc) ;
   *      .and. upper (flocal1) <> "REWRITE"
   set filter to frano > 0 .and. .not. empty (floc) ;
         .and. upper (freferal) <> "REWRITE"
   * --
   go top
   do while .not. eof ()
      if raagr->fdatein = ydate
         select raflts
         seek raagr->fuclass + raagr->floc
         if eof ()
            append blank
            replace fclass with raagr->fuclass, floc with raagr->floc
            replace fdue with fdue + 1
            commit
            unlock
         else
            reclock ()
            replace fdue with fdue + 1
            commit
            unlock
         endif
      endif
      select raagr
      skip
   enddo
   select raagr
   use

   * accumulate total
   select raflts
   go top
   do while .not. eof ()
      reclock ()
      replace fnet with favail + fin + fdue - fres
      commit
      unlock
      skip
   enddo
   use
   f_popback ()
endif

f_use ("raflts")
declare l_dbfld [9], l_dbcol [9], l_dbvalid [9], l_dbpict [9]

l_dbfld[1]="fclass"
l_dbfld[2]="floc"
l_dbfld[3]="favail"
l_dbfld[4]="fin"
l_dbfld[5]="fout"
l_dbfld[6]="fdue"
l_dbfld[7]="fhold"
l_dbfld[8]="fres"
l_dbfld[9]="fnet"

l_dbcol[1]="Class"
l_dbcol[2]="Location"
l_dbcol[3]="Avail"
l_dbcol[4]="In"
l_dbcol[5]="On Rent"
l_dbcol[6]="Due"
l_dbcol[7]="Hold"
l_dbcol[8]="Res"
l_dbcol[9]="Net"

l_dbvalid[1]=".t."
l_dbvalid[2]=".t."
l_dbvalid[3]=".t."
l_dbvalid[4]=".t."
l_dbvalid[5]=".t."
l_dbvalid[6]=".t."
l_dbvalid[7]=".t."
l_dbvalid[8]=".t."
l_dbvalid[9]=".t."


l_dbpict[1]="!!!!"
l_dbpict[2]="!!!!!!!!!!"
l_dbpict[3]="9999"
l_dbpict[4]="9999"
l_dbpict[5]="9999"
l_dbpict[6]="9999"
l_dbpict[7]="9999"
l_dbpict[8]="9999"
l_dbpict[9]="9999"

select raflts
go top
f_edit (l_dbfld, l_dbcol, 0, .t., l_dbvalid, l_dbpict)
use

***********************
* fleet variance table
procedure rrvra4
private yfil

f_clrscn ("UPDATE FLEET VARIANCE TABLE")
* build  fleet variance table
yfil = gdbfpath + "rafltv.dbf"
if .not. file (yfil)
   select 0
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
   replace field_name with "FVAR"
   replace field_type with "N"
   replace field_len with 4
   replace field_dec with 0
   use
   create (gdbfpath + "rafltv") from (gstnpath + "stru")
   erase (gstnpath + "stru.dbf")
   use &yfil alias rafltv
else
   select 0
   use &yfil alias rafltv
endif

declare l_dbfld [3], l_dbcol [3], l_dbvalid [3], l_dbpict [3]

l_dbfld[1]="fdate"
l_dbfld[2]="fclass"
l_dbfld[3]="fvar"

l_dbcol[1]="Date"
l_dbcol[2]="Class"
l_dbcol[3]="Variant"

l_dbvalid[1]=".t."
l_dbvalid[2]=".t."
l_dbvalid[3]=".t."


l_dbpict[1]="@D"
l_dbpict[2]="!!!!"
l_dbpict[3]="9999"

f_edit (l_dbfld, l_dbcol, 0, .t., l_dbvalid, l_dbpict)
use

