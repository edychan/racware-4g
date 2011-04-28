*
* Reservation projection module
*   setup rez availability table
*   calaulate fleet summary table
parameter xtick
set excl off
set color to
clear
*
if pcount() = 0
   xtick = 600    && default to 10 minutes
else
   xtick = val(xtick)
endif

if empty (gete ("RACPTH"))
   xpath = "\"
else
   xpath = alltrim (upper (gete ("RACPTH")))
endif
if empty (gete ("RACDBF"))
   gdbfpath = "J:" + xpath + "RACWARE\DBF\"
else
   gdbfpath = alltrim (upper (gete ("RACDBF")))
endif

do while .t.
   if inkey (xtick) = 27      && wait x sec
      exit
   endif
   ? "Setup Rez Availability..."
   do s_rez
   ? "Rebuilding fleet summary table..."
   * reset to zero
   ? time() + ":: Initialize..."
   yfil = gdbfpath + "raflts"
   select 0
   use &yfil index &yfil alias raflts
   select raflts
   go top
   do while .not. eof ()
      rlock ()
      replace favail with 0, fin with 0, fout with 0
      replace fdue with 0, fhold with 0, fres with 0
      replace fnet with 0
      commit
      unlock
      skip
   enddo
   ? time() + ":: Working on Ravm..."
   yfil = gdbfpath + "ravm"
   select 0
   use &yfil alias ravm
   * set relation to fclass+floc into raflts
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
      rlock ()
      if ravm->fstatus = "A"
         replace favail with favail + 1
      elseif ravm->fstatus = "O"
         replace fout with fout + 1
      elseif ravm->fstatus = "H"
         replace fhold with fhold + 1
      elseif ravm->fstatus = "I"
         replace fin with fin + 1
      endif
      commit
      unlock
      select ravm
      skip
   enddo
   select ravm
   use
   ? time() + ":: Working on Rez..."
   yfil = gdbfpath + "rares"
   yntx = gdbfpath + "rares3"
   select 0
   use &yfil index &yntx alias rares
   ydate = date()
   seek ydate
   do while .not. eof () .and. fdateout = ydate
      if fresvstat = "O"
         select raflts
         seek rares->fclass + rares->floc
         if eof ()
            append blank
            replace fclass with rares->fclass, floc with rares->floc
            replace fres with 1
            commit
            unlock
         else
            rlock ()
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
   ? time() + ":: Working on Raagr..."
   yfil = gdbfpath + "raagr"
   select 0
   use &yfil alias raagr
   set filter to frano > 0 .and. .not. empty (floc) ;
         .and. upper (flocal1) <> "REWRITE"
   go top
   do while .not. eof ()
      if raagr->fdatein = ydate
         select raflts
         seek raagr->fuclass + raagr->floc
         if eof ()
            append blank
            replace fclass with raagr->fuclass, floc with raagr->floc
            replace fdue with 1
            commit
            unlock
         else
            rlock ()
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
   ? time() + ":: Accumulate totals..."
   select raflts
   go top
   do while .not. eof ()
      rlock ()
      replace fnet with favail + fin + fdue - fres
      commit
      unlock
      skip
   enddo
   use
   ? time() + ":: Process completed..."
   if xtick = 1
      exit
   endif
enddo
close all

******************************
procedure s_rez

ypjdays = 45                    && default to 45 days
yfil = gdbfpath + "ravehs"      &&
select 0
use &yfil exclusive alias ravehs
zap
index on fclass to (gdbfpath + "ravehs")

? time() + ":: Initialize Ravehs..."
yfil = gdbfpath + "ravm"
select 0
use &yfil alias ravm
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
select ravehs
private yclass [reccount ()]
ynumclass = 0
go top
do while .not. eof ()
   ynumclass = ynumclass + 1
   yclass [ynumclass] = fclass
   skip
enddo
yfil = gdbfpath+"raproj"
select 0
use &yfil index &yfil alias raproj
xdate = date()
? time() + ":: Initialize Raproj with "+str(ynumclass,3)+" car class..."
for n = 0 to ypjdays
   select ravehs
   go top
   do while .not. eof ()
      select raproj
      seek ravehs->fclass + dtos(xdate + n) 
      if eof ()
         append blank
      endif
      rlock ()
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
? time() + ":: Working on rares..."
* calculate # res & # due back from res
x = 0
yfil = gdbfpath+"rares"
yntx = gdbfpath+"rares2"
select 0
use &yfil index &yntx alias rares
seek "O"
ycond1=[fdateout <= xdate + ypjdays]
ycond2=[fdatein <= xdate + ypjdays]
do while fresvstat = "O" .and. .not. eof ()
   x = x + 1
   if &ycond1
      select raproj
      seek rares->fclass + dtos (rares->fdateout) 
      if .not. eof ()
         rlock ()
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
         rlock ()
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
? time() + ":: Working on fleet variant..."
yfil = gdbfpath + "rafltv"
select 0
use &yfil alias rafltv
go top
do while .not. eof ()
   select raproj
   seek rafltv->fclass + dtos (rafltv->fdate)
   if .not. eof ()
      rlock ()
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
? time() + ":: Working on raagr..."
yfil = gdbfpath + "raagr"
select 0
use &yfil alias raagr
set filter to frano > 0 .and. .not. empty (floc) ;
      .and. upper (flocal1) <> "REWRITE"
go top
do while .not. eof ()
   if &ycond2
      select raproj
      seek raagr->fuclass + dtos (raagr->fdatein) 
      if .not. eof ()
         rlock ()
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
? time() + ":: Accumulate data..."
for n = 1 to ynumclass
   ykey = yclass [n]
   select raproj
   seek ykey + dtos(xdate)
   * 1st record
   if .not. eof ()
      ynextnet = favail + fdue - fres
      rlock ()
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
      rlock ()
      replace favail with favail + ynextnet
      replace fnet with ytmp + ynextnet + fdue - fres
      commit
      unlock
      ynextnet = favail + fdue - fres
      ynextnet = if(ynextnet > 0, ynextnet, 0)
      skip
   enddo
next 
? time() + ":: Process completed..."
close data

