*
* 1. update vm for sch. update
* 2. clean up raagr (05.18.01)
*
parameter xloc

set exclusive off
set delete on

if pcount () = 0
   quit
endif

xloc = upper(xloc)

if empty (gete ("RACDRV"))
   xdrive = "C:"
else
   xdrive = alltrim (upper (gete ("RACDRV")))
endif

if empty (gete ("RACPTH"))
   xpath = "\"
else
   xpath = alltrim (upper (gete ("RACPTH")))
endif

if empty (gete ("RACSID"))
   gstation = ""
else
   gstation = alltrim (upper (gete ("RACSID")))
endif

if empty (gete ("RACDBF"))
   gdbfpath = "J:" + xpath + "RACWARE\DBF\"
else
   gdbfpath = alltrim (upper (gete ("RACDBF")))
endif
 
if empty (gete ("RACSTN"))
   gstnpath = "I:" + xpath + "RACWARE\STN\" + gstation + "\"
else
   gstnpath = alltrim (upper (gete ("RACSTN")))
endif

if empty (gete ("RACMEM"))
   gmempath = xdrive + xpath + "RACWARE\MEM\"
else
   gmempath = alltrim (upper (gete ("RACMEM")))
endif

if empty (gete ("RACSUP"))
   gsuppath = xdrive + xpath + "RACWARE\SUP\"
else
   gsuppath = alltrim (upper (gete ("RACSUP")))
endif
gsuppath = gsuppath + "DBF\"

restore from (gmempath + "RAPATH") additive

f_use ("ravm")

xfil = gsuppath + "RAVM.DBF"
xntx = gsuppath + "RAVM.NTX"

if file (xfil) .and. file (xntx)
   xkeyfld = "_GKRAVM"
   xkeyfld = substr (&xkeyfld, 2)
   xblankfld = "_GBRAVM"
   xblankfld = descend (&xblankfld)
   select 0
   use &xfil index &xntx alias supfil

   select ravm
   go top

   do while .not. eof () 
      if ravm->floc <> xloc
         skip
         loop
      endif

      ? ravm->funit
      select supfil
      seek descend (ravm->funit)
      if eof ()
         seek xblankfld
         xupd = .f.
         do while .not. eof () .and. &xkeyfld = xblankfld
            if rlock ()
               xupd = .t.
               exit
            else
               skip
            endif
         enddo
         if .not. xupd
            xupd = .t.
            append blank
            rlock ()
         endif
      else
         xupd = .t.
         rlock ()
      endif

      for n = 1 to fcount ()
         xfld1 = field (n)
         xfld2 = "RAVM->" + xfld1
         replace &xfld1 with &xfld2
      next
      replace fupdate with "A"
      commit
      unlock
      select ravm
      skip
   enddo
   select supfil
   use
endif
select ravm
use

* 05.18.01 check raagr
f_use ("raagrh",1)
f_use ("raagr")
do while .not. eof ()
   if raagr->frano = 0
      skip
      loop
   endif
   select raagrh
   seek raagr->floc+str(raagr->frano,6)
   if .not. eof ()
      ? frano
      select raagr
      if f_clrskip () = 0
         exit
      endif
   else
      select raagr
      skip
   endif
enddo
close data

*************************
function f_clrrec

private xfld, xlfld, n
rlock ()
for n = 1 to fcount ()
   xfld = field (n)
   if type (xfld) = "D"
      replace &xfld with ctod ("  /  /  ")
   elseif type (xfld) = "N"
      replace &xfld with 0
   elseif type (xfld) = "C"
      replace &xfld with " "
   elseif type (xfld) = "L"
      replace &xfld with .f.
   endif
next
commit
unlock

******************************
function f_clrskip

parameter xfunc

private yrec1, yrec2
yrec1 = recno ()
skip 1
if eof ()
   yrec2 = 0
else
   yrec2 = recno ()
endif
go (yrec1)
if pcount () < 1
   f_clrrec ()
else
   do &xfunc
endif
if yrec2 > 0
   go (yrec2)
endif
return (yrec2)

******************************
function f_use

parameters xfile, xorder
private xfilestr, xslct, xpath

if pcount () < 2
   xorder = 0
endif
xfile = upper (alltrim (xfile))

xfilestr = "_G" + xfile

xslct = left (&xfilestr, 2)
select &xslct

if .not. used () .or.  upper (alias ()) <> xfile

   xpath = substr (&xfilestr, 3, 1)
   do case
   case xpath = "D"
      xpath = gdbfpath
   case xpath = "S"
      xpath = gstnpath
   otherwise
      xpath = ""
   endcase

   store "" to xndx1, xndx2, xndx3, xndx4, xndx5, xndx6, xndx7, xndx8, xndx9
   xndx = substr (&xfilestr, 4)
   xndxcnt = 0
   do while .not. empty (xndx) .and. xndxcnt < 9
      xndxcnt = xndxcnt + 1
      xndxstr = "xndx" + str (xndxcnt, 1)
      &xndxstr = xpath + alltrim (left (xndx, 8))
      xndx = substr (xndx, 9)
   enddo

   do while .t.
      if xndxcnt = 0
         use (xpath + xfile) alias &xfile
      else
         use (xpath + xfile) index &xndx1, &xndx2, &xndx3, &xndx4, &xndx5, ;
            &xndx6, &xndx7, &xndx8, &xndx9 alias &xfile
      endif
      if neterr ()
         inkey (2)
         loop
      else 
         exit
      endif
   enddo
endif

if xorder > 0
   set order to xorder
endif


