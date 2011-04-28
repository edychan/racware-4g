* ===========================================================================
* update vehicle status
*
* revision
*
* date: 01/28/93
* edc: update vehicle mileage also.
* 07.13.98
* add fleet summary table (raflts)
* ===========================================================================
f_clrscn ("Vehicle Status Update")
f_use ("RAVM", 1)
set century on

@ 01, 01 say "Unit No.   Description                     License    " + ;
   "Stall Status Code Mileage"
@ 02, 01 say "ÄÄÄÄÄÄÄÄÄÄ ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ÄÄÄÄÄÄÄÄÄÄ " + ;
   "ÄÄÄÄÄ ÄÄÄÄÄÄ ÄÄÄÄ ÄÄÄÄÄÄÄ"
yrow = 3
yunit = space (10)
set key 28 to rrnvuf1
do while .t.
   @ yrow, 1 get yunit picture "!!!!!!!!!!"
   if f_rd () = 27
      exit
   endif
   select ravm
   seek yunit
   if .not. f_valid (found (), "Unit Number Not Found!!!")
      loop
   endif
   ystr = ltrim (trim(floc)+"->"+fyear + " " + fcolor + " " + fmodel)
   do while "  " $ ystr
      ystr = strtran (ystr, "  ", " ")
   enddo
   @ yrow, 12 say f_truncate (ystr, 31)
   @ yrow, 44 say flic
   ystat = fstatus
   ystall = fstall
   yreason = freason
   ymiles = fmileage
   ydmg1 = fdmg1
   ydmg2 = fdmg2
   ydmg3 = fdmg3
   yholddt = dtoc(fholddt)
   do while .t.
      @ yrow, 56 get ystall pict "!!!!"
      @ yrow, 64 get ystat picture "!" valid rrnvus1 () 
      @ yrow, 69 get yreason picture "!" valid rrnvus2 ()
      @ yrow, 73 get ymiles pict "999999" ;
         valid f_valid (ymiles >= fmileage)
      f_rd ()
      xkey = f_confirm ("[C]onfirm  [E]dit  [I]gnore Changes", "CEI")
      do case
      case xkey = "C"
         * update fleet summary
         f_use ("RAFLTS")
         seek ravm->fclass + ravm->floc
         if .not. eof ()
            reclock ()
            do case
            case ravm->fstatus = "A" 
               replace favail with favail - 1
            case ravm->fstatus = "I" 
               replace fin with fin - 1
            case ravm->fstatus = "O" 
               replace fout with fout - 1
            case ravm->fstatus = "H" 
               replace fhold with fhold - 1
            endcase
            commit
            unlock
            *
            reclock ()
            do case
            case ystat = "A" 
               replace favail with favail + 1
            case ystat = "I" 
               replace fin with fin + 1
            case ystat = "O" 
               replace fout with fout + 1
            case ystat = "H" 
               replace fhold with fhold + 1
            endcase
            replace fnet with favail + fin + fdue - fres
            commit
            unlock
         endif
         use
         select ravm
         * f_fupdate ("C")
         reclock ()
         if fstatus = "H" .and. ystat <> "H"
            replace freason with " "
            replace fholddt with ctod("  /  /  ")
         endif
         if ystat = "H"
            replace freason with yreason
            replace fholddt with ctod(yholddt)
         endif
         replace fstatus with ystat, fmileage with ymiles
         replace fstall with ystall, fdmg1 with ydmg1
         replace fdmg2 with ydmg2, fdmg3 with ydmg3
         replace futime with dtos (date ()) + time ()
         commit
         unlock
         exit
      case xkey = "E"
         loop
      case xkey = "I"
         exit
      endcase
   enddo
   if xkey <> "I"
      if yrow < 20
         yrow = yrow + 1
      else
         scroll (3, 0, 20, 79, 1)
      endif
   else
      @ yrow, 12
   endif
   yunit = space (10)
enddo

set century off
set key 28 to
close database

*************************
function rrnvus1

if .not. f_valid (ystat $ "AOHI")
   return .f.
endif

if ystat = "H"
   yscn = f_box (yrow+1, 55, yrow+3, 66, "Due Back")
   if empty(ctod(yholddt))
      yholddt = dtoc(date())
   endif
   f_getfld (@yholddt, yrow+2, 56, "W/N", 0, "99/99/9999", .t.)
   f_restbox (yscn)
endif

return .t.

*****************************
function rrnvus2

if ystat = "H" .and. empty(yreason)
   return f_valid (.f., "Please selection Reason Code...")  
elseif .not. empty(yreason)
   return f_valid ((f_locate ("RAVS","fcode",yreason)))
else
   return .t.
endif
