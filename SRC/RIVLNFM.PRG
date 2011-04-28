* ===========================================================================
* review vehicle loss notice (claims)
*
* date: 09/28/92
* author: edc
*
* date: 01/28/93
* edc: refine inquiry function
* ===========================================================================
PRIVATE yarray [2]
f_clrscn ("REVIEW VEHICLE LOSS NOTICE")
f_use ("RAVLN")
if .not. f_valid (.not. eof(), "File is empty !")
   use
   return
endif

yarray[1] = "Plate #....."
yarray[2] = "Unit  #....."
do while .t.
   yret = f_pick_a (02, 05, "", "", yarray, 2, 1)
   if yret = 0
      exit
   endif
   if yret = 1
     yval = space (10)
   else
     yval = space (10)
   endif
   @ 02, 05 say yarray[yret] get yval
   if f_rd () = 27
      exit
   endif
   yval = upper (yval)
   set softseek on
   set exact off
   select ravln
   set order to (yret)
   seek yval
   set softseek off
   if f_pick_f (4, 2, "", "Unit #ÄÄÄÄÄPlate #ÄÄÄÄDateÄÄÄÄÄ" + ;
         "Violation #ÄÄÄRemark", ;
         "funit+[ ]+flic+[ ]+dtoc(fviodate)+[ ]+" + ;
         "+fviono+[  ]+substr(frmk1,1,25)")
      f_update ("ravln", "ravln")
      exit
   endif
enddo

close databases

*****************************
* Calculate cost totals
function rivlnfm1

f_compute (@l_ftotal1, l_festamt1+l_festamt2+l_festamt3+ ;
                       l_fwrecamt+l_fadmin+l_floss+l_fdimval)
@ 20, 50 say l_ftotal1 pict [99999.99]

return .t.

********************************
* calculate paid totals
function rivlnfm2

f_compute (@l_ftotal2, l_fpaid1+l_fpaid2+l_fpaid3+l_fpaidw+ ;
                       l_fpaidadm+l_fpaidlu+l_fpaiddv)
@ 20, 69 say l_ftotal2 pict [99999.99]
return .t.

