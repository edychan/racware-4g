*
* special program for resort rent a car 
* (create $1 ax transaction)
*
set excl off
clear

ypaytyp = "AX "
yccnum = space(20)
yexp = "  /  "
yfname = space(12)
ylname = space(14)
yauthcode = space (6)
yauthstat = space (20)
yamt = 1.00
yloc = space(10)
ydbrno = 9999
yrano = 0
ytimes = 0
yrectype = 3
yauthonly = .f.

@ 03,01 say "Credit Card Type....."
@ 04,01 say "Credit Card Number..."
@ 05,01 say "Expiration Date......"
@ 06,01 say "First Name..........."
@ 07,01 say "Last Name............"
@ 08,01 say "Auth. Code..........."
@ 09,01 say "Amount per trans....."
@ 10,01 say "Location Code........"
@ 11,01 say "Rental Agreement....."
@ 12,01 say "No. of trans........."

do while .t.
   @ 03,23 get ypaytyp pict "!!!"
   @ 04,23 get yccnum 
   @ 05,23 get yexp    pict "99/99"
   @ 06,23 get yfname 
   @ 07,23 get ylname
   @ 08,23 get yauthcode 
   @ 09,23 get yamt    pict "99.99"
   @ 10,23 get yloc    pict "!!!!!!!!!!"
   @ 11,23 get yrano   pict "999999"
   @ 12,23 get ytimes  pict "999"
   read
   yans = "N"
   @ 14,01 say "Are you sure [Y/N/Q] " get yans pict "!"
   read
   if yans = "Y"
      exit
   elseif yans = "Q"
      quit
   endif
enddo

@ 16, 01 say "Please Wait...        "
select 1
use j:racred index j:racred1, j:racred2, j:racred3, j:racred4, j:racred5

for i = 1 to ytimes
   append blank
   replace fauthonly with yauthonly, fauthamt with abs (yamt)
   replace fauthcode with yauthcode, fauthstat with yauthstat
   replace fauthdate with date (), fauthtime with time ()
   replace fccexp with yexp, fccnum with yccnum
   replace fcctype with ypaytyp, ffname with yfname
   replace flname with ylname, floc with yloc
   replace frano with yrano, frectype with yrectype
   replace fdbrno with ydbrno, frloc with yloc
   replace ftranstyp with "S"
   commit
   unlock
next i

@ 16, 01 say "Processed Completed..."
