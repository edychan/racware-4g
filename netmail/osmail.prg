* ====================================================================
* Onsum netmail driver program.
* --------------------------------------------------------------------
* 06.15.09: 1. Output rezdata.txt for reservation
* 07.15.09: 2. Output radata.txt for contract
*
* 10.14.09: Implement PV Rentals email format
* ----------------------------------------------
* 12.01.09: format adj for RA (makagr)
* =====================================================================
clear
set delete on
set excl off

do makrez

do makagr

close all

* =====================================================================
procedure makagr
yfil = "ramsg.dbf"
if .not. file (yfil)
   return
endif

select 0
use &yfil alias ra
go top
if eof ()
   return
endif

? "Start processing RA email queue ..." + str(reccount())
yfil = "radata.txt"
set device to print
set printer to &yfil
setprc (0,0)

* -- write header
yln = 0
@yln, 0 say [to,from,location,customer,addcust,vehicle,rateinfo,return,racharge,payment]        
yln = yln + 1

select ra
do while .not. eof () 

   yto = alltrim(ra->femail)

   yfrom = "information@pvrentals.com"

   * --location section
   if ra->floc = "PV"
      ylocation = "PV Car & Truck Rentals<br>" +;
                  "5810 South Rice<br>" +;
                  "Houston, TX 77081<br>" +;
                  "Phone: 713-667-0665<br>"
   elseif ra->floc = "IAH"
      ylocation = "PV Car & Truck Rentals<br>" +;
                  "16422 Aldine Westfield Road<br>" +;
                  "Houston, TX 77032<br>" +;
                  "Phone: 713-821-1180<br>"
   else
      ylocation = "PV Car & Truck Rentals<br>" +;
                  "2015 Milam<br>" +;
                  "Houston, TX 77002<br>" +;
                  "Phone: 713-659-2335<br>"
   endif
   * --customer section
   ycust = alltrim(ra->ffname)+" "+alltrim(ra->flname)+"<br>" +;
           alltrim(ra->faddr)+"<br>" + ;
           alltrim(ra->fcity)+", "+ra->fstate+" "+ra->fzip+"<br>"
   * --addition driver section
   if empty(ra->falname)
      yaddcust = ""
   else
      yaddcust = alltrim(ra->fafname)+" "+alltrim(ra->falname)+"<br>" +;
           alltrim(ra->faaddr)+"<br>" + ;
           alltrim(ra->facity)+", "+ra->fastate+" "+ra->fazip+"<br>"
   endif
   * --vehicle section
   yvehicle = "Check Out: "+dtoc(ra->fdateout)+" " +;
              "Time: "+ra->ftimeout+"<br>" +;
              "Check In: "+dtoc(ra->fdatein)+" " +;
              "Time: "+ra->ftimein+"<br><br>" +;
              "Vehicle #: "+ra->funit+"<br>" +;
              "Mileage In: "+str(ra->fmlgin,6,0)+"<br>" +;
              "Mileage Out: "+str(ra->fmlgout,6,0)+"<br>" +;
              "Total Miles Driven: "+str(ra->fmlgin-ra->fmlgout,6,0)+"<br>" +; 
              "Fuel Level In: "+str(ra->ffuelin,1)+"/8<br>" +;
              "Fuel Level Out: "+str(ra->ffuelout,1)+"/8<br>"
   * --rate information
   yrateinfo = if(ra->fdlychg>0,"Daily:   $"+str(ra->fdlychg,8,2)+"<br>","") +;
            if(ra->fwkdchg>0,"Ex day:  $"+str(ra->fwkdchg,8,2)+"<br>","") +;
            if(ra->fwkchg>0, "Weekly:  $"+str(ra->fwkchg,8,2)+"<br>","") +;
            if(ra->fmthchg>0,"Monthly: $"+str(ra->fmthchg,8,2)+"<br>","") +;
            if(ra->fhrchg>0, "Hour:    $"+str(ra->fhrchg,8,2)+"<br>","") +;
            "<br>" 
            * "<br>" +;
            * "State Tax<br>" +;
            * "VIT Reimbursement<br>" +;
            * "Stadium 5% <br>"
   * --return section
   yreturn = "RA #: "+trim(ra->floc)+" "+str(ra->frano,6,0)+"<br>" 
   * --charge section
   yracharge = if(ra->fhrtot>0,str(fhr,2)+" Hour @ "+str(ra->fhrchg,7,2)+": "+str(ra->fhrtot,8,2)+"<br>","") +;
    if(ra->fdlytot>0,str(fdly,2)+" Day @ "+str(ra->fdlychg,7,2)+": "+str(ra->fdlytot,8,2)+"<br>","") +;
    if(ra->fwkdtot>0,str(fwkd,2)+" Ex Day @ "+str(ra->fwkdchg,7,2)+": "+str(ra->fwkdtot,8,2)+"<br>","") +;
    if(ra->fwktot>0,str(fwk,2)+" Week @ "+str(ra->fwkchg,7,2)+": "+str(ra->fwktot,8,2)+"<br>","") +;
    if(ra->fmthtot>0,str(fmth,2)+" Month @ "+str(ra->fmthchg,7,2)+": "+str(ra->fmthtot,8,2)+"<br>","") +;
    if(ra->fmlgtot>0,"Mileage: "+str(ra->fmlgtot,8,2)+"<br>","") +;
    if(ra->fcdwtot>0,"LDW: "+str(ra->fcdwtot,8,2)+"<br>","") +;
    if(ra->fpaitot>0,"PEC: "+str(ra->fpaitot,8,2)+"<br>","") +;
    if(ra->fdmgtot>0,"Damage: "+str(ra->fdmgtot,8,2)+"<br>","") +;
    if(ra->ffueltot>0,"Fuel: "+str(ra->ffueltot,8,2)+"<br>","") +;
    if(ra->fotot1>0,if(ra->foitem1 = [SUR1],[Stadium Tax],ra->foitem1)+": "+str(ra->fotot1,8,2)+"<br>","") +;
    if(ra->fotot2>0,ra->foitem2+": "+str(ra->fotot2,8,2)+"<br>","") +;
    if(ra->fotot3>0,ra->foitem3+": "+str(ra->fotot3,8,2)+"<br>","") +;
    if(ra->fotot4>0,ra->foitem4+": "+str(ra->fotot4,8,2)+"<br>","") +;
    if(ra->fdisctot>0,"Discount: "+str(ra->fdisctot,8,2)+"<br>","") +;
    if(ra->fcredtot>0,"Credit: "+str(ra->fcredtot,8,2)+"<br>","") +;
    if(ra->fsurchg>0,"VIT Reimbursement: "+str(ra->fsurchg,8,2)+"<br>","") +;
    if(ra->ftaxtot>0,"Tax: "+str(ra->ftaxtot,8,2)+"<br>","") +;
    "<br>" +;
    "Total: "+str(ra->ftotal,8,2)+"<br>" +;
    if(ra->fdepamt>0,"Deposit: "+str(ra->fdepamt,8,2)+"<br>","") 
   * --payment section
   ypayment = if(ra->famt1>0,"[1] "+trim(ra->fpaytyp1)+" "+substr(fccnum1,len(trim(fccnum1))-3)+"....  "+str(ra->famt1,8,2)+"<br>","") + ;
    if(ra->famt2>0,"[2] "+trim(ra->fpaytyp2)+" "+substr(fccnum2,len(trim(fccnum1))-3)+"....  "+str(ra->famt2,8,2)+"<br>","") + ;
    if(ra->famt3>0,"[3] "+trim(ra->fpaytyp3)+" "+substr(fccnum3,len(trim(fccnum1))-3)+"....  "+str(ra->famt3,8,2)+"<br>","") 

   yfld = ["] + yto + ["] + [,] + ;
          ["] + yfrom + ["] + [,] + ;
          ["] + ylocation + ["] + [,] + ;
          ["] + ycust + ["] + [,] + ;
          ["] + yaddcust + ["] + [,] + ;
          ["] + yvehicle + ["] + [,] + ;
          ["] + yrateinfo + ["] + [,] + ;
          ["] + yreturn + ["] + [,] + ;
          ["] + yracharge + ["] + [,] + ;
          ["] + ypayment + ["] 

   @yln, 0 say yfld
   yln = yln + 1
   rlock ()
   delete
   skip
enddo

set printer to
set console on
set print off
set device to screen

? "Complete processing RA queue ..."
?
?

* =====================================================================
procedure makrez

yfil = "rezmsg.dbf"
if .not. file (yfil)
   return
endif

select 0
use &yfil alias rez
go top
if eof ()
   return
endif

? "Start processing REZ email queue ..." + str(reccount())
yfil = "cartype"
select 0
use &yfil index &yfil alias cartype

yfil = "rezdata.txt"
set device to print
set printer to &yfil
setprc (0,0)

* -- write header
yln = 0
@yln, 0 say [to,from,name,resno,cartype,dateout,datein,rate,surchg,surchg1,taxtot,estchg,locout,locin]        
yln = yln + 1

select rez
do while .not. eof () 

   * --define cartype
   select cartype
   seek alltrim(rez->fcartype)
   if eof ()
      ycardesc = rez->fcartype
   else
      ycardesc = alltrim(cartype->fdesc)
   endif

   * --define from
   yfrom = "reservations@pvrentals.com"

   * --location section
   if rez->floc = "PV"
      ylocout = "5810 South Rice, Houston, TX 77081<br>" +;
                  "Phone: 713-667-0665<br>"
   elseif rez->floc = "IAH"
      ylocout = "16422 Aldine Westfield Road, Houston, TX 77032<br>" +;
                  "Phone: 713-821-1180<br>"
   else
      ylocout = "2015 Milam, Houston, TX 77002<br>" +;
                  "Phone: 713-659-2335<br>"
   endif

   if rez->frloc = "PV"
      ylocin = "5810 South Rice, Houston, TX 77081<br>" +;
                  "Phone: 713-667-0665<br>"
   elseif rez->frloc = "IAH"
      ylocin = "16422 Aldine Westfield Road, Houston, TX 77032<br>" +;
                  "Phone: 713-821-1180<br>"
   else
      ylocin = "2015 Milam, Houston, TX 77002<br>" +;
                  "Phone: 713-659-2335<br>"
   endif
   * --

   select rez
   yfld = ["] + alltrim(femail) + ["] + [,] + ;
          ["] + yfrom + ["] + [,] + ;
          ["] + alltrim(fname) + ["] + [,] + ;
          ["] + alltrim(fresvno) + ["] + [,] + ;
          ["] + ycardesc + ["] + [,] + ;
          ["] + dformat(fdateout) + ["] + [,] + ;
          ["] + dformat(fdatein) + ["] + [,] + ;
          ["] + alltrim(frate) + ["] + [,] + ;
          ["] + [$]+alltrim(str(fsurchg,10,2)) + ["] + [,] + ;
          ["] + [$]+alltrim(str(fsurchg1,10,2)) + ["] + [,] + ;
          ["] + [$]+alltrim(str(ftaxtot,10,2)) + ["] + [,] + ;
          ["] + [$]+alltrim(str(ftotal,10,2)) + ["] + [,] + ;
          ["] + alltrim(ylocout) + ["] + [,] + ;
          ["] + alltrim(ylocin) + ["]

   @yln, 0 say yfld
   yln = yln + 1
   rlock ()
   delete
   skip
enddo

set printer to
set console on
set print off
set device to screen

? "Complete processing REZ queue ..."
?
?

* =========================================
* dformat:
* xdate = mm/dd/yyyy 99:99
*    return January 1, 2010 12:00pm
* 
function dformat
parameters xdate
private ymo, yday, ytime

ymo = substr(xdate,1,2)

do case
case ymo = [01]
   ymonth = "January"
case ymo = [02]
   ymonth = "Feburary"
case ymo = [03]
   ymonth = "March"
case ymo = [04]
   ymonth = "April"
case ymo = [05]
   ymonth = "May"
case ymo = [06]
   ymonth = "June"
case ymo = [07]
   ymonth = "July"
case ymo = [08]
   ymonth = "August"
case ymo = [09]
   ymonth = "September"
case ymo = [10]
   ymonth = "October"
case ymo = [11]
   ymonth = "November"
otherwise
   ymonth = "December"
endcase

yhr = val(substr(xdate,12,2))
if yhr = 0
   ytime = trim([12]+substr(xdate,14,3))+" am"
elseif yhr = 12
   ytime = trim([12]+substr(xdate,14,3))+" pm"
elseif yhr > 12
   yhr = yhr - 12
   ytime = trim(str(yhr,2)+substr(xdate,14,3))+" pm"
else 
   ytime = substr(xdate,12,5)+" am"
endif

ystr = ymonth + " " + substr(xdate,4,2)+[, ]+substr(xdate,7,4)+[ @ ]+ytime
return ystr
