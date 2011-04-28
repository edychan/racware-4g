* ===========================================================
* customer file maint.
* -----------------------------------------------------------
* 05.21.09: add CRM function
*           add FCRM1, FCRM2 ... FCRM5 to racust.dbf
*           FCRM ::= date,Id,purpose,note1,note2
* ===========================================================
private yarray[3], yptr, yret, ykey, yval  

do while .t.
   xret2 = f_pushmenu (xoption1)
   if xret2 = 0
      exit
   endif
   xoption2 = substr (goption [xret2], 4)
   f_clrscn ("CUSTOMER FILE MAINTENANCE")
   rest from (gmempath+"racust") additive
   set key 28 to rcusthlp
   * --05.21.09:
   set key -1 to rcrmshow
   * set key -2 to rcrmget    && not use
   do case
   case xret2 = 1
      f_use ("racust", 2)
      do while .t.
         @ 02, 05 say "Account Number " get l_fcustno ;
            picture "!!!!!!!!!!!!!!!"
         if f_rd() = 27 .or. empty(l_fcustno)
            exit
         endif
         set exact on
         seek l_fcustno
         set exact off
         if eof()
            l_fmoddt = date()
            f_add ("racust", "racust")
            close databases
            exit
         else
            tone (500, 9)
            f_popup ("Duplicate Customer #. Press Any Key to Retry ...",.t.)
         endif
      enddo
   case xret2 = 2
      f_use ("racust",2)
      go top
      if .not. eof()
         yarray[1] = " Account Number "
         yarray[2] = " Last Name..... "
         yarray[3] = " First Name.... "
         yret = f_pick_a (02, 05, "", "", yarray, 3, 1)
         if yret = 0
            loop
         endif
         yval = space (15)
         @ 02, 05 say yarray[yret] get yval
         read
         yval = upper (yval)
         if yret = 1
            f_use ("racust", 2)
         elseif yret = 2
            f_use ("racust", 1)
         else
            f_use ("racust", 4)
         endif
         set softseek on
         set exact off
         seek yval
         set softseek off
         if eof()
            tone (500, 9)
            f_popup ("No Matching Record. Press Any Key to Retry...",.t.)
            close databases
            loop
         endif
         if (found () .and. yret = 1) .or. ;
               f_pick_f (02, 3, "", "Customer No.컴컴Last Name컴컴컴First " + ;
               "Name컴훀ate", "fcustno+[ ]+flname+[ ]+ffname+[ ]+frate")
            f_update ("racust", "racust", "", "rmkcust1")
            close databases
         endif
      else
         close databases
         tone (500, 9)
         f_popup ("File is empty. Press Any Key...",.t.)
      endif
   endcase
enddo

set key 28 to
set key -1 to
set key -2 to

release all like l_*
close database

* ===================================================================
procedure rcrmshow

private yscn, ycolor, yptr, ydate1, ydate2, ydob, ylicexp

ycolor = setcolor (gsubcolor)
yscn = f_box (3, 3, 22, 75)

@ 04,05 say "Date: "
@ 04,19 say "UID: "
@ 04,28 say "Subj: "

@ 07,05 say "Date: "
@ 07,19 say "UID: "
@ 07,28 say "Subj: "

@ 10,05 say "Date: "
@ 10,19 say "UID: "
@ 10,28 say "Subj: "

@ 13,05 say "Date: "
@ 13,19 say "UID: "
@ 13,28 say "Subj: "

@ 16,05 say "Date: "
@ 16,19 say "UID: "
@ 16,28 say "Subj: "

@ 19,05 say "Date: "
@ 19,19 say "UID: "
@ 19,28 say "Subj: "

setcolor (gsubget)
@ 04, 10 say date()
@ 04, 23 say space(3)
@ 04, 33 say space(40)

@ 07, 10 say substr(racust->fcrm1,1,8)
@ 07, 23 say substr(racust->fcrm1,9,3)
@ 07, 33 say substr(racust->fcrm1,12,40)

@ 10, 10 say substr(racust->fcrm2,1,8)
@ 10, 23 say substr(racust->fcrm2,9,3)
@ 10, 33 say substr(racust->fcrm2,12,40)

@ 13, 10 say substr(racust->fcrm3,1,8)
@ 13, 23 say substr(racust->fcrm3,9,3)
@ 13, 33 say substr(racust->fcrm3,12,40)

@ 16, 10 say substr(racust->fcrm4,1,8)
@ 16, 23 say substr(racust->fcrm4,9,3)
@ 16, 33 say substr(racust->fcrm4,12,40)

@ 19, 10 say substr(racust->fcrm5,1,8)
@ 19, 23 say substr(racust->fcrm5,9,3)
@ 19, 33 say substr(racust->fcrm5,12,40)

setcolor (gsubcolor)
@ 05, 07 say space(66)
@ 06, 07 say space(66)

@ 08, 07 say substr(racust->fcrm1,52,66)
@ 09, 07 say substr(racust->fcrm1,118,66)

@ 11, 07 say substr(racust->fcrm2,52,66)
@ 12, 07 say substr(racust->fcrm2,118,66)

@ 14, 07 say substr(racust->fcrm3,52,66)
@ 15, 07 say substr(racust->fcrm3,118,66)

@ 17, 07 say substr(racust->fcrm4,52,66)
@ 18, 07 say substr(racust->fcrm4,118,66)

@ 20, 07 say substr(racust->fcrm5,52,66)
@ 21, 07 say substr(racust->fcrm5,118,66)

yptr = 1
ydate = dtoc(date())
yid = gusrid
ysubject = space(40)
ynote1 = space(66)
ynote2 = space(66)
do while .t.
   if lastkey () = 27
      exit
   endif
   do case
   case yptr = 1
      f_getfld (@yid, 4, 23, "W/N", 0, replicate ("!", 3), .t.)
      if .not. f_verify ("rausr",1,yid)
         f_valid (.f.,"Invalid Id...")
         loop
      endif
   case yptr = 2
      f_getfld (@ysubject, 4, 33, "W/N", 0, replicate ("X", 40), .t.)
      if empty(ysubject)
         f_valid (.f., "Please enter the subject...")
         loop
      endif
   case yptr = 3
      f_getfld (@ynote1, 5, 7, "W/N", 0, replicate ("X", 66), .t.)
   case yptr = 4
      f_getfld (@ynote2, 6, 7, "W/N", 0, replicate ("X", 66), .t.)
   endcase
   ykey = lastkey ()
   if (ykey = 24 .or. ykey = 13) .and. yptr < 4
      yptr = yptr + 1
   elseif ykey = 5 .and. yptr > 1
      yptr = yptr - 1
   elseif ykey = 27 .or. ykey = 13 .or. ykey = 3 .or. ykey = 18
      exit
   endif
enddo

* -- push on to the message stack
if .not.empty(yid) .and. .not.empty(ysubject) .and. lastkey() <> 27
   * --
   l_fcrm5 = racust->fcrm4
   l_fcrm4 = racust->fcrm3
   l_fcrm3 = racust->fcrm2
   l_fcrm2 = racust->fcrm1
   l_fcrm1 = ydate+yid+ysubject+ynote1+ynote2
   reclock ()
   replace fcrm1 with l_fcrm1, fcrm2 with l_fcrm2, fcrm3 with l_fcrm3
   replace fcrm4 with l_fcrm4, fcrm5 with l_fcrm5
   commit
   unlock
   * --
endif

f_restbox (yscn)
setcolor (ycolor)

* ===================================================================
procedure rcusthlp

tone (500,9)
f_popup ("No Help information for this Field. Press Any Key to Continue...",.t.)
set cursor on
return


* ===================================================================
procedure rmkcust1

l_fmoddt = max (date (), l_fmoddt)


* ===================================================================
function rmkcustf

if empty (l_fcrpno)
   return .t.
endif
f_use ("racrp", 1)
seek l_fcrpno
if found ()
   l_fcompany = fcompany
   l_fcaddr = faddr
   l_fccity = fcity
   l_fcstate = fstate
   l_fczip = fzip
   l_frate = frate
   l_fdisc = fdisc
   l_fcphone = fphone
   l_fdbacct = fdbacct
   l_fcctype = fcctype
   l_fccnum = fccnum
endif
select racust
return .t.

