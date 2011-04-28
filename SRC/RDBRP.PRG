private yok

l_floc = floc
l_fdbrno = fdbrno
l_fdbrptdate = frptdate
l_fperoid = substr(dtoc(l_fdbrptdate),7,2)  + substr(dtoc(l_fdbrptdate),1,2)
yok = .t.

if notzero(ftotal+fdep-frecv-fprev)
   f_valid (.f.,"Please reprint DBR after Audits ...")
   close data
   return
else
   select radbr
   f_fupdate ("C")
   reclock ()
   replace fglpstd with .t.
   commit
   unlock
endif

do rdbrp1 with l_floc, l_fdbrno, yok                     

if yok
   do rdbrp2                            
   select radbr
   f_fupdate ("C")
   reclock ()
   replace fpost with date(), fglpstd with .t.
   commit
   unlock
   do rdbrp3                            
else
   select radbr
   f_fupdate ("C")
   reclock ()
   replace fglpstd with .f.
   commit
   unlock
endif

close data
release all like l_*
