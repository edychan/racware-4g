*
set excl on
clear
select 1
use j:raidx index j:raidx
go top
do while .not. eof ()
   if a->fname = "RAIDX"
      skip
      loop
   endif
   yfil="j:"+trim(a->fname) 
   yexpr=trim(a->fidx)
   yntx="j:"+trim(a->fndx)
   ? 'Creating... '+yntx
   select 2
   use &yfil
   index on &yexpr to &yntx
   select 1
   skip
enddo
