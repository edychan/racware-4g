set lib=f:\rac\lib
clipper @racmaint -o..\obj
cd..\obj
blinker @racmaint
copy racmaint.exe f:\rac\4.0g\racware
del racmaint.obj
del racmaint.exe
cd..\src

