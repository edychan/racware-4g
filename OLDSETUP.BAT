@echo off
subst f: c:\
subst i: c:\
subst j: c:\
SET CLIPPER=V16;F55;E0;R100
set racdrv=f:
set racsid=stn00
set racpth=\rac\4.0g\
cd j:%racpth%racware\dbf
cd i:%racpth%racware\stn\stn00
f:
cd %racpth%racware

