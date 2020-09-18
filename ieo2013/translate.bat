@echo off

if not exist lst mkdir lst

title	READALL IEO data 
gams readall
pause


title	READPOP UN population data
gams readpop

pause

:ieogtap

title	Merge the data and match to GTAP.
gams ieogtap --byr=2011
gams ieogtap --byr=2007
gams ieogtap --byr=2004
pause


:	Extrapolate data to 2100 (in 5 year steps)
gams extrapgtap.gms --byr=2011
gams extrapgtap.gms --byr=2007 	 
gams extrapgtap.gms --byr=2004 	 
pause


