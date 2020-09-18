$if set noextrap $exit

$set r %1
$label start
$shift
$if "%1"=="" $exit
$set tbl %1

*	We are unable to use annual growth rate from 2007-2015 to impute 2004.

*	We therefore rely on the data provided in earlier releases:

*	Beyond 2035 use geometric extrapolation

loop(%r%$(%tbl%(%r%,"2035")*%tbl%(%r%,"2030")>0),
	%tbl%(%r%,"2040") = %tbl%(%r%,"2035") * %tbl%(%r%,"2035")/%tbl%(%r%,"2030");
	%tbl%(%r%,"2045") = %tbl%(%r%,"2040") * %tbl%(%r%,"2035")/%tbl%(%r%,"2030");
	%tbl%(%r%,"2050") = %tbl%(%r%,"2045") * %tbl%(%r%,"2035")/%tbl%(%r%,"2030");
);
$goto start
