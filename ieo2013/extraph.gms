$if set noextrap $exit

$set r %1
$label start
$shift
$if "%1"=="" $exit
$set tbl %1

*	We cannot infer 2004 data based on levels in 2007 and 2015.
*	These values must be adopted from the 2009 data.

*	Beyond 2035 use geometric extrapolation

loop(%r%$(%tbl%(%r%,"2035")*%tbl%(%r%,"2030")>0),
	%tbl%(%r%,"2040") = %tbl%(%r%,"2035") * %tbl%(%r%,"2035")/%tbl%(%r%,"2030");
	%tbl%(%r%,"2045") = %tbl%(%r%,"2040") * %tbl%(%r%,"2035")/%tbl%(%r%,"2030");
	%tbl%(%r%,"2050") = %tbl%(%r%,"2045") * %tbl%(%r%,"2035")/%tbl%(%r%,"2030");
);
$goto start
