$if set noextrap $exit

$set r %1
$label start
$shift
$if "%1"=="" $exit
$set tbl %1

*	Fill in 2004 data based on levels in 2006 and 2007:

*	1+g  = (x2007/x2006)

*	x2004 = x2006 / (1+g)**2 = x2006 * (x2006/x2007)**2;

loop(%r%$(%tbl%(%r%,"2007")>0),
	%tbl%(%r%,"2004") = %tbl%(%r%,"2006") * (%tbl%(%r%,"2006")/%tbl%(%r%,"2007"))**2;
);
loop(%r%$(%tbl%(%r%,"2007")=0),
	if (%tbl%(%r%,"2010")>0,
	  %tbl%(%r%,"2004") = %tbl%(%r%,"2006") * (%tbl%(%r%,"2006")/%tbl%(%r%,"2010"))**(2/4);
));


*	Beyond 2035 use geometric extrapolation

loop(%r%$(%tbl%(%r%,"2035")*%tbl%(%r%,"2030")>0),
	%tbl%(%r%,"2040") = %tbl%(%r%,"2035") * %tbl%(%r%,"2035")/%tbl%(%r%,"2030");
	%tbl%(%r%,"2045") = %tbl%(%r%,"2040") * %tbl%(%r%,"2035")/%tbl%(%r%,"2030");
	%tbl%(%r%,"2050") = %tbl%(%r%,"2045") * %tbl%(%r%,"2035")/%tbl%(%r%,"2030");
);
$goto start
