$if set noextrap $exit

$set r %1
$label start
$shift
$if "%1"=="" $exit
$set tbl %1

*	Assuming uniform annual growth rate to impute 2004 from provided values in 2005 and 2006:

loop(%r%$(%tbl%(%r%,"2006")>0),
	%tbl%(%r%,"2004") = %tbl%(%r%,"2005") * %tbl%(%r%,"2005") / %tbl%(%r%,"2006");
);
*	Beyond 2040 use geometric extrapolation
parameter year(y)	Year values;

year(y) = 1990 + ord(y)-1;

loop(y$((year(y) ge 2040) and (year(y) lt 2050)),
  loop(%r%$%tbl%(%r%,y-1),
	%tbl%(%r%,y+1) = %tbl%(%r%,y) * %tbl%(%r%,y)/%tbl%(%r%,y-1);
      );
     );
display year, %tbl%; 


$goto start
