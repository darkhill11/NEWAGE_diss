$TITLE merge GDX files

$if not set resultsdir   $set resultsdir "..\results\"

* ------ Read results from GDX-files

$gdxin  %resultsdir%reference.gdx
$gdxin  %resultsdir%covid.gdx

* ------ Create merged.gdx
Execute 'gdxmerge %resultsdir%reference.gdx %resultsdir%covid.gdx';

* ------ Create report_pivot_merged.xlsx for NEWAGE-Version WITH bottom-up household energy demand
Execute 'gdxxrw.exe i=merged.gdx  o=%resultsdir%report_pivot_merged.xlsx  epsout=0  @dumppar1.txt'

* ------ Create report_pivot_merged.xlsx for NEWAGE-Version WITHOUT bottom-up household energy demand
*Execute 'gdxxrw.exe i=merged.gdx  o=%resultsdir%report_pivot_merged.xlsx  epsout=0  @dumppar_HH.txt'


* ------ Create report_pivot_merged.xlsx for NEWAGE-Version WITHOUT bottom-up household energy demand
*Execute 'gdxxrw.exe i=merged.gdx  o=%resultsdir%report_pivot_merged.xlsx  epsout=0  @dumppar_extra.txt'