$TITLE merge GDX files

$if not set resultsdir   $set resultsdir "..\results\diss\test\"

* ------ Read results from GDX-files

$gdxin  %resultsdir%ref.gdx
$gdxin  %resultsdir%test.gdx
*$gdxin  %resultsdir%diss_CAP.gdx
*$gdxin  %resultsdir%diss_VAT.gdx
*$gdxin  %resultsdir%diss_LAB.gdx
*$gdxin  %resultsdir%diss_inv_pay.gdx

* ------ Create merged.gdx
Execute 'gdxmerge %resultsdir%ref.gdx %resultsdir%test.gdx  output=%resultsdir%merged.gdx';

* ------ Create report_pivot_merged.xlsx for NEWAGE-Version WITH bottom-up household energy demand
*Execute 'gdxxrw.exe i=merged.gdx  o=%resultsdir%report_pivot_merged.xlsx  epsout=0  @dumppar1.txt'

* ------ Create report_pivot_merged.xlsx for NEWAGE-Version WITHOUT bottom-up household energy demand
*Execute 'gdxxrw.exe i=merged.gdx  o=%resultsdir%report_pivot_merged.xlsx  epsout=0  @dumppar_HH.txt'


* ------ Create report_pivot_merged.xlsx for NEWAGE-Version WITHOUT bottom-up household energy demand
*Execute 'gdxxrw.exe i=merged.gdx  o=%resultsdir%report_pivot_merged.xlsx  epsout=0  @dumppar_extra.txt'