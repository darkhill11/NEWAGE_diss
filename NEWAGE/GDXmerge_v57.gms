$TITLE merge GDX files

$if not set resultsdir   $set resultsdir "..\results\diss\210112\"

* ------ Read results from GDX-files

$gdxin  %resultsdir%diss_ref.gdx
$gdxin  %resultsdir%diss_BAU.gdx
$gdxin  %resultsdir%diss_CAP.gdx
$gdxin  %resultsdir%diss_VAT.gdx
$gdxin  %resultsdir%diss_LAB.gdx
$gdxin  %resultsdir%diss_inv_pay.gdx

* ------ Create merged.gdx
Execute 'gdxmerge %resultsdir%diss_ref.gdx %resultsdir%diss_BAU.gdx %resultsdir%diss_CAP.gdx %resultsdir%diss_VAT.gdx %resultsdir%diss_LAB.gdx %resultsdir%diss_inv_pay.gdx output=%resultsdir%merged.gdx';

* ------ Create report_pivot_merged.xlsx for NEWAGE-Version WITH bottom-up household energy demand
*Execute 'gdxxrw.exe i=merged.gdx  o=%resultsdir%report_pivot_merged.xlsx  epsout=0  @dumppar1.txt'

* ------ Create report_pivot_merged.xlsx for NEWAGE-Version WITHOUT bottom-up household energy demand
*Execute 'gdxxrw.exe i=merged.gdx  o=%resultsdir%report_pivot_merged.xlsx  epsout=0  @dumppar_HH.txt'


* ------ Create report_pivot_merged.xlsx for NEWAGE-Version WITHOUT bottom-up household energy demand
*Execute 'gdxxrw.exe i=merged.gdx  o=%resultsdir%report_pivot_merged.xlsx  epsout=0  @dumppar_extra.txt'