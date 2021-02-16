$TITLE merge GDX files

$if not set resultsdir   $set resultsdir "..\results\diss\210214\"

* ------ Read results from GDX-files

$gdxin  %resultsdir%ref.gdx
$gdxin  %resultsdir%bau.gdx
$gdxin  %resultsdir%vat.gdx
$gdxin  %resultsdir%lab.gdx
$gdxin  %resultsdir%cap.gdx
$gdxin  %resultsdir%inv.gdx

* ------ Create merged.gdx
Execute 'gdxmerge %resultsdir%ref.gdx %resultsdir%bau.gdx  %resultsdir%vat.gdx  %resultsdir%lab.gdx  %resultsdir%cap.gdx  %resultsdir%inv.gdx  output=%resultsdir%merged_results.gdx';

* ------ Create report_pivot_merged.xlsx for NEWAGE-Version WITH bottom-up household energy demand
*Execute 'gdxxrw.exe i=merged.gdx  o=%resultsdir%report_pivot_merged.xlsx  epsout=0  @dumppar1.txt'

* ------ Create report_pivot_merged.xlsx for NEWAGE-Version WITHOUT bottom-up household energy demand
*Execute 'gdxxrw.exe i=merged.gdx  o=%resultsdir%report_pivot_merged.xlsx  epsout=0  @dumppar_HH.txt'


* ------ Create report_pivot_merged.xlsx for NEWAGE-Version WITHOUT bottom-up household energy demand
*Execute 'gdxxrw.exe i=merged.gdx  o=%resultsdir%report_pivot_merged.xlsx  epsout=0  @dumppar_extra.txt'