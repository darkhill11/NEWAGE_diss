$TITLE  NEWAGE 18x18x4 -- GTAP9 -- Calibration of Electricity Generation

$onempty

OPTION decimals = 5;

* eolcom // does not work correctly with xldump? (ele_calib_10.xls)
$oneolcom
$eolcom //


* ------ Read GTAP9 data set without BAW

* ------ source must match target in gtapaggr.gms
$if not set source       $set source     18x18x4_gtap9
$if not set output       $set output     %source%

* ------ 15.04.2014
$if not set ds           $set ds         %output%
$if not set yr           $set yr         11
*$if not set yr           $set yr         11

$if not set datadir      $set datadir "..\data%yr%\"

$setglobal datadir %datadir%

$if not set xcel_datadir $set xcel_datadir "..\xcel_data\"

*$include ..\build\gtap9data_newage

*display r, i, j, g, f, vfm, vdfm, vom, vifm, vxmd, vst, vtwr;
*$exit

*alias(r,rr), (s,ss), (i,ii), (j,jj), (g,gg), (f,ff);

*SET      D       Submarkets for which import shares are differentiated /
*         G       Public,
*         C       Private Consumption,
*         I       Intermediate input
*         /;

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*    SET OF REGIONS AND SECTORS
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

SET      
        r       'regions' / DEU, FRA, ITA, POL, UKI, ESP, BNL, EUN, EUS, USA, OEC, BRZ, RUS, IND, CHI, RSA, OPA, ROW /
        i       'sectors' / CHM, PPP, IRS, NFM, NMM, FOT, MVH, MAC, ROI, AGR, BUI, SER, TRN, ELE, OIL, CRU, COL, GAS /;




* ############################################################################ *
* ------ Parameter declaration for new tax for consumers -----------------------

PARAMETER

         tp_vat(r,i)    tax for consumers witout VAT
;

* ---------------------------------------------------------------------------- *

$libinclude    xlimport    tp_vat   %xcel_datadir%vat_for_newage.xlsx   read_NEWAGE!a1:r10


display tp_vat;



Execute_Unload '%datadir%%output%_tp_no_vat', tp_vat;


*display hh_consumption;
$exit