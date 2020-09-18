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
$if not set xcel_datadir $set xcel_datadir "..\xcel_data\"
$setglobal datadir %datadir%

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
*    SET OF HOUSEHOLDS
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

SET     hh      'household groups' 
        r       'regions'
        i       'sectors';



* ############################################################################ * GDXIN

* ------ 18.07.2016
$GDXIN %datadir%%output%.GDX
*$LOAD pf0 vfm vafm voa vom
*$LOAD vfm vifm vdfm vom rto rtf

* ------ 14.04.2014


* ############################################################################ *
* ------ Parameter declaration for calculated parameters -----------------------

PARAMETER

* ------ From disaggregation output
         hh_consumption(r,i,hh)        household consumption per sector and region
         hh_income(r,*,hh)
* ------ for calculation
         total_cons(r,i)                     Sum over households to get total sectoral consumption
         hh_sector_share(r,i,hh)             share that each household consumes from each sector
         hh_total_consumption_share(r,hh)    share that each houshold consumes from total consumption 

         total_skl(r)
         total_usk(r)
         total_rkr(r)
         total_rkx_ele(r)
         total_tax_in(r)
         total_tax_out(r)
         total_savings(r)

         hh_skl_share(r,hh)
         hh_usk_share(r,hh)
         hh_rkr_share(r,hh)
         hh_rkx_ele_share(r,hh)
         hh_tax_in_share(r,hh)
         hh_tax_out_share(r,hh)
         hh_savings_share(r,hh)

* ------ DEFINE rounding parameter rd ------------------------------------------

         rd              Number of decimals for rounding cut-off         / 12 /
;

* ---------------------------------------------------------------------------- *

$call csv2gdx %xcel_datadir%consumption_diss_out.csv output=%datadir%%output%_consumption_diss.gdx id=hh_consumption fieldSep=semiColon decimalSep=Period index=1,2,3 useHeader=y value=4 trace=0
$ifE errorLevel<>0 $abort Problems reading distance.csv!
$gdxIn %datadir%%output%_consumption_diss.gdx
$load r = Dim1
$load i = Dim2
$load hh = Dim3

$load hh_consumption
$gdxIn

display hh_consumption;

$call csv2gdx %xcel_datadir%hh_income.csv output=%datadir%%output%_consumption_diss.gdx id=hh_income fieldSep=semiColon decimalSep=Period index=1,2,3 useHeader=y value=4 trace=0
$ifE errorLevel<>0 $abort Problems reading distance.csv!
$gdxIn %datadir%%output%_consumption_diss.gdx

$load hh_income
$gdxIn


*hh_consumption(r,i,hh) = hh_consumption(r,i,hh)$round(hh_consumption(r,i,hh),5);

total_cons(r,i) = sum(hh, hh_consumption(r,i,hh));
hh_sector_share(r,i,hh)$(total_cons(r,i)) = hh_consumption(r,i,hh)/total_cons(r,i);
*hh_sector_share(r,i,hh)$(NOT hh_consumption(r,i,hh)) = 0;
hh_total_consumption_share(r,hh) = sum(i,hh_consumption(r,i,hh))/sum(i,total_cons(r,i));

total_skl(r) = sum(hh, hh_income(r,"skl_labor",hh));
total_usk(r) = sum(hh, hh_income(r,"usk_labor",hh));
total_rkr(r) = sum(hh, hh_income(r,"rkr",hh));
total_rkx_ele(r) = sum(hh, hh_income(r,"rkx_ele",hh));
total_tax_in(r) = sum(hh, hh_income(r,"taxes income",hh));
total_tax_out(r) = sum(hh, hh_income(r,"tax on income",hh));
total_savings(r) = sum(hh, hh_income(r,"savings",hh));

hh_skl_share(r,hh) = hh_income(r,"skl_labor",hh)/total_skl(r);
hh_usk_share(r,hh) = hh_income(r,"usk_labor",hh)/total_usk(r);
hh_rkr_share(r,hh) = hh_income(r,"rkr",hh)/total_rkr(r);
hh_rkx_ele_share(r,hh) = hh_income(r,"rkx_ele",hh)/total_rkx_ele(r);
hh_tax_in_share(r,hh) = hh_income(r,"taxes income",hh)/total_tax_in(r);
hh_tax_out_share(r,hh) = hh_income(r,"tax on income",hh)/total_tax_out(r);
hh_savings_share(r,hh) = hh_income(r,"savings",hh)/total_savings(r);

*loop((i,r,hh),
*   if(hh_consumption(r,i,hh) < 0.00001),
*      hh_hh_consumption(r,i,hh) = 0)

$ontext
hh_skl_share("EUS",hh) = 0.2;
hh_usk_share("EUS",hh) =  0.2;
hh_rkr_share("EUS",hh) =  0.2;
hh_rkx_ele_share("EUS",hh) =  0.2;
hh_tax_in_share("EUS",hh) =  0.2;
hh_tax_out_share("EUS",hh) =  0.2;
hh_savings_share("EUS",hh) =  0.2;
hh_total_consumption_share("EUS",hh) = 0.2;
hh_sector_share("EUS",i,hh) =0.2;
$offtext

Execute_Unload '%datadir%%output%_consumption_diss', hh_consumption, total_cons, hh_sector_share, hh_total_consumption_share,
 hh_income, hh_skl_share, hh_usk_share, hh_rkr_share, hh_rkx_ele_share, hh_tax_in_share, hh_tax_out_share, hh_savings_share;
*Execute_Unload '%datadir%%output%_consumption_diss', hh_tax_in_share, hh_tax_out_share, hh_savings_share;


*display hh_consumption;
$exit