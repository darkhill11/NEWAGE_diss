$TITLE  NEWAGE 18x19x4 -- GTAP9 -- Calibration of Electricity Generation

$onempty

OPTION decimals = 5;

* eolcom // does not work correctly with xldump? (ele_calib_10.xls)
$oneolcom
$eolcom //


* ------ Read GTAP9 data set without BAW


$if not set resultsdir   $set resultsdir "..\results\"

$if not set gdx_file	 $set gdx_file "results_no_VAT.gdx"

$if not set gdx_out_file	 $set gdx_out_file "extra_no_VAT.gdx"


set
	yr(*)	years
	sec(*)	sectors
	r(*)	regions
	hh(*)	households;

parameter
	pco2_deu_yr(sec,yr)
	vd_pinv_hh_yr(hh,r,yr)
	pinv_yr(*,yr)
	trdblnc_yr(*,*,yr)
	vd_pc_gov_yr(r,yr)
	gdpreal_yr(*,yr)
	abs_sector_hh_yr(r,*,hh,yr)
	gdpreal_disag_yr(*,*,yr)

$GDXIN %resultsdir%%gdx_file%
$load yr = yr
$load sec = sec
$load r = r
$load hh = hh

$load pco2_deu_yr = pco2_deu_yr
$load vd_pinv_hh_yr = vd_pinv_hh_yr
$load pinv_yr = pinv_yr
$load trdblnc_yr = trdblnc_yr
$load vd_pc_gov_yr = vd_pc_gov_yr
$load gdpreal_yr = gdpreal_yr
$load abs_sector_hh_yr = abs_sector_hh_yr
$GDXIN

display hh,r;

set          HH_DISAG(R)     countries with household disaggregation
                         / DEU, FRA, ITA, UKI, BNL, ESP, POL, EUN, EUS /;

gdpreal_disag_yr("cons_HH",r,yr)$HH_DISAG(r) = sum(hh, abs_sector_hh_yr(r,"total",hh,yr));
gdpreal_disag_yr("inv",r,yr)$HH_DISAG(r) = sum(hh, vd_pinv_hh_yr(hh,r,yr) * pinv_yr(r,yr));
gdpreal_disag_yr("trade",r,yr)$HH_DISAG(r) =  trdblnc_yr(r,"total",yr);
gdpreal_disag_yr("cons_gov",r,yr)$HH_DISAG(r) = gdpreal_yr(r,yr) - gdpreal_disag_yr("cons_HH",r,yr) - gdpreal_disag_yr("inv",r,yr) - gdpreal_disag_yr("trade",r,yr);
gdpreal_disag_yr("total",r,yr)$HH_DISAG(r) = gdpreal_yr(r,yr);

display gdpreal_disag_yr;

execute_unload  '%resultsdir%%gdx_out_file%' , gdpreal_disag_yr ;



Execute  'gdxxrw.exe i=%resultsdir%%gdx_file% o=%resultsdir%report_pivot.xlsx epsout=0 @dumppar_ohneDiss.txt'
Execute  'gdxxrw.exe i=%resultsdir%%gdx_out_file% o=%resultsdir%report_pivot.xlsx epsout=0 @dumppar_extra.txt'
$exit