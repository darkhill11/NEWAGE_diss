$TITLE merge GDX files

$if not set target   $set target "210221/merged_results.gdx"
* ------ Read results from GDX-files

$onEcho > howToWrite.txt
par=gdpreal_disag_yr	rng=gdpreal_disag_yr!B2 	cDim=0
par=pco2_deu_yr 		rng=pco2_deu_yr!B2 			cDim=0
par=pco2_yr		 		rng=pco2_yr!B2	 			cDim=0
par=pco2_ets_yr	 		rng=pco2_ets_yr!B2 			cDim=0
par=py_yr		 		rng=py_yr!B2	 			cDim=0
par=gva_yr		 		rng=gva_yr!B2	 			cDim=0
par=gva_real_yr	 		rng=gva_real_yr!B2 			cDim=0
par=pco2_netsr_yr		rng=pco2_netsr_yr!B2 		cDim=0
par=pco2w_yr			rng=pco2w_yr!B2 			cDim=0
par=prod_accounts		rng=prod_accounts!B2		cDim=0
par=elex_accounts		rng=elex_accounts!B2		cDim=0
par=elen_accounts		rng=elen_accounts!B2		cDim=0
par=armi_accounts		rng=armi_accounts!B2		cDim=0
par=cons_accounts		rng=cons_accounts!B2		cDim=0
par=taxes_region		rng=taxes_region!B2			cDim=0
par=cons_hh_accounts	rng=cons_hh_accounts!B2		cDim=0
par=hh_income			rng=hh_income!B2			cDim=0
par=no_lab_summary		rng=no_lab_summary!B2		cDim=0
par=no_vat_summary		rng=no_vat_summary!B2		cDim=0
par=share_sector_hh_yr	rng=share_sector_hh_yr!B2	cDim=0
par=abs_sector_hh_yr	rng=abs_sector_hh_yr!B2		cDim=0
par=RA_hh_par_yr		rng=RA_hh_par_yr!B2			cDim=0
par=share_income_hh		rng=share_income_hh!B2		cDim=0
par=vc_hh_pc_yr			rng=vc_hh_pc_yr!B2			cDim=0
par=gini				rng=gini!B2					cDim=0
par=emplmtno_yr			rng=emplmtno_yr!b2			cDim=0
par=share_co2_gdp		rng=share_co2_gdp!B2		cDim=0
par=elecontwh2_yr		rng=elecontwh2_yr!B2		cDim=0		

$offEcho


* ------ Create report_pivot_merged.xlsx for NEWAGE-Version WITH bottom-up household energy demand
Execute 'gdxxrw.exe i=%target%  o=report_pivot_merged.xlsx  epsout=0  @howToWrite.txt'

$ontext
SETS
	r  /DEU, FRA, ITA, POL, UKI, ESP, BNL, EUN, EUS, USA, OEC, BRZ, RUS, IND, CHI, RSA, OPA, ROW/,
	hh /hh1, hh2, hh3, hh4, hh5/,
	yr /2011, 2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050/,
	sce /diss_ref, diss_BAU, diss_CAP, diss_VAT, diss_LAB/
	eu28(r)	/DEU, FRA, ITA, POL, UKI, ESP, BNL, EUN, EUS/;

PARAMETER
	RA_hh_par_yr(*,*,*,*,*)
	gini(*,*,*,r,yr);



$GDXIN %target%
$LOAD RA_hh_par_yr
$GDXIN

alias (hh,hh_);

display RA_hh_par_yr, hh_;

gini(sce,"net", "net_all", r, yr) = sum( hh, RA_hh_par_yr(sce,"net_income", r, hh, yr));
gini(sce,"net", hh, r, yr)$RA_hh_par_yr(sce,"net_income", r, hh, yr) = sum( hh_$(ord(hh_) <= ord(hh)), RA_hh_par_yr(sce,"net_income", r, hh_, yr)) / gini(sce,"net", "net_all", r, yr);

gini(sce,"net_area", hh, r, yr)$gini(sce,"net", hh, r, yr) = ((gini(sce,"net", hh, r, yr) -( sum(hh_$((ord(hh_) = (ord(hh) - 1)) AND (ord(hh) > 0)), gini(sce,"net", hh_, r, yr)) + 0) ) /10) +( sum(hh_$((ord(hh_) = ord(hh) - 1) AND (ord(hh) > 0)), gini(sce,"net", hh_, r, yr))  * 0.2);

gini(sce,"net", "gini", r, yr) = 2 * (0.5 - sum(hh_,gini(sce,"net_area", hh_, r, yr)));


gini(sce,"total", "total_all", r, yr) = sum( hh, RA_hh_par_yr(sce,"total_income", r, hh, yr));
gini(sce,"total", hh, r, yr)$RA_hh_par_yr(sce,"total_income", r, hh, yr) = sum( hh_$(ord(hh_) <= ord(hh)), RA_hh_par_yr(sce,"total_income", r, hh_, yr)) / gini(sce,"total", "total_all", r, yr);

gini(sce,"total_area", hh, r, yr)$gini(sce,"total", hh, r, yr) = ((gini(sce,"total", hh, r, yr) -( sum(hh_$((ord(hh_) = (ord(hh) - 1)) AND (ord(hh) > 0)), gini(sce,"total", hh_, r, yr)) + 0) ) /10) +( sum(hh_$((ord(hh_) = ord(hh) - 1) AND (ord(hh) > 0)), gini(sce,"total", hh_, r, yr))  * 0.2);

gini(sce,"total", "gini", r, yr) = 2 * (0.5 - sum(hh_,gini(sce,"total_area", hh_, r, yr)));

display gini;
$offtext
*gini(sce,"net", hh, r, yr) = sum( hh_$(ord(hh_) <= ord(hh)), RA_hh_par_yr(sce,"net_income", r, hh_, yr)) / sum( hh_, RA_hh_par_yr(sce,"net_income", r, hh_, yr));

*Execute_unload   "201201/gini.gdx" 

*Execute 'gdxxrw.exe i=201201/gini.gdx  o=report_pivot_merged.xlsx  epsout=0  par=gini	rng=gini!B2	cDim=0'