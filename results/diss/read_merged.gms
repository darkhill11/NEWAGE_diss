$TITLE merge GDX files

$if not set target   $set target "201201/merged.gdx"
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
*par=prod_accounts		rng=prod_accounts!B2		cDim=0
*par=elex_accounts		rng=elex_accounts!B2		cDim=0
*par=elen_accounts		rng=elen_accounts!B2		cDim=0
*par=armi_accounts		rng=armi_accounts!B2		cDim=0
*par=cons_accounts		rng=cons_accounts!B2		cDim=0
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
$offEcho


* ------ Create report_pivot_merged.xlsx for NEWAGE-Version WITH bottom-up household energy demand
Execute 'gdxxrw.exe i=%target%  o=report_pivot_merged.xlsx  epsout=0  @howToWrite.txt'

