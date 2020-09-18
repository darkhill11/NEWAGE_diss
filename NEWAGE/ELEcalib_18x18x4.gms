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

$include ..\build\gtap9data_newage

display r, i, j, g, f, vfm, vdfm, vom, vifm, vxmd, vst, vtwr;
*$exit

alias(r,rr), (s,ss), (i,ii), (j,jj), (g,gg), (f,ff);

SET      D       Submarkets for which import shares are differentiated /
         G       Public,
         C       Private Consumption,
         I       Intermediate input
         /;

SET      gen     Stromerzeugungsarten /
                 bNUC, bBC, bBIO, bGEO, bHYDRO, bHC, bGAS, bOIL, bCCS,
                 mHC, mGAS, mWIND, mSOLAR, mOIL, mCCS,
                 pOIL, pGAS, pHYDRO
*                 HYDRO, SOLAR, WIND, BIO, GEO
                 / ;

ALIAS (i,j), (f,ff), (r,s), (r,rr), (gen, gengen);

display r, i, f, d, gen;

* ------ 14.04.2014
PARAMETER
         pf0(f,g,r)      Reference price of factor inputs
         vafm(i,g,r)     Intermediates - value of aggregate firms inputs at agents prices
         voa(g,r)        Intermediates - value of total output at input agents prices
;


* ############################################################################ * GDXIN

* ------ 18.07.2016
$GDXIN %datadir%%output%.GDX
*$LOAD pf0 vfm vafm voa vom
*$LOAD vfm vifm vdfm vom rto rtf

* ------ 14.04.2014
display vfm, vifm, vdfm, vom, rto, rtf;

pf0(f,g,r) = 1 + rtf(f,g,r);
voa(g,r) = vom(g,r) * (1 - rto(g,r));
vafm(i,g,r) = vdfm(i,g,r) + vifm(i,g,r) ;

display voa, pf0;

* ############################################################################ *
* ------ Parameter declaration for calculated parameters -----------------------

PARAMETER
* ------ Derived from GTAP data
         skld0(i,r)              Base year labor demand skilled by sector
         uskd0(i,r)              Base year labor demand unskilled by sector
         kd0(i,r)                Base year capital earnings by sector
         kd0_no_tax(i,r)         Base year capital earnings by sector without inlcuding taxes
         skld0_no_tax(i,r)       Base year labor demand skilled by sector without inlcuding taxes
         uskd0_no_tax(i,r)       Base year labor demand unskilled by sector without inlcuding taxes

* ------ Adjustment of GTAP data to 18 different electricity generation technologies
         vafm_input(i,*,r)       Extended parameter vafm value of aggregate firms inputs at market prices
         cap_input(*,r)          Extended parameter kd0 capital earnings
         usk_input(*,r)          Extended parameter uskd0 labor demand unskilled
         skl_input(*,r)          Extended parameter skld0 labor demand skilled
         lab_input(gen,r)        Sum of labor demand skilled and unskilled per generation technology
         vafm_ele(i,r)           Intermediate inputs in ELE

         cap_input_no_tax(*,r)          Extended parameter kd0 capital earnings without including taxes
         usk_input_no_tax(*,r)          Extended parameter uskd0 labor demand unskilled
         skl_input_no_tax(*,r)          Extended parameter skld0 labor demand skilled         

* ------ calibration for 2015 germany electricity generatio costs
         vafm_input_15(*,*)       Extended parameter vafm value of aggregate firms inputs at market prices
         cap_input_15(*)          Extended parameter kd0 capital earnings
         usk_input_15(*)          Extended parameter uskd0 labor demand unskilled
         skl_input_15(*)          Extended parameter skld0 labor demand skilled

* ------ Data read in from EXCEL
         ele_prod(*,*)           Electricity generation [TWh]
         ele_prod_costs(*,*)     Electricity generation (unit) costs [USD per MWh]
         costshr(*,*,*)          Input cost shares of generation technologies (sum to 1)

* ------ Further electricity generation parameters
         cost_gen(*,r)           Electricity generation (overall) costs per technology as of EXCEL data [Mio. USD2007]
         cost_gen_shr(*,r)       Cost shares of electricity generation technologies --> cost_gen(r) divided by cost_gen("total")
         cost_gen_gtap(*,r)      Electricity generation (overall) costs per technology applied for GTAP data voa("ele") [10 Bn. USD2004]

         cost_gen_input(r,*,*)   Derive input cost structure per technology [costshr*cost_gen_gtap] [10 Bn. USD2004]
         costshr_calib(*,*,r)    Calculates i- and f-inputs as a share of i- and f-input-sums (RK)
         costshr_calib2(r,gen,*) Equals costshr for technologies that produce electricity in the basee year (e.g. theres no DEU.bGEO)

* ------ 4.03.2013: before, cost_gen_shr was gen_cost_shr; and cost_gen_input was gen_scale

*         gen_cost(gen,r)         Electricity generation costs per technology via costshares
*         gen_cost_test(gen,r)    Test if cost_gen_tot and gen_cost are equal

* ------ Check parameters
         checkSKL                Check whether skl_input equals skld0("ele")
         checkUSK                Check whether usk_input equals uskd0("ele")
         checkCAP                Check whether cap_input equals kd0("ele")
         checkVAFM               Check whether vafm_input equals vafm("ele")
         checkGENSHR(r)          Check whether technology shares of electricity costs [cost_gen_shr] sum up to 100%
         checkCOSTSHR(r,gen)     Check whether input cost shares per technology [costshr] sum up to 100%
         checkCOSTGEN(r,gen)     Check whether total cost_gen_input equals cost_gen_gtap
         checkELEVOA(r)          Check whether voa("ele") equals calculated overal electricity costs
         checkCOSTSHRcal(*,*)    Check costshr_calib
         checkCOSTSHRcal2(*,*)   Check costshr_calib2 (row sums of costshr equal 1)
         checkCOSTSHRcal22(*,*,*) Check whether costshr_calib2 equals costshr in case of cost_gen
         checkGEN(gen,r)         Differences of own IEA-data (cost_gen) to GTAP data (cost_gen_gtap) [

         vafm_test(*,r)          not yet defined...

* ------ DEFINE rounding parameter rd ------------------------------------------

         rd              Number of decimals for rounding cut-off         / 12 /

         ele_dev_switch(*,*)     switch to choose which technology in with country will follow the TIMES PanEU development
;

* ---------------------------------------------------------------------------- *
* ------ Parameter definitions

skld0(i,r) = vfm("SKL",i,r)*pf0("SKL",i,r) ;
uskd0(i,r) = vfm("USK",i,r)*pf0("USK",i,r) ;
kd0(i,r)   = vfm("cap",i,r)*pf0("cap",i,r) ;

* ----- parameter for disaggregation of labor and capital tax
kd0_no_tax(i,r)     = vfm("cap",i,r) ;
skld0_no_tax(i,r)   = vfm("skl",i,r) ;
uskd0_no_tax(i,r)   = vfm("usk",i,r) ;

display skld0, uskd0, kd0;

* ############################################################################## LIBINCLUDE
* ------ READ in Electricity Generation Data (quantities, costs, shares) ------- Daten müssen auf 26 Regionen angepasst werden!!!!!

* ------ GTAP 7: Electricity generation quantities in TWh (region specific, with CCS)
*$libinclude xlimport     ele_prod        ele_data_10.xls                 ee_prod_10_2004!A1:K19
* ------ GTAP 8: Gross Electricity Production, TWh, 2007 (IEA Data excl. peat, gases and waste)
*$libinclude xlimport     ele_prod        ele_prod.xlsx           ele_prod!A2:AA20                # ohne BAW
*$libinclude xlimport     ele_prod        %xcel_datadir%ele_prod.xlsx           ele_prod_bw!A2:AB20              // 26x18x4 mit BAW
* ------ 23.04.2014
*$libinclude xlimport     ele_prod        %xcel_datadir%ele_prod.xlsx           ele_prod_bw!A2:S20               // 17x18x4 mit BAW
* ------ 3.02.2016 BAW entfernen
$libinclude xlimport     ele_prod        %xcel_datadir%ele_prod.xlsx           ele_prod!A2:S20                  // 18x18x4 for REEEM

* ------ GTAP 7: Electricity generation costs (region specific, with IGCC)
*$libinclude xlimport     ele_prod_costs  av_ele_cost_10reg.xls           e_cost_10!A1:K19
* ------ GTAP 7: RK regionenspezifische Daten mit angepassten Kosten für Solar in allen und für Nuc in NEU
*$LIBINCLUDE XLIMPORT ELE_PROD_COSTS   AV_ELE_COST_10REG.XLS              E_COST_10!A26:K44
* ------ GTAP 8: Electricity generation costs [DUMMY DATA]
*$libinclude xlimport     ele_prod_costs  ele_prod_costs.xlsx     ele_prod_costs!A2:AA20          # ohne BAW
*$libinclude xlimport     ele_prod_costs  %xcel_datadir%ele_prod_costs.xlsx     ele_prod_costs_bw!A2:AB20        // 26x18x4 mit BAW
* ------ 23.04.2014
*$libinclude xlimport     ele_prod_costs  %xcel_datadir%ele_prod_costs.xlsx     ele_prod_costs_bw!A2:S20         // 17x18x4 mit BAW
* ------ 3.02.2016 BAW entfernen
$libinclude xlimport     ele_prod_costs  %xcel_datadir%ele_prod_costs.xlsx     ele_prod_costs!A2:S20         // 18x18x4 for REEEM

$libinclude xlimport  skl_input_15        %xcel_datadir%ele_prod_costs.xlsx            values_2015_ENAVI!B3:R4                // 18x18x4 for ENAVI
$libinclude xlimport  usk_input_15        %xcel_datadir%ele_prod_costs.xlsx            values_2015_ENAVI!B8:R9                // 18x18x4 for ENAVI
$libinclude xlimport  vafm_input_15       %xcel_datadir%ele_prod_costs.xlsx            values_2015_ENAVI!B12:R30                // 18x18x4 for ENAVI
$libinclude xlimport  cap_input_15        %xcel_datadir%ele_prod_costs.xlsx            values_2015_ENAVI!B32:S33                // 18x18x4 for ENAVI

* ------ GTAP 7: Input cost shares (region specific)
*$libinclude xlimport     costshr         av_ele_costshare_10reg.xls      cshr!A1:P161
* ------ 4.03.2013 Corrected Excel sheet for column names (BUI for BUIL, IRS for I_S and ROI for Y)
* ------ GTAP 8: Input cost shares
* ------ 17.06.2013 costshr.xlsx angepasst und um Industrien (NFM, NMM, MVH, FOT, ROI, DWE) erweitert
*$libinclude xlimport     costshr         costshr.xlsx            costshr!A3:W471
*$libinclude xlimport     costshr         %xcel_datadir%costshr.xlsx            costshr_bw!A3:W489                // 26x18x4 mit BAW
* ------ 23.04.2014
*$libinclude xlimport     costshr         %xcel_datadir%costshr.xlsx            costshr_bw!A3:V327                // 17x18x4 mit BAW
* ------ 3.02.2016 BAW entfernen
$libinclude xlimport     costshr         %xcel_datadir%costshr.xlsx            costshr!A3:V327                // 18x18x4 for REEEM
display ele_prod, ele_prod_costs, costshr;

$libinclude      xlimport        ele_dev_switch                %xcel_datadir%ele_dev_switch.xlsx             ele_dev_switch!A1:j19 

* ##############################################################################


* ------ PARAMETER DEFINITIONS on electricity generation costs and shares ------

* ------ Electricity generation costs [EXCEL: ele_prod, ele_prod_costs]
*cost_gen(gen,r)          = (ele_prod_costs(gen,r) / 10000) * ele_prod(gen,r) ;

* ------ 15.04.2014      let's say Mrd. USD/MWh
* -----> ele_prod_costs is in [USD/MWh] + ele_prod is in [TWh]
* -----> multiply ele_prod by 1,000,000 for having MWh and then divide by 1,000,000,000 for having Bn.USD/MWh --> divide by 1,000!
cost_gen(gen,r)          = ele_prod_costs(gen,r) * ele_prod(gen,r) / 1000; display cost_gen;

* ------ 23.08.2018 test times
cost_gen(gen,r)$(ele_dev_switch(gen,r) and NOT ele_prod(gen,r)) = ele_prod_costs(gen,r)/ 1000;

* ------ 4.03.2013: Here, cost_gen is in 10 Mio. USD2004, why?
* ------ Multiply by 1000 [for $/TWh] and divide by 10.000.000.000 [for 10 bn. USD-2004] --> effectively divide by 10.000.000
*cost_gen(gen,r)          = ele_prod_costs(gen,r) * ele_prod(gen,r) / 10000000 ;
* ------ The unit doesn't matter here, because only relative numbers (gen divided by total) will be used subsequently
cost_gen("total",r)      = sum(gen, cost_gen(gen,r)) ;

* ------ Electricity generation technology shares
cost_gen_shr(gen,r)$cost_gen("total",r) = cost_gen(gen,r) / cost_gen("total",r) ;
cost_gen_shr("total",r)  = sum(gen, cost_gen_shr(gen,r)) ;
* ------ Check whether technology shares of electricity costs [cost_gen_shr] sum up to 100%
checkGENSHR(r)           = round( cost_gen_shr("total",r) - 1, rd);

* ------ Apply calculated technology shares to GTAP data to obtain overall generation costs per technology [10 Bn. USD2004]


* ------ ORIGINAL:                                                               // CHANGED 24.07.2013
cost_gen_gtap(gen,r)     = cost_gen_shr(gen,r) * VOA("ele",r) ;
* ------ 24.07.2013
*cost_gen_gtap(gen,r)     = cost_gen_shr(gen,r) * VOM("ele",r) ;

cost_gen_gtap("total",r) = sum(gen, cost_gen_gtap(gen,r)) ;  display cost_gen_gtap;

* ------ 24.07.2013
parameter chk; chk(r)= sum(gen, cost_gen_shr(gen,r)); display chk;
*parameter cost_gen_gtap2(*,r);
*cost_gen_gtap2(gen,r)     = cost_gen_shr(gen,r) * VOM("ele",r) ;
*cost_gen_gtap2("total",r) = sum(gen, cost_gen_gtap2(gen,r)) ; display cost_gen_gtap2;



* ------ Adjust labour input cost shares per technology [EXCEL: costshr]
costshr(r,gen,"skl")     = costshr(r,gen,"lab") * (vfm("skl","ele",r) / (vfm("skl","ele",r) + vfm("usk","ele",r)));
costshr(r,gen,"usk")     = costshr(r,gen,"lab") * (vfm("usk","ele",r) / (vfm("skl","ele",r) + vfm("usk","ele",r)));
costshr(r,gen,"lab")     = costshr(r,gen,"lab") - costshr(r,gen,"skl") - costshr(r,gen,"usk") ;
costshr(r,gen,"total")   = sum(i, costshr(r,gen,i)) + sum(f, costshr(r,gen,f)) ;
* ------ Check whether input cost shares per technology [costshr] sum up to 100%
checkCOSTSHR(r,gen)$costshr(r,gen,"total") = round( costshr(r,gen,"total") - 1, rd) ;

* ------ Derive input costs per technology [costshr*cost_gen_gtap] [10 Bn. USD2004]
* ------ 14.05.2014
cost_gen_input(r,i,gen)          = costshr(r,gen,i)     * cost_gen_gtap(gen,r) ;
*cost_gen_input(r,i,gen)          = costshr(r,i,gen)     * cost_gen_gtap(gen,r) ;

cost_gen_input(r,"cap",gen)      = costshr(r,gen,"cap") * cost_gen_gtap(gen,r) ;
cost_gen_input(r,"skl",gen)      = costshr(r,gen,"skl") * cost_gen_gtap(gen,r) ;
cost_gen_input(r,"usk",gen)      = costshr(r,gen,"usk") * cost_gen_gtap(gen,r) ;
cost_gen_input(r,"total",gen)    = sum(i,   cost_gen_input(r,i,gen)) + sum(f, cost_gen_input(r,f,gen)) ;
* ------ Check whether sum of inputs (total cost_gen_input) equals overall electricity costs (cost_gen_gtap)
checkCOSTGEN(r,gen)              = round(   cost_gen_input(r,"total",gen) - cost_gen_gtap(gen,r), rd) ;
cost_gen_input(r,"cap","total")  = sum(gen, cost_gen_input(r,"cap",gen)) ;
cost_gen_input(r,"skl","total")  = sum(gen, cost_gen_input(r,"skl",gen)) ;
cost_gen_input(r,"usk","total")  = sum(gen, cost_gen_input(r,"usk",gen)) ;
cost_gen_input(r,i,"total")      = sum(gen, cost_gen_input(r,i,gen)) ;
cost_gen_input(r,"total","total")= sum(i,   cost_gen_input(r,i,"total")) + sum(f, cost_gen_input(r,f,"total")) ;
* ------  Check whether voa("ele") equals calculated overal electricity costs
checkELEVOA(r)                   = round(   cost_gen_input(r,"total","total") - voa("ele",r), rd) ;
* ------ 24.07.2013
*checkELEVOA(r)                   = round(   cost_gen_input(r,"total","total") - vom("ele",r), rd) ;


* ------ Specify further paramters
costshr_calib(i,gen,r)$cost_gen_input(r,i,"total") = cost_gen_input(r,i,gen) / cost_gen_input(r,i,"total") ;
costshr_calib(f,gen,r)$cost_gen_input(r,f,"total") = cost_gen_input(r,f,gen) / cost_gen_input(r,f,"total") ;
costshr_calib(i,"total",r)    = sum(gen, costshr_calib(i,gen,r)) ;
costshr_calib(f,"total",r)    = sum(gen, costshr_calib(f,gen,r)) ;
costshr_calib2(r,gen,i)$cost_gen_input(r,"total",gen) = cost_gen_input(r,i,gen) / cost_gen_input(r,"total",gen);
costshr_calib2(r,gen,f)$cost_gen_input(r,"total",gen) = cost_gen_input(r,f,gen) / cost_gen_input(r,"total",gen);
* ------ Sum over all i and f in costshr_calib2(*,gen,r) should eqal 1 (row sums of inputs = 1)
checkCOSTSHRcal2(r,gen)$cost_gen_input(r,"total",gen) = round(
         sum(i, costshr_calib2(r,gen,i)) + sum(f, costshr_calib2(r,gen,f)) - 1, rd);
* ------ costshr_calib2(r,gen,i) is equal to the original parameter costshr(r,gen,i) read in from EXCEL
checkCOSTSHRcal22(r,gen,i)$cost_gen(gen,r) = round( costshr_calib2(r,gen,i) - costshr(r,gen,i), rd);

* ------ Set cvalues for CRU equal to those for OIL
costshr_calib("cru","pOil",r) = costshr_calib("oil","pOil",r) ;
costshr_calib("cru","mOil",r) = costshr_calib("oil","mOil",r) ;
costshr_calib("cru","bOil",r) = costshr_calib("oil","bOil",r) ;
* ------ Sum over all gen in costshr_calib(*,gen,r) should eqal 1
checkCOSTSHRcal(i,r)$costshr_calib(i,"total",r) = round( costshr_calib(i,"total",r) - 1, rd);
checkCOSTSHRcal(f,r)$costshr_calib(f,"total",r) = round( costshr_calib(f,"total",r) - 1, rd);

display
cost_gen, cost_gen_shr, cost_gen_gtap, costshr, cost_gen_input, costshr_calib, costshr_calib2;


* ------ Distribute GTAP data of intermediate inputs to 18 electricity generation technologies via costshr_calib(i,gen,r)
vafm_input(i,gen,r) = costshr_calib(i,gen,r)  * vafm(i,"ele",r) ;
cap_input(gen,r) = costshr_calib("cap",gen,r) * kd0("ele",r) ;
skl_input(gen,r) = costshr_calib("skl",gen,r) * skld0("ele",r) ;
usk_input(gen,r) = costshr_calib("usk",gen,r) * uskd0("ele",r) ;
lab_input(gen,r) = skl_input(gen,r) + usk_input(gen,r) ;
vafm_ele(i,r)  = vafm(i,"ele",r) ;

* ----- input values for disaggregation of capital and labor taxes

cap_input_no_tax(gen,r) = costshr_calib("cap",gen,r) * kd0_no_tax("ele",r) ;
skl_input_no_tax(gen,r) = costshr_calib("skl",gen,r) * skld0_no_tax("ele",r) ;
usk_input_no_tax(gen,r) = costshr_calib("usk",gen,r) * uskd0_no_tax("ele",r) ;

* ------ Check whether generation specific inputs equal overall GTAP inputs:
checkSKL(r)      = round( sum(gen, skl_input(gen,r)) - skld0("ele",r), rd) ;
checkUSK(r)      = round( sum(gen, usk_input(gen,r)) - uskd0("ele",r), rd) ;
checkCAP(r)      = round( sum(gen, cap_input(gen,r)) - kd0("ele",r), rd) ;

* ------ Der Parameter costshr enthält keine Daten zu den Vorleistungen von den Industrien NFM, NMM und SER an ELE(gen)
* ------ Daher enthält auch vafm_input keine Informationen zu diesen Industrien
* ------ Der folgende Check-Parameter enthält für die Industrien daher Werte ungleich Null:
checkVAFM(i,r)   = round( sum(gen, vafm_input(i,gen,r)) - vafm(i,"ele",r), rd) ; display checkVAFM;
* ------ 17.06.2013 costshr.xlsx angepasst und um Industrien (NFM, NMM, MVH, FOT, ROI, DWE) erweitert
* ------ In einer alten Version war der Check-Parameter nur gültig, wenn man die Elemente ausschloss, die keine Vorleistungen enthielten (NFM, NMM, MVH, FOT, ROI, DWE)
*checkVAFM(i,r)$(sum(gen, vafm_input(i,gen,r)))  = round( sum(gen, vafm_input(i,gen,r)) - vafm(i,"ele",r), rd) ; display checkVAFM;



* ------ checkGEN zeigt die Unterschiede zwischen GTAP und IEA-Daten an:
checkGEN(gen,r) = round( cost_gen_gtap(gen,r) - cost_gen(gen,r), rd) ;
* ------ checkGEN ist kleiner oder gleich Null für RUS, OPE und ROW:
*checkGEN(gen,r)$((cost_gen_gtap(gen,r) - cost_gen(gen,r)) le 0 ) = round( cost_gen_gtap(gen,r) - cost_gen(gen,r), rd) ;





display
vafm, vafm_input, vafm_ele, cap_input, skl_input, usk_input, lab_input,
checkGENSHR, checkCOSTSHR, checkCOSTGEN, checkELEVOA,
checkCOSTSHRcal, checkCOSTSHRcal2, checkCOSTSHRcal22,
checkSKL, checkUSK, checkCAP, checkVAFM, checkGEN
;


* ##############################################################################
* ------ WRITE adjusted parameters to GDX file

Execute_Unload '%datadir%%output%_ELE', vafm_input, cap_input, skl_input, usk_input, vafm_ele, costshr_calib, ele_prod, ele_prod_costs, skl_input_15, usk_input_15, vafm_input_15, cap_input_15, cap_input_no_tax skl_input_no_tax usk_input_no_tax;

$exit
* ##############################################################################


