$TITLE  NEWAGE-W
$ONEMPTY

* ====================================================================== *
*                          N E W A G E - World                           *
* ====================================================================== *
*                                *                                       *
*                        GTAP-EG based CGE-Model                         *
*                        Database GTAP 9 (2011)                          *
*                        18 Regions - 18 Sectors - 4 Factors             *
*                                *                                       *
*                        CO2 emissions trading system                    *
*                        Imperfect Labor Markets                         *
*                        BoUp Electricity Sector                         *
*                        Autonomous Energy Efficiency Improvements       *
*                        Endogenous Savings                              *
*                                                                        *
*                        Recursive dynamic 2011-2050                     *
*                                *                                       *
*                        Coal dicomissioning till 2030 in germany        *
*                        No CCS in Germany                               *
*                        Exogenous prices for coal, gas and crude oil    *
*                        Version October 2016                            *
*                        Cunha Montenegro                                *
*                                *                                       *
* ====================================================================== *

* ------ 26.10.2016 - Set the SOURCE (.map file that contains mapping of regions and comodities)
$if not set source  $set source     18x19x4_gtap9

* ------ 26.10.2016 - Set the name of the OUTPUT file (.gdx) 
$if not set output       $set output     %source%
* ------ 10.04.2014 DataSet
* ------ 26.07.2016 Folgende Zeile für GTAP9 einkommentieren
$if not set ds           $set ds         %output%

* ------ 26.10.2016 - Set the base YEAR, which is the year that the GTAP9 information is taken from
$if not set yr           $set yr         11

* ------ 26.10.2016 - Set folders for Data, Excel data and Results
$if not set datadir      $set datadir "..\data%yr%\"
$if not set xcel_datadir $set xcel_datadir "..\xcel_data\"
$if not set resultsdir   $set resultsdir "..\results\"
$setglobal datadir %datadir%

* ------ 26.10.2016 - Set source for .gms file that reads the gtap9 dataset
$include ..\build\gtap9data_newage

display s, j, g, f, vfm, vdfm, vom, vifm, vxmd, vst, vtwr, rto;
*display vdep, vkb, save;


parameter
    vom_total   sum of total supply at market prices over products
    ;

vom_total(r) = sum(g, vom(g,r));

* ------ 18.07.2016
parameter
    evd_sum2    total domestic energy use (mtoe) in one region
    ;
evd_sum2(r)= sum((i,g), evd(i,g,r));
display evd_sum2;
display evd;

* xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
* ------------------------------------------------------------------------------
* ------ (A) Initial definitions and Scenario switches
* ------------------------------------------------------------------------------

* ------ Define the solve algorithm for the MCP problem and the number of decimals
OPTION   MCP = PATH ;
OPTION   decimals = 3;
alias (r,rr), (s,ss), (i,ii), (j,jj), (g,gg), (f,ff);

* The following lines first activate the use of end-of-line comments and then specifies the activating character to be //
$oneolcom
$eolcom //


SCALAR
* ------ Flag: Define physical capital to be regionally mobile or immobile
         rsk     Flag for region-specific capital        / 1 /
         gk      Flag for global capital                 / 0 /

* ------ Number of cut offs in decimals rounding
         rdd     Number of cut offs in decimals rounding for data                / 6 /
         rd      Number of cut offs in decimals rounding for check parameters    / 6 /

* ------ Define scenario for REPORTING of RESULTS
         ref     / 1 /
         sc1     / 0 /
         sc2     / 0 /
         sc3     / 0 /
         sc4     / 0 /
         sc5     / 0 /
         sc6     / 0 /


* ------ 15.04.2014 Create aggregated Intermediates (Armington)
parameter
         vafm(i,g,r)     Sum of domestic and imported intermediates
         rtfa(i,g,r)     Tax rate of vafm
         rtfa0(i,g,r)    Tax rate of vafm [benchmark];

vafm(i,g,r) = vifm(i,g,r) + vdfm(i,g,r);                                         // Summe über g entspricht a0(i,r) ohne "i"  ??
rtfa(i,g,r)$vafm(i,g,r) = (vifm(i,g,r)*rtfi(i,g,r) + vdfm(i,g,r)*rtfd(i,g,r)) / vafm(i,g,r) ;	// with rtfi being firms import tax rates and rtfd being firms domestic tax rates
rtfa0(i,g,r) = rtfa(i,g,r);

* xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
* ------ Scenario switches
* ------------------------------------------------------------------------------
SCALARS
         EUPOL   EU unilateral carbon policy     / 1 /


* ------ Quotas/Flags/Shares in electricty generation and ETS allowances
         notrade         Defines carbon regime as regional permit price PCO2 in the EU28                 / 0 /   // notrade
         worldtrade      Defines carbon regime as international  permit price PCO2W                      / 0 /
         worldtrade2     Defines carbon regime as international  permit price PCO2W (set if ets + nets)  / 0 /
         eutrade         Defines carbon regime as international  permit price PCO2W for EU28             / 0 /   // EUtrade
         etstrade        Defines carbon regime as EU-28 ETS      permit price PCO2_ETS                   / 0 /   // ETStrade --> Schalter for basis scenario //article
         row_notrade     Defines carbon regime in rest of world (without EU28) as regional permit price PCO2        / 0 /   // Schalter for basis scenario
         netstrade       Defines carbon regime as EU-28 Non-ETS  permit price PCO2_NETS                  / 0 /
         netstrade_r     Defines carbon regime as regional Non-ETS permit price PCO2_NETSr(r)            / 0 /  // Schalter for basis scenario
         hhets           Defines carbon regime as including Households into ETS permit price PCO2_ETS    / 0 /
         bawnets         Defines carbon regime as BAW Non-ETS  permit price PCO2_NETS                    / 0 /
         trade           Schalter f?r Emissionsreduktion mit  Handel                                     / 0 /
         sectrade        Schalter f?r Emissionsreduktion mit  Handel zwischen ausgew?lten Sektoren       / 0 /
         septrade        Schalter f?r Emissionsreduktion mit  Handel zwischen ausgew?lten Sektoren und Regionen / 0 /   // BAU on
         gf_allow        Switch to turn on the free allocation of EU ETS                                 / 0 /
         DEU_sec         sector-specific emission targets in Germany                                     / 0 /
         detrade         defines carbon regime as national permit price for Germany in all SECTORS       / 0 /



         REEEM_calib    Force NEWAGE to produce the same electricity as EUROSTAT in 2015                    / 1 /   

* ------ Technology specifc scenarios
         noCOAL_DEU     switch for coal decomissioning in germany till 2035                                 / 1 / // if (1) = coal decomissioning till 2035; else = NO coal decomissioning till 2035
         DEU_CCS        Switch that enable (1) or desable (0) CCS in Germany                                / 0 /         

* ------ NEWAGE structure
         times_coupling         Swicth that activates the coupling with TIMES-PanEU                         / 0 / 

* ------ Switch to read new consumpiton taxes
         no_vat                                                                                             / 0 /
         low_lab_tax                                                                                        / 0 /
         per_capita_dis         switch to turn on the per capita redistribution                             / 0 /
         per_capita_dis_NETSr   switch to turn on the per capita redistribution for non-ETS-sectors         / 0 /
         diss_factor_tax        switch that disaggregates the taxes on primary FACTORS                      / 1 / // leave it on for all DISSERTATION scenarios
         h_t_cons               switch that reorganizes hh consumption according to heat and transport      / 0 / 
         inverse_co2_pay        redistributes co2 revenues to hh inversely to expenditures                  / 0 /

* ------ Switch to turn on cross border carbon tax
         bta                    switch to turn on the border tax Adjustment for CO2 Emissions               / 0 /
         bta_test                                                                                           / 0 /
         bta_rebate                                                                                         / 0 /

         carbon_tax_de                                                                                      / 0 /

* ----- Scenario switch
         reference_scenario_2020                                                                            / 0 /
* ------ carbon tax switch
         corona_scenario_2020                                                                               / 0 /
         germany_reference_nEHS                                                                             / 0 /

         diss_ref                                                                                           / 0 /
         diss_BAU                                                                                           / 0 /
         diss_CAP                                                                                           / 0 /
         diss_VAT                                                                                           / 0 /
         diss_LAB                                                                                           / 0 /
         diss_inv_payment                                                                                   / 1 /

         test_lower_tax /0/
;



* xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
* ------------------------------------------------------------------------------
* ------ (B) Initial CHECKS
* ------------------------------------------------------------------------------

parameter chk_vxmd, chk_vtwr, chk_vifm, chk_vom_i, chk_vom_o, chk_vb, chk_chk_vb;

*display vafm, rtfa0, rtfi0, rtfd0;

* ------ Sum of all VTWR (Trade - Margins for international transportation at world prices) must match sum of VST (Trade - exports for international transportation)
chk_vtwr = round(sum((j,i,r,s), vtwr(j,i,r,s)) - sum((i,r), vst(i,r)),10);  display chk_vtwr;           // must be zero

* ------ Zero profit check for imports and exports --> Sum over goods (Verwendungsseite) (g)
chk_vifm(i,r) = round(
+ sum(g, vifm(i,g,r))   // = vim(i,r)
- sum(s, vxmd(i,s,r)      * [(1+rtms(i,s,r))*(1-rtxs(i,s,r))])
- sum(s, vtwr("trn",i,s,r)*  (1+rtms(i,s,r))), 7);
display chk_vifm;

* ------ 15.04.2014
* ------ Zero profit check for VOM market supply (input based) --> Summe über die Entstehungsseite (i,f)
chk_vom_i(g,r) = round(
+ vom(g,r)*(1-rto(g,r))                 // for g=c,g,i there is no vfm(f,g,r)
*- sum(j, vdfm(j,g,r)*(1+rtfd(j,g,r)))
*- sum(j, vifm(j,g,r)*(1+rtfi(j,g,r)))
- sum(j, vafm(j,g,r)*(1+rtfa(j,g,r)))
- sum(f, vfm(f,g,r) *(1+rtf(f,g,r))), 7);
display chk_vom_i;

* ------ Zero profit check for VOM market supply (output based) --> Summe über die Verwendungsseite (g)
chk_vom_o(i,r) = round(
+ vom(i,r)
- sum(s, vxmd(i,r,s)) - vst(i,r)       // VST exports only for TRN
- sum(g, vdfm(i,g,r)), 7);
display chk_vom_o;

* ------ Income Balance: Regional net transfers from abroad (sum must be zero)
chk_vb(r) = 0;
chk_vb(r) = vom("c",r) + vom("g",r) + vom("i",r)
        - sum((f,g), vfm(f,g,r))                      // = sum(f, evom(f,r))
        - sum(j,  vom(j,r)*rto(j,r))
        - sum(g,  sum(i, vdfm(i,g,r)*rtfd(i,g,r) + vifm(i,g,r)*rtfi(i,g,r)))
        - sum(g,  sum(f, vfm(f,g,r)*rtf(f,g,r)))
        - sum((i,s), rtms(i,s,r) * (vxmd(i,s,r) * (1-rtxs(i,s,r)) + sum(j,vtwr(j,i,s,r))))
        + sum((i,s), rtxs(i,r,s) *  vxmd(i,r,s));

chk_vb("chksum") = sum(r, chk_vb(r));
display chk_vb;


chk_chk_vb("chk","vom",r) = vom("c",r) + vom("g",r) + vom("i",r);
chk_chk_vb("chk","vfm",r) = sum((f,g), vfm(f,g,r));                      // = sum(f, evom(f,r))
chk_chk_vb("chk","vom_tax",r) = sum(j,  vom(j,r)*rto(j,r));
chk_chk_vb("chk","vdifm",r) = sum(g,  sum(i, vdfm(i,g,r)*rtfd(i,g,r) + vifm(i,g,r)*rtfi(i,g,r)));
chk_chk_vb("chk","vfm_tax",r) = sum(g,  sum(f, vfm(f,g,r)*rtf(f,g,r)));
chk_chk_vb("chk","imp",r) = sum((i,s), rtms(i,s,r) * (vxmd(i,s,r) * (1-rtxs(i,s,r)) + sum(j,vtwr(j,i,s,r))));
chk_chk_vb("chk","exp",r) = sum((i,s), rtxs(i,r,s) *  vxmd(i,r,s));

* xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
* ------------------------------------------------------------------------------
*        (C) Additional set (subset) definitions for REGIONS
* ------------------------------------------------------------------------------


* ------ 18.10.2014 18x18x4 Mapping
SET
         DEU(r)     Germany                         / deu /
         FRA(r)     France                          / fra /
         ITA(r)     Italy                           / ita /
         POL(r)     Poland                          / pol /
         UKI(r)     United Kingdom                  / uki /
         ESP(r)     Espain and Portugal             / esp /
         BNL(r)     Benelux                         / bnl /
         EUN(r)     Northern and Baltic             / eun /
         EUS(r)     South-Eastern EU-28             / eus /
         USA(r)     United States of America        / usa /
         OEC(r)     Rest of OECD                    / oec /
         BRZ(r)     Brazil                          / brz /
         RUS(r)     Russia                          / rus /
         INDIA(r)   India                           / ind /
         CHI(r)     China and Hong Kong             / chi /
         RSA(r)     South Africa                    / rsa /
         OPA(r)     OPEC and Arabian World          / opa /
         ROW(r)     Rest of the world               / row /

*------- 05.10.2017	When sector-specific targets in Germany are used, Germany cannot be part of the EU
         EU28(r)
         EU28_de(r)        EU28 (excluding Germany)
                         /        FRA, ITA, POL, UKI, ESP, BNL, EUN, EUS /

         EU28_eu(r)         EU28 (including Germany)
                         /       DEU, FRA, ITA, POL, UKI, ESP, BNL, EUN, EUS /

         coali(r)   coalition of the EU regions for scenario e2 from kopernikus with deutschland
                          /      DEU, BNL, FRA, ITA, EUN, ESP, UKI /
         
         coali_small(r)     small coallition of the EU region for scenario e2k from kopernikus 
                         / BNL, FRA, EUN /

* ------ 18.10.2016 EU28

         notrad(r)       Dynamic set for country-specific CO2 targets
         pco2w_r(r)      Dynamic set for PCO2W and world trade (all except notrad)
*$ontext
*         EU15(R)         EU15 = OEU
*                         /      DEU, ITA, FRA, ESP, BNL, EUN       /
*         NMS12(R)        NMS12 = NEU
*                         / EUS /
*         adreg(r)        Regions with exposed climate change risk and strong adaptation needs
*                         / IND, ROW /
*         OECD(r)         OECD
*                         /       DEU, ITA, FRA, ESP, BNL, EUS, EUN, POL, UKI, OEC, USA /
*         OECD2(r)        OECD without EU28
*                         / OEC, USA /
*         BRICS(r)        BRICS
*                         / BRZ, RUS, IND, CHI, RSA /
*         REST(r)         Rest of the world + ARB + OPE
*                         / OPA, ROW /
* ------ EMERGE had equaitons changed
         EMERGE(r)       Emerging markets with high growth but "low" unemployment growth and or high AEEI
                         / BRZ, RUS, IND, CHI, RSA, EUS, OPA /   // the URSK constraint works better (without REDEF) if RUS and ARB are in emerge
*         Annexb_EU(r)    Annex B Ratifizierer mit EU Gemäß 666 ZEW aber ohne RUS!
*                         /       DEU, ITA, BNL, ESP, FRA, UKI, EUS, EUN, OEC /
*         Annexb(r)       Annex B Ratifizierer ohne EU gemäß 666 ZEW aber ohne RUS!
*                         / OEC /
*         Annexb_broad(r) Annex B Ratifizierer ohne EU gemäß 666 ZEW mit USA aber ohne RUS!
*                         / OEC, USA /
         NUCPOT(R)       Countries with limited nuclear addition
                         / FRA, ITA, UKI, BNL, ESP, POL, EUN, EUS, USA, OEC, BRZ, RUS, IND, CHI, RSA, OPA, ROW /
         NUCOUT(R)       Countries with Nuclear phase-out
*                         / DEU, BAW /
                         / DEU /

         HH_DISAG(R)     countries with household disaggregation
                         / DEU, FRA, ITA, UKI, BNL, ESP, POL, EUN, EUS /
*                         / /

         h_t_cons_reg(r)    countries where consumption will be reorganized with heat and transport consumption
                            / DEU, FRA, ITA, UKI, BNL, ESP, POL, EUN, EUS /
*                            / /

*         groupA(r)       Adaptation group A - high vulnerability
*                         / IND, ROW /
*         groupB(r)       Adaptation group B - medium vulnerability
*                         / ITA, FRA, EUS, OEC, RSA, BRZ, OPA /
*         groupC(r)       Adaptation group C - low vulnerability
*                         /       DEU, EUN, USA, RUS, CHI /
* ------ 17.06.2013 Define additional sets for replacing "eab" and "rab"
         eab_s(r)        / OEC /
         rab_s(r)        / OEC /
* ------ 30.07.2013
*         woBAW(r)        All without BAW / DEU, FRA, AUT, SWZ, EUN, EUS, EUE, USA, BRZ, RUS, IND, CHI, RSA, OEC, ARB, OPE, ROW /
*         woIND(r)        All without IND / DEU, FRA, AUT, SWZ, EUN, EUS, EUE, USA, BRZ, RUS, BAW, CHI, RSA, OEC, ARB, OPE, ROW /
*         woROW(r)        All without IND / DEU, FRA, AUT, SWZ, EUN, EUS, EUE, USA, BRZ, RUS, BAW, CHI, RSA, OEC, ARB, OPE, IND /
*$offtext
         num(R)  Identifies the numeraire region
                        /USA/

* ------ 01.04.2020
         BTA_coa(r)     coalition BCA experiment  
*                        / DEU, FRA, ITA, POL, UKI, ESP, BNL, EUN, EUS, USA, OEC/
                        / /
;

EU28(r) =EU28_eu(r);
EU28(r)$(detrade or DEU_sec) =EU28_de(r);

* ------ Identify numeraire region
*        ACHTUNG
*         num(r)= YES$(ORD(r) EQ 1);                                             // 28.02.2013 überprüfen: Hier wäre das Ergebnis DEU!
         num(R)= USA(R);
*display num;

* ------ Detailed set definitions for ENERGY                                   *
SET
         e(i)            energy markets    /
                         gas     natural gas works
                         ele     electricity and heat
                         oil     refined oil products
                         col     coal transformation
                         cru     crude oil /
         xe(i)           exhaustible energy      / cru, gas, col /
         xeoil(i)        exhaustible energy + refined oil  / cru, gas, col, oil /
         fe(i)           final energy            / oil, gas, col /
         ele(i)          electricity             / ele /
         eleg(g)         electricity             / ele /
         cru(i)          crude oil               / cru /
         oil(i)          refined oil             / oil /
         gas(i)          natural gas             / gas /
         col(i)          coal                    / col /
         chm(i)          Chemical industry       / chm /
         nfm(i)          NF-metalls industry     / nfm /
         trn(i)          Transport industry      / trn /
         mac(i)          Machinery               / mac /
         fot(i)          Food and tobacco        / fot /
         man(i)          Manufacturing           / OIL, PPP, IRS, NFM, NMM, CHM, mvh, mac, roi, fot /
         ets(i)          EU-ETS sectors
                         / ELE, OIL, PPP, IRS, NFM, NMM, CHM /
         eii(i)          EU-ETS energy intensive sectors without Electricity
                         /      OIL, PPP, IRS, NFM, NMM, CHM /
         ind(i)          Verarbeitendes Gewerbe - Manufacturing - 
                         / CHM, PPP, IRS, NFM, NMM, FOT, MVH, MAC, ROI /
         indE(i)         Energieintensive Industrie
                         / CHM, PPP, IRS, NFM, NMM /
         indEN(i)        Nicht-Energieintensive Industrie
                         / MAC, FOT, MVH, ROI /
         agr(i)          Agriculture sector
                         / agr /
         agrind(i)       Agriculture and industry
                         / CHM, PPP, IRS, NFM, NMM, FOT, MVH, MAC, ROI, agr /
         other_sec(i)    other sectors
                         /agr, dwe, bui, trn/
* ------ 13.09.2015 Diss
         mvh(i)          Motor vehicles                  / mvh /
         bui(i)          Building and construction       / bui /
         dwe(i)          Dwelings Sector                 / dwe /
         ser(i)          Building and construction       / ser /
         buiser(i)       Building and construction       / bui, ser /

* ----- 09.03.2020 heat and transport consumption
         non_h_t_goods(i)   commodities that are not in the transport and heat nests
                            / AGR, SER, CHM, PPP, IRS, NFM, NMM, FOT, MAC, ROI /

* ----- 15.04.2014
         cg(g)           / c, g /

* ----- 01.04.2020 bta test
         BTA_sec(i)     sectors that have to pay export CO2 tariffs
                        / IRS, CHM, NFM, NMM, OIL/ 

;

*------15.09.2017 additional sets needed to include sector-specific CO2 targets for Germany
set     sec        /energy, buildings, transport, industry, residential, commerce/
        indu(sec)  /industry/
        LS_sec(sec)/residential, commerce, transport/;
*-------15.09.2017 end

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*    SET OF HOUSEHOLDS
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

SET     hh      household groups /
                hh1,
                hh2,
                hh3,
                hh4,
                hh5/

        HH1(hh) /hh1/
        HH2(hh) /hh2/
        HH3(hh) /hh3/
        HH4(hh) /hh4/
        HH5(hh) /hh5/;

alias (hh,hh_);
* ACHTUNG
* ETS soll im szenario notrad(r) leer sein da sonst falsche report werte für carbon
* SET      ETS(I) / /;

SET      D       Submarkets for which import shares are differentiated /
                 G       Public,
                 C       Private Consumption,
                 I       Intermediate input
                 E                           /
         X(D)    Markets for EEI calculation / I, E, C / ;

* ------------------------------------------------------------------------------
*        SET and additional sets (SUBSETS) for technology BOUP
*        in ELECTRICITY GENERATION
* ------------------------------------------------------------------------------
*        b = baseload, m = middleload, p = peakload

SET      GEN             Types of power generation
                         /  bNUC, bBC,  bBIO, bGEO,  bHYDRO, bHC,  bGAS, bOIL, bCCS,
                            mHC, mGAS, mWIND, mSOLAR, mOIL, mCCS, pOIL, pGAS, pHYDRO /
         reg(gen)        EEG-funded renewable                   / bGEO, bHYDRO, bBIO, mWIND, mSOLAR /
         bgeo(gen)       Geothermal                              / bGEO /
         bhydro(gen)     Hydropower (run of river)               / bHYDRO /
         hydro(gen)      Hydropower                              / bHYDRO, pHydro /
         bbio(gen)       Biomass                                 / bBIO /
         bgas(gen)       Base Gas                                / bGAS /
         mgas(gen)       Middle Gas                              / mGAS /
         bHC(gen)        Base Coal                               / bHC /
         mHC(gen)        Middle Coal                             / mHC /
         HC(gen)         Base and Middle                         / bHC, mHC /
         bccs(gen)       Baseload BK CCS                         / bCCS /
         mccs(gen)       Middleload SK CCS                       / mCCS /
         mwind(gen)      Wind                                    / mWIND /
         msolar(gen)     Solar                                   / mSOLAR /
         phydro(gen)     pump storage                            / phydro /
         poil(gen)       Peak Oil                                / pOIL /
         moil(gen)       Middle Oil                              / mOIL /
         boil(gen)       Base Oil                                / bOIL /
         notreg(gen)     Non EEG-funded renewables               / bNUC, bBC,  bHC,  bGAS, bOIL, bCCS,
                                                                   mHC, mGAS, mOIL, mCCS, pOIL, pGAS, pHYDRO /
         bNUC(gen)       Nuclear Power                           / bNUC /
         bBC(gen)        Lignite                                 / bBC /
         FOScoal(gen)    Coal power generation (+CCS)            / bHC, mHC, bBC, bCCS, mCCS /
         FOScoalh(gen)   Hard coal power generation              / bHC, mHC /
         FOScoalb(gen)   Lignite power generation                / bBC /
         FOSoil(gen)     Oil Power generation                    / bOil, mOil, pOil /
         FOSgas(gen)     Gas Power generation                    / bGas, mGas, pGas /
         FOSccs(gen)     CCS Power generation                    / bccs, mccs /
         FOSrest(gen)    Non-Fossil Power generation             / bGEO, bHYDRO, bBIO, mWIND, mSOLAR, pHydro, bNUC /
         coalgasccs(gen) Coal-Gas-CCS Power generation           / bGas, mGas, pGas, bHC, mHC, bBC, bCCS, mCCS /
* ------ 05.08.2011: SETS für 4) Electricity Production im MPSGE-Block
         base(gen)       Baseload Technologies                   / bNUC, bBC,  bBIO, bGEO,  bHYDRO, bHC,  bGAS, bOIL, bCCS /
         middle(gen)     Middleload Technologies                 / mHC, mGAS, mWIND, mSOLAR, mOIL, mCCS /
         og(gen)         Oil-Gas Peak Technologies               / pOIL, pGAS /
         peak(gen)       Peakload Technologies                   / pHYDRO /
         fosgen(gen)     Conventional technologies               / bNUC, bBC, bHC, bGAS, bOIL, mHC, mGAS, mOIL, pOIL, pGAS, pHYDRO, bCCS, mCCS /
         fosfosgen(gen)  Fossil technologies wo CCS              / bBC, bHC, bGAS, mHC, mGAS, bOIL, mOIL, pOIL, pGAS /
         fosccsgen(gen)  Fossil technologies w CCS               / bBC, bHC, bGAS, bOIL, mHC, mGAS, mOIL, pOIL, pGAS, bCCS, mCCS /
         fos(gen)                                                / bBC, bHC, bGAS, bOIL, mHC, mGAS, mOIL, pOIL, pGAS, bCCS, mCCS /
* ------ 12.01.2012
         fosnuc(GEN)     Power generation of fossils and nuclear     / bNUC, bBC, bHC, bGAS, bOIL, mHC, mGAS, mOIL, pOIL, pGAS, bCCS, mCCS /
* ------ 18.06.2014
         foslim(gen)     Generation technologies to be limited for ELEn.UP / bBC, bOIL, mOIL /
         reslim(gen)     Generation technologies to be limited for ELEn.UP / bHYDRO, bBIO, mWIND, pHYDRO /
;

Parameter
    sec2cluster(sec,*);

 sec2cluster("energy","bNUC")=1;
 sec2cluster("energy","bBC")=1;
 sec2cluster("energy","bBIO")=1;
 sec2cluster("energy","bGEO")=1;
 sec2cluster("energy","bHYDRO")=1;
 sec2cluster("energy","bHC")=1;
 sec2cluster("energy","bGAS")=1;
 sec2cluster("energy","bOIL")=1;
 sec2cluster("energy","bCCS")=1;
 sec2cluster("energy","mHC")=1;
 sec2cluster("energy","mGAS")=1;
 sec2cluster("energy","mWIND")=1;
 sec2cluster("energy","mSOLAR")=1;
 sec2cluster("energy","mOIL")=1;
 sec2cluster("energy","mCCS")=1;
 sec2cluster("energy","pOIL")=1;
 sec2cluster("energy","pGAS")=1;
 sec2cluster("energy","pHYDRO")=1;
 sec2cluster("energy","COL")=1;
 sec2cluster("energy","CRU")=1;
 sec2cluster("energy","GAS")=1;
 sec2cluster("energy","OIL")=1;
 sec2cluster("energy","ELE")=1;


sec2cluster("residential","c") = 1;
sec2cluster("buildings","AGR") = 1;

sec2cluster("commerce","SER") = 1;
sec2cluster("buildings","BUI") = 1;

sec2cluster("transport","TRN") = 1;

sec2cluster("industry","IRS")=1;
sec2cluster("industry","NFM")=1;
sec2cluster("industry","NMM")=1;
sec2cluster("industry","PPP")=1;
sec2cluster("industry","CHM")=1;
sec2cluster("industry","FOT")=1;
sec2cluster("industry","MVH")=1;
sec2cluster("industry","MAC")=1;
sec2cluster("industry","ROI")=1;

*-------13.10.2017 end

* ------------------------------------------------------------------------------
* ------ Additional scenario specific sets or dynamic sets
* ------------------------------------------------------------------------------
SET
         nr(i,r)         Non-exhaustible energy production
         nr_ele(i,r)     Non-exhaustible energy production including ELE
         quota(gen,r)    Flag for technologies contributing to green quota
         constr(i,r)     Constrain calculations for specific sectors and regions
         nonEU28(r)      Non-EU28;

* ------ Das Set NR (Non-exhaustible energy production) enthält alle Regionen und alle
* ------ Sektoren aus VOM(i,r) außer CRU, GAS, COL, OIL und ELE. Aus der 17x10- wird eine 12x10-Matrix.
* ------ The Set NR (non-exhaustible energy products) contains all the regions and sectors
* ------ from VOM(i,r) except for CRU, GAS, COL, OIL and ELE.
nr(i,r)     = yes$(vom(i,r)$(not xe(i)) $(not ele(i)) $(not oil(i)))     ;
nr_ele(i,r) = yes$(vom(i,r)$(not xe(i)) $(not oil(i)))                   ;

quota(gen,r)     = no ; quota(fosgen,"deu") = no;
noneu28(r)       = no ; noneu28(r) = yes$(not eu28(r));
constr(i,r)      = yes;
*constr(i,r)$ele(i) = no;
*display nr, nr_ele, quota, constr, eu28, noneu28;

notrad(r) = no;
pco2w_r(r) = no;


* ---------------------------------------------------------------------- *
*        SET and additional sets (SUBSETS) for DYNAMIC MILESTONES
* ---------------------------------------------------------------------- *
* ------ 24.10.2016 Time series for REEEM Project

SET      YR              MILESTONES
                         / 2011, 2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050    / ;
SET      BEFORE(YR)      / 2011                                          / ;
SET      BEFORE2050(YR)  / 2011,  2015, 2020, 2025, 2030, 2035, 2040, 2045     / ;
SET      BEFORE2045(YR)  / 2011,  2015, 2020, 2025, 2030, 2035, 2040     / ;
SET      BEFORE2040(YR)  / 2011,  2015, 2020, 2025, 2030, 2035     / ;
SET      BEFORE2035(YR)  / 2011,  2015, 2020, 2025, 2030    / ;
SET      BEFORE2030(YR)  / 2011,  2015, 2020, 2025          / ;
SET      BEFORE2025(YR)  / 2011,  2015, 2020                / ;
SET      BEFORE2020(YR)  / 2011,  2015                      / ;
SET      BEFORE2015(YR)  / 2011                             / ;
SET      AFTER(YR)       /        2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050  / ;
SET      AFTER2015(YR)   /              2020, 2025, 2030, 2035, 2040, 2045, 2050  / ;
SET      AFTER2020(YR)   /                    2025, 2030, 2035, 2040, 2045, 2050  / ;
SET      AFTER2025(YR)   /                          2030, 2035, 2040, 2045, 2050  / ;
SET      AFTER2030(YR)   /                                2035, 2040, 2045, 2050  / ;
SET      AFTER2035(YR)   /                                      2040, 2045, 2050  / ;
SET      AFTER2040(YR)   /                                            2045, 2050  / ;
SET      AFTER2045(YR)   /                                                  2050  / ;
SET      KYOTO_OUT(YR)   /        2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050  / ;

* ------ 26.10.2016 for REEEM project and GTAP9
SET      YR2011(YR) / 2011 / ;
SET      YR2015(YR) / 2015 / ;
SET      YR2020(YR) / 2020 / ;
SET      YR2025(YR) / 2025 / ;
SET      YR2030(YR) / 2030 / ;
SET      YR2035(YR) / 2035 / ;
SET      YR2040(YR) / 2040 / ;
SET      YR2045(YR) / 2045 / ;
SET      YR2050(YR) / 2050 / ;

* ------ 3.11.2014
SET      YRx(yr)        Subset for 10-year-milestones Calculations
         / 2011, 2020, 2030, 2040, 2050 / ;
SET      AFTERx(yr)        Subset for 10-year-milestones Calculations
         /       2020, 2030, 2040, 2050 / ;


* ------ Define ALIAS indices for interactions etc.
alias (gen,gengen), (yr,yryr);

* xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
* ------------------------------------------------------------------------------
*        (D) Specifying MAIN DATA (Static)
* ------------------------------------------------------------------------------
* ------ All Parameters explained in Rutherford & Paltsev (2000) --> GTAP-EG !
PARAMETERS
* ------ Taxes:
         ti(j,g,r)       Intermediate input tax
         ty(i,r)         Output tax
         tg(i,r)         Tax rates on government demand
         tp(i,r)         Tax rate on private demand
         tp0(i,r)
         tf(f,i,r)       Factor tax
         tx(i,s,r)       Export tax rate (defined on a net basis)
         tm(i,s,r)       Import tariff rate
* ------ Prices:
         pmx0(i,s,r)     Reference price of imports,
         pmt0(i,s,r)     Reference price of transport services,
         pc0(i,r)        Reference price for private demand,
         pai0(j,g,r)     Reference price for intermediate inputs,
         pf0(f,i,r)      Reference price for factor inputs
         py0(i,r)        Reference output price (domestic or export) --> not in GTAP-EG,
         pwm             Weltmarktpreise
* ------ Adopt some notation which is different from the core GTAPinGAMS model (Aggregate data)
         a0(i,r)         Armington supply
         m0(i,r)         Aggregate import supply
         d0(i,r)         Aggregate domestic supply
         c0(i,r)         Final demand by commodity
         ct0(r)          Total final demand
         tc(i,r)         Tax rate on final demand
         pc0(i,r)        Reference price for final demand
         evoa(f,r)       Value of factor income
* ------ 30.06.2014 export and import reporting
         vxm(i,r)        Trade - Value of export (gross excise tax)
         vim(i,r)        Trade - Total value of imports (gross tariff)
         vtwr1(i,r,s)    Trade - import\export wedge or value of transport services (viws - vxwd) (see vtwr1_chk)
* ------ Other:
         dep(r)          Depreciation - Abschreibungssatz - es werden 4 % angenommen
         vi(r)           Total value of investment
         vg(r)           Total value of public expenditures
         vp(r)           Total value of private expenditures
* ------ Define parameters for Keynes savings constraint
         welf(r)         GDP incl. current account
         savrate(r)      Scaling Factor for Savings Rate

* ------ 19.02.2019
* ------ parameters specifc for hh disaggregation

         c_hh0(*,r)     "final demand by commodity for households"
         c_gov0(*,r)    "final demand by commodity for government"
         pc_hh0(i,r)    "reference price for hh consumption"
         pc_gov0(i,r)   "reference price for the government"
;

* ------ Taxes:
ty(i,r)          = rto(i,r);                    // rto(gr) - output (or income) subsidy rates
ti(j,i,r)        = rtfa0(j,i,r);                // rtfa0(i,g,r) - Tax rate for vafm [benchmark]
ti(j,"i",r)      = rtfa0(j,"i",r);              // Taxation of "i"?
tg(i,r)          = rtfa0(i,"g",r);
tp(i,r)          = rtfa0(i,"c",r);
tp0(i,r)         = tp(i,r);
tf(f,i,r)        = rtf0(f,i,r);                 // rtf0(f,g,r) - Primary factor and commodity rates taxes
tx(i,s,r)        = rtxs0(i,s,r) * (-1);         // rtxs is explicitly defined as a Subsidy
tm(i,s,r)        = rtms0(i,s,r);                // rtms0(i,r,s) - Import taxes rates
* ------ Prices
pmx0(i,s,r)      = pvxmd(i,s,r);                 // = (1+rtms0(i,s,r)) * (1-rtxs0(i,s,r))   // pvxmd(i,s,r) - Import price (power of benchmark tariff)
pmt0(i,s,r)      = pvtwr(i,s,r);                 // =  1+rtms0(i,s,r)                       // pvtwr(i,s,r) - Import price for transport services
pc0(i,r)         = 1 + rtfa0(i,"c",r);
pai0(i,j,r)      = 1 + rtfa0(i,j,r);
pai0(i,"i",r)    = 1 + rtfa0(i,"i",r);
pf0(f,i,r)       = 1 + rtf0(f,i,r);
py0(i,r)         = 1 - rto(i,r) ;
pwm(i,r,s)       = 1 ;                           // World market prices

* ------ Adopt some notation which is different from the core GTAPinGAMS model (Aggregate data)
a0(i,r)  = sum(g, vafm(i,g,r));                                              //Sum of domestic and imported intermediates over goods  // Wie muss "i" behandelt werden? "i" bzw. cgd ist Teil von j in vdfm(i,j,r)
d0(i,r)  = sum(g, vdfm(i,g,r));                                              //Intermediates - firms  domestic purchases at market prices - sum over goods
m0(i,r)  = sum(g, vifm(i,g,r));                                              //Intermediates - firms  imports at market prices - sum over goods
c0(i,r)  = sum(g$cg(g), vdfm(i,g,r) + vifm(i,g,r));
tc(i,r)$c0(i,r) = (tp(i,r)*vafm(i,"c",r) + tg(i,r)*vafm(i,"g",r) ) / c0(i,r);
ct0(r)   = sum(i, c0(i,r)*(1+tc(i,r)) ) ;
pc0(i,r) = 1 + tc(i,r);
vi(r)    = sum(i, vafm(i,"i",r) * (1 + ti(i,"i",r)));                        // price of "i" with taxation // Mit oder ohne Steuern?
vg(r)    = sum(i, vafm(i,"g",r) * (1 + tg(i,r)));                            // price of "g" with taxation // Mit oder ohne Steuern?
vp(r)    = sum(i, vafm(i,"c",r) * (1 + tp(i,r)));                            // price of "c" with taxation // Mit oder ohne Steuern?
vxm(i,r) = sum(s, vxmd(i,r,s))+ vst(i,r) ;
vim(i,r) = sum(g, vifm(i,g,r));                                              // according to the equations, m0(i,r) = vim(i,r) = sum(g, vifm(i,g,r))

* ------ parameters for HH disaggregation
c_hh0(i,r)$HH_DISAG(r) = vafm(i,"c",r);                                           
c_gov0(i,r)$HH_DISAG(r) = vafm(i,"g",r);

pc_hh0(i,r)$HH_DISAG(r) = 1 + tp(i,r);
pc_gov0(i,r)$HH_DISAG(r) = 1 + tg(i,r);

* ------ 15.04.2014
vtwr1(i,r,s) = vtwr("trn",i,r,s);                                            // Trade - Margins for international transportation at world prices

* ------ DEP wird mit 4% angenommen (vgl. MRTdata_26_19_4.gms)
dep(r) = 0.04;

* ------ Define parameters for Keynes savings constraint
* ------ GDP as sum of private and public expenditure and investment expenditures minus current account
welf(r)     =   vp(r) + vg(r) + vi(r) - vb(r) ;
* ------ 17.06.2013 Do not calculate savrate for BAW, because otherwise there is an error (division by zero!)
savrate(r)  =   vi(r) / welf(r) ;


* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------------------------------------------------------------------------------
* ------ (E) Compute Benchmark factor inputs and factor incomes
* ------------------------------------------------------------------------------

PARAMETER
* ------ Production factors
         skld0(i,r)      Base year skilled labor demand by sector
         uskd0(i,r)      Base year unskilled labor demand by sector
         kd0(i,r)        Base year capital earnings by sector
         rd0(i,r)        Resource inputs for fossil fuel production

* ------ Other
         chkFAC(*,r)     Factor income test;

* ------ Endowments at firms purchases at market prices (vfm) and the reference price (pf0) which includes factor taxes (tf)
*skld0(i,r) = vfm("skl",i,r) * pf0("skl",i,r) ;
*uskd0(i,r) = vfm("usk",i,r) * pf0("usk",i,r) ;
*kd0(i,r)   = vfm("cap",i,r) * pf0("cap",i,r) ;
rd0(i,r)   = vfm("res",i,r) * pf0("res",i,r) ;

* ------ endowment for disaggregation of rtf (27.02.2019)

skld0(i,r)$(NOT (diss_factor_tax AND HH_DISAG(r)))   = vfm("skl",i,r) * pf0("skl",i,r) ;
skld0(i,r)$(diss_factor_tax and HH_DISAG(r))   = vfm("skl",i,r);

uskd0(i,r)$(NOT (diss_factor_tax AND HH_DISAG(r)))   = vfm("usk",i,r) * pf0("usk",i,r) ;
uskd0(i,r)$(diss_factor_tax and HH_DISAG(r))   = vfm("usk",i,r);

kd0(i,r)$(NOT (diss_factor_tax AND HH_DISAG(r)))   = vfm("cap",i,r) * pf0("cap",i,r) ;
kd0(i,r)$(diss_factor_tax and HH_DISAG(r))   = vfm("cap",i,r);

rd0("agr",r)$(NOT (diss_factor_tax AND HH_DISAG(r)))   = vfm("res","agr",r) * pf0("res","agr",r) ;
rd0("agr",r)$(diss_factor_tax and HH_DISAG(r))   = vfm("res","agr",r);

rd0("roi",r)$(NOT (diss_factor_tax AND HH_DISAG(r)))   = vfm("res","roi",r) * pf0("res","roi",r) ;
rd0("roi",r)$(diss_factor_tax and HH_DISAG(r))   = vfm("res","roi",r);

* ------ Aggregate natural resources other than primary energy to capital
kd0("agr",r) = kd0("agr",r) + rd0("agr",r) ;
kd0("roi",r) = kd0("roi",r) + rd0("roi",r) ;
rd0("roi",r) = 0 ;
rd0("agr",r) = 0 ;

rtf("cap","agr",r)$(diss_factor_tax and HH_DISAG(r)) = (((vfm("res","agr",r) * pf0("res","agr",r))  +  (vfm("cap","agr",r) * pf0("cap","agr",r)))/kd0("agr",r)) - 1;
rtf("cap","roi",r)$(diss_factor_tax and HH_DISAG(r)) = (((vfm("res","roi",r) * pf0("res","roi",r))  +  (vfm("cap","roi",r) * pf0("cap","roi",r)))/kd0("roi",r)) - 1;

rtf0("cap","agr",r)$(diss_factor_tax and HH_DISAG(r)) = rtf("cap","agr",r);
rtf0("cap","roi",r)$(diss_factor_tax and HH_DISAG(r)) = rtf("cap","roi",r);

pf0("cap","agr",r)$(diss_factor_tax and HH_DISAG(r)) = rtf("cap","agr",r) + 1;
pf0("cap","roi",r)$(diss_factor_tax and HH_DISAG(r)) = rtf("cap","roi",r) + 1;

vfm("res","agr",r)$(diss_factor_tax and HH_DISAG(r)) = 0;
vfm("res","roi",r)$(diss_factor_tax and HH_DISAG(r)) = 0;

vfm("cap","agr",r)$(diss_factor_tax and HH_DISAG(r)) = kd0("agr",r);
vfm("cap","roi",r)$(diss_factor_tax and HH_DISAG(r)) = kd0("roi",r);

* ------ Factor income is sum of all sectoral factor inputs including tax  // evoa: Value of factor income
evoa("skl",r) = sum(i, skld0(i,r)) ;
evoa("usk",r) = sum(i, uskd0(i,r)) ;
evoa("cap",r) = sum(i, kd0(i,r)) ;
evoa("res",r) = sum(i, rd0(i,r)) ;

parameter evoa_alt(r);
evoa_alt(r) = evoa("cap",r);

* ------ Checking Factor income
chkFAC("skl",r) = sum(i, skld0(i,r)) - evoa("skl",r) ;
chkFAC("usk",r) = sum(i, uskd0(i,r)) - evoa("usk",r) ;
chkFAC("cap",r) = sum(i, kd0(i,r))   - evoa("cap",r) ;
chkFAC("res",r) = sum(i, rd0(i,r))   - evoa("res",r) ;

*display skld0, uskd0, kd0, rd0, evoa ;
display chkFAC;
*display pf0, tf, rtf, vfm, skld0, uskd0;

* ----- 26.04.2019 - building evoa for tax transfer from gov to households



* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------------------------------------------------------------------------------
* ------ (F) BOUP: ELECTRICITY SPECIFICATIONS
* ------------------------------------------------------------------------------

* ------ READ calibrated electricity data                                       // GDX
PARAMETERS
* ------ Extension of GTAP data to 18 different electricity generation technologies
         vafm_input(i,*,r)       Extended parameter vafm value of aggregate firms inputs at market prices
         cap_input(*,r)          Extended parameter kd0 capital earnings
         usk_input(*,r)          Extended parameter uskd0 labor demand unskilled
         skl_input(*,r)          Extended parameter skld0 labor demand skilled
         skl_test
         usk_test
         cap_test
         vafm_test(i,r)

         cap_input_no_tax(*,r)          Extended parameter kd0 capital earnings without including taxes
         usk_input_no_tax(*,r)          Extended parameter uskd0 labor demand unskilled
         skl_input_no_tax(*,r)          Extended parameter skld0 labor demand skilled  

* ------ calibration for 2015 germany electricity generation costs
* control of economic development in GER --> input for electricity generation for GER in 2015 can be manipulated, such that it reaches real values
         vafm_input_deu0(i,*)       Extended parameter vafm value of aggregate firms inputs at market prices
         cap_input_deu0(*)          Extended parameter kd0 capital earnings
         usk_input_deu0(*)          Extended parameter uskd0 labor demand unskilled
         skl_input_deu0(*)          Extended parameter skld0 labor demand skilled

* ------ 27.02.2018 parameter that writes down the yearly labor and capital endowment per region
         skl_input_yr(r,gen,yr)
         usk_input_yr(r,gen,yr)
         cap_input_yr(r,gen,yr)

* ------ Stored ELE values
         vafm_ele(i,r)           Intermediate inputs in ELE
         kd0_ele
         skld0_ele
         uskd0_ele
* ------ Old/New Capital
         ks_n(gen,r)             New vintage capital
         ks_x(gen,r)             Extant capital
         ks_x0(gen,r)            Benchmark extant capital
         capgenX(R)              Sum of extant capital in generation technologies
         capgenNEW(R)            Sum of new capital in generation technologies
         chkXTANT(gen,r)
* ------ Electricity generation per technology (13.01.2012: Vereinfachte SET-schreibweise)
         out_gen(gen,r)          Output electricity generation technologies
         out_gen_chk(gen,r)      Checkparameter für gen_out
         gen_out_chk(gen,r)      Checkparameter für gen_out
* ------ Other
         costshr_calib
         cap_input3(gen,r)
         cap_input0(*,r)
         elast_ene(r)           Elasticity for energy inputs (fossil fuels and electricity)
         ;

SCALAR
* ------ Extant production share
         thetax                  Extant production share / 0.985 /;

;
* ##############################################################################
* ----- 23.08.2018 - The following block is used to choose between two different GDX files, more especifically between the usual one or the TIMES coupling one
*$ifThenE times_coupling == 1

*$GDXIN %datadir%%source%_ELE_TIMES.gdx
*$LOAD vafm_input  cap_input  skl_input  usk_input  costshr_calib

*$else

$GDXIN %datadir%%source%_ELE.gdx
$LOAD vafm_input  cap_input  skl_input  usk_input  costshr_calib cap_input_no_tax skl_input_no_tax usk_input_no_tax

*$endif

cap_input(gen,r)$(HH_DISAG(r) AND diss_factor_tax) = cap_input_no_tax(gen,r);
skl_input(gen,r)$(HH_DISAG(r) AND diss_factor_tax) = skl_input_no_tax(gen,r);
usk_input(gen,r)$(HH_DISAG(r) AND diss_factor_tax) = usk_input_no_tax(gen,r);

* ------ Consistency checks for primary factor inputs in ele by gen
skl_test(r)      = round( sum(gen, skl_input(gen,r))    - skld0("ele",r), rd) ;
usk_test(r)      = round( sum(gen, usk_input(gen,r))    - uskd0("ele",r), rd) ;
cap_test(r)      = round( sum(gen, cap_input(gen,r))    - kd0("ele",r), rd) ;
vafm_test(i,r)   = round( sum(gen, vafm_input(i,gen,r)) - vafm(i,"ele",r), rd);

* ----- 23.08.2018 - TIMES coupling
cap_input0(gen,r) = cap_input(gen,r);

display skl_test, usk_test, cap_test, vafm_test;

* ------ Calculate output of generation technologies
out_gen(gen,r)= sum(i,[  vafm_input(i,gen,r)*(1+(ti(i,"ele",r)))] ) + skl_input(gen,r) + usk_input(gen,r) + cap_input(gen,r) 
                + (cap_input(gen,r) * rtf("cap","ele",r))$(HH_DISAG(r) AND diss_factor_tax)
                + (skl_input(gen,r) * rtf("skl","ele",r))$(HH_DISAG(r) AND diss_factor_tax)
                + (usk_input(gen,r) * rtf("usk","ele",r))$(HH_DISAG(r) AND diss_factor_tax) ;

* ##############################################################################

* ------ Store initial values
vafm_input_deu0(i,gen)  =  vafm_input(i,gen,"deu");
skl_input_deu0(gen)     =  skl_input(gen,"deu");
usk_input_deu0(gen)     =  usk_input(gen,"deu");
cap_input_deu0(gen)     =  cap_input(gen,"deu");

* ---------------------------------------------------------------------- *
* Berechnung der Parameter für die Disaggregierung der Kapitalakkumulation
* der Stromerzeugung

* Calculation of the parameters for the disaggregation of the capital
* accumulation of the electricity generation
* ---------------------------------------------------------------------- *
ks_x(gen,r)      = cap_input(gen,r)      *  thetax;
ks_n(gen,r)      = cap_input(gen,r)      *  (1 - thetax) ;

ks_x0(gen,r)     = ks_x(gen,r);

capgenx(r)       = (sum(gen, ks_x(gen,r))) ;
capgennew(r)     = (sum(gen, ks_n(gen,r))) ;
* ------------------------------------------------------------------------------
* Anpassung der capital factor income durch gen-spezifsiche kapitalstockveränderungen
* Kapitaleinsatzes im Stromsektor wird abgezogen aus dem oben berechneten capital factor income
* Fuer diesen Sektor gibt es technologiespezifische Kapitalausstattungen
* evoa muss um extant capital in ele bereinigt werden, da extant capital ele in demand block im modell separat vorkommt
* -------
* Adjustment of the capital factor income by gen-specific capital stock changes Capital use in the electricity
* sector is deducted from the capital factor income calculated above.
* For this sector, there are specific capital investments.
* take all kd0 including those for ele and take out extant ele as this is treated separately
* ------------------------------------------------------------------------------
evoa("cap",r)    = sum(i, kd0(i,r)) - capgenx(r)  ;                             //evoa: Value of factor income
* ------------------------------------------------------------------------------
chkXTANT(gen,r)  = round( cap_input(gen,r) - ks_x(gen,r) - ks_n(gen,r), rd) ;
chkfac("cap",r)  = round( sum(i, kd0(i,r)) - evoa("cap",r)- capgenx(r), rd) ;
display chkFAC, chkXTANT;
* ------------------------------------------------------------------------------
* ------ setze inputs in ele gelich null, da nun vafm_input für die einzelnen Technologien relevant
* ------ dies ist aber lediglich ein "sicherheitsschalter", denn vafm wäre nur gefragt in prod:y
* ------ für prod:y ist der sektor ele aber über $-operator ausgenommen; selbiges gilt für primärfaktoren
kd0_ele(r)       = kd0("ele",r);
skld0_ele(r)     = skld0("ele",r);
uskd0_ele(r)     = uskd0("ele",r);
vafm_ele(i,r)    = vafm(i,"ele",r) ;

skld0("ele",r) = 0 ;
uskd0("ele",r) = 0 ;
kd0("ele",r)   = 0 ;
vafm(i,"ele",r)= 0 ;

chkfac("skl",r) = round( sum(i, skld0(i,r)) + sum(gen, skl_input(gen,r)) - evoa("skl",r), rd) ;
chkfac("usk",r) = round( sum(i, uskd0(i,r)) + sum(gen, usk_input(gen,r)) - evoa("usk",r), rd) ;
chkfac("cap",r) = round( sum(i, kd0(i,r))   + sum(gen, cap_input(gen,r)) - evoa("cap",r) - capgenx(r), rd) ;
display chkFAC;
* ------------------------------------------------------------------------------


* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------------------------------------------------------------------------------
* ------ (G) Energy and fossil fuel specifications
* ------------------------------------------------------------------------------

PARAMETER
* ------ Reading in ENERGY Parameters (GTAP Energy-Data)  --> 10_16_4
*        EVF     =       Volume of (energy) input purchases by firms (Mtoe) --> 10_16_4_EnergyData.gdx
*        EVH     =       Volume of (energy) purchases by households  (Mtoe) --> 10_16_4_EnergyData.gdx
*        eind(*,*,r)     Energy input (cru oil gas col ele agr) into SPECIFIC SECTOR for this NEWAGE-model (16x16x10)
*        evf(*,*,r)      Energy input into INDUSTRIES for every original GTAP energy class
*        evh(*,r)        Energy input into FINAL DEMAND (HOUSEHOLDS) for every original GTAP energy class
         efd(*,r)        Energy input (cru oil gas col ele agr) into FINAL DEMAND for this NEWAGE-model (6x10)
         evd_sum1(i,r)    Total industry energy input (oil gas col cru ele und agr) for every region (6x10)
* ------ Energy (13.09.2015 Diss)
         enrg            Energy use
         energyinp       Energy sector inputs (echo print)
         vshare(r,i)     Energy value shares for non-energy sectors
         ele_share
         ele_share_chk
         fos_input
         fos_input_chk
* ------ Elasticities and value shares
         esub_es(i,r)    Elasticity of substitution in energy production
         eta_s(xe)       Elasticity of supply
         thetar(i,r)     Value share of resource inputs in fossil supply
* ------ Autonomous Energy Efficiency Index (AEEI)
         aeei(i,g,r)      Autonomous Energy Efficiency Index (AEEI)
         aeei_elex(gen,r) AEEI for electricity generation technologies
         aeei_elen(gen,r) AEEI for electricity generation technologies

*------- 05.10.2017	Sector-specific AEEI
	 aeei_sec(i,sec,r)	Sector-specific AEEI
;

* ##############################################################################
* ------ 26.07.2016 auskommentieren (GTAP 8 rep)
*display evd, evi;

* ------ 18.07.2016
parameter evd_sum;
evd_sum(r)       = sum((i,g), evd(i,g,r));
evd_sum("all")   = sum(r, evd_sum(r)); display evd_sum;

* ------ 26.07.2016 In filter.gms wird evd um die Werte bereinigt, die nicht in vdfm enthalten sind.
*                   Die für NEWAGE relevante Berechnung von evd, die in NEWAGE auch evi (Importe) enthält --> evd = evd + evi,
*                   darf daher erst hier vorgenommen werden, statt in flex2gdx.gms wie in GTAP8inGAMS
         evd(i,j,r)       = evd(i,j,r)   + evi(i,j,r);
         evd(i,"c",r)     = evd(i,"c",r) + evi(i,"c",r);
         evd(i,"g",r)     = evd(i,"g",r) + evi(i,"g",r);
*display evd, evi;

evd_sum(r) = 0;     evd_sum(r)       = sum((i,g), evd(i,g,r));
evd_sum("all") = 0; evd_sum("all")   = sum(r, evd_sum(r)); display evd_sum;

* ##############################################################################

* ---------------------------------------------------------------------------- *
* ------ Die Energieverbräuche werden von Mtoe in PJ umgerechnet (1 PJ = Billiarden J):
*        1 Mtoe = 41868 TJ = 41.868 PJ = 0.041868 EJ
*        1 toe = 1 Tonne Öläquivalent = 41.858 GJ  = 0.000041868 PJ
*        1 GWh = 3.6 TJ = 0,000086 Mtoe (8.6*10**-5)
*        Kilo: k = 10**3, Mega: = M = 10**6, Giga: = G = 10**9, Tera: = T = 10**12, Peta: = P = 10**15, Exa: = E = 10**18
*        [http://www.uni-klu.ac.at/iff/ikn/downloads/Teil-04_Grundlagen_der_Energieversorgung.pdf]
* ---------------------------------------------------------------------------- *


* ------ 11.06.2014
* ------ Die Energieverbräuche werden von Mtoe in EJ umgerechnet (1 PJ = Billiarden J):
*        1 Mtoe = 41868 TJ = 41.868 PJ = 0.041868 EJ
*        1 toe = 1 Tonne Öläquivalent = 41.858 GJ  = 0.000041868 PJ
display evd, evt;
evd(i,g,r)      = evd(i,g,r) * 0.041868  ;
* ------ 26.07.2016 Folgende Zeile auskommentiert  (GTAP8 rep)
evi(i,g,r)      = evi(i,g,r) * 0.041868  ;
evt(i,r,s)      = evt(i,r,s) * 0.041868  ;
display evd, evt;

evd_sum(r) = 0;     evd_sum(r)       = sum((i,g), evd(i,g,r));
evd_sum("all") = 0; evd_sum("all")   = sum(r, evd_sum(r)); display evd_sum;

* ------ 03.07.2014 Weltweiter Energieverbrauch war 2011 nach EIA ca. 13620 mtoe = 570,25 EJ
* ------ http://www.eia.gov/forecasts/aeo/data/browser/#/?id=1-IEO2016&sourcekey=0

display evd;
*evd(i,g,r) = 10000 * evd(i,g,r);         // Wichtige Skalierung   EJ bzw. Mio. tCO2
* ------ 26.07.2016 auskommentiert
* ------ 26.07.2016 Folgende Zeile wieder einkommentiert  (GTAP8 rep)
*evd(i,g,r) = 10 * evd(i,g,r);            // Wichtige Skalierung   ZJ bzw. Mrd. tCO2

evd_sum(r) = 0;     evd_sum(r)       = sum((i,g), evd(i,g,r));
evd_sum("all") = 0; evd_sum("all")   = sum(r, evd_sum(r)); display evd_sum;

* ------ 20.05.2015
parameter evd_ele;
evd_ele(i,r) = evd("ele",i,r) / sum(gg, evd("ele",gg,r));
*display evd_ele;

* ##############################################################################

efd(i,r)        = evd(i,"c",r) + evd(i,"g",r);
*evd(i,j,r)      = evd(i,j,r) * 41.868  ;         // Die Energieverbräuche werden von Mtoe in PJ umgerechnet
*efd(i,r)        = efd(i,r)   * 41.868  ;         // Die Energieverbräuche werden von Mtoe in PJ umgerechnet

*evd(i,j,r)      = evd(i,j,r) * 0.041868  ;       // Die Energieverbräuche werden von Mtoe in EJ umgerechnet
*efd(i,r)        = efd(i,r)   * 0.041868  ;       // Die Energieverbräuche werden von Mtoe in EJ umgerechnet
evd_sum1(i,r)    = sum(j, evd(i,j,r)) ;

*display efd, evd, evd_sum1, evt;

* ------ Elasticities of fossil fuel supply:
eta_s("cru")  = 1;
eta_s("gas")  = 1;
eta_s("col")  = 1;

* ------ Compute elasticity of resource production / extraction
thetar(xe,r)$VOM(xe,R)           = rd0(xe,r) / (vom(xe,r) * (1-ty(xe,r)));
esub_es(xe,R)$(1-thetar(xe,r))   = thetar(xe,r) * eta_s(xe) / (1-thetar(xe,r));

* ------  (13.09.2015 Diss) Generate a report of energy use in physical units:
enrg(r,i,j)       = round(evd(i,j,r), 1);                                    // NEWAGE-7: eind(i,j,r)
enrg(r,i,"final") = round(efd(i,r), 1);                                       // NEWAGE-7: efd(fe,r)

* ------ 21.05.2014
*OPTION   energy:1:1:1;

* ------ Energy value shares in non-energy production:
vshare(r,i)$(vom(i,r) and (not e(i))) = round( 100 * sum(e, vafm(e,i,r)*(1+ti(e,i,r))) / vom(i,r), 1);

* ------ Echo print inputs into energy sectors
energyinp(e,j,r)$vom(e,r)        = vafm(j,e,r)   / vom(e,r);
energyinp(e,"skl",r)$vom(e,r)    = skld0(e,r)    / vom(e,r);
energyinp(e,"usk",r)$vom(e,r)    = uskd0(e,r)    / vom(e,r);
energyinp(e,"k",r)$vom(e,r)      = kd0(e,r)      / vom(e,r);
energyinp(e,"r",r)$vom(e,r)      = rd0(e,r)      / vom(e,r);
*display eta_s, thetar, esub_es, vshare, energy, energyinp;

* ------ 7.03.2013: Weiter vereinfachte SET-schreibweise
* ------ All input that COL/GAS/OIL delivers to ELE must equal the sum of all COL/GAS/OIL inputs in coal/gas/oil technologies
* ------ COL does not deliver to OIL and GAS etc.
* ------ Why does not CRU deliver to oil technologies?
fos_input(i,r)$col(i) = sum(gen$foscoal(gen), vafm_input(i,gen,r)) ;
fos_input(i,r)$gas(i) = sum(gen$fosgas(gen),  vafm_input(i,gen,r)) ;
fos_input(i,r)$oil(i) = sum(gen$fosoil(gen),  vafm_input(i,gen,r)) ;
fos_input_chk(i,r)$fos_input(i,r) = round( vafm(i,"ele",r) - fos_input(i,r), rd) ;
*display vafm_ele, fos_input, fos_input_chk ;

* ------ 7.03.2013: Weiter vereinfachte SET-schreibweise
ele_share(i,gen,R)$(fos_input(i,r) and foscoal(gen))=  vafm_input(i,gen,r) / fos_input(i,r) ;
ele_share(i,gen,R)$(fos_input(i,r) and fosgas(gen)) =  vafm_input(i,gen,r) / fos_input(i,r) ;
ele_share(i,gen,R)$(fos_input(i,r) and fosoil(gen)) =  vafm_input(i,gen,r) / fos_input(i,r) ;
ele_share(i,"TOTAL",R) = sum(gen, ele_share(i,gen,r)) ;
ele_share_chk(i,r)$ele_share(i,"TOTAL",R) = round( ele_share(i,"TOTAL",R) - 1, rd) ;
display ele_share, ele_share_chk;


* ------ 18.06.2014: Set AEEI to 1 in Benchmark case
aeei(i,g,r)      = 1 ;
aeei_elex(gen,r) = 1 ;
aeei_elen(gen,r) = 1 ;

*--------05.10.2017	Set aeei_sec to 1 in benchmark case
aeei_sec(i,sec,r)	= 1;


* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------------------------------------------------------------------------------
* ------ (H) CO2-Emissions
* ------------------------------------------------------------------------------
* Generate base year carbon emissions by sector and then echo-print some
* benchmark statistics:
* co2em bmk emissions (vorher carbon97) are needed for definition of co2pfad
* total emissions over all carriers and all indutsries plus final demand are also reported by carbon_bmk
* ---------------------------------------------------------------------- *

*OPTION   decimals = 6;

PARAMETER
* ------ cecphys(*) ist kalibriert auf 901 t in 2001 DEU (04.06.) mischung stadt- und erdgas und auf über 30 Gt welt                     // NEWAGE7 ORIGINAL
*         cecphys(*)              Carbon emission coefficients for physical units [Mio. tC per PJ]  [C=Kohlenstoff]                           // NEWAGE7 ORIGINAL
*                                                        / col 0.024, gas 0.0135, oil 0.0183, hc 0.0227, bc 0.0267, ccs 0.002951 /       // NEWAGE7 ORIGINAL

* --->>> Vgl. Küster (2009) Diss., S. 106   [Mio. tCO2/PJ]    / col 0.090, gas 0.0495, oil 0.0671, hc 0.0832, bc 0.0972, ccs 0.0110 /
* --->>> Impliziter Umrechnungsfaktor: tCO2/tC = 3.7 --> ccs 0.002951 Mio. tC/PJ * 3.7 Mio. tCO2/tC = 0.0110

* ------ 10.06.2014
* UBA: http://www.umweltbundesamt.de/themen/klima-energie/klimaschutz-energiepolitik-in-deutschland/treibhausgas-emissionen
* UBA: http://www.umweltbundesamt.de/sites/default/files/medien/377/dokumente/co2_faktoren_brennstoffe.xls
* --->>> Braunkohle [BC]:       112 t CO2/TJ öffentliche Fernheizwerke + Industrie, Kleinverbrauch Deutschland
* --->>> Steinkohle [HC]:        94 t CO2/TJ Steinkohle roh (Kraftwerke, Industrie)
* --->>> Mineralöl [OIL]:        75 t CO2/TJ Durchschnitt über alle Mineralölprodukte
* --->>> Erdgas [GAS]:           56 t CO2/TJ Erdgas Deutschland
* --->>> Kohle [COL]:           103 t CO2/TJ Durchschnitt BC und HC
* --->>> CCS [CCS]:          ca. 11 t CO2/TJ Durchschnitt BC und HC --> Impliziter Umrechnungsfaktor Küster Diss, S. 106: tCO2/tC = 3.7 --> ccs 0.002951 tC/PJ * 3.7 tCO2/tC = 0.0110
         cecphys(*)              Carbon emission coefficients for physical units [tCO2 per TJ = Mio. tCO2 per EJ]
                                 / hc 94, bc 110, col 98, gas 56, oil 75, ccs 11 /              // Quelle: UBA s.o.
*                                / col 0.024, gas 0.0135, oil 0.0183, hc 0.0227, bc 0.0267, ccs 0.002951 /       // NEWAGE7 ORIGINAL
         co2em(i,*,r)            CO2 emissions (by energy carrier) [Mio. tCO2]
         co2em_ele               CO2 emissions from ELE
         co2em_coal_chk(r)
         co2em_gas_chk(r)
         co2em_oil_chk(r)
         carblim(r)              Carbon emission limit
         carblim_ets(r)          Carbon emission limit for sectors ets
         carblim0(r)             Carbon emission limit (benchmark)
         carblim0_world          Carbon emission limit as world sum (benchmark)
         carblim_ets0(r)         Carbon emission limit for sectors ets (benchmark)
         carbshare               Relative share of a specific sector in total emissions
         carbon_bmk(r,*)         2001 Carbon Emissions (IEA) GT Kohlenstoff (GT C)
         carbon_ETS_bmk(r,*)     2001 Carbon Emissions (IEA) GT Kohlenstoff (GT C) in ETS sectors

* ----- 11.09.17 sector-specific emissions
    carblim_sec(*,r)  sector-specific carbon emission limits for Germany
    carblim_sec0(*,r) sector-specific carbon emission limits for Germany (benchmark)

* ------ 26.01.2018 make carblim for germany, so it can have a separate target - only active if "detrade" is active
    carblim_deu		carbon emission limits for Germany (over all sectors)
    carblim_deu0	carbon emission limits for Germany (over all sectors) (benchmark)

    tax_delta(r)

;

* ------ 26.07.2016 Mrd. tCO2 statt Mio. tCO2  [change unit from "Million tCO2 per EJ" to "Billion tCO2 per EJ"]
* ------ 26.07.2016 Folgender Block GTAP8 rep auskommentiert
*$ontext
cecphys("hc") = cecphys("hc") / 1000 ;
cecphys("bc") = cecphys("bc") / 1000 ;
cecphys("col") = cecphys("col") / 1000 ;
cecphys("gas") = cecphys("gas") / 1000 ;
cecphys("oil") = cecphys("oil") / 1000 ;
cecphys("ccs") = cecphys("ccs") / 1000 ;
display cecphys;

*$offtext

* ------ 27.02.2015 Die CO2-Emissionen fallen nur dort an, wo es auch wirklich Nachfrage gibt (z.B. gas.gas.FRA --> kein Eintrag in vafm aber es wäre einer in co2em)
* ------ 26.07.2016 Folgende zwei Zeilen einkommentiert (GTAP8 rep)
co2em(fe,i,r)$vafm(fe,i,r)  = cecphys(fe) * evd(fe,i,r) ;           // Mio. tCO2/EJ * EJ = Mio. tCO2 // NEWAGE-7: eind(i,j,r)
co2em(fe,"final",r)$c0(fe,r)= cecphys(fe) * efd(fe,r) ;             // Mio. tCO2/EJ * EJ = Mio. tCO2 //(carbcoef = co2em); NEWAGE-7: efd(fe,r)

display co2em;

tax_delta(r)$HH_DISAG(r) = 1;

* ------ 26.07.2016 Hinzugefügt bzw. CO2-Emissionen neu definiert
* ------ 26.07.2016 Folgender Block auskommentiert (GTAP8 rep)
$ontext
parameter efi(i,r);
efi(i,r)        = evi(i,"c",r) + evi(i,"g",r);
co2em(fe,i,r)$vafm(fe,i,r)  = cecphys(fe) * (evd(fe,i,r) + evi(fe,i,r));           // Mio. tCO2/EJ * EJ = Mio. tCO2 // NEWAGE-7: eind(i,j,r)
co2em(fe,"final",r)$c0(fe,r)= cecphys(fe) * (efd(fe,r) + efi(fe,r)) ;             // Mio. tCO2/EJ * EJ = Mio. tCO2 //(carbcoef = co2em); NEWAGE-7: efd(fe,r)

co2em(fe,"c",r)$(HH_DISAG(r) AND c_hh0(fe,r)) = cecphys(fe) * (evi(fe,"c",r) + evd(fe,"c",r));
co2em(fe,"g",r)$(HH_DISAG(r) AND c_gov0(fe,r)) = cecphys(fe) * (evi(fe,"g",r) + evd(fe,"g",r));

display co2em, efd, efi, evd;

*$exit
$offtext

* ------ Electricity (NEWAGE8 calculation routine)
co2em(i,gen,r)$(col(i) and foscoalb(gen))= cecphys("bc") * ele_share(i,gen,r) * evd(i,"ele",r) ;   // NEWAGE-7: eind(i,j,r)
co2em(i,gen,r)$(col(i) and foscoalh(gen))= cecphys("hc") * ele_share(i,gen,r) * evd(i,"ele",r) ;    // NEWAGE-7: eind(i,j,r)
co2em(i,gen,r)$(col(i) and fosccs(gen))  = cecphys("CCS")* ele_share(i,gen,r) * evd(i,"ele",r) ;    // NEWAGE-7: eind(i,j,r)
co2em(i,gen,r)$(gas(i) and fosgas(gen))  = cecphys("gas")* ele_share(i,gen,r) * evd(i,"ele",r) ;    // NEWAGE-7: eind(i,j,r)
co2em(i,gen,r)$(oil(i) and fosoil(gen))  = cecphys("oil")* ele_share(i,gen,r) * evd(i,"ele",r) ;    // NEWAGE-7: eind(i,j,r)
co2em(i,"sumGEN",r)       = sum(gen, co2em(i,gen,r));
co2em(i,"ele",r)         = co2em(i,"sumGEN",r);
co2em_ele(fe,r)          = co2em(fe,"ele",r);
co2em_ele("all",r)       = sum(fe, co2em(fe,"ele",r));
co2em_ele("all","all")   = sum(r, co2em_ele("all",r));
display co2em, co2em_ele;


*co2em(i,"TOTAL",r)       = 0;
* ------ Benchmark
carbon_bmk(r,fe)         = co2em(fe,"final",r) + sum(i, co2em(fe,i,r));          // neu
carbon_bmk(r,"total")    = sum(fe, carbon_bmk(r,fe));                            // neu
carbon_ETS_BMK(r,fe)     = sum(i$ETS(I), co2em(fe,i,r));                         // neu
carbon_ETS_BMK(r,"total")= sum(fe, carbon_ETS_bmk(r,fe));                        // neu
* ------ Other
carbshare(r,"final")     = sum(fe, co2em(fe,"final",r)) / carbon_bmk(r,"total"); // neu
carbshare(r,i)           = sum(fe, co2em(fe,i,r)) / carbon_bmk(r,"total");       // neu
co2em_coal_chk(r)        = co2em("col","total",r) - co2em("col","ele",r) ;
co2em_gas_chk(r)         = co2em("gas","total",r) - co2em("gas","ele",r) ;
co2em_oil_chk(r)         = co2em("oil","total",r) - co2em("oil","ele",r) ;
display co2em_coal_chk, co2em_gas_chk, co2em_oil_chk;

display co2em, co2em_ele, cecphys, evd, efd;

* ------ 03.07.2014 Weltweiter Energieverbrauch war 2011 nach EIA ca. 13620 mtoe = 570,25 EJ
* ------ http://www.eia.gov/forecasts/aeo/data/browser/#/?id=1-IEO2016&sourcekey=0
parameter evd_total in ZJ;
evd_total(r) = sum((i,g), evd(i,g,r)); display evd_total;
evd_total("World") = sum(r, evd_total(r)); display evd_total;

* ------ 26.07.2016  Umrechnung von Mio. tCO2 in Mrd. tCO2 bzw. Gt CO2
*co2em(fe,i,r) = co2em(fe,i,r) / 1000 ;
*co2em(i,gen,r)= co2em(i,gen,r) / 1000 ;
*co2em(i,"sumGEN",r) = co2em(i,"sumGEN",r) / 1000;
*co2em(i,"ele",r) = co2em(i,"ele",r) / 1000;
*co2em(i,"ele",r) = co2em(i,"ele",r) / 1000;
*co2em_ele(fe,r) = co2em_ele(fe,r) / 1000;
*co2em(fe,"final",r) = co2em(fe,"final",r) / 1000 ;


* ------ http://de.wikipedia.org/wiki/Liste_der_gr%C3%B6%C3%9Ften_Kohlenstoffdioxidemittenten
parameter co2em_total in Mrd. tCO2;
co2em_total(r) = sum((i,j), co2em(i,j,r)) + sum(fe, co2em(fe,"final",r));
co2em_total("World") = sum(r, co2em_total(r)); display co2em_total;
display co2em_total;

* ------ 1.08.2016
parameter co2em_final; co2em_final("final",r) = sum(fe, co2em(fe,"final",r)) ;
display co2em_final;


parameter coco;
coco(r) = sum(fe, co2em(fe,"final",r));
*display coco;

* ##############################################################################
* ------ 25.07.2016 CO2-Emissionen aus GTPA9 entnehmen und nicht bottom-up über evd und cecphys herleiten!
*$ontext
parameter eco2, eco2_all, eco2d_all, eco2i_all, evd_all, evi_all, eco2_cru_diff;

* ------ 08.08.2016 Store eco2d original values
Parameter
         eco2d0          Store eco2d GTAP9 original values
         eco2i0          Store eco2i GTAP9 original values
         CHK_eco2        Check eco2 parameter (should be zero)
         co2em_ele_share Technologies CO2 emissions share per fuel
         chk_co2em_ele   Check co2-emissions in ELE (should be zero)
;

* ------ 1.08.2016 Define eco2d0 and eco2i0 and scale eco2d and eco2i to Billion t CO2 instead of Mt CO2
eco2d0(i,g,r) = eco2d(i,g,r);
eco2i0(i,g,r) = eco2i(i,g,r);
eco2d(i,g,r) = eco2d(i,g,r) / 1000;
eco2i(i,g,r) = eco2i(i,g,r) / 1000;

* ------ 1.08.2016 eco2 is the sum of domestic plus imported co2-emissions
eco2(i,g,r) = eco2d(i,g,r) + eco2i(i,g,r); display eco2;
* ------ Check eco2
CHK_eco2 = sum((r), eco2d("cru","ele",r) + eco2i("cru","ele",r) + eco2d("oil","ele",r) + eco2i("oil","ele",r)) - sum((r), eco2("cru","ele",r) + eco2("oil","ele",r)); display CHK_eco2;

* ------ 8.08.2016 Move CO2 emissions from CRU to OIL (because NEWAGE is defined to only have emissions from OIL not from CRU)
eco2("oil",g,r) = eco2("oil",g,r) + eco2("cru",g,r);
eco2("cru",g,r) = 0;
* ------ Check eco2
CHK_eco2 = sum((r), eco2d("cru","ele",r) + eco2i("cru","ele",r) + eco2d("oil","ele",r) + eco2i("oil","ele",r)) - sum((r), eco2("cru","ele",r) + eco2("oil","ele",r)); display CHK_eco2;

* ------ 8.08.2016 calculate difference of "new" and "old" CO2-emissions from OIL
eco2_cru_diff(r) = (sum(g, eco2("oil",g,r)) - sum(g, eco2d("oil",g,r) + eco2i("oil",g,r))) * 1000; // in Mio. t instead of Bn. t CO2

* ------ 8.08.2016 Calculate sums
eco2d_all(g,r)           = sum(i,        eco2d(i,g,r));
eco2d_all("all",r)       = sum((i,g),    eco2d(i,g,r));
eco2d_all("all","all")   = sum((i,g,r),  eco2d(i,g,r));
eco2i_all(g,r)           = sum(i,        eco2i(i,g,r));
eco2i_all("all",r)       = sum((i,g),    eco2i(i,g,r));
eco2i_all("all","all")   = sum((i,g,r),  eco2i(i,g,r));
eco2_all(g,r)            = sum(i,        eco2(i,g,r));
eco2_all("all",r)        = sum((i,g),    eco2(i,g,r));
eco2_all("all","all")    = sum((i,g,r),  eco2(i,g,r));
evd_all(g,r)             = sum(i,        evd(i,g,r));
evd_all("all",r)         = sum((i,g),    evd(i,g,r));
evi_all(g,r)             = sum(i,        evi(i,g,r));
evi_all("all",r)         = sum((i,g),    evi(i,g,r));
display eco2d, eco2i, eco2, eco2_all, eco2d_all, eco2i_all, evd_all, evi_all, eco2_cru_diff;

* ------ 8.08.2016 Calculate CO2 emissions for the electricity technologies based on NEWAGE8 co2em bottom-up routine (s. above)
co2em_ele_share(i,gen,r)$co2em(i,gen,r) = co2em(i,gen,r) / sum(gengen, co2em(i,gengen,r)) ;
co2em_ele_share(i,"not_balanced",r)$sum(gen, co2em_ele_share(i,gen,r)) = round(sum(gen, co2em_ele_share(i,gen,r)) - 1, 10); // if balanced, there is no entry in "not_balanced"
display co2em_ele_share;

* ------ 1.08.2016 Adjust CO2 Emissions to GTAP9 data source eco2d + eco2i
* ------ 8.08.2016 Reset co2em to zero
co2em(i,j,r)       = 0;
co2em(i,"final",r) = 0;
co2em(i,gen,r)     = 0;
co2em(i,"sumGEN",r)= 0;
co2em_total(r) = 0;
display co2em, co2em_total;

* ------ 1.08.2016 Industries j
co2em(i,j,r)     = eco2(i,j,r);
* ------ 1.08.2016 Final demand
co2em(i,"final",r) = eco2(i,"g",r) + eco2(i,"c",r) ;

* ------ 21.02.2019 for household disaggregation
co2em(i,"c",r)$(HH_DISAG(r)) = eco2(i,"c",r) ;
co2em(i,"g",r)$(HH_DISAG(r)) = eco2(i,"g",r);

* ------ 8.08.2016 Apply co2em_ele_share(i,gen,r) to the sum of eco2d(i,"ele",r) and eco2i(i,"ele",r) per fuel
co2em(i,gen,r)     = co2em_ele_share(i,gen,r) * (eco2(i,"ele",r)) ;

* ------ 8.08.2016 Check co2em (should be zero)
chk_co2em_ele(r) = 0; chk_co2em_ele(r) = round(sum((i,gen), co2em(i,gen,r)) - sum(i, eco2(i,"ele",r)) ,8); display chk_co2em_ele;
chk_co2em_ele(r) = 0; chk_co2em_ele(r) = round(sum((i,gen), co2em(i,gen,r)) - (sum(i, (eco2d(i,"ele",r) + eco2i(i,"ele",r)))),8); display chk_co2em_ele;
chk_co2em_ele("all") = sum(r, chk_co2em_ele(r)); display chk_co2em_ele;
chk_co2em_ele(r) = 0; chk_co2em_ele(r) = round(sum((i,gen), co2em(i,gen,r)) - sum(i, (eco2d0(i,"ele",r) + eco2i0(i,"ele",r))/1000) ,8); display chk_co2em_ele;

* ------ 8.08.2016 Calculate co2em sum over gen and calculate co2em_ele
co2em(i,"j_sum",r)       = sum(j,   co2em(i,j,r));
co2em(i,"gen_sum",r)     = sum(gen, co2em(i,gen,r));
co2em(i,"ele",r)         = co2em(i,"gen_sum",r);
co2em_ele(fe,r)          = co2em(fe,"ele",r);
co2em_ele("all",r)       = sum(fe, co2em_ele(fe,r));
co2em_ele("all","all")   = sum(r,  co2em_ele("all",r));
* ------ 8.08.2016 Total CO2 emissions
co2em_total(r)$(not HH_DISAG(r)) = sum((i,j), co2em(i,j,r)) + sum(fe, co2em(fe,"final",r));

co2em_total("World") = sum(r, co2em_total(r));

display co2em_ele_share, eco2d, eco2i, co2em, co2em_ele, co2em_total;

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------------------------------------------------------------------------------
* ------ (I) Additional parameters from EXCEL
* ----->>>>  Labour, CO2, ELE costs + potentials + decommissioning, AEEI, pricetargets
* ------------------------------------------------------------------------------

PARAMETER
         size_usk(*,*)                   Wachstumsparameter für unskilled labor (lfhc_usk + tfp)
         size_skl(*,*)                   Wachstumsparameter für skilled labor (lfhc_skl + tfp)
* ------ 15.07.2014 Energy productivity and total factor productivity development (BASELINE Database)
         lfhc_usk(r,yr)                  Labor force and human capital development for unskilled labor (BASELINE Database)
         lfhc_skl(r,yr)                  Labor force and human capital development for   skilled labor (BASELINE Database)
         ep(yr,r)                        Energy productivity development (BASELINE Database)
         tfp(yr,r)                       Total factor productivity development (BASELINE Database)

         tfp_corona(yr,r)                   Multiplier for TFP to match the GDP development affected by the COVID-19 pandemic

         ele_prod(gen,r)                 Benchmark electricity production [TWh]
         ele_prod_costs
         ressize(r,gen,yr)               Growth rates for renewable energy sources
         co2pfad_ets_eu28_ref(*,yr)      CO2 pathway for ETS sectors in the EU for Reference scenario 2020
         co2pfad(r,yr)                   CO2-growth
         co2pfad_ets(r,yr)               CO2-growth in the EU-ETS (regional)
         co2pfad_ets_eu(*,yr)            CO2-growth in the whole EU-ETS
         co2pfad_E3_eu(*,yr)		CO2-growth for EU in ENavi E3 (strong EU) scenario --> 80% reduction in 2050 compared to 1990
         co2pfad_E4_eu(*,yr)		CO2-growth for EU in ENavi E4 (super strong EU) scenario --> 95% reduction in 2050 compared to 1990
         co2pfad_nets(r,yr)              CO2-growth in the EU non-ETS-sectors (regional)

         co2pfad_ets_row(r,yr)          CO2-growth in the RoW (non-EU)
         co2pfad_ets_row_2dc(r,yr)      CO2-growth in the non-EU contries for a 2 dC target
         co2pfad_nonets(r,yr)           CO2-growth for non-ets sectors in EU countries
         co2pfad_nonets_2dc(r,yr)       CO2- growth for non-ets sectors in EU countries for a 2 dc target

         co2pfad_reeem_ind(yr)

* ----- 05-02-2018 co2-growth for cluster union from REEEM
         co2pfad_cluni_ets(*,yr)        CO2-growth for ets sectors on cluster union REEEM scenario
         co2pfad_cluni_nonets(r,yr)     CO2-growth for non-ets sectors on cluster union REEEM scenario

*--------30.10.2017 CO2 growth
	co2pfad_eu(*,yr)		CO2-growth in the EU (all sectors)

	co2pfad_eu_ets(*,yr)		CO2-growth in the EU (ETS sectors)

	co2pfad_eu_nonets(r,yr)		CO2-growth in the EU (non ETS sectors) - INDC scenario
	co2pfad_eu_nonets_2d(r,yr)	CO2-growth in the EU (non ETS sectors) - 2 degree scenario

	co2pfad_row(r,yr)		CO2-growth in the RoW (non-EU) - INDC scenario
	co2pfad_row_2d(r,yr)		CO2-growth in the RoW (non-EU) - 2 degree scenario

	co2pfad_DEU(sec,yr)		sector-specific CO2-growth in Germany

* ------ 20.02.2018
    co2pfad_regpush(r,yr)            CO2-growth for countries outside the eU28

* ----- 05.03.2018 CO2-growth path for ets sectors under the tighther cap scenario
         co2pfad_REEEM_ets90(*,yr)
         co2pfad_REEEM_ets95(*,yr)
         co2pfad_REEEM_ALL90(*,yr)
         co2pfad_REEEM_ALL95(*,yr)

*-------30.10.2017 end

         urun0(r)                        Initial unemployment rate among unskilled
         ursk0(r)                        Initial unemployment rate among skilled
         un_numb0(r)                     Initial employment supply (potential) absolute unskilled
         sk_numb0(r)                     Initial employment supply (potential) absolute skilled
         labsize(r,yr)                   Changes in economically active population based ilo from 2020 onward constant
         ur_miss(r)                      Average unemployment rate for missing countries

         diffcost(gen,r)                 Differenzkosten
         cost_red(gen,yr)                Renewable energies cost reduction
         abschreibung(r,gen,yr)          Decommissioning geg. vorperiode
         abschreibung_z(r,gen,yr)        Decommissioning geg. vorperiode für länder ohne daten über sterbekurven
         abschreibung_bmk(r,gen,yr)      Decommissioning geg. basis-bmk
         abschreibung_bmk_z(r,gen,yr)    Decommissioning geg. basis-bmk für länder ohne daten über sterbekurven

         abschreibung_noCOAL(r,gen,yr)      Decommissioning geg. vorperiode (for coal decomissioning till 2030 in germany)
         abschreibung_bmk_noCOAL(r,gen,yr)  Decommissioning geg. basis-bmk (for coal decomissioning till 2030 in germany)

         aeei_ind_ff(r,yr)                  AEEI exogen for industrial sectors (especially for kopernikus project)
         aeei_ind_ele(r,yr)                 AEEI exogen for industrial sectors (especially for kopernikus project)
         AEEI_c_ele(r,yr)                   AEEI exogen for residential sector (especially for kopernikus project)
         AEEI_c_ff(r,yr)                    AEEI exogen for residential sector (especially for kopernikus project)
         AEEI_trans_ele(r,yr)               AEEI exogen for residential sector (especially for kopernikus project)
         AEEI_trans_ff(r,yr)                AEEI exogen for residential sector (especially for kopernikus project)
         AEEI_ser_ele(r,yr)                 AEEI exogen for residential sector (especially for kopernikus project)
         AEEI_ser_ff(r,yr)                  AEEI exogen for residential sector (especially for kopernikus project)
         AEEI_ele(r,*,yr)
         AEEI_ff(r,*,yr)

         wg0(gen,r)                      Wirkungsgrad im bmk
         wg_yr(gen,yr)                   Wirkungsgrad nach jahren jeweils bat
         nucsize(r,yr)                   Ausbau- bzw. Rückbaupfad Kernenergie
         diffcost_exogen(r,gen)
         pytarget_yr(r,i,yr)             Exogenous price paths for resources
         pytarget_E3_yr(r,i,yr)          Exogenous price paths for resources in scneario E3 from eNavi
         elast_ene_yr(r,yr)              Elasticity for energy inputs per year

         savrt


* ------ 23.08.2018 test times         
         ele_dev(GEN,r,yr)
         ele_dev_REEEM_path(GEN,r,yr)
         ele_dev_REEEM(GEN,r,yr)
         ele_calib_2015(gen,r)
         ele_dev_switch(*,*)

* --- PARAMETERS FOR ARIADNE SCENARIOS (06.11.2020)
* ----- AP5

* ------- CO2 Pathways
        CO2_AP5_sce7(*,*,yr)                "ARIADNE - AP5 - CO2 emissions pathway for scenario 7 (06.11.2020) [REG*TYPE*YEAR]"

        CO2_AP5_world(r,yr)                 "ARIADNE - AP5 - CO2 emissions pathway for non-EU regions (09.11.2020) [REG*TYPE*YEAR]"


;

* ##############################################################################
* ------ READING DATA ----------------------------------------------------------

* ------ GDX_IN --- 19.05.2014
* ------ 26.07.2016 Folgende Zeile für GTAP8 einkommentieren
*$GDXIN   %datadir%xlsdata_%output%.gdx                                    //GDXin
* ------ 18.07.2016
* ------ 26.07.2016 Folgende Zeile für GTAP9 einkommentieren
$GDXIN   %datadir%%output%_XCEL.gdx                                    //GDXin

*$GDXIN   %datadir%newage_dataload_26x19x4.gdx                                    //GDXin
* ------ LOAD
$LOAD    ressize
$LOAD    co2pfad_ets_eu28_ref
$LOAD    URUN0  URSK0
*$LOAD    un_numb0  sk_numb0  labsize
$LOAD    diffcost  cost_red
$LOAD    abschreibung  abschreibung_bmk  abschreibung_z  abschreibung_bmk_z  abschreibung_bmk_noCOAL  abschreibung_noCOAL
$LOAD    aeei_ind_ff aeei_c_ff aeei_trans_ff aeei_ser_ff
$LOAD    aeei_ind_ele aeei_c_ele aeei_trans_ele aeei_ser_ele
$LOAD    aeei_ele aeei_ff
$LOAD    wg0   wg_yr
$LOAD    nucsize
$LOAD    pytarget_yr pytarget_E3_yr
$LOAD    DIFFCOST_EXOGEN
$LOAD    elast_ene_yr

* ------ 15.07.2014
$LOAD    lfhc_usk  lfhc_skl
$LOAD    ep  tfp tfp_corona
$load    ele_dev_REEEM ele_calib_2015 ele_dev_switch ele_dev ele_dev_REEEM_path

$LOAD CO2_AP5_sce7
$LOAD CO2_AP5_world



$GDXIN %datadir%%source%_ELE.gdx
$LOAD ele_prod ele_prod_costs


display ep, tfp;


savrt(r) = save(r) / sum(g$(not i(g)), vom(g,r)); display savrt;

* ----- 23.08.2018 - TIMES coupling - the next block changes the ele_prod parameter

loop ((gen,r),
    if ((ele_prod(gen,r) = Eps) and (ele_dev_switch(gen,r) = 1),
        ele_prod(gen,r) = 1;)
    );

loop ((gen,r,yr),
    if ((ele_dev(gen,r,yr) < 0) and (ele_dev_switch(gen,r) = 1),
        ele_dev(gen,r,yr) = 0;)
    );

* ------ 15.07.2014 Add tfp growth to lfhc_usk and lfhc_skl
size_usk(r,yr)   = lfhc_usk(r,yr) + (tfp(yr,r) - 1);
size_skl(r,yr)   = lfhc_skl(r,yr) + (tfp(yr,r) - 1);


* ----- 23.09.2018  - TIMES coupling - this parameter can be used if decided that the electricity production will be controlled through an auxiliary constraint in the MPSGE block
parameter
    ele_dev_act(GEN,r)  ;

* ##############################################################################
* ------ Specifying parameters -------------------------------------------------

* ------ Initial benchmark prices for wage curve formulation:
parameter
         psklbmk(r)
         puskbmk(r)
         pcbmk(r)
         ;


* ------ hierdurch verschwindet das Jahr 2004, weil dort alle Werte auf 1 waren --> (4x10x10)
ressize(r,gen,yr)$ressize(r,gen,yr) = ressize(r,gen,yr) - 1 ;

* ------ Nucsize specification
nucsize(r,yr)$(nucpot(r)) = nucsize(r,yr) - 1;
* ------ Nucout in DEU and BAW
nucsize(r,yr)$(nucout(r) and after(yr)) = 0;
* ------ 03.03.2017 get values for coal decomissioning DEU 2030, in case the switch is on
abschreibung(r,gen,yr)$(noCOAL_DEU AND (diss_ref OR diss_VAT OR diss_CAP OR diss_BAU OR diss_inv_payment OR diss_LAB)) = abschreibung_noCOAL(r,gen,yr);
abschreibung_BMK(r,gen,yr)$(noCOAL_DEU AND (diss_ref OR diss_VAT OR diss_CAP OR diss_BAU OR diss_inv_payment OR diss_LAB)) = abschreibung_bmk_noCOAL(r,gen,yr);

* ------ Define abschreibung for decomissioning curves for other regions than EU28
abschreibung(r,gen,yr)$(NOT (EU28(r) or deu(r)))     = abschreibung_z(r,gen,yr);
abschreibung_BMK(r,gen,yr)$(NOT (EU28(r) or deu(r))) = abschreibung_BMK_Z(r,gen,yr);
abschreibung(r,"bCCS",yr)                = 0;
abschreibung(r,"mCCS",yr)                = 0;
abschreibung_BMK(r,"bCCS",yr)            = 1;
abschreibung_BMK(r,"mCCS",yr)            = 1;

* ------ 14.10.2014
parameter abschr_pfad(r,gen,yr), ks_x_yr2;

abschr_pfad(r,gen,"2011") = 1;

loop(yr$after(yr),abschr_pfad(r,gen,yr) = abschr_pfad(r,gen,yr-1) * (1 - abschreibung(r,gen,yr)););

ks_x_yr2(r,gen,"2011") = ks_x0(gen,r);

loop(yr$after(yr), ks_x_yr2(r,gen,yr) = ks_x0(gen,r) * abschr_pfad(r,gen,yr));

*display abschreibung, abschr_pfad, ks_x_yr2, ks_x0;

* ------ 16.02.2015 Define CRU-Price in 2050 same as 2045:
*pytarget_yr(r,i,"2050") = 0.9 * pytarget_yr(r,i,"2045") ;
* ------ TEXTMARKE RWE

*display nucsize, ressize, co2pfad, co2pfad_ets, cost_red, abschreibung, abschreibung_z, abschreibung_bmk, abschreibung_bmk_z, wg_yr, pytarget_yr;
*display size_usk, abschreibung;


* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------------------------------------------------------------------------------
* ------ (J) Scenario Switches - extra definitions [Quotas/Flags/Shares]
* ------------------------------------------------------------------------------

* ------ 2.02.2016
notrad(r)$(eu28(r) and notrade) = yes ;

*------22.08.2017: needed for additional switch for CO2 price in ROW
notrad(r)$(not eu28(r) and row_notrade and not (deu(r) and DEU_sec)) = yes;

display notrad;

* ------ 24.07.2014 Switch on BAW Non-Ets Carbon reduction if SC2 = 1
*bawnets$sc2 = 1;

*#flag #schalter
PARAMETERS
* ------ ETS policies:
         sectrade_yr(yr)
         septrade_yr(yr)
         trade_yr(yr)
* ------ Price policies
         carbtax(r)
         share_vfm(i,r)     share of global fossil fuel endowment per region
         pricetarget(i,r)	Flag for exogenous price path for fossil fuels
         pytarget(i,r)		Exogenous price path for fossil fuels

* ------ Other share and path parameters:
         res_share(yr,r)

* ------ 06.11.2017 Carbon tax
        carbon_tax(r)

* ------ 18.05.2018 - share of allowances per sector that should be allocated for free
        gf_shr(g,r)
        gf_shr_yr(yr,g,r)

* ------ CO2-Emissionen (Wert-Koeffizienten)
         co2coefy(*,*,*)
         co2coefc(*,*)
         co2coefele(*,*,*)

* ------ 24.01.2019 times coupling - matching energy production
         ele_dev_switch_actual(gen,r)
         ele_dev_actual(gen,r)

* ------ 21.02.2019 household disaggregation 
         co2coefc_hh(*,*)
         co2coefc_gov(*,*)

* ------ 30.01.2020 removing vat
        tp_vat(r,i)
        sec_no_vat(i,r)
        sec_no_lab(*,i,r)
        no_vat_summary(r,yr,*)
        no_lab_summary(r,yr,*)
;

$GDXIN   %datadir%%output%_tp_no_vat.gdx                                    //GDXin
$LOAD    tp_vat
$GDXIN

sec_no_vat(i,r)$(eU28(r) AND (tp(i,r) > 0)) = 1; 
sec_no_lab('skl',i,r)$(eU28(r) AND (rtf0('skl',i,r) > 0)) = 1;
sec_no_lab('usk',i,r)$(eU28(r) AND (rtf0('usk',i,r) > 0)) = 1; 

ele_dev_switch_actual(gen,r) = 0;
ele_dev_actual(gen,r) = 0;

display ele_dev_switch_actual;
* ------ 06.11.2017 carbon tax for bmk
carbon_tax(r) = 0;


* ----- 18.05.2018 - define free allowances share
gf_shr_yr("2011","ppp", eu28(r)) = 0 ;
gf_shr_yr("2011","irs", eu28(r)) = 0 ;
gf_shr_yr("2011","nfm", eu28(r)) = 0 ;
gf_shr_yr("2011","nmm", eu28(r)) = 0 ;
gf_shr_yr("2011","chm", eu28(r)) = 0 ;
gf_shr_yr("2015","oil", eu28(r)) = 0.796706035969752 ;
gf_shr_yr("2015","ppp", eu28(r)) = 1 ;
gf_shr_yr("2015","irs", eu28(r)) = 1 ;
gf_shr_yr("2015","nfm", eu28(r)) = 0.948953308132301 ;
gf_shr_yr("2015","nmm", eu28(r)) = 1 ;
gf_shr_yr("2015","chm", eu28(r)) = 1 ;
gf_shr_yr("2020","oil", eu28(r)) = 0.7347 ;
gf_shr_yr("2020","ppp", eu28(r)) = 1 ;
gf_shr_yr("2020","irs", eu28(r)) = 1 ;
gf_shr_yr("2020","nfm", eu28(r)) = 0.834 ;
gf_shr_yr("2020","nmm", eu28(r)) = 0.9049 ;
gf_shr_yr("2020","chm", eu28(r)) = 0.9187 ;
gf_shr_yr("2025","oil", eu28(r)) = 0.6432 ;
gf_shr_yr("2025","ppp", eu28(r)) = 0.9355 ;
gf_shr_yr("2025","irs", eu28(r)) = 1 ;
gf_shr_yr("2025","nfm", eu28(r)) = 0.689 ;
gf_shr_yr("2025","nmm", eu28(r)) = 0.7169 ;
gf_shr_yr("2025","chm", eu28(r)) = 0.7612 ;
gf_shr_yr("2030","oil", eu28(r)) = 0.5517 ;
gf_shr_yr("2030","ppp", eu28(r)) = 0.7945 ;
gf_shr_yr("2030","irs", eu28(r)) = 0.9737 ;
gf_shr_yr("2030","nfm", eu28(r)) = 0.544 ;
gf_shr_yr("2030","nmm", eu28(r)) = 0.5289 ;
gf_shr_yr("2030","chm", eu28(r)) = 0.6037 ;
gf_shr_yr("2035","oil", eu28(r)) = 0.4602 ;
gf_shr_yr("2035","ppp", eu28(r)) = 0.6535 ;
gf_shr_yr("2035","irs", eu28(r)) = 0.8787 ;
gf_shr_yr("2035","nfm", eu28(r)) = 0.399 ;
gf_shr_yr("2035","nmm", eu28(r)) = 0.3409 ;
gf_shr_yr("2035","chm", eu28(r)) = 0.4462 ;
gf_shr_yr("2040","oil", eu28(r)) = 0.3687 ;
gf_shr_yr("2040","ppp", eu28(r)) = 0.5125 ;
gf_shr_yr("2040","irs", eu28(r)) = 0.7837 ;
gf_shr_yr("2040","nfm", eu28(r)) = 0.254 ;
gf_shr_yr("2040","nmm", eu28(r)) = 0.1529 ;
gf_shr_yr("2040","chm", eu28(r)) = 0.2887 ;
gf_shr_yr("2045","oil", eu28(r)) = 0.2772 ;
gf_shr_yr("2045","ppp", eu28(r)) = 0.3715 ;
gf_shr_yr("2045","irs", eu28(r)) = 0.6887 ;
gf_shr_yr("2045","nfm", eu28(r)) = 0.109 ;
gf_shr_yr("2045","nmm", eu28(r)) = 0 ;
gf_shr_yr("2045","chm", eu28(r)) = 0.1312 ;
gf_shr_yr("2050","oil", eu28(r)) = 0.1857 ;
gf_shr_yr("2050","ppp", eu28(r)) = 0.2305 ;
gf_shr_yr("2050","irs", eu28(r)) = 0.5937 ;
gf_shr_yr("2050","nfm", eu28(r)) = 0 ;
gf_shr_yr("2050","nmm", eu28(r)) = 0 ;
gf_shr_yr("2050","chm", eu28(r)) = 0 ;

gf_shr(g,r) = 0;


* ------ Carbon Regimes
co2pfad_ets(r,yr)$trade  = 0 ;
seCtrade_yr("2011")      = 0 ;
seCtrade_yr(yr)$after(yr)= 0 ;
sePtrade_yr("2011")      = 0 ;           // BAU on
sePtrade_yr(yr)$after(yr)= 0 ;           // BAU on
trade_yr("2011")         = 0 ;
trade_yr(yr)$after(yr)   = 0 ;

* ------ Price targets
share_vfm(i,r)$xe(i)    = vfm("res",i,r)/sum(s,vfm("res",i,s));

pricetarget(i,r)$(xe(i) AND (share_vfm(i,r) > 0.05))    = 1 ;


*pricetarget(i,r)                 = 1 ;	// exogenous price path for crude oil, coal and gas
*pricetarget("cru",r)             = 0 ;	// exogenous price path for crude oil
*pricetarget("col",r)             = 0 ;	// exogenous price path for coal
*pricetarget("gas",r)             = 0 ;	// exogenous price path for gas

*pricetarget(i,"EUS")             = 0 ;  // exogenous price path for gas
* ------ 11.06.2014
pytarget(i,r)$pricetarget(i,r)   = 1;	// needed for benchmark solve where all prices must equal 1

* ------ 12.06.2014
*rd0("cru",r)$(not rd0("cru",r)) = 1e-8; display rd0;
*pytarget(i,r)$(rd0(i,r) and pricetarget(i,r))   = 1;    // BAU on
*display pricetarget, pytarget, rd0;

*#flag #schalter
* ------ Other specific scenarios
* 1.) fix carbon price
carbtax(r) = 0 ;





* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------------------------------------------------------------------------------
* ------ (K) ZPF Checks and value shares
* ------------------------------------------------------------------------------
parameter
         zpf_c(r)        ZPF condition for final consumption in the model
         zpf_c_bmk(r)    ZPF condition for final consumption in the benchmark data
         zpf_y(i,r)      ZPF condition for Y production in the model
         zpf_a(i,r)
         zpf_y_bmk(i,r)  ZPF condition for Y production in the benchmark data
         zpf_ele(i,r)    ZPF condition for ELE benchmark production
         zpf_genx(gen,r) ZPF condition for ELEx (REG) benchmark production
         zpf_genn(gen,r) ZPF condition for ELEn (REG) benchmark production
         sharec_bmk(i,r) Final consumption value shares (benchmark data)
         sharect_bmk(r)  Final consumption value shares (benchmark data) check parameter
         sharey_bmk(*,i,r) Y production input value shares (benchmark data)
         shareyt_bmk(i,r)  Y production input value shares (benchmark data) check parameter
         sharec          Final consumption value shares
         sharect         Final consumption value shares check parameter
         sharey          Y production input value shares
         shareyt         Y production input value shares check parameter
;
* ------ ZPF consumption in benchmark data
zpf_c_bmk(r) = round( ct0(r) - sum(i, c0(i,r)*(1+tc(i,r)))   , rd);
* ------ ZPF production in benchmark data
zpf_y_bmk(i,r)$(not ele(i)) = round(
         + vom(i,r)*(1-ty(i,r))
         - sum(j, vafm(j,i,r)*(1+ti(j,i,r))) - skld0(i,r) - uskd0(i,r) - kd0(i,r) - rd0(i,r), rd);
* ------ ZPF production of ELE
zpf_ele(i,r)$ele(i) = round(             vom(i,r)*(1-ty(i,r)) - sum(gen, out_gen(gen,r)), rd);
*zpf_ele(i,r)$ele(i) = round((vdm(i,r)+vxm(i,r))  *(1-ty(i,r)) - sum(gen, out_gen(gen,r)), rd);

* ------ ZPF production of ELE GENn
zpf_genn(gen,r) = round(
         + out_gen(gen,r)
         - (sum(i, [vafm_input(i,gen,r)*(1+(ti(i,"ele",r)))] ) + skl_input(gen,r) + usk_input(gen,r) + cap_input(gen,r)), rd);
* ------ this one, too [incl. diffcost]:
zpf_genn(gen,r) = round(
         + out_gen(gen,r)*(1-(1-diffcost(gen,r)))
         - (sum(i, [vafm_input(i,gen,r)*(1+(ti(i,"ele",r)))]) + skl_input(gen,r) + usk_input(gen,r) + cap_input(gen,r))*diffcost(gen,r), rd);
* ------ ZPF production of ELE GENn
zpf_genx(gen,r) = round(
         + out_gen(gen,r)
         - (sum(i, [vafm_input(i,gen,r)*(1+(ti(i,"ele",r)))] ) + skl_input(gen,r) + usk_input(gen,r) + cap_input(gen,r)), rd);

display zpf_c_bmk, zpf_y_bmk, zpf_ele, zpf_genn, zpf_genx;

* ------ ZPF benchmark shares in C
sharec_bmk(i,r)      = c0(i,r)*(1+tc(i,r))/ct0(r);
sharect_bmk(r)       = sum(i, sharec_bmk(i,r));
* ------ ZPF benchmark shares in Y
sharey_bmk(j,i,r)$vom(i,r)        = vafm(j,i,r)*(1+ti(j,i,r)) / (vom(i,r)*(1-ty(i,r)));
sharey_bmk("SKL",i,r)$vom(i,r)    = skld0(i,r) / (vom(i,r)*(1-ty(i,r)));
sharey_bmk("USK",i,r)$vom(i,r)    = uskd0(i,r) / (vom(i,r)*(1-ty(i,r)));
sharey_bmk("CAP",i,r)$vom(i,r)    = kd0(i,r)   / (vom(i,r)*(1-ty(i,r)));
sharey_bmk("RES",i,r)$vom(i,r)    = rd0(I,r)   / (vom(i,r)*(1-ty(i,r)));
shareyt_bmk(i,r)  = sum(j, sharey_bmk(j,i,r)) + sharey_bmk("SKL",i,r) + sharey_bmk("USK",i,r)
                 + sharey_bmk("CAP",i,r) + sharey_bmk("RES",i,r);
display sharec_bmk, sharect_bmk, shareyt_bmk,sharey_bmk;


* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------------------------------------------------------------------------------
* ------ (L) ELE Differenzkosten
* ------------------------------------------------------------------------------
PARAMETER
* ------ Differenzkosten / Schalter für diffcostsubvention
         diffsub(gen,r)
         diffsub_x(gen,r)
         diffrebate(r)
         epa_on          Schalter für diffcost-Korrektur in E:PA... im BMK auf 1 /1/ // Muss im BMK 1 sein, damit balance. Später auf 0 damit E:pa... über rebate_diff umgelegt wird.
         deltadiff(r)
         diffcost0
         diffcost2(gen,r)
         tdiff(gen,r)
         tyd(i,r)
         rebate_par
;
* ------ Differenzkosten / Schalter für Diffcostsubvention
diffsub_x(gen,r)         = 0 ;
diffsub(gen,r)           = 0 ;
diffrebate(r)            = 0 ;
* ------ 09.07.2014 in BMK case set diffrebate = 0 and epa_on = 1
*diffrebate(bawdeu)       = 0 ;
diffrebate("deu")        = 0 ;
epa_on                   = 1 ;

deltadiff(r)             = 0 ;
diffcost(gen,r)$diffcost(gen,r) = diffcost(gen,r) / (1+deltadiff(r));
* ------ [Diffcost-Schalter]
diffcost0(gen,r) = diffcost(gen,r);
diffcost(gen,r) = 1;
diffcost(reg,r) = diffcost0(reg,r) ;


diffcost("msolar",r)$deu(r) = 8;	// for Germany diffcost should be lower (original value: 11,3), otherwise solar is used too little --> value of 8 replicates real solar generation best
diffcost("msolar",r)$esp(r) = 6;	// for Spain diffcost should be lower (original value: 11,3), otherwise solar is used too little --> value of 6 replicates real solar generation best

diffcost2(gen,r) = diffcost0(gen,r);
diffcost2(reg,eu28) = 2;
*diffcost(reg,eu28) = 5;

*display diffcost, diffcost0, diffcost_exogen;

* ------ 24.04.2014 Calculate different taxes for specifying initial value of REBATE_DIFF.L
tdiff(gen,r) = 0;
tdiff(gen,r) = ((-1)*(1-diffcost2(gen,r)) * out_gen(gen,r) * (1-thetax)) / vom("ele",r);
tyd("ele",r) = ty("ele",r) - sum(reg, (-1)*(1-diffcost2(reg,r)) * out_gen(reg,r) * (1-thetax)) / vom("ele",r);
* ------ Calculate ingredients of rebate_diff auxiliary constraint
rebate_par("rebate_diff",r) = sum(gen$reg(gen), (-1)*(1-diffcost(gen,r)) * (out_gen(gen,r) * (1-thetax))) / vom("ele",r);
rebate_par("zähler",r) = sum(gen$reg(gen), (-1)*(1-diffcost(gen,r)) * (out_gen(gen,r) * (1-thetax)));
rebate_par("nenner",r) = vom("ele",r);

*display diffcost0, tdiff, tyd, rebate_par ;

* ------------------------------------------------------------------------------
* Erhöhe Faktorausstattungen um die Differenzkosten damit Kalibrierung gegeben
* hier vorerst nur Faktorausstattung neues Kapital
* ------------------------------------------------------------------------------
evoa("cap",r) = evoa("cap",r) - sum(reg, cap_input(reg,r)*(1-thetax) * (1 - diffcost(reg,r)))  ;
evoa("skl",r) = evoa("skl",r) - sum(reg, skl_input(reg,r)*(1-thetax) * (1 - diffcost(reg,r)))  ;
evoa("usk",r) = evoa("usk",r) - sum(reg, usk_input(reg,r)*(1-thetax) * (1 - diffcost(reg,r)))  ;


* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------------------------------------------------------------------------------
* ------ (M) E-mail Rutherford 5.08.2013: Ensure Benchmark consistency
* ------------------------------------------------------------------------------
parameter vtw, evom;
vtw(j) = sum(r, vst(j,r));
vim(i,r) =  sum(g, vifm(i,g,r));
evom(f,r) = sum(g, vfm(f,g,r));
* ------ The final calculation determines consistency of the current account for the globe (net savings must be zero).
vb(r) = 0; 
vb(r) =  vom("c",r) + vom("g",r) + vom("i",r)
         - sum(f, evom(f,r))
         - sum(g,  vom(g,r)*rto(g,r))
         - sum(g,  sum(i, vdfm(i,g,r)*rtfd(i,g,r) + vifm(i,g,r)*rtfi(i,g,r)))
         - sum(g,  sum(f, vfm(f,g,r)* rtf(f,g,r)))
         - sum((i,s), rtms(i,s,r) *(vxmd(i,s,r) * (1-rtxs(i,s,r)) + sum(j,vtwr(j,i,s,r))))
         + sum((i,s), rtxs(i,r,s) * vxmd(i,r,s));
vb("chksum") = sum(r, vb(r)); display vb;
* ------ The final set of calculations checks consistency between domestic supply and demand for traded goods:
mprofit(i,r) = 0; mprofit(i,r) = round(
         + vim(i,r)
         - sum(s, pvxmd(i,s,r)*vxmd(i,s,r)+sum(j,vtwr(j,i,s,r))*pvtwr(i,s,r)), 7); display mprofit;
yprofit(g,r) = 0; yprofit(g,r) = round(
         vom(g,r)*(1-rto(g,r))
         - sum(i, vdfm(i,g,r)*(1+rtfd0(i,g,r)) + vifm(i,g,r)*(1+rtfi0(i,g,r)))
         - sum(f, vfm(f,g,r)*(1+rtf0(f,g,r))), 7); display yprofit;



chk_chk_vb("vb","vom",r) =  vom("c",r) + vom("g",r) + vom("i",r);
chk_chk_vb("vb","vfm",r) = sum(f, evom(f,r));
chk_chk_vb("vb","vom_tax",r) = sum(g,  vom(g,r)*rto(g,r));
chk_chk_vb("vb","vdifm",r) = sum(g,  sum(i, vdfm(i,g,r)*rtfd(i,g,r) + vifm(i,g,r)*rtfi(i,g,r)));
chk_chk_vb("vb","vfm_tax",r) = sum(g,  sum(f, vfm(f,g,r)* rtf(f,g,r)));
chk_chk_vb("vb","imp",r) = sum((i,s), rtms(i,s,r) *(vxmd(i,s,r) * (1-rtxs(i,s,r)) + sum(j,vtwr(j,i,s,r))));
chk_chk_vb("vb","exp",r) = sum((i,s), rtxs(i,r,s) * vxmd(i,r,s));

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ 07.10.2014  read in data for constraining new electricity generation in BAW
* ------ 10.12.2014  PARAMETERS for RWE and BMWi projects
parameter
         bbc_deu_elen(gen,yr)    Maximum new electricity generation (bBC)

* ------ Endogene Subvention für Untergrenz für ELE-Produktion
         ylo_on(i,r)
         ylo_par(i,r)

* ------ 28.05.2015
         co2fak(r)               CO2-Emissionsfaktor der inländischen Stromerzeugung [tCO2 per MWh]

* ------ Anzahl Erwerbstätige im Jahr 2011
         emplmtno_2011(r,*)
;


* ------ 28.05.2015 Emissionsfaktor für Strom
* ------ 0.76 tCO2/MWh ist der Wert aus dem VET-Bericht 2013(?)
* ------ Felix Nickel E-Mail 20.05.2015: "Das ganze bei (...) einem Emissionsfaktor der Stromproduktion, der von 560 gCO2/kWh in 2014 auf 490 gCO2/kWh in 2030 absinkt."
*co2fak(r) = 0.760 ; // 0.76 ist der Wert aus dem VET-Bericht 2013
co2fak(r) = 0.525 ; // Durchschnitt aus 560 und 490 gCO2/kWh (FutureCamp)

* ------ Initialize ylo_par and ylo_on (<-- 5.05.2015: set to zero)
ylo_par(i,r) = 1;
ylo_on(i,r)  = 0;

* ------ Read in additional data -----------------------------------------------
$onecho >temp1.tmp
par=bbc_deu_elen         rng=bBC_data!B9:L10
par=emplmtno_2011        rng=emplmtno_2011!B5:D23
$offecho

$call "gdxxrw %xcel_datadir%Annahmen_Stromerzeugung.xlsx @temp1.tmp"

$gdxin "Annahmen_Stromerzeugung"
$load bbc_deu_elen
$load emplmtno_2011



* ------ 18.02.2016 Read non-ETS reduction path for netstrade_r scenario
parameter nonets_yr(r,yr), nonetsx(r,yr);

nonetsx(r,yr)$eu28(r) = 1 ;
* =========================================================================== *
*       FACTORS FOR HH DISAGGREGATION
* =========================================================================== *
parameter
         hh_consumption(r,i,hh)        household consumption per sector and region
         hh_income(r,*,hh)
         total_cons(r,i)                     Sum over households to get total sectoral consumption
         hh_sector_share(r,i,hh)             share that each household consumes from each sector
         hh_total_consumption_share(r,hh)
         hh_skl_share(r,hh)
         hh_usk_share(r,hh)
         hh_rkr_share(r,hh)
         hh_rkx_ele_share(r,hh)
         hh_tax_in_share(r,hh)
         hh_tax_out_share(r,hh)
         hh_savings_share(r,hh)
         hh_energy_share(r,*,*,yr)           "Share of Electricity and oil used in transport and other usages"        
         ;

$GDXIN %datadir%%source%_consumption_diss.gdx
$LOAD hh_sector_share hh_total_consumption_share hh_income hh_skl_share hh_usk_share
$LOAD hh_rkr_share hh_rkx_ele_share hh_tax_in_share hh_tax_out_share hh_savings_share
$LOAD hh_consumption, total_cons, hh_energy_share
$GDXIN

* =========================================================================== *
*       SHARE OF OIL USED FOR HEATING AND TRANSPORTATION
* =========================================================================== *

* disaggregate parameter c_hh0 for electricity and oil consumption
c_hh0("oil_transport",r)$h_t_cons_reg(r)   = hh_energy_share(r,"oil","Transport","2011") * c_hh0("oil",r);
c_hh0("oil",r)$h_t_cons_reg(r)             = hh_energy_share(r,"oil","Others","2011") * c_hh0("oil",r);

c_hh0("ele_transport",r)$h_t_cons_reg(r)   = hh_energy_share(r,"Electricity","Transport","2011") * c_hh0("ele",r);
c_hh0("ele",r)$h_t_cons_reg(r)             = hh_energy_share(r,"Electricity","Others","2011") * c_hh0("ele",r);


parameter 
    share_oil_heat(*,r)        share of oil used for heating according to hh and region
    bmk_ele_trans;



* =========================================================================== *
*       TAX REVENUE FOR GOVERNMENT BLOCK
* =========================================================================== *
parameter taxsum(*,r);

* tax on output production
taxsum("ty",r) = sum(i, vom(i,r) * ty(i,r));

* tax on input PA
taxsum("ti",r) = sum((i,j),vafm(j,i,r) * ti(j,i,r)) - sum(j,vafm(j,"ele",r) * ti(j,"ele",r));

* tax on input PC
taxsum("tc",r) = sum(i,c0(i,r) * tc(i,r));

taxsum('tp',r) = sum(i, c_hh0(i,r) * tp(i,r));
taxsum('tg',r) = sum(i, c_gov0(i,r) * tg(i,r));

*tax on output of electricity generation pgen
taxsum("diffcost",r) = sum(gen,out_gen(gen,r)$out_gen(gen,r)$ks_n(gen,r)$reg(gen) * (1 - diffcost(gen,r)) * 0.015);

*tax on inputs of armingtom bloc
taxsum("tx",r) = sum((i,s),vxmd(i,r,s) * (tx(i,r,s))) ;

taxsum("txm",r) = sum((i,s),vxmd(i,s,r) * tm(i,s,r) * (1+tx(i,s,r)) ) ;
taxsum("tm",r) = sum((i,s),vtwr1(i,s,r) * tm(i,s,r));

*tax on inputs of electricty generation
taxsum("ti_ele",r) = sum((i,gen),vafm_input(i,gen,r)$out_gen(gen,r)$ks_n(gen,r)  * ti(i,"ele",r)) * 0.985 +
                            sum((i,gen),vafm_input(i,gen,r)$((out_gen(gen,r))$(ks_n(gen,r))$(not reg(gen)))  * ti(i,"ele",r)) * 0.015 +
                            sum((i,gen),(vafm_input(i,gen,r) * diffcost(gen,r))$((out_gen(gen,r))$(ks_n(gen,r))$(reg(gen)))  * ti(i,"ele",r)) * 0.015;

* tax on investments inputs
taxsum("pa_inv",r) = sum(i, vafm(i,"i",r) * ti(i,"i",r));

*tax on capital from production
*taxsum("capital",r)$(diss_factor_tax and HH_DISAG(r)) = sum(i, kd0(i,r) * rtf("cap",i,r)) + sum(gen,cap_input(gen,r) * rtf("cap","ele",r));
taxsum("cap_ele",r) = sum(gen,cap_input(gen,r)$out_gen(gen,r)$ks_n(gen,r)  * rtf("cap","ele",r)) * 0.985 +
                            sum(gen,cap_input(gen,r)$((out_gen(gen,r))$(ks_n(gen,r))$(not reg(gen)))  * rtf("cap","ele",r)) * 0.015 +
                            sum(gen,(cap_input(gen,r) * diffcost(gen,r))$((out_gen(gen,r))$(ks_n(gen,r))$(reg(gen)))  * rtf("cap","ele",r)) * 0.015;

taxsum("skl_ele",r) = sum(gen,skl_input(gen,r)$out_gen(gen,r)$ks_n(gen,r)  * rtf("skl","ele",r)) * 0.985 +
                            sum(gen,skl_input(gen,r)$((out_gen(gen,r))$(ks_n(gen,r))$(not reg(gen)))  * rtf("skl","ele",r)) * 0.015 +
                            sum(gen,(skl_input(gen,r) * diffcost(gen,r))$((out_gen(gen,r))$(ks_n(gen,r))$(reg(gen)))  * rtf("skl","ele",r)) * 0.015;

taxsum("usk_ele",r) = sum(gen,usk_input(gen,r)$out_gen(gen,r)$ks_n(gen,r)  * rtf("usk","ele",r)) * 0.985 +
                            sum(gen,usk_input(gen,r)$((out_gen(gen,r))$(ks_n(gen,r))$(not reg(gen)))  * rtf("usk","ele",r)) * 0.015 +
                            sum(gen,(usk_input(gen,r) * diffcost(gen,r))$((out_gen(gen,r))$(ks_n(gen,r))$(reg(gen)))  * rtf("usk","ele",r)) * 0.015;

taxsum("capital",r)$(diss_factor_tax and HH_DISAG(r)) = sum(i, kd0(i,r) * rtf("cap",i,r)) + taxsum("cap_ele",r);

taxsum("skl",r)$(diss_factor_tax and HH_DISAG(r))     = sum(i, skld0(i,r) * rtf("skl",i,r)) + taxsum("skl_ele",r);

taxsum("usk",r)$(diss_factor_tax and HH_DISAG(r))     = sum(i, uskd0(i,r) * rtf("usk",i,r)) + taxsum("usk_ele",r);

taxsum("total",r) = taxsum("ty",r) + taxsum("ti",r) + taxsum("tp",r) + taxsum("tg",r) + taxsum("diffcost",r) + taxsum("tx",r) + taxsum("tm",r) + taxsum("txm",r) + taxsum("ti_ele",r) + taxsum("pa_inv",r) +
                   (taxsum("capital",r) + taxsum("skl",r) + taxsum("usk",r))$(diss_factor_tax and HH_DISAG(r));
display taxsum;

* =========================================================================== *
*       INPUTS OF REPRESENTATIVE AGENT
* =========================================================================== *
parameter check_ra_inputs(*,r);
check_ra_inputs("cons_gov",r) =  -vom("g",r);
check_ra_inputs("RA_cons_hh",r) =  -vom("c",r);
check_ra_inputs("RA_cons_inv",r) =  -vom("i",r);
check_ra_inputs("acc_balance",r) =  vb(r);
check_ra_inputs("resources",r) =  sum(i,rd0(i,r));
check_ra_inputs("RA_skl_labor",r) =  ( evoa("skl",r) / (1-ursk0(r))) - (evoa("skl",r) / (1-ursk0(r))) * URSK0(r);
check_ra_inputs("RA_usk_labor",r) =  ( evoa("usk",r) / (1-urun0(r))) - (evoa("usk",r) / (1-urun0(r))) * URUN0(r);
check_ra_inputs("RA_rkr",r) =  evoa("cap",r);
check_ra_inputs("RA_rkx_ele",r) =  sum(gen, ks_x(gen,r)) ;
check_ra_inputs("RA_taxes",r) =  taxsum("total",r);
check_ra_inputs("pa_diffcost",r) = sum(i,((-sum(reg, vafm_input(i,reg,r)*(1-thetax)*(1-diffcost(reg,r)))) * epa_on ));

check_ra_inputs("LKTAX",r) = (check_ra_inputs("cons_gov",r) + check_ra_inputs("resources",r) + check_ra_inputs("pa_diffcost",r) + check_ra_inputs("acc_balance",r))*(-1);

display check_ra_inputs;

* =========================================================================== *
*       INPUTS OF HOUSEHOLDS
* =========================================================================== *

parameters 
    tax_hh_2_gov(r)
    tax_gov_2_hh(r) ;


*tax_hh_2_gov(r)$HH_DISAG(r) = -sum(hh,hh_income(r,"tax on income",hh));


tax_hh_2_gov(r)$(HH_DISAG(r) AND NOT diss_factor_tax) = -sum(hh,hh_income(r,"tax on income",hh));
tax_hh_2_gov(r)$(HH_DISAG(r) AND diss_factor_tax)     = -sum(hh,hh_income(r,"tax on income",hh)) - ((taxsum("capital",r) + taxsum("skl",r) + taxsum("usk",r))/2);

hh_tax_out_share(r,hh)$(HH_DISAG(r) AND diss_factor_tax) = -( 
                                                            hh_income(r,"tax on income",hh)
                                                            + (taxsum("capital",r) * hh_rkr_share(r,hh) * 0.5) 
                                                            + (taxsum("skl",r) * hh_skl_share(r,hh) * 0.5)
                                                            + (taxsum("usk",r) * hh_usk_share(r,hh) * 0.5)  
                                                            )
                                                            / tax_hh_2_gov(r);

* ---- I am adding two different equations for tax_gov_2_hh because in the second case I account for taxes payed from capital
tax_gov_2_hh(r)$(HH_DISAG(r) AND NOT diss_factor_tax) = sum(hh,hh_income(r,"taxes income",hh)); 
tax_gov_2_hh(r)$(HH_DISAG(r) AND diss_factor_tax)     = sum(hh,hh_income(r,"taxes income",hh)) + (taxsum("capital",r) + taxsum("skl",r) + taxsum("usk",r))/2;

* ---- Here I calculate new coefficients for hh_tax_in_share in order to account for the extra taxes coming from capital payments
hh_tax_in_share(r,hh)$(HH_DISAG(r) AND diss_factor_tax) = (
                                                            hh_income(r,"taxes income",hh) 
                                                          + (taxsum("capital",r) * hh_rkr_share(r,hh) * 0.5) 
                                                          + (taxsum("skl",r) * hh_skl_share(r,hh) * 0.5)
                                                          + (taxsum("usk",r) * hh_usk_share(r,hh) * 0.5) 
                                                          )   
                                                          / tax_gov_2_hh(r);

$ontext
* ---- I am adding two different equations for tax_gov_2_hh because in the second case I account for taxes payed from capital
tax_gov_2_hh(r)$(HH_DISAG(r) AND NOT diss_factor_tax) = sum(hh,hh_income(r,"taxes income",hh)); 
tax_gov_2_hh(r)$(HH_DISAG(r) AND diss_factor_tax)     = sum(hh,hh_income(r,"taxes income",hh)) + (taxsum("capital",r) + taxsum("skl",r) + taxsum("usk",r));

* ---- Here I calculate new coefficients for hh_tax_in_share in order to account for the extra taxes coming from capital payments
hh_tax_in_share(r,hh)$(HH_DISAG(r) AND diss_factor_tax) = (
                                                            hh_income(r,"taxes income",hh) 
                                                          + (taxsum("capital",r) * hh_rkr_share(r,hh)) 
                                                          + (taxsum("skl",r) * hh_skl_share(r,hh))
                                                          + (taxsum("usk",r) * hh_usk_share(r,hh)) 
                                                          )   
                                                          / tax_gov_2_hh(r);
$offtext

parameter check_ra_hh(*,r,hh);

check_ra_hh("cons",r,hh)$HH_DISAG(r)                                = -(vom("c",r) * hh_total_consumption_share(r,hh));

check_ra_hh("savings",r,hh)$HH_DISAG(r)                             = -(vom("i",r) * hh_savings_share(r,hh));

check_ra_hh("P2G",r,hh)$HH_DISAG(r)                                 = -(tax_hh_2_gov(r) * hh_tax_out_share(r,hh));

check_ra_hh("skl",r,hh)$HH_DISAG(r)                                 = (( evoa("skl",r) / 1) * hh_skl_share(r,hh));

check_ra_hh("usk",r,hh)$HH_DISAG(r)                                 = (( evoa("usk",r) / 1) * hh_usk_share(r,hh));
   

check_ra_hh("rkr",r,hh)$HH_DISAG(r)                                 =(evoa("cap",r) * hh_rkr_share(r,hh));

check_ra_hh("rkx_ele",r,hh)$HH_DISAG(r)                             = sum(gen$ks_x(gen,r), ks_x(gen,r) ) * hh_rkx_ele_share(r,hh);

*check_ra_hh("tax_rev",r,hh)$HH_DISAG(r)                             = (tax_gov_2_hh(r) * hh_tax_in_share(r,hh));

check_ra_hh("tax_rev",r,hh)$(HH_DISAG(r) AND NOT diss_factor_tax)   = (tax_gov_2_hh(r) * hh_tax_in_share(r,hh));

*check_ra_hh("tax_rev",r,hh)$(HH_DISAG(r) AND diss_factor_tax)       = (tax_gov_2_hh(r) * hh_tax_in_share(r,hh));

check_ra_hh("tax_rev",r,hh)$(HH_DISAG(r) AND diss_factor_tax)       = (tax_gov_2_hh(r) * hh_tax_in_share(r,hh));
*                                                                      ( (sum(i, kd0(i,r) * (rtf("cap",i,r))) + taxsum("cap_ele",r)) * hh_rkr_share(r,hh));
 

check_ra_hh("total",r,hh)$HH_DISAG(r) = check_ra_hh("cons",r,hh) + check_ra_hh("savings",r,hh) + check_ra_hh("P2G",r,hh) + check_ra_hh("skl",r,hh) 
                                        + check_ra_hh("usk",r,hh) + check_ra_hh("rkr",r,hh) + check_ra_hh("rkx_ele",r,hh) + check_ra_hh("tax_rev",r,hh);



display check_ra_hh;

parameter check_cons_hh(r,*,hh);

check_cons_hh(r,"c",hh)$HH_DISAG(r) = (vom("c",r) * hh_total_consumption_share(r,hh));

check_cons_hh(r,i,hh)$HH_DISAG(r) = (c_hh0(i,r) * hh_sector_share(r,i,hh));

check_cons_hh(r,"tax",hh)$HH_DISAG(r) = sum(i,(c_hh0(i,r) * tp(i,r) * hh_sector_share(r,i,hh)));

check_cons_hh(r,"total",hh)$HH_DISAG(r) = check_cons_hh(r,"c",hh) - check_cons_hh(r,"tax",hh) - sum(i,check_cons_hh(r,i,hh));


display check_cons_hh;


parameter cons_with_tax(*,i,r);

cons_with_tax("HH-Dom",i,r) = vdfm(i,"c",r) * pc_hh0(i,r);
cons_with_tax("HH-Imp",i,r) = vifm(i,"c",r) * pc_hh0(i,r);

cons_with_tax("Gov-Dom",i,r) = vdfm(i,"g",r) * pc_gov0(i,r);
cons_with_tax("Gov-Imp",i,r) = vifm(i,"g",r) * pc_gov0(i,r);

display cons_with_tax, c_hh0, pc_hh0;

parameter  check_gov_inputs(*,r);

check_gov_inputs("PC_GOV",r)$HH_DISAG(r)        = vom("g",r);
check_gov_inputs("tax_gov_2_hh",r)$HH_DISAG(r)  = tax_gov_2_hh(r);
check_gov_inputs("PR",r)$HH_DISAG(r)            = sum(i$rd0(i,r), rd0(i,r));
check_gov_inputs("diffcosts",r)$HH_DISAG(r)     = sum(i,((-sum(reg, vafm_input(i,reg,r)*(1-thetax)*(1-diffcost(reg,r)))) * epa_on ));
check_gov_inputs("tax_hh_2_gov",r)$HH_DISAG(r)  = tax_hh_2_gov(r);
check_gov_inputs("vb",r)$HH_DISAG(r)            = vb(r);
check_gov_inputs("tax_input",r)$HH_DISAG(r)     = taxsum("total",r);
check_gov_inputs("sumCHK",r)$HH_DISAG(r)        = check_gov_inputs("PR",r) + check_gov_inputs("diffcosts",r) + check_gov_inputs("tax_hh_2_gov",r) + check_gov_inputs("vb",r) 
                                                  + check_gov_inputs("tax_input",r) - (check_gov_inputs("PC_GOV",r) + check_gov_inputs("tax_gov_2_hh",r));

* =========================================================================== *
*       Calculation of sectoral composites
*        In this section I implement paramenters that will show exactly how
*        much is the revenue, inputs from intermediates and tax payments of
*        each sector.
* =========================================================================== *

Parameters
    prod_accounts(r,i,*,yr)      account from production sectors
    elex_accounts(r,gen,*,yr)    account from elex production
    elen_accounts(r,gen,*,yr)    account from eley production
    armi_accounts(r,i,*,yr)      account from Armington Aggregation
    inv_accounts(r,*,yr)         account from investments
    cons_accounts(r,*,yr)        accounts from consumption
    cons_hh_accounts(r,hh,*,yr)  accounts from consumption per household
    cons_gov_accounts(r,*,yr)    accounts from government consumption

    taxes_region(*,*,yr)         list of taxes being paid per region
;

* =========================================================================== *
*       Calculation of carbon content (CO2 per Euro)
*            This number is necessary for the implementation of a 
*            border tax adjustment
* =========================================================================== *

parameters 

    cc(i,s,r)               Embodied carbon
    bta_sec_reg(i,s,r)      list to define which sectors and countries are included in the tax
    bta_reb_sec_reg(i,s,r);

cc(i,s,r)$(vom(i,s) AND vxmd(i,s,r)) = sum(fe, co2em(fe,i,s))/vom(i,s);

display "Direct Carbon Content:", cc;

cc(i,s,r)$(vom(i,s) AND vxmd(i,s,r))= sum(fe, co2em(fe,i,s))/vom(i,s) +
                    sum(fe, co2em(fe,"ele",s)) / vom("ele",s) * vafm("ele",i,s) /vom(i,s);

display "Indirect Carbon Content:", cc;

bta_sec_reg(i,s,r) = 0;
bta_sec_reg(i,s,r)$(NOT BTA_coa(s) and BTA_coa(r) and BTA_sec(i) and cc(i,s,r) AND bta) = 1;

bta_reb_sec_reg(i,s,r) = 0;
bta_reb_sec_reg(i,s,r)$(BTA_coa(s) AND NOT BTA_coa(r) and BTA_sec(i) and cc(i,s,r) AND bta AND bta_rebate) = 1;


* ----- 12.11.2020
Parameter        e_sub(r)                       "elasticity of substitution between electricity and fossil fuels"
                 cons_sub(r)
;

e_sub(r) = 0.1;
cons_sub(r) =1;


*Execute_Unload 'cons_with_tax', cons_with_tax;
*$EXIT
*EXITbefore
* ============================================================================ *
* ---------------------------------------------------------------------------- *
* M P S G E - M O D E L
* ---------------------------------------------------------------------------- *

$ontext

$model:NEWAGE

$PEPS:0
*PEPS is the smallest price for which price-responsive demand and supply functions
*are evaluated. If a price is below PEPS, it is perturbed (set equal to PEPS) prior to
*the evaluation. (cf. Rutherford, 1999)

* -- V A R I A B L E S ------------------------------------------------------- *

* ------ SECTORS / ACTIVITIES ------------------------------------------------ *

$sectors:
         C(r)$(not HH_DISAG(r))                            ! Private consumption
         Y(i,r)$vom(i,r)                 ! Output
         A(i,r)$a0(i,r)                  ! Armington Aggregation of Domestic and Imports
         INV(r)                          ! Investments
         YT                              ! Transport

* ------ Electricity BoUp
         ELEx(gen,r)$(out_gen(gen,r)$ks_x(gen,r)) ! Electricity generation from extant capacity
         ELEn(gen,r)$(out_gen(gen,r)$ks_n(gen,r)) ! Electricity generation from new capacity

* ------ Tax transfer
         TAXTRANS(r)$HH_DISAG(r)
         LKTAX(r)$HH_DISAG(r)

* ------ HH disaggregation
         C_hh(hh,r)$HH_DISAG(r)               ! Private consumption
         C_gov(r)$HH_DISAG(r)                 ! Government consumption    

* ------ transportation consumption
         FF_trans(r)$h_t_cons_reg(r)     ! private transportation using fossil fuels
         ELE_trans(r)$h_t_cons_reg(r)     ! private transportation using electricity

* ------ carbon emission
         CEMIT(r)$(EU28(r) AND eutrade and bta_test)
         CONVERT_PCO2_HH(hh,r)$(inverse_co2_pay AND HH_DISAG(r) AND netstrade_r)


* ------ COMMODITIES / PRICES ------------------------------------------------ *

$commodities:
         PC(r)$(not HH_DISAG(r))                           ! Final demand
         PY(i,r)$vom(i,r)                ! Output price
         PA(i,r)$a0(i,r)                 ! Armington composite price
         PSKL(r)                         ! Wage rate skilled labor
         PUSK(r)                         ! Wage rate unskilled labor
         PR(i,r)$rd0(i,r)                ! Energy resource
         RKR(r)$rsk                      ! Return to regional capital
         RKG$gk                          ! Return to global capital
         PT                              ! Transport services
         PINV(r)                         ! Price for new capital

* ------ Electricity BoUp
         PGEN(GEN,R)$OUT_GEN(GEN,R)      ! Price for electricity generation technologies
         RKX_ELE(gen,r)$ks_x(gen,r)      ! Return to extant capital

* ------ Carbon regimes:
         PCO2(r)$notrad(r)               ! Regional Carbon Price
         PCO2W$worldtrade                ! International Permit Price

*         PCO2W$eutrade                         ! International Permit Price
         PCO2Wr(r)$(eu28(r) AND eutrade AND bta_test)        ! International Permit Price. This price is used in combination with PCO2W to decribe how many carbon certificates each region within PCO2W uses
         PCO2W$eutrade                          ! International Permit Price

*         PCO2W$detrade                   ! all sectors permit for germany
         PCO2W$worldtrade2               ! International Permit Price for Non-EU28 if EU-ETS and or EU-non-ETS in place
         PCO2_ETS$etstrade               ! EU28 allowance price for ETS-sectors
         PCO2_NETS$netstrade             ! EU28 allowance price for Non-ETS-sectors
         PCO2_NETSr(r)$eu28(r)$netstrade_r ! Regional allowance price for Non-ETS-sectors in the EU28
         
*         PCO2Wr(r)$(BTA_coa(r) AND bta_test)                   ! International Permit Price
         PCO2W$(bta_test)        ! International Permit Price

*------28.09.2017   introduce sector-specific co2 markets in Germany --> co2 prices in sectors will then indicate how much effort sectors have to spend in order to fulfill the sector-specific reduction targets
         PCO2_DEU(sec)$(DEU_sec)      ! sector-specific carbon prices in Germany

* ----- 26.01.2018 germany specific co2 targets
        PCO2(r)$(deu(r) and detrade)

* ----- 11.01.2021 specific price for households
        PCO2_inv_pay(hh,r)$(inverse_co2_pay AND HH_DISAG(r) AND netstrade_r)

* ----- 08.02.2018 adding prices for taxes that the government receives and that are transferred to the RA
        PGOV(r)$HH_DISAG(r)
        PTAX(r)$HH_DISAG(r)
        PITAX(r)$HH_DISAG(r)
        PLKTAX(r)$HH_DISAG(r) 

        PC_hh(hh,r)$HH_DISAG(r)
        PC_gov(r)$HH_DISAG(r)

* ----- 09.30.2020      adding the sector of private transportation
        p_oil_trans(r)$h_t_cons_reg(r)      ! private transportation in the selected regions
        p_ele_trans(r)$h_t_cons_reg(r)      ! private transportation in the selected regions

* ----- 03.11.2020      adding the carbon tax with fixed price
        Pcarbon_tax(r)$(carbon_tax_de and deu(r))

* ------ CONSUMERS ----------------------------------------------------------- *

$consumers:
         RA(r)$(not HH_DISAG(r))                          ! Representative agent
         RA_HH(hh,r)$HH_DISAG(r)                          ! Representative agent divided by households
         GOV(r)$HH_DISAG(r)                               ! government
*         CO2_agent(r)$HH_DISAG(r)                         ! agent responsible to distribute CO2 revenues


* ------ AUXILIARY ----------------------------------------------------------- *

$auxiliary:
         URUN(r)$URUN0(r)                !Unemployment Rate (Rationing Multiplier) for Unskilled Labor

$auxiliary:
         URSK(r)$URSK0(r)                !Unemployment Rate (Rationing Multiplier) for Skilled Labor

$auxiliary:
         REBATE_DIFF(r)$diffrebate(r)    !Rebating of differential costs

* ------ 17.06.2014
$auxiliary:
         R_SUPPLY(i,r)$(rd0(i,r)$PRICETARGET(i,r))       !Anpssung auf exogene preispfade

* ------ 12.01.2015
$auxiliary:
         YLO(i,r)$ylo_on(i,r)

* ------ 18.05.2018
$auxiliary:
         GF_SUB(i,r)$(vom(i,r) AND gf_shr(i,r) AND gf_allow)    !Free allowances allocation

* ------ 24.01.2019 times coupling
*$auxiliary:
*         ele_tar(gen,r)$ele_dev_switch_actual(gen,r)     !tax to control the electricity output by technology

$auxiliary:
          ect(i,s,r)$bta_sec_reg(i,s,r)         ! embodied carbon tariffs
          ect_reb(i,s,r)$bta_reb_sec_reg(i,s,r) ! export rebate
          CO2_supply$bta                        ! multiplier for bca test

$auxiliary:
          tax_reb(r)$(no_vat and eu28(r))       ! abatement of income taxes for no-vat scenario
          tax_reb(r)$(low_lab_tax and eu28(r))       ! abatement of income taxes for no-vat scenario  

$auxiliary:
          CO2_inv_pay(hh,r)$(inverse_co2_pay AND HH_DISAG(r) AND netstrade_r)   !multiplier for payment of co2 revenues inversely proportional to expenditures

* ----- 03.11.2020  auxiliarry to fix the price of co2 emssions
$auxiliary:
            carb_tax_m(r)$(carbon_tax_de AND deu(r))       !multiplier for the carbon tax

* -- E Q U A T I O N S ------------------------------------------------------- *

* ------ FINAL DEMAND -------------------------------------------------------- *

* ------ 10.12.2014 (Erste Zeile Original)
* s = elasticity of substitution; c and e = nests of s; oil(e), col(e) and gas(e) = nests of e
* for a more detailed explanation and illustration see Rutherford, Paltsev (2000): GTAPinGAMS and GTAP-EG - Global Datasets for Economic Research and Illustrative Models

$prod:C(r)$(not HH_DISAG(r))       s:0.5   c:1     e:cons_sub(r)     oil(e):0   col(e):0   gas(e):0

         o:PC(r)                 Q:(vom("c",r) + vom("g",r))

* ------ All consumption goods (DEU: except transport, energy, cars and buildings)
* .tl alerts MPSGE that a set of nests are being declared
         i:PA(i,r)$(not e(i))                    q:c0(i,r)                       p:pc0(i,r) i.tl:$fe(i) c:$(not e(i)) e:$ele(i)  a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:tc(i,r)
* ------ Residential crude oil is NOT used for combustion --> no CO2-emissions --> no climate policy relevance
         i:PA(i,r)$cru(i)                        q:c0(i,r)                       p:pc0(i,r) i.tl:$fe(i)               e:$ele(i)  a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:tc(i,r)

* ------ Non-DEU: energy
         i:PA(e,r)$oil(e)                        q:(c0(e,r)*aeei(e,"c",r))       p:pc0(e,r) e.tl:$fe(e)  e:$ele(e)  a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:tc(e,r)
         i:PA(e,r)$ele(e)                        q:(c0(e,r)*aeei(e,"c",r))       p:pc0(e,r) e.tl:$fe(e)  e:$ele(e)  a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:tc(e,r)
         i:PA(e,r)$gas(e)                        q:(c0(e,r)*aeei(e,"c",r))       p:pc0(e,r) e.tl:$fe(e)  e:$ele(e)  a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:tc(e,r)
         i:PA(e,r)$col(e)                        q:(c0(e,r)*aeei(e,"c",r))       p:pc0(e,r) e.tl:$fe(e)  e:$ele(e)  a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:tc(e,r)

* ------ Carbon regimes:
* the # represents multiple inputs for PCO2, one for each element of set fe
         i:PCO2(r)#(fe)$notrad(r)                 q:(co2em(fe,"final",r) * aeei(fe,"c",r))        p:1e-6  fe.tl:          a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)
         i:PCO2W#(fe)$pco2w_r(r)$worldtrade       q:(co2em(fe,"final",r) * aeei(fe,"c",r))        p:1e-6  fe.tl:          a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)
         i:PCO2W#(fe)$eu28(r)$eutrade             q:(co2em(fe,"final",r) * aeei(fe,"c",r))        p:1e-6  fe.tl:          a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)
*         i:PCO2W#(fe)$deu(r)$detrade              q:(co2em(fe,"final",r) * aeei(fe,"c",r))        p:1e-6  fe.tl:          a:RA(r)
         i:PCO2W#(fe)$noneu28(r)$worldtrade2      q:(co2em(fe,"final",r) * aeei(fe,"c",r))        p:1e-6  fe.tl:          a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)
         i:PCO2_NETS#(fe)$eu28(r)$netstrade       q:(co2em(fe,"final",r) * aeei(fe,"c",r))        p:1e-6  fe.tl:
         i:PCO2_NETSr(r)#(fe)$eu28(r)$netstrade_r q:(co2em(fe,"final",r) * aeei(fe,"c",r))        p:1e-6  fe.tl:          a:ra(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) 

*--------16.10.2017 sector-specific targets in Germany
*         i:PCO2W#(fe)$(DEU_sec and eu28(r) and eutrade)   q:(co2em(fe,"final",r) * aeei(fe,"c",r))        p:1e-6  fe.tl:  a:RA(r)
         i:PCO2_DEU("buildings")#(fe)$(DEU_sec and deu(r))        q:(co2em(fe,"final",r) * aeei(fe,"c",r))    p:1e-6  fe.tl:  a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)

*--------16.10.2017 end

* ----- 26.01.2018
         i:PCO2(r)#(fe)$(deu(r) and detrade)                 q:(co2em(fe,"final",r) * aeei(fe,"c",r))        p:1e-6  fe.tl:          a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)

* ----- 01.04.2020  bta test
         i:PCO2W#(fe)$BTA_coa(r)$bta_test             q:(co2em(fe,"final",r) * aeei(fe,"c",r))        p:1e-6  fe.tl:          a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)

* ----- 03.11.2020  carbon tax with fixed price
         i:Pcarbon_tax(r)#(fe)$(carbon_tax_de AND deu(r))    q:(co2em(fe,"final",r) * aeei(fe,"c",r)) fe.tl:  a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

* ------ INCOME BALANCE --------------------------------------------------------

* ------ 18.04.2014 Rutherford GAMS-L 5.08.2013 (--> S:1 means constant marginal propensity to save)
$demand:RA(r)$(not HH_DISAG(r))    s:1

         d:PC(r)                 q:(vom("c",r) + vom("g",r))

         d:PINV(r)               q:(vom("i",r))

         e:PC("USA")             q:vb(r)

         e:PR(i,r)$rd0(i,r)      q:rd0(i,r)                      R:R_SUPPLY(i,r)$(rd0(i,r)$PRICETARGET(i,r))

         e:PSKL(r)               q:( evoa("skl",r) / (1-ursk0(r)))
         e:PSKL(r)               q:(-evoa("skl",r) / (1-ursk0(r)))       R:URSK(r)$ursk0(r)
         e:PUSK(r)               q:( evoa("usk",r) / (1-urun0(r)))
         e:PUSK(r)               q:(-evoa("usk",r) / (1-urun0(r)))       R:URUN(r)$urun0(r)
         e:RKR(r)$rsk            q:evoa("cap",r)
         e:RKG$gk                q:evoa("cap",r)
         e:RKX_ELE(gen,r)$ks_x(gen,r)    q:ks_x(gen,r)


***       E:NCAR(r,ct,cf)                  q:sndc(r,ct,cf)
* ------ 7.02.2016 auskommentiert (unten)
***        E:PXTHOUSE07(r,bt,bf)$bsdyr(r,bt,bf,"2011")      q:bsdyr(r,bt,bf,"2011")
***        E:PXTHOUSE10(r,bt,bf)$bsdyr(r,bt,bf,"2010")      q:bsdyr(r,bt,bf,"2010")
***        E:PXTHOUSE15(r,bt,bf)$bsdyr(r,bt,bf,"2015")      q:bsdyr(r,bt,bf,"2015")
***        E:PXTHOUSE20(r,bt,bf)$bsdyr(r,bt,bf,"2020")      q:bsdyr(r,bt,bf,"2020")
***        E:PXTHOUSE25(r,bt,bf)$bsdyr(r,bt,bf,"2025")      q:bsdyr(r,bt,bf,"2025")
***        E:PXTHOUSE30(r,bt,bf)$bsdyr(r,bt,bf,"2030")      q:bsdyr(r,bt,bf,"2030")


* ------ 24.04.2014 Add PA-Marginals from agent's endowment, in order to ensure income balance, if diffcost <> 1
         e:pa(i,r)               q:((-sum(reg, vafm_input(i,reg,r)*(1-thetax)*(1-diffcost(reg,r)))) * epa_on )

* ------ Carbon regimes
         e:PCO2(r)$carblim(r)$notrad(r)                  q:carblim(r)
         e:PCO2W$carblim(r)$pco2w_r(r)$worldtrade        q:carblim(r)
         e:PCO2W$carblim(r)$eu28(r)$eutrade              q:carblim(r)
         e:PCO2W$carblim(r)$noneu28(r)$worldtrade2       q:carblim(r)
         e:PCO2_ETS$carblim_ets(r)$eu28(r)$etstrade      q:carblim_ets(r)

         e:PCO2_NETS$carblim(r)$eu28(r)$netstrade        q:carblim(r)
         e:PCO2_NETSr(r)$carblim(r)$eu28(r)$netstrade_r  q:carblim(r)


*------28.09.2017 sector-specific CO2 reduction targets in Germany
*         e:PCO2W$carblim(r)$(DEU_sec and eu28(r) and eutrade) q:carblim(r)
         e:PCO2_DEU(sec)$(DEU_sec and deu(r) and carblim_sec(sec,r))                            q:carblim_sec(sec,r)

*         e:PCO2_DEU(sec)$(DEU_sec and deu(r) and carblim_sec(sec,r)) q:carblim(r)
*------28.09.2017 end

* ----- 26.01.2018
         e:PCO2(r)$(carblim_deu and deu(r) and detrade)                  q:carblim_deu

* ----- 08.02.2019 tax transfer 
*         e:PTAX(r)$deu(r)       q:(taxsum("total",r))

* ----- 01.04.2020 bta test
         e:PCO2W$carblim(r)$BTA_coa(r)$bta_test              q:carblim(r)       R:CO2_supply$(BTA_coa(r) and bta)
*         e:PCO2W$carblim(r)$BTA_coa(r)$bta_test              q:carblim(r)

* ----- 03.11.2020 carbon tax fixed price
        e:Pcarbon_tax(r)$(carbon_tax_de AND deu(r))         q:(carblim_sec("residential",r) + carblim_sec("transport",r))      R:carb_tax_m(r)$(carbon_tax_de AND deu(r))

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*        BLOCKS FOR HOUSEHOLDS DISAGGREGATION
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

$prod:TAXTRANS(r)$HH_DISAG(r)    s:1
         o:PGOV(r)      q:tax_gov_2_hh(r)
*         o:PGOV(r)      q:check_ra_inputs("RA_taxes",r)

         i:PTAX(r)      q:tax_gov_2_hh(r)
*         i:PTAX(r)      q:check_ra_inputs("RA_taxes",r)

$prod:LKTAX(r)$HH_DISAG(r)
         o:PITAX(r)     q:tax_hh_2_gov(r)
*         o:PITAX(r)     q:(check_ra_inputs("LKTAX",r))

         i:PLKTAX(r)    q:tax_hh_2_gov(r)
*         i:PLKTAX(r)    q:(check_ra_inputs("LKTAX",r))

$demand:GOV(r)$HH_DISAG(r)         
         d:PGOV(r)               q:tax_gov_2_hh(r)
*         d:PGOV(r)      q:check_ra_inputs("RA_taxes",r)

         d:PC_gov(r)             q:(vom("g",r))
         
         e:PR(i,r)$rd0(i,r)      q:rd0(i,r)                      R:R_SUPPLY(i,r)$(rd0(i,r)$PRICETARGET(i,r))

         e:pa(i,r)               q:((-sum(reg, vafm_input(i,reg,r)*(1-thetax)*(1-diffcost(reg,r)))) * epa_on )

         e:PLKTAX(r)             q:tax_hh_2_gov(r)
*         e:PLKTAX(r)             q:(check_ra_inputs("LKTAX",r))
         e:PLKTAX(r)$(test_lower_tax and DEU(r))             q:(-0.2 * tax_hh_2_gov(r))

         e:PC("USA")             q:vb(r)

* ------ Carbon regimes
         e:PCO2(r)$(carblim(r) AND notrad(r) AND NOT per_capita_dis)                                                              q:carblim(r)
         e:PCO2W$(carblim(r) AND pco2w_r(r) AND worldtrade AND NOT per_capita_dis)                                                q:carblim(r)
*         e:PCO2W$(carblim(r) AND eu28(r) AND eutrade AND NOT per_capita_dis)                                                      q:carblim(r)
         e:PCO2W$(carblim(r) AND eu28(r) AND eutrade AND NOT per_capita_dis)                                                      q:carblim(r)
         e:PCO2W$(carblim(r) AND noneu28(r) AND worldtrade2 AND NOT per_capita_dis)                                               q:carblim(r)
         e:PCO2_ETS$(carblim_ets(r) AND eu28(r) AND etstrade AND NOT per_capita_dis)                                              q:carblim_ets(r)

         e:PCO2_NETS$(carblim(r) AND eu28(r) AND netstrade AND NOT per_capita_dis)                                                q:carblim(r)
         e:PCO2_NETSr(r)$(carblim(r) AND eu28(r) AND netstrade_r AND NOT (per_capita_dis_NETSr OR inverse_co2_pay))                     q:carblim(r)

*------28.09.2017 sector-specific CO2 reduction targets in Germany
         e:PCO2_DEU(sec)$(DEU_sec and deu(r) and carblim_sec(sec,r) AND NOT per_capita_dis)                                       q:carblim_sec(sec,r)


* ----- 26.01.2018
         e:PCO2(r)$(carblim_deu and deu(r) and detrade AND NOT per_capita_dis)                                                    q:carblim_deu

         e:Pcarbon_tax(r)$(carbon_tax_de AND deu(r))         q:(carblim_sec("residential",r) + carblim_sec("transport",r))      R:carb_tax_m(r)$(carbon_tax_de AND deu(r))

* ------ Representative agent for disaggregated households
$demand:RA_HH(hh,r)$HH_DISAG(r)    s:8      c(s):1
         d:PC_hh(hh,r)           q:(vom("c",r) * hh_total_consumption_share(r,hh))  c:

         d:PINV(r)               q:(vom("i",r) * hh_savings_share(r,hh))            c:

*         d:PITAX(r)              q:(check_ra_inputs("LKTAX",r) * hh_tax_out_share(r,hh))
         d:PITAX(r)              q:(tax_hh_2_gov(r) * hh_tax_out_share(r,hh))
         d:PITAX(r)$(test_lower_tax and DEU(r))              q:(-0.2* tax_hh_2_gov(r) * hh_tax_out_share(r,hh))

         e:PSKL(r)               q:(( evoa("skl",r) / (1-ursk0(r))) * hh_skl_share(r,hh))
         e:PSKL(r)               q:((-evoa("skl",r) / (1-ursk0(r))) * hh_skl_share(r,hh))       R:URSK(r)$ursk0(r)

         e:PUSK(r)               q:(( evoa("usk",r) / (1-urun0(r))) * hh_usk_share(r,hh))
         e:PUSK(r)               q:((-evoa("usk",r) / (1-urun0(r))) * hh_usk_share(r,hh))       R:URUN(r)$urun0(r)

         e:RKR(r)$rsk            q:(evoa("cap",r) * hh_rkr_share(r,hh))
         e:RKG$gk                q:(evoa("cap",r)/5)
         e:RKX_ELE(gen,r)$ks_x(gen,r)    q:(ks_x(gen,r) * hh_rkx_ele_share(r,hh))

         e:PTAX(r)               q:(tax_gov_2_hh(r) * tax_delta(r) * hh_tax_in_share(r,hh))
*         e:PTAX(r)               q:(check_ra_inputs("RA_taxes",r) * hh_tax_in_share(r,hh))

* ------ Carbon regimes
*         e:PCO2(r)$(carblim(r) AND notrad(r) AND per_capita_dis)                                                                    q:(carblim(r)/5)
*         e:PCO2W$(carblim(r) AND pco2w_r(r) AND worldtrade AND per_capita_dis)                                                      q:(carblim(r)/5)
*         e:PCO2W$(carblim(r) AND eu28(r) AND eutrade AND per_capita_dis)                                                           q:(carblim(r)/5)
*         e:PCO2W$(carblim(r) AND eu28(r) AND eutrade AND per_capita_dis)                                                            q:(carblim(r)/5)
*         e:PCO2W$(carblim(r) AND noneu28(r) AND worldtrade2 AND per_capita_dis)                                                     q:(carblim(r)/5)
*         e:PCO2_ETS$(carblim_ets(r) AND eu28(r) AND etstrade AND per_capita_dis)                                                    q:(carblim_ets(r)/5)

*         e:PCO2_NETS$(carblim(r) AND eu28(r) AND netstrade AND per_capita_dis)                                                      q:(carblim(r)/5)
*         e:PCO2_NETSr(r)$(carblim(r) AND eu28(r) AND netstrade_r AND per_capita_dis)                                                q:(carblim(r)/5)  R:CO2_inv_pay(hh,r)$inverse_co2_pay  
         e:PCO2_NETSr(r)$(carblim(r) AND eu28(r) AND netstrade_r AND per_capita_dis_NETSr)                                           q:(carblim(r)/5)  
         e:PCO2_NETSr(r)$(carblim(r) AND eu28(r) AND netstrade_r AND inverse_co2_pay)                                                q:(carblim(r)/5)  R:CO2_inv_pay(hh,r)$(inverse_co2_pay)

*------28.09.2017 sector-specific CO2 reduction targets in Germany
         e:PCO2_DEU(sec)$(DEU_sec and deu(r) and carblim_sec(sec,r) AND per_capita_dis)                                             q:(carblim_sec(sec,r)/5)



* ----- 26.01.2018
         e:PCO2(r)$(carblim_deu and deu(r) and detrade AND per_capita_dis)                                                          q:(carblim_deu/5)


* ---------------------------------------------------------------------
* ----- 09.03.2020 - Classical consumption for disaggregated government
* ---------------------------------------------------------------------

$prod:CEMIT(r)$(EU28(r) and eutrade AND bta_test)
         o:PCO2Wr(r)        q:1
         i:PCO2W            q:1
         i:PUSK(r)          q:2e-6

$prod:CONVERT_PCO2_HH(hh,r)$(inverse_co2_pay AND HH_DISAG(r) AND netstrade_r)
         o:PCO2_inv_pay(hh,r)   q:1
         i:PCO2_NETSr(r)        q:1


$prod:C_gov(r)$HH_DISAG(r)       s:0.5   c:1     e:1     oil(e):0   col(e):0   gas(e):0

         o:PC_gov(r)                 Q:(vom("g",r))

* ------ All consumption goods (DEU: except transport, energy, cars and buildings)
* .tl alerts MPSGE that a set of nests are being declared
         i:PA(i,r)$(not e(i))                    q:c_gov0(i,r)                       p:pc_gov0(i,r) i.tl:$fe(i) c:$(not e(i)) e:$ele(i)  a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:tg(i,r)
* ------ Residential crude oil is NOT used for combustion --> no CO2-emissions --> no climate policy relevance
         i:PA(i,r)$cru(i)                        q:c_gov0(i,r)                       p:pc_gov0(i,r) i.tl:$fe(i)               e:$ele(i)  a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:tg(i,r)

* ------ Non-DEU: energy
         i:PA(e,r)$oil(e)                        q:(c_gov0(e,r)*aeei(e,"c",r))       p:pc_gov0(e,r) e.tl:$fe(e)  e:$ele(e)  a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:tg(e,r)
         i:PA(e,r)$ele(e)                        q:(c_gov0(e,r)*aeei(e,"c",r))       p:pc_gov0(e,r) e.tl:$fe(e)  e:$ele(e)  a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:tg(e,r)
         i:PA(e,r)$gas(e)                        q:(c_gov0(e,r)*aeei(e,"c",r))       p:pc_gov0(e,r) e.tl:$fe(e)  e:$ele(e)  a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:tg(e,r)
         i:PA(e,r)$col(e)                        q:(c_gov0(e,r)*aeei(e,"c",r))       p:pc_gov0(e,r) e.tl:$fe(e)  e:$ele(e)  a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:tg(e,r)

* ------ Carbon regimes:
* the # represents multiple inputs for PCO2, one for each element of set fe
         i:PCO2(r)#(fe)$notrad(r)                 q:(co2em(fe,"g",r) * aeei(fe,"c",r))        p:1e-6  fe.tl:          a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)
         i:PCO2W#(fe)$pco2w_r(r)$worldtrade       q:(co2em(fe,"g",r) * aeei(fe,"c",r))        p:1e-6  fe.tl:          a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)
         i:PCO2Wr(r)#(fe)$eu28(r)$eutrade             q:(co2em(fe,"g",r) * aeei(fe,"c",r))        p:1e-6  fe.tl:          a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)
*         i:PCO2W#(fe)$deu(r)$detrade              q:(co2em(fe,"final",r) * aeei(fe,"c",r))        p:1e-6  fe.tl:          a:RA(r)
         i:PCO2W#(fe)$noneu28(r)$worldtrade2      q:(co2em(fe,"g",r) * aeei(fe,"c",r))        p:1e-6  fe.tl:          a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)
         i:PCO2_NETS#(fe)$eu28(r)$netstrade       q:(co2em(fe,"g",r) * aeei(fe,"c",r))        p:1e-6  fe.tl:
         i:PCO2_NETSr(r)#(fe)$eu28(r)$netstrade_r q:(co2em(fe,"g",r) * aeei(fe,"c",r))        p:1e-6  fe.tl:             a:ra(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) 

*--------16.10.2017 sector-specific targets in Germany
*         i:PCO2W#(fe)$(DEU_sec and eu28(r) and eutrade)   q:(co2em(fe,"final",r) * aeei(fe,"c",r))        p:1e-6  fe.tl:  a:RA(r)
         i:PCO2_DEU("buildings")#(fe)$(DEU_sec and deu(r))               q:(co2em(fe,"g",r) * aeei(fe,"c",r))    p:1e-6  fe.tl:  a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)


*--------16.10.2017 end

* ----- 26.01.2018
         i:PCO2(r)#(fe)$(deu(r) and detrade)                 q:(co2em(fe,"g",r) * aeei(fe,"c",r))        p:1e-6  fe.tl:          a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)

* ---------------------------------------------------------------------
* ----- 09.03.2020 - Classical consumption for disaggregated households
* ---------------------------------------------------------------------

$prod:C_hh(hh,r)$(HH_DISAG(r) AND NOT h_t_cons_reg(r))       s:0.5   c:1     e:cons_sub(r)     oil(e):0   col(e):0   gas(e):0

         o:PC_hh(hh,r)                 Q:(vom("c",r) * hh_total_consumption_share(r,hh))

* ------ All consumption goods (DEU: except transport, energy, cars and buildings)
* .tl alerts MPSGE that a set of nests are being declared
         i:PA(i,r)$(not e(i))                    q:(c_hh0(i,r) * hh_sector_share(r,i,hh))                       p:pc_hh0(i,r) i.tl:$fe(i) c:$(not e(i)) e:$ele(i)  
+        a:RA(r)$(not HH_DISAG(r))    a:GOV(r)$HH_DISAG(r)   t:tp(i,r)
+        a:RA(r)$(not HH_DISAG(r))    a:GOV(r)$HH_DISAG(r)   n:tax_reb(r)$(sec_no_vat(i,r) AND no_vat)      m:(-tp(i,r))$(sec_no_vat(i,r) AND no_vat)

* ------ Residential crude oil is NOT used for combustion --> no CO2-emissions --> no climate policy relevance
         i:PA(i,r)$cru(i)                        q:(c_hh0(i,r) * hh_sector_share(r,i,hh))                       p:pc_hh0(i,r) i.tl:$fe(i)               e:$ele(i)  
+        a:RA(r)$(not HH_DISAG(r))    a:GOV(r)$HH_DISAG(r)   t:tp(i,r)
+        a:RA(r)$(not HH_DISAG(r))    a:GOV(r)$HH_DISAG(r)   n:tax_reb(r)$(sec_no_vat(i,r) AND no_vat)      m:(-tp(i,r))$(sec_no_vat(i,r) AND no_vat)

* ------ Non-DEU: energy
         i:PA(e,r)$oil(e)                        q:((c_hh0(e,r)*aeei(e,"c",r)) * hh_sector_share(r,e,hh))       p:pc_hh0(e,r) e.tl:$fe(e)  e:$ele(e)  
+        a:RA(r)$(not HH_DISAG(r))    a:GOV(r)$HH_DISAG(r)   t:tp(e,r)
+        a:RA(r)$(not HH_DISAG(r))    a:GOV(r)$HH_DISAG(r)   n:tax_reb(r)$(sec_no_vat(e,r) AND no_vat)      m:(-tp(e,r))$(sec_no_vat(e,r) AND no_vat)

         i:PA(e,r)$ele(e)                        q:((c_hh0(e,r)*aeei(e,"c",r)) * hh_sector_share(r,e,hh))       p:pc_hh0(e,r) e.tl:$fe(e)  e:$ele(e)  
+        a:RA(r)$(not HH_DISAG(r))    a:GOV(r)$HH_DISAG(r) t:tp(e,r)
+        a:RA(r)$(not HH_DISAG(r))    a:GOV(r)$HH_DISAG(r)   n:tax_reb(r)$(sec_no_vat(e,r) AND no_vat)      m:(-tp(e,r))$(sec_no_vat(e,r) AND no_vat)

         i:PA(e,r)$gas(e)                        q:((c_hh0(e,r)*aeei(e,"c",r)) * hh_sector_share(r,e,hh))       p:pc_hh0(e,r) e.tl:$fe(e)  e:$ele(e)  
+        a:RA(r)$(not HH_DISAG(r))    a:GOV(r)$HH_DISAG(r)   t:tp(e,r)
+        a:RA(r)$(not HH_DISAG(r))    a:GOV(r)$HH_DISAG(r)   n:tax_reb(r)$(sec_no_vat(e,r) AND no_vat)      m:(-tp(e,r))$(sec_no_vat(e,r) AND no_vat)

         i:PA(e,r)$col(e)                        q:((c_hh0(e,r)*aeei(e,"c",r)) * hh_sector_share(r,e,hh))       p:pc_hh0(e,r) e.tl:$fe(e)  e:$ele(e)  
+        a:RA(r)$(not HH_DISAG(r))    a:GOV(r)$HH_DISAG(r)   t:tp(e,r)
+        a:RA(r)$(not HH_DISAG(r))    a:GOV(r)$HH_DISAG(r)   n:tax_reb(r)$(sec_no_vat(e,r) AND no_vat)      m:(-tp(e,r))$(sec_no_vat(e,r) AND no_vat)

* ------ Carbon regimes:
* the # represents multiple inputs for PCO2, one for each element of set fe
         i:PCO2(r)#(fe)$notrad(r)                 q:(co2em(fe,"c",r) * aeei(fe,"c",r) * hh_sector_share(r,fe,hh))        p:1e-6  fe.tl:          a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)
         i:PCO2W#(fe)$pco2w_r(r)$worldtrade       q:(co2em(fe,"c",r) * aeei(fe,"c",r) * hh_sector_share(r,fe,hh))        p:1e-6  fe.tl:          a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)
         i:PCO2Wr(r)#(fe)$eu28(r)$eutrade             q:(co2em(fe,"c",r) * aeei(fe,"c",r) * hh_sector_share(r,fe,hh))        p:1e-6  fe.tl:          a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)
*         i:PCO2W#(fe)$deu(r)$detrade              q:(co2em(fe,"final",r) * aeei(fe,"c",r))        p:1e-6  fe.tl:          a:RA(r)
         i:PCO2W#(fe)$noneu28(r)$worldtrade2      q:(co2em(fe,"c",r) * aeei(fe,"c",r) * hh_sector_share(r,fe,hh))        p:1e-6  fe.tl:          a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)
         i:PCO2_NETS#(fe)$eu28(r)$netstrade       q:(co2em(fe,"c",r) * aeei(fe,"c",r) * hh_sector_share(r,fe,hh))        p:1e-6  fe.tl:

         i:PCO2_NETSr(r)#(fe)$eu28(r)$(netstrade_r AND NOT inverse_co2_pay)  
+        q:(co2em(fe,"c",r) * aeei(fe,"c",r) * hh_sector_share(r,fe,hh))        p:1e-6  fe.tl:          a:ra(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)
         
         i:PCO2_inv_pay(hh,r)#(fe)$eu28(r)$(netstrade_r AND inverse_co2_pay) 
+        q:(co2em(fe,"c",r) * aeei(fe,"c",r) * hh_sector_share(r,fe,hh))        p:1e-6  fe.tl:          a:ra(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)

*--------16.10.2017 sector-specific targets in Germany
*         i:PCO2W#(fe)$(DEU_sec and eu28(r) and eutrade)   q:(co2em(fe,"final",r) * aeei(fe,"c",r))        p:1e-6  fe.tl:  a:RA(r)
         i:PCO2_DEU("buildings")#(fe)$(DEU_sec and deu(r))               q:(co2em(fe,"c",r) * aeei(fe,"c",r) * hh_sector_share(r,fe,hh))    p:1e-6  fe.tl:  a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)

*--------16.10.2017 end

* ----- 26.01.2018
         i:PCO2(r)#(fe)$(deu(r) and detrade)                 q:(co2em(fe,"c",r) * aeei(fe,"c",r) * hh_sector_share(r,fe,hh))        p:1e-6  fe.tl:          a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)

* ----- 03.11.2020  carbon tax with fixed price
         i:Pcarbon_tax(r)#(fe)$(carbon_tax_de AND deu(r))    q:(co2em(fe,"c",r) * aeei(fe,"c",r)  * hh_sector_share(r,fe,hh)) fe.tl:  a:GOV(r)

* -------------------------------------------------------------------------------
* ----- 09.03.2020 - Consumption with specified nests for heat and transportation
* -------------------------------------------------------------------------------

$prod:C_hh(hh,r)$(HH_DISAG(r) and h_t_cons_reg(r))   s:0.5   c:1   tr:1    ct(tr):0   tr_e(ct):1  tr_o(tr_e):0   h:0.6   bd(h):0.1   e(h):cons_sub(r)     oil(e):0   col(e):0   gas(e):0

         o:PC_hh(hh,r)                 Q:(vom("c",r) * hh_total_consumption_share(r,hh))

* ------ All consumption goods (DEU: except transport, energy, cars and buildings)
* .tl alerts MPSGE that a set of nests are being declared
         i:PA(i,r)$non_h_t_goods(i)              q:(c_hh0(i,r) * hh_sector_share(r,i,hh))         p:pc_hh0(i,r)  c:$non_h_t_goods(i)  a:GOV(r)$HH_DISAG(r)
+        a:GOV(r)$HH_DISAG(r)   t:tp(i,r)
+        a:GOV(r)$HH_DISAG(r)   n:tax_reb(r)$(sec_no_vat(i,r) AND no_vat)      m:(-tp(i,r))$(sec_no_vat(i,r) AND no_vat)

* ------ Building and dwellings
         i:PA(i,r)$bui(i)                        q:(c_hh0(i,r) * hh_sector_share(r,i,hh))         p:pc_hh0(i,r)  bd:$bui(i)  a:GOV(r)$HH_DISAG(r)
+        a:GOV(r)$HH_DISAG(r)   t:tp(i,r)
+        a:GOV(r)$HH_DISAG(r)   n:tax_reb(r)$(sec_no_vat(i,r) AND no_vat)      m:(-tp(i,r))$(sec_no_vat(i,r) AND no_vat)

         i:PA(i,r)$dwe(i)                        q:(c_hh0(i,r) * hh_sector_share(r,i,hh))         p:pc_hh0(i,r)  bd:$dwe(i)  a:GOV(r)$HH_DISAG(r)
+        a:GOV(r)$HH_DISAG(r)   t:tp(i,r)
+        a:GOV(r)$HH_DISAG(r)   n:tax_reb(r)$(sec_no_vat(i,r) AND no_vat)      m:(-tp(i,r))$(sec_no_vat(i,r) AND no_vat)

* ------ Transportation, vehicles and fuels (oil, ele)
         i:PA(i,r)$trn(i)                        q:(c_hh0(i,r) * hh_sector_share(r,i,hh))         p:pc_hh0(i,r)  tr:$trn(i)  a:GOV(r)$HH_DISAG(r)
+        a:GOV(r)$HH_DISAG(r)   t:tp(i,r)
+        a:GOV(r)$HH_DISAG(r)   n:tax_reb(r)$(sec_no_vat(i,r) AND no_vat)      m:(-tp(i,r))$(sec_no_vat(i,r) AND no_vat)
         
         i:PA(i,r)$mvh(i)                        q:(c_hh0(i,r) * hh_sector_share(r,i,hh))         p:pc_hh0(i,r)  ct:$mvh(i)  a:GOV(r)$HH_DISAG(r)
+        a:GOV(r)$HH_DISAG(r)   t:tp(i,r)
+        a:GOV(r)$HH_DISAG(r)   n:tax_reb(r)$(sec_no_vat(i,r) AND no_vat)      m:(-tp(i,r))$(sec_no_vat(i,r) AND no_vat) 
         
         i:p_oil_trans(r)                        q:(c_hh0("oil_transport",r) * pc_hh0("oil",r) * hh_sector_share(r,"oil",hh))         tr_o:
+        a:GOV(r)$HH_DISAG(r)   t:tp("oil",r)
+        a:GOV(r)$HH_DISAG(r)   n:tax_reb(r)$(sec_no_vat("oil",r) AND no_vat)      m:(-tp("oil",r))$(sec_no_vat("oil",r) AND no_vat)
         
         i:p_ele_trans(r)                        q:(c_hh0("ele_transport",r) * pc_hh0("ele",r) * hh_sector_share(r,"ele",hh))         tr_e:
+        a:GOV(r)$HH_DISAG(r)   t:tp("ele",r)
+        a:GOV(r)$HH_DISAG(r)   n:tax_reb(r)$(sec_no_vat("ele",r) AND no_vat)      m:(-tp("ele",r))$(sec_no_vat("ele",r) AND no_vat)

* ------ Residential energy use
         i:PA(e,r)$oil(e)                        q:((c_hh0(e,r)*aeei(e,"c",r)) * hh_sector_share(r,e,hh))       p:pc_hh0(e,r) e.tl:$fe(e)  e:$ele(e)  
+        a:GOV(r)$HH_DISAG(r)   t:tp(e,r)
+        a:GOV(r)$HH_DISAG(r)   n:tax_reb(r)$(sec_no_vat(e,r) AND no_vat)      m:(-tp(e,r))$(sec_no_vat(e,r) AND no_vat)

         i:PA(e,r)$ele(e)                        q:((c_hh0(e,r)*aeei(e,"c",r)) * hh_sector_share(r,e,hh))       p:pc_hh0(e,r) e.tl:$fe(e)  e:$ele(e)  
+        a:GOV(r)$HH_DISAG(r) t:tp(e,r)
+        a:GOV(r)$HH_DISAG(r)   n:tax_reb(r)$(sec_no_vat(e,r) AND no_vat)      m:(-tp(e,r))$(sec_no_vat(e,r) AND no_vat)

         i:PA(e,r)$gas(e)                        q:((c_hh0(e,r)*aeei(e,"c",r)) * hh_sector_share(r,e,hh))       p:pc_hh0(e,r) e.tl:$fe(e)  e:$ele(e)  
+        a:GOV(r)$HH_DISAG(r)   t:tp(e,r)
+        a:GOV(r)$HH_DISAG(r)   n:tax_reb(r)$(sec_no_vat(e,r) AND no_vat)      m:(-tp(e,r))$(sec_no_vat(e,r) AND no_vat)

         i:PA(e,r)$col(e)                        q:((c_hh0(e,r)*aeei(e,"c",r)) * hh_sector_share(r,e,hh))       p:pc_hh0(e,r) e.tl:$fe(e)  e:$ele(e)  
+        a:GOV(r)$HH_DISAG(r)   t:tp(e,r)
+        a:GOV(r)$HH_DISAG(r)   n:tax_reb(r)$(sec_no_vat(e,r) AND no_vat)      m:(-tp(e,r))$(sec_no_vat(e,r) AND no_vat)

* ------ Residential crude oil is NOT used for combustion --> no CO2-emissions --> no climate policy relevance
         i:PA(i,r)$cru(i)                        q:(c_hh0(i,r) * hh_sector_share(r,i,hh))                       p:pc_hh0(i,r) i.tl:$fe(i)               e:$ele(i)  a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:tp(i,r)

* ------ Non-DEU: energy
         i:PA(e,r)$oil(e)                       q:((c_hh0(e,r)*aeei(e,"c",r)) * hh_sector_share(r,e,hh))      p:pc_hh0(e,r) e.tl:$fe(e)  e:$ele(e)  a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:tp(e,r)
         i:PA(e,r)$ele(e)                       q:((c_hh0(e,r)*aeei(e,"c",r)) * hh_sector_share(r,e,hh))         p:pc_hh0(e,r) e.tl:$fe(e)  e:$ele(e)  a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:tp(e,r)
         i:PA(e,r)$gas(e)                       q:((c_hh0(e,r)*aeei(e,"c",r)) * hh_sector_share(r,e,hh))         p:pc_hh0(e,r) e.tl:$fe(e)  e:$ele(e)  a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:tp(e,r)
         i:PA(e,r)$col(e)                       q:((c_hh0(e,r)*aeei(e,"c",r)) * hh_sector_share(r,e,hh))         p:pc_hh0(e,r) e.tl:$fe(e)  e:$ele(e)  a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:tp(e,r)

* ------ Carbon regimes (not for transportation):
* the # represents multiple inputs for PCO2, one for each element of set fe
         i:PCO2(r)#(fe)$notrad(r)                 q:(co2em(fe,"c",r) * aeei(fe,"c",r) * hh_sector_share(r,fe,hh))      p:1e-6  fe.tl:      a:GOV(r)
         i:PCO2W#(fe)$pco2w_r(r)$worldtrade       q:(co2em(fe,"c",r) * aeei(fe,"c",r) * hh_sector_share(r,fe,hh))      p:1e-6  fe.tl:      a:GOV(r)
         i:PCO2Wr(r)#(fe)$eu28(r)$eutrade         q:(co2em(fe,"c",r) * aeei(fe,"c",r) * hh_sector_share(r,fe,hh))      p:1e-6  fe.tl:      a:GOV(r)
         i:PCO2W#(fe)$noneu28(r)$worldtrade2      q:(co2em(fe,"c",r) * aeei(fe,"c",r) * hh_sector_share(r,fe,hh))      p:1e-6  fe.tl:      a:GOV(r)
         i:PCO2_NETS#(fe)$eu28(r)$netstrade       q:(co2em(fe,"c",r) * aeei(fe,"c",r) * hh_sector_share(r,fe,hh))      p:1e-6  fe.tl:

         i:PCO2_DEU("buildings")#(fe)$(DEU_sec and deu(r))               q:(co2em(fe,"c",r) * aeei(fe,"c",r) * hh_sector_share(r,fe,hh))   p:1e-6  fe.tl:  a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)
         i:PCO2(r)#(fe)$(deu(r) and detrade)                             q:(co2em(fe,"c",r) * aeei(fe,"c",r) * hh_sector_share(r,fe,hh))        p:1e-6  fe.tl:      a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)
         
         i:PCO2_NETSr(r)#(fe)$eu28(r)$(netstrade_r AND NOT inverse_co2_pay)  
+        q:(co2em(fe,"c",r) * aeei(fe,"c",r) * hh_sector_share(r,fe,hh))      p:1e-6  fe.tl:      a:GOV(r)

         i:PCO2_inv_pay(hh,r)#(fe)$eu28(r)$(netstrade_r AND inverse_co2_pay)   
+        q:(co2em(fe,"c",r) * aeei(fe,"c",r) * hh_sector_share(r,fe,hh))      p:1e-6  fe.tl:      a:GOV(r) 

* ----- 03.11.2020  carbon tax with fixed price
         i:Pcarbon_tax(r)#(fe)$(carbon_tax_de AND deu(r))    q:(co2em(fe,"c",r) * aeei(fe,"c",r)  * hh_sector_share(r,fe,hh)) fe.tl:  a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)

* ------ Carbon regimes (for transportation):
         i:PCO2(r)$notrad(r)                 q:(co2em("oil","c",r) * aeei("oil","c",r) * HH_ENERGY_SHARE(R,"OIL","TRANSPORT","2011") * hh_sector_share(r,"oil",hh))        p:1e-6  tr_o:           a:GOV(r)
         i:PCO2W$pco2w_r(r)$worldtrade       q:(co2em("oil","c",r) * aeei("oil","c",r) * HH_ENERGY_SHARE(R,"OIL","TRANSPORT","2011") * hh_sector_share(r,"oil",hh))        p:1e-6  tr_o:           a:GOV(r)
         i:PCO2W$eu28(r)$eutrade             q:(co2em("oil","c",r) * aeei("oil","c",r) * HH_ENERGY_SHARE(R,"OIL","TRANSPORT","2011") * hh_sector_share(r,"oil",hh))        p:1e-6  tr_o:           a:GOV(r)
         i:PCO2W$noneu28(r)$worldtrade2      q:(co2em("oil","c",r) * aeei("oil","c",r) * HH_ENERGY_SHARE(R,"OIL","TRANSPORT","2011") * hh_sector_share(r,"oil",hh))        p:1e-6  tr_o:           a:GOV(r)
         i:PCO2_NETS$eu28(r)$netstrade       q:(co2em("oil","c",r) * aeei("oil","c",r) * HH_ENERGY_SHARE(R,"OIL","TRANSPORT","2011") * hh_sector_share(r,"oil",hh))        p:1e-6  tr_o:           a:GOV(r)
         
         i:PCO2_DEU("buildings")#(fe)$(DEU_sec and deu(r))               q:(co2em("oil","c",r) * aeei("oil","c",r) * HH_ENERGY_SHARE(R,"OIL","TRANSPORT","2011") * hh_sector_share(r,"oil",hh))
+        p:1e-6  tr_o:  a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)

         i:PCO2_NETSr(r)$eu28(r)$(netstrade_r  AND NOT inverse_co2_pay)
+        q:(co2em("oil","c",r) * aeei("oil","c",r) * HH_ENERGY_SHARE(R,"OIL","TRANSPORT","2011") * hh_sector_share(r,"oil",hh))        p:1e-6  tr_o:           a:GOV(r) 

         i:PCO2_inv_pay(hh,r)$eu28(r)$(netstrade_r AND inverse_co2_pay)
+        q:(co2em("oil","c",r) * aeei("oil","c",r) * HH_ENERGY_SHARE(R,"OIL","TRANSPORT","2011") * hh_sector_share(r,"oil",hh))        p:1e-6  tr_o:           a:GOV(r)       

         i:Pcarbon_tax(r)#(fe)$(carbon_tax_de AND deu(r))    q:(co2em("oil","c",r) * aeei("oil","c",r)  * hh_energy_share(r,"oil","transport","2011") * hh_sector_share(r,"oil",hh)) 
+        tr_o:  a:GOV(r)   
* --------------------------------------------------------------------------------

$prod:FF_trans(r)$(HH_DISAG(r) and h_t_cons_reg(r))   s:0     oil(s):0
    
         o:p_oil_trans(r)                    q:(c_hh0("oil_transport",r) * pc_hh0("oil",r))
         i:PA(e,r)$oil(e)                    q:(c_hh0("oil_transport",r) * pc_hh0("oil",r))      p:pc_hh0(e,r)     oil:          

*--------16.10.2017 end

* ----- 26.01.2018
         i:PCO2(r)#(fe)$(deu(r) and detrade)                 q:(co2em("oil","c",r) * aeei("oil","c",r) * HH_ENERGY_SHARE(R,"OIL","TRANSPORT","2011"))        p:1e-6  oil:          a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)

$prod:ELE_trans(r)$(HH_DISAG(r) and h_t_cons_reg(r))   s:0
    
         o:p_ele_trans(r)              q:(c_hh0("ele_transport",r) * pc_hh0("ele",r))

         i:PA(e,r)$ele(e)              q:c_hh0("ele_transport",r)            p:pc_hh0(e,r) 


* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


* ------ ZERO PROFIT Conditions (Production of Y-Output) --------------------- *

* --> (1) INDUSTRY production (all but COL, GAS, CRU, OIL, ELE, TRN) --> TRN is included?!
* --> only difference to (3) is the nesting of PSKL and PUSK (lab: vs. va:)

* ------ 10.12.2014 (Erste Zeile Original)
$prod:Y(i,r)$nr(i,r)   s:0  vae(s):0.5  va(vae):1  e(vae):e_sub(r)  nel(e):0.5  lqd(nel):2  oil(lqd):0  col(nel):0  gas(lqd):0
*$prod:Y(i,r)$nr(i,r)   s:0  vae(s):0.25  va(vae):1  e(vae):0.1  nel(e):0.5  lqd(nel):2  oil(lqd):0  col(nel):0  gas(lqd):0

* ------ 23.05.2014
         o:PY(i,r)               q:vom(i,r)                     p:py0(i,r)           a:RA(r)$(not HH_DISAG(r))  a:GOV(r)$HH_DISAG(r) t:ty(i,r) N:GF_SUB(i,r)$(vom(i,r) AND gf_shr(i,r) AND gf_allow) M:(-1)$(vom(i,r) AND gf_shr(i,r) AND gf_allow)

         i:PA(j,r)$(not e(j))    q:vafm(j,i,r)                  p:pai0(j,i,r)               a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:ti(j,i,r)
         i:PA(e,r)$cru(e)        q:vafm(e,i,r)                  p:pai0(e,i,r)               a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:ti(e,i,r)
         i:PA(fe,r)              q:(vafm(fe,i,r)*aeei(fe,i,r))  p:pai0(fe,i,r)  fe.tl:      a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:ti(fe,i,r)
         I:PA(e,r)$ele(e)        q:(vafm(e,i,r)*aeei(e,i,r))    p:pai0(e,i,r)   e:$ele(e)   a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:ti(e,i,r)

*         i:PAY(j,i,r)$(not e(j))    q:vafm(j,i,r)                  p:pai0(j,i,r)               a:RA(r) t:ti(j,i,r)
*         i:PAY(e,i,r)$cru(e)        q:vafm(e,i,r)                  p:pai0(e,i,r)               a:RA(r) t:ti(e,i,r)
*         i:PAY(fe,i,r)              q:(vafm(fe,i,r)*aeei(fe,i,r))  p:pai0(fe,i,r)  fe.tl:      a:RA(r) t:ti(fe,i,r)
*         I:PAY(e,i,r)$ele(e)        q:(vafm(e,i,r)*aeei(e,i,r))    p:pai0(e,i,r)   e:$ele(e)   a:RA(r) t:ti(e,i,r)

         i:PSKL(r)$(NOT (diss_factor_tax AND HH_DISAG(r)))                    q:skld0(i,r)           va:
         i:PSKL(r)$(diss_factor_tax AND HH_DISAG(r))                          q:skld0(i,r)           p:pf0("skl",i,r)      va:        a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:rtf("skl",i,r)
+        a:GOV(r)$HH_DISAG(r)   n:tax_reb(r)$(sec_no_lab('skl',i,r) AND low_lab_tax)      m:(-rtf('skl',i,r))$(sec_no_lab('skl',i,r) AND low_lab_tax)
         
         i:PUSK(r)$(NOT (diss_factor_tax AND HH_DISAG(r)))                    q:uskd0(i,r)           va:
         i:PUSK(r)$(diss_factor_tax AND HH_DISAG(r))                          q:uskd0(i,r)           p:pf0("usk",i,r)      va:        a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:rtf("usk",i,r)
+        a:GOV(r)$HH_DISAG(r)   n:tax_reb(r)$(sec_no_lab('usk',i,r) AND low_lab_tax)      m:(-rtf('usk',i,r))$(sec_no_lab('usk',i,r) AND low_lab_tax)

         i:RKG$(gk)                                                           q:kd0(i,r)             va:
         
         i:RKR(r)$(rsk AND NOT (diss_factor_tax AND HH_DISAG(r)))             q:kd0(i,r)             va:
         i:RKR(r)$(rsk AND diss_factor_tax AND HH_DISAG(r))                   q:kd0(i,r)             p:pf0("cap",i,r)      va:        a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:rtf("cap",i,r)
*         i:RKR(r)$rsk                                           q:kd0(i,r)             va:
         i:PR(I,r)$rd0(i,r)                                     q:rd0(i,r)             va:

* ------ Carbon regimes:
         i:PCO2(r)#(fe)$notrad(r)                                q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:
         i:PCO2W#(fe)$pco2w_r(r)$worldtrade                      q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:
         i:PCO2W#(fe)$eu28(r)$eutrade                            q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:
*         i:PCO2Wr(r)#(fe)$eu28(r)$eutrade                            q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:
*         i:PCO2W#(fe)$deu(r)$detrade                             q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:
         i:PCO2W#(fe)$noneu28(r)$worldtrade2                     q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:
         i:PCO2_ETS#(fe)$eu28(r)$ets(i)$etstrade                 q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:     a:ra(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)  
         i:PCO2_NETS#(fe)$eu28(r)$(not ets(i))$netstrade         q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:
         i:PCO2_NETSr(r)#(fe)$eu28(r)$(not ets(i))$netstrade_r   q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:     a:ra(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)  

*-----28.09.2017 sector-specific CO2 reduction targets in Germany
*         i:PCO2W#(fe)$(DEU_sec and eu28(r) and eutrade)                           q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:
         i:PCO2_DEU(sec)#(fe)$(DEU_sec and deu(r) and sec2cluster(sec,i))         q:(co2em(fe,i,r) * aeei(fe,i,r))    p:1e-6  fe.tl:

* ----- 03.11.2020  carbon tax with fixed price
         i:Pcarbon_tax(r)#(fe)$(carbon_tax_de AND deu(r) AND trn(i))    q:(co2em(fe, i,r) * aeei(fe, i,r)) fe.tl:  a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)

*-----28.09.2017 end

* ----- 26.01.2018
         i:PCO2(r)#(fe)$(detrade and deu(r))                                q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:

* ----- 01.04.2020 BTA test
         i:PCO2W#(fe)$BTA_coa(r)$bta_test                            q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:

* Textmarke SE

* --> (3) REFINED OIL production
* --> only difference to (2) is the nesting of PSKL and PUSK (lab: vs. va:)
* --> REFINING (oil Production), which is capital intensiv , define va <=0.5    ??????

$prod:Y(i,r)$(vom(i,r)$oil(i))   s:0  vae(s):0.5  va(vae):0.2 lab(va):0.5  e(vae):0.1  nel(e):0.5 lqd(nel):2  oil(lqd):0  col(nel):0  gas(lqd):0

* ------ 23.05.2014
         o:PY(i,r)               q:vom(i,r)                      p:py0(i,r)           a:RA(r)$(not HH_DISAG(r))    a:GOV(r)$HH_DISAG(r)   t:ty(i,r)

         i:PA(j,r)$(not e(j))    q:vafm(j,i,r)                   p:pai0(j,i,r)               a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:ti(j,i,r)
         i:PA(e,r)$cru(e)        q:vafm(e,i,r)                   p:pai0(e,i,r)               a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:ti(e,i,r)
         i:PA(fe,r)              q:(vafm(fe,i,r)*aeei(fe,i,r))   p:pai0(fe,i,r)  fe.tl:      a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:ti(fe,i,r)
         I:PA(e,r)$ele(e)        q:(vafm(e,i,r)*aeei(e,i,r))     p:pai0(e,i,r)   e:$ele(e)   a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:ti(e,i,r)

*         i:PAY(j,i,r)$(not e(j))    q:vafm(j,i,r)                   p:pai0(j,i,r)               a:RA(r) t:ti(j,i,r)
*         i:PAY(e,i,r)$cru(e)        q:vafm(e,i,r)                   p:pai0(e,i,r)               a:RA(r) t:ti(e,i,r)
*         i:PAY(fe,i,r)              q:(vafm(fe,i,r)*aeei(fe,i,r))   p:pai0(fe,i,r)  fe.tl:      a:RA(r) t:ti(fe,i,r)
*         I:PAY(e,i,r)$ele(e)        q:(vafm(e,i,r)*aeei(e,i,r))     p:pai0(e,i,r)   e:$ele(e)   a:RA(r) t:ti(e,i,r)

         i:PSKL(r)$(NOT (diss_factor_tax AND HH_DISAG(r)))                    q:skld0(i,r)           lab:
         i:PSKL(r)$(diss_factor_tax AND HH_DISAG(r))                          q:skld0(i,r)           p:pf0("skl",i,r)      lab:        a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:rtf("skl",i,r)
+        a:GOV(r)$HH_DISAG(r)   n:tax_reb(r)$(sec_no_lab('skl',i,r) AND low_lab_tax)      m:(-rtf('skl',i,r))$(sec_no_lab('skl',i,r) AND low_lab_tax)

         i:PUSK(r)$(NOT (diss_factor_tax AND HH_DISAG(r)))                    q:uskd0(i,r)           lab:
         i:PUSK(r)$(diss_factor_tax AND HH_DISAG(r))                          q:uskd0(i,r)           p:pf0("usk",i,r)      lab:        a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:rtf("usk",i,r)
+        a:GOV(r)$HH_DISAG(r)   n:tax_reb(r)$(sec_no_lab('usk',i,r) AND low_lab_tax)      m:(-rtf('usk',i,r))$(sec_no_lab('usk',i,r) AND low_lab_tax)

         i:RKG$(gk)                                                           q:kd0(i,r)             va:
         
         i:RKR(r)$(rsk AND NOT (diss_factor_tax AND HH_DISAG(r)))             q:kd0(i,r)             va:
         i:RKR(r)$(rsk AND diss_factor_tax AND HH_DISAG(r))                   q:kd0(i,r)             p:pf0("cap",i,r)      va:        a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:rtf("cap",i,r)
*         i:RKR(r)$rsk                                           q:kd0(i,r)      va:
         i:PR(I,r)$rd0(i,r)                                                   q:rd0(I,r)      va:

* ------ Carbon regimes:
         i:PCO2(r)#(fe)$notrad(r)                                q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:
         i:PCO2W#(fe)$pco2w_r(r)$worldtrade                      q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:
         i:PCO2W#(fe)$eu28(r)$eutrade                            q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:
*         i:PCO2Wr(r)#(fe)$eu28(r)$eutrade                            q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:
*         i:PCO2W#(fe)$deu(r)$detrade                             q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:
         i:PCO2W#(fe)$noneu28(r)$worldtrade2                     q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:
         i:PCO2_ETS#(fe)$eu28(r)$ets(i)$etstrade                 q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:     a:ra(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)  

*-------17.10.2017 sector-specific CO2 reduction targets in Germany
*    i:PCO2W#(fe)$(DEU_sec and eu28(r) and eutrade)                      q:(co2em(fe,i,r) * aeei(fe,i,r))    p:1e-6  fe.tl:
    i:PCO2_DEU(sec)#(fe)$(DEU_sec and deu(r) and sec2cluster(sec,i))    q:(co2em(fe,i,r) * aeei(fe,i,r))    p:1e-6  fe.tl:
*-------17.10.2017 end

* ----- 26.01.2018
         i:PCO2(r)#(fe)$(detrade and deu(r))                                q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:

* ----- 01.04.2020 BTA test
         i:PCO2W#(fe)$BTA_coa(r)$bta_test                            q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:

* --> 10) EXHAUSTIBLE Energy production [CRU, GAS, COL]
* ------------------------------------------------------------------------------
*        Fossil fuel production activity (crude, gas and coal):
*        COAL, GAS, CRU Production are fixed technology description that id=0,
*        but id may be substituted against resources,
*        because more production faclities (id) provided
*        enable more efficient resource extraction
* ------------------------------------------------------------------------------
* ------ 20.05.2014 Fossil fuel production does not create CO2 emissions, because they are not burned here!
$prod:Y(i,r)$(vom(i,r)$xe(i))  s:(esub_es(i,r))  id:0   lab(id):0

         o:PY(i,r)               q:vom(i,r)                      p:py0(i,r)           a:RA(r)$(not HH_DISAG(r))    a:GOV(r)$HH_DISAG(r)   t:ty(i,r)

*T:tax11(i,r)    A:RA(r)$baw(r)

         i:PA(j,r)$(not e(j))    q:vafm(j,i,r)                   p:pai0(j,i,r)           a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:ti(j,i,r)    id:
         i:PA(e,r)$cru(e)        q:vafm(e,i,r)                   p:pai0(e,i,r)           a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:ti(e,i,r)    id:
         i:PA(fe,r)              q:(vafm(fe,i,r)*aeei(fe,i,r))   p:pai0(fe,i,r)          a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:ti(fe,i,r)   id:
         i:PA(e,r)$ele(e)        q:(vafm(e,i,r)*aeei(e,i,r))     p:pai0(e,i,r)           a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:ti(e,i,r)    id:

*         i:PAY(j,i,r)$(not e(j))    q:vafm(j,i,r)                   p:pai0(j,i,r)           a:RA(r) t:ti(j,i,r)    id:
*         i:PAY(e,i,r)$cru(e)        q:vafm(e,i,r)                   p:pai0(e,i,r)           a:RA(r) t:ti(e,i,r)    id:
*         i:PAY(fe,i,r)              q:(vafm(fe,i,r)*aeei(fe,i,r))   p:pai0(fe,i,r)          a:RA(r) t:ti(fe,i,r)   id:
*         i:PAY(e,i,r)$ele(e)        q:(vafm(e,i,r)*aeei(e,i,r))     p:pai0(e,i,r)           a:RA(r) t:ti(e,i,r)    id:

         i:PSKL(r)$(NOT (diss_factor_tax AND HH_DISAG(r)))                    q:skld0(i,r)           lab:
         i:PSKL(r)$(diss_factor_tax AND HH_DISAG(r))                          q:skld0(i,r)           p:pf0("skl",i,r)      lab:        a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:rtf("skl",i,r)
+        a:GOV(r)$HH_DISAG(r)   n:tax_reb(r)$(sec_no_lab('skl',"ele",r) AND low_lab_tax)      m:(-rtf('skl',"ele",r))$(sec_no_lab('skl',"ele",r) AND low_lab_tax)

         i:PUSK(r)$(NOT (diss_factor_tax AND HH_DISAG(r)))                    q:uskd0(i,r)           lab:
         i:PUSK(r)$(diss_factor_tax AND HH_DISAG(r))                          q:uskd0(i,r)           p:pf0("usk",i,r)      lab:        a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:rtf("usk",i,r)
+        a:GOV(r)$HH_DISAG(r)   n:tax_reb(r)$(sec_no_lab('usk',"ele",r) AND low_lab_tax)      m:(-rtf('usk',"ele",r))$(sec_no_lab('usk',"ele",r) AND low_lab_tax)

*         i:RKR(r)$rsk                                           q:kd0(i,r)            id:
         i:RKR(r)$(rsk AND NOT (diss_factor_tax AND HH_DISAG(r)))             q:kd0(i,r)             id:
         i:RKR(r)$(rsk AND diss_factor_tax AND HH_DISAG(r))                   q:kd0(i,r)             p:pf0("cap",i,r)      id:        a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:rtf("cap",i,r)
         
         i:RKG$gk                                                             q:kd0(i,r)             id:
         i:PR(i,r)$rd0(i,r)                                                   q:rd0(i,r)

* ------ Carbon regimes:
*         i:PCO2W#(fe)$worldtrade                                 q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:
*         i:PCO2W#(fe)$noneu28(r)$worldtrade2                     q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:
*         i:PCO2_NETS#(fe)$eu28(r)$(not ets(i))$netstrade         q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:



* --> 4) Electricity production (aggregating generation technologies)
* ------ 10.12.2014  Aufteilung in EU28 und nicht-EU28 um S: und/oder BM: einzuschränken (s.u.)
$prod:Y(i,r)$(ele(i) and EU28(r))     s:0.8    pl:5.0  og(pl):2.5      bm:5.0  bl(bm):8.0      ml(bm):5.0     ee:5.0
*$prod:Y(i,r)$(ele(i) and EU28(r))     s:0.8    pl:5.0  og(pl):2.5      bm:1.0  bl(bm):8.0      ml(bm):16.0     ee:5.0

         o:PY(i,r)       q:vom(i,r)       p:py0(i,r)    a:RA(r)$(not HH_DISAG(r))    a:GOV(r)$HH_DISAG(r)   t:ty(i,r)    N:REBATE_DIFF(r)$diffrebate(r)   N:YLO(i,r)$ylo_on(i,r)  M:(-1)$ylo_on(i,r)

         i:PGEN(gen,r)   q:out_gen(gen,r) bl:$base(gen)  ml:$middle(gen)   og:$og(gen)    pl:$peak(gen)


$prod:Y(i,r)$(ele(i) and not EU28(r)) s:0.8    pl:5.0  og(pl):2.5      bm:4.0  bl(bm):8.0      ml(bm):5.0     ee:5.0
*$prod:Y(i,r)$(ele(i) and not EU28(r)) s:0.8    pl:5.0  og(pl):2.5      bm:4.0  bl(bm):8.0      ml(bm):16.0     ee:5.0
         o:PY(i,r)       q:vom(i,r)       p:py0(i,r)    a:RA(r)$(not HH_DISAG(r))    a:GOV(r)$HH_DISAG(r)        t:ty(i,r)       N:REBATE_DIFF(r)$diffrebate(r)  N:YLO(i,r)$ylo_on(i,r) M:(-1)$ylo_on(i,r)
         i:PGEN(gen,r)   q:out_gen(gen,r) bl:$base(gen)  ml:$middle(gen)   og:$og(gen)    pl:$peak(gen)


* --> 6) electricity generation with extant capital (eeg-geförderte erneuerbare)
* --> only difference to (7) are o:pgen, i:pq, i:rkx_ele(gen,r)$eupol and i:rkx_ele(gen,r)$(not eupol)

$prod:ELEx(gen,r)$((out_gen(gen,r))$(ks_x(gen,r))$(reg(gen)))    s:0

         o:PGEN(gen,r)           q:out_gen(gen,r) 

         i:PA(i,r)$(not e(i))    q:vafm_input(i,gen,r)                    p:pai0(i,"ele",r)   a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)   t:ti(i,"ele",r)
         i:PA(e,r)$cru(e)        q:vafm_input(e,gen,r)                    p:pai0(e,"ele",r)   a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)   t:ti(e,"ele",r)
         i:PA(e,r)$fe(e)         q:(vafm_input(e,gen,r)*aeei_elex(gen,r)) p:pai0(e,"ele",r)   a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)   t:ti(e,"ele",r)
         i:PA(e,r)$ele(e)        q:(vafm_input(e,gen,r)*aeei(e,"ele",r))  p:pai0(e,"ele",r)   a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)   t:ti(e,"ele",r)

*         i:PSKL(r)               q:skl_input(gen,r)
*         i:PUSK(r)               q:usk_input(gen,r)

         i:PSKL(r)$(NOT (diss_factor_tax AND HH_DISAG(r)))                    q:skl_input(gen,r)           
         i:PSKL(r)$(diss_factor_tax AND HH_DISAG(r))                          q:skl_input(gen,r)           p:pf0("skl","ele",r)       a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:rtf("skl","ele",r)
+        a:GOV(r)$HH_DISAG(r)   n:tax_reb(r)$(sec_no_lab('skl',"ele",r) AND low_lab_tax)      m:(-rtf('skl',"ele",r))$(sec_no_lab('skl',"ele",r) AND low_lab_tax)
         
         i:PUSK(r)$(NOT (diss_factor_tax AND HH_DISAG(r)))                    q:usk_input(gen,r)           
         i:PUSK(r)$(diss_factor_tax AND HH_DISAG(r))                          q:usk_input(gen,r)           p:pf0("usk","ele",r)       a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:rtf("usk","ele",r)
+        a:GOV(r)$HH_DISAG(r)   n:tax_reb(r)$(sec_no_lab('usk',"ele",r) AND low_lab_tax)      m:(-rtf('usk',"ele",r))$(sec_no_lab('usk',"ele",r) AND low_lab_tax)

*         i:RKX_ELE(gen,r)        q:cap_input0(gen,r)
         i:RKX_ELE(gen,r)$(NOT (diss_factor_tax AND HH_DISAG(r)))             q:cap_input0(gen,r)
         i:RKX_ELE(gen,r)$(diss_factor_tax AND HH_DISAG(r))                   q:cap_input0(gen,r)          p:pf0("cap","ele",r)       a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:rtf("cap","ele",r)

* ------ Carbon regimes:
         i:PCO2(r)#(fe)$notrad(r)                                q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6
         i:PCO2W#(fe)$pco2w_r(r)$worldtrade                      q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6
         i:PCO2W#(fe)$eu28(r)$eutrade                            q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6
*         i:PCO2Wr(r)#(fe)$eu28(r)$eutrade                            q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6
*         i:PCO2W#(fe)$deu(r)$detrade                             q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6
         i:PCO2W#(fe)$noneu28(r)$worldtrade2                     q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6
         i:PCO2_ETS#(fe)$eu28(r)$etstrade                        q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6     a:ra(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)    

*-------13.10.2017 Sector-specific targets for Germany
*    i:PCO2W#(fe)$(DEU_sec and eu28(r) and eutrade)           q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6
        i:PCO2_DEU(sec)#(fe)$(DEU_sec and deu(r) and sec2cluster(sec,gen))     q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6
*-------13.10.2017 end

* ----- 26.01.2018
         i:PCO2(r)#(fe)$( detrade and deu(r))                                  q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6

* ----- 01.04.2020
         i:PCO2W#(fe)$BTA_coa(r)$bta_test                            q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6

* --> 7) electricity production of extant capital (nicht eeg-gef�rderte erneuerbare: bNUC, bBC, bHC, bGAS, bOIL, bCCS, mHC, mGAS, mOIL, mCCS, pOIL, pGAS, pHYDRO)
* --> only difference to (6) are o:pgen, i:pq, i:rkx_ele(gen,r)$eupol and i:rkx_ele(gen,r)$(not eupol)

$prod:ELEx(gen,r)$((out_gen(gen,r))$(ks_x(gen,r))$(not reg(gen)))    s:0

         o:PGEN(gen,r)           q:out_gen(gen,r)

         i:PA(i,r)$(not e(i))    q:vafm_input(i,gen,r)                    p:pai0(i,"ele",r)   a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)   t:ti(i,"ele",r)
         i:PA(e,r)$cru(e)        q:vafm_input(e,gen,r)                    p:pai0(e,"ele",r)   a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)   t:ti(e,"ele",r)
         i:PA(e,r)$fe(e)         q:(vafm_input(e,gen,r)*aeei_elex(gen,r)) p:pai0(e,"ele",r)   a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)   t:ti(e,"ele",r)
         i:PA(e,r)$ele(e)        q:(vafm_input(e,gen,r)*aeei(e,"ele",r))  p:pai0(e,"ele",r)   a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)   t:ti(e,"ele",r)

*         i:PSKL(r)               q:skl_input(gen,r)
*         i:PUSK(r)               q:usk_input(gen,r)

         i:PSKL(r)$(NOT (diss_factor_tax AND HH_DISAG(r)))                    q:skl_input(gen,r)           
         i:PSKL(r)$(diss_factor_tax AND HH_DISAG(r))                          q:skl_input(gen,r)           p:pf0("skl","ele",r)       a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:rtf("skl","ele",r)
+        a:GOV(r)$HH_DISAG(r)   n:tax_reb(r)$(sec_no_lab('skl',"ele",r) AND low_lab_tax)      m:(-rtf('skl',"ele",r))$(sec_no_lab('skl',"ele",r) AND low_lab_tax)
         
         i:PUSK(r)$(NOT (diss_factor_tax AND HH_DISAG(r)))                    q:usk_input(gen,r)           
         i:PUSK(r)$(diss_factor_tax AND HH_DISAG(r))                          q:usk_input(gen,r)           p:pf0("usk","ele",r)       a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:rtf("usk","ele",r)
+        a:GOV(r)$HH_DISAG(r)   n:tax_reb(r)$(sec_no_lab('usk',"ele",r) AND low_lab_tax)      m:(-rtf('usk',"ele",r))$(sec_no_lab('usk',"ele",r) AND low_lab_tax)

*         i:RKX_ELE(gen,r)        q:cap_input0(gen,r)
         i:RKX_ELE(gen,r)$(NOT (diss_factor_tax AND HH_DISAG(r)))             q:cap_input0(gen,r)
         i:RKX_ELE(gen,r)$(diss_factor_tax AND HH_DISAG(r))                   q:cap_input0(gen,r)          p:pf0("cap","ele",r)       a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:rtf("cap","ele",r)

* ------ Carbon regimes:
         i:PCO2(r)#(fe)$notrad(r)                                q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6
         i:PCO2W#(fe)$pco2w_r(r)$worldtrade                      q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6
         i:PCO2W#(fe)$eu28(r)$eutrade                            q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6
*         i:PCO2Wr(r)#(fe)$eu28(r)$eutrade                            q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6
*         i:PCO2W#(fe)$deu(r)$detrade                             q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6
         i:PCO2W#(fe)$noneu28(r)$worldtrade2                     q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6
         i:PCO2_ETS#(fe)$eu28(r)$etstrade                        q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6     a:ra(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) 

*-------13.10.2017 Sector-specific targets for Germany
*         i:PCO2W#(fe)$(DEU_sec and eu28(r) and eutrade)                      q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6
         i:PCO2_DEU(sec)#(fe)$(DEU_sec and deu(r) and sec2cluster(sec,gen))  q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6
*-------13.10.2017 end

* ----- 26.01.2018
         i:PCO2(r)#(fe)$( detrade and deu(r))                                q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6

* ----- 01.04.2020
         i:PCO2W#(fe)$BTA_coa(r)$bta_test                            q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6

* --> 8) electricity generation from new capital (eeg-geförderte erneuerbare)
* --> completely different to (9) except all carbon inputs i:pcarb...
* --> see difference to (6) in separate txt-file

$prod:ELEn(gen,r)$out_gen(gen,r)$ks_n(gen,r)$reg(gen)     s:0  lab(s):0

         o:PGEN(gen,r)           q:out_gen(gen,r) a:RA(r)$(not HH_DISAG(r))    a:GOV(r)$HH_DISAG(r)   t:(1-diffcost(gen,r))

         i:PA(i,r)$(not fe(i))   q:(vafm_input(i,gen,r)*(diffcost(gen,r)))                   p:pai0(i,"ele",r)   a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)   t:ti(i,"ele",r)
         i:PA(i,r)$fe(i)         q:(vafm_input(i,gen,r)*(diffcost(gen,r))*aeei_elen(gen,r))  p:pai0(i,"ele",r)   a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)   t:ti(i,"ele",r)

*         i:PSKL(r)               q:(skl_input(gen,r)*(diffcost(gen,r)))    lab:
*         i:PUSK(r)               q:(usk_input(gen,r)*(diffcost(gen,r)))    lab:

         i:PSKL(r)$(NOT (diss_factor_tax AND HH_DISAG(r)))            q:(skl_input(gen,r)*(diffcost(gen,r)))           lab:           
         i:PSKL(r)$(diss_factor_tax AND HH_DISAG(r))                  q:(skl_input(gen,r)*(diffcost(gen,r)))           p:pf0("skl","ele",r)       lab:          a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:rtf("skl","ele",r)
+        a:GOV(r)$HH_DISAG(r)   n:tax_reb(r)$(sec_no_lab('skl',"ele",r) AND low_lab_tax)      m:(-rtf('skl',"ele",r))$(sec_no_lab('skl',"ele",r) AND low_lab_tax)
         
         i:PUSK(r)$(NOT (diss_factor_tax AND HH_DISAG(r)))            q:(usk_input(gen,r)*(diffcost(gen,r)))           lab:
         i:PUSK(r)$(diss_factor_tax AND HH_DISAG(r))                  q:(usk_input(gen,r)*(diffcost(gen,r)))           p:pf0("usk","ele",r)       lab:          a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:rtf("usk","ele",r)
+        a:GOV(r)$HH_DISAG(r)   n:tax_reb(r)$(sec_no_lab('usk',"ele",r) AND low_lab_tax)      m:(-rtf('usk',"ele",r))$(sec_no_lab('usk',"ele",r) AND low_lab_tax)

         i:RKG$gk                q:(cap_input(gen,r)*(diffcost(gen,r)))
*         i:RKR(r)$rsk            q:(cap_input(gen,r)*(diffcost(gen,r)))
         i:RKR(r)$(rsk AND NOT (diss_factor_tax AND HH_DISAG(r)))            q:(cap_input(gen,r)*(diffcost(gen,r)))
         i:RKR(r)$(rsk AND (diss_factor_tax AND HH_DISAG(r)))                q:(cap_input(gen,r)*(diffcost(gen,r)))     a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:rtf("cap","ele",r)

* ------ Carbon regimes:
         i:PCO2(r)#(fe)$notrad(r)                                q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6
         i:PCO2W#(fe)$pco2w_r(r)$worldtrade                      q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6
         i:PCO2W#(fe)$eu28(r)$eutrade                            q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6
*         i:PCO2Wr(r)#(fe)$eu28(r)$eutrade                            q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6
*         i:PCO2W#(fe)$deu(r)$detrade                             q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6
         i:PCO2W#(fe)$noneu28(r)$worldtrade2                     q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6
         i:PCO2_ETS#(fe)$eu28(r)$etstrade                        q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6     a:ra(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)

*-------13.10.2017 Sector-specific targets for Germany
*    i:PCO2W#(fe)$(DEU_sec and eu28(r) and eutrade)                      q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6
         i:PCO2_DEU(sec)#(fe)$(DEU_sec and deu(r) and sec2cluster(sec,gen))  q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6
*-------13.10.2017 end

* ----- 26.01.2018
         i:PCO2(r)#(fe)$( detrade and deu(r))                                q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6

* ----- 01.04.2020 bta test
         i:PCO2W#(fe)$BTA_coa(r)$bta_test                            q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6

* --> 9) electricity generation from new capital (nicht eeg-geförderte erneuerbare)
* --> completely different to (8) except all carbon inputs i:pcarb...
* --> see difference to (7) in separate txt-file

$prod:ELEn(gen,r)$((out_gen(gen,r))$(ks_n(gen,r))$(not reg(gen))) s:0  lab(s):0

         o:PGEN(gen,r)           q:out_gen(gen,r)

         i:PA(i,r)$(not e(i))    q:vafm_input(i,gen,r)                    p:pai0(i,"ele",r)   a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)   t:ti(i,"ele",r)
         i:PA(e,r)$cru(e)        q:vafm_input(e,gen,r)                    p:pai0(e,"ele",r)   a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)   t:ti(e,"ele",r)
         i:PA(e,r)$ele(e)        q:(vafm_input(e,gen,r)*aeei(e,"ele",r))  p:pai0(e,"ele",r)   a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)   t:ti(e,"ele",r)
         i:PA(e,r)$fe(e)         q:(vafm_input(e,gen,r)*aeei_elen(gen,r)) p:pai0(e,"ele",r)   a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)   t:ti(e,"ele",r)

*         i:PSKL(r)               q:skl_input(gen,r)      lab:
*         i:PUSK(r)               q:usk_input(gen,r)      lab:

         i:PSKL(r)$(NOT (diss_factor_tax AND HH_DISAG(r)))                  q:skl_input(gen,r)           lab:           
         i:PSKL(r)$(diss_factor_tax AND HH_DISAG(r))                        q:skl_input(gen,r)           p:pf0("skl","ele",r)       lab:          a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:rtf("skl","ele",r)
         
         i:PUSK(r)$(NOT (diss_factor_tax AND HH_DISAG(r)))                  q:usk_input(gen,r)           lab:
         i:PUSK(r)$(diss_factor_tax AND HH_DISAG(r))                        q:usk_input(gen,r)           p:pf0("usk","ele",r)       lab:          a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:rtf("usk","ele",r)

         i:RKG$gk                q:cap_input(gen,r)

*         i:RKR(r)$rsk            q:(cap_input(gen,r)*(diffcost(gen,r)))
         i:RKR(r)$(rsk AND NOT (diss_factor_tax AND HH_DISAG(r)))           q:cap_input(gen,r)
         i:RKR(r)$(rsk AND (diss_factor_tax AND HH_DISAG(r)))               q:cap_input(gen,r)     a:RA(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r) t:rtf("cap","ele",r)

* ------ Carbon regimes:
         i:PCO2(r)#(fe)$notrad(r)                                q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6
         i:PCO2W#(fe)$pco2w_r(r)$worldtrade                      q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6
         i:PCO2W#(fe)$eu28(r)$eutrade                            q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6
*         i:PCO2Wr(r)#(fe)$eu28(r)$eutrade                            q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6
*         i:PCO2W#(fe)$deu(r)$detrade                             q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6
         i:PCO2W#(fe)$noneu28(r)$worldtrade2                     q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6
         i:PCO2_ETS#(fe)$eu28(r)$etstrade                        q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6     a:ra(r)$(not HH_DISAG(r)) a:GOV(r)$HH_DISAG(r)  

*-------13.10.2017 Sector-specific targets for Germany
    i:PCO2W#(fe)$(DEU_sec and eu28(r) and eutrade)                      q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6
    i:PCO2_DEU(sec)#(fe)$(DEU_sec and deu(r) and sec2cluster(sec,gen))  q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6
*-------13.10.2017 end

* ----- 26.01.2018
         i:PCO2(r)#(fe)$( detrade and deu(r))                              q:(co2em(fe,gen,r))  p:1e-6

* ----- 01.04.2020 bta test
         i:PCO2W#(fe)$BTA_coa(r)$bta_test                            q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6

* --> 11)  Foreign Trade: ARMINGTON Aggregation over Domestic versus Imports (Armington supply):

$prod:A(i,r)$a0(i,r)   s:4       m:8     s.tl(m):0

         o:PA(i,r)        q:a0(i,r)

         i:PY(i,r)        q:d0(i,r)
         i:PY(i,s)        q:vxmd(i,s,r)      p:pmx0(i,s,r) s.tl: a:RA(s)$(not HH_DISAG(s))    a:GOV(s)$HH_DISAG(s)    t:tx(i,s,r)     
+         a:RA(r)$(not HH_DISAG(r))    a:GOV(r)$HH_DISAG(r)   t:(tm(i,s,r)*(1+tx(i,s,r)))
+         a:RA(r)$(not HH_DISAG(r))    a:GOV(r)$HH_DISAG(r)   n:ect(i,s,r)$bta_sec_reg(i,s,r)      m:(cc(i,s,r) * aeei("oil",i,r))$bta_sec_reg(i,s,r)
+         a:RA(s)$(not HH_DISAG(s))    a:GOV(s)$HH_DISAG(s)   n:ect_reb(i,s,r)$(bta_reb_sec_reg(i,s,r) AND bta_rebate)      m:(-cc(i,s,r) * aeei("oil",i,s))$(bta_reb_sec_reg(i,s,r))  
         i:PT#(s)         q:vtwr1(i,s,r)     p:pmt0(i,s,r) s.tl: a:RA(r)$(not HH_DISAG(r))    a:GOV(r)$HH_DISAG(r)    t:tm(i,s,r)


* --> 12) International transport services (Cobb-Douglas):

$prod:YT  s:1
         o:PT            q:(sum((i,r), vst(i,r)))
         i:PY(i,r)       q:(           vst(i,r))


* --> 13) Investment sector (Final demand):

* ------------------------------------------------------------------------------
*$prod:INV(R) s:10
*$prod:INV(R) s:0.1
* s:0.1 performs best: only 4048 minor iterations in 2050 (time: 2:28 min)
* s:10 : 7884 minor iterations in 2050 (time: 2:38 min)
* ------ 05.05.2015 vorher s:0.1
$prod:INV(R)     s:1
*s:5    pa(s):1

* ------ 10.10.2014
         O:PINV(R)               q:vom("i",r)            a:RA("USA")
         i:PA(j,r)               q:vafm(j,"i",r)         p:pai0(j,"i",r) a:RA(r)$(not HH_DISAG(r))    a:GOV(r)$HH_DISAG(r) t:ti(j,"i",r)

* ------------------------------------------------------------------------------

* ------------------------------------------------------------------------------
*$prod:INV(R)    s:0  vae(s):0.5  va(vae):1  e(vae):0.1  nel(e):0.5  lqd(nel):2  oil(lqd):0  col(nel):0  gas(lqd):0
*         O:PINV(R)               q:vom("i",r)    a:RA("baw")  N:ADAPT(r)$(wobaw(r)$sc3par)
*         i:PA(j,r)$(not e(j))    q:vafm(j,"i",r)         p:pai0(j,"i",r)               a:RA(r) t:ti(j,"i",r)
*         i:PA(fe,r)              q:vafm(fe,"i",r)        p:pai0(fe,"i",r)  fe.tl:      a:RA(r) t:ti(fe,"i",r)
*         i:PA(e,r)$cru(e)        q:vafm(e,"i",r)         p:pai0(e,"i",r)               a:RA(r) t:ti(e,"i",r)
*         I:PA(e,r)$ele(e)        q:vafm(e,"i",r)         p:pai0(e,"i",r)   e:$ele(e)   a:RA(r) t:ti(e,"i",r)
* ------------------------------------------------------------------------------


* ------ AUXILIARY EQUATIONS: Price/Quantity constraints (refering to $AUXILIARY variables)

$constraint:URSK(r)$(ursk0(r)$(not emerge(r) and not HH_DISAG(r)))

*         PSKL(r)/PC(r)   =E= (( (psklbmk(r)/pcbmk(r)) / (ursk0(r)**(-0.1))) * (URSK(r)**(-0.1)) );      // ORIGINAL
         PSKL(r)/PC(r)   =E= (( (psklbmk(r)/pcbmk(r)) / (ursk0(r)**(-0.12))) * (URSK(r)**(-0.12)) );       // vermeidet REDEF bei RUS und ARB, wenn RUS und ARB nicht in emerge

$constraint:URSK(r)$(ursk0(r)$(emerge(r)  and not HH_DISAG(r)))
         PSKL(r)/PC(r)   =E= (( (psklbmk(r)/pcbmk(r)) / (ursk0(r)**(-0.6))) * (URSK(r)**(-0.6)) );

$constraint:URUN(r)$(urun0(r)$(emerge(r)  and not HH_DISAG(r)))
         PUSK(r)/PC(r)   =E= (( (puskbmk(r)/pcbmk(r)) / (urun0(r)**(-0.9))) * (URUN(r)**(-0.9)) );

$constraint:URUN(r)$(ursk0(r)$(not emerge(r) and not HH_DISAG(r)))
         PUSK(r)         =G= PC(r);



* ------ for HH disaggregation
$constraint:URSK(r)$(ursk0(r)$(HH_DISAG(r) and not emerge(r)))
         PSKL(r)/(sum(hh,PC_hh(hh,r))/5)   =E= (( (psklbmk(r)/pcbmk(r)) / (ursk0(r)**(-0.12))) * (URSK(r)**(-0.12)) );

$constraint:URUN(r)$(ursk0(r)$(HH_DISAG(r) and not emerge(r)))
         PUSK(r)         =G= (sum(hh,PC_hh(hh,r))/5);

$constraint:URSK(r)$(ursk0(r)$(emerge(r) and HH_DISAG(r)))
         PSKL(r)/(sum(hh,PC_hh(hh,r))/5)   =E= (( (psklbmk(r)/pcbmk(r)) / (ursk0(r)**(-0.6))) * (URSK(r)**(-0.6)) );

$constraint:URUN(r)$(urun0(r)$(emerge(r)and HH_DISAG(r)))
         PUSK(r)/(sum(hh,PC_hh(hh,r))/5)   =E= (( (puskbmk(r)/pcbmk(r)) / (urun0(r)**(-0.9))) * (URUN(r)**(-0.9)) );


$constraint:R_SUPPLY(i,r)$(rd0(i,r)$PRICETARGET(i,R) AND HH_DISAG(r))
         pytarget(i,r) =e= pr(i,r)/(sum(hh,PC_hh(hh,r))/5) ;
*-------

$constraint:REBATE_DIFF(r)$diffrebate(r)
         REBATE_DIFF(r) * PY("ele",r) * Y("ele",r) * vom("ele",r)
         =E= (-1)*(sum(gen$reg(gen), (1-diffcost(gen,r)) * (PGEN(gen,r) * out_gen(gen,r) * ELEn(gen,r))));

* ------ 17.06.2014
$constraint:R_SUPPLY(i,r)$(rd0(i,r)$PRICETARGET(i,R) AND not HH_DISAG(r))
         pytarget(i,r) =e= pr(i,r)/pc(r) ;

* ------ 12.01.2015
$constraint:YLO(i,r)$ylo_on(i,r)
         Y(i,r) =G= ylo_par(i,r) ;

* ------ 15.08.2018 constraint that controls the free allocation of allowances 
$constraint:GF_SUB(i,r)$(vom(i,r) AND gf_shr(i,r) AND gf_allow)
         PY(i,r) * GF_SUB(i,r) * vom(i,r) * Y(i,r)
         =e= gf_shr(i,r) * PCO2_ETS
         * sum(fe$vafm(fe,i,r), aeei(fe,i,r)*co2em(fe,i,r)*VY_PA(fe,i,r)/vafm(fe,i,r)) ;

* ------ 24.01.2019 constraint for the electricity target from times
*$constraint:ele_tar(gen,r)$ele_dev_switch_actual(gen,r)
*         ELEn(gen,r) =e= 0.99;

$constraint:ect(i,s,r)$(bta_sec_reg(i,s,r) AND eutrade)
          PY(i,s)*ect(i,s,r) =e= PCO2W;

$constraint:ect(i,s,r)$(bta_sec_reg(i,s,r) AND bta_test)
          PY(i,s)*ect(i,s,r) =e= PCO2W;

$constraint:ect(i,s,r)$(bta_sec_reg(i,s,r) AND etstrade)
          PY(i,s)*ect(i,s,r) =e= PCO2_ETS;

$constraint:ect_reb(i,s,r)$(bta_reb_sec_reg(i,s,r) AND bta_test)
          PY(i,r)*ect_reb(i,s,r) =e= PCO2W;

$constraint:CO2_supply$bta
          CO2_supply =e= 1 +
          (sum((i,s,r)$bta_sec_reg(i,s,r),PY(i,s) * vxmd(i,s,r) * A(i,r) * ect(i,s,r) * cc(i,s,r) * aeei("oil",i,s))/
                    sum(r, carblim(r) * PCO2W));

$constraint:tax_reb(r)$(eu28(r) AND no_vat)
          tax_reb(r) *
          (sum((hh,i)$(sec_no_vat(i,r) AND NOT e(i)), tp(i,r) * PA(i,r) * C_hh(hh,r) * (c_hh0(i,r) * hh_sector_share(r,i,hh))) +
           sum((hh,i)$(sec_no_vat(i,r) AND e(i)), tp(i,r) * PA(i,r) * C_hh(hh,r) * (c_hh0(i,r) * aeei(i,"c",r)) * hh_sector_share(r,i,hh)))
          =e= PCO2_NETSr(r) * carblim(r);

$constraint:tax_reb(r)$(eu28(r) AND low_lab_tax)
          tax_reb(r) *
          (sum((i)$(sec_no_lab("skl",i,r) AND NOT e(i)), rtf("skl",i,r) * PSKL(r) * Y(i,r) * skld0(i,r)) +
           sum((i)$(sec_no_lab("usk",i,r) AND NOT e(i)), rtf("usk",i,r) * PUSK(r) * Y(i,r) * uskd0(i,r)) +

           sum((gen,i)$(sec_no_lab("skl",i,r) AND e(i)), rtf("skl","ele",r) * PSKL(r) * ELEx(gen,r) * (skl_input(gen,r))) +
           sum((gen,i)$(sec_no_lab("usk",i,r) AND e(i)), rtf("usk","ele",r) * PUSK(r) * ELEx(gen,r) * (usk_input(gen,r))) +
           sum((gen,i)$(sec_no_lab("skl",i,r) AND e(i)), rtf("skl","ele",r) * PSKL(r) * ELEn(gen,r) * (skl_input(gen,r))) +
           sum((gen,i)$(sec_no_lab("usk",i,r) AND e(i)), rtf("usk","ele",r) * PUSK(r) * ELEn(gen,r) * (usk_input(gen,r))) 
           )
          =e= PCO2_NETSr(r) * carblim(r); 

$constraint:CO2_inv_pay(hh,r)$(netstrade_r AND inverse_co2_pay AND HH_DISAG(r))
          CO2_inv_pay(hh,r) =e=
          (0.00001 + CONVERT_PCO2_HH("hh5",r) - CONVERT_PCO2_HH(hh,r))*5/ sum(hh_, 0.00001 + CONVERT_PCO2_HH("hh5",r) - CONVERT_PCO2_HH(hh_,r));   

$constraint:carb_tax_m(r)$(carbon_tax_de AND deu(r) AND NOT HH_disag(r))
          carbon_tax(r) =e= pcarbon_tax(r)/pc(r);

$constraint:carb_tax_m(r)$(carbon_tax_de AND deu(r) AND HH_disag(r))
          carbon_tax(r) =e= pcarbon_tax(r)/(sum(hh,PC_hh(hh,r))/5);

* ------ REPORTING -------------------------------------------------------------

$REPORT:

* ------ $demand:RA
         V:W(r)$(not HH_DISAG(r))                          w:RA(r)
         V:VD_PC(r)$(not HH_DISAG(r))                      d:PC(r)         demand:RA(r)
         V:VD_PINV(r)$(not HH_DISAG(r))                    d:PINV(r)       demand:RA(r)

* ------ $demand:RA_HH
         v:W_hh(hh,r)$HH_DISAG(r)                    w:RA_HH(hh,r)
         v:VD_PC_hh(hh,r)$HH_DISAG(r)                d:PC_hh(hh,r)         demand:RA_HH(hh,r)
         V:VD_PINV_hh(hh,r)$HH_DISAG(r)              d:PINV(r)             demand:RA_HH(hh,r)
         V:VD_PITAX_hh(hh,r)$HH_DISAG(r)             d:PITAX(r)            demand:RA_HH(hh,r)

* ------ $demand:GOV
         v:W_gov(r)$HH_DISAG(r)                      w:GOV(r)
         v:VD_PC_gov(r)$HH_DISAG(r)                  d:PC_gov(r)         demand:GOV(r)

* ------ $prod:C
         V:VC_PC(r)$(not HH_DISAG(r))                               o:PC(R)         prod:C(r)
         V:VC_PA(i,r)$(not HH_DISAG(r))                             i:PA(i,r)       prod:C(r)
         V:VC_CO2(r)$notrad(r)$(not HH_DISAG(r))                    i:PCO2(r)       prod:C(r)
         V:VC_CO2W(r)$pco2w_r(r)$worldtrade$(not HH_DISAG(r))       i:PCO2W         prod:C(r)
         V:VC_CO2W(r)$eu28(r)$eutrade$(not HH_DISAG(r))             i:PCO2W         prod:C(r)
         V:VC_CO2W(r)$worldtrade2$(not HH_DISAG(r))                 i:PCO2W         prod:C(r)
         V:VC_CO2_NETS(r)$eu28(r)$netstrade$(not HH_DISAG(r))       i:PCO2_NETS     prod:C(r)
         V:VC_CO2_NETSr(r)$eu28(r)$netstrade_r$(not HH_DISAG(r))    i:PCO2_NETSr(r) prod:C(r)
         V:VC_CO2_tax(r)$(deu(r) AND (NOT HH_DISAG(r)) AND carbon_tax_de)      i:Pcarbon_tax(r)     prod:C(r)


* ------ $prod:C_hh
         V:VC_hh_PC(hh,r)$HH_DISAG(r)                                   o:PC_hh(hh,R)   prod:C_hh(hh,r)
         V:VC_hh_PA(i,hh,r)$HH_DISAG(r)                                 i:PA(i,r)       prod:C_hh(hh,r)
         V:VC_HH_p_oil_trans(hh,r)$(HH_DISAG(r) AND h_t_cons_reg(r))    i:p_oil_trans(r)    prod:C_hh(hh,r)
         V:VC_HH_p_ele_trans(hh,r)$(HH_DISAG(r) AND h_t_cons_reg(r))    i:p_ele_trans(r)    prod:C_hh(hh,r)   
         V:VC_hh_CO2(hh,r)$notrad(r)$HH_DISAG(r)                        i:PCO2(r)       prod:C_hh(hh,r)
         V:VC_hh_CO2W(hh,r)$pco2w_r(r)$worldtrade$HH_DISAG(r)           i:PCO2W         prod:C_hh(hh,r)
*         V:VC_hh_CO2W(hh,r)$eu28(r)$eutrade$HH_DISAG(r)            i:PCO2W         prod:C_hh(hh,r)
         V:VC_hh_CO2W(hh,r)$eu28(r)$eutrade$HH_DISAG(r)                 i:PCO2Wr(r)      prod:C_hh(hh,r)
         V:VC_hh_CO2W(hh,r)$worldtrade2$HH_DISAG(r)                     i:PCO2W         prod:C_hh(hh,r)
         V:VC_hh_CO2_NETS(hh,r)$eu28(r)$netstrade$HH_DISAG(r)           i:PCO2_NETS     prod:C_hh(hh,r)
         V:VC_hh_CO2_NETSr(hh,r)$(netstrade_r AND HH_DISAG(r))          i:PCO2_NETSr(r)       prod:C_hh(hh,r)
         V:VC_hh_CO2_inv_pay(hh,r)$(netstrade_r AND HH_DISAG(r) AND   inverse_co2_pay)      i:PCO2_inv_pay(hh,r)  prod:C_hh(hh,r)
         V:VC_hh_CO2_tax(hh,r)$(deu(r) AND HH_DISAG(r) AND carbon_tax_de)      i:Pcarbon_tax(r)     prod:C_hh(hh,r)


* ------ $prod:C_gov
         V:VC_gov_PC(r)$HH_DISAG(r)                              o:PC_gov(R)     prod:C_gov(r)
         V:VC_gov_PA(i,r)$HH_DISAG(r)                            i:PA(i,r)       prod:C_gov(r)
         V:VC_gov_CO2(r)$notrad(r)$HH_DISAG(r)                   i:PCO2(r)       prod:C_gov(r)
         V:VC_gov_CO2W(r)$pco2w_r(r)$worldtrade$HH_DISAG(r)      i:PCO2W         prod:C_gov(r)
*         V:VC_gov_CO2W(r)$eu28(r)$eutrade$HH_DISAG(r)            i:PCO2W         prod:C_gov(r)
         V:VC_gov_CO2W(r)$eu28(r)$eutrade$HH_DISAG(r)            i:PCO2Wr(r)     prod:C_gov(r)
         V:VC_gov_CO2W(r)$worldtrade2$HH_DISAG(r)                i:PCO2W         prod:C_gov(r)
         V:VC_gov_CO2_NETS(r)$eu28(r)$netstrade$HH_DISAG(r)      i:PCO2_NETS     prod:C_gov(r)
         V:VC_gov_CO2_NETSr(r)$eu28(r)$netstrade_r$HH_DISAG(r)   i:PCO2_NETSr(r) prod:C_gov(r)                  

* ------ $prod:A
         V:VA_PA(i,r)                    o:PA(i,r)       prod:A(i,r)
         V:VA_PT(i,s,r)                    i:PT            prod:A(i,r)
* ------ Handelsmatrix [adjust: VA_PYxm.L(i,r,r) = VA_PYxm.L(i,r,r) - VA_PY.L(i,r) = 0 ]
         V:VA_PY(i,r)                    i:PY(i,r)       prod:A(i,r) // Güter aus Binnenproduktion in r (Hauptdiagonale bzw. in-sich-Lieferungen der Handelsmatrix)
         V:VA_PYxm(i,s,r)                i:PY(i,s)       prod:A(i,r) // Bilaterale Handelsmatrix bzw. importierte Güter in r aus s

* ------ $prod:YT
         V:VYT_PT                        o:PT            prod:YT
         V:VYT_PY(i,r)$vst(i,r)          i:PY(i,r)       prod:YT

* ------ $prod:INV
         V:VINV_PINV(r)                  o:PINV(r)       prod:INV(r)
         V:VINV_PA(i,r)                  i:PA(i,r)       prod:INV(r)

* ------ $prod:Y
         V:VY_PY(i,r)                    o:PY(i,r)       prod:Y(i,r)
         V:VY_PSKL(i,r)                  i:PSKL(r)       prod:Y(i,r)
         V:VY_PUSK(i,r)                  i:PUSK(r)       prod:Y(i,r)
         V:VY_RKR(i,r)$RSK               i:RKR(r)        prod:Y(i,r)
         V:VY_RKG(i,r)$GK                i:RKG           prod:Y(i,r)
         V:VY_PA(j,i,r)$vafm(j,i,r)      i:PA(j,r)       prod:Y(i,r)
         V:VY_PR(i,r)$rd0(i,r)           i:PR(i,r)       prod:Y(i,r)

         V:VY_CO2(i,r)$notrad(r)                 i:PCO2(r)       prod:Y(i,r)
         V:VY_CO2W(i,r)$pco2w_r(r)$worldtrade    i:PCO2W         prod:Y(i,r)
*         V:VY_CO2W(i,r)$eu28(r)$eutrade          i:PCO2W         prod:Y(i,r)
         V:VY_CO2W(i,r)$eu28(r)$eutrade          i:PCO2Wr(r)     prod:Y(i,r)
         V:VY_CO2W(i,r)$worldtrade2              i:PCO2W         prod:Y(i,r)
         V:VY_CO2_NETS(i,r)$netstrade            i:PCO2_NETS     prod:Y(i,r)
         V:VY_CO2_NETSr(i,r)$netstrade_r         i:PCO2_NETSr(r) prod:Y(i,r)
         V:VY_CO2_ETS(i,r)$eu28(r)$ets(i)$etstrade i:PCO2_ETS    prod:Y(i,r)
         V:VY_CO2_tax(i,r)$(deu(r) AND carbon_tax_de and TRN(i))      i:Pcarbon_tax(r)     prod:Y(i,r)

* ------ $prod:Y$ele
         V:VY_PGEN(gen,r)                        i:PGEN(gen,r)           prod:Y("ele",r)

* ------ $prod:ELEx
         V:VELEx_PGEN(gen,r)                                     o:PGEN(gen,r)           prod:ELEx(gen,r)
         V:VELEx_PSKL(gen,r)$(out_gen(gen,r)$ks_x(gen,r))                        i:PSKL(r)               prod:ELEx(gen,r)
         V:VELEx_PUSK(gen,r)$(out_gen(gen,r)$ks_x(gen,r))                        i:PUSK(r)               prod:ELEx(gen,r)
         V:VELEx_RKX_ELE(gen,r)$(out_gen(gen,r)$ks_x(gen,r))                     i:RKX_ELE(gen,r)        prod:ELEx(gen,r)
         V:VELEx_PA(i,gen,r)$(out_gen(gen,r)$ks_x(gen,r))                        i:PA(i,r)               prod:ELEx(gen,r)
         V:VELEx_CO2(gen,r)$(out_gen(gen,r)$ks_x(gen,r)$notrad(r))               i:PCO2(r)               prod:ELEx(gen,r)
         V:VELEx_CO2W(gen,r)$(out_gen(gen,r)$ks_x(gen,r)$pco2w_r(r)$worldtrade)  i:PCO2W                 prod:ELEx(gen,r)
*         V:VELEx_CO2W(gen,r)$(out_gen(gen,r)$ks_x(gen,r)$eu28(r)$eutrade)        i:PCO2W                 prod:ELEx(gen,r)
         V:VELEx_CO2W(gen,r)$(out_gen(gen,r)$ks_x(gen,r)$eu28(r)$eutrade)        i:PCO2Wr(r)             prod:ELEx(gen,r)
         V:VELEx_CO2W(gen,r)$(out_gen(gen,r)$ks_x(gen,r)$worldtrade2)            i:PCO2W                 prod:ELEx(gen,r)
*         V:VELEx_CO2_ETS(gen,r)$(out_gen(gen,r)$ks_x(gen,r)$eu28(r)$etstrade)    i:PCO2_ETS              prod:ELEx(gen,r)
         V:VELEx_CO2_ETS(gen,r)$(out_gen(gen,r)$ks_x(gen,r)$eu28(r)$etstrade)    i:PCO2_ETS              prod:ELEx(gen,r)

* ------ $prod:ELEn
         V:VELEn_PGEN(gen,r)                                     o:PGEN(gen,r)           prod:ELEn(gen,r)
         V:VELEn_PSKL(gen,r)$(out_gen(gen,r)$ks_n(gen,r))                        i:PSKL(r)               prod:ELEn(gen,r)
         V:VELEn_PUSK(gen,r)$(out_gen(gen,r)$ks_n(gen,r))                        i:PUSK(r)               prod:ELEn(gen,r)
         V:VELEn_RKR(gen,r)$(out_gen(gen,r)$ks_n(gen,r)$RSK)                     i:RKR(r)                prod:ELEn(gen,r)
         V:VELEn_RKR(gen,r)$(out_gen(gen,r)$ks_n(gen,r)$GK)                      i:RKG                   prod:ELEn(gen,r)
         V:VELEn_PA(i,gen,r)$(out_gen(gen,r)$ks_n(gen,r))                        i:PA(i,r)               prod:ELEn(gen,r)
         V:VELEn_CO2(gen,r)$(out_gen(gen,r)$ks_n(gen,r)$notrad(r))               i:PCO2(r)               prod:ELEn(gen,r)
         V:VELEn_CO2W(gen,r)$(out_gen(gen,r)$ks_n(gen,r)$pco2w_r(r)$worldtrade)  i:PCO2W                 prod:ELEn(gen,r)
*         V:VELEn_CO2W(gen,r)$(out_gen(gen,r)$ks_n(gen,r)$eu28(r)$eutrade)        i:PCO2W                 prod:ELEn(gen,r)
         V:VELEn_CO2W(gen,r)$(out_gen(gen,r)$ks_n(gen,r)$eu28(r)$eutrade)        i:PCO2Wr(r)             prod:ELEn(gen,r)
         V:VELEn_CO2W(gen,r)$(out_gen(gen,r)$ks_n(gen,r)$worldtrade2)            i:PCO2W                 prod:ELEn(gen,r)
*         V:VELEn_CO2_ETS(gen,r)$(out_gen(gen,r)$ks_n(gen,r)$eu28(r)$etstrade)    i:PCO2_ETS              prod:ELEn(gen,r)
         V:VELEn_CO2_ETS(gen,r)$(out_gen(gen,r)$ks_n(gen,r)$eu28(r)$etstrade)    i:PCO2_ETS              prod:ELEn(gen,r)

* ------ $prod:TAXTRANS
         V:VTAXTRANS_PGov(r)$HH_DISAG(r)                                         o:PGov(r)               prod:TAXTRANS(r)

* ------ $prod:TAXTRANS
         V:VLKTAX_PITAX(r)$HH_DISAG(r)                                           o:PITAX(r)               prod:LKTAX(r)

* ------ $prod:FF_trans
         V:VFF_p_oil_trans(r)$(HH_DISAG(r) AND h_t_cons_reg(r))                                   o:p_oil_trans(r)        prod:FF_trans(r)
         V:VFF_mvh(r)$(HH_DISAG(r) AND h_t_cons_reg(r))                                           i:PA("mvh",r)           prod:FF_trans(r)
         V:VFF_oil(r)$(HH_DISAG(r) AND h_t_cons_reg(r))                                           i:PA("oil",r)           prod:FF_trans(r)
         V:VFF_CO2(r)$notrad(r)$(HH_DISAG(r) AND h_t_cons_reg(r))                                 i:PCO2(r)               prod:FF_trans(r)
         V:VFF_CO2W(r)$pco2w_r(r)$worldtrade$(HH_DISAG(r) AND h_t_cons_reg(r))                    i:PCO2W                 prod:FF_trans(r)
*         V:VFF_CO2W(r)$eu28(r)$eutrade$(HH_DISAG(r) AND h_t_cons_reg(r))                          i:PCO2W                 prod:FF_trans(r)
         V:VFF_CO2W(r)$eu28(r)$eutrade$(HH_DISAG(r) AND h_t_cons_reg(r))                          i:PCO2Wr(r)             prod:FF_trans(r)
         V:VFF_CO2W(r)$worldtrade2$(HH_DISAG(r) AND h_t_cons_reg(r))                              i:PCO2W                 prod:FF_trans(r)
         V:VFF_CO2_NETS(r)$eu28(r)$netstrade$(HH_DISAG(r) AND h_t_cons_reg(r))                    i:PCO2_NETS             prod:FF_trans(r)
         V:VFF_CO2_NETSr(r)$eu28(r)$netstrade_r$(HH_DISAG(r) AND h_t_cons_reg(r))                 i:PCO2_NETSr(r)         prod:FF_trans(r)

* ------ $prod:ELE_trans
         V:VEL_p_ele_trans(r)$(HH_DISAG(r) AND h_t_cons_reg(r))                                   o:p_ele_trans(r)        prod:ELE_trans(r)
         V:VEL_mvh(r)$(HH_DISAG(r) AND h_t_cons_reg(r))                                           i:PA("mvh",r)           prod:ELE_trans(r)
         V:VEL_ele(r)$(HH_DISAG(r) AND h_t_cons_reg(r))                                           i:PA("ele",r)           prod:ELE_trans(r)

$offtext                                                                        // End of MPSGE-Model
$sysinclude mpsgeset NEWAGE

* ---------------------------------------------------------------------- *
* End of M P S G E - Model
* ---------------------------------------------------------------------- *
* ====================================================================== *

* -----> Run a BENCHMARK REPLICATION:
* ------ Labor market initial values
psklbmk(r)       = PSKL.L(r);
puskbmk(r)       = PUSK.L(r);
pcbmk(r)         = PC.L(r);

URSK.L(r)        = ursk0(r);
URUN.L(r)        = urun0(r);
* ------ Following limit on skilled unemployment is necessary to avoid tht 0^-0.1, which is not defined
URSK.LO(r)       = 1e-6 ;
URUN.LO(r)       = 1e-6;

* ------ Extant capital --> Die Startwerte der Variablen ELEx und ELEn werden gemäß thetax=0.985 und (1-thetax)=0.015 vorgegeben
ELEx.L(gen,r)= thetax$cap_input(gen,r);
ELEn.L(gen,r)= 1-thetax$cap_input(gen,r);

ELE_trans.L(r) = 0;
* ------ 26.09.2012: Fix variables which do not enter the model to avoid unmatched variables if:
* ---->> see file GAMS-L archives - Unmatched variables (Jan 2005, Rut).pdf URL: http://www.listserv.dfn.de/cgi-bin/wa?A2=ind0501&L=GAMS-L&P=R2639&I=-3
ELEx.FX(GEN,R)$(Not OUT_GEN(GEN,R)) = 0;
ELEn.FX(GEN,R)$(Not OUT_GEN(GEN,R)) = 0;
ELEx.FX(GEN,R)$(Not ks_x(GEN,R)) = 0;
ELEn.FX(GEN,R)$(Not ks_n(GEN,R)) = 0;

* ------ Diffrebate constraint
REBATE_DIFF.LO(r)$diffrebate(r)  = -inf;

R_SUPPLY.L(i,r)$(rd0(i,r) and pricetarget(i,r)) = 1;

* ------ 05.05.2015
YLO.L(i,r)$ylo_on(i,r) = 1;
YLO.LO(i,r)$ylo_on(i,r) = -inf;

* ------ Set initial values of CO2 prices = 0
PCO2.L(r)        = 1e-6 ;
PCO2W.L          = 1e-6 ;
PCO2_ETS.L       = 1e-6 ;
PCO2_NETS.L      = 1e-6 ;
PCO2_NETSr.L(r)  = 1e-6 ;
PCO2_DEU.L(sec)	 = 1e-6 ;

* ------ CARBON REGIMES --------------------------------------------------------
* ------ Define CO2 endowment
carblim(r)       = + sum(fe,    co2em(fe,"final",r))                  // carbon emissions from final consumption
                   + sum(i,     sum(fe, co2em(fe,i,r)$nr(i,r))        // Carbon emissions from industry
                   + sum(fe,    co2em(fe,i,r)$ele(i))                 // carbon emissions from electricity generation
                   + sum(fe,    co2em(fe,i,r)$(vom(i,r)$oil(i))))     // carbon emisisons from oil refinery
***                   + sum(fe, co2em(fe,i,r)$(vom(i,r)$xe(i)))       // Carbon emissions do not arise in fossil fuel production (GTAP-EG)
;
*------- 13.09.17
*carblim_sec(sec,"DEU")  = sum((fe), co2em_sec(fe,sec,"DEU"));

carblim_sec(sec,r)$eu28(r) = sum(i$sec2cluster(sec,i),     sum(fe, co2em(fe,i,r)$nr(i,r)));

carblim_sec("buildings",r)$eu28(r) = carblim_sec("buildings",r) +  sum(fe,    co2em(fe,"final",r))  ;                    // carbon emissions from final consumption

carblim_sec("residential",r)$eu28(r) =  sum(fe,    co2em(fe,"final",r))  ;

carblim_sec("energy",r)$eu28(r)  = + sum(i,
                               + sum(fe,    co2em(fe,i,r)$ele(i))                                   // carbon emissions from electricity generation
                               + sum(fe,    co2em(fe,i,r)$(vom(i,r)$oil(i))));                      // carbon emisisons from oil refinery

carblim_sec("transport",r) = sum(fe, co2em(fe,"trn",r));

display co2em, carblim, carblim_sec;

* ------ 2.3.2015
carblim0(r)      = carblim(r);
carblim0_world   = sum(r, carblim(r));

* ----- 26.01.2018 carblim_deu
carblim_deu = carblim("deu");
carblim_deu0 = carblim_deu;

*------18.09.2017
carblim_sec0(sec,r)	= carblim_sec(sec,r);

* ------ Carblim_ets
carblim_ets(r)$eu28(r)   =
         + sum((fe,i)$(ets(i) and nr(i,r)),      co2em(fe,i,r))
         + sum((fe,i)$ele(i),                    co2em(fe,i,r))
         + sum((fe,i)$oil(i),                    co2em(fe,i,r));

* ------ 7.01.2016 --> Carblim_ets
carblim_ets0(r)                   = carblim_ets(r);

* ------ 5.05.2015 --> Carblim
*carblim(r)$(eu28(r) and etstrade or netstrade) = round(carblim(r) - carblim_ets(r),10);
* ------ 11.02.2016

* ------ 24.10.2017 carblim test
carblim(r)$(eu28(r) and (etstrade or netstrade or netstrade_r))          = round(carblim(r) - carblim_ets(r),10);

*carblimnets(r)$(eu28(r) and (etstrade or netstrade or netstrade_r))      = carblim(r) ;
display carblim, carblim0, carblim0_world, carblim_ets, carblim_ets0, carblim_sec, carblim_sec0 ;

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
PC.fx(num)=1;
* ------ Size of work array in MB
NEWAGE.workspace = 128 ;
* ------ Set iteration limit to zero
NEWAGE.iterlim = 0;
* ------ INCLUDE and SOLVE model "GTAP"
$include NEWAGE.gen
SOLVE    NEWAGE  using   MCP ;                                                   // # SOLVE Statement 1 #


display  NEWAGE.objval;

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ Becnhmark REPORTING --------------------------------------------------- // # Benchmark reporting #

* ------ 17.04.2014 Look at .L- and .M-values of variables
parameter id, idr, idco2, idgen, idyt, idgov;
id(i,r,"Y.L")$(Y.L(i,r) <> 1) = Y.L(i,r);
id(i,r,"Y.M")$(Y.M(i,r) >  1.E-05) = Y.M(i,r);
id(i,r,"Y.M")$(Y.M(i,r) < -1.E-05) = Y.M(i,r);
id(i,r,"A.L")$(A.L(i,r) <> 1) = A.L(i,r);
id(i,r,"A.M")$(A.M(i,r) >  1.E-05) = A.M(i,r);
id(i,r,"A.M")$(A.M(i,r) < -1.E-05) = A.M(i,r);
id(i,r,"PY.L")$(PY.L(i,r) <> 1) = PY.L(i,r);
id(i,r,"PY.M")$(PY.M(i,r) >  1.E-05) = PY.M(i,r);
id(i,r,"PY.M")$(PY.M(i,r) < -1.E-05) = PY.M(i,r);
id(i,r,"PA.L")$(PA.L(i,r) <> 1) = PA.L(i,r);
id(i,r,"PA.M")$(PA.M(i,r) >  1.E-05) = PA.M(i,r);
id(i,r,"PA.M")$(PA.M(i,r) < -1.E-05) = PA.M(i,r);
id(i,r,"PR.L")$(PR.L(i,r) <> 1) = PR.L(i,r);
id(i,r,"PR.M")$(PR.M(i,r) >  1.E-05) = PR.M(i,r);
id(i,r,"PR.M")$(PR.M(i,r) < -1.E-05) = PR.M(i,r);

idgen(gen,r,"ELEn.L")$(ELEn.L(gen,r)<>(1-thetax))= ELEn.L(gen,r);
idgen(gen,r,"ELEn.M")$(ELEn.M(gen,r) >  1.E-05)  = ELEn.M(gen,r);
idgen(gen,r,"ELEn.M")$(ELEn.M(gen,r) < -1.E-05)  = ELEn.M(gen,r);
idgen(gen,r,"ELEx.L")$(ELEx.L(gen,r) <> thetax)  = ELEx.L(gen,r);
idgen(gen,r,"ELEx.M")$(ELEx.M(gen,r) >  1.E-05)  = ELEx.M(gen,r);
idgen(gen,r,"ELEx.M")$(ELEx.M(gen,r) < -1.E-05)  = ELEx.M(gen,r);
idgen(gen,r,"PGEN.L")$(PGEN.L(gen,r) <> 1)               = PGEN.L(gen,r);
idgen(gen,r,"PGEN.M")$(PGEN.M(gen,r) >  1.E-05)          = PGEN.M(gen,r);
idgen(gen,r,"PGEN.M")$(PGEN.M(gen,r) < -1.E-05)          = PGEN.M(gen,r);
idgen(gen,r,"RKX_ELE.L")$(RKX_ELE.L(gen,r) <> 1)         = RKX_ELE.L(gen,r);
idgen(gen,r,"RKX_ELE.M")$(RKX_ELE.M(gen,r) >  1.E-05)    = RKX_ELE.M(gen,r);
idgen(gen,r,"RKX_ELE.M")$(RKX_ELE.M(gen,r) < -1.E-05)    = RKX_ELE.M(gen,r);

idr(r,"C.L")$(C.L(r) <> 1) = C.L(r);
idr(r,"C.M")$(C.M(r) >  1.E-05) = C.M(r);
idr(r,"C.M")$(C.M(r) < -1.E-05) = C.M(r);
idr(r,"PC.L")$(PC.L(r) <> 1) = PC.L(r);
idr(r,"PC.M")$(PC.M(r) >  1.E-05) = PC.M(r);
idr(r,"PC.M")$(PC.M(r) < -1.E-05) = PC.M(r);
idr(r,"Pinv.L")$(Pinv.L(r) <> 1) = Pinv.L(r);
idr(r,"Pinv.M")$(Pinv.M(r) >  1.E-05) = Pinv.M(r);
idr(r,"Pinv.M")$(Pinv.M(r) < -1.E-05) = Pinv.M(r);
idr(r,"INV.L")$(INV.L(r) <> 1) = INV.L(r);
idr(r,"INV.M")$(INV.M(r) >  1.E-05) = INV.M(r);
idr(r,"INV.M")$(INV.M(r) < -1.E-05) = INV.M(r);
idr(r,"RKR.L")$(RKR.L(r) <> 1) = RKR.L(r);
idr(r,"RKR.M")$(RKR.M(r) >  1.E-05) = RKR.M(r);
idr(r,"RKR.M")$(RKR.M(r) < -1.E-05) = RKR.M(r);
idr(r,"PSKL.L")$(PSKL.L(r) <> 1) = PSKL.L(r);
idr(r,"PSKL.M")$(PSKL.M(r) >  1.E-05) = PSKL.M(r);
idr(r,"PSKL.M")$(PSKL.M(r) < -1.E-05) = PSKL.M(r);
idr(r,"PUSK.L")$(PUSK.L(r) <> 1) = PUSK.L(r);
idr(r,"PUSK.M")$(PUSK.M(r) >  1.E-05) = PUSK.M(r);
idr(r,"PUSK.M")$(PUSK.M(r) < -1.E-05) = PUSK.M(r);

idr(r,"PITAX.L")$(HH_DISAG(r) AND (PITAX.L(r) <> 1)) = PITAX.L(r);
idr(r,"PITAX.M")$(HH_DISAG(r) AND (PITAX.M(r) >  1.E-05)) = PITAX.M(r);
idr(r,"PITAX.M")$(HH_DISAG(r) AND (PITAX.M(r) < -1.E-05)) = PITAX.M(r);

idr(r,"PTAX.L")$(HH_DISAG(r) AND (PTAX.L(r) <> 1)) = PTAX.L(r);
idr(r,"PTAX.M")$(HH_DISAG(r) AND (PTAX.M(r) >  1.E-05)) = PTAX.M(r);
idr(r,"PTAX.M")$(HH_DISAG(r) AND (PTAX.M(r) < -1.E-05)) = PTAX.M(r);

idr(r,"PC_hh.L")$(HH_DISAG(r) AND (sum(hh, PC_hh.L(hh,r)) <> 5)) = (sum(hh, PC_hh.L(hh,r))/5);
idr(r,"PC_hh.M")$(HH_DISAG(r) AND (sum(hh, PC_hh.M(hh,r)) >  1.E-05)) = sum(hh, PC_hh.M(hh,r));
idr(r,"PC_hh.M")$(HH_DISAG(r) AND (sum(hh, PC_hh.M(hh,r)) < -1.E-05)) = sum(hh, PC_hh.M(hh,r));

idr(r,"C_hh.L")$(HH_DISAG(r) AND (sum(hh, C_hh.L(hh,r)) <> 5)) = (sum(hh, C_hh.L(hh,r))/5);
idr(r,"C_hh.M")$(HH_DISAG(r) AND (sum(hh, C_hh.M(hh,r)) >  1.E-05)) = sum(hh, C_hh.M(hh,r));
idr(r,"C_hh.M")$(HH_DISAG(r) AND (sum(hh, C_hh.M(hh,r)) < -1.E-05)) = sum(hh, C_hh.M(hh,r));

idgov(r,"PGOV.L")$(HH_DISAG(r) AND (PGOV.L(r) <> 1))      =     PGOV.L(r);
idgov(r,"PGOV.M")$(HH_DISAG(r) AND (PGOV.M(r) >  1.E-05)) =     PGOV.M(r);
idgov(r,"PGOV.M")$(HH_DISAG(r) AND (PGOV.M(r) <  -1.E-05)) =     PGOV.M(r);

idgov(r,"PC_GOV.L")$(HH_DISAG(r) AND (PC_GOV.L(r) <> 1))      =     PC_GOV.L(r);
idgov(r,"PC_GOV.M")$(HH_DISAG(r) AND (PC_GOV.M(r) >  1.E-05)) =     PC_GOV.M(r);
idgov(r,"PC_GOV.M")$(HH_DISAG(r) AND (PC_GOV.M(r) <  -1.E-05)) =     PC_GOV.M(r);

idgov(r,"PLKTAX.L")$(HH_DISAG(r) AND (PLKTAX.L(r) <> 1))      =     PLKTAX.L(r);
idgov(r,"PLKTAX.M")$(HH_DISAG(r) AND (PLKTAX.M(r) >  1.E-05)) =     PLKTAX.M(r);
idgov(r,"PLKTAX.M")$(HH_DISAG(r) AND (PLKTAX.M(r) <  -1.E-05)) =     PLKTAX.M(r);


* ------ Additional information for idr
idr(r,"PA.L") = sum(i, id(i,r,"PA.L"));
idr(r,"PA.M") = sum(i, id(i,r,"PA.M"));
idr(r,".M sum") = idr(r,"C.M") +  idr(r,"PC.M") + idr(r,"Pinv.M") + idr(r,"INV.M") + idr(r,"RKR.M") + idr(r,"PSKL.M") +  idr(r,"PUSK.M") + (idr(r,"PITAX.M") + idr(r,"PTAX.M") + idr(r,"PC_hh.M"))$HH_DISAG(r);
idr(r,"PCO2.L")$(PCO2.L(r) <> 1e-6) = PCO2.L(r);
idr(r,"PCO2.M")$(PCO2.M(r) >  1.E-05) = PCO2.M(r);
idr(r,"PCO2.M")$(PCO2.M(r) < -1.E-05) = PCO2.M(r);

idco2("PCO2_NETS.L")$(PCO2_NETS.L <> 1e-6) = PCO2_NETS.L;
idco2("PCO2_NETS.M")$(PCO2_NETS.M >  1.E-05) = PCO2_NETS.M;
idco2("PCO2_NETS.M")$(PCO2_NETS.M < -1.E-05) = PCO2_NETS.M;
idco2("PCO2W.L")$(PCO2W.L <> 1e-6) = PCO2W.L;
idco2("PCO2W.M")$(PCO2W.M >  1.E-05) = PCO2W.M;
idco2("PCO2W.M")$(PCO2W.M < -1.E-05) = PCO2W.M;
idco2("PCO2_ETS.L")$(PCO2_ETS.L <> 1e-6) = PCO2_ETS.L;
idco2("PCO2_ETS.M")$(PCO2_ETS.M >  1.E-05) = PCO2_ETS.M;
idco2("PCO2_ETS.M")$(PCO2_ETS.M < -1.E-05) = PCO2_ETS.M;

idyt("YT.L")$(YT.L <> 1) = YT.L;
idyt("YT.M")$(YT.M >  1.E-05) = YT.M;
idyt("YT.M")$(YT.M < -1.E-05) = YT.M;
idyt("PT.L")$(PT.L <> 1) = PT.L;
idyt("PT.M")$(PT.M >  1.E-05) = PT.M;
idyt("PT.M")$(PT.M < -1.E-05) = PT.M;

display id, idr, idco2, idgen, idyt;


*$exit
* ------ 28.04.2014
rebate_par("rebate_diff",r) = (sum(gen$reg(gen), (-1)*(1-diffcost(gen,r)) * (PGEN.L(gen,r) * out_gen(gen,r) * ELEn.L(gen,r)))) / (PY.L("ele",r) * Y.L("ele",r) * vom("ele",r));
rebate_par("zähler",r) = (sum(gen$reg(gen), (-1)*(1-diffcost(gen,r)) * (PGEN.L(gen,r) * out_gen(gen,r) * ELEn.L(gen,r))));
rebate_par("nenner",r) = (PY.L("ele",r) * Y.L("ele",r) * vom("ele",r));


* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ Declaration of STATIC parameters before the loop! --------------------- // # STATIC report parameters #

PARAMETERS
* ------ Capital Stock to flow conversion for capital augmentation
         ke(r)           Capital earnings
         kst(r)          Capital stock
         brk0(r)         Benchmark gross rate of return on capital
         evoa0(*,r)      Benchmark factor endowment

* ------ Macroeconomic Indicators ----------------------------------------------
         welf(r)
         welf_hh(hh,r)
         gdpreal(r)
         emplmt(*,r)
         ex(i,r)
         im(i,r)
         trdblnc(*,r)
         ur(*,r)

* ------ 11.12.2013 Report parameter for solvestat-modelstat-objval-numredef
         status(*,*)     Report parameter for solvestat-modelstat-objval-numredef (1-1-0-0)

* ------ Cars and buildings
         cars2
         houseqm2
         eff_reso
         eff_resi
         effh_reso
         effh_resi

* ------ CO2-Emissionen (Wert-Koeffizienten)
*         co2coefy
*         co2coefc
*         co2coefele
         co2coefhh
         co2coefhhsum
* ------ CO2-Emissionen (wertbasierte Berechnung mit Wert-Koeffizienten)
         co2real
         co2realc
         co2realy
         co2realele
         co2realhhc
         co2realhhb

* ------ CO2-Emissionen (Checks)
         co2realc_chk
         co2realy_chk
         co2realele_chk
         co2em_totalchk
         co2chk

* ------ 24.08.2018 - Energy/euro rate
         energy_euro_rate(*,*,r)    displays the conversion rate from mtoe to euro based on the parameter evd [e*g*r] [PJ\Euro]
;
* ------ Capital Stock to flow conversion for capital augmentation
ke(r)$rsk    =  sum(i, VY_RKR.l(i,r) * RKR.L(r)) ;
ke(r)$gk     =  sum(i, VY_RKR.l(i,r) * RKG.L   ) ;
kst(r)       =  vdep(r)  / dep(r);
brk0(r)      =  ke(r)    / kst(r);
* ------ 05.06.2014 save current status of evoa in bmk-parameter evoa0
evoa0("skl",r) = evoa("skl",r);
evoa0("usk",r) = evoa("usk",r);
evoa0("cap",r) = evoa("cap",r);
evoa0("res",r) = evoa("res",r);
*display ke, kst, brk0, evoa, evoa0;

* ------ Macroeconomic Indicators ----------------------------------------------
welf(r) = W.L(r);
welf_hh(hh,r)$HH_DISAG(r) = W_hh.L(hh,r);
VA_PYxm.L(i,r,r) = 0;
ex(i,r) = sum(s, VA_PYxm.L(i,r,s));
im(i,r) = sum(s, VA_PYxm.L(i,s,r));
trdblnc(i,r) = ex(i,r) - im(i,r);
trdblnc("total",r) = sum(i, ex(i,r) - im(i,r));

gdpreal(r)$(NOT HH_DISAG(r)) =  (VC_PC.L(r) * PC.L(r)) + (VINV_PINV.L(r) * PINV.L(r)) + trdblnc("total",r);
gdpreal(r)$(HH_DISAG(r)) =  sum(hh,VC_hh_PC.L(hh,r)) + VC_gov_PC.L(r) + VINV_PINV.L(r) + trdblnc("total",r);

emplmt("skl",r) = sum(i, VY_PSKL.L(i,r)) + sum(gen, VELEx_PSKL.L(gen,r) + VELEn_PSKL.L(gen,r));
emplmt("usk",r) = sum(i, VY_PUSK.L(i,r)) + sum(gen, VELEx_PUSK.L(gen,r) + VELEn_PUSK.L(gen,r));
emplmt("total",r) = emplmt("skl",r) + emplmt("usk",r) ;
ur("skl",r) = URSK.L(r) ;
ur("usk",r) = URUN.L(r) ;
ur("total",r) = (emplmt("skl",r) * URSK.L(r) + emplmt("usk",r) * URUN.L(r)) / emplmt("total",r);
*display  welf, ex, im, trdblnc, gdpreal, emplmt, ur;


* ----- Energy Euro rate
energy_euro_rate(e,"c+g",r) = (evd(e,"c",r) + evd(e,"g",r) ) / (vafm(e,"c",r) + vafm(e,"g",r)) * 1000;
energy_euro_rate(e,"c",r)$(vafm(e,"c",r)) = (evd(e,"c",r)) / (vafm(e,"c",r)) * 1000;
energy_euro_rate(e,"g",r)$(vafm(e,"g",r)) = (evd(e,"g",r)) / (vafm(e,"g",r)) * 1000;
energy_euro_rate(e,i,r)$vafm(e,i,r) = (evd(e,i,r) ) / vafm(e,i,r) * 1000;


* ------ Execute Unload GDX-file ----------------------------------------------- // # Execute Unload bmk.GDX #

Execute_Unload       "bmk.gdx";

display co2em, co2em_total, carblim, carblim_ets, evd;

* XXXXXXXXXXXXXXXXXXXXXXXXXX Static REPORTING XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

* ------ 11.12.2013 Report parameter for solvestat-modelstat-objval-numredef (http://www.gams.de/mccarl/mccarlhtml/model_attributes_mainly_used_a.htm)
status("solvestat","Bmk")= NEWAGE.solvestat;
status("modelstat","Bmk")= NEWAGE.modelstat;
status("objval","Bmk")   = round(NEWAGE.objval, 6);
status("numredef","Bmk") = NEWAGE.numredef;
* ------ 11.05.2013
status("iterUsd","Bmk")  = NEWAGE.iterUsd;
display status;
*$stop

* ------ Macroeconomic Indicators ----------------------------------------------
welf(r) = W.L(r);
VA_PYxm.L(i,r,r) = 0;
ex(i,r) = sum(s, VA_PYxm.L(i,r,s));
im(i,r) = sum(s, VA_PYxm.L(i,s,r));
trdblnc(i,r) = ex(i,r) - im(i,r);
trdblnc("total",r) = sum(i, ex(i,r) - im(i,r));
gdpreal(r)$(NOT HH_DISAG(r)) =  VC_PC.L(r) + VINV_PINV.L(r) + trdblnc("total",r);
gdpreal(r)$(HH_DISAG(r)) =  sum(hh,VC_hh_PC.L(HH,R)) + VC_gov_PC.L(r) + VINV_PINV.L(r) + trdblnc("total",r);
emplmt("skl",r) = sum(i, VY_PSKL.L(i,r)) + sum(gen, VELEx_PSKL.L(gen,r) + VELEn_PSKL.L(gen,r));
emplmt("usk",r) = sum(i, VY_PUSK.L(i,r)) + sum(gen, VELEx_PUSK.L(gen,r) + VELEn_PUSK.L(gen,r));
emplmt("total",r) = emplmt("skl",r) + emplmt("usk",r) ;

ur("skl",r) = URSK.L(r) ;
ur("usk",r) = URUN.L(r) ;
ur("total",r) = (emplmt("skl",r) * URSK.L(r) + emplmt("usk",r) * URUN.L(r)) / emplmt("total",r);
*display  welf, ex, im, trdblnc, gdpreal, emplmt, ur;

* ------ 5.05.2015 Korrektur der CO2-Emissionen, um sie mit carblim0 vergleichbar zu machen (s.o.) --> Keine Emissionen in CRU, GAS, COL (GTAP-EG)
co2em(fe,i,r)$xe(i) = 0;
co2em_total(r) = sum((i,j), co2em(i,j,r)) + sum(fe, co2em(fe,"final",r));
co2em_total("world") = sum(r, co2em_total(r)); display co2em_total;

* ------ CO2-Berechnung --------------------------------------------------------
* ------ 25.02.2015 CO2-Koeffizienten-Berechnung
co2coefy(fe,i,r)$co2em(fe,i,r)           = vafm(fe,i,r)  / co2em(fe,i,r);
co2coefele(fe,gen,r)$co2em(fe,gen,r)     = vafm_input(fe,gen,r) / co2em(fe,gen,r);
co2realy(fe,i,r)$co2coefy(fe,i,r)        = VY_PA.L(fe,i,r) / co2coefy(fe,i,r) ;
co2realele(fe,gen,r)$co2coefele(fe,gen,r)= (VELEX_PA.L(fe,gen,r) + VELEN_PA.L(fe,gen,r)) / co2coefele(fe,gen,r) ;

* ------ 24.01.2016 (Diss)
co2coefc(fe,r)$co2em(fe,"final",r)                       = c0(fe,r)              / co2em(fe,"final",r);
co2realc(fe,r)$(co2coefc(fe,r) AND NOT HH_DISAG(r))      = VC_PA.L(fe,r)         / co2coefc(fe,r);
co2realc(fe,r)$(co2coefc(fe,r) AND HH_DISAG(r))          = (sum(hh,VC_hh_PA.L(fe,hh,r)) + VC_gov_PA.L(fe,r))  / co2coefc(fe,r);

* ----- 21.02.2019 HH disaggregation
co2coefc_hh(fe,r)$co2em(fe,"c",r)                       = c_hh0(fe,r)              / co2em(fe,"c",r);
co2coefc_gov(fe,r)$co2em(fe,"g",r)                      = c_gov0(fe,r)              / co2em(fe,"g",r);

* ------ Wertbasierte CO2-Berechnung -------------------------------------------
co2real(fe,r)            = co2realc(fe,r) + sum(i, co2realy(fe,i,r)) + sum(gen, co2realele(fe,gen,r));
co2real("total",r)       = sum(fe, co2real(fe,r)) ;
co2real(fe,"world")      = sum(r, co2real(fe,r)) ;
co2real("total","world") = sum(fe, co2real(fe,"world")) ;
*co2real(fe,r)           = co2realc(fe,r) + sum((cf,ct), co2realhhc(fe,ct,cf,r)) + sum((bf,bt), co2realhhb(fe,bt,bf,r)) + sum(i, co2realy(fe,i,r)) + sum(gen, co2realele(fe,gen,r)); // c und b getrennt

* ------ CO2 Checks ------------------------------------------------------------
co2realc_chk(fe,r)               = round(co2realc(fe,r) - (co2em(fe,"final",r)), 7);

co2realy_chk(fe,i,r)$(not ele(i))= round(co2realy(fe,i,r) - co2em(fe,i,r), 7) ;
co2realele_chk(fe,gen,r)         = round(co2realele(fe,gen,r) - co2em(fe,gen,r), 7);

co2em_totalchk(r)                = round(co2real("total",r) - sum(fe, (co2em(fe,"final",r)) +  sum(i$(not ele(i)), co2em(fe,i,r)) + sum(gen, co2em(fe,gen,r))),7);

*co2realc_chk(fe,r) = round(co2realc(fe,r)   - co2em(fe,"final",r), 7);    // vor EnHH-Disaggregierung
*co2realc_chk(fe,r) = round(co2realc(fe,r) + sum((cf,ct), co2realhhc(fe,ct,cf,r)) + sum((bf,bt), co2realhhb(fe,bt,bf,r)) - (co2em(fe,"final",r) + sum((cbt,cbf), cbco2i(r,fe,cbt,cbf))), 7); // c und b getrennt
*co2em_totalchk(r) = round(co2real("total",r) - sum(fe, co2em(fe,"final",r) +  sum(i$(not ele(i)), co2em(fe,i,r)) + sum(gen, co2em(fe,gen,r))),7); // vor EnHH-Disaggregierung

* ------ 14.09.2015 (Diss)-Vortrag
co2chk(r) = co2em_totalchk(r) / sum(fe, (co2em(fe,"final",r)) +  sum(i$(not ele(i)), co2em(fe,i,r)) + sum(gen, co2em(fe,gen,r))) * 100;
co2chk("world") = sum(r, co2em_totalchk(r)) / sum(r, sum(fe, (co2em(fe,"final",r)) +  sum(i$(not ele(i)), co2em(fe,i,r)) + sum(gen, co2em(fe,gen,r)))) * 100;


display co2coefy, co2coefele, co2coefc, co2realc, co2realy, co2realele, co2real, co2realc_chk, co2realy_chk, co2realele_chk, co2em_totalchk;
display co2em_total, co2em, vafm, vafm_input, c0, carblim0, carblim0_world, co2chk;

* ------ Zero Profit for Armington aggregate -----------------------------------
* ------ 02.07.2014 Remember:
*pmx0(i,s,r)      = pvxmd(i,s,r);                 // = (1+rtms0(i,s,r)) * (1-rtxs0(i,s,r))
*pmt0(i,s,r)      = pvtwr(i,s,r);                 // = (1+rtms(i,s,r))

*zpf_a(i,r) = a0(i,r) - d0(i,r) - sum(s, vxmd(i,s,r)) - sum(s, vtwr1(i,s,r)); // geht nicht
*zpf_a(i,r) = round(sum(s, vxmd(i,s,r) * [(1+rtms(i,s,r))*(1-rtxs(i,s,r))]) - sum(s, VA_PYxm.L(i,s,r)),7);   // geht nicht
*zpf_a(i,r) = round(sum(s, vtwr("trn",i,s,r)*  (1+rtms(i,s,r))) - VA_PT.L(i,r), 7);       // geht nicht
zpf_a(i,r) = round(
         + a0(i,r)
         - d0(i,r)
         - sum(s, vxmd(i,s,r) * [(1+rtms(i,s,r))*(1-rtxs(i,s,r))])
         - sum(s, vtwr("trn",i,s,r)*  (1+rtms(i,s,r))), 7);                             // geht
zpf_a(i,r) = round(a0(i,r) - VA_PA.L(i,r), 6);                                          // geht
zpf_a(i,r) = round(sum(s, vtwr("trn",i,s,r)) - sum(s, VA_PT.L(i,s,r)), 7);                        // geht // here


*zpf_a(i,r) = round(      + VA_PA.L(i,r)
*                         - VA_PY.L(i,r) - sum(s, VA_PYxm.L(i,s,r)) - VA_PT.L(i,r), 7);  // geht nicht
*zpf_a(i,r) = round(vim(i,r) - sum(s, VA_PYxm.L(i,s,r)), 7);                             // geht nicht
*zpf_a(i,r) = round(vim(i,r) - sum(s, VA_PYxm.L(i,s,r)* pmx0(i,s,r)), 7);                // geht nicht
*zpf_a(i,r) = round(vim(i,r) - sum(s, VA_PYxm.L(i,s,r)* pmx0(i,s,r)) - VA_PT.L(i,r), 7); // geht nicht
*zpf_a(i,r) = round(vim(i,r) - sum(s, VA_PYxm.L(i,s,r)) - VA_PT.L(i,r), 7);              // geht nicht
*zpf_a(i,r) = round(vim(i,r)      - sum(s, VA_PYxm.L(i,s,r)* pmx0(i,s,r)) - sum(s, vtwr("trn",i,s,r) * pmt0(i,s,r)), 7); // geht nur für Einzelländer, nicht für Ländergruppen
*zpf_a(i,r) = round( + vim(i,r)   - sum(s, vxmd(i,s,r) * pmx0(i,s,r))
*                                 - sum(s, vtwr("trn",i,s,r) * pmt0(i,s,r)), 7);         // geht
zpf_a(i,r) = round(d0(i,r) - VA_PY.L(i,r), 6);                                           // geht nur für Einzelländer, nicht für Ländergruppen
*zpf_a(i,r) = round(d0(i,r) - VA_PY.L(i,r), 6);                                          // geht nur für Einzelländer, nicht für Ländergruppen
*zpf_a(i,r) = round(d0(i,r) - VA_PY.L(i,r) +  sum(s$vxmd(i,r,r), vxmd(i,s,r) ), 6);      // geht nur für Einzelländer, nicht für Ländergruppen
*zpf_a(i,r) = round(sum(s, vxmd(i,s,r)) - sum(s, VA_PYxm.L(i,s,r)), 7);                  // geht nur für Einzelländer, nicht für Ländergruppen

*display VA_PA.L, VA_PY.L, VA_PYxm.L, VA_PT.L, a0, d0, vxmd, vtwr1, vtwr, rtms, rtxs, pmx0, pmt0, tm, tx;
display zpf_a;

* ------ 14.09.2015 (Diss) Vortrag --> Reporting
*Execute_Unload       "%resultsdir%results_static.gdx";
*Execute  'gdxxrw.exe i=%resultsdir%results_static.gdx o=%resultsdir%report_pivot_static.xlsx epsout=0 @dumppar_static.txt'


*$EXIT
*EXITsolve2
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ Declaration of DYNAMIC (yr) parameters before the loop! --------------- // # DYNAMIC parameters #
* ------------------------------------------------------------------------------

PARAMETERS
* ------ 18.06.2014 Yearly
         aeei_yr(r,i,g,yr)       Yearly AEEI
         aeei_elexyr(r,gen,yr)   Yearly AEEI for ELE extant technologies
         aeei_elenyr(r,gen,yr)   Yearly AEEI for ELE new technologies
*------- 04.10.2017 sector-specific AEEEI
	 aeei_yr_sec(r,i,sec,yr) sector-specific AEEI
* ------ 25.02.2015 [ORIGINAL 0.1]
         aeei_fct                Average yearly efficiency improvement in %      / 0.25 /
* ------ Textmarke RWE
         velex_pgenyr(r,gen,yr)
         velen_pgenyr(r,gen,yr)
         zubau_ele(r,gen,yr)
         gew_wg(r,gen,yr)
         wg_diff(r,gen,yr)
         delta_wg_before(r,gen,yr)
         ele_eff(r)              SCHALTER für einfache AEEI-Effizienzverbesserungen (statt detailliertem Wirkungsgrad) in der Stromerzeugung
* ------ 18.06.2014 Specify upper and lower boundes for ELE gen technologies
         gen_limit_yr(r,gen,yr)  Specify upper bounds for ELE gen technologies
         genlimit_twh_yr         Report upper bounds for new ELE gen technologies in TWh
         up_yr(r,gen,yr)         Report yearly upper boundes for ELE gen technologies
         lo_yr(r,gen,yr)         Report yearly lower boundes for ELE gen technologies

* ------ Straight model results ------------------------------------------------
         pk_yr(*,yr)             Yearly price of capital earnings
         pusk_yr(*,yr)
         pskl_yr(*,yr)
         pc_yr(*,yr)
         pr_yr(*,*,yr)
         py_yr(*,*,yr)
         pa_yr(*,*,yr)
         pgen_yr(*,*,yr)
         pinv_yr(*,yr)
         rkx_ele_yr(r,gen,yr)
         pco2_ets_yr(yr)
         pco2_nets_yr(yr)
         pco2_netsr_yr(r,yr)
         PCO2_inv_pay_yr(hh,r,yr)
         CO2_inv_pay_yr(hh,r,yr)
         pco2w_yr(yr)
         price_pco2w_yr(yr)
         pco2_yr(r,yr)
	 price_fuel_yr(*,*,yr)
        price_fuel_yr_chk(*,*,yr)
	 pco2_DEU_yr(sec,yr)

         VY_PY_yr(*,i,yr)
         VY_PGEN_yr(*,gen,yr)
* ------ 18.05.2015 ersetze VY_PA(j,i,r) durch VY_PAY(j,i,r)
         VY_PA_yr(*,j,i,yr)
*         VY_PAY_yr(*,j,i,yr)

         VINV_PINV_yr(*,yr)
         VYT_PT_yr(yr)
         VA_PA_yr(*,i,yr)
         VC_PA_yr(i,r,yr)
	     VC_PC_yr(*,yr)
         VELEx_PGEN_yr
         VELEn_PGEN_yr
         VELEx_PA_yr(i,gen,r,yr)
         VELEn_PA_yr(i,gen,r,yr)

         VD_PINV_yr
         VD_PC_yr
         VD_PC_HH_yr(hh,r,yr)
         VD_PINV_HH_yr(hh,r,yr)
         VD_PC_gov_yr(r,yr)
         VINV_PA_yr

         Y_yr(*,i,yr)
         INV_yr(*,yr)
         YT_yr(yr)
         A_yr(*,i,yr)
         C_yr(*,yr)
         c_hh_yr(r,hh,yr)
         ELEx_yr(*,gen,yr)
         ELEn_yr(*,gen,yr)
         ELE_yr(*,gen,yr)

         velex_yr
         velen_yr
         vele_yr
         elextwh_yr
         elentwh_yr
         ele_twh_yr

         ELEn0
         ELEx0

         elen_up_yr(gen,r,yr)
         elex_up_yr(gen,r,yr)

         delta_ele(*,gen,r,yr)

* ------ 30.07.2014
         eletwhyr        Write all ELEx and ELEn bounds and levels (UP-LO-L) into a parameter (in TWh)
         invgdp_yr(*,yr) Investments share of GDP in %
         elecontwh_yr    Stromverbrauch im Inland inkl. Nettostromimporte
         elecontwh2_yr   Stromverbrauch im Inland inkl. Nettostromimporte sortiert nach Energieträgern
         eleytwh_yr(r,*,yr)      Electricity demand [TWh] per production sector
         enrg_yr
         rwa_yr(*,*,yr)
         rca_yr(*,*,yr)

* ------ Macroeconomic Indicators ----------------------------------------------
         welf_yr(r,yr)
         welf_hh_yr(hh,r,yr)
         gdpreal_yr(*,yr)
         gdpreal_disag_yr(*,*,yr)
         share_co2_gdp(*,*,yr)
         gdprealcum_yr(*,yr)
         gdpnom_yr(*,yr)
         gdpreal5_yr
         gva_real_yr
         gva_yr
         gva_yr_chk1
         gva_yr_chk2
         gva_yr_chk3

         demand_yr(*,i,*,yr)
         emplmt_yr(*,*,yr)
         emplmt_endow_yr(r,*,yr)        Employment endowment per region per year
         emplmt_sec_yr(*,*,*,yr)        Employment level per region per year per sector
         emplmtno_sec_yr(*,*,*,yr)      Employment level (absolute values) per region per year per sector
         emplmtno_yr(*,*,yr)
         ex_yr(*,*,yr)
         im_yr(*,*,yr)
         trdblnc_yr(*,*,yr)
         tradematrix_yr
         trdblnc2_yr
         ur_yr(*,*,yr)

* ------ CO2 parameters
         co2ets_yr(*,yr)         CO2 emissions in EU28 in ETS regime regime based on MPSGE report variables
         co2ets_y
         co2ets_elex
         co2ets_elen
         co2ets_ele
         co2ets_heat
         co2ets_cars
         co2c_yr
         co2y_yr
         co2ele_yr
         co2_yr
         co2_yr_2
         co2all_yr(*,*,yr)

         co2sec_yr(sec,yr)

         co2w_yr(*,*,yr)         CO2 emissions in worldtrade regime based on MPSGE report variables
         co2nets_yr(*,*,yr)      CO2 emissions in EU28 in non-ets regime based on MPSGE report variables
         co2nets_yr1(*,*,*,yr)   CO2 emissions in EU28 in non-ets regime based on co2em...
         carblim_yr(*,yr)
         carblim_ets_yr(*,yr)
         carblim_sec_yr(sec,r,yr)
         carblim_deu_yr(yr)
         co2gdp_yr(*,yr)         CO2-intensity relative to GDP (gCO2 per €GDP)
         co2gdp20_yr(*,yr)       CO2-costs (with 25 € per tCO2) relative to GDP (in %)
         co2gdp50_yr(*,yr)       CO2-costs (with 50 € per tCO2) relative to GDP (in %)
         gdpco2_yr(*,yr)

* ------ 1.04.2016
         gdpreal_yr2             gdpreal_yr without HH-GDP
         co2gdp_yr2              co2gdp_yr without HH-GDP



* ------ Capital reporting
         capgdp_yr(*,yr)         Capital intensity relative to GDP

         ks_x_yr(r,gen,yr)       ELEx capital ks_x per period
         evoa_yr(*,f,yr)         Factor endowment per period
         evoa1
         evoa11
         evoa2

         ele_capdem(r,gen,yr)    Yearly capital demand per technology and region
         ele_labdem(r,gen,yr)    Yearly labor demand per technology and region

* ------ 11.05.2015 Calculate input shares of Y and C as well as dynamic ZPF
         zpf_C_yr(r,yr)          ZPF for C per year
         zpf_Y_yr(i,r,yr)        ZPF for Y per year (except ele)
         shareC_yr(r,*,yr)       Input shares of C
         shareY_yr(r,*,i,yr)     Input shares of Y (except ele)
         shareCt_yr(r,yr)        1 minus sum of input shares of C must be zero
         shareYt_yr(r,i,yr)      1 minus sum of input shares of Y must be zero (except ele)

         zpf_gva_yr(i,r,yr)
         gva_real_chk
         shareYgva_yr(r,*,i,yr)
         shareYgva2_yr(r,*,i,yr)
         shareYgva3_yr(r,fe,*,i,yr)
         shareYtgva_yr(r,i,yr)
         eleYgva_yr(r,*,i,yr)    Electricity costs relative to GVA
         co2elecost_yr(r,i,yr)   Sum of sectoral direct and indirect CO2 costs per GVA
         co2elespec_yr(*,yr)     Specific CO2 emissions in electricity generation per region and year [Mio. t CO2 per TWh]

* ------ 18.05.2015 Check-Parameter
         chk1_bmk
         chk1_yr
         chkco2(i,r,yr)

*-------30.10.2017 R_supply_yr to check the development of the variable through the years (for easier debugging)
	     r_supply_yr(*,*,yr)

* ------ 06.11.2017 carbon tax yr
         carbon_tax_yr(r,yr)
         Pcarbon_tax_yr(r,yr)
         carb_tax_m_yr(r,yr)


* ----- 24.08.2018 calculate the energy demand per sector per energy carrier
         energy_cons_sec_yr(*,*,*,yr)   energy cosumption per sector per energy carrier (PJ)

* ----- 22.02.2019 HH disag
         VC_hh_PA_yr(i,hh,r,yr)
         VC_gov_PA_yr(i,r,yr)

         VC_hh_PC_yr(hh,*,yr)
         VC_gov_PC_yr(*,yr)


         VTAXTRANS_PGov_yr(r,yr)

         VLKTAX_PITAX_yr(r,yr)

         share_sector_hh_yr(r,*,hh,yr)

         abs_sector_hh_yr(r,*,hh,yr)

         VC_hh_CO2_NETSr_yr(hh,r,yr)   
         VC_hh_CO2_SEC_yr(hh,r,yr)  

         VC_hh_CO2_inv_pay_yr(hh,r,yr)

         VC_CO2_SEC_yr(r,yr)

         RA_hh_par_yr(*,r,hh,yr)

         share_income_hh(*,r,hh,yr)

;

* ______________________________________________________________________________
* ------ Parameter Declarations ------------------------------------------------
* ------ 06.02.2018 yearly carbon tax --> tax pattern as decided in ENavi
carbon_tax_yr(r,yr) =0;
carbon_tax_yr(r,"2025")$(deu(r)) =50;
carbon_tax_yr(r,"2030")$(deu(r)) =80;
carbon_tax_yr(r,"2035")$(deu(r)) =100;
carbon_tax_yr(r,"2040")$(deu(r)) =125;
carbon_tax_yr(r,"2045")$(deu(r)) =175;
carbon_tax_yr(r,"2050")$(deu(r)) =240;


* ------ 18.06.2014 Yearly AEEI
aeei_yr(r,i,g,yr) = 1;
aeei_elexyr(r,gen,yr) = 1;
aeei_elenyr(r,gen,yr) = 1;
display aeei_yr, aeei_elexyr, aeei_elenyr;

*-----04.10.2017 sector-specific AEEI
aeei_yr_sec("DEU",i,sec,yr) = 1;
display aeei_yr_sec;

* ------ 18.06.2014 Wirkungsgrade
ele_eff(r) = 0 ;         //  Schalter auf 1 bedeutet delta_wg=0 also einfache AEEI-verbesserung in Strom i.H. v. 0.1% p.a. [ORIGINAL]
*ele_eff(r) = 1 ;         //  Schalter auf 1 bedeutet delta_wg=0 also einfache AEEI-verbesserung in Strom i.H. v. 0.1% p.a.
* ------ 14.01.2015 Tectmarke RWE --Y Schalter auf 1, weil sonst aeei_elenyr negativ für USA, OEC, OPE...

* ------ Store benchmark electricity generation activity into a parameter
ELEn0(gen,r) = ELEn.L(gen,r);
ELEx0(gen,r) = ELEx.L(gen,r);
display ELEn0, ELEx0;

* ----- LOOK HERE to CHECK YEARS (DOWN)


* ------ CO2 regimes -----------------------------------------------------------



* ----- LOOK HERE to CHECK YEARS (UP)

scalar check_nuc;

*$EXIT
*ExitLOOP
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ---------------------------------------------------------------------------- *
*        L O O P
* ---------------------------------------------------------------------------- *

LOOP (yr,                                                                        // # LOOP-start #
*LOOP (yrx(yr),                                                                  // 10-year-milestones Berechnungen
*LOOP (before2011(yr),
*LOOP (before2015(yr),
*LOOP (before2020(yr),
*LOOP (before2025(yr),
*LOOP (before2030(yr),
*LOOP (before2035(yr),
*LOOP (before2040(yr),
*LOOP (before2050(yr),
* Textmarke LOOP

* ------ BEFORE-SOLVE ----------------------------------------------------------

c_hh0("oil_transport",r)$h_t_cons_reg(r)   = hh_energy_share(r,"oil","Transport",yr) * vafm("oil","c",r);
c_hh0("oil",r)$h_t_cons_reg(r)             = hh_energy_share(r,"oil","Others",yr) * vafm("oil","c",r);

c_hh0("ele_transport",r)$h_t_cons_reg(r)   = hh_energy_share(r,"Electricity","Transport",yr) * vafm("ele","c",r);
c_hh0("ele",r)$h_t_cons_reg(r)             = hh_energy_share(r,"Electricity","Others",yr) * vafm("ele","c",r);

* ------ DISSERTATION SCENARIO SWITCHES

* ------ Scenario 1: Reference - Reference 2020 with corona
    reference_scenario_2020$diss_ref =   1;
    corona_scenario_2020$diss_ref    =   1;
    germany_reference_nEHS$diss_ref  =   1;

*    test_lower_tax$(after2020(yr) and diss_ref) = 1;

* ------ Scenario 2: New targets and no recycling (business as usual) - same as ARIADNE 7
    reference_scenario_2020$diss_BAU =   1;
    corona_scenario_2020$diss_BAU    =   1;


    etstrade$(diss_BAU and after2020(yr))                     =   1;      // ETS with new targets is part of the green Deal
    netstrade_r$(diss_BAU and after2020(yr))                  =   1;      // regional reduction targets for non-ETS sectors in the EU
    row_notrade$(diss_BAU and after2020(yr))                  =   1;      // National reduction targets for non-EU countries
    notrad(r)$(NOT eu28(r) AND diss_BAU AND after2020(yr))    =   1;      // activating the co2 price for non-EU countries


* ------ Scenario 3: New targets (ARIADNE 7) and per capita redistribution
    reference_scenario_2020$diss_CAP =   1;
    corona_scenario_2020$diss_CAP    =   1;


    etstrade$(diss_CAP and after2020(yr))                     =   1;      // ETS with new targets is part of the green Deal
    netstrade_r$(diss_CAP and after2020(yr))                  =   1;      // regional reduction targets for non-ETS sectors in the EU
    row_notrade$(diss_CAP and after2020(yr))                  =   1;      // National reduction targets for non-EU countries
    notrad(r)$(NOT eu28(r) AND diss_CAP AND after2020(yr))    =   1;      // activating the co2 price for non-EU countries

    per_capita_dis_NETSr$(after2020(yr) AND diss_CAP)         =   1;      // activates per capita redistribution after 2020


* ------ Scenario 4: New targets (ARIADNE 7) and lower VAT
    reference_scenario_2020$diss_VAT =   1;
    corona_scenario_2020$diss_VAT    =   1;


    etstrade$(diss_VAT and after2020(yr))                     =   1;      // ETS with new targets is part of the green Deal
    netstrade_r$(diss_VAT and after2020(yr))                  =   1;      // regional reduction targets for non-ETS sectors in the EU
    row_notrade$(diss_VAT and after2020(yr))                  =   1;      // National reduction targets for non-EU countries
    notrad(r)$(NOT eu28(r) AND diss_VAT AND after2020(yr))    =   1;      // activating the co2 price for non-EU countries

    no_vat$(after2020(yr) AND diss_VAT)                       =   1;      // activates redistribution by reducing VAT


* ------ Scenario 5: New targets (ARIADNE 7) and lower LAbor tax
    reference_scenario_2020$diss_LAB =   1;
    corona_scenario_2020$diss_LAB    =   1;


    etstrade$(diss_LAB and after2020(yr))                     =   1;      // ETS with new targets is part of the green Deal
    netstrade_r$(diss_LAB and after2020(yr))                  =   1;      // regional reduction targets for non-ETS sectors in the EU
    row_notrade$(diss_LAB and after2020(yr))                  =   1;      // National reduction targets for non-EU countries
    notrad(r)$(NOT eu28(r) AND diss_LAB AND after2020(yr))    =   1;      // activating the co2 price for non-EU countries

    low_lab_tax$(after2020(yr) AND diss_LAB)                  =   1;      // activates redistribution by reducing VAT

* ------ Scenario 6: New targets (ARIADNE 7) and co2 revenue inversely to expenditure
    reference_scenario_2020$diss_inv_payment =   1;
    corona_scenario_2020$diss_inv_payment    =   1;


    etstrade$(diss_inv_payment and after2020(yr))                     =   1;      // ETS with new targets is part of the green Deal
    netstrade_r$(diss_inv_payment and after2020(yr))                  =   1;      // regional reduction targets for non-ETS sectors in the EU
    row_notrade$(diss_inv_payment and after2020(yr))                  =   1;      // National reduction targets for non-EU countries
    notrad(r)$(NOT eu28(r) AND diss_inv_payment AND after2020(yr))    =   1;      // activating the co2 price for non-EU countries

    inverse_co2_pay$(after2020(yr) AND diss_inv_payment)              =   1;      // activates redistribution by reducing VAT


    e_sub(r)$(eu28(r) AND yr2025(yr))         =   0.5;
    e_sub(r)$(eu28(r) AND after2025(yr))         =   e_sub(r) + 0.5;

    cons_sub(r)$(eu28(r) AND yr2025(yr))         =   1.5;
    cons_sub(r)$(eu28(r) AND after2025(yr))         =   cons_sub(r) + 0.5;


bmk_ele_trans$yr2015(yr) = 1;
ELE_trans.l(r)$(h_t_cons_reg(r) AND yr2015(yr))=1;

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ PATHs -----------------------------------------------------------------
* --->>> RECURSIVE Labor DYNAMICS (growth / endowment driver) ------------------ // # LABOR dynamics #
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

*ele_dev_switch_actual(gen,r)$(after2015(yr) AND times_coupling) = ele_dev_switch(gen,r);
*ele_dev_actual(gen,r)$(after2015(yr) AND times_coupling AND ele_dev_switch_actual(gen,r)) = ele_dev(gen,r,yr) - abschr_pfad(r,gen,yr);

*ele_dev_switch("bBC","EUS")$(yr2045(yr) OR yr2040(yr)) = 0;

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ----- NEW TAXATION - 11.03.2019
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  



* ---- For german Tax that starts in 2021
carbon_tax_de$(germany_reference_nEHS AND carbon_tax_yr("deu",yr)) = 1;
carbon_tax(r) = carbon_tax_yr(r,yr);

* ----- 08.02.2018 - carbon tax update

*carbon_tax(r) = carbon_tax_yr(r,yr);

* 06.01.2018 set cost reduction for the acutal year

* --------- 07.11.2017 reduce renewables costs
diffcost(gen,r)$(cost_red(gen,yr)) = diffcost0(gen,r) * cost_red(gen,yr);

* ----- 27.02.2018 calculate the input of labour and capital for each region, technology and year
skl_input_yr(r,gen,yr) = (skl_input(gen,r)*(diffcost(gen,r)));
usk_input_yr(r,gen,yr) = (usk_input(gen,r)*(diffcost(gen,r)));
cap_input_yr(r,gen,yr) = (cap_input(gen,r)*(diffcost(gen,r)));

* ------ 18.01.2018 - price endog for last years
pricetarget(i,r)$( (cru(i) or col(i) or gas(i) ) and not (eu28(r) or deu(r)) and after2045(yr) and DEU_sec ) = 0;

* ------ 24.01.2018 - changing switches for different scenarios
ETStrade$reference_scenario_2020 = 1;

* ------ 05.06.2014 Change LABOR Force according to
* ------ "size_usk" + "size_skl" multiplied with evoa0
evoa("skl",r)  =  evoa0("skl",r) * size_skl(r,yr) ;
evoa("usk",r)  =  evoa0("usk",r) * size_usk(r,yr) ;

* ------ 27.08.2020 coefficient for corona
evoa("skl",r)$(after2015(YR) AND corona_scenario_2020) = evoa("skl",r) * tfp_corona(yr,r);
evoa("usk",r)$(after2015(YR) AND corona_scenario_2020) = evoa("usk",r) * tfp_corona(yr,r);

* ------ Save evoa in yearly parameter
evoa_yr(r,f,yr) = evoa(f,r);

* ----- 27.02.2018 calculate the empoyment endowment per yr and region
emplmt_endow_yr(r,"skl",yr) = ( evoa("skl",r) / (1-ursk0(r)));
emplmt_endow_yr(r,"usk",yr) = ( evoa("usk",r) / (1-urun0(r)));

* --->>> Electricity Generation Capital Dynamics ------------------------------- // # ELE capital dynamics #
* ------ Übergabe des bestehenden kapitalstocks
* ------ Depreciate extant ELE CAPITAL holdings according to "ABSCHREIBUNG" [ORIGINAL]:
* ------ the respective entries for 2007 should be neutral (1 or 0, respectively)
* ------ ORIGINAL
*ks_x(gen,r)$(out_gen(gen,r)$ks_x(gen,r))         = ks_x(gen,r) * (1 - abschreibung(r,gen,yr)) ;
*ks_x(gen,r)$(out_gen(gen,r)$(ks_x(gen,r) LE 0))  = 1e-7 ;
* ------ 14.10.2014 --> 3.11.2014
ks_x(gen,r)$(out_gen(gen,r)$ks_x(gen,r))         = ks_x0(gen,r) * abschr_pfad(r,gen,yr) ;
ks_x(gen,r)$(out_gen(gen,r)$(ks_x(gen,r) LE 1e-7))  = 1e-7 ;

* ------ Zur Speicherung an parameter "ks_x_yr":
ks_x_yr(r,gen,yr)   = ks_x(gen,r) ;


* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* --->>> AEEI + Wirkungsgrad ---------------------------------------------------  // # AEEI #
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

* ------ 16.07.2014 SET energy productivity increase from BASELINE-Database as AEEI
aeei_yr(r,e,g,yr) = ep(yr,r) ;

* ------- 24.10.16	Increase AEEI in Germany, France and southern EU (very low in baseline database: >0.95 in 2050) --> Achtung! This seems very arbitrary!!
aeei_yr(r,e,g,yr)$(deu(r) or fra(r) or eus(r))      = 0.9 * ep(yr,r) ;
aeei_yr(r,e,g,"2011")$(deu(r) or fra(r) or eus(r))  = ep("2011",r) ;

*-------04.10.2017	Implement sector-specific AEEEI
aeei_yr_sec("DEU",e,sec,yr) = ep(yr,"DEU");
*aeei_yr_sec(r,e,sec,yr)$(deu(r))	= 0.9 * ep(yr,r);
aeei_yr_sec("DEU",e,sec,"2011") = ep("2011","DEU");

* ------ 21.02.2018 update specific sectors AEEI to make emissions match the EU reference scenario

aeei_yr(r,fe,g,yr)$(eu28(r) AND sec2cluster("industry",g)) = aeei_ff(r,g,yr);
aeei_yr(r,"ele",g,yr)$(eu28(r) AND sec2cluster("industry",g)) = aeei_ele(r,g,yr);

aeei_yr(r,fe,"TRN",yr)$(eu28(r)) = aeei_ff(r,"TRN",yr);
aeei_yr(r,"ele","TRN",yr)$(eu28(r)) = aeei_ele(r,"TRN",yr);

aeei_yr(r,fe,"SER",yr)$(eu28(r)) = aeei_ff(r,"SER",yr);
aeei_yr(r,"ele","SER",yr)$(eu28(r)) = aeei_ele(r,"SER",yr);

aeei_yr(r,fe,"c",yr)$(eu28(r)) = aeei_ff(r,"c",yr);
aeei_yr(r,"ele","c",yr)$(eu28(r)) = aeei_ele(r,"c",yr);

aeei_yr(r,fe,"AGR",yr)$(eu28(r)) = aeei_ff(r,"AGR",yr);
aeei_yr(r,"ele","AGR",yr)$(eu28(r)) = aeei_ele(r,"AGR",yr);


* ------ 25.02.2018 try
aeei_elexyr(r,gen,yr) = ep(yr,r);
aeei_elenyr(r,gen,yr) = ep(yr,r);

* ------ 18.06.2014 Report yearly ELE output
velex_pgenyr(r,gen,yr) = VELEx_PGEN.L(gen,r)   ;
velen_pgenyr(r,gen,yr) = VELEn_PGEN.L(gen,r)   ;

* ------ 18.06.2014 Calculate yearly ELE annex
zubau_ele(r,gen,yr)$(velen_pgenyr(r,gen,yr) and after(yr)) = velen_pgenyr(r,gen,yr) - velen_pgenyr(r,gen,yr-1);

* ------ 18.06.2014 Calculate yearly efficiency factors
gew_wg(r,gen,"2011") = wg0(gen,r);
gew_wg(r,gen,yr)$(velen_pgenyr(r,gen,yr) and after(yr))  = (gew_wg(r,gen,yr-1) * velen_pgenyr(r,gen,yr-1) + wg_yr(gen,yr) * zubau_ele(r,gen,yr) ) / velen_pgenyr(r,gen,yr);
gew_wg(r,gen,yr)$(not velen_pgenyr(r,gen,yr))            = gew_wg(r,gen,yr-1);
gew_wg(r,gen,yr)$(ele_eff(r) and after(yr))              = wg0(gen,r);
wg_diff(r,gen,yr)                                        = wg_yr(gen,yr) - gew_wg(r,gen,yr);
delta_wg_before(r,gen,yr)$(gew_wg(r,gen,yr)$after(yr))   = gew_wg(r,gen,yr) / gew_wg(r,gen,yr-1) - 1;

* ------ 18.06.2014 NEWAGE7: Andere AEEIs definieren (gen / ELE / ...)
* ------ Define AEEI for ELEx
aeei_elexyr(r,"bNUC",yr)                 = 1 ;
aeei_elexyr(r,foscoal,yr)                = aeei_yr(r,"col","ele",yr);        // inkl. CCS
aeei_elexyr(r,fosgas,yr)                 = aeei_yr(r,"gas","ele",yr);
aeei_elexyr(r,fosoil,yr)                 = aeei_yr(r,"oil","ele",yr);

* ------ 26.02.2015 BBC pushen durch Wirkungsgradsteigerung der alten Anlagen!
*aeei_elexyr(r,bbc,yr)$after(yr) = 0.95 * aeei_elexyr(r,bbc,yr-1);
*aeei_elexyr(r,gen,yr)$after(yr) = 0.975 * aeei_elexyr(r,gen,yr-1);
aeei_elexyr(r,foscoal,yr)$after(yr) = 0.98 * aeei_elexyr(r,foscoal,yr-1);      // bGAS weglassen

* ------ Define AEEI for ELEn
aeei_eleNyr(r,gen,yr)$yr2011(yr)         = aeei_eleXyr(r,gen,yr);
aeei_eleNyr(r,gen,yr)$yr2015(yr)         = aeei_eleNyr(r,gen,yr-1) * (1 - aeei_fct/100)**3 ;
aeei_eleNyr(r,gen,yr)$after2015(yr)      = aeei_eleNyr(r,gen,yr-1) * (1 - aeei_fct/100)**5
         - (DELTA_WG_BEFORE(r,gen,yr-1) * (aeei_eleNyr(r,gen,yr-1) * (1 - aeei_fct/100)**5));
aeei_eleNyr(r,gen,yr)$ele_eff(r)         = aeei_eleXyr(r,gen,yr);        // Schalter auf 1 bedeutet delta_wg=0 also einfache AEEI-verbesserung in Strom i.H. v. 0.1% p.a.

* ------ 14.01.2015 Bei worldtrade wird aeei_eleNyr für manche Regionen und ELEn-Technologien negativ: in diesem Falle = 0.1 setzen!
*aeei_eleNyr(r,gen,yr)$(aeei_eleNyr(r,gen,yr) lt 0.1) = 0.1;
aeei_eleNyr(r,gen,yr)$(aeei_eleNyr(r,gen,yr) lt 0.001)   = 0.01;
aeei_eleNyr(r,gen,yr)$(aeei_eleNyr(r,gen,yr) gt 1)       = 1;

* ---->> 18.08.2020 - fossil fuel generation with constant efficiency
aeei_elexyr(r,gen,yr)$(fosfosgen(gen)) = 1;
aeei_elenyr(r,gen,yr)$(fosfosgen(gen)) = 1;


* --->>> 18.06.2014 Hand-over model specific paramaters for each period <<< ----
aeei(i,g,r)      = aeei_yr(r,i,g,yr) ;
aeei_elex(gen,r) = aeei_elexyr(r,gen,yr) ;
aeei_elen(gen,r) = aeei_elenyr(r,gen,yr) ;

*-----04.10.2017	for sector-specific AEEI
aeei_sec(i,sec,"DEU")	= aeei_yr_sec("DEU",i,sec,yr);

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* --->>> Create Upper Bounds for ELEn (.UP)
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

* ------ Upper limit for Thermal plants (genlim = bbc, boil, moil)
gen_limit_yr(r,gen,yr)$foslim(gen)= ELEn0(gen,r)*(1+(1-aeei_elenyr(r,gen,yr)))
                                  + ((1-abschreibung_bmk(r,gen,yr))*ELEx0(gen,r))*(1+(1-aeei_elenyr(r,gen,yr)));
* ------ Upper limit for RES plants
gen_limit_yr(r,gen,yr)$reslim(gen)= ELEn0(gen,r)
                                  + ((1-abschreibung_bmk(r,gen,yr))*ELEx0(gen,r)) + ressize(r,gen,yr)*(ELEn0(gen,r) + ELEx0(gen,r)) ;
* ------ Upper limit for NUC plants (phase-out)
gen_limit_yr(r,gen,yr)$(bnuc(gen) and nucpot(r)) = ELEn0(gen,r)
                                  + ((1-abschreibung_bmk(r,gen,yr))*ELEx0(gen,r)) + nucsize(r,yr)    *(ELEn0(gen,r) + ELEx0(gen,r)) ;
gen_limit_yr(r,gen,yr)$(bnuc(gen) and nucout(r)) = ELEn0(gen,r) * nucsize(r,yr) ;

* ------ 07.07.2014 Reporting upper bound limits in TWh
genlimit_twh_yr(r,gen,yr) = gen_limit_yr(r,gen,yr) * ele_prod(gen,r);


* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ###### MODEL CONSTRAINTS
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

* xxxxxx SZENARIO SCHALTER - CO2 Regimes xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx



* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* --->>> PYtarget --------------------------------------------------------------  // # Price dynamics #
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ 5.01.2016 R_SUPPLY ausschalten

pytarget("cru",r)$pricetarget("cru",r) = pytarget_yr(r,"cru",yr);
pytarget("gas",r)$pricetarget("gas",r) = pytarget_yr(r,"gas",yr);
pytarget("col",r)$pricetarget("col",r) = pytarget_yr(r,"col",yr);



display pytarget, pricetarget;

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* --->>> diffcost / diffrebate / REBATE_DIFF
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ 09.07.2014 Remove e:pa... line in order to give room for rebate_diff to rebate diffcosts to ele price; Set diffrebate(bawdeu) = 1
* ------ NOT in 2007 --> $after(yr)
epa_on$after(yr)                 = 0;
*diffrebate(bawdeu)$after(yr)     = 1;
* ------ 3.02.2016 BAW auskommentieren
*diffrebate(deu)$after(yr)        = 1;


* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* --->>> ELEn.UP / Y.LO / A.LO --- Bounds for electricity generation
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*if ((times_coupling = 0 OR yr2011(yr)),
* OR before2020(yr)
*$ontext

elex.up(gen,r)$(eu28(r) AND REEEM_calib AND yr2020(yr)) = +Inf;
elen.up(gen,r)$(eu28(r) AND REEEM_calib AND yr2020(yr) ) = +Inf;


* ------ 5.05.2015 Overall ELE limitation for upper generation bounds
**ELEn.UP(gen,r)$(REEEM_calib AND after2015(yr) and deu(r) and FOSgas(gen)) = 5 * ELEn.L(gen,r) + 10 * (1-thetax) ;
*ELEn.UP(gen,r)$(after2015(yr)) = 5 * ELEn.L(gen,r) + 10 * (1-thetax) ;
ELEn.UP(gen,r)$after2040(yr) = 2 * ELEn.L(gen,r) + 2  * (1-thetax) ;

* ------ NUC 
check_nuc = ELEn.UP("bNUC","EUS");
display "check_nuc -1. ",yr, ">>" ,check_nuc;

ELEn.UP(gen,r)$(bnuc(gen)            and after(yr))      = 1.0 * gen_limit_yr(r,gen,yr);
ELEn.UP(gen,r)$(bnuc(gen)            and after2015(yr))  = 0.8 * gen_limit_yr(r,gen,yr);

check_nuc = ELEn.UP("bNUC","EUS");
display "check_nuc 0. ",yr, ">>" ,check_nuc;

*ELEn.UP(gen,r)$(bnuc(gen) and fra(r) and     yr2030(yr))  = 2 * ELEn.UP(gen,r);
*ELEn.UP(gen,r)$(bnuc(gen) and fra(r) and after2030(yr))  = 0.8 * gen_limit_yr(r,gen,yr);
*ELEn.UP(gen,r)$(bnuc(gen) and fra(r) and after2040(yr))  = 0.5 * gen_limit_yr(r,gen,yr);

ELEn.UP(gen,r)$(bnuc(gen) and fra(r) and after2015(yr))  = 0.149 * (elen.l(gen,r) + elex.l(gen,r));
*ELEn.UP(gen,r)$(bnuc(gen) and fra(r) and after2020(yr))  =   (-21.586 * (ord(yr) - 3) ** 2) + (140.04 * (ord(yr) - 3)) + 63.935;

ELEn.UP(gen,r)$(bnuc(gen) and fra(r) and after2020(yr))  = ((-1.13 * log(ord(yr) - 3)) + 2.5) * elen.l(gen,r);

ELEn.UP(gen,r)$(bnuc(gen) and pol(r) and before2035(yr))  = 0;
ELEn.UP(gen,r)$(bnuc(gen) and pol(r) and after2030(yr))  = 10000;
ELEx.UP(gen,r)$(bnuc(gen) and pol(r))  = 0;
*ELEn.UP(gen,r)$(bnuc(gen) and esp(r))  = 0;
ELEn.UP(gen,r)$(bnuc(gen) and bnl(r))  = 0;
 
ELEn.UP(gen,r)$(bnuc(gen) and eus(r) and after2030(yr))  = 0.9 * elen.l(gen,r);

check_nuc = ELEn.UP("bNUC","EUS");
display "check_nuc 1. ",yr, ">>" ,check_nuc;


* ------ COAL, GAS and OIL

ELEn.up(gen,r)$(bbc(gen)  and pol(r) and after2020(yr)) = ((-0.247 * log(ord(yr) - 3)) + 0.9079) * elen.l(gen,r);

ELEn.up(gen,r)$(bbc(gen)  and esp(r) and after2015(yr)) = 0;
ELEn.up(gen,r)$(bbc(gen)  and uki(r) and after2015(yr)) = 0;

ELEx.up(gen,r)$(bbc(gen)  and eus(r) and after2015(yr)) = ((-0.287 * log(ord(yr) - 2)) + 0.5644) * elex.l(gen,r);
ELEn.up(gen,r)$(bbc(gen)  and eus(r) and after2025(yr)) = ((-0.493 * log(ord(yr) - 4)) + 1.1759) * elen.l(gen,r);

ELEn.up(gen,r)$(FOScoalh(gen) AND UKI(r) AND yr2020(yr)) = 4 * ELEN.l(gen,r);
ELEn.up(gen,r)$(FOScoalh(gen) AND UKI(r) AND after2020(yr)) = 0.5 * ELEN.l(gen,r);
ELEn.up(gen,r)$(FOScoalh(gen) AND UKI(r) AND after2035(yr)) = 0;

ELEn.up(gen,r)$(FOSoil(gen) AND eu28(r) AND after2020(yr)) = 0.9 * ELEN.l(gen,r);

* ------ 10.12.2014 Hydro, Bio and Geo

ELEn.UP(gen,r)$(bhydro(gen)          and after(yr) AND NOT (REEEM_calib AND yr2015(yr)))      = 0.75 * gen_limit_yr(r,gen,yr);
ELEn.UP(gen,r)$(bhydro(gen)          and after2030(yr))  = 1.10 * ELEn.L(gen,r);
ELEn.UP(gen,r)$(bhydro(gen) AND EU28(r) and yr2020(yr))  = 1.7 * ELEn.L(gen,r);
ELEn.UP(gen,r)$(bhydro(gen) AND (ITA(r) OR ESP(r) OR EUN(r) OR EUS(r)) and yr2020(yr))  = 1.9 * ELEn.L(gen,r);
ELEn.UP(gen,r)$(bhydro(gen) AND EU28(r) and after2020(yr))  = 1.2 * ELEn.L(gen,r);


ELEn.UP(gen,r)$(phydro(gen)  AND eu28(r) AND after2020(yr))  = 1.25 * ELEn.L(gen,r);
*ELEn.UP(gen,r)$(phydro(gen)  AND EUS(r) AND after2015(yr))  = 1.1 * ELEn.L(gen,r);

ELEn.UP(gen,r)$(bbio(gen)     and fra(r)     and after2020(yr))  = ELEn.L(gen,r);

ELEn.UP(gen,r)$(bhydro(gen) and eun(r) and after2015(yr))  = 0.62;
ELEn.UP(gen,r)$(bhydro(gen) and eun(r) and after2025(yr))  = 0.85;
ELEn.UP(gen,r)$(bhydro(gen) and eun(r) and after2040(yr))  = 0.98;


ELEn.UP(gen,r)$(bbio(gen) and esp(r) and after2040(yr))      =  ELEn.L(gen,r);
ELEn.UP(gen,r)$(bbio(gen) and ita(r) and after2025(yr))      = 1.2 * ELEn.L(gen,r);
ELEn.UP(gen,r)$(bbio(gen) and ita(r) and yr2050(yr))      =  ELEn.L(gen,r);
ELEn.UP(gen,r)$(bbio(gen) and pol(r) and after2020(yr))      = 1.1 * ELEn.L(gen,r);
ELEn.UP(gen,r)$(bbio(gen) and bnl(r) and after2030(yr))      = 1.1 * ELEn.L(gen,r);

ELEn.UP(gen,r)$(bbio(gen) and eus(r) and after2030(yr))      = 1.35 * ELEn.L(gen,r);

ELEn.UP(gen,r)$(bbio(gen) and eun(r) and after2015(yr))      = 1.4 * ELEn.L(gen,r);

ELEn.UP(gen,r)$(bbio(gen) and uki(r) and after2015(yr))      = 1.2 * ELEn.L(gen,r);

ELEn.fx(gen,r)$(bbio(gen) and deu(r) and yr2015(yr))      = 0.59;
ELEn.UP(gen,r)$(bbio(gen) and deu(r) and after2015(yr))      = 1.3 * ELEn.L(gen,r);
ELEn.UP(gen,r)$(bbio(gen) and deu(r) and after2025(yr))      = 1.005 * ELEn.L(gen,r);
ELEn.UP(gen,r)$(bbio(gen) and deu(r) and after2040(yr))      = 1.7 * ELEn.L(gen,r);
ELEn.UP(gen,r)$(bbio(gen) and deu(r) and after2045(yr))      = 1.15 * ELEn.L(gen,r);

ELEn.UP(gen,r)$(bgeo(gen)            and after2015(yr))  = 1.25 * ELEn.L(gen,r);

* ------ mWIND and mSOLAR

ELEn.up(gen,r)$(mwind(gen)   and eu28(r) and after2015(yr))= 2.2 * ((ord(yr) - 2)**(-0.41)) * elen.l(gen,r);
ELEn.up(gen,r)$(mwind(gen)   and deu(r) and after2015(yr))= 1.6 * ((ord(yr) - 2)**(-0.2)) * elen.l(gen,r);
ELEn.up(gen,r)$(mwind(gen)   and deu(r) and yr2020(yr))= 3.5 * elen.l(gen,r);
 //
ELEn.up(gen,r)$(msolar(gen)  and eu28(r) and after2015(yr))= 2.4 * ((ord(yr) - 2)**(-0.31)) * elen.l(gen,r);
ELEn.up(gen,r)$(msolar(gen)  and (fra(r)) and after2015(yr))= 2.4 * ((ord(yr) - 2)**(-0.21)) * elen.l(gen,r);
ELEn.up(gen,r)$(msolar(gen)  and (ITA(r)) and after2015(yr))= 2.4 * ((ord(yr) - 2)**(-0.40)) * elen.l(gen,r);
ELEn.up(gen,r)$(msolar(gen)  and deu(r)  and after2020(yr))= ((-0.293 * log(ord(yr) - 3)) + 1.5979) * elen.l(gen,r);

ELEn.up(gen,r)$(mwind(gen)   and eu28(r) and after2020(yr) AND (diss_BAU OR diss_LAB OR diss_VAT OR diss_CAP or diss_inv_payment))= 2.45 * ((ord(yr) - 2)**(-0.39)) * elen.l(gen,r); //
ELEn.up(gen,r)$(msolar(gen)  and eu28(r) and after2020(yr) AND (diss_BAU OR diss_LAB OR diss_VAT OR diss_CAP or diss_inv_payment))= 2.45 * ((ord(yr) - 2)**(-0.33)) * elen.l(gen,r);
ELEn.up(gen,r)$(msolar(gen)  and deu(r)  and after2020(yr) AND (diss_BAU OR diss_LAB OR diss_VAT OR diss_CAP or diss_inv_payment))= ((-0.293 * log(ord(yr) - 3)) + 1.69) * elen.l(gen,r);


check_nuc = ELEn.UP("bNUC","EUS");
display "check_nuc 2. ",yr, ">>" ,check_nuc;

* ------ 16.02.2015 CCS Limitations
ELEn.UP(gen,r)$(fosccs(gen)              and before2040(yr))     = ELEn.L(gen,r) ;
ELEn.UP(gen,r)$(mccs(gen)                and after2040(yr) and not DEU(r))      = ELEn.L(gen,r) + 750 * (1-thetax);
ELEn.UP(gen,r)$(bccs(gen)                and after2040(yr) and not DEU(r))      = ELEn.L(gen,r) + 750 * (1-thetax);

*----- 03.03.2017 Switch to turn germany's CCS on and off
ELEn.UP(gen,r)$(mccs(gen)                and after2040(yr) and DEU(r) and DEU_CCS)      = ELEn.L(gen,r) + 750 * (1-thetax);
ELEn.UP(gen,r)$(bccs(gen)                and after2040(yr) and DEU(r) and DEU_CCS)      = ELEn.L(gen,r) + 750 * (1-thetax);

ELEn.UP(gen,r)$(mccs(gen)                and after2040(yr) and DEU(r) and not DEU_CCS)      = ELEn.L(gen,r);
ELEn.UP(gen,r)$(bccs(gen)                and after2040(yr) and DEU(r) and not DEU_CCS)      = ELEn.L(gen,r);

*------ 10.12.2014 RWE Szenario für bBC-Zubau-Beschränkung
*ELEn.UP(gen,r)$(bbc(gen) and deu(r))     = 1.0 * bbc_deu_elen(gen,yr) * (1-thetax);  // REF
*ELEn.UP(gen,r)$(bbc(gen) and deu(r)      and after2010(yr)) = 0 ;                 // Scen1
*ELEn.UP(gen,r)$(bccs(gen) and deu(r)     and after2010(yr)) = 0 ;

ELEn.UP(gen,r)$(bbc(gen) and deu(r) and yr2030(yr))     = 0.2;
ELEn.UP(gen,r)$(bbc(gen) and deu(r) and after2030(yr))     = abschreibung_bmk(r,gen,yr);
ELEn.UP(gen,r)$(mhc(gen) and deu(r) and after2030(yr))     = abschreibung_bmk(r,gen,yr);

*------ 03.03.2017 decomissioning coal in germany till 2035
ELEn.UP('bBC','DEU')$(noCOAL_DEU and after2020(yr)) = abschreibung_bmk('DEU','bBC',yr);
ELEn.UP('bHC','DEU')$(noCOAL_DEU and after2020(yr)) = abschreibung_bmk('DEU','bHC',yr);
ELEn.UP('mHC','DEU')$(noCOAL_DEU and after2020(yr)) = abschreibung_bmk('DEU','mHC',yr);

*ELEn.UP('bBC','DEU')$(noCOAL_DEU and after2035(yr)) = 0;
*ELEn.UP('bHC','DEU')$(noCOAL_DEU and after2035(yr)) = 0;
*ELEn.UP('mHC','DEU')$(noCOAL_DEU and after2035(yr)) = 0;
* ------ TEXTMARKE RWE

* ------ 26.02.2015
ELEx.FX(gen,r)$(ELEx.L(gen,r) le 0) = 0;
* ------ Textmarke RWE

* ----- 23.08.2017 - test TIMES coupling
check_nuc = ELEn.UP("bNUC","EUS");
display "check_nuc 3. ",yr, ">>" ,check_nuc;

*ELEx.up(gen,r)$(ele_dev_switch(gen,r) ) = ele_dev(gen,r,yr);
*ELEn.up(gen,r)$(ele_dev_switch(gen,r) ) = ele_dev(gen,r,yr);

* -- end if );

* --------  22.11.2018 Adding block to make electricity production in EU28 match with EUROSTAT for REEEM project

if ((REEEM_calib AND yr2015(yr)),

ELEx.up(gen,r)$(eu28(r) AND REEEM_calib ) = (abschr_pfad(r,gen,yr)$((ele_dev_REEEM(gen,r,yr) - abschr_pfad(r,gen,yr)) gt 0) + ele_dev_REEEM(gen,r,yr)$((ele_dev_REEEM(gen,r,yr) - abschr_pfad(r,gen,yr)) le 0)) ;
ELEn.up(gen,r)$(eu28(r) AND REEEM_calib ) = 1.02 * (ele_dev_REEEM(gen,r,yr)$((ele_dev_REEEM(gen,r,yr) - abschr_pfad(r,gen,yr)) gt 0) - abschr_pfad(r,gen,yr)$((ele_dev_REEEM(gen,r,yr) - abschr_pfad(r,gen,yr)) gt 0) + 0$((ele_dev_REEEM(gen,r,yr) - abschr_pfad(r,gen,yr)) le 0));


ELEn.lo(gen,r)$(eu28(r) AND REEEM_calib ) = 0.98 * (ele_dev_REEEM(gen,r,yr)$((ele_dev_REEEM(gen,r,yr) - abschr_pfad(r,gen,yr)) gt 0) - abschr_pfad(r,gen,yr)$((ele_dev_REEEM(gen,r,yr) - abschr_pfad(r,gen,yr)) gt 0) + 0$((ele_dev_REEEM(gen,r,yr) - abschr_pfad(r,gen,yr)) le 0));

  

ELEx.up(gen,r)$(eu28(r) AND REEEM_calib  AND NOT ele_dev_REEEM(gen,r,"2011")) = 0;
ELEn.up(gen,r)$(eu28(r) AND REEEM_calib  AND NOT ele_dev_REEEM(gen,r,"2011")) = 1.02 * ele_dev(gen,r,yr);
ELEn.lo(gen,r)$(eu28(r) AND REEEM_calib  AND NOT ele_dev_REEEM(gen,r,"2011")) = 0.98 * ele_dev(gen,r,yr);
 
cap_input(gen,r)$eu28(r) = cap_input0(gen,r)*cost_red(gen,yr);
);

elen.lo(gen,r)$(eu28(r) AND REEEM_calib AND after2015(yr)) = 0;

* ---------  The following block assures the coupling of NEWAGE and TIMES

ELEn.UP(gen,r)$(ele_dev_switch(gen,r) and after2015(yr) and times_coupling)= 1000 ; // BASELINE

check_nuc = ELEn.UP("bNUC","EUS");
display "check_nuc 4. ",yr, ">>" ,check_nuc;

if ((times_coupling = 1 AND NOT (yr2011(yr) OR yr2015(yr))),
* AND NOT before2020(yr)
ELEx.up(gen,r)$(ele_dev_switch(gen,r) AND times_coupling ) = 1.05 * (abschr_pfad(r,gen,yr)$((ele_dev(gen,r,yr) - abschr_pfad(r,gen,yr)) gt 0) + ele_dev(gen,r,yr)$((ele_dev(gen,r,yr) - abschr_pfad(r,gen,yr)) le 0)) ;
ELEn.up(gen,r)$(ele_dev_switch(gen,r) AND times_coupling AND NOT (mWIND(gen) OR msolar(gen) ) ) = 1.05 * (ele_dev(gen,r,yr)$((ele_dev(gen,r,yr) - abschr_pfad(r,gen,yr)) gt 0) - abschr_pfad(r,gen,yr)$((ele_dev(gen,r,yr) - abschr_pfad(r,gen,yr)) gt 0) + 0$((ele_dev(gen,r,yr) - abschr_pfad(r,gen,yr)) le 0));
ELEn.up(gen,r)$(ele_dev_switch(gen,r) AND times_coupling AND (mWIND(gen) OR msolar(gen)) ) = 1.02 * (ele_dev(gen,r,yr)$((ele_dev(gen,r,yr) - abschr_pfad(r,gen,yr)) gt 0) - abschr_pfad(r,gen,yr)$((ele_dev(gen,r,yr) - abschr_pfad(r,gen,yr)) gt 0) + 0$((ele_dev(gen,r,yr) - abschr_pfad(r,gen,yr)) le 0));

*ELEx.lo(gen,r)$(ele_dev_switch(gen,r) AND times_coupling AND ELEx.up(gen,r) ) = 0.95 * (abschr_pfad(r,gen,yr)$((ele_dev(gen,r,yr) - abschr_pfad(r,gen,yr)) gt 0) + ele_dev(gen,r,yr)$((ele_dev(gen,r,yr) - abschr_pfad(r,gen,yr)) le 0)) ;
ELEn.lo(gen,r)$(ele_dev_switch(gen,r) AND times_coupling AND NOT (mWIND(gen) OR msolar(gen) OR phydro(gen)) ) = 0.95 * (ele_dev(gen,r,yr)$((ele_dev(gen,r,yr) - abschr_pfad(r,gen,yr)) gt 0) - abschr_pfad(r,gen,yr)$((ele_dev(gen,r,yr) - abschr_pfad(r,gen,yr)) gt 0) + 0$((ele_dev(gen,r,yr) - abschr_pfad(r,gen,yr)) le 0));
ELEn.lo(gen,r)$(ele_dev_switch(gen,r) AND times_coupling AND (msolar(gen) OR mwind(gen) OR phydro(gen))) = 0.98 * (ele_dev(gen,r,yr)$((ele_dev(gen,r,yr) - abschr_pfad(r,gen,yr)) gt 0) - abschr_pfad(r,gen,yr)$((ele_dev(gen,r,yr) - abschr_pfad(r,gen,yr)) gt 0) + 0$((ele_dev(gen,r,yr) - abschr_pfad(r,gen,yr)) le 0));

ELEn.lo(gen,r)$(ele_dev_switch(gen,r) AND yr2025(yr) AND times_coupling AND NOT (mWIND(gen) OR msolar(gen)) ) = 0.95 * (ele_dev(gen,r,yr)$((ele_dev(gen,r,yr) - abschr_pfad(r,gen,yr)) gt 0) - abschr_pfad(r,gen,yr)$((ele_dev(gen,r,yr) - abschr_pfad(r,gen,yr)) gt 0) + 0$((ele_dev(gen,r,yr) - abschr_pfad(r,gen,yr)) le 0));
ELEn.lo(gen,r)$(ele_dev_switch(gen,r) AND yr2025(yr) AND times_coupling AND (msolar(gen) OR mwind(gen) )) = 0.98 * (ele_dev(gen,r,yr)$((ele_dev(gen,r,yr) - abschr_pfad(r,gen,yr)) gt 0) - abschr_pfad(r,gen,yr)$((ele_dev(gen,r,yr) - abschr_pfad(r,gen,yr)) gt 0) + 0$((ele_dev(gen,r,yr) - abschr_pfad(r,gen,yr)) le 0));
  

ELEx.up(gen,r)$(ele_dev_switch(gen,r) AND times_coupling  AND NOT ele_dev(gen,r,"2011")) = 0;
ELEn.up(gen,r)$(ele_dev_switch(gen,r) AND times_coupling  AND NOT ele_dev(gen,r,"2011")) = 1.1 * ele_dev(gen,r,yr);
ELEn.lo(gen,r)$(ele_dev_switch(gen,r) AND times_coupling  AND NOT ele_dev(gen,r,"2011")) = 0.9 * ele_dev(gen,r,yr);


* ------ case 1 - test times

*ELEx.fx(gen,r)$(ele_dev_switch(gen,r) AND times_coupling ) = abschr_pfad(r,gen,yr)$((ele_dev(gen,r,yr) - abschr_pfad(r,gen,yr)) gt 0) + ele_dev(gen,r,yr)$((ele_dev(gen,r,yr) - abschr_pfad(r,gen,yr)) le 0) ;
*ELEn.fx(gen,r)$(ele_dev_switch(gen,r) AND times_coupling ) = ele_dev(gen,r,yr)$((ele_dev(gen,r,yr) - abschr_pfad(r,gen,yr)) gt 0) - abschr_pfad(r,gen,yr)$((ele_dev(gen,r,yr) - abschr_pfad(r,gen,yr)) gt 0) + 0$((ele_dev(gen,r,yr) - abschr_pfad(r,gen,yr)) le 0);

* ------ case 2 - test times  

*ELEx.fx(gen,r)$(ele_dev_switch(gen,r) AND times_coupling  AND NOT ele_dev(gen,r,"2011")) = 0;
*ELEn.fx(gen,r)$(ele_dev_switch(gen,r) AND times_coupling  AND NOT ele_dev(gen,r,"2011")) = ele_dev(gen,r,yr);
 
cap_input(gen,r)$ele_dev_switch(gen,r) = cap_input0(gen,r)*cost_red(gen,yr);
);

check_nuc = ELEn.UP("bNUC","EUS");
display "check_nuc 5. ",yr, ">>" ,check_nuc;
* ------ 29.10.2018 - commenting the following lines to make it easier to couple the electricityproduciton with times
$ontext
* ------ 5.05.2015
Y.LO(i,r)$(ele(i)        and deu(r)   and yr2015(yr)) = 0.98 * Y.L(i,r) ;
Y.LO(i,r)$(ele(i)        and deu(r)   and yr2020(yr)) = 1.05 * Y.L(i,r) ;
Y.LO(i,r)$(ele(i)        and deu(r)   and yr2025(yr)) = 0.98 * Y.L(i,r) ;
Y.LO(i,r)$(ele(i)        and deu(r)   and yr2030(yr)) = 0.90 * Y.L(i,r) ;
Y.LO(i,r)$(ele(i)        and deu(r)   and yr2025(yr)) = 1.00 * Y.L(i,r) ;
Y.LO(i,r)$(ele(i)        and deu(r)   and yr2030(yr)) = 0.98 * Y.L(i,r) ;


* ------ 3.02.2016 BAW auskommentieren
ylo_on(i,r)$(ele(i) and deu(r) and after(yr)) = 0;
ylo_on(i,r)$(ele(i) and deu(r) and after2035(yr)) = 0;

ylo_par(i,r)$yr2015(yr) = 0.98 ;
ylo_par(i,r)$yr2020(yr) = 0.95 ;
ylo_par(i,r)$yr2025(yr) = 0.85 ;
ylo_par(i,r)$yr2030(yr) = 0.80 ;
ylo_par(i,r)$yr2035(yr) = 0.75 ;
ylo_par(i,r)$yr2040(yr) = 0.70 ;
ylo_par(i,r)$yr2045(yr) = 0.60 ;
ylo_par(i,r)$yr2050(yr) = 0.60 ;
$offtext
* ------ Report yearly ELEn bounds
up_yr(r,gen,yr) = ELEn.UP(gen,r) ;
lo_yr(r,gen,yr) = ELEn.LO(gen,r) ;

* ------ 31.07.2014 Write all ELE bounds into a parameter
eletwhyr("NEW.UP",r,gen,yr)              = ELEn.UP(gen,r) * ele_prod(gen,r)  ;
eletwhyr("NEW.LO",r,gen,yr)              = ELEn.LO(gen,r) * ele_prod(gen,r)  ;
eletwhyr("NEW.UP","EU28",gen,yr)         = sum(r$eu28(r),eletwhyr("NEW.UP",r,gen,yr)) ;
eletwhyr("NEW.LO","EU28",gen,yr)         = sum(r$eu28(r),eletwhyr("NEW.LO",r,gen,yr)) ;
eletwhyr("NEW.UP",r,"total",yr)          = sum(gen,      eletwhyr("NEW.UP",r,gen,yr)) ;
eletwhyr("NEW.LO",r,"total",yr)          = sum(gen,      eletwhyr("NEW.LO",r,gen,yr)) ;
eletwhyr("NEW.UP","EU28","total",yr)     = sum(gen,      eletwhyr("NEW.UP","EU28",gen,yr))   ;
eletwhyr("NEW.LO","EU28","total",yr)     = sum(gen,      eletwhyr("NEW.LO","EU28",gen,yr))   ;
eletwhyr("X.UP",r,gen,yr)                =               Elex.up(gen,r) * ele_prod(gen,r)  ;
eletwhyr("X.UP","EU28",gen,yr)           = sum(r$eu28(r),eletwhyr("X.UP",r,gen,yr)) ;
eletwhyr("X.UP",r,"total",yr)            = sum(gen,      eletwhyr("X.UP",r,gen,yr)) ;
eletwhyr("X.UP","EU28","total",yr)       = sum(gen,      eletwhyr("X.UP","EU28",gen,yr))   ;

* ----- 23.09.2018 - TIMES coupling
*ELE_EX.fx(gen,r)$ele_dev_switch(gen,r) = ele_dev(gen,r,yr);
ele_dev_act(gen,r)$ele_dev_switch(gen,r)=ele_dev(gen,r,yr);
;

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ SCENARIOS (Diss)
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

* ------ REFERENCE 2020

* ----- 03.08.2020 set CO2 pathway for reference scenario 2020
carblim_ets(r)$(eu28(r) and reference_scenario_2020)    = carblim_ets0(r) * co2pfad_ets_eu28_ref('EU28',yr);  // if etstrade = 1

* ------ DISSERTATION Scenarios

* ------ diss_BAU
carblim_ets(r)$(eu28(r) and diss_BAU AND after2020(yr))      = carblim_ets0(r) * CO2_AP5_sce7("EU28","ETS",yr);              // ETS in the EU
carblim(r)$(eu28(r) and diss_BAU AND after2020(yr))          = (carblim0(r) - carblim_ets0(r)) * CO2_AP5_sce7(r,"ESD",yr) ;  // ESD in the EU
carblim(r)$(NOT eu28(r) and diss_BAU AND after2020(yr))      = carblim0(r) * CO2_AP5_world(r,yr) ;                           // national targets outside of the EU

* ------ diss_CAP
carblim_ets(r)$(eu28(r) and diss_CAP AND after2020(yr))      = carblim_ets0(r) * CO2_AP5_sce7("EU28","ETS",yr);              // ETS in the EU
carblim(r)$(eu28(r) and diss_CAP AND after2020(yr))          = (carblim0(r) - carblim_ets0(r)) * CO2_AP5_sce7(r,"ESD",yr) ;  // ESD in the EU
carblim(r)$(NOT eu28(r) and diss_CAP AND after2020(yr))      = carblim0(r) * CO2_AP5_world(r,yr) ;                           // national targets outside of the EU

* ------ diss_VAT
carblim_ets(r)$(eu28(r) and diss_VAT AND after2020(yr))      = carblim_ets0(r) * CO2_AP5_sce7("EU28","ETS",yr);              // ETS in the EU
carblim(r)$(eu28(r) and diss_VAT AND after2020(yr))          = (carblim0(r) - carblim_ets0(r)) * CO2_AP5_sce7(r,"ESD",yr) ;  // ESD in the EU
carblim(r)$(NOT eu28(r) and diss_VAT AND after2020(yr))      = carblim0(r) * CO2_AP5_world(r,yr) ;                           // national targets outside of the EU

* ------ diss_LAB
carblim_ets(r)$(eu28(r) and diss_LAB AND after2020(yr))      = carblim_ets0(r) * CO2_AP5_sce7("EU28","ETS",yr);              // ETS in the EU
carblim(r)$(eu28(r) and diss_LAB AND after2020(yr))          = (carblim0(r) - carblim_ets0(r)) * CO2_AP5_sce7(r,"ESD",yr) ;  // ESD in the EU
carblim(r)$(NOT eu28(r) and diss_LAB AND after2020(yr))      = carblim0(r) * CO2_AP5_world(r,yr) ;                           // national targets outside of the EU

* ------ diss_inv_payment
carblim_ets(r)$(eu28(r) and diss_inv_payment AND after2020(yr))      = carblim_ets0(r) * CO2_AP5_sce7("EU28","ETS",yr);              // ETS in the EU
carblim(r)$(eu28(r) and diss_inv_payment AND after2020(yr))          = (carblim0(r) - carblim_ets0(r)) * CO2_AP5_sce7(r,"ESD",yr) ;  // ESD in the EU
carblim(r)$(NOT eu28(r) and diss_inv_payment AND after2020(yr))      = carblim0(r) * CO2_AP5_world(r,yr) ;                           // national targets outside of the EU



* ------ 22.07.2014 Set carblim_ets equal to co2pfad_ets
*co2pfad_ets_eu28_refcarblim_ets(r)$(eu28(r) and etstrade)    = carblim_ets0(r) * co2pfad_ets(r,yr);  // if etstrade = 1
* ------ 15.02.2016 Set carblim_ets equal to co2pfad_ets
*carblim_ets(r)$(eu28(r) and hhets)       = carblim_ets0(r) * co2pfad_ets(r,yr);  // if hhets = 1
* ------ 2.02.2016 EU-wide, cross-sectoral ETS
*carblim(r)$(eu28(r) and eutrade)         = carblim0(r) * co2pfad_ets(r,yr) ;     // if eutrade = 1
* ------ 2.02.2016 Regionale, sektor?bergreifende CO2-Reduktionspflicht ohne ETS
*notrad(r)$(eu28(r) and notrade)          = yes ;                                 // if notrade = 1
*carblim(r)$(eu28(r) and notrad(r))       = carblim0(r) * co2pfad_ets(r,yr) ;     // if notrade = 1

*-------22.08.2017 regional, cross-sectoral CO2-reduction obligation in RoW
*carblim(r)$(not eu28(r) and row_notrade) = carblim0(r) * co2pfad_ets(r,yr); // if row_notrade = 1

*------28.09.2017
*carblim_sec(sec,"DEU")$DEU_sec = carblim_sec0(sec,"DEU") * co2pfad_DEU(sec,yr);


* ----- 26.01.2018
*carblim_deu$detrade = carblim_deu0 * co2pfad_ets_row("DEU",yr);
carblim_deu_yr(yr)$detrade = carblim_deu;

carblim(r)$(BTA_coa(r) and bta_test) = carblim(r) * 0.8;

* ------ Carbon Regimes for ETS + Non-ETS sectors ------------------------------


* ------ CO2 Tax ---------------------------------------------------------------




* ---------------------------------------------------------------------- *
* ###### SOLVE STATEMENT 2                                                       // # LOOP-solve #
* ---------------------------------------------------------------------- *

* ------ Set iteration limit to a reasonable size, e.g. 2500:
NEWAGE.iterlim =  5000;
NEWAGE.iterlim = 100000;
* ------ INCLUDE and SOLVE
$include NEWAGE.gen
SOLVE    NEWAGE    using   MCP;

Pcarbon_tax_yr(r,yr)$(carbon_tax_de AND deu(r)) = Pcarbon_tax.l(r);
carb_tax_m_yr(r,yr)$(carbon_tax_de AND deu(r)) = carb_tax_m.l(r);

elen_up_yr(gen,r,yr) = ELEn.up(gen,r);
elex_up_yr(gen,r,yr) = ELEx.up(gen,r);

LOOP (gen,

LOOP (r,  

if (ELEn.up(gen,r) = ELEn.l(gen,r),
    delta_ele("elen_UP_flag",gen,r,yr) = ELEn.l(gen,r);
    );

if (ELEn.lo(gen,r) = ELEn.l(gen,r),
    delta_ele("elen_LO_flag",gen,r,yr) = ELEn.l(gen,r);
    );

if (ELEx.up(gen,r) = ELEx.l(gen,r),
    delta_ele("elex_UP_flag",gen,r,yr) = ELEx.l(gen,r);
    );

if (ELEx.lo(gen,r) = ELEx.l(gen,r),
    delta_ele("elex_LO_flag",gen,r,yr) = ELEx.l(gen,r);
    );


);

);

delta_ele("elen_UP",gen,r,yr) = ELEn.up(gen,r) - ELEn.l(gen,r);
delta_ele("elen_LO",gen,r,yr) = ELEn.lo(gen,r) - ELEn.l(gen,r);
delta_ele("elex_UP",gen,r,yr) = ELEx.up(gen,r) - ELEx.l(gen,r);
delta_ele("elex_LO",gen,r,yr) = ELEx.lo(gen,r) - ELEx.l(gen,r); 


*$exit
*EXITsolve2

*---------------------------------------------------------------------------
*---------- 25.04.2019 - demands and endowments from disaggregted households
*---------------------------------------------------------------------------


RA_hh_par_yr("PC",r,hh,yr)$HH_DISAG(r) =    -VD_PC_hh.L(hh,r) * PC_hh.L(hh,r);
RA_hh_par_yr("PINV",r,hh,yr)$HH_DISAG(r) =  -VD_PINV_hh.L(hh,r) * PINV.L(r);
RA_hh_par_yr("PITAX",r,hh,yr)$HH_DISAG(r) = -VD_PITAX_hh.L(hh,r) * PITAX.L(r);

RA_hh_par_yr("PSKL",r,hh,yr)$HH_DISAG(r) = ((evoa("skl",r) / (1-ursk0(r))) * hh_skl_share(r,hh)) * (1 - URSK.L(r)) * PSKL.L(r);
RA_hh_par_yr("PUSK",r,hh,yr)$HH_DISAG(r) = ((evoa("USK",r) / (1-urun0(r))) * hh_usk_share(r,hh)) * (1 - URUN.L(r)) * PUSK.L(r);
RA_hh_par_yr("RKR",r,hh,yr)$HH_DISAG(r)  = (evoa("cap",r) * hh_rkr_share(r,hh)) * RKR.L(r);
RA_hh_par_yr("RKX_ELE",r,hh,yr)$HH_DISAG(r)  = (sum(gen$ks_x(gen,r),ks_x(gen,r) * hh_rkx_ele_share(r,hh) * RKX_ELE.L(gen,r))) ;
RA_hh_par_yr("tax_income",r,hh,yr)$HH_DISAG(r)  = (tax_gov_2_hh(r) * tax_delta(r) * hh_tax_in_share(r,hh)) * PTAX.L(r);

RA_hh_par_yr("CO2_payments",r,hh,yr)$(HH_DISAG(r) AND per_capita_dis)    =   (PCO2.l(r)         * (carblim(r)/5)) + 
                                                                             (PCO2W.l           * (carblim(r)/5)) +
                                                                             (PCO2_ETS.l        * (carblim_ets(r)/5)) +
                                                                             (PCO2_NETS.l       * (carblim(r)/5)) + 
                                                                             (PCO2_NETSr.l(r)   * (carblim(r)/5));

RA_hh_par_yr("CO2_payments",r,hh,yr)$(HH_DISAG(r) AND per_capita_dis_NETSr)    =   (PCO2_NETSr.l(r)   * (carblim(r)/5));

RA_hh_par_yr("CO2_payments",r,hh,yr)$(HH_DISAG(r) AND inverse_co2_pay)    =   (PCO2_NETSr.l(r)   * (carblim(r)/5) * CO2_inv_pay.L(hh,r));


RA_hh_par_yr("total_income",r,hh,yr)$HH_DISAG(r) =  RA_hh_par_yr("PSKL",r,hh,yr) + RA_hh_par_yr("PUSK",r,hh,yr)  + RA_hh_par_yr("RKR",r,hh,yr)  + RA_hh_par_yr("RKX_ELE",r,hh,yr)  + RA_hh_par_yr("tax_income",r,hh,yr) + RA_hh_par_yr("CO2_payments",r,hh,yr);
RA_hh_par_yr("net_income",r,hh,yr)$HH_DISAG(r) = RA_hh_par_yr("total_income",r,hh,yr) + RA_hh_par_yr("PITAX",r,hh,yr);

RA_hh_par_yr("chk_sum",r,hh,yr)$HH_DISAG(r) = RA_hh_par_yr("PC",r,hh,yr) + RA_hh_par_yr("PINV",r,hh,yr)
                                                + RA_hh_par_yr("PITAX",r,hh,yr) + RA_hh_par_yr("PSKL",r,hh,yr)
                                                + RA_hh_par_yr("PUSK",r,hh,yr) + RA_hh_par_yr("RKR",r,hh,yr)
                                                + RA_hh_par_yr("RKX_ELE",r,hh,yr) + RA_hh_par_yr("tax_income",r,hh,yr)
                                                + RA_hh_par_yr("CO2_payments",r,hh,yr);


* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ 14.10.2014 Reporting the two Elements of RECURSIVE Capital DYNAMICS
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

evoa1("cap",r,yr)$(ord(yr) = 1)  = ((1-dep(r))**4) * evoa("cap",r) ;
evoa1("cap",r,yr)$(ord(yr) > 1)  = ((1-dep(r))**5) * evoa("cap",r) ;
evoa11("cap",r,yr)$(ord(yr) = 1) = ((1-dep(r))**4) * evoa0("cap",r) ;
evoa11("cap",r,yr)$(ord(yr) > 1) = ((1-dep(r))**5) * evoa11("cap",r,yr-1) ;
evoa2("cap",r,yr)$(ord(yr) = 1)  = VINV_PINV.L(r) * PINV.L(r) * (4*brk0(r)) ;
evoa2("cap",r,yr)$(ord(yr) > 1)  = VINV_PINV.L(r) * PINV.L(r) * (5*brk0(r)) ;

* ------ RECURSIVE Capital DYNAMICS (growth / endowment driver) ---------------- // # CAPITAL dynamics #
* ------ Capital (2011-2015: 4 years, 2015-2020: 5 years) ORIGINAL
evoa("cap",r)$(ord(yr) = 1)  = ((1-dep(r))**4) * evoa("cap",r) + VINV_PINV.L(r) * PINV.L(r) * (4*brk0(r))  ;  // 2011
evoa("cap",r)$(ord(yr) > 1)  = ((1-dep(r))**5) * evoa("cap",r) + VINV_PINV.L(r) * PINV.L(r) * (5*brk0(r))  ;  // 2015 <


* ------ 03.11.2014 10-year-milestones
* ------ Capital (2007-2010: 3 years, 2010-2015: 5 years)
*evoa("cap",r)$(ord(yr) = 1)  = ((1-dep(r))**3)  * evoa("cap",r) + VINV_PINV.L(r) * PINV.L(r) * (3*brk0(r))   ;  // 2007-->2010
*evoa("cap",r)$(ord(yr) > 1)  = ((1-dep(r))**5)  * evoa("cap",r) + VINV_PINV.L(r) * PINV.L(r) * (5*brk0(r))   ;  // 2010-->2015, 2015-->2020
*evoa("cap",r)$(ord(yr) > 3)  = ((1-dep(r))**10) * evoa("cap",r) + VINV_PINV.L(r) * PINV.L(r) * (10*brk0(r))  ;  // 2020-2050
* ------ 3.11.2014 10-year-milestones (oben)

*    tax_delta(r)$HH_DISAG(r) = (evoa("cap",r) + evoa("skl",r) + evoa("usk", r))/(evoa0("cap",r) + evoa0("skl",r) + evoa0("usk", r) );
    tax_delta(r)$HH_DISAG(r) = evoa("cap",r)/evoa0("cap",r);
*-------30.10.2017 R_supply_yr
	r_supply_yr(i,r,yr)$pricetarget(i,r) = R_SUPPLY.L(i,r);
*-------30.10.2017 end

* ------ 1.07.2015 SC1: Klimaschäden in BAW und Welt --> Gleichmäßige Beeinträchtigung des Produktivkapitals um 1,3 % Welt-BIP-Schaden p.a. zu erreichen.
* ------ evoa wird NACH dem SOLVE aktualisiert

evoa("cap",r)$(sc1 and yr2011(yr))       = 0.982 * evoa("cap",r);      // for 2015
evoa("cap",r)$(sc1 and yr2015(yr))       = 0.991 * evoa("cap",r);      // for 2020
evoa("cap",r)$(sc1 and yr2020(yr))       = 0.988 * evoa("cap",r);      // for 2025
evoa("cap",r)$(sc1 and yr2025(yr))       = 0.990 * evoa("cap",r);      // for 2030
evoa("cap",r)$(sc1 and yr2030(yr))       = 0.990 * evoa("cap",r);      // for 2035
evoa("cap",r)$(sc1 and yr2035(yr))       = 0.991 * evoa("cap",r);      // for 2040
evoa("cap",r)$(sc1 and yr2040(yr))       = 0.991 * evoa("cap",r);      // for 2045
evoa("cap",r)$(sc1 and yr2045(yr))       = 0.993 * evoa("cap",r);      // for 2050
* ------ 1.07.2015 Textmarke SC1

$ontext
* ---- before changing years 24.08.2016
evoa("cap",r)$(sc1 and yr2007(yr))       = 0.955 * evoa("cap",r);      // for 2010
evoa("cap",r)$(sc1 and yr2010(yr))       = 0.982 * evoa("cap",r);      // for 2015
evoa("cap",r)$(sc1 and yr2015(yr))       = 0.991 * evoa("cap",r);      // for 2020
evoa("cap",r)$(sc1 and yr2020(yr))       = 0.988 * evoa("cap",r);      // for 2025
evoa("cap",r)$(sc1 and yr2025(yr))       = 0.990 * evoa("cap",r);      // for 2030
evoa("cap",r)$(sc1 and yr2030(yr))       = 0.990 * evoa("cap",r);      // for 2035
evoa("cap",r)$(sc1 and yr2035(yr))       = 0.991 * evoa("cap",r);      // for 2040
evoa("cap",r)$(sc1 and yr2040(yr))       = 0.991 * evoa("cap",r);      // for 2045
evoa("cap",r)$(sc1 and yr2045(yr))       = 0.993 * evoa("cap",r);      // for 2050
$offtext

* ------ 1.07.2015 Capital intensity
VA_PYxm.L(i,r,r)         = 0;
capgdp_yr(r,yr)$(NOT HH_DISAG(r))          =                       evoa("cap",r)   / (VC_PC.L(r) + VINV_PINV.L(r) + sum(i, sum(s, VA_PYxm.L(i,r,s) - VA_PYxm.L(i,s,r))));

capgdp_yr(r,yr)$(HH_DISAG(r))              =                       evoa("cap",r)   / (sum(hh,VC_hh_PC.L(HH,R)) + VC_gov_PC.L(r) + VINV_PINV.L(r) + sum(i, sum(s, VA_PYxm.L(i,r,s) - VA_PYxm.L(i,s,r))));

*capgdp_yr("BRD",yr)      = sum(r$bawdeu(r),      evoa("cap",r))   / sum(r$bawdeu(r),(VC_PC.L(r) + VINV_PINV.L(r) + sum(i, sum(s, VA_PYxm.L(i,r,s) - VA_PYxm.L(i,s,r)))));
capgdp_yr("EU28",yr)     = sum(r$eu28(r),        evoa("cap",r))   / (sum(r$(eu28(r) and not HH_DISAG(r)), (VC_PC.L(r) + VINV_PINV.L(r) + sum(i, sum(s, VA_PYxm.L(i,r,s) - VA_PYxm.L(i,s,r))))) +
                                                                     sum(r$(eu28(r) and HH_DISAG(r)), (sum(hh,VC_hh_PC.L(HH,R)) + VC_gov_PC.L(r) + VINV_PINV.L(r) + sum(i, sum(s, VA_PYxm.L(i,r,s) - VA_PYxm.L(i,s,r)))))
                                                                        );


capgdp_yr("World",yr)    = sum(r,                evoa("cap",r))   / (sum(r$(not HH_DISAG(r)), (VC_PC.L(r) + VINV_PINV.L(r) + sum(i, sum(s, VA_PYxm.L(i,r,s) - VA_PYxm.L(i,s,r))))) +
                                                                     sum(r$(HH_DISAG(r)), (sum(hh,VC_hh_PC.L(HH,R)) + VC_gov_PC.L(r) + VINV_PINV.L(r) + sum(i, sum(s, VA_PYxm.L(i,r,s) - VA_PYxm.L(i,s,r)))))
                                                                        );

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ Cars and buildings REPORTING
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ Updating / Dynamics of cars and buildings stocks ----------------------
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

* ------ CAR STOCK DYNAMICS ----------------------------------------------------

* ------ 14.09.2015 (Diss)
**ssd(r,ct,cf)$yr2007(yr) = ssd(r,ct,cf) * (1-0.15)**3;
**ssd(r,ct,cf)$after(yr)  = ssd(r,ct,cf) * (1-0.15)**5;

* ------ 30.12.2015
**ssd(r,ct,cf)$yr2007(yr) = ssd(r,ct,cf) * (1+0.015)**3;
**ssd(r,ct,cf)$after(yr)  = ssd(r,ct,cf) * (1+0.015)**5;
**ssd(r,ct,cf)$yr2007(yr) = ssd(r,ct,cf) * (1-0.20)**3 + VNC_NEWCAR.L(r,ct,cf) * PNEWCAR.L(r,ct,cf) * 1.5;
**ssd(r,ct,cf)$after(yr)  = ssd(r,ct,cf) * (1-0.20)**5 + VNC_NEWCAR.L(r,ct,cf) * PNEWCAR.L(r,ct,cf) * 3;
**ssd0(r,ct,cf) = ssd(r,ct,cf);
* ------ 30.12.2015 (oben)
**NEWCAR.UP(r,ct,cf) = NEWCAR.L(r,ct,cf) * 4 ;
**NEWCAR.LO(r,ct,cf) = NEWCAR.L(r,ct,cf) * 0.1 ;
**PXTHOUSE.UP(r,bt,bf) = 1 ;

* ------ 23.01.2016
$ontext
*ssd(r,ct,cf)$yr2007(yr) = ssd(r,ct,cf) * (1-0.20)**3 + VNC_NEWCAR.L(r,ct,cf) * PNEWCAR.L(r,ct,cf) * 1.5;
*ssd(r,ct,cf)$after(yr)  = ssd(r,ct,cf) * (1-0.20)**5 + VNC_NEWCAR.L(r,ct,cf) * PNEWCAR.L(r,ct,cf) * 3;
ssd(r,ct,cf)$yr2011(yr) = ssd(r,ct,cf) * (1-0.15)**3 + ssd0(r,ct,cf) * NEWCAR.L(r,ct,cf) * 3;
ssd(r,ct,cf)$(yr2011(yr) and sum((cct,ccf), ssd(r,cct,ccf))) = (ssd(r,ct,cf) / sum((cct,ccf), ssd(r,cct,ccf))) * sum((cct,ccf), ssd0(r,cct,ccf)) ;
* ------ Die Werte werden in Summe gleich gehalten, nur anteilsmäßig anders verteilt
ssd(r,ct,cf)$after(yr)  = ssd(r,ct,cf) * (1-0.15)**5 + ssd0(r,ct,cf) * NEWCAR.L(r,ct,cf) * 5; display ssd;
ssd(r,ct,cf)$(after(yr) and sum((cct,ccf), ssd(r,cct,ccf)))  = (ssd(r,ct,cf) / sum((cct,ccf), ssd(r,cct,ccf))) * sum((cct,ccf), ssd0(r,cct,ccf)) ; display ssd;
$offtext

*$ontext

*ssd(r,ct,cf)$after(yr) = ssd(r,ct,cf) * (1-0.15)**5 + newcar_yr(r,ct,cf,yr)/newcar_yr(r,ct,cf,yr-1) * cars(r,ct,cf,"new") *
*ssd(r,ct,cf)$after(yr)    = ssd(r,ct,cf) * (1-0.05)**3 + anz_car(r,ct,cf,"new",yr) * ssd0price(r,ct,cf) * pcar0(r,ct,cf) * 3; display ssd;
*ssd(r,ct,cf)$after2010(yr)= ssd(r,ct,cf) * (1-0.05)**5 + anz_car(r,ct,cf,"new",yr) * ssd0price(r,ct,cf) * pcar0(r,ct,cf) * 5; display ssd;

* ----- LOOK HERE to CHECK YEARS (DOWN)


* ------ 7.02.2016 auskommentiert (unten)
$ontext
bsdyr(r,bt,bf,"2015")$yr2011(yr) = anz_qm(r,bt,bf,yr)$new(bt) * qmcostc_disc(r,bt,bf) ;
bsdyr(r,bt,bf,"2020")$yr2015(yr) = anz_qm(r,bt,bf,yr)$new(bt) * qmcostc_disc(r,bt,bf) ;
bsdyr(r,bt,bf,"2025")$yr2020(yr) = anz_qm(r,bt,bf,yr)$new(bt) * qmcostc_disc(r,bt,bf) ;
bsdyr(r,bt,bf,"2030")$yr2025(yr) = anz_qm(r,bt,bf,yr)$new(bt) * qmcostc_disc(r,bt,bf) ;

bsdyr(r,bt,bf,"2015")$after(yr) = bsdyr(r,bt,bf,"2015") * (1 - 0.1)**5 ;
bsdyr(r,bt,bf,"2020")$after2015(yr) = bsdyr(r,bt,bf,"2020") * (1 - 0.1)**5 ;
bsdyr(r,bt,bf,"2025")$after2020(yr) = bsdyr(r,bt,bf,"2025") * (1 - 0.1)**5 ;
bsdyr(r,bt,bf,"2030")$after2025(yr) = bsdyr(r,bt,bf,"2030") * (1 - 0.1)**5 ;

* ------ 23.01.2016
*bsdyr(r,bt,bf,"2010")$yr2007(yr) = bsd0(r,bt,bf) * NEWHOUSE.L(r,bt,bf) * 3 ;
*bsdyr(r,bt,bf,"2015")$yr2010(yr) = bsd0(r,bt,bf) * NEWHOUSE.L(r,bt,bf) * 5 ;
*bsdyr(r,bt,bf,"2020")$yr2015(yr) = bsd0(r,bt,bf) * NEWHOUSE.L(r,bt,bf) * 5 ;
*bsdyr(r,bt,bf,"2025")$yr2020(yr) = bsd0(r,bt,bf) * NEWHOUSE.L(r,bt,bf) * 5 ;
*bsdyr(r,bt,bf,"2030")$yr2025(yr) = bsd0(r,bt,bf) * NEWHOUSE.L(r,bt,bf) * 5 ;
* ------ 23.01.2016 (oben)

*bsd_sum(r,yr)$yr2007(yr) = sum((bt,bf), VHEATF_PXTHOUSE.L(r,bt,bf) * PXTHOUSE.L(r,bt,bf) + VSTOCK_HXH07.L(r,bt,bf) * PXTHOUSE07.L(r,bt,bf)) ;
*bsd_sum(r,yr)$yr2010(yr) = sum((bt,bf), VHEATF_PXTHOUSE.L(r,bt,bf) * PXTHOUSE.L(r,bt,bf) + VSTOCK_HXH10.L(r,bt,bf) * PXTHOUSE10.L(r,bt,bf)) ;
*bsd_sum(r,yr)$yr2015(yr) = sum((bt,bf), VHEATF_PXTHOUSE.L(r,bt,bf) * PXTHOUSE.L(r,bt,bf) + VSTOCK_HXH15.L(r,bt,bf) * PXTHOUSE15.L(r,bt,bf)) ;
*bsd_sum(r,yr)$yr2020(yr) = sum((bt,bf), VHEATF_PXTHOUSE.L(r,bt,bf) * PXTHOUSE.L(r,bt,bf) + VSTOCK_HXH20.L(r,bt,bf) * PXTHOUSE20.L(r,bt,bf)) ;
*bsd_sum(r,yr)$yr2025(yr) = sum((bt,bf), VHEATF_PXTHOUSE.L(r,bt,bf) * PXTHOUSE.L(r,bt,bf) + VSTOCK_HXH25.L(r,bt,bf) * PXTHOUSE25.L(r,bt,bf)) ;
*bsd_sum(r,yr)$yr2030(yr) = sum((bt,bf), VHEATF_PXTHOUSE.L(r,bt,bf) * PXTHOUSE.L(r,bt,bf) + VSTOCK_HXH30.L(r,bt,bf) * PXTHOUSE30.L(r,bt,bf)) ;

bsd_sum(r,bt,bf,yr)$after(yr) = bsd(r,bt,bf) +bsdyr(r,bt,bf,"2015")+bsdyr(r,bt,bf,"2020")+bsdyr(r,bt,bf,"2025")+bsdyr(r,bt,bf,"2030") ;

bsdyr0(r,bt,bf,"2011") = bsdyr(r,bt,bf,"2011") ;
bsdyr0(r,bt,bf,"2015") = bsdyr(r,bt,bf,"2015") ;
bsdyr0(r,bt,bf,"2020") = bsdyr(r,bt,bf,"2020") ;
bsdyr0(r,bt,bf,"2025") = bsdyr(r,bt,bf,"2025") ;
bsdyr0(r,bt,bf,"2030") = bsdyr(r,bt,bf,"2030") ;
$offtext

* ----- LOOK HERE to CHECK YEARS (UP)

* ------ Textmarke bsd dynamics
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ in-LOOP-REPORTING for dynamic (yr) parameters -------------------------
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

* ------ 05.06.2014 Reporting STATUS-Parameter ---------------------------------
* ------ http://www.gams.de/mccarl/mccarlhtml/model_attributes_mainly_used_a.htm
status("solvestat",yr) = NEWAGE.solvestat;
status("modelstat",yr) = NEWAGE.modelstat;
status("objval",yr) = round(NEWAGE.objval,6);
status("numredef",yr) = NEWAGE.numredef;
* ------ 11.05.2015
status("iterUsd (Thd.)",yr) = NEWAGE.iterUsd / 1000;

display STATUS;



* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ 11.05.2015 Calculate ZPFs and input shares in Y and C
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

* ------ ZPF consumption in model
* ------ 06.02.2013 - (ZfES2, unten)
* ------ Epro Ra enthält die Schalter eupol=1 und septrade=1 --> dadurch gehen PcarbW und PcarbEU_NET ins Modell ein
zpf_c_yr(r,yr)= round(
*        + C.L(r) * ct0(r) * PC.L(r)
         + VC_PC.L(r) * PC.L(r)
         - sum(i, VC_PA.L(i,r) * PA.L(i,r) * pc0(i,r))

         - VC_CO2.L(r)$notrad(r)         * PCO2.L(r)
         - VC_CO2W.L(r)$pco2w_r(r)$worldtrade * PCO2W.L
         - VC_CO2W.L(r)$eutrade          * PCO2W.L
         - VC_CO2W.L(r)$worldtrade2      * PCO2W.L
         - VC_CO2_NETS.L(r)$netstrade    * PCO2_NETS.L
         - VC_CO2_NETSr.L(r)$netstrade_r * PCO2_NETSr.L(r)
, rd);

*ct0(r) = vom("c",r) + vom("g",r) ??????
*sharec_yr(r,i,yr)              = ( VC_PA.L(i,r)* PA.L(i,r) * pc0(i,r))   / (C.L(r) * ct0(r) * PC.L(r));
*sharec_yr(r,"ESD",yr)          = ( VC_ESD.L(r) * PESD.L(r) )             / (C.L(r) * ct0(r) * PC.L(r));
*sharec_yr(r,"PCO2",yr)         = ( VC_CO2W.L(r) * PCO2.L(r) )            / (C.L(r) * ct0(r) * PC.L(r));
*sharec_yr(r,"PCO2W",yr)        = ( VC_CO2W.L(r) * PCO2W.L )              / (C.L(r) * ct0(r) * PC.L(r));
*sharec_yr(r,"PCO2_NETS",yr)    = ( VC_CO2_NETS.L(r) * PCO2_NETS.L )      / (C.L(r) * ct0(r) * PC.L(r));

*sharec_yr(r,i,yr)              = ( VC_PA.L(i,r)* PA.L(i,r) * pc0(i,r))   / (VC_PC.L(r) * PC.L(r));

sharec_yr(r,"PCO2",yr)$(NOT HH_DISAG(r))         = ( VC_CO2.L(r) * PCO2.L(r) )            / (VC_PC.L(r) * PC.L(r));
sharec_yr(r,"PCO2W",yr)$(NOT HH_DISAG(r))        = ( VC_CO2W.L(r) * PCO2W.L )              / (VC_PC.L(r) * PC.L(r));
sharec_yr(r,"PCO2_NETS",yr)$(NOT HH_DISAG(r))    = ( VC_CO2_NETS.L(r) * PCO2_NETS.L )      / (VC_PC.L(r) * PC.L(r));
sharec_yr(r,"PCO2_NETSr",yr)$(NOT HH_DISAG(r))   = ( VC_CO2_NETSr.L(r) * PCO2_NETSr.L(r) ) / (VC_PC.L(r) * PC.L(r));
sharec_yr(r,"co2_tax",yr)$(DEU(r) AND carbon_tax_de AND NOT HH_DISAG(r))   = ( VC_CO2_tax.L(r) * Pcarbon_tax.L(r) ) / (VC_PC.L(r) * PC.L(r));

sharec_yr(r,"PCO2",yr)$(HH_DISAG(r))         = (( sum(hh,VC_hh_CO2.L(hh,r)) * PCO2.L(r) ) + ( VC_gov_CO2.L(r) * PCO2.L(r) )) / (sum(hh,(VC_hh_PC.L(hh,r) * PC_hh.L(hh,r))) + (VC_gov_PC.L(r) * PC_gov.L(r)));
sharec_yr(r,"PCO2W",yr)$(HH_DISAG(r))        = (( sum(hh,VC_hh_CO2W.L(hh,r)) * PCO2W.L )  + ( VC_gov_CO2W.L(r) * PCO2W.L ))  / (sum(hh,(VC_hh_PC.L(hh,r) * PC_hh.L(hh,r))) + (VC_gov_PC.L(r) * PC_gov.L(r)));
sharec_yr(r,"PCO2_NETS",yr)$(HH_DISAG(r))    = (( sum(hh,VC_hh_CO2_NETS.L(hh,r)) * PCO2_NETS.L ) + ( VC_gov_CO2_NETS.L(r) * PCO2_NETS.L )) /
                                               (sum(hh,(VC_hh_PC.L(hh,r) * PC_hh.L(hh,r))) + (VC_gov_PC.L(r) * PC_gov.L(r)));
sharec_yr(r,"PCO2_NETSr",yr)$(HH_DISAG(r))   = (( sum(hh,VC_hh_CO2_NETSr.L(hh,r)) * PCO2_NETSr.L(r) ) + ( VC_gov_CO2_NETSr.L(r) * PCO2_NETSr.L(r) )) / 
                                               (sum(hh,(VC_hh_PC.L(hh,r) * PC_hh.L(hh,r))) + (VC_gov_PC.L(r) * PC_gov.L(r)));

sharec_yr(r,"PCO2_inv_pay",yr)$(HH_DISAG(r))   = (( sum(hh,VC_hh_CO2_inv_pay.L(hh,r) * PCO2_inv_pay.L(hh,r) )) + ( VC_gov_CO2_NETSr.L(r) * PCO2_NETSr.L(r) )) / 
                                               (sum(hh,(VC_hh_PC.L(hh,r) * PC_hh.L(hh,r))) + (VC_gov_PC.L(r) * PC_gov.L(r)));

sharec_yr(r,"co2_tax",yr)$(HH_DISAG(r))   = sum(hh,VC_hh_CO2_tax.L(hh,r) * Pcarbon_tax.L(r) ) / 
                                               sum(hh,(VC_hh_PC.L(hh,r) * PC_hh.L(hh,r)));


* ------ Check input shares --> shareCt_yr must be zero
shareCt_yr(r,yr) = round(
                 + sum(i, sharec_yr(r,i,yr))
                 + sharec_yr(r,"PCO2",yr) + sharec_yr(r,"PCO2W",yr) + sharec_yr(r,"PCO2_NETS",yr) + sharec_yr(r,"PCO2_NETSr",yr)
                 + sharec_yr(r,"PCO2_inv_pay",yr)
                 - 1,    rd);

* ------------------------------------------------------------------------------
constr(i,r) = no;
constr(i,r) = yes;
constr(i,r)$ele(i) = no; display constr;
* ------------------------------------------------------------------------------

* ------ ZPF production in model
zpf_y_yr(i,r,yr)$constr(i,r) = round(
*        + Y.L(i,r) * vom(i,r)  * PY.L(i,r)* py0(i,r)        // identisch zur Zeile drunter??
         + VY_PY.L(i,r) * PY.L(i,r) * py0(i,r)
*         + VY_PY.L(i,r) * PY.L(i,r) * (1-ty(i,r))
         - sum(j, VY_PA.L(j,i,r)* PA.L(j,r)* (1+ti(j,i,r)))
         - VY_PSKL.L(i,r) * PSKL.L(r) - VY_PUSK.L(i,r)* PUSK.L(r)
         - VY_RKR.L(i,r)  * RKR.L(R)  - VY_PR.L(I,R)  * PR.L(i,R)
         - VY_CO2.L(i,r)$notrad(r)         * PCO2.L(r)
         - VY_CO2W.L(i,r)$pco2w_r(r)$worldtrade * PCO2W.L
         - VY_CO2W.L(i,r)$eutrade          * PCO2W.L
         - VY_CO2W.L(i,r)$worldtrade2      * PCO2W.L
         - VY_CO2_NETS.L(i,r)$netstrade    * PCO2_NETS.L
         - VY_CO2_NETSr.L(i,r)$netstrade_r * PCO2_NETSr.L(r)
         - VY_CO2_ETS.L(i,r)$etstrade      * PCO2_ETS.L        // works
, rd);

* ------------------------------------------------------------------------------
constr(i,r) = no;
constr(i,r)$(VY_PY.L(i,r) * PY.L(i,r) * py0(i,r)) = yes;
constr(i,r)$ele(i) = no; display constr;
* ------------------------------------------------------------------------------

sharey_yr(r,j,i,yr)$constr(i,r)        = (VY_PA.L(j,i,r)* PA.L(j,r)* (1+ti(j,i,r)))     / (VY_PY.L(i,r) * PY.L(i,r)* py0(i,r));
sharey_yr(r,"SKL",i,yr)$constr(i,r)    = (VY_PSKL.L(i,r)* PSKL.L(r)) / (VY_PY.L(i,r) * PY.L(i,r)* py0(i,r));
sharey_yr(r,"USK",i,yr)$constr(i,r)    = (VY_PUSK.L(i,r)* PUSK.L(r)) / (VY_PY.L(i,r) * PY.L(i,r)* py0(i,r));
sharey_yr(r,"CAP",i,yr)$constr(i,r)    = (VY_RKR.L(i,r) * RKR.L(r))  / (VY_PY.L(i,r) * PY.L(i,r)* py0(i,r));
sharey_yr(r,"RES",i,yr)$constr(i,r)    = (VY_PR.L(i,r)  * PR.L(i,r)) / (VY_PY.L(i,r) * PY.L(i,r)* py0(i,r));
sharey_yr(r,"PCO2",i,yr)$constr(i,r)   = (VY_CO2.L(i,r) * PCO2.L(r)) / (VY_PY.L(i,r) * PY.L(i,r)* py0(i,r));
sharey_yr(r,"PCO2W",i,yr)$constr(i,r)  = (VY_CO2W.L(i,r)* PCO2W.L)   / (VY_PY.L(i,r) * PY.L(i,r)* py0(i,r));
sharey_yr(r,"PCO2_ETS",i,yr)$constr(i,r)  = (VY_CO2_ETS.L(i,r) * PCO2_ETS.L)  / (VY_PY.L(i,r) * PY.L(i,r)* py0(i,r)); // works
sharey_yr(r,"PCO2_NETS",i,yr)$constr(i,r) = (VY_CO2_NETS.L(i,r)* PCO2_NETS.L) / (VY_PY.L(i,r) * PY.L(i,r)* py0(i,r));
sharey_yr(r,"PCO2_NETSr",i,yr)$constr(i,r)= (VY_CO2_NETSr.L(i,r)* PCO2_NETSr.L(r)) / (VY_PY.L(i,r) * PY.L(i,r)* py0(i,r));
sharey_yr(r,"co2_tax",i,yr)$(trn(i) AND DEU(r) AND carbon_tax_de AND constr(i,r))= (VY_CO2_tax.L(i,r)* Pcarbon_tax.L(r)) / (VY_PY.L(i,r) * PY.L(i,r)* py0(i,r));

* ------ Check input shares --> shareYt_yr must be zero
shareyt_yr(r,i,yr)$constr(i,r) = round(
                 sum(j, sharey_yr(r,j,i,yr)) + sharey_yr(r,"SKL",i,yr) + sharey_yr(r,"USK",i,yr) + sharey_yr(r,"CAP",i,yr) + sharey_yr(r,"RES",i,yr)
                 + sharey_yr(r,"PCO2",i,yr) + sharey_yr(r,"PCO2W",i,yr) + sharey_yr(r,"PCO2_ETS",i,yr) + sharey_yr(r,"PCO2_NETS",i,yr) + sharey_yr(r,"PCO2_NETSr",i,yr)
                 - 1, rd);

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ 22.07.2014 GROSS VALUE ADDED = output value - intermediates
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
gva_yr(r,i,yr)$constr(i,r)= VY_PY.L(i,r) * PY.L(i,r) * py0(i,r) - sum(j, VY_PA.L(j,i,r)* PA.L(j,r)* (1+ti(j,i,r))) ;
gva_yr(r,i,yr)$ele(i)     = Y.L(i,r) * vom(i,r) * PY.L(i,r) * py0(i,r) - sum((j,gen), (VELEx_PA.L(j,gen,r) + VELEn_PA.L(j,gen,r)) * PA.L(j,r) * (1+ti(j,i,r)));
gva_yr(r,"total",yr)      = sum(i, gva_yr(r,i,yr)) ;
gva_yr("EU28",i,yr)       = sum(r$eu28(r),   gva_yr(r,i,yr)) ;
gva_yr("EU28","total",yr) = sum(r$eu28(r),   gva_yr(r,"total",yr)) ;
gva_yr("World",i,yr)      = sum(r,   gva_yr(r,i,yr)) ;
gva_yr("World","total",yr)= sum(r,   gva_yr(r,"total",yr)) ;

* ------ 20.05.2015 BWS Berechnung
* ------ VY_PY.L("ele",r) - sum(j, VY_PAY.L(j,"ele",r)) = vom("ele",r)
gva_real_yr(r,i,yr)$constr(i,r)  = Y.L(i,r) * vom(i,r) *      py0(i,r) - sum(j, VY_PA.L(j,i,r) * (1+ti(j,i,r)));
gva_real_yr(r,i,yr)$ele(i)       = Y.L(i,r) * vom(i,r) *      py0(i,r) - sum((j,gen), (VELEx_PA.L(j,gen,r) + VELEn_PA.L(j,gen,r)) *   (1+ti(j,i,r)));
gva_real_yr(r,"Energy",yr)       = sum(i$e(i), gva_real_yr(r,i,yr));
gva_real_yr(r,"Energy Intensive Ind",yr)       = sum(i$indE(i), gva_real_yr(r,i,yr));
gva_real_yr(r,"non-Energy Intensive Ind",yr)       = sum(i$indEN(i), gva_real_yr(r,i,yr));
gva_real_yr(r,"Other Sectors",yr)       = sum(i$other_sec(i), gva_real_yr(r,i,yr));
gva_real_yr(r,"total",yr)        = sum(i,   gva_real_yr(r,i,yr)) ;

gva_real_yr("EU28",i,yr)                            = sum(r$eu28(r),   gva_real_yr(r,i,yr)) ;
gva_real_yr("EU28","Energy",yr)                     = sum(i$e(i), gva_real_yr("EU28",i,yr));
gva_real_yr("EU28","Energy Intensive Ind",yr)       = sum(i$indE(i), gva_real_yr("EU28",i,yr));
gva_real_yr("EU28","non-Energy Intensive Ind",yr)   = sum(i$indEN(i), gva_real_yr("EU28",i,yr));
gva_real_yr("EU28","Other Sectors",yr)              = sum(i$other_sec(i), gva_real_yr("EU28",i,yr));
gva_real_yr("EU28","total",yr)                      = sum(r$eu28(r),   gva_real_yr(r,"total",yr)) ;

gva_real_yr("World",i,yr)                           = sum(r,   gva_real_yr(r,i,yr)) ;
gva_real_yr("World","Energy",yr)                    = sum(i$e(i), gva_real_yr("World",i,yr));
gva_real_yr("World","Energy Intensive Ind",yr)      = sum(i$indE(i), gva_real_yr("World",i,yr));
gva_real_yr("World","non-Energy Intensive Ind",yr)  = sum(i$indEN(i), gva_real_yr("World",i,yr));
gva_real_yr("World","Other Sectors",yr)             = sum(i$other_sec(i), gva_real_yr("World",i,yr));
gva_real_yr("World","total",yr)                     = sum(r,   gva_real_yr(r,"total",yr)) ;



* ------ ZPF production in model
zpf_gva_yr(i,r,yr)$constr(i,r) = round(
         gva_yr(r,i,yr)
         - VY_PSKL.L(i,r) * PSKL.L(r) - VY_PUSK.L(i,r)* PUSK.L(r)
         - VY_RKR.L(i,r)  * RKR.L(R)  - VY_PR.L(I,R)  * PR.L(i,R)
         - VY_CO2.L(i,r)$notrad(r)         * PCO2.L(r)
         - VY_CO2W.L(i,r)$pco2w_r(r)$worldtrade * PCO2W.L
         - VY_CO2W.L(i,r)$eutrade * PCO2W.L
         - VY_CO2W.L(i,r)$worldtrade2      * PCO2W.L
         - VY_CO2_NETS.L(i,r)$netstrade    * PCO2_NETS.L
         - VY_CO2_NETSr.L(i,r)$netstrade_r * PCO2_NETSr.L(r)
         - VY_CO2_ETS.L(i,r)$etstrade      * PCO2_ETS.L
, rd);

* ------ 19.05.2015 Calculations for all but ELE -------------------------------
constr(i,r) = no;
constr(i,r)$gva_yr(r,i,yr) = yes;
constr(i,r)$ele(i) = no; display constr;
* ------------------------------------------------------------------------------

shareYgva_yr(r,"SKL",i,yr)$constr(i,r)    = (VY_PSKL.L(i,r)* PSKL.L(r)) / gva_yr(r,i,yr);
shareYgva_yr(r,"USK",i,yr)$constr(i,r)    = (VY_PUSK.L(i,r)* PUSK.L(r)) / gva_yr(r,i,yr);
shareYgva_yr(r,"CAP",i,yr)$constr(i,r)    = (VY_RKR.L(i,r) * RKR.L(r))  / gva_yr(r,i,yr);
shareYgva_yr(r,"RES",i,yr)$constr(i,r)    = (VY_PR.L(i,r)  * PR.L(i,r)) / gva_yr(r,i,yr);
shareYgva_yr(r,"PCO2",i,yr)$constr(i,r)   = (VY_CO2.L(i,r) * PCO2.L(r)) / gva_yr(r,i,yr);
shareYgva_yr(r,"PCO2W",i,yr)$constr(i,r)  = (VY_CO2W.L(i,r)* PCO2W.L)   / gva_yr(r,i,yr);
shareYgva_yr(r,"PCO2_NETS",i,yr)$constr(i,r) = (VY_CO2_NETS.L(i,r)* PCO2_NETS.L) / gva_yr(r,i,yr);
shareYgva_yr(r,"PCO2_NETSr",i,yr)$constr(i,r) = (VY_CO2_NETSr.L(i,r)* PCO2_NETSr.L(r)) / gva_yr(r,i,yr);
shareYgva_yr(r,"PCO2_ETS",i,yr)$constr(i,r) = (VY_CO2_ETS.L(i,r) * PCO2_ETS.L)  / gva_yr(r,i,yr);
shareYgva2_yr(r,"PCO2_ETS",i,yr)$gva_real_yr(r,i,yr) = (VY_CO2_ETS.L(i,r) * PCO2_ETS.L)  / gva_real_yr(r,i,yr);
shareYgva_yr(r,"co2_tax",i,yr)$(trn(i) AND DEU(r) AND carbon_tax_de AND constr(i,r))  = (VY_CO2_tax.L(i,r)* Pcarbon_tax.L(r))   / gva_yr(r,i,yr);

* ------ Check input shares --> shareYtgva_yr must be zero
shareYtgva_yr(r,i,yr)$constr(i,r) = round(
                 sum(j, shareYgva_yr(r,j,i,yr)) + shareYgva_yr(r,"SKL",i,yr) + shareYgva_yr(r,"USK",i,yr) + shareYgva_yr(r,"CAP",i,yr) + shareYgva_yr(r,"RES",i,yr)
                 + shareYgva_yr(r,"PCO2",i,yr) + shareYgva_yr(r,"PCO2W",i,yr) + shareYgva_yr(r,"PCO2_ETS",i,yr) + shareYgva_yr(r,"PCO2_NETS",i,yr) + shareYgva_yr(r,"PCO2_NETSr",i,yr)
                 - 1, rd);

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ---------------------------------------------------------------------- *
* ECONOMIC RESULTS
* ---------------------------------------------------------------------- *

* ------ Macroeconomic Indicators ----------------------------------------------
welf_yr(r,yr)    = W.L(r);
welf_hh_yr(hh,r,yr)$HH_DISAG(r)    = W_hh.L(hh,r);

* ------ Trade
VA_PYxm.L(i,r,r)         = 0;
ex_yr(r,i,yr)            = sum(s, VA_PYxm.L(i,r,s) * PY.L(i,r));
im_yr(r,i,yr)            = sum(s, VA_PYxm.L(i,s,r) * PY.L(i,s));
ex_yr("EU28",i,yr)       = sum(r$eu28(r), ex_yr(r,i,yr));
im_yr("EU28",i,yr)       = sum(r$eu28(r), im_yr(r,i,yr));
ex_yr(r,"total",yr)      = sum(i, ex_yr(r,i,yr));
im_yr(r,"total",yr)      = sum(i, im_yr(r,i,yr));
ex_yr("EU28","total",yr) = sum(i, ex_yr("EU28",i,yr));
im_yr("EU28","total",yr) = sum(i, im_yr("EU28",i,yr));
trdblnc_yr(r,i,yr)       = ex_yr(r,i,yr) - im_yr(r,i,yr);
trdblnc_yr(r,"total",yr) = sum(i,        ex_yr(r,i,yr) - im_yr(r,i,yr));
trdblnc_yr("EU28",i,yr)  = sum(r$eu28(r),ex_yr(r,i,yr) - im_yr(r,i,yr)) ;
trdblnc_yr("EU28","total",yr) = sum(r$eu28(r), trdblnc_yr(r,"total",yr));
trdblnc_yr("world",i,yr)      = eps + round(sum(r,    trdblnc_yr(r,i,yr)), 8) ;
trdblnc_yr("world","total",yr)= eps + round(sum((r,i),trdblnc_yr(r,i,yr)), 8) ;
*ex_yr("BRD",i,yr)        = sum(r$bawdeu(r), ex_yr(r,i,yr));
*im_yr("BRD",i,yr)        = sum(r$bawdeu(r), im_yr(r,i,yr));
*ex_yr("BRD","total",yr)  = sum(i, ex_yr("BRD",i,yr));
*im_yr("BRD","total",yr)  = sum(i, im_yr("BRD",i,yr));
*trdblnc_yr("BRD",i,yr)   = sum(r$bawdeu(r),ex_yr(r,i,yr) - im_yr(r,i,yr)) ;
*trdblnc_yr("BRD","total",yr)  = sum(r$bawdeu(r), trdblnc_yr(r,"total",yr));

* ------ Tradematrix
tradematrix_yr(i,r,s,yr)         = VA_PYxm.L(i,r,s);
tradematrix_yr("total",r,s,yr)   = sum(i, tradematrix_yr(i,r,s,yr));
tradematrix_yr(i,"EU28",s,yr)            = sum(r$EU28(r),   tradematrix_yr(i,r,s,yr));
tradematrix_yr(i,r,"EU28",yr)            = sum(s$EU28(s),   tradematrix_yr(i,r,s,yr));
tradematrix_yr("total","EU28",s,yr)      = sum(r$eu28(r),   tradematrix_yr("total",r,s,yr));
tradematrix_yr("total",r,"EU28",yr)      = sum(s$eu28(s),   tradematrix_yr("total",r,s,yr));
tradematrix_yr(i,"World",s,yr)           = sum(r,   tradematrix_yr(i,r,s,yr));
tradematrix_yr(i,r,"World",yr)           = sum(s,   tradematrix_yr(i,r,s,yr));
tradematrix_yr(i,"World","EU28",yr)      = sum(r,   tradematrix_yr(i,r,"EU28",yr));
tradematrix_yr(i,"EU28","World",yr)      = sum(s,   tradematrix_yr(i,"EU28",s,yr));
tradematrix_yr("total","World",s,yr)     = sum(r,   tradematrix_yr("total",r,s,yr));
tradematrix_yr("total",r,"World",yr)     = sum(s,   tradematrix_yr("total",r,s,yr));
tradematrix_yr("total","World","EU28",yr)= sum(r,   tradematrix_yr("total",r,"EU28",yr));
tradematrix_yr("total","EU28","World",yr)= sum(s,   tradematrix_yr("total","EU28",s,yr));

*tradematrix_yr(i,"BRD",s,yr)             = sum(r$bawdeu(r), tradematrix_yr(i,r,s,yr));
*tradematrix_yr(i,r,"BRD",yr)             = sum(s$bawdeu(s), tradematrix_yr(i,r,s,yr));
*tradematrix_yr(i,"BRD","EU28",yr)        = sum(s$eu28(s),   tradematrix_yr(i,"BRD",s,yr));
*tradematrix_yr(i,"EU28","BRD",yr)        = sum(r$eu28(r),   tradematrix_yr(i,r,"BRD",yr));
*tradematrix_yr("total","BRD",s,yr)       = sum(r$bawdeu(r), tradematrix_yr("total",r,s,yr));
*tradematrix_yr("total",r,"BRD",yr)       = sum(s$bawdeu(s), tradematrix_yr("total",r,s,yr));
*tradematrix_yr("total","EU28","BRD",yr)  = sum(r$eu28(r),   tradematrix_yr("total",r,"BRD",yr));
*tradematrix_yr("total","BRD","EU28",yr)  = sum(s$eu28(s),   tradematrix_yr("total","BRD",s,yr));
*tradematrix_yr(i,"World","BRD",yr)       = sum(r,   tradematrix_yr(i,r,"BRD",yr));
*tradematrix_yr(i,"BRD","World",yr)       = sum(s,   tradematrix_yr(i,"BRD",s,yr));
*tradematrix_yr("total","World","BRD",yr) = sum(r,   tradematrix_yr("total",r,"BRD",yr));
*tradematrix_yr("total","BRD","World",yr) = sum(s,   tradematrix_yr("total","BRD",s,yr));

* ------ GDP
*gdpreal(r)              =  VC_PC.L(r) + VINV_PINV.L(r) + sum(i, sum(s, VA_PYxm.L(i,r,s)) - sum(s, VA_PYxm.L(i,s,r)));
gdpreal5_yr(r,yr)        =  VC_PC.L(r) + VINV_PINV.L(r) + (sum(i, Y.L(i,r) * vxm(i,r))) - (sum(i, A.L(i,r) * vim(i,r)));
gdpreal5_yr("EU28",yr)   =  sum(r$eu28(r), gdpreal5_yr(r,yr));
trdblnc2_yr(r,i,yr)      = Y.L(i,r)*vxm(i,r) - A.L(i,r) * vim(i,r);

gdpreal_yr(r,yr)$(NOT HH_DISAG(r))     =  (VC_PC.L(r) * PC.L(r)) + (VINV_PINV.L(r) * PINV.L(r)) + trdblnc_yr(r,"total",yr) ;
gdpreal_yr(r,yr)$(HH_DISAG(r))         =  sum(hh,VC_hh_PC.L(hh,r) * pc_hh.l(hh,r)) + (VC_gov_PC.L(r) * pc_gov.l(r)) + (VINV_PINV.L(r) * PINV.L(r)) + trdblnc_yr(r,"total",yr) ;

gdpreal_yr("EU28",yr)    =  sum(r$eu28(r), gdpreal_yr(r,yr));
gdpreal_yr("World",yr)   =  sum(r, gdpreal_yr(r,yr));

* ------ 1.04.2016
*gdpreal5_yr("BRD",yr)    =  sum(r$bawdeu(r), gdpreal5_yr(r,yr));
*gdpreal_yr("BRD",yr)     =  sum(r$bawdeu(r), gdpreal_yr(r,yr));

* ------ 6.07.2015 Cumulative GDP
gdprealcum_yr(r,yr)$yr2011(yr) = gdpreal_yr(r,yr);
gdprealcum_yr(r,yr)$yr2015(yr) = 1/2 * 4 * (gdpreal_yr(r,yr-1) + gdpreal_yr(r,yr)) ;
gdprealcum_yr(r,yr)$after2015(yr) = 1/2 * 5 * (gdpreal_yr(r,yr-1) + gdpreal_yr(r,yr)) ;
gdprealcum_yr("EU28",yr)$yr2011(yr) = gdpreal_yr("EU28",yr);
gdprealcum_yr("EU28",yr)$yr2015(yr) = 1/2 * 4 * (gdpreal_yr("EU28",yr-1) + gdpreal_yr("EU28",yr)) ;
gdprealcum_yr("EU28",yr)$after2015(yr) = 1/2 * 5 * (gdpreal_yr("EU28",yr-1) + gdpreal_yr("EU28",yr)) ;
gdprealcum_yr("World",yr)$yr2011(yr) = gdpreal_yr("World",yr);
gdprealcum_yr("World",yr)$yr2015(yr) = 1/2 * 4 * (gdpreal_yr("World",yr-1) + gdpreal_yr("World",yr)) ;
gdprealcum_yr("World",yr)$after2015(yr) = 1/2 * 5 * (gdpreal_yr("World",yr-1) + gdpreal_yr("World",yr)) ;
*gdprealcum_yr("BRD",yr)$yr2007(yr) = gdpreal_yr("BRD",yr);
*gdprealcum_yr("BRD",yr)$yr2010(yr) = 1/2 * 3 * (gdpreal_yr("BRD",yr-1) + gdpreal_yr("BRD",yr)) ;
*gdprealcum_yr("BRD",yr)$after2010(yr) = 1/2 * 5 * (gdpreal_yr("BRD",yr-1) + gdpreal_yr("BRD",yr)) ;

*display gdprealcum_yr;


* ------ Input demands ---------------------------------------------------------
demand_yr(r,i,j,yr)      = VY_PA.L(i,j,r);
demand_yr(r,i,"ele",yr)  = sum(gen, VELEx_PA.L(i,gen,r) + VELEn_PA.L(i,gen,r)) ;
demand_yr(r,i,"c",yr)$(not HH_DISAG(r))    = VC_PA.L(i,r);
demand_yr(r,i,"c",yr)$(HH_DISAG(r))    = sum(hh,VC_hh_PA.L(i,hh,r)) + VC_gov_PA.L(i,r);
demand_yr(r,i,"i",yr)    = VINV_PA.L(i,r);
demand_yr("EU28",i,j,yr)      = sum(r$eu28(r), demand_yr(r,i,j,yr)) ;
demand_yr("EU28",i,"ele",yr)  = sum(r$eu28(r), demand_yr(r,i,"ele",yr)) ;
demand_yr("EU28",i,"c",yr)    = sum(r$eu28(r), demand_yr(r,i,"c",yr)) ;
demand_yr("EU28",i,"i",yr)    = sum(r$eu28(r), demand_yr(r,i,"i",yr)) ;
*demand_yr("BRD",i,j,yr)      = sum(r$bawdeu(r), demand_yr(r,i,j,yr)) ;
*demand_yr("BRD",i,"ele",yr)  = sum(r$bawdeu(r), demand_yr(r,i,"ele",yr)) ;
*demand_yr("BRD",i,"c",yr)    = sum(r$bawdeu(r), demand_yr(r,i,"c",yr)) ;
*demand_yr("BRD",i,"i",yr)    = sum(r$bawdeu(r), demand_yr(r,i,"i",yr)) ;


* ------ 13.03.2019 - usage of disposable income--------------------------------

VC_hh_CO2_NETSr_yr(hh,r,yr) = VC_hh_CO2_NETSr.L(hh,r);

*VC_hh_CO2_inv_pay_yr(hh,r,yr) = VC_hh_CO2_inv_pay.L(hh,r);
*VC_hh_CO2_SEC_yr(hh,r,yr)   = VC_hh_CO2_SEC.L(hh,r);

*VC_CO2_SEC_yr(r,yr) = VC_CO2_SEC.l(r);

abs_sector_hh_yr(r,i,hh,yr)$HH_DISAG(r) = VC_hh_PA.L(i,hh,r) * (PA.L(i,r) + tp(i,r));
abs_sector_hh_yr(r,"oil_trans",hh,yr)$(HH_DISAG(r) AND h_t_cons_reg(r)) = VC_HH_p_oil_trans.L(hh,r) * p_oil_trans.L(r);
abs_sector_hh_yr(r,"ele_trans",hh,yr)$(HH_DISAG(r) AND h_t_cons_reg(r)) = VC_HH_p_ele_trans.L(hh,r) * p_ele_trans.L(r);

abs_sector_hh_yr(r,"NETSr",hh,yr)$HH_DISAG(r) = VC_hh_CO2_NETSr.L(hh,r) * PCO2_NETSr.L(r);
abs_sector_hh_yr(r,"CO2_inv_pay",hh,yr)$HH_DISAG(r) = VC_hh_CO2_inv_pay.L(hh,r) * PCO2_inv_pay.L(hh,r);
*abs_sector_hh_yr(r,"LS_target",hh,yr)$HH_DISAG(r) = VC_hh_CO2_SEC.L(hh,r) * PCO2_DEU.L("residential");
abs_sector_hh_yr(r,"PCO2W",hh,yr)$HH_DISAG(r) = VC_hh_CO2W.L(hh,r) * PCO2W.L;
abs_sector_hh_yr(r,"Energy",hh,yr)$HH_DISAG(r) = sum(e, abs_sector_hh_yr(r,e,hh,yr)) 
*                                                + abs_sector_hh_yr(r,"CO2_inv_pay",hh,yr) 
                                                + abs_sector_hh_yr(r,"NETSr",hh,yr) 
                                                + abs_sector_hh_yr(r,"PCO2W",hh,yr)
                                                + (abs_sector_hh_yr(r,"ele_trans",hh,yr) 
                                                + abs_sector_hh_yr(r,"oil_trans",hh,yr))$h_t_cons_reg(r);

abs_sector_hh_yr(r,"Energy_no_transport",hh,yr)$(HH_DISAG(r) AND h_t_cons_reg(r)) = sum(e, abs_sector_hh_yr(r,e,hh,yr)) + abs_sector_hh_yr(r,"NETSr",hh,yr) + abs_sector_hh_yr(r,"PCO2W",hh,yr) 
+ abs_sector_hh_yr(r,"CO2_inv_pay",hh,yr)
;                                                

abs_sector_hh_yr(r,"Total",hh,yr)$HH_DISAG(r) = sum(i, abs_sector_hh_yr(r,i,hh,yr)) + abs_sector_hh_yr(r,"NETSr",hh,yr) + abs_sector_hh_yr(r,"PCO2W",hh,yr)
                                                + (abs_sector_hh_yr(r,"ele_trans",hh,yr) + abs_sector_hh_yr(r,"oil_trans",hh,yr))$h_t_cons_reg(r);


share_sector_hh_yr(r,i,hh,yr)$HH_DISAG(r) = abs_sector_hh_yr(r,i,hh,yr) / abs_sector_hh_yr(r,"Total",hh,yr);
share_sector_hh_yr(r,"oil_trans",hh,yr)$(HH_DISAG(r) AND h_t_cons_reg(r)) = abs_sector_hh_yr(r,"oil_trans",hh,yr) / abs_sector_hh_yr(r,"Total",hh,yr);
share_sector_hh_yr(r,"ele_trans",hh,yr)$(HH_DISAG(r) AND h_t_cons_reg(r)) = abs_sector_hh_yr(r,"ele_trans",hh,yr) / abs_sector_hh_yr(r,"Total",hh,yr);

share_sector_hh_yr(r,"NETSr",hh,yr)$HH_DISAG(r) = abs_sector_hh_yr(r,"NETSr",hh,yr) / abs_sector_hh_yr(r,"Total",hh,yr);

share_sector_hh_yr(r,"CO2_inv_pay",hh,yr)$HH_DISAG(r) = abs_sector_hh_yr(r,"CO2_inv_pay",hh,yr) / abs_sector_hh_yr(r,"Total",hh,yr);
*share_sector_hh_yr(r,"LS_target",hh,yr)$HH_DISAG(r) = abs_sector_hh_yr(r,"LS_target",hh,yr) / abs_sector_hh_yr(r,"Total",hh,yr);
share_sector_hh_yr(r,"PCO2W",hh,yr)$HH_DISAG(r) = abs_sector_hh_yr(r,"PCO2W",hh,yr) / abs_sector_hh_yr(r,"Total",hh,yr);
share_sector_hh_yr(r,"Energy",hh,yr)$HH_DISAG(r) = sum(e, share_sector_hh_yr(r,e,hh,yr)) 
                                                + share_sector_hh_yr(r,"NETSr",hh,yr) 
                                                + share_sector_hh_yr(r,"PCO2W",hh,yr) 
                                                + share_sector_hh_yr(r,"CO2_inv_pay",hh,yr)
                                                + (share_sector_hh_yr(r,"oil_trans",hh,yr) 
                                                + share_sector_hh_yr(r,"ele_trans",hh,yr))$h_t_cons_reg(r);

share_sector_hh_yr(r,"Energy_no_transport",hh,yr)$(HH_DISAG(r) AND h_t_cons_reg(r)) = sum(e, share_sector_hh_yr(r,e,hh,yr)) 
                                                                                    + share_sector_hh_yr(r,"NETSr",hh,yr) 
                                                                                    + share_sector_hh_yr(r,"PCO2W",hh,yr) 
                                                                                    + share_sector_hh_yr(r,"CO2_inv_pay",hh,yr)
                                                                                    ;

share_sector_hh_yr(r,"Total",hh,yr)$HH_DISAG(r) = sum(i, share_sector_hh_yr(r,i,hh,yr)) 
                                                + share_sector_hh_yr(r,"NETSr",hh,yr) 
                                                + share_sector_hh_yr(r,"PCO2W",hh,yr) 
                                                + share_sector_hh_yr(r,"CO2_inv_pay",hh,yr)
                                                + (share_sector_hh_yr(r,"oil_trans",hh,yr) 
                                                + share_sector_hh_yr(r,"ele_trans",hh,yr))$h_t_cons_reg(r);


*--------------------------------------------------------------------------
*---------- 25.04.2019 - components of income
*--------------------------------------------------------------------------                                               

share_income_hh("Energy",r,hh,yr)$HH_DISAG(r) = abs_sector_hh_yr(r,"Energy",hh,yr)/RA_hh_par_yr("net_income",r,hh,yr);
share_income_hh(e,r,hh,yr)$HH_DISAG(r) = abs_sector_hh_yr(r,e,hh,yr)/RA_hh_par_yr("net_income",r,hh,yr);
share_income_hh("NETSr",r,hh,yr)$HH_DISAG(r) = abs_sector_hh_yr(r,"NETSr",hh,yr)/RA_hh_par_yr("net_income",r,hh,yr);
share_income_hh("PCO2W",r,hh,yr)$HH_DISAG(r) = abs_sector_hh_yr(r,"PCO2W",hh,yr)/RA_hh_par_yr("net_income",r,hh,yr);
share_income_hh("CO2_inv_pay",r,hh,yr)$HH_DISAG(r) = abs_sector_hh_yr(r,"CO2_inv_pay",hh,yr)/RA_hh_par_yr("net_income",r,hh,yr);
*share_income_hh("LS_target",r,hh,yr)$HH_DISAG(r) = abs_sector_hh_yr(r,"LS_target",hh,yr)/RA_hh_par_yr("net_income",r,hh,yr);

* ----- 23.02.2018 - Adding sectorial disaggregation to the employment information
emplmt_sec_yr(r,"skl",i,yr) = VY_PSKL.L(i,r);
emplmt_sec_yr(r,"skl",gen,yr) = VELEx_PSKL.L(gen,r) + VELEn_PSKL.L(gen,r);
emplmt_sec_yr(r,"usk",i,yr) = VY_PUSK.L(i,r);
emplmt_sec_yr(r,"usk",gen,yr) = VELEx_PUSK.L(gen,r) + VELEn_PUSK.L(gen,r);
emplmt_sec_yr(r,"total",i,yr) = emplmt_sec_yr(r,"skl",i,yr) + emplmt_sec_yr(r,"usk",i,yr);
emplmt_sec_yr(r,"total",gen,yr) = emplmt_sec_yr(r,"skl",gen,yr) + emplmt_sec_yr(r,"usk",gen,yr);
emplmt_sec_yr("EU28","skl",i,yr) = sum(r$eu28(r),emplmt_sec_yr(r,"skl",i,yr));
emplmt_sec_yr("EU28","usk",i,yr) = sum(r$eu28(r),emplmt_sec_yr(r,"usk",i,yr));
emplmt_sec_yr("EU28","skl",gen,yr) = sum(r$eu28(r),emplmt_sec_yr(r,"skl",gen,yr));
emplmt_sec_yr("EU28","usk",gen,yr) = sum(r$eu28(r),emplmt_sec_yr(r,"usk",gen,yr));
emplmt_sec_yr("EU28","total",i,yr) = sum(r$eu28(r),emplmt_sec_yr(r,"total",i,yr));
emplmt_sec_yr("EU28","total",gen,yr) = sum(r$eu28(r),emplmt_sec_yr(r,"total",gen,yr));

* ------ Employment levels -----------------------------------------------------
emplmt_yr(r,"skl",yr)    = sum(i, VY_PSKL.L(i,r)) + sum(gen, VELEx_PSKL.L(gen,r) + VELEn_PSKL.L(gen,r));
emplmt_yr(r,"usk",yr)    = sum(i, VY_PUSK.L(i,r)) + sum(gen, VELEx_PUSK.L(gen,r) + VELEn_PUSK.L(gen,r));
emplmt_yr(r,"total",yr)  = emplmt_yr(r,"skl",yr) + emplmt_yr(r,"usk",yr) ;
emplmt_yr("EU28","skl",yr) = sum(r$eu28(r), emplmt_yr(r,"skl",yr)) ;
emplmt_yr("EU28","usk",yr) = sum(r$eu28(r), emplmt_yr(r,"usk",yr)) ;
emplmt_yr("EU28","total",yr)=sum(r$eu28(r), emplmt_yr(r,"total",yr)) ;
*emplmt_yr("BRD","skl",yr) = sum(r$bawdeu(r), emplmt_yr(r,"skl",yr)) ;
*emplmt_yr("BRD","usk",yr) = sum(r$bawdeu(r), emplmt_yr(r,"usk",yr)) ;
*emplmt_yr("BRD","total",yr)=sum(r$bawdeu(r), emplmt_yr(r,"total",yr)) ;

* ------ 16.02.2015 Calculate absolute employment numbers --> corrected by size-Parameter!
emplmtno_yr(r,"skl",yr)$yr2011(yr)=emplmtno_2011(r,"skl");
emplmtno_yr(r,"usk",yr)$yr2011(yr)=emplmtno_2011(r,"usk");
emplmtno_yr(r,"skl",yr)$after(yr)= emplmtno_2011(r,"skl") * (emplmt_yr(r,"skl",yr) / emplmt_yr(r,"skl","2011")) / size_skl(r,yr) ;
emplmtno_yr(r,"usk",yr)$after(yr)= emplmtno_2011(r,"usk") * (emplmt_yr(r,"usk",yr) / emplmt_yr(r,"usk","2011")) / size_usk(r,yr) ;
emplmtno_yr(r,"total",yr)        = emplmtno_yr(r,"skl",yr) + emplmtno_yr(r,"usk",yr) ;
emplmtno_yr("EU28","skl",yr)     = sum(r$eu28(r),   emplmtno_yr(r,"skl",yr)) ;
emplmtno_yr("EU28","usk",yr)     = sum(r$eu28(r),   emplmtno_yr(r,"usk",yr)) ;
emplmtno_yr("EU28","total",yr)   = sum(r$eu28(r),   emplmtno_yr(r,"total",yr)) ;
*emplmtno_yr("BRD","skl",yr)      = sum(r$bawdeu(r), emplmtno_yr(r,"skl",yr)) ;
*emplmtno_yr("BRD","usk",yr)      = sum(r$bawdeu(r), emplmtno_yr(r,"usk",yr)) ;
*emplmtno_yr("BRD","total",yr)    = sum(r$bawdeu(r), emplmtno_yr(r,"total",yr)) ;

* ----- 27.02.2018 - Employment per sector
emplmtno_sec_yr(r,"skl",i,yr) = emplmt_sec_yr(r,"skl",i,yr)/emplmt_yr(r,"skl",yr) * emplmtno_yr(r,"skl",yr);
emplmtno_sec_yr(r,"skl",gen,yr) = emplmt_sec_yr(r,"skl",gen,yr)/emplmt_yr(r,"skl",yr) * emplmtno_yr(r,"skl",yr);
emplmtno_sec_yr(r,"usk",i,yr) = emplmt_sec_yr(r,"usk",i,yr)/emplmt_yr(r,"usk",yr) * emplmtno_yr(r,"usk",yr);
emplmtno_sec_yr(r,"usk",gen,yr) = emplmt_sec_yr(r,"usk",gen,yr)/emplmt_yr(r,"usk",yr) * emplmtno_yr(r,"usk",yr);
emplmtno_sec_yr(r,"total",i,yr) = emplmtno_sec_yr(r,"skl",i,yr) + emplmtno_sec_yr(r,"usk",i,yr);
emplmtno_sec_yr(r,"total",gen,yr) = emplmtno_sec_yr(r,"skl",gen,yr) + emplmtno_sec_yr(r,"usk",gen,yr);
emplmtno_sec_yr("EU28","skl",i,yr) = sum(r$eu28(r), emplmtno_sec_yr(r,"skl",i,yr));
emplmtno_sec_yr("EU28","skl",gen,yr) = sum(r$eu28(r), emplmtno_sec_yr(r,"skl",gen,yr));
emplmtno_sec_yr("EU28","usk",i,yr) = sum(r$eu28(r), emplmtno_sec_yr(r,"usk",i,yr));
emplmtno_sec_yr("EU28","usk",gen,yr) = sum(r$eu28(r), emplmtno_sec_yr(r,"usk",gen,yr));
emplmtno_sec_yr("EU28","total",i,yr) = sum(r$eu28(r), emplmtno_sec_yr(r,"total",i,yr));
emplmtno_sec_yr("EU28","total",gen,yr) = sum(r$eu28(r), emplmtno_sec_yr(r,"total",gen,yr));

* ------ Unemployment ----------------------------------------------------------
ur_yr(r,"skl",yr)        = URSK.L(r) ;
ur_yr(r,"usk",yr)        = URUN.L(r) ;
ur_yr(r,"total",yr)      = (emplmtno_yr(r,"skl",yr) * URSK.L(r) + emplmtno_yr(r,"usk",yr) * URUN.L(r)) / emplmtno_yr(r,"total",yr) ;
ur_yr("EU28","skl",yr)   = (sum(r$eu28(r), emplmtno_yr(r,"skl",yr) * ur_yr(r,"skl",yr))) / sum(r$eu28(r), emplmtno_yr(r,"skl",yr)) ;
ur_yr("EU28","usk",yr)   = (sum(r$eu28(r), emplmtno_yr(r,"usk",yr) * ur_yr(r,"usk",yr))) / sum(r$eu28(r), emplmtno_yr(r,"usk",yr)) ;
ur_yr("EU28","total",yr) = (sum(r$eu28(r), emplmtno_yr(r,"skl",yr) * ur_yr(r,"skl",yr)) + sum(r$eu28(r), emplmtno_yr(r,"usk",yr) * ur_yr(r,"usk",yr))) / sum(r$eu28(r), emplmtno_yr(r,"total",yr));
*ur_yr("BRD","skl",yr)    = (sum(r$bawdeu(r), emplmtno_yr(r,"skl",yr) * ur_yr(r,"skl",yr))) / sum(r$eu28(r), emplmtno_yr(r,"skl",yr)) ;
*ur_yr("BRD","usk",yr)    = (sum(r$bawdeu(r), emplmtno_yr(r,"usk",yr) * ur_yr(r,"usk",yr))) / sum(r$eu28(r), emplmtno_yr(r,"usk",yr)) ;
*ur_yr("BRD","total",yr)  = (sum(r$bawdeu(r), emplmtno_yr(r,"skl",yr) * ur_yr(r,"skl",yr)) + sum(r$eu28(r), emplmtno_yr(r,"usk",yr) * ur_yr(r,"usk",yr))) / sum(r$eu28(r), emplmtno_yr(r,"total",yr));

display emplmtno_yr, emplmtno_2011, ur_yr;



*display report_emp(r,yr,emplmtno_yr(r,"total",yr));
*$stop
* ---- check here error UP

* ------ Prices ----------------------------------------------------------------
pk_yr(r,yr)$rsk  = RKR.L(r)      ;
pk_yr(r,yr)$gk   = RKG.L         ;
pusk_yr(r,yr)    = PUSK.L(r)     ;
pskl_yr(r,yr)    = PSKL.L(r)     ;
pinv_yr(r,yr)    = PINV.L(r)     ;
pc_yr(r,yr)      = PC.L(r)       ;
py_yr(r,i,yr)    = PY.L(i,r)     ;
pa_yr(r,i,yr)    = PA.L(i,r)     ;
pr_yr(r,i,yr)    = PR.L(i,r)     ;
pgen_yr(r,gen,yr)= PGEN.L(gen,r) ;
pco2_ets_yr(yr)  = PCO2_ETS.L    ;
pco2_nets_yr(yr) = PCO2_NETS.L   ;
pco2_netsr_yr(r,yr) = PCO2_NETSr.L(r)   ;
PCO2_inv_pay_yr(hh,r,yr) = PCO2_inv_pay.L(hh,r);
CO2_inv_pay_yr(hh,r,yr) = CO2_inv_pay.L(hh,r);
pco2w_yr(yr)     = PCO2W.L       ;
price_pco2w_yr(yr) = PCO2W.L/PC.L("USA");
pco2_yr(r,yr)    = PCO2.L(r)     ;
pco2_yr(r,yr)$eu28(r) = PCO2W.L;
pco2_DEU_yr(sec,yr) = PCO2_DEU.L(sec);
rkx_ele_yr(r,gen,yr)= RKX_ELE.L(gen,r);

*25.10.2017------- Fuel prices check
price_fuel_yr(r,"gas",yr) = py_yr(r,"gas",yr)/pc_yr(r,yr);
price_fuel_yr(r,"CRU",yr) = py_yr(r,"CRU",yr)/pc_yr(r,yr);
price_fuel_yr(r,"COL",yr) = py_yr(r,"COL",yr)/pc_yr(r,yr);

price_fuel_yr_chk(r,"gas",yr) = price_fuel_yr(r,"gas",yr) - pytarget("gas",r);
price_fuel_yr_chk(r,"COL",yr) = price_fuel_yr(r,"COL",yr) - pytarget("COL",r);
price_fuel_yr_chk(r,"CRU",yr) = price_fuel_yr(r,"CRU",yr) - pytarget("CRU",r);

display price_fuel_yr;

* ------ Output activities -----------------------------------------------------
Y_yr(r,i,yr) = Y.L(i,r);
INV_yr(r,yr) = INV.L(r);
YT_yr(yr)    = YT.L;
A_yr(r,i,yr) = A.L(i,r);
C_yr(r,yr)   = C.L(r);
ELEx_yr(r,gen,yr) = ELEx.L(gen,r);
ELEn_yr(r,gen,yr) = ELEn.L(gen,r);
ELE_yr(r,gen,yr) = ELEx.L(gen,r) + ELEn.L(gen,r);

* ------ Output quantities -----------------------------------------------------
*VY_PAY_yr(r,j,i,yr)      = VY_PAY.L(j,i,r);
VY_PA_yr(r,j,i,yr)       = VY_PA.L(j,i,r);
VY_PY_yr(r,i,yr)         = VY_PY.L(i,r);
VA_PA_yr(r,i,yr)         = VA_PA.L(i,r);
VINV_PINV_yr(r,yr)       = VINV_PINV.L(r);
VC_PA_yr(i,r,yr)$(NOT HH_DISAG(r))  = VC_PA.L(i,r);
VC_PA_yr(i,r,yr)$(HH_DISAG(r))      = sum(hh,VC_hh_PA.L(i,hh,r)) + VC_gov_PA.L(i,r);

VC_hh_PA_yr(i,hh,r,yr)$(HH_DISAG(r))   = VC_hh_PA.L(i,hh,r);
VC_gov_PA_yr(i,r,yr)$(HH_DISAG(r))  = VC_gov_PA.L(i,r);

VC_PC_yr(r,yr)$(NOT HH_DISAG(r))    = VC_PC.L(r);
VC_PC_yr(r,yr)$(HH_DISAG(r))        = sum(hh,VC_hh_PC.L(hh,r)) + VC_gov_PC.L(r);

VC_hh_PC_yr(hh,r,yr)$(HH_DISAG(r))        = VC_hh_PC.L(hh,r);
VC_gov_PC_yr(r,yr)$(HH_DISAG(r))        = VC_gov_PC.L(r);

VYT_PT_yr(yr)            = VYT_PT.L;
VY_PGEN_yr(r,gen,yr)     = VY_PGEN.L(gen,r);
VELEx_PGEN_yr(r,gen,yr)  = VELEx_PGEN.L(gen,r);
VELEn_PGEN_yr(r,gen,yr)  = VELEn_PGEN.L(gen,r);
VELEx_PA_yr(i,gen,r,yr)  = VELEx_PA.L(i,gen,r);
VELEn_PA_yr(i,gen,r,yr)  = VELEn_PA.L(i,gen,r);
VELEx_PA_yr(i,gen,r,yr)  = VELEx_PA.L(i,gen,r);
VELEn_PA_yr(i,gen,r,yr)  = VELEn_PA.L(i,gen,r);

* ------ Input quantities and demand
VD_PINV_yr(r,yr)         = VD_PINV.L(r)   ;
VD_PC_yr(r,yr)           = VD_PC.L(r)     ;
VINV_PA_yr(r,i,yr)       = VINV_PA.L(i,r) ;

VD_PC_HH_yr(hh,r,yr)$HH_DISAG(r)     = VD_PC_hh.L(hh,r);
VD_PC_gov_yr(r,yr)$HH_DISAG(r)       = VD_PC_gov.l(r);
VD_PINV_HH_yr(hh,r,yr)$HH_DISAG(r)   = VD_PINV_hh.L(hh,r);

* ------ EU28
VY_PY_yr("EU28",i,yr)    = sum(r$eu28(r), VY_PY.L(i,r)) ;
VA_PA_yr("EU28",i,yr)    = sum(r$eu28(r), VA_PA.L(i,r)) ;
VINV_PINV_yr("EU28",yr)  = sum(r$eu28(r), VINV_PINV.L(r)) ;
VC_PC_yr("EU28",yr)      = sum(r$eu28(r), VC_PC.L(r)) ;
VY_PGEN_yr("EU28",gen,yr)= sum(r$eu28(r), VY_PGEN.L(gen,r)) ;
VELEx_PGEN_yr("EU28",gen,yr) = sum(r$eu28(r), VELEx_PGEN.L(gen,r)) ;
VELEn_PGEN_yr("EU28",gen,yr) = sum(r$eu28(r), VELEn_PGEN.L(gen,r)) ;

* ------ 23.07.2015
*VINV_PINV_yr("BRD",yr)  = sum(r$bawdeu(r), VINV_PINV_yr(r,yr)) ;
VINV_PINV_yr("EU28",yr) = sum(r$eu28(r), VINV_PINV_yr(r,yr)) ;
VINV_PINV_yr("World",yr)= sum(r, VINV_PINV_yr(r,yr)) ;

invgdp_yr(r,yr)          = VINV_PINV_yr(r,yr) / gdpreal_yr(r,yr) * 100 ;
invgdp_yr("EU28",yr)     = VINV_PINV_yr("EU28",yr) / gdpreal_yr("EU28",yr) * 100  ;
invgdp_yr("World",yr)    = VINV_PINV_yr("World",yr) / gdpreal_yr("World",yr) * 100  ;
*invgdp_yr("BRD",yr)      = VINV_PINV_yr("BRD",yr) / gdpreal_yr("BRD",yr) * 100  ;

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ Summary Tax Substituion  ----------------------------------------------
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

    no_vat_summary(r,yr,"tax_reb")$(eU28(r) AND no_vat)     = tax_reb.L(r);
    no_vat_summary(r,yr,"sum_tax")$(eU28(r) AND no_vat)     = sum((hh,i)$(sec_no_vat(i,r) AND NOT e(i)), tp(i,r) * PA.L(i,r) * C_hh.L(hh,r) * (c_hh0(i,r) * hh_sector_share(r,i,hh))) +
                                                              sum((hh,i)$(sec_no_vat(i,r) AND e(i)), tp(i,r) * PA.L(i,r) * C_hh.L(hh,r) * (c_hh0(i,r) * aeei(i,"c",r)) * hh_sector_share(r,i,hh));
    no_vat_summary(r,yr,"total")$(eU28(r) AND no_vat)       = tax_reb.L(r) * no_vat_summary(r,yr,"sum_tax");


    no_lab_summary(r,yr,"tax_reb")$(eU28(r) AND low_lab_tax)     = tax_reb.L(r);
    no_lab_summary(r,yr,"sum_tax")$(eU28(r) AND low_lab_tax)     =  sum((i)$(sec_no_lab("skl",i,r) AND NOT e(i)), rtf("skl",i,r) * PSKL.L(r) * Y.L(i,r) * skld0(i,r)) +
                                                                    sum((i)$(sec_no_lab("usk",i,r) AND NOT e(i)), rtf("usk",i,r) * PUSK.L(r) * Y.L(i,r) * uskd0(i,r)) +

                                                                    sum((gen,i)$(sec_no_lab("skl",i,r) AND e(i)), rtf("skl","ele",r) * PSKL.L(r) * ELEx.L(gen,r) * (skl_input(gen,r))) +
                                                                    sum((gen,i)$(sec_no_lab("usk",i,r) AND e(i)), rtf("usk","ele",r) * PUSK.L(r) * ELEx.L(gen,r) * (usk_input(gen,r))) +
                                                                    sum((gen,i)$(sec_no_lab("skl",i,r) AND e(i)), rtf("skl","ele",r) * PSKL.L(r) * ELEn.L(gen,r) * (skl_input(gen,r))) +
                                                                    sum((gen,i)$(sec_no_lab("usk",i,r) AND e(i)), rtf("usk","ele",r) * PUSK.L(r) * ELEn.L(gen,r) * (usk_input(gen,r))) ;

    no_lab_summary(r,yr,"total")$(eU28(r) AND low_lab_tax)       = tax_reb.L(r) * no_lab_summary(r,yr,"sum_tax");


* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ Sectoral composites  --------------------------------------------------
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

* ---- Production
* ---- Revenue
    prod_accounts(r,i,"revenue",yr)                                = VY_PY.l(i,r)       * PY.l(i,r);

* ---- Inputs, including production factors
    prod_accounts(r,i,"PSKL",yr)$(not ele(i))                                   = VY_PSKL.l(i,r)     * PSKL.l(r);
    prod_accounts(r,i,"PUSK",yr)$(not ele(i))                                   = VY_PUSK.l(i,r)     * PUSK.l(r);
    prod_accounts(r,i,"CAP",yr)$(not ele(i))                                    = VY_RKR.l(i,r)      * RKR.l(r);
    prod_accounts(r,i,"PR",yr)$(not ele(i) and rd0(i,r))                        = VY_PR.l(i,r)       * PR.l(i,r);
    prod_accounts(r,i,j,yr)$(vafm(j,i,r) AND NOT ele(i))                        = VY_PA.l(j,i,r)     * PA.l(j,r);
    prod_accounts(r,i,gen,yr)$(ele(i))                                          = VY_PGEN.l(gen,r)   * PGEN.l(gen,r);

* ---- CO2 certificates
    prod_accounts(r,i,"CO2",yr)$(notrad(r) AND NOT ele(i))                      = VY_CO2.l(i,r)  * PCO2.l(r);
    prod_accounts(r,i,"CO2W",yr)$(pco2w_r(r) AND worldtrade AND NOT ele(i))     = VY_CO2W.l(i,r) * PCO2W.l;
    prod_accounts(r,i,"CO2W",yr)$(eu28(r) AND eutrade AND NOT ele(i))           = VY_CO2W.l(i,r) * PCO2W.l;
    prod_accounts(r,i,"CO2W",yr)$(worldtrade2 AND NOT ele(i))                   = VY_CO2W.l(i,r) * PCO2W.l;
    prod_accounts(r,i,"CO2_NETS",yr)$(netstrade AND NOT ele(i))                 = VY_CO2_NETS.l(i,r)  * PCO2_NETS.l;
    prod_accounts(r,i,"CO2_NETSr",yr)$(netstrade_r AND NOT ele(i))              = VY_CO2_NETSr.l(i,r) * PCO2_NETSr.l(r);
    prod_accounts(r,i,"CO2_ETS",yr)$(eu28(r) AND ets(i) AND etstrade AND NOT ele(i))   = VY_CO2_ETS.l(i,r) * PCO2_ETS.l;
    prod_accounts(r,i,"co2_tax",yr)$(deu(r) AND trn(i) AND carbon_tax_de)   = VY_CO2_tax.l(i,r) * Pcarbon_tax.l(r);

* ---- Taxes
    prod_accounts(r,i,"tax_output",yr)                          = VY_PY.l(i,r)       * PY.l(i,r)  * ty(i,r);
    prod_accounts(r,i,"tax_input",yr)$( NOT ele(i))             = sum(j$vafm(j,i,r), VY_PA.l(j,i,r)     * PA.l(j,r)  * ti(j,i,r));
    prod_accounts(r,i,"rebate",yr)$(ele(i) AND diffrebate(r))   = VY_PY.l(i,r)       * PY.l(i,r)  * REBATE_DIFF.l(r);

    prod_accounts(r,i,"tax_capital",yr)$(HH_DISAG(r) AND diss_factor_tax AND NOT ele(i))         = VY_RKR.L(i,r)      * RKR.l(r)  * rtf("cap",i,r);
    prod_accounts(r,i,"tax_SKL",yr)$(HH_DISAG(r) AND diss_factor_tax AND NOT ele(i))             = VY_PSKL.L(i,r)     * PSKL.l(r) * rtf("skl",i,r);
    prod_accounts(r,i,"tax_USK",yr)$(HH_DISAG(r) AND diss_factor_tax AND NOT ele(i))             = VY_PUSK.L(i,r)     * PUSK.l(r) * rtf("usk",i,r);

* ---- total

    prod_accounts(r,i,"total_input",yr)$(not ele(i))         =  prod_accounts(r,i,"PSKL",yr) + prod_accounts(r,i,"PUSK",yr) + prod_accounts(r,i,"CAP",yr) + prod_accounts(r,i,"PR",yr) +
                                                                sum(j, prod_accounts(r,i,j,yr)) + prod_accounts(r,i,"CO2",yr) + prod_accounts(r,i,"CO2W",yr) + prod_accounts(r,i,"CO2_NETS",yr) + 
                                                                prod_accounts(r,i,"CO2_NETSr",yr) + prod_accounts(r,i,"CO2_ETS",yr) + prod_accounts(r,i,"tax_output",yr) + prod_accounts(r,i,"tax_input",yr)
                                                                + (prod_accounts(r,i,"tax_capital",yr) + prod_accounts(r,i,"tax_SKL",yr) + prod_accounts(r,i,"tax_USK",yr))$(HH_DISAG(r) AND diss_factor_tax)
                                                                + prod_accounts(r,i,"co2_tax",yr)$(deu(r) AND trn(i) AND carbon_tax_de);

    prod_accounts(r,i,"total_input",yr)$(ele(i))              = sum(gen, prod_accounts(r,i,gen,yr)) + prod_accounts(r,i,"tax_output",yr) + prod_accounts(r,i,"rebate",yr)$REBATE_DIFF.l(r);

    prod_accounts(r,i,"sumCHK",yr) = prod_accounts(r,i,"total_input",yr) - prod_accounts(r,i,"revenue",yr);

* ---- Electricity - eleX
* ---- Revenue
    elex_accounts(r,gen,"revenue",yr)$(out_gen(gen,r)$ks_x(gen,r))      =   VELEx_PGEN.l(gen,r)  *  PGEN.l(gen,r);

* ---- Inputs, including production factors
    elex_accounts(r,gen,"PSKL",yr)$(out_gen(gen,r)$ks_x(gen,r))         = VELEx_PSKL.l(gen,r)     * PSKL.l(r);
    elex_accounts(r,gen,"PUSK",yr)$(out_gen(gen,r)$ks_x(gen,r))         = VELEx_PUSK.l(gen,r)     * PUSK.l(r);
    elex_accounts(r,gen,"CAP",yr)$(out_gen(gen,r)$ks_x(gen,r))          = VELEx_RKX_ELE.l(gen,r)  * RKX_ELE.l(gen,r);
    elex_accounts(r,gen,i,yr)$(out_gen(gen,r)$ks_x(gen,r))              = VELEx_PA.l(i,gen,r)     * PA.l(i,r);

* ---- CO2 certificates
    elex_accounts(r,gen,"CO2",yr)$(notrad(r) AND out_gen(gen,r)$ks_x(gen,r))                      = VELEx_CO2.l(gen,r)  * PCO2.l(r);
    elex_accounts(r,gen,"CO2W",yr)$(pco2w_r(r) AND worldtrade AND out_gen(gen,r)$ks_x(gen,r))     = VELEx_CO2W.l(gen,r) * PCO2W.l;
    elex_accounts(r,gen,"CO2W",yr)$(eu28(r) AND eutrade AND out_gen(gen,r)$ks_x(gen,r))           = VELEx_CO2W.l(gen,r) * PCO2W.l;
    elex_accounts(r,gen,"CO2W",yr)$(worldtrade2 AND out_gen(gen,r)$ks_x(gen,r))                   = VELEx_CO2W.l(gen,r) * PCO2W.l;
    elex_accounts(r,gen,"CO2_ETS",yr)$(eu28(r) AND etstrade AND out_gen(gen,r)$ks_x(gen,r))       = VELEx_CO2_ETS.l(gen,r) * PCO2_ETS.l;

* ---- Taxes
    elex_accounts(r,gen,"tax_input",yr)$(out_gen(gen,r)$ks_x(gen,r))          = sum(i$vafm_input(i,gen,r), VELEx_PA.l(i,gen,r)     * PA.l(i,r)  * ti(i,"ele",r));

    elex_accounts(r,gen,"tax_capital",yr)$(HH_DISAG(r) AND diss_factor_tax)         = VELEx_RKX_ELE.L(gen,r)      * RKX_ELE.l(gen,r)  * rtf("cap","ele",r);
    elex_accounts(r,gen,"tax_SKL",yr)$(HH_DISAG(r) AND diss_factor_tax)             = VELEx_PSKL.L(gen,r)         * PSKL.l(r)         * rtf("skl","ele",r);
    elex_accounts(r,gen,"tax_USK",yr)$(HH_DISAG(r) AND diss_factor_tax)             = VELEx_PUSK.L(gen,r)         * PUSK.l(r)         * rtf("usk","ele",r);

* ---- total
    elex_accounts(r,gen,"total_input",yr)$(out_gen(gen,r)$ks_x(gen,r))        =  elex_accounts(r,gen,"PSKL",yr) + elex_accounts(r,gen,"PUSK",yr) + elex_accounts(r,gen,"CAP",yr) +
                                                                                  sum(i, elex_accounts(r,gen,i,yr)) + elex_accounts(r,gen,"CO2",yr) + elex_accounts(r,gen,"CO2W",yr) + 
                                                                                  elex_accounts(r,gen,"CO2_ETS",yr) + elex_accounts(r,gen,"tax_input",yr) +
                                                                                  (elex_accounts(r,gen,"tax_capital",yr) + elex_accounts(r,gen,"tax_SKL",yr) + elex_accounts(r,gen,"tax_USK",yr))$(HH_DISAG(r) AND diss_factor_tax);

    elex_accounts(r,gen,"sumCHK",yr)$(out_gen(gen,r)$ks_x(gen,r)) = elex_accounts(r,gen,"total_input",yr) - elex_accounts(r,gen,"revenue",yr);

* ---- Electricity - elen
* ---- Revenue
    elen_accounts(r,gen,"revenue",yr)$(out_gen(gen,r)$ks_n(gen,r))      =   VELEn_PGEN.l(gen,r)  *  PGEN.l(gen,r);

* ---- Inputs, including production factors
    elen_accounts(r,gen,"PSKL",yr)$(out_gen(gen,r)$ks_n(gen,r))         = VELEn_PSKL.l(gen,r)     * PSKL.l(r);
    elen_accounts(r,gen,"PUSK",yr)$(out_gen(gen,r)$ks_n(gen,r))         = VELEn_PUSK.l(gen,r)     * PUSK.l(r);
    elen_accounts(r,gen,"CAP",yr)$(out_gen(gen,r)$ks_n(gen,r))          = VELEn_RKR.l(gen,r)      * RKR.l(r);
    elen_accounts(r,gen,i,yr)$(out_gen(gen,r)$ks_n(gen,r))              = VELEn_PA.l(i,gen,r)     * PA.l(i,r);

* ---- CO2 certificates
    elen_accounts(r,gen,"CO2",yr)$(notrad(r) AND out_gen(gen,r)$ks_n(gen,r))                      = VELEn_CO2.l(gen,r)  * PCO2.l(r);
    elen_accounts(r,gen,"CO2W",yr)$(pco2w_r(r) AND worldtrade AND out_gen(gen,r)$ks_n(gen,r))     = VELEn_CO2W.l(gen,r) * PCO2W.l;
    elen_accounts(r,gen,"CO2W",yr)$(eu28(r) AND eutrade AND out_gen(gen,r)$ks_n(gen,r))           = VELEn_CO2W.l(gen,r) * PCO2W.l;
    elen_accounts(r,gen,"CO2W",yr)$(worldtrade2 AND out_gen(gen,r)$ks_n(gen,r))                   = VELEn_CO2W.l(gen,r) * PCO2W.l;
    elen_accounts(r,gen,"CO2_ETS",yr)$(eu28(r) AND etstrade AND out_gen(gen,r)$ks_n(gen,r))       = VELEn_CO2_ETS.l(gen,r) * PCO2_ETS.l;

* ---- Taxes
    elen_accounts(r,gen,"tax_input",yr)$(out_gen(gen,r)$ks_n(gen,r))                    = sum(i$vafm_input(i,gen,r), VELEn_PA.l(i,gen,r)     * PA.l(i,r)  * ti(i,"ele",r));
    elen_accounts(r,gen,"diffcost",yr)$(out_gen(gen,r) AND ks_n(gen,r) AND reg(gen))    = VELEn_PGEN.l(gen,r)  *  PGEN.l(gen,r)  * (1 - diffcost(gen,r));

    elen_accounts(r,gen,"tax_capital",yr)$(HH_DISAG(r) AND diss_factor_tax)         = VELEn_RKR.L(gen,r)          * RKR.l(r)      * rtf("cap","ele",r);
    elen_accounts(r,gen,"tax_SKL",yr)$(HH_DISAG(r) AND diss_factor_tax)             = VELEn_PSKL.L(gen,r)         * PSKL.l(r)         * rtf("skl","ele",r);
    elen_accounts(r,gen,"tax_USK",yr)$(HH_DISAG(r) AND diss_factor_tax)             = VELEn_PUSK.L(gen,r)         * PUSK.l(r)         * rtf("usk","ele",r);

* ---- total
    elen_accounts(r,gen,"total_input",yr)$(out_gen(gen,r)$ks_n(gen,r))        =  elen_accounts(r,gen,"PSKL",yr) + elen_accounts(r,gen,"PUSK",yr) + elen_accounts(r,gen,"CAP",yr) +
                                                                                  sum(i, elen_accounts(r,gen,i,yr)) + elen_accounts(r,gen,"CO2",yr) + elen_accounts(r,gen,"CO2W",yr) + 
                                                                                  elen_accounts(r,gen,"CO2_ETS",yr) + elen_accounts(r,gen,"tax_input",yr) + 
                                                                                  elen_accounts(r,gen,"diffcost",yr)$reg(gen) +
                                                                                  (elen_accounts(r,gen,"tax_capital",yr) + elen_accounts(r,gen,"tax_SKL",yr) + elen_accounts(r,gen,"tax_USK",yr))$(HH_DISAG(r) AND diss_factor_tax);

    elen_accounts(r,gen,"sumCHK",yr)$(out_gen(gen,r)$ks_n(gen,r)) = elen_accounts(r,gen,"total_input",yr) - elen_accounts(r,gen,"revenue",yr);

* ---- Armington Aggregation
* ---- Revenues    
    armi_accounts(r,i,"revenue",yr)$a0(i,r)             = VA_PA.l(i,r) * PA.l(i,r);

* ---- Inputs
* ---- domestic production
    armi_accounts(r,i,"domestic",yr)$a0(i,r)            = VA_PY.l(i,r) * PY.l(i,r);

* ---- Imported goods
    armi_accounts(r,i,"imports",yr)$a0(i,r)             = sum(s, VA_PYXM.l(i,s,r) * PY.l(i,s));

* ---- Transportation
    armi_accounts(r,i,"transport",yr)$a0(i,r)           = sum(s, VA_PT.l(i,s,r) * PT.l);

* ---- Taxes
    armi_accounts(r,i,"tax_transport",yr)$a0(i,r)       = sum(s, VA_PT.l(i,s,r) * PT.l * tm(i,s,r));
    armi_accounts(r,i,"tax_import_s",yr)$a0(i,r)        = sum(s, VA_PYXM.l(i,s,r) * PY.l(i,s) * tx(i,s,r));
    armi_accounts(r,i,"tax_import_r",yr)$a0(i,r)        = sum(s, VA_PYXM.l(i,s,r) * PY.l(i,s) * (tm(i,s,r)*(1+tx(i,s,r)))) ;

* ---- total
    armi_accounts(r,i,"total_input",yr)$a0(i,r)         =  armi_accounts(r,i,"domestic",yr) + armi_accounts(r,i,"imports",yr) + armi_accounts(r,i,"tax_transport",yr) +
                                                            armi_accounts(r,i,"tax_import_r",yr) + armi_accounts(r,i,"tax_import_s",yr);    

* ---- Investments
* ---- Revenues
    inv_accounts(r,"revenue",yr)    = VINV_PINV.l(r) * PINV.l(r);

* ---- Inputs
    inv_accounts(r,i,yr)            = VINV_PA.l(i,r) * PA.l(i,r);

* ---- Taxes
    inv_accounts(r,"tax_input",yr)  = sum(j,VINV_PA.l(j,r) * PA.l(j,r) * ti(j,"i",r));

* ---- total
    inv_accounts(r,"total",yr)      = sum(i, inv_accounts(r,i,yr)) + inv_accounts(r,"tax_input",yr);


* ---- Consumption - Single RA
* ---- total consumpiton
    cons_accounts(r,"total",yr)$(not HH_DISAG(r))           = VC_PC.l(r)  *  PC.l(r);

* ---- Inputs
    cons_accounts(r,i,yr)$(NOT HH_DISAG(r))                 = VC_PA.l(i,r)  *  PA.l(i,r);

* ---- CO2 payments
    cons_accounts(r,"CO2",yr)$(notrad(r) AND NOT HH_DISAG(r))                       =  VC_CO2.l(r) * PCO2.l(r); 
    cons_accounts(r,"CO2W",yr)$(pco2w_r(r) AND worldtrade AND NOT HH_DISAG(r))      =  VC_CO2W.l(r) * PCO2W.l; 
    cons_accounts(r,"CO2W",yr)$(eu28(r) AND eutrade AND NOT HH_DISAG(r))            =  VC_CO2W.l(r) * PCO2W.l; 
    cons_accounts(r,"CO2W",yr)$(worldtrade2 AND NOT HH_DISAG(r))                    =  VC_CO2W.l(r) * PCO2W.l; 
    cons_accounts(r,"CO2_NETS",yr)$(eu28(r) AND netstrade AND NOT HH_DISAG(r))      =  VC_CO2_NETS.l(r) * PCO2_NETS.l; 
    cons_accounts(r,"CO2_NETSr",yr)$(eu28(r) AND netstrade_r AND NOT HH_DISAG(r))   =  VC_CO2_NETSr.l(r) * PCO2_NETSr.l(r);
    cons_accounts(r,"co2_tax",yr)$(deu(r) AND carbon_tax_de  AND NOT HH_DISAG(r))   =  VC_CO2_tax.l(r) * Pcarbon_tax.l(r); 



* ---- Taxes
    cons_accounts(r,"tax_inputs",yr)$(NOT HH_DISAG(r))                                    =  sum(i, VC_PA.l(i,r)  *  PA.l(i,r)  *  tc(i,r));

* ---- Total
    cons_accounts(r,"total_inputs",yr)$(NOT HH_DISAG(r))          = sum(i, cons_accounts(r,i,yr)) + cons_accounts(r,"tax_inputs",yr) + cons_accounts(r,"CO2",yr) +
                                                              cons_accounts(r,"CO2W",yr) + cons_accounts(r,"CO2_NETS",yr) + cons_accounts(r,"CO2_NETSr",yr)
                                                              + cons_accounts(r,"co2_tax",yr)
                                                              ;

    cons_accounts(r,"total_no_tax",yr)$(NOT HH_DISAG(r))          = sum(i, cons_accounts(r,i,yr));


* ---- Consumption - Multiple HH
* ---- total consumpiton
    cons_hh_accounts(r,hh,"total",yr)$(HH_DISAG(r))           = VC_hh_PC.l(hh,r)  *  PC_hh.l(hh,r);

* ---- Inputs
    cons_hh_accounts(r,hh,i,yr)$(HH_DISAG(r))                 = VC_hh_PA.l(i,hh,r)  *  PA.l(i,r);

* ---- transportation
    cons_hh_accounts(r,hh,"oil_car",yr)$(HH_DISAG(r) AND h_t_cons_reg(r))       = VC_HH_p_oil_trans.L(hh,r)  *  p_oil_trans.l(r);
    cons_hh_accounts(r,hh,"ele_car",yr)$(HH_DISAG(r) AND h_t_cons_reg(r))       = VC_HH_p_ele_trans.L(hh,r)  *  p_ele_trans.l(r);

* ---- CO2 payments
    cons_hh_accounts(r,hh,"CO2",yr)$(notrad(r) AND HH_DISAG(r))                       =  VC_hh_CO2.l(hh,r) * PCO2.l(r); 
    cons_hh_accounts(r,hh,"CO2W",yr)$(pco2w_r(r) AND worldtrade AND HH_DISAG(r))      =  VC_hh_CO2W.l(hh,r) * PCO2W.l; 
    cons_hh_accounts(r,hh,"CO2W",yr)$(eu28(r) AND eutrade AND HH_DISAG(r))            =  VC_hh_CO2W.l(hh,r) * PCO2W.l; 
    cons_hh_accounts(r,hh,"CO2W",yr)$(worldtrade2 AND NOT HH_DISAG(r))                =  VC_hh_CO2W.l(hh,r) * PCO2W.l; 
    cons_hh_accounts(r,hh,"CO2_NETS",yr)$(eu28(r) AND netstrade AND HH_DISAG(r))      =  VC_hh_CO2_NETS.l(hh,r) * PCO2_NETS.l;  
    cons_hh_accounts(r,hh,"CO2_NETSr",yr)$(eu28(r) AND netstrade_r AND HH_DISAG(r))   =  VC_hh_CO2_NETSr.l(hh,r) * PCO2_NETSr.l(r); 
    cons_hh_accounts(r,hh,"CO2_inv_pay",yr)$(eu28(r) AND netstrade_r AND HH_DISAG(r))   =  VC_hh_CO2_inv_pay.l(hh,r) * PCO2_inv_pay.l(hh,r);
    cons_hh_accounts(r,hh,"co2_tax",yr)$(deu(r) AND carbon_tax_de AND HH_DISAG(r))   =  VC_hh_CO2_tax.l(hh,r) * Pcarbon_tax.l(r);

* ---- Taxes
    cons_hh_accounts(r,hh,"tax_inputs",yr)$(HH_DISAG(r))                = sum(i, VC_hh_PA.l(i,hh,r)  *  PA.l(i,r) * tp(i,r));
    cons_hh_accounts(r,hh,"tax_rebate",yr)$(HH_DISAG(r) and no_vat)     = sum(i$(sec_no_vat(i,r) AND no_vat), VC_hh_PA.l(i,hh,r)  *  PA.l(i,r) * tp(i,r)) * tax_reb.L(r);

* ---- Total
    cons_hh_accounts(r,hh,"total_inputs",yr)$(HH_DISAG(r))      = sum(i, cons_hh_accounts(r,hh,i,yr)) + cons_hh_accounts(r,hh,"tax_inputs",yr) + cons_hh_accounts(r,hh,"CO2",yr) +
                                                              cons_hh_accounts(r,hh,"CO2W",yr) + cons_hh_accounts(r,hh,"CO2_NETS",yr) + cons_hh_accounts(r,hh,"CO2_NETSr",yr)
                                                             + cons_hh_accounts(r,hh,"co2_tax",yr)
                                                              - cons_hh_accounts(r,hh,"tax_rebate",yr)$no_vat
                                                              + cons_hh_accounts(r,hh,"oil_car",yr)$h_t_cons_reg(r)
                                                              + cons_hh_accounts(r,hh,"ele_car",yr)$h_t_cons_reg(r);

* ---- Consumption - Government
* ---- total consumpiton
    cons_gov_accounts(r,"total",yr)$(HH_DISAG(r))           = VC_gov_PC.l(r)  *  PC_gov.l(r);

* ---- Inputs
    cons_gov_accounts(r,i,yr)$(HH_DISAG(r))                 = VC_gov_PA.l(i,r)  *  PA.l(i,r);

* ---- CO2 payments
    cons_gov_accounts(r,"CO2",yr)$(notrad(r) AND HH_DISAG(r))                       =  VC_gov_CO2.l(r) * PCO2.l(r); 
    cons_gov_accounts(r,"CO2W",yr)$(pco2w_r(r) AND worldtrade AND HH_DISAG(r))      =  VC_gov_CO2W.l(r) * PCO2W.l; 
    cons_gov_accounts(r,"CO2W",yr)$(eu28(r) AND eutrade AND HH_DISAG(r))            =  VC_gov_CO2W.l(r) * PCO2W.l; 
    cons_gov_accounts(r,"CO2W",yr)$(worldtrade2 AND NOT HH_DISAG(r))                =  VC_gov_CO2W.l(r) * PCO2W.l; 
    cons_gov_accounts(r,"CO2_NETS",yr)$(eu28(r) AND netstrade AND HH_DISAG(r))      =  VC_gov_CO2_NETS.l(r) * PCO2_NETS.l; 
    cons_gov_accounts(r,"CO2_NETSr",yr)$(eu28(r) AND netstrade_r AND HH_DISAG(r))   =  VC_gov_CO2_NETSr.l(r) * PCO2_NETSr.l(r);

* ---- Taxes
    cons_gov_accounts(r,"tax_inputs",yr)$(HH_DISAG(r))         = sum(i, VC_gov_PA.l(i,r)  *  PA.l(i,r) * tg(i,r));

* ---- Total
    cons_gov_accounts(r,"total_inputs",yr)$(HH_DISAG(r))      = sum(i, cons_gov_accounts(r,i,yr)) + cons_gov_accounts(r,"tax_inputs",yr) + cons_gov_accounts(r,"CO2",yr) +
                                                              cons_gov_accounts(r,"CO2W",yr) + cons_gov_accounts(r,"CO2_NETS",yr) + cons_gov_accounts(r,"CO2_NETSr",yr)
*                                                             + cons_gov_accounts(r,"CO2_SEC",yr)
                                                              ;

* ---- List of taxes per region and year

* ---- Consumption
    taxes_region("cons_RA",r,yr)$(NOT HH_DISAG(r))  = cons_accounts(r,"tax_inputs",yr);

    taxes_region("cons_HH",r,yr)$(HH_DISAG(r))      = sum(hh, cons_hh_accounts(r,hh,"tax_inputs",yr))
                                                      - sum(hh, cons_hh_accounts(r,hh,"tax_rebate",yr))$no_vat;

    taxes_region("cons_gov",r,yr)$(HH_DISAG(r))     = cons_gov_accounts(r,"tax_inputs",yr);

    taxes_region("cons_CO2",r,yr)                   = (cons_accounts(r,"CO2",yr) + cons_accounts(r,"CO2W",yr) + cons_accounts(r,"CO2_NETS",yr) + cons_accounts(r,"CO2_NETSr",yr) 
                                                      + cons_accounts(r,"co2_tax",yr)
                                                      )$(NOT HH_DISAG(r)) 
                                                      + sum(hh, cons_hh_accounts(r,hh,"CO2",yr) + cons_hh_accounts(r,hh,"CO2W",yr) + cons_hh_accounts(r,hh,"CO2_NETS",yr) + cons_hh_accounts(r,hh,"CO2_NETSr",yr)
                                                      + cons_hh_accounts(r,hh,"co2_tax",yr) + cons_hh_accounts(r,hh,"CO2_inv_pay",yr)
                                                      )$(HH_DISAG(r))
                                                      + (cons_gov_accounts(r,"CO2",yr) + cons_gov_accounts(r,"CO2W",yr) + cons_gov_accounts(r,"CO2_NETS",yr) + cons_gov_accounts(r,"CO2_NETSr",yr))$(HH_DISAG(r));  

* ---- Production
    taxes_region("prod_input",r,yr)                 = sum(i$(NOT ele(i)), prod_accounts(r,i,"tax_input",yr));
    taxes_region("prod_input","eu28",yr)            = sum(r$eu28(r), taxes_region("prod_input",r,yr));

    taxes_region("prod_output",r,yr)                = sum(i, prod_accounts(r,i,"tax_output",yr));
    taxes_region("prod_output","eu28",yr)           = sum(r$eu28(r), taxes_region("prod_output",r,yr));

    taxes_region("ele_rebate",r,yr)$diffrebate(r)   = prod_accounts(r,"ele","rebate",yr);
    taxes_region("ele_rebate","eu28",yr)            = sum(r$eu28(r), taxes_region("ele_rebate",r,yr));

    taxes_region("prod_CO2",r,yr)                   = sum(i, prod_accounts(r,i,"CO2",yr) + prod_accounts(r,i,"CO2W",yr) + prod_accounts(r,i,"CO2_NETS",yr) +  prod_accounts(r,i,"CO2_NETSr",yr) + prod_accounts(r,i,"CO2_ETS",yr));
    taxes_region("prod_CO2","eu28",yr)              = sum(r$eu28(r), taxes_region("prod_CO2",r,yr));

* ---- ELEx
    taxes_region("ELEx_input",r,yr)                 = sum(gen$(out_gen(gen,r) AND ks_x(gen,r)), elex_accounts(r,gen,"tax_input",yr));
    taxes_region("ELEx_input","eu28",yr)            = sum(r$eu28(r), taxes_region("ELEx_input",r,yr));

    taxes_region("ELEx_CO2",r,yr)                   = sum(gen, elex_accounts(r,gen,"CO2",yr) + elex_accounts(r,gen,"CO2W",yr) + elex_accounts(r,gen,"CO2_ETS",yr));
    taxes_region("ELEx_CO2","eu28",yr)           = sum(r$eu28(r), taxes_region("ELEx_CO2",r,yr));

* ---- ELEn
    taxes_region("ELEn_input",r,yr)                 = sum(gen$(out_gen(gen,r) AND ks_x(gen,r)), elen_accounts(r,gen,"tax_input",yr));
    taxes_region("ELEn_input","eu28",yr)           = sum(r$eu28(r), taxes_region("ELEn_input",r,yr));

    taxes_region("ELEn_diffcost",r,yr)              = sum(gen$(out_gen(gen,r) AND ks_x(gen,r) AND reg(gen)), elen_accounts(r,gen,"diffcost",yr));
    taxes_region("ELEn_diffcost","eu28",yr)         = sum(r$eu28(r), taxes_region("ELEn_diffcost",r,yr));

    taxes_region("ELEn_CO2",r,yr)                   = sum(gen, elen_accounts(r,gen,"CO2",yr) + elen_accounts(r,gen,"CO2W",yr) + elen_accounts(r,gen,"CO2_ETS",yr));
    taxes_region("ELEn_CO2","eu28",yr)               = sum(r$eu28(r), taxes_region("ELEn_CO2",r,yr));
* ---- Investments
    taxes_region("investments",r,yr)                = inv_accounts(r,"tax_input",yr);
    taxes_region("investments","eu28",yr)           = sum(r$eu28(r), taxes_region("investments",r,yr));

* ---- Armington
    taxes_region("transport",r,yr)                  = sum(i,armi_accounts(r,i,"tax_transport",yr));
    taxes_region("transport","eu28",yr)             = sum(r$eu28(r), taxes_region("transport",r,yr));

    taxes_region("imports",r,yr)                    = sum(i, armi_accounts(r,i,"tax_import_r",yr));
    taxes_region("imports","eu28",yr)           = sum(r$eu28(r), taxes_region("imports",r,yr));

* ---- Production Factors by firms
    taxes_region("Capital",r,yr)$(HH_DISAG(r) AND diss_factor_tax)              = sum(i,prod_accounts(r,i,"tax_capital",yr)) + sum(gen, elex_accounts(r,gen,"tax_capital",yr) + elen_accounts(r,gen,"tax_capital",yr));
    taxes_region("Capital","eu28",yr)$(diss_factor_tax)                     = sum(r$eu28(r), taxes_region("Capital",r,yr));

    taxes_region("SKL",r,yr)$(HH_DISAG(r) AND diss_factor_tax)                  = sum(i,prod_accounts(r,i,"tax_SKL",yr)) + sum(gen, elex_accounts(r,gen,"tax_skl",yr) + elen_accounts(r,gen,"tax_skl",yr));
    taxes_region("SKL","eu28",yr)$(diss_factor_tax)                         = sum(r$eu28(r), taxes_region("SKL",r,yr));

    taxes_region("USK",r,yr)$(HH_DISAG(r) AND diss_factor_tax)                  = sum(i,prod_accounts(r,i,"tax_USK",yr)) + sum(gen, elex_accounts(r,gen,"tax_usk",yr) + elen_accounts(r,gen,"tax_usk",yr));
    taxes_region("USK","eu28",yr)$(diss_factor_tax)                         = sum(r$eu28(r), taxes_region("USK",r,yr));

* ---- Sum
    taxes_region("sum_CO2",r,yr)        =       taxes_region("cons_CO2",r,yr) + taxes_region("prod_CO2",r,yr) + taxes_region("ELEx_CO2",r,yr) + taxes_region("ELEn_CO2",r,yr);
    taxes_region("sum_CO2","eu28",yr)   =       sum(r$eu28(r), taxes_region("sum_CO2",r,yr));

    taxes_region("sum_parts",r,yr)  =       taxes_region("cons_RA",r,yr) + taxes_region("cons_HH",r,yr) + taxes_region("cons_gov",r,yr) + taxes_region("cons_CO2",r,yr) +  
                                            taxes_region("prod_input",r,yr) + taxes_region("prod_output",r,yr) + taxes_region("ele_rebate",r,yr) + taxes_region("prod_CO2",r,yr) +
                                            taxes_region("ELEx_CO2",r,yr) + taxes_region("ELEx_input",r,yr) +
                                            taxes_region("ELEn_input",r,yr) + taxes_region("ELEn_diffcost",r,yr) + taxes_region("ELEn_CO2",r,yr) +
                                            taxes_region("investments",r,yr) + taxes_region("transport",r,yr) + taxes_region("imports",r,yr) + 
                                           ( taxes_region("Capital",r,yr) + taxes_region("SKL",r,yr) +  taxes_region("USK",r,yr))$(HH_DISAG(r) AND diss_factor_tax);               
    taxes_region("sum_parts","eu28",yr) =   sum(r$eu28(r), taxes_region("sum_parts",r,yr));

* ---- GDP Composites
    gdpreal_disag_yr("trade",r,yr)      = trdblnc_yr(r,"total",yr);
    
    gdpreal_disag_yr("cons_HH",r,yr)$HH_DISAG(r)    = sum(hh, abs_sector_hh_yr(r,"total",hh,yr));
    gdpreal_disag_yr("inv",r,yr)$HH_DISAG(r)        = sum(hh, vd_pinv_hh_yr(hh,r,yr) * pinv_yr(r,yr));    
    gdpreal_disag_yr("cons_gov",r,yr)$HH_DISAG(r)   = gdpreal_yr(r,yr) - gdpreal_disag_yr("cons_HH",r,yr) - gdpreal_disag_yr("inv",r,yr) - gdpreal_disag_yr("trade",r,yr);

    gdpreal_disag_yr("cons",r,yr)$(NOT HH_DISAG(r))    = (VC_PC.L(r) * PC.L(r));
    gdpreal_disag_yr("inv",r,yr)$(NOT HH_DISAG(r))        = (VINV_PINV.L(r) * PINV.L(r));

    gdpreal_disag_yr("total",r,yr)      = gdpreal_yr(r,yr);
    gdpreal_disag_yr("total_no_cons_tax",r,yr)      = gdpreal_yr(r,yr) 
                                                      - sum(hh, cons_hh_accounts(r,hh,"tax_inputs",yr))
                                                      + sum(hh, cons_hh_accounts(r,hh,"tax_rebate",yr))$no_vat
                                                      ;
    
    gdpreal_disag_yr("trade", "EU28", yr) = sum(r$eu28(r), gdpreal_disag_yr("trade", r, yr));
    gdpreal_disag_yr("inv", "EU28", yr) = sum(r$eu28(r), gdpreal_disag_yr("inv", r, yr));
    gdpreal_disag_yr("total", "EU28", yr) = sum(r$eu28(r), gdpreal_disag_yr("total", r, yr));
    gdpreal_disag_yr("total_no_cons_tax", "EU28", yr) = sum(r$eu28(r), gdpreal_disag_yr("total_no_cons_tax", r, yr));
    
    gdpreal_disag_yr("cons", "EU28", yr) = sum(r$(eu28(r) AND NOT HH_DISAG(r)), gdpreal_disag_yr("cons", r, yr));
    
    gdpreal_disag_yr("cons_gov", "EU28", yr) = sum(r$(eu28(r) AND HH_DISAG(r)), gdpreal_disag_yr("cons_gov", r, yr));
    gdpreal_disag_yr("cons_HH", "EU28", yr) = sum(r$(eu28(r)  AND HH_DISAG(r)), gdpreal_disag_yr("cons_HH", r, yr));
     
c_hh_yr(r,hh,yr)$HH_DISAG(r) = c_hh.l(hh,r);


* ---- Share of GDP for CO2 payments
    share_co2_gdp(r,"consumpiton",yr)       =  taxes_region("cons_CO2",r,yr) / gdpreal_disag_yr("total", r, yr);
    share_co2_gdp("eu28","consumpiton",yr)  =  taxes_region("cons_CO2","eu28",yr) / gdpreal_disag_yr("total", "eu28", yr);

    share_co2_gdp(r,"production",yr)        =  taxes_region("prod_CO2",r,yr) / gdpreal_disag_yr("total", r, yr);
    share_co2_gdp("eu28","production",yr)   =  taxes_region("prod_CO2","eu28",yr) / gdpreal_disag_yr("total", "eu28", yr);

    share_co2_gdp(r,"electricity",yr)       =  (taxes_region("ELEx_CO2",r,yr) + taxes_region("ELEn_CO2",r,yr)) / gdpreal_disag_yr("total", r, yr);
    share_co2_gdp("eu28","electricity",yr)  =  (taxes_region("ELEx_CO2","eu28",yr) + taxes_region("ELEn_CO2","eu28",yr)) / gdpreal_disag_yr("total", "eu28", yr);

    share_co2_gdp(r,"total",yr)             =  taxes_region("sum_CO2",r,yr) / gdpreal_disag_yr("total", r, yr);
    share_co2_gdp("eu28","total",yr)        =  taxes_region("sum_CO2","eu28",yr) / gdpreal_disag_yr("total", "eu28", yr);


* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ Competitiveness  ------------------------------------------------------
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
rwa_yr(r,i,yr)$sum(s, ex_yr(s,i,yr)) = (ex_yr(r,i,yr) / sum(s, ex_yr(s,i,yr)))/ (sum(j, ex_yr(r,j,yr)) / sum((j,s), ex_yr(s,j,yr))) ; // wie in vogel(2000) - technologiewettlauf...
rca_yr(r,i,yr)$im_yr(r,i,yr)         = (ex_yr(r,i,yr) /        im_yr(r,i,yr)) / (sum(j, ex_yr(r,j,yr)) / sum(j,     im_yr(r,j,yr))) ; // wie in vogel(2000) - technologiewettlauf...

* ------ EU28
rwa_yr("EU28",i,yr)$sum(s, ex_yr(s,i,yr))= (ex_yr("EU28",i,yr) / sum(s, ex_yr(s,i,yr)))     / (sum(j, ex_yr("EU28",j,yr)) / sum((j,s), ex_yr(s,j,yr))) ;         // wie in vogel(2000) - technologiewettlauf...
rca_yr("EU28",i,yr)$im_yr("EU28",i,yr)   = (ex_yr("EU28",i,yr) /        im_yr("EU28",i,yr)) / (sum(j, ex_yr("EU28",j,yr)) / sum(j,     im_yr("EU28",j,yr))) ;    // wie in vogel(2000) - technologiewettlauf...
*rwa_yr("BRD",i,yr)$sum(s, ex_yr(s,i,yr))= (ex_yr("BRD",i,yr) / sum(s, ex_yr(s,i,yr)))     / (sum(j, ex_yr("BRD",j,yr)) / sum((j,s), ex_yr(s,j,yr))) ;         // wie in vogel(2000) - technologiewettlauf...
*rca_yr("BRD",i,yr)$im_yr("BRD",i,yr)   = (ex_yr("BRD",i,yr) /        im_yr("BRD",i,yr)) / (sum(j, ex_yr("BRD",j,yr)) / sum(j,     im_yr("BRD",j,yr))) ;    // wie in vogel(2000) - technologiewettlauf...
*RCA(R,I,YR)$VIM_I(i,r,yr)        = (VXM_I(i,r,yr)/VIM_I(i,R,YR)) / (VXM_I_GES(r,yr)/VIM_I_GES(r,yr));
*RWA(r,i,yr)$VXM_R_GES(i,yr)      = (VXM_I(i,r,yr)/VXM_R_GES(i,yr))/(VXM_I_GES(r,yr)/W_EX(yr));  // Wie in Vogel(2000) - Technologiewettlauf...

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ Electricity  ----------------------------------------------------------
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
velex_yr(r,gen,yr)        = VELEx_PGEN.L(gen,r);
velex_yr(r,"total",yr)    = sum(gen, velex_yr(r,gen,yr));
velen_yr(r,gen,yr)        = VELEn_PGEN.L(gen,r);
velen_yr(r,"total",yr)    = sum(gen, velen_yr(r,gen,yr));
vele_yr(r,gen,yr)         = velex_yr(r,gen,yr) + velen_yr(r,gen,yr) ;
vele_yr(r,"total",yr)     = sum(gen, vele_yr(r,gen,yr));

eleXtwh_yr(r,gen,yr)     = eps + ELEx.L(gen,r) * ele_prod(gen,r);
eleNtwh_yr(r,gen,yr)     = eps + ELEn.L(gen,r) * ele_prod(gen,r);
ele_twh_yr(r,gen,yr)     = eps + (ELEx.L(gen,r) + ELEn.L(gen,r)) * ele_prod(gen,r);

* ------ 25.02.2015 Bei CCS Dummy-Wert im Benchmark abziehen
eleXtwh_yr(r,gen,yr)$(fosccs(gen)) = eleXtwh_yr(r,gen,yr) - thetax       * ele_prod(gen,r);
eleNtwh_yr(r,gen,yr)$(fosccs(gen)) = eleNtwh_yr(r,gen,yr) - (1-thetax)   * ele_prod(gen,r);
ele_twh_yr(r,gen,yr)$(fosccs(gen)) = ele_twh_yr(r,gen,yr) -                ele_prod(gen,r);
eleXtwh_yr(r,gen,yr)$(fosccs(gen) and eleXtwh_yr(r,gen,yr) lt 0) = 0;
eleNtwh_yr(r,gen,yr)$(fosccs(gen) and eleNtwh_yr(r,gen,yr) lt 0) = 0;
ele_twh_yr(r,gen,yr)$(fosccs(gen) and ele_twh_yr(r,gen,yr) lt 0) = 0;

eleXtwh_yr(r,"total",yr) = sum(gen, eleXtwh_yr(r,gen,yr));
eleNtwh_yr(r,"total",yr) = sum(gen, eleNtwh_yr(r,gen,yr));
ele_twh_yr(r,"total",yr) = sum(gen, ele_twh_yr(r,gen,yr));
eleXtwh_yr("EU28",gen,yr)        = sum(r$eu28(r), eleXtwh_yr(r,gen,yr));
eleNtwh_yr("EU28",gen,yr)        = sum(r$eu28(r), eleNtwh_yr(r,gen,yr));
ele_twh_yr("EU28",gen,yr)        = sum(r$eu28(r), ele_twh_yr(r,gen,yr));
eleXtwh_yr("EU28","total",yr)    = sum((gen,r)$eu28(r), eleXtwh_yr(r,gen,yr));
eleNtwh_yr("EU28","total",yr)    = sum((gen,r)$eu28(r), eleNtwh_yr(r,gen,yr));
ele_twh_yr("EU28","total",yr)    = sum((gen,r)$eu28(r), ele_twh_yr(r,gen,yr));
*eleXtwh_yr("BRD",gen,yr)         = sum(r$bawdeu(r), eleXtwh_yr(r,gen,yr));
*eleNtwh_yr("BRD",gen,yr)         = sum(r$bawdeu(r), eleNtwh_yr(r,gen,yr));
*ele_twh_yr("BRD",gen,yr)         = sum(r$bawdeu(r), ele_twh_yr(r,gen,yr));
*eleXtwh_yr("BRD","total",yr)     = sum((gen,r)$bawdeu(r), eleXtwh_yr(r,gen,yr));
*eleNtwh_yr("BRD","total",yr)     = sum((gen,r)$bawdeu(r), eleNtwh_yr(r,gen,yr));
*ele_twh_yr("BRD","total",yr)     = sum((gen,r)$bawdeu(r), ele_twh_yr(r,gen,yr));

elecontwh_yr(r,gen,yr)           = ele_twh_yr(r,gen,yr) ;
elecontwh_yr(r,"NetImp",yr)      = (- trdblnc_yr(r,"ele",yr)) + trdblnc_yr(r,"ele","2011") ;
elecontwh_yr(r,"total",yr)       = ele_twh_yr(r,"total",yr) + elecontwh_yr(r,"NetImp",yr) ;
elecontwh_yr("EU28",gen,yr)      = ele_twh_yr("EU28",gen,yr) ;
elecontwh_yr("EU28","NetImp",yr) = (- trdblnc_yr("EU28","ele",yr)) + trdblnc_yr("EU28","ele","2011");
elecontwh_yr("EU28","total",yr)  = ele_twh_yr("EU28","total",yr) + elecontwh_yr("EU28","NetImp",yr) ;
*elecontwh_yr("BRD",gen,yr)       = ele_twh_yr("BRD",gen,yr) ;
*elecontwh_yr("BRD","NetImp",yr)  = (- trdblnc_yr("BRD","ele",yr)) + trdblnc_yr("BRD","ele","2011");
*elecontwh_yr("BRD","total",yr)   = ele_twh_yr("BRD","total",yr) + elecontwh_yr("BRD","NetImp",yr) ;

elecontwh2_yr(r,"Oil",yr)        = sum(gen$fosoil(gen), elecontwh_yr(r,gen,yr)) ;
elecontwh2_yr(r,"Gas",yr)        = sum(gen$fosgas(gen), elecontwh_yr(r,gen,yr)) ;
elecontwh2_yr(r,"Coal",yr)       = sum(gen$foscoalh(gen), elecontwh_yr(r,gen,yr)) ;
elecontwh2_yr(r,"CCS",yr)        = sum(gen$fosccs(gen), elecontwh_yr(r,gen,yr)) ;
elecontwh2_yr(r,"Hydro",yr)      = sum(gen$hydro(gen), elecontwh_yr(r,gen,yr)) ;
elecontwh2_yr(r,"Lignite",yr)    = elecontwh_yr(r,"bBC",yr)      ;
elecontwh2_yr(r,"Nuclear",yr)    = elecontwh_yr(r,"bNUC",yr)     ;
elecontwh2_yr(r,"Wind",yr)       = elecontwh_yr(r,"mWIND",yr)    ;
elecontwh2_yr(r,"Solar",yr)      = elecontwh_yr(r,"mSOLAR",yr)   ;
elecontwh2_yr(r,"Biomass",yr)    = elecontwh_yr(r,"bBIO",yr)     ;
elecontwh2_yr(r,"Geo",yr)        = elecontwh_yr(r,"bGEO",yr)     ;
elecontwh2_yr(r,"NetImp",yr)     = elecontwh_yr(r,"NetImp",yr)   ;
elecontwh2_yr(r,"total",yr)      = elecontwh_yr(r,"total",yr)    ;

elecontwh2_yr("EU28","Oil",yr)    = sum(r$eu28(r), elecontwh2_yr(r,"Oil",yr))    ;
elecontwh2_yr("EU28","Gas",yr)    = sum(r$eu28(r), elecontwh2_yr(r,"Gas",yr))    ;
elecontwh2_yr("EU28","Coal",yr)   = sum(r$eu28(r), elecontwh2_yr(r,"Coal",yr))   ;
elecontwh2_yr("EU28","CCS",yr)    = sum(r$eu28(r), elecontwh2_yr(r,"CCS",yr))    ;
elecontwh2_yr("EU28","Hydro",yr)  = sum(r$eu28(r), elecontwh2_yr(r,"Hydro",yr))  ;
elecontwh2_yr("EU28","Lignite",yr)= sum(r$eu28(r), elecontwh2_yr(r,"Lignite",yr));
elecontwh2_yr("EU28","Nuclear",yr)= sum(r$eu28(r), elecontwh2_yr(r,"Nuclear",yr));
elecontwh2_yr("EU28","Wind",yr)   = sum(r$eu28(r), elecontwh2_yr(r,"Wind",yr))   ;
elecontwh2_yr("EU28","Solar",yr)  = sum(r$eu28(r), elecontwh2_yr(r,"Solar",yr))  ;
elecontwh2_yr("EU28","Biomass",yr)= sum(r$eu28(r), elecontwh2_yr(r,"Biomass",yr));
elecontwh2_yr("EU28","Geo",yr)    = sum(r$eu28(r), elecontwh2_yr(r,"Geo",yr))    ;
elecontwh2_yr("EU28","NetImp",yr) = sum(r$eu28(r), elecontwh2_yr(r,"NetImp",yr)) ;
elecontwh2_yr("EU28","total",yr)  = sum(r$eu28(r), elecontwh2_yr(r,"total",yr))  ;

*elecontwh2_yr("BRD","Oil",yr)    = sum(r$bawdeu(r), elecontwh2_yr(r,"Oil",yr))    ;
*elecontwh2_yr("BRD","Gas",yr)    = sum(r$bawdeu(r), elecontwh2_yr(r,"Gas",yr))    ;
*elecontwh2_yr("BRD","Coal",yr)   = sum(r$bawdeu(r), elecontwh2_yr(r,"Coal",yr))   ;
*elecontwh2_yr("BRD","CCS",yr)    = sum(r$bawdeu(r), elecontwh2_yr(r,"CCS",yr))    ;
*elecontwh2_yr("BRD","Hydro",yr)  = sum(r$bawdeu(r), elecontwh2_yr(r,"Hydro",yr))  ;
*elecontwh2_yr("BRD","Lignite",yr)= sum(r$bawdeu(r), elecontwh2_yr(r,"Lignite",yr));
*elecontwh2_yr("BRD","Nuclear",yr)= sum(r$bawdeu(r), elecontwh2_yr(r,"Nuclear",yr));
*elecontwh2_yr("BRD","Wind",yr)   = sum(r$bawdeu(r), elecontwh2_yr(r,"Wind",yr))   ;
*elecontwh2_yr("BRD","Solar",yr)  = sum(r$bawdeu(r), elecontwh2_yr(r,"Solar",yr))  ;
*elecontwh2_yr("BRD","Biomass",yr)= sum(r$bawdeu(r), elecontwh2_yr(r,"Biomass",yr));
*elecontwh2_yr("BRD","Geo",yr)    = sum(r$bawdeu(r), elecontwh2_yr(r,"Geo",yr))    ;
*elecontwh2_yr("BRD","NetImp",yr) = sum(r$bawdeu(r), elecontwh2_yr(r,"NetImp",yr)) ;
*elecontwh2_yr("BRD","total",yr)  = sum(r$bawdeu(r), elecontwh2_yr(r,"total",yr))  ;

* ------ 31.07.2014
eletwhyr("XTANT",r,gen,yr)       =               elextwh_yr(r,gen,yr) ;
eletwhyr("Capital",r,gen,yr)     =               elextwh_yr(r,gen,"2011") * abschr_pfad(r,gen,yr) ;
eletwhyr("NEW",r,gen,yr)         =               elentwh_yr(r,gen,yr) ;
eletwhyr("X+N",r,gen,yr)         =               ele_twh_yr(r,gen,yr) ;
eletwhyr("XTANT",r,"total",yr)   = sum(gen,      eletwhyr("XTANT",r,gen,yr))  ;
eletwhyr("Capital",r,"total",yr) = sum(gen,      eletwhyr("Capital",r,gen,yr))  ;
eletwhyr("NEW",r,"total",yr)     = sum(gen,      eletwhyr("NEW",r,gen,yr))  ;
eletwhyr("X+N",r,"total",yr)     = sum(gen,      eletwhyr("X+N",r,gen,yr));
eletwhyr("XTANT","EU28",gen,yr)  = sum(r$eu28(r),eletwhyr("XTANT",r,gen,yr))     ;
eletwhyr("Capital","EU28",gen,yr)= sum(r$eu28(r),eletwhyr("Capital",r,gen,yr))     ;
eletwhyr("NEW","EU28",gen,yr)    = sum(r$eu28(r),eletwhyr("NEW",r,gen,yr))     ;
eletwhyr("X+N","EU28",gen,yr)    = sum(r$eu28(r),eletwhyr("X+N",r,gen,yr))   ;
eletwhyr("XTANT","EU28","total",yr)=sum(r$eu28(r),eletwhyr("XTANT",r,"total",yr))    ;
eletwhyr("Capital","EU28","total",yr)=sum(r$eu28(r),eletwhyr("Capital",r,"total",yr))    ;
eletwhyr("NEW","EU28","total",yr)= sum(r$eu28(r),eletwhyr("NEW",r,"total",yr))    ;
eletwhyr("X+N","EU28","total",yr)= sum(r$eu28(r),eletwhyr("X+N",r,"total",yr))  ;

*eletwhyr("XTANT","BRD",gen,yr)  = sum(r$bawdeu(r),eletwhyr("XTANT",r,gen,yr))     ;
*eletwhyr("Capital","BRD",gen,yr)= sum(r$bawdeu(r),eletwhyr("Capital",r,gen,yr))     ;
*eletwhyr("NEW","BRD",gen,yr)    = sum(r$bawdeu(r),eletwhyr("NEW",r,gen,yr))     ;
*eletwhyr("X+N","BRD",gen,yr)    = sum(r$bawdeu(r),eletwhyr("X+N",r,gen,yr))   ;
*eletwhyr("XTANT","BRD","total",yr)=sum(r$bawdeu(r),eletwhyr("XTANT",r,"total",yr))    ;
*eletwhyr("Capital","BRD","total",yr)=sum(r$bawdeu(r),eletwhyr("Capital",r,"total",yr))    ;
*eletwhyr("NEW","BRD","total",yr)= sum(r$bawdeu(r),eletwhyr("NEW",r,"total",yr))    ;
*eletwhyr("X+N","BRD","total",yr)= sum(r$bawdeu(r),eletwhyr("X+N",r,"total",yr))  ;

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ CO2 constraints
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
carblim_yr(r,yr)         = carblim(r);
carblim_yr("EU28",yr)    = sum(r$eu28(r), carblim(r));
carblim_ets_yr(r,yr)     = carblim_ets(r);
carblim_ets_yr("EU28",yr)= sum(r$eu28(r), carblim_ets(r));
carblim_sec_yr(sec,r,yr)$carblim_sec(sec,r) = carblim_sec(sec,r);
*carblim_yr("BRD",yr)     = sum(r$bawdeu(r), carblim(r));
*carblim_ets_yr("BRD",yr) = sum(r$bawdeu(r), carblim_ets(r));

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ CO2 Emissions
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ 03.07.2014

*co2ets_yr(r,yr)$etstrade         = sum(i, VY_CO2_ETS.L(i,r)) + sum(gen, VELEx_CO2_ETS.L(gen,r) + VELEn_CO2_ETS.L(gen,r)) ;

co2ets_y(r,yr)$etstrade          = sum(i, VY_CO2_ETS.L(i,r));
co2ets_elex(r,yr)$etstrade       = sum(gen, VELEx_CO2_ETS.L(gen,r));
co2ets_elen(r,yr)$etstrade       = sum(gen, VELEn_CO2_ETS.L(gen,r));


co2ets_yr(r,yr)$(eu28(r) and worldtrade or eutrade and not etstrade)    = sum(i$ets(i), VY_CO2W.L(i,r)) + sum(gen, VELEx_CO2W.L(gen,r) + VELEn_CO2W.L(gen,r));
co2ets_y(r,yr)$(eu28(r) and worldtrade or eutrade and not etstrade)     = sum(i$ets(i), VY_CO2W.L(i,r));
co2ets_elex(r,yr)$(eu28(r) and worldtrade or eutrade and not etstrade)  = sum(gen, VELEx_CO2W.L(gen,r));
co2ets_elen(r,yr)$(eu28(r) and worldtrade or eutrade and not etstrade)  = sum(gen, VELEn_CO2W.L(gen,r));

co2ets_yr(r,yr)$notrad(r)        = sum(i, VY_CO2.L(i,r)) + sum(gen, VELEx_CO2.L(gen,r) + VELEn_CO2.L(gen,r));
co2ets_y(r,yr)$notrad(r)         = sum(i, VY_CO2.L(i,r));
co2ets_elex(r,yr)$notrad(r)      = sum(gen, VELEx_CO2.L(gen,r));
co2ets_elen(r,yr)$notrad(r)      = sum(gen, VELEn_CO2.L(gen,r));

co2ets_yr(r,yr)$(eu28(r) and worldtrade2 and not etstrade)   = sum(i$ets(i), VY_CO2W.L(i,r)) + sum(gen, VELEx_CO2W.L(gen,r) + VELEn_CO2W.L(gen,r));
co2ets_y(r,yr)$(eu28(r) and worldtrade2 and not etstrade)    = sum(i$ets(i), VY_CO2W.L(i,r));
co2ets_elex(r,yr)$(eu28(r) and worldtrade2 and not etstrade) = sum(gen, VELEx_CO2W.L(gen,r));
co2ets_elen(r,yr)$(eu28(r) and worldtrade2 and not etstrade) = sum(gen, VELEn_CO2W.L(gen,r));

co2ets_ele(r,yr) = co2ets_elex(r,yr) + co2ets_elen(r,yr);

co2ets_yr("EU28",yr)     = sum(r, co2ets_yr(r,yr));
co2ets_y("EU28",yr)      = sum(r, co2ets_y(r,yr));
co2ets_elex("EU28",yr)   = sum(r, co2ets_elex(r,yr));
co2ets_elen("EU28",yr)   = sum(r, co2ets_elen(r,yr));
co2ets_ele("EU28",yr)    = sum(r, co2ets_ele(r,yr));

*co2ets_yr("BRD",yr)      = sum(r$bawdeu(r), co2ets_yr(r,yr));
*co2ets_y("BRD",yr)       = sum(r$bawdeu(r), co2ets_y(r,yr));
*o2ets_elex("BRD",yr)    = sum(r$bawdeu(r), co2ets_elex(r,yr));
*co2ets_elen("BRD",yr)    = sum(r$bawdeu(r), co2ets_elen(r,yr));
*co2ets_ele("BRD",yr)     = sum(r$bawdeu(r), co2ets_ele(r,yr));

* ------ 5.05.2015 Include or exclude AEEI??
co2c_yr(fe,r,yr)$(co2coefc(fe,r) AND NOT HH_DISAG(r))     = VC_PA.L(fe,r) / co2coefc(fe,r)  ;
co2c_yr(fe,r,yr)$(co2coefc(fe,r) AND HH_DISAG(r))         = ( sum(hh,VC_hh_PA.L(fe,hh,r)) + VC_gov_PA.L(fe,r)) / co2coefc(fe,r)  ;
co2c_yr("total",r,yr)                    = sum(fe,       co2c_yr(fe,r,yr)) ;
co2c_yr(fe,"EU28",yr)                    = sum(r$eu28(r),co2c_yr(fe,r,yr)) ;
co2c_yr("total","EU28",yr)               = sum(r$eu28(r),co2c_yr("total",r,yr));
co2c_yr(fe,"World",yr)                   = sum(r,        co2c_yr(fe,r,yr)) ;
co2c_yr("total","World",yr)              = sum(r,        co2c_yr("total",r,yr));

co2y_yr(fe,i,r,yr)$co2coefy(fe,i,r)      = VY_PA.L(fe,i,r) / co2coefy(fe,i,r);
co2y_yr("total",i,r,yr)                  = sum(fe,       co2y_yr(fe,i,r,yr)) ;
co2y_yr(fe,"total",r,yr)                 = sum(i,        co2y_yr(fe,i,r,yr)) ;
co2y_yr("total","total",r,yr)            = sum((fe,i),   co2y_yr(fe,i,r,yr)) ;
co2y_yr(fe,i,"EU28",yr)                  = sum(r$eu28(r),co2y_yr(fe,i,r,yr)) ;
co2y_yr("total",i,"EU28",yr)             = sum(r$eu28(r),co2y_yr("total",i,r,yr)) ;
co2y_yr(fe,"total","EU28",yr)            = sum(r$eu28(r),co2y_yr(fe,"total",r,yr)) ;
co2y_yr("total","total","EU28",yr)       = sum(r$eu28(r),co2y_yr("total","total",r,yr));
co2y_yr(fe,i,"World",yr)                 = sum(r,        co2y_yr(fe,i,r,yr)) ;
co2y_yr("total",i,"World",yr)            = sum(r,        co2y_yr("total",i,r,yr)) ;
co2y_yr(fe,"total","World",yr)           = sum(r,        co2y_yr(fe,"total",r,yr)) ;
co2y_yr("total","total","World",yr)      = sum(r,        co2y_yr("total","total",r,yr));

co2ele_yr(fe,gen,r,yr)$co2coefele(fe,gen,r) = ( VELEX_PA.L(fe,gen,r)  + VELEN_PA.L(fe,gen,r) ) / co2coefele(fe,gen,r) ;
co2ele_yr("total",gen,r,yr)              = sum(fe,       co2ele_yr(fe,gen,r,yr)) ;
co2ele_yr(fe,"total",r,yr)               = sum(gen,      co2ele_yr(fe,gen,r,yr)) ;
co2ele_yr("total","total",r,yr)          = sum((fe,gen), co2ele_yr(fe,gen,r,yr)) ;
co2ele_yr(fe,gen,"EU28",yr)              = sum(r$eu28(r),co2ele_yr(fe,gen,r,yr)) ;
co2ele_yr("total",gen,"EU28",yr)         = sum(r$eu28(r),co2ele_yr("total",gen,r,yr)) ;
co2ele_yr(fe,"total","EU28",yr)          = sum(r$eu28(r),co2ele_yr(fe,"total",r,yr) ) ;
co2ele_yr("total","total","EU28",yr)     = sum(r$eu28(r),co2ele_yr("total","total",r,yr));
co2ele_yr(fe,gen,"World",yr)             = sum(r,        co2ele_yr(fe,gen,r,yr)) ;
co2ele_yr("total",gen,"World",yr)        = sum(r,        co2ele_yr("total",gen,r,yr)) ;
co2ele_yr(fe,"total","World",yr)         = sum(r,        co2ele_yr(fe,"total",r,yr) ) ;
co2ele_yr("total","total","World",yr)    = sum(r,        co2ele_yr("total","total",r,yr));


*co2c_yr(fe,r,yr)$co2coefc(fe,r)          = VC_PA.L(fe,r) * aeei(fe,"c",r) / co2coefc(fe,r)  ;
*co2y_yr(fe,i,r,yr)$co2coefy(fe,i,r)      = VY_PAY.L(fe,i,r) * aeei(fe,i,r) / co2coefy(fe,i,r);
*co2ele_yr(fe,gen,r,yr)$co2coefele(fe,gen,r) = ( (VELEX_PA.L(fe,gen,r) * aeei_elex(gen,r)) + (VELEN_PA.L(fe,gen,r) * aeei_elen(gen,r)) ) / co2coefele(fe,gen,r) ;

* ------ Overall CO2-Emissions
co2_yr(fe,r,yr)          = co2c_yr(fe,r,yr) + sum(i, co2y_yr(fe,i,r,yr)) + sum(gen, co2ele_yr(fe,gen,r,yr)) ;
co2_yr("total",r,yr)     = sum(fe,       co2_yr(fe,r,yr)) ;
co2_yr(fe,"EU28",yr)                     = sum(r$eu28(r),   co2_yr(fe,r,yr));
co2_yr("total","EU28",yr)                = sum(r$eu28(r),   co2_yr("total",r,yr));
co2_yr(fe,"World",yr)                    = sum(r,   co2_yr(fe,r,yr));
co2_yr("total","World",yr)               = sum(r,   co2_yr("total",r,yr));

*co2c_yr(fe,"BRD",yr)                     = sum(r$bawdeu(r), co2c_yr(fe,r,yr)) ;
*co2hhsum_yr(fe,"BRD",yr)                 = sum(r$bawdeu(r), co2hhsum_yr(fe,r,yr)) ;
*co2y_yr(fe,i,"BRD",yr)                   = sum(r$bawdeu(r), co2y_yr(fe,i,r,yr)) ;
*co2ele_yr(fe,gen,"BRD",yr)               = sum(r$bawdeu(r), co2ele_yr(fe,gen,r,yr)) ;
*co2_yr(fe,"BRD",yr)                      = sum(r$bawdeu(r), co2_yr(fe,r,yr));
*co2c_yr("total","BRD",yr)                = sum(r$bawdeu(r), co2c_yr("total",r,yr));
*co2hhsum_yr("total","BRD",yr)            = sum(r$bawdeu(r), co2hhsum_yr("total",r,yr));
*co2y_yr("total","total","BRD",yr)        = sum(r$bawdeu(r), co2y_yr("total","total",r,yr));
*co2ele_yr("total","total","BRD",yr)      = sum(r$bawdeu(r), co2ele_yr("total","total",r,yr));
*co2_yr("total","BRD",yr)                 = sum(r$bawdeu(r), co2_yr("total",r,yr));

* ------ 17.02.2016 Summarize all CO2-Emissions parameter in one report parameter
co2all_yr(r,"Cars",yr)           = 0 ;
co2all_yr(r,"Buildings",yr)      = 0 ;
co2all_yr(r,"C",yr)              = co2c_yr("total",r,yr)   ;
co2all_yr(r,i,yr)                = co2y_yr("total",i,r,yr) ;
co2all_yr(r,"ELEx+n",yr)         = co2ele_yr("total","total",r,yr) ;
co2all_yr(r,"Y",yr)              = sum(i, co2y_yr("total",i,r,yr)) + co2all_yr(r,"ELEx+n",yr) ;
co2all_yr(r,"ETS",yr)            = sum(i$ets(i), co2y_yr("total",i,r,yr)) + co2ele_yr("total","total",r,yr) ;
co2all_yr(r,"EII",yr)            = sum(i$eii(i), co2y_yr("total",i,r,yr));
co2all_yr(r,"IND",yr)            = sum(i$ind(i), co2y_yr("total",i,r,yr));
co2all_yr(r,"IND+oil",yr)        = sum(i$ind(i), co2y_yr("total",i,r,yr)) + co2y_yr("total","oil",r,yr);
co2all_yr("EU28","IND",yr)       = sum(r$eu28(r), co2all_yr(r,"IND",yr));
co2all_yr("EU28","IND+oil",yr)       = sum(r$eu28(r), co2all_yr(r,"IND+oil",yr));

* ------ 17.02.2016 Summarize all CO2-Emissions parameter in one report parameter
co2all_yr(r,"nonETS",yr) = sum(i$(not ets(i)), co2y_yr("total",i,r,yr)) + co2all_yr(r,"C",yr);
co2all_yr(r,"TOTAL1",yr)  = co2all_yr(r,"C",yr) + co2all_yr(r,"Y",yr) ;
co2all_yr(r,"TOTAL2",yr)  = co2all_yr(r,"ETS",yr) + co2all_yr(r,"nonETS",yr) ;

co2all_yr("EU28","Cars",yr)       = sum(r$eu28(r), co2all_yr(r,"Cars",yr) )        ;
co2all_yr("EU28","Buildings",yr)  = sum(r$eu28(r), co2all_yr(r,"Buildings",yr) )   ;
co2all_yr("EU28","C",yr)          = sum(r$eu28(r), co2all_yr(r,"C",yr))            ;
co2all_yr("EU28",i,yr)            = sum(r$eu28(r), co2all_yr(r,i,yr))              ;
co2all_yr("EU28","ELEx+n",yr)     = sum(r$eu28(r), co2all_yr(r,"ELEx+n",yr))       ;
co2all_yr("EU28","Y",yr)          = sum(r$eu28(r), co2all_yr(r,"Y",yr))            ;
co2all_yr("EU28","ETS",yr)        = sum(r$eu28(r), co2all_yr(r,"ETS",yr))          ;
co2all_yr("EU28","nonETS",yr)     = sum(r$eu28(r), co2all_yr(r,"nonETS",yr))       ;
co2all_yr("EU28","TOTAL1",yr)     = sum(r$eu28(r), co2all_yr(r,"TOTAL1",yr))       ;
co2all_yr("EU28","TOTAL2",yr)     = sum(r$eu28(r), co2all_yr(r,"TOTAL2",yr))       ;

co2all_yr("World","Cars",yr)      = sum(r, co2all_yr(r,"Cars",yr) )       ;
co2all_yr("World","Buildings",yr) = sum(r, co2all_yr(r,"Buildings",yr) )  ;
co2all_yr("World","C",yr)         = sum(r, co2all_yr(r,"C",yr))           ;
co2all_yr("World",i,yr)           = sum(r, co2all_yr(r,i,yr))             ;
co2all_yr("World","ELEx+n",yr)    = sum(r, co2all_yr(r,"ELEx+n",yr))      ;
co2all_yr("World","Y",yr)         = sum(r, co2all_yr(r,"Y",yr))           ;
co2all_yr("World","ETS",yr)       = sum(r, co2all_yr(r,"ETS",yr))         ;
co2all_yr("World","nonETS",yr)    = sum(r, co2all_yr(r,"nonETS",yr))      ;
co2all_yr("World","TOTAL1",yr)    = sum(r, co2all_yr(r,"TOTAL1",yr))      ;
co2all_yr("World","TOTAL2",yr)    = sum(r, co2all_yr(r,"TOTAL2",yr))      ;

co2sec_yr(sec,yr) = sum(i$sec2cluster(sec,i), co2all_yr("DEU",i,yr));
co2sec_yr("energy",yr) = co2sec_yr("energy",yr) + co2all_yr("DEU","ELEx+n",yr);
co2sec_yr("buildings",yr) = co2sec_yr("buildings",yr) + co2all_yr("DEU","c",yr);

* ------ 5.05.2015 Calculate total CO2 emissions based on MPSGE Report values
co2_yr_2(r,yr)$(NOT HH_DISAG(r)) = VC_CO2.L(r) + VC_CO2_NETS.L(r) + VC_CO2_NETSr.L(r) + sum(i, VY_CO2.L(i,r) + VY_CO2_NETS.L(i,r) + VY_CO2_NETSr.L(i,r) + VY_CO2_ETS.L(i,r))
                 + sum(gen, VELEx_CO2.L(gen,r) + VELEn_CO2.L(gen,r) + VELEx_CO2_ETS.L(gen,r) + VELEn_CO2_ETS.L(gen,r)) ;
co2_yr_2(r,yr)$(HH_DISAG(r)) = sum(hh,VC_hh_CO2.L(hh,r)) + VC_gov_CO2.L(r) + sum(hh,VC_hh_CO2_NETS.L(hh,r)) + VC_gov_CO2_NETS.L(r) + sum(hh,VC_hh_CO2_NETSr.L(hh,r)) + VC_gov_CO2_NETSr.L(r) 
                 + sum(i, VY_CO2.L(i,r) + VY_CO2_NETS.L(i,r) + VY_CO2_NETSr.L(i,r) + VY_CO2_ETS.L(i,r))
                 + sum(gen, VELEx_CO2.L(gen,r) + VELEn_CO2.L(gen,r) + VELEx_CO2_ETS.L(gen,r) + VELEn_CO2_ETS.L(gen,r)) ;
co2_yr_2("EU28",yr)      = sum(r$eu28(r),        co2_yr_2(r,yr) ) ;
co2_yr_2("World",yr)     = sum(r,                co2_yr_2(r,yr) ) ;

* ------ 10.12.2014 ---> CO2 in EU28 for non-ets sectors only
co2nets_yr1(r,fe,"final",yr)$((c0(fe,r)*aeei(fe,"c",r)) AND NOT HH_DISAG(r))      = VC_PA.L(fe,r) / (c0(fe,r)*aeei(fe,"c",r))     * co2em(fe,"final",r)   * aeei(fe,"c",r)     ;
co2nets_yr1(r,fe,"final",yr)$((c0(fe,r)*aeei(fe,"c",r)) AND HH_DISAG(r))          = (sum(hh,VC_hh_PA.L(fe,hh,r)) + VC_gov_PA.L(fe,r)) / (c0(fe,r)*aeei(fe,"c",r))     * co2em(fe,"final",r)   * aeei(fe,"c",r)     ;
co2nets_yr1(r,fe,i,yr)$(eu28(r) and not ets(i) and ( vafm(fe,i,r)*aeei(fe,i,r)))  = VY_PA.L(fe,i,r)/ (vafm(fe,i,r)*aeei(fe,i,r))  * co2em(fe,i,r)         * aeei(fe,i,r)  ;
co2nets_yr1(r,fe,i,yr)$(not eu28(r) and (vafm(fe,i,r)*aeei(fe,i,r)))              = VY_PA.L(fe,i,r)/ (vafm(fe,i,r)*aeei(fe,i,r))  * co2em(fe,i,r)         * aeei(fe,i,r);
co2nets_yr1(r,fe,gen,yr)$(not eu28(r) and vafm_input(fe,gen,r))                   = VELEx_PA.L(fe,gen,r) / (vafm_input(fe,gen,r)*aeei_elex(gen,r))                        * co2em(fe,gen,r) * aeei_elex(gen,r)
                                                                                  + VELEn_PA.L(fe,gen,r) / (vafm_input(fe,gen,r)*(diffcost(gen,r))*aeei_elen(gen,r))      * co2em(fe,gen,r) * aeei_elen(gen,r) ;
co2nets_yr1(r,fe,"nets",yr)       = sum(i, co2nets_yr1(r,fe,i,yr))  ;
co2nets_yr1(r,fe,"ele",yr)        = sum(gen, co2nets_yr1(r,fe,gen,yr)) ;
co2nets_yr1(r,fe,"total",yr)      = co2nets_yr1(r,fe,"ele",yr) + co2nets_yr1(r,fe,"final",yr) + co2nets_yr1(r,fe,"nets",yr) ;
co2nets_yr1(r,"sum","final",yr)   = sum(fe, co2nets_yr1(r,fe,"final",yr)) ;
co2nets_yr1(r,"sum","nets",yr)    = sum(fe, co2nets_yr1(r,fe,"nets",yr)) ;
co2nets_yr1(r,"sum","ele",yr)     = sum(fe, co2nets_yr1(r,fe,"ele",yr)) ;
co2nets_yr1(r,"sum","total",yr)   =  co2nets_yr1(r,"sum","final",yr) + co2nets_yr1(r,"sum","nets",yr) + co2nets_yr1(r,"sum","ele",yr) ;
co2nets_yr1("all","sum","final",yr)   = sum(r, co2nets_yr1(r,"sum","final",yr)) ;
co2nets_yr1("all","sum","nets",yr)    = sum(r, co2nets_yr1(r,"sum","nets",yr)) ;
co2nets_yr1("all","sum","ele",yr)     = sum(r, co2nets_yr1(r,"sum","ele",yr)) ;
co2nets_yr1("all","sum","total",yr)   = sum(r, co2nets_yr1(r,"sum","total",yr)) ;
co2nets_yr1("chk","chk","chk","2011") = co2nets_yr1("all","sum","total","2011") + co2ets_yr("EU28","2011") - co2em_total("world") ;

* ------ PCO2_NETS
co2nets_yr(r,"final",yr)$(netstrade AND NOT HH_DISAG(r))  = VC_CO2_NETS.L(r) ;
co2nets_yr(r,"final",yr)$(netstrade AND HH_DISAG(r))      = sum(hh,VC_hh_CO2_NETS.L(hh,r)) + VC_gov_CO2_NETS.L(r) ;
co2nets_yr(r,"final",yr)$(netstrade_r AND NOT HH_DISAG(r))   = VC_CO2_NETSr.L(r) ;
co2nets_yr(r,"final",yr)$(netstrade_r AND HH_DISAG(r))       = sum(hh,VC_hh_CO2_NETSr.L(hh,r)) + VC_gov_CO2_NETSr.L(r) ;
co2nets_yr(r,i,yr)$netstrade             = VY_CO2_NETS.L(i,r) ;
co2nets_yr(r,i,yr)$netstrade_r           = VY_CO2_NETSr.L(i,r) ;
co2nets_yr(r,"Y",yr)$netstrade           = sum(i, VY_CO2_NETS.L(i,r)) ;
co2nets_yr(r,"Y",yr)$netstrade_r         = sum(i, VY_CO2_NETSr.L(i,r)) ;

co2nets_yr(r,"total",yr)$(netstrade AND NOT HH_DISAG(r))   = VC_CO2_NETS.L(r) + sum(i, VY_CO2_NETS.L(i,r)) ;
co2nets_yr(r,"total",yr)$(netstrade AND HH_DISAG(r))       = sum(hh,VC_hh_CO2_NETS.L(hh,r)) + VC_gov_CO2_NETS.L(r) + sum(i, VY_CO2_NETS.L(i,r)) ;

co2nets_yr(r,"total",yr)$(netstrade_r AND NOT HH_DISAG(r))     = VC_CO2_NETSr.L(r) + sum(i, VY_CO2_NETSr.L(i,r)) ;
co2nets_yr(r,"total",yr)$(netstrade_r AND HH_DISAG(r))         = sum(hh,VC_hh_CO2_NETSr.L(hh,r)) + VC_gov_CO2_NETSr.L(r) + sum(i, VY_CO2_NETSr.L(i,r)) ;

co2nets_yr("EU28","final",yr)    = sum(r$EU28(r), co2nets_yr(r,"final",yr)) ;
co2nets_yr("EU28",i,yr)          = sum(r$EU28(r), co2nets_yr(r,i,yr)) ;
co2nets_yr("EU28","Y",yr)        = sum(r$EU28(r), co2nets_yr(r,"Y",yr)) ;
co2nets_yr("EU28","total",yr)    = sum(r$EU28(r), co2nets_yr(r,"total",yr)) ;
co2nets_yr("all","final",yr)     = sum(r, co2nets_yr(r,"final",yr)) ;
co2nets_yr("all",i,yr)           = sum(r, co2nets_yr(r,i,yr)) ;
co2nets_yr("all","Y",yr)         = sum(r, co2nets_yr(r,"Y",yr)) ;
co2nets_yr("all","total",yr)     = sum(r, co2nets_yr(r,"total",yr)) ;
*co2nets_yr("BRD","final",yr)     = sum(r$bawdeu(r), co2nets_yr(r,"final",yr)) ;
*co2nets_yr("BRD",i,yr)           = sum(r$bawdeu(r), co2nets_yr(r,i,yr)) ;
*co2nets_yr("BRD","Y",yr)         = sum(r$bawdeu(r), co2nets_yr(r,"Y",yr)) ;
*co2nets_yr("BRD","total",yr)     = sum(r$bawdeu(r), co2nets_yr(r,"total",yr)) ;

* ------ PCO2W
co2w_yr(r,"final",yr)$(NOT HH_DISAG(r))    = VC_CO2W.L(r) ;
co2w_yr(r,"final",yr)$(HH_DISAG(r))    = sum(hh,VC_hh_CO2W.L(hh,r)) + VC_gov_CO2W.L(r) ;
co2w_yr(r,i,yr)          = VY_CO2W.L(i,r) ;
co2w_yr(r,gen,yr)        = VELEx_CO2W.L(gen,r) + VELEn_CO2W.L(gen,r) ;
co2w_yr(r,"ELEn",yr)     = sum(gen, VELEn_CO2W.L(gen,r)) ;
co2w_yr(r,"ELEx",yr)     = sum(gen, VELEx_CO2W.L(gen,r)) ;
co2w_yr(r,"ELE",yr)      = sum(gen, co2w_yr(r,gen,yr)) ;
co2w_yr(r,"Y",yr)        = sum(i, VY_CO2W.L(i,r)) + co2w_yr(r,"ELE",yr) ;
co2w_yr(r,"TOTAL",yr)    = co2w_yr(r,"final",yr) + co2w_yr(r,"Y",yr) ;
co2w_yr(r,"ets",yr)$eu28(r)      = sum(i$ets(i), co2w_yr(r,i,yr)) ;
co2w_yr(r,"nets",yr)$eu28(r)     = sum(i$(not ets(i)), co2w_yr(r,i,yr)) ;

*co2w_yr("BRD","final",yr)    = sum(r$bawdeu(r), co2w_yr(r,"final",yr)) ;
*co2w_yr("BRD",i,yr)          = sum(r$bawdeu(r), co2w_yr(r,i,yr)) ;
*co2w_yr("BRD",gen,yr)        = sum(r$bawdeu(r), co2w_yr(r,gen,yr)) ;
*co2w_yr("BRD","ELEn",yr)     = sum(r$bawdeu(r), co2w_yr(r,"ELEn",yr)) ;
*co2w_yr("BRD","ELEx",yr)     = sum(r$bawdeu(r), co2w_yr(r,"ELEx",yr)) ;
*co2w_yr("BRD","ELE",yr)      = sum(r$bawdeu(r), co2w_yr(r,"ELE",yr) ) ;
*co2w_yr("BRD","Y",yr)        = sum(r$bawdeu(r), co2w_yr(r,"Y",yr)) ;
*co2w_yr("BRD","ets",yr)      = sum(r$bawdeu(r), co2w_yr(r,"ets",yr) ) ;
*co2w_yr("BRD","nets",yr)     = sum(r$bawdeu(r), co2w_yr(r,"nets",yr)) ;
*co2w_yr("BRD","TOTAL",yr)    = sum(r$bawdeu(r), co2w_yr(r,"TOTAL",yr)) ;

co2w_yr("EU28","final",yr)    = sum(r$eu28(r), co2w_yr(r,"final",yr)) ;
co2w_yr("EU28",i,yr)          = sum(r$eu28(r), co2w_yr(r,i,yr)) ;
co2w_yr("EU28",gen,yr)        = sum(r$eu28(r), co2w_yr(r,gen,yr)) ;
co2w_yr("EU28","ELEn",yr)     = sum(r$eu28(r), co2w_yr(r,"ELEn",yr)) ;
co2w_yr("EU28","ELEx",yr)     = sum(r$eu28(r), co2w_yr(r,"ELEx",yr)) ;
co2w_yr("EU28","ELE",yr)      = sum(r$eu28(r), co2w_yr(r,"ELE",yr) ) ;
co2w_yr("EU28","Y",yr)        = sum(r$eu28(r), co2w_yr(r,"Y",yr)) ;
co2w_yr("EU28","ets",yr)      = sum(r$eu28(r), co2w_yr(r,"ets",yr) ) ;
co2w_yr("EU28","nets",yr)     = sum(r$eu28(r), co2w_yr(r,"nets",yr)) ;
co2w_yr("EU28","TOTAL",yr)    = sum(r$eu28(r), co2w_yr(r,"TOTAL",yr)) ;

co2w_yr("all","final",yr)    = sum(r, co2w_yr(r,"final",yr)) ;
co2w_yr("all",i,yr)          = sum(r, co2w_yr(r,i,yr)) ;
co2w_yr("all",gen,yr)        = sum(r, co2w_yr(r,gen,yr)) ;
co2w_yr("all","ELEn",yr)     = sum(r, co2w_yr(r,"ELEn",yr)) ;
co2w_yr("all","ELEx",yr)     = sum(r, co2w_yr(r,"ELEx",yr)) ;
co2w_yr("all","ELE",yr)      = sum(r, co2w_yr(r,"ELE",yr) ) ;
co2w_yr("all","Y",yr)        = sum(r, co2w_yr(r,"Y",yr)) ;
co2w_yr("all","ets",yr)      = sum(r, co2w_yr(r,"ets",yr) ) ;
co2w_yr("all","nets",yr)     = sum(r, co2w_yr(r,"nets",yr)) ;
co2w_yr("all","TOTAL",yr)    = sum(r, co2w_yr(r,"TOTAL",yr)) ;
*$offtext

* ------ 19.05.2015 Spezifische CO2-Emissionen im Stromsektor pro Region [Mio.tCO2/TWh = tCO2/MWh]
co2elespec_yr(r,yr)  = sum((fe,gen),                 co2ele_yr(fe,gen,r,yr)) / (sum(gen, ele_twh_yr(r,gen,yr)) * 1000 + 1e6) ;
*co2elespec_yr("BRD",yr)  = sum((fe,gen,r)$bawdeu(r),     co2ele_yr(fe,gen,r,yr)) / sum((gen,r)$bawdeu(r),ele_twh_yr(r,gen,yr)) * 1000 ;
co2elespec_yr("EU28",yr) = sum((fe,gen,r)$eu28(r),       co2ele_yr(fe,gen,r,yr)) / sum((gen,r)$eu28(r),  ele_twh_yr(r,gen,yr)) * 1000 ;
co2elespec_yr("World",yr)= sum((fe,gen,r),               co2ele_yr(fe,gen,r,yr)) / sum((gen,r),          ele_twh_yr(r,gen,yr)) * 1000 ;

* ------ 1.07.2015 Calculate CO2-intensity and CO2-costs relative to GDP (in %)
co2gdp_yr(r,yr)          = 1E+6 * co2_yr("total",r,yr)           / gdpreal_yr(r,yr)  ;     // multiply tCO2/€GDP in order to get gCO2/€GDP
co2gdp_yr("EU28",yr)     = 1E+6 * co2_yr("total","EU28",yr)      / gdpreal_yr("EU28",yr);  // multiply tCO2/€GDP in order to get gCO2/€GDP
co2gdp_yr("World",yr)    = 1E+6 * co2_yr("total","World",yr)     / gdpreal_yr("World",yr); // multiply tCO2/€GDP in order to get gCO2/€GDP

* ------ 1.04.2016
co2gdp20_yr(r,yr)        = co2_yr("total",r,yr) * 20 / gdpreal_yr(r,yr);
co2gdp50_yr(r,yr)        = co2_yr("total",r,yr) * 50 / gdpreal_yr(r,yr);
co2gdp20_yr("EU28",yr)   = co2_yr("total","EU28",yr)  * 20 / gdpreal_yr("EU28",yr)  ;
co2gdp50_yr("EU28",yr)   = co2_yr("total","EU28",yr)  * 50 / gdpreal_yr("EU28",yr)  ;
co2gdp20_yr("World",yr)  = co2_yr("total","World",yr) * 20 / gdpreal_yr("World",yr) ;
co2gdp50_yr("World",yr)  = co2_yr("total","World",yr) * 50 / gdpreal_yr("World",yr) ;
*co2gdp_yr("BRD",yr)      = co2_yr("total","BRD",yr)        / gdpreal_yr("BRD",yr)   ;
*co2gdp20_yr("BRD",yr)    = co2_yr("total","BRD",yr)   * 20 / gdpreal_yr("BRD",yr)   ;
*co2gdp50_yr("BRD",yr)    = co2_yr("total","BRD",yr)   * 50 / gdpreal_yr("BRD",yr)   ;

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ Other Reporting
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

* ------ 14.10.2014
ele_capdem(r,gen,yr) = VELEx_RKX_ELE.L(gen,r) + VELEn_RKR.L(gen,r) ;
ele_labdem(r,gen,yr) = VELEx_PUSK.L(gen,r)    + VELEx_PSKL.L(gen,r) + VELEn_PUSK.L(gen,r) + VELEn_PSKL.L(gen,r);

* ------ 19.05.2015 Electricity costs
eleYgva_yr(r,j,i,yr)$(ele(j) and gva_yr(r,i,yr)) = VY_PA.L(j,i,r) * PA.L(j,r)* (1+ti(j,i,r)) / gva_yr(r,i,yr) ;

* ------ 20.05.2015
gva_real_chk(i,r)= round(  (VY_PY.L(i,r)         - sum(j, VY_PA.L(j,i,r)))
                         - (Y.L(i,r) * vom(i,r)  - sum(j, VY_PA.L(j,i,r))), rd) ;    // correct

* ------ 16.02.2016
gdpco2_yr(r,yr)      = gdpreal_yr(r,yr) / co2_yr("total",r,yr);
gdpco2_yr("EU28",yr) = sum(r$eu28(r), gdpreal_yr(r,yr)) / sum(r$eu28(r), co2_yr("total",r,yr));
gdpco2_yr("World",yr)= sum(r, gdpreal_yr(r,yr)) / sum(r, co2_yr("total",r,yr)) ;

*-------23.10.2017 Variable to calculate endogenous price path
*pytarget_endo_yr(r,"cru",yr) = py.L("cru",r)/pc.L(r);
*pytarget_endo_yr(r,"gas",yr) = py.L("gas",r)/pc.L(r);
*pytarget_endo_yr(r,"col",yr) = py.L("col",r)/pc.L(r);
*-------23.10.2017 end


VTAXTRANS_PGov_yr(r,yr)$HH_DISAG(r) = VTAXTRANS_PGov.L(r);

VLKTAX_PITAX_yr(r,yr)$HH_DISAG(r) = VLKTAX_PITAX.L(r);

* xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
* ENERGY DEMAND
* xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

energy_cons_sec_yr(e,"c+g",r,yr)$energy_euro_rate(e,"c+g",r) = vc_pa_yr(e,r,yr) * energy_euro_rate(e,"c+g",r);
energy_cons_sec_yr("total","c+g",r,yr) = sum(e,energy_cons_sec_yr(e,"c+g",r,yr));

energy_cons_sec_yr(e,i,r,yr)$energy_euro_rate(e, i, r) = vy_pa_yr(r,e,i,yr) * energy_euro_rate(e, i, r);
energy_cons_sec_yr("total",i,r,yr) = sum(e,energy_cons_sec_yr(e,i,r,yr));

energy_cons_sec_yr(e,i,"EU28",yr) = sum(r$eu28_eu(r), energy_cons_sec_yr(e,i,r,yr));
energy_cons_sec_yr(e,"c+g","EU28",yr) = sum(r$eu28_eu(r), energy_cons_sec_yr(e,"c+g",r,yr));

energy_cons_sec_yr("total",i,"EU28",yr) = sum(e, energy_cons_sec_yr(e,i,"EU28",yr));
energy_cons_sec_yr("total","c+g","EU28",yr) = sum(e, energy_cons_sec_yr(e,"c+g","EU28",yr));

* ------ Energy demand for household disaggregation case

energy_cons_sec_yr(e,hh,r,yr)$(energy_euro_rate(e,"c",r) AND HH_disag(r))   = vc_hh_pa_yr(e,hh,r,yr) * energy_euro_rate(e,"c",r);
energy_cons_sec_yr("total",hh,r,yr)$HH_disag(r)                             = sum(e,energy_cons_sec_yr(e,hh,r,yr));

energy_cons_sec_yr(e,"sum_hh",r,yr)$HH_disag(r)         = sum(hh,energy_cons_sec_yr(e,hh,r,yr));
energy_cons_sec_yr("total","sum_hh",r,yr)$HH_disag(r)   = sum((e,hh),energy_cons_sec_yr(e,hh,r,yr));

energy_cons_sec_yr(e,"gov",r,yr)$(energy_euro_rate(e,"g",r) AND HH_disag(r))    = vc_gov_pa_yr(e,r,yr) * energy_euro_rate(e,"g",r);
energy_cons_sec_yr("total","gov",r,yr)$HH_disag(r)                              = sum(e,energy_cons_sec_yr(e,"gov",r,yr));

energy_cons_sec_yr(e,hh,"EU28",yr)$sum(r$eu28(r), energy_cons_sec_yr(e,hh,r,yr))     = sum(r$eu28_eu(r), energy_cons_sec_yr(e,hh,r,yr));
energy_cons_sec_yr("total",hh,"EU28",yr)$sum(e,energy_cons_sec_yr(e,hh,"eu28",yr))   = sum(e, energy_cons_sec_yr(e,hh,"eu28",yr));

energy_cons_sec_yr(e,"gov","EU28",yr)$sum(r$eu28(r),energy_cons_sec_yr(e,"gov",r,yr))       = sum(r$eu28_eu(r), energy_cons_sec_yr(e,"gov",r,yr));
energy_cons_sec_yr("total","gov","EU28",yr)$sum(e, energy_cons_sec_yr(e,"gov","EU28",yr))   = sum(e, energy_cons_sec_yr(e,"gov","eu28",yr));

energy_cons_sec_yr(e,"sum_hh","EU28",yr)$sum(r$eu28_eu(r), energy_cons_sec_yr(e,"sum_hh",r,yr))       = sum(r$eu28_eu(r), energy_cons_sec_yr(e,"sum_hh",r,yr));
energy_cons_sec_yr("total","sum_hh","EU28",yr)$sum(e, energy_cons_sec_yr(e,"sum_hh","EU28",yr))       = sum(e, energy_cons_sec_yr(e,"sum_hh","eu28",yr));


* ---------------------------------------------------------------------- *
* LOOP-ENDE                                                                      // # LOOP-end #
* ---------------------------------------------------------------------- *
);
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

* XXXXX  GET GINI COEFFICIENT

PARAMETER
    gini(*,*,r,yr);

gini("net", "net_all", r, yr) = sum( hh, RA_hh_par_yr("net_income", r, hh, yr));
gini("net", hh, r, yr)$RA_hh_par_yr("net_income", r, hh, yr) = sum( hh_$(ord(hh_) <= ord(hh)), RA_hh_par_yr("net_income", r, hh_, yr)) / gini("net", "net_all", r, yr);

gini("net_area", hh, r, yr)$gini("net", hh, r, yr) = ((gini("net", hh, r, yr) -( sum(hh_$((ord(hh_) = (ord(hh) - 1)) AND (ord(hh) > 0)), gini("net", hh_, r, yr)) + 0) ) /10) +( sum(hh_$((ord(hh_) = ord(hh) - 1) AND (ord(hh) > 0)), gini("net", hh_, r, yr))  * 0.2);

gini("net", "gini", r, yr) = 2 * (0.5 - sum(hh_,gini("net_area", hh_, r, yr)));


gini("total", "total_all", r, yr) = sum( hh, RA_hh_par_yr("total_income", r, hh, yr));
gini("total", hh, r, yr)$RA_hh_par_yr("total_income", r, hh, yr) = sum( hh_$(ord(hh_) <= ord(hh)), RA_hh_par_yr("total_income", r, hh_, yr)) / gini("total", "total_all", r, yr);

gini("total_area", hh, r, yr)$gini("total", hh, r, yr) = ((gini("total", hh, r, yr) -( sum(hh_$((ord(hh_) = (ord(hh) - 1)) AND (ord(hh) > 0)), gini("total", hh_, r, yr)) + 0) ) /10) +( sum(hh_$((ord(hh_) = ord(hh) - 1) AND (ord(hh) > 0)), gini("total", hh_, r, yr))  * 0.2);

gini("total", "gini", r, yr) = 2 * (0.5 - sum(hh_,gini("total_area", hh_, r, yr)));


*display wg_diff, delta_wg_before, gew_wg, velen_pgenyr, zubau_ele, gen_limit_yr, genlimit_twh_yr;

*display ep, aeei_yr, aeei, aeei_elexyr, aeei_elenyr, wg0, wg_yr, abschreibung, abschreibung_bmk, evoa_yr, ressize, nucsize, up_yr, lo_yr;
display size_usk, size_skl;

display welf_yr, ex_yr, vxm, im_yr, trdblnc_yr, tradematrix_yr, trdblnc2_yr, gdpreal5_yr, gdpreal_yr, gva_yr, gva_real_yr, demand_yr, emplmt_yr, ur_yr;
*display gdpreal_yr2;

display rwa_yr, rca_yr, velex_yr, velen_yr, vele_yr, eleXtwh_yr, eleNtwh_yr, ele_twh_yr, eletwhyr, elecontwh_yr, elecontwh2_yr, ele_prod, ks_x_yr, ks_x_yr2;
*display evoa1, evoa11, evoa2;

*display pk_yr, pusk_yr, pskl_yr, pc_yr, pa_yr, py_yr, pr_yr, pgen_yr, pinv_yr, rkx_ele_yr, pco2_ets_yr;

*display VY_PY_yr, VINV_PINV_yr, invgdp_yr ;
*display VYT_PT_yr, VA_PA_yr, VC_PC_yr, VELEx_PGEN_yr, VELEn_PGEN_yr;
*display Y_yr, INV_yr, YT_yr, A_yr, C_yr, ELEx_yr, ELEn_yr, ELE_yr;

*display vd_pc_yr, vd_pinv_yr, vinv_pa_yr, ele_capdem, ele_labdem ;

display carblim_yr, co2nets_yr, co2nets_yr1, carblim_ets_yr, co2ets_yr, co2ets_y, co2c_yr, co2y_yr, co2ele_yr, co2_yr, co2all_yr, co2ets_elex, co2ets_elen, co2ets_ele, co2pfad_ets, pco2w_yr, pco2_yr, pco2_netsr_yr, up_yr, lo_yr;

display zpf_c_yr, zpf_y_yr, zpf_gva_yr, sharec_yr, sharey_yr, shareYgva_yr, shareYgva2_yr, gva_real_chk, sharect_yr, shareyt_yr, shareYtgva_yr, eleYgva_yr ;

*display co2_yr, gdpreal_yr, co2gdp_yr, co2gdp20_yr, co2gdp50_yr, capgdp_yr, tm, tx;

*display  vc_esd_yr, vesd_o_yr, vesd_cart_yr, vesd_trn_yr, vesd_heat_yr, vheat_o_yr, vheat_f_yr, vcart_o_yr, vcart_f_yr, vheatf_o_yr,
*         vheatf_pa_yr, vheatf_pbio_yr, vheatf_pfuels_yr, vheatf_stock_yr, vheatf_pco2nets_yr, vcartf_o_yr, vcartf_pa_yr, vcartf_pfuels_yr,
*         vcartf_stock_yr, vcartf_pco2nets_yr, vnc_pa_yr, vnc_peff_i_yr, vnc_peff_o_yr, vnh_pa_yr,
*         vnh_peff_i_yr, vnh_peff_o_yr, vstock_co_yr, vstock_cnc_yr, vstock_cxc_yr, vstock_ho_yr,
*         vstock_hpeffi_yr, vstock_hpeffo_yr, voils_o_yr, voils_pa_yr, vb_bio_yr, vb_pa_yr ;

display aeei, aeei_yr, ep;

*display pricetarget, pytarget, pytarget_yr, pytarget_endo_yr



* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

*$EXIT
*EXITGDX

* Textmarke GDX
* ---------------------------------------------------------------------- *
* ------ After-LOOP-REPORTING ------------------------------------------ *
* ---------------------------------------------------------------------- *

* ---------------------------------------------------------------------- *
* E X C E L :  Result Export to scenrio specif gdx bau scen1 scen2 EXCEL *
* ---------------------------------------------------------------------- *

* ---------------------------------------------------------------------- *
* G D X :  Write out ALL results for GDX merger                                *
*          All these results will be merged in MERGE_REPORTS_GMS!              *
* ---------------------------------------------------------------------- *

* ------ EXECUTE_UNLOAD
Execute_Unload$ref       "%resultsdir%ref.gdx";
Execute_Unload$sc1       "%resultsdir%sc1.gdx";
Execute_Unload$sc2       "%resultsdir%sc2.gdx";
Execute_Unload$sc3       "%resultsdir%sc3.gdx";
Execute_Unload$sc4       "%resultsdir%sc4.gdx";
Execute_Unload$sc5       "%resultsdir%sc5.gdx";
Execute_Unload$sc6       "%resultsdir%sc6.gdx";

* ------ 05.01.2012: Dynamische Report-Datei
Execute_unload           "%resultsdir%results.gdx" ;
*Execute_unload           "%resultsdir%sc2row.gdx" ;
*Execute_unload           "%resultsdir%sc2baw.gdx" ;
* Textmarke #baw-row

* - GAMS-L Archives: hhttp://www.mpsge.org/pivotdata.htm -----------------
* ------ Delete existing workbook
*$if exist %resultsdir%report_pivot.xlsx $call 'rm %resultsdir%report_pivot.xlsx'

* - GAMS-L Archives: http://www.listserv.dfn.de/cgi-bin/wa?A2=ind1107&L=GAMS-L&P=R2377&I=-3 ---
* ------ Instead of using a separate gdxxrw statement for each single parameter you want to dump,
* ------ one can speed up things significantly by calling Excel only once.
* ------ For example, froma batch file you one can use the line below
* ------ where dumppar.rpt is a text file that can contain the separate statements, e.g.:
* ------ par=parameter_1 rng=parameter_1!a2 cdim=0
parameter ks_x_yr3  ;
ks_x_yr3(r,gen,yr) = round(ks_x_yr2(r,gen,yr) - ks_x_yr(r,gen,yr),7); display ks_x_yr3;

*$call gdxxrw.exe i=results.gdx o=%resultsdir%report_pivot.xlsx @dumppar.txt

* ------ 21.07.2014 (auskommentiert)
Execute  'gdxxrw.exe i=%resultsdir%results.gdx o=%resultsdir%report_pivot.xlsx epsout=0 @dumppar_diss_rm.txt'

* ------ 03.07.2014
*Execute 'gdxxrw.exe i=results.gdx o=%resultsdir%report_pivot.xlsx @dumppar_noele.txt'

$EXIT
*EXITEND
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

* Reporting version (B) "batinclude pivotdata.gms"
* - GAMS-L Archives: hhttp://www.mpsge.org/pivotdata.htm -----------------
*$ontext
*        Define the name of the workbook to be generated:
$setglobal workbook %resultsdir%mypivotdata

*        Optionally delete the workbook if you want to have a fresh start:
$if exist %resultsdir%mypivotdata.xlsx $call 'rm %resultsdir%mypivotdata.xlsx'

*        Dump out the data along with column headers -- identifiers with which to label each colum of data.
*        The column with numeric values is always labelled "value".
*        You may wish to use set IDs as column headings:
$batinclude pivotdata_v03 welf_yr region year
$batinclude pivotdata_v03 gdpreal_yr region year
$batinclude pivotdata_v03 emplmt_yr type region year
$batinclude pivotdata_v03 trdblnc_yr sector* region year
$batinclude pivotdata_v03 ex_yr sector region year
$batinclude pivotdata_v03 im_yr sector region year
$batinclude pivotdata_v03 ur_yr type region year
$batinclude pivotdata_v03 elextwh_yr region technology year
$batinclude pivotdata_v03 elentwh_yr region technology year
$batinclude pivotdata_v03 ele_twh_yr region technology year
$batinclude pivotdata_v03 elecontwh_yr region technology year
$batinclude pivotdata_v03 elecontwh2_yr region technology year


* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
