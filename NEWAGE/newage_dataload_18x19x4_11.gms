$Title Satellite code to transform LIBINCLUDE Data from Excel to GDX
$onempty

$oneolcom
$eolcom #
* if you want a comment in brackets, like: y = 2 ; { this is also a comment } z = 3 ; type:
*$inlinecom {}
* --------------------------------------------------------------------- *

* ------ Definiere den Lösungsalgorithmus (Solver) für das MCP-Problem
OPTION   MCP = PATH ;


$if not set source       $set source     18x19x4_gtap9
$if not set output       $set output     %source%

* ------ 10.04.2014 DataSe
$if not set ds           $set ds         %output%
*$if not set yr           $set yr         07
$if not set yr           $set yr         11

$if not set datadir      $set datadir "..\data%yr%\"
$setglobal datadir %datadir%

$if not set xcel_datadir $set xcel_datadir "..\xcel_data\"

$include ..\build\gtap9data_newage


* ---------------------------------------------------------------------- *
*        SET and additional sets (SUBSETS) for technology BOUP
*        in ELECTRICITY GENERATION
* ---------------------------------------------------------------------- *
*        b = baseload, m = middleload, p = peakload

SET      GEN             Stromerzeugungsarten /  bNUC, bBC,  bBIO, bGEO,  bHYDRO, bHC,  bGAS, bOIL, bCCS,
                                                 mHC, mGAS, mWIND, mSOLAR, mOIL, mCCS, pOIL, pGAS, pHYDRO,
                                                 HYDRO, GEO, BIO, SOLAR, WIND /;

*------15.09.17   additional sets needed to include sector-specific CO2 targets for Germany
SET   energy      /COL, CRU, GAS, OIL, ELE/ ;
SET   buildings   /BUI, SER, AGR/ ;
SET   transport   /TRN/ ;
SET   industry /IRS, NFM, NMM, PPP, CHM, FOT, MVH, MAC, ROI/ ;


set   sec      /energy, buildings, transport, industry/;


* ---------------------------------------------------------------------------- *
*        SET and additional sets (SUBSETS) for DYNAMIC MILESTONES
* ---------------------------------------------------------------------------- *

* ------ 17.01.2012 ORIGINAL Zeithorizont bis 2050
*$ontext
SET      YR MILESTONES  / 2011, 2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050    / ;

* ---------------------------------------------------------------------------- *
*        Define ALIAS indices for interactions etc.
* ---------------------------------------------------------------------------- *
ALIAS    (i,j), (r,s), (r,rr), (gen,gengen), (yr,yryr);


* ##############################################################################
* ---------------------------------------------------------------------------- *
* ------ Parameter declaration for exogenous data ---------------------------- *
* ---------------------------------------------------------------------------- *

PARAMETER
* ------ 23.08.2018 test times coupling
         ele_dev_switch(*,*)     switch to choose which technology in with country will follow the TIMES PanEU development
         ele_dev(gen,r,yr)               growth curve from TIMES-PanEU
         ele_dev_REEEM_path(gen,r,yr)    growth curve from TIMES-PanEU for the base pathway
         ele_dev_REEEM(gen,r,yr)         growth curve from eurostat for calibrating y2015
         ele_calib_2015(gen,r)

         ele_prod(gen,r)                 Benchmark electricity production [TWh]
         ressize(r,gen,yr)               Growth rates for renewable energy sources
         co2pfad(r,yr)                   CO2-growth
         co2pfad_ets(r,yr)               CO2-growth in the EU-ETS
         pytarget_yr(r,i,yr)             Exogenous price paths for resources
         pytarget_E3_yr(r,i,yr)
         co2pfad_ets_eu28_ref(*,yr)        CO2 pathway for the reference scenario in 2020


* ------ 10.06.2013: Zusammenfassung aller Unemployment rates über aggregierte Tabelle aus ursk0+urun0.xlsx
         ur_read(r,*)                    Einlesen aller Unemployment rates über aggregierte Tabelle aus ursk0+urun0.xlsx
         urun0(*)                        Initial unemployment rate among unskilled
         ursk0(*)                        Initial unemployment rate among skilled
         diffcost(gen,r)                 Differenzkosten
         diffcost_exogen(gen,r)          Differenzkosten
         cost_red(gen,yr)                Renewable energies cost reduction
         wg0(gen,r)                      Wirkungsgrad im bmk
         wg_yr(gen,yr)                   Wirkungsgrad nach jahren jeweils bat
         nucsize(r,yr)                   Ausbau- bzw. Rückbaupfad Kernenergie
*         un_numb0(r)                     Initial employment supply (potential) absolute unskilled
*         sk_numb0(r)                     Initial employment supply (potential) absolute skilled
*         labsize(r,yr)                   Changes in economically active population based ilo from 2020 onward constant

* ------ 10.06.2013:
*        --> Die Parameter abschreibung_z und abschreibung_bmk_z sind nun auch über das set r definiert
*        --> der Einfachheit sollten diese beiden Parameter aber im NEWAGE Code entfernt werden
*        --> auch mit bCCS und mCCS aufpassen --> werden in Epro Ra nicht mit eingelesen sondern erst im Code definiert
         abschreibung(r,gen,yr)          Decommissioning geg. vorperiode
         abschreibung_z(r,gen,yr)        Decommissioning geg. vorperiode für länder ohne daten über sterbekurven
         abschreibung_bmk(r,gen,yr)      Decommissioning geg. basis-bmk
         abschreibung_bmk_z(r,gen,yr)    Decommissioning geg. basis-bmk für länder ohne daten über sterbekurven

         abschreibung_noCOAL(r,gen,yr)             Decommissioning geg. vorperiode (for coal decomissioning till 2030 in germany)
         abschreibung_bmk_noCOAL(r,gen,yr)         Decommissioning geg. basis-bmk (for coal decomissioning till 2030 in germany)

* ------ 10.06.2013: Zusammenfassung aller AEEI-Pfade in Tabelle aeei.xlsx
         aeei_ind_ele(r,yr)                  AEEI exogen for industrial sectors (especially for kopernikus project)
         aeei_ind_ff(r,yr)                  AEEI exogen for industrial sectors (especially for kopernikus project)
         AEEI_c_ele(r,yr)                    AEEI exogen for residential sector (especially for kopernikus project)
         AEEI_c_ff(r,yr)                    AEEI exogen for residential sector (especially for kopernikus project)
         AEEI_trans_ele(r,yr)                    AEEI exogen for residential sector (especially for kopernikus project)
         AEEI_trans_ff(r,yr)                    AEEI exogen for residential sector (especially for kopernikus project)
         AEEI_ser_ele(r,yr)                    AEEI exogen for residential sector (especially for kopernikus project)
         AEEI_ser_ff(r,yr)                    AEEI exogen for residential sector (especially for kopernikus project)
         AEEI_ff(r,*,yr)
         AEEI_ele(r,*,yr)

         AEEI_hh_transport(r,*,yr)             "AEEI  for energy consumption related to transport in Households (Roland's Dissertation)"



* ------ 15.07.2014 Reading in Energy productivity and total factor productivity from the BASELINE Database
         lfhc_usk(r,yr)                  Labor force and human capital development for unskilled labor (BASELINE Database)
         lfhc_skl(r,yr)                  Labor force and human capital development for   skilled labor (BASELINE Database)

         ep(yr,r)                        Energy productivity development (BASELINE Database)
         tfp(yr,r)                       Total factor productivity development (BASELINE Database)

         tfp_corona(yr,r)                   Multiplier for TFP to match the GDP development affected by the COVID-19 pandemic


* ------- 14.11.2017 data for PA pfad
         vy_pa_pfad(j,i,yr)
         vc_pa_pfad(i,yr)
         velex_pa_pfad(i,gen,yr)
         velen_pa_pfad(i,gen,yr)

* ------- 18.10.2018 elasticities for energy
         elast_ene_yr(r,yr)

*         tp_no_vat(r,i)    tax for consumers witout VAT

*-----------------------------------
* CO2 Paths for specific scenarios
*-----------------------------------

* ----- REFERENCE SCENARIO 2020 (06.11.2020)
         co2pfad_ets_eu28_ref(*,yr)       "CO2 pathway for the reference scenario in 2020"

* ----- OTHER (06.11.2020)
         co2pfad_eu(*,yr)                 "CO2-growth in the EU (all sectors)"
         co2pfad_eu_nonets(r,yr)          "CO2-growth in the EU (non ETS sectors) - INDC scenario"
         co2pfad_row(r,yr)                "CO2-growth in the RoW (non-EU) - INDC scenario"
         co2pfad_DEU(sec,yr)              "sector-specific CO2-growth in Germany"
         co2pfad_ets_eu(*,yr)
         co2pfad_ets_row(r,yr)            "CO2-growth in the RoW (non-EU)"
         co2pfad_nonets(r,yr)             "CO2 growth for non-ETS"

* ----- ARIADNE Scenarios - European CO2 emissions
         CO2_AP5_sce3(*,*,yr)                 "ARIADNE - AP5 - scenario 3 (06.11.2020)"
         CO2_AP5_sce4(*,*,yr)                 "ARIADNE - AP5 - scenario 4 (06.11.2020)"
         CO2_AP5_sce5(*,*,yr)                 "ARIADNE - AP5 - scenario 5 (06.11.2020)"
         CO2_AP5_sce6(*,*,yr)                 "ARIADNE - AP5 - scenario 6 (06.11.2020)"
         CO2_AP5_sce7(*,*,yr)                 "ARIADNE - AP5 - scenario 7 (06.11.2020)"
         CO2_AP5_sce8(*,*,yr)                 "ARIADNE - AP5 - scenario 8 (06.11.2020)"
         CO2_AP5_sce9(*,*,yr)                 "ARIADNE - AP5 - scenario 9 (06.11.2020)"
         CO2_AP5_sce10(*,*,yr)                "ARIADNE - AP5 - scenario 10 (06.11.2020)"

* ----- ARIADNE scenarios - Non-EU emissions pathways
         CO2_AP5_world(*,yr)                  "ARIADNE - AP5 - sustainable development scenario"

;

*$libinclude    xlimport    tp_no_vat   %xcel_datadir%VAT_for_NEWAGE.xlsx   read_NEWAGE!a2:c152

*$ontext
* ---------------------------------------------------------------------------- *
*        READ AEEI
* ---------------------------------------------------------------------------- *
* ###### AEEI ##################################################################
* Modellparameter AEEI basiert auf AEEI_yr, welches im Wesentlichen auf AEEI_EXO beruht.(AEEI_EXOGEN... = AEEI_Variante)
* REF: AEEI = AEEI_HH = AEEI_TRN
$libinclude      xlimport        aeei_ind_ele           %xcel_datadir%aeei.xlsx       AEEI_ele!a1:j10
$libinclude      xlimport        aeei_c_ele             %xcel_datadir%aeei.xlsx       AEEI_ele!a13:j22
$libinclude      xlimport        AEEI_trans_ele         %xcel_datadir%aeei.xlsx       AEEI_ele!a25:j34
$libinclude      xlimport        aeei_ser_ele           %xcel_datadir%aeei.xlsx       AEEI_ele!a37:j46

$libinclude      xlimport        aeei_ind_ff           %xcel_datadir%aeei.xlsx       AEEI_ff!a1:j10
$libinclude      xlimport        aeei_c_ff             %xcel_datadir%aeei.xlsx       AEEI_ff!a13:j22
$libinclude      xlimport        AEEI_trans_ff         %xcel_datadir%aeei.xlsx       AEEI_ff!a25:j34
$libinclude      xlimport        aeei_ser_ff           %xcel_datadir%aeei.xlsx       AEEI_ff!a37:j46

$libinclude      xlimport        aeei_ff           %xcel_datadir%aeei.xlsx       AEEI_ff_2!a2:k146
$libinclude      xlimport        aeei_ele           %xcel_datadir%aeei.xlsx       AEEI_ele_2!a2:k137

$call gdxxrw   %xcel_datadir%AEEI_hh.xlsx  par=AEEI_hh_transport  rng=AEEI_hh_transport!A1  rDim=2 cDim=1
$GDXIN   AEEI_hh.gdx
$LOAD AEEI_hh_transport
$GDXIN

;
*$exit
* ---------------------------------------------------------------------------- *
*        READ Decommissioning (Sterbelinien)
* ---------------------------------------------------------------------------- *
* ###### abschreibung ##########################################################
* ------ include abschreibung for decommissioning curves for all regions (basically already includes abschreibung_z and abschreibung_bmk_z)
*$libinclude      xlimport        abschreibung            %xcel_datadir%abschreibung.xlsx       abschreibung!A3:L327       // mit BAW
*$libinclude      xlimport        abschreibung_bmk        %xcel_datadir%abschreibung.xlsx       abschreibung_bmk!A3:L327   // mit BAW
* ------ 3.02.2016 BAW entfernen
$libinclude      xlimport        abschreibung            %xcel_datadir%abschreibung.xlsx       abschreibung!A3:K327
$libinclude      xlimport        abschreibung_bmk        %xcel_datadir%abschreibung.xlsx       abschreibung_bmk!A3:K327

* ###### abschreibung_z ########################################################
* ------ include abschreibung for decommissioning curves for others
$libinclude      xlimport        abschreibung_z          %xcel_datadir%abschreibung.xlsx       abschreibung_z!A3:K165
$libinclude      xlimport        abschreibung_bmk_z      %xcel_datadir%abschreibung.xlsx       abschreibung_bmk_z!A170:K332


$libinclude      xlimport        abschreibung_noCOAL        %xcel_datadir%abschreibung_coal_decomission_DEU.xlsx     abschreibung!A3:K327             
$libinclude      xlimport        abschreibung_bmk_noCOAL    %xcel_datadir%abschreibung_coal_decomission_DEU.xlsx     abschreibung_bmk!A3:K327    
display abschreibung, abschreibung_bmk, abschreibung_z, abschreibung_bmk_z;

* 12.08.2018 -------READ Ele_dev
* ---------------------------------------------------------------------------- *
* ----- 23.08.2018 - test times coupling
$libinclude      xlimport        ele_dev_switch                %xcel_datadir%ele_dev_switch.xlsx             ele_dev_switch!A1:j19 
$libinclude      xlimport        ele_dev                       %xcel_datadir%ele_dev.xlsx                    ele_dev!A1:K163 
$libinclude      xlimport        ele_dev_REEEM_path            %xcel_datadir%ele_dev.xlsx                    ele_dev_REEEM_path!A1:K163 
$libinclude      xlimport        ele_dev_REEEM                 %xcel_datadir%ele_dev.xlsx                    ele_dev_REEEM!A1:D163
$libinclude      xlimport        ele_calib_2015                %xcel_datadir%ele_dev.xlsx                    ele_calib_2015!A1:s19

display ele_dev;


* ---------------------------------------------------------------------------- *
* ---------------------------------------------------------------------------- *
*        READ co2-growth / co2-schalter
* ---------------------------------------------------------------------------- *
* ###### co2pfad_ets, co2pfad ##################################################
* ------ 12.05.2015 Einlesen der DEHst-Szenarien

* ------ 1.04.2016
$libinclude      xlimport        co2pfad_ets_eu          %xcel_datadir%160330_co2pfad.xlsx               EU-Pfad_ENAVI!a42:j43           # EU-Emissionsreduktionspfad gemäß historisch verfügbarer Zertifikate bis 2013 (http://www.eea.europa.eu/data-and-maps/data/data-viewers/emissions-trading-viewer) und der Emissionsreduktionsziele -21% in 2020 und -43% in 2030

$libinclude      xlimport        co2pfad_ets_row         %xcel_datadir%160330_co2pfad.xlsx              RoW-Pfad!A14:J24 

$libinclude      xlimport        co2pfad_nonets          %xcel_datadir%160330_co2pfad.xlsx              EU-Pfad_ENAVI!a46:j55

$libinclude      xlimport        co2pfad_DEU             %xcel_datadir%160330_co2pfad.xlsx   DEU-Pfad!A91:J95

$libinclude      xlimport        co2pfad                 %xcel_datadir%150730_co2pfad_BMWi.xlsx         co2pfad_nonets_vorher!B22:K40   # bleibt vorerst gleich [ORIGINAL]

$libinclude      xlimport        co2pfad_ets_eu28_ref    %xcel_datadir%160330_co2pfad.xlsx              Reference_2020!a3:j4

* ----- AP5 Paths

$onEcho > read_ARIADNE.txt
par = CO2_AP5_sce3   rng=sce_3!L7:V28   rDIM=2 cDim=1
par = CO2_AP5_sce4   rng=sce_4!L7:V28   rDIM=2 cDim=1
par = CO2_AP5_sce5   rng=sce_5!L7:V24   rDIM=2 cDim=1
par = CO2_AP5_sce6   rng=sce_6!L7:V24   rDIM=2 cDim=1
par = CO2_AP5_sce7   rng=sce_7!L7:V24   rDIM=2 cDim=1
par = CO2_AP5_sce8   rng=sce_8!L7:V24   rDIM=2 cDim=1
par = CO2_AP5_sce9   rng=sce_9!L7:V24   rDIM=2 cDim=1
par = CO2_AP5_sce10  rng=sce_10!L7:V24  rDIM=2 cDim=1
$offEcho
$call gdxxrw   %xcel_datadir%European_Targets_ARIADNE.xlsx  @read_ARIADNE.txt
$GDXIN   European_Targets_ARIADNE.gdx
$LOAD CO2_AP5_sce3   CO2_AP5_sce4   CO2_AP5_sce5   CO2_AP5_sce6
$LOAD CO2_AP5_sce7   CO2_AP5_sce8   CO2_AP5_sce9   CO2_AP5_sce10
$GDXIN





$call gdxxrw   %xcel_datadir%world_CO2.xlsx  par=CO2_AP5_world rng=NEWAGE_read!A15  rDim=1 cDim=1
$GDXIN   world_CO2.gdx
$LOAD CO2_AP5_world
$GDXIN

* ---------------------------------------------------------------------------- *
*        READ renewable energies cost reduction
* ---------------------------------------------------------------------------- *
* ###### cost_red ##############################################################
$libinclude      xlimport        cost_red        %xcel_datadir%cost_red.xlsx     cost_red!c2:l6
display cost_red;

* ---------------------------------------------------------------------------- *
*        READ Differenzkosten / schalter für diffcostsubvention
* ---------------------------------------------------------------------------- *
* ###### diffcost und difccost_exogen ##########################################
*$libinclude      xlimport        diffcost        %xcel_datadir%diffcost.xlsx     diffcost!b2:g20
*$libinclude      xlimport        diffcost_exogen %xcel_datadir%diffcost.xlsx     diffcost_exogen!b2:g20

*$libinclude      xlimport        diffcost        %xcel_datadir%diffcost.xlsx     diffcost!b25:t30
*$libinclude      xlimport        diffcost_exogen %xcel_datadir%diffcost.xlsx     diffcost_exogen!b25:t30
* ------ 3.02.2016 BAW entfernen
$libinclude      xlimport        diffcost        %xcel_datadir%diffcost.xlsx     diffcost!b24:s29
$libinclude      xlimport        diffcost_exogen %xcel_datadir%diffcost.xlsx     diffcost_exogen!b24:s29


display diffcost, diffcost_exogen;

*diffcost(gen,r)=diffcost_exogen(gen,r);

* ---------------------------------------------------------------------------- *
*        READ nucsize (scenario specific parameter declaration and definition)
* ---------------------------------------------------------------------------- *
* ###### nucsize ###############################################################
*$libinclude      xlimport        nucsize         %xcel_datadir%nucsize.xlsx      nucsize!B22:k39
* ------ 3.02.2016 BAW entfernen
$libinclude      xlimport        nucsize         %xcel_datadir%nucsize.xlsx      nucsize!B22:k40
display nucsize;

* ---------------------------------------------------------------------------- *
*        READ Rohölpreise (pytarget_yr)                                         // ÄNDERUNG 13.02.2013
* ---------------------------------------------------------------------------- *
* ###### pytarget_yr ###########################################################
*$libinclude      xlimport        pytarget_yr     %xcel_datadir%pytarget.xlsx     pytarget!B2:l20
* ------ 16.02.2015 RWE-Projekt
*$libinclude      xlimport        pytarget_yr     %xcel_datadir%pytarget.xlsx     pytarget_RWE!B2:l19
* ------ 3.02.2016 BAW entfernen
*$libinclude      xlimport        pytarget_yr     %xcel_datadir%pytarget.xlsx     pytarget_RWE!B2:l56
*display  pytarget_yr;

*-----25.08.17 use energy prices from REMIND results
$libinclude xlimport pytarget_yr %xcel_datadir%pytarget.xlsx   pytarget_REMIND!B2:L58
$libinclude xlimport pytarget_E3_yr %xcel_datadir%pytarget.xlsx   pytarget_REMIND_E3!B2:L58

display  pytarget_yr;

* ---------------------------------------------------------------------------- *
*        READ growth rates for renewable energy sources
* ---------------------------------------------------------------------------- *
* ###### ressize ###############################################################
*$libinclude      xlimport        ressize         %xcel_datadir%ressize.xlsx      ressize!B2:M74
* ------ 3.02.2016 BAW entfernen
$libinclude      xlimport        ressize         %xcel_datadir%ressize.xlsx      ressize!B73:L145
display  ressize ;

* ---------------------------------------------------------------------------- *
*        READ gdp-growth rate / labor-growth rate
* ---------------------------------------------------------------------------- *
* ###### lfhc_usk, lfhc_skl ####################################################
*$libinclude      xlimport        lfhc_usk        %xcel_datadir%size_18x18x4.xlsx  size_18x18x4!G21:Q39
*$libinclude      xlimport        lfhc_skl        %xcel_datadir%size_18x18x4.xlsx  size_18x18x4!G2:P20
* ------ 3.02.2016 BAW entfernen
* ------ 5.02.2016 size_17x18x4.xlsx verändert um Wirtschaftswachstum in DEU zu erhöhen!
$libinclude      xlimport        lfhc_usk        %xcel_datadir%size_18x18x4.xlsx  size_18x18x4!G21:P39
$libinclude      xlimport        lfhc_skl        %xcel_datadir%size_18x18x4.xlsx  size_18x18x4!G2:P20
*------- 23.08.2016 command line to get data for new mapping
*$libinclude      xlimport        lfhc_usk        %xcel_datadir%size_18x18x4.xlsx  size_18x18x4!G21:P39
*$libinclude      xlimport        lfhc_skl        %xcel_datadir%size_18x18x4.xlsx  size_18x18x4!G2:P20
display lfhc_usk, lfhc_skl;

* ---------------------------------------------------------------------------- *
*        READ specifiactions for unemployment and labor market modeling
* ---------------------------------------------------------------------------- *
* ###### urun0, ursk0 ##########################################################
*$libinclude      xlimport        ur_read         %xcel_datadir%ursk0+urun0.xlsx  ur!b2:d20
* ------ 3.02.2016 BAW entfernen
$libinclude      xlimport        ur_read         %xcel_datadir%ursk0+urun0.xlsx  ur!b2:d20
display ur_read;



ursk0(r) = ur_read(r,"ursk0"); display ursk0;
urun0(r) = ur_read(r,"urun0"); display urun0;

* ---------------------------------------------------------------------------- *
*        READ Wirkungsgradsteigerungen
* ---------------------------------------------------------------------------- *
* ###### wg0, wg_yr ########################################################## *
*$libinclude      xlimport        wg0             %xcel_datadir%wg.xlsx           wg0!a2:s20
*$libinclude      xlimport        wg_yr           %xcel_datadir%wg.xlsx           wg_yr!a2:k20
* ------ 3.02.2016 BAW entfernen
$libinclude      xlimport        wg0             %xcel_datadir%wg.xlsx           wg0!a2:s20
$libinclude      xlimport        wg_yr           %xcel_datadir%wg.xlsx           wg_yr!a2:j20

display wg0, wg_yr;

* ---------------------------------------------------------------------------- *
*        Energy elasticity
* ---------------------------------------------------------------------------- *

$libinclude      xlimport        elast_ene_yr           %xcel_datadir%elast_ene.xlsx       elast!a1:j19

* ---------------------------------------------------------------------------- *
*        Energy productivity and total factor productivity from the BASELINE Database
* ---------------------------------------------------------------------------- *

$libinclude      xlimport        tfp            %xcel_datadir%ep+tfp.xlsx       tfp_newage!A32:s41
$libinclude      xlimport        tfp_corona     %xcel_datadir%ep+tfp.xlsx       tfp_corona!A32:s41

* ------ 16.07.2014 ORIGINAL
*$libinclude      xlimport         ep     %xcel_datadir%ep+tfp.xlsx        ep_newage!A30:R40
* ------ 16.07.2014 Halber Zuwachs
$libinclude      xlimport         ep     %xcel_datadir%ep+tfp.xlsx        ep_newage!A91:s100


display ep, tfp;

* ########################################################################### *

EXECUTE_UNLOAD '%datadir%%output%_XCEL.gdx'

display diffcost;

* ############################################################################ *
*$offtext

$EXIT






