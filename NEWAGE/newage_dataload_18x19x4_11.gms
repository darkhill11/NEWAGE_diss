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



* ------ 15.07.2014 Reading in Energy productivity and total factor productivity from the BASELINE Database
         lfhc_usk(r,yr)                  Labor force and human capital development for unskilled labor (BASELINE Database)
         lfhc_skl(r,yr)                  Labor force and human capital development for   skilled labor (BASELINE Database)

         ep(yr,r)                        Energy productivity development (BASELINE Database)
         tfp(yr,r)                       Total factor productivity development (BASELINE Database)

         tfp_corona(r)                   Multiplier for TFP to match the GDP development affected by the COVID-19 pandemic

*-------27.10.2017 CO2 growth
         co2pfad_eu(*,yr)     CO2-growth in the EU (all sectors)

         co2pfad_eu_ets(*,yr)    CO2-growth in the EU (ETS sectors)

         co2pfad_E3_eu(*,yr)

         co2pfad_E4_eu(*,yr)

         co2pfad_eu_nonets(r,yr)    CO2-growth in the EU (non ETS sectors) - INDC scenario
         co2pfad_eu_nonets_2d(r,yr) CO2-growth in the EU (non ETS sectors) - 2 degree scenario

         co2pfad_row(r,yr)    CO2-growth in the RoW (non-EU) - INDC scenario
         co2pfad_row_2d(r,yr)    CO2-growth in the RoW (non-EU) - 2 degree scenario

         co2pfad_DEU(sec,yr)     sector-specific CO2-growth in Germany
*-------27.10.2017 end

* ------ 28.05.2015
         co2pfad_ets_eu(*,yr)

*-------22.08.2017
         co2pfad_ets_row(r,yr)            CO2-growth in the RoW (non-EU)
         co2pfad_ets_row_2dc(r,yr)        CO2-growth in the non-EU contries for a 2 dC target
         co2pfad_nonets(r,yr)             CO2 growth for non-ETS
         co2pfad_nonets_2dc(r,yr)             CO2 growth for non-ETS

* ----- 05.02.2018
         co2pfad_cluni_ets(*,yr)          CO2-growth for ets sectors on cluster union REEEM scenario
         co2pfad_cluni_nonets(r,yr)       CO2-growth for non-ets sectors on cluster union REEEM scenario

* ----- 20.02.2018
         co2pfad_regpush(r,yr)            CO2-growth for countries outside the eU28
         
* ----- 05.03.2018 CO2-growth path for ets sectors under the tighther cap scenario
         co2pfad_REEEM_ets90(*,yr)
         co2pfad_REEEM_ets95(*,yr)
         co2pfad_REEEM_ALL90(*,yr)
         co2pfad_REEEM_ALL95(*,yr)

* ------- 14.11.2017 data for PA pfad
         vy_pa_pfad(j,i,yr)
         vc_pa_pfad(i,yr)
         velex_pa_pfad(i,gen,yr)
         velen_pa_pfad(i,gen,yr)

* ------- 18.10.2018 elasticities for energy
         elast_ene_yr(r,yr)

*         tp_no_vat(r,i)    tax for consumers witout VAT

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

$libinclude      xlimport        aeei_ff           %xcel_datadir%aeei.xlsx       AEEI_ff_2!a2:k137
$libinclude      xlimport        aeei_ele           %xcel_datadir%aeei.xlsx       AEEI_ele_2!a2:k128
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
*$libinclude      xlimport        co2pfad_ets             %xcel_datadir%150730_co2pfad_BMWi.xlsx         DEHst_1vs4!B90:k96       # DEHst Szenario 1 - Status Quo
$libinclude      xlimport        co2pfad_ets             %xcel_datadir%150730_co2pfad_BMWi.xlsx         DEHst_1vs4!B72:k81       # DEHst Szenario 1 - Status Quo (aktualisiert) [REF]

*$libinclude      xlimport        co2pfad_ets_eu          %xcel_datadir%150730_co2pfad_BMWi.xlsx         EU-Pfad!B41:l42          # REF EU28-Cap
*$libinclude      xlimport        co2pfad_ets_eu          %xcel_datadir%150730_co2pfad_BMWi.xlsx         EU-Pfad!B77:k78          # Szenario 2 - KOM     [FC2 EU28-Pfad]
*$libinclude      xlimport        co2pfad_ets_eu          %xcel_datadir%150730_co2pfad_BMWi.xlsx         EU-Pfad!B81:k82          # Szenario 4 - BMWi    [FC2 EU28-Pfad]
*$libinclude      xlimport        co2pfad_ets_eu          %xcel_datadir%150730_co2pfad_BMWi.xlsx         EU-Pfad!B73:k74          # Szenario 1 - Status-Quo  [FC1 EU28-Pfad]
* ------ 1.04.2016
$libinclude      xlimport        co2pfad_ets_eu          %xcel_datadir%160330_co2pfad.xlsx               EU-Pfad_ENAVI!a42:j43           # EU-Emissionsreduktionspfad gemäß historisch verfügbarer Zertifikate bis 2013 (http://www.eea.europa.eu/data-and-maps/data/data-viewers/emissions-trading-viewer) und der Emissionsreduktionsziele -21% in 2020 und -43% in 2030
$libinclude      xlimport        co2pfad_E3_eu          %xcel_datadir%160330_co2pfad.xlsx               EU-Pfad_ENAVI!a11:j12
*$libinclude      xlimport        co2pfad_ets_eu          %xcel_datadir%160330_co2pfad.xlsx               EU-Pfad_ENAVI!a66:j67
$libinclude      xlimport        co2pfad_E4_eu          %xcel_datadir%160330_co2pfad.xlsx               EU-Pfad_ENAVI!a75:j76
$libinclude      xlimport        co2pfad_cluni_ets           %xcel_datadir%160330_co2pfad.xlsx              EU-Pfad_REEEM!a51:j52

*$libinclude      xlimport        co2pfad_ets_eu          %xcel_datadir%160330_co2pfad.xlsx               EU-Pfad!B118:k119           # 95% decrease in 2050

*-------22.08.2017   read data on emission reduction targets for RoW according to the INDCs
*$libinclude      xlimport        co2pfad_ets_row                  %xcel_datadir%160330_co2pfad.xlsx              RoW-Pfad!A14:J24
$libinclude      xlimport        co2pfad_ets_row                  %xcel_datadir%160330_co2pfad.xlsx              RoW-Pfad!A14:J24 
$libinclude      xlimport        co2pfad_ets_row_2dc              %xcel_datadir%160330_co2pfad.xlsx              RoW-Pfad!A2:J12
$libinclude      xlimport        co2pfad_regpush                  %xcel_datadir%160330_co2pfad.xlsx              RoW-Pfad!A29:J38

$libinclude      xlimport        co2pfad_nonets                   %xcel_datadir%160330_co2pfad.xlsx              EU-Pfad_ENAVI!a46:j55
$libinclude      xlimport        co2pfad_cluni_nonets             %xcel_datadir%160330_co2pfad.xlsx              EU-Pfad_REEEM!a55:j64
$libinclude      xlimport        co2pfad_nonets_2dc               %xcel_datadir%160330_co2pfad.xlsx              EU-Pfad!b151:k160

$libinclude      xlimport        co2pfad_REEEM_ets90             %xcel_datadir%160330_co2pfad.xlsx              EU-Pfad_REEEM!a72:j73
$libinclude      xlimport        co2pfad_REEEM_ets95             %xcel_datadir%160330_co2pfad.xlsx              EU-Pfad_REEEM!a81:j82
$libinclude      xlimport        co2pfad_REEEM_ALL90             %xcel_datadir%160330_co2pfad.xlsx              EU-Pfad_REEEM!a93:j94
$libinclude      xlimport        co2pfad_REEEM_ALL95             %xcel_datadir%160330_co2pfad.xlsx              EU-Pfad_ENAVI!a66:j67


*-------14.09.2017   read data on sector-specific emission reduction targets for Germany according to the Klimaschutzplan 2050 and the UNFCC paper
*$libinclude   xlimport co2pfad_DEU    %xcel_datadir%160330_co2pfad.xlsx      DEU-Pfad!A38:J44
*commented out because co2pfad_DEU from DEU-Pfad-2 is used instead! (see lines below)

*-------20.10.2017   read data on sector-specific emission reduction targets for Germany according to the Klimaschutzplan 2050 and the UNFCC paper
*difference of DEU-Pfad 2 (compared to DEU-Pfad): process emissions from industry are distinguished and emission reduction in industry sector is therefore less stringent (meaning that reduction in other sectors must be stronger)
*$libinclude   xlimport co2pfad_DEU    %xcel_datadir%160330_co2pfad.xlsx      DEU-Pfad-2!A38:J44

*-------27.10.2017
$libinclude xlimport co2pfad_DEU    %xcel_datadir%160330_co2pfad.xlsx   DEU-Pfad!A91:J95
*-------27.10.2017 end

*$libinclude      xlimport        co2pfad_ets             %xcel_datadir%150730_co2pfad_BMWi.xlsx         DEHst_1vs4!B81:k87      # DEHst Szenario 4 - ETS ambitioniert
*$libinclude      xlimport        co2pfad                 %xcel_datadir%150730_co2pfad_BMWi.xlsx         co2pfad_nonets!B22:k40   # bleibt vorerst gleich [ORIGINAL]   // mit BAW
* ------ 3.02.2016 BAW entfernen
$libinclude      xlimport        co2pfad                 %xcel_datadir%150730_co2pfad_BMWi.xlsx         co2pfad_nonets_vorher!B22:K40   # bleibt vorerst gleich [ORIGINAL]

$libinclude      xlimport        co2pfad_ets_eu28_ref    %xcel_datadir%160330_co2pfad.xlsx              Reference_2020!a3:j4

* ------ 28.11.2014 (A) Sinkender Differenzbetrag (% vom Vorjahr, konstante Belastung)
*$libinclude      xlimport        co2pfad_ets     %xcel_datadir%co2pfad_80.xlsx      co2pfad_ets!A21:K27           # ETS-80-Ziel Version (A) [ORIGINAL]
*$libinclude      xlimport        co2pfad         %xcel_datadir%co2pfad_80.xlsx      co2pfad_nonets!B2:L20         # bleibt vorerst gleich   [ORIGINAL]

* ------ 28.11.2014 Vergleich ETS60 mit ETS75
*$libinclude      xlimport        co2pfad_ets     %xcel_datadir%co2pfad_70.xlsx   co2pfad_ets!A21:K27             # ETS-70 Ziel Version (A)
*$libinclude      xlimport        co2pfad         %xcel_datadir%co2pfad_70.xlsx   co2pfad_nonets!B2:L20           # bleibt vorerst gleich



* ------ 10.07.2014 (B) Konstanter Differenzbetrag (% von 2013, steigende Belastung)
*$libinclude      xlimport        co2pfad_ets     %xcel_datadir%co2pfad.xlsx      co2pfad_ets!A50:K56             # ETS-60 Ziel Version (B)
*$libinclude      xlimport        co2pfad         %xcel_datadir%co2pfad.xlsx      co2pfad_nonets!B2:L20           # bleibt vorerst gleich
display co2pfad, co2pfad_ets, co2pfad_ets_eu, co2pfad_ets_row, co2pfad_ets_row_2dc, co2pfad_nonets, co2pfad_nonets_2dc;

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
$libinclude      xlimport        tfp_corona     %xcel_datadir%ep+tfp.xlsx       tfp_corona!A1:r2

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






