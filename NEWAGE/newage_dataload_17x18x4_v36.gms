$Title Satellite code to transform LIBINCLUDE Data from Excel to GDX
$onempty

$oneolcom
$eolcom #
* if you want a comment in brackets, like: y = 2 ; { this is also a comment } z = 3 ; type:
*$inlinecom {}
* --------------------------------------------------------------------- *

* ------ Definiere den Lösungsalgorithmus (Solver) für das MCP-Problem
OPTION   MCP = PATH ;

*$if not set source       $set source     26x18x4_baw
*$if not set source       $set source     17x18x4_baw_new
*$if not set source       $set source     18x18x4
$if not set source       $set source     17x18x4_gtap9

* ------ 10.04.2014 OUTPUT
$if not set output       $set output     %source%
* ------ 10.04.2014 DataSet
$if not set ds           $set ds         %output%
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

* ---------------------------------------------------------------------------- *
*        SET and additional sets (SUBSETS) for DYNAMIC MILESTONES
* ---------------------------------------------------------------------------- *

* ------ 17.01.2012 ORIGINAL Zeithorizont bis 2050
*$ontext
SET      YR MILESTONES  / 2007, 2010, 2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050    / ;

* ---------------------------------------------------------------------------- *
*        Define ALIAS indices for interactions etc.
* ---------------------------------------------------------------------------- *
ALIAS    (i,j), (r,s), (r,rr), (gen,gengen), (yr,yryr);


* ##############################################################################
* ---------------------------------------------------------------------------- *
* ------ Parameter declaration for exogenous data ---------------------------- *
* ---------------------------------------------------------------------------- *

PARAMETER
         ele_prod(gen,r)                 Benchmark electricity production [TWh]
         ressize(r,gen,yr)               Growth rates for renewable energy sources
         co2pfad(r,yr)                   CO2-growth
         co2pfad_ets(r,yr)               CO2-growth in the EU-ETS
         pytarget_yr(r,i,yr)             Exogenous price paths for resources

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

* ------ 10.06.2013: Zusammenfassung aller AEEI-Pfade in Tabelle aeei.xlsx
         aeei_read(*,yr)                 Einlesen aller AEEI-Pfade über aggregierte Tabelle aus aeei.xlsx

         AEEI_exo(yr)                    AEEI exogen
         AEEI_exo_emerge(yr)             AEEI exogen emerge
         AEEI_exo_deu(yr)                AEEI exogen deu
         AEEI_exo_neu(yr)                AEEI exogen neu
         AEEI_exo_ele_c(yr)              AEEI exogen ele_c
         AEEI_exogen(yr)                 AEEI exogen für deutschland
         AEEI_exogen_after2020(yr)       AEEI exogen für deutschland nach erreichen von mckinsey
         AEEI_exogen_after2025(yr)       AEEI exogen für deutschland nach erreichen von mckinsey II
         AEEI_exogen_gen_yr(yr)          AEEI exogen für den einstaz von fe in stromerzeugung in deu!
         AEEI_exogen_gen_yr_after2020(yr) AEEI exogen electricity generation nach erreichen von mckinsey
         AEEI_exogen_gen_yr_after2025(yr) AEEI exogen electricity generation nach erreichen von mckinsey II
         AEEI_exogen_ele(yr)             AEEI exogen für deutschland stromverbrauch
         AEEI_exogen_ele_after2020(yr)   AEEI exogen für deutschland stromverbrauch nach erreichen von mckinsey
         AEEI_exogen_ele_after2025(yr)   AEEI exogen für deutschland stromverbrauch nach erreichen von mckinsey II
         AEEI_exogen_trn(yr)             AEEI exogen für deutschland TRN
         AEEI_exogen_trn_after2020(yr)   AEEI exogen für deutschland TRN nach erreichen von mckinsey
         AEEI_exogen_trn_after2025(yr)   AEEI exogen für deutschland TRN nach erreichen von mckinsey II
         AEEI_exogen_hh(yr)              AEEI exogen für deutschland HH
         AEEI_exogen_hh_after2020(yr)    AEEI exogen für deutschland HH nach erreichen von mckinsey
         AEEI_exogen_hh_after2025(yr)    AEEI exogen für deutschland HH nach erreichen von mckinsey II


* ------ 15.07.2014 Reading in Energy productivity and total factor productivity from the BASELINE Database
         lfhc_usk(r,yr)                  Labor force and human capital development for unskilled labor (BASELINE Database)
         lfhc_skl(r,yr)                  Labor force and human capital development for   skilled labor (BASELINE Database)

         ep(yr,r)                        Energy productivity development (BASELINE Database)
         tfp(yr,r)                       Total factor productivity development (BASELINE Database)

* ------ 28.05.2015
         co2pfad_ets_eu(*,yr)
;

*$ontext
* ---------------------------------------------------------------------------- *
*        READ AEEI
* ---------------------------------------------------------------------------- *
* ###### AEEI ##################################################################
* Modellparameter AEEI basiert auf AEEI_yr, welches im Wesentlichen auf AEEI_EXO beruht.(AEEI_EXOGEN... = AEEI_Variante)
* REF: AEEI = AEEI_HH = AEEI_TRN

$libinclude      xlimport        aeei_read       %xcel_datadir%aeei.xlsx       aeei!b2:m41

aeei_exo(yr)                     = aeei_read("aeei_exo",yr);
aeei_exo_ele_c(yr)               = aeei_read("aeei_exo_ele_c",yr);
aeei_exo_deu(yr)                 = aeei_read("aeei_exo_deu",yr);
aeei_exo_neu(yr)                 = aeei_read("aeei_exo_neu",yr);
aeei_exo_emerge(yr)              = aeei_read("aeei_exo_emerge",yr);
aeei_exogen(yr)                  = aeei_read("aeei_exogen",yr);

aeei_exogen_after2020(yr)        = aeei_read("aeei_exogen_after2020",yr);
aeei_exogen_after2025(yr)        = aeei_read("aeei_exogen_after2025",yr);
aeei_exogen_ele(yr)              = aeei_read("aeei_exogen_ele",yr);
aeei_exogen_ele_after2020(yr)    = aeei_read("aeei_exogen_ele_after2020",yr);
aeei_exogen_ele_after2025(yr)    = aeei_read("aeei_exogen_ele_after2025",yr);
aeei_exogen_gen_yr(yr)           = aeei_read("aeei_exogen_gen_yr",yr);
aeei_exogen_gen_yr_after2020(yr) = aeei_read("aeei_exogen_gen_yr_after2020",yr);
aeei_exogen_gen_yr_after2025(yr) = aeei_read("aeei_exogen_gen_yr_after2025",yr);
aeei_exogen_hh(yr)               = aeei_read("aeei_exogen_hh",yr);
aeei_exogen_hh_after2020(yr)     = aeei_read("aeei_exogen_hh_after2020",yr);
aeei_exogen_hh_after2025(yr)     = aeei_read("aeei_exogen_hh_after2025",yr);
aeei_exogen_trn(yr)              = aeei_read("aeei_exogen_trn",yr);
aeei_exogen_trn_after2020(yr)    = aeei_read("aeei_exogen_trn_after2020",yr);
aeei_exogen_trn_after2025(yr)    = aeei_read("aeei_exogen_trn_after2025",yr);

display
aeei_read
AEEI_exo, AEEI_exo_emerge, AEEI_exo_deu, AEEI_exo_neu, AEEI_exo_ele_c,
AEEI_exogen_gen_yr, AEEI_exogen_gen_yr_after2020, AEEI_exogen_gen_yr_after2025
AEEI_exogen, AEEI_exogen_after2020, AEEI_exogen_after2025
AEEI_exogen_ele, AEEI_exogen_ele_after2020, AEEI_exogen_ele_after2025
AEEI_exogen_trn, AEEI_exogen_trn_after2020, AEEI_exogen_trn_after2025
AEEI_exogen_hh, AEEI_exogen_hh_after2020, AEEI_exogen_hh_after2025
;


* ---------------------------------------------------------------------------- *
*        READ Decommissioning (Sterbelinien)
* ---------------------------------------------------------------------------- *
* ###### abschreibung ##########################################################
* ------ include abschreibung for decommissioning curves for all regions (basically already includes abschreibung_z and abschreibung_bmk_z)
*$libinclude      xlimport        abschreibung            %xcel_datadir%abschreibung.xlsx       abschreibung!A3:L327       // mit BAW
*$libinclude      xlimport        abschreibung_bmk        %xcel_datadir%abschreibung.xlsx       abschreibung_bmk!A3:L327   // mit BAW
* ------ 3.02.2016 BAW entfernen
$libinclude      xlimport        abschreibung            %xcel_datadir%abschreibung.xlsx       abschreibung!A3:L309
$libinclude      xlimport        abschreibung_bmk        %xcel_datadir%abschreibung.xlsx       abschreibung_bmk!A3:L309

* ###### abschreibung_z ########################################################
* ------ include abschreibung for decommissioning curves for others
$libinclude      xlimport        abschreibung_z          %xcel_datadir%abschreibung.xlsx       abschreibung_z!A3:L201
$libinclude      xlimport        abschreibung_bmk_z      %xcel_datadir%abschreibung.xlsx       abschreibung_bmk_z!A3:L201
display abschreibung, abschreibung_bmk, abschreibung_z, abschreibung_bmk_z;

* ---------------------------------------------------------------------------- *
*        READ co2-growth / co2-schalter
* ---------------------------------------------------------------------------- *
* ###### co2pfad_ets, co2pfad ##################################################
* ------ 12.05.2015 Einlesen der DEHst-Szenarien
*$libinclude      xlimport        co2pfad_ets             %xcel_datadir%150730_co2pfad_BMWi.xlsx         DEHst_1vs4!B90:L96       # DEHst Szenario 1 - Status Quo
$libinclude      xlimport        co2pfad_ets             %xcel_datadir%150730_co2pfad_BMWi.xlsx         DEHst_1vs4!B72:L78       # DEHst Szenario 1 - Status Quo (aktualisiert) [REF]

*$libinclude      xlimport        co2pfad_ets_eu          %xcel_datadir%150730_co2pfad_BMWi.xlsx         EU-Pfad!B41:L42          # REF EU28-Cap
*$libinclude      xlimport        co2pfad_ets_eu          %xcel_datadir%150730_co2pfad_BMWi.xlsx         EU-Pfad!B77:L78          # Szenario 2 - KOM     [FC2 EU28-Pfad]
*$libinclude      xlimport        co2pfad_ets_eu          %xcel_datadir%150730_co2pfad_BMWi.xlsx         EU-Pfad!B81:L82          # Szenario 4 - BMWi    [FC2 EU28-Pfad]
*$libinclude      xlimport        co2pfad_ets_eu          %xcel_datadir%150730_co2pfad_BMWi.xlsx         EU-Pfad!B73:L74          # Szenario 1 - Status-Quo  [FC1 EU28-Pfad]
* ------ 1.04.2016
$libinclude      xlimport        co2pfad_ets_eu          %xcel_datadir%160330_co2pfad.xlsx               EU-Pfad!B69:L70           # EU-Emissionsreduktionspfad gemäß historisch verfügbarer Zertifikate bis 2013 (http://www.eea.europa.eu/data-and-maps/data/data-viewers/emissions-trading-viewer) und der Emissionsreduktionsziele -21% in 2020 und -43% in 2030

*$libinclude      xlimport        co2pfad_ets             %xcel_datadir%150730_co2pfad_BMWi.xlsx         DEHst_1vs4!B81:L87      # DEHst Szenario 4 - ETS ambitioniert
*$libinclude      xlimport        co2pfad                 %xcel_datadir%150730_co2pfad_BMWi.xlsx         co2pfad_nonets!B2:L20   # bleibt vorerst gleich [ORIGINAL]   // mit BAW
* ------ 3.02.2016 BAW entfernen
$libinclude      xlimport        co2pfad                 %xcel_datadir%150730_co2pfad_BMWi.xlsx         co2pfad_nonets!B2:L19   # bleibt vorerst gleich [ORIGINAL]


* ------ 28.11.2014 (A) Sinkender Differenzbetrag (% vom Vorjahr, konstante Belastung)
*$libinclude      xlimport        co2pfad_ets     %xcel_datadir%co2pfad_80.xlsx      co2pfad_ets!A21:K27           # ETS-80-Ziel Version (A) [ORIGINAL]
*$libinclude      xlimport        co2pfad         %xcel_datadir%co2pfad_80.xlsx      co2pfad_nonets!B2:L20         # bleibt vorerst gleich   [ORIGINAL]

* ------ 28.11.2014 Vergleich ETS60 mit ETS75
*$libinclude      xlimport        co2pfad_ets     %xcel_datadir%co2pfad_70.xlsx   co2pfad_ets!A21:K27             # ETS-70 Ziel Version (A)
*$libinclude      xlimport        co2pfad         %xcel_datadir%co2pfad_70.xlsx   co2pfad_nonets!B2:L20           # bleibt vorerst gleich



* ------ 10.07.2014 (B) Konstanter Differenzbetrag (% von 2013, steigende Belastung)
*$libinclude      xlimport        co2pfad_ets     %xcel_datadir%co2pfad.xlsx      co2pfad_ets!A50:K56             # ETS-60 Ziel Version (B)
*$libinclude      xlimport        co2pfad         %xcel_datadir%co2pfad.xlsx      co2pfad_nonets!B2:L20           # bleibt vorerst gleich
display co2pfad, co2pfad_ets, co2pfad_ets_eu;

* ---------------------------------------------------------------------------- *
*        READ renewable energies cost reduction
* ---------------------------------------------------------------------------- *
* ###### cost_red ##############################################################
$libinclude      xlimport        cost_red        %xcel_datadir%cost_red.xlsx     cost_red!c2:n6
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
*$libinclude      xlimport        nucsize         %xcel_datadir%nucsize.xlsx      nucsize!B2:L20
* ------ 3.02.2016 BAW entfernen
$libinclude      xlimport        nucsize         %xcel_datadir%nucsize.xlsx      nucsize!B2:L19
display nucsize;

* ---------------------------------------------------------------------------- *
*        READ Rohölpreise (pytarget_yr)                                         // ÄNDERUNG 13.02.2013
* ---------------------------------------------------------------------------- *
* ###### pytarget_yr ###########################################################
*$libinclude      xlimport        pytarget_yr     %xcel_datadir%pytarget.xlsx     pytarget!B2:M20
* ------ 16.02.2015 RWE-Projekt
*$libinclude      xlimport        pytarget_yr     %xcel_datadir%pytarget.xlsx     pytarget_RWE!B2:M20
* ------ 3.02.2016 BAW entfernen
$libinclude      xlimport        pytarget_yr     %xcel_datadir%pytarget.xlsx     pytarget_RWE!B2:M19
display  pytarget_yr;

* ---------------------------------------------------------------------------- *
*        READ growth rates for renewable energy sources
* ---------------------------------------------------------------------------- *
* ###### ressize ###############################################################
*$libinclude      xlimport        ressize         %xcel_datadir%ressize.xlsx      ressize!B2:M74
* ------ 3.02.2016 BAW entfernen
$libinclude      xlimport        ressize         %xcel_datadir%ressize.xlsx      ressize!B2:M70
display  ressize ;

* ---------------------------------------------------------------------------- *
*        READ gdp-growth rate / labor-growth rate
* ---------------------------------------------------------------------------- *
* ###### lfhc_usk, lfhc_skl ####################################################
*$libinclude      xlimport        lfhc_usk        %xcel_datadir%size_18x18x4.xlsx  size_18x18x4!G21:Q39
*$libinclude      xlimport        lfhc_skl        %xcel_datadir%size_18x18x4.xlsx  size_18x18x4!G2:Q20
* ------ 3.02.2016 BAW entfernen
* ------ 5.02.2016 size_17x18x4.xlsx verändert um Wirtschaftswachstum in DEU zu erhöhen!
$libinclude      xlimport        lfhc_usk        %xcel_datadir%size_17x18x4.xlsx  size_17x18x4!G20:Q37
$libinclude      xlimport        lfhc_skl        %xcel_datadir%size_17x18x4.xlsx  size_17x18x4!G2:Q19
display lfhc_usk, lfhc_skl;

* ---------------------------------------------------------------------------- *
*        READ specifiactions for unemployment and labor market modeling
* ---------------------------------------------------------------------------- *
* ###### urun0, ursk0 ##########################################################
*$libinclude      xlimport        ur_read         %xcel_datadir%ursk0+urun0.xlsx  ur!b2:d20
* ------ 3.02.2016 BAW entfernen
$libinclude      xlimport        ur_read         %xcel_datadir%ursk0+urun0.xlsx  ur!b2:d19
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
$libinclude      xlimport        wg0             %xcel_datadir%wg.xlsx           wg0!a2:r20
$libinclude      xlimport        wg_yr           %xcel_datadir%wg.xlsx           wg_yr!a2:k20

display wg0, wg_yr;


* ---------------------------------------------------------------------------- *
*        Energy productivity and total factor productivity from the BASELINE Database
* ---------------------------------------------------------------------------- *

$libinclude      xlimport        tfp     %xcel_datadir%ep+tfp.xlsx       tfp_newage!A3:R13

* ------ 16.07.2014 ORIGINAL
*$libinclude      xlimport         ep     %xcel_datadir%ep+tfp.xlsx        ep_newage!A30:R40
* ------ 16.07.2014 Halber Zuwachs
$libinclude      xlimport         ep     %xcel_datadir%ep+tfp.xlsx        ep_newage!A59:R69


display ep, tfp;

* ############################################################################ *

EXECUTE_UNLOAD '%datadir%xlsdata_%output%.gdx'

display diffcost;

* ############################################################################ *
*$offtext

$EXIT






