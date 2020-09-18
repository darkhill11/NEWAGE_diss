$TITLE  NEWAGE-W
$ONEMPTY

* ====================================================================== *
*                          N E W A G E - World                           *
* ====================================================================== *
*                                *                                       *
*                        GTAP-EG based CGE-Model                         *
*                        Database GTAP 8 (2007)                          *
*                        17 Regions - 18 Sectors - 4 Factors             *
*                                *                                       *
*                        CO2 emissions trading system                    *
*                        Imperfect Labor Markets                         *
*                        BoUp Electricity Sector                         *
*                        Autonomous Energy Efficiency Improvements       *
*                        Endogenous Savings                              *
*                        BoUP household energy demand                    *
*                        Recursive dynamic 2007-2030                     *
*                                *                                       *
*                        Version February 2016                           *
*                        Beestermoeller                                  *
*                                *                                       *
* ====================================================================== *


* ------ 10.04.2014 SOURCE
*$if not set source  $set source     18x18x4
$if not set source  $set source     17x18x4_gtap9

* ------ 10.04.2014 OUTPUT
$if not set output       $set output     %source%
* ------ 10.04.2014 DataSet
* ------ 26.07.2016 Folgende Zeile für GTAP9 einkommentieren
$if not set ds           $set ds         %output%
* ------ 26.07.2016 Folgende Zeile für GTAP8 einkommentieren
*$if not set ds           $set ds         %output%FINAL

$if not set yr           $set yr         11
*$if not set yr           $set yr         11

$if not set datadir      $set datadir "..\data%yr%\"
$if not set xcel_datadir $set xcel_datadir "..\xcel_data\"
$if not set resultsdir   $set resultsdir "..\results\"
$setglobal datadir %datadir%

* ------ 26.07.2016
* ------ 26.07.2016 Folgende Zeile für GTAP8 einkommentieren
*$include ..\build\gtap8data
* ------ 26.07.2016 Folgende Zeile für GTAP9 einkommentieren
$include ..\build\gtap9data_newage

display s, j, g, f, vfm, vdfm, vom, vifm, vxmd, vst, vtwr, rto;
*display vdep, vkb, save;

parameter vom_total;
vom_total(r) = sum(g, vom(g,r));
*display vom_total;

* ------ 18.07.2016
parameter evd_sum2;
evd_sum2(r)= sum((i,g), evd(i,g,r)); display evd_sum2;
display evd;

*$exit

* xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
* ------------------------------------------------------------------------------
* ------ (A) Initial definitions
* ------------------------------------------------------------------------------

* ------ Definiere den Lösungsalgorithmus (Solver) für das MCP-Problem und die Anzahl der Dezimalstellen
OPTION   MCP = PATH ;
OPTION   decimals = 3;
alias (r,rr), (s,ss), (i,ii), (j,jj), (g,gg), (f,ff);
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
;

* ------ 15.04.2014 Create aggregated Intermediates (Armington)
parameter
         vafm(i,g,r)     Sum of domestic and imported intermediates
         rtfa(i,g,r)     Tax rate of for vafm
         rtfa0(i,g,r)    Tax rate of for vafm [benchmark];
vafm(i,g,r) = vifm(i,g,r) + vdfm(i,g,r);                                         // Summe über g entspricht a0(i,r) ohne "i"  ??
rtfa(i,g,r)$vafm(i,g,r) = (vifm(i,g,r)*rtfi(i,g,r) + vdfm(i,g,r)*rtfd(i,g,r)) / vafm(i,g,r) ;
rtfa0(i,g,r) = rtfa(i,g,r);

* xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
* ------------------------------------------------------------------------------
* ------ (B) Initial CHECKS
* ------------------------------------------------------------------------------

parameter chk_vxmd, chk_vtwr, chk_vifm, chk_vom_i, chk_vom_o, chk_vb;

*display vafm, rtfa0, rtfi0, rtfd0;

* ------ Sum of all VTWR must match sum of VST
chk_vtwr = round(sum((j,i,r,s), vtwr(j,i,r,s)) - sum((i,r), vst(i,r)),10);  display chk_vtwr;           // must be zero

* ------ Zero profit check for imports and exports --> Summe über die Verwendungsseite (g)
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

* xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
* ------------------------------------------------------------------------------
*        (C) Additional set (subset) definitions for REGIONS
* ------------------------------------------------------------------------------
* ------ 23.04.2014 18x18x4 Mapping
SET
*         bawdeu(r)       Germany and Baden-Württemberg   / deu, baw /
         deu(r)          Germany                         / deu /
*         baw(r)          Baden-Württemberg               / baw /
         fra(r)          France                          / fra /
         eus(r)          Southern EU                     / eus /
         eue(r)          Eastern EU                      / eue /
         eun(r)          Norhtern EU                     / eun /
         aut(r)          Austria                         / aut /
         rus(r)          Russia                          / rus /
         brz(r)          Brazil                          / brz /
         rsa(r)          South Africa                    / rsa /
         oec(r)          Other OECD countries            / oec /
         ope(r)          Other OPEC countries            / ope /
         swz(r)          Switzerland                     / swz /
         arb(r)          Arabian World + Iran            / arb /
         usa(r)          USA                             / usa /
         india(r)        India                           / ind /
         chi(r)          China                           / chi /
         EU28(r)         EU28
*                         / BAW, DEU, AUT, FRA, EUS, EUN, EUE  /
                         /       DEU, AUT, FRA, EUS, EUN, EUE  /
* ------ 5.05.2015 EU28 ohne Baden-Württemberg
         EU28x(r)        Dynamisches Set für EU28 ohne BAW für ETS
         EU28xx(r)       Dynamisches Set für EU28 ohne BAW für Non-ETS
         notrad(r)       Dynamisches Set für Länderspezifische CO2-Vorgaben
         pco2w_r(r)      Dynamisches Set für PCO2W und worldtrade (alle außer notrad)

         EU15(R)         EU15 = OEU
                         /      DEU, AUT, FRA, EUS, EUN       /
         NMS12(R)        NMS12 = NEU
                         / EUE /
         adreg(r)        Regions with exposed climate change risk and strong adaptation needs
                         / IND, ROW /
         OECD(r)         OECD
*                         / BAW, DEU, AUT, FRA, EUS, EUN, EUE, OEC, USA, SWZ  /
                         /       DEU, AUT, FRA, EUS, EUN, EUE, OEC, USA, SWZ  /
         OECD2(r)        OECD without EU28
                         / OEC, USA, SWZ /
         BRICS(r)        BRICS
                         / BRZ, RUS, IND, CHI, RSA /
         REST(r)         Rest of the world + ARB + OPE
                         / ARB, OPE, ROW /
         EMERGE(r)       Emerging markets with high growth but "low" unemployment growth and or high AEEI
*                        / BRZ,      IND, CHI, RSA, EUE,     /   // ORIGINAL
                         / BRZ, RUS, IND, CHI, RSA, EUE, ARB /   // the URSK constraint works better (without REDEF) if RUS and ARB are in emerge
         Annexb_EU(r)    Annex B Ratifizierer mit EU Gemäß 666 ZEW aber ohne RUS!
*                         / BAW, DEU, AUT, FRA, EUS, EUN, EUE, SWZ, OEC /
                         /       DEU, AUT, FRA, EUS, EUN, EUE, SWZ, OEC /
         Annexb(r)       Annex B Ratifizierer ohne EU gemäß 666 ZEW aber ohne RUS!
                         / SWZ, OEC /
         Annexb_broad(r) Annex B Ratifizierer ohne EU gemäß 666 ZEW mit USA aber ohne RUS!
                         / SWZ, OEC, USA /
         NUCPOT(R)       Länder mit begrenztem Nuclear-Zubau
                         / EUE, FRA, AUT, SWZ, EUN, EUS, USA, OEC, BRZ, RUS, IND, CHI, RSA, ARB, OPE, ROW /
         NUCOUT(R)       Länder mit Nuclear phase-out
*                         / DEU, BAW /
                         / DEU /

         groupA(r)       Adaptation group A - high vulnerability
                         / IND, ROW /
         groupB(r)       Adaptation group B - medium vulnerability
                         / AUT, SWZ, FRA, EUS, OEC, RSA, BRZ, OPE /
         groupC(r)       Adaptation group C - low vulnerability
*                         / DEU, BAW, EUN, EUE, USA, RUS, CHI, ARB /
                         /       DEU, EUN, EUE, USA, RUS, CHI, ARB /
* ------ 17.06.2013 Define additional sets for replacing "eab" and "rab"
         eab_s(r)        / OEC /
         rab_s(r)        / OEC /
* ------ 30.07.2013
         row(r)          Rest of the world / ROW /
*         woBAW(r)        All without BAW / DEU, FRA, AUT, SWZ, EUN, EUS, EUE, USA, BRZ, RUS, IND, CHI, RSA, OEC, ARB, OPE, ROW /
*         woIND(r)        All without IND / DEU, FRA, AUT, SWZ, EUN, EUS, EUE, USA, BRZ, RUS, BAW, CHI, RSA, OEC, ARB, OPE, ROW /
*         woROW(r)        All without IND / DEU, FRA, AUT, SWZ, EUN, EUS, EUE, USA, BRZ, RUS, BAW, CHI, RSA, OEC, ARB, OPE, IND /

         num(R)  Identifies the numeraire region
;

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
         ind(i)          Verarbeitendes Gewerbe - Manufacturing - Industrie
                         / CHM, PPP, IRS, NFM, NMM, FOT, MVH, MAC, ROI /
         indE(i)         Energieintensive Industrie
                         / CHM, PPP, IRS, NFM, NMM /
         indEN(i)        Nicht-Energieintensive Industrie
                         / MAC, FOT, MVH, ROI /
         agr(i)          Agriculture sector
                         / agr /
         agrind(i)       Agriculture and industry
                         / CHM, PPP, IRS, NFM, NMM, FOT, MVH, MAC, ROI, agr /
* ------ 13.09.2015 Diss
         mvh(i)          Motor vehicles                  / mvh /
         bui(i)          Building and construction       / bui /
         ser(i)          Building and construction       / ser /
         buiser(i)       Building and construction       / bui, ser /

* ------ 15.04.2014
         cg(g)           / c, g /;
;
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

SET      GEN             Stromerzeugungsarten
                         /  bNUC, bBC,  bBIO, bGEO,  bHYDRO, bHC,  bGAS, bOIL, bCCS,
                            mHC, mGAS, mWIND, mSOLAR, mOIL, mCCS, pOIL, pGAS, pHYDRO /
         reg(gen)        EEG-geförderte Erneuerbare              / bGEO, bHYDRO, bBIO, mWIND, mSOLAR /
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
         notreg(gen)     Nicht EEG-geförderte Erneuerbare        / bNUC, bBC,  bHC,  bGAS, bOIL, bCCS,
                                                                   mHC, mGAS, mOIL, mCCS, pOIL, pGAS, pHYDRO /
         bNUC(gen)       Nuclear Power                           / bNUC /
         bBC(gen)        Lignite                                 / bBC /
         FOScoal(gen)    Stromerzeugung Kohle (+CCS)             / bHC, mHC, bBC, bCCS, mCCS /
         FOScoalh(gen)   Stromerzeugung Steinkohle               / bHC, mHC /
         FOScoalb(gen)   Stromerzeugung Braunkohle               / bBC /
         FOSoil(gen)     Stromerzeugung Öl                       / bOil, mOil, pOil /
         FOSgas(gen)     Stromerzeugung Gas                      / bGas, mGas, pGas /
         FOSccs(gen)     Stromerzeugung CCS                      / bccs, mccs /
         FOSrest(gen)    Stromerzeugung Nicht-Fossil             / bGEO, bHYDRO, bBIO, mWIND, mSOLAR, pHydro, bNUC /
         coalgasccs(gen) Stromerzeugung Kohle-Gas-CCS            / bGas, mGas, pGas, bHC, mHC, bBC, bCCS, mCCS /
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
         fosnuc(GEN)     Stromerzeugung Fossil plus Nuclear      / bNUC, bBC, bHC, bGAS, bOIL, mHC, mGAS, mOIL, pOIL, pGAS, bCCS, mCCS /
* ------ 18.06.2014
         foslim(gen)     Generation technologies to be limited for ELEn.UP / bBC, bOIL, mOIL /
         reslim(gen)     Generation technologies to be limited for ELEn.UP / bHYDRO, bBIO, mWIND, pHYDRO /
;
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
* ------ 17.01.2012 ORIGINAL Zeithorizont bis 2050
*$ontext
SET      YR              MILESTONES
                         / 2007, 2010, 2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050    / ;
SET      BEFORE(YR)      / 2007                                          / ;
SET      BEFORE2050(YR)  / 2007,  2010, 2015, 2020, 2025, 2030, 2035, 2040, 2045     / ;
SET      BEFORE2045(YR)  / 2007,  2010, 2015, 2020, 2025, 2030, 2035, 2040     / ;
SET      BEFORE2040(YR)  / 2007,  2010, 2015, 2020, 2025, 2030, 2035     / ;
SET      BEFORE2035(YR)  / 2007,  2010, 2015, 2020, 2025, 2030    / ;
SET      BEFORE2030(YR)  / 2007,  2010, 2015, 2020, 2025          / ;
SET      BEFORE2025(YR)  / 2007,  2010, 2015, 2020                / ;
SET      BEFORE2020(YR)  / 2007,  2010, 2015                      / ;
SET      BEFORE2015(YR)  / 2007,  2010                            / ;
SET      BEFORE2010(YR)  / 2007                                   / ;
SET      AFTER(YR)       /        2010, 2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050  / ;
SET      AFTER2010(YR)   /              2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050  / ;
SET      AFTER2015(YR)   /                    2020, 2025, 2030, 2035, 2040, 2045, 2050  / ;
SET      AFTER2020(YR)   /                          2025, 2030, 2035, 2040, 2045, 2050  / ;
SET      AFTER2025(YR)   /                                2030, 2035, 2040, 2045, 2050  / ;
SET      AFTER2030(YR)   /                                      2035, 2040, 2045, 2050  / ;
SET      AFTER2035(YR)   /                                            2040, 2045, 2050  / ;
SET      AFTER2040(YR)   /                                                  2045, 2050  / ;
SET      AFTER2045(YR)   /                                                        2050  / ;
SET      KYOTO_OUT(YR)   /                     2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050  / ;
*$offtext
* ------ 17.01.2012 ORIGINAL (kann bleiben)
SET      YR2007(YR) / 2007 / ;
SET      YR2010(YR) / 2010 / ;
SET      YR2015(YR) / 2015 / ;
SET      YR2020(YR) / 2020 / ;
SET      YR2025(YR) / 2025 / ;
SET      YR2030(YR) / 2030 / ;
SET      YR2035(YR) / 2035 / ;
SET      YR2040(YR) / 2040 / ;
SET      YR2045(YR) / 2045 / ;
SET      YR2050(YR) / 2050 / ;

* ------ 3.11.2014
SET      YRx(yr)        Subset für 10-year-milestones Berechnungen
         / 2007, 2010, 2015, 2020, 2030, 2040, 2050 / ;
SET      AFTERx(yr)        Subset für 10-year-milestones Berechnungen
         /       2010, 2015, 2020, 2030, 2040, 2050 / ;
* ------ 3.11.2014 10-year-milestones (oben)


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
;

* ------ Taxes:
ty(i,r)          = rto(i,r);
ti(j,i,r)        = rtfa0(j,i,r);
ti(j,"i",r)      = rtfa0(j,"i",r);              // Besteuerung von "i"?
tg(i,r)          = rtfa0(i,"g",r);
tp(i,r)          = rtfa0(i,"c",r);
tf(f,i,r)        = rtf0(f,i,r);
tx(i,s,r)        = rtxs0(i,s,r) * (-1);          // rtxs ist explizit als Subvention definiert
tm(i,s,r)        = rtms0(i,s,r);
* ------ Prices
pmx0(i,s,r)      = pvxmd(i,s,r);                 // = (1+rtms0(i,s,r)) * (1-rtxs0(i,s,r))
pmt0(i,s,r)      = pvtwr(i,s,r);                 // =  1+rtms0(i,s,r)
pc0(i,r)         = 1 + rtfa0(i,"c",r);
pai0(i,j,r)      = 1 + rtfa0(i,j,r);
pai0(i,"i",r)    = 1 + rtfa0(i,"i",r);
pf0(f,i,r)       = 1 + rtf0(f,i,r);
py0(i,r)         = 1 - rto(i,r) ;
pwm(i,r,s)       = 1 ;                           // Weltmarktpreise

* ------ Adopt some notation which is different from the core GTAPinGAMS model (Aggregate data)
a0(i,r)  = sum(g, vafm(i,g,r));                                                          // Wie muss "i" behandelt werden? "i" bzw. cgd ist Teil von j in vdfm(i,j,r)
d0(i,r)  = sum(g, vdfm(i,g,r));
m0(i,r)  = sum(g, vifm(i,g,r));
c0(i,r)  = sum(g$cg(g), vdfm(i,g,r) + vifm(i,g,r));
tc(i,r)$c0(i,r) = (tp(i,r)*vafm(i,"c",r) + tg(i,r)*vafm(i,"g",r) ) / c0(i,r);
ct0(r)   = sum(i, c0(i,r)*(1+tc(i,r)) ) ;
pc0(i,r) = 1 + tc(i,r);
vi(r)    = sum(i, vafm(i,"i",r) * (1 + ti(i,"i",r)));                        // Mit oder ohne Steuern?
vg(r)    = sum(i, vafm(i,"g",r) * (1 + tg(i,r)));                            // Mit oder ohne Steuern?
vp(r)    = sum(i, vafm(i,"c",r) * (1 + tp(i,r)));                            // Mit oder ohne Steuern?
vxm(i,r) = sum(s, vxmd(i,r,s))+ vst(i,r) ;
vim(i,r) = sum(g, vifm(i,g,r));

* ------ 15.04.2014
vtwr1(i,r,s) = vtwr("trn",i,r,s);
* ------ DEP wird mit 4% angenommen (vgl. MRTdata_26_19_4.gms)
dep(r) = 0.04;
* ------ Define parameters for Keynes savings constraint
* ------ GDP as sum of private and public expenditure and investment expenditures minus current account
welf(r)     =   vp(r) + vg(r) + vi(r) - vb(r) ;
* ------ 17.06.2013 Do not calculate savrate for BAW, because otherwise there is an error (division by zero!)
savrate(r)  =   vi(r) / welf(r) ;

*display c0;
*display a0, d0, c0, tc, ti, tp, tg, vafm, pc0, ct0, vi, vg, vp, welf, savrate;


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
skld0(i,r) = vfm("skl",i,r) * pf0("skl",i,r) ;
uskd0(i,r) = vfm("usk",i,r) * pf0("usk",i,r) ;
kd0(i,r)   = vfm("cap",i,r) * pf0("cap",i,r) ;
rd0(i,r)   = vfm("res",i,r) * pf0("res",i,r) ;

* ------ Aggregate natural resources other than primary energy to capital
kd0("agr",r) = kd0("agr",r) + rd0("agr",r) ;
kd0("roi",r) = kd0("roi",r) + rd0("roi",r) ;
rd0("roi",r) = 0 ;
rd0("agr",r) = 0 ;

* ------ Factor income is sum of all sectoral factor inputs including tax
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

SCALAR
* ------ Extant production share
         thetax                  Extant production share / 0.985 /;

;
* ##############################################################################

* ------ GDX_IN
* ------ 14.05.2014
* ------ 26.07.2016 Folgende Zeile für GTAP9 einkommentieren
$GDXIN %datadir%%source%_ELE.gdx
* ------ 26.07.2016 Folgende Zeile für GTAP8 einkommentieren
*$GDXIN %datadir%%source%ELE.gdx
* ------ LOAD
$LOAD vafm_input  cap_input  skl_input  usk_input  costshr_calib
* ------ DISPLAY
*display vafm_input, cap_input, skl_input, usk_input;

* ------ Consistency checks for primary factor inputs in ele by gen
skl_test(r)      = round( sum(gen, skl_input(gen,r))    - skld0("ele",r), rd) ;
usk_test(r)      = round( sum(gen, usk_input(gen,r))    - uskd0("ele",r), rd) ;
cap_test(r)      = round( sum(gen, cap_input(gen,r))    - kd0("ele",r), rd) ;
vafm_test(i,r)   = round( sum(gen, vafm_input(i,gen,r)) - vafm(i,"ele",r), rd);
display skl_test, usk_test, cap_test, vafm_test;

* ------ Calculate output of generation technologies
out_gen(gen,r)= sum(i,[  vafm_input(i,gen,r)*(1+(ti(i,"ele",r)))] ) + skl_input(gen,r) + usk_input(gen,r) + cap_input(gen,r);

* ##############################################################################

* ---------------------------------------------------------------------- *
* Berechnung der Parameter für die Disaggregierung der Kapitalakkumulation
* der Stromerzeugung
* ---------------------------------------------------------------------- *
ks_x(gen,r)      = cap_input(gen,r)      *  thetax;
ks_n(gen,r)      = cap_input(gen,r)      * (1 - thetax) ;

ks_x0(gen,r)     = ks_x(gen,r);

capgenx(r)       = (sum(gen, ks_x(gen,r))) ;
capgennew(r)     = (sum(gen, ks_n(gen,r))) ;
* ------------------------------------------------------------------------------
* Anpassung der capital factor income durch gen-spezifsiche kapitalstockveränderungen
* Kapitaleinsatzes im Stromsektor wird abgezogen aus dem oben berechneten capital factor income
* Fuer diesen Sektor gibt es technologiespezifische Kapitalausstattungen
* evoa muss um extant capital in ele bereinigt werden, da extant capital ele in demand block im modell separat vorkommt
* take all kd0 including those for ele and take out extant ele as this is treated separately
* ------------------------------------------------------------------------------
evoa("cap",r)    = sum(i, kd0(i,r)) - capgenx(r)  ;
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

* ------ 03.07.2014 Weltweiter Energieverbrauch war 2007 nach Wikipedia ca. 11319 mtoe = 474 EJ
* ------ http://de.wikipedia.org/wiki/Liste_der_Staaten_mit_dem_h%C3%B6chsten_Energieverbrauch
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
*display aeei, aeei_elen, aeei_elex;


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
                                 / hc 94, bc 112, col 103, gas 56, oil 75, ccs 11 /              // Quelle: UBA s.o.
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

* ------ General specification
*co2em(fe,i,r)            = cecphys(fe) * evd(fe,i,r);           // tCO2/TJ * EJ         = Mio. tCO2 // NEWAGE-7: eind(i,j,r)
*co2em(fe,"final",r)      = cecphys(fe) * efd(fe,r) ;            // tCO2/TJ * EJ         = Mio. tCO2 //(carbcoef = co2em); NEWAGE-7: efd(fe,r)
*co2em(fe,i,r)            = cecphys(fe) * evd(fe,i,r) / 1000;     // tCO2/TJ * PJ * 1000  = Mio. tCO2 // NEWAGE-7: eind(i,j,r)
*co2em(fe,"final",r)      = cecphys(fe) * efd(fe,r)   / 1000;     // tCO2/TJ * PJ * 1000  = Mio. tCO2 //(carbcoef = co2em); NEWAGE-7: efd(fe,r)

* ------ 11.06.2014 evtl. adjust cecphys
*cecphys(fe)      = cecphys(fe)   / 41868; // umrechnen von tCO2/TJ in tCO2/mtoe
*cecphys("hc")    = cecphys("hc") / 41868; // umrechnen von tCO2/TJ in tCO2/mtoe
*cecphys("bc")    = cecphys("bc") / 41868; // umrechnen von tCO2/TJ in tCO2/mtoe
*cecphys("ccs")   = cecphys("ccs")/ 41868; // umrechnen von tCO2/TJ in tCO2/mtoe

* ------ 02.07.2014
* Remember: efd(i,r) = evd(i,"c",r) + evd(i,"g",r);
*cecphys(fe) = cecphys(fe) * 1000;

* ------ 11.06.2014
*co2em(fe,i,r)            = cecphys(fe) * evd(fe,i,r) ;           // Mio. tCO2/EJ * EJ = Mio. tCO2 // NEWAGE-7: eind(i,j,r)
*co2em(fe,"final",r)      = cecphys(fe) * efd(fe,r) ;             // Mio. tCO2/EJ * EJ = Mio. tCO2 //(carbcoef = co2em); NEWAGE-7: efd(fe,r)

* ------ 27.02.2015 Die CO2-Emissionen fallen nur dort an, wo es auch wirklich Nachfrage gibt (z.B. gas.gas.FRA --> kein Eintrag in vafm aber es wäre einer in co2em)
* ------ 26.07.2016 Folgende zwei Zeilen einkommentiert (GTAP8 rep)
co2em(fe,i,r)$vafm(fe,i,r)  = cecphys(fe) * evd(fe,i,r) ;           // Mio. tCO2/EJ * EJ = Mio. tCO2 // NEWAGE-7: eind(i,j,r)
co2em(fe,"final",r)$c0(fe,r)= cecphys(fe) * efd(fe,r) ;             // Mio. tCO2/EJ * EJ = Mio. tCO2 //(carbcoef = co2em); NEWAGE-7: efd(fe,r)
display co2em;

* ------ 26.07.2016 Hinzugefügt bzw. CO2-Emissionen neu definiert
* ------ 26.07.2016 Folgender Block auskommentiert (GTAP8 rep)
$ontext
parameter efi(i,r);
efi(i,r)        = evi(i,"c",r) + evi(i,"g",r);
co2em(fe,i,r)$vafm(fe,i,r)  = cecphys(fe) * (evd(fe,i,r) + evi(fe,i,r));           // Mio. tCO2/EJ * EJ = Mio. tCO2 // NEWAGE-7: eind(i,j,r)
co2em(fe,"final",r)$c0(fe,r)= cecphys(fe) * (efd(fe,r) + efi(fe,r)) ;             // Mio. tCO2/EJ * EJ = Mio. tCO2 //(carbcoef = co2em); NEWAGE-7: efd(fe,r)
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

*$exit
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

* ------ 03.07.2014 Weltweiter Energieverbrauch war 2007 nach Wikipedia ca. 11319 mtoe = 474 EJ
* ------ http://de.wikipedia.org/wiki/Liste_der_Staaten_mit_dem_h%C3%B6chsten_Energieverbrauch
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
co2em_total(r) = sum((i,j), co2em(i,j,r)) + sum(fe, co2em(fe,"final",r));
co2em_total("World") = sum(r, co2em_total(r));

display co2em_ele_share, eco2d, eco2i, co2em, co2em_ele, co2em_total;


*$exit
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

         ele_prod(gen,r)                 Benchmark electricity production [TWh]
         ele_prod_costs
         ressize(r,gen,yr)               Growth rates for renewable energy sources
         co2pfad(r,yr)                   CO2-growth
         co2pfad_ets(r,yr)               CO2-growth in the EU-ETS (regional)
         co2pfad_ets_eu(*,yr)            CO2-growth in the whole EU-ETS
         co2pfad_nets(r,yr)              CO2-growth in the EU non-ETS-sectors (regional)

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

         wg0(gen,r)                      Wirkungsgrad im bmk
         wg_yr(gen,yr)                   Wirkungsgrad nach jahren jeweils bat
         nucsize(r,yr)                   Ausbau- bzw. Rückbaupfad Kernenergie
         diffcost_exogen(r,gen)
         pytarget_yr(r,i,yr)             Exogenous price paths for resources

         savrt
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
$LOAD    co2pfad_ets  co2pfad co2pfad_ets_eu
$LOAD    URUN0  URSK0
*$LOAD    un_numb0  sk_numb0  labsize
$LOAD    diffcost  cost_red
$LOAD    abschreibung  abschreibung_bmk  abschreibung_z  abschreibung_bmk_z
$LOAD    aeei_exogen_gen_yr aeei_exogen_gen_yr_after2020 aeei_exogen_gen_yr_after2025
$LOAD    aeei_exogen     aeei_exogen_after2020           aeei_exogen_after2025
$LOAD    aeei_exogen_ele aeei_exogen_ele_after2020       aeei_exogen_ele_after2025
$LOAD    aeei_exogen_trn aeei_exogen_trn_after2020       aeei_exogen_trn_after2025
$LOAD    aeei_exogen_hh  aeei_exogen_hh_after2020        aeei_exogen_hh_after2025
$LOAD    aeei_exo        aeei_exo_emerge   aeei_exo_deu  aeei_exo_neu    aeei_exo_ele_c
$LOAD    wg0   wg_yr
$LOAD    nucsize
$LOAD    pytarget_yr
$LOAD    DIFFCOST_EXOGEN

* ------ 15.07.2014
$LOAD    lfhc_usk  lfhc_skl
$LOAD    ep  tfp

* ------ 26.07.2016 Folgende Zeile für GTAP8 einkommentieren
*$GDXIN   %datadir%%output%ELE.gdx                                    //GDXin
* ------ 26.07.2016 Folgende Zeile für GTAP9 einkommentieren
$GDXIN   %datadir%%output%_ELE.gdx                                    //GDXin

$LOAD    ele_prod ele_prod_costs

*display lfhc_usk, lfhc_skl, nucsize, ressize, co2pfad, co2pfad_ets, co2pfad_ets_eu, cost_red, abschreibung, abschreibung_z, abschreibung_bmk, abschreibung_bmk_z, wg_yr, pytarget_yr;
*display AEEI_exo, AEEI_exo_emerge, AEEI_exo_deu, AEEI_exo_neu, AEEI_exo_ele_c, AEEI_exogen, AEEI_exogen_after2020, AEEI_exogen_after2025,
*AEEI_exogen_gen_yr, AEEI_exogen_gen_yr_after2020, AEEI_exogen_gen_yr_after2025, AEEI_exogen_ele, AEEI_exogen_ele_after2020,
*AEEI_exogen_ele_after2025, AEEI_exogen_trn, AEEI_exogen_trn_after2020, AEEI_exogen_trn_after2025, AEEI_exogen_hh, AEEI_exogen_hh_after2020, AEEI_exogen_hh_after2025;
*display urun0, ursk0;
*display pytarget_yr, save, vdep, vi, vom, vb, ele_prod, ele_prod_costs;
display ep, tfp;
*display diffcost;

savrt(r) = save(r) / sum(g$(not i(g)), vom(g,r)); display savrt;

* ------ 24.06.2014
*size_skl(baw,yr) = 1;

* ------ 15.07.2014 Set BAW equal to DEU (for TFP and EP)
*tfp(yr,"baw")    = tfp(yr,"deu");
*ep(yr,"baw")     = ep(yr,"deu") ;
* ------ 17.07.2014
*ep("2050","aut") = ep("2045","aut") ;
*ep("2050","chi") = ep("2045","chi") ;
* ------ 15.07.2014 Add tfp growth to lfhc_usk and lfhc_skl
size_usk(r,yr)   = lfhc_usk(r,yr) + (tfp(yr,r) - 1);
size_skl(r,yr)   = lfhc_skl(r,yr) + (tfp(yr,r) - 1);
* ------ 5.02.2016 lfhc_usk und lfhc_skl mit newage_dataload_17x18x4_v34.gms neu eingelesen
* ------ 5.02.2016 >>> in order to increase economic growth in Germany from 1% p.a. (2007-2030) to 1,3% p.a.
* ------ 5.02.2016 es wurden size_17x18x4.xlsx und ep+tfp.xlsx (sheet ep_newage) geändert!



*display lfhc_usk, lfhc_skl, size_usk, size_skl, tfp, ep;


* ##############################################################################
* ------ Specifying parameters -------------------------------------------------

* ------ Initial benchmark prices for wage curve formulation:
parameter
         psklbmk(r)
         puskbmk(r)
         pcbmk(r);

* ------ 23.04.2014 Calculate average for missing countries ARB, OEC, OPE and ROW
*ur_miss(r)$(not urun0(r) or not ursk0(r)) = 1;
*urun0(r)$ur_miss(r) = sum(rr, urun0(rr)) / 14;
*ursk0(r)$ur_miss(r) = sum(rr, ursk0(rr)) / 14;
*display ur_miss, urun0, ursk0;

* ------ 15.04.2014
*size_usk(r,"2010") = (size_usk(r,"2007") + size_usk(r,"2015")) / 2 ;
*size_skl(r,"2010") = (size_skl(r,"2007") + size_skl(r,"2015")) / 2  ;

* ------ hierdurch verschwindet das Jahr 2004, weil dort alle Werte auf 1 waren --> (4x10x10)
ressize(r,gen,yr)$ressize(r,gen,yr) = ressize(r,gen,yr) - 1 ;

* ------ Nucsize specification
nucsize(r,yr)$(nucpot(r)) = nucsize(r,yr) - 1;
* ------ Nucout in DEU and BAW
nucsize(r,yr)$(nucout(r) and after(yr)) = 0;

* ------ Define abschreibung for decomissioning curves for other regions than EU28
abschreibung(r,gen,yr)$(NOT EU28(r))     = abschreibung_z(r,gen,yr);
abschreibung_BMK(r,gen,yr)$(NOT EU28(r)) = abschreibung_BMK_Z(r,gen,yr);
abschreibung(r,"bCCS",yr)                = 0;
abschreibung(r,"mCCS",yr)                = 0;
abschreibung_BMK(r,"bCCS",yr)            = 1;
abschreibung_BMK(r,"mCCS",yr)            = 1;

* ------ 14.10.2014
parameter abschr_pfad(r,gen,yr), ks_x_yr2;

abschr_pfad(r,gen,"2007") = 1;

loop(yr$after(yr),abschr_pfad(r,gen,yr) = abschr_pfad(r,gen,yr-1) * (1 - abschreibung(r,gen,yr)););

ks_x_yr2(r,gen,"2007") = ks_x0(gen,r);

loop(yr$after(yr), ks_x_yr2(r,gen,yr) = ks_x0(gen,r) * abschr_pfad(r,gen,yr));

*display abschreibung, abschr_pfad, ks_x_yr2, ks_x0;

* ------ 16.02.2015 Define CRU-Price in 2050 same as 2045:
pytarget_yr(r,i,"2050") = 0.9 * pytarget_yr(r,i,"2045") ;
* ------ TEXTMARKE RWE

*display nucsize, ressize, co2pfad, co2pfad_ets, cost_red, abschreibung, abschreibung_z, abschreibung_bmk, abschreibung_bmk_z, wg_yr, pytarget_yr;
*display AEEI_exo, AEEI_exo_emerge, AEEI_exo_deu, AEEI_exo_neu, AEEI_exo_ele_c, AEEI_exogen, AEEI_exogen_after2020, AEEI_exogen_after2025,
*AEEI_exogen_gen_yr, AEEI_exogen_gen_yr_after2020, AEEI_exogen_gen_yr_after2025, AEEI_exogen_ele, AEEI_exogen_ele_after2020,
*AEEI_exogen_ele_after2025, AEEI_exogen_trn, AEEI_exogen_trn_after2020, AEEI_exogen_trn_after2025, AEEI_exogen_hh, AEEI_exogen_hh_after2020, AEEI_exogen_hh_after2025;
*display size_usk, abschreibung;


* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------------------------------------------------------------------------------
* ------ (J) SZENARIO SCHALTER [Quotas/Flags/Shares]
* ------------------------------------------------------------------------------
SCALARS
         EUPOL   EU unilateral carbon policy     / 1 /


* ------ Quotas/Flags/Shares in electricty generation and ETS allowances
         notrade         Defines carbon regime as regional permit price PCO2 in the EU28                 / 0 /   // notrade
         worldtrade      Defines carbon regime as international  permit price PCO2W                      / 0 /
         worldtrade2     Defines carbon regime as international  permit price PCO2W (set if ets + nets)  / 0 /
         eutrade         Defines carbon regime as international  permit price PCO2W for EU28             / 0 /   // EUtrade
         etstrade        Defines carbon regime as EU-28 ETS      permit price PCO2_ETS                   / 1 /   // ETStrade
         netstrade       Defines carbon regime as EU-28 Non-ETS  permit price PCO2_NETS                  / 0 /
         netstrade_r     Defines carbon regime as regional Non-ETS permit price PCO2_NETSr(r)            / 0 /
         hhets           Defines carbon regime as including Households into ETS permit price PCO2_ETS    / 0 /
         bawnets         Defines carbon regime as BAW Non-ETS  permit price PCO2_NETS                    / 0 /
         trade           Schalter für Emissionsreduktion mit  Handel                                     / 0 /
         sectrade        Schalter für Emissionsreduktion mit  Handel zwischen ausgewählten Sektoren      / 0 /
         septrade        Schalter für Emissionsreduktion mit  Handel zwischen ausgewählten Sektoren und Regionen / 0 /   // BAU on

* ------ Rebating scalars
         rebate_co2                              / 0 /
         rebate_ind_scal Industries schalter     / 0 /
         rebate_oil                              / 0 /
         rebate_reg                              / 0 /
         rebate_notreg                           / 0 /

* ------ Other
         euquota                                                                                         / 0 /
         aeei_variante   Schalter für Einstaz der McKinsey AEEI sektoral u. zeitlich spezifiziert        / 0 /
;

* ------ 2.02.2016
notrad(r)$(eu28(r) and notrade) = yes ;

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
         pricetarget(i,r) Flag for exogenous price path for fossil fuels
         pytarget(i,r)   Exogenous price path for fossil fuels
* ------ RES Policies:
         eegon(r)        Schalter für res subvention
         rebateon(r)     Schalter für überwälzung eegon-subvention auf ele-preis
         eegout(r)       Schalter für res regulator
         capsub(r)       Schalter für capital subsidy for reg
* ------ Limiting technologies
         biolimit(r)     schalter für limitierung von biomasse in strom in EU28 (zusätzlich zu bio_pot)
         bhydrolimit(r)  schalter für limitierung von grundlast-wasserkraft in strom in EU28 ?
         phydrolimit(r)  schalter für limitierung von spitzenlast-pumpspeicher in strom in EU28 ?
         mwindlimit(r)   schalter für limitierung von mittellast-windkraft in strom in EU28 ?
* ------ AEEI-Faktoren
*         aeei(*,*,*)
         aeei_trn(*,*,*)
         aeei_hh(*,*,*)
         aeei_ele(gen,r)
         aeei_ele_x(gen,r)
         aeei_ele_n(gen,r)
* ------ Other share and path parameters:
         share(r)
         res_share(yr,r)
         rebate_ind(i)   These industries get auctioning revenues
* ------ Other parameters:
         ele_import
         kalib(r)                Kalibierungstuning für strombverbrauch um gesamtstrommenge an mb anzupassen
         redu(r)
         redu_ets(r)
         alpha(i,r)              Share of auction rebated
         gskl(i,r)               Labor productivity growth
         gusk(i,r)               Labor productivity growth
         gpoil(r)                Oil price growth exogenous
         qendow(gen,r)
;

* ------ Carbon Regimes
co2pfad_ets(r,yr)$trade  = 0 ;
seCtrade_yr("2007")      = 0 ;
seCtrade_yr(yr)$after(yr)= 0 ;
sePtrade_yr("2007")      = 0 ;           // BAU on
sePtrade_yr(yr)$after(yr)= 0 ;           // BAU on
trade_yr("2007")         = 0 ;
trade_yr(yr)$after(yr)   = 0 ;
* ------ RES Policies:
eegon(r)                 = 0 ;
rebateon(r)              = 0 ;
eegout(r)                = 0 ;
capsub(r)                = 0 ;
* ------ Limiting technologies
biolimit(r)              = 1 ;           // BAU on
bhydrolimit(r)           = 1 ;           // BAU on
phydrolimit(r)           = 1 ;           // BAU on
mwindlimit(r)            = 1 ;           // BAU on
* ------ AEEI-Faktoren (Initial Definition)
*aeei(e,d,r)              = 1 ;
aeei_trn(e,d,r)          = 1 ;
aeei_hh(e,d,r)           = 1 ;
aeei_ele(gen,r)          = 1 ;
aeei_ele_x(gen,r)        = 1 ;
aeei_ele_n(gen,r)        = 1 ;

* ------ Price targets
pricetarget(i,r)                 = 0 ;
pricetarget("cru",r)             = 1 ;   // BAU on
* ------ 9.01.2015
*pricetarget("cru",r)             = 0 ;   // Textmarke RWE
* ------ 11.06.2014
pytarget(i,r)$pricetarget(i,r)   = 1;    // BAU on
* ------ 12.06.2014
*rd0("cru",r)$(not rd0("cru",r)) = 1e-8; display rd0;
*pytarget(i,r)$(rd0(i,r) and pricetarget(i,r))   = 1;    // BAU on
*display pricetarget, pytarget, rd0;

* ------ Rebating --> Which industries should revenues accrue to?
rebate_ind(i)$(ets(i)$rebate_ind_scal) = 0;
rebate_ind("ele") = 0;
*#flag #schalter
* ------ Other specific scenarios
* 1.) fix carbon price
carbtax(r) = 0 ;
* 2.) taxation of energy resources
* parameter restax(i,r);
* restax(i,r) = 0 ;
* 3.) direct capital subsidies
* parameter eeg(r,gen);
* eeg(r,gen)=0;
* schalter
*$offtext

* ------ ???
Qendow(FOSGEN,"DEU")$(card(quota)) = 10.0 ;
Qendow(FOSGEN,nms12)$(card(quota)) = 10.0 ;
Qendow(FOSGEN,eu15)$(card(quota))  = 10.0 ;

* ------ ???
share(r)         = 0 ;
ele_import       = 1 ;
kalib (r)        = 1 ;
redu(r)          = 1 ;
redu_ets(r)      = 1 ;
alpha(i,r)       = 1 ;
gskl(i,r)        = 1 ;
gusk(i,r)        = 1 ;
gpoil(r)         = 1 ;


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

*diffcost("msolar",r)$bawdeu(r) = 8;
diffcost("msolar",r)$deu(r) = 8;

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
vb(r) = 0; vb(r) =
         vom("c",r) + vom("g",r) + vom("i",r)
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


* ###### 5.02.2016 Textmarke Comment out Diss (unten)
*$ontext
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ 13.10.2014 Store data until here temporarily in GDX-file, which will be used to create BoUp data in dataload_hhenergy_v88.gms
execute_unload "%resultsdir%temp_data.gdx"

* ------ 13.09.2015 Read in BoUp data for HH energy demand from dataload_hhenergy_v88.gms
*$include dataload_trn_v86
$include dataload_hhenergy_v88

*display c0, c0_oil, snd0, ssd0, sy0, cbt, cbf, pc0, fuel0, pfuel0, tfuel, tcar, pcar0, ssd0price ;
parameter bnd0sum;
bnd0sum = sum((i,bt,bf), bnd0("deu",i,bt,bf)) ; display bnd0sum, c0;
*$exit

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ Efficiency standards for cars and buildings
parameter eff_target, effh_target, eff_avg, effh_avg;

* ------ 18.02.2015
*carco2(r,ct,cf)          = carco2(r,ct,cf) ;
carco2(r,ct,cf)          = carco2(r,ct,cf) / 1000 ;
eff_avg(r)$deu(r)        = sum((ct,cf), carco2(r,ct,cf)*cars(r,ct,cf,"new")) / sum((ct,cf), cars(r,ct,cf,"new"));
effh_avg(r)$deu(r)       = sum((bt,bf)$new(bt), kwhqm(r,bt,bf)*houseqm(r,bt,bf)) / sum((bt,bf)$new(bt), houseqm(r,bt,bf));
eff_target(r)$deu(r)     = 0;
effh_target(r)$deu(r)    = 0;
display eff_avg, eff_target, effh_avg, effh_target;

* ------ 10.02.2016 Additional Set for new buildings
set new_ep(bt) Set für Neubauten / new_e, new_p / ;

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ 10.02.2015 AEEI
parameter
         aeei_co2(r,cbt,cbf)
         aeei_cb(r,i,cbt,cbf)
         aeei_cb_yr(r,i,cbt,cbf,yr)
         aeei_co2_yr(r,cbt,cbf,yr)
;

aeei_co2(r,cbt,cbf)$sum(i, cbco2i(r,i,cbt,cbf))  = 1 ;
aeei_cb(r,i,cbt,cbf)$c0_en(r,i,cbt,cbf)          = 1 ;

aeei_cb_yr(r,i,cbt,cbf,"2007")   = aeei_cb(r,i,cbt,cbf) ;
aeei_co2_yr(r,cbt,cbf,"2007")    = aeei_co2(r,cbt,cbf) ;


**loop(yr$after(yr),
**         aeei_cb_yr(r,i,cbt,cbf,yr)$(c0_en(r,i,cbt,cbf) and yr2010(yr))          = aeei_cb_yr(r,i,cbt,cbf,yr-1) * (1-0.015)**3 ;
**         aeei_cb_yr(r,i,cbt,cbf,yr)$(c0_en(r,i,cbt,cbf) and after2010(yr))       = aeei_cb_yr(r,i,cbt,cbf,yr-1) * (1-0.015)**5 ;
**
**         aeei_co2_yr(r,ct,cf,yr)$(sum(i, cbco2i(r,i,ct,cf)) and yr2010(yr))   = aeei_co2_yr(r,ct,cf,yr-1) * (1-0.015)**3 ;
**         aeei_co2_yr(r,ct,cf,yr)$(sum(i, cbco2i(r,i,ct,cf)) and after2010(yr))= aeei_co2_yr(r,ct,cf,yr-1) * (1-0.015)**5 ;
**
**         aeei_co2_yr(r,bt,bf,yr)$(sum(i, cbco2i(r,i,bt,bf)) and yr2010(yr))   = aeei_co2_yr(r,bt,bf,yr-1) * (1-0)**3 ;
**         aeei_co2_yr(r,bt,bf,yr)$(sum(i, cbco2i(r,i,bt,bf)) and after2010(yr))= aeei_co2_yr(r,bt,bf,yr-1) * (1-0)**5 ;
**);


* ------ 15.02.2016
loop(yr$after(yr),
         aeei_cb_yr(r,i,cbt,cbf,yr)$(c0_en(r,i,cbt,cbf) and yr2010(yr))          = aeei_cb_yr(r,i,cbt,cbf,yr-1) * (1-0.01 )**3 ;
         aeei_cb_yr(r,i,cbt,cbf,yr)$(c0_en(r,i,cbt,cbf) and after2010(yr))       = aeei_cb_yr(r,i,cbt,cbf,yr-1) * (1-0.01 )**5 ;
* ------ cars:
         aeei_co2_yr(r,ct,cf,yr)$(sum(i, cbco2i(r,i,ct,cf)) and yr2010(yr))   = aeei_co2_yr(r,ct,cf,yr-1) * (1-0.01 )**3 ;
         aeei_co2_yr(r,ct,cf,yr)$(sum(i, cbco2i(r,i,ct,cf)) and after2010(yr))= aeei_co2_yr(r,ct,cf,yr-1) * (1-0.01 )**5 ;
* ------ buildings:
         aeei_co2_yr(r,bt,bf,yr)$(sum(i, cbco2i(r,i,bt,bf)) and yr2010(yr))   = aeei_co2_yr(r,bt,bf,yr-1) * (1-0 )**3 ;
         aeei_co2_yr(r,bt,bf,yr)$(sum(i, cbco2i(r,i,bt,bf)) and after2010(yr))= aeei_co2_yr(r,bt,bf,yr-1) * (1-0 )**5 ;
);


display aeei_co2, aeei_cb, aeei_cb_yr, aeei_co2_yr;
*$exit
* ------ Textmarke Diss

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ 10.02.2015 Dynamics
parameter
         sy(r,ct,cf), by(r,bt,bf),
         bsd(r,bt,bf), ssd(r,ct,cf),
         bsd_yr(r,bt,bf,yr), ssd_yr(r,ct,cf,yr),
         bsd_rate / 0.02 /, ssd_rate / 0.05 /
         ssd_sum
         ssd_rel
         bsd_sum

* ------ 5.01.2016 Create extra vintage capital for buildings for every milestone
         bsdyr(r,bt,bf,*)
         bsdyr0(r,bt,bf,*)
         bsdyr_init(r,bt,bf)
;

bsd(r,bt,bf)$bsd0(r,bt,bf) = bsd0(r,bt,bf) ;
ssd(r,ct,cf)$ssd0(r,ct,cf) = ssd0(r,ct,cf) ;
by(r,bt,bf)$by0(r,bt,bf)   = by0(r,bt,bf)  ;
sy(r,ct,cf)$sy0(r,ct,cf)   = sy0(r,ct,cf)  ;

bsd_yr(r,bt,bf,"2007")$bsd(r,bt,bf)  = 1 ;
ssd_yr(r,ct,cf,"2007")$ssd(r,ct,cf)  = 1 ;
bsd_yr(r,bt,bf,"2010")$bsd(r,bt,bf) = bsd_yr(r,bt,bf,"2007") * (1 - 3 * bsd_rate) ;
ssd_yr(r,ct,cf,"2010")$ssd(r,ct,cf) = ssd_yr(r,ct,cf,"2007") * (1 - 3 * ssd_rate) ;

loop(yr$after2010(yr),
         bsd_yr(r,bt,bf,yr)$bsd(r,bt,bf) = bsd_yr(r,bt,bf,yr-1)  - (5 * bsd_rate * bsd_yr(r,bt,bf,"2007")) ;
         ssd_yr(r,ct,cf,yr)$ssd(r,ct,cf) = ssd_yr(r,ct,cf,yr-1)  - (5 * ssd_rate * ssd_yr(r,ct,cf,"2007")) ;
         ssd_yr(r,ct,cf,yr)$(ssd_yr(r,ct,cf,yr) lt 0) = 0 ;
);

* ------ 5.01.2016
bsdyr(r,bt,bf,"2007")$(new(bt) and sum(i, bnd0(r,i,bt,bf))) = 1e-6 ;
bsdyr(r,bt,bf,"2010")$(new(bt) and sum(i, bnd0(r,i,bt,bf))) = 1e-6 ;
bsdyr(r,bt,bf,"2015")$(new(bt) and sum(i, bnd0(r,i,bt,bf))) = 1e-6 ;
bsdyr(r,bt,bf,"2020")$(new(bt) and sum(i, bnd0(r,i,bt,bf))) = 1e-6 ;
bsdyr(r,bt,bf,"2025")$(new(bt) and sum(i, bnd0(r,i,bt,bf))) = 1e-6 ;
bsdyr(r,bt,bf,"2030")$(new(bt) and sum(i, bnd0(r,i,bt,bf))) = 1e-6 ;
*bsdyr0(r,bt,bf,"2007")$(new(bt) and sum(i, bnd0(r,i,bt,bf))) = sum(i, bnd0(r,i,bt,bf)) ;
*bsdyr0(r,bt,bf,"2010")$(new(bt) and sum(i, bnd0(r,i,bt,bf))) = sum(i, bnd0(r,i,bt,bf));
*bsdyr0(r,bt,bf,"2015")$(new(bt) and sum(i, bnd0(r,i,bt,bf))) = sum(i, bnd0(r,i,bt,bf)) ;
*bsdyr0(r,bt,bf,"2020")$(new(bt) and sum(i, bnd0(r,i,bt,bf))) = sum(i, bnd0(r,i,bt,bf)) ;
*bsdyr0(r,bt,bf,"2025")$(new(bt) and sum(i, bnd0(r,i,bt,bf))) = sum(i, bnd0(r,i,bt,bf)) ;
*bsdyr0(r,bt,bf,"2030")$(new(bt) and sum(i, bnd0(r,i,bt,bf))) = sum(i, bnd0(r,i,bt,bf)) ;
bsdyr0(r,bt,bf,"2007") = bsdyr(r,bt,bf,"2007") ;
bsdyr0(r,bt,bf,"2010") = bsdyr(r,bt,bf,"2010") ;
bsdyr0(r,bt,bf,"2015") = bsdyr(r,bt,bf,"2015") ;
bsdyr0(r,bt,bf,"2020") = bsdyr(r,bt,bf,"2020") ;
bsdyr0(r,bt,bf,"2025") = bsdyr(r,bt,bf,"2025") ;
bsdyr0(r,bt,bf,"2030") = bsdyr(r,bt,bf,"2030") ;

bsdyr_init(r,bt,bf) = bsdyr(r,bt,bf,"2007") + bsdyr(r,bt,bf,"2010") + bsdyr(r,bt,bf,"2015") + bsdyr(r,bt,bf,"2020") + bsdyr(r,bt,bf,"2025") + bsdyr(r,bt,bf,"2030") ;
* ------ 7.02.2016 auskommentiert (unten) bzw. gleich Null gesetzt
bsdyr_init(r,bt,bf) = 0 ;

display bsd, ssd, sy, by, bsd_rate, ssd_rate, bsd_yr, ssd_yr, bsdyr, bsdyr_init;

* ------ 4.01.2016 Parameter, um Sanierungsrate anzuheben
parameter bt_sub(r,bt,bf), bt_san(r,bt,bf), carsub(r,ct,cf);
bt_sub(r,bt,bf) = 0;
bt_san(r,bt,bf) = 0;
carsub(r,ct,cf) = 0;
* ------ 4.01.2016 Discounted price for new buildings from the previous period
parameter qmcostc_disc(r,bt,bf)  Discounted price for new buildings from the previous period;
qmcostc_disc(r,bt,bf) = qmcostc(r,bt,bf) * 0.5 ;
display qmcostc_disc, qmcostc;


* ------ 7.02.2016
parameter bsd0nh(r,bt,bf), bsdnh(r,bt,bf) ;
bsd0nh(r,bt,bf)$new(bt) = 0 ;
bsd0nh(r,bt,bf)$new(bt) = 0.01 * (bnd0(r,"bui",bt,bf) + bnd0(r,"ser",bt,bf)) ;
bsdnh(r,bt,bf) = bsd0nh(r,bt,bf) ;
display bsd0nh, bsdnh;
*$exit

* ------ Textmarke Diss
*$offtext
* ###### 5.02.2016 Textmarke Comment out Diss (oben)

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ 07.10.2014  read in data for constraining new electricity generation in BAW
* ------ 10.12.2014  PARAMETERS for RWE and BMWi projects
parameter
         bbc_deu_elen(gen,yr)    Maximum new electricity generation (bBC)

* ------ Globales Klimaschutzabkommen
         ds2v6_yr(r,yr)            Emission pathways for carblim 2D vs. 6D scenario (ETP 2014)
         ds4v6_yr(r,yr)            Emission pathways for carblim 4D vs. 6D scenario (ETP 2014)
         ds2v4_yr(r,yr)            Emission pathways for carblim 2D vs. 4D scenario (ETP 2014)
         ds2v6_yr_50(r,yr)         Emission pathways for carblim 2D vs. 6D scenario (ETP 2014) 50% burden

* ------ Endogene Subvention für Untergrenz für ELE-Produktion
         ylo_on(i,r)
         ylo_par(i,r)

* ------ 28.05.2015
         co2fak(r)               CO2-Emissionsfaktor der inländischen Stromerzeugung [tCO2 per MWh]

* ------ Anzahl Erwerbstätige im Jahr 2007
         emplmtno_2007(r,*)
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
par=emplmtno_2007        rng=emplmtno_2007!B5:D23
$offecho

$call "gdxxrw %xcel_datadir%Annahmen_Stromerzeugung.xlsx @temp1.tmp"

$gdxin "Annahmen_Stromerzeugung"
$load bbc_deu_elen
$load emplmtno_2007

* ------ Read in additional data -----------------------------------------------
$onecho >temp2.tmp
par=ds2v6_yr           rng=CO2_data_2v6!A76:K94
par=ds4v6_yr           rng=CO2_data_4v6!A76:K94
par=ds2v4_yr           rng=CO2_data_2v4!A76:K94
par=ds2v6_yr_50        rng=CO2_data_2v6!A124:K142
$offecho

$call "gdxxrw %xcel_datadir%ETP2014_CO2_emissions.xlsx @temp2.tmp"

$gdxin "ETP2014_CO2_emissions"
$load ds2v6_yr
$load ds4v6_yr
$load ds2v4_yr
$load ds2v6_yr_50

*display ds2v6_yr, ds4v6_yr, ds2v4_yr, ds2v6_yr_50;

*$ontext
* ------ 13.01.2015 Set regional limits in 2045 and 2050 to ease co2 reduction burden
set co2ease(r)
          /                 RUS,                OPE, ARB      /;         // BEST
*         /                                     OPE, ARB      /;         // works (hardly in 2045 and 2050)!
*         /                 RUS,                OPE           /;         // does not work in 2045!
*         /                 RUS,                OPE, ARB      /;         // works
*         /                 RUS,                     ARB, ROW /;         // does not work in 2045!
*         /                 RUS,                OPE, ARB, ROW /;         // works
*         /                 RUS,           RSA, OPE, ARB, ROW /;         // works
*         /                 RUS,      CHI, RSA, OPE, ARB, ROW /;         // works
*         /                      IND, CHI, RSA, OPE, ARB, ROW /;         // works (hardly in 2045)!
*         /                 RUS, IND, CHI, RSA, OPE, ARB, ROW /;         // works
*         / USA,            RUS, IND, CHI, RSA, OPE, ARB, ROW /;         // works
*         / USA,       BRZ, RUS, IND, CHI, RSA, OPE, ARB, ROW /;         // works
*         / USA, OEC, BRZ, RUS, IND, CHI, RSA, OPE, ARB, ROW /;          // works

* ------ Set limit at 2040-value and then linearize/smoothen path from 2030 to 2050
ds2v6_yr(r,yr)$(co2ease(r) and yr2050(yr)) = ds2v6_yr(r,"2030") ;
ds2v6_yr(r,yr)$(co2ease(r) and yr2025(yr)) = ds2v6_yr(r,yr-1) - (1/6 * (ds2v6_yr(r,"2020") - ds2v6_yr(r,"2050"))) ;
ds2v6_yr(r,yr)$(co2ease(r) and yr2030(yr)) = ds2v6_yr(r,yr-1) - (1/6 * (ds2v6_yr(r,"2020") - ds2v6_yr(r,"2050"))) ;
ds2v6_yr(r,yr)$(co2ease(r) and yr2035(yr)) = ds2v6_yr(r,yr-1) - (1/6 * (ds2v6_yr(r,"2020") - ds2v6_yr(r,"2050"))) ;
ds2v6_yr(r,yr)$(co2ease(r) and yr2040(yr)) = ds2v6_yr(r,yr-1) - (1/6 * (ds2v6_yr(r,"2020") - ds2v6_yr(r,"2050"))) ;
ds2v6_yr(r,yr)$(co2ease(r) and yr2045(yr)) = ds2v6_yr(r,yr-1) - (1/6 * (ds2v6_yr(r,"2020") - ds2v6_yr(r,"2050"))) ;

ds2v6_yr_50("arb",yr) = ds2v6_yr("arb",yr) ; // ease reduction effort also in 4DS in ARB

display bbc_deu_elen, ds2v6_yr, ds2v6_yr_50, out_gen;
*$offtext

* ------ 18.02.2016 Read non-ETS reduction path for netstrade_r scenario
parameter nonets_yr(r,yr), nonetsx(r,yr);

$onecho >temp3.tmp
par=nonets_yr           rng=non-ETS!o51:u57
$offecho
$call "gdxxrw %xcel_datadir%CO2-Pfad_EU-ETS_v7.xlsx @temp3.tmp"
$gdxin "CO2-Pfad_EU-ETS_v7"
$load nonets_yr
display nonets_yr;

nonetsx(r,yr)$eu28(r) = 1 ;
*nonetsx(r,yr)$(eu28(r) and yr2015(yr)) = 1.1 ;
display nonetsx;




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
         C(r)                            ! Private consumption
         Y(i,r)$vom(i,r)                 ! Output
         A(i,r)$a0(i,r)                  ! Armington Aggregation of Domestic and Imports
         INV(r)                          ! Investments
         YT                              ! Transport

* ------ Electricity BoUp
         ELEx(gen,r)$(out_gen(gen,r)$ks_x(gen,r)) ! Electricity generation from extant capacity
         ELEn(gen,r)$(out_gen(gen,r)$ks_n(gen,r)) ! Electricity generation from new capacity


* ###### 5.02.2016 Textmarke Comment out Diss (unten)
* ------ Cars, buildings and fuels (Diss)
         ESD(r)$esdout(r)                                ! Energy service aggregate
         HEAT(r,bt)$heatout(r,bt)                        ! Heat demand classes aggregate
         CART(r,ct)$cartout(r,ct)                        ! Mobility demand classes aggregate
         HEATF(r,bt,bf)$heatfout(r,bt,bf)                ! Energy and capital cost aggregation for space heating
         CARTF(r,ct,cf)$cartfout(r,ct,cf)                ! Energy and capital cost aggregation for car transport
         FUELS(r,i,cbt,cbf)$c0_fuels(r,i,cbt,cbf)        ! CO2 relevant fuels supply activity for cars and buildings
         OILS(r,cbt,cbf)$c0_oil(r,"oil",cbt,cbf)         ! Oil based fuels supply activity for cars and buildings
         BIO(r,cbt,cbf)$c0_en(r,"agr",cbt,cbf)           ! Biomass supply activity of biomass energy for space heating
         NEWCAR(r,ct,cf)$snd0(r,"mvh",ct,cf)$deu(r)      ! New car purchases by technology
         NEWHOUSE(r,bt,bf)$sum(i, bnd0(r,i,bt,bf))       ! New building and construction services by technology
         STOCK_C(r,ct,cf)$sy0(r,ct,cf)$deu(r)            ! Car stock activity
         STOCK_H(r,bt,bf)$by0(r,bt,bf)$new(bt)$deu(r)    ! Buildings stock activity
         HEATAGG(r)$sum(bt, heatout(r,bt))               ! Aggregation of heating technologies
* ###### 5.02.2016 Textmarke Comment out Diss (oben)


* ------ COMMODITIES / PRICES ------------------------------------------------ *

$commodities:
         PC(r)                           ! Final demand
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

* ###### 5.02.2016 Textmarke Comment out Diss (unten)
* ------ Cars, buildings and fuels (Diss)
         PESD(r)$esdout(r)                               ! Price for energy service aggregate
         PHEAT(r,bt)$heatout(r,bt)                       ! Price for heat demand classes
         PCART(r,ct)$cartout(r,ct)                       ! Price for mobility demand classes
         PHEATF(r,bt,bf)$heatfout(r,bt,bf)               ! Price for heating technologies
         PCARTF(r,ct,cf)$cartfout(r,ct,cf)               ! Price for car transport technologies
         PFUELS(r,i,cbt,cbf)$c0_fuels(r,i,cbt,cbf)       ! Price for CO2 relevant fuels for cars and buildings
         POILS(r,cbt,cbf)$c0_oil(r,"oil",cbt,cbf)        ! Price for oil based fuels for cars and buildings
         PBIO(r,cbt,cbf)$c0_en(r,"agr",cbt,cbf)          ! Biomass part from agriculture for buildings
         PNEWCAR(r,ct,cf)$snd0(r,"mvh",ct,cf)$deu(r)     ! New car purchases by technology (MVH parts go to C:)
         PNEWHOUSE(r,bt,bf)$sum(i, bnd0(r,i,bt,bf))      ! New building and construction services by technology
         PSTOCK_C(r,ct,cf)$sy0(r,ct,cf)$deu(r)           ! Rental price for combined car stock
         PSTOCK_H(r,bt,bf)$by0(r,bt,bf)$new(bt)$deu(r)   ! Rental price for combined buildings stock
         PHEATAGG(r)$sum(bt, heatout(r,bt))              ! Price for aggregation of heating technologies

* ------ Capital endowment with existing cars and buildings
         PXTCAR(r,ct,cf)$ssd0(r,ct,cf)$deu(r)            ! Existing car stock (endowment)
         PXTHOUSE(r,bt,bf)$bsd0(r,bt,bf)$deu(r)          ! Existing buildings stock (endowment)
         PNEWH(r,bt,bf)$bsd0nh(r,bt,bf)                  ! New buildings stock (endowment)

* ------ Efficiency standards
         PEFF(r)$eff_target(r)                           ! Price for specific carbon efficiency permits for cars
         PEFFh(r)$effh_target(r)                         ! Price for specific energy demand permits for buildings

***         NCAR(r,ct,cf)$snd0c(r,ct,cf)$deu(r)             ! New car stock (endowment)
* ------ 7.02.2016 auskommentiert (unten)
***         PXTHOUSE07(r,bt,bf)$bsdyr(r,bt,bf,"2007")       ! Buildings stock from period 2007 (endowment)
***         PXTHOUSE10(r,bt,bf)$bsdyr(r,bt,bf,"2010")       ! Buildings stock from period 2010 (endowment)
***         PXTHOUSE15(r,bt,bf)$bsdyr(r,bt,bf,"2015")       ! Buildings stock from period 2015 (endowment)
***         PXTHOUSE20(r,bt,bf)$bsdyr(r,bt,bf,"2020")       ! Buildings stock from period 2020 (endowment)
***         PXTHOUSE25(r,bt,bf)$bsdyr(r,bt,bf,"2025")       ! Buildings stock from period 2025 (endowment)
***         PXTHOUSE30(r,bt,bf)$bsdyr(r,bt,bf,"2030")       ! Buildings stock from period 2030 (endowment)

* ###### 5.02.2016 Textmarke Comment out Diss (oben)

* ------ Carbon regimes:
         PCO2(r)$notrad(r)               ! Regional Carbon Price
         PCO2W$worldtrade                ! International Permit Price
         PCO2W$eutrade                   ! International Permit Price
         PCO2W$worldtrade2               ! International Permit Price for Non-EU28 if EU-ETS and or EU-non-ETS in place
         PCO2_ETS$etstrade               ! EU28 allowance price for ETS-sectors
         PCO2_NETS$netstrade             ! EU28 allowance price for Non-ETS-sectors
         PCO2_NETSr(r)$eu28(r)$netstrade_r ! Regional allowance price for Non-ETS-sectors in the EU28

* ------ CONSUMERS ----------------------------------------------------------- *

$consumers:
         RA(r)                           ! Representative agent


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

* ###### 5.02.2016 Textmarke Comment out Diss (unten)
$auxiliary:
         OLDBTAX(r,bt,bf)$(bsd0(r,bt,bf)$old(bt))  !Bestrafung der alten Gebäudenutzung, wenn neue Gebäude gefördert werden
$auxiliary:
         OLDCTAX(r,ct,cf)$ssd0(r,ct,cf)          !Bestrafung der alten PKW-nutzung, wenn neue PKW gefördert werden
* ###### 5.02.2016 Textmarke Comment out Diss (oben)


* -- E Q U A T I O N S ------------------------------------------------------- *

* ------ FINAL DEMAND -------------------------------------------------------- *

* ------ 10.12.2014 (Erste Zeile Original)
$prod:C(r)       s:0.5   c:1     e:1     oil(e):0   col(e):0   gas(e):0

*         o:PC(r)                 Q:(vom("c",r) + vom("g",r))
* ###### 5.02.2016 Textmarke Comment out Diss (unten/oben)
         o:PC(r)                 Q:(vom("c",r) + vom("g",r) + sum((ct,cf), ssd0(r,ct,cf)) + sum((bt,bf), bsd0(r,bt,bf) + bsdyr_init(r,bt,bf) + bsd0nh(r,bt,bf)))
* ###### 5.02.2016 Textmarke Comment out Diss (oben)

* ------ All consumption goods (DEU: except transport, energy, cars and buildings)
         i:PA(i,r)$(not e(i))                    q:c0(i,r)                       p:pc0(i,r) i.tl:$fe(i) c:$(not e(i)) e:$ele(i)  a:RA(r) t:tc(i,r)

* ------ Residential crude oil is NOT used for combustion --> no CO2-emissions --> no climate policy relevance
         i:PA(i,r)$cru(i)                        q:c0(i,r)                       p:pc0(i,r) i.tl:$fe(i)               e:$ele(i)  a:RA(r) t:tc(i,r)

* ------ Non-DEU: energy
         i:PA(e,r)$oil(e)                        q:(c0(e,r)*aeei(e,"c",r))       p:pc0(e,r) e.tl:$fe(e)  e:$ele(e)  a:RA(r) t:tc(e,r)
         i:PA(e,r)$ele(e)                        q:(c0(e,r)*aeei(e,"c",r))       p:pc0(e,r) e.tl:$fe(e)  e:$ele(e)  a:RA(r) t:tc(e,r)
         i:PA(e,r)$gas(e)                        q:(c0(e,r)*aeei(e,"c",r))       p:pc0(e,r) e.tl:$fe(e)  e:$ele(e)  a:RA(r) t:tc(e,r)
         i:PA(e,r)$col(e)                        q:(c0(e,r)*aeei(e,"c",r))       p:pc0(e,r) e.tl:$fe(e)  e:$ele(e)  a:RA(r) t:tc(e,r)

* ###### 5.02.2016 Textmarke Comment out Diss (unten)
* ------ DEU: Energy service demand
         i:PESD(r)                               q:(esdout(r)+sum((bt,bf), bsd0nh(r,bt,bf)))
* ###### 5.02.2016 Textmarke Comment out Diss (oben)

* ------ Carbon regimes:
         i:PCO2(r)#(fe)$notrad(r)                 q:(co2em(fe,"final",r) * aeei(fe,"c",r))        p:1e-6  fe.tl:          a:RA(r)
         i:PCO2W#(fe)$pco2w_r(r)$worldtrade       q:(co2em(fe,"final",r) * aeei(fe,"c",r))        p:1e-6  fe.tl:          a:RA(r)
         i:PCO2W#(fe)$eu28(r)$eutrade             q:(co2em(fe,"final",r) * aeei(fe,"c",r))        p:1e-6  fe.tl:          a:RA(r)
         i:PCO2W#(fe)$noneu28(r)$worldtrade2      q:(co2em(fe,"final",r) * aeei(fe,"c",r))        p:1e-6  fe.tl:          a:RA(r)
         i:PCO2_NETS#(fe)$eu28(r)$netstrade       q:(co2em(fe,"final",r) * aeei(fe,"c",r))        p:1e-6  fe.tl:
         i:PCO2_NETSr(r)#(fe)$eu28(r)$netstrade_r q:(co2em(fe,"final",r) * aeei(fe,"c",r))        p:1e-6  fe.tl:

* ###### 5.02.2016 Textmarke Comment out Diss (unten)

*BLOCK unten komplett entfernen und in "MPSGE-Block_HHenergy.txt"-Datei zwischenspeichern

* ------ ENERGY SERVICES (zero-profit) --------------------------------------- *

* ###### 5.02.2016 Textmarke Comment out Diss (unten)
* ------ Energy services aggregation
$PROD:ESD(r)$esdout(r)           s:0.3   trn:2   ct(trn):0.3
*bt:0.3  nbt(bt):5
*$PROD:ESD(r)$esdout(r)           s:0.1   trn:2   ct(trn):0.1   bt:2
*$PROD:ESD(r)$esdout(r)           s:0.5   trn:2   ct(trn):0.1   bt:2
*$PROD:ESD(r)$esdout(r)           s:0.1   trn:1   ct(trn):0.1   bt:2

         O:PESD(r)$esdout(r)                     q:(esdout(r)+sum((bt,bf), bsd0nh(r,bt,bf)))

         i:PA(e,r)$ele(e)                        q:(otherele(r)*aeei("ele","c",r))       p:pothele0(r)   a:RA(r) t:tothele0(r)
         i:PA(i,r)$c0_trn(i,r)                   q:c0_trn(i,r)                           p:pc0(i,r)      a:RA(r) t:tc(i,r)      trn:
         i:PHEATAGG(r)                           q:(sum(bt, heatout(r,bt))+sum((bt,bf), bsd0nh(r,bt,bf)))
         i:PCART(r,ct)$cartout(r,ct)             q:cartout(r,ct)                                                                ct:

*         i:PHEAT(r,bt)$heatout(r,bt)$old(bt)     q:heatout(r,bt)                                                                bt:
*         i:PHEAT(r,bt)$heatout(r,bt)$new(bt)     q:heatout(r,bt)                                                                nbt:


* ------ Heating technology demand class aggregation by region
$PROD:HEATAGG(r)$sum(bt, heatout(r,bt))     s:1   nbt:5
*$PROD:HEATAGG(r)$sum(bt, heatout(r,bt))     s:2   nbt:5
*$PROD:HEATAGG(r)$sum(bt, heatout(r,bt))     s:4   nbt:5
*$PROD:HEATAGG(r)$sum(bt, heatout(r,bt))     s:0.3   nbt:5
         O:PHEATAGG(r)                           q:(sum(bt, heatout(r,bt))+sum((bt,bf), bsd0nh(r,bt,bf)))
         i:PHEAT(r,bt)$heatout(r,bt)$old(bt)     q:heatout(r,bt)
         i:PHEAT(r,bt)$heatout(r,bt)$new(bt)     q:(heatout(r,bt)+sum(bf, bsd0nh(r,bt,bf)))         nbt:

* ------ Heating technology aggregation by demand class
$PROD:HEAT(r,bt)$heatout(r,bt)$old(bt)   s:1
*$PROD:HEAT(r,bt)$heatout(r,bt)$old(bt)   s:0.2
         o:PHEAT(r,bt)$heatout(r,bt)             q:heatout(r,bt)
         i:PHEATF(r,bt,bf)$heatfout(r,bt,bf)     q:heatfout(r,bt,bf)     a:RA(r) N:OLDBTAX(r,bt,bf)$(bsd0(r,bt,bf)$old(bt))

* ------ Heating technology aggregation by demand class
$PROD:HEAT(r,bt)$heatout(r,bt)$new(bt)   s:2
         o:PHEAT(r,bt)$heatout(r,bt)             q:(heatout(r,bt)+sum(bf, bsd0nh(r,bt,bf)))
         i:PHEATF(r,bt,bf)$heatfout(r,bt,bf)     q:(heatfout(r,bt,bf)+bsd0nh(r,bt,bf))

* ------ Car technology aggregation by demand class
$PROD:CART(r,ct)$cartout(r,ct)   s:2
         o:PCART(r,ct)$cartout(r,ct)             q:cartout(r,ct)
         i:PCARTF(r,ct,cf)$cartfout(r,ct,cf)     q:cartfout(r,ct,cf)    a:RA(r) N:OLDCTAX(r,ct,cf)$ssd0(r,ct,cf)

* ------ Heating technology formulation (energy + capital aggregation)
$PROD:HEATF(r,bt,bf)$heatfout(r,bt,bf)$old(bt)   s:0    fe.tl:0
         O:PHEATF(r,bt,bf)$heatfout(r,bt,bf)     q:heatfout(r,bt,bf)

         i:PFUELS(r,fe,bt,bf)                    q:(c0_fuels(r,fe,bt,bf)*aeei_cb(r,fe,bt,bf))    p:pfueli0(r,fe,bt,bf)           fe.tl:  a:RA(r) t:(tfueli(r,fe,bt,bf)+co2tax(r,fe,bt,bf))
         i:PBIO(r,bt,bf)                         q:(c0_en(r,"agr",bt,bf)*aeei_cb(r,"agr",bt,bf)) p:pfueli0(r,"agr",bt,bf)                a:RA(r) t:tfueli(r,"agr",bt,bf)
         i:PA(i,r)$ele(i)                        q:(c0_en(r,i,bt,bf)*aeei_cb(r,i,bt,bf))         p:pfueli0(r,"ele","old_x","ele")        a:RA(r) t:tfueli(r,"ele","old_x","ele")

         i:PXTHOUSE(r,bt,bf)$deu(r)              q:bsd0(r,bt,bf)

         i:PCO2(r)#(fe)$notrad(r)                q:(cbco2i(r,fe,bt,bf)*aeei_cb(r,fe,bt,bf))     p:1e-6       fe.tl:
         i:PCO2W#(fe)$worldtrade                 q:(cbco2i(r,fe,bt,bf)*aeei_cb(r,fe,bt,bf))     p:1e-6       fe.tl:
         i:PCO2W#(fe)$eu28(r)$eutrade            q:(cbco2i(r,fe,bt,bf)*aeei_cb(r,fe,bt,bf))     p:1e-6       fe.tl:
         i:PCO2W#(fe)$noneu28(r)$worldtrade2     q:(cbco2i(r,fe,bt,bf)*aeei_cb(r,fe,bt,bf))     p:1e-6       fe.tl:
         i:PCO2_ETS#(fe)$eu28(r)$hhets           q:(cbco2i(r,fe,bt,bf)*aeei_cb(r,fe,bt,bf))     p:1e-6       fe.tl:
         i:PCO2_NETS#(fe)$eu28(r)$netstrade      q:(cbco2i(r,fe,bt,bf)*aeei_cb(r,fe,bt,bf))     p:1e-6       fe.tl:
         i:PCO2_NETSr(r)#(fe)$eu28(r)$netstrade_r q:(cbco2i(r,fe,bt,bf)*aeei_cb(r,fe,bt,bf))    p:1e-6       fe.tl:

* ------ Heating technology formulation (energy + capital aggregation)
$PROD:HEATF(r,bt,bf)$heatfout(r,bt,bf)$new(bt)   s:0    fe.tl:0
         O:PHEATF(r,bt,bf)$heatfout(r,bt,bf)     q:(heatfout(r,bt,bf)+bsd0nh(r,bt,bf))

         i:PFUELS(r,fe,bt,bf)                    q:(c0_fuels(r,fe,bt,bf)*aeei_cb(r,fe,bt,bf))    p:pfueli0(r,fe,bt,bf)           fe.tl:  a:RA(r) t:(tfueli(r,fe,bt,bf)+co2tax(r,fe,bt,bf))
         i:PBIO(r,bt,bf)                         q:(c0_en(r,"agr",bt,bf)*aeei_cb(r,"agr",bt,bf)) p:pfueli0(r,"agr",bt,bf)                a:RA(r) t:tfueli(r,"agr",bt,bf)
         i:PA(i,r)$ele(i)                        q:(c0_en(r,i,bt,bf)*aeei_cb(r,i,bt,bf))         p:pfueli0(r,"ele","old_x","ele")        a:RA(r) t:tfueli(r,"ele","old_x","ele")

         i:PSTOCK_H(r,bt,bf)                     q:(by0(r,bt,bf)+bsd0nh(r,bt,bf))

         i:PCO2(r)#(fe)$notrad(r)                q:(cbco2i(r,fe,bt,bf)*aeei_cb(r,fe,bt,bf))     p:1e-6       fe.tl:
         i:PCO2W#(fe)$worldtrade                 q:(cbco2i(r,fe,bt,bf)*aeei_cb(r,fe,bt,bf))     p:1e-6       fe.tl:
         i:PCO2W#(fe)$eu28(r)$eutrade            q:(cbco2i(r,fe,bt,bf)*aeei_cb(r,fe,bt,bf))     p:1e-6       fe.tl:
         i:PCO2W#(fe)$noneu28(r)$worldtrade2     q:(cbco2i(r,fe,bt,bf)*aeei_cb(r,fe,bt,bf))     p:1e-6       fe.tl:
         i:PCO2_ETS#(fe)$eu28(r)$hhets           q:(cbco2i(r,fe,bt,bf)*aeei_cb(r,fe,bt,bf))     p:1e-6       fe.tl:
         i:PCO2_NETS#(fe)$eu28(r)$netstrade      q:(cbco2i(r,fe,bt,bf)*aeei_cb(r,fe,bt,bf))     p:1e-6       fe.tl:
         i:PCO2_NETSr(r)#(fe)$eu28(r)$netstrade_r q:(cbco2i(r,fe,bt,bf)*aeei_cb(r,fe,bt,bf))    p:1e-6       fe.tl:

* ------ Car technology formulation (energy + capital aggregation)
$PROD:CARTF(r,ct,cf)$cartfout(r,ct,cf)   s:0   fe.tl:0
         O:PCARTF(r,ct,cf)$cartfout(r,ct,cf)     q:cartfout(r,ct,cf)

         i:PFUELS(r,fe,ct,cf)                    q:(c0_fuels(r,fe,ct,cf)*aeei_cb(r,fe,ct,cf))     p:pfueli0(r,fe,ct,cf)           fe.tl:  a:RA(r) t:(tfueli(r,fe,ct,cf)+co2tax(r,fe,ct,cf))
         i:PA(e,r)$ele(e)                        q:(c0_en(r,e,ct,cf)$eles(cf)*aeei_cb(r,e,ct,cf)) p:pfueli0(r,e,"small",cf)               a:RA(r) t:tfueli(r,e,"small","ele")
         i:PA(e,r)$ele(e)                        q:(c0_en(r,e,ct,cf)$phe(cf)*aeei_cb(r,e,ct,cf))  p:pfueli0(r,e,"large",cf)               a:RA(r) t:tfueli(r,e,"large","phe")

         i:PSTOCK_C(r,ct,cf)                     q:sy0(r,ct,cf)

         i:PCO2(r)#(fe)$notrad(r)                q:(cbco2i(r,fe,ct,cf)*aeei_cb(r,fe,ct,cf))     p:1e-6       fe.tl:
         i:PCO2W#(fe)$worldtrade                 q:(cbco2i(r,fe,ct,cf)*aeei_cb(r,fe,ct,cf))     p:1e-6       fe.tl:
         i:PCO2W#(fe)$eu28(r)$eutrade            q:(cbco2i(r,fe,ct,cf)*aeei_cb(r,fe,ct,cf))     p:1e-6       fe.tl:
         i:PCO2W#(fe)$noneu28(r)$worldtrade2     q:(cbco2i(r,fe,ct,cf)*aeei_cb(r,fe,ct,cf))     p:1e-6       fe.tl:
         i:PCO2_ETS#(fe)$eu28(r)$hhets           q:(cbco2i(r,fe,ct,cf)*aeei_cb(r,fe,ct,cf))     p:1e-6       fe.tl:
         i:PCO2_NETS#(fe)$eu28(r)$netstrade      q:(cbco2i(r,fe,ct,cf)*aeei_cb(r,fe,ct,cf))     p:1e-6       fe.tl:
         i:PCO2_NETSr(r)#(fe)$eu28(r)$netstrade_r q:(cbco2i(r,fe,ct,cf)*aeei_cb(r,fe,ct,cf))    p:1e-6       fe.tl:


* ------ Car stock activity
*$PROD:STOCK_C(r)$(sum((cct,ccf), sy0(r,cct,ccf)))                s:10
$PROD:STOCK_C(r,ct,cf)$sy0(r,ct,cf)                              s:4
*         O:PSTOCK_C(r)$(sum((cct,ccf), sy0(r,cct,ccf)))        q:(sum((cct,ccf), sy0(r,cct,ccf)))
         O:PSTOCK_C(r,ct,cf)$sy0(r,ct,cf)        q:sy0(r,ct,cf)

         I:PXTCAR(r,ct,cf)$ssd0(r,ct,cf)         q:ssd0(r,ct,cf)
         I:PNEWCAR(r,ct,cf)$snd0(r,"mvh",ct,cf)  q:(snd0(r,"mvh",ct,cf)*pcar0(r,ct,cf))   a:RA(r) t:carsub(r,ct,cf)

*         I:NCAR(r,ct,cf)$snd0c(r,ct,cf)          q:(snd0c(r,ct,cf)*pcar0(r,ct,cf))

* ------ Buildings stock activity
*$PROD:STOCK_H(r)$(sum((bbt,bbf), by0(r,bbt,bbf)))         s:10     t:0       hh(s):10
$PROD:STOCK_H(r,bt,bf)$by(r,bt,bf)$new(bt)$deu(r)     s:4     t:0       hh(s):10     y(s):3

*         O:PSTOCK_H(r)$(sum((bbt,bbf), by0(r,bbt,bbf)))  q:(sum((bbt,bbf), by0(r,bbt,bbf)))
         O:PSTOCK_H(r,bt,bf)$new(bt)$deu(r)              q:(by0(r,bt,bf)+bsd0nh(r,bt,bf))

*         I:PXTHOUSE(r,bt,bf)$deu(r)          q:bsd0(r,bt,bf)
         I:PNEWH(r,bt,bf)$bsd0nh(r,bt,bf)       q:bsd0nh(r,bt,bf)

* ------ 7.02.2016 auskommentiert (unten)
*         I:PXTHOUSE07(r,bt,bf)$deu(r)        q:bsdyr0(r,bt,bf,"2007")         y:
*         I:PXTHOUSE10(r,bt,bf)$deu(r)        q:bsdyr0(r,bt,bf,"2010")         y:
*         I:PXTHOUSE15(r,bt,bf)$deu(r)        q:bsdyr0(r,bt,bf,"2015")         y:
*         I:PXTHOUSE20(r,bt,bf)$deu(r)        q:bsdyr0(r,bt,bf,"2020")         y:
*         I:PXTHOUSE25(r,bt,bf)$deu(r)        q:bsdyr0(r,bt,bf,"2025")         y:
*         I:PXTHOUSE30(r,bt,bf)$deu(r)        q:bsdyr0(r,bt,bf,"2030")         y:

         I:PNEWHOUSE(r,bt,bf)$deu(r)         q:(sum(i, bnd0(r,i,bt,bf)*pc0(i,r)))     A:RA(r)  t:bt_sub(r,bt,bf)   A:RA(r)  t:BT_SAN(r,bt,bf)

*         I:PXTHOUSE(r,bt,bf)$deu(r)          q:bsd0(r,bt,bf)                                     hh:
*         I:PNEWHOUSE(r,bt,bf)$deu(r)         q:(sum(i, bnd0(r,i,bt,bf)*pc0(i,r)))                hh:
* ------ Energy demand standards
*         O:PEFFh(r)$effh_target(r)           q:(effh_target(r)*houseqm(r,bt,bf))
*         I:PEFFh(r)$effh_target(r)           q:(kwhqm(r,bt,bf)*houseqm(r,bt,bf))      P:1e-6


* ------ CO2 relevant fuels aggregation
$PROD:FUELS(r,fe,cbt,cbf)$c0_fuels(r,fe,cbt,cbf)$oil(fe)    s:10
         O:PFUELS(r,fe,cbt,cbf)$c0_fuels(r,fe,cbt,cbf)   q:c0_fuels(r,fe,cbt,cbf)
         I:POILS(r,cbt,cbf)                              q:c0_en(r,"oil",cbt,cbf)

* ------ CO2 relevant fuels aggregation
$PROD:FUELS(r,fe,cbt,cbf)$c0_fuels(r,fe,cbt,cbf)$gas(fe)    s:0
         O:PFUELS(r,fe,cbt,cbf)$c0_fuels(r,fe,cbt,cbf)   q:c0_fuels(r,fe,cbt,cbf)
         i:PA(fe,r)$gas(fe)                              q:c0_en(r,fe,cbt,cbf)

* ------ CO2 relevant fuels aggregation
$PROD:FUELS(r,fe,cbt,cbf)$c0_fuels(r,fe,cbt,cbf)$col(fe)    s:0
         O:PFUELS(r,fe,cbt,cbf)$c0_fuels(r,fe,cbt,cbf)   q:c0_fuels(r,fe,cbt,cbf)
         i:PA(fe,r)$col(fe)                              q:c0_en(r,fe,cbt,cbf)

* ------ 4.02.2016 CO2-Steuer hier oder besser direkt in HEATF und CARTF
*         I:POILS(r,cbt,cbf)                              q:c0_en(r,"oil",cbt,cbf)     a:RA(r) t:co2tax(r,"oil",cbt,cbf)
*         i:PA(fe,r)$gas(fe)                              q:c0_en(r,fe,cbt,cbf)        a:RA(r) t:co2tax(r,fe,cbt,cbf)
*         i:PA(fe,r)$col(fe)                              q:c0_en(r,fe,cbt,cbf)        a:RA(r) t:co2tax(r,fe,cbt,cbf)


* ------ Oil to car fuels transformation
$PROD:OILS(r,cbt,cbf)$c0_oil(r,"oil",cbt,cbf)    t:10
         O:POILS(r,cbt,cbf)              q:c0_en(r,"oil",cbt,cbf)
         I:PA(i,r)$oil(i)                q:c0_en(r,i,cbt,cbf)

* ------ Agriculture to biomass transformation
$PROD:BIO(r,cbt,cbf)$c0_en(r,"agr",cbt,cbf)      t:10
         O:PBIO(r,cbt,cbf)               q:(sum(i, c0_en(r,i,cbt,cbf)))
         I:PA(i,r)$agr(i)                q:c0_en(r,i,cbt,cbf)

* ------ New Car purchases    (4.2.15: die Elastizitäten ctcf und eff scheinen keine Aussagekraft auf die Wirkungsweise von PEFF zu haben --> warum nicht?)
$PROD:NEWCAR(r,ct,cf)$snd0(r,"mvh",ct,cf)        t:0     ctcf(t):1    eff(t):10
         O:PNEWCAR(r,ct,cf)$deu(r)       q:(snd0(r,"mvh",ct,cf)*pcar0(r,ct,cf))                 ctcf:
         I:PA("mvh",r)                   q:snd0(r,"mvh",ct,cf)           p:pcar0(r,ct,cf) a:RA(r) t:tcar(r,ct,cf)
* ------ CO2 efficiency standards
         O:PEFF(r)$eff_target(r)         q:(eff_target(r)*cars(r,ct,cf,"new"))                   eff:
         I:PEFF(r)$eff_target(r)         q:(aeei_co2(r,ct,cf)*carco2(r,ct,cf)*cars(r,ct,cf,"new"))      P:1e-6

* ------ New Buildings purchases (+ services)
$PROD:NEWHOUSE(r,bt,bf)$sum(i, bnd0(r,i,bt,bf))  bs(s):0     t:0  btbf(t):1    eff(t):10
*$PROD:NEWHOUSE(r,bt,bf)$sum(i, bnd0(r,i,bt,bf))  s:0     t:0  btbf(t):10    eff(t):10
         O:PNEWHOUSE(r,bt,bf)$deu(r)     q:(sum(i, bnd0(r,i,bt,bf)*pc0(i,r)))   btbf:
         I:PA(i,r)$bui(i)                q:bnd0(r,i,bt,bf)       p:pc0(i,r)  a:RA(r) t:tc(i,r)   bs:
         I:PA(i,r)$ser(i)                q:bnd0(r,i,bt,bf)       p:pc0(i,r)  a:RA(r) t:tc(i,r)   bs:

* ------ Energy demand standards
         O:PEFFh(r)$effh_target(r)       q:(effh_target(r)*houseqm(r,bt,bf)$new(bt))             eff:
         I:PEFFh(r)$effh_target(r)       q:(aeei_co2(r,bt,bf)*kwhqm(r,bt,bf)*houseqm(r,bt,bf)$new(bt))     P:1e-6


* ------ REPORTING of energy services and technologies -------------------------

$REPORT:
* ------ $prod:C
         v:VC_ESD(r)$esdout(r)                           i:PESD(r)               prod:C(r)

* ------ $prod:ESD
         v:vESD_o(r)$esdout(r)                           o:PESD(r)               prod:ESD(r)
         v:vESD_cart(r,ct)$cartout(r,ct)                 i:PCART(r,ct)           prod:ESD(r)
         v:vESD_heat(r,bt)$heatout(r,bt)                 i:PHEATAGG(r)           prod:ESD(r)
         v:vESD_trn(i,r)$c0_trn(i,r)                     i:PA(i,r)               prod:ESD(r)
         v:vESD_ele(i,r)$(ele(i) and otherele(r))        i:PA(i,r)               prod:ESD(r)


* ------ $prod:HEAT
         v:vHEAT_o(r,bt)$heatout(r,bt)                   o:PHEAT(r,bt)           prod:HEAT(r,bt)
         v:vHEAT_F(r,bt,bf)$heatfout(r,bt,bf)            i:PHEATF(r,bt,bf)       prod:HEAT(r,bt)
* ------ $prod:CART
         v:vCART_o(r,ct)$cartout(r,ct)                   o:PCART(r,ct)           prod:CART(r,ct)
         v:vCART_F(r,ct,cf)$cartfout(r,ct,cf)            i:PCARTF(r,ct,cf)       prod:CART(r,ct)

* ------ $prod:HEATF
         v:vHEATF_o(r,bt,bf)$heatfout(r,bt,bf)           o:PHEATF(r,bt,bf)       prod:HEATF(r,bt,bf)
         v:vHEATF_pa(r,i,bt,bf)$c0_en(r,i,bt,bf)$ele(i)  i:PA(i,r)               prod:HEATF(r,bt,bf)
         v:vHEATF_pbio(r,bt,bf)$c0_en(r,"agr",bt,bf)     i:PBIO(r,bt,bf)         prod:HEATF(r,bt,bf)
         v:vHEATF_pxthouse(r,bt,bf)$bsd0(r,bt,bf)        i:PXTHOUSE(r,bt,bf)     prod:HEATF(r,bt,bf)
         v:vHEATF_pfuels(r,i,bt,bf)$c0_fuels(r,i,bt,bf)  i:PFUELS(r,i,bt,bf)     prod:HEATF(r,bt,bf)
         v:vHEATF_stock(r,bt,bf)$heatfout(r,bt,bf)       i:PSTOCK_H(r,bt,bf)     prod:HEATF(r,bt,bf)
         v:vHEATF_PCO2ets(r,bt,bf)$heatfout(r,bt,bf)     i:PCO2_ETS              prod:HEATF(r,bt,bf)
         v:vHEATF_PCO2W(r,bt,bf)$heatfout(r,bt,bf)       i:PCO2W                 prod:HEATF(r,bt,bf)
         v:vHEATF_PCO2(r,bt,bf)$heatfout(r,bt,bf)        i:PCO2(r)               prod:HEATF(r,bt,bf)
         v:vHEATF_PCO2nets(r,bt,bf)$heatfout(r,bt,bf)$netstrade  i:PCO2_NETS     prod:HEATF(r,bt,bf)
         v:vHEATF_PCO2netsr(r,bt,bf)$heatfout(r,bt,bf)$netstrade_r i:PCO2_NETSr(r) prod:HEATF(r,bt,bf)

* ------ $prod:CARTF
         v:vCARTF_o(r,ct,cf)$cartfout(r,ct,cf)           o:PCARTF(r,ct,cf)       prod:CARTF(r,ct,cf)
         v:vCARTF_pa(r,i,ct,cf)$c0_en(r,i,ct,cf)$ele(i)  i:PA(i,r)               prod:CARTF(r,ct,cf)
         v:vCARTF_pfuels(r,i,ct,cf)$c0_fuels(r,i,ct,cf)  i:PFUELS(r,i,ct,cf)     prod:CARTF(r,ct,cf)
         v:vCARTF_stock(r,ct,cf)$cartfout(r,ct,cf)       i:PSTOCK_C(r,ct,cf)     prod:CARTF(r,ct,cf)
         v:vCARTF_PCO2ets(r,ct,cf)$cartfout(r,ct,cf)     i:PCO2_ETS              prod:CARTF(r,ct,cf)
         v:vCARTF_PCO2W(r,ct,cf)$cartfout(r,ct,cf)       i:PCO2W                 prod:CARTF(r,ct,cf)
         v:vCARTF_PCO2(r,ct,cf)$cartfout(r,ct,cf)        i:PCO2(r)               prod:CARTF(r,ct,cf)
         v:vCARTF_PCO2nets(r,ct,cf)$cartfout(r,ct,cf)$netstrade  i:PCO2_NETSr(r) prod:CARTF(r,ct,cf)
         v:vCARTF_PCO2netsr(r,ct,cf)$cartfout(r,ct,cf)$netstrade_r i:PCO2_NETSr(r) prod:CARTF(r,ct,cf)

* ------ $prod:NEWCAR
         V:VNC_NEWCAR(r,ct,cf)$deu(r)                    o:PNEWCAR(r,ct,cf)      prod:NEWCAR(r,ct,cf)
         V:VNC_PA(r,i,ct,cf)$deu(r)                      i:PA(i,r)               prod:NEWCAR(r,ct,cf)
         V:VNC_PEFF_I(r,ct,cf)$deu(r)                    i:PEFF(r)               prod:NEWCAR(r,ct,cf)
         V:VNC_PEFF_O(r,ct,cf)$deu(r)                    o:PEFF(r)               prod:NEWCAR(r,ct,cf)

* ------ $prod:NEWHOUSE
         V:VNH_NEWHOUSE(r,bt,bf)$sum(i, bnd0(r,i,bt,bf)) o:PNEWHOUSE(r,bt,bf)    prod:NEWHOUSE(r,bt,bf)
         V:VNH_PA(r,i,bt,bf)$bnd0(r,i,bt,bf)             i:PA(i,r)               prod:NEWHOUSE(r,bt,bf)
         V:VNH_PEFF_I(r,bt,bf)$deu(r)                    i:PEFFh(r)              prod:NEWHOUSE(r,bt,bf)
         V:VNH_PEFF_O(r,bt,bf)$deu(r)                    o:PEFFh(r)              prod:NEWHOUSE(r,bt,bf)

* ------ $prod:STOCK_C
         v:vstock_co(r,ct,cf)$sy0(r,ct,cf)$deu(r)        o:PSTOCK_C(r,ct,cf)     prod:STOCK_C(r,ct,cf)
         v:vstock_cnc(r,ct,cf)$sy0(r,ct,cf)              i:PNEWCAR(r,ct,cf)      prod:STOCK_C(r,ct,cf)
         v:vstock_cxc(r,ct,cf)$sy0(r,ct,cf)              i:PXTCAR(r,ct,cf)       prod:STOCK_C(r,ct,cf)

* ------ $prod:STOCK_H
*         v:vstock_ho(r,bt,bf)$by0(r,bt,bf)$new(bt)$deu(r) o:PSTOCK_H(r,bt,bf)   prod:STOCK_H(r,bt,bf)
         v:vstock_ho(r,bt,bf)$by0(r,bt,bf)$deu(r)        o:PSTOCK_H(r,bt,bf)     prod:STOCK_H(r,bt,bf)
         v:vstock_hnh(r,bt,bf)$bnd0(r,"bui",bt,bf)       i:PNEWHOUSE(r,bt,bf)    prod:STOCK_H(r,bt,bf)
* ------ 7.02.2016 auskommentiert (unten)
*         v:vstock_hxh07(r,bt,bf)$bsdyr0(r,bt,bf,"2007")  i:PXTHOUSE07(r,bt,bf)   prod:STOCK_H(r,bt,bf)
*         v:vstock_hxh10(r,bt,bf)$bsdyr0(r,bt,bf,"2010")  i:PXTHOUSE10(r,bt,bf)   prod:STOCK_H(r,bt,bf)
*         v:vstock_hxh15(r,bt,bf)$bsdyr0(r,bt,bf,"2015")  i:PXTHOUSE15(r,bt,bf)   prod:STOCK_H(r,bt,bf)
*         v:vstock_hxh20(r,bt,bf)$bsdyr0(r,bt,bf,"2020")  i:PXTHOUSE20(r,bt,bf)   prod:STOCK_H(r,bt,bf)
*         v:vstock_hxh25(r,bt,bf)$bsdyr0(r,bt,bf,"2025")  i:PXTHOUSE25(r,bt,bf)   prod:STOCK_H(r,bt,bf)
*         v:vstock_hxh30(r,bt,bf)$bsdyr0(r,bt,bf,"2030")  i:PXTHOUSE30(r,bt,bf)   prod:STOCK_H(r,bt,bf)

         v:VSTOCK_HNEWH(r,bt,bf)$bsd0nh(r,bt,bf)         i:PNEWH(r,bt,bf)        prod:STOCK_H(r,bt,bf)

         V:vstock_hpeffi(r,bt,bf)$deu(r)                 i:PEFFh(r)              prod:STOCK_H(r,bt,bf)
         V:vstock_hpeffo(r,bt,bf)$deu(r)                 o:PEFFh(r)              prod:STOCK_H(r,bt,bf)

* ------ $prod:FUELS
*         V:VFUELS_O(r,fe,cbt,cbf)$c0_fuels(r,fe,cbt,cbf)                 o:PFUELS(r,fe,cbt,cbf)  prod:PFUELS(r,fe,cbt,cbf)
*         V:VFUELS_PA(r,j,cbt,cbf)                                    i:PA(j,r)           prod:PFUELS(r,i,cbt,cbf)
*         V:VFUELS_PA(r,"col",cbt,cbf)                                    i:PA("col",r)           prod:PFUELS(r,"col",cbt,cbf)
*         V:VFUELS_POILS(r,fe,cbt,cbf)$c0_en(r,"oil",cbt,cbf)             i:POILS(r,cbt,cbf)      prod:PFUELS(r,fe,cbt,cbf)

* ------ $prod:OILS
         V:VOILS_O(r,cbt,cbf)$c0_oil(r,"oil",cbt,cbf)            o:POILS(r,cbt,cbf)      prod:OILS(r,cbt,cbf)
         V:VOILS_PA(r,i,cbt,cbf)$c0_oil(r,i,cbt,cbf)$oil(i)      i:PA(i,r)               prod:OILS(r,cbt,cbf)

* ------ $prod:BIO
         V:VB_BIO(r,cbt,cbf)$c0_en(r,"agr",cbt,cbf)              o:PBIO(r,cbt,cbf)       prod:BIO(r,cbt,cbf)
         V:VB_PA(r,i,cbt,cbf)$c0_en(r,i,cbt,cbf)$agr(i)          i:PA(i,r)               prod:BIO(r,cbt,cbf)


*BLOCK oben komplett entfernen und in "MPSGE-Block_HHenergy.txt"-Datei zwischenspeichern
* ###### 5.02.2016 Textmarke Comment out Diss (oben)

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

* ------ INCOME BALANCE --------------------------------------------------------

* ------ 18.04.2014 Rutherford GAMS-L 5.08.2013 (--> S:1 means constant marginal propensity to save)
$demand:RA(r)    s:1

         d:PC(r)                 q:(vom("c",r) + vom("g",r))
* ###### 5.02.2016 Textmarke Comment out Diss (oben/unten)
         d:PC(r)                 q:(vom("c",r) + vom("g",r) + sum((ct,cf), ssd0(r,ct,cf)) + sum((bt,bf), bsd0(r,bt,bf) + bsdyr_init(r,bt,bf) + bsd0nh(r,bt,bf)))
* ###### 5.02.2016 Textmarke Comment out Diss (oben)
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

* ###### 5.02.2016 Textmarke Comment out Diss (unten)
* ------ Energy service capital (cars + buildings)
         E:PXTCAR(r,ct,cf)              q:ssd(r,ct,cf)
         E:PNEWH(r,bt,bf)               q:bsdnh(r,bt,bf)
         E:PXTHOUSE(r,bt,bf)$old(bt)    q:bsd(r,bt,bf)
* ###### 5.02.2016 Textmarke Comment out Diss (oben)

***       E:NCAR(r,ct,cf)                  q:sndc(r,ct,cf)
* ------ 7.02.2016 auskommentiert (unten)
***        E:PXTHOUSE07(r,bt,bf)$bsdyr(r,bt,bf,"2007")      q:bsdyr(r,bt,bf,"2007")
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

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ ZERO PROFIT Conditions (Production of Y-Output) --------------------- *

* --> (1) INDUSTRY production (all but COL, GAS, CRU, OIL, ELE, TRN)
* --> only difference to (3) is the nesting of PSKL and PUSK (lab: vs. va:)

* ------ 10.12.2014 (Erste Zeile Original)
$prod:Y(i,r)$nr(i,r)   s:0  vae(s):0.5  va(vae):1  e(vae):0.1  nel(e):0.5  lqd(nel):2  oil(lqd):0  col(nel):0  gas(lqd):0
*$prod:Y(i,r)$nr(i,r)   s:0  vae(s):0.25  va(vae):1  e(vae):0.1  nel(e):0.5  lqd(nel):2  oil(lqd):0  col(nel):0  gas(lqd):0

* ------ 23.05.2014
         o:PY(i,r)               q:vom(i,r)             a:RA(r) t:ty(i,r)

         i:PA(j,r)$(not e(j))    q:vafm(j,i,r)                  p:pai0(j,i,r)               a:RA(r) t:ti(j,i,r)
         i:PA(e,r)$cru(e)        q:vafm(e,i,r)                  p:pai0(e,i,r)               a:RA(r) t:ti(e,i,r)
         i:PA(fe,r)              q:(vafm(fe,i,r)*aeei(fe,i,r))  p:pai0(fe,i,r)  fe.tl:      a:RA(r) t:ti(fe,i,r)
         I:PA(e,r)$ele(e)        q:(vafm(e,i,r)*aeei(e,i,r))    p:pai0(e,i,r)   e:$ele(e)   a:RA(r) t:ti(e,i,r)

*         i:PAY(j,i,r)$(not e(j))    q:vafm(j,i,r)                  p:pai0(j,i,r)               a:RA(r) t:ti(j,i,r)
*         i:PAY(e,i,r)$cru(e)        q:vafm(e,i,r)                  p:pai0(e,i,r)               a:RA(r) t:ti(e,i,r)
*         i:PAY(fe,i,r)              q:(vafm(fe,i,r)*aeei(fe,i,r))  p:pai0(fe,i,r)  fe.tl:      a:RA(r) t:ti(fe,i,r)
*         I:PAY(e,i,r)$ele(e)        q:(vafm(e,i,r)*aeei(e,i,r))    p:pai0(e,i,r)   e:$ele(e)   a:RA(r) t:ti(e,i,r)

         i:PSKL(r)               q:skld0(i,r)           va:
         i:PUSK(r)               q:uskd0(i,r)           va:
         i:RKG$gk                q:kd0(i,r)             va:
         i:RKR(r)$rsk            q:kd0(i,r)             va:
         i:PR(I,r)$rd0(i,r)      q:rd0(i,r)             va:
* ------ Carbon regimes:
         i:PCO2(r)#(fe)$notrad(r)                                q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:
         i:PCO2W#(fe)$pco2w_r(r)$worldtrade                      q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:
         i:PCO2W#(fe)$eu28(r)$eutrade                            q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:
         i:PCO2W#(fe)$noneu28(r)$worldtrade2                     q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:
         i:PCO2_ETS#(fe)$eu28(r)$ets(i)$etstrade                 q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:
         i:PCO2_NETS#(fe)$eu28(r)$(not ets(i))$netstrade         q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:
         i:PCO2_NETSr(r)#(fe)$eu28(r)$(not ets(i))$netstrade_r   q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:

* Textmarke SE

* --> (3) REFINED OIL production
* --> only difference to (2) is the nesting of PSKL and PUSK (lab: vs. va:)
* --> REFINING (oil Production), which is capital intensiv , define va <=0.5    ??????

$prod:Y(i,r)$(vom(i,r)$oil(i))   s:0  vae(s):0.5  va(vae):0.2 lab(va):0.5  e(vae):0.1  nel(e):0.5 lqd(nel):2  oil(lqd):0  col(nel):0  gas(lqd):0

* ------ 23.05.2014
         o:PY(i,r)               q:vom(i,r)              a:RA(r) t:ty(i,r)

         i:PA(j,r)$(not e(j))    q:vafm(j,i,r)                   p:pai0(j,i,r)               a:RA(r) t:ti(j,i,r)
         i:PA(e,r)$cru(e)        q:vafm(e,i,r)                   p:pai0(e,i,r)               a:RA(r) t:ti(e,i,r)
         i:PA(fe,r)              q:(vafm(fe,i,r)*aeei(fe,i,r))   p:pai0(fe,i,r)  fe.tl:      a:RA(r) t:ti(fe,i,r)
         I:PA(e,r)$ele(e)        q:(vafm(e,i,r)*aeei(e,i,r))     p:pai0(e,i,r)   e:$ele(e)   a:RA(r) t:ti(e,i,r)

*         i:PAY(j,i,r)$(not e(j))    q:vafm(j,i,r)                   p:pai0(j,i,r)               a:RA(r) t:ti(j,i,r)
*         i:PAY(e,i,r)$cru(e)        q:vafm(e,i,r)                   p:pai0(e,i,r)               a:RA(r) t:ti(e,i,r)
*         i:PAY(fe,i,r)              q:(vafm(fe,i,r)*aeei(fe,i,r))   p:pai0(fe,i,r)  fe.tl:      a:RA(r) t:ti(fe,i,r)
*         I:PAY(e,i,r)$ele(e)        q:(vafm(e,i,r)*aeei(e,i,r))     p:pai0(e,i,r)   e:$ele(e)   a:RA(r) t:ti(e,i,r)

         i:PSKL(r)               q:skld0(i,r)            lab:
         i:PUSK(r)               q:uskd0(i,r)            lab:
         i:RKG$gk                q:kd0(i,r)      va:
         i:RKR(r)$rsk            q:kd0(i,r)      va:
         i:PR(I,r)$rd0(i,r)      q:rd0(I,r)      va:

* ------ Carbon regimes:
         i:PCO2(r)#(fe)$notrad(r)                                q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:
         i:PCO2W#(fe)$pco2w_r(r)$worldtrade                      q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:
         i:PCO2W#(fe)$eu28(r)$eutrade                            q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:
         i:PCO2W#(fe)$noneu28(r)$worldtrade2                     q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:
         i:PCO2_ETS#(fe)$eu28(r)$ets(i)$etstrade                 q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:


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

         o:PY(i,r)               q:vom(i,r)              a:RA(r) t:ty(i,r)

*T:tax11(i,r)    A:RA(r)$baw(r)

         i:PA(j,r)$(not e(j))    q:vafm(j,i,r)                   p:pai0(j,i,r)           a:RA(r) t:ti(j,i,r)    id:
         i:PA(e,r)$cru(e)        q:vafm(e,i,r)                   p:pai0(e,i,r)           a:RA(r) t:ti(e,i,r)    id:
         i:PA(fe,r)              q:(vafm(fe,i,r)*aeei(fe,i,r))   p:pai0(fe,i,r)          a:RA(r) t:ti(fe,i,r)   id:
         i:PA(e,r)$ele(e)        q:(vafm(e,i,r)*aeei(e,i,r))     p:pai0(e,i,r)           a:RA(r) t:ti(e,i,r)    id:

*         i:PAY(j,i,r)$(not e(j))    q:vafm(j,i,r)                   p:pai0(j,i,r)           a:RA(r) t:ti(j,i,r)    id:
*         i:PAY(e,i,r)$cru(e)        q:vafm(e,i,r)                   p:pai0(e,i,r)           a:RA(r) t:ti(e,i,r)    id:
*         i:PAY(fe,i,r)              q:(vafm(fe,i,r)*aeei(fe,i,r))   p:pai0(fe,i,r)          a:RA(r) t:ti(fe,i,r)   id:
*         i:PAY(e,i,r)$ele(e)        q:(vafm(e,i,r)*aeei(e,i,r))     p:pai0(e,i,r)           a:RA(r) t:ti(e,i,r)    id:

         i:PSKL(R)               q:skld0(i,r)          lab:
         i:PUSK(R)               q:uskd0(i,r)          lab:
         i:RKR(r)$rsk            q:kd0(i,r)     id:
         i:RKG$gk                q:kd0(i,r)     id:
         i:PR(i,r)$rd0(i,r)      q:rd0(i,r)

* ------ Carbon regimes:
*         i:PCO2W#(fe)$worldtrade                                 q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:
*         i:PCO2W#(fe)$noneu28(r)$worldtrade2                     q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:
*         i:PCO2_NETS#(fe)$eu28(r)$(not ets(i))$netstrade         q:(co2em(fe,i,r) * aeei(fe,i,r))        p:1e-6  fe.tl:



* --> 4) Electricity production (aggregating generation technologies)
* ------ 10.12.2014  Aufteilung in EU28 und nicht-EU28 um S: und/oder BM: einzuschränken (s.u.)
$prod:Y(i,r)$(ele(i) and EU28(r))     s:0.8    pl:5.0  og(pl):2.5      bm:5.0  bl(bm):8.0      ml(bm):5.0     ee:5.0
*$prod:Y(i,r)$(ele(i) and EU28(r))     s:0.8    pl:5.0  og(pl):2.5      bm:1.0  bl(bm):8.0      ml(bm):16.0     ee:5.0

         o:PY(i,r)       q:vom(i,r)       a:RA(r) t:ty(i,r)    N:REBATE_DIFF(r)$diffrebate(r)   N:YLO(i,r)$ylo_on(i,r)  M:(-1)$ylo_on(i,r)

         i:PGEN(gen,r)   q:out_gen(gen,r) bl:$base(gen)  ml:$middle(gen)   og:$og(gen)    pl:$peak(gen)


$prod:Y(i,r)$(ele(i) and not EU28(r)) s:0.8    pl:5.0  og(pl):2.5      bm:4.0  bl(bm):8.0      ml(bm):5.0     ee:5.0
*$prod:Y(i,r)$(ele(i) and not EU28(r)) s:0.8    pl:5.0  og(pl):2.5      bm:4.0  bl(bm):8.0      ml(bm):16.0     ee:5.0
         o:PY(i,r)       q:vom(i,r)       a:RA(r)        t:ty(i,r)       N:REBATE_DIFF(r)$diffrebate(r)  N:YLO(i,r)$ylo_on(i,r) M:(-1)$ylo_on(i,r)
         i:PGEN(gen,r)   q:out_gen(gen,r) bl:$base(gen)  ml:$middle(gen)   og:$og(gen)    pl:$peak(gen)


* --> 6) electricity generation with extant capital (eeg-geförderte erneuerbare)
* --> only difference to (7) are o:pgen, i:pq, i:rkx_ele(gen,r)$eupol and i:rkx_ele(gen,r)$(not eupol)

$prod:ELEx(gen,r)$((out_gen(gen,r))$(ks_x(gen,r))$(reg(gen)))    s:0

         o:PGEN(gen,r)           q:out_gen(gen,r)

         i:PA(i,r)$(not e(i))    q:vafm_input(i,gen,r)                    p:pai0(i,"ele",r)   a:ra(r)   t:ti(i,"ele",r)
         i:PA(e,r)$cru(e)        q:vafm_input(e,gen,r)                    p:pai0(e,"ele",r)   a:ra(r)   t:ti(e,"ele",r)
         i:PA(e,r)$fe(e)         q:(vafm_input(e,gen,r)*aeei_elex(gen,r)) p:pai0(e,"ele",r)   a:ra(r)   t:ti(e,"ele",r)
         i:PA(e,r)$ele(e)        q:(vafm_input(e,gen,r)*aeei(e,"ele",r))  p:pai0(e,"ele",r)   a:ra(r)   t:ti(e,"ele",r)

         i:PSKL(r)               q:skl_input(gen,r)
         i:PUSK(r)               q:usk_input(gen,r)
         i:RKX_ELE(gen,r)        q:cap_input(gen,r)

* ------ Carbon regimes:
         i:PCO2(r)#(fe)$notrad(r)                                q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6
         i:PCO2W#(fe)$pco2w_r(r)$worldtrade                      q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6
         i:PCO2W#(fe)$eu28(r)$eutrade                            q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6
         i:PCO2W#(fe)$noneu28(r)$worldtrade2                     q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6
         i:PCO2_ETS#(fe)$eu28(r)$etstrade                        q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6

* --> 7) electricity production of extant capital (nicht eeg-geförderte erneuerbare)
* --> only difference to (6) are o:pgen, i:pq, i:rkx_ele(gen,r)$eupol and i:rkx_ele(gen,r)$(not eupol)

$prod:ELEx(gen,r)$((out_gen(gen,r))$(ks_x(gen,r))$(not reg(gen)))    s:0

         o:PGEN(gen,r)           q:out_gen(gen,r)

         i:PA(i,r)$(not e(i))    q:vafm_input(i,gen,r)                    p:pai0(i,"ele",r)   a:ra(r)   t:ti(i,"ele",r)
         i:PA(e,r)$cru(e)        q:vafm_input(e,gen,r)                    p:pai0(e,"ele",r)   a:ra(r)   t:ti(e,"ele",r)
         i:PA(e,r)$fe(e)         q:(vafm_input(e,gen,r)*aeei_elex(gen,r)) p:pai0(e,"ele",r)   a:ra(r)   t:ti(e,"ele",r)
         i:PA(e,r)$ele(e)        q:(vafm_input(e,gen,r)*aeei(e,"ele",r))  p:pai0(e,"ele",r)   a:ra(r)   t:ti(e,"ele",r)

         i:PSKL(r)               q:skl_input(gen,r)
         i:PUSK(r)               q:usk_input(gen,r)
         i:RKX_ELE(gen,r)        q:cap_input(gen,r)

* ------ Carbon regimes:
         i:PCO2(r)#(fe)$notrad(r)                                q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6
         i:PCO2W#(fe)$pco2w_r(r)$worldtrade                      q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6
         i:PCO2W#(fe)$eu28(r)$eutrade                            q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6
         i:PCO2W#(fe)$noneu28(r)$worldtrade2                     q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6
         i:PCO2_ETS#(fe)$eu28(r)$etstrade                        q:(co2em(fe,gen,r) * aeei_elex(gen,r))  p:1e-6

* --> 8) electricity generation from new capital (eeg-geförderte erneuerbare)
* --> completely different to (9) except all carbon inputs i:pcarb...
* --> see difference to (6) in separate txt-file

$prod:ELEn(gen,r)$out_gen(gen,r)$ks_n(gen,r)$reg(gen)     s:0  lab(s):0

         o:PGEN(gen,r)           q:out_gen(gen,r) a:ra(r) t:(1-diffcost(gen,r))

         i:PA(i,r)$(not fe(i))   q:(vafm_input(i,gen,r)*(diffcost(gen,r)))                   p:pai0(i,"ele",r)   a:ra(r)   t:ti(i,"ele",r)
         i:PA(i,r)$fe(i)         q:(vafm_input(i,gen,r)*(diffcost(gen,r))*aeei_elen(gen,r))  p:pai0(i,"ele",r)   a:ra(r)   t:ti(i,"ele",r)

         i:PSKL(r)               q:(skl_input(gen,r)*(diffcost(gen,r)))    lab:
         i:PUSK(r)               q:(usk_input(gen,r)*(diffcost(gen,r)))    lab:
         i:RKG$gk                q:(cap_input(gen,r)*(diffcost(gen,r)))
         i:RKR(r)$rsk            q:(cap_input(gen,r)*(diffcost(gen,r)))

* ------ Carbon regimes:
         i:PCO2(r)#(fe)$notrad(r)                                q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6
         i:PCO2W#(fe)$pco2w_r(r)$worldtrade                      q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6
         i:PCO2W#(fe)$eu28(r)$eutrade                            q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6
         i:PCO2W#(fe)$noneu28(r)$worldtrade2                     q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6
         i:PCO2_ETS#(fe)$eu28(r)$etstrade                        q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6

* --> 9) electricity generation from new capital (nicht eeg-geförderte erneuerbare)
* --> completely different to (8) except all carbon inputs i:pcarb...
* --> see difference to (7) in separate txt-file

$prod:ELEn(gen,r)$((out_gen(gen,r))$(ks_n(gen,r))$(not reg(gen))) s:0  lab(s):0

         o:PGEN(gen,r)           q:out_gen(gen,r)

         i:PA(i,r)$(not e(i))    q:vafm_input(i,gen,r)                    p:pai0(i,"ele",r)   a:ra(r)   t:ti(i,"ele",r)
         i:PA(e,r)$cru(e)        q:vafm_input(e,gen,r)                    p:pai0(e,"ele",r)   a:ra(r)   t:ti(e,"ele",r)
         i:PA(e,r)$ele(e)        q:(vafm_input(e,gen,r)*aeei(e,"ele",r))  p:pai0(e,"ele",r)   a:ra(r)   t:ti(e,"ele",r)
         i:PA(e,r)$fe(e)         q:(vafm_input(e,gen,r)*aeei_elen(gen,r)) p:pai0(e,"ele",r)   a:ra(r)   t:ti(e,"ele",r)

         i:PSKL(r)               q:skl_input(gen,r)      lab:
         i:PUSK(r)               q:usk_input(gen,r)      lab:
         i:RKG$gk                q:cap_input(gen,r)
         i:RKR(r)$rsk            q:cap_input(gen,r)

* ------ Carbon regimes:
         i:PCO2(r)#(fe)$notrad(r)                                q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6
         i:PCO2W#(fe)$pco2w_r(r)$worldtrade                      q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6
         i:PCO2W#(fe)$eu28(r)$eutrade                            q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6
         i:PCO2W#(fe)$noneu28(r)$worldtrade2                     q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6
         i:PCO2_ETS#(fe)$eu28(r)$etstrade                        q:(co2em(fe,gen,r) * aeei_elen(gen,r))  p:1e-6


* --> 11)  Foreign Trade: ARMINGTON Aggregation over Domestic versus Imports (Armington supply):

$prod:A(i,r)$a0(i,r)   s:4       m:8     s.tl(m):0

         o:PA(i,r)        q:a0(i,r)

         i:PY(i,r)        q:d0(i,r)
         i:PY(i,s)        q:vxmd(i,s,r)      p:pmx0(i,s,r) s.tl: a:RA(s) t:tx(i,s,r)     a:RA(r) t:(tm(i,s,r)*(1+tx(i,s,r)))
         i:PT#(s)         q:vtwr1(i,s,r)     p:pmt0(i,s,r) s.tl: a:RA(r) t:tm(i,s,r)


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
         i:PA(j,r)               q:vafm(j,"i",r)         p:pai0(j,"i",r) a:RA(r) t:ti(j,"i",r)

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

$constraint:URSK(r)$(ursk0(r)$(not(emerge(r))))
*         PSKL(r)/PC(r)   =E= (( (psklbmk(r)/pcbmk(r)) / (ursk0(r)**(-0.1))) * (URSK(r)**(-0.1)) );      // ORIGINAL
         PSKL(r)/PC(r)   =E= (( (psklbmk(r)/pcbmk(r)) / (ursk0(r)**(-0.12))) * (URSK(r)**(-0.12)) );       // vermeidet REDEF bei RUS und ARB, wenn RUS und ARB nicht in emerge

$constraint:URSK(r)$(ursk0(r)$emerge(r))
         PSKL(r)/PC(r)   =E= (( (psklbmk(r)/pcbmk(r)) / (ursk0(r)**(-0.6))) * (URSK(r)**(-0.6)) );

$constraint:URUN(r)$(urun0(r)$emerge(r))
         PUSK(r)/PC(r)   =E= (( (puskbmk(r)/pcbmk(r)) / (urun0(r)**(-0.9))) * (URUN(r)**(-0.9)) );

$constraint:URUN(r)$(ursk0(r)$(not(emerge(r))))
         PUSK(r)         =G= PC(r);

$constraint:REBATE_DIFF(r)$diffrebate(r)
         REBATE_DIFF(r) * PY("ele",r) * Y("ele",r) * vom("ele",r)
         =E= (-1)*(sum(gen$reg(gen), (1-diffcost(gen,r)) * (PGEN(gen,r) * out_gen(gen,r) * ELEn(gen,r))));

* ------ 17.06.2014
$constraint:R_SUPPLY(i,r)$(rd0(i,r)$PRICETARGET(i,R))
         pytarget(i,r) =e= py(i,r)/pc(r) ;

* ------ 12.01.2015
$constraint:YLO(i,r)$ylo_on(i,r)
         Y(i,r) =G= ylo_par(i,r) ;

* ###### 5.02.2016 Textmarke Comment out Diss (unten)
$constraint:OLDBTAX(r,bt,bf)$(bsd0(r,bt,bf)$old(bt))
       OLDBTAX(r,bt,bf) =G= (bt_san(r,"old_r",bf) + bt_sub(r,"new_p",bf)) / 20 * (-1);
$constraint:OLDCTAX(r,ct,cf)$ssd0(r,ct,cf)
       OLDCTAX(r,ct,cf) =G= carsub(r,"small","ele") / 10 * (-1) ;
* ###### 5.02.2016 Textmarke Comment out Diss (oben)


* ------ REPORTING -------------------------------------------------------------

$REPORT:

* ------ $demand:RA
         V:W(r)                          w:RA(r)
         V:VD_PC(r)                      d:PC(r)         demand:RA(r)
         V:VD_PINV(r)                    d:PINV(r)       demand:RA(r)

* ------ $prod:C
         V:VC_PC(r)                              o:PC(R)         prod:C(r)
         V:VC_PA(i,r)                            i:PA(i,r)       prod:C(r)
         V:VC_CO2(r)$notrad(r)                   i:PCO2(r)       prod:C(r)
         V:VC_CO2W(r)$pco2w_r(r)$worldtrade      i:PCO2W         prod:C(r)
         V:VC_CO2W(r)$eu28(r)$eutrade            i:PCO2W         prod:C(r)
         V:VC_CO2W(r)$worldtrade2                i:PCO2W         prod:C(r)
         V:VC_CO2_NETS(r)$eu28(r)$netstrade      i:PCO2_NETS     prod:C(r)
         V:VC_CO2_NETSr(r)$eu28(r)$netstrade_r   i:PCO2_NETSr(r) prod:C(r)

* ------ $prod:A
         V:VA_PA(i,r)                    o:PA(i,r)       prod:A(i,r)
         V:VA_PT(i,r)                    i:PT            prod:A(i,r)
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
         V:VY_CO2W(i,r)$eu28(r)$eutrade          i:PCO2W         prod:Y(i,r)
         V:VY_CO2W(i,r)$worldtrade2              i:PCO2W         prod:Y(i,r)
         V:VY_CO2_NETS(i,r)$netstrade            i:PCO2_NETS     prod:Y(i,r)
         V:VY_CO2_NETSr(i,r)$netstrade_r         i:PCO2_NETSr(r) prod:Y(i,r)
         V:VY_CO2_ETS(i,r)$eu28(r)$ets(i)$etstrade i:PCO2_ETS    prod:Y(i,r)

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
         V:VELEx_CO2W(gen,r)$(out_gen(gen,r)$ks_x(gen,r)$eu28(r)$eutrade)       i:PCO2W                 prod:ELEx(gen,r)
         V:VELEx_CO2W(gen,r)$(out_gen(gen,r)$ks_x(gen,r)$worldtrade2)            i:PCO2W                 prod:ELEx(gen,r)
*         V:VELEx_CO2_ETS(gen,r)$(out_gen(gen,r)$ks_x(gen,r)$eu28(r)$etstrade)    i:PCO2_ETS              prod:ELEx(gen,r)
         V:VELEx_CO2_ETS(gen,r)$(out_gen(gen,r)$ks_x(gen,r)$eu28(r)$etstrade)   i:PCO2_ETS              prod:ELEx(gen,r)

* ------ $prod:ELEn
         V:VELEn_PGEN(gen,r)                                     o:PGEN(gen,r)           prod:ELEn(gen,r)
         V:VELEn_PSKL(gen,r)$(out_gen(gen,r)$ks_n(gen,r))                        i:PSKL(r)               prod:ELEn(gen,r)
         V:VELEn_PUSK(gen,r)$(out_gen(gen,r)$ks_n(gen,r))                        i:PUSK(r)               prod:ELEn(gen,r)
         V:VELEn_RKR(gen,r)$(out_gen(gen,r)$ks_n(gen,r)$RSK)                     i:RKR(r)                prod:ELEn(gen,r)
         V:VELEn_RKR(gen,r)$(out_gen(gen,r)$ks_n(gen,r)$GK)                      i:RKG                   prod:ELEn(gen,r)
         V:VELEn_PA(i,gen,r)$(out_gen(gen,r)$ks_n(gen,r))                        i:PA(i,r)               prod:ELEn(gen,r)
         V:VELEn_CO2(gen,r)$(out_gen(gen,r)$ks_n(gen,r)$notrad(r))               i:PCO2(r)               prod:ELEn(gen,r)
         V:VELEn_CO2W(gen,r)$(out_gen(gen,r)$ks_n(gen,r)$pco2w_r(r)$worldtrade)  i:PCO2W                 prod:ELEn(gen,r)
         V:VELEn_CO2W(gen,r)$(out_gen(gen,r)$ks_n(gen,r)$eu28(r)$eutrade)       i:PCO2W                 prod:ELEn(gen,r)
         V:VELEn_CO2W(gen,r)$(out_gen(gen,r)$ks_n(gen,r)$worldtrade2)            i:PCO2W                 prod:ELEn(gen,r)
*         V:VELEn_CO2_ETS(gen,r)$(out_gen(gen,r)$ks_n(gen,r)$eu28(r)$etstrade)    i:PCO2_ETS              prod:ELEn(gen,r)
         V:VELEn_CO2_ETS(gen,r)$(out_gen(gen,r)$ks_n(gen,r)$eu28(r)$etstrade)   i:PCO2_ETS              prod:ELEn(gen,r)


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
* ------ 26.09.2012: Fix variables which do not enter the model to avoid unmatched variables if:
* ---->> see file GAMS-L archives - Unmatched variables (Jan 2005, Rut).pdf URL: http://www.listserv.dfn.de/cgi-bin/wa?A2=ind0501&L=GAMS-L&P=R2639&I=-3
ELEx.FX(GEN,R)$(Not OUT_GEN(GEN,R)) = 0;
ELEn.FX(GEN,R)$(Not OUT_GEN(GEN,R)) = 0;
ELEx.FX(GEN,R)$(Not ks_x(GEN,R)) = 0;
ELEn.FX(GEN,R)$(Not ks_n(GEN,R)) = 0;

* ------ Diffrebate constraint
REBATE_DIFF.LO(r)$diffrebate(r)  = -inf;

* ------ 24.04.2014 Mit diesem Startwert lässt sich der Marginal auf REBATE_DIFF(r) entfernen
* -----> dadurch klaffen aber PA.M und PC.M+PINV.M auseinander
* -----> außerdem ergibt sich dadurch ein Marginal auf Y("ele",r)!!!
* ------ 28.04.2014 works, if diffrebate = 1, diffcost2 = diffcost1:
*REBATE_DIFF.L(r)$diffrebate(r)   = (sum(gen, (-1)*(1-diffcost(gen,r)) * out_gen(gen,r) * 0.015)) / vom("ele",r);
*REBATE_DIFF.L(r)$diffrebate(r)   = 0;

* ------ 11.06.2014 doch nicht nötig...
* ------ GAMS-L: http://www.listserv.dfn.de/cgi-bin/wa?A2=ind0501&L=GAMS-L&P=R2639&I=-3
*R_SUPPLY.FX(i,r)$(not rd0(i,r) or not pricetarget(i,r)) = 0;
* ------ 17.06.2014 R_SUPPLY.FX doch nicht nötig wenn R_SUPPLY.L = 1
R_SUPPLY.L(i,r)$(rd0(i,r) and pricetarget(i,r)) = 1;

* ------ 05.05.2015
YLO.L(i,r)$ylo_on(i,r) = 1;
YLO.LO(i,r)$ylo_on(i,r) = -inf;

* ###### 5.02.2016 Textmarke Comment out Diss (unten)
OLDBTAX.LO(r,bt,bf)$(bsd0(r,bt,bf)$old(bt)) = -inf;
OLDCTAX.LO(r,ct,cf)$ssd0(r,ct,cf) = -inf;
* ###### 5.02.2016 Textmarke Comment out Diss (oben)

* ------ Set initial values of CO2 prices = 0
PCO2.L(r)        = 1e-6 ;
PCO2W.L          = 1e-6 ;
PCO2_ETS.L       = 1e-6 ;
PCO2_NETS.L      = 1e-6 ;
PCO2_NETSr.L(r)  = 1e-6 ;

* ------ CARBON REGIMES --------------------------------------------------------
* ------ Define CO2 endowment
carblim(r)       = + sum(fe, co2em(fe,"final",r))
                   + sum(i,  sum(fe, co2em(fe,i,r)$nr(i,r)) + sum(fe, co2em(fe,i,r)$ele(i))  + sum(fe, co2em(fe,i,r)$(vom(i,r)$oil(i))))
***                   + sum(fe, co2em(fe,i,r)$(vom(i,r)$xe(i)))       // Carbon emissions do not arise in fossil fuel production (GTAP-EG)
* ###### 5.02.2016 Textmarke Comment out Diss (unten)
* ------ 13.09.2015 (Diss)
                   + sum((fe,cbt,cbf), cbco2i(r,fe,cbt,cbf))
* ###### 5.02.2016 Textmarke Comment out Diss (oben)
;
display co2em, carblim;

* ------ 2.3.2015
carblim0(r)      = carblim(r);
carblim0_world   = sum(r, carblim(r));

* ------ Carblim_ets
carblim_ets(r)$eu28(r)   =
         + sum((fe,i)$(ets(i) and nr(i,r)),      co2em(fe,i,r))
         + sum((fe,i)$ele(i),                    co2em(fe,i,r))
         + sum((fe,i)$oil(i),                    co2em(fe,i,r));

* ------ 7.01.2016 --> Carblim_ets
* ###### 5.02.2016 Textmarke Comment out Diss (unten)
carblim_ets(r)$(eu28(r) and hhets)= carblim_ets(r) + sum((fe,cbt,cbf), cbco2i(r,fe,cbt,cbf)) ; display cbco2i;
* ###### 5.02.2016 Textmarke Comment out Diss (oben)

carblim_ets0(r)                   = carblim_ets(r);

* ------ 5.05.2015 --> Carblim
*carblim(r)$(eu28(r) and etstrade or netstrade) = round(carblim(r) - carblim_ets(r),10);
* ------ 11.02.2016
carblim(r)$(eu28(r) and (etstrade or netstrade or netstrade_r))          = round(carblim(r) - carblim_ets(r),10);
*carblimnets(r)$(eu28(r) and (etstrade or netstrade or netstrade_r))      = carblim(r) ;
display carblim, carblim0, carblim0_world, carblim_ets, carblim_ets0 ;

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

* ------ Size of work array in MB
NEWAGE.workspace = 128 ;
* ------ Set iteration limit to zero
NEWAGE.iterlim = 0;
* ------ INCLUDE and SOLVE model "GTAP"
$include NEWAGE.gen
SOLVE    NEWAGE  using   MCP ;                                                   // # SOLVE Statement 1 #

* ------ Abort if objective value is greater than 1E-04 --> Residual of Rutherford's GTAP8inGAMS model is around 1E-05
*abort$(  NEWAGE.objval > 1e-4) "Base year data not balanced.", NEWAGE.objval;
display  NEWAGE.objval;

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ Becnhmark REPORTING --------------------------------------------------- // # Benchmark reporting #

* ------ 17.04.2014 Look at .L- and .M-values of variables
parameter id, idr, idco2, idgen, idyt;
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
* ------ Additional information for idr
idr(r,"PA.L") = sum(i, id(i,r,"PA.L"));
idr(r,"PA.M") = sum(i, id(i,r,"PA.M"));
idr(r,".M sum") = idr(r,"C.M") +  idr(r,"PC.M") + idr(r,"Pinv.M") + idr(r,"INV.M") + idr(r,"RKR.M") + idr(r,"PSKL.M") +  idr(r,"PUSK.M");
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

* ------ 28.04.2014
*display rebate_par;
rebate_par("rebate_diff",r) = (sum(gen$reg(gen), (-1)*(1-diffcost(gen,r)) * (PGEN.L(gen,r) * out_gen(gen,r) * ELEn.L(gen,r)))) / (PY.L("ele",r) * Y.L("ele",r) * vom("ele",r));
rebate_par("zähler",r) = (sum(gen$reg(gen), (-1)*(1-diffcost(gen,r)) * (PGEN.L(gen,r) * out_gen(gen,r) * ELEn.L(gen,r))));
rebate_par("nenner",r) = (PY.L("ele",r) * Y.L("ele",r) * vom("ele",r));
*display rebate_par;
*display pricetarget, pytarget, pytarget_yr;
*display bsd0, bsd, bnd0, by0, by;

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
         co2coefy
         co2coefc
         co2coefele
         co2coefhh
         co2coefhhsum
* ------ CO2-Emissionen (wertbasierte Berechnung mit Wert-Koeffizienten)
         co2real
         co2realc
         co2realy
         co2realele
         co2realhhc
         co2realhhb
* ###### 5.02.2016 Textmarke Comment out Diss (unten)
         co2realhh(fe,cbt,cbf,r)
* ###### 5.02.2016 Textmarke Comment out Diss (oben)
* ------ CO2-Emissionen (Checks)
         co2realc_chk
         co2realy_chk
         co2realele_chk
         co2em_totalchk
         co2chk
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
VA_PYxm.L(i,r,r) = 0;
ex(i,r) = sum(s, VA_PYxm.L(i,r,s));
im(i,r) = sum(s, VA_PYxm.L(i,s,r));
trdblnc(i,r) = ex(i,r) - im(i,r);
trdblnc("total",r) = sum(i, ex(i,r) - im(i,r));
gdpreal(r) =  VC_PC.L(r) + VINV_PINV.L(r) + trdblnc("total",r);
emplmt("skl",r) = sum(i, VY_PSKL.L(i,r)) + sum(gen, VELEx_PSKL.L(gen,r) + VELEn_PSKL.L(gen,r));
emplmt("usk",r) = sum(i, VY_PUSK.L(i,r)) + sum(gen, VELEx_PUSK.L(gen,r) + VELEn_PUSK.L(gen,r));
emplmt("total",r) = emplmt("skl",r) + emplmt("usk",r) ;
ur("skl",r) = URSK.L(r) ;
ur("usk",r) = URUN.L(r) ;
ur("total",r) = (emplmt("skl",r) * URSK.L(r) + emplmt("usk",r) * URUN.L(r)) / emplmt("total",r);
*display  welf, ex, im, trdblnc, gdpreal, emplmt, ur;

* ------ Execute Unload GDX-file ----------------------------------------------- // # Execute Unload bmk.GDX #

Execute_Unload       "bmk.gdx";

display co2em, co2em_total, carblim, carblim_ets, evd;

*$EXIT
*EXITsolve1
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* XXXXXXXXXXXXXXXXXXXXXXXXXX Static Scenarios XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*$ontext
* ------ 04.02.2015 policy scenarios: energy taxes
*tfueli("deu","oil",ct,cf)$oilc(cf) = tfueli("deu","oil",ct,cf) * 2 ;
*tfueli("deu","oil",ct,cf)$phe(cf)  = tfueli("deu","oil",ct,cf) * 0.8 * 2 ;
*tc("oil","deu") = tc("oil","deu") * 2 ;

* ------ 02.04.2015 Policy scenario: Cars efficiency targets
*eff_target(r)$deu(r) = eff_avg(r) * 0.85; // works
*eff_target(r)$deu(r) = eff_avg(r) * 0.05; // works
*eff_target(r)$deu(r) = eff_avg(r) * 0.95; // works
*eff_target(r)$deu(r) = eff_avg(r) * 0.75;  // does not work!
*eff_target(r)$deu(r) = 40 / 1000 ;
*eff_target(r)$deu(r) = 2 / 1000 ;     // goes down to 1! Divide by 1000 in order to scale it more adequately  --> look at carco2


* ------ 04.02.2015 Policy scenario: (New) buildings demand targets
*effh_target(r)$deu(r) = effh_avg(r);  // NEUTRAL !
*effh_target(r)$deu(r) = effh_avg(r) * 0.5;
*effh_target(r)$deu(r) = 150;
*effh_target(r)$deu(r) = 60;

*effh_target(r)$deu(r) = 30;
*effh_target(r)$deu(r) = 23;          // (new) minimum

* ------ 06.02.2015 Policy Scenario co2tax
*co2price = 35.8;
*co2tax(r,i,cbt,cbf)$c0_fuels(r,i,cbt,cbf) = cbco2i(r,i,cbt,cbf) * co2price / c0_fuels(r,i,cbt,cbf) ;
*co2price = 903.2;
*co2tax(r,i,bt,bf)$c0_fuels(r,i,bt,bf) = cbco2i(r,i,bt,bf) * co2price / c0_fuels(r,i,bt,bf) ;

* ------ 14.09.2015 (Diss) Emissionshandel
*netstrade = 1;
*carblim(r)$deu(r) = carblim(r) * (1 - 0.0567);

*bsd(r,bt,bf) = 0.9 * bsd(r,bt,bf);
*ESD.LO(r) = 0.98;

* ------ Emissions trading
*netstrade        = 0;
*worldtrade       = 1;
*carblim(r)$eu28(r) = 0.9 * carblim(r) ;
*carblim_ets(r)     = 0.9 * carblim_ets(r) ;

*diffcost(gen,r) = 1;

*display eff_target, eff_avg, effh_target, effh_avg, carco2, co2tax, diffcost;

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
$ontext
* ------ Size of work array in MB
NEWAGE.workspace = 128 ;
* ------ Set iteration limit to zero
NEWAGE.iterlim = 2000;
* ------ INCLUDE and SOLVE model "GTAP"
$include NEWAGE.gen
SOLVE    NEWAGE  using   MCP ;                                                   // # SOLVE Statement 1 #
* ------ Abort if objective value is greater than 1E-04 --> Residual of Rutherford's GTAP8inGAMS model is around 1E-05
*abort$(  NEWAGE.objval > 1e-4) "Base year data not balanced.", NEWAGE.objval;
display  NEWAGE.objval;
$offtext
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

* XXXXXXXXXXXXXXXXXXXXXXXXXX Static REPORTING XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

* ------ 11.12.2013 Report parameter for solvestat-modelstat-objval-numredef (http://www.gams.de/mccarl/mccarlhtml/model_attributes_mainly_used_a.htm)
status("solvestat","Bmk")= NEWAGE.solvestat;
status("modelstat","Bmk")= NEWAGE.modelstat;
status("objval","Bmk")   = round(NEWAGE.objval, 6);
status("numredef","Bmk") = NEWAGE.numredef;
* ------ 11.05.2013
status("iterUsd","Bmk")  = NEWAGE.iterUsd;
display status;

* ###### 5.02.2016 Textmarke Comment out Diss (unten)
*$ontext
* ------ Cars and buildings (incl. efficiency standards)
cars2(r,ct,cf,"new") = cars(r,ct,cf,"new") * NEWCAR.L(r,ct,cf) ;
cars2(r,"total","total","new") = sum((ct,cf), cars(r,ct,cf,"new") * NEWCAR.L(r,ct,cf)) ;
houseqm2(r,bt,bf)$new(bt) = houseqm(r,bt,bf) * NEWHOUSE.L(r,bt,bf) ;
houseqm2(r,"total","total") = sum((bt,bf)$new(bt), houseqm(r,bt,bf) * NEWHOUSE.L(r,bt,bf)) ;

eff_reso(r)$sum((ct,cf), cars(r,ct,cf,"new") * NEWCAR.L(r,ct,cf)) = sum((ct,cf), VNC_PEFF_O.L(r,ct,cf)) / sum((ct,cf), cars(r,ct,cf,"new") * NEWCAR.L(r,ct,cf)) ;
eff_resi(r)$sum((ct,cf), cars(r,ct,cf,"new") * NEWCAR.L(r,ct,cf)) = sum((ct,cf), VNC_PEFF_I.L(r,ct,cf)) / sum((ct,cf), cars(r,ct,cf,"new") * NEWCAR.L(r,ct,cf)) ;
effh_reso(r)$sum((bt,bf)$new(bt), houseqm(r,bt,bf) * NEWHOUSE.L(r,bt,bf)) = sum((bt,bf)$new(bt), VNH_PEFF_O.L(r,bt,bf)) / sum((bt,bf)$new(bt), houseqm(r,bt,bf) * NEWHOUSE.L(r,bt,bf)) ;
effh_resi(r)$sum((bt,bf)$new(bt), houseqm(r,bt,bf) * NEWHOUSE.L(r,bt,bf)) = sum((bt,bf)$new(bt), VNH_PEFF_I.L(r,bt,bf)) / sum((bt,bf)$new(bt), houseqm(r,bt,bf) * NEWHOUSE.L(r,bt,bf)) ;
*display houseqm, houseqm2, effh_reso, effh_resi, eff_target, eff_avg, eff_reso, eff_resi, cars, cars2;
*$offtext
* ###### 5.02.2016 Textmarke Comment out Diss (oben)


* ------ Macroeconomic Indicators ----------------------------------------------
welf(r) = W.L(r);
VA_PYxm.L(i,r,r) = 0;
ex(i,r) = sum(s, VA_PYxm.L(i,r,s));
im(i,r) = sum(s, VA_PYxm.L(i,s,r));
trdblnc(i,r) = ex(i,r) - im(i,r);
trdblnc("total",r) = sum(i, ex(i,r) - im(i,r));
gdpreal(r) =  VC_PC.L(r) + VINV_PINV.L(r) + trdblnc("total",r);
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
co2realc(fe,r)$co2coefc(fe,r)                            = VC_PA.L(fe,r)         / co2coefc(fe,r);

* ###### 5.02.2016 Textmarke Comment out Diss (unten)
*$ontext
co2coefhh(fe,cbt,cbf,r)$cbco2i(r,fe,cbt,cbf)             = c0_fuels(r,fe,cbt,cbf)/ cbco2i(r,fe,cbt,cbf) ;
co2coefhhsum(fe,r)$sum((cbt,cbf), cbco2i(r,fe,cbt,cbf))  = sum((cbt,cbf), c0_fuels(r,fe,cbt,cbf)) / sum((cbt,cbf), cbco2i(r,fe,cbt,cbf));
co2realhhc(fe,ct,cf,r)$co2coefhh(fe,ct,cf,r)             = VCARTF_PFUELS.L(r,fe,ct,cf) / co2coefhh(fe,ct,cf,r) ;
co2realhhb(fe,bt,bf,r)$co2coefhh(fe,bt,bf,r)             = VHEATF_PFUELS.L(r,fe,bt,bf) / co2coefhh(fe,bt,bf,r) ;
co2realhh(fe,ct,cf,r) = co2realhhc(fe,ct,cf,r) ;
co2realhh(fe,bt,bf,r) = co2realhhb(fe,bt,bf,r) ;
*$offtext
* ###### 5.02.2016 Textmarke Comment out Diss (oben)

* ------ Wertbasierte CO2-Berechnung -------------------------------------------
*co2real(fe,r)            = co2realc(fe,r) + sum(i, co2realy(fe,i,r)) + sum(gen, co2realele(fe,gen,r));
* ###### 5.02.2016 Textmarke Comment out Diss (unten/oben)
co2real(fe,r)            = co2realc(fe,r) + sum((cbf,cbt), co2realhh(fe,cbt,cbf,r)) + sum(i, co2realy(fe,i,r)) + sum(gen, co2realele(fe,gen,r));
* ###### 5.02.2016 Textmarke Comment out Diss (oben)
co2real("total",r)       = sum(fe, co2real(fe,r)) ;
co2real(fe,"world")      = sum(r, co2real(fe,r)) ;
co2real("total","world") = sum(fe, co2real(fe,"world")) ;
*co2real(fe,r)           = co2realc(fe,r) + sum((cf,ct), co2realhhc(fe,ct,cf,r)) + sum((bf,bt), co2realhhb(fe,bt,bf,r)) + sum(i, co2realy(fe,i,r)) + sum(gen, co2realele(fe,gen,r)); // c und b getrennt

* ------ CO2 Checks ------------------------------------------------------------
*co2realc_chk(fe,r)               = round(co2realc(fe,r) - (co2em(fe,"final",r)), 7);
* ###### 5.02.2016 Textmarke Comment out Diss (unten + oben)
co2realc_chk(fe,r)               = round(co2realc(fe,r) + sum((cbf,cbt), co2realhh(fe,cbt,cbf,r)) - (co2em(fe,"final",r) + sum((cbt,cbf), cbco2i(r,fe,cbt,cbf))), 7);
* ###### 5.02.2016 Textmarke Comment out Diss (oben)

co2realy_chk(fe,i,r)$(not ele(i))= round(co2realy(fe,i,r) - co2em(fe,i,r), 7) ;
co2realele_chk(fe,gen,r)         = round(co2realele(fe,gen,r) - co2em(fe,gen,r), 7);

*co2em_totalchk(r)                = round(co2real("total",r) - sum(fe, (co2em(fe,"final",r)) +  sum(i$(not ele(i)), co2em(fe,i,r)) + sum(gen, co2em(fe,gen,r))),7);
* ###### 5.02.2016 Textmarke Comment out Diss (unten + oben)
co2em_totalchk(r)                = round(co2real("total",r) - sum(fe, (co2em(fe,"final",r) + sum((cbt,cbf), cbco2i(r,fe,cbt,cbf))) +  sum(i$(not ele(i)), co2em(fe,i,r)) + sum(gen, co2em(fe,gen,r))),7);
* ###### 5.02.2016 Textmarke Comment out Diss (oben)

*co2realc_chk(fe,r) = round(co2realc(fe,r)   - co2em(fe,"final",r), 7);    // vor EnHH-Disaggregierung
*co2realc_chk(fe,r) = round(co2realc(fe,r) + sum((cf,ct), co2realhhc(fe,ct,cf,r)) + sum((bf,bt), co2realhhb(fe,bt,bf,r)) - (co2em(fe,"final",r) + sum((cbt,cbf), cbco2i(r,fe,cbt,cbf))), 7); // c und b getrennt
*co2em_totalchk(r) = round(co2real("total",r) - sum(fe, co2em(fe,"final",r) +  sum(i$(not ele(i)), co2em(fe,i,r)) + sum(gen, co2em(fe,gen,r))),7); // vor EnHH-Disaggregierung

* ------ 14.09.2015 (Diss)-Vortrag
*co2chk(r) = co2em_totalchk(r) / sum(fe, (co2em(fe,"final",r)) +  sum(i$(not ele(i)), co2em(fe,i,r)) + sum(gen, co2em(fe,gen,r))) * 100;
*co2chk("world") = sum(r, co2em_totalchk(r)) / sum(r, sum(fe, (co2em(fe,"final",r)) +  sum(i$(not ele(i)), co2em(fe,i,r)) + sum(gen, co2em(fe,gen,r)))) * 100;
* ###### 5.02.2016 Textmarke Comment out Diss (unten + oben)
co2chk(r) = co2em_totalchk(r)/sum(fe, (co2em(fe,"final",r) + sum((cbt,cbf), cbco2i(r,fe,cbt,cbf))) +  sum(i$(not ele(i)), co2em(fe,i,r)) + sum(gen, co2em(fe,gen,r))) * 100;
co2chk("world") = sum(r, co2em_totalchk(r))/sum(r, sum(fe, (co2em(fe,"final",r) + sum((cbt,cbf), cbco2i(r,fe,cbt,cbf))) +  sum(i$(not ele(i)), co2em(fe,i,r)) + sum(gen, co2em(fe,gen,r)))) * 100;
* ###### 5.02.2016 Textmarke Comment out Diss (oben)


display co2coefy, co2coefele, co2coefc, co2realc, co2realy, co2realele, co2real, co2realc_chk, co2realy_chk, co2realele_chk, co2em_totalchk;
display co2coefhh, co2realhhb, co2coefhhsum, co2realhhc, co2realhh; // * ###### 5.02.2016 Textmarke Comment out Diss (links)
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
zpf_a(i,r) = round(sum(s, vtwr("trn",i,s,r)) - VA_PT.L(i,r), 7);                        // geht
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
* ------ 18.06.2014 Yearly AEEI
         aeei_yr(r,i,g,yr)       Yearly AEEI
         aeei_elexyr(r,gen,yr)   Yearly AEEI for ELE extant technologies
         aeei_elenyr(r,gen,yr)   Yearly AEEI for ELE new technologies
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
         pco2w_yr(yr)
         pco2_yr(r,yr)

         VY_PY_yr(*,i,yr)
         VY_PGEN_yr(*,gen,yr)
* ------ 18.05.2015 ersetze VY_PA(j,i,r) durch VY_PAY(j,i,r)
         VY_PA_yr(*,j,i,yr)
*         VY_PAY_yr(*,j,i,yr)

         VINV_PINV_yr(*,yr)
         VYT_PT_yr(yr)
         VA_PA_yr(*,i,yr)
         VC_PC_yr(*,yr)
         VELEx_PGEN_yr
         VELEn_PGEN_yr

         VD_PINV_yr
         VD_PC_yr
         VINV_PA_yr

         Y_yr(*,i,yr)
         INV_yr(*,yr)
         YT_yr(yr)
         A_yr(*,i,yr)
         C_yr(*,yr)
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
         gdpreal_yr(*,yr)
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
* ###### 5.02.2016 Textmarke Comment out Diss (unten)
         co2hh_yr(*,cbt,cbf,r,yr)
         co2hhc_yr(*,r,yr)
         co2hhb_yr(*,r,yr)
         co2hhsum_yr
* ###### 5.02.2016 Textmarke Comment out Diss (oben)
         co2w_yr(*,*,yr)         CO2 emissions in worldtrade regime based on MPSGE report variables
         co2nets_yr(*,*,yr)      CO2 emissions in EU28 in non-ets regime based on MPSGE report variables
         co2nets_yr1(*,*,*,yr)   CO2 emissions in EU28 in non-ets regime based on co2em...
         carblim_yr(*,yr)
         carblim_ets_yr(*,yr)
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


* ###### 5.02.2016 Textmarke Comment out Diss (unten)
*$ontext
* ------ 13.09.2015 (Diss) Write all report variables in a dynamic (yearly) parameter
         vc_esd_yr(r,yr)
         vesd_o_yr(r,yr)
         vesd_cart_yr(r,ct,yr)
         vesd_trn_yr(i,r,yr)
         vesd_heat_yr(r,bt,yr)
         vheat_o_yr(r,bt,yr)
         vheat_f_yr(r,bt,bf,yr)
         vcart_o_yr(r,ct,yr)
         vcart_f_yr(r,ct,cf,yr)
         vheatf_o_yr(r,bt,bf,yr)
         vheatf_pa_yr(r,i,bt,bf,yr)
         vheatf_pbio_yr(r,bt,bf,yr)
         vheatf_pfuels_yr(r,i,bt,bf,yr)
         vheatf_stock_yr(r,bt,bf,yr)
         vheatf_pco2nets_yr(r,bt,bf,yr)
         vcartf_o_yr(r,ct,cf,yr)
         vcartf_pa_yr(r,i,ct,cf,yr)
         vcartf_pfuels_yr(r,i,ct,cf,yr)
         vcartf_stock_yr(r,ct,cf,yr)
         vcartf_pco2nets_yr(r,ct,cf,yr)
         vnc_newcar_yr(r,ct,cf,yr)
         vnc_pa_yr(r,i,ct,cf,yr)
         vnc_peff_i_yr(r,ct,cf,yr)
         vnc_peff_o_yr(r,ct,cf,yr)
         vnh_newhouse_yr(r,bt,bf,yr)
         vnh_pa_yr(r,i,bt,bf,yr)
         vnh_peff_i_yr(r,bt,bf,yr)
         vnh_peff_o_yr(r,bt,bf,yr)
         vstock_co_yr(r,ct,cf,yr)
         vstock_cnc_yr(r,ct,cf,yr)
         vstock_cxc_yr(r,ct,cf,yr)
         vstock_ho_yr(r,bt,bf,yr)
         vstock_hnh_yr(r,bt,bf,yr)
         vstock_hxh_yr(r,bt,bf,yr)
         vstock_hnewh_yr(r,bt,bf,yr)
         vstock_hpeffi_yr(r,bt,bf,yr)
         vstock_hpeffo_yr(r,bt,bf,yr)
         voils_o_yr(r,cbt,cbf,yr)
         voils_pa_yr(r,i,cbt,cbf,yr)
         vb_bio_yr(r,cbt,cbf,yr)
         vb_pa_yr(r,i,cbt,cbf,yr)

         anz_car(r,*,*,*,yr)
         anz_car_rel(r,*,*,*,yr)
         anz_car_newrel(r,yr)
         anz_qm(r,*,*,yr)
         anz_qm_new(r,*,*,yr)
         anz_qm_newrel(r,yr)
         anz_qm_newsum(r,*,yr)
         anz_qm_rel(r,*,*,yr)
         sanrate(r,yr)
         newcar_yr
         houseqm_newrel(r,yr)

         ssdrep_yr
         bsdrep_yr
         bsdnh_yr
         sy0rep_yr
         by0rep_yr
         cars2_yr

         eff_reso_yr
         eff_resi_yr
         effh_reso_yr
         effh_resi_yr

* ------ 29.03.2016
         avgkwh_yr(r,yr)

* ------ 1.04.2016
         pesd_yr(r,yr)
         pheat_yr(r,bt,yr)
         pcart_yr(r,ct,yr)
         pheatf_yr(r,bt,bf,yr)
         pcartf_yr(r,ct,cf,yr)
         pfuels_yr(r,i,cbt,cbf,yr)
         poils_yr(r,cbt,cbf,yr)
         pbio_yr(r,cbt,cbf,yr)
         pnewcar_yr(r,ct,cf,yr)
         pnewhouse_yr(r,bt,bf,yr)
         pstock_c_yr(r,ct,cf,yr)
         pstock_h_yr(r,bt,bf,yr)
         pheatagg_yr(r,yr)
         pxtcar_yr(r,ct,cf,yr)
         pxthouse_yr(r,bt,bf,yr)
         pnewh_yr(r,bt,bf,yr)
         peff_yr(r,yr)
         peffh_yr(r,yr)


*$offtext
* ###### 5.02.2016 Textmarke Comment out Diss (oben)


;

* ______________________________________________________________________________
* ------ Parameter Declarations ------------------------------------------------

* ------ 18.06.2014 Yearly AEEI
aeei_yr(r,i,g,yr) = 1;
aeei_elexyr(r,gen,yr) = 1;
aeei_elenyr(r,gen,yr) = 1;
display aeei_yr, aeei_elexyr, aeei_elenyr;

* ------ 18.06.2014 Wirkungsgrade
ele_eff(r) = 0 ;         //  Schalter auf 1 bedeutet delta_wg=0 also einfache AEEI-verbesserung in Strom i.H. v. 0.1% p.a. [ORIGINAL]
*ele_eff(r) = 1 ;         //  Schalter auf 1 bedeutet delta_wg=0 also einfache AEEI-verbesserung in Strom i.H. v. 0.1% p.a.
* ------ 14.01.2015 Tectmarke RWE --Y Schalter auf 1, weil sonst aeei_elenyr negativ für USA, OEC, OPE...

* ------ Store benchmark electricity generation activity into a parameter
ELEn0(gen,r) = ELEn.L(gen,r);
ELEx0(gen,r) = ELEx.L(gen,r);
display ELEn0, ELEx0;

$ontext
* ------ CO2 regimes -----------------------------------------------------------

* ------ 9.02.2016 CO2-Pfad für ETStrade [Quelle: CO2-Pfad EU-ETS.xlsx in D:\GAMS\GTAP8inGAMS\Rutherford - EnHH - 11\xcel_data]
co2pfad_ets(r,yr)$(eu28(r) and etstrade) = 0 ;
co2pfad_ets(r,"2007")$(eu28(r) and etstrade) = 1.000 ;
co2pfad_ets(r,"2010")$(eu28(r) and etstrade) = 0.887 ;
co2pfad_ets(r,"2015")$(eu28(r) and etstrade) = 0.825 ;
co2pfad_ets(r,"2020")$(eu28(r) and etstrade) = 0.730 ;
co2pfad_ets(r,"2025")$(eu28(r) and etstrade) = 0.615 ;
co2pfad_ets(r,"2030")$(eu28(r) and etstrade) = 0.500 ;  // = -50.0 % ggü. 2007 = -43.1 % ggü. 2005 (und -43.7 % ggü. 2010)

* ------ 9.02.2016 CO2-Pfad für EUtrade [Quelle: CO2-Pfad EU-ETS.xlsx in D:\GAMS\GTAP8inGAMS\Rutherford - EnHH - 11\xcel_data]
co2pfad_ets(r,yr)$(eu28(r) and eutrade or notrade) = 0 ;
co2pfad_ets(r,"2007")$(eu28(r) and (eutrade or notrade)) = 1.000 ;
co2pfad_ets(r,"2010")$(eu28(r) and (eutrade or notrade)) = 0.928 ;  // 0.928 ist nur in EUN und EUE bindend
co2pfad_ets(r,"2015")$(eu28(r) and (eutrade or notrade)) = 0.850 ;
co2pfad_ets(r,"2020")$(eu28(r) and (eutrade or notrade)) = 0.786 ;
co2pfad_ets(r,"2025")$(eu28(r) and (eutrade or notrade)) = 0.723 ;
co2pfad_ets(r,"2030")$(eu28(r) and (eutrade or notrade)) = 0.659 ;  // = -34.1 % ggü. 2007 = -40.0 % ggü. 1990 (und -26.9 % ggü. 2010)
display co2pfad_ets;
$offtext

*$EXIT
*ExitLOOP
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ---------------------------------------------------------------------------- *
*        L O O P
* ---------------------------------------------------------------------------- *

*LOOP (yr,                                                                        // # LOOP-start #
*LOOP (yrx(yr),                                                                  // 10-year-milestones Berechnungen
*LOOP (before2010(yr),
*LOOP (before2015(yr),
*LOOP (before2020(yr),
*LOOP (before2025(yr),
*LOOP (before2030(yr),
LOOP (before2035(yr),
*LOOP (before2040(yr),
*LOOP (before2050(yr),
* Textmarke LOOP

* ------ BEFORE-SOLVE ----------------------------------------------------------

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ PATHs -----------------------------------------------------------------
* --->>> RECURSIVE Labor DYNAMICS (growth / endowment driver) ------------------ // # LABOR dynamics #
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

* ------ 05.06.2014 Change LABOR Force according to
* ------ "size_usk" + "size_skl" multiplied with evoa0
evoa("skl",r)  =  evoa0("skl",r) * size_skl(r,yr) ;
evoa("usk",r)  =  evoa0("usk",r) * size_usk(r,yr) ;
* ------ Save evoa in yearly parameter
evoa_yr(r,f,yr) = evoa(f,r);

* --->>> Electricity Generation Capital Dynamics ------------------------------- // # ELE capital dynamics #
* ------ Übergabe des bestehenden kapitalstocks
* ------ Depreciate extant ELE CAPITAL holdings according to "ABSCHREIBUNG" [ORIGINAL]:
* ------ the respective entries for 2007 should be neutral (1 or 0, respectively)
* ------ ORIGINAL
*ks_x(gen,r)$(out_gen(gen,r)$ks_x(gen,r))         = ks_x(gen,r) * (1 - abschreibung(r,gen,yr)) ;
*ks_x(gen,r)$(out_gen(gen,r)$(ks_x(gen,r) LE 0))  = 1e-7 ;
* ------ 14.10.2014 --> 3.11.2014
ks_x(gen,r)$(out_gen(gen,r)$ks_x(gen,r))         = ks_x0(gen,r) * abschr_pfad(r,gen,yr) ;
ks_x(gen,r)$(out_gen(gen,r)$(ks_x(gen,r) LE 0))  = 1e-7 ;

* ------ Zur Speicherung an parameter "ks_x_yr":
ks_x_yr(r,gen,yr)   = ks_x(gen,r) ;


* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* --->>> AEEI + Wirkungsgrad ---------------------------------------------------  // # AEEI #
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

* ------ 16.07.2014 SET energy productivity increase from BASELINE-Database as AEEI
aeei_yr(r,e,g,yr) = ep(yr,r) ;
* ------ 3.02.2016 BAW entfernen
aeei_yr(r,e,g,yr)$(deu(r) or fra(r) or aut(r) or eus(r))      = 0.9 * ep(yr,r) ;
aeei_yr(r,e,g,"2007")$(deu(r) or fra(r) or aut(r) or eus(r))  = ep("2007",r) ;

* ------ 18.06.2014 Assume aeei_fct = 0.1 % efficiency growth for e(i) per annum
*aeei_yr(r,e,g,yr)$yr2007(yr)     = 1;
*aeei_yr(r,e,g,yr)$yr2010(yr)     = aeei_yr(r,e,g,yr-1) * (1 - aeei_fct/100)**3;
*aeei_yr(r,e,g,yr)$after2010(yr)  = aeei_yr(r,e,g,yr-1) * (1 - aeei_fct/100)**5;

* ------ 11.05.2015 Increase AEEI in Germany, France, Austria and southern EU (very low in baseline database: >0.95 in 2050)
*aeei_yr(r,e,g,yr)$(bawdeu(r) or fra(r) or aut(r) or eus(r))      = 0.9 * ep(yr,r) ;
*aeei_yr(r,e,g,"2007")$(bawdeu(r) or fra(r) or aut(r) or eus(r))  = ep("2007",r) ;

* ------ 18.06.2014 Set aeei_elexyr(r,gen,yr) and aeei_elenyr(r,gen,yr) equal to aeei_yr(r,"ele","c",yr) ["ele" and "c": arbitrary]
aeei_elexyr(r,gen,yr) = aeei_yr(r,"ele","c",yr);
aeei_elenyr(r,gen,yr) = aeei_yr(r,"ele","c",yr);

* ###### 5.02.2016 Textmarke Comment out Diss (unten)
* ------ 13.01.2016 AEEI für HH-Energie und Standards (CO2 bei Fahrzeugen und Energie bei Gebäuden) aktualisieren
aeei_cb(r,i,cbt,cbf) = aeei_cb_yr(r,i,cbt,cbf,yr) ;
aeei_co2(r,cbt,cbf)  = aeei_co2_yr(r,cbt,cbf,yr) ;
* ###### 5.02.2016 Textmarke Comment out Diss (oben)

* ------ 18.06.2014 Report yearly ELE output
velex_pgenyr(r,gen,yr) = VELEx_PGEN.L(gen,r)   ;
velen_pgenyr(r,gen,yr) = VELEn_PGEN.L(gen,r)   ;

* ------ 18.06.2014 Calculate yearly ELE annex
zubau_ele(r,gen,yr)$(velen_pgenyr(r,gen,yr) and after(yr)) = velen_pgenyr(r,gen,yr) - velen_pgenyr(r,gen,yr-1);

* ------ 18.06.2014 Calculate yearly efficiency factors
gew_wg(r,gen,"2007") = wg0(gen,r);
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
aeei_eleNyr(r,gen,yr)$yr2007(yr)         = aeei_eleXyr(r,gen,yr);
aeei_eleNyr(r,gen,yr)$yr2010(yr)         = aeei_eleNyr(r,gen,yr-1) * (1 - aeei_fct/100)**3 ;
aeei_eleNyr(r,gen,yr)$after2010(yr)      = aeei_eleNyr(r,gen,yr-1) * (1 - aeei_fct/100)**5
         - (DELTA_WG_BEFORE(r,gen,yr-1) * (aeei_eleNyr(r,gen,yr-1) * (1 - aeei_fct/100)**5));
aeei_eleNyr(r,gen,yr)$ele_eff(r)         = aeei_eleXyr(r,gen,yr);        // Schalter auf 1 bedeutet delta_wg=0 also einfache AEEI-verbesserung in Strom i.H. v. 0.1% p.a.

* ------ 14.01.2015 Bei worldtrade wird aeei_eleNyr für manche Regionen und ELEn-Technologien negativ: in diesem Falle = 0.1 setzen!
*aeei_eleNyr(r,gen,yr)$(aeei_eleNyr(r,gen,yr) lt 0.1) = 0.1;
aeei_eleNyr(r,gen,yr)$(aeei_eleNyr(r,gen,yr) lt 0.001)   = 0.01;
aeei_eleNyr(r,gen,yr)$(aeei_eleNyr(r,gen,yr) gt 1)       = 1;
* ------ 14.01.2015 Textmarke RWE

* --->>> 18.06.2014 Hand-over model specific paramaters for each period <<< ----
aeei(i,g,r)      = aeei_yr(r,i,g,yr) ;
aeei_elex(gen,r) = aeei_elexyr(r,gen,yr) ;
aeei_elen(gen,r) = aeei_elenyr(r,gen,yr) ;


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

*worldtrade  = 0;
*eutrade     = 0;
*etstrade    = 1;
*netstrade   = 0;
*worldtrade2 = 0;

pco2w_r(r) = no;
notrad(r) = no;

* --->>> 6.06.2014 Implement CO2 pathways -------------------------------------- // # CO2 dynamics #
* http://www.eea.europa.eu/data-and-maps/figures/perspective-on-eu-ets-cap
* http://www.emissions-euets.com/linear-reduction-factor-lrf
$ontext
carblim_ets(r)$(eu28(r) and ord(yr) < 3) = carblim_ets0(r);
carblim_ets(r)$(eu28(r) and ord(yr) = 3) = carblim_ets(r) - 2 * 0.0174 * carblim_ets0(r);
carblim_ets(r)$(eu28(r) and ord(yr) > 3) = carblim_ets(r) - 5 * 0.0174 * carblim_ets0(r);

co2par(r,yr)$(eu28(r) and ord(yr) < 3)   = carblim_ets(r);
co2par(r,yr)$(eu28(r) and ord(yr) = 3)   = carblim_ets(r) - 2 * 0.0174 * carblim_ets0(r);
co2par(r,yr)$(eu28(r) and ord(yr) > 3)   = carblim_ets(r) - 5 * 0.0174 * carblim_ets0(r);
display co2par;
$offtext


* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* --->>> PYtarget --------------------------------------------------------------  // # Price dynamics #
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ 5.01.2016 R_SUPPLY ausschalten
pytarget("cru",r)$pricetarget("cru",r) = pytarget_yr(r,"cru",yr);


* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* --->>> diffcost / diffrebate / REBATE_DIFF
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ 09.07.2014 Remove e:pa... line in order to give room for rebate_diff to rebate diffcosts to ele price; Set diffrebate(bawdeu) = 1
* ------ NOT in 2007 --> $after(yr)
epa_on$after(yr)                 = 0;
*diffrebate(bawdeu)$after(yr)     = 1;
* ------ 3.02.2016 BAW auskommentieren
diffrebate(deu)$after(yr)        = 1;


* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* --->>> ELEn.UP / Y.LO / A.LO --- Bounds for electricity generation
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

*$ontext
* ------ 5.05.2015 Overall ELE limitation for upper generation bounds
ELEn.UP(gen,r)$after2010(yr) = 5 * ELEn.L(gen,r) + 10 * (1-thetax) ;
ELEn.UP(gen,r)$after2040(yr) = 2 * ELEn.L(gen,r) + 2  * (1-thetax) ;

* ------ NUC
ELEn.UP(gen,r)$(bnuc(gen)            and after(yr))      = 1.0 * gen_limit_yr(r,gen,yr);
ELEn.UP(gen,r)$(bnuc(gen)            and after2015(yr))  = 0.8 * gen_limit_yr(r,gen,yr);
ELEn.UP(gen,r)$(bnuc(gen) and fra(r) and after(yr))      = 0.5 * gen_limit_yr(r,gen,yr);
ELEn.UP(gen,r)$(bnuc(gen) and fra(r) and    yr2030(yr))  = 0.6 * gen_limit_yr(r,gen,yr);
ELEn.UP(gen,r)$(bnuc(gen) and fra(r) and after2030(yr))  = 0.5 * gen_limit_yr(r,gen,yr);
ELEn.UP(gen,r)$(bnuc(gen) and fra(r) and after2030(yr))  = 0.9 * ELEn.L(gen,r);

* ------ COAL, GAS and OIL
ELEn.UP(gen,r)$(moil(gen)            and after2010(yr))  = 2.0 * ELEn.L(gen,r) + (1-thetax);
ELEn.UP(gen,r)$(bGAS(gen) and eu28(r) and after2010(yr)) = 2 - ELEx.L(gen,r);
ELEn.UP(gen,r)$(bGAS(gen) and deu(r) and after2010(yr))  = 1.5 - ELEx.L(gen,r);
ELEn.UP(gen,r)$(bgas(gen) and arb(r) and after2035(yr))  = 1.2 * ELEn.L(gen,r) + (1-thetax);
ELEn.UP(gen,r)$(mgas(gen) and swz(r) and after2030(yr))  = 1.5 * ELEn.L(gen,r) ;
ELEn.UP(gen,r)$(bbc(gen)  and eun(r) and after2030(yr))  = ELEn.L(gen,r) + (1-thetax);

* ------ 10.12.2014 Hydro, Bio and Geo
ELEn.UP(gen,r)$(bhydro(gen)          and after(yr))      = 0.75 * gen_limit_yr(r,gen,yr);
ELEn.UP(gen,r)$(bhydro(gen)          and after2030(yr))  = 1.10 * ELEn.L(gen,r);
ELEn.UP(gen,r)$(bbio(gen)            and after(yr))      = 0.60 * gen_limit_yr(r,gen,yr);
ELEn.UP(gen,r)$(bbio(gen) and deu(r) and after(yr))      = 0.40 * gen_limit_yr(r,gen,yr);
ELEn.UP(gen,r)$(bbio(gen) and eue(r) and after2015(yr))  = 1.30 * ELEn.L(gen,r) ;
ELEn.UP(gen,r)$(bgeo(gen)            and after2015(yr))  = 1.25 * ELEn.L(gen,r);

* ------ mWIND and mSOLAR
ELEn.UP(gen,r)$(mwind(gen)  and deu(r) and after2010(yr))= 1.5 * ELEn.L(gen,r) ;
ELEn.UP(gen,r)$(mwind(gen)  and eus(r) and after2010(yr))= 1.5 * ELEn.L(gen,r) ;
ELEn.UP(gen,r)$(mwind(gen)  and aut(r) and after2020(yr))= 1.4 * ELEn.L(gen,r) ;
ELEn.UP(gen,r)$(mSolar(gen) and eus(r) and after2030(yr))= 1.5 * ELEn.L(gen,r) ;
ELEn.UP(gen,r)$(mWind(gen)  and eun(r) and after2020(yr))= 1.3 * ELEn.L(gen,r) ;
ELEn.UP(gen,r)$(mWind(gen)  and fra(r) and after2020(yr))= 1.5 * ELEn.L(gen,r) ;
ELEn.UP(gen,r)$(mSolar(gen) and fra(r) and after2020(yr))= 1.6 * ELEn.L(gen,r) ;
ELEn.UP(gen,r)$(mWind(gen)  and rus(r) and after2030(yr))= 1.5 * ELEn.L(gen,r) + (1-thetax);
ELEn.UP(gen,r)$(mSolar(gen) and arb(r) and after2030(yr))= 1.5 * ELEn.L(gen,r) + (1-thetax);
ELEn.UP(gen,r)$(mwind(gen)  and arb(r) and after2030(yr))= 1.5 * ELEn.L(gen,r) + (1-thetax);
ELEn.UP(gen,r)$(msolar(gen) and deu(r) and after2015(yr))= 1.5 * ELEn.L(gen,r) ;

* ------ 16.02.2015 CCS Limitations
ELEn.UP(gen,r)$(fosccs(gen)              and before2030(yr))     = ELEn.L(gen,r) ;
ELEn.UP(gen,r)$(mccs(gen)                and after2025(yr))      = ELEn.L(gen,r) + 750 * (1-thetax);
ELEn.UP(gen,r)$(bccs(gen)                and after2025(yr))      = ELEn.L(gen,r) + 750 * (1-thetax);

*------ 10.12.2014 RWE Szenario für bBC-Zubau-Beschränkung
ELEn.UP(gen,r)$(bbc(gen) and deu(r))     = 1.0 * bbc_deu_elen(gen,yr) * (1-thetax);  // REF
*ELEn.UP(gen,r)$(bbc(gen) and deu(r)      and after2010(yr)) = 0 ;                 // Scen1
*ELEn.UP(gen,r)$(bccs(gen) and deu(r)     and after2010(yr)) = 0 ;
* ------ TEXTMARKE RWE

* ------ 26.02.2015
ELEx.FX(gen,r)$(ELEx.L(gen,r) le 0) = 0;
* ------ Textmarke RWE

* ------ 5.05.2015
Y.LO(i,r)$(ele(i)        and deu(r)   and yr2015(yr)) = 0.98 * Y.L(i,r) ;
Y.LO(i,r)$(ele(i)        and deu(r)   and yr2020(yr)) = 1.05 * Y.L(i,r) ;
Y.LO(i,r)$(ele(i)        and deu(r)   and yr2025(yr)) = 0.98 * Y.L(i,r) ;
Y.LO(i,r)$(ele(i)        and deu(r)   and yr2030(yr)) = 0.90 * Y.L(i,r) ;
Y.LO(i,r)$(ele(i)        and deu(r)   and yr2025(yr)) = 1.00 * Y.L(i,r) ;
Y.LO(i,r)$(ele(i)        and deu(r)   and yr2030(yr)) = 0.98 * Y.L(i,r) ;

* ------ 9.03.2015 Set lower bound for overall ELE production --> modelled as endogenous subsidy
*ylo_on(i,r)$(ele(i) and bawdeu(r) and after(yr)) = 0;
*ylo_on(i,r)$(ele(i) and bawdeu(r) and after2035(yr)) = 0;
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

*eletwhyr("NEW.UP","BRD",gen,yr)    = sum(r$bawdeu(r),eletwhyr("NEW.UP",r,gen,yr)) ;
*eletwhyr("NEW.LO","BRD",gen,yr)    = sum(r$bawdeu(r),eletwhyr("NEW.LO",r,gen,yr)) ;
*eletwhyr("NEW.UP","BRD","total",yr)=sum(gen,      eletwhyr("NEW.UP","BRD",gen,yr))   ;
*eletwhyr("NEW.LO","BRD","total",yr)=sum(gen,      eletwhyr("NEW.LO","BRD",gen,yr))   ;
*eletwhyr("X.UP","BRD",gen,yr)    = sum(r$bawdeu(r),eletwhyr("X.UP",r,gen,yr)) ;
*eletwhyr("X.UP","BRD","total",yr)=sum(gen,      eletwhyr("X.UP","BRD",gen,yr))   ;

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ SCENARIOS (Diss)
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

* ------ Carbon Regimes for ETS sectors ----------------------------------------
* ------ 02.07.2014 Set co2pfad_ets for BAW equal to DEU
*co2pfad_ets("baw",yr)            = co2pfad_ets("deu",yr);
* ------ 26.01.2016
*co2pfad_ets_eu("EU28",yr)$yr2010(yr) = 0.98;
*co2pfad_ets_eu("EU28",yr)$yr2015(yr) = 0.95;
*co2pfad_ets_eu("EU28",yr)$yr2020(yr) = 0.90;

* ------ 9.02.2016 CO2-Pfad für ETStrade [Quelle: CO2-Pfad EU-ETS.xlsx in D:\GAMS\GTAP8inGAMS\Rutherford - EnHH - 11\xcel_data]
* ------ 13.04.2016
co2pfad_ets(r,yr)$(eu28(r) and etstrade) = co2pfad_ets_eu("EU28",yr);
co2pfad_ets(r,yr)$(eu28(r) and eutrade)  = co2pfad_ets_eu("EU28",yr);
* ------ 13.04.2016 auskommentiert, da EU-Pfad über 160330_co2pfad.xlsx [EU-Pad!B69:L70] eingelesen wird
*loop(yryr$yr2007(yr), co2pfad_ets(r,yryr)$(eu28(r) and etstrade) = 0;) ;
*co2pfad_ets(r,yr)$(eu28(r) and etstrade  and yr2007(yr)) = 1.000 ;
*co2pfad_ets(r,yr)$(eu28(r) and etstrade  and yr2010(yr)) = 1.000 ;
*co2pfad_ets(r,yr)$(eu28(r) and etstrade  and yr2015(yr)) = 0.950 ;
*co2pfad_ets(r,yr)$(eu28(r) and etstrade  and yr2020(yr)) = 0.900 ;
*co2pfad_ets(r,yr)$(eu28(r) and etstrade  and yr2025(yr)) = 0.733 ;   // wie in 150730_co2pfad_BMWi.xlsx (EU-Pfad)
*co2pfad_ets(r,yr)$(eu28(r) and etstrade  and yr2030(yr)) = 0.576 ;   // wie in 150730_co2pfad_BMWi.xlsx (EU-Pfad)

$ontext
* ------ 15.02.2016 CO2-Pfad für HHETS: Vary co2pfad_ets in order to achieve same CO2 level as with Standards here (std)
co2pfad_ets(r,yr)$(deu(r)  and hhets     and yr2007(yr)) = 1.000 ;  // einkommentieren, wenn CO2-Level angepasst werden soll!
co2pfad_ets(r,yr)$(deu(r)  and hhets     and yr2010(yr)) = 0.740 ;  // einkommentieren, wenn CO2-Level angepasst werden soll!
co2pfad_ets(r,yr)$(deu(r)  and hhets     and yr2015(yr)) = 0.820 ;  // einkommentieren, wenn CO2-Level angepasst werden soll!
co2pfad_ets(r,yr)$(deu(r)  and hhets     and yr2020(yr)) = 0.680 ;  // einkommentieren, wenn CO2-Level angepasst werden soll!
co2pfad_ets(r,yr)$(deu(r)  and hhets     and yr2025(yr)) = 0.580 ;  // einkommentieren, wenn CO2-Level angepasst werden soll!
co2pfad_ets(r,yr)$(deu(r)  and hhets     and yr2030(yr)) = 0.495 ;  // einkommentieren, wenn CO2-Level angepasst werden soll!
$offtext

* ------ 9.02.2016 CO2-Pfad für EUtrade [Quelle: CO2-Pfad EU-ETS.xlsx in D:\GAMS\GTAP8inGAMS\Rutherford - EnHH - 11\xcel_data]
*loop(yryr$yr2007(yr), co2pfad_ets(r,yryr)$(eu28(r) and (eutrade or notrade)) = 0;) ;
* ------ 17.04.2016 EUTRADE-Pfad anpassen an 'CO2_yr.p' in report_pivot_dias.xlsx des Referenzszenarios NETSTRADE_R!
$ontext
co2pfad_ets(r,yr)$(eu28(r) and (eutrade or notrade) and yr2007(yr)) = 1.000 * co2pfad_ets(r,yr) ;
co2pfad_ets(r,yr)$(eu28(r) and (eutrade or notrade) and yr2010(yr)) = 1.002 * co2pfad_ets(r,yr) ;
co2pfad_ets(r,yr)$(eu28(r) and (eutrade or notrade) and yr2015(yr)) = 1.034 * co2pfad_ets(r,yr) ;
co2pfad_ets(r,yr)$(eu28(r) and (eutrade or notrade) and yr2020(yr)) = 1.047 * co2pfad_ets(r,yr) ;
co2pfad_ets(r,yr)$(eu28(r) and (eutrade or notrade) and yr2025(yr)) = 1.072 * co2pfad_ets(r,yr) ;
co2pfad_ets(r,yr)$(eu28(r) and (eutrade or notrade) and yr2030(yr)) = 1.095 * co2pfad_ets(r,yr) ;   // EU-Pfad gemäß netstrade_r-Ergebnis aus 'CO2_yr.p' in report_pivot_dias.xlsx [ETS -43 % und non-ETS -30 % jeweils ggü. 2005; Gesamt -36 % ggü. 2007 entspricht ungefähr -42 % ggü. 1990]
***co2pfad_ets(r,yr)$(eu28(r) and (eutrade or notrade) and yr2030(yr)) = 1.00 * co2pfad_ets(r,yr) ;  // = -34.1 % ggü. 2007 = -40.0 % ggü. 1990 (und -26.9 % ggü. 2010)
$offtext
* ------ 22.04.2016 EUTRADE-Pfad anpassen an 'CO2_yr.p' in report_pivot_dias.xlsx des Referenzszenarios NETSTRADE_R!
co2pfad_ets(r,yr)$(eu28(r) and (eutrade or notrade) and yr2007(yr)) = 1.000  ;      // ohne Diss
co2pfad_ets(r,yr)$(eu28(r) and (eutrade or notrade) and yr2010(yr)) = 0.905635 ;    // ohne Diss
co2pfad_ets(r,yr)$(eu28(r) and (eutrade or notrade) and yr2015(yr)) = 0.878312 ;    // ohne Diss
co2pfad_ets(r,yr)$(eu28(r) and (eutrade or notrade) and yr2020(yr)) = 0.837089 ;    // ohne Diss
co2pfad_ets(r,yr)$(eu28(r) and (eutrade or notrade) and yr2025(yr)) = 0.737676 ;    // ohne Diss
co2pfad_ets(r,yr)$(eu28(r) and (eutrade or notrade) and yr2030(yr)) = 0.631778 ;    // ohne Diss

* ###### 22.04.2016 Textmarke Comment out Diss (unten)
*$ontext
co2pfad_ets(r,yr)$(eu28(r) and (eutrade or notrade) and yr2007(yr)) = 1.000  ;       // mit Diss
co2pfad_ets(r,yr)$(eu28(r) and (eutrade or notrade) and yr2010(yr)) = 0.909218 ;     // mit Diss
co2pfad_ets(r,yr)$(eu28(r) and (eutrade or notrade) and yr2015(yr)) = 0.878756 ;     // mit Diss
co2pfad_ets(r,yr)$(eu28(r) and (eutrade or notrade) and yr2020(yr)) = 0.837546 ;     // mit Diss
co2pfad_ets(r,yr)$(eu28(r) and (eutrade or notrade) and yr2025(yr)) = 0.737879 ;     // mit Diss
co2pfad_ets(r,yr)$(eu28(r) and (eutrade or notrade) and yr2030(yr)) = 0.631868 ;     // mit Diss
*$offtext
* ###### 22.04.2016 Textmarke Comment out Diss (oben)




* ------ 22.07.2014 Set carblim_ets equal to co2pfad_ets
carblim_ets(r)$(eu28(r) and etstrade)    = carblim_ets0(r) * co2pfad_ets(r,yr);  // if etstrade = 1
* ------ 15.02.2016 et carblim_ets equal to co2pfad_ets
carblim_ets(r)$(eu28(r) and hhets)       = carblim_ets0(r) * co2pfad_ets(r,yr);  // if hhets = 1
* ------ 2.02.2016 EU-weites, sektorübergreifendes ETS
carblim(r)$(eu28(r) and eutrade)         = carblim0(r) * co2pfad_ets(r,yr) ;     // if eutrade = 1
* ------ 2.02.2016 Regionale, sektorübergreifende CO2-Reduktionspflicht ohne ETS
notrad(r)$(eu28(r) and notrade)          = yes ;                                 // if notrade = 1
carblim(r)$(eu28(r) and notrad(r))       = carblim0(r) * co2pfad_ets(r,yr) ;     // if notrade = 1

* ------ Carbon Regimes for ETS + Non-ETS sectors ------------------------------
* ------ 12.02.2016 Non-ETS restrictions with sectoral trade only (netstrade_r)
*co2pfad_nets(r,yr)$(eu28(r) and netstrade_r and yr2007(yr)) = 1.000 ;
*co2pfad_nets(r,yr)$(eu28(r) and netstrade_r and yr2010(yr)) = 1.000 ;
***co2pfad_nets(r,yr)$(eu28(r) and netstrade_r and yr2015(yr)) = 0.418 ;
***co2pfad_nets(r,yr)$(eu28(r) and netstrade_r and yr2020(yr)) = 0.306 ;
***co2pfad_nets(r,yr)$(eu28(r) and netstrade_r and yr2025(yr)) = 0.3342 ;
***co2pfad_nets(r,yr)$(eu28(r) and netstrade_r and yr2030(yr)) = 0.3568 ;
*co2pfad_nets(r,yr)$(eu28(r) and netstrade_r and yr2015(yr)) = 0.4160 ;    //eutrade 160216
*co2pfad_nets(r,yr)$(eu28(r) and netstrade_r an d yr2020(yr)) = 0.2965 ;    //eutrade 160216
*co2pfad_nets(r,yr)$(eu28(r) and netstrade_r and yr2025(yr)) = 0.3255 ;    //eutrade 160216
*co2pfad_nets(r,yr)$(eu28(r) and netstrade_r and yr2030(yr)) = 0.3495 ;    //eutrade 160216

* ------ 18.02.2016 nonets_yr aus CO2-Pfad_EU-ETS_v4.xlsx eingelesen
co2pfad_nets(r,yr)$(eu28(r) and netstrade_r)= nonets_yr(r,yr) ;                   display co2pfad_nets;
*loop(yryr$after(yryr), nonetsx(r,yryr)$(not eue(r))  = nonetsx(r,yryr-1)  * (1+0.002)**5 ;) ; display nonetsx;
co2pfad_nets(r,yr)$(eu28(r) and netstrade_r)= co2pfad_nets(r,yr) * nonetsx(r,yr); display co2pfad_nets;

* ------ 12.02.2016 Non-ETS restrictions with reagional and sectoral trade within the EU28 (netstrade)
co2pfad_nets(r,yr)$(eu28(r) and netstrade   and yr2007(yr)) = 1.000 ;
co2pfad_nets(r,yr)$(eu28(r) and netstrade   and yr2010(yr)) = 1.000 ;
co2pfad_nets(r,yr)$(eu28(r) and netstrade   and yr2015(yr)) = 0.394 ;
co2pfad_nets(r,yr)$(eu28(r) and netstrade   and yr2020(yr)) = 0.305 ;
co2pfad_nets(r,yr)$(eu28(r) and netstrade   and yr2025(yr)) = 0.332 ;
co2pfad_nets(r,yr)$(eu28(r) and netstrade   and yr2030(yr)) = 0.352 ;

* ------ 15.02.2016 carblim für jede Modellperiode über [carblim0(r) * co2pfad_nets(r,yr)] definieren
*carblim(r)$(eu28(r) and netstrade_r)     = carblim0(r) * co2pfad_nets(r,yr) ;
*carblim(r)$(eu28(r) and netstrade)       = carblim0(r) * co2pfad_nets(r,yr) ;
* ------ 18.02.2016 carblim für jede Modellperiode über [ (carblim0(r) - carblim_ets0(r)) * co2pfad_nets(r,yr)] definieren
carblim(r)$(eu28(r) and netstrade_r)     = (carblim0(r) - carblim_ets0(r)) * co2pfad_nets(r,yr) ;
carblim(r)$(eu28(r) and netstrade)       = (carblim0(r) - carblim_ets0(r)) * co2pfad_nets(r,yr) ;

display co2pfad_ets, co2pfad_ets_eu, co2pfad_nets, carblim_ets, carblim_ets0, carblim0, carblim;


* ------ CO2 Tax ---------------------------------------------------------------
$ontext  // CO2 tax hier einschalten
* ------ 1.04.2016 CO2-Steuer im HH-Bereich (aufdiskontiert mit 4 %, Referenzjahr 2007 = 30 --> vgl. co2tax_30.xlsx)
co2price = 0;
co2price$after(yr)  = 34;
co2price$yr2015(yr) = 41;
co2price$yr2020(yr) = 50;
co2price$yr2025(yr) = 61;
co2price$yr2030(yr) = 74;
co2tax(r,i,cbt,cbf)$c0_fuels(r,i,cbt,cbf) = cbco2i(r,i,cbt,cbf) * co2price / c0_fuels(r,i,cbt,cbf) ;
display co2price, co2tax ;
$offtext  // CO2 tax hier einschalten


* ------ Efficiency standards here --------------------------------------------------
*$ontext  // Standards hier einschalten
* ------ 7.01.2016 Fahrzeug-Standards (SC3)
eff_target(r)$(deu(r) and yr2007(yr))    = eff_avg(r) ;
eff_target(r)$(deu(r) and yr2010(yr))    = 145/1000 ;
eff_target(r)$(deu(r) and yr2015(yr))    = 115/1000 ;
eff_target(r)$(deu(r) and yr2020(yr))    = 95/1000;
eff_target(r)$(deu(r) and yr2025(yr))    = 78/1000;
eff_target(r)$(deu(r) and yr2030(yr))    = 65/1000;
*NEWCAR.UP(r,"small","ele") = 10 * NEWCAR.L(r,"small","ele") ;

* ------ 7.01.2016 Gebäude-Standards (SC3)
*effh_target(r)$(deu(r) and yr2010(yr))   = effh_avg(r) ;
*effh_target(r)$(deu(r) and yr2015(yr))   = 55 ;
*effh_target(r)$(deu(r) and yr2020(yr))   = 46 ;
*effh_target(r)$(deu(r) and yr2025(yr))   = 38 ;
*effh_target(r)$(deu(r) and yr2030(yr))   = 30 ;
* ------ 29.03.2016 bringt keine wesentlichen Änderungen hinsichtlich avgkwh_yr
effh_target(r)$(deu(r) and yr2007(yr))   = effh_avg(r) ;
effh_target(r)$(deu(r) and yr2010(yr))   = 56 ;
effh_target(r)$(deu(r) and yr2015(yr))   = 47 ;
effh_target(r)$(deu(r) and yr2020(yr))   = 39 ;
effh_target(r)$(deu(r) and yr2025(yr))   = 32 ;
effh_target(r)$(deu(r) and yr2030(yr))   = 27 ;
* ------ 5.02.2016 Folgendes ist nötig um bei Effizienzstandards Lösungsprobleme in 2015 zu vermeiden
*carsub(r,"small","ele")$yr2015(yr)    = -0.1 ;
*$offtext  // Standards hier einschalten


* ------ Electricity tax -------------------------------------------------------
* ------ 14.09.2015 (Diss) Stromsteuer in Deutschland entfernen
*tfueli(r,i,cbt,cbf)$(ele(i) and eu28(r) and after(yr))   = tfueli(r,i,cbt,cbf)   * 0.5 ;
*tothele0(r)$(eu28(r) and after(yr))                      = tothele0(r)           * 0.5 ;
*tc(i,r)$(ele(i) and eu28(r) and after(yr))               = tc(i,r)               * 0.5 ;
*ti(i,j,r)$(ele(i) and eu28(r) and after(yr))             = ti(i,j,r)             * 0.5 ;
*display ti, ty, tc, tfueli, tothele0;


* ------ Public funding of low-carbon energy technologies ----------------------
$ontext   // Förd. hier einschalten
* ------ 1.04.2016 E-Autos Zuschuss/Kaufprämie (Electric Vehicle Initiative) --> Diss!
carsub(r,"small","ele")$after(yr)    = -0.23 ;
carsub(r,"middl","phe")$after(yr)    = -0.14 ;
carsub(r,"large","phe")$after(yr)    = -0.08 ;
display carsub;
*$offtext
*$ontext
* ------ 1.04.2016 Passivhäuser subventionieren (KfW-Förderung) --> Diss!
bt_sub(r,"new_p",bf)$(after(yr)  and (sum(i, bnd0(r,i,"new_p",bf))))  = -0.25 ;
* ------ 29.03.2016
*bt_sub(r,"new_p",bf)$(after(yr)  and (sum(i, bnd0(r,i,"new_p",bf))))  = -0.5 ;
*bt_sub(r,"new_e",bf)$(after(yr)  and (sum(i, bnd0(r,i,"new_e",bf))))  = -0.5 ;
display bt_sub ;
*$offtext
*$ontext
* ------ 4.01.2016 Sanierungsrate manuell ansteuern --> Achtung: Wirkung auf CO2_yr tritt verzögert ein (wirkt erst über altbau-abschreibung in der folgenden Periode)
*bt_san(r,"old_r",bf)$(yr2007(yr) and (sum(i, bnd0(r,i,"old_r",bf))))    = -0.20 ;
*bt_san(r,"old_r",bf)$(yr2010(yr) and (sum(i, bnd0(r,i,"old_r",bf))))    = -0.45 ;
*bt_san(r,"old_r",bf)$(yr2015(yr) and (sum(i, bnd0(r,i,"old_r",bf))))    = -0.20 ;
*bt_san(r,"old_r",bf)$(yr2020(yr) and (sum(i, bnd0(r,i,"old_r",bf))))    = -0.80 ;
*bt_san(r,"old_r",bf)$(yr2025(yr) and (sum(i, bnd0(r,i,"old_r",bf))))    = -0.80 ;
*bt_san(r,"old_r",bf)$(yr2030(yr) and (sum(i, bnd0(r,i,"old_r",bf))))    = -0.40 ;
*bt_san(r,"old_r",bf)$(after(yr)  and (sum(i, bnd0(r,i,"old_r",bf))))    = bt_san(r,"old_r",bf)/2 ;
* ------ 1.04.2016  Sanierungen subventionieren--> Diss!
bt_san(r,"old_r",bf)$(after(yr)  and (sum(i, bnd0(r,i,"old_r",bf))))    = -0.24 ;
* ------ 29.03.2016
*bt_san(r,"old_r",bf)$(after(yr)  and (sum(i, bnd0(r,i,"old_r",bf))))    = -0.5 ;
display bt_san;
$offtext  // Förd. hier einschalten

* ------ 11.01.2016 Diss: Textmarke Scenarios
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

* ---------------------------------------------------------------------- *
* ###### SOLVE STATEMENT 2                                                       // # LOOP-solve #
* ---------------------------------------------------------------------- *

* ------ Set iteration limit to a reasonable size, e.g. 2500:
NEWAGE.iterlim =  5000;
NEWAGE.iterlim = 10000;
* ------ INCLUDE and SOLVE
$include NEWAGE.gen
SOLVE    NEWAGE    using   MCP;

*$exit
*EXITsolve2
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ 14.10.2014 Reporting the two Elements of RECURSIVE Capital DYNAMICS
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

evoa1("cap",r,yr)$(ord(yr) = 1)  = ((1-dep(r))**3) * evoa("cap",r) ;
evoa1("cap",r,yr)$(ord(yr) > 1)  = ((1-dep(r))**5) * evoa("cap",r) ;
evoa11("cap",r,yr)$(ord(yr) = 1) = ((1-dep(r))**3) * evoa0("cap",r) ;
evoa11("cap",r,yr)$(ord(yr) > 1) = ((1-dep(r))**5) * evoa11("cap",r,yr-1) ;
evoa2("cap",r,yr)$(ord(yr) = 1)  = VINV_PINV.L(r) * PINV.L(r) * (3*brk0(r)) ;
evoa2("cap",r,yr)$(ord(yr) > 1)  = VINV_PINV.L(r) * PINV.L(r) * (5*brk0(r)) ;

* ------ RECURSIVE Capital DYNAMICS (growth / endowment driver) ---------------- // # CAPITAL dynamics #
* ------ Capital (2007-2010: 3 years, 2010-2015: 5 years) ORIGINAL
evoa("cap",r)$(ord(yr) = 1)  = ((1-dep(r))**3) * evoa("cap",r) + VINV_PINV.L(r) * PINV.L(r) * (3*brk0(r))  ;  // 2007
evoa("cap",r)$(ord(yr) > 1)  = ((1-dep(r))**5) * evoa("cap",r) + VINV_PINV.L(r) * PINV.L(r) * (5*brk0(r))  ;  // 2010 <

* ------ 03.11.2014 10-year-milestones
* ------ Capital (2007-2010: 3 years, 2010-2015: 5 years)
*evoa("cap",r)$(ord(yr) = 1)  = ((1-dep(r))**3)  * evoa("cap",r) + VINV_PINV.L(r) * PINV.L(r) * (3*brk0(r))   ;  // 2007-->2010
*evoa("cap",r)$(ord(yr) > 1)  = ((1-dep(r))**5)  * evoa("cap",r) + VINV_PINV.L(r) * PINV.L(r) * (5*brk0(r))   ;  // 2010-->2015, 2015-->2020
*evoa("cap",r)$(ord(yr) > 3)  = ((1-dep(r))**10) * evoa("cap",r) + VINV_PINV.L(r) * PINV.L(r) * (10*brk0(r))  ;  // 2020-2050
* ------ 3.11.2014 10-year-milestones (oben)

* ------ 1.07.2015 SC1: Klimaschäden in BAW und Welt --> Gleichmäßige Beeinträchtigung des Produktivkapitals um 1,3 % Welt-BIP-Schaden p.a. zu erreichen.
* ------ evoa wird NACH dem SOLVE aktualisiert
evoa("cap",r)$(sc1 and yr2007(yr))       = 0.955 * evoa("cap",r);      // for 2010
evoa("cap",r)$(sc1 and yr2010(yr))       = 0.982 * evoa("cap",r);      // for 2015
evoa("cap",r)$(sc1 and yr2015(yr))       = 0.991 * evoa("cap",r);      // for 2020
evoa("cap",r)$(sc1 and yr2020(yr))       = 0.988 * evoa("cap",r);      // for 2025
evoa("cap",r)$(sc1 and yr2025(yr))       = 0.990 * evoa("cap",r);      // for 2030
evoa("cap",r)$(sc1 and yr2030(yr))       = 0.990 * evoa("cap",r);      // for 2035
evoa("cap",r)$(sc1 and yr2035(yr))       = 0.991 * evoa("cap",r);      // for 2040
evoa("cap",r)$(sc1 and yr2040(yr))       = 0.991 * evoa("cap",r);      // for 2045
evoa("cap",r)$(sc1 and yr2045(yr))       = 0.993 * evoa("cap",r);      // for 2050
* ------ 1.07.2015 Textmarke SC1

* ------ 1.07.2015 Capital intensity
VA_PYxm.L(i,r,r)         = 0;
capgdp_yr(r,yr)          =                       evoa("cap",r)   / (VC_PC.L(r) + VINV_PINV.L(r) + sum(i, sum(s, VA_PYxm.L(i,r,s) - VA_PYxm.L(i,s,r))));
*capgdp_yr("BRD",yr)      = sum(r$bawdeu(r),      evoa("cap",r))   / sum(r$bawdeu(r),(VC_PC.L(r) + VINV_PINV.L(r) + sum(i, sum(s, VA_PYxm.L(i,r,s) - VA_PYxm.L(i,s,r)))));
capgdp_yr("EU28",yr)     = sum(r$eu28(r),        evoa("cap",r))   / sum(r$eu28(r), (VC_PC.L(r) + VINV_PINV.L(r) + sum(i, sum(s, VA_PYxm.L(i,r,s) - VA_PYxm.L(i,s,r)))));
capgdp_yr("World",yr)    = sum(r,                evoa("cap",r))   / sum(r, (VC_PC.L(r) + VINV_PINV.L(r) + sum(i, sum(s, VA_PYxm.L(i,r,s) - VA_PYxm.L(i,s,r)))));

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ Cars and buildings REPORTING
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


* ###### 5.02.2016 Textmarke Comment out Diss (unten)
*$ontext
ssdrep_yr(r,ct,cf,yr) = ssd(r,ct,cf) ;
ssdrep_yr(r,"total","total",yr) = sum((ct,cf), ssd(r,ct,cf)) ;
bsdrep_yr(r,bt,bf,yr) = bsd(r,bt,bf) ;
bsdrep_yr(r,"total","total",yr) = sum((bt,bf), bsd(r,bt,bf)) ;
cars2_yr(r,ct,cf,"new",yr) = cars(r,ct,cf,"new") * NEWCAR.L(r,ct,cf) ;
cars2_yr(r,"total","total","new",yr) = sum((ct,cf), cars(r,ct,cf,"new") * NEWCAR.L(r,ct,cf)) ;

* ------ 4.01.2016
* ------ Anzahl Fahrzeuge gesamt und pro Typ
*anz_car(r,ct,cf,"old")$ssd0price(r,ct,cf) = PXTCAR.L(r,ct,cf) * ssd0(r,ct,cf)/ssd0price(r,ct,cf);
*anz_car(r,ct,cf,"new")$snd0price(r,ct,cf) = NEWCAR.L(r,ct,cf)* snd0(r,"mvh",ct,cf)/snd0price(r,ct,cf);
newcar_yr(r,ct,cf,yr) = NEWCAR.L(r,ct,cf) ;
anz_car(r,ct,cf,"old",yr)$ssd0price(r,ct,cf) = VSTOCK_CXC.L(r,ct,cf) / ssd0price(r,ct,cf);
anz_car(r,ct,cf,"new",yr)$snd0price(r,ct,cf) = VSTOCK_CNC.L(r,ct,cf) / pcar0(r,ct,cf) / snd0price(r,ct,cf);
anz_car(r,ct,cf,"Stock",yr) = anz_car(r,ct,cf,"old",yr) + anz_car(r,ct,cf,"new",yr);
anz_car(r,"total","total","old",yr) = sum((ct,cf), anz_car(r,ct,cf,"old",yr));
anz_car(r,"total","total","new",yr) = sum((ct,cf), anz_car(r,ct,cf,"new",yr));
anz_car(r,"total","total","Stock",yr)= sum((ct,cf), anz_car(r,ct,cf,"Stock",yr));
anz_car_rel(r,ct,cf,"old",yr)$cars(r,ct,cf,"old") = anz_car(r,ct,cf,"old",yr)/cars(r,ct,cf,"old");
anz_car_rel(r,ct,cf,"new",yr)$cars(r,ct,cf,"new") = anz_car(r,ct,cf,"new",yr)/cars(r,ct,cf,"new");
anz_car_rel(r,ct,cf,"Stock",yr)$cars(r,ct,cf,"Stock") = anz_car(r,ct,cf,"Stock",yr)/cars(r,ct,cf,"Stock");
anz_car_rel(r,"total","total","old",yr)$cars(r,"total","total","old") = anz_car(r,"total","total","old",yr)/cars(r,"total","total","old");
anz_car_rel(r,"total","total","new",yr)$cars(r,"total","total","new") = anz_car(r,"total","total","new",yr)/cars(r,"total","total","new");
anz_car_rel(r,"total","total","Stock",yr)$cars(r,"total","total","Stock")= anz_car(r,"total","total","Stock",yr)/cars(r,"total","total","Stock");
* ------ 26.01.2016 Anteil Neufahrzeuge am Gesamtbestand [Mio. Fahrzeuge] in % --> Fließt in die ssd-Abschreibung mit ein
anz_car_newrel(r,yr)$sum((ct,cf), ssd(r,ct,cf)) = anz_car(r,"total","total","new",yr) / anz_car(r,"total","total","Stock",yr) * 100 ;

* ------ Anzahl Wohnfläche gesamt und pro Typ
*anz_qm(r,bt,bf)$(qmcostc(r,bt,bf) and old(bt)) = PXTHOUSE.L(r,bt,bf) * bsd0(r,bt,bf) / qmcostc(r,bt,bf) ;
*anz_qm(r,bt,bf)$(qmcostc(r,bt,bf) and new(bt)) = NEWHOUSE.L(r,bt,bf)* sum(i, bnd0(r,i,bt,bf)) / qmcostc(r,bt,bf) ;

anz_qm(r,bt,bf,yr)$(qmcostc(r,bt,bf) and old(bt)) = vHEATF_PXTHOUSE.L(r,bt,bf) / qmcostc(r,bt,bf) ;
*anz_qm(r,bt,bf,yr)$(qmcostc(r,bt,bf) and new(bt)) = VSTOCK_HNH.L(r,bt,bf) / pc0_serbui(r,bt,bf) / qmcostc(r,bt,bf) ;

anz_qm_new(r,bt,bf,yr)$(qmcostc(r,bt,bf) and new(bt)) = VSTOCK_HNH.L(r,bt,bf) / pc0_serbui(r,bt,bf) / qmcostc(r,bt,bf) ;
anz_qm_newsum(r,bt,yr) = sum(bf, anz_qm_new(r,bt,bf,yr)) ; // * 1000 ;
anz_qm_newsum(r,"total",yr) = sum((bt,bf), anz_qm_new(r,bt,bf,yr)) ; // * 1000 ;

* ------ 7.02.2016 auskommentiert (unten)
*anz_qm(r,bt,bf,yr)$(qmcostc(r,bt,bf) and new(bt)) = anz_qm_new(r,bt,bf,yr) + (VSTOCK_HXH07.L(r,bt,bf) + VSTOCK_HXH10.L(r,bt,bf) + VSTOCK_HXH15.L(r,bt,bf)
*+ VSTOCK_HXH20.L(r,bt,bf) + VSTOCK_HXH25.L(r,bt,bf) + VSTOCK_HXH30.L(r,bt,bf)) / qmcostc_disc(r,bt,bf);

anz_qm(r,bt,bf,yr)$(qmcostc(r,bt,bf) and new(bt)) = anz_qm_new(r,bt,bf,yr) + VSTOCK_HNEWH.L(r,bt,bf) / qmcostc_disc(r,bt,bf);

anz_qm(r,"total","total",yr) = sum((bt,bf), anz_qm(r,bt,bf,yr)) ;
anz_qm_rel(r,bt,bf,yr)$(houseqm(r,bt,bf) and old(bt)) = anz_qm(r,bt,bf,yr) / houseqm(r,bt,bf) ;
anz_qm_rel(r,bt,bf,yr)$(houseqm(r,bt,bf) and new(bt)) = anz_qm_new(r,bt,bf,yr) / houseqm(r,bt,bf) ;
anz_qm_rel(r,"total","total",yr)$houseqm(r,"total","total") = anz_qm(r,"total","total",yr) / houseqm(r,"total","total") ;

* ------ 26.01.2016 Anteil Neubauten (inkl. Sanierung) am Gesamtbestand [Mrd. m²] in % --> Fließt in die bsd-Abschreibung mit ein
anz_qm_newrel(r,yr)$anz_qm(r,"total","total",yr) = ((anz_qm_newsum(r,"total",yr)) / (anz_qm(r,"total","total",yr))) * 100 ;
* ------ 26.01.2016 Anteil Neufahrzeuge am Gesamtbestand im Benchmark-Datensatz (ca. 1,5 % p.a.)
houseqm_newrel(r,yr)$sum((bt,bf), houseqm(r,bt,bf)) = sum((bt,bf)$new(bt), houseqm(r,bt,bf)) / sum((bt,bf), houseqm(r,bt,bf)) * 100 ;

* ------ Sanierungsrate
sanrate(r,yr)$sum((bt,bf), anz_qm(r,bt,bf,yr)) = sum(bf, anz_qm_new(r,"old_r",bf,yr)) / sum((bt,bf), anz_qm(r,bt,bf,yr)) * 100 ;

* ------ Cars and buildings (incl. efficiency standards)
cars2(r,ct,cf,"new") = cars(r,ct,cf,"new") * NEWCAR.L(r,ct,cf) ;
cars2(r,"total","total","new") = sum((ct,cf), cars2(r,ct,cf,"new")) ;
houseqm2(r,bt,bf)$new(bt) = houseqm(r,bt,bf) * NEWHOUSE.L(r,bt,bf) ;
houseqm2(r,"total","total") = sum((bt,bf)$new(bt), houseqm(r,bt,bf) * NEWHOUSE.L(r,bt,bf)) ;

eff_reso(r)$sum((ct,cf), cars(r,ct,cf,"new") * NEWCAR.L(r,ct,cf)) = sum((ct,cf), VNC_PEFF_O.L(r,ct,cf)) / sum((ct,cf), cars(r,ct,cf,"new") * NEWCAR.L(r,ct,cf)) ;
eff_resi(r)$sum((ct,cf), cars(r,ct,cf,"new") * NEWCAR.L(r,ct,cf)) = sum((ct,cf), VNC_PEFF_I.L(r,ct,cf)) / sum((ct,cf), cars(r,ct,cf,"new") * NEWCAR.L(r,ct,cf)) ;
effh_reso(r)$sum((bt,bf)$new(bt), houseqm(r,bt,bf) * NEWHOUSE.L(r,bt,bf)) = sum((bt,bf)$new(bt), VNH_PEFF_O.L(r,bt,bf)) / sum((bt,bf)$new(bt), houseqm(r,bt,bf) * NEWHOUSE.L(r,bt,bf)) ;
effh_resi(r)$sum((bt,bf)$new(bt), houseqm(r,bt,bf) * NEWHOUSE.L(r,bt,bf)) = sum((bt,bf)$new(bt), VNH_PEFF_I.L(r,bt,bf)) / sum((bt,bf)$new(bt), houseqm(r,bt,bf) * NEWHOUSE.L(r,bt,bf)) ;

eff_reso_yr(r,yr)  = eff_reso(r);
eff_resi_yr(r,yr)  = eff_resi(r);
effh_reso_yr(r,yr) = effh_reso(r);
effh_resi_yr(r,yr) = effh_resi(r);

* ------ 29.03.2016 Average heat demand of the whole building stock p.a.
*avgkwh_yr(r,yr)$deu(r) = sum((bt,bf), aeei_co2_yr(r,bt,bf,yr)  * kwhqm(r,bt,bf)*anz_qm(r,bt,bf,yr)) / sum((bt,bf), anz_qm(r,bt,bf,yr)) ;
avgkwh_yr(r,yr)$deu(r) = sum((bt,bf),                            kwhqm(r,bt,bf)*anz_qm(r,bt,bf,yr)) / sum((bt,bf), anz_qm(r,bt,bf,yr)) ;

display effh_target, effh_avg, effh_reso, effh_resi, eff_target, eff_avg, eff_reso, eff_resi, cars, cars2;
display anz_car, cars, anz_car_rel, anz_car_newrel, anz_qm, anz_qm_new, anz_qm_newsum, houseqm, anz_qm_rel, anz_qm_newrel, houseqm_newrel, sanrate;
display anz_qm, aeei_co2, avgkwh_yr;
*$offtext
* ###### 5.02.2016 Textmarke Comment out Diss (oben)


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
ssd(r,ct,cf)$yr2007(yr) = ssd(r,ct,cf) * (1-0.15)**3 + ssd0(r,ct,cf) * NEWCAR.L(r,ct,cf) * 3;
ssd(r,ct,cf)$(yr2007(yr) and sum((cct,ccf), ssd(r,cct,ccf))) = (ssd(r,ct,cf) / sum((cct,ccf), ssd(r,cct,ccf))) * sum((cct,ccf), ssd0(r,cct,ccf)) ;
* ------ Die Werte werden in Summe gleich gehalten, nur anteilsmäßig anders verteilt
ssd(r,ct,cf)$after(yr)  = ssd(r,ct,cf) * (1-0.15)**5 + ssd0(r,ct,cf) * NEWCAR.L(r,ct,cf) * 5; display ssd;
ssd(r,ct,cf)$(after(yr) and sum((cct,ccf), ssd(r,cct,ccf)))  = (ssd(r,ct,cf) / sum((cct,ccf), ssd(r,cct,ccf))) * sum((cct,ccf), ssd0(r,cct,ccf)) ; display ssd;
$offtext

*$ontext

*ssd(r,ct,cf)$after(yr) = ssd(r,ct,cf) * (1-0.15)**5 + newcar_yr(r,ct,cf,yr)/newcar_yr(r,ct,cf,yr-1) * cars(r,ct,cf,"new") *
*ssd(r,ct,cf)$after(yr)    = ssd(r,ct,cf) * (1-0.05)**3 + anz_car(r,ct,cf,"new",yr) * ssd0price(r,ct,cf) * pcar0(r,ct,cf) * 3; display ssd;
*ssd(r,ct,cf)$after2010(yr)= ssd(r,ct,cf) * (1-0.05)**5 + anz_car(r,ct,cf,"new",yr) * ssd0price(r,ct,cf) * pcar0(r,ct,cf) * 5; display ssd;






* ###### 5.02.2016 Textmarke Comment out Diss (unten)
*$ontext
* ------ CAR STOCK DYNAMICS
***ssd(r,ct,cf)$after(yr)    = ssd(r,ct,cf) * (1-0.05)**3 + anz_car(r,ct,cf,"new",yr) * ssd0price(r,ct,cf) * 3; display ssd;
***ssd(r,ct,cf)$after2010(yr)= ssd(r,ct,cf) * (1-0.05)**5 + anz_car(r,ct,cf,"new",yr) * ssd0price(r,ct,cf) * 5; display ssd;
ssd(r,ct,cf)$after(yr)    = ssd(r,ct,cf) * (1 - anz_car_newrel(r,yr)/100)**3 + anz_car(r,ct,cf,"new",yr) * ssd0price(r,ct,cf) * 3; display ssd;
ssd(r,ct,cf)$after2010(yr)= ssd(r,ct,cf) * (1 - anz_car_newrel(r,yr)/100)**5 + anz_car(r,ct,cf,"new",yr) * ssd0price(r,ct,cf) * 5; display ssd;
* ------ 8.02.2016  Bestand soll bis 2030 um 4% wachsen (http://www.rwi-essen.de/presse/mitteilung/128/) --> von 39 auf 40,5

*$offtext
* ------ 23.01.2016 (oben)
*ssd(r,ct,cf)$yr2007(yr) = ssd(r,ct,cf) * 0.90 + VNC_NEWCAR.L(r,ct,cf)*3;
*ssd(r,ct,cf)$after(yr)  = ssd(r,ct,cf) * 0.80 + VNC_NEWCAR.L(r,ct,cf)*5;
*sndc(r,ct,cf)$yr2007(yr) = sndc(r,ct,cf) * 0.8 + VNC_NEWCAR.L(r,ct,cf)*3 ;
*sndc(r,ct,cf)$after(yr)  = sndc(r,ct,cf) * 0.7 + VNC_NEWCAR.L(r,ct,cf)*5 ;


* ------ BUILDING STOCK DYNAMICS -----------------------------------------------
*bsd(r,bt,bf)$(yr2007(yr) and old(bt)) = bsd(r,bt,bf) * (1 - 0.05)**3 ;
*bsd(r,bt,bf)$(after(yr)  and old(bt)) = bsd(r,bt,bf) * (1 - 0.05)**5 ;

* ------ 30.12.2015
*bsd(r,bt,bf)$yr2007(yr) = bsd(r,bt,bf) * (1 + 0.005)**3 + VNH_NEWHOUSE.L(r,bt,bf)*1.5;
*bsd(r,bt,bf)$after(yr)  = bsd(r,bt,bf) * (1 + 0.005)**5 + VNH_NEWHOUSE.L(r,bt,bf)*2.5;
*bsd(r,bt,bf)$(yr2007(yr) and old(bt)) = bsd(r,bt,bf) * (1 - 0.01)**3 ;
*bsd(r,bt,bf)$(after(yr)  and old(bt)) = bsd(r,bt,bf) * (1 - 0.01)**5 ;
*bsd(r,bt,bf)$(yr2007(yr) and new(bt)) = bsd(r,bt,bf) + VNH_NEWHOUSE.L(r,bt,bf) * 1 ;
*bsd(r,bt,bf)$(after(yr)  and new(bt)) = bsd(r,bt,bf) + VNH_NEWHOUSE.L(r,bt,bf) * 1 ;
*bsd0(r,bt,bf) = bsd(r,bt,bf) ;

* ------ 26.01.2016
***bsd(r,bt,bf)$(yr2007(yr) and old(bt)) = bsd(r,bt,bf) * (1 - 0.02)**3 ;
***bsd(r,bt,bf)$(after(yr)  and old(bt)) = bsd(r,bt,bf) * (1 - 0.02)**5 ;

*bsd(r,bt,bf)$(yr2007(yr) and old(bt)) = bsd(r,bt,bf) * (1 - 0.0045 - anz_qm_newrel(r,yr)/100)**3 ; // soll in der Referenz ca. 2 % p.a. abgeschrieben werden
*bsd(r,bt,bf)$(after(yr)  and old(bt)) = bsd(r,bt,bf) * (1 - 0.0045 - anz_qm_newrel(r,yr)/100)**5 ; // soll in der Referenz ca. 2 % p.a. abgeschrieben werden

* ------ BUILDING STOCK DYNAMICS
bsd(r,bt,bf)$(yr2007(yr) and old(bt)) = bsd(r,bt,bf) * (1 - 0.8 * anz_qm_newrel(r,yr)/100)**3 ;
bsd(r,bt,bf)$(after(yr)  and old(bt)) = bsd(r,bt,bf) * (1 - 0.8 * anz_qm_newrel(r,yr)/100)**5 ;
* ------ 7.02.2016 auskommentiert (unten) bzw. da anz_qm_newrel nicht richtig berechnet wurde, wurde es auf 1,552 % p.a. gesetzt
*bsd(r,bt,bf)$(yr2007(yr) and old(bt)) = bsd(r,bt,bf) * (1 - 1.552/100)**3 ;
*bsd(r,bt,bf)$(after(yr)  and old(bt)) = bsd(r,bt,bf) * (1 - 1.552/100)**5 ;
* ------ 8.02.2016 Wohnfläche steigt um 5m² von 49 auf 54 --> ca. +10% (http://www.bbsr.bund.de/BBSR/DE/WohnenImmobilien/Wohnungsmarktprognosen/Fachbeitraege/Prognose2030/Prognose2030.html?nn=445310)

* ------ 8.02.2016 Reporting
bsdnh_yr(r,bt,bf,yr) =  bsdnh(r,bt,bf);                                  // Zuerst Reporting, dann Updating
bsdnh_yr(r,"total","total",yr) = sum((bt,bf), bsdnh_yr(r,bt,bf,yr));        // Zuerst Reporting, dann Updating
* ------ 8.02.2016 Updating
bsdnh(r,bt,bf)$yr2007(yr) = bsdnh(r,bt,bf) + anz_qm_new(r,bt,bf,yr) * qmcostc_disc(r,bt,bf) * 3;
bsdnh(r,bt,bf)$after(yr)  = bsdnh(r,bt,bf) + anz_qm_new(r,bt,bf,yr) * qmcostc_disc(r,bt,bf) * 5;
* ------ 8.02.2016 Upper Bound für NEWHOUSE
NEWHOUSE.UP(r,bt,bf) = NEWHOUSE.L(r,bt,bf) * 1.8 ;
*NEWHOUSE.L(r,bt,bf) = NEWHOUSE.L(r,bt,bf) * 2 ;

*$offtext
* ###### 5.02.2016 Textmarke Comment out Diss (oben)





* ------ 7.02.2016 auskommentiert (unten)
$ontext
bsdyr(r,bt,bf,"2010")$yr2007(yr) = anz_qm(r,bt,bf,yr)$new(bt) * qmcostc_disc(r,bt,bf) ;
bsdyr(r,bt,bf,"2015")$yr2010(yr) = anz_qm(r,bt,bf,yr)$new(bt) * qmcostc_disc(r,bt,bf) ;
bsdyr(r,bt,bf,"2020")$yr2015(yr) = anz_qm(r,bt,bf,yr)$new(bt) * qmcostc_disc(r,bt,bf) ;
bsdyr(r,bt,bf,"2025")$yr2020(yr) = anz_qm(r,bt,bf,yr)$new(bt) * qmcostc_disc(r,bt,bf) ;
bsdyr(r,bt,bf,"2030")$yr2025(yr) = anz_qm(r,bt,bf,yr)$new(bt) * qmcostc_disc(r,bt,bf) ;

bsdyr(r,bt,bf,"2010")$after(yr)     = bsdyr(r,bt,bf,"2010") * (1 - 0.1)**3 ;
bsdyr(r,bt,bf,"2015")$after2010(yr) = bsdyr(r,bt,bf,"2015") * (1 - 0.1)**5 ;
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

bsd_sum(r,bt,bf,yr)$after(yr) = bsd(r,bt,bf) + bsdyr(r,bt,bf,"2010")+bsdyr(r,bt,bf,"2015")+bsdyr(r,bt,bf,"2020")+bsdyr(r,bt,bf,"2025")+bsdyr(r,bt,bf,"2030") ;

bsdyr0(r,bt,bf,"2007") = bsdyr(r,bt,bf,"2007") ;
bsdyr0(r,bt,bf,"2010") = bsdyr(r,bt,bf,"2010") ;
bsdyr0(r,bt,bf,"2015") = bsdyr(r,bt,bf,"2015") ;
bsdyr0(r,bt,bf,"2020") = bsdyr(r,bt,bf,"2020") ;
bsdyr0(r,bt,bf,"2025") = bsdyr(r,bt,bf,"2025") ;
bsdyr0(r,bt,bf,"2030") = bsdyr(r,bt,bf,"2030") ;
$offtext



* ###### 5.02.2016 Textmarke Comment out Diss (unten)
*$ontext
***bsd(r,bt,bf)$(yr2007(yr) and new(bt)) = bsd(r,bt,bf) + VNH_NEWHOUSE.L(r,bt,bf) * 3 ;
***bsd(r,bt,bf)$(after(yr)  and new(bt)) = bsd(r,bt,bf) + VNH_NEWHOUSE.L(r,bt,bf) * 5 ;

* ------ 30.12.2015 .UP and .LO Beschränkungen ---------------------------------
*NEWHOUSE.UP(r,bt,bf) = NEWHOUSE.L(r,bt,bf) * 5 ;
*NEWHOUSE.LO(r,bt,bf) = NEWHOUSE.L(r,bt,bf) * 0.1 ;
*NEWHOUSE.LO(r,bt,bf) = NEWHOUSE.LO(r,bt,bf) * 0.98 ;
*HEAT.LO(r,bt)$new(bt) = HEAT.L(r,bt) * 0.95 ;
*HEAT.UP(r,bt)$old(bt)= HEAT.L(r,bt)*1.1 ;
*HEATAGG.LO(r) = 0.9 ;
*PXTHOUSE.L(r,bt,bf) = 1;

NEWCAR.UP(r,"small","ele")$deu(r) = 20000 ;
*NEWCAR.UP(r,"small","ele")$(deu(r) and yr2015(yr)) = 8000 ;

display ssdrep_yr, bsdrep_yr, bsdyr, cars2_yr, bsd0, bsd, ssd0, ssd, bnd0, snd0, bsdnh, bsdnh_yr ;
*display bsdyr, bsd_sum;

*$offtext
* ###### 5.02.2016 Textmarke Comment out Diss (oben)


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
* ###### 5.02.2016 Textmarke Comment out Diss (unten)
         - VC_ESD.L(r) * PESD.L(r)
* ###### 5.02.2016 Textmarke Comment out Diss (oben)
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
* ###### 5.02.2016 Textmarke Comment out Diss (unten)
sharec_yr(r,"ESD",yr)          = ( VC_ESD.L(r) * PESD.L(r) )             / (VC_PC.L(r) * PC.L(r));
* ###### 5.02.2016 Textmarke Comment out Diss (oben)
sharec_yr(r,"PCO2",yr)         = ( VC_CO2W.L(r) * PCO2.L(r) )            / (VC_PC.L(r) * PC.L(r));
sharec_yr(r,"PCO2W",yr)        = ( VC_CO2W.L(r) * PCO2W.L )              / (VC_PC.L(r) * PC.L(r));
sharec_yr(r,"PCO2_NETS",yr)    = ( VC_CO2_NETS.L(r) * PCO2_NETS.L )      / (VC_PC.L(r) * PC.L(r));
sharec_yr(r,"PCO2_NETSr",yr)   = ( VC_CO2_NETSr.L(r) * PCO2_NETSr.L(r) ) / (VC_PC.L(r) * PC.L(r));

* ------ Check input shares --> shareCt_yr must be zero
shareCt_yr(r,yr) = round(
                 + sum(i, sharec_yr(r,i,yr))
                 + sharec_yr(r,"PCO2",yr) + sharec_yr(r,"PCO2W",yr) + sharec_yr(r,"PCO2_NETS",yr) + sharec_yr(r,"PCO2_NETSr",yr)
* ###### 5.02.2016 Textmarke Comment out Diss (unten)
                + sharec_yr(r,"ESD",yr)
* ###### 5.02.2016 Textmarke Comment out Diss (oben)
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
gva_real_yr(r,"total",yr)        = sum(i,   gva_real_yr(r,i,yr)) ;
gva_real_yr("EU28",i,yr)         = sum(r$eu28(r),   gva_real_yr(r,i,yr)) ;
gva_real_yr("EU28","total",yr)   = sum(r$eu28(r),   gva_real_yr(r,"total",yr)) ;
gva_real_yr("World",i,yr)        = sum(r,   gva_real_yr(r,i,yr)) ;
gva_real_yr("World","total",yr)  = sum(r,   gva_real_yr(r,"total",yr)) ;

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

* ------ Trade
VA_PYxm.L(i,r,r)         = 0;
ex_yr(r,i,yr)            = sum(s, VA_PYxm.L(i,r,s));
im_yr(r,i,yr)            = sum(s, VA_PYxm.L(i,s,r));
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

gdpreal_yr(r,yr)         =  VC_PC.L(r) + VINV_PINV.L(r) + trdblnc_yr(r,"total",yr) ;
gdpreal_yr("EU28",yr)    =  sum(r$eu28(r), gdpreal_yr(r,yr));
gdpreal_yr("World",yr)   =  sum(r, gdpreal_yr(r,yr));

* ------ 1.04.2016
* ###### 5.02.2016 Textmarke Comment out Diss (unten)
*$ontext
gdpreal_yr2(r,yr)        =  gdpreal_yr(r,yr) - VC_ESD.L(r) ;
gdpreal_yr2("EU28",yr)   =  sum(r$eu28(r), gdpreal_yr2(r,yr));
gdpreal_yr2("World",yr)  =  sum(r, gdpreal_yr2(r,yr));
*$offtext
* ###### 5.02.2016 Textmarke Comment out Diss (oben)


*gdpreal5_yr("BRD",yr)    =  sum(r$bawdeu(r), gdpreal5_yr(r,yr));
*gdpreal_yr("BRD",yr)     =  sum(r$bawdeu(r), gdpreal_yr(r,yr));

* ------ 6.07.2015 Cumulative GDP
gdprealcum_yr(r,yr)$yr2007(yr) = gdpreal_yr(r,yr);
gdprealcum_yr(r,yr)$yr2010(yr) = 1/2 * 3 * (gdpreal_yr(r,yr-1) + gdpreal_yr(r,yr)) ;
gdprealcum_yr(r,yr)$after2010(yr) = 1/2 * 5 * (gdpreal_yr(r,yr-1) + gdpreal_yr(r,yr)) ;
gdprealcum_yr("EU28",yr)$yr2007(yr) = gdpreal_yr("EU28",yr);
gdprealcum_yr("EU28",yr)$yr2010(yr) = 1/2 * 3 * (gdpreal_yr("EU28",yr-1) + gdpreal_yr("EU28",yr)) ;
gdprealcum_yr("EU28",yr)$after2010(yr) = 1/2 * 5 * (gdpreal_yr("EU28",yr-1) + gdpreal_yr("EU28",yr)) ;
gdprealcum_yr("World",yr)$yr2007(yr) = gdpreal_yr("World",yr);
gdprealcum_yr("World",yr)$yr2010(yr) = 1/2 * 3 * (gdpreal_yr("World",yr-1) + gdpreal_yr("World",yr)) ;
gdprealcum_yr("World",yr)$after2010(yr) = 1/2 * 5 * (gdpreal_yr("World",yr-1) + gdpreal_yr("World",yr)) ;
*gdprealcum_yr("BRD",yr)$yr2007(yr) = gdpreal_yr("BRD",yr);
*gdprealcum_yr("BRD",yr)$yr2010(yr) = 1/2 * 3 * (gdpreal_yr("BRD",yr-1) + gdpreal_yr("BRD",yr)) ;
*gdprealcum_yr("BRD",yr)$after2010(yr) = 1/2 * 5 * (gdpreal_yr("BRD",yr-1) + gdpreal_yr("BRD",yr)) ;

*display gdprealcum_yr;


* ------ Input demands ---------------------------------------------------------
demand_yr(r,i,j,yr)      = VY_PA.L(i,j,r);
demand_yr(r,i,"ele",yr)  = sum(gen, VELEx_PA.L(i,gen,r) + VELEn_PA.L(i,gen,r)) ;
demand_yr(r,i,"c",yr)    = VC_PA.L(i,r);
demand_yr(r,i,"i",yr)    = VINV_PA.L(i,r);
demand_yr("EU28",i,j,yr)      = sum(r$eu28(r), demand_yr(r,i,j,yr)) ;
demand_yr("EU28",i,"ele",yr)  = sum(r$eu28(r), demand_yr(r,i,"ele",yr)) ;
demand_yr("EU28",i,"c",yr)    = sum(r$eu28(r), demand_yr(r,i,"c",yr)) ;
demand_yr("EU28",i,"i",yr)    = sum(r$eu28(r), demand_yr(r,i,"i",yr)) ;
*demand_yr("BRD",i,j,yr)      = sum(r$bawdeu(r), demand_yr(r,i,j,yr)) ;
*demand_yr("BRD",i,"ele",yr)  = sum(r$bawdeu(r), demand_yr(r,i,"ele",yr)) ;
*demand_yr("BRD",i,"c",yr)    = sum(r$bawdeu(r), demand_yr(r,i,"c",yr)) ;
*demand_yr("BRD",i,"i",yr)    = sum(r$bawdeu(r), demand_yr(r,i,"i",yr)) ;

* ###### 5.02.2016 Textmarke Comment out Diss (unten)
*$ontext
demand_yr(r,i,"ESD",yr)$trn(i)   = vESD_trn.L(i,r) ;
demand_yr(r,i,"ESD",yr)$ele(i)   = vESD_ele.L(i,r) ;
demand_yr(r,i,"CARTF",yr)        = sum((ct,cf), vCARTF_PA.L(r,i,ct,cf));
demand_yr(r,i,"HEATF",yr)        = sum((bt,bf), vHEATF_PA.L(r,i,bt,bf));
*demand_yr(r,i,"FUELS",yr)        = vFUELS_PA.L(fe,r) ;
demand_yr(r,i,"OILS",yr)         = sum((cbt,cbf), vOILS_PA.L(r,i,cbt,cbf)) ;
demand_yr(r,i,"BIO",yr)          = sum((cbt,cbf), vB_PA.L(r,i,cbt,cbf)) ;
demand_yr(r,i,"NEWCAR",yr)       = sum((ct,cf), vNC_PA.L(r,i,ct,cf)) ;
demand_yr(r,i,"NEWHOUSE",yr)     = sum((bt,bf),   vNH_PA.L(r,i,bt,bf)) ;
demand_yr(r,i,"HH",yr)           = demand_yr(r,i,"ESD",yr) + demand_yr(r,i,"CARTF",yr) + demand_yr(r,i,"HEATF",yr)
*                                 + demand_yr(r,i,"FUELS",yr)
                                 + demand_yr(r,i,"OILS",yr) + demand_yr(r,i,"BIO",yr)
                                 + demand_yr(r,i,"NEWCAR",yr) + demand_yr(r,i,"NEWHOUSE",yr) ;
*$offtext
* ###### 5.02.2016 Textmarke Comment out Diss (oben)


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
emplmtno_yr(r,"skl",yr)$yr2007(yr)=emplmtno_2007(r,"skl");
emplmtno_yr(r,"usk",yr)$yr2007(yr)=emplmtno_2007(r,"usk");
emplmtno_yr(r,"skl",yr)$after(yr)= emplmtno_2007(r,"skl") * (emplmt_yr(r,"skl",yr) / emplmt_yr(r,"skl","2007")) / size_skl(r,yr) ;
emplmtno_yr(r,"usk",yr)$after(yr)= emplmtno_2007(r,"usk") * (emplmt_yr(r,"usk",yr) / emplmt_yr(r,"usk","2007")) / size_usk(r,yr) ;
emplmtno_yr(r,"total",yr)        = emplmtno_yr(r,"skl",yr) + emplmtno_yr(r,"usk",yr) ;
emplmtno_yr("EU28","skl",yr)     = sum(r$eu28(r),   emplmtno_yr(r,"skl",yr)) ;
emplmtno_yr("EU28","usk",yr)     = sum(r$eu28(r),   emplmtno_yr(r,"usk",yr)) ;
emplmtno_yr("EU28","total",yr)   = sum(r$eu28(r),   emplmtno_yr(r,"total",yr)) ;
*emplmtno_yr("BRD","skl",yr)      = sum(r$bawdeu(r), emplmtno_yr(r,"skl",yr)) ;
*emplmtno_yr("BRD","usk",yr)      = sum(r$bawdeu(r), emplmtno_yr(r,"usk",yr)) ;
*emplmtno_yr("BRD","total",yr)    = sum(r$bawdeu(r), emplmtno_yr(r,"total",yr)) ;

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

*display emplmtno_yr, ur_yr;

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
pco2w_yr(yr)     = PCO2W.L       ;
pco2_yr(r,yr)    = PCO2.L(r)     ;
rkx_ele_yr(r,gen,yr)= RKX_ELE.L(gen,r);

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
VC_PC_yr(r,yr)           = VC_PC.L(r);
VYT_PT_yr(yr)            = VYT_PT.L;
VY_PGEN_yr(r,gen,yr)     = VY_PGEN.L(gen,r);
VELEx_PGEN_yr(r,gen,yr)  = VELEx_PGEN.L(gen,r);
VELEn_PGEN_yr(r,gen,yr)  = VELEn_PGEN.L(gen,r);

* ------ Input quantities and demand
VD_PINV_yr(r,yr)         = VD_PINV.L(r)   ;
VD_PC_yr(r,yr)           = VD_PC.L(r)     ;
VINV_PA_yr(r,i,yr)       = VINV_PA.L(i,r) ;

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
elecontwh_yr(r,"NetImp",yr)      = (- trdblnc_yr(r,"ele",yr)) + trdblnc_yr(r,"ele","2007") ;
elecontwh_yr(r,"total",yr)       = ele_twh_yr(r,"total",yr) + elecontwh_yr(r,"NetImp",yr) ;
elecontwh_yr("EU28",gen,yr)      = ele_twh_yr("EU28",gen,yr) ;
elecontwh_yr("EU28","NetImp",yr) = (- trdblnc_yr("EU28","ele",yr)) + trdblnc_yr("EU28","ele","2007");
elecontwh_yr("EU28","total",yr)  = ele_twh_yr("EU28","total",yr) + elecontwh_yr("EU28","NetImp",yr) ;
*elecontwh_yr("BRD",gen,yr)       = ele_twh_yr("BRD",gen,yr) ;
*elecontwh_yr("BRD","NetImp",yr)  = (- trdblnc_yr("BRD","ele",yr)) + trdblnc_yr("BRD","ele","2007");
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
eletwhyr("Capital",r,gen,yr)     =               elextwh_yr(r,gen,"2007") * abschr_pfad(r,gen,yr) ;
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
*carblim_yr("BRD",yr)     = sum(r$bawdeu(r), carblim(r));
*carblim_ets_yr("BRD",yr) = sum(r$bawdeu(r), carblim_ets(r));

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ CO2 Emissions
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
* ------ 03.07.2014

*co2ets_yr(r,yr)$etstrade         = sum(i, VY_CO2_ETS.L(i,r)) + sum(gen, VELEx_CO2_ETS.L(gen,r) + VELEn_CO2_ETS.L(gen,r)) ;
* ###### 5.02.2016 Textmarke Comment out Diss (unten/oben)
co2ets_yr(r,yr)$etstrade         = sum(i, VY_CO2_ETS.L(i,r)) + sum(gen, VELEx_CO2_ETS.L(gen,r) + VELEn_CO2_ETS.L(gen,r))
                                 + sum((bt,bf), VHEATF_PCO2ETS.L(r,bt,bf)) + sum((ct,cf), VCARTF_PCO2ETS.L(r,ct,cf)) ;
co2ets_heat(r,yr)$hhets          = sum((bt,bf), VHEATF_PCO2ETS.L(r,bt,bf)) ;
co2ets_cars(r,yr)$hhets          = sum((ct,cf), VCARTF_PCO2ETS.L(r,ct,cf)) ;
* ###### 5.02.2016 Textmarke Comment out Diss (oben)
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
co2c_yr(fe,r,yr)$co2coefc(fe,r)          = VC_PA.L(fe,r) / co2coefc(fe,r)  ;
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


* ###### 5.02.2016 Textmarke Comment out Diss (unten)
*$ontext
* ------ 23.01.2016 HH CO2 emissions
co2hh_yr(fe,ct,cf,r,yr)$co2coefhh(fe,ct,cf,r)  = VCARTF_PFUELS.L(r,fe,ct,cf) / co2coefhh(fe,ct,cf,r) ;
co2hh_yr(fe,bt,bf,r,yr)$co2coefhh(fe,bt,bf,r)  = VHEATF_PFUELS.L(r,fe,bt,bf) / co2coefhh(fe,bt,bf,r) ;
co2hh_yr("Total",cbt,cbf,r,yr)   = sum(fe, co2hh_yr(fe,cbt,cbf,r,yr));
co2hhc_yr(fe,r,yr)               = sum((ct,cf), co2hh_yr(fe,ct,cf,r,yr)) ;
co2hhb_yr(fe,r,yr)               = sum((bt,bf), co2hh_yr(fe,bt,bf,r,yr)) ;
co2hhc_yr("cars",r,yr)           = sum((fe,ct,cf), co2hh_yr(fe,ct,cf,r,yr)) ;
co2hhb_yr("buildings",r,yr)      = sum((fe,bt,bf), co2hh_yr(fe,bt,bf,r,yr)) ;
co2hhsum_yr(fe,r,yr)             = sum((cbt,cbf), co2hh_yr(fe,cbt,cbf,r,yr)) ;
co2hhsum_yr("total",r,yr)        = sum(fe,       co2hhsum_yr(fe,r,yr));
*$offtext
* ###### 5.02.2016 Textmarke Comment out Diss (oben)


* ------ Overall CO2-Emissions
*co2_yr(fe,r,yr)          = co2c_yr(fe,r,yr) + sum(i, co2y_yr(fe,i,r,yr)) + sum(gen, co2ele_yr(fe,gen,r,yr)) ;
* ###### 5.02.2016 Textmarke Comment out Diss (unten/oben)
co2_yr(fe,r,yr)          = co2c_yr(fe,r,yr) + co2hhsum_yr(fe,r,yr) + sum(i, co2y_yr(fe,i,r,yr)) + sum(gen, co2ele_yr(fe,gen,r,yr)) ;
* ###### 5.02.2016 Textmarke Comment out Diss (oben)
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

* ###### 5.02.2016 Textmarke Comment out Diss (unten)
*$ontext
co2hhsum_yr(fe,"EU28",yr)                = sum(r$EU28(r),   co2hhsum_yr(fe,r,yr)) ;
co2hhsum_yr("total","EU28",yr)           = sum(r$eu28(r),   co2hhsum_yr("total",r,yr));
co2hhsum_yr(fe,"World",yr)               = sum(r,   co2hhsum_yr(fe,r,yr)) ;
co2hhsum_yr("total","World",yr)          = sum(r,   co2hhsum_yr("total",r,yr));
* ------ 17.02.2016
co2all_yr(r,"Cars",yr)      = co2hhc_yr("cars",r,yr);
co2all_yr(r,"Buildings",yr) = co2hhb_yr("buildings",r,yr);
co2all_yr(r,"C",yr)$deu(r)  = co2hhsum_yr("total",r,yr);
*$offtext
* ###### 5.02.2016 Textmarke Comment out Diss (oben)

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


* ------ 5.05.2015 Calculate total CO2 emissions based on MPSGE Report values
co2_yr_2(r,yr) = VC_CO2.L(r) + VC_CO2_NETS.L(r) + VC_CO2_NETSr.L(r) + sum(i, VY_CO2.L(i,r) + VY_CO2_NETS.L(i,r) + VY_CO2_NETSr.L(i,r) + VY_CO2_ETS.L(i,r))
                 + sum(gen, VELEx_CO2.L(gen,r) + VELEn_CO2.L(gen,r) + VELEx_CO2_ETS.L(gen,r) + VELEn_CO2_ETS.L(gen,r)) ;
co2_yr_2("EU28",yr)      = sum(r$eu28(r),        co2_yr_2(r,yr) ) ;
co2_yr_2("World",yr)     = sum(r,                co2_yr_2(r,yr) ) ;

* ------ 10.12.2014 ---> CO2 in EU28 for non-ets sectors only
co2nets_yr1(r,fe,"final",yr)$(c0(fe,r)*aeei(fe,"c",r))                            = VC_PA.L(fe,r) / (c0(fe,r)*aeei(fe,"c",r))     * co2em(fe,"final",r)   * aeei(fe,"c",r)     ;
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
co2nets_yr1("chk","chk","chk","2007") = co2nets_yr1("all","sum","total","2007") + co2ets_yr("EU28","2007") - co2em_total("world") ;

* ------ PCO2_NETS
co2nets_yr(r,"final",yr)$netstrade       = VC_CO2_NETS.L(r) ;
co2nets_yr(r,"final",yr)$netstrade_r     = VC_CO2_NETSr.L(r) ;
co2nets_yr(r,i,yr)$netstrade             = VY_CO2_NETS.L(i,r) ;
co2nets_yr(r,i,yr)$netstrade_r           = VY_CO2_NETSr.L(i,r) ;
co2nets_yr(r,"Y",yr)$netstrade           = sum(i, VY_CO2_NETS.L(i,r)) ;
co2nets_yr(r,"Y",yr)$netstrade_r         = sum(i, VY_CO2_NETSr.L(i,r)) ;
co2nets_yr(r,"total",yr)$netstrade       = VC_CO2_NETS.L(r) + sum(i, VY_CO2_NETS.L(i,r)) ;
co2nets_yr(r,"total",yr)$netstrade_r     = VC_CO2_NETSr.L(r) + sum(i, VY_CO2_NETSr.L(i,r)) ;
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
co2w_yr(r,"final",yr)    = VC_CO2W.L(r) ;
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
* ###### 5.02.2016 Textmarke Comment out Diss (unten)
*$ontext
co2gdp_yr2(r,yr)         = 1E+6 * co2_yr("total",r,yr)           / gdpreal_yr2(r,yr)  ;     // multiply tCO2/€GDP in order to get gCO2/€GDP
co2gdp_yr2("EU28",yr)    = 1E+6 * co2_yr("total","EU28",yr)      / gdpreal_yr2("EU28",yr);  // multiply tCO2/€GDP in order to get gCO2/€GDP
co2gdp_yr2("World",yr)   = 1E+6 * co2_yr("total","World",yr)     / gdpreal_yr2("World",yr); // multiply tCO2/€GDP in order to get gCO2/€GDP
*$offtext
* ###### 5.02.2016 Textmarke Comment out Diss (oben)


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


* ###### 5.02.2016 Textmarke Comment out Diss (unten)
*$ontext
* ------ 14.09.2015 (Diss) Writing of the dynamic report parameters
vc_esd_yr(r,yr)                  =         VC_ESD.L(r)                   ;
vesd_o_yr(r,yr)                  =         VESD_O.L(r)                   ;
vesd_cart_yr(r,ct,yr)            =         VESD_CART.L(r,ct)             ;
vesd_trn_yr(i,r,yr)              =         VESD_TRN.L(i,r)               ;
vesd_heat_yr(r,bt,yr)            =         VESD_HEAT.L(r,bt)             ;
vheat_o_yr(r,bt,yr)              =         VHEAT_O.L(r,bt)               ;
vheat_f_yr(r,bt,bf,yr)           =         VHEAT_F.L(r,bt,bf)            ;
vcart_o_yr(r,ct,yr)              =         VCART_O.L(r,ct)               ;
vcart_f_yr(r,ct,cf,yr)           =         VCART_F.L(r,ct,cf)            ;
vheatf_o_yr(r,bt,bf,yr)          =         VHEATF_O.L(r,bt,bf)           ;
vheatf_pa_yr(r,i,bt,bf,yr)       =         VHEATF_PA.L(r,i,bt,bf)        ;
vheatf_pbio_yr(r,bt,bf,yr)       =         VHEATF_PBIO.L(r,bt,bf)        ;
vheatf_pfuels_yr(r,i,bt,bf,yr)   =         VHEATF_PFUELS.L(r,i,bt,bf)    ;
vheatf_stock_yr(r,bt,bf,yr)      =         VHEATF_STOCK.L(r,bt,bf)       ;
vheatf_pco2nets_yr(r,bt,bf,yr)   =         VHEATF_PCO2NETS.L(r,bt,bf)    ;
vcartf_o_yr(r,ct,cf,yr)          =         VCARTF_O.L(r,ct,cf)           ;
vcartf_pa_yr(r,i,ct,cf,yr)       =         VCARTF_PA.L(r,i,ct,cf)        ;
vcartf_pfuels_yr(r,i,ct,cf,yr)   =         VCARTF_PFUELS.L(r,i,ct,cf)    ;
vcartf_stock_yr(r,ct,cf,yr)      =         VCARTF_STOCK.L(r,ct,cf)       ;
vcartf_pco2nets_yr(r,ct,cf,yr)   =         VCARTF_PCO2NETS.L(r,ct,cf)    ;
vnc_newcar_yr(r,ct,cf,yr)        =         VNC_NEWCAR.L(r,ct,cf)         ;
vnc_pa_yr(r,i,ct,cf,yr)          =         VNC_PA.L(r,i,ct,cf)           ;
vnc_peff_i_yr(r,ct,cf,yr)        =         VNC_PEFF_I.L(r,ct,cf)         ;
vnc_peff_o_yr(r,ct,cf,yr)        =         VNC_PEFF_O.L(r,ct,cf)         ;
vnh_newhouse_yr(r,bt,bf,yr)      =         VNH_NEWHOUSE.L(r,bt,bf)       ;
vnh_pa_yr(r,i,bt,bf,yr)          =         VNH_PA.L(r,i,bt,bf)           ;
vnh_peff_i_yr(r,bt,bf,yr)        =         VNH_PEFF_I.L(r,bt,bf)         ;
vnh_peff_o_yr(r,bt,bf,yr)        =         VNH_PEFF_O.L(r,bt,bf)         ;
vstock_co_yr(r,ct,cf,yr)         =         VSTOCK_CO.L(r,ct,cf)          ;
vstock_cnc_yr(r,ct,cf,yr)        =         VSTOCK_CNC.L(r,ct,cf)         ;
vstock_cxc_yr(r,ct,cf,yr)        =         VSTOCK_CXC.L(r,ct,cf)         ;
vstock_ho_yr(r,bt,bf,yr)         =         VSTOCK_HO.L(r,bt,bf)          ;
vstock_hnh_yr(r,bt,bf,yr)        =         VSTOCK_HNH.L(r,bt,bf)         ;
vstock_hnewh_yr(r,bt,bf,yr)      =         VSTOCK_HNEWH.L(r,bt,bf)       ;
vstock_hpeffi_yr(r,bt,bf,yr)     =         VSTOCK_HPEFFI.L(r,bt,bf)      ;
vstock_hpeffo_yr(r,bt,bf,yr)     =         VSTOCK_HPEFFO.L(r,bt,bf)      ;
voils_o_yr(r,cbt,cbf,yr)         =         VOILS_O.L(r,cbt,cbf)          ;
voils_pa_yr(r,i,cbt,cbf,yr)      =         VOILS_PA.L(r,i,cbt,cbf)       ;
vb_bio_yr(r,cbt,cbf,yr)          =         VB_BIO.L(r,cbt,cbf)           ;
vb_pa_yr(r,i,cbt,cbf,yr)         =         VB_PA.L(r,i,cbt,cbf)          ;

* ------ 1.04.2016
pesd_yr(r,yr)$deu(r)             = PESD.L(r) ;
pheat_yr(r,bt,yr)$deu(r)         = PHEAT.L(r,bt) ;
pcart_yr(r,ct,yr)$deu(r)         = PCART.L(r,ct) ;
pheatf_yr(r,bt,bf,yr)$deu(r)     = PHEATF.L(r,bt,bf) ;
pcartf_yr(r,ct,cf,yr)$deu(r)     = PCARTF.L(r,ct,cf) ;
pfuels_yr(r,i,cbt,cbf,yr)$deu(r) = PFUELS.L(r,i,cbt,cbf) ;
poils_yr(r,cbt,cbf,yr)$deu(r)    = POILS.L(r,cbt,cbf) ;
pbio_yr(r,cbt,cbf,yr)$deu(r)     = PBIO.L(r,cbt,cbf) ;
pnewcar_yr(r,ct,cf,yr)$deu(r)    = PNEWCAR.L(r,ct,cf) ;
pnewhouse_yr(r,bt,bf,yr)$deu(r)  = PNEWHOUSE.L(r,bt,bf) ;
pstock_c_yr(r,ct,cf,yr)$deu(r)   = PSTOCK_C.L(r,ct,cf) ;
pstock_h_yr(r,bt,bf,yr)$deu(r)   = PSTOCK_H.L(r,bt,bf) ;
pheatagg_yr(r,yr)$deu(r)         = PHEATAGG.L(r) ;
pxtcar_yr(r,ct,cf,yr)$deu(r)     = PXTCAR.L(r,ct,cf) ;
pxthouse_yr(r,bt,bf,yr)$deu(r)   = PXTHOUSE.L(r,bt,bf) ;
pnewh_yr(r,bt,bf,yr)$deu(r)      = PNEWH.L(r,bt,bf) ;
peff_yr(r,yr)$deu(r)             = PEFF.L(r) ;
peffh_yr(r,yr)$deu(r)            = PEFFH.L(r) ;


*$offtext
* ###### 5.02.2016 Textmarke Comment out Diss (oben)

* ------ 16.02.2016
gdpco2_yr(r,yr)      = gdpreal_yr(r,yr) / co2_yr("total",r,yr);
gdpco2_yr("EU28",yr) = sum(r$eu28(r), gdpreal_yr(r,yr)) / sum(r$eu28(r), co2_yr("total",r,yr));
gdpco2_yr("World",yr)= sum(r, gdpreal_yr(r,yr)) / sum(r, co2_yr("total",r,yr)) ;


* ---------------------------------------------------------------------- *
* LOOP-ENDE                                                                      // # LOOP-end #
* ---------------------------------------------------------------------- *
);
* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

*display wg_diff, delta_wg_before, gew_wg, velen_pgenyr, zubau_ele, gen_limit_yr, genlimit_twh_yr;

*display ep, aeei_yr, aeei, aeei_elexyr, aeei_elenyr, wg0, wg_yr, abschreibung, abschreibung_bmk, evoa_yr, ressize, nucsize, up_yr, lo_yr, aeei_elenyr;
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

* ###### 5.02.2016 Textmarke Comment out Diss (unten)
*$ontext
display gdpreal_yr2;
display co2ets_heat, co2ets_cars, co2hh_yr, co2hhc_yr, co2hhb_yr, co2hhsum_yr, co2_yr_2;
display vstock_hnewh_yr, vstock_hnh_yr, vnh_newhouse_yr, vnc_newcar_yr;
display aeei_co2, aeei_cb, aeei_cb_yr, aeei_co2_yr;
display eff_reso_yr, eff_resi_yr, effh_reso_yr, effh_resi_yr ;
display co2gdp_yr, co2gdp_yr2, gdpco2_yr;
* ------ 1.04.2016
display pesd_yr, pheat_yr, pcart_yr, pheatf_yr, pcartf_yr, pfuels_yr, poils_yr, pbio_yr, pnewcar_yr, pnewhouse_yr, pstock_c_yr, pstock_h_yr, pheatagg_yr, pxtcar_yr, pxthouse_yr, pnewh_yr, peff_yr, peffh_yr ;
*$offtext
* ###### 5.02.2016 Textmarke Comment out Diss (oben)


display zpf_c_yr, zpf_y_yr, zpf_gva_yr, sharec_yr, sharey_yr, shareYgva_yr, shareYgva2_yr, gva_real_chk, sharect_yr, shareyt_yr, shareYtgva_yr, eleYgva_yr ;

*display co2_yr, gdpreal_yr, co2gdp_yr, co2gdp20_yr, co2gdp50_yr, capgdp_yr, tm, tx;

*display  vc_esd_yr, vesd_o_yr, vesd_cart_yr, vesd_trn_yr, vesd_heat_yr, vheat_o_yr, vheat_f_yr, vcart_o_yr, vcart_f_yr, vheatf_o_yr,
*         vheatf_pa_yr, vheatf_pbio_yr, vheatf_pfuels_yr, vheatf_stock_yr, vheatf_pco2nets_yr, vcartf_o_yr, vcartf_pa_yr, vcartf_pfuels_yr,
*         vcartf_stock_yr, vcartf_pco2nets_yr, vnc_pa_yr, vnc_peff_i_yr, vnc_peff_o_yr, vnh_pa_yr,
*         vnh_peff_i_yr, vnh_peff_o_yr, vstock_co_yr, vstock_cnc_yr, vstock_cxc_yr, vstock_ho_yr,
*         vstock_hpeffi_yr, vstock_hpeffo_yr, voils_o_yr, voils_pa_yr, vb_bio_yr, vb_pa_yr ;

display aeei, aeei_yr, ep;



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
*Execute  'gdxxrw.exe i=%resultsdir%results.gdx o=%resultsdir%report_pivot.xlsx epsout=0 @dumppar_ohneDiss.txt'
* ###### 5.02.2016 Textmarke Comment out Diss (unten/oben)
Execute  'gdxxrw.exe i=%resultsdir%results.gdx o=%resultsdir%report_pivot.xlsx epsout=0 @dumppar.txt'
* ###### 5.02.2016 Textmarke Comment out Diss (oben)

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
