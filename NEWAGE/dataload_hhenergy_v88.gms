$Title Load technology specific car data

* ------ 30.09.2014 Mainly taken from impose_transport_v02.gms
* ------ in D:\GAMS\Modell_TRN\Dataload\With CO2 - 2011-12-07\w efficiency - wo distance (new)\Code

*$if not set source       $set source     18x18x4
$if not set source       $set source     17x18x4_gtap9
$if not set output       $set output     %source%

*$if not set ds           $set ds         %output%FINAL
$if not set ds           $set ds         %output%

$if not set yr           $set yr         11
$if not set cdat         $set cdat       car_data
$if not set edat         $set edat       energy_data

$if not set datadir      $set datadir "..\data%yr%\"
$if not set xcel_datadir $set xcel_datadir "..\xcel_data\"
$if not set resultsdir   $set resultsdir "..\results\"
$setglobal  datadir      %datadir%

* ------ temp_data.gdx kommt aus NEWAGE8, wo die vorübergehenden Daten gespeichert werden
* ------ Der Include-Befehl für dataload_hhenergy... in NEWAGE8 darf daher nicht NICHT VOR dem Execute_unload-Befehl kommen (vgl. NEWAGE...gms)
$if not exist temp_data.gdx   $gdxin '%resultsdir%temp_data.gdx'

*$stop

$if not defined r        set r(*)        Regions;
$if not defined g        set g(*)        Sectors;
$if not defined i        set i(*)        Goods;
$if not defined e        set e(*)        Energy Goods;
$if not defined bawdeu   set bawdeu(r)   BRD ;
$if not defined cru      set cru(*)      Crude oil good;
$if not defined deu      set deu(*)      Germany;
$if not defined buiser   set buiser(*)   Buildings and Services;
$if not defined oil      set oil(*)      Mineral oil;
$if not defined gas      set gas(*)      Gas;
$if not defined col      set col(*)      Coal;
$if not defined fe       set fe(*)       Final energy goods col-gas-oil;
$if not defined agr      set agr(*)      Agricultural goods;
$if not defined trn      set trn(*)      Transport services;
$if not defined ele      set ele(*)      Electricity sector;
$if not defined r        $load r
$if not defined g        $load g
$if not defined i        $load i
$if not defined e        $load e
$if not defined fe       $load fe
*$if not defined bawdeu   $load bawdeu
$if not defined cru      $load cru
$if not defined deu      $load deu
$if not defined buiser   $load buiser
$if not defined oil      $load oil
$if not defined gas      $load gas
$if not defined col      $load col
$if not defined agr      $load agr
$if not defined trn      $load trn
$if not defined ele      $load ele

$if not defined c0       parameter c0    Final demand by commodity
$if not defined tc       parameter tc    Tax rate on final demand
$if not defined pc0      parameter pc0   Reference price for private demand
$if not defined ty       parameter ty
$if not defined vom      parameter vom
$if not defined aeei     parameter aeei
$if not defined co2em    parameter co2em
$if not defined co2em_total    parameter co2em_total
$if not defined eco2d    parameter eco2d
$if not defined eco2i    parameter eco2i
$if not defined eco2     parameter eco2
$if not defined cecphys  parameter cecphys
$if not defined c0       $load c0
$if not defined tc       $load tc
$if not defined pc0      $load pc0
$if not defined ty       $load ty
$if not defined vom      $load vom
$if not defined aeei     $load aeei
$if not defined co2em    $load co2em
$if not defined co2em_total    $load co2em_total
$if not defined eco2d    $load eco2d
$if not defined eco2i    $load eco2i
$if not defined eco2     $load eco2
$if not defined cecphys  $load cecphys

SET      select(r)       Dynamic set for selecting regions ;
select("deu") = yes;

*display r, g, i, e, cru, c0, tc, pc0, select;


*$eolcom //
* ------ 7.01.2015

*###############################################################################
*                  (2) LOAD BOTTOM UP CAR TECHNOLOGY DATA
*###############################################################################




* ##############################################################################
* ------ 7.01.2015 Energy Quantities
set cbt, cbf, bt(cbt), bf(cbf), ct(cbt), cf(cbf);



parameter
         HH_boup                 BoUp dataset on household energy oonsumption by technology
         HH_boup_deu             BoUp dataset on household energy oonsumption by technology in DEU
         HH_boup_baw             BoUp dataset on household energy oonsumption by technology in BAW
         energy                  Energy demand quantities in cars and buildings [GWh]
         ener_c(r,ct,cf)         Energy demand quantities in cars [TWh]
         ener_b(r,bt,bf)         Energy demand quantities in buildings [TWh]
         price_mwh(r,cbf)        Energy carrier prices [€ per MWh]
         price_qm(r,bt)          Floor space prices per housing type [K€ per m²]
         house_qm(r,cbt,cbt)     Living space per housing type [m²]
         assets(r,*)             Net assets of xtant and new cars and buildings [Bn €]
         phev_ele                Electricity share in PHEV fuel consumption per 100km / 0.20 /
         PJ_TWh                  PJ per TWh / 3.6 /
         carfactor               Price proportion of new and old cars (DAT Report 2008)  / 3.1 /
         hrents                  Housing rents in 2007 per m² (IVD Preisspiegel 2007-2008)
         avrent                  Average Housing rent in 2007 per m² (IVD Preisspiegel 2007-2008)
;

* Energiemengen einlesen, Preise ergeben sich durch Division mit den Werten
*enerb(

*------------------------ LOAD DATA FROM SPREADSHEET ---------------------------
* ------ write gdxxrw option file
$onecho >temp.tmp
set=cbt          rng=sets!B4     cdim=0  rdim=1
set=cbf          rng=sets!C4     cdim=0  rdim=1
set=bt           rng=sets!D4     cdim=0  rdim=1
set=bf           rng=sets!E4     cdim=0  rdim=1
set=ct           rng=sets!F4     cdim=0  rdim=1
set=cf           rng=sets!G4     cdim=0  rdim=1

par=energy       rng=Summary!B7:K14      cdim=1  rdim=2
par=price_mwh    rng=Summary!A65:C71             rdim=2
par=house_qm     rng=Summary!A55:G59     cdim=1  rdim=2
par=price_qm     rng=Summary!B39:D42             rdim=2
par=assets       rng=Summary!B35:D36             rdim=2

par=hrents       rng=IVD_Preise!AX72:AY75 cdim=0 rdim=1
par=avrent       rng=IVD_Preise!AX70:AY71 cdim=1 rdim=1

par=HH_boup      rng=dataset!D4:X32      cdim=1  rdim=3
*par=HH_boup      rng=dataset_DEU!D4:X32   cdim=1  rdim=3
*par=HH_boup      rng=dataset_komb!D4:X60 cdim=1  rdim=3

$offecho

$call "gdxxrw %xcel_datadir%%edat%.xlsx @temp.tmp"
$gdxin "%edat%"

$load energy
$load cbt cbf
$load bt bf
$load ct cf
$load price_mwh price_qm house_qm assets hrents avrent
$load HH_boup

ener_b(r,bt,bf)  = energy(r,bf,bt) / 1000;
ener_c(r,ct,cf)  = energy(r,cf,ct) / 1000;
price_qm(r,bt)   = price_qm(r,bt)  / 1000;

alias (r,rr), (ct,cct), (cf,ccf), (bt,bbt), (bf,bbf), (cbt,ccbt), (cbf,ccbf) ;

set

         car(cbt)        / small, middl, large /
         house(cbt)      / old_x, old_r, new_e, new_p /
         other(cbt)      / other /
         new(cbt)        / old_r, new_e, new_p /
         old(cbt)        / old_x /
         oldxr(cbt)      / old_x, old_r /
         large(cbt)      / large /

         oilc(cbf)       Oil based fuels                 / ben, die, hoi /
         bendie(cf)      Gasoline and diesel fuels       / ben, die /
         hoi(bf)         Heating oil                     / hoi /
         co2cbf(cbf)     CO2 relevant fuels              / col, gas, ben, die, hoi, phe /
         co2cf(cf)       CO2 relevant fuels              /      gas, ben, die,      phe /
         co2bf(bf)       CO2 relevant fuels              / col, gas,           hoi      /
         gass(cbf)       / gas /
         eles(cbf)       / ele /
         cols(cbf)       / col /
         bios(cbf)       / bio /
         phe(cbf)        / phe /
;

*display cbt, car, house, other;
*display cbt, cbf, ct, cf, bt, bf, new, energy, ener_b, ener_c, price_mwh, price_qm, house_qm, assets, hrents, avrent ;
*display old, large, new ;
display HH_boup ;
*$exit

parameter
* ------ Cars technology and costs
         cars            Car stock and new cars per technology [Mio.]
         carkm           Average kilometer demand per year per car class [K km]
         carco2          Specific CO2 emissions per car technology [gCO2 per km]
         carccm          Cubic capacity per car technology [ccm]
         carliter        Fuel demand per car technology [l per 100km]
         carkwh          Specific energy demand per car technology [kWh per km]
         carprice        Car rental costs for old and new cars [Mio. Euro]
         carpricenew     Catalog price for new cars [K Euro]
         car_ekn         Relation factor of rental and (net) energy costs for cars
         car_ekg         Relation factor of rental and (gross) energy costs for cars

* ------ Buildings technology and costs
         houseqm         Buildings floor space per technology [Bn. m²]
         housekwh        Specific energy demand per buildings technology [kWh per m²]
         houseprice      Rental costs per per buildings technology (IVD Preisspiegel) [Euro per m²]

* ------ Energy quantities, prices and costs
         carTWh          Total energy demand of cars per technology [TWH]
         carPJ           Total energy demand of cars per technology [PJ]
         houseTWh        Total heat demand in buildings per technology [TWh]
         housePJ         Total heat demand in buildings per technology [PJ]
         othereleTWh     Non-heating electricity demand in buildings per technology [TWh]
         otherelePJ      Non-heating electricity demand in buildings per technology [PJ]
         HH_energy       Total energy demand of households [TWh and PJ]
         TWh(r,cbt,cbf)

         eprice_n        Energy price net of taxes [€ per MWh]
         eprice_g        Energy price gross of taxes [€ per MWh]

         kmcosten        Specific energy costs (net) per car technology and km [€ per km]
         kmcosteg        Specific energy costs (gross) per car technology and km [€ per km]
         kmcostc         Specific capital or rental costs per car technology and km [€ per km]
         qmcosten        Specific energy costs (net) per buildings technology and qm [€ per m²]
         qmcosteg        Specific energy costs (gross) per buildings technology and qm [€ per m²]
         qmcostc         Specific capital or rental costs per buildings technology and qm [€ per m²]

         house_ekn       Relation factor of rental and energy costs (net) for buildings
         house_ekg       Relation factor of rental and energy costs (gross) for buildings

         carcostcap      Capital costs for existing cars per technology [Bn €]
         carcostnew      Capital costs for new cars per technology [Bn €]
         carcostenn      Energy costs (net) for cars per technology [Bn €]
         carcosteng      Energy costs (gross) for cars per technology [Bn €]
         housecostcap    Capital costs for existing buildings per technology [Bn €]
         housecostnew    Capital costs for new and renovated buildings per technology [Bn €]
         housecostenn    Energy costs (net) for buildings per technology [Bn €]
         housecosteng    Energy costs (gross) for buildings per technology [Bn €]
         othercostenn    Energy costs (net) for non-heating electricity demand in buildings [Bn €]
         othercosteng    Energy costs (gross) for non-heating electricity demand in buildings [Bn €]

         c00
;

* ------ Store existing final demand in parameter c00:
c00(i,r)   = c0(i,r) ; display c00;

* ------ Cars stock and technology
cars(r,ct,cf,"stock")    = HH_boup(r,ct,cf,"Stock")/1000;
cars(r,ct,cf,"new")      = HH_boup(r,ct,cf,"New")/1000;
cars(r,ct,cf,"old")      = cars(r,ct,cf,"stock") - cars(r,ct,cf,"new") ;
cars(r,"total","total","stock")  = sum((ct,cf), cars(r,ct,cf,"stock")) ;
cars(r,"total","total","new")    = sum((ct,cf), cars(r,ct,cf,"new")) ;
cars(r,"total","total","old")    = sum((ct,cf), cars(r,ct,cf,"old")) ;

carkm(r,ct,cf)           = HH_boup(r,ct,cf,"Kkm/car")    ;
carco2(r,ct,cf)          = HH_boup(r,ct,cf,"gCO2/km")    ;
carccm(r,ct,cf)          = HH_boup(r,ct,cf,"ccm")        ;
carliter(r,ct,cf)        = HH_boup(r,ct,cf,"l/100km")    ;
carkwh(r,ct,cf)          = HH_boup(r,ct,cf,"kWh/km")     ;
car_ekn(r,ct,cf)         = HH_boup(r,ct,cf,"K/E_n")     ;
car_ekg(r,ct,cf)         = HH_boup(r,ct,cf,"K/E_b")     ;
carpricenew(r,ct,cf)     = HH_boup(r,ct,cf,"K€/car")     ;
carpricenew(r,"avg","avg")$cars(r,"total","total","stock") = sum((ct,cf), carpricenew(r,ct,cf) * cars(r,ct,cf,"stock")) / cars(r,"total","total","stock") ;

* ------ Buildings floor and technology
houseqm(r,bt,bf)         = HH_boup(r,bt,bf,"Mio_qm")/1000 ;
houseqm(r,"total","total")= sum((bt,bf), houseqm(r,bt,bf)) ;

housekwh(r,bt,bf)        = HH_boup(r,bt,bf,"kWh/m²") ;
houseprice(r,bt,bf)      = HH_boup(r,bt,bf,"€/m²(K)") ;

*display cars, carkm, carco2, carliter, carkwh, carpricenew, car_ekn, car_ekg, houseqm, housekwh, houseprice;


* ------ Energy quantities, prices and costs
carTWh(r,ct,cf)                  = cars(r,ct,cf,"stock") * carkm(r,ct,cf) * carkwh(r,ct,cf);
carTWh(r,"total","total")        = sum((ct,cf), carTWh(r,ct,cf));
carPJ(r,ct,cf)                   = carTWh(r,ct,cf) * PJ_TWh;
carPJ(r,"total","total")         = carTWh(r,"total","total") * PJ_TWh;
houseTWh(r,bt,bf)                = houseqm(r,bt,bf) * housekwh(r,bt,bf) ;
houseTWh(r,"total","total")      = sum((bt,bf), houseTWh(r,bt,bf)) ;
housePJ(r,bt,bf)                 = houseTWh(r,bt,bf) * PJ_TWh;
housePJ(r,"total","total")       = houseTWh(r,"total","total") * PJ_TWh;
othereleTWh(r,"other",cbf,"TWh") = HH_boup(r,"other",cbf,"TWh") ;
otherelePJ(r,"other",cbf,"PJ")   = HH_boup(r,"other",cbf,"TWh") * PJ_TWh ;

HH_energy(r,"cars","TWh")        = carTWh(r,"total","total") ;
HH_energy(r,"houses","TWh")      = houseTWh(r,"total","total");
HH_energy(r,"other","TWh")       = sum(cbf, othereleTWh(r,"other",cbf,"TWh"));
HH_energy(r,"cars","PJ")         = HH_energy(r,"cars","TWh") * PJ_TWh ;
HH_energy(r,"houses","PJ")       = HH_energy(r,"houses","TWh") * PJ_TWh ;
HH_energy(r,"other","PJ")        = HH_energy(r,"other","TWh") * PJ_TWh ;
HH_energy(r,"SUM","TWh")         = HH_energy(r,"cars","TWh") + HH_energy(r,"houses","TWh") + HH_energy(r,"other","TWh") ;
HH_energy(r,"SUM","PJ")          = HH_energy(r,"cars","PJ") + HH_energy(r,"houses","PJ") + HH_energy(r,"other","PJ") ;

TWh(r,ct,cf) = carTWh(r,ct,cf) ;
TWh(r,bt,bf) = houseTWh(r,bt,bf) ;
TWh(r,"other","ele") = othereleTWh(r,"other","ele","TWh") ;

* ------ Assign energy prices
eprice_n(r,cbf)                          = sum(cbt$old(cbt), HH_boup(r,cbt,cbf,"€/MWh_n"));
eprice_n(r,cbf)$(not eprice_n(r,cbf))    = sum(cbt$large(cbt), HH_boup(r,cbt,cbf,"€/MWh_n"));
eprice_g(r,cbf)                          = sum(cbt$old(cbt), HH_boup(r,cbt,cbf,"€/MWh_b"));
eprice_g(r,cbf)$(not eprice_g(r,cbf))    = sum(cbt$large(cbt), HH_boup(r,cbt,cbf,"€/MWh_b"));

*display carTWh, carPJ, houseTWh, housePJ, othereleTWh, otherelePJ, hh_energy, eprice_n, eprice_g, cbf ;


* ------ Energy and capital costs per km and qm
kmcosten(r,ct,cf) = carkwh(r,ct,cf)    * eprice_n(r,cf) / 1000 ;
kmcosteg(r,ct,cf) = carkwh(r,ct,cf)    * eprice_g(r,cf) / 1000 ;
*kmcostc(r,ct,cf)  = kmcosteg(r,ct,cf)  * car_ekg(r,ct,cf) ; display kmcostc;
kmcostc(r,ct,cf)  = kmcosten(r,ct,cf)  * car_ekn(r,ct,cf) ; display kmcostc;
*$exit
* ------ 2.04.2015 (oben) bei kmcostc-Berechnung und eff_target = 40 führt car_ekb zu Lösungs-Problemen, car_ekn geht aber problemlos (Warum?) --> kmcostc wird weiter unten für ssd0 benötigt!

qmcosten(r,bt,bf) = housekwh(r,bt,bf)  * eprice_n(r,bf) / 1000 ;
qmcosteg(r,bt,bf) = housekwh(r,bt,bf)  * eprice_g(r,bf) / 1000 ;
qmcostc(r,bt,bf)  = houseprice(r,bt,bf) ;

house_ekn(r,bt,bf)$qmcosten(r,bt,bf) = qmcostc(r,bt,bf) / qmcosten(r,bt,bf) ;
house_ekg(r,bt,bf)$qmcosteg(r,bt,bf) = qmcostc(r,bt,bf) / qmcosteg(r,bt,bf) ;

*display car_ekg, car_ekn, kmcosten, kmcosteg, kmcostc, qmcosten, qmcosteg, qmcostc, house_ekn, house_ekg ;

* ------ Energy and capital costs for cars and buildings
carprice(r,ct,cf,"new")  = carfactor * kmcostc(r,ct,cf) * carkm(r,ct,cf) ;
carprice(r,ct,cf,"old")  =             kmcostc(r,ct,cf) * carkm(r,ct,cf)  ;
carprice(r,ct,cf,"AVG")$cars(r,ct,cf,"stock") = (carprice(r,ct,cf,"new") * cars(r,ct,cf,"new") + carprice(r,ct,cf,"old") * cars(r,ct,cf,"old")) / cars(r,ct,cf,"stock") ;
carprice(r,"TOTAL","AVG","new")$sum((ct,cf), cars(r,ct,cf,"new")) = sum((ct,cf),carprice(r,ct,cf,"new") * cars(r,ct,cf,"new")) / sum((ct,cf), cars(r,ct,cf,"new")) ;
carprice(r,"TOTAL","AVG","old")$sum((ct,cf), cars(r,ct,cf,"new")) = sum((ct,cf),carprice(r,ct,cf,"old") * cars(r,ct,cf,"old")) / sum((ct,cf), cars(r,ct,cf,"old")) ;
carprice(r,"TOTAL","AVG","AVG")$cars(r,"total","total","stock") = (carprice(r,"TOTAL","AVG","new") * cars(r,"total","total","new") + carprice(r,"TOTAL","AVG","old") * cars(r,"total","total","old")) / cars(r,"total","total","stock") ;

carcostcap(r,ct,cf)              = cars(r,ct,cf,"old") * carprice(r,ct,cf,"old") ;
carcostcap(r,"total","total")    = sum((ct,cf), carcostcap(r,ct,cf));
carcostnew(r,ct,cf)              = cars(r,ct,cf,"new") * carprice(r,ct,cf,"new") ;
carcostnew(r,"total","total")    = sum((ct,cf), carcostnew(r,ct,cf));
carcostenn(r,ct,cf)              = cars(r,ct,cf,"stock") * kmcosten(r,ct,cf) * carkm(r,ct,cf) ;
carcostenn(r,"stock","stock")    = sum((ct,cf), cars(r,ct,cf,"stock") * kmcosten(r,ct,cf) * carkm(r,ct,cf)) ;
carcosteng(r,ct,cf)              = cars(r,ct,cf,"stock") * kmcosteg(r,ct,cf) * carkm(r,ct,cf) ;
carcosteng(r,"stock","stock")    = sum((ct,cf), cars(r,ct,cf,"stock") * kmcosteg(r,ct,cf) * carkm(r,ct,cf)) ;

housecostcap(r,bt,bf)$old(bt)    = houseqm(r,bt,bf) * qmcostc(r,bt,bf) ;
housecostcap(r,"total","total")  = sum((bt,bf), housecostcap(r,bt,bf)) ;
housecostnew(r,bt,bf)$new(bt)    = houseqm(r,bt,bf) * qmcostc(r,bt,bf) ;
housecostnew(r,"total","total")  = sum((bt,bf), housecostnew(r,bt,bf)) ;
housecostenn(r,bt,bf)            = houseqm(r,bt,bf) * qmcosten(r,bt,bf) ;
housecostenn(r,"total","total")  = sum((bt,bf), housecostenn(r,bt,bf)) ;
housecosteng(r,bt,bf)            = houseqm(r,bt,bf) * qmcosteg(r,bt,bf) ;
housecosteng(r,"total","total")  = sum((bt,bf), housecosteng(r,bt,bf)) ;

othercostenn(r,cbt,cbf)          = othereleTWh(r,cbt,cbf,"TWh") * eprice_n(r,cbf) /1000 ;
othercosteng(r,cbt,cbf)          = othereleTWh(r,cbt,cbf,"TWh") * eprice_g(r,cbf) /1000 ;

*display carprice, carcostcap, carcostnew, carcostenn, housecostcap, housecostnew, housecostenn, othercostenn, carcosteng, housecosteng, othercosteng ;

* ------------------------------------------------------------------------------

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

* ------ 21.01.2015 Energy purchases
parameter
         c0_oil(r,i,cbt,cbf)         Fuel transformation from OIL to fuels
         c0_gas(r,i,cbt,cbf)
         c0_ele(r,i,cbt,cbf)
         c0_col(r,i,cbt,cbf)
         c0_agr(r,i,cbt,cbf)

         c0_en(r,i,cbt,cbf)
         c0_eg(r,i,cbt,cbf)

         fbun
         fbug
         oilnrel
         oilgrel
         gasnrel
         gasgrel
         elenrel
         elegrel
         colnrel
         colgrel
         bionrel
         biogrel
         elephenrel
         elephegrel
         oilphenrel
         oilphegrel

         fuel0(r,cbt,cbf)            Fuel demand [former c0]
         fueln0
         fuelg0
         tfuel
         tfuel0
         pfuel0
         tfueli
         tfueli0
         pfueli0
         pcar0
         pbui0
         tcar
         tbui
         tcar0
         tbui0

         chk1            Relative shares of fbunrel must sum up to 1
         chk2            Relative shares of fbugrel must sum up to 1
         chk3            Check if tfuel connects fuelg0 and fuel0 correctly [= 0]
         chk4            Check if twh weighted average of tfuel equals tc for "oil" [= 0]
         chk5
         chk6
         chk7
         chk8
         chk12
         chkc0
         chkc0t
         chkc
         chkstockc
         chkstockh

         snd0
         ssd0
         sy0

         bnd0rel
         bnd0(r,i,bt,bf)
         bsd0
         by0

         sr0by
         sr0qm
;


* ------ Calculate BoUp energy demand values as well as relative shares per energy carrier
fbun(r,cbt,cbf)          = TWh(r,cbt,cbf) * eprice_n(r,cbf) / 1000 ;
fbug(r,cbt,cbf)          = TWh(r,cbt,cbf) * eprice_g(r,cbf) / 1000 ;
oilnrel(r,cbt,cbf)$sum((ccbt,ccbf)$oilc(ccbf), fbun(r,ccbt,ccbf))= fbun(r,cbt,cbf)$oilc(cbf) / sum((ccbt,ccbf)$oilc(ccbf), fbun(r,ccbt,ccbf)) ;
oilgrel(r,cbt,cbf)$sum((ccbt,ccbf)$oilc(ccbf), fbun(r,ccbt,ccbf))= fbug(r,cbt,cbf)$oilc(cbf) / sum((ccbt,ccbf)$oilc(ccbf), fbug(r,ccbt,ccbf)) ;
gasnrel(r,cbt,cbf)$sum((ccbt,ccbf)$oilc(ccbf), fbun(r,ccbt,ccbf))= fbun(r,cbt,cbf)$gass(cbf) / sum((ccbt,ccbf)$gass(ccbf), fbun(r,ccbt,ccbf)) ;
gasgrel(r,cbt,cbf)$sum((ccbt,ccbf)$oilc(ccbf), fbun(r,ccbt,ccbf))= fbug(r,cbt,cbf)$gass(cbf) / sum((ccbt,ccbf)$gass(ccbf), fbug(r,ccbt,ccbf)) ;
elenrel(r,cbt,cbf)$sum((ccbt,ccbf)$oilc(ccbf), fbun(r,ccbt,ccbf))= fbun(r,cbt,cbf)$eles(cbf) / sum((ccbt,ccbf)$eles(ccbf), fbun(r,ccbt,ccbf)) ;
elegrel(r,cbt,cbf)$sum((ccbt,ccbf)$oilc(ccbf), fbun(r,ccbt,ccbf))= fbug(r,cbt,cbf)$eles(cbf) / sum((ccbt,ccbf)$eles(ccbf), fbug(r,ccbt,ccbf)) ;
colnrel(r,cbt,cbf)$sum((ccbt,ccbf)$oilc(ccbf), fbun(r,ccbt,ccbf))= fbun(r,cbt,cbf)$cols(cbf) / sum((ccbt,ccbf)$cols(ccbf), fbun(r,ccbt,ccbf)) ;
colgrel(r,cbt,cbf)$sum((ccbt,ccbf)$oilc(ccbf), fbun(r,ccbt,ccbf))= fbug(r,cbt,cbf)$cols(cbf) / sum((ccbt,ccbf)$cols(ccbf), fbug(r,ccbt,ccbf)) ;
bionrel(r,cbt,cbf)$sum((ccbt,ccbf)$oilc(ccbf), fbun(r,ccbt,ccbf))= fbun(r,cbt,cbf)$bios(cbf) / sum((ccbt,ccbf)$bios(ccbf), fbun(r,ccbt,ccbf)) ;
biogrel(r,cbt,cbf)$sum((ccbt,ccbf)$oilc(ccbf), fbun(r,ccbt,ccbf))= fbug(r,cbt,cbf)$bios(cbf) / sum((ccbt,ccbf)$bios(ccbf), fbug(r,ccbt,ccbf)) ;

$ontext
* ------ Check relative proportions (must sum up to 1)
chk1(r) = 0; chk1(r) = sum((cbt,cbf), oilnrel(r,cbt,cbf)); display chk1;
chk2(r) = 0; chk2(r) = sum((cbt,cbf), oilgrel(r,cbt,cbf)); display chk2;
chk1(r) = 0; chk1(r) = sum((cbt,cbf), gasnrel(r,cbt,cbf)); display chk1;
chk2(r) = 0; chk2(r) = sum((cbt,cbf), gasgrel(r,cbt,cbf)); display chk2;
chk1(r) = 0; chk1(r) = sum((cbt,cbf), elenrel(r,cbt,cbf)); display chk1;
chk2(r) = 0; chk2(r) = sum((cbt,cbf), elegrel(r,cbt,cbf)); display chk2;
chk1(r) = 0; chk1(r) = sum((cbt,cbf), colnrel(r,cbt,cbf)); display chk1;
chk2(r) = 0; chk2(r) = sum((cbt,cbf), colgrel(r,cbt,cbf)); display chk2;
chk1(r) = 0; chk1(r) = sum((cbt,cbf), bionrel(r,cbt,cbf)); display chk1;
chk2(r) = 0; chk2(r) = sum((cbt,cbf), biogrel(r,cbt,cbf)); display chk2;
$offtext

* ------ PHEV aggregation
elephenrel(r,cbt,cbf)$fbun(r,cbt,cbf) = fbun(r,cbt,cbf)$(eles(cbf) or phe(cbf)) / sum((ccbt,ccbf)$(eles(ccbf) or phe(ccbf)), fbun(r,ccbt,ccbf)) ;
elephegrel(r,cbt,cbf)$fbug(r,cbt,cbf) = fbug(r,cbt,cbf)$(eles(cbf) or phe(cbf)) / sum((ccbt,ccbf)$(eles(ccbf) or phe(ccbf)), fbug(r,ccbt,ccbf)) ;
elephenrel(r,"sum","sum") = sum((cbt,cbf), elephenrel(r,cbt,cbf));
elephegrel(r,"sum","sum") = sum((cbt,cbf), elephegrel(r,cbt,cbf));

oilphenrel(r,cbt,cbf)$fbun(r,cbt,cbf) = fbun(r,cbt,cbf)$(oilc(cbf) or phe(cbf)) / sum((ccbt,ccbf)$(oilc(ccbf) or phe(ccbf)), fbun(r,ccbt,ccbf)) ;
oilphegrel(r,cbt,cbf)$fbug(r,cbt,cbf) = fbug(r,cbt,cbf)$(oilc(cbf) or phe(cbf)) / sum((ccbt,ccbf)$(oilc(ccbf) or phe(ccbf)), fbug(r,ccbt,ccbf)) ;
oilphenrel(r,"sum","sum") = sum((cbt,cbf), oilphenrel(r,cbt,cbf));
oilphegrel(r,"sum","sum") = sum((cbt,cbf), oilphegrel(r,cbt,cbf));

display twh, fbun, fbug, oilnrel, oilgrel, gasnrel, gasgrel, elenrel, elegrel, colnrel, colgrel, bionrel, biogrel, elephenrel, elephegrel, oilphenrel, oilphegrel;

* ------ Calibrate oil purchases (net/gross) and taxes to GTAP values --> fuel0(r,cbf)
fueln0(r,cbt,cbf)$(oilc(cbf) or phe(cbf))= oilphenrel(r,cbt,cbf) * c0("oil",r) ;
fueln0(r,cbt,cbf)$(eles(cbf) or phe(cbf))= elephenrel(r,cbt,cbf) * c0("ele",r) + fueln0(r,cbt,cbf)$phe(cbf);
fueln0(r,cbt,cbf)$(gass(cbf))            = gasnrel(r,cbt,cbf)    * c0("gas",r) ;
fueln0(r,cbt,cbf)$(cols(cbf))            = colnrel(r,cbt,cbf)    * c0("col",r) ;
fueln0(r,cbt,cbf)$(bios(cbf))            = fbun(r,cbt,cbf);

fuelg0(r,cbt,cbf)$(oilc(cbf) or phe(cbf))= oilphegrel(r,cbt,cbf) * c0("oil",r)*(1+tc("oil",r)) ;
fuelg0(r,cbt,cbf)$(eles(cbf) or phe(cbf))= elephegrel(r,cbt,cbf) * c0("ele",r)*(1+tc("ele",r)) + fuelg0(r,cbt,cbf)$phe(cbf) ;
fuelg0(r,cbt,cbf)$(gass(cbf))            = gasgrel(r,cbt,cbf)    * c0("gas",r)*(1+tc("gas",r)) ;
fuelg0(r,cbt,cbf)$(cols(cbf))            = colgrel(r,cbt,cbf)    * c0("col",r)*(1+tc("col",r)) ;
fuelg0(r,cbt,cbf)$(bios(cbf))            = fbun(r,cbt,cbf) * (1+tc("agr",r)) ;

fuel0(r,cbt,cbf)           = fueln0(r,cbt,cbf) ;

* ------ Energy purchases c0_en = fuel0n = fuel0 = (c0_oil, etc.)
c0_oil(r,"oil",cbt,cbf)        = oilphenrel(r,cbt,cbf) * c0("oil",r) ;
c0_ele(r,"ele",cbt,cbf)$elephenrel(r,cbt,cbf)    = elephenrel(r,cbt,cbf) * c0("ele",r) ;
c0_gas(r,"gas",cbt,cbf)$gasnrel(r,cbt,cbf)       = gasnrel(r,cbt,cbf) * c0("gas",r) ;
c0_col(r,"col",cbt,cbf)$colnrel(r,cbt,cbf)       = colnrel(r,cbt,cbf) * c0("col",r) ;
c0_agr(r,"agr",cbt,cbf)$fbun(r,cbt,cbf)          = fbun(r,cbt,cbf)$bios(cbf);

c0_en(r,"oil",cbt,cbf) = c0_oil(r,"oil",cbt,cbf) ;
c0_en(r,"ele",cbt,cbf) = c0_ele(r,"ele",cbt,cbf) ;
c0_en(r,"gas",cbt,cbf) = c0_gas(r,"gas",cbt,cbf) ;
c0_en(r,"col",cbt,cbf) = c0_col(r,"col",cbt,cbf) ;
c0_en(r,"agr",cbt,cbf) = c0_agr(r,"agr",cbt,cbf) ;

c0_eg(r,"oil",cbt,cbf) = oilphegrel(r,cbt,cbf) * c0("oil",r)*(1+tc("oil",r)) ;
c0_eg(r,"ele",cbt,cbf) = elephegrel(r,cbt,cbf) * c0("ele",r)*(1+tc("ele",r)) ;
c0_eg(r,"gas",cbt,cbf) = gasgrel(r,cbt,cbf)    * c0("gas",r)*(1+tc("gas",r)) ;
c0_eg(r,"col",cbt,cbf) = colgrel(r,cbt,cbf)    * c0("col",r)*(1+tc("col",r)) ;
c0_eg(r,"agr",cbt,cbf) = fbun(r,cbt,cbf) * (1+tc("agr",r)) ;

*display fuel0, fueln0, fuelg0, c0_en, c0_eg;

* ------ Taxes calibration
tfuel(r,cbt,cbf)$fueln0(r,cbt,cbf) = fuelg0(r,cbt,cbf) /fueln0(r,cbt,cbf)  - 1 ;
tfueli(r,i,cbt,cbf)$c0_en(r,i,cbt,cbf)            = c0_eg(r,i,cbt,cbf)/c0_en(r,i,cbt,cbf) - 1 ;
tcar(r,ct,cf)$cars(r,ct,cf,"new")= tc("mvh",r) ;
tbui(r,new,bf)$houseqm(r,new,bf) = tc("bui",r) ;

tcar0(r,ct,cf)           = tcar(r,ct,cf);
tbui0(r,bt,bf)           = tbui(r,bt,bf);
tfuel0(r,cbt,cbf)        = tfuel(r,cbt,cbf);
tfueli0(r,i,cbt,cbf)     = tfueli(r,i,cbt,cbf);
pfuel0(r,cbt,cbf)$tfuel(r,cbt,cbf)       = 1 + tfuel0(r,cbt,cbf) ;
pfueli0(r,i,cbt,cbf)$tfueli(r,i,cbt,cbf) = 1 + tfueli0(r,i,cbt,cbf) ;
pcar0(r,ct,cf)$tcar0(r,ct,cf)    = 1 + tcar0(r,ct,cf);
pbui0(r,bt,bf)$tbui0(r,bt,bf)    = 1 + tbui0(r,bt,bf);

display tc, tcar, tfuel, tfueli, tbui;
display pc0, pcar0, pfuel0, pfueli0, pbui0;

* ------ Assign total mvh demand to new ("mvh") and old car demand of stock activity
*ssd0(r,ct,cf)            = HH_boup(r,ct,cf,"€/km(K)") * carkm(r,ct,cf)             * cars(r,ct,cf,"old") ;
*snd0(r,"mvh",ct,cf)      = HH_boup(r,ct,cf,"€/km(K)") * carkm(r,ct,cf) * carfactor * cars(r,ct,cf,"new") ;
ssd0(r,ct,cf)            = kmcostc(r,ct,cf) * carkm(r,ct,cf)             * cars(r,ct,cf,"old") ;
snd0(r,"mvh",ct,cf)      = kmcostc(r,ct,cf) * carkm(r,ct,cf) * carfactor * cars(r,ct,cf,"new") ;
sy0(r,ct,cf)             = ssd0(r,ct,cf) + snd0(r,"mvh",ct,cf)*pcar0(r,ct,cf) ;


* ------ Buildings capital costs for purchases (new buildings) and rents (existing buildings)
bnd0rel(r,bt,bf)$sum((bbt,bbf), housecostnew(r,bbt,bbf)) = housecostnew(r,bt,bf)/sum((bbt,bbf), housecostnew(r,bbt,bbf));
bnd0rel(r,"sum","sum")   = sum((bt,bf), bnd0rel(r,bt,bf)) ;
bnd0(r,"bui",bt,bf)$bnd0rel(r,bt,bf)=bnd0rel(r,bt,bf) * c0("bui",r) ;
bsd0(r,bt,bf)$old(bt)    = houseqm(r,bt,bf) * qmcostc(r,bt,bf) ;
*bsd0("DEU",bt,bf) = bsd0("DEU",bt,bf) + 1e-6 ;
*bsd0(r,bt,bf)            = houseqm(r,bt,bf) * qmcostc(r,bt,bf) ;
bnd0(r,"ser",bt,bf)$housecostnew(r,bt,bf)=housecostnew(r,bt,bf) - bnd0rel(r,bt,bf) * c0("bui",r) ;
by0(r,bt,bf)             = bsd0(r,bt,bf) + bnd0(r,"bui",bt,bf)*(1+tc("bui",r)) + bnd0(r,"ser",bt,bf)*(1+tc("ser",r));


* ------ 4.01.2016
parameter ssd0price, snd0price, pc0_serbui;
ssd0price(r,ct,cf) = kmcostc(r,ct,cf) * carkm(r,ct,cf) ;
snd0price(r,ct,cf) = kmcostc(r,ct,cf) * carkm(r,ct,cf) * carfactor ;

pc0_serbui(r,bt,bf)$sum(i, bnd0(r,i,bt,bf)) = (sum(i, bnd0(r,i,bt,bf)*pc0(i,r))) / (sum(i, bnd0(r,i,bt,bf)))

display snd0, ssd0, sy0, ssd0price, snd0price, qmcostc, pc0_serbui, pc0;
*$exit


*parameter chkbnd;
*chkbnd(r) = sum((bt,bf), (bnd0(r,"bui",bt,bf)*(1+tc("bui",r)) + bnd0(r,"ser",bt,bf)*(1+tc("ser",r)))); display chkbnd;
*chkbnd(r) = sum((bt,bf), (bnd0(r,"bui",bt,bf))); display chkbnd;
*chkbnd(r) = sum((bt,bf), bnd0(r,"ser",bt,bf)); display chkbnd;
*chkbnd(r) = sum((bt,bf), (bnd0(r,"ser",bt,bf)+bnd0(r,"bui",bt,bf))); display chkbnd;

* ------ 17.02.2015
sr0qm(r)$sum((bt,bf), houseqm(r,bt,bf))  = sum(bf, houseqm(r,"old_r",bf))/ sum((bt,bf)$oldxr(bt), houseqm(r,bt,bf)) * 100;
sr0by(r)$sum((bt,bf), by0(r,bt,bf))      = sum(bf, by0(r,"old_r",bf))    / sum((bf,bt)$oldxr(bt), by0(r,bt,bf)) * 100;

display bnd0rel, bnd0, bsd0, by0, c0, sr0qm, sr0by;



* ------ 13.09.2015 Es wird ein zusätzliches Kapitelgut für die neuen Fahrzeuge definiert, dass auch ins Endowment einfließt, um es für die Dynamik kontinuierlich erhöhen zu können
*parameter snd0c(r,ct,cf), sndc(r,ct,cf);
*snd0c(r,ct,cf)$snd0(r,"mvh",ct,cf) = snd0(r,"mvh",ct,cf) ;
*sndc(r,ct,cf)$snd0(r,"mvh",ct,cf)  = snd0(r,"mvh",ct,cf) ;

*snd0c(r,ct,cf)$snd0(r,"mvh",ct,cf)= 1e-6 ;
*sndc(r,ct,cf)$snd0(r,"mvh",ct,cf) = 1e-6 ;

*ssd0(r,ct,cf)    = ssd0(r,ct,cf) - snd0c(r,ct,cf)*pcar0(r,ct,cf);
*sy0(r,ct,cf)     = ssd0(r,ct,cf) + snd0(r,"mvh",ct,cf)*pcar0(r,ct,cf) + snd0c(r,ct,cf)*pcar0(r,ct,cf);

*display snd0, snd0c, snd0c, ssd0, sy0, c0;


* ------ CHECK c0_en
parameter constrr(r);
constrr(r)$sum((i,cbt,cbf), c0_en(r,i,cbt,cbf)) = yes; display constrr;

chk12(r) = 0; chk12(r)$constrr(r) = round(
*                         + sum((i,cbt,cbf), c0_en(r,i,cbt,cbf)*(1+tfuel(r,cbt,cbf)))
                         + sum((i,cbt,cbf), c0_en(r,i,cbt,cbf)*(1+tfueli(r,i,cbt,cbf)))
                         - (c0("oil",r)*(1+tc("oil",r)))
                         - (c0("ele",r)*(1+tc("ele",r)))
                         - (c0("gas",r)*(1+tc("gas",r)))
                         - (c0("col",r)*(1+tc("col",r)))
                         - sum(cbt, fbun(r,cbt,"bio")*(1+tc("agr",r)))   ,12); display chk12;

chk12(r) = 0; chk12(r)$constrr(r) = round(
                         + sum((i,cbt,cbf), c0_en(r,i,cbt,cbf))
                         - (c0("oil",r))
                         - (c0("ele",r))
                         - (c0("gas",r))
                         - (c0("col",r))
                         - sum(cbt, fbun(r,cbt,"bio"))   ,12); display chk12;

display chk12;
display c0;
* ------ Correct final demand for energy purchases and new assigned mvh, bui and ser demand
c0("mvh",r)              = c0("mvh",r) - sum((ct,cf), snd0(r,"mvh",ct,cf));
c0("bui",r)              = c0("bui",r) - sum((bt,bf), bnd0(r,"bui",bt,bf));
c0("ser",r)              = c0("ser",r) - sum((bt,bf), bnd0(r,"ser",bt,bf));

c0("oil",r)              = c0("oil",r) - sum((cbt,cbf), c0_oil(r,"oil",cbt,cbf)) ;
c0("gas",r)              = c0("gas",r) - sum((cbt,cbf), c0_gas(r,"gas",cbt,cbf)) ;
c0("ele",r)              = c0("ele",r) - sum((cbt,cbf), c0_ele(r,"ele",cbt,cbf)) ;
c0("col",r)              = c0("col",r) - sum((cbt,cbf), c0_col(r,"col",cbt,cbf)) ;
c0("agr",r)              = c0("agr",r) - sum((cbt,cbf), c0_agr(r,"agr",cbt,cbf)) ;

* ------ Set values smaller than 1E-14 equal to zero! --------------------------
c0(i,"deu")$(c0(i,"deu") lt 1e-14)  = 0;
*c0(i,"baw")$(c0(i,"baw") lt 1e-14)  = 0;
display c00, c0;


* ------ CHECK assignment of c0 ------------------------------------------------

chk3(r,cbt,cbf)  = round(fuelg0(r,cbt,cbf) - (fueln0(r,cbt,cbf) * (1+tfuel(r,cbt,cbf))),12) ; display chk3;

chk12(r) = 0; chk12(r) = round(sum((i,cbt,cbf), c0_en(r,i,cbt,cbf)) - sum((cbt,cbf), fuel0(r,cbt,cbf)),12) ; display chk12;
chk12(r) = 0; chk12(r) = round(sum((i,cbt,cbf), c0_en(r,i,cbt,cbf)) - sum((cbt,cbf), fueln0(r,cbt,cbf)),12) ; display chk12;
chk12(r) = 0; chk12(r) = round(sum((i,cbt,cbf), c0_en(r,i,cbt,cbf)*pfuel0(r,cbt,cbf)) - sum((cbt,cbf), fuelg0(r,cbt,cbf)),12) ; display chk12;

chkc0(r)$constrr(r) = round(
                 + SUM(i, c00(i,r))
                 - sum(i, c0(i,r))
                 - sum((i,bt,bf), bnd0(r,i,bt,bf))
                 - sum((ct,cf), snd0(r,"mvh",ct,cf))
                 - sum((cbt,cbf), c0_oil(r,"oil",cbt,cbf))
                 - sum((cbt,cbf), c0_gas(r,"gas",cbt,cbf))
                 - sum((cbt,cbf), c0_ele(r,"ele",cbt,cbf))
                 - sum((cbt,cbf), c0_col(r,"col",cbt,cbf))
                 - sum((cbt,cbf), c0_agr(r,"agr",cbt,cbf)), 12) ;

chkc0t(r)$constrr(r) = round(
                 + SUM(i, c00(i,r)*(1+tc(i,r)))
                 - sum(i, c0(i,r)*(1+tc(i,r)))
                 - sum((bt,bf),  bnd0(r,"bui",bt,bf)*(1+tc("bui",r)))
                 - sum((bt,bf),  bnd0(r,"ser",bt,bf)*(1+tc("ser",r)))
                 - sum((ct,cf),  snd0(r,"mvh",ct,cf)*(1+tc("mvh",r)))
                 - sum((cbt,cbf),c0_oil(r,"oil",cbt,cbf)*(1+tfuel(r,cbt,cbf)))
                 - sum((cbt,cbf),c0_ele(r,"ele",cbt,cbf)*(1+tfuel(r,cbt,cbf)))
                 - sum((cbt,cbf),c0_gas(r,"gas",cbt,cbf)*(1+tfuel(r,cbt,cbf)))
                 - sum((cbt,cbf),c0_col(r,"col",cbt,cbf)*(1+tfuel(r,cbt,cbf)))
                 - sum((cbt,cbf),c0_agr(r,"agr",cbt,cbf)*(1+tfuel(r,cbt,cbf)))
, 12) ;

chkc0(r)$(not constrr(r))  = round(
                 + SUM(i, c00(i,r))
                 - sum(i, c0(i,r)), 12) ;
chkc0t(r)$(not constrr(r)) = round(
                 + SUM(i, c00(i,r)*(1+tc(i,r)))
                 - sum(i, c0(i,r) *(1+tc(i,r))), 12)   ;

chkstockc(r) = round(sum((ct,cf), sy0(r,ct,cf) - ssd0(r,ct,cf) - snd0(r,"mvh",ct,cf)*pcar0(r,ct,cf)),12);
chkstockh(r) = round(sum((bt,bf), by0(r,bt,bf) - bsd0(r,bt,bf) - bnd0(r,"bui",bt,bf)*pc0("bui",r) - bnd0(r,"ser",bt,bf)*pc0("ser",r)),12);

chkc(r)= round(vom("c",r) + vom("g",r) + sum((ct,cf), ssd0(r,ct,cf)) + sum((bt,bf), bsd0(r,bt,bf))
         - sum(i, c0(i,r) * pc0(i,r))
         - sum((ct,cf), sy0(r,ct,cf))
         - sum((bt,bf), by0(r,bt,bf))
         - sum((i,cbt,cbf), c0_en(r,i,cbt,cbf) * pfueli0(r,i,cbt,cbf))
*         - sum((i,cbt,cbf), c0_en(r,i,cbt,cbf) * pfuel0(r,cbt,cbf))
*         - sum((cbt,cbf), fuel0(r,cbt,cbf) * pfuel0(r,cbt,cbf))
,11);

display chkc0, chkc0t, chkstockc, chkstockh, chkc;

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

PARAMETER
         c0_trn(i,r)
         cartout(r,ct)
         heatout(r,bt)
         cartfout(r,ct,cf)
         heatfout(r,bt,bf)
         esdout(r)
         otherele(r)
         tothele0(r)
         pothele0(r)
         c0_fuels
;

c0_trn(i,r)$(trn(i) and constrr(r))  = c0(i,r) ;
c0(i,r)$(trn(i) and constrr(r))      = 0 ;

cartfout(r,ct,cf) =      + sy0(r,ct,cf)
                         + c0_en(r,"ele",ct,cf)$eles(cf)*aeei("ele","c",r) * pfueli0(r,"ele","small","ele")
                         + c0_en(r,"ele",ct,cf)$phe(cf)*aeei("ele","c",r) * pfueli0(r,"ele","large","phe")
                         + c0_en(r,"gas",ct,cf)*aeei("gas","c",r)    * pc0("gas",r)
                         + c0_en(r,"oil",ct,cf)*aeei("oil","c",r)* pfueli0(r,"oil",ct,cf)       ;

heatfout(r,bt,bf) =      + by0(r,bt,bf)
                         + c0_en(r,"ele",bt,bf)$eles(bf)*aeei("ele","c",r)*pfueli0(r,"ele","old_x","ele")
                         + c0_en(r,"gas",bt,bf)*aeei("gas","c",r)        * pc0("gas",r)
                         + c0_en(r,"col",bt,bf)*aeei("col","c",r)        * pc0("col",r)
                         + c0_en(r,"oil",bt,bf)*aeei("oil","c",r)        * pfueli0(r,"oil",bt,bf)
                         + c0_en(r,"agr",bt,bf)*aeei("agr","c",r)        * pfueli0(r,"agr",bt,bf) ;

cartout(r,ct)    =       sum(cf, cartfout(r,ct,cf)) ;
heatout(r,bt)    =       sum(bf, heatfout(r,bt,bf)) ;

otherele(r)      =       sum((cbt,cbf)$(eles(cbf) and other(cbt)), c0_en(r,"ele",cbt,cbf)) ;
tothele0(r)      =       tfueli(r,"ele","other","ele") ;
pothele0(r)      =       1 + tothele0(r);

esdout(r)        =       + sum(ct, cartout(r,ct))
                         + sum(bt, heatout(r,bt))
                         + c0_trn("trn",r) * pc0("trn",r)
                         + otherele(r) * pothele0(r)
;


c0_fuels(r,i,cbt,cbf)$fe(i) = c0_en(r,i,cbt,cbf);

c0_fuels(r,i,cbt,cbf)$fe(i) = c0_en(r,i,cbt,cbf);



*c0_fuels(r,i,cbt,cbf)$fe(i) = c0_en(r,i,cbt,cbf)*pfueli0(r,i,cbt,cbf) ;

*c0_fuels(r,i,cbt,cbf)$oil(i) = c0_en(r,i,cbt,cbf)*pfueli0(r,i,cbt,cbf) ;
*c0_fuels(r,i,cbt,cbf)$gas(i) = c0_en(r,i,cbt,cbf)*pfueli0(r,i,cbt,cbf) ;
*c0_fuels(r,i,cbt,cbf)$col(i) = c0_en(r,i,cbt,cbf)*pfueli0(r,i,cbt,cbf) ;

display sy0, c0_en, tfueli, pfueli0, pc0, cartfout, heatfout, cartout, heatout, c0, c0_trn, esdout, otherele, tothele0, pothele0, c0_fuels;

* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

* ------ 4.02.2015 CO2-Emissionen ----------------------------------------------

parameter
         co2em_sum
         cbco2(r,cbt,cbf)        CO2-Emissions of households [Gt CO2]
cbco2i
         cbco2_sum               CO2-Emissions of households - sums [Gt CO2]
* ------ www.umweltbundesamt.de/sites/default/files/medien/377/dokumente/co2_faktoren_brennstoffe.xls:
         cecphys2(cbf)           Emissions coefficients [tCO2 per TJ = gCO2 per MJ]
                                 / ben 72, die 74, hoi 74, gas 56, col 103 /
;

display co2em, cecphys, cecphys2, co2em_total, eco2;


co2em_sum(r,"final")     = sum(i, co2em(i,"final",r)) ;
co2em_sum(r,"ELE")       = sum(i, co2em(i,"ELE",r)) ;
co2em_sum(r,"Y")         = sum((fe,i)$(not ele(i)), co2em(fe,i,r)) ;
co2em_sum(r,"total")     = co2em_sum(r,"final") + co2em_sum(r,"ELE") + co2em_sum(r,"Y") ;

display  co2em_sum;

* ------ UGR_2010 - Teil_3.xls: CO2-Emissionen HH in Gesamt-DEU:        179,390 Mio.tCO2
* ------ UGR_2010 - Teil_3.xls: Gesamt-CO2-Emissionen in Gesamt-DEU:    882,561 Mio.tCO2

cbco2(r,cbt,cbf)         = cecphys2(cbf)                 * TWh(r,cbt,cbf)        * PJ_TWh / 1e6 ;
cbco2(r,cbt,"phe")       = cecphys2("ben") * (1-phev_ele)* TWh(r,cbt,"phe")      * PJ_TWh / 1e6 ;


cbco2i(r,i,cbt,cbf)$gas(i) = cbco2(r,cbt,cbf)$gass(cbf) ;
cbco2i(r,i,cbt,cbf)$col(i) = cbco2(r,cbt,cbf)$cols(cbf) ;
cbco2i(r,i,cbt,cbf)$oil(i) = cbco2(r,cbt,cbf)$(oilc(cbf) or phe(cbf)) ;
display cbco2, cbco2i;


cbco2_sum(r,co2cbf)      = sum(cbt, cbco2(r,cbt,co2cbf)) ;
cbco2_sum(r,"TOTAL")     = sum((cbt,co2cbf), cbco2(r,cbt,co2cbf)) ;

display cbco2, cbco2_sum ;


* ------ ADJUST existing CO2-Emissions -----------------------------------------
co2em(i,"final",r)$sum((cbt,cbf), cbco2(r,cbt,cbf)) = 0 ;
* ------------------------------------------------------------------------------

co2em_sum(r,"final")     = sum(i, co2em(i,"final",r)) ;
co2em_sum(r,"ELE")       = sum(i, co2em(i,"ELE",r)) ;
co2em_sum(r,"Y")         = sum((fe,i)$(not ele(i)), co2em(fe,i,r)) ;
co2em_sum(r,"C+B")       = sum((cbt,co2cbf), cbco2(r,cbt,co2cbf));
co2em_sum(r,"total")     = co2em_sum(r,"final") + co2em_sum(r,"ELE") + co2em_sum(r,"Y") + co2em_sum(r,"C+B") ;


display co2em_sum, co2em;


parameter chk, chk_c0fuels;
chk(r,cbt,cbf) = sum(i, cbco2i(r,i,cbt,cbf)) -  cbco2(r,cbt,cbf);
chk_c0fuels(r)$sum((fe,cbt,cbf), c0_fuels(r,fe,cbt,cbf)) = round(sum((fe,cbt,cbf), c0_fuels(r,fe,cbt,cbf)) - sum(fe, c00(fe,r)),9);
display chk, chk_c0fuels;


* ##############################################################################

parameter
         gco2kwh
         houseco2 CO2 per m² [gCO2 per m²]
         tco2qm
         kwhqm
;
* ------ gco2kwh(r,bt,bf): PJ/TWh = TJ/GWh --> tCO2/TJ * TJ/GWh = tCO2/GWh = gCO2/kWh
kwhqm(r,bt,bf)   = housekwh(r,bt,bf) ;
gco2kwh(cbf)     = cecphys2(cbf) * PJ_TWh ;
tco2qm(r,bt,bf)  = gco2kwh(bf) * kwhqm(r,bt,bf) / 1e6;
display gco2kwh, tco2qm, kwhqm, cecphys2, PJ_TWh;


* ##############################################################################
* ------ CO2-tax

parameter
         c0_sum
         co2tax
         co2tax0
         co2price        CO2-Preis für Berechnung der CO2-Steuer [€ per tCO2 = Mrd. € per Mrd. tCO2]
                         / 30 /
         rev_price
         rev_price2
;
c0_sum(r,"c0_en") = sum((i,cbt,cbf), c0_en(r,i,cbt,cbf)) ; display c0_sum;
c0_sum(r,"c0_fuels") = sum((i,cbt,cbf), c0_fuels(r,i,cbt,cbf)) ; display c0_sum;

*30€/tCO2 = 30 Mrd. €/Mrd. tCO2 --> 30 * 0.195 = 5.85 Mrd. € für 195 Mio. tCO2 bei 24.612 Mrd. € Fuel-Kosten --> tco2 = 24 %!

rev_price(r,cbf)$sum(cbt, TWh(r,cbt,cbf)) = 1000 * sum((i,cbt), c0_en(r,i,cbt,cbf)) / sum(cbt, TWh(r,cbt,cbf)) ;

* ----- 06.02.2015 Berechnung der CO2-Steuer als Wertsteuer unter Berücksichtigung der CO2-Intensität = CO2-Wert / Gesamtwert des Energieträgers
co2tax(r,i,cbt,cbf) = 0;
co2tax0(r,i,cbt,cbf)$c0_fuels(r,i,cbt,cbf) = cbco2i(r,i,cbt,cbf) * co2price / c0_fuels(r,i,cbt,cbf) ;

rev_price2(r,cbf)$eprice_n(r,cbf) = rev_price(r,cbf) / eprice_n(r,cbf)  ;

*display rev_price, eprice_n, rev_price2, fbun, twh, c0_fuels, cbco2i, co2tax0, c0_oil, c0_ele;


*display tfuel, tfueli, pfuel0, pfueli0;

*display eprice_n, eprice_g ;

* ##############################################################################
* ------ UNLOAD DATA


execute_unload '%datadir%hh_data.gdx'

$exit
