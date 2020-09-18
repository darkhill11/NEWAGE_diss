$title	Read the IEO 2013 Appendix A Workbook (reference case) 

$call  'xlsdump xls\ieorefcase.xls'
$gdxin 'xls\ieorefcase.gdx'

$include layout

parameter
	a1(*,*)		"World Total Primary Energy Consumption by Region, Reference Case, 2005-2040 (Quadrillion Btu)"
	a2(*,*,*)	"World Total Energy Consumption by Region and Fuel, Reference Case, 2005-2040 (Quadrillion Btu)"
	a3(*,*)		"World Gross Domestic Product (GDP) by Region Expressed in Purchasing Power Parity, Reference Case, 2005-2040 (Billion 2005 Dollars)"
	a4(*,*)		"World Gross Domestic Product (GDP) by Region Expressed in Market Exchange Rates, Reference Case, 2005-2040 (Billion 2005 Dollars)"
	a5(*,*)		"World Liquids Consumption by Region, Reference Case, 2005-2040 (Million Barrels per Day)"
	a6(*,*)		"World Natural Gas Consumption by Region, Reference Case, 2005-2040 (Trillion Cubic Feet)"
	a7(*,*)		"World Coal Consumption by Region, Reference Case, 2005-2040 (Quadrillion Btu)"
	a8(*,*)		"World Nuclear Energy Consumption by Region, Reference Case, 2005-2040 (Billion Kilowatthours)"
	a9(*,*)		"World Consumption of Hydroelectricity and Other Renewable Energy by Region, Reference Case, 2005-2040 (Quadrillion Btu)"
	a10(*,*)	"World Carbon Dioxide Emissions by Region, Reference Case, 2005-2040 (Million Metric Tons Carbon Dioxide)"
	a11(*,*)	"World Carbon Dioxide Emissions from Liquids Use by Region, Reference Case, 2005-2040 (Million Metric Tons Carbon Dioxide)"
	a12(*,*)	"World Carbon Dioxide Emissions from Natural Gas Use by Region, Reference Case, 2005-2040 (Million Metric Tons Carbon Dioxide)"
	a13(*,*)	"World Carbon Dioxide Emissions from Coal Use by Region, Reference Case, 2005-2040 (Million Metric Tons Carbon Dioxide)"
	a14(*,*)	"World Population by Region, Reference Case, 2005-2040 (Millions)";


*	Select year based on values appearing in row 11:

loop((sn,cn,yy)$(vf(sn,"r4",cn)=yy.val),

*	Keep track of years which are provided:

	yr(yy) = yes;

	loop(enmap(rn,r,en), loop(ws(sn,"Table A2"), a2(r,en,yy) = vf(sn,rn,cn);  vf(sn,rn,cn)=0; ););

	loop(rgmap(rn,r),
	  if(ws(sn,"Table A1"), a1(r,yy) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  if(ws(sn,"Table A3"), a3(r,yy) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  if(ws(sn,"Table A4"), a4(r,yy) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  if(ws(sn,"Table A5"), a5(r,yy) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  if(ws(sn,"Table A6"), a6(r,yy) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  if(ws(sn,"Table A7"), a7(r,yy) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  if(ws(sn,"Table A8"), a8(r,yy) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  if(ws(sn,"Table A9"), a9(r,yy) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  if(ws(sn,"Table A10"), a10(r,yy) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  if(ws(sn,"Table A11"), a11(r,yy) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  if(ws(sn,"Table A12"), a12(r,yy) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  if(ws(sn,"Table A13"), a13(r,yy) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  if(ws(sn,"Table A14"), a14(r,yy) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	);
	
*	Reset active year:

	yr(yy) = no;

);

set	dropr(rn)	Row to be ignored  /r4/, 
	dropc(cn)	Column to be ignored /c38/;
vf(sn,dropr(rn),cn) = 0;
vf(sn,rn,dropc(cn)) = 0;
abort$card(vf) "Not all data have been processed:",vf;


*	Perform a geometric extrapolation to produce numbers for 2004 and post-2040: 

$batinclude extrap r a1 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14

set ren(*,*);
ren(r,en) = yes;
$batinclude extrap ren a2

parameter	a(*,r,*)	Reference Growth Path;
a("energy",r,y)$a1(r,y)		= a1(r,y);
a("gdp",r,y)$a3(r,y)		= a3(r,y);
a("gdp_mkt",r,y)$a4(r,y)	= a4(r,y);
a("liquids_mbd",r,y)$a5(r,y)	= a5(r,y);
a("gas_tcf",r,y)$a6(r,y)	= a6(r,y);
a("col_quad",r,y)$a7(r,y)	= a7(r,y);
a("nuc_bkwh",r,y)$a8(r,y)	= a8(r,y);
a("renew",r,y)$a9(r,y)		= a9(r,y);
a("co2",r,y)$a10(r,y)		= a10(r,y);
a("co2liquids",r,y)		= a11(r,y);
a("co2gas",r,y)			= a12(r,y);
a("co2col",r,y)			= a13(r,y);
a("population",r,y)		= a14(r,y);



*	------------------- Updating and completing of Table a2 ==> a(en,r,yr) -----------------
*	Share out the BTU energy data using regional shares of physical consumption:

parameter	rshare(*,*,*,yy)	Regional shares
		subsum			Regional sums;

subsum(rd,yr,enshare(en)) = sum(rr$rmap(rd,rr),a(en,rr,yr));
rshare(enshare(en),rmap(rd,r),yr)$subsum(rd,yr,en) 
	= a(en,r,yr) / subsum(rd,yr,en);

parameter	chksum	Cross check on regional shares;
chksum(enshare(en),rd,yr)$sum(r$rmap(rd,r),a(en,r,yr)) = round(1 - sum(r,rshare(en,rd,r,yr)),6);
abort$card(chksum) "Error in checksum:", chksum;

set	collision;
collision(en,r,y)$a(en,r,y) = yes$a2(r,en,y);
display collision;
abort$card(collision) "Data collision when loading table A.";

*	Read in energy consumption in quads here:

a(en,r,y)$a2(r,en,y) = a2(r,en,y);
loop(rmap(rd,r), a(en,r,yr)$a2(rd,en,yr) = rshare(en,rd,r,yr) * a2(rd,en,yr););


option a:3:1:1;
display a;

*	Some code to check the growth path:

set fuel /co2liquids,co2gas,co2col/;

parameter	co2emit	Global carbon emissions;
co2emit(yy,fuel) = na;
co2emit(yy,"total") = na;

co2emit(y,fuel)$a(fuel,"world",y) = a(fuel,"world",y);
co2emit(y,"total")$sum(fuel,co2emit(y,fuel)) = sum(fuel,co2emit(y,fuel));

set	rg(r) /can,usa,oecdeur,jpn,chn,ind,world/

parameter	regco2	Regional carbon emissions;
regco2(yy,rg) = na;
regco2(yy,rg)$sum(fuel, a(fuel,rg,yy)) = sum(fuel, a(fuel,rg,yy));

$setglobal domain yy
$setglobal labels yr

regco2(yy,rg) = 0;
regco2(yy,rg)$a("co2",rg,yy) = a("co2",rg,yy);
display regco2;

