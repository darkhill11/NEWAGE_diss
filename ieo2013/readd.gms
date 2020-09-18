$title	Read the IEO 2013 Appendix D Workbook (high oil price case) 

$call  'xlsdump xls\ieohprce.xls'
$gdxin 'xls\ieohprce.gdx'

$include layout

parameter
	d1(*,*)		"Total Energy Consumption by Region, High Oil Price  Case (Quadrillion Btu)",
	d2(*,*,*)	"Total Energy Consumption by Region and Fuel, High Oil Price  Case (Quadrillion Btu)",
	d3(*,*)		"Gross Domestic Product (GDP) by Region, High Oil Price  Case (Billion 2005 Dollars)",
	d4(*,*)		"Liquids Consumption by Region in Purchasing Power Parity, High Oil Price  Case (Million Barrels per Day)",
	d5(*,*)		"Natural Gas Consumption by Region, High Oil Price  Case (Trillion Cubic Feet)",
	d6(*,*)		"Coal Consumption by Region, High Oil Price  Case (Million Short Tons)",
	d7(*,*)		"Nuclear Energy Consumption by Region, High Oil Price  Case (Billion Kilowatthours)",
	d8(*,*)		"Consumption of Hydroelectricity and Other Renewable Energy by Region, High Oil Price  Case (Quadrillion Btu)"
	d9(*,*)		"Carbon Dioxide Emissions by Region, High Oil Price  Case (Million Metric Tons Carbon Dioxide)",
	d10(*,*)	"Carbon Dioxide Emissions from Liquids Use by Region, High Oil Price  Case (Million Metric Tons Carbon Dioxide)",
	d11(*,*)	"Carbon Dioxide Emissions from Natural Gas Use by Region, High Oil Price  Case (Million Metric Tons Carbon Dioxide)",
	d12(*,*)	"Carbon Dioxide Emissions from Coal Use by Region, High Oil Price  Case (Million Metric Tons Carbon Dioxide)";
	
loop((sn,cn,yy)$(vf(sn,"r4",cn)=yy.val),

*	Keep track of years which are provided:

	yr(yy) = yes;

	loop(enmap(rn,r,en),
	  loop(ws(sn,"Table D2"), d2(r,en,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;)
	);

	loop(rgmap(rn,r),
	  loop(ws(sn,"Table D1"), d1(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table D3"), d3(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table D4"), d4(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table D5"), d5(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table D6"), d6(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table D7"), d7(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table D8"), d8(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table D9"), d9(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table D10"), d10(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table D11"), d11(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table D12"), d12(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	);

*	Reset active year:

	yr(yy) = no;


);

set	dropr(rn) /r4/, dropc(cn) /c38/;
vf(sn,dropr(rn),cn) = 0;
vf(sn,rn,dropc(cn)) = 0;
abort$card(vf) "Not all data have been processed:",vf;

*	Perform a geometric extrapolation to produce numbers for 2004 and post-2040: 

$batinclude extrap r d1 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 

set ren(*,*);
ren(r,en) = yes;
$batinclude extrap ren d2

parameter	d(*,r,*)	Reference Growth Path;
d("energy",r,y)		= d1(r,y);
d("gdp",r,y)		= d3(r,y);
d("liquids_mbd",r,y)	= d4(r,y);
d("gas_tcf",r,y)	= d5(r,y);
d("col_quad",r,y)	= d6(r,y);
d("nuc_bkwh",r,y)	= d7(r,y);
d("renew",r,y)		= d8(r,y);
d("co2",r,y)		= d9(r,y);
d("co2liquids",r,y)	= d10(r,y);
d("co2gas",r,y)		= d11(r,y);
d("co2col",r,y)		= d12(r,y);


*	Share out the BTU energy data using regional shares of physical consumption:

parameter	rshare(*,*,*,yy)	Regional shares
		subsum			Regional sums;

subsum(rd,yr,enshare(en)) = sum(rr$rmap(rd,rr),d(en,rr,yr));
rshare(enshare(en),rmap(rd,r),yr)$subsum(rd,yr,en) 
	= d(en,r,yr) / subsum(rd,yr,en);

parameter	chksum	Cross check on regional shares;
chksum(enshare(en),rd,yr)$sum(r$rmap(rd,r),d(en,r,yr)) = round(1 - sum(r,rshare(en,rd,r,yr)),6);
abort$card(chksum) "Error in checksum:", chksum;

set	collision;
collision(en,r,y)$d(en,r,y) = yes$d2(r,en,y);
display collision;
abort$card(collision) "Data collision when loading Table D.";

*	Read in energy consumption in quads here:

d(en,r,y)$d2(r,en,y) = d2(r,en,y);
loop(rmap(rd,r), d(en,r,yr)$d2(rd,en,yr) = rshare(en,rd,r,yr) * d2(rd,en,yr););

*.parameter	d2009(en,r,y)		Values of d() projected in 2009;
*.$gdxin ..\ieo2009\gdx\readd.gdx
*.$load d2009=d
*.$gdxin
*.d(en,r,y)$(d2009(en,r,y) and (not d(en,r,y))) = d2009(en,r,y);

option d:3:1:1;
display d;

