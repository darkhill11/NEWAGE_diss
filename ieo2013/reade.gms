$title	Read the IEO 2013 Appendix E Workbook (low oil price case) 

$call  'xlsdump xls\ieolprce.xls'
$gdxin 'xls\ieolprce.gdx'


$include layout

parameter
	e1(*,*)		"Total Energy Consumption by Region, Low Oil Price Case (Quadrillion Btu)",
	e2(*,*,*)	"Total Energy Consumption by Region and Fuel, Low Oil Price Case (Quadrillion Btu)",
	e3(*,*)		"Gross Domestic Product (GDP) by Region in Purchasing Power Parity, Low Oil Price Case (Billion 2005 Dollars)",
	e4(*,*)		"Liquids Consumption by Region, Low Oil Price Case (Million Barrels per Day)",
	e5(*,*)		"Natural Gas Consumption by Region, Low Oil Price Case (Trillion Cubic Feet)",
	e6(*,*)		"Coal Consumption by Region, Low Oil Price Case (Million Short Tons)",
	e7(*,*)		"Nuclear Energy Consumption by Region, Low Oil Price Case (Billion Kilowatthours)",
	e8(*,*)		"Consumption of Hydroelectricity and Other Renewable Energy by Region, Low Oil Price Case (Quadrillion Btu)"
	e9(*,*)		"Carbon Dioxide Emissions by Region, Low Oil Price Case (Million Metric Tons Carbon Dioxide)",
	e10(*,*)	"Carbon Dioxide Emissions from Liquids Use by Region, Low Oil Price Case (Million Metric Tons Carbon Dioxide)",
	e11(*,*)	"Carbon Dioxide Emissions from Natural Gas Use by Region, Low Oil Price Case (Million Metric Tons Carbon Dioxide)",
	e12(*,*)	"Carbon Dioxide Emissions from Coal Use by Region, Low Oil Price Case (Million Metric Tons Carbon Dioxide)";
	
loop((sn,cn,yy)$(vf(sn,"r4",cn)=yy.val),

*	Keep track of years which are provided:

	yr(yy) = yes;

	loop(enmap(rn,r,en),
	  loop(ws(sn,"Table E2"), e2(r,en,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;)
	);

	loop(rgmap(rn,r),
	  loop(ws(sn,"Table E1"), e1(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table E3"), e3(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table E4"), e4(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table E5"), e5(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table E6"), e6(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table E7"), e7(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table E8"), e8(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table E9"), e9(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table E10"), e10(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table E11"), e11(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table E12"), e12(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	);

*	Reset active year:

	yr(yy) = no;


);

set	dropr(rn) /r4/, dropc(cn) /c38/;
vf(sn,dropr(rn),cn) = 0;
vf(sn,rn,dropc(cn)) = 0;
abort$card(vf) "Not all data have been processed:",vf;

*	Perform a geometric extrapolation to produce numbers for 2004 and post-2040: 

$batinclude extrap r e1 e3 e4 e5 e6 e7 e8 e9 e10 e11 e12 

set ren(*,*);
ren(r,en) = yes;
$batinclude extrap ren e2

parameter	e(*,r,*)	Reference Growth Path;
e("energy",r,y)		= e1(r,y);
e("gdp",r,y)		= e3(r,y);
e("liquids_mbd",r,y)	= e4(r,y);
e("gas_tcf",r,y)	= e5(r,y);
e("col_quad",r,y)	= e6(r,y);
e("nuc_bkwh",r,y)	= e7(r,y);
e("renew",r,y)		= e8(r,y);
e("co2",r,y)		= e9(r,y);
e("co2liquids",r,y)	= e10(r,y);
e("co2gas",r,y)		= e11(r,y);
e("co2col",r,y)		= e12(r,y);


*	Share out the BTU energy data using regional shares of physical consumption:

parameter	rshare(*,*,*,yy)	Regional shares
		subsum			Regional sums;

subsum(rd,yr,enshare(en)) = sum(rr$rmap(rd,rr),e(en,rr,yr));
rshare(enshare(en),rmap(rd,r),yr)$subsum(rd,yr,en) 
	= e(en,r,yr) / subsum(rd,yr,en);

parameter	chksum	Cross check on regional shares;
chksum(enshare(en),rd,yr)$sum(r$rmap(rd,r),e(en,r,yr)) = round(1 - sum(r,rshare(en,rd,r,yr)),6);
abort$card(chksum) "Error in checksum:", chksum;

set	collision;
collision(en,r,y)$e(en,r,y) = yes$e2(r,en,y);
display collision;
abort$card(collision) "Data collision when loading Table E.";

*	Read in energy consumption in quads here:

e(en,r,y)$e2(r,en,y) = e2(r,en,y);
loop(rmap(rd,r), e(en,r,yr)$e2(rd,en,yr) = rshare(en,rd,r,yr) * e2(rd,en,yr););


option e:3:1:1;
display e;

