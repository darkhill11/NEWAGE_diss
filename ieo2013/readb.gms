$title	Read the IEO 2013 Appendix B Workbook (high economic growth case) 

$call  'xlsdump xls\ieohecon.xls'
$gdxin 'xls\ieohecon.gdx'

$include layout

parameter
	b1(*,*)		"Total Energy Consumption by Region, High Economic Growth Case (Quadrillion Btu)",
	b2(*,*,*)	"Total Energy Consumption by Region and Fuel, High Economic Growth Case (Quadrillion Btu)",
	b3(*,*)		"Gross Domestic Product (GDP) by Region in Purchasing Power Parity, High Economic Growth Case (Billion 2000 Dollars)",
	b4(*,*)		"Liquids Consumption by Region, High Economic Growth Case (Million Barrels per Day)",
	b5(*,*)		"Natural Gas Consumption by Region, High Economic Growth Case (Trillion Cubic Feet)",
	b6(*,*)		"Coal Consumption by Region, High Economic Growth Case (Million Short Tons)",
	b7(*,*)		"Nuclear Energy Consumption by Region, High Economic Growth Case (Billion Kilowatthours)",
	b8(*,*)		"Consumption of Hydroelectricity and Other Renewable Energy by Region, High Economic Growth Case (Quadrillion Btu)"
	b9(*,*)		"Carbon Dioxide Emissions by Region, High Economic Growth Case (Million Metric Tons Carbon Dioxide)",
	b10(*,*)	"Carbon Dioxide Emissions from Liquids Use by Region, High Economic Growth Case (Million Metric Tons Carbon Dioxide)",
	b11(*,*)	"Carbon Dioxide Emissions from Natural Gas Use by Region, High Economic Growth Case (Million Metric Tons Carbon Dioxide)",
	b12(*,*)	"Carbon Dioxide Emissions from Coal Use by Region, High Economic Growth Case (Million Metric Tons Carbon Dioxide)";
	
loop((sn,cn,yy)$(vf(sn,"r4",cn)=yy.val),

*	Keep track of years which are provided:

	yr(yy) = yes;

	loop(enmap(rn,r,en),
	  loop(ws(sn,"Table B2"), b2(r,en,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;)
	);

	loop(rgmap(rn,r),
	  loop(ws(sn,"Table B1"), b1(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table B3"), b3(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table B4"), b4(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table B5"), b5(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table B6"), b6(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table B7"), b7(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table B8"), b8(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table B9"), b9(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table B10"), b10(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table B11"), b11(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table B12"), b12(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	);

*	Reset active year:

	yr(yy) = no;

);

set	dropr(rn) /r4/, dropc(cn) /c38/;
vf(sn,dropr(rn),cn) = 0;
vf(sn,rn,dropc(cn)) = 0;
abort$card(vf) "Not all data have been processed:",vf;

*	Perform a geometric extrapolation to produce numbers for 2004 and post-2040: 

$batinclude extrap r b1 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 

set ren(*,*);
ren(r,en) = yes;
$batinclude extrap ren b2

parameter	b(*,r,*)	Reference Growth Path;
b("energy",r,y)$b1(r,y) = b1(r,y);
b("gdp",r,y)$b3(r,y)    = b3(r,y);
b("liquids_mbd",r,y)$b5(r,y)= b4(r,y);
b("gas_tcf",r,y)$b6(r,y)   = b5(r,y);
b("col_quad",r,y)$b7(r,y)  = b6(r,y);
b("nuc_bkwh",r,y)$b8(r,y) = b7(r,y);
b("renew",r,y)$b9(r,y)	= b8(r,y);
b("co2",r,y)$b10(r,y)	= b9(r,y);
b("co2liquids",r,y)	= b10(r,y);
b("co2gas",r,y)		= b11(r,y);
b("co2col",r,y)		= b12(r,y);


*	Share out the BTU energy data using regional shares of physical consumption:

parameter	rshare(*,*,*,yy)	Regional shares
		subsum			Regional sums;

subsum(rd,yr,enshare(en)) = sum(rr$rmap(rd,rr),b(en,rr,yr));
rshare(enshare(en),rmap(rd,r),yr)$subsum(rd,yr,en) 
	= b(en,r,yr) / subsum(rd,yr,en);

parameter	chksum	Cross check on regional shares;
chksum(enshare(en),rd,yr)$sum(r$rmap(rd,r),b(en,r,yr)) = round(1 - sum(r,rshare(en,rd,r,yr)),6);
abort$card(chksum) "Error in checksum:", chksum;

set	collision;
collision(en,r,y)$b(en,r,y) = yes$b2(r,en,y);
display collision;
abort$card(collision) "Data collision when loading Table B.";

*	Read in energy consumption in quads here:

b(en,r,y)$b2(r,en,y) = b2(r,en,y);
loop(rmap(rd,r), b(en,r,yr)$b2(rd,en,yr) = rshare(en,rd,r,yr) * b2(rd,en,yr););

option b:3:1:1;
display b;

