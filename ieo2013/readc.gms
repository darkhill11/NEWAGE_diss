$title	Read the IEO 2013 Appendix C Workbook (low economic growth case) 

$call  'xlsdump xls\ieolecon.xls'
$gdxin 'xls\ieolecon.gdx'


$include layout

parameter
	c1(*,*)		"Total Energy Consumption by Region, Low Economic Growth Case (Quadrillion Btu)",
	c2(*,*,*)	"Total Energy Consumption by Region and Fuel, Low Economic Growth Case (Quadrillion Btu)",
	c3(*,*)		"Gross Domestic Product (GDP) by Region in Purchasing Power Parity, Low Economic Growth Case (Billion 2005 Dollars)",
	c4(*,*)		"Liquids Consumption by Region, Low Economic Growth Case (Million Barrels per Day)",
	c5(*,*)		"Natural Gas Consumption by Region, Low Economic Growth Case (Trillion Cubic Feet)",
	c6(*,*)		"Coal Consumption by Region, Low Economic Growth Case (Million Short Tons)",
	c7(*,*)		"Nuclear Energy Consumption by Region, Low Economic Growth Case (Billion Kilowatthours)",
	c8(*,*)		"Consumption of Hydroelectricity and Other Renewable Energy by Region, Low Economic Growth Case (Quadrillion Btu)"
	c9(*,*)		"Carbon Dioxide Emissions by Region, Low Economic Growth Case (Million Metric Tons Carbon Dioxide)",
	c10(*,*)	"Carbon Dioxide Emissions from Liquids Use by Region, Low Economic Growth Case (Million Metric Tons Carbon Dioxide)",
	c11(*,*)	"Carbon Dioxide Emissions from Natural Gas Use by Region, Low Economic Growth Case (Million Metric Tons Carbon Dioxide)",
	c12(*,*)	"Carbon Dioxide Emissions from Coal Use by Region, Low Economic Growth Case (Million Metric Tons Carbon Dioxide)";
	
loop((sn,cn,yy)$(vf(sn,"r4",cn)=yy.val),

*	Keep track of years which are provided:

	yr(yy) = yes;

	loop(enmap(rn,r,en),
	  loop(ws(sn,"Table c2"), c2(r,en,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;)
	);

	loop(rgmap(rn,r),
	  loop(ws(sn,"Table c1"), c1(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table c3"), c3(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table c4"), c4(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table c5"), c5(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table c6"), c6(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table c7"), c7(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table c8"), c8(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table c9"), c9(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table c10"), c10(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table c11"), c11(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table c12"), c12(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	);

*	Reset active year:

	yr(yy) = no;

);

set	dropr(rn) /r4/, dropc(cn) /c38/;
vf(sn,dropr(rn),cn) = 0;
vf(sn,rn,dropc(cn)) = 0;
abort$card(vf) "Not all data have been processed:",vf;

*	Perform a geometric extrapolation to produce numbers for 2004 and post-2040: 

$batinclude extrap r c1 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 

set ren(*,*);
ren(r,en) = yes;
$batinclude extrap ren c2

parameter	c(*,r,*)	Reference Growth Path;
c("energy",r,y)		= c1(r,y);
c("gdp",r,y)		= c3(r,y);
c("liquids_mbd",r,y)	= c4(r,y);
c("gas_tcf",r,y)	= c5(r,y);
c("col_quad",r,y)	= c6(r,y);
c("nuc_bkwh",r,y)	= c7(r,y);
c("renew",r,y)		= c8(r,y);
c("co2",r,y)		= c9(r,y);
c("co2liquids",r,y)	= c10(r,y);
c("co2gas",r,y)		= c11(r,y);
c("co2col",r,y)		= c12(r,y);


*	Share out the BTU energy data using regional shares of physical consumption:

parameter	rshare(*,*,*,yy)	Regional shares
		subsum			Regional sums;

subsum(rd,yr,enshare(en)) = sum(rr$rmap(rd,rr),c(en,rr,yr));
rshare(enshare(en),rmap(rd,r),yr)$subsum(rd,yr,en) 
	= c(en,r,yr) / subsum(rd,yr,en);

parameter	chksum	Cross check on regional shares;
chksum(enshare(en),rd,yr)$sum(r$rmap(rd,r),c(en,r,yr)) = round(1 - sum(r,rshare(en,rd,r,yr)),6);
abort$card(chksum) "Error in checksum:", chksum;

set	collision;
collision(en,r,y)$c(en,r,y) = yes$c2(r,en,y);
display collision;
abort$card(collision) "Data collision when loading Table C.";

*	Read in energy consumption in quads here:

c(en,r,y)$c2(r,en,y) = c2(r,en,y);
loop(rmap(rd,r), c(en,r,yr)$c2(rd,en,yr) = rshare(en,rd,r,yr) * c2(rd,en,yr););

option c:3:1:1;
display c;

