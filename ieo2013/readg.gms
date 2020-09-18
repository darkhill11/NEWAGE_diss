$title	Read the IEO 2013 Appendix G Workbook (liquids supply)

$call  'xlsdump xls\ieopol.xls'
$gdxin 'xls\ieopol.gdx'

$include layout

*	All units are "Million Barrels Oil Equivalent per Day"

parameter
	g1(*,*)	   "Total Liquids Production by Region and Country, Reference Case"
	g2(*,*)	   "Conventional Liquids Production by Region and Country, Reference Case"
	g3(*,*,*)  "Unconventional Liquids Production by Region and Country, Reference Case"

	g4(*,*)    "Total Liquids Production by Region and Country, High Economic Growth Case"
	g5(*,*)    "Conventional Liquids Production by Region and Country, High Economic Growth Case"
	g6(*,*,*)  "Unconventional Liquids Production by Region and Country, High Economic Growth Case"

	g7(*,*)    "Total Liquids Production by Region and Country, Low Economic Growth Case"
	g8(*,*)    "Conventional Liquids Production by Region and Country, Low Economic Growth Case"
	g9(*,*,*)  "Unconventional Liquids Production by Region and Country, Low Economic Growth Case"

	g10(*,*)   "Total Liquids Production by Region and Country, High Oil Price Case"
	g11(*,*)   "Conventional Liquids Production by Region and Country, High Oil Price Case"
	g12(*,*,*) "Unconventional Liquids Production by Region and Country, High Oil Price Case"

	g13(*,*)   "Total Liquids Production by Region and Country, Low Oil Price Case"
	g14(*,*)   "Conventional Liquids Production by Region and Country, Low Oil Price Case"
	g15(*,*,*) "Unconventional Liquids Production by Region and Country, Low Oil Price Case";



loop((sn,cn,yy)$(vf(sn,"r4",cn)=yy.val),

*	Keep track of years which are provided:

	yr(yy) = yes;

	loop(oilmap(rn,rx),
	  loop(ws(sn,"Table G1"), g1(rx,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table G4"), g4(rx,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table G7"), g7(rx,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table G10"), g10(rx,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table G13"), g13(rx,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	);

	loop(conv(rn,rx),
	  loop(ws(sn,"Table G2"), g2(rx,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table G5"), g5(rx,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table G8"), g8(rx,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table G11"), g11(rx,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table G14"), g14(rx,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	);
	  
	loop(unconv(rn,rx,en),
	  loop(ws(sn,"Table G3"), g3(rx,en,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table G6"), g6(rx,en,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table G9"), g9(rx,en,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table G12"), g12(rx,en,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table G15"), g15(rx,en,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	);

*	Reset active year:

	yr(yy) = no;


);

set droprow(rn)/r4/, dropcol(cn)/c38/;
vf(sn,droprow(rn),cn) = 0;
vf(sn,rn,dropcol(cn)) = 0;
abort$card(vf) "Not all parameters have been transferred.",vf;

set scn /ref,high_gdp,low_gdp,high_oil,low_oil/;

$batinclude extrap rx g1 g2 g4 g5 g7 g8 g10 g11 g13 g14

set ren(rx,en); ren(rx,en) = yes;

$batinclude extrap ren g3 g6 g9

parameter	g(scn,*,*,y)	Oil market statistics;

g("ref",rx,en,y)	= g3(rx,en,y);
g("high_gdp",rx,en,y)	= g6(rx,en,y);
g("low_gdp",rx,en,y)	= g9(rx,en,y);
g("high_oil",rx,en,y)	= g12(rx,en,y);
g("low_oil",rx,en,y)	= g15(rx,en,y);

g("ref",rx,"total",y)	   = g1(rx,y);
g("high_gdp",rx,"total",y) = g4(rx,y);
g("low_gdp",rx,"total",y)  = g7(rx,y);
g("high_oil",rx,"total",y) = g10(rx,y);
g("low_oil",rx,"total",y)  = g13(rx,y);

g("ref",rx,"conven",y)	   = g2(rx,y);
g("high_gdp",rx,"conven",y)= g5(rx,y);
g("low_gdp",rx,"conven",y) = g8(rx,y);
g("high_oil",rx,"conven",y)= g11(rx,y);
g("low_oil",rx,"conven",y) = g14(rx,y);

set   r_oieo(*)	IEO region with crude oil production /

*	Comment out the region identifiers which are redundant:
*	opec	! OPEC /a
	opecmideast	'Middle East'
	opecnafr	'North Africa'
	opecwafr	'West Africa'
	opecsam		'South America'
*	nopec	! Non-OPEC
*	oecd	! OECD
*	oecdnam	! OECD North America
	USA		'United States'
	CAN		'Canada'
	mexchi		'Mexico and Chile'
	oecdeur		'OECD Europe'
*	northsea	! North Sea
*	othoecdeur	! Other
*	oecdasia	! OECD Asia
	ausnzl		'Australia and New Zealand'
	othoecdasia	'Other OECD Asia'
*	noecd	! Non-OECD
*	noecdeur	! Non-OECD Europe and Eurasia
	RUS		'Russia'
	caspian		'Caspian Area'
	KAZ		'Kazakhstan'
	othcaspian	'Other Caspian'
	othnoecdeur	'Other Non-OECD Europe and Eurasia'
*	noecdasia	! Non-OECD Asia
	CHN		'China'
	IND		'India'
	othnoecdasia	'Other Non-OECD Asia'
*	mideast		'Middle East (Non-OPEC)'
	OMN		! Oman
	othmideast	! Other
*	africa		'Africa'
	GHA		! Ghana
	othafrica	! Other
*	csa		"Central and South America"
	BRA		'Brazil'
	othcsa		'Other Central and South America'  /;

g(scn,rx,en,y)$(not r_oieo(rx)) = 0;
option g:1:2:1;
display g;

