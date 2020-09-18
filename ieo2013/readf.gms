$title	Read the IEO 2013 Appendix F Workbook (sectoral energy consumption statistics)

$if not set ds $set ds a
$call  'xlsdump xls\ieoenduse_%ds%.xls'
$gdxin 'xls\ieoenduse_%ds%.gdx'

$include layout

parameter
	f1(*,*,*)	"Total World Delivered Energy Consumption by End-Use Sector and Fuel, (Quadrillion Btu)",
	f2(*,*,*)	"Total OECD Delivered Energy Consumption by End-Use Sector and Fuel (Quadrillion Btu)",
	f3(*,*,*)	"Delivered Energy Consumption in the United States by End-Use Sector and Fuel (Quadrillion Btu)",
	f4(*,*,*)	"Delivered Energy Consumption in Canada by End-Use Sector and Fuel (Quadrillion Btu)",
	f5(*,*,*)	"Delivered Energy Consumption in Mexico by End-Use Sector and Fuel (Quadrillion Btu)",
	f6(*,*,*)	"Delivered Energy Consumption in OECD Europe by End-Use Sector and Fuel (Quadrillion Btu)",
	f7(*,*,*)	"Delivered Energy Consumption in Japan by End-Use Sector and Fuel (Quadrillion Btu)",
	f8(*,*,*)	"Delivered Energy Consumption in South Korea by End-Use Sector and Fuel (Quadrillion Btu)",
	f9(*,*,*)	"Delivered Energy Consumption in Australia/New Zealand by End-Use Sector and Fuel (Quadrillion Btu)",
	f10(*,*,*)	"Total Non-OECD Delivered Energy Consumption by End-Use Sector and Fuel (Quadrillion Btu)",
	f11(*,*,*)	"Delivered Energy Consumption in Russia by End-Use Sector and Fuel (Quadrillion Btu)",
	f12(*,*,*)	"Delivered Energy Consumption Otherin  Non-OECD Europe and Eurasia by End-Use Sector and Fuel (Quadrillion Btu)",
	f13(*,*,*)	"Delivered Energy Consumption in China by End-Use Sector and Fuel (Quadrillion Btu)",
	f14(*,*,*)	"Delivered Energy Consumption in India by End-Use Sector and Fuel (Quadrillion Btu)",
	f15(*,*,*)	"Delivered Energy Consumption in Other Non-OECD Asia by End-Use Sector and Fuel (Quadrillion Btu)",
	f16(*,*,*)	"Delivered Energy Consumption in the Middle East by End-Use Sector and Fuel (Quadrillion Btu)",
	f17(*,*,*)	"Delivered Energy Consumption in Africa by End-Use Sector and Fuel (Quadrillion Btu)",
	f18(*,*,*)	"Delivered Energy Consumption in Brazil by End-Use Sector and Fuel (Quadrillion Btu)",
	f19(*,*,*)	"Delivered Energy Consumption in Other Central and South America by End-Use Sector and Fuel (Quadrillion Btu)";

loop((sn,cn,yy)$(vf(sn,"r4",cn)=yy.val),

*	Keep track of years which are provided:

	yr(yy) = yes;

	loop(smap(rn,s,en),
	  loop(ws(sn,"Table F1"), f1(s,en,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table F2"), f2(s,en,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table F3"), f3(s,en,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table F4"), f4(s,en,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table F5"), f5(s,en,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table F6"), f6(s,en,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table F7"), f7(s,en,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table F8"), f8(s,en,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table F9"), f9(s,en,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table F10"), f10(s,en,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table F11"), f11(s,en,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table F12"), f12(s,en,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table F13"), f13(s,en,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table F14"), f14(s,en,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table F15"), f15(s,en,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table F16"), f16(s,en,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table F17"), f17(s,en,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table F18"), f18(s,en,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table F19"), f19(s,en,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	);

*	Reset active year:

	yr(yy) = no;

);

set droprow(rn)/r4/, dropcol(cn)/c38/; 
vf(sn,droprow(rn),cn) = 0;
vf(sn,rn,dropcol(cn)) = 0;
abort$card(vf) "Not all parameters have been transferred.",vf;

set sen(*,*);
sen(s,en) = yes;

$batinclude extrap sen f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19

parameter	f(r,s,en,y)	Sectoral demand projections;

f("world",s,en,y)	= f1(s,en,y);
f("oecd",s,en,y)	= f2(s,en,y);
f("usa",s,en,y)		= f3(s,en,y);
f("can",s,en,y)		= f4(s,en,y);
f("mexchi",s,en,y)	= f5(s,en,y);
f("oecdeur",s,en,y)	= f6(s,en,y);
f("jpn",s,en,y)		= f7(s,en,y);
f("kor",s,en,y)		= f8(s,en,y);
f("ausnzl",s,en,y)	= f9(s,en,y);
f("noecd",s,en,y)	= f10(s,en,y);
f("rus",s,en,y)		= f11(s,en,y);
f("othnoecdeur",s,en,y)	= f12(s,en,y);
f("chn",s,en,y)		= f13(s,en,y);
f("ind",s,en,y)		= f14(s,en,y);
f("othnoecdasia",s,en,y)= f15(s,en,y);
f("mideast",s,en,y)	= f16(s,en,y);
f("africa",s,en,y)	= f17(s,en,y);
f("bra",s,en,y)		= f18(s,en,y);
f("othcsa",s,en,y)	= f19(s,en,y);

option f:0:2:1;
display f;

parameter	euse	Energy use statistics;
euse(y,r) = f(r,"total","total",y);
display euse;
