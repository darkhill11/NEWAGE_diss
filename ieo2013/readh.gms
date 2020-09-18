$title	Read the IEO 2013 Appendix H Workbook (electricity capacity and generation statistics)

$if not set ds $set ds a
$call  'xlsdump xls\ieoecg_%ds%.xls'
$gdxin 'xls\ieoecg_%ds%.gdx'

$include layout

parameter
*	Units: Gigawatts

	h1(*,*)		"Total Installed Generating Capacity",
	h2(*,*)		"Installed Liquids-Fired Generating Capacity",
	h3(*,*)		"Installed Natural-Gas-Fired Generating Capacity",
	h4(*,*)		"Installed Coal-Fired Generating Capacity",
	h5(*,*)		"Installed Nuclear Generating Capacity",
	h6(*,*)		"Installed Hydroelectric Renewable Generating Capacity",
	h7(*,*)		"Installed Wind-Powered Generating Capacity",
	h8(*,*)		"Installed Geothermal Generating Capacity",
	h9(*,*)		"Installed Other Renewable Generating Capacity",
	
*	units:	"Billion Kilowatthours --  from central producers"

	h10(*,*)	"Total Net Electricity Generation",
	h11(*,*)	"Net Liquids-Fired Electricity Generation",
	h12(*,*)	"Net Natural-Gas-Fired Electricity Generation",
	h13(*,*)	"Net Coal-Fired Electricity Generation",
	h14(*,*)	"Net Nuclear Electricity Generation",
	h15(*,*)	"Net Hydroelectric Generation",
	h16(*,*)	"Net Wind-Powered Electricity Generation",
	h17(*,*)	"Net Geothermal Electricity Generation",
	h18(*,*)	"Net Other Renewable Electricity Generation",

	h19(*,*)	"Installed solar generating capacity",
	h20(*,*)	"Net solar electricity generation from central producers";

loop((sn,cn,yy)$(vf(sn,"r4",cn)=yy.val),

*	Keep track of years which are provided:

	yr(yy) = yes;

	loop(rgmap(rn,r),
	  loop(ws(sn,"Table H1"), h1(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table H2"), h2(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table H3"), h3(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table H4"), h4(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table H5"), h5(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table H6"), h6(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table H7"), h7(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table H8"), h8(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table H9"), h9(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table H10"), h10(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table H11"), h11(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table H12"), h12(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table H13"), h13(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table H14"), h14(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table H15"), h15(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table H16"), h16(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table H17"), h17(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table H18"), h18(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table H19"), h19(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  loop(ws(sn,"Table H20"), h20(r,yr) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	);

*	Reset active year:

	yr(yy) = no;

);

set droprow(rn)/r4/, dropcol(cn)/c38/;
vf(sn,droprow(rn),cn) = 0;
vf(sn,rn,dropcol(cn)) = 0;
abort$card(vf) "Not all parameters have been transferred.",vf;

$batinclude extrap r h1 h2 h3 h4 h5 h6 h7 h8 h9 h10 h11 h12 h13 h14 h15 h16 h17 h18 h19 h20

set	rec	Data records /
	capacity, oilcap, gascap, colcap, nuccap, hydrocap, windcap, geocap,
	othrnwcap, generation, oilgen, gasgen, colgen, nucgen, hydrogen,
	windgen, geogen, othrnwgen, solcap, solgen/;

parameter	h(rec,r,y)	Electricity capacity and generation statistics;

h("capacity",r,y)	= h1(r,y);
h("oilcap",r,y)		= h2(r,y);
h("gascap",r,y)		= h3(r,y);
h("colcap",r,y)		= h4(r,y);
h("nuccap",r,y)		= h5(r,y);
h("hydrocap",r,y)	= h6(r,y);
h("windcap",r,y)	= h7(r,y);
h("geocap",r,y)		= h8(r,y);
h("othrnwcap",r,y)	= h9(r,y);

h("generation",r,y)	= h10(r,y);
h("oilgen",r,y)		= h11(r,y);
h("gasgen",r,y)		= h12(r,y);
h("colgen",r,y)		= h13(r,y);
h("nucgen",r,y)		= h14(r,y);
h("hydrogen",r,y)	= h15(r,y);
h("windgen",r,y)	= h16(r,y);
h("geogen",r,y)		= h17(r,y);
h("othrnwgen",r,y)	= h18(r,y);
h("solcap",r,y)		= h19(r,y);
h("solgen",r,y)		= h20(r,y);

display h;


option h:1:1:1;
display h;
