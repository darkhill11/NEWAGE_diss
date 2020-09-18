$title	Read the UN Population Projections (http://esa.un.org/wpp/Excel-Data/population.htm)


set	ison, gtap9, gtap9ison;
$if not exist gdx\isoregions.gdx $call gams codes
$gdxin gdx\isoregions.gdx
$load ison gtap9 gtap9ison 

display ison, gtap9ison, gtap9;

$if not exist xls\UN_WPP2012.gdx $call 'xlsdump xls\UN_WPP2012.xls'
$gdxin 'xls\UN_WPP2012.gdx'


set	rn(*)	Row numbers from the Excel file
	cn(*)	Column numbers from the Excel file
	sn(*)	Sheet numbers
	wn(*)	Worksheet names;

$load rn=r cn=c sn=s wn=w

display rn;

set	ws(sn,wn)	Worksheet correspondence;
$loaddc ws

display ws;

parameter	vf(sn,rn,cn)	Worksheet values;
set		vu(sn,rn,cn,*)	Worksheet UELs;

$load vf vu

*.set y	Years in database /1950*2100/;

set	yy		All years /1950*2100/,
	yr(yy)		Years referenced in the tables;


set	map(rn,*)	Mapping of row numbers to ISON /
	R18.900	WORLD
	R19.901	More developed regions
	R20.902	Less developed regions
	R21.941	Least developed countries
	R22.934	Less developed regions excluding least developed countries
	R23.948	Less developed regions excluding China
	R24.947	Sub-Saharan Africa
	R25.903	AFRICA
	R26.910	Eastern Africa
	R27.108	Burundi
	R28.174	Comoros
	R29.262	Djibouti
	R30.232	Eritrea
	R31.231	Ethiopia
	R32.404	Kenya
	R33.450	Madagascar
	R34.454	Malawi
	R35.480	Mauritius
	R36.175	Mayotte
	R37.508	Mozambique
	R38.638	Réunion
	R39.646	Rwanda
	R40.690	Seychelles
	R41.706	Somalia
	R42.728	South Sudan
	R43.800	Uganda
	R44.834	United Republic of Tanzania
	R45.894	Zambia
	R46.716	Zimbabwe
	R47.911	Middle Africa
	R48.24	Angola
	R49.120	Cameroon
	R50.140	Central African Republic
	R51.148	Chad
	R52.178	Congo
	R53.180	Democratic Republic of the Congo
	R54.226	Equatorial Guinea
	R55.266	Gabon
	R56.678	Sao Tome and Principe
	R57.912	Northern Africa
	R58.12	Algeria
	R59.818	Egypt
	R60.434	Libya
	R61.504	Morocco
	R62.729	Sudan
	R63.788	Tunisia
	R64.732	Western Sahara
	R65.913	Southern Africa
	R66.72	Botswana
	R67.426	Lesotho
	R68.516	Namibia
	R69.710	South Africa
	R70.748	Swaziland
	R71.914	Western Africa
	R72.204	Benin
	R73.854	Burkina Faso
	R74.132	Cape Verde
	R75.384	"Côte d'Ivoire"
	R76.270	Gambia
	R77.288	Ghana
	R78.324	Guinea
	R79.624	Guinea-Bissau
	R80.430	Liberia
	R81.466	Mali
	R82.478	Mauritania
	R83.562	Niger
	R84.566	Nigeria
	R85.654	Saint Helena
	R86.686	Senegal
	R87.694	Sierra Leone
	R88.768	Togo
	R89.935	ASIA
	R90.906	Eastern Asia
	R91.156	China
	R92.344	China Hong Kong SAR
	R93.446	China Macao SAR
	R94.408	"Dem. People's Republic of Korea"
	R95.392	Japan
	R96.496	Mongolia
	R97.410	Republic of Korea
	R98.158	Other non-specified areas
	R99.921	South-Central Asia
	R100.5500	Central Asia
	R101.398	Kazakhstan
	R102.417	Kyrgyzstan
	R103.762	Tajikistan
	R104.795	Turkmenistan
	R105.860	Uzbekistan
	R106.5501	Southern Asia
	R107.4	Afghanistan
	R108.50	Bangladesh
	R109.64	Bhutan
	R110.356	India
	R111.364	Iran (Islamic Republic of)
	R112.462	Maldives
	R113.524	Nepal
	R114.586	Pakistan
	R115.144	Sri Lanka
	R116.920	South-Eastern Asia
	R117.96	Brunei Darussalam
	R118.116	Cambodia
	R119.360	Indonesia
	R120.418	"Lao People's Democratic Republic"
	R121.458	Malaysia
	R122.104	Myanmar
	R123.608	Philippines
	R124.702	Singapore
	R125.764	Thailand
	R126.626	Timor-Leste
	R127.704	Viet Nam
	R128.922	Western Asia
	R129.51	Armenia
	R130.31	Azerbaijan
	R131.48	Bahrain
	R132.196	Cyprus
	R133.268	Georgia
	R134.368	Iraq
	R135.376	Israel
	R136.400	Jordan
	R137.414	Kuwait
	R138.422	Lebanon
	R139.512	Oman
	R140.634	Qatar
	R141.682	Saudi Arabia
	R142.275	State of Palestine
	R143.760	Syrian Arab Republic
	R144.792	Turkey
	R145.784	United Arab Emirates
	R146.887	Yemen
	R147.908	EUROPE
	R148.923	Eastern Europe
	R149.112	Belarus
	R150.100	Bulgaria
	R151.203	Czech Republic
	R152.348	Hungary
	R153.616	Poland
	R154.498	Republic of Moldova
	R155.642	Romania
	R156.643	Russian Federation
	R157.703	Slovakia
	R158.804	Ukraine
	R159.924	Northern Europe
	R160.830	Channel Islands
	R161.208	Denmark
	R162.233	Estonia
	R163.234	Faeroe Islands
	R164.246	Finland
	R165.352	Iceland
	R166.372	Ireland
	R167.833	Isle of Man
	R168.428	Latvia
	R169.440	Lithuania
	R170.578	Norway
	R171.752	Sweden
	R172.826	United Kingdom
	R173.925	Southern Europe
	R174.8	Albania
	R175.20	Andorra
	R176.70	Bosnia and Herzegovina
	R177.191	Croatia
	R178.292	Gibraltar
	R179.300	Greece
	R180.336	Holy See
	R181.380	Italy
	R182.470	Malta
	R183.499	Montenegro
	R184.620	Portugal
	R185.674	San Marino
	R186.688	Serbia
	R187.705	Slovenia
	R188.724	Spain
	R189.807	TFYR Macedonia
	R190.926	Western Europe
	R191.40	Austria
	R192.56	Belgium
	R193.250	France
	R194.276	Germany
	R195.438	Liechtenstein
	R196.442	Luxembourg
	R197.492	Monaco
	R198.528	Netherlands
	R199.756	Switzerland
	R200.904	LATIN AMERICA AND THE CARIBBEAN
	R201.915	Caribbean
	R202.660	Anguilla
	R203.28	Antigua and Barbuda
	R204.533	Aruba
	R205.44	Bahamas
	R206.52	Barbados
	R207.92	British Virgin Islands
	R208.535	Caribbean Netherlands
	R209.136	Cayman Islands
	R210.192	Cuba
	R211.531	Curaçao
	R212.212	Dominica
	R213.214	Dominican Republic
	R214.308	Grenada
	R215.312	Guadeloupe
	R216.332	Haiti
	R217.388	Jamaica
	R218.474	Martinique
	R219.500	Montserrat
	R220.630	Puerto Rico
	R221.659	Saint Kitts and Nevis
	R222.662	Saint Lucia
	R223.670	Saint Vincent and the Grenadines
	R224.534	Sint Maarten (Dutch part)
	R225.780	Trinidad and Tobago
	R226.796	Turks and Caicos Islands
	R227.850	United States Virgin Islands
	R228.916	Central America
	R229.84		Belize
	R230.188	Costa Rica
	R231.222	El Salvador
	R232.320	Guatemala
	R233.340	Honduras
	R234.484	Mexico
	R235.558	Nicaragua
	R236.591	Panama
	R237.931	South America
	R238.32		Argentina
	R239.68		Bolivia (Plurinational State of)
	R240.76		Brazil
	R241.152	Chile
	R242.170	Colombia
	R243.218	Ecuador
	R244.238	Falkland Islands (Malvinas)
	R245.254	French Guiana
	R246.328	Guyana
	R247.600	Paraguay
	R248.604	Peru
	R249.740	Suriname
	R250.858	Uruguay
	R251.862	Venezuela (Bolivarian Republic of)
	R252.905	NORTHERN AMERICA
	R253.60		Bermuda
	R254.124	Canada
	R255.304	Greenland
	R256.666	Saint Pierre and Miquelon
	R257.840	United States of America
	R258.909	OCEANIA
	R259.927	Australia and New Zealand
	R260.36		Australia
	R261.554	New Zealand
	R262.928	Melanesia
	R263.242	Fiji
	R264.540	New Caledonia
	R265.598	Papua New Guinea
	R266.90		Solomon Islands
	R267.548	Vanuatu
	R268.954	Micronesia
	R269.316	Guam
	R270.296	Kiribati
	R271.584	Marshall Islands
	R272.583	Micronesia (Fed. States of)
	R273.520	Nauru
	R274.580	Northern Mariana Islands
	R275.585	Palau
	R276.957	Polynesia
	R277.16		American Samoa
	R278.184	Cook Islands
	R279.258	French Polynesia
	R280.570	Niue
	R281.882	Samoa
	R282.772	Tokelau
	R283.776	Tonga
	R284.798	Tuvalu
	R285.876	Wallis and Futuna Islands
	/;

parameter	
		a1(*,*)	"Historical data -- 1950-2010"
		a2(*,*)	"Medium Fertility -- 2010-2100"
		a3(*,*)	"High Fertility -- 2010-2100"
		a4(*,*)	"Low Fertility -- 2010-2100"
		a5(*,*)	"Constant Fertility -- 2010-2100"
		a6(*,*)	"Instant Replacement -- 2010-2100"
		a7(*,*)	"Zero Migration -- 2010-2100"
		a8(*,*)	"Constant Mortality -- 2010-2100"
		a9(*,*)	"No change -- 2010-2100"
		

*	Select year based on values appearing in row 17:

loop((sn,cn,yy)$(vf(sn,"r17",cn)=yy.val),
*	Keep track of years which are provided:
	yr(yy) = yes;
 loop(map(rn,ison),
	  if(ws(sn,"ESTIMATES"), a1(ison,yy) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  if(ws(sn,"MEDIUM FERTILITY"), a2(ison,yy) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  if(ws(sn,"HIGH FERTILITY"), a3(ison,yy) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  if(ws(sn,"LOW FERTILITY"), a4(ison,yy) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  if(ws(sn,"CONSTANT FERTILITY"), a5(ison,yy) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  if(ws(sn,"INSTANT-REPLACEMENT"), a6(ison,yy) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  if(ws(sn,"ZERO-MIGRATION"), a7(ison,yy) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  if(ws(sn,"CONSTANT-MORTALITY"), a8(ison,yy) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	  if(ws(sn,"NO CHANGE"), a9(ison,yy) = vf(sn,rn,cn); vf(sn,rn,cn)=0;);
	);
	
*	Reset active year:

	yr(yy) = no;

);


set	dropr(rn)	Row to be ignored /r17/, 
	dropc(cn)	Column to be ignored /c1*c5/;

dropr(rn)$(not sum(ison, map(rn,ison))) = yes;
vf(sn,dropr(rn),cn) = 0;
vf(sn,rn,dropc(cn)) = 0;
abort$card(vf) "Not all data have been processed:",vf;


parameter pop	Population data;

*	Fill up with hisorical data and projections for medium fertility
pop(gtap9,yy) = sum(ison$gtap9ison(gtap9,ison), a1(ison,yy));
pop(gtap9,yy)$(not pop(gtap9,yy)) = sum(ison$gtap9ison(gtap9,ison), a2(ison,yy));

execute_unload 'gdx\UNPOP2013.gdx',pop;


