*	Generate IEO projections/statistics for use with GTAP9 dataset:

*	N.B.:	One critical issue for our base-year indexed data projections are
*		data entries in years following the base-year where there is no base-year entry.
*		In this case we do not get an index and keep indexed entries to zero while
*		the projecton indicates that there are positive entries in absolute terms at some time in point
*		For example: In the base-year there might be no electrictiy demand in transportation as there
*		are no electrical cars but the baseline projections might propose a substantial share of
*		electric cars in future years. 
*		We need to flag this issue in the GTAP-IEO projections! 


$if not set byr	$set byr 2007

set	yr /1990*2050/;

set	gtap9(*);
$if not exist gdx\isoregions.gdx $call gams codes
$gdxin gdx\isoregions.gdx
$load gtap9
display gtap9;


set	t_ieo(yr) /2004*2050/;

set	fuel /col,gas,oil/;

 	

$ontext
List of OECD countries and their accession date
Country		Accession date 
* oecdeur/OECD Europe
AUSTRIA 	29 September 1961
BELGIUM 	13 September 1961
CZECH REPUBLIC 	21 December 1995
DENMARK 	30 May 1961
ESTONIA 	9 December 2010
FINLAND 	28 January 1969
FRANCE		7 August 1961
GERMANY 	27 September 1961
GREECE		27 September 1961
HUNGARY 	7 May 1996
ICELAND 	5 June 1961
IRELAND 	17 August 1961
ITALY		29 March 1962
LUXEMBOURG 	7 December 1961
NETHERLANDS 	13 November 1961
NORWAY		4 July 1961
POLAND		22 November 1996
PORTUGAL 	4 August 1961
SLOVAK REPUBLIC 14 December 2000
SLOVENIA 	21 July 2010
SPAIN		3 August 1961
SWEDEN		28 September 1961
SWITZERLAND 	28 September 1961
UNITED KINGDOM 	2 May 1961

ISRAEL		7 September 2010
TURKEY		2 August 1961



* oecdnam/OECD North America
CANADA		10 April 1961
UNITED STATES 	12 April 1961
MEXICO		18 May 1994
CHILE		7 May 2010


* oecdasia/OECD Asia
AUSTRALIA 	7 June 1971
JAPAN		28 April 1964
KOREA		12 December 1996
NEW ZEALAND 	29 May 1973

$offtext

set rall	All region identifiers used here /
	usa                United States
	can                Canada
	mexchi             Mexico
	oecdeur            OECD Europe
	jpn                Japan
	kor                South Korea
	ausnzl             Australia and New Zealand
	rus                Russia
	kaz		   Kazakhstan
	caspian            Caspian Area
	othcaspian	   Other Caspian Area
	othnoecdeur        Other non-OECD Europe
	chn                China
	ind                India
	othnoecdasia       Other
	mideast            Rest of Middle East
	OMN		   Oman
	othmideast	   Other
	africa             Africa
	GHA		   Ghana
	othafrica	   Other Africa
	bra                Brazil
	othcsa             Other Central and South America
	OECDNAM            OECD North America
	OECDASIA           OECD Asia
	OECD               Total OECD
	NOECDEUR           Non-OECD Europe and Eurasia
	NOECDASIA          Non-OECD Asia
	CSA                Central and South America
	NOECD              Total Non-OECD
	WORLD              Total World
	opecmideast        Middle East
	opecnafr           North Africa
	opecwafr           West Africa
	opecsam            South America
	othoecdasia	   Other OECD Asia 	
	/;
display rall;

set r_ieo(rall)	IEO 2013 regions /

*	Comment out redundant regions:

*		oecdnam		"OECD North America USA+CAN+MEX"
		usa		"United States"
		can		"Canada"
		mexchi		"Mexico and Chile"
		oecdeur		"OECD Europe"
*		oecdasia	"OECD Asia JPN+KOR+AUSNZ"
		jpn		"Japan"
		kor		"South Korea"
		ausnzl		"Australia/New Zealand"
*		oecd		"Total OECD"
		rus		"Russia"
*		noecdeur	"Non-OECD Europe and Eurasia RUS+Other"
		othnoecdeur	"Other"
*		noecdasia	"Non-OECD Asia CHN+IND+other Non-OECD Asia"
		chn		"China"
		ind		"India"
		othnoecdasia	"Other Non-OECD Asia"
		mideast		"Middle East"
		africa		"Africa"
*		csa		"Central and South America BRA+Other central and south america"
		bra		"Brazil"
		othcsa		"Other Central and South America"
*		noecd		"Total Non-OECD"
*		world		"Total World" 
	/;

set	i_a	Entries in the A table /
	energy		Total Primary Energy Consumption (Quadrillion Btu)
	gdp		Gross Domestic Product in Purchasing Power Parity
	gdp_mkt		Gross Domestic Product at Market Exchange Rates
	liquids_mbd	Liquids Consumption (Million Barrels per Day)
	gas_tcf		Natural Gas Consumption (Trillion Cubic Feet)
	col_quad	Coal Consumption (Quadrillion Btu)
	nuc_bkwh	Nuclear Energy Consumption (Billion Kilowatthours)
	renew		Consumption of Hydroelectricity and Other Renewable Energy (Quadrillion Btu)
	co2		Carbon Dioxide Emissions (Million Metric Tons Carbon Dioxide)
	co2liquids	Carbon Dioxide Emissions from Liquids (Million Metric Tons Carbon Dioxide)
	co2gas		Carbon Dioxide Emissions from Natural Gas (Million Metric Tons Carbon Dioxide)
	co2col		Carbon Dioxide Emissions from Coal (Million Metric Tons Carbon Dioxide)
	population	Population (millions),

	tot		Total energy consumption -- (quadrillion Btu),
	liquids		Total energy consumption -- liquids (quadrillion Btu),
	gas		Total energy consumption -- Natural gas (quadrillion Btu),
	col		Total energy consumption -- coal (quadrillion Btu),
	nuc		Total energy consumption -- coal (quadrillion Btu),
	oth		Total energy consumption -- other (quadrillion Btu) /;

set	i_b	Entries in the B table /
	energy		Total Primary Energy Consumption (Quadrillion Btu)
	gdp		Gross Domestic Product in Purchasing Power Parity
	liquids_mbd	Liquids Consumption (Million Barrels per Day)
	gas_tcf		Natural Gas Consumption (Trillion Cubic Feet)
	col_quad	Coal Consumption (Quadrillion Btu)
	nuc_bkwh	Nuclear Energy Consumption (Billion Kilowatthours)
	renew		Consumption of Hydroelectricity and Other Renewable Energy (Quadrillion Btu)
	co2		Carbon Dioxide Emissions (Million Metric Tons Carbon Dioxide)
	co2liquids	Carbon Dioxide Emissions from Liquids (Million Metric Tons Carbon Dioxide)
	co2gas		Carbon Dioxide Emissions from Natural Gas (Million Metric Tons Carbon Dioxide)
	co2col		Carbon Dioxide Emissions from Coal (Million Metric Tons Carbon Dioxide)

	tot		Total energy consumption -- (quadrillion Btu),
	liquids		Total energy consumption -- liquids (quadrillion Btu),
	gas		Total energy consumption -- Natural gas (quadrillion Btu),
	col		Total energy consumption -- coal (quadrillion Btu),
	nuc		Total energy consumption -- coal (quadrillion Btu),
	oth		Total energy consumption -- other (quadrillion Btu) /;


set en	Energy in enduse demand /
	Liquids
	Gas
	Coal
	Electricity
	Nuclear
	Renewables
	DeliveredEnergy 
	Electricity-RelatedLosses
*        Electricity losses incurred in the transmission and distribution of electric power. 
*	May include some heat production.
	Total /;

set s	Sectors referenced in end-use data /
	Residential
	Commercial
	Industrial
	Transportation
	All		All End-Use Sectors
	ElectricPower	Electric Power 
*	(Fuel inputs used in the production of electricity and heat at central-station generators)
	Total		Total Energy Consumption /

set r(rall)	Regions /
	OECDNAM		OECD North America
	USA		United States 
	CAN		Canada
	MEXCHI		Mexico and Chile
	OECDEUR		OECD Europe
	OECDASIA	OECD Asia
	JPN		Japan
	KOR		South Korea
	AUSNZL		Australia - New Zealand
	OECD		Total OECD
	NOECDEUR	Non-OECD Europe and Eurasia
	RUS		Russia
	OTHNOECDEUR	Other
	NOECDASIA	Non-OECD Asia
	CHN		China
	IND		India
	OTHNOECDASIA	Other Non-OECD Asia
	MIDEAST		Middle East
	AFRICA		Africa
	CSA		Central and South America
	BRA		Brazil
	OTHCSA		Other Central and South America,
	NOECD		Total Non-OECD,
	WORLD		Total World /;

set	cat /
	conven		Conventional
	total		Total
	biof		Biofuels
	ctl		Coal-to-Liquids
	gtl		Gas-to-Liquids (primarily Qatar)
	sands		Oil Sands - Bitumen (Canada)
	/;

set i_h /
	capacity	Generating Capacity
	oilcap		Liquids-Fired Generating Capacity
	gascap		Natural-Gas-Fired Generating Capacity
	colcap		Coal-Fired Generating Capacity
	nuccap		Nuclear Generating Capacity
	hydrocap	Hydroelectric Renewable Generating Capacity,
	windcap		Wind-Powered Generating Capacity,
	geocap		Geothermal Generating Capacity,
	othrnwcap	Other Renewable Generating Capacity,
	solcap		Solar Generating Capacity,

	generation	Net Electricity Generation,
	oilgen		Net Liquids-Fired Electricity Generation,
	gasgen		Net Natural-Gas-Fired Electricity Generation,
	colgen		Net Coal-Fired Electricity Generation,
	nucgen		Net Nuclear Electricity Generation,
	hydrogen        Net Hydroelectric Generation,
	windgen	        Net Wind-Powered Electricity Generation,
	geogen	        Net Geothermal Electricity Generation,
	othrnwgen	Net Other Renewable Electricity Generation, 
	solgen		Net Solar Electricity Generation
	/;

set	scn		IEO scenarios	 /
		ref		Reference case,
		high_gdp	High GDP growth,
		low_gdp		Low GDP growth,
		high_oil	High oil price,
		low_oil		Low oil price/;

set	fd(s)	Final demand sectors /
			Commercial,
			Residential,
			Transportation,
			Industrial,
			ElectricPower /;

parameter	a(i_a,rall,yr)		Reference Growth Path,
		b(i_b,rall,yr)		High economic growth path,
		c(i_b,rall,yr)		Low Economic Growth Path,
		d(i_b,rall,yr)		High Oil Price Path
		e(i_b,rall,yr)		Low oil price path,
		f(scn,rall,s,en,yr)	Sectoral demand projections,
		g(scn,rall,cat,yr)	World oil market statistics,
		h(scn,i_h,rall,yr)	Electricity capacity and generation statistics;


$gdxin 'gdx\ieo2013.gdx' 
$loaddc a b c d e f g h

parameter	ieocarbon(scn,fuel,rall,yr)	IEO carbon emissions by baseline scenario,
		ieogdp(scn,rall,yr)		IEO GDP by baseline scenario,
		ieocrude(scn,rall,yr)		IEO crude oil equivalent supply by baseline scenario,
		ieoelesup(scn,rall,yr)		IEO electricity supply by baseline scenario,
		ieoenergy(scn,*,fd,rall,yr)	IEO energy use by sector and by baseline scenario,
		ieoprice(scn,yr,yr)		IEO oil prices by baseline scenario and base-year;


ieocarbon(scn,fuel,rall,yr) = 0;	
ieogdp(scn,rall,yr) = 0;		
ieocrude(scn,rall,yr) = 0;		
ieoelesup(scn,rall,yr) = 0;		
ieoenergy(scn,fuel,fd,rall,yr) = 0;	
ieoenergy(scn,"ele",fd,rall,yr) = 0;	
ieoprice(scn,yr,yr) = 0;		

*	Crude oil projections
*	(i)	Insert historical crude-oil price for 2011 and 2012 (in 2011 dollars) in fig21
*	(ii)	Do a linear interpolation until 2040
*	(iii)	Do a geometric extrapolation until 2050
*	(iv)	Then convert all prices from 2011 dollars in either 2007 dollars (baseyear 2007)
*		or 2004 dollars (baseyear 2004)


Table CPI	Conversion factors for CPI (base years 2007 or 2004)
*	Source: http://oregonstate.edu/cla/polisci/individual-year-conversion-factor-tables
	2007		2004		2011   	
2004	0.911		1.000		0.840		
2005	0.942		1.034		0.868
2006	0.972		1.067		0.896
2007	1.000		1.098		0.922
2008	1.038		1.140		0.957
2009	1.035		1.136		0.954
2010	1.052		1.154		0.969
2011	1.085		1.191		1.000
2012	1.107		1.215		1.021
2013	1.128		1.238		1.039;
     
parameter	Crudeprice	Crude oil price in current prices
*	Source: http://tonto.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=PET&s=RWTC&f=A
*	Cushing, OK WTI Spot Price FOB (Dollars per Barrel)
	/2004	41.51
	 2005	56.64
	 2006	66.05
	 2007	72.34
	 2008	99.67
	 2009	61.95
	 2010	79.48
	 2011	94.88
	 2012	94.05
	 /;

Table fig21(yr,scn)	World oil prices in three cases (2011 dollars per barrel) -- IEO 2013
	ref	low_oil	high_oil
2010	81	81	81
2015	96	79	134
2020	106	69	155
2025	117	70	173
2030	130	72	192
2035	145	73	213
2040	163	75	237;

fig21(yr,scn)$(not fig21(yr,scn)) = fig21(yr,"ref");

fig21("2004",scn) = crudeprice("2004")/CPI("2004","2011");
fig21("2005",scn) = crudeprice("2005")/CPI("2005","2011");;
fig21("2006",scn) = crudeprice("2006")/CPI("2006","2011");;
fig21("2007",scn) = crudeprice("2007")/CPI("2007","2011");;
fig21("2008",scn) = crudeprice("2008")/CPI("2008","2011");;
fig21("2009",scn) = crudeprice("2009")/CPI("2009","2011");;

fig21("2011",scn) = crudeprice("2011");
fig21("2012",scn) = crudeprice("2012")/CPI("2012","2011");

*	Linear interpolation to provide baseline data in annual time steps
alias(start,end, yr);
set map(yr,start,end) Time structure for interpolation
	/(2012*2015).2012.2015,
	 (2015*2020).2015.2020,
	 (2020*2025).2020.2025,
	 (2025*2030).2025.2030,
	 (2030*2035).2030.2035,
	 (2035*2040).2035.2040
	 /; 

parameter year(yr)	Numerical values for year;

year(yr) = 1990 + ord(yr)-1;

display year;

loop((yr,start,end)$map(yr,start,end),
	fig21(yr,scn) = (1 - (year(yr)-year(start))/(year(end) - year(start)))*fig21(start,scn)
				+ (year(yr)-year(start))/(year(end) - year(start))*fig21(end,scn);
     );				

*	Geometric interpolation beyond 2040
loop(yr$((year(yr) ge 2040) and (year(yr) lt 2050)),
	fig21(yr+1,scn) = fig21(yr,scn)*fig21(yr,scn)/fig21(yr-1,scn);
      );

display fig21;

ieoprice(scn,yr,"2004")		= fig21(yr,scn)/CPI("2011","2004");
ieoprice(scn,yr,"2007")		= fig21(yr,scn)/CPI("2011","2007");

display ieoprice;

ieogdp("ref",r_ieo,t_ieo(yr))			= a("gdp",r_ieo,yr);
ieogdp("high_gdp",r_ieo,t_ieo(yr))		= b("gdp",r_ieo,yr);
ieogdp("low_gdp",r_ieo,t_ieo(yr))		= c("gdp",r_ieo,yr);
ieogdp("high_oil",r_ieo,t_ieo(yr))		= d("gdp",r_ieo,yr);
ieogdp("low_oil",r_ieo,t_ieo(yr))		= e("gdp",r_ieo,yr);


ieocarbon("ref","oil",r_ieo,t_ieo(yr))		= a("co2liquids",r_ieo,yr);
ieocarbon("high_gdp","oil",r_ieo,t_ieo(yr))	= b("co2liquids",r_ieo,yr);
ieocarbon("low_gdp","oil",r_ieo,t_ieo(yr))	= c("co2liquids",r_ieo,yr);
ieocarbon("high_oil","oil",r_ieo,t_ieo(yr))	= d("co2liquids",r_ieo,yr);
ieocarbon("low_oil","oil",r_ieo,t_ieo(yr))	= e("co2liquids",r_ieo,yr);

ieocarbon("ref","gas",r_ieo,t_ieo(yr))		= a("co2gas",r_ieo,yr);
ieocarbon("high_gdp","gas",r_ieo,t_ieo(yr))	= b("co2gas",r_ieo,yr);
ieocarbon("low_gdp","gas",r_ieo,t_ieo(yr))	= c("co2gas",r_ieo,yr);
ieocarbon("high_oil","gas",r_ieo,t_ieo(yr))	= d("co2gas",r_ieo,yr);
ieocarbon("low_oil","gas",r_ieo,t_ieo(yr))	= e("co2gas",r_ieo,yr);

ieocarbon("ref","col",r_ieo,t_ieo(yr))		= a("co2col",r_ieo,yr);
ieocarbon("high_gdp","col",r_ieo,t_ieo(yr))	= b("co2col",r_ieo,yr);
ieocarbon("low_gdp","col",r_ieo,t_ieo(yr))	= c("co2col",r_ieo,yr);
ieocarbon("high_oil","col",r_ieo,t_ieo(yr))	= d("co2col",r_ieo,yr);
ieocarbon("low_oil","col",r_ieo,t_ieo(yr))	= e("co2col",r_ieo,yr);
display ieocarbon;

ieoenergy(scn,"col",fd,r_ieo,t_ieo(yr)) = f(scn,r_ieo,fd,"Coal",yr);
ieoenergy(scn,"gas",fd,r_ieo,t_ieo(yr)) = f(scn,r_ieo,fd,"Gas",yr);
ieoenergy(scn,"oil",fd,r_ieo,t_ieo(yr)) = f(scn,r_ieo,fd,"Liquids",yr);
ieoenergy(scn,"ele",fd,r_ieo,t_ieo(yr)) = f(scn,r_ieo,fd,"Electricity",yr);
display ieoenergy;



set   r_oieo(rall)	IEO region with crude oil production /

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
	othcsa		'Other Central and South America' 
	/;

set   rieomap(r_oieo,r_ieo) /
		opecmideast.mideast		"Middle East"
		opecnafr.mideast		"North Africa"
		opecwafr.africa			"West Africa"
		opecsam.othcsa			"South America"
*		nopec				"Non-OPEC"
*		oecd				"OECD"
*		oecdnam				"OECD North America"
		usa.usa				"United States"
		can.can				"Canada"
		mexchi.mexchi			"Mexico and Chile"
		oecdeur.oecdeur			"OECD Europe"
*		oecdasia			"OECD Asia"
		ausnzl.ausnzl			"Australia and New Zealand"
*		noecd				"Non-OECD"
*		noecdeur			"Non-OECD Europe and Eurasia"
		rus.rus				"Russia"
		caspian.othnoecdeur		"Caspian Area"
		kaz.othnoecdeur			"Kazakhstan"
		othcaspian.othnoecdeur		"Other Caspian"
		othnoecdeur.othnoecdeur		"Other Non-OECD Europe and Eurasia"

*		noecdasia			"Non-OECD Asia"
		chn.chn				"China"
		ind.ind				"India"
		othnoecdasia.othnoecdasia	"Other"
		omn.mideast			"Oman"
		othmideast.mideast		"Other mideast"
		gha.africa			"Ghana"
		othafrica.africa		"Other Africa"
*		csa				"Central and South America"
		bra.bra				"Brazil"
		othcsa.othcsa			"Other"  /;

ieocrude(scn,r_ieo,t_ieo(yr))	= sum(rieomap(r_oieo,r_ieo), g(scn,r_oieo,"total",yr));
display ieocrude;

*	Electricity supply equals electricity demand across all sectors: 
ieoelesup(scn,r_ieo,t_ieo)= sum(fd,ieoenergy(scn,"ele",fd,r_ieo,t_ieo));


display ieoelesup, ieoenergy;

*	Mapping of GTAP regions to IEO key regions (r_ieo)
set    rmap_e(gtap9,r_ieo)	Mapping between GTAP9 and IEO regions /

CAN.CAN		"Canada				CAN	CAN"
USA.USA		"United States of America	USA	USA"
XNA.USA		"Rest of North America		XNA	USA"
*		- Bermuda
*		- Greenland
*		- Saint Pierre and Miquelon
MEX.MEXCHI	"Mexico				MEX	MEXCHI"
CHL.MEXCHI	"Chile				CHL	MEXCHI"

CHN.CHN		"China				CHN	CHN"
HKG.CHN		"Hong Kong			HKG	CHN"
JPN.JPN		"Japan				JPN	JPN"
KOR.KOR		"Korea				KOR	KOR"
TWN.CHN		"Taiwan				TWN	CHN"
AUS.AUSNZL	"Australia			AUS	anz"
NZL.AUSNZL	"New Zealand			NZL	anz"
BRA.BRA		"Brazil				BRA	BRA"
RUS.RUS		"Russian Federation		RUS	RUS"
IND.IND		"India				IND	IND"


XOC.othnoecdasia	"Rest of Oceania"
*		- American Samoa
*		- Cook Islands
*		- Fiji
*		- French Polynesia
*		- Guam
*		- Kiribati
*		- Marshall Islands
*		- Micronesia Federated States of
*		- Nauru
*		- New Caledonia
*		- Niue
*		- Northern Mariana Islands
*		- Palau
*		- Papua New Guinea
*		- Pitcairn
*		- Samoa
*		- Solomon Islands
*		- Tokelau
*		- Tonga
*		- Tuvalu
*		- United States Minor Outlying Islands
*		- Vanuatu
*		- Wallis and Futuna

XEA.othnoecdasia	"Rest of East Asia"
*		- Korea Democratic Peoples Republic of
*		- Macao
MNG.othnoecdasia	"Mongolia			MNG	othnoecdasia"	
KHM.othnoecdasia	"Cambodia			KHM	othnoecdasia"
IDN.othnoecdasia	"Indonesia			IDN	othnoecdasia"
LAO.othnoecdasia	"Lao People’s Dem Rep		LAO	othnoecdasia"
MYS.othnoecdasia	"Malaysia			MYS	othnoecdasia"
PHL.othnoecdasia	"Philippines			PHL	othnoecdasia"
SGP.othnoecdasia	"Singapore			SGP	othnoecdasia"
THA.othnoecdasia	"Thailand			THA	othnoecdasia"
VNM.othnoecdasia	"Viet Nam			VNM	othnoecdasia"
XSE.othnoecdasia	"Rest of Southeast Asia"
*		- Brunei Darussalam
*		- Myanmar
*		- Timor Leste
BGD.othnoecdasia	"Bangladesh			BGD	othnoecdasia"
NPL.othnoecdasia	"Nepal				NPL	othnoecdasia"
PAK.othnoecdasia	"Pakistan			PAK	othnoecdasia"
LKA.othnoecdasia	"Sri Lanka			LKA	othnoecdasia"
XSA.othnoecdasia	"Rest of South Asia"
*		- Afghanistan
*		- Bhutan
*		- Maldives
ARG.othcsa	"Argentina			ARG	othcsa"
BOL.othcsa    	"Bolivia			BOL	othcsa"
COL.othcsa	"Colombia			COL	othcsa"
ECU.othcsa    	"Ecuador			ECU	othcsa"
PRY.othcsa   	"Paraguay			PRY	othcsa"
PER.othcsa	"Peru				PER	othcsa"
URY.othcsa	"Uruguay			URY	othcsa"
VEN.othcsa	"Venezuela			VEN	othcsa"
XSM.othcsa	"Rest of South America"
*		- Falkland Islands (Malvinas)
*		- French Guiana
*		- Guyana
*		- South Georgia and the South Sandwich Islands
*		- Suriname
CRI.othcsa    	"Costa Rica			CRI	othcsa"
GTM.othcsa    	"Guatemala			GTM	othcsa"
HND.othcsa	"Honduras			HND	othcsa"
NIC.othcsa	"Nicaragua			NIC	othcsa"
PAN.othcsa	"Panama				PAN	othcsa"	
SLV.othcsa	"El Salvador			SLV	othcsa"
XCA.othcsa	"Rest of Central America"	
*		- Belize
XCB.othcsa	"Caribbean",
*		- Anguilla
*		- Antigua & Barbuda
*		- Aruba
*		- Bahamas
*		- Barbados
*		- Cayman Islands
*		- Cuba
*		- Dominica
*		- Dominican Republic
*		- Grenada
*		- Haiti
*		- Jamaica
*		- Montserrat
*		- Netherlands Antilles
*		- Puerto Rico
*		- Saint Kitts and Nevis
*		- Saint Lucia
*		- Saint Vincent and the Grenadines
*		- Trinidad and Tobago
*		- Turks and Caicos Islands
*		- Virgin Islands British
*		- Virgin Islands U.S.
AUT.oecdeur	"Austria			AUT	oecdeur"
BEL.oecdeur	"Belgium			BEL	oecdeur"
CYP.othnoecdeur	"Cyprus				CYP	oecdeur"
CZE.oecdeur	"Czech Republic			CZE	oecdeur"
DNK.oecdeur	"Denmark			DNK	oecdeur"
EST.othnoecdeur	"Estonia			EST	othnoecdeur"
FIN.oecdeur	"Finland			FIN	oecdeur"
*		- Aland Islands
*		- Finland
FRA.oecdeur	"France				FRA	oecdeur"
*		- France
*		- Guadeloupe
*		- Martinique
*		- Reunion
DEU.oecdeur	"Germany			DEU	oecdeur"
GBR.oecdeur	"United Kingdom			GBR	oecdeur"
GRC.oecdeur	"Greece				GRC	oecdeur"
HUN.oecdeur	"Hungary			HUN	oecdeur"
IRL.oecdeur	"Ireland			IRL	oecdeur"
ITA.oecdeur	"Italy				ITA	oecdeur"
LVA.othnoecdeur	"Latvia				LVA	othnoecdeur"
LTU.othnoecdeur	"Lithuania			LTU	othnoecdeur"
LUX.oecdeur	"Luxembourg			LUX	oecdeur"
MLT.othnoecdeur	"Malta				MLT	othnoecdeur"
NLD.oecdeur	"Netherlands			NLD	oecdeur"
POL.oecdeur	"Poland				POL	oecdeur"
PRT.oecdeur	"Portugal			PRT	oecdeur"
SVK.oecdeur	"Slovakia			SVK	oecdeur"
SVN.oecdeur	"Slovenia			SVN	oecdeur"
ESP.oecdeur	"Spain				ESP	oecdeur"
SWE.oecdeur	"Sweden				SWE	oecdeur"
CHE.oecdeur	"Switzerland			CHE	oecdeur"
NOR.oecdeur	"Norway				NOR	oecdeur"
*		- Norway
*		- Svalbard and Jan Mayen
XEF.oecdeur	"Rest of EFTA"	
*			Iceland			ISL	oecdeur"
*			Liechtenstein		LIE	
ALB.othnoecdeur	"Albania			ALB	othnoecdeur"
BGR.othnoecdeur	"Bulgaria			BGR	othnoecdeur"
BLR.othnoecdeur	"Belarus			BLR	othnoecdeur"
HRV.othnoecdeur	"Croatia			HRV	othnoecdeur"
ROU.othnoecdeur	"Romania			ROM	othnoecdeur"
UKR.othnoecdeur	"Ukraine			UKR	othnoecdeur"
XEE.othnoecdeur	"Rest of Eastern Europe		XEE	othnoecdeur"
*		- Moldova Republic of
XER.othnoecdeur	"Rest of Europe"
*		- Andorra
*		- Bosnia and Herzegovina
*		- Faroe Islands
*		- Gibraltar
*		- Guernsey
*		- Holy See (Vatican City State)
*		- Isle of Man
*		- Jersey
*		- Macedonia the former Yugoslav Republic of
*		- Monaco
*		- Montenegro
*		- San Marino
*		- Serbia
KAZ.othnoecdeur	"Kazakhstan		KAZ	othnoecdeur"
KGZ.othnoecdeur	"Kyrgyzstan		KGZ	othnoecdeur"
XSU.othnoecdeur	"Rest of Former Soviet Union"
*		- Tajikistan
*		- Turkmenistan
*		- Uzbekistan
ARM.othnoecdeur "Armenia		ARM	othnoecdeur"
AZE.othnoecdeur "Azerbaijan		AZE	othnoecdeur"
GEO.othnoecdeur	"Georgia		GEO	othnoecdeur"
BHR.mideast	"Bahrain		BHR	mideast"
IRN.mideast    	"Iran			BHR	mideast"

ISR.oecdeur	"Israel			ISR	oecdeur"
TUR.oecdeur	"Turkey			TUR	oecdeur"

KWT.mideast	"Kuwait			KWT	mideast"
OMN.mideast	"Oman			OMN	mideast"
QAT.mideast	"Qatar			QAT	mideast"
SAU.mideast	"Saudi Arabia		SAU	mideast"
ARE.mideast	"United Arab Emirates	ARE	mideast"
XWS.mideast	"Rest of Western Asia"
*		- Iraq
*		- Jordan
*		- Lebanon
*		- Palestinian Territory Occupied
*		- Syrian Arab Republic
*		- Yemen
EGY.mideast	"Egypt			EGY	mideast"
MAR.mideast	"Morocco		MAR	mideast"
TUN.mideast	"Tunisia		TUN	mideast"
XNF.mideast	"Rest of North Africa	XNF	mideast"
*		- Algeria
*		- Libyan Arab Jamahiriya
*		- Western Sahara
CMR.africa	"Cameroon		CMR	africa"
CIV.africa	"Cote d'Ivoire		CIV	africa"
GHA.africa	"Ghana			GHA	africa"
NGA.africa	"Nigeria		NGA	africa"
SEN.africa	"Senegal		SEN	africa"
XWF.africa	"Rest of Western Africa	XWF	africa"
*		- Benin
*		- Burkina Faso
*		- Cape Verde
*		- Gambia
*		- Guinea
*		- Guinea-Bissau
*		- Liberia
*		- Mali
*		- Mauritania
*		- Niger
*		- Saint Helena, Ascension and Tristan Da Cunha
*		- Sierra Leone
*		- Togo
XCF.africa	"Central Africa"
*		- Central African Republic
*		- Chad
*		- Congo
*		- Equatorial Guinea
*		- Gabon
*		- Sao Tome and Principe
XAC.africa	"South Central Africa"
*		- Angola
*		- Congo the Democratic Republic of the

ETH.africa		"Ethiopia		ETH	africa"
KEN.africa		"Kenya			KEN	africa"
MDG.africa		"Madagascar		MDG	africa"
MWI.africa		"Malawi	Malawi		MWI	africa"
MUS.africa		"Mauritius		MUS	africa"
MOZ.africa		"Mozambique		MOZ	africa"
TZA.africa		"Tanzania		TZA	africa"
UGA.africa		"Uganda			UGA	africa"
ZMB.africa		"Zambia			ZMB	africa"
ZWE.africa		"Zimbabwe		ZWE	africa"
XEC.africa		"Rest of Eastern Africa XEC	africa"
*		- Burundi
*		- Comoros
*		- Djibouti
*		- Eritrea
*		- Mayotte
*		- Rwanda
*		- Seychelles
*		- Somalia
*		- Sudan
BWA.africa		"Botswana		BWA	africa"
NAM.africa		"Namibia		NAM	africa"
ZAF.africa		"South Africa		ZAF	africa"
XSC.africa	"Rest of South African Customs Union"
*		- Lesotho
*		- Swaziland
XTW.othnoecdasia	"Rest of the World" 
*		- Antarctica
*		- Bouvet Island
*		- British Indian Ocean Territory
*		- French Southern Territories 

BEN.africa		Benin
BFA.africa		Burkina Faso
BRN.othnoecdasia	Brunei Darussalam
DOM.othcsa		Dominican Republic
GIN.africa		Guinea
JAM.othcsa		Jamaica
JOR.mideast		Jordan
PRI.othcsa		Puerto Rico
RWA.africa		Rwanda
TGO.africa		Togo
TTO.othcsa		Trinidad and Tobago

/;

set mappingbug;
mappingbug(gtap9) = yes$(sum(rmap_e(gtap9,r_ieo),1)<>1);
abort$card(mappingbug) "Error in mapping.  GTAP9 regions must be mapped exactly once.",mappingbug;


mappingbug(r_ieo) = yes$(sum(rmap_e(gtap9,r_ieo),1)=0);
abort$card(mappingbug) "Error in mapping.  IEO regions must be mapped to at least one GTAP region.",mappingbug;



*	Electricity generation statistics
parameter	ieoele(scn,i_h,rall,yr)	IEO electricity generation and installed capacities;

ieoele(scn,i_h,r_ieo,yr) = h(scn,i_h,r_ieo,yr);


*	Convert total numbers in data projections to growth indices with base year %byr% (index -- %byr%=1):

parameter	ieocarbon_	Carbon emissions  (index -- %byr%=1),
		ieogdp_		GDP (index -- %byr%=1),
		ieocrude_	Crude oil equivalent supply (index -- %byr%=1),
		ieoenergy_	IEO energy use by sector (index -- %byr%=1),
		ieoelesup_	IEO electricity supply (index -- %byr%=1),
		ieoele_		IEO electricity generation and capacity (index -- %byr%=1),
		ieoprice_	IEO price (index --%byr%);


set	eg /col,oil,gas,ele/;


loop(rmap_e(gtap9,r_ieo),

	ieocarbon_(scn,fuel,gtap9,t_ieo)$ieocarbon(scn,fuel,r_ieo,"%byr%") 
		= ieocarbon(scn,fuel,r_ieo,t_ieo)/ieocarbon(scn,fuel,r_ieo,"%byr%");

	ieoelesup_(scn,gtap9,t_ieo)$sum(fd,ieoenergy(scn,"ele",fd,r_ieo,"%byr%"))
		= sum(fd,ieoenergy(scn,"ele",fd,r_ieo,t_ieo)) / sum(fd,ieoenergy(scn,"ele",fd,r_ieo,"%byr%"));

	ieoele_(scn,i_h,gtap9,t_ieo)$ieoele(scn,i_h,r_ieo,"%byr%")
		= ieoele(scn,i_h,r_ieo,t_ieo)/ieoele(scn,i_h,r_ieo,"%byr%");

	ieoenergy_(scn,eg,fd,gtap9,t_ieo)$ieoenergy(scn,eg,fd,r_ieo,"%byr%") = 
		ieoenergy(scn,eg,fd,r_ieo,t_ieo) /ieoenergy(scn,eg,fd,r_ieo,"%byr%");

	ieogdp_(scn,gtap9,t_ieo) = ieogdp(scn,r_ieo,t_ieo)/ieogdp(scn,r_ieo,"%byr%");

	ieocrude_(scn,gtap9,t_ieo)$ieocrude(scn,r_ieo,"%byr%") = ieocrude(scn,r_ieo,t_ieo) / ieocrude(scn,r_ieo,"%byr%");

    );

ieoprice_(scn,yr)$ieoprice(scn,"%byr%","%byr%") =	ieoprice(scn,yr,"%byr%")/ieoprice(scn,"%byr%","%byr%");

*	Add population statistics from CIESIN

parameter unpop(gtap9,*) United Nations population trajectory;

$gdxin 'gdx\UNPOP2013.gdx'
$load unpop=pop

display unpop;



execute_unload 'gdx\ieo_gtap9_%byr%.gdx', 
	gtap9,
	ieocarbon_=ieocarbon, 
	ieoelesup_=ieoelesup, 
	ieoenergy_=ieoenergy, 
	ieogdp_=ieogdp, 
	ieocrude_=ieocrude,
	ieoele_=ieoele,  
	ieoprice_=ieoprice,
	unpop
	;

