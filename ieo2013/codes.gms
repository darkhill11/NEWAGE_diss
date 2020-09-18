$title	Read the ISO mappings from GEONAMES

$eolcom !

set n	Set defined to maintain consistent ordering of integers /1*1000/

set gtap9	GTAP 8 Regions /
ALB	Albania
ARE	United Arab Emirates
ARG	Argentina
ARM	Armenia
AUS	Australia
AUT	Austria
AZE	Azerbaijan
BEL	Belgium
BEN	Benin
BFA	Burkina Faso
BGD	Bangladesh
BGR	Bulgaria
BHR	Bahrain
BLR	Belarus
BOL	Bolivia
BRA	Brazil
BRN	Brunei Darussalam
BWA	Botswana
CAN	Canada
CHE	Switzerland
CHL	Chile
CHN	China
CIV	Cote d'Ivoire
CMR	Cameroon
COL	Colombia
CRI	Costa Rica
CYP	Cyprus
CZE	Czech Republic
DEU	Germany
DNK	Denmark
DOM	Dominican Republic
ECU	Ecuador
EGY	Egypt
ESP	Spain
EST	Estonia
ETH	Ethiopia
FIN	Finland
FRA	France
GBR	United Kingdom
GEO	Georgia
GHA	Ghana
GIN	Guinea
GRC	Greece
GTM	Guatemala
HKG	Hong Kong
HND	Honduras
HRV	Croatia
HUN	Hungary
IDN	Indonesia
IND	India
IRL	Ireland
IRN	Iran Islamic Republic of
ISR	Israel
ITA	Italy
JAM	Jamaica
JOR	Jordan
JPN	Japan
KAZ	Kazakhstan
KEN	Kenya
KGZ	Kyrgyzstan
KHM	Cambodia
KOR	Korea Republic of
KWT	Kuwait
LAO	Lao People's Democratic Republic
LKA	Sri Lanka
LTU	Lithuania
LUX	Luxembourg
LVA	Latvia
MAR	Morocco
MDG	Madagascar
MEX	Mexico
MLT	Malta
MNG	Mongolia
MOZ	Mozambique
MUS	Mauritius
MWI	Malawi
MYS	Malaysia
NAM	Namibia
NGA	Nigeria
NIC	Nicaragua
NLD	Netherlands
NOR	Norway
NPL	Nepal
NZL	New Zealand
OMN	Oman
PAK	Pakistan
PAN	Panama
PER	Peru
PHL	Philippines
POL	Poland
PRI	Puerto Rico
PRT	Portugal
PRY	Paraguay
QAT	Qatar
ROU	Romania
RUS	Russian Federation
RWA	Rwanda
SAU	Saudi Arabia
SEN	Senegal
SGP	Singapore
SLV	El Salvador
SVK	Slovakia
SVN	Slovenia
SWE	Sweden
TGO	Togo
THA	Thailand
TTO	Trinidad and Tobago
TUN	Tunisia
TUR	Turkey
TWN	Taiwan
TZA	Tanzania United Republic of
UGA	Uganda
UKR	Ukraine
URY	Uruguay
USA	United States of America
VEN	Venezuela
VNM	Viet Nam
ZAF	South Africa
ZMB	Zambia
ZWE	Zimbabwe

XAC	South Central Africa
XCA	Rest of Central America
XCB	Caribbean
XCF	Central Africa
XEA	Rest of East Asia
XEC	Rest of Eastern Africa
XEE	Rest of Eastern Europe
XEF	Rest of EFTA
XER	Rest of Europe
XNA	Rest of North America
XNF	Rest of North Africa
XOC	Rest of Oceania
XSA	Rest of South Asia
XSE	Rest of Southeast Asia
XSC	Rest of South African Customs Union
XSM	Rest of South America
XSU	Rest of Former Soviet Union
XTW	Rest of the World
XWF	Rest of Western Africa
XWS	Rest of Western Asia
	 /;

*	set isomap(iso2,iso3,ison,fips)	Mappings 

*	http://www.geonames.org/countries/

set isomap(*,*,*,*)	Mappings  /
	AD.AND.20.AN	"Andorra",
	AE.ARE.784.AE	"United Arab Emirates",
	AF.AFG.4.AF	"Afghanistan",
	AG.ATG.28.AC	"Antigua and Barbuda",
	AI.AIA.660.AV	"Anguilla",
	AL.ALB.8.AL	"Albania",
	AM.ARM.51.AM	"Armenia",
	AN.ANT.530.NT	"Netherlands Antilles",
	AO.AGO.24.AO	"Angola",
	AQ.ATA.10.AY	"Antarctica",
	AR.ARG.32.AR	"Argentina",
	AS.ASM.16.AQ	"American Samoa",
	AT.AUT.40.AU	"Austria",
	AU.AUS.36.AS	"Australia",
	AW.ABW.533.AA	"Aruba",
	AX.ALA.248.FI	"Aland Islands",
	AZ.AZE.31.AJ	"Azerbaijan",
	BA.BIH.70.BK	"Bosnia and Herzegovina",
	BB.BRB.52.BB	"Barbados",
	BD.BGD.50.BG	"Bangladesh",
	BE.BEL.56.BE	"Belgium",
	BF.BFA.854.UV	"Burkina Faso",
	BG.BGR.100.BU	"Bulgaria",
	BH.BHR.48.BA	"Bahrain",
	BI.BDI.108.BY	"Burundi",
	BJ.BEN.204.BN	"Benin",
	BL.BLM.652.RY	"Barthelemy",
	BM.BMU.60.BD	"Bermuda",
	BN.BRN.96.BX	"Brunei",
	BO.BOL.68.BL	"Bolivia",
	BR.BRA.76.BR	"Brazil",
	BS.BHS.44.BF	"Bahamas",
	BT.BTN.64.BT	"Bhutan",
	BV.BVT.74.BV	"Bouvet Island",
	BW.BWA.72.BC	"Botswana",
	BY.BLR.112.BO	"Belarus",
	BZ.BLZ.84.BH	"Belize",
	CA.CAN.124.CA	"Canada",
	CC.CCK.166.CK	"Cocos (Keeling) Islands",
	CD.COD.180.CG	"Congo (Kinshasa)",
	CF.CAF.140.CT	"Central African Republic",
	CG.COG.178.CF	"Congo (Brazzaville)",
	CH.CHE.756.SZ	"Switzerland",
	CI.CIV.384.IV	"Ivory Coast",
	CK.COK.184.CW	"Cook Islands",
	CL.CHL.152.CI	"Chile",
	CM.CMR.120.CM	"Cameroon",
	CN.CHN.156.CH	"China",
	CO.COL.170.CO	"Colombia",
	CR.CRI.188.CS	"Costa Rica",
	CS.SCG.891.YI	"Serbia And Montenegro",
	CU.CUB.192.CU	"Cuba",
	CV.CPV.132.CV	"Cape Verde",
	CX.CXR.162.KT	"Christmas Island",
	CY.CYP.196.CY	"Cyprus",
	CZ.CZE.203.EZ	"Czech Republic",
	DE.DEU.276.GM	"Germany",
	DJ.DJI.262.DJ	"Djibouti",
	DK.DNK.208.DA	"Denmark",
	DM.DMA.212.DO	"Dominica",
	DO.DOM.214.DR	"Dominican Republic",
	DZ.DZA.12.AG	"Algeria",
	EC.ECU.218.EC	"Ecuador",
	EE.EST.233.EN	"Estonia",
	EG.EGY.818.EG	"Egypt",
	EH.ESH.732.WI	"Western Sahara",
	ER.ERI.232.ER	"Eritrea",
	ES.ESP.724.SP	"Spain",
	ET.ETH.231.ET	"Ethiopia",
	FI.FIN.246.FI	"Finland",
	FJ.FJI.242.FJ	"Fiji",
	FK.FLK.238.FK	"Falkland Islands",
	FM.FSM.583.FM	"Micronesia",
	FO.FRO.234.FO	"Faroe Islands",
	FR.FRA.250.FR	"France",
	GA.GAB.266.GB	"Gabon",
	GB.GBR.826.UK	"United Kingdom",
	GD.GRD.308.GJ	"Grenada",
	GE.GEO.268.GG	"Georgia",
	GF.GUF.254.FG	"French Guiana",
	GG.GGY.831.GK	"Guernsey",
	GH.GHA.288.GH	"Ghana",
	GI.GIB.292.GI	"Gibraltar",
	GL.GRL.304.GL	"Greenland",
	GM.GMB.270.GA	"Gambia",
	GN.GIN.324.GV	"Guinea",
	GP.GLP.312.GP	"Guadeloupe",
	GQ.GNQ.226.EK	"Equatorial Guinea",
	GR.GRC.300.GR	"Greece",
	GS.SGS.239.SX	"South Georgia and the South Sandwich Islands",
	GT.GTM.320.GT	"Guatemala",
	GU.GUM.316.GQ	"Guam",
	GW.GNB.624.PU	"Guinea-Bissau",
	GY.GUY.328.GY	"Guyana",
	HK.HKG.344.HK	"Hong Kong S.A.R., China",
	HM.HMD.334.HM	"Heard Island and McDonald Islands",
	HN.HND.340.HO	"Honduras",
	HR.HRV.191.HR	"Croatia",
	HT.HTI.332.HA	"Haiti",
	HU.HUN.348.HU	"Hungary",
	ID.IDN.360.ID	"Indonesia",
	IE.IRL.372.EI	"Ireland",
	IL.ISR.376.IS	"Israel",
	IM.IMN.833.IM	"Isle of Man",
	IN.IND.356.IN	"India",
	IO.IOT.86.IO	"British Indian Ocean Territory",
	IQ.IRQ.368.IZ	"Iraq",
	IR.IRN.364.IR	"Iran",
	IS.ISL.352.IC	"Iceland",
	IT.ITA.380.IT	"Italy",
	JE.JEY.832.JE	"Jersey",
	JM.JAM.388.JM	"Jamaica",
	JO.JOR.400.JO	"Jordan",
	JP.JPN.392.JA	"Japan",
	KE.KEN.404.KE	"Kenya",
	KG.KGZ.417.KG	"Kyrgyzstan",
	KH.KHM.116.CB	"Cambodia",
	KI.KIR.296.KR	"Kiribati",
	KM.COM.174.CN	"Comoros",
	KN.KNA.659.SC	"Saint Kitts and Nevis",
	KP.PRK.408.KN	"North Korea",
	KR.KOR.410.KS	"South Korea",
	KW.KWT.414.KU	"Kuwait",
	KY.CYM.136.CJ	"Cayman Islands",
	KZ.KAZ.398.KZ	"Kazakhstan",
	LA.LAO.418.LA	"Laos",
	LB.LBN.422.LE	"Lebanon",
	LC.LCA.662.ST	"Saint Lucia",
	LI.LIE.438.LS	"Liechtenstein",
	LK.LKA.144.CE	"Sri Lanka",
	LR.LBR.430.LI	"Liberia",
	LS.LSO.426.LT	"Lesotho",
	LT.LTU.440.LH	"Lithuania",
	LU.LUX.442.LU	"Luxembourg",
	LV.LVA.428.LG	"Latvia",
	LY.LBY.434.LY	"Libya",
	MA.MAR.504.MO	"Morocco",
	MC.MCO.492.MN	"Monaco",
	MD.MDA.498.MD	"Moldova",
	ME.MNE.499.MJ	"Montenegro",
	MG.MDG.450.MA	"Madagascar",
	MH.MHL.584.RM	"Marshall Islands",
	MK.MKD.807.MK	"Macedonia",
	ML.MLI.466.ML	"Mali",
	MM.MMR.104.BM	"Myanmar",
	MN.MNG.496.MG	"Mongolia",
	MO.MAC.446.MC	"Macao S.A.R., China",
	MP.MNP.580.CQ	"Northern Mariana Islands",
	MQ.MTQ.474.MB	"Martinique",
	MR.MRT.478.MR	"Mauritania",
	MS.MSR.500.MH	"Montserrat",
	MT.MLT.470.MT	"Malta",
	MU.MUS.480.MP	"Mauritius",
	MV.MDV.462.MV	"Maldives",
	MW.MWI.454.MI	"Malawi",
	MX.MEX.484.MX	"Mexico",
	MY.MYS.458.MY	"Malaysia",
	MZ.MOZ.508.MZ	"Mozambique",
	NA.NAM.516.WA	"Namibia",
	NC.NCL.540.NC	"New Caledonia",
	NE.NER.562.NG	"Niger",
	NF.NFK.574.NF	"Norfolk Island",
	NG.NGA.566.NI	"Nigeria",
	NI.NIC.558.NU	"Nicaragua",
	NL.NLD.528.NL	"Netherlands",
	NO.NOR.578.NO	"Norway",
	NP.NPL.524.NP	"Nepal",
	NR.NRU.520.NR	"Nauru",
	NU.NIU.570.NE	"Niue",
	NZ.NZL.554.NZ	"New Zealand",
	OM.OMN.512.MU	"Oman",
	PA.PAN.591.PM	"Panama",
	PE.PER.604.PE	"Peru",
	PF.PYF.258.FP	"French Polynesia",
	PG.PNG.598.PP	"Papua New Guinea",
	PH.PHL.608.RP	"Philippines",
	PK.PAK.586.PK	"Pakistan",
	PL.POL.616.PL	"Poland",
	PM.SPM.666.SB	"Saint Pierre and Miquelon",
	PN.PCN.612.PC	"Pitcairn",
	PR.PRI.630.RQ	"Puerto Rico",
	PS.PSE.275.WE	"Palestinian Territory",
	PT.PRT.620.PO	"Portugal",
	PW.PLW.585.PS	"Palau",
	PY.PRY.600.PA	"Paraguay",
	QA.QAT.634.QA	"Qatar",
	RE.REU.638.RE	"Reunion",
	RO.ROU.642.RO	"Romania",
	RS.SRB.688.RB	"Serbia",
	RU.RUS.643.RS	"Russia",
	RW.RWA.646.RW	"Rwanda",
	SA.SAU.682.SA	"Saudi Arabia",
	SB.SLB.90.BP	"Solomon Islands",
	SC.SYC.690.SE	"Seychelles",
	SD.SDN.736.SU	"Sudan",
	SE.SWE.752.SW	"Sweden",
	SG.SGP.702.SN	"Singapore",
	SH.SHN.654.SH	"Saint Helena",
	SI.SVN.705.SI	"Slovenia",
	SJ.SJM.744.SV	"Svalbard and Jan Mayen",
	SK.SVK.703.LO	"Slovakia",
	SL.SLE.694.SL	"Sierra Leone",
	SM.SMR.674.SM	"San Marino",
	SN.SEN.686.SG	"Senegal",
	SO.SOM.706.SO	"Somalia",
	SR.SUR.740.NS	"Suriname",
	ST.STP.678.TP	"Sao Tome and Principe",
	SV.SLV.222.ES	"El Salvador",
	SY.SYR.760.SY	"Syria",
	SZ.SWZ.748.WZ	"Swaziland",
	TC.TCA.796.TK	"Turks and Caicos Islands",
	TD.TCD.148.CD	"Chad",
	TF.ATF.260.FS	"French Southern Territories",
	TG.TGO.768.TO	"Togo",
	TH.THA.764.TH	"Thailand",
	TJ.TJK.762.TI	"Tajikistan",
	TK.TKL.772.TL	"Tokelau",
	TL.TLS.626.TT	"East Timor",
	TM.TKM.795.TX	"Turkmenistan",
	TN.TUN.788.TS	"Tunisia",
	TO.TON.776.TN	"Tonga",
	TR.TUR.792.TU	"Turkey",
	TT.TTO.780.TD	"Trinidad and Tobago",
	TV.TUV.798.TV	"Tuvalu",
	TW.TWN.158.TW	"Taiwan",
	TZ.TZA.834.TZ	"Tanzania",
	UA.UKR.804.UP	"Ukraine",
	UG.UGA.800.UG	"Uganda",
	UM.UMI.581."-"	"United States Minor Outlying Islands",
	US.USA.840.US	"United States",
	UY.URY.858.UY	"Uruguay",
	UZ.UZB.860.UZ	"Uzbekistan",
	VA.VAT.336.VT	"Vatican",
	VC.VCT.670.VC	"Saint Vincent and the Grenadines",
	VE.VEN.862.VE	"Venezuela",
	VG.VGB.92.VI	"British Virgin Islands",
	VI.VIR.850.VQ	"U.S. Virgin Islands",
	VN.VNM.704.VM	"Vietnam",
	VU.VUT.548.NH	"Vanuatu",
	WF.WLF.876.WF	"Wallis and Futuna",
	WS.WSM.882.WS	"Samoa",
	YE.YEM.887.YM	"Yemen",
	YT.MYT.175.MF	"Mayotte",
	ZA.ZAF.710.SF	"South Africa",
	ZM.ZMB.894.ZA	"Zambia",
	ZW.ZWE.716.ZI	"Zimbabwe"/;


*	Mapping from gtap9 to ISO3: 

set gtap9iso3(gtap9,*) / 

* Only list the aggregated regions:

AUS.(	AUS !	Australia
	CCK !	Cocos (Keeling) Islands
	CXR !	Christmas Island
	HMD !	Heard Island and McDonald Islands
	NFK !	Norfolk Island
    )

XOC.(	COK !	Cook Islands
	FJI !	Fiji
	FSM !	Micronesia Federated States of
	GUM !	Guam
	KIR !	Kiribati
	MHL !	Marshall Islands
	MNP !	Northern Mariana Islands
	NCL !	New Caledonia
	NIU !	Niue
	NRU !	Nauru
	PLW !	Palau
	PNG !	Papua New Guinea
	PYF !	French Polynesia
	SLB !	Solomon Islands
	TKL !	Tokelau
	TON !	Tonga
	TUV !	Tuvalu
	VUT !	Vanuatu
	WLF !	Wallis and Futuna
	WSM !	Samoa
	PCN !	Pitcairn
	UMI !	United States Minor Outlying Islands 
     )

XEA.(	MAC !	Macao
	PRK !	Korea Democratic Peoples Republic of
     )

XSE.(	BRN !	Brunei Darussalam
	MMR !	Myanmar
	TLS !	Timor Leste 
     )

XSA.(	AFG !	Afghanistan
	BTN !	Bhutan
	MDV !	Maldives
    )

XNA.(	BMU !	Bermuda
	GRL !	Greenland
	SPM !	Saint Pierre and Miquelon 
    )


XSM.(	FLK !	Falkland Islands (Malvinas)
	GUF !	French Guiana
	GUY !	Guyana
	SUR !	Suriname
	SGS !	South Georgia and the South Sandwich Islands)
    )


XCA.(	BLZ !	Belize
    )
	

XCB.(	ABW !	Aruba
	AIA !	Anguilla
	ANT !	Netherlands Antilles
	ATG !	Antigua & Barbuda
	BHS !	Bahamas
	BRB !	Barbados
	CUB !	Cuba
	CYM !	Cayman Islands
	DMA !	Dominica
	GRD !	Grenada
	HTI !	Haiti
	KNA !	Saint Kitts and Nevis
	LCA !	Saint Lucia
	MSR !	Montserrat
	TCA !	Turks and Caicos Islands
	VCT !	Saint Vincent and the Grenadines
	VGB !	Virgin Islands British
	VIR !	Virgin Islands U.S. 
    )

FIN.(	FIN !	Finland 
	ALA !	Aland Islands
    )

FRA.(	FRA !	France 
	GLP !	Guadeloupe
	MTQ !	Martinique
	REU !	Reunion
    )

NOR.(	NOR !	Norway
	SJM !	Svalbard and Jan Mayen 
    )

XEF.(	ISL !	Iceland
	LIE !	Liechtenstein 
    )

XEE.(	MDA !	Republic of Moldova
    )

XER.(	AND !	Andorra
	BIH !	Bosnia and Herzegovina
	FRO !	Faroe Islands
	GIB !	Gibraltar
	MCO !	Monaco
	MKD !	The former Yugoslav Republic of Macedonia 
	SMR !	San Marino
	SRB !	Serbia
	GGY !	Guernsey
	IMN !	Isle of Man
	JEY !	Jersey
	MNE !	Montenegro
	VAT !	Holy See (Vatican City State) 
    )

XSU.(	TJK !	Tajikistan
	TKM !	Turkmenistan
	UZB !	Uzbekistan
    )

XWS.(	IRQ !	Iraq
	LBN !	Lebanon
	PSE !	Palestinian Territory Occupied
	SYR !	Syrian Arab Republic
	YEM !	Yemen
    )

XNF.(	DZA !	Algeria
	LBY !	Libyan Arab Jamahiriya
	ESH !	Western Sahara 
    )

XWF.(	CPV !	Cape Verde
	GMB !	Gambia
	GNB !	Guinea-Bissau
	LBR !	Liberia
	MLI !	Mali
	MRT !	Mauritania
	NER !	Niger
	SHN !	Saint Helena - Ascension and Tristan Da Cunha
	SLE !	Sierra Leone
    )

XCF.(	CAF !	Central African Republic
	COG !	Congo
	GAB !	Gabon
	GNQ !	Equatorial Guinea
	STP !	Sao Tome and Principe
	TCD !	Chad 
    )

XAC.(	AGO !	Angola
	COD !	The Democratic Republic of the Congo
    )

XEC.(	BDI !	Burundi
	COM !	Comoros
	DJI !	Djibouti
	ERI !	Eritrea
	MYT !	Mayotte
	SDN !	Sudan
	SOM !	Somalia
	SYC !	Seychelles 
    )

XSC.(	LSO !	Lesotho
	SWZ !	Swaziland
    )

XTW.(	ATA !	Antarctica
	ATF !	French Southern Territories
	BVT !	Bouvet Island
	IOT !	British Indian Ocean Territory
    ) /;


set	iso3(*)	ISO-3166 three letter codes,
	iso2(*)	ISO-3166 two letter,
	ison(*)	ISO-3166 numeric code,
	fips(*)	FIPS two letter codes;

alias (u1,u2,u3,u4,*);


set	gtap9iso2(gtap9,*)	Mapping from gtap9 to ISO2,
	gtap9fips(gtap9,*)	Mapping from gtap9 to FIPS
	gtap9ison(gtap9,*)	Mapping from gtap9 to ISO numeric codes;

set iso2iso3(*,*)	Mapping from ISO2 to ISO3;

gtap9iso3(gtap9,u2)$sameas(gtap9,u2) = yes;

loop(isomap(u1,u2,u3,u4),
	iso2(u1) = isomap(u1,u2,u3,u4);
	iso3(u2) = isomap(u1,u2,u3,u4);
	ison(u3) = isomap(u1,u2,u3,u4);
	fips(u4) = isomap(u1,u2,u3,u4);
	iso2iso3(u1,u2) = isomap(u1,u2,u3,u4);
	gtap9iso3(gtap9,u2)$gtap9iso3(gtap9,u2) = isomap(u1,u2,u3,u4);
	gtap9iso2(gtap9,u1)$gtap9iso3(gtap9,u2) = isomap(u1,u2,u3,u4);
	gtap9ison(gtap9,u3)$gtap9iso3(gtap9,u2) = isomap(u1,u2,u3,u4);
	gtap9fips(gtap9,u4)$gtap9iso3(gtap9,u2) = isomap(u1,u2,u3,u4);
);
gtap9iso2(gtap9,u2)$(not iso2(u2)) = no;
gtap9iso3(gtap9,u2)$(not iso3(u2)) = no;
gtap9fips(gtap9,u2)$(not fips(u2)) = no;

display fips;

execute_unload 'gdx\isoregions.gdx', gtap9, iso2, iso3, ison,fips,iso2iso3,gtap9iso3,gtap9iso2,gtap9fips,gtap9ison;
