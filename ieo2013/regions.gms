set	r	Regions (with explicit descriptions) /
AUS	Australia
*	 Australia
*	 Christmas Island
*	 Cocos (Keeling) Islands
*	 Heard Island and McDonald Islands
*	 Norfolk Island
NZL	New Zealand
XOC	Rest of Oceania
*	 American Samoa
*	 Cook Islands
*	 Fiji
*	 French Polynesia
*	 Guam
*	 Kiribati
*	 Marshall Islands
*	 Micronesia Federated States of
*	 Nauru
*	 New Caledonia
*	 Niue
*	 Northern Mariana Islands
*	 Palau
*	 Papua New Guinea
*	 Pitcairn
*	 Samoa
*	 Solomon Islands
*	 Tokelau
*	 Tonga
*	 Tuvalu
*	 United States Minor Outlying Islands
*	 Vanuatu
*	 Wallis and Futuna
CHN	China
HKG	Hong Kong
JPN	Japan
KOR	Korea Republic of
MNG	Mongolia
TWN	Taiwan
XEA	Rest of East Asia
*	 Korea Democratic Peoples Republic of
*	 Macao
BRN	Brunei Darussalam
KHM	Cambodia
IDN	Indonesia
LAO	Lao People's Democratic Republic
MYS	Malaysia
PHL	Philippines
SGP	Singapore
THA	Thailand
VNM	Viet Nam
XSE	Rest of Southeast Asia
*	 Myanmar
*	 Timor Leste
BGD	Bangladesh
IND	India
NPL	Nepal
PAK	Pakistan
LKA	Sri Lanka
XSA	Rest of South Asia
*	 Afghanistan
*	 Bhutan
*	 Maldives
CAN	Canada
USA	United States of America
MEX	Mexico
XNA	Rest of North America
*	 Bermuda
*	 Greenland
*	 Saint Pierre and Miquelon
ARG	Argentina
BOL	Bolivia
BRA	Brazil
CHL	Chile
COL	Colombia
ECU	Ecuador
PRY	Paraguay
PER	Peru
URY	Uruguay
VEN	Venezuela
XSM	Rest of South America
*	 Falkland Islands (Malvinas)
*	 French Guiana
*	 Guyana
*	 South Georgia and the South Sandwich Islands
*	 Suriname
CRI	Costa Rica
GTM	Guatemala
HND	Honduras
NIC	Nicaragua
PAN	Panama
SLV	El Salvador
XCA	Rest of Central America
*	 Belize
DOM	Dominican Republic
JAM	Jamaica
PRI	Puerto Rico
TTO	Trinidad and Tobago
XCB	Caribbean
*	 Anguilla
*	 Antigua & Barbuda
*	 Aruba
*	 Bahamas
*	 Barbados
*	 Cayman Islands
*	 Cuba
*	 Dominica
*	 Grenada
*	 Haiti
*	 Montserrat
*	 Netherlands Antilles
*	 Saint Kitts and Nevis
*	 Saint Lucia
*	 Saint Vincent and the Grenadines
*	 Turks and Caicos Islands
*	 Virgin Islands British 
*	 Virgin Islands U.S.
AUT	Austria
BEL	Belgium
CYP	Cyprus
CZE	Czech Republic
DNK	Denmark
EST	Estonia
FIN	Finland
*	 Aland Islands
*	 Finland
FRA	France
*	 France
*	 Guadeloupe
*	 Martinique
*	 Reunion
DEU	Germany
GRC	Greece
HUN	Hungary
IRL	Ireland
ITA	Italy
LVA	Latvia
LTU	Lithuania
LUX	Luxembourg
MLT	Malta
NLD	Netherlands
POL	Poland
PRT	Portugal
SVK	Slovakia
SVN	Slovenia
ESP	Spain
SWE	Sweden
GBR	United Kingdom
CHE	Switzerland
NOR	Norway
*	 Norway
*	 Svalbard and Jan Mayen
XEF	Rest of EFTA
*	 Iceland
*	 Liechtenstein
ALB	Albania
BGR	Bulgaria
BLR	Belarus
HRV	Croatia
ROU	Romania
RUS	Russian Federation
UKR	Ukraine
XEE	Rest of Eastern Europe
*	 Moldova Republic of
XER	Rest of Europe
*	 Andorra
*	 Bosnia and Herzegovina
*	 Faroe Islands
*	 Gibraltar
*	 Guernsey 
*	 Holy See (Vatican City State)
*	 Isle of Man
*	 Jersey
*	 Macedonia the former Yugoslav Republic of
*	 Monaco
*	 Montenegro
*	 San Marino
*	 Serbia
KAZ	Kazakhstan
KGZ	Kyrgyzstan
XSU	Rest of Former Soviet Union
*	 Tajikistan
*	 Turkmenistan
*	 Uzbekistan
ARM	Armenia
AZE	Azerbaijan
GEO	Georgia
BHR	Bahrain
IRN	Iran Islamic Republic of
ISR	Israel
JOR	Jordan
KWT	Kuwait
OMN	Oman
QAT	Qatar
SAU	Saudi Arabia
TUR	Turkey
ARE	United Arab Emirates
XWS	Rest of Western Asia
*	 Iraq
*	 Lebanon
*	 Palestinian Territory Occupied 
*	 Syrian Arab Republic
*	 Yemen
EGY	Egypt
MAR	Morocco
TUN	Tunisia
XNF	Rest of North Africa
*	 Algeria
*	 Libyan Arab Jamahiriya
*	 Western Sahara
BEN	Benin
BFA	Burkina Faso
CMR	Cameroon
CIV	Cote d'Ivoire
GHA	Ghana
GIN	Guinea
NGA	Nigeria
SEN	Senegal
TGO	Togo
XWF	Rest of Western Africa
*	 Cape Verde
*	 Gambia
*	 Guinea-Bissau
*	 Liberia
*	 Mali
*	 Mauritania
*	 Niger
*	 Saint Helena, ASCENSION AND TRISTAN DA CUNHA
*	 Sierra Leone
XCF	Central Africa
*	 Central African Republic
*	 Chad
*	 Congo
*	 Equatorial Guinea
*	 Gabon
*	 Sao Tome and Principe
XAC	South Central Africa
*	 Angola
*	 Congo the Democratic Republic of the
ETH	Ethiopia
KEN	Kenya
MDG	Madagascar
MWI	Malawi
MUS	Mauritius
MOZ	Mozambique
RWA	Rwanda
TZA	Tanzania United Republic of
UGA	Uganda
ZMB	Zambia
ZWE	Zimbabwe
XEC	Rest of Eastern Africa
*	 Burundi
*	 Comoros
*	 Djibouti
*	 Eritrea
*	 Mayotte
*	 Seychelles
*	 Somalia
*	 Sudan
BWA	Botswana
NAM	Namibia
ZAF	South Africa
XSC	Rest of South African Customs Union
*	 Lesotho
*	 Swaziland
XTW	Rest of the World
*	 Antarctica
*	 Bouvet Island
*	 British Indian Ocean Territory
*	 French Southern Territories
	/;


set mapr(r,*) /

$onembedded

$eolcom  !

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

set single(r);
alias (u,*);
single(r) = yes$(not sum(mapr(r,u),1));
display single;
mapr(r,r)$single(r) = yes;
option mapr:0:0:1;
display mapr;
