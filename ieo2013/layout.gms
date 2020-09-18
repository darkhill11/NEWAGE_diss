$stitle	Define the Worksheet Layouts for the IEO 2010

set	rn(*)	Row numbers from the Excel file
	cn(*)	Column numbers from the Excel file
	sn(*)	Sheet numbers
	wn(*)	Worksheet names;

$load rn=r cn=c sn=s wn=w

set	ws(sn,wn)	Worksheet correspondence;
$loaddc ws

parameter	vf(sn,rn,cn)	Worksheet values;
set		vu(sn,rn,cn,*)	Worksheet UELs;

$load vf vu

set y	Years in database and extrapolated values /1990*2050/;

set	yy		All years /1990*2040/,
	yr(yy)		Years referenced in the tables;

alias (s,en,*);

set r	Regions /
	OECDNAM		OECD North America					
	USA		United States 
	CAN		Canada
	MEXCHI		Mexico and CHile
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

	alias (r,rr);

$eolcom !

set rgmap(*,r) /
	r6.OECDNAM	! OECD North America
	r7.USA		! United States /a
	r8.CAN		! Canada
	r9.MEXCHI	! Mexico and Chile
	r10.OECDEUR	! OECD Europe
	r11.OECDASIA	! OECD Asia
	r12.JPN		! Japan
	r13.KOR		! South Korea
	r14.AUSNZL	! Australia/New Zealand
	r15.OECD	! Total OECD
	r18.NOECDEUR	! Non-OECD Europe and Eurasia
	r19.RUS		! Russia
	r20.OTHNOECDEUR	! Other
	r21.NOECDASIA	! Non-OECD Asia
	r22.CHN		! China
	r23.IND		! India
	r24.OTHNOECDASIA ! Other Non-OECD Asia
	r25.MIDEAST	! Middle East
	r26.AFRICA	! Africa
	r27.CSA		! Central and South America
	r28.BRA		! Brazil
	r29.OTHCSA	! Other Central and South America
	r30.NOECD	! Total Non-OECD
	r32.WORLD	! Total World
/;

set enmap(*,r,*)  Energy table /

*	  OECD North America
r7.oecdnam.liquids	! Liquids
r8.oecdnam.gas		! Natural Gas
r9.oecdnam.col		! Coal
r10.oecdnam.nuc		! Nuclear
r11.oecdnam.oth		! Other
r12.oecdnam.tot		! Total
  
*         OECD Europe
r14.oecdeur.liquids	! Liquids
r15.oecdeur.gas		! Natural Gas
r16.oecdeur.col		! Coal
r17.oecdeur.nuc		! Nuclear
r18.oecdeur.oth		! Other
r19.oecdeur.tot		!   Total
  
*         OECD Asia
r21.oecdasia.liquids	! Liquids
r22.oecdasia.gas	! Natural Gas
r23.oecdasia.col	! Coal
r24.oecdasia.nuc	! Nuclear
r25.oecdasia.oth	! Other
r26.oecdasia.tot	! Total
  
*         Total OECD
r28.oecd.liquids	! Liquids
r29.oecd.gas		! Natural Gas
r30.oecd.col		! Coal
r31.oecd.nuc		! Nuclear
r32.oecd.oth		! Other
r33.oecd.tot		! Total
  
*       Non-OECD
r37.noecdeur.liquids	! Liquids
r38.noecdeur.gas	! Gas
r39.noecdeur.col	! Coal
r40.noecdeur.nuc	! Nuclear
r41.noecdeur.oth	! Other
r42.noecdeur.tot	! Total
  
*         Non-OECD Asia
r44.noecdasia.liquids	! Liquids
r45.noecdasia.gas	! Natural Gas
r46.noecdasia.col	! Coal
r47.noecdasia.nuc	! Nuclear
r48.noecdasia.oth	! Other
r49.noecdasia.tot	! Total
  
*         Middle East
r51.mideast.liquids	! Liquids
r52.mideast.gas		! Natural Gas
r53.mideast.col		! Coal
r54.mideast.nuc		! Nuclear
r55.mideast.oth		! Other
r56.mideast.tot		! Total
  
*         Africa
r58.africa.liquids	! Liquids
r59.africa.gas		! Natural Gas
r60.africa.col		! Coal
r61.africa.nuc		! Nuclear
r62.africa.oth		! Other
r63.africa.tot		! Total
  
*         Central and South America
r65.csa.liquids		! Liquids
r66.csa.gas		! Natural Gas
r67.csa.col		! Coal
r68.csa.nuc		! Nuclear
r69.csa.oth		! Other
r70.csa.tot		! Total
  
*         Total Non-OECD
r72.noecd.liquids	! Liquids
r73.noecd.gas		! Natural Gas
r74.noecd.col		! Coal
r75.noecd.nuc		! Nuclear
r76.noecd.oth		! Other
r77.noecd.tot		! Total
  
*       Total World
r80.world.liquids	! Liquids
r81.world.gas		! Natural Gas
r82.world.col		! Coal
r83.world.nuc		! Nuclear
r84.world.oth		! Other
r85.world.tot		! Total
/;


*	Regions with regional data to be shared out:

set rd /oecdnam,oecdasia,noecdeur,noecdasia,csa/;

set rmap(rd,*) /
	oecdnam.(usa,can,mexchi)
	oecdasia.(jpn,kor,ausnzl)
	noecdeur.(rus,othnoecdeur)
	noecdasia.(chn,ind,othnoecdasia)
	csa.(bra,othcsa)/;

set enshare/liquids,gas,col,nuc,oth/;

set  smap(*,s,en)  Mapping for end-use sectors /


*	Residential
	r6.Residential.Liquids
	r7.Residential.Gas
	r8.Residential.Coal
	r9.Residential.Electricity
	r10.Residential.Renewables
	r11.Residential.Total
*	Commercial
	r14.Commercial.Liquids
	r15.Commercial.Gas
	r16.Commercial.Coal
	r17.Commercial.Electricity
	r18.Commercial.Renewables
	r19.Commercial.Total
*	Industrial
	r22.Industrial.Liquids
	r23.Industrial.Gas
	r24.Industrial.Coal
	r25.Industrial.Electricity
	r26.Industrial.Renewables
	r27.Industrial.Total
*	Transportation
	r30.Transportation.Liquids
	r31.Transportation.Gas
	r32.Transportation.Coal
	r33.Transportation.Electricity
	r34.Transportation.Total
*	All End-Use Sectors
	r37.All.Liquids
	r38.All.Gas
	r39.All.Coal
	r40.All.Electricity
	r41.All.Renewables
	r42.All.DeliveredEnergy 
	r43.All.Electricity-RelatedLosses
	r44.All.Total
*	Electric Power/a
*	a/Fuel inputs used in the production of electricity and heat at central-station generators.
	r47.ElectricPower.Liquids
	r48.ElectricPower.Gas
	r49.ElectricPower.Coal
	r50.ElectricPower.Nuclear
	r51.ElectricPower.Renewables
	r52.ElectricPower.Total
*	Total Energy Consumption
	r55.Total.Liquids
	r56.Total.Gas
	r57.Total.Coal
	r58.Total.Nuclear
	r59.Total.Renewables
	r60.Total.Total /;


alias (rx,*);

set oilmap(*,rx)	"Region mapping for tables G1,G4,G7,G10,G13"/
r5.opec		! OPEC /a
r6.opecmideast	! Middle East
r7.opecnafr	! North Africa
r8.opecwafr	! West Africa
r9.opecsam	! South America
r10.nopec	! Non-OPEC
r11.oecd	! OECD
r12.oecdnam	! OECD North America
r13.USA		! United States
r14.CAN		! Canada
r15.mexchi	! Mexico and Chile
r16.oecdeur	! OECD Europe
r17.northsea	! North Sea
r18.othoecdeur	! Other
r19.oecdasia	! OECD Asia
r20.ausnzl		! Australia and New Zealand
r21.othoecdasia	! Other
r22.noecd	! Non-OECD
r23.noecdeur	! Non-OECD Europe and Eurasia
r24.RUS		! Russia
r25.caspian	! Caspian Area
r26.KAZ		! Kazakhstan
r27.othcaspian	! Other
r28.othnoecdeur	! Other
r29.noecdasia	! Non-OECD Asia
r30.CHN		! China
r31.IND		! India
r32.othnoecdasia ! Other
r33.mideast	! Middle East (Non-OPEC)
r34.OMN		! Oman
r35.othmideast	! Other
r36.africa	! Africa
r37.GHA		! Ghana
r38.othafrica	! Other
r39.csa		! Central and South America
r40.BRA		! Brazil
r41.othcsa      ! Other
r42.world       ! Total World
r43.pctopec	! OPEC Share of World Production
r44.pctgulf	! Persian Gulf Share of World Production
/;

set conv(*,rx) "Region mapping for tables G2,G5,G8,G11,G14" /
r5.opec		! OPEC /a
r6.opecmideast	! Middle East
r7.opecnafr	! North Africa
r8.opecwafr	! West Africa
r9.opecsam	! South America
r10.nopec	! Non-OPEC
r11.oecd	! OECD
r12.oecdnam	! OECD North America
r13.USA		! United States
r14.CAN		! Canada
r15.mexchi	! Mexico and Chile
r16.oecdeur	! OECD Europe
r17.northsea	! North Sea
r18.othoecdeur	! Other
r19.oecdasia	! OECD Asia
r20.ausnzl		! Australia and New Zealand
r21.othoecdasia	! Other
r22.noecd	! Non-OECD
r23.noecdeur	! Non-OECD Europe and Eurasia
r24.RUS		! Russia
r25.caspian	! Caspian Area
r26.KAZ		! Kazakhstan
r27.othcaspian	! Other
r28.othnoecdeur	! Other
r29.noecdasia	! Non-OECD Asia
r30.CHN		! China
r31.IND		! India
r32.othnoecdasia ! Other
r33.mideast	! Middle East (Non-OPEC)
r34.africa	! Africa
r35.csa		! Central and South America
r36.BRA		! Brazil
r37.othcsa 	! Other
r38.world  	! Total World
r39.pctopec	! OPEC Share of World Production
r40.pctgulf	! Persian Gulf Share of World Production
/;

set unconv(*,rx,en) "Region mapping for tables G3,G6,G9,G12,G15"/

r5.opec.total	! OPEC /a
r6.opec.biof	! Biofuels/b
r7.opec.ctl     ! Coal-to-liquids
r8.opec.gtl     ! Gas-to-liquids (primarily Qatar)
r9.nopec.total  ! Non-OPEC
r10.oecd.total  ! OECD
r11.oecd.biof   ! Biofuels/b
r12.oecd.ctl    ! Coal-to-liquids
r13.oecd.gtl    ! Gas-to-liquids
r14.oecd.sands  ! Kerogen
r15.noecd.total ! Non-OECD
r16.noecd.biof  ! Biofuels/b
r17.noecd.ctl   ! Coal-to-liquids
r18.noecd.gtl   ! Gas-to-liquids
r19.world.total ! World 
r20.world.biof  ! Biofuels/b
r21.BRA.biof	! Brazil
r22.CHN.biof	! China
r23.IND.biof	! India
r24.USA.biof	! United States
r25.world.ctl	! Coal-to-liquids
r26.AUSNZL.ctl	! Australia/New Zealand
r27.CHN.ctl	! China
r28.DEU.ctl	! Germany
r29.IND.ctl	! India
r30.ZAF.ctl	! South Africa
r31.USA.ctl	! United States
r32.world.gtl	! Gas-to-liquids
r33.QAT.gtl	! Qatar
r34.ZAF.gtl	! South Africa
/;
