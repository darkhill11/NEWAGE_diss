$title	Extrapolate GTAP regional statistics to 2050 or 2100

$set extrap2100 yes
$if not set byr $set byr 2007

set	scn		IEO scenarios	 /
		ref		Reference case,
		high_gdp	High GDP growth,
		low_gdp		Low GDP growth,
		high_oil	High oil price,
		low_oil		Low oil price/,

	eg	Final energy goods	/ele,col,oil,gas/,
	fd	Final demand sectors	/Residential, Commercial, Industrial, ElectricPower, Transportation/,
	ieo_tec	IEO technologies /
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
			solgen		Net Solar Electricity Generation /,

	t	Projected and historical time periods /1950*2050,
$if set extrap2100 2055,2060,2065,2070,2075,2080,2085,2090,2095,2100
		/,
	fuel	/col,oil,gas/,
	r(*)	Regions;

$gdxin gdx\ieo_gtap9_%byr%.gdx
$load r=gtap9

parameter	ieocarbon(scn,fuel,r,t)		IEO Carbon emissions by scenario (index -- %byr%=1),
		ieogdp(scn,r,t)			IEO GDP by scenario (index -- %byr%=1),
		ieocrude(scn,r,t)		IEO Crude oil equivalent supply (index -- %byr%=1),
		ieoelesup(scn,r,t)		IEO electricity supply (index -- %byr%=1),
		ieoenergy(scn,eg,fd,r,t)	IEO energy use by sector (index -- %byr%=1),
		ieoprice(scn,t)			IEO oil prices (index -- %byr%=1), 
		ieoele(scn,ieo_tec,r,t)		IEO electricity generation and capacity (index -- %byr%=1),
		unpop(r,*)			UN population trajectories (in millions);


$loaddc ieocarbon ieogdp ieocrude ieoelesup ieoenergy ieoprice ieoele 
$load unpop

set tlate(t) /
$if set extrap2100 2055,2060,2065,2070,2075,2080,2085,2090,2095,2100
/;

display ieogdp;

alias (t,tm5,tm10);

loop(tlate(t),
	loop((tm5,tm10)$(tm5.val=t.val-5 and tm10.val=t.val-10),
	  ieocarbon(scn,fuel,r,t)$ieocarbon(scn,fuel,r,tm10) = 
		sqr(ieocarbon(scn,fuel,r,tm5))/ieocarbon(scn,fuel,r,tm10);
	  ieogdp(scn,r,t)$ieogdp(scn,r,tm10)			= 
		sqr(ieogdp(scn,r,tm5))/ieogdp(scn,r,tm10);
	  ieocrude(scn,r,t)$ieocrude(scn,r,tm10)	= 
		sqr(ieocrude(scn,r,tm5))/ieocrude(scn,r,tm10);
	  ieoelesup(scn,r,t)$ieoelesup(scn,r,tm10)	= 
		sqr(ieoelesup(scn,r,tm5))/ieoelesup(scn,r,tm10);
	  ieoenergy(scn,eg,fd,r,t)$ieoenergy(scn,eg,fd,r,tm10)	= 
		sqr(ieoenergy(scn,eg,fd,r,tm5))/ieoenergy(scn,eg,fd,r,tm10);
	  ieoprice(scn,t)$ieoprice(scn,tm10)		= 
		sqr(ieoprice(scn,tm5))/ieoprice(scn,tm10);
	  ieoele(scn,ieo_tec,r,t)$ieoele(scn,ieo_tec,r,tm10)		= 
		sqr(ieoele(scn,ieo_tec,r,tm5))/ieoele(scn,ieo_tec,r,tm10);
   ));

$set fname gdx\ieo_gtap9_2050_%byr%.gdx
$if set extrap2100 $set fname gdx\ieo_gtap9_2100_%byr%.gdx
execute_unload '%fname%',ieocarbon, ieogdp, ieocrude, ieoelesup, ieoele, ieoenergy, ieoprice, unpop;
