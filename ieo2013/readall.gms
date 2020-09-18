$title	Read IEO2013 worksheets into a GDX file

*.$set load yes
$if set load $goto load

*	Ref case economic indicators
$if exist gdx\reada.gdx $call del gdx\reada.gdx
$call 'gams reada o=lst\reada.lst gdx=gdx\reada.gdx'

*	High_gdp economic indicators
$if exist gdx\readb.gdx $call del gdx\readb.gdx
$call 'gams readb o=lst\readb.lst gdx=gdx\readb.gdx'

*	Low_gdp (macro) economic indicators
$if exist gdx\readc.gdx $call del gdx\readc.gdx
$call 'gams readc o=lst\readc.lst gdx=gdx\readc.gdx'

*	High_oil price economic indicators
$if exist gdx\readd.gdx $call del gdx\readd.gdx
$call 'gams readd o=lst\readd.lst gdx=gdx\readd.gdx'

*	Low_oil price economic indicators
$if exist gdx\reade.gdx $call del gdx\reade.gdx
$call 'gams reade o=lst\reade.lst gdx=gdx\reade.gdx'

*	World fuel production (across all 5 scenarios)
$if exist gdx\readg.gdx $call del gdx\readg.gdx
$call 'gams readg o=lst\readg.lst gdx=gdx\readg.gdx'

*	Delivered energy consumption by end-use sector and fuel and region
*	Here we read out data separately by scenario and then merge data
$if exist gdx\readf.gdx $call del gdx\readf.gdx
$call 'gams readf --ds=a o=lst\readf.lst gdx=gdx\ref.gdx'     
$call 'gams readf --ds=b o=lst\readf.lst gdx=gdx\high_gdp.gdx'
$call 'gams readf --ds=c o=lst\readf.lst gdx=gdx\low_gdp.gdx' 
$call 'gams readf --ds=d o=lst\readf.lst gdx=gdx\high_oil.gdx'
$call 'gams readf --ds=e o=lst\readf.lst gdx=gdx\low_oil.gdx'   
$call 'gdxmerge gdx\ref.gdx gdx\high_gdp.gdx gdx\low_gdp.gdx gdx\high_oil.gdx gdx\low_oil.gdx' 
$call 'copy merged.gdx gdx\readf.gdx'
$call 'del merged.gdx gdx\ref.gdx gdx\high_gdp.gdx gdx\low_gdp.gdx gdx\high_oil.gdx gdx\low_oil.gdx'

*	Electricity genernation and capacity projections
*	Here we read out data separately by scenario and then merge data
$if exist gdx\readh.gdx $call del gdx\readh.gdx
$call 'gams readh --ds=a o=lst\readh.lst gdx=gdx\ref.gdx'     
$call 'gams readh --ds=b o=lst\readh.lst gdx=gdx\high_gdp.gdx'
$call 'gams readh --ds=c o=lst\readh.lst gdx=gdx\low_gdp.gdx' 
$call 'gams readh --ds=d o=lst\readh.lst gdx=gdx\high_oil.gdx'
$call 'gams readh --ds=e o=lst\readh.lst gdx=gdx\low_oil.gdx'   
$call 'gdxmerge gdx\ref.gdx gdx\high_gdp.gdx gdx\low_gdp.gdx gdx\high_oil.gdx gdx\low_oil.gdx' 
$call 'copy merged.gdx gdx\readh.gdx'
$call 'del merged.gdx gdx\ref.gdx gdx\high_gdp.gdx gdx\low_gdp.gdx gdx\high_oil.gdx gdx\low_oil.gdx'

$label load

alias (scn,s,r,en,yr,*);

parameter	a(*,*,yr)	Reference Growth Path,
		b(*,r,yr)	High economic growth path,
		c(*,r,yr)	Low Economic Growth Path,
		d(*,r,yr)	High Oil Price Path
		e(*,r,yr)	Low oil price path,

		f(scn,r,s,*,yr)	Sectoral demand projections,
		g(scn,r,*,yr)	World oil market statistics,
		h(scn,*,r,yr)	Electricity capacity and generation statistics;

$gdxin 'gdx\reada.gdx' 
$load a
$gdxin 'gdx\readb.gdx' 
$load b
$gdxin 'gdx\readc.gdx' 
$load c
$gdxin 'gdx\readd.gdx' 
$load d
$gdxin 'gdx\reade.gdx' 
$load e
$gdxin 'gdx\readf.gdx' 
$load f
$gdxin 'gdx\readg.gdx' 
$load g
$gdxin 'gdx\readh.gdx' 
$load h

execute_unload 'gdx\ieo2013.gdx',a,b,c,d,e,f,g,h;
display a,b,c,d,e,f,g,h;
