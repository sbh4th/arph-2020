capture log close
log using code/usa-decomp-sex-race, replace text

//  program: usa-decomp-sex-race.do
//  task:    decompose life expectancy by sex and race over time        
//  input:   allcause mortality
//  output:  none
//  project: life expectancy	
//  author:  sam harper \ 2020-04-08


// #0
// program setup

version 14
set linesize 80
clear all
macro drop _all



// #1
// load the mortality data, downloaded from CDC WONDER database
tempname nhisp
import delimited "data/asmr-sex-race-nhisp-1999-2018.txt", ///
  encoding(ISO-8859-1)clear

* drop extra rows for Notes from CDC WONDER
drop notes
drop if year==.


* fix up variable names and labels
encode gender, gen(sex)
encode race, gen(race5)
drop race
rename race5 race
label define race 1 "NH AIAN" 2 "NH API" 3 "NH Black" ///
  4 "NH White" 5 "Hispanic", modify
label values race race
label var race "Race-ethnicity"

replace tenyearagegroups="01-04 years" if tenyearagegroups=="1-4 years"
replace tenyearagegroups="05-14 years" if tenyearagegroups=="5-14 years"
replace tenyearagegroups="00-01 years" if tenyearagegroups=="< 1 year"
encode tenyearagegroups, gen(age)
rename (deaths population) (count pop)

drop gender-tenyearagegroupscode yearcode cruderate

save "data/`nhisp'", replace

* Now for Hispanics
tempname hisp
import delimited "data/asmr-sex-race-hisp-1999-2018.txt", ///
  encoding(ISO-8859-1)clear

* drop extra rows for Notes from CDC WONDER
drop notes
drop if year==.

* fix up variable names and labels
encode gender, gen(sex)
gen race=5
label define race 1 "NH AIAN" 2 "NH API" 3 "NH Black" ///
  4 "NH White" 5 "Hispanic", modify
label values race race
label var race "Race-ethnicity"

replace tenyearagegroups="01-04 years" if tenyearagegroups=="1-4 years"
replace tenyearagegroups="05-14 years" if tenyearagegroups=="5-14 years"
replace tenyearagegroups="00-01 years" if tenyearagegroups=="< 1 year"
encode tenyearagegroups, gen(age)
rename (deaths population) (count pop)

drop gender-tenyearagegroupscode yearcode cruderate

save "data/`hisp'", replace

use "data/`nhisp'", clear
append using "data/`hisp'"

gen rate = count / pop * 100000
label var rate "death rate"
label var count "no. of deaths"
label var pop "mid-year population"

* save this dataset for life expectancy calculations (#3)
save "data/usa-decomp-race", replace

* erase temporary datasets
erase "data/`nhisp'.dta"
erase "data/`hisp'.dta"



// #2
// set up for life table calculation
use "data/usa-decomp-race.dta", clear

* have a look at the rates by year
table age year race, c(mean rate) by(sex) format(%7.1f)

* group by sex and year for faster life table construction
egen class=group(sex race year)

* define number of years in age interval (10-year age groups)
gen n=1 if age==1
replace n=4 if age==2
replace n=10 if age>2
replace n=1 if age==11
label var n "no. years in age interval"

* average person-years contributed by those dying within interval
* assumed to be 1/2 apart from infant mortality
gen ax=0.1 if age==1 // infants
replace ax=0.5 if age>1 & age<=11 // all other age groups

* life table variables
foreach var in m q p l d L T e var_q v sv var_e se_e {
	qui gen `var'x=.
	}

* labels
label var ax "avg time contributed by deaths"
label var mx "death rate at age x"
label var qx "probability of death at age x"
label var px "probability of survival at age x"
label var lx "number alive at age x"
label var dx "expected deaths at age x"
label var Lx "person-years lived in interval"
label var Tx "time lived beyond age x"
label var ex "life expectancy at age x"
label var var_qx "variance of prob. of death"
label var vx "Chiang formula for variance"
label var svx "sum of Chiang formula"
label var var_ex "variance of life expectancy"
label var se_ex "standard error of life expectancy"



// #4
// calculate life table values by group

sort class age

qui levelsof class, local(levels)
foreach l of local levels {

	* mortality rate
	qui replace mx=count/pop if class==`l'   
	
	* probability of death	
	qui replace qx=n*mx/(1+n*(1-ax)*mx) if class==`l'
	qui replace qx = 1 if age==11 & class==`l'
	
	* conditional prob of survival
	qui replace px=1-qx if class==`l'
	
	* no alive at beginning of interval
	qui replace lx = 100000 if age==1 & class==`l'
	qui replace lx = lx[_n-1] * px[_n-1] if age>1 & class==`l'
	
	* Generate deaths by differencing the number of survivors and 
	* noting that everyone dies in the end
	qui replace dx = lx - lx[_n+1] if class==`l'
	qui replace dx = lx if age==11 & class==`l'
	
	* Compute person-years lived in each age group
	* n for those who survive the age group and nax for those who die
	qui replace Lx = n * (lx[_n+1] + (ax*dx)) if class==`l'
	qui replace Lx = lx/mx if age==11 & class==`l'
	

	/* Accumulating from the bottom up is a bit tricky because Stata likes 
	to sum from the top down. You could sort the data from oldest to 
	youngest, sum, and then sort again. I will subtract the cumulative 
	sum from the total.*/
	qui sum Lx if class==`l'
	qui replace Tx = r(sum) - sum(Lx) + Lx if class==`l'
	
	
	* Compute life expectancy 
	*(time lived after each age / survivors to that age)
	qui replace ex = Tx/lx if class==`l'
	
	* variance of cond. probability of death
	qui replace var_qx = [n^2 * mx*(1-ax*n*mx)] / [pop*(1+(1-ax)*n*mx)^3] if class==`l'
	qui replace var_qx = 0 in -1 if class==`l'


	* calculate second part of Chiang formula for variance of LE [add cite] 
	qui replace vx = (lx^2)*[((1-ax)*n+ex[_n+1])^2]*var_qx if class==`l'
	qui replace vx = 0 in -1 if class==`l'
	
	* sum of vx
	qui sum vx if class==`l'
	qui replace svx = r(sum) - sum(vx) + vx if class==`l'
	
	* variance and se of life expectancy
	qui replace var_ex = svx / lx^2 if class==`l'
	qui replace se_ex = sqrt(var_ex) if class==`l'
	}


* specify a few formats
format %6.3f ax ex var_ex se_ex
format %8.6f mx qx px
format %9.0fc pop count lx dx Lx Tx

* table of life expectancies by year
table race year sex if age==1, c(mean ex) format(%4.2f)

* export for joinpoint analysis
gen year0=year-1999
gen age3 = 1 if age==1
replace age3 = 2 if age==5
replace age3 = 3 if age==9
sort age3 sex race year0
export delimited age3 sex race year0 ex se_ex using ///
  "data/le-age-sex-race.csv" if age==1 | age==5 | age==9, ///
  nolabel replace

// #5
// Decompose by age

// drop unnecessary variables and reshape the data to wide format with
// rows for each sex year age and colums for each race group

keep lx Tx Lx mx sex race year age
reshape wide lx Tx Lx mx, i(sex race age) j(year)

/* decompose LE by age, using formulas from Arriaga (1984) 
	Measuring and explaining the change in life expectancies. 
	Demography 1984;21: 83-96. */
	
* choose first and last years for decomposition
local first 2014
local last 2018

* generate direct effect
gen de=(lx`last'/100000) * ((Lx`first'/lx`first') - (Lx`last'/lx`last'))
label var de "direct effect"

* generate indirect effect and interaction term
gen ie=(Tx2014[_n+1]/100000) * ///
  ((lx`last'/lx`first') - (lx`last'[_n+1]/lx`first'[_n+1])) if age!=11
replace ie=0 if age==11
label var ie "indirect effect+interact"

* total effect (direct + indirect + interaction)
* contribution in years of LE
gen te=de+ie
label var te "diff in life exp"

keep de ie te mx`first' mx`last' sex race age

* reshape dataset to wide format to calculate total
reshape wide de ie te mx`first' mx`last', i(sex race) j(age)

foreach var of newlist de ie te {
	egen `var'12 = rsum(`var'*) // sum across age groups
	}

* reshape dataset back to long
reshape long de ie te mx`first' mx`last', i(sex race) j(age)

* total across all age groups
label define age 12 "Total", add
label values age age

* proportional contribution
sort sex race age
bysort sex race: gen pctgap=( te[_n] / te[12] ) * 100

table age race, c(sum te) by(sex) format(%4.3f)
table age race, c(sum pctgap) by(sex) format(%3.2f)

graph hbar (sum) te if sex==1 & race!=1, over(age) by(race, note("")) ///
  label scheme(tufte) ytitle(" ", margin(small)) bar(1, lcolor(black) /// 
  lwidth(medium) fcolor(lavender)) ///
  ytitle("Years of life expectancy", size(medsmall)) ///
  name(women, replace)
  
graph hbar (sum) te if sex==2 & race!=1, over(age) by(race, note("")) ///
  label scheme(tufte) ytitle(" ", margin(small)) bar(1, lcolor(black) /// 
  lwidth(medium) fcolor(lavender)) ///
  ytitle("Years of life expectancy", size(medsmall)) ///
  name(men, replace)


* save this as a dataset for plotting in R
export delimited using ///
  "data/le-age-decomp.csv", nolabel replace


log close
exit
