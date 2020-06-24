capture log close
log using "code/usa-decomp-age-cause-2010-2018.txt", replace text

//  program: usa-decomp-age-cause-2010-2018.do
//  task:    decompose life expectancy by sex and race over time        
//  input:   allcause and cause-specific mortality from CDC WONDER
//  output:  none
//  project: life expectancy	
//  author:  sam harper \ 2020-06-24


// #0
// program setup

version 16
set linesize 80
clear all
macro drop _all



// #1
// load the mortality data, downloaded from CDC WONDER database
tempname nhisp
import delimited "data/cdc-wonder/us-age-cause-nhisp-2010-2018.txt", ///
  encoding(ISO-8859-1) clear

* drop extra rows for Notes from CDC WONDER
drop if year==.
drop notes

* fix up variable names and labels
encode gender, gen(sex)
encode race, gen(race4)
drop if race4==1 // drop American Indians (unreliable)
drop race
rename race4 race
replace race = race - 1
label define race 1 "NH API" 2 "NH Black" 3 "NH White", modify
label values race race

replace tenyearagegroups="01-04 years" if tenyearagegroups=="1-4 years"
replace tenyearagegroups="05-14 years" if tenyearagegroups=="5-14 years"
replace tenyearagegroups="00-01 years" if tenyearagegroups=="< 1 year"
encode tenyearagegroups, gen(age)
rename (deaths population icd10113causelist) (count pop icdcode)

* extract numeric causes of death from CDCs 113 cause list
gen cod113s=substr(icd10113causelistcode,7,3)
destring cod113s, gen(cod113)

* recode to a small number of causes (arbitrary)
recode cod113 (16 = 6 "HIV") (19 = 2 "Cancers") (46 = 3 "Diabetes") ///
  (52 = 4 "Alzheimer's") (53 = 1 "CVDs") (76 = 5 "Flu/pneumonia") ///
  (82 = 7 "Chronic Resp dx") (93 = 8 "Liver dx") (97 = 9 "Kidney dx") ///
  (114 = 10 "MV crashes") (122 = 11 "Poisoning") ///
  (124 = 12 "Suicide") (127 = 13 "Homicide") ///
  (1/15 17 18 44 45 47 50 51 79 87/92 96 102/105 108/111 115 116 118/121 ///
  123 130 131 134/136 = 14 "Residual") ///
  (20/43 48 49 54/75 77 78 80 81 83/86 94 95 98/101 106 107 112 113 117 ///
  125 126 128 129 132 133 = .), gen(cod14)

* summarize over sex, age, cause and year  
collapse (sum) count (max) pop, by(sex race age cod14 year)

* drop extra cells for causes subsumed within broad categories
* to avoid double counting
drop if cod14==.
drop if year==.

* save as temporary dataset
save "data/cdc-wonder/`nhisp'.dta", replace


// #2 Hispanics
// load the mortality data, downloaded from CDC WONDER database
tempname hisp
import delimited "data/cdc-wonder/us-age-cause-hisp-2010-2018.txt", ///
  encoding(ISO-8859-1) clear

* drop extra rows for Notes from CDC WONDER
drop if year==.
drop notes

* fix up variable names and labels
encode gender, gen(sex)
gen race = 4

replace tenyearagegroups="01-04 years" if tenyearagegroups=="1-4 years"
replace tenyearagegroups="05-14 years" if tenyearagegroups=="5-14 years"
replace tenyearagegroups="00-01 years" if tenyearagegroups=="< 1 year"
encode tenyearagegroups, gen(age)
rename (deaths population icd10113causelist) (count pop icdcode)

* extract numeric causes of death from CDCs 113 cause list
gen cod113s=substr(icd10113causelistcode,7,3)
destring cod113s, gen(cod113)

* recode to a small number of causes (arbitrary)
recode cod113 (16 = 6 "HIV") (19 = 2 "Cancers") (46 = 3 "Diabetes") ///
  (52 = 4 "Alzheimer's") (53 = 1 "CVDs") (76 = 5 "Flu/pneumonia") ///
  (82 = 7 "Chronic Resp dx") (93 = 8 "Liver dx") (97 = 9 "Kidney dx") ///
  (114 = 10 "MV crashes") (122 = 11 "Poisoning") ///
  (124 = 12 "Suicide") (127 = 13 "Homicide") ///
  (1/15 17 18 44 45 47 50 51 79 87/92 96 102/105 108/111 115 116 118/121 ///
  123 130 131 134/136 = 14 "Residual") ///
  (20/43 48 49 54/75 77 78 80 81 83/86 94 95 98/101 106 107 112 113 117 ///
  125 126 128 129 132 133 = .), gen(cod14)

* summarize over sex, age, cause and year  
collapse (sum) count (max) pop, by(sex race age cod14 year)

* drop extra cells for causes subsumed within broad categories
* to avoid double counting
drop if cod14==.
drop if year==.

save "data/cdc-wonder/`hisp'.dta", replace

use "data/cdc-wonder/`nhisp'.dta", clear
append using "data/cdc-wonder/`hisp'.dta"

* calculate crude rates, label some variables
gen rate = count / pop * 100000
label define race 1 "NH API" 2 "NH Black" 3 "NH White" 4 "Hispanic", modify
label values race race
label var cod14 "cause of death"
label var rate "death rate"
label var count "no. of deaths"
label var pop "mid-year population"

* save this dataset for life expectancy calculations
save "data/cdc-wonder/usa-decomp-age-cause-2010-2018.dta", replace


// #2
// set up for life table calculation

* sum deaths and population over causes (i.e., ignoring cause of death)
use "data/cdc-wonder/usa-decomp-age-cause-2010-2018.dta", clear
collapse (sum) count (max) pop, by(sex race age year)

* mortality rate
gen rate = count / pop * 100000
label var rate "death rate per 100,000"

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
local first 2010
local last 2018

* generate direct effect
gen de=(lx`last'/100000) * ((Lx`first'/lx`first') - (Lx`last'/lx`last'))
label var de "direct effect"

* generate indirect effect and interaction term
gen ie=(Tx`first'[_n+1]/100000) * ///
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

/*
graph hbar (sum) te if sex==1, over(age) by(race, note("")) ///
  label scheme(tufte) ytitle(" ", margin(small)) bar(1, lcolor(black) /// 
  lwidth(medium) fcolor(lavender)) ///
  ytitle("Years of life expectancy", size(medsmall)) ///
  name(women, replace)
  
graph hbar (sum) te if sex==2, over(age) by(race, note("")) ///
  label scheme(tufte) ytitle(" ", margin(small)) bar(1, lcolor(black) /// 
  lwidth(medium) fcolor(lavender)) ///
  ytitle("Years of life expectancy", size(medsmall)) ///
  name(men, replace)
*/

* save this as a dataset for plotting in R
export delimited using ///
  "data/cdc-wonder/le-age-decomp-2010-2018.csv", nolabel replace
  
* save as temporary dataset for cause-specific decompositon
tempname agerace
save "data/cdc-wonder/`agerace'.dta", replace



// #6
// estimate cause-specific proportion of deaths

* load the mortality data
use "data/cdc-wonder/usa-decomp-age-cause-2010-2018.dta", clear

* calculate proportion of deaths for each cause by sex, year age
rename cod14 cod
drop pop rate
reshape wide count, i(sex race age cod) j(year)

* choose first and last years for decomposition
local first 2010
local last 2018 

* now reshape wide again to get deaths by cause as variables
reshape wide count`first' count`last', i(sex race age) j(cod)

* total deaths for each group
foreach v of numlist `first' `last' {
  egen tdeaths`v' = rsum(count`v'*)
}

* proportion of deaths for each cause, by age, year, race
forvalues i=1/14 {
  gen pdeaths`first'`i' = count`first'`i' / tdeaths`first'
  gen pdeaths`last'`i' = count`last'`i' / tdeaths`last'
}

* save dataset for merging with age-decompositions
tempname codrace
save "data/cdc-wonder/`codrace'.dta", replace


// #7
// now decomposition by age and cause of death

* load the age decomposition
use "data/cdc-wonder/`agerace'.dta", clear
drop if age==12 // drop total for all ages

* merge with proportion of deaths by cause
merge 1:1 sex race age using "data/cdc-wonder/`codrace'.dta"
drop _merge

/* formula for partitioning of each age group component by cause of death
  from Arriaga EE. Changing trends in mortality decline during the last
  decades. In Ruzicka et al. Differential mortality: Methodological issues 
  and biosocial factors. 1989;p. 105–29.*/

local i = 1
while `i' < 15 {
	gen cause`i' = te* (((mx`first'*pdeaths`first'`i') - ///
	  (mx`last'*pdeaths`last'`i')) ///
	/ (mx`first'-mx`last'))
	local ++i
}

* drop proportions of deaths by cause
drop count* pdeaths*

* reshape long by cause
reshape long cause, i(sex race age) j(cod)

rename cause cont
label var cont "contribution to LE gap"

* save this as a dataset for plotting in R
export delimited using ///
  "data/cdc-wonder/le-age-cause-decomp-2010-2018.csv", nolabel replace

  
* proportional contribution to change in the gap
egen gap=total(cont), by(sex race)
gen pctgapc=cont/gap * 100

table cod race, contents(sum pctgapc) row by(sex) format(%3.1f)

collapse (sum) cont, by(sex race cod)

reshape wide cont, i(sex race) j(cod)
egen cont15 = rsum(cont*)
reshape long cont, i(sex race) j(cod)

label define cod 15 "Total", add

* proportional contribution
sort sex race cod
bysort sex race: gen pctgapc=cont[_n] / cont[15] * 100

* erase temporary datasets
erase "data/cdc-wonder/`hisp'.dta"
erase "data/cdc-wonder/`nhisp'.dta"
erase "data/cdc-wonder/`agerace'.dta"
erase "data/cdc-wonder/`codrace'.dta"
log close
exit
