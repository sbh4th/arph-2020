capture log close
log using "code/ur-decomp.txt", replace text

//  program: ur-decomp.do
//  task:    decompose life expectancy by sex and race over time        
//  input:   age- and ca
//  output:  none
//  project: ARPH life expectancy	
//  author:  sam harper \ 2020-06-25


// #0
// program setup

version 16
set linesize 80
clear all
macro drop _all


// #1
// bring in mortality data from SEER*Stat

insheet cod6 sex age19 year ur rate count pop using ///
  "data/seer-stat/asmr-rural-urban-2016.txt", clear
drop rate

* define labels for age groups
label var age19 "age group"
label define age19  0 "00 years" 1 "01-04 years" 2 "05-09 years" ///
  3 "10-14 years" 4 "15-19 years" 5 "20-24 years" 6 "25-29 years" ///
  7 "30-34 years" 8 "35-39 years" 9 "40-44 years" 10 "45-49 years" ///
  11 "50-54 years" 12 "55-59 years" 13 "60-64 years" 14 "65-69 years" ///
  15 "70-74 years" 16 "75-79 years" 17 "80-84 years" 18 "85+ years", modify
label values age19 age19

* label values for sex
label var sex "sex"
label define sex 0 "Both sexes" 1 "Male" 2 "Female", modify
label values sex sex

* label values for rural-urban
label var ur "urban/rural"
label define ur 0 "Urban" 1 "Rural", modify
label values ur ur

* label values for year
label var year "death year"
label define year 0 "1969-71" 1 "2012-2016", modify
label values year year

* destring counts of death
destring count, replace

* proportion of population with suppressed data
sum pop if count== .
scalar miss = r(sum)
sum pop if count!= .
scalar nmiss = r(sum)
disp "Proportion of population with suppressed cells= " ///
  as result %4.2f miss/nmiss*100 "%"

* impute death count of 1-9 based on uniform for missing
set seed 83797
replace count = floor((9 - 1 + 1)*runiform() + 1) if count== .

save "data/seer-stat/ur-data.dta", replace

* collapse by age
collapse (sum) count (mean) pop, by(sex age19 year ur)


// #2
// First collapse across causes regions and estimate LE 

gen rate=count/pop
label var rate "death rate"
label var count "no. of deaths"
label var pop "mid-year population"

egen class=group(sex ur year)

// #2
// set up for life table calculation

* define number of years in age interval
gen n=1 if age19==0 | age19==18
replace n=4 if age19==1
replace n=5 if age19>1 & age19<18
label var n "no. years in age interval"
replace n=. if age19==18

* average person-years contributed by those dying within interval
gen ax=0.1 if age19==0
replace ax=0.5 if age19>0 & age19<=18

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


// #3
// calculate life table values by group

sort class age19

qui levelsof class, local(levels)
foreach l of local levels {

		* mortality rate
	qui replace mx=count/pop if class==`l'    
	
	* probability of death	
	qui replace qx=n*mx/(1+n*(1-ax)*mx) if class==`l'
	qui replace qx = 1 if age19==18 & class==`l'
	
	* conditional prob of survival
	qui replace px=1-qx if class==`l'
	
	
	
	* no alive at beginning of interval
	qui replace lx = 100000 if age19==0 & class==`l'
	qui replace lx = lx[_n-1] * px[_n-1] if age19>0 & class==`l'
	

	
	* Generate deaths by differencing the number of survivors 
	* and noting that everyone dies in the end
	qui replace dx = lx - lx[_n+1] if class==`l'
	qui replace dx = lx if age19==18 & class==`l'
	
	* Compute person-years lived in each age group
	* n for those who survive the age group and nax for those who die
	qui replace Lx = n * (lx[_n+1] + (ax*dx)) if class==`l'
	qui replace Lx = lx/mx if age19==18 & class==`l'
	

	/* Accumulating from the bottom up is a bit tricky because 
	Stata likes to sum from the top down. 
	You could sort the data from oldest to youngest, sum, 
	and then sort again. I will subtract the cumulative sum from the total.*/
	qui sum Lx if class==`l'
	qui replace Tx = r(sum) - sum(Lx) + Lx if class==`l'
	
	
	* Compute life expectancy (time lived after each age / survivors to that age)
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

table ur year if age19==0, c(mean ex) by(sex) format(%3.1f)


// #4
// Decompose by age

// drop unnecessary variables and reshape the data to wide format with
// rows for each sex year age and colums for each race group

keep lx Tx Lx mx sex year age19 ur
reshape wide lx Tx Lx mx, i(sex year age19) j(ur)

	/* decompose LE by age, using formulas from Arriaga (1984) 
	Measuring and explaining the change in life expectancies. 
	Demography 1984;21: 83-96. */

	/* generate direct effect */
gen de=(lx1/100000) * ((Lx0/lx0) - (Lx1/lx1))
label var de "direct effect"

	/* generate indirect effect and interaction term */
gen ie=(Tx0[_n+1]/100000) * ((lx1/lx0) - (lx1[_n+1]/lx0[_n+1])) if age19!=18
replace ie=0 if age19==18
label var ie "indirect effect+interact"

	/* total effect (direct + indirect + interaction) */
gen te=de+ie
label var te "diff in life exp"

	/* calculate total LE gap by race in each year */
egen gap=total(te), by(sex year)
gen pctgap=te/gap

table age19 year sex, contents(sum pctgap) row

keep sex year age19 mx0 mx1 te 
save "data/seer-stat/ur-age.dta", replace




// #5
// cause-specific deaths

* load the mortality data

use "data/seer-stat/ur-data.dta", clear

* label for causes of death
label var cod6 "cause of death"

label define cod6 0 "Cancers" 1 "CVD" 2 "Communicable" 3 "Injuries" ///
  4 "Infant" 5 "Other", modify
label values cod6 cod6
  
* calculate proportion of deaths for each cause by sex, year age
rename cod6 cod
drop pop
reshape wide count, i(year sex age19 cod) j(ur)

rename count0 countu
rename count1 countr

* now reshape wide again to get deaths by cause as variables
reshape wide countu countr, i(year sex age19) j(cod)

* total deaths for each group
foreach v of newlist u r {
  egen tdeaths`v' = rsum(count`v'0 count`v'1 count`v'2 count`v'3 ///
    count`v'4 count`v'5)
}

* proportion of deaths for each cause, by age, year, race
forvalues i=0/5 {
  local j=`i'+1
  gen pdeathsu`j' = countu`i' / tdeathsu
  gen pdeathsr`j' = countr`i' / tdeathsr
}

drop count*

save "data/seer-stat/ur-cod.dta", replace



// #6
// now decomposition by age and cause of death

use "data/seer-stat/ur-age.dta", clear
merge 1:1 year sex age19 using "data/seer-stat/ur-cod.dta"
drop _merge

local i = 1
while `i' < 7 {
	gen cause`i' = te* (((mx0*pdeathsu`i') - (mx1*pdeathsr`i')) / (mx0-mx1))
	local ++i
}

* drop proportions of deaths
drop pdeaths*

egen cause7 = rsum(cause*)

* reshape long by cause
reshape long cause, i(year sex age19) j(cod6)

* label for causes of death
label var cod6 "cause of death"
label define cod6 1 "CVD" 2 "Cancers" 3 "Communicable" 4 "Injuries" ///
  5 "Infant" 6 "Other" 7 "Total", modify
label values cod6 cod6

rename cause cont
label var cont "contribution to LE gap"


* contribution to change in the gap
sort sex age19 cod year
gen diff = .

tempvar start
gen `start'=.
bysort sex age19 cod: replace `start' = cont[1]
bysort sex age19 cod: replace diff = cont[_n] - `start'


* sum over age group, by gender and cause
collapse (sum) cont, by(year sex cod6)

* export summary by cause for plotting
export delimited using "data/seer-stat/ur-decomp-cod.csv", nolabel replace
  
log close
exit
