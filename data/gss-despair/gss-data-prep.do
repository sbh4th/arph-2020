capture log close
log using gss-data-prep, replace text

//  program: gss-data-prep.do
//  task:    create GSS data for analysis     
//  input:   GSS.do, GSS.dct, GSS.dat
//  output:  none
//  project: ARPH-2020
//  author:  sam harper \ 2020-04-02


// #0
// program setup

version 14
set linesize 80
clear all
macro drop _all

// #1
// input GSS data from GSS Explorer extract
do GSS.do


// #2
//demographics

* 10-year age cohorts
egen cohortg = cut(cohort), at(1885(10)2005) icodes
label var cohortg "10 year birth cohort"
label define cohortg 0 "1885-1894" 1 "1895-1904" 2 "1905-1914" ///
  3 "1915-1924" 4 "1925-1934" 5 "1935-1944" 6 "1945-1954" ///
  7 "1955-1964" 8 "1965-1974" 9 "1975-1984" 10 "1985-1994" ///
  11 "1995-2004", modify
label values cohortg cohortg
numlabel cohortg, add

tab cohort cohortg, mis

* 10-year age groups
egen agegp = cut(age), at(20(10)90) icodes
replace agegp = 0 if age==18 | age==19
replace agegp = . if age==98 | age==99
label var agegp "10 year age group"
label define agegp 0 "18-29 yrs" 1 "30-39 yrs" 2 "40-49 yrs" ///
  3 "50-59 yrs" 4 "60-69 yrs" 5 "70-79 yrs" 6 "80+ yrs", modify
label values agegp agegp
numlabel agegp, add

tab age agegp, mis


* education groups
recode degree (0=0 "<HS") (1=1 "HS") (3=2 "College") ///
  (4=3 "Graduate") (8/9 = . ), gen(educ4)
label var educ4 "Highest education degree"
numlabel educ4, add


// #3 
// self-reported measures of happiness, etc.


* binary happiness variables
recode happy (0 = .a) (1 2 8 = 0) (3 = 1) (9 = .b), gen(nthappy)
label var nthappy "Not too happy?"

recode happy (0 = .a) (3 2 8 = 0) (1 = 1) (9 = .b), gen(vhappy)
label var vhappy "Very happy?"

* selfishness
recode helpful (0 = .a) (1 3 8 = 0) (2 = 1) (9 = .b), gen(selfish)
label var self "People look out for themselves?"

* trust
recode trust (0 = .a) (1 3 8 = 0) (2 = 1) (9 = .b), gen(ctrust)
label var ctrust "People can't be trusted?"

* trust in scientific
recode consci (0 = .a) (2 3 8 = 0) (1 = 1) (9 = .b), gen(tsci)
label var tsci "Great deal of trust in scientific community?"

* label for binary measures
label define noyes 0 "No" 1 "Yes" .a "N/A" .b "No answer", modify

label values nthappy vhappy self ctrust tsci noyes

// #4
// save as dataset
export delimited "gss-data.csv", nolabel replace
save gss-data, replace

log close
exit


