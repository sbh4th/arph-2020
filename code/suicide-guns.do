import delimited "data/suic-state-gender-1999-2018.txt", ///
  encoding(ISO-8859-1) clear
  
* drop extra rows for Notes from CDC WONDER
drop notes
drop if year==.

encode gender, gen(sex)

rename (statecode deaths population) (stfips count pop)

drop gender gendercode yearcode cruderate

gen rate = count / pop * 100000

tempname suic
save `suic', replace

import excel "/Users/samharper/Downloads/RAND_TL354.database/TL-354-State-Level Estimates of Household Firearm Ownership.xlsx", sheet("State-Level Data & Factor Score") firstrow clear
rename FIP stfips
rename Year year

tempname guns
save `guns', replace

use `suic'
merge m:1 stfips year using `guns'

keep if _merge==3

tw (scatter rate HFR if year==2016), by(sex)



sort state sex year
bysort state sex: gen crate = rate - rate[1]
bysort state sex: gen chfr = HFR - HFR[1]
tw (lfitci crate chfr if year==2016) (scatter crate chfr if year==2016, mlab(state)), by(sex) ytitle("Difference in suicide rate, 2016-1999") xtitle("Difference in HFR, 2016-1999")

nbreg count i.sex HFR, exp(pop100k) vce(cl stfips)
nbreg count i.sex i.year HFR, exp(pop100k) vce(cl stfips)
nbreg count i.sex i.stfips HFR, exp(pop100k) vce(cl stfips)
nbreg count i.sex i.year i.stfips HFR, exp(pop100k) vce(cl stfips)
margins, at(HFR=(0.25 0.50)) predict(ir)
