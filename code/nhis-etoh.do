use "/Users/samharper/git/arph-2020/data/nhis_00013.dta"
recode alcstat2 (0 = .) (10 = 1 "Abstainer") (21/23 = 2 "Former drinker") (31/32 = 3 "Current light") (33 = 4 "Current moderate") (34 = 5 "Current heavy") (99 = .), gen(drink5)
gen heavy = (drink5==5)
recode educrec2 (0 = .) (10 20 31 32 41 = 1 "<HS") (42 = 2 "HS") (51 = 3 "Some coll") (54 60 = 4 "Univ") (97/99 = .), gen(educ4)
logit heavy year##educ4 [pw= perweight] if year>2000 & year<2014
margins educ4#year
marginsplot, xdim(year)
logit heavy age year##educ4 [pw= perweight] if year>2000 & year<2014
margins educ4#year
marginsplot, xdim(year)
