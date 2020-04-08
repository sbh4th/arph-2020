#delimit ;

   infix
      year     1 - 20
      kidssol  21 - 40
      suicide1 41 - 60
      suicide2 61 - 80
      suicide3 81 - 100
      suicide4 101 - 120
      cohort   121 - 140
      zodiac   141 - 160
      ballot   161 - 180
      sample   181 - 200
      oversamp 201 - 220
      wtss     221 - 240
      wtssnr   241 - 260
      wtssall  261 - 280
      vstrat   281 - 300
      parsol   301 - 320
      satfin   321 - 340
      id_      341 - 360
      age      361 - 380
      degree   381 - 400
      race     401 - 420
      region   421 - 440
      attend   441 - 460
      happy    461 - 480
      hapmar   481 - 500
      helpful  501 - 520
      trust    521 - 540
      satfrnd  541 - 560
      sathealt 561 - 580
      consci   581 - 600
      satjob   601 - 620
      vpsu     621 - 640
using GSS.dat;

label variable year     "Gss year for this respondent                       ";
label variable kidssol  "Rs kids living standard compared to r";
label variable suicide1 "Suicide if incurable disease";
label variable suicide2 "Suicide if bankrupt";
label variable suicide3 "Suicide if dishonored family";
label variable suicide4 "Suicide if tired of living";
label variable cohort   "Year of birth";
label variable zodiac   "Respondents astrological sign";
label variable ballot   "Ballot used for interview";
label variable sample   "Sampling frame and method";
label variable oversamp "Weights for black oversamples";
label variable wtss     "Weight variable";
label variable wtssnr   "Weight variable";
label variable wtssall  "Weight variable";
label variable vstrat   "Variance stratum";
label variable parsol   "Rs living standard compared to parents";
label variable satfin   "Satisfaction with financial situation";
label variable id_      "Respondent id number";
label variable age      "Age of respondent";
label variable degree   "Rs highest degree";
label variable race     "Race of respondent";
label variable region   "Region of interview";
label variable attend   "How often r attends religious services";
label variable happy    "General happiness";
label variable hapmar   "Happiness of marriage";
label variable helpful  "People helpful or looking out for selves";
label variable trust    "Can people be trusted";
label variable satfrnd  "Friendships";
label variable sathealt "Health and physical condition";
label variable consci   "Confidence in scientific community";
label variable satjob   "Job or housework";
label variable vpsu     "Variance primary sampling unit";


label define gsp001x
   9        "No answer"
   8        "Don't know"
   6        "No children -volunteered-"
   5        "Much worse"
   4        "Somewhat worse"
   3        "About the same"
   2        "Somewhat better"
   1        "Much better"
   0        "Not applicable"
;
label define gsp002x
   9        "No answer"
   8        "Don't know"
   2        "No"
   1        "Yes"
   0        "Not applicable"
;
label define gsp003x
   9        "No answer"
   8        "Don't know"
   2        "No"
   1        "Yes"
   0        "Not applicable"
;
label define gsp004x
   9        "No answer"
   8        "Don't know"
   2        "No"
   1        "Yes"
   0        "Not applicable"
;
label define gsp005x
   9        "No answer"
   8        "Don't know"
   2        "No"
   1        "Yes"
   0        "Not applicable"
;
label define gsp006x
   9999     "No answer"
   0        "Not applicable"
;
label define gsp007x
   99       "No answer"
   98       "Don't know"
   12       "Pisces"
   11       "Aquarius"
   10       "Capricorn"
   9        "Sagittarius"
   8        "Scorpio"
   7        "Libra"
   6        "Virgo"
   5        "Leo"
   4        "Cancer"
   3        "Gemini"
   2        "Taurus"
   1        "Aries"
   0        "Not applicable"
;
label define gsp008x
   4        "Ballot d"
   3        "Ballot c"
   2        "Ballot b"
   1        "Ballot a"
   0        "Not applicable"
;
label define gsp009x
   10       "2010 fp"
   9        "2000 fp"
   8        "1990 fp"
   7        "1980 fp blk oversamp"
   6        "1980 fp"
   5        "1980 bfp blk oversamp"
   4        "1970 fp blk oversamp"
   3        "1970 fp"
   2        "1970 bq"
   1        "1960 bq"
;
label define gsp010x
   9        "No answer"
   8        "Don't know"
   5        "Much worse"
   4        "Somewhat worse"
   3        "About the same"
   2        "Somewhat better"
   1        "Much better"
   0        "Not applicable"
;
label define gsp011x
   9        "No answer"
   8        "Don't know"
   3        "Not at all sat"
   2        "More or less"
   1        "Satisfied"
   0        "Not applicable"
;
label define gsp012x
   99       "No answer"
   98       "Don't know"
   89       "89 or older"
;
label define gsp013x
   9        "No answer"
   8        "Don't know"
   7        "Not applicable"
   4        "Graduate"
   3        "Bachelor"
   2        "Junior college"
   1        "High school"
   0        "Lt high school"
;
label define gsp014x
   3        "Other"
   2        "Black"
   1        "White"
   0        "Not applicable"
;
label define gsp015x
   9        "Pacific"
   8        "Mountain"
   7        "W. sou. central"
   6        "E. sou. central"
   5        "South atlantic"
   4        "W. nor. central"
   3        "E. nor. central"
   2        "Middle atlantic"
   1        "New england"
   0        "Not assigned"
;
label define gsp016x
   9        "Dk,na"
   8        "More thn once wk"
   7        "Every week"
   6        "Nrly every week"
   5        "2-3x a month"
   4        "Once a month"
   3        "Sevrl times a yr"
   2        "Once a year"
   1        "Lt once a year"
   0        "Never"
;
label define gsp017x
   9        "No answer"
   8        "Don't know"
   3        "Not too happy"
   2        "Pretty happy"
   1        "Very happy"
   0        "Not applicable"
;
label define gsp018x
   9        "No answer"
   8        "Don't know"
   3        "Not too happy"
   2        "Pretty happy"
   1        "Very happy"
   0        "Not applicable"
;
label define gsp019x
   9        "No answer"
   8        "Don't know"
   3        "Depends"
   2        "Lookout for self"
   1        "Helpful"
   0        "Not applicable"
;
label define gsp020x
   9        "No answer"
   8        "Don't know"
   3        "Depends"
   2        "Cannot trust"
   1        "Can trust"
   0        "Not applicable"
;
label define gsp021x
   9        "No answer"
   8        "Don't know"
   7        "None"
   6        "A little"
   5        "Some"
   4        "A fair amount"
   3        "Quite a bit"
   2        "Great deal"
   1        "Very great deal"
   0        "Not applicable"
;
label define gsp022x
   9        "No answer"
   8        "Don't know"
   7        "None"
   6        "A little"
   5        "Some"
   4        "A fair amount"
   3        "Quite a bit"
   2        "Great deal"
   1        "Very great deal"
   0        "Not applicable"
;
label define gsp023x
   9        "No answer"
   8        "Don't know"
   3        "Hardly any"
   2        "Only some"
   1        "A great deal"
   0        "Not applicable"
;
label define gsp024x
   9        "No answer"
   8        "Don't know"
   4        "Very dissatisfied"
   3        "A little dissat"
   2        "Mod. satisfied"
   1        "Very satisfied"
   0        "Not applicable"
;


label values kidssol  gsp001x;
label values suicide1 gsp002x;
label values suicide2 gsp003x;
label values suicide3 gsp004x;
label values suicide4 gsp005x;
label values cohort   gsp006x;
label values zodiac   gsp007x;
label values ballot   gsp008x;
label values sample   gsp009x;
label values parsol   gsp010x;
label values satfin   gsp011x;
label values age      gsp012x;
label values degree   gsp013x;
label values race     gsp014x;
label values region   gsp015x;
label values attend   gsp016x;
label values happy    gsp017x;
label values hapmar   gsp018x;
label values helpful  gsp019x;
label values trust    gsp020x;
label values satfrnd  gsp021x;
label values sathealt gsp022x;
label values consci   gsp023x;
label values satjob   gsp024x;


