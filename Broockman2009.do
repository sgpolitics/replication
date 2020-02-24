/*Drop anamolous observations as outlined*/
drop if t2_3rdpartyinc == 1
drop if t2_incumbent_has_switched_prty == 1
drop if t2_specialelectiontoeelect == 1
drop if t1_atlargeormulticandidate == 1
drop if t2_redist == 1


/*Create these variables so that they can be used in running a 4th order polynomial*/
	g dwin = 0
	replace dwin = 1 if dv_c_t1 >= 0
	g dv_c_t1_2 = dv_c_t1^2
	g dv_c_t1_3 = dv_c_t1^3
	g dv_c_t1_4 = dv_c_t1^4
	
	g i_dv_c_t1 = dv_c_t1 * dwin
	g i_dv_c_t1_2 = dv_c_t1_2 * dwin
	g i_dv_c_t1_3 = dv_c_t1_3 * dwin
	g i_dv_c_t1_4 = dv_c_t1_4 * dwin
	
	g margin = abs(dv_c_t1)

	
g bandwidth = .25

/*Overall incumbency effect*/
regress dv_c_t2 dwin dv_c_t1 dv_c_t1_2 dv_c_t1_3 dv_c_t1_4 i_dv_c_t1 i_dv_c_t1_2 i_dv_c_t1_3 i_dv_c_t1_4 if margin < bandwidth, robust

/*Incumbency only in midterm years*/
regress dv_c_t2 dwin dv_c_t1 dv_c_t1_2 dv_c_t1_3 dv_c_t1_4 i_dv_c_t1 i_dv_c_t1_2 i_dv_c_t1_3 i_dv_c_t1_4 if margin < bandwidth & t2_is_midterm == 1, robust


/*Randomization Check*/
gen south = 0
gen dwint2 = 0
gen t2_frinc = max(t2_repfrinc,t2_demfrinc)
gen t2_margin = abs(dv_c_t2)
replace dwint2 = 1 if dv_c_t2 > 0
replace south = 1 if statesabbrev == "VA"
replace south = 1 if statesabbrev == "DC"
replace south = 1 if statesabbrev == "WV"
replace south = 1 if statesabbrev == "KY"
replace south = 1 if statesabbrev == "TN"
replace south = 1 if statesabbrev == "NC"
replace south = 1 if statesabbrev == "SC"
replace south = 1 if statesabbrev == "GA"
replace south = 1 if statesabbrev == "FL"
replace south = 1 if statesabbrev == "AL"
replace south = 1 if statesabbrev == "MS"
replace south = 1 if statesabbrev == "LA"
replace south = 1 if statesabbrev == "AR"
replace south = 1 if statesabbrev == "OK"
replace south = 1 if statesabbrev == "TX"

/*Becomes less and less random as you go further from the threshold*/
regress dwint2 dwin south if t2_is_midterm == 1 & t2_margin < .01
regress dwint2 dwin south if t2_is_midterm == 1 & t2_margin < .015
regress dwint2 dwin south if t2_is_midterm == 1 & t2_margin < .02


/*Midterm years no longer needed, drop; Also, the 2008 observations have no presidential info*/
drop if t2_is_midterm == 1
drop if t2_year == 2008


/*Display how many observations are left from each year*/
tabulate t2_year


/*RD incumbency for presidential years*/
regress dv_c_t2 dwin dv_c_t1 dv_c_t1_2 dv_c_t1_3 dv_c_t1_4 i_dv_c_t1 i_dv_c_t1_2 i_dv_c_t1_3 i_dv_c_t1_4 if margin < bandwidth, robust
	predict incumbency_yhat1 if dv_c_t1<0
	predict incumbency_yhat2 if dv_c_t1>=0
	predict incumbency_stderror1 if dv_c_t1<0, stdp
	predict incumbency_stderror2 if dv_c_t1>=0, stdp

/*RD reverse coattails*/
regress dv_p_t2 dwin dv_c_t1 dv_c_t1_2 dv_c_t1_3 dv_c_t1_4 i_dv_c_t1 i_dv_c_t1_2 i_dv_c_t1_3 i_dv_c_t1_4 if margin < bandwidth, robust
	predict rvsctls_yhat1 if dv_c_t1<0
	predict rvsctls_yhat2 if dv_c_t1>=0
	predict rvsctls_stderror1 if dv_c_t1<0, stdp
	predict rvsctls_stderror2 if dv_c_t1>=0, stdp
	

/*Try different bandwidths.*/
regress dv_p_t2 dwin dv_c_t1 dv_c_t1_2 dv_c_t1_3 dv_c_t1_4 i_dv_c_t1 i_dv_c_t1_2 i_dv_c_t1_3 i_dv_c_t1_4 if margin < .5, robust
regress dv_p_t2 dwin dv_c_t1 dv_c_t1_2 dv_c_t1_3 dv_c_t1_4 i_dv_c_t1 i_dv_c_t1_2 i_dv_c_t1_3 i_dv_c_t1_4 if margin < .25, robust
regress dv_p_t2 dwin dv_c_t1 dv_c_t1_2 dv_c_t1_3 dv_c_t1_4 i_dv_c_t1 i_dv_c_t1_2 i_dv_c_t1_3 i_dv_c_t1_4 if margin < .2, robust
regress dv_p_t2 dwin dv_c_t1 dv_c_t1_2 dv_c_t1_3 dv_c_t1_4 i_dv_c_t1 i_dv_c_t1_2 i_dv_c_t1_3 i_dv_c_t1_4 if margin < .15, robust
regress dv_p_t2 dwin dv_c_t1 dv_c_t1_2 dv_c_t1_3 dv_c_t1_4 i_dv_c_t1 i_dv_c_t1_2 i_dv_c_t1_3 i_dv_c_t1_4 if margin < .1, robust
regress dv_p_t2 dwin dv_c_t1 dv_c_t1_2 dv_c_t1_3 dv_c_t1_4 i_dv_c_t1 i_dv_c_t1_2 i_dv_c_t1_3 i_dv_c_t1_4 if margin < .05, robust

set matsize 800
/*Try different bandwidths - state FE.*/
xi: regress dv_p_t2 dwin dv_c_t1 dv_c_t1_2 dv_c_t1_3 dv_c_t1_4 i_dv_c_t1 i_dv_c_t1_2 i_dv_c_t1_3 i_dv_c_t1_4 i.statesabbrev if margin < .5, robust
xi: regress dv_p_t2 dwin dv_c_t1 dv_c_t1_2 dv_c_t1_3 dv_c_t1_4 i_dv_c_t1 i_dv_c_t1_2 i_dv_c_t1_3 i_dv_c_t1_4 i.statesabbrev if margin < .25, robust
xi: regress dv_p_t2 dwin dv_c_t1 dv_c_t1_2 dv_c_t1_3 dv_c_t1_4 i_dv_c_t1 i_dv_c_t1_2 i_dv_c_t1_3 i_dv_c_t1_4 i.statesabbrev if margin < .2, robust
xi: regress dv_p_t2 dwin dv_c_t1 dv_c_t1_2 dv_c_t1_3 dv_c_t1_4 i_dv_c_t1 i_dv_c_t1_2 i_dv_c_t1_3 i_dv_c_t1_4 i.statesabbrev if margin < .15, robust
xi: regress dv_p_t2 dwin dv_c_t1 dv_c_t1_2 dv_c_t1_3 dv_c_t1_4 i_dv_c_t1 i_dv_c_t1_2 i_dv_c_t1_3 i_dv_c_t1_4 i.statesabbrev if margin < .1, robust
xi: regress dv_p_t2 dwin dv_c_t1 dv_c_t1_2 dv_c_t1_3 dv_c_t1_4 i_dv_c_t1 i_dv_c_t1_2 i_dv_c_t1_3 i_dv_c_t1_4 i.statesabbrev if margin < .05, robust

/*Try different bandwidths - statexyear FE.*/
xi: regress dv_p_t2 dwin dv_c_t1 dv_c_t1_2 dv_c_t1_3 dv_c_t1_4 i_dv_c_t1 i_dv_c_t1_2 i_dv_c_t1_3 i_dv_c_t1_4 i.statesabbrev*i.t2_year if margin < .5, robust
xi: regress dv_p_t2 dwin dv_c_t1 dv_c_t1_2 dv_c_t1_3 dv_c_t1_4 i_dv_c_t1 i_dv_c_t1_2 i_dv_c_t1_3 i_dv_c_t1_4 i.statesabbrev*i.t2_year if margin < .25, robust
xi: regress dv_p_t2 dwin dv_c_t1 dv_c_t1_2 dv_c_t1_3 dv_c_t1_4 i_dv_c_t1 i_dv_c_t1_2 i_dv_c_t1_3 i_dv_c_t1_4 i.statesabbrev*i.t2_year if margin < .2, robust
xi: regress dv_p_t2 dwin dv_c_t1 dv_c_t1_2 dv_c_t1_3 dv_c_t1_4 i_dv_c_t1 i_dv_c_t1_2 i_dv_c_t1_3 i_dv_c_t1_4 i.statesabbrev*i.t2_year if margin < .15, robust
xi: regress dv_p_t2 dwin dv_c_t1 dv_c_t1_2 dv_c_t1_3 dv_c_t1_4 i_dv_c_t1 i_dv_c_t1_2 i_dv_c_t1_3 i_dv_c_t1_4 i.statesabbrev*i.t2_year if margin < .1, robust
xi: regress dv_p_t2 dwin dv_c_t1 dv_c_t1_2 dv_c_t1_3 dv_c_t1_4 i_dv_c_t1 i_dv_c_t1_2 i_dv_c_t1_3 i_dv_c_t1_4 i.statesabbrev*i.t2_year if margin < .05, robust


/*Democratic Spending Advantage stats I discuss briefly in a footnote*/
/* ICF = inflation conversion factors */
replace t2_dexp = t2_dexp / t2_icf
replace t2_rexp = t2_rexp / t2_icf
g t2_dspendadvantage = t2_dexp - t2_rexp
regress t2_dspendadvantage dwin dv_c_t1 dv_c_t1_2 dv_c_t1_3 dv_c_t1_4 i_dv_c_t1 i_dv_c_t1_2 i_dv_c_t1_3 i_dv_c_t1_4 if margin < bandwidth & t2_missingrexp==0 & t2_missingdexp==0, robust


/*Variables I used to create the graphs.*/
	g rvsctls_ciupper1 = rvsctls_yhat1 + 1.96*rvsctls_stderror1
	g rvsctls_cilower1 = rvsctls_yhat1 - 1.96*rvsctls_stderror1
	g rvsctls_ciupper2 = rvsctls_yhat2 + 1.96*rvsctls_stderror2
	g rvsctls_cilower2 = rvsctls_yhat2 - 1.96*rvsctls_stderror2
	
	g incumbency_ciupper1 = incumbency_yhat1 + 1.96*incumbency_stderror1
	g incumbency_cilower1 = incumbency_yhat1 - 1.96*incumbency_stderror1
	g incumbency_ciupper2 = incumbency_yhat2 + 1.96*incumbency_stderror2
	g incumbency_cilower2 = incumbency_yhat2 - 1.96*incumbency_stderror2
	

gen bin=int(200*(dv_c_t1+.005))/200
sort bin
by bin: egen rvsctls_binavg=mean(dv_p_t2)
by bin: egen rvs_num=count(dv_p_t2)

by bin: egen incumbency_binavg=mean(dv_c_t2)
by bin: egen inc_num=count(dv_p_t2)

by bin: egen rexp_binavg=mean(t2_rexp) if t2_missingrexp==0
by bin: egen rexp_num=count(t2_rexp) if t2_missingrexp==0

by bin: egen dexp_binavg=mean(t2_dexp) if t2_missingdexp==0
by bin: egen dexp_num=count(t2_dexp) if t2_missingdexp==0

by bin: egen dadv_binavg=mean(t2_dspendadvantage) if t2_missingrexp==0 & t2_missingdexp==0
by bin: egen dadv_num=count(t2_dspendadvantage) if t2_missingrexp==0 & t2_missingdexp==0

gen binx= bin - .0025
