* replication_Broockman 2014_chae *
* 2020/02/24 *

* Table 1
ta next_cycle_year State
egen stateyear=group(next_cycle_year State)
xtset stateyear

reg femaleonballotnextyear i.womanwon##c.femalecand_margin_of_victory i.womanwon##c.fem_v_c_p_2 i.womanwon##c.fem_v_c_p_3 i.womanwon##c.fem_v_c_p_4 if abs(femalecand_margin_of_victory)<0.15
xtreg femaleonballotnextyear i.womanwon##c.femalecand_margin_of_victory i.womanwon##c.fem_v_c_p_2 i.womanwon##c.fem_v_c_p_3 i.womanwon##c.fem_v_c_p_4 if abs(femalecand_margin_of_victory)<0.15, fe 

reg femaleturnout_nextcycle i.womanwon##c.femalecand_margin_of_victory i.womanwon##c.fem_v_c_p_2 i.womanwon##c.fem_v_c_p_3 i.womanwon##c.fem_v_c_p_4 if abs(femalecand_margin_of_victory)<0.15
xtreg femaleturnout_nextcycle i.womanwon##c.femalecand_margin_of_victory i.womanwon##c.fem_v_c_p_2 i.womanwon##c.fem_v_c_p_3 i.womanwon##c.fem_v_c_p_4 if abs(femalecand_margin_of_victory)<0.15, fe

reg female_percentageofelectorate_ne i.womanwon##c.femalecand_margin_of_victory i.womanwon##c.fem_v_c_p_2 i.womanwon##c.fem_v_c_p_3 i.womanwon##c.fem_v_c_p_4 if abs(femalecand_margin_of_victory)<0.15
xtreg female_percentageofelectorate_ne i.womanwon##c.femalecand_margin_of_victory i.womanwon##c.fem_v_c_p_2 i.womanwon##c.fem_v_c_p_3 i.womanwon##c.fem_v_c_p_4 if abs(femalecand_margin_of_victory)<0.15, fe


* Fig. 1

reg femaleonballotnextyear i.womanwon##c.femalecand_margin_of_victory i.womanwon##c.fem_v_c_p_2 i.womanwon##c.fem_v_c_p_3 i.womanwon##c.fem_v_c_p_4 if abs(femalecand_margin_of_victory)<0.15

/*Predict*/
	predict yhat1 if femalecand_margin_of_victory<0
	predict yhat2 if femalecand_margin_of_victory>0
	predict stde1 if femalecand_margin_of_victory<0, stdp
	predict stde2 if femalecand_margin_of_victory>0, stdp

/*Use the predicted values and the standard errors to generate 95% confidence intervals*/
	g ciupper1 = yhat1 + 1.96*stde1
	g cilower1 = yhat1 - 1.96*stde1
	g ciupper2 = yhat2 + 1.96*stde2
	g cilower2 = yhat2 - 1.96*stde2

g numbins1 = 250
gen bin1=round(numbins1*(femalecand_margin_of_victory+(0.5/numbins1)))/numbins1

sort bin1
by bin1: egen binavg1=mean(femaleonballotnextyear)
by bin1: egen number1=count(femaleonballotnextyear)
gen binx1= bin1 - (0.5/numbins1)

sort femalecand_margin_of_victory

sort bin1
by bin1: gen n1 = _n

twoway (scatter binavg binx[fweight = number] if absolute_margin < 0.15 & n1==1, mcolor(black) msize(*.26) msymbol(circle_hollow) mlwidth(vthin))
(line yhat1 femalecand_margin_of_victory if absolute_margin < 0.15, clcolor(black) lwidth(vthin))
(line ciupper1 femalecand_margin_of_victory if absolute_margin < 0.15, clcolor(black) clpat(dash) lwidth(vthin))
(line cilower1 femalecand_margin_of_victory if absolute_margin < 0.15, clcolor(black) clpat(dash) lwidth(vthin))
(line yhat2 femalecand_margin_of_victory if absolute_margin < 0.15, clcolor(black) lwidth(vthin))
(line ciupper2 femalecand_margin_of_victory if absolute_margin < 0.15, clcolor(black) clpat(dash) lwidth(vthin))
(line cilower2 femalecand_margin_of_victory if absolute_margin < 0.15, clcolor(black) clpat(dash) lwidth(vthin))
(scatteri .3 0 .5 0, recast(line) lcolor(black) lpattern(vshortdash))
, 
xline
ytitle(Woman on Ballot next year In Same District - Election 2)
xtitle(Female Candidates' Margin of Victory - Election 1) 
ylabel(.3(.05).5)
ytick(.3(.05).5)
xtick(-.15(.05).15)
xlabel(-.15(.05).15)
legend(off)
plotregion(style(none)) plotregion(fcolor(white)) graphregion(fcolor(white))

graph export "g1.png", replace
	

* Fig. 2
	
reg femaleturnout_nextcycle i.womanwon##c.femalecand_margin_of_victory i.womanwon##c.fem_v_c_p_2 i.womanwon##c.fem_v_c_p_3 i.womanwon##c.fem_v_c_p_4 if abs(femalecand_margin_of_victory)<0.15

/*Predict*/
	predict yhat3 if femalecand_margin_of_victory<0
	predict yhat4 if femalecand_margin_of_victory>0
	predict stde3 if femalecand_margin_of_victory<0, stdp
	predict stde4 if femalecand_margin_of_victory>0, stdp

/*Use the predicted values and the standard errors to generate 95% confidence intervals*/
	g ciupper3 = yhat3 + 1.96*stde3
	g cilower3 = yhat3 - 1.96*stde3
	g ciupper4 = yhat4 + 1.96*stde4
	g cilower4 = yhat4 - 1.96*stde4

g numbins2 = 250
gen bin2=round(numbins2*(femalecand_margin_of_victory+(0.5/numbins2)))/numbins2

sort bin2
by bin2: egen binavg2=mean(femaleturnout_nextcycle)
by bin2: egen number2=count(femaleturnout_nextcycle)
gen binx2= bin2 - (0.5/numbins2)

sort femalecand_margin_of_victory

sort bin2
by bin2: gen n2 = _n
	
twoway 
(scatter binavg2 binx2[fweight = number] if absolute_margin < 0.15 & n2==1, mcolor(black) msize(*.26) msymbol(circle_hollow) mlwidth(vthin))
(line yhat3 femalecand_margin_of_victory if absolute_margin < 0.15, clcolor(black) lwidth(vthin))
(line ciupper3 femalecand_margin_of_victory if absolute_margin < 0.15, clcolor(black) clpat(dash) lwidth(vthin))
(line cilower3 femalecand_margin_of_victory if absolute_margin < 0.15, clcolor(black) clpat(dash) lwidth(vthin))
(line yhat4 femalecand_margin_of_victory if absolute_margin < 0.15, clcolor(black) lwidth(vthin))
(line ciupper4 femalecand_margin_of_victory if absolute_margin < 0.15, clcolor(black) clpat(dash) lwidth(vthin))
(line cilower4 femalecand_margin_of_victory if absolute_margin < 0.15, clcolor(black) clpat(dash) lwidth(vthin))
(scatteri .3 0 .5 0, recast(line) lcolor(black) lpattern(vshortdash))
, 
xline
ytitle(Female Voter Turnout - Election 2) 
xtitle(Female Candidates' Margin of Victory - Election 1) 
ylabel(.3(.05).5) 
ytick(.3(.05).5) 
xtick(-.15(.05).15) 
xlabel(-.15(.05).15) 
legend(off) plotregion(style(none)) plotregion(fcolor(white)) graphregion(fcolor(white))

graph export "g2.png", replace
	
	
* Fig. 3
	
reg female_percentageofelectorate_ne i.womanwon##c.femalecand_margin_of_victory i.womanwon##c.fem_v_c_p_2 i.womanwon##c.fem_v_c_p_3 i.womanwon##c.fem_v_c_p_4 if abs(femalecand_margin_of_victory)<0.15

/*Predict*/
	predict yhat5 if femalecand_margin_of_victory<0
	predict yhat6 if femalecand_margin_of_victory>0
	predict stde5 if femalecand_margin_of_victory<0, stdp
	predict stde6 if femalecand_margin_of_victory>0, stdp

/*Use the predicted values and the standard errors to generate 95% confidence intervals*/
	g ciupper5 = yhat5 + 1.96*stde5
	g cilower5 = yhat5 - 1.96*stde5
	g ciupper6 = yhat6 + 1.96*stde6
	g cilower6 = yhat6 - 1.96*stde6

g numbins3 = 250
gen bin3=round(numbins3*(femalecand_margin_of_victory+(0.5/numbins3)))/numbins3

sort bin3
by bin3: egen binavg3=mean(female_percentageofelectorate_ne)
by bin3: egen number3=count(female_percentageofelectorate_ne)
gen binx3= bin3 - (0.5/numbins3)

sort femalecand_margin_of_victory

sort bin3
by bin3: gen n3 = _n
	
twoway 
(scatter binavg3 binx3[fweight = number] if absolute_margin < 0.15 & n3==1, mcolor(black) msize(*.26) msymbol(circle_hollow) mlwidth(vthin))
(line yhat5 femalecand_margin_of_victory if absolute_margin < 0.15, clcolor(black) lwidth(vthin))
(line ciupper5 femalecand_margin_of_victory if absolute_margin < 0.15, clcolor(black) clpat(dash) lwidth(vthin))
(line cilower5 femalecand_margin_of_victory if absolute_margin < 0.15, clcolor(black) clpat(dash) lwidth(vthin))
(line yhat6 femalecand_margin_of_victory if absolute_margin < 0.15, clcolor(black) lwidth(vthin))
(line ciupper6 femalecand_margin_of_victory if absolute_margin < 0.15, clcolor(black) clpat(dash) lwidth(vthin))
(line cilower6 femalecand_margin_of_victory if absolute_margin < 0.15, clcolor(black) clpat(dash) lwidth(vthin))
(scatteri .3 0 .5 0, recast(line) lcolor(black) lpattern(vshortdash))
, 
xline
ytitle(Female Voter Turnout - Election 2) 
xtitle(Female Candidates' Margin of Victory - Election 1) 
ylabel(.3(.05).5) 
ytick(.3(.05).5) 
xtick(-.15(.05).15) 
xlabel(-.15(.05).15) 
legend(off) plotregion(style(none)) plotregion(fcolor(white)) graphregion(fcolor(white))

graph export "g3.png", replace
		


* Table 2

reg femcands_over_contests_next_75m i.womanwon##c.femalecand_margin_of_victory i.womanwon##c.fem_v_c_p_2 i.womanwon##c.fem_v_c_p_3 i.womanwon##c.fem_v_c_p_4 if abs(femalecand_margin_of_victory)<0.15
reg femcands_over_contests_next_10r i.womanwon##c.femalecand_margin_of_victory i.womanwon##c.fem_v_c_p_2 i.womanwon##c.fem_v_c_p_3 i.womanwon##c.fem_v_c_p_4 if abs(femalecand_margin_of_victory)<0.15

reg femwins_over_contests_next_75m i.womanwon##c.femalecand_margin_of_victory i.womanwon##c.fem_v_c_p_2 i.womanwon##c.fem_v_c_p_3 i.womanwon##c.fem_v_c_p_4 if abs(femalecand_margin_of_victory)<0.15
reg femwins_over_contests_next_10r i.womanwon##c.femalecand_margin_of_victory i.womanwon##c.fem_v_c_p_2 i.womanwon##c.fem_v_c_p_3 i.womanwon##c.fem_v_c_p_4 if abs(femalecand_margin_of_victory)<0.15

	
* Fig. 4
	
reg femcands_over_contests_next_75m i.womanwon##c.femalecand_margin_of_victory i.womanwon##c.fem_v_c_p_2 i.womanwon##c.fem_v_c_p_3 i.womanwon##c.fem_v_c_p_4 if abs(femalecand_margin_of_victory)<0.15

/*Predict*/
	predict yhat7 if femalecand_margin_of_victory<0
	predict yhat8 if femalecand_margin_of_victory>0
	predict stde7 if femalecand_margin_of_victory<0, stdp
	predict stde8 if femalecand_margin_of_victory>0, stdp

/*Use the predicted values and the standard errors to generate 95% confidence intervals*/
	g ciupper7 = yhat7 + 1.96*stde7
	g cilower7 = yhat7 - 1.96*stde7
	g ciupper8 = yhat8 + 1.96*stde8
	g cilower8 = yhat8 - 1.96*stde8

g numbins4 = 250
gen bin4=round(numbins4*(femalecand_margin_of_victory+(0.5/numbins4)))/numbins4

sort bin4
by bin4: egen binavg4=mean(femcands_over_contests_next_75m)
by bin4: egen number4=count(femcands_over_contests_next_75m)
gen binx4= bin4 - (0.5/numbins4)

sort femalecand_margin_of_victory

sort bin4
by bin4: gen n4 = _n
	
twoway 
(scatter binavg4 binx4[fweight = number] if absolute_margin < 0.15 & n4==1, mcolor(black) msize(*.26) msymbol(circle_hollow) mlwidth(vthin))
(line yhat7 femalecand_margin_of_victory if absolute_margin < 0.15, clcolor(black) lwidth(vthin))
(line ciupper7 femalecand_margin_of_victory if absolute_margin < 0.15, clcolor(black) clpat(dash) lwidth(vthin))
(line cilower7 femalecand_margin_of_victory if absolute_margin < 0.15, clcolor(black) clpat(dash) lwidth(vthin))
(line yhat8 femalecand_margin_of_victory if absolute_margin < 0.15, clcolor(black) lwidth(vthin))
(line ciupper8 femalecand_margin_of_victory if absolute_margin < 0.15, clcolor(black) clpat(dash) lwidth(vthin))
(line cilower8 femalecand_margin_of_victory if absolute_margin < 0.15, clcolor(black) clpat(dash) lwidth(vthin))
(scatteri .3 0 .5 0, recast(line) lcolor(black) lpattern(vshortdash))
, 
xline
ytitle(Female Candidates per Contest District within 75miles - Election 2) 
xtitle(Female Candidates' Margin of Victory - Election 1) 
ylabel(.3(.05).5) 
ytick(.3(.05).5) 
xtick(-.15(.05).15) 
xlabel(-.15(.05).15) 
legend(off) plotregion(style(none)) plotregion(fcolor(white)) graphregion(fcolor(white))

graph export "g4.png", replace
		

