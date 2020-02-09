clear
set more off
cd "~/Documents/Docs/research/Completed/2013 Female RD/replication data"

use "main data.dta"

*Indicator for midterm elections
gen midterm = next_cycle_year == 2002 | next_cycle_year == 2006 | next_cycle_year == 2010

*Generate StateXYear FEs
tostring next_cycle_year, gen(next_cycle_year_string)
gen stateyear = State + next_cycle_year_string
encode stateyear, gen(stateyear_fes)
xtset stateyear_fes

log using female-rd-log.smcl, replace
*Non-identified observational results to demonstrate bias in traditional approaches
local dvs femcands_over_contests_next_10r femcands_over_contests_next_50m femaleturnout_nextcycle female_percentageofelectorate_ne
foreach dv in `dvs'{
	reg `dv' womanwon
	reg `dv' femalecand_margin_of_victory
}

*Woman on ballot next year
local bws 0.05 0.1 0.15 0.2 0.25
foreach bandwidth in `bws'{
	reg femaleonballotnextyear womanwon - winXfem_v_c_p_4 if absolute_margin < `bandwidth', robust
}

*Voter turnout RD results
local dvs femaleturnout_nextcycle female_percentageofelectorate_ne
local bws 0.05 0.1 0.15 0.2 0.25
foreach bandwidth in `bws'{
	foreach dv in `dvs'{
		reg `dv' womanwon - winXfem_v_c_p_4 if absolute_margin < `bandwidth', robust	
		*reg `dv' womanwon - winXfem_v_c_p_4 midterm if absolute_margin < `bandwidth', robust
		*xtreg `dv' womanwon - winXfem_v_c_p_4 if absolute_margin < `bandwidth', fe
	}
}

*Female candidacies nearby RD results
#delimit;
local dvs
femcands_over_contests_next5r femwins_over_contests_next5r
femcands_over_contests_next_10r femwins_over_contests_next_10r
femcands_over_contests_next_15r femwins_over_contests_next_15r
femcands_over_contests_next_50m femwins_over_contests_next_50m
femcands_over_contests_next_75m femwins_over_contests_next_75m
femcands_over_contests_next_100m femwins_over_contests_next_100m;
#delimit cr
foreach bandwidth in `bws'{
	foreach dv in `dvs'{
		reg `dv' womanwon - winXfem_v_c_p_4 if absolute_margin < `bandwidth', robust
		*xtreg femcands_over_contests_next_75m womanwon - winXfem_v_c_p_4 if absolute_margin < `bandwidth' & exclude_cand_analysis == 0, fe
	}
}

*Not different in senate only, open seat, challenger, etc.
gen open = Seat == "O"
gen challenger = Seat == "C"
gen senate = Chamber == "S"
reg femcands_over_contests_next_75m womanwon - winXfem_v_c_p_4 if open & absolute_margin < .15, robust
reg femcands_over_contests_next_75m womanwon - winXfem_v_c_p_4 if challenger & absolute_margin < .15, robust
reg femcands_over_contests_next_75m womanwon - winXfem_v_c_p_4 if senate & absolute_margin < .15, robust

*Placebo Tests - effect on turnout in the very same year
local dvs female_percentageofelectorate_th femaleturnout_thiscycle femcands_over_contests_this_10r femwins_over_contests_this_10r femwins_over_femcands_this_10r femcands_over_contests_this_75m femwins_over_contests_this_75m femwins_over_femcands_this_75m
foreach bandwidth in `bws'{
	foreach dv in `dvs'{
		reg `dv' womanwon - winXfem_v_c_p_4 if absolute_margin < `bandwidth', robust
	}
}

log close
