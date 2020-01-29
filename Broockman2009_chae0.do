*** 과정학회 replication assignment 1st (Chaeyeong) ***
*** David E. Broockman, "Do Congressional Candidates Have Reverse Coattails? Evidence from a Regression Discontinuity Design ***

use "/Users/chaeyeong/Downloads/Broockman_PA_2009.dta"

drop if t2_3rdpartyinc==1
drop if t2_incumbent_has_switched_prty==1
drop if t1_atlargeormulticandidate==1
drop if t2_specialelectiontoeelect==1
drop if t2_redist==1

*** table 1 ***
ta t2_year if dv_p_t2!=0

** RDD; reverse coattail effect model : (1)
*** Pi = a + b1Vi + b2Vi^2 + b3Vi^3 + b4Vi^4 + rdi + b5ViDi + b6Vi^2Di + b7Vi^3Di + b8Vi^4Di + errorterm

*** Pi = Democratic Presidential Vote Share Time 2 = dv_p_t2
*** Vi = Demoratic Congressional Vote Share Time 1 = dv_c_t1
*** Di = a dummy variable set to 1 if Vi>=0.5 / 아니면 0

gen victory = 0
replace victory = 1 if dv_c_t1 >= 0

gen vi_2 = dv_c_t1^2
gen vi_3 = dv_c_t1^3
gen vi_4 = dv_c_t1^4

gen vidi = dv_c_t1 * victory
gen vidi_2 = vi_2 * victory
gen vidi_3 = vi_3 * victory
gen vidi_4 = vi_4 * victory

reg dv_c_t2 i.victory##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1 if abs(dv_c_t1)<0.25, vce(robust)
reg dv_c_t2 i.victory##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1 if abs(dv_c_t1)<0.25&t2_is_midterm==0, vce(robust)
areg dv_c_t2 i.victory##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1 if abs(dv_c_t1)<0.25 & t2_is_midterm==0, vce(robust) a(t2_year)
reg dv_p_t2 i.victory##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1 if abs(dv_c_t1)<0.25 & t2_is_midterm==0, vce(robust)
areg dv_p_t2 i.victory##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1 if abs(dv_c_t1)<0.25 & t2_is_midterm==0, vce(robust) a(t2_year)

**

areg dv_c_t2 i.victory##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1 if abs(dv_c_t1)<0.25 & t2_is_midterm==0, vce(robust) a(t2_year)
rdplot dv_c_t2 dv_c_t1 if abs(dv_c_t1)<0.15, c(0) nbins(30 30) p(4) graph_options(xlabel(-0.15(0.05)0.15) legend(off) xtitle("DVCT1") ytitle("DCVT2")) ci(95) shade genvars p(2)

areg dv_p_t2 i.victory##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1 if abs(dv_c_t1)<0.25 & t2_is_midterm==0, vce(robust) a(t2_year)
rdplot dv_c_t2 dv_c_t1 if abs(dv_c_t1)<0.15, c(0) nbins(30 30) p(4) graph_options(xlabel(-0.15(0.05)0.15) legend(off) xtitle("DVCT1") ytitle("DPVT2")) ci(95) shade genvars p(2)

** variable rdplot_id already defined
r(110); **







