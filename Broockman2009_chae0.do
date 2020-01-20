*** 과정학회 replication assignment 1st (Chaeyeong) ***
*** David E. Broockman, "Do Congressional Candidates Have Reverse Coattails? Evidence from a Regression Discontinuity Design ***

use "/Users/chaeyeong/Downloads/Broockman_PA_2009.dta"

/* replication 과정 개괄
논문만 보고 레플리케이션을 해보는 과정이 처음이라 대강의 과정을 기술해보고자 함. 
1. 다운받은 replication data 파일을 stata에서 연 다음, data browser를 열어 데이터 생김새와 변수명을 확인함
2. 논문에 적힌 3. Method, Data and Results 파트의 서술의 순서에 따라 진행함
	1) 424p_변수 몇 개를 버림 
	2) 식에 따라 변수의 reg 식을 만듦 (cheating..) 
	*/
	
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

gen dummy = 0
replace dummy = 1 if dv_c_t1 >= 0

gen vi_2 = dv_c_t1^2
gen vi_3 = dv_c_t1^3
gen vi_4 = dv_c_t1^4

gen vidi = dv_c_t1 * dummy
gen vidi_2 = vi_2 * dummy
gen vidi_3 = vi_3 * dummy
gen vidi_4 = vi_4 * dummy

g margin = abs(dv_c_t1)
g bandwidth = .25

reg dv_p_t2 dummy dv_c_t1 vi_2 vi_3 vi_4 vidi vidi_2 vidi_3 vidi_4 if margin < bandwidth, robust
outreg2 using replication.doc, alpha(0.01, 0.05, 0.1) addstat("Adj. R-squared", e(r2_a)) ctitle("Model 1") replace

** incumbency advantage model : (2)
*** DV만 바꾸면 됨.
reg dv_c_t2 dummy dv_c_t1 vi_2 vi_3 vi_4 vidi vidi_2 vidi_3 vidi_4 if t2_is_midterm==1&margin < bandwidth, robust
outreg2 using replication.doc, alpha(0.01, 0.05, 0.1) addstat("Adj. R-squared", e(r2_a)) ctitle("Model 2") append

/* 결과 : 주룩주룩
?? state-year fixed effect? (주석 8) 









