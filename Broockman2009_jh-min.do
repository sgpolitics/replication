cd "C:\Users\J\Dropbox\대학원\과학으로서의정치학회\Replication2019FW"
use Broockman2009, clear

*** 전처리: Table 1

	drop if t2_year==2008 // 425, Table 1, Time 2 year: 1952-2006
	tab t2_year if t2_is_midterm==0
	* 424: discard anomalous districts
		drop if t2_3rdpartyinc==1 // Time 1에서 제3당이 승리한 경우 제외
		drop if t2_incumbent_has_switched_prty==1 // Time 1과 Time 2 사이에 현직 하원의원의 소속 정당이 바뀐 경우 제외
		drop if t1_atlargeormulticandidate==1 // 주 전체(statewide) 선거구에서 여러 명의 하원의원을 선출한 경우 제외
		drop if t2_specialelectiontoeelect==1 // Time 1과 Time 2 사이에 특별한 선거가 일어나 Time 1의 DCVS를 무의미하게 만든 경우 제외
		drop if t2_redist==1 // Time 1과 Time 2 사이에 하원 선거구 재획정이 일어난 경우 제외
		tab t2_year if t2_is_midterm==0

	gen victory=(dv_c_t1>=0) // dv_c_t1 is margin not vote share
	egen stateyear=group(statesabbrev t2_year)
	xtset stateyear // 분석 단위가 선거구이기 때문에 statesabbrev, t2_year를 각각 panelvar, timevar로 지정하게 되면 조합이 unique하지 않아서 오류: repeated time values within panel

*** 분석: Table 2 (xtreg는 standard error 계산 방식 상이)

	/*
		425: only use observations where DCVS Time 1 is within 25 percentage points of the threshold
		427, Table 2, Note: all regressions use robust SEs
	*/

	* model (1): .1010** (.0185)
		reg dv_c_t2 i.victory##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1 if abs(dv_c_t1)<0.25, vce(robust)

	* model (2): .1079** (.0154)
		reg dv_c_t2 i.victory##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1 if abs(dv_c_t1)<0.25 & t2_is_midterm==0, vce(robust)

	* model (3): .0947** (.0156)
		xtreg dv_c_t2 i.victory##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1 if abs(dv_c_t1)<0.25 & t2_is_midterm==0, vce(robust) fe
		areg dv_c_t2 i.victory##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1 if abs(dv_c_t1)<0.25 & t2_is_midterm==0, vce(robust) a(stateyear)

	* model (4): .0056 (.0144)
		reg dv_p_t2 i.victory##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1 if abs(dv_c_t1)<0.25 & t2_is_midterm==0, vce(robust)

	* model (5): -.0043 (.0099)
		xtreg dv_p_t2 i.victory##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1 if abs(dv_c_t1)<0.25 & t2_is_midterm==0, vce(robust) fe
		areg dv_p_t2 i.victory##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1 if abs(dv_c_t1)<0.25 & t2_is_midterm==0, vce(robust) a(stateyear)

*** 그래프

	* generate variables for bin scatter plots
		egen bin=cut(dv_c_t1) if t2_is_midterm==0, at(-0.5(0.005)0.5) // 독립변수를 0.5 percentage point wide 구간으로 쪼갬
		egen dv_c_t1_bin=mean(dv_c_t1), by(bin)
		egen dv_c_t2_bin=mean(dv_c_t2), by(bin)
		egen dv_p_t2_bin=mean(dv_p_t2), by(bin)

	* generate variables for fitted lines and confidence intervals
		areg dv_c_t2 i.victory##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1 if abs(dv_c_t1)<0.25 & t2_is_midterm==0, vce(robust) a(stateyear) // model (3)
		predict yhat_c, xb // 독립변수값별 종속변수값 계산
		predict sehat_c, stdp // 독립변수값별 종속변수의 standard error값 계산
		gen yhatu_c=yhat_c+2*sehat_c // 신뢰구간 상한 계산
		gen yhatl_c=yhat_c-2*sehat_c // 신뢰구간 하한 계산
		areg dv_p_t2 i.victory##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1 if abs(dv_c_t1)<0.25 & t2_is_midterm==0, vce(robust) a(stateyear) // model(5)
		predict yhat_p, xb // 독립변수값별 종속변수값 계산
		predict sehat_p, stdp // 독립변수값별 종속변수의 standard error값 계산
		gen yhatu_p=yhat_p+2*sehat_p // 신뢰구간 상한 계산
		gen yhatl_p=yhat_p-2*sehat_p // 신뢰구간 하한 계산

	* graph settings	
		set scheme plotplainblind // 그래프 스타일 지정
		graph set window fontface "Sandoll 고딕 TTF 03 Bold" // 그래프 범례 및 축 표시에 사용될 폰트를 지정

	* Fig. 1
		twoway (scatter dv_c_t2_bin dv_c_t1_bin if abs(dv_c_t1)<0.15, /// 산점도 출력
			ylabel(-0.2(0.05)0.2) xlabel(-0.15(0.05)0.15, nogrid) /// x,y축 구간 지정
			xline(0, lp(solid) lc(gs10)) /// x=0에 직선 출력
			mcolor(gs4%50)) ///
			(line yhat_c dv_c_t1 if abs(dv_c_t1)<0.15, sort /// 회귀선 출력
			ylabel(-0.2(0.05)0.2) xlabel(-0.15(0.05)0.15, nogrid) /// x,y축 구간 지정
			lp(solid) lc(gs4%50) legend(off)) ///
			(line yhatu_c dv_c_t1 if abs(dv_c_t1)<0.15, sort /// 신뢰구간 상한 출력
			ylabel(-0.2(0.05)0.2) xlabel(-0.15(0.05)0.15, nogrid) /// x,y축 구간 지정
			lp(dash) lc(gs4%50) legend(off)) ///
			(line yhatl_c dv_c_t1 if abs(dv_c_t1)<0.15, sort /// 신뢰구간 하한 출력
			ylabel(-0.2(0.05)0.2) xlabel(-0.15(0.05)0.15, nogrid) /// x,y축 구간 지정
			lp(dash) lc(gs4%50) legend(off) ///
			xtitle("Democratic Congressional Margin of Victory Time 1") ytitle("Democratic Congressional Margin of Victory Time 2"))
		graph export "Broockman2009_Fig. 1.tif", as(tif) replace

	* Fig. 2
		twoway (scatter dv_p_t2_bin dv_c_t1_bin if abs(dv_c_t1)<0.15, /// 산점도 출력
			ylabel(-0.2(0.05)0.2) xlabel(-0.15(0.05)0.15, nogrid) /// x,y축 구간 지정
			xline(0, lp(solid) lc(gs10)) /// x=0에 직선 출력
			mcolor(gs4%50)) ///
			(line yhat_p dv_c_t1 if abs(dv_c_t1)<0.15, sort /// 회귀선 출력
			ylabel(-0.2(0.05)0.2) xlabel(-0.15(0.05)0.15, nogrid) /// x,y축 구간 지정
			lp(solid) lc(gs4%50) legend(off)) ///
			(line yhatu_p dv_c_t1 if abs(dv_c_t1)<0.15, sort /// 신뢰구간 상한 출력
			ylabel(-0.2(0.05)0.2) xlabel(-0.15(0.05)0.15, nogrid) /// x,y축 구간 지정
			lp(dash) lc(gs4%50) legend(off)) ///
			(line yhatl_p dv_c_t1 if abs(dv_c_t1)<0.15, sort /// 신뢰구간 하한 출력
			ylabel(-0.2(0.05)0.2) xlabel(-0.15(0.05)0.15, nogrid) /// x,y축 구간 지정
			lp(dash) lc(gs4%50) legend(off) ///
			xtitle("Democratic Congressional Margin of Victory Time 1") ytitle("Democratic Presidential Margin of Victory Time 2"))
		graph export "Broockman2009_Fig. 2.tif", as(tif) replace

*** Appendix: Table 1A (일부 값 반올림 수준에서 불일치)

	quietly {
		noisily di as text " DCVS, Time 2	 DPVS, Time 2" // 제목 행 출력
		foreach bw of numlist 0.50 0.25 0.196 0.10 0.05 {
		* loop over Bandwidth
			local bandwidth "abs(dv_c_t1)<`bw'" // 대역폭이 대칭인 경우의 if 조건식
			if `bw'==0.196 {
				local bandwidth "dv_c_t1<0.196 & dv_c_t1>-0.123" // 대역폭이 비대칭인 경우의 if 조건식
			}
			forval i=1/3 {
			* loop over Specifications
				local regtype "reg" // 고정효과를 고려하지 않은 회귀분석
				if `i'!=1 {
					local regtype "areg" // 고정효과를 고려하는 회귀분석
				}
				local fixed ""		
				if `i'==2 {
					local fixed "a(t2_year)" // Year fixed effects?: Y
				}
				else if `i'==3 {
					local fixed "a(stateyear)" // State × year fixed effects?: Y
				}
				foreach dep of varlist dv_c_t2 dv_p_t2 {
				* loop over Dependent variables
					`regtype' `dep' i.victory##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1##c.dv_c_t1 if `bandwidth' & t2_is_midterm==0, vce(robust) `fixed'
					local coef_`dep'=_b[1.victory] // 종속변수별로 계수값 저장
					local se_`dep'=_se[1.victory] // 종속변수별로 표준오차값 저장
					local space_`dep' "" // -부호 유무를 고려한 계수값 정렬
					if `coef_`dep''>=0 {
						local space_`dep' " "
					}
				}
				noisily di as result "`space_dv_c_t2'" %5.4f `coef_dv_c_t2' " (" %5.4f `se_dv_c_t2' ")	" "`space_dv_p_t2'" %5.4f `coef_dv_p_t2' " (" %5.4f `se_dv_p_t2' ")" // 모델별로 계수값과 표준오차값을 출력
			}
		}
	}
