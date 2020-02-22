cd "C:\Users\J\Dropbox\대학원\과학으로서의정치학회\Replication2019FW"
use Broockman2014, clear

*** 전처리: 2.2. Data

	rename * , low
	set scheme plotplainblind // 그래프 스타일 지정
	graph set window fontface "Sandoll 고딕 TTF 03 Bold" // 그래프 범례 및 축 표시에 사용될 폰트를 지정

	* 2.2.1 Female candidates
		tab year // 홀수 해 및 redistricting 일어난 2000/2010년 제외
		tab chamber if state=="AZ" // multi-member districts 제외; AZ, NJ, SD, WA는 모든 주 하원의원을 MMDs로 선출함: https://ballotpedia.org/State_legislative_chambers_that_use_multi-member_districts
		tab name year if seat=="I/U" & state=="CA" // I/U는 incumbent/unopposed로 추정됨: https://ballotpedia.org/Anna_Caballero
		tab femalecand_margin_of_victory if seat=="I/U" // 1572명 중 1492명은 unopposed인데, margin!=0.5인 사례도 존재함
		tab name year if seat=="I/U" & femalecand_margin_of_victory!=0.5

	* generate indicator variables for state-year fixed effects
		encode state , gen(st) // string을 numeric으로 변환
		gen styear=st*year
		egen styear_egen=group(state year)
		capture snapshot erase _all
		snapshot save // sort 적용 이전의 데이터 상태를 기록
		foreach x of varlist styear* {  // gen으로 생성한 변수와 egen으로 생성한 변수가 동일함을 확인
			by `x', sort: gen nvals_`x' = _n == 1
			count if nvals_`x'
		} // https://www.stata.com/support/faqs/data-management/number-of-distinct-observations/
		snapshot restore 1 // sort 적용 이전의 데이터 상태를 복원
		xtset styear

	* generate variables for bin scatter plots
		gen bin=int(200*(femalecand_margin_of_victory+.005))/200
		sort bin
		gen binx=bin-.0025

*** 분석: 3. Does electing women empower other women to vote? (산점도 재현 불가)

	local rdvarlist "i.womanwon##c.femalecand_margin_of_victory##c.femalecand_margin_of_victory##c.femalecand_margin_of_victory##c.femalecand_margin_of_victory" // 독립변수 목록
	local massdepvarlist "femaleonballotnextyear femaleturnout_nextcycle female_percentageofelectorate_ne" // Table 1의 종속변수 목록
	local bandwidth "abs(femalecand_margin_of_victory)<0.15"

	* Table 1, model (1), (3), (5)
		local i=1
		foreach dv of varlist `massdepvarlist' {
			reg `dv' `rdvarlist' if `bandwidth', robust
			predict yhat_m`i' if `dv'!=., xb // 독립변수값별 종속변수값 계산
			predict sehat_m`i' if `dv'!=., stdp // 독립변수값별 종속변수의 standard error값 계산
			gen yhatu_m`i'=yhat_m`i'+2*sehat_m`i' if `dv'!=. // 신뢰구간 상한 계산
			gen yhatl_m`i'=yhat_m`i'-2*sehat_m`i' if `dv'!=. // 신뢰구간 하한 계산
			by bin: egen bin_m`i'_mean=mean(`dv') if `dv'!=. // 각 구간에서 DV의 평균값을 계산
			by bin: egen bin_m`i'_n=count(`dv') if `dv'!=. // 각 구간에서 DV의 관측치 수를 계산
			local i=`i'+1
		}

	* Table 1, model (2), (4), (6): 계수값 불일치
		foreach dv of varlist `massdepvarlist' {
			areg `dv' `rdvarlist' if `bandwidth', robust a(styear)
		}

	* Fig. 3, panel (c)
		twoway (scatter bin_m1_mean binx if `bandwidth' [w=bin_m1_n], /// 산점도 출력
			ylabel(0.1(0.1)1) xlabel(-0.15(0.05)0.15, nogrid) /// x,y축 구간 지정
			xline(0, lp(solid) lc(gs10)) /// x=0에 직선 출력
			mcolor(gs4%50) msize(tiny)) ///
			(line yhat_m1 femalecand_margin_of_victory if `bandwidth', sort /// 회귀선 출력
			lp(solid) lc(gs4%50) legend(off)) ///
			(line yhatu_m1 femalecand_margin_of_victory if `bandwidth', sort /// 신뢰구간 상한 출력
			lp(dash) lc(gs4%50) legend(off)) ///
			(line yhatl_m1 femalecand_margin_of_victory if `bandwidth', sort /// 신뢰구간 하한 출력
			lp(dash) lc(gs4%50) legend(off) ///
			xtitle("Female Candidate’s Margin of Victory - Election 1") title("Woman on Ballot Next Year in Same District - Election 2"))

	* Fig. 3, panel (a)
		twoway (scatter bin_m2_mean binx if `bandwidth' [w=bin_m2_n], /// 산점도 출력
			ylabel(0.35(0.05)0.65) xlabel(-0.15(0.05)0.15, nogrid) /// x,y축 구간 지정
			xline(0, lp(solid) lc(gs10)) /// x=0에 직선 출력
			mcolor(gs4%50) msize(tiny)) ///
			(line yhat_m2 femalecand_margin_of_victory if `bandwidth', sort /// 회귀선 출력
			lp(solid) lc(gs4%50) legend(off)) ///
			(line yhatu_m2 femalecand_margin_of_victory if `bandwidth', sort /// 신뢰구간 상한 출력
			lp(dash) lc(gs4%50) legend(off)) ///
			(line yhatl_m2 femalecand_margin_of_victory if `bandwidth', sort /// 신뢰구간 하한 출력
			lp(dash) lc(gs4%50) legend(off) ///
			xtitle("Female Candidate’s Margin of Victory - Election 1") title("Female Voter Turnout - Election 2"))

	* Fig. 3, panel (b)
		twoway (scatter bin_m3_mean binx if `bandwidth' [w=bin_m3_n], /// 산점도 출력
			ylabel(0.5(0.01)0.55) xlabel(-0.15(0.05)0.15, nogrid) /// x,y축 구간 지정
			xline(0, lp(solid) lc(gs10)) /// x=0에 직선 출력
			mcolor(gs4%50) msize(tiny)) ///
			(line yhat_m3 femalecand_margin_of_victory if `bandwidth', sort /// 회귀선 출력
			lp(solid) lc(gs4%50) legend(off)) ///
			(line yhatu_m3 femalecand_margin_of_victory if `bandwidth', sort /// 신뢰구간 상한 출력
			lp(dash) lc(gs4%50) legend(off)) ///
			(line yhatl_m3 femalecand_margin_of_victory if `bandwidth', sort /// 신뢰구간 하한 출력
			lp(dash) lc(gs4%50) legend(off) ///
			xtitle("Female Candidate’s Margin of Victory - Election 1") title("Female Percentage of Electorate - Election 2"))
	
*** 분석: 4. Do female politicians break glass ceilings for other women to run? (산점도 재현 불가)

	local rdvarlist "i.womanwon##c.femalecand_margin_of_victory##c.femalecand_margin_of_victory##c.femalecand_margin_of_victory##c.femalecand_margin_of_victory" // 독립변수 목록
	local elitedepvarlist "femcands_over_contests_next_75m femcands_over_contests_next_10r femwins_over_contests_next_75m femwins_over_contests_next_10r" // Table 2의 종속변수 목록
	local bandwidth "abs(femalecand_margin_of_victory)<0.15"

	* Table 2
		local i=1
		foreach dv of varlist `elitedepvarlist' {
			reg `dv' `rdvarlist' if `bandwidth', robust
			predict yhat_e`i' if `dv'!=., xb // 독립변수값별 종속변수값 계산
			predict sehat_e`i' if `dv'!=., stdp // 독립변수값별 종속변수의 standard error값 계산
			gen yhatu_e`i'=yhat_e`i'+2*sehat_e`i' if `dv'!=. // 신뢰구간 상한 계산
			gen yhatl_e`i'=yhat_e`i'-2*sehat_e`i' if `dv'!=. // 신뢰구간 하한 계산
			by bin: egen bin_e`i'_mean=mean(`dv') if `dv'!=. // 각 구간에서 DV의 평균값을 계산
			by bin: egen bin_e`i'_n=count(`dv') if `dv'!=. // 각 구간에서 DV의 관측치 수를 계산
			local i=`i'+1
		}

	* Fig. 4, panel (a)
		twoway (scatter bin_e1_mean binx if `bandwidth' [w=bin_e1_n], /// 산점도 출력
			ylabel(0.3(0.05)0.65) xlabel(-0.15(0.05)0.15, nogrid) /// x,y축 구간 지정
			xline(0, lp(solid) lc(gs10)) /// x=0에 직선 출력
			mcolor(gs4%50) msize(tiny)) ///
			(line yhat_e1 femalecand_margin_of_victory if `bandwidth', sort /// 회귀선 출력
			lp(solid) lc(gs4%50) legend(off)) ///
			(line yhatu_e1 femalecand_margin_of_victory if `bandwidth', sort /// 신뢰구간 상한 출력
			lp(dash) lc(gs4%50) legend(off)) ///
			(line yhatl_e1 femalecand_margin_of_victory if `bandwidth', sort /// 신뢰구간 하한 출력
			lp(dash) lc(gs4%50) legend(off) ///
			xtitle("Female Candidate’s Margin of Victory - Election 1") title("Female Candidates per Contest Districts within 75 Miles - Election 2"))

	* Fig. 4, panel (c)
		twoway (scatter bin_e2_mean binx if `bandwidth' [w=bin_e2_n], /// 산점도 출력
			ylabel(0.3(0.05)0.65) xlabel(-0.15(0.05)0.15, nogrid) /// x,y축 구간 지정
			xline(0, lp(solid) lc(gs10)) /// x=0에 직선 출력
			mcolor(gs4%50) msize(tiny)) ///
			(line yhat_e2 femalecand_margin_of_victory if `bandwidth', sort /// 회귀선 출력
			lp(solid) lc(gs4%50) legend(off)) ///
			(line yhatu_e2 femalecand_margin_of_victory if `bandwidth', sort /// 신뢰구간 상한 출력
			lp(dash) lc(gs4%50) legend(off)) ///
			(line yhatl_e2 femalecand_margin_of_victory if `bandwidth', sort /// 신뢰구간 하한 출력
			lp(dash) lc(gs4%50) legend(off) ///
			xtitle("Female Candidate’s Margin of Victory - Election 1") title("Female Candidates per Contest 10 Closest Districts - Election 2"))

	* Fig. 4, panel (b)
		twoway (scatter bin_e3_mean binx if `bandwidth' [w=bin_e3_n], /// 산점도 출력
			ylabel(0.1(0.05)0.4) xlabel(-0.15(0.05)0.15, nogrid) /// x,y축 구간 지정
			xline(0, lp(solid) lc(gs10)) /// x=0에 직선 출력
			mcolor(gs4%50) msize(tiny)) ///
			(line yhat_e3 femalecand_margin_of_victory if `bandwidth', sort /// 회귀선 출력
			lp(solid) lc(gs4%50) legend(off)) ///
			(line yhatu_e3 femalecand_margin_of_victory if `bandwidth', sort /// 신뢰구간 상한 출력
			lp(dash) lc(gs4%50) legend(off)) ///
			(line yhatl_e3 femalecand_margin_of_victory if `bandwidth', sort /// 신뢰구간 하한 출력
			lp(dash) lc(gs4%50) legend(off) ///
			xtitle("Female Candidate’s Margin of Victory - Election 1") title("Female Victories per Contest Districts within 75 Miles - Election 2"))

	* Fig. 4, panel (d)
		twoway (scatter bin_e4_mean binx if `bandwidth' [w=bin_e4_n], /// 산점도 출력
			ylabel(0.1(0.05)0.4) xlabel(-0.15(0.05)0.15, nogrid) /// x,y축 구간 지정
			xline(0, lp(solid) lc(gs10)) /// x=0에 직선 출력
			mcolor(gs4%50) msize(tiny)) ///
			(line yhat_e4 femalecand_margin_of_victory if `bandwidth', sort /// 회귀선 출력
			lp(solid) lc(gs4%50) legend(off)) ///
			(line yhatu_e4 femalecand_margin_of_victory if `bandwidth', sort /// 신뢰구간 상한 출력
			lp(dash) lc(gs4%50) legend(off)) ///
			(line yhatl_e4 femalecand_margin_of_victory if `bandwidth', sort /// 신뢰구간 하한 출력
			lp(dash) lc(gs4%50) legend(off) ///
			xtitle("Female Candidate’s Margin of Victory - Election 1") title("Female Victories per Contest 10 Closest Districts - Election 2"))

*** Appendix: Table A1

	qui {
		noisily di as text " Specification	 0.05		 0.10		 0.15		 0.20		 0.25" // 제목 행 출력
		noisily di as input "{hline}" // 구분선 출력
		local rdvarlist "i.womanwon##c.femalecand_margin_of_victory##c.femalecand_margin_of_victory##c.femalecand_margin_of_victory##c.femalecand_margin_of_victory" // 독립변수 목록
		local depvarlist "femaleturnout_nextcycle female_percentageofelectorate_ne femaleonballotnextyear femcands_over_contests_next5r femwins_over_contests_next5r femcands_over_contests_next_10r femwins_over_contests_next_10r femcands_over_contests_next_15r femwins_over_contests_next_15r femcands_over_contests_next_50m femwins_over_contests_next_50m femcands_over_contests_next_75m femwins_over_contests_next_75m femcands_over_contests_next_100m femwins_over_contests_next_100m" // 종속변수 목록
		local speclist "fem_turnout fem_electorate fem_ballot femcands_dist5r femwins_dist5r femcands_dist10r femwins_dist10r femcands_dist15r femwins_dist15r femcands_dist50m femwins_dist50m femcands_dist75m femwins_dist75m femcands_dist100m femwins_dist100m" // Specification 목록
		local j=1		
		foreach dv of varlist `depvarlist' {
		* loop over Dependent variables
			local i=1
			foreach bw of numlist 0.05 0.10 0.15 0.20 0.25 {
			* loop over Bandwidth
				local bandwidth "abs(femalecand_margin_of_victory)<`bw'"
				qui reg `dv' `rdvarlist' if `bandwidth', robust
				local coef_`i'=_b[1.womanwon] // 대역폭별로 계수값 저장
				local se_`i'=_se[1.womanwon] // 대역폭별로 표준오차값 저장
				local space_`i' "" // -부호 유무를 고려한 계수값 정렬
				if `coef_`i''>=0 {
					local space_`i' " "
				}
				local i=`i'+1
			}
			local spec : word `j' of `speclist'
			while strlen("`spec'")<17 { // Specification 길이가 17보다 짧으면
				local spec="`spec'"+" " // 공백을 추가해서 17로 맞춰줌
			}
			noisily di as text "`spec'  " as result "`space_1'" %4.3f `coef_1' " (" %4.3f `se_1' ")  " "`space_2'" %4.3f `coef_2' " (" %4.3f `se_2' ")  " "`space_3'" %4.3f `coef_3' " (" %4.3f `se_3' ")  " "`space_4'" %4.3f `coef_4' " (" %4.3f `se_4' ")  " "`space_5'" %4.3f `coef_5' " (" %4.3f `se_5' ")" // 대역폭별로 계수값과 표준오차값을 출력
			local j=`j'+1
		}
	}
