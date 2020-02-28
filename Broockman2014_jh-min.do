cd "C:\Users\J\Dropbox\대학원\과학으로서의정치학회\Replication2019FW"
use Broockman2014, clear

*** 전처리: 2.2. Data

	rename * , low
	set scheme plotplainblind // 그래프 스타일 지정
	graph set window fontface "Sandoll 고딕 TTF 03 Bold" // 그래프 범례 및 축 표시에 사용될 폰트를 지정

	* generate State × year FEs via -gen-
		encode state , gen(st) // string을 numeric으로 변환
		gen styear=st*next_cycle_year // 종속변수의 다음 선거 연도 기준
		xtset styear
		* generate State × year FEs via -egen- and verify both ways are the same
			capture snapshot erase _all
			snapshot save // sort 적용 이전의 데이터 상태를 기록
			egen styear_egen=group(state next_cycle_year)
			foreach x of varlist styear* {
				by `x', sort: gen nvals_`x' = _n == 1
				count if nvals_`x'
			} // https://www.stata.com/support/faqs/data-management/number-of-distinct-observations/
			snapshot restore 1 // sort 적용 이전의 데이터 상태를 복원

	* generate 0.4-percentage point wide bins
		local numbins 250
		gen bin=round(`numbins'*(femalecand_margin_of_victory+(0.5/`numbins')))/`numbins'
		sort bin femalecand_margin_of_victory
		by bin: gen binid=_n // 각 bin별로 id 생성
		gen binx=bin-(0.5/`numbins') // x축에 표시할 좌표를 조정

*** 분석: 3. Does electing women empower other women to vote?

	qui {
	* Table 1
		local rdvarlist "i.womanwon##c.femalecand_margin_of_victory##c.femalecand_margin_of_victory##c.femalecand_margin_of_victory##c.femalecand_margin_of_victory" // 독립변수 목록
		local massdepvarlist "femaleonballotnextyear femaleturnout_nextcycle female_percentageofelectorate_ne" // Table 1의 종속변수 목록
		local bandwidth "abs(femalecand_margin_of_victory)<0.15"

		local i=1 // dv id
		foreach dv of varlist `massdepvarlist'  { // 고정효과를 고려하지 않은 모델별 회귀분석 결과를 저장
			reg `dv' `rdvarlist' if `bandwidth', robust
			local t1_`i'_1b : di %4.3f _b[1.womanwon]
			local t1_`i'_1se : di "(" %4.3f _se[1.womanwon] ")"
			local t1_`i'_1 "`t1_`i'_1b' `t1_`i'_1se'"
			local t1_`i'_2 "No"
			local t1_`i'_3b : di %4.3f _b[_cons]
			local t1_`i'_3se : di "(" %4.3f _se[_cons] ")"
			local t1_`i'_3 "`t1_`i'_3b' `t1_`i'_3se'"
			local t1_`i'_4 : di %4.3f e(r2)
			local t1_`i'_5 : di e(N)
			local i=`i'+1
		}

		local i=1 // dv id
		foreach dv of varlist `massdepvarlist'  { // 고정효과를 고려한 모델별 회귀분석 결과 저장
			xtreg `dv' `rdvarlist' if `bandwidth', fe
			local t1f_`i'_1b : di %4.3f _b[1.womanwon]
			local t1f_`i'_1se : di "(" %4.3f _se[1.womanwon] ")"
			local t1f_`i'_1 "`t1f_`i'_1b' `t1f_`i'_1se'"
			local t1f_`i'_2 "Yes"
			local t1f_`i'_3 "n/a"
			local t1f_`i'_4 : di %4.3f e(r2)
			local t1f_`i'_5 : di e(N)
			local i=`i'+1
		}

		local row_1 "Woman won at time 1"
		local row_2 "State × year FEs?"
		local row_3 "Constant"
		local row_4 "R-squared"
		local row_5 "N"
		local i=1 // dv id
		foreach dv of varlist `massdepvarlist' { // 표 출력
			noisily di as text " 	`dv'" _newline as input "{hline 60}" // 제목 행 출력
			local j=1 // row id
			while `j'<6 { // estimate id
				while ustrlen("`row_`j''")<ustrlen("`row_1'") {
					local row_`j'="`row_`j''"+" "
				} // 공백을 추가해서 문자열 길이를 맞춰줌
				while ustrlen("`t1_`i'_`j''")<ustrlen("`t1_`i'_1'") {
					local t1_`i'_`j'="`t1_`i'_`j''"+" "
				} // 공백을 추가해서 문자열 길이를 맞춰줌
				noisily di as text " `row_`j''	" as result "`t1_`i'_`j''	`t1f_`i'_`j''" // 행별로 고정효과를 통제하지 않은 모델과 통제한 모델의 회귀분석 결과를 출력
				local j=`j'+1
			}
			noisily di as input "{hline 60}" _newline
			local i=`i'+1
		}

	}

	qui {
	* Fig. 3
		local rdvarlist "i.womanwon##c.femalecand_margin_of_victory##c.femalecand_margin_of_victory##c.femalecand_margin_of_victory##c.femalecand_margin_of_victory" // 독립변수 목록
		local massdepvarlist "femaleonballotnextyear femaleturnout_nextcycle female_percentageofelectorate_ne" // Table 1의 종속변수 목록
		local bandwidth "abs(femalecand_margin_of_victory)<0.15"

		local i=1
		foreach dv of varlist `massdepvarlist' { // predict y & generate bins
			reg `dv' `rdvarlist' if `bandwidth', robust
			predict yhat_m`i', xb // 독립변수값별 종속변수값 계산
			predict sehat_m`i', stdp // 독립변수값별 종속변수의 standard error값 계산
			gen yhatu_m`i'=yhat_m`i'+2*sehat_m`i' // 신뢰구간 상한 계산
			gen yhatl_m`i'=yhat_m`i'-2*sehat_m`i' // 신뢰구간 하한 계산
			by bin: egen bin_m`i'_mean=mean(`dv') // 각 구간에서 DV의 평균값을 계산
			by bin: egen bin_m`i'_n=count(`dv') // 각 구간에서 DV의 관측치 수를 계산
			local i=`i'+1
		}

		twoway /// Fig. 3, panel (a)
			(scatter bin_m2_mean binx if `bandwidth' & binid==1 [w=bin_m2_n], /// 산점도 출력
			ylabel(0.35(0.05)0.65) xlabel(-0.15(0.05)0.15, nogrid) /// x,y축 구간 지정
			xline(0, lp(solid) lc(gs10)) /// x=0에 직선 출력
			mcolor(gs6) msize(tiny)) ///
			(line yhat_m2 femalecand_margin_of_victory if `bandwidth', sort /// 회귀선 출력
			lp(solid) lc(gs6) legend(off)) ///
			(line yhatu_m2 femalecand_margin_of_victory if `bandwidth', sort /// 신뢰구간 상한 출력
			lp(dash) lc(gs4%50)) ///
			(line yhatl_m2 femalecand_margin_of_victory if `bandwidth', sort /// 신뢰구간 하한 출력
			lp(dash) lc(gs4%50) ///
			xtitle("Female Candidate’s Margin of Victory - Election 1") title("Female Voter Turnout - Election 2"))
		graph copy f3a

		twoway /// Fig. 3, panel (b)
			(scatter bin_m3_mean binx if `bandwidth'  & binid==1 [w=bin_m3_n], /// 산점도 출력
			ylabel(0.5(0.01)0.55) xlabel(-0.15(0.05)0.15, nogrid) /// x,y축 구간 지정
			xline(0, lp(solid) lc(gs10)) /// x=0에 직선 출력
			mcolor(gs6) msize(tiny)) ///
			(line yhat_m3 femalecand_margin_of_victory if `bandwidth', sort /// 회귀선 출력
			lp(solid) lc(gs6) legend(off)) ///
			(line yhatu_m3 femalecand_margin_of_victory if `bandwidth', sort /// 신뢰구간 상한 출력
			lp(dash) lc(gs4%50)) ///
			(line yhatl_m3 femalecand_margin_of_victory if `bandwidth', sort /// 신뢰구간 하한 출력
			lp(dash) lc(gs4%50) ///
			xtitle("Female Candidate’s Margin of Victory - Election 1") title("Female Percentage of Electorate - Election 2"))
		graph copy f3b

		twoway /// Fig. 3, panel (c): N 불일치
			(scatter bin_m1_mean binx if `bandwidth' & binid==1 [w=bin_m1_n], /// 산점도 출력
			ylabel(0.1(0.1)1) xlabel(-0.15(0.05)0.15, nogrid) /// x,y축 구간 지정
			xline(0, lp(solid) lc(gs10)) /// x=0에 직선 출력
			mcolor(gs6) msize(tiny)) ///
			(line yhat_m1 femalecand_margin_of_victory if `bandwidth', sort /// 회귀선 출력
			lp(solid) lc(gs6) legend(off)) ///
			(line yhatu_m1 femalecand_margin_of_victory if `bandwidth', sort /// 신뢰구간 상한 출력
			lp(dash) lc(gs4%50)) ///
			(line yhatl_m1 femalecand_margin_of_victory if `bandwidth', sort /// 신뢰구간 하한 출력
			lp(dash) lc(gs4%50) ///
			xtitle("Female Candidate’s Margin of Victory - Election 1") title("Woman on Ballot Next Year in Same District - Election 2"))
		graph copy f3c
	
	}

*** 분석: 4. Do female politicians break glass ceilings for other women to run?

	local rdvarlist "i.womanwon##c.femalecand_margin_of_victory##c.femalecand_margin_of_victory##c.femalecand_margin_of_victory##c.femalecand_margin_of_victory" // 독립변수 목록
	local elitedepvarlist "femcands_over_contests_next_75m femcands_over_contests_next_10r femwins_over_contests_next_75m femwins_over_contests_next_10r" // Table 2의 종속변수 목록
	local bandwidth "abs(femalecand_margin_of_victory)<0.15"

	* Table 2
		local i=1
		foreach dv of varlist `elitedepvarlist' {
			reg `dv' `rdvarlist' if `bandwidth', robust
			predict yhat_e`i', xb // 독립변수값별 종속변수값 계산
			predict sehat_e`i', stdp // 독립변수값별 종속변수의 standard error값 계산
			gen yhatu_e`i'=yhat_e`i'+2*sehat_e`i' // 신뢰구간 상한 계산
			gen yhatl_e`i'=yhat_e`i'-2*sehat_e`i' // 신뢰구간 하한 계산
			by bin: egen bin_e`i'_mean=mean(`dv') // 각 구간에서 DV의 평균값을 계산
			by bin: egen bin_e`i'_n=count(`dv') // 각 구간에서 DV의 관측치 수를 계산
			local i=`i'+1
		}

	* Fig. 4, panel (a)
		twoway (scatter bin_e1_mean binx if `bandwidth' & binid==1  [w=bin_e1_n], /// 산점도 출력
			ylabel(0.3(0.05)0.65) xlabel(-0.15(0.05)0.15, nogrid) /// x,y축 구간 지정
			xline(0, lp(solid) lc(gs10)) /// x=0에 직선 출력
			mcolor(gs6) msize(tiny)) ///
			(line yhat_e1 femalecand_margin_of_victory if `bandwidth', sort /// 회귀선 출력
			lp(solid) lc(gs6) legend(off)) ///
			(line yhatu_e1 femalecand_margin_of_victory if `bandwidth', sort /// 신뢰구간 상한 출력
			lp(dash) lc(gs4%50)) ///
			(line yhatl_e1 femalecand_margin_of_victory if `bandwidth', sort /// 신뢰구간 하한 출력
			lp(dash) lc(gs4%50) ///
			xtitle("Female Candidate’s Margin of Victory - Election 1") title("Female Candidates per Contest Districts within 75 Miles - Election 2"))
		graph copy f4a

	* Fig. 4, panel (b)
		twoway (scatter bin_e3_mean binx if `bandwidth' & binid==1 [w=bin_e3_n], /// 산점도 출력
			ylabel(0.1(0.05)0.4) xlabel(-0.15(0.05)0.15, nogrid) /// x,y축 구간 지정
			xline(0, lp(solid) lc(gs10)) /// x=0에 직선 출력
			mcolor(gs6) msize(tiny)) ///
			(line yhat_e3 femalecand_margin_of_victory if `bandwidth', sort /// 회귀선 출력
			lp(solid) lc(gs6) legend(off)) ///
			(line yhatu_e3 femalecand_margin_of_victory if `bandwidth', sort /// 신뢰구간 상한 출력
			lp(dash) lc(gs4%50)) ///
			(line yhatl_e3 femalecand_margin_of_victory if `bandwidth', sort /// 신뢰구간 하한 출력
			lp(dash) lc(gs4%50) ///
			xtitle("Female Candidate’s Margin of Victory - Election 1") title("Female Victories per Contest Districts within 75 Miles - Election 2"))
		graph copy f4b

	* Fig. 4, panel (c)
		twoway (scatter bin_e2_mean binx if `bandwidth' & binid==1 [w=bin_e2_n], /// 산점도 출력
			ylabel(0.3(0.05)0.65) xlabel(-0.15(0.05)0.15, nogrid) /// x,y축 구간 지정
			xline(0, lp(solid) lc(gs10)) /// x=0에 직선 출력
			mcolor(gs6) msize(tiny)) ///
			(line yhat_e2 femalecand_margin_of_victory if `bandwidth', sort /// 회귀선 출력
			lp(solid) lc(gs6) legend(off)) ///
			(line yhatu_e2 femalecand_margin_of_victory if `bandwidth', sort /// 신뢰구간 상한 출력
			lp(dash) lc(gs4%50)) ///
			(line yhatl_e2 femalecand_margin_of_victory if `bandwidth', sort /// 신뢰구간 하한 출력
			lp(dash) lc(gs4%50) ///
			xtitle("Female Candidate’s Margin of Victory - Election 1") title("Female Candidates per Contest 10 Closest Districts - Election 2"))
		graph copy f4c

	* Fig. 4, panel (d)
		twoway (scatter bin_e4_mean binx if `bandwidth' & binid==1 [w=bin_e4_n], /// 산점도 출력
			ylabel(0.1(0.05)0.4) xlabel(-0.15(0.05)0.15, nogrid) /// x,y축 구간 지정
			xline(0, lp(solid) lc(gs10)) /// x=0에 직선 출력
			mcolor(gs6) msize(tiny)) ///
			(line yhat_e4 femalecand_margin_of_victory if `bandwidth', sort /// 회귀선 출력
			lp(solid) lc(gs6) legend(off)) ///
			(line yhatu_e4 femalecand_margin_of_victory if `bandwidth', sort /// 신뢰구간 상한 출력
			lp(dash) lc(gs4%50)) ///
			(line yhatl_e4 femalecand_margin_of_victory if `bandwidth', sort /// 신뢰구간 하한 출력
			lp(dash) lc(gs4%50) ///
			xtitle("Female Candidate’s Margin of Victory - Election 1") title("Female Victories per Contest 10 Closest Districts - Election 2"))
		graph copy f4d

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
			while strlen("`spec'")<17 {
				local spec="`spec'"+" "
			} // 공백을 추가해서 문자열 길이를 맞춰줌
			noisily di as text "`spec'  " as result "`space_1'" %4.3f `coef_1' " (" %4.3f `se_1' ")  " "`space_2'" %4.3f `coef_2' " (" %4.3f `se_2' ")  " "`space_3'" %4.3f `coef_3' " (" %4.3f `se_3' ")  " "`space_4'" %4.3f `coef_4' " (" %4.3f `se_4' ")  " "`space_5'" %4.3f `coef_5' " (" %4.3f `se_5' ")" // 대역폭별로 계수값과 표준오차값을 출력
			local j=`j'+1
		}
	}
