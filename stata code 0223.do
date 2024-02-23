// Impact of Digital Empowerment on Labor Employment in Manufacturing Enterprises: Evidence from China //

cd"D:\stata\personal\paper_1"

by year Stkcd,sort: egen humans=mean(Amount)
by year code,sort: egen AC=total( TransactionRank )
by year code,sort: egen SAC=total(university)
by year code,sort: egen HAC=total(hengxiang )
by year code,sort: egen ZAC=total(zongxiang)
duplicates drop year Stkcd,force
drop zongxiang hengxiang university TransactionRank Repart
gen DT=Dige/100
gen humansource=humans/renshu

//删除缺失值
. nmissing
drop if lnhumans==.
drop if DT==.
drop if Scale==.
drop if ROE==.
drop if AGE==.
drop if RDS==.
drop if ROA==.
drop if CIR==.
drop if TobinQ==.
drop if Manage==.
drop if Sales==.
drop if opacf==.
drop if lnManager==.
drop if lnrdp==.
drop if lnlabor==.

//样本缩尾处理
ssc install winsor2, replace
winsor2 lnlabor DT Scale ROE AGE RDS ROA CIR TobinQ, replace cuts(1 99) //lnlabor?
trim
recode humansource (mis=0)

//1.描述性统计
logout, save(miaoshutongji2) word replace: tabstat lnlabor lnhumans lnrdp lnManagerNumber DT Scale ROE AGE RDS ROA CIR TobinQ Manage Sales opacf,stat(mean sd  min max p50 N) format(%6.2f) c(s)
outreg2 using miao.doc,replace sum(log) title(Decriptive statistics)
outreg2 using miaoshu1.doc, replace sum(log) keep(lnhumans DF Scale ROE AGE RDP ROA CIR TobinQ)
local x "lnhumans DF Scale ROE AGE RDP ROA CIR TobinQ"
	   tabstat `x', s(n mean sd min p25 p50 p75 max) ///
	                format(%6.3f) c(s)	  
//2.检验多重共线性
reg lnhumans DT Scale ROE AGE RDS ROA CIR TobinQ
estat vif
reg lnlabor DT Scale ROE AGE RDS ROA CIR TobinQ
estat vif
reg lnrdp DT Scale ROE AGE RDS ROA CIR TobinQ 
estat vif
reg lnManagerNumber DT Scale ROE AGE RDS ROA CIR TobinQ
estat vif

//3.相关系数检验
duplicates list code year //删除重复项
duplicates list code year
duplicates tag code year,gen(isdup2)
edit if isdup
xtserial lnhumans DT Scale ROE AGE RDS ROA CIR TobinQ
local v  "lnhumans DT Scale ROE AGE RDS ROA CIR TobinQ"
local s  "tables2 "
logout, save("`s'") word replace: ///
    pwcorr_a `v'

//4.实证分析
xtset code year
ssc install reghdfe
gen DT2=DT^2

//单位根检验
xtunitroot fisher lnhumans, trend dfuller demean lags(1) 
xtunitroot fisher DT, trend dfuller demean lags(1)
xtunitroot fisher Scale, trend dfuller demean lags(1)
xtunitroot fisher ROE, trend dfuller demean lags(1)
xtunitroot fisher AGE, trend dfuller demean lags(1)
xtunitroot fisher RDS, trend dfuller demean lags(1)
xtunitroot fisher ROA, trend dfuller demean lags(1)
xtunitroot fisher CIR, trend dfuller demean lags(1)
xtunitroot fisher TobinQ, trend dfuller demean lags(1)
xtunitroot fisher Manage, trend dfuller demean lags(1)
xtunitroot fisher Sales, trend dfuller demean lags(1)
xtunitroot fisher opacf, trend dfuller demean lags(1)
xtunitroot fisher lnManager, trend dfuller demean lags(1)
xtunitroot fisher lnrdp, trend dfuller demean lags(1)
xtunitroot fisher lnlabor, trend dfuller demean lags(1)

//基础回归
xtreg lnhumans DT Scale ROE AGE RDS ROA CIR TobinQ,fe
estimates store fe
xtreg lnhumans DT Scale ROE AGE RDS ROA CIR TobinQ,re
estimates store re
hausman fe re
reg lnhumans DF Scale ROE AGE RDP ROA CIR TobinQ
estimates store ols
reghdfe lnlabor DT Scale ROE AGE RDS ROA CIR TobinQ,a(code year CITYCODE IndustryCode) vce(robust)
est store q1
reghdfe lnhumans DT Scale ROE AGE RDS ROA CIR TobinQ,a(code year CITYCODE IndustryCode) vce(robust)
est store q2
reghdfe lnrdp DT Scale ROE AGE RDS ROA CIR TobinQ,a(code year CITYCODE IndustryCode) vce(robust)
est store q3
reghdfe lnManagerNumber DT Scale ROE AGE RDS ROA CIR TobinQ,a(code year CITYCODE IndustryCode) vce(robust)
est store q4
esttab q1 q2 q3 q4 using tab1.rtf, star(* 0.10 ** 0.05 *** 0.01) r2(4) ar2(4) t(3) b(%9.4f) replace nogap

//稳健性检验
egen mhumansource=median(humansource)
gen humansource1=0
replace humansource1=1  if humansource >= mhumansource //二值变量
probit humansource1 DT Scale ROE AGE RDS ROA CIR TobinQ
est store q5
predict y_hat, xb
gen pdf = normalden(y_hat)  //概率密度函数
gen cdf = normal(y_hat)     //累积分布函数
gen imr = pdf/cdf           //计算逆米尔斯比率
reghdfe lnhumans DT Scale ROE AGE RDS ROA CIR TobinQ imr,a(code year CITYCODE IndustryCode) vce(robust)
est store q6
ivreghdfe lnhumans (DT=neisheng1) Scale ROE AGE RDS ROA CIR TobinQ,absorb(code year CITYCODE IndustryCode) robust first endog(DT)
est store q7
reghdfe lnhumans 总指数 Scale ROE AGE RDS ROA CIR TobinQ,a(code year CITYCODE IndustryCode) vce(robust)
est store q8
reghdfe lnhumans DT Scale ROE AGE RDS ROA CIR TobinQ if 2012<=year& year<=2019,a(code year CITYCODE IndustryCode) vce(robust)
est store q9
reghdfe lnhumans DT TreatPost Scale ROE AGE RDS ROA CIR TobinQ,a(code year CITYCODE IndustryCode) vce(robust)
est store q10
esttab q5 q6 q7 q8 q9 q10 using tab25.rtf,star(* 0.10 ** 0.05 *** 0.01) r2(4) ar2(4) t(3) b(%9.4f) replace nogap

//行业竞争异质性检验
reghdfe  lnhumans DT Scale ROE AGE RDS ROA CIR TobinQ if HHI>=0.1,a(code year  CITYCODE IndustryCode) vce(robust)
est store q11
reghdfe  lnhumans DT Scale ROE AGE RDS ROA CIR TobinQ if HHI<0.1,a(code year  CITYCODE IndustryCode) vce(robust)
est store q12
reghdfe  lnhumans DT Scale ROE AGE RDS ROA CIR TobinQ if State==1,a(code year  CITYCODE IndustryCode) vce(robust)
est store q13
reghdfe  lnhumans DT Scale ROE AGE RDS ROA CIR TobinQ if State==0,a(code year  CITYCODE IndustryCode) vce(robust)
est store q14
reghdfe  lnhumans DT Scale ROE AGE RDS ROA CIR TobinQ if 营商环境==1,a(code year  CITYCODE IndustryCode) vce(robust)
est store q15
reghdfe  lnhumans DT Scale ROE AGE RDS ROA CIR TobinQ if 营商环境==0,a(code year  CITYCODE IndustryCode) vce(robust)
est store q16
esttab q11 q12 q13 q14 q15 q16   using tab8.rtf,star(* 0.10 ** 0.05 *** 0.01) r2(4) ar2(4) t(3) b(%9.4f) replace nogap

//计算全要素生产率
gen lny=ln(营业收入)
gen lnl=ln(renshu)
gen lnk=ln(固定资产净额)
gen lnm=ln(营业成本+ 销售费用+ 管理费用+ 财务费用-折旧摊销-支付给职工以及为职工支付的现金)
gen lninvest=ln(invest)
ssc install prodest
//LP method with ACF correction
prodest lny, method(lp) free(lnl) proxy(lnm) state(lnk) valueadded acf id(code) t(year) reps(50)
predict lpacf, resid
//WRDG method
prodest lny, method(wrdg) free(lnl) proxy(lnm) state(lnk) valueadded id(code) t(year) poly(2)
predict WRDG, resid
//MrEst method？
prodest lny, method(mr) free(lnl) proxy(lnm) state(lnk) valueadded lags(1) id(code) t(year) poly(2) 
predict MrEst, resid
//OP method with ACF correction
prodest lny, method(op) free(lnl) proxy(lninvest) state(lnk) valueadded acf optimizer(nm) id(code) t(year) reps(50)  
predict opacf, resid

//机制检验
reghdfe opacf DT Scale ROE AGE RDS ROA CIR TobinQ,a(code year CITYCODE IndustryCode) vce(robust)
est store q17
reghdfe lnhumans opacf DT Scale ROE AGE RDS ROA CIR TobinQ,a(code year CITYCODE IndustryCode) vce(robust)
est store q18
reghdfe Sales DT Scale ROE AGE RDS ROA CIR TobinQ,a(code year CITYCODE IndustryCode) vce(robust)
est store q19
reghdfe lnhumans Sales DT Scale ROE AGE RDS ROA CIR TobinQ,a(code year CITYCODE IndustryCode) vce(robust)
est store q20
reghdfe Manage DT Scale ROE AGE ROA RDS CIR TobinQ,a(code year CITYCODE IndustryCode) vce(robust)
est store q21
reghdfe lnhumans Manage DT Scale ROE AGE RDS ROA CIR TobinQ,a(code year CITYCODE IndustryCode) vce(robust)
est store q22
esttab q17 q18 q19 q20 q21 q22 using tab3.rtf,star(* 0.10 ** 0.05 *** 0.01) r2(4) ar2(4) t(3) b(%9.4f) replace nogap
reg lnhumans DT Scale ROE AGE RDS ROA CIR TobinQ

//中介检验
bootstrap r(ind_eff) r(dir_eff), reps(1000): sgmediation lnhumans, mv(Manage) iv(DT) cv(Scale ROE AGE RDS ROA CIR  TobinQ)
est store q26
bootstrap r(ind_eff) r(dir_eff), reps(1000): sgmediation lnhumans, mv(Sales) iv(DT) cv(Scale ROE AGE RDS ROA CIR  TobinQ)
est store q27
bootstrap r(ind_eff) r(dir_eff), reps(1000): sgmediation lnhumans, mv(opacf) iv(DT) cv(Scale ROE AGE RDS ROA CIR  TobinQ)
est store q28
esttab q26 q27 q28 using tab40.rtf,star(* 0.10 ** 0.05 *** 0.01) r2(4) ar2(4) t(3) b(%9.4f) replace nogap



