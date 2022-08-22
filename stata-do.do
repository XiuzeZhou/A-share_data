***Impact of Digital Empowerment on Labor Employment in Manufacturing Enterprises: Evidence from China
xtset code year
//基础回归
reghdfe lnlabor DT Scale ROE AGE RDS ROA CIR TobinQ,a(code year CITYCODE IndustryCode) vce(robust)
est store q1
reghdfe lnhumans DT Scale ROE AGE RDS ROA CIR TobinQ,a(code year CITYCODE IndustryCode) vce(robust)
est store q2
reghdfe lnrdp DT Scale ROE AGE RDS ROA CIR TobinQ,a(code year CITYCODE IndustryCode) vce(robust)
est store q3
reghdfe lnManagerNumber DT Scale ROE AGE RDS ROA CIR TobinQ,a(code year CITYCODE IndustryCode) vce(robust)
est store q4
esttab q1 q2 q3 q4 using tab1.rtf, star(* 0.10 ** 0.05 *** 0.01) r2(4) ar2(4) t(3) b(%9.4f) replace nogap

//稳健
probit humansource DT Scale ROE AGE RDS ROA CIR TobinQ
est store q1
predict y_hat, xb
gen pdf = normalden(y_hat)  //概率密度函数
gen cdf = normal(y_hat)     //累积分布函数
gen imr = pdf/cdf           //计算逆米尔斯比率
reghdfe lnhumans DT Scale ROE AGE RDS ROA CIR TobinQ imr,a(code year CITYCODE IndustryCode) vce(robust)
est store q2
ivreghdfe lnhumans (DT=neisheng1) Scale ROE AGE RDS ROA CIR TobinQ,absorb(code year CITYCODE IndustryCode) robust first endog(DT)
est store q3
reghdfe lnhumans 总指数 Scale ROE AGE RDS ROA CIR TobinQ,a(code year CITYCODE IndustryCode) vce(robust)
est store q4
reghdfe lnhumans DT Scale ROE AGE RDS ROA CIR TobinQ if 2012<=year& year<=2019,a(code year CITYCODE IndustryCode) vce(robust)
est store q5
reghdfe lnhumans DT TreatPost Scale ROE AGE RDS ROA CIR TobinQ,a(code year CITYCODE IndustryCode) vce(robust)
est store q6
esttab q1 q2 q3 q4 q5 q6 using tab2.rtf,star(* 0.10 ** 0.05 *** 0.01) r2(4) ar2(4) t(3) b(%9.4f) replace nogap

//机制检验
reghdfe opacf DT Scale ROE AGE RDS ROA CIR TobinQ,a(code year CITYCODE IndustryCode) vce(robust)
est store q1
reghdfe lnhumans opacf DT Scale ROE AGE RDS ROA CIR TobinQ,a(code year CITYCODE IndustryCode) vce(robust)
est store q2
reghdfe Sales DT Scale ROE AGE RDS ROA CIR TobinQ,a(code year CITYCODE IndustryCode) vce(robust)
est store q3
reghdfe lnhumans Sales DT Scale ROE AGE RDS ROA CIR TobinQ,a(code year CITYCODE IndustryCode) vce(robust)
est store q4
reghdfe Manage DT Scale ROE AGE ROA RDS CIR TobinQ,a(code year CITYCODE IndustryCode) vce(robust)
est store q5
reghdfe lnhumans Manage DT Scale ROE AGE RDS ROA CIR TobinQ,a(code year CITYCODE IndustryCode) vce(robust)
est store q6
esttab q1 q2 q3 q4 q5 q6 using tab3.rtf,star(* 0.10 ** 0.05 *** 0.01) r2(4) ar2(4) t(3) b(%9.4f) replace nogap
reg lnhumans DT Scale ROE AGE RDS ROA CIR TobinQ
//中介检验
bootstrap r(ind_eff) r(dir_eff), reps(1000): sgmediation lpacf, mv(Manage) iv(DT) cv(Scale ROE AGE RDS ROA CIR  TobinQ)
est store q1
bootstrap r(ind_eff) r(dir_eff), reps(1000): sgmediation lpacf, mv(Sales) iv(DT) cv(Scale ROE AGE RDS ROA CIR  TobinQ)
est store q2
bootstrap r(ind_eff) r(dir_eff), reps(1000): sgmediation lpacf, mv(opacf) iv(DT) cv(Scale ROE AGE RDS ROA CIR  TobinQ)
est store q3
esttab q1 q2 q3 using tab4.rtf,star(* 0.10 ** 0.05 *** 0.01) r2(4) ar2(4) t(3) b(%9.4f) replace nogap

//行业竞争异质性检验
reghdfe  lnhumans DT Scale ROE AGE RDS ROA CIR TobinQ if HHI>=0.1,a(code year  CITYCODE IndustryCode) vce(robust)
est store q1
reghdfe  lnhumans DT Scale ROE AGE RDS ROA CIR TobinQ if HHI<0.1,a(code year  CITYCODE IndustryCode) vce(robust)
est store q2
reghdfe  lnhumans DT Scale ROE AGE RDS ROA CIR TobinQ if State==1,a(code year  CITYCODE IndustryCode) vce(robust)
est store q3
reghdfe  lnhumans DT Scale ROE AGE RDS ROA CIR TobinQ if State==0,a(code year  CITYCODE IndustryCode) vce(robust)
est store q4
reghdfe  lnhumans DT Scale ROE AGE RDS ROA CIR TobinQ if 营商环境==1,a(code year  CITYCODE IndustryCode) vce(robust)
est store q5
reghdfe  lnhumans DT Scale ROE AGE RDS ROA CIR TobinQ if 营商环境==0,a(code year  CITYCODE IndustryCode) vce(robust)
est store q6
esttab q1 q2 q3 q4 q5 q6   using tab8.rtf,star(* 0.10 ** 0.05 *** 0.01) r2(4) ar2(4) t(3) b(%9.4f) replace nogap


//1.描述性统计
logout, save(miaoshutongji2) word replace: tabstat lnl lnhumans lnrdp lnManagerNumber DT Scale ROE AGE RDS ROA CIR TobinQ Manage Sales opacf,stat(mean sd  min max p50 N) format(%6.2f) c(s)
outreg2 using miao.doc,replace sum(log) title(Decriptive statistics)
outreg2 using miaoshu1.doc, replace sum(log) keep(lnl DF Scale ROE AGE RDP ROA CIR TobinQ)
local x "lnl DF Scale ROE AGE RDP ROA CIR TobinQ"
	   tabstat `x', s(n mean sd min p25 p50 p75 max) ///
	                format(%6.3f) c(s)	  
//2.检验多重共线性
reg lnhumans DT Scale ROE AGE RDS ROA CIR TobinQ
estat vif
//3.相关系数检验
xtserial lnhumans DT Scale ROE AGE RDS ROA CIR TobinQ Manage Sales opacf
local v  "lnhumans DT Scale ROE AGE RDS ROA CIR TobinQ Manage Sales opacf"
local s  "tables1 "
logout, save("`s'") word replace: ///
    pwcorr_a `v'
//4.hausman
xtreg lnhumans DT Scale ROE AGE RDS ROA CIR TobinQ,fe
estimates store fe
xtreg lnhumans DT Scale ROE AGE RDS ROA CIR TobinQ,re
estimates store re
hausman fe re
//5.样本缩尾处理
winsor2 lnlabor DT Scale ROE AGE RDS ROA CIR TobinQ, replace cuts(1 99) 
trim

