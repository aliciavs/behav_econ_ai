*xxx
clear
cd "C:/Users/`=c(username)'/Seafile/Research/Beh Econ of AI/Data"

*Dictator
use DataDictatorHuman.dta, clear
append using "DataDictatorRobot.dta"
keep if role == "dictator"

tab treatment kept, row
table treatment, contents(mean kept)

ranksum kept if treatment == "human" | treatment == "burned", by(treatment)
ranksum kept if treatment == "human" | treatment == "no info", by(treatment)
ranksum kept if treatment == "no info" | treatment == "programmer", by(treatment)
ranksum kept if treatment == "machine earns" | treatment == "programmer", by(treatment)
ranksum kept if treatment == "burned" | treatment == "programmer", by(treatment)

collapse (mean) mean_y = kept (semean) se_y = kept, by(treatment)
gen x = 1 if treatment == "human"
replace x = 2 if treatment == "programmer"
replace x = 3 if treatment == "no info"
replace x = 4 if treatment == "machine earns"
replace x = 5 if treatment == "burned"
sort x
label define x 1 "human" 2 "programmer" 3 "no info" 4 "machine earns" 5 "burned"
label value x x

gen y_u = mean_y + 1.96*se_y 
gen y_l = mean_y - 1.96*se_y
replace y_u = 10 if y_u > 10
replace y_l = 0 if y_l < 0

twoway (scatter mean_y x) (rcap y_u y_l x), xlabel(1 2 3 4 5, valuelabel) title("Dictator game") legend(off) xtitle("") ytitle("Amount kept") name(kept, replace) nodraw


*Trust First
use DataTrustHuman.dta, clear
append using "DataTrustRobotSecond.dta"
keep if role == "first"

tab treatment sent_amount, row
table treatment, contents(mean sent_amount)

ranksum sent_amount if treatment == "burned" | treatment == "programmer", by(treatment)
ranksum sent_amount if treatment == "human" | treatment == "machine earns", by(treatment)

bys sent_amount: table treatment, contents(count send_back_belief mean send_back_belief)

collapse (mean) mean_y = sent_amount (semean) se_y = sent_amount, by(treatment)
gen x = 1 if treatment == "human"
replace x = 2 if treatment == "token"
replace x = 3 if treatment == "programmer"
replace x = 4 if treatment == "no info"
replace x = 5 if treatment == "machine earns"
replace x = 6 if treatment == "burned"
sort x
label define x 1 "human" 2 "token" 3 "programmer" 4 "no info" 5 "machine earns" 6 "burned"
label value x x

gen y_u = mean_y + 1.96*se_y 
gen y_l = mean_y - 1.96*se_y
replace y_u = 5 if y_u > 5
replace y_l = 0 if y_l < 0

twoway (scatter mean_y x) (rcap y_u y_l x), xlabel(1 2 3 4 5 6, valuelabel) title("Trust game - first mover") legend(off) xtitle("") ytitle("Amount sent") name(sent, replace) nodraw


*Trust Second
use DataTrustHuman.dta, clear
append using "DataTrustRobotFirst.dta"
keep if role == "second"

table treatment, contents(mean sent_back0 mean sent_back1 mean sent_back2)
table treatment, contents(mean sent_back3 mean sent_back4 mean sent_back5)

ranksum sent_back5 if treatment == "human" | treatment == "burned", by(treatment)
ranksum sent_back5 if treatment == "human" | treatment == "machine earns", by(treatment)
ranksum sent_back5 if treatment == "human" | treatment == "token", by(treatment)
ranksum sent_back5 if treatment == "no info" | treatment == "token", by(treatment)
ranksum sent_back5 if treatment == "machine earns" | treatment == "token", by(treatment)

collapse (mean) mean_y0 = sent_back0 (semean) se_y0 = sent_back0 (mean) mean_y1 = sent_back1 (semean) se_y1 = sent_back1 (mean) mean_y2 = sent_back2 (semean) se_y2 = sent_back2 (mean) mean_y3 = sent_back3 (semean) se_y3 = sent_back3 (mean) mean_y4 = sent_back4 (semean) se_y4 = sent_back4 (mean) mean_y5 = sent_back5 (semean) se_y5 = sent_back5, by(treatment)
gen x = 1 if treatment == "human"
replace x = 2 if treatment == "token"
replace x = 3 if treatment == "programmer"
replace x = 4 if treatment == "no info"
replace x = 5 if treatment == "machine earns"
replace x = 6 if treatment == "burned"
sort x
label define x 1 "human" 2 "token" 3 "programmer" 4 "no info" 5 "machine earns" 6 "burned"
label value x x

gen y_u0 = mean_y0 + 1.96*se_y0
gen y_l0 = mean_y0 - 1.96*se_y0
replace y_u0 = 5 if y_u0 > 5
replace y_l0 = 0 if y_l0 < 0
gen y_u1 = mean_y1 + 1.96*se_y1
gen y_l1 = mean_y1 - 1.96*se_y1
replace y_u1 = 8 if y_u1 > 8
replace y_l1 = 0 if y_l1 < 0
gen y_u2 = mean_y2 + 1.96*se_y2
gen y_l2 = mean_y2 - 1.96*se_y2
replace y_u2 = 11 if y_u2 > 11
replace y_l2 = 0 if y_l2 < 0
gen y_u3 = mean_y3 + 1.96*se_y3
gen y_l3 = mean_y3 - 1.96*se_y3
replace y_u3 = 14 if y_u3 > 14
replace y_l3 = 0 if y_l3 < 0
gen y_u4 = mean_y4 + 1.96*se_y4
gen y_l4 = mean_y4 - 1.96*se_y4
replace y_u4 = 17 if y_u4 > 17
replace y_l4 = 0 if y_l4 < 0
gen y_u5 = mean_y5 + 1.96*se_y5
gen y_l5 = mean_y5 - 1.96*se_y5
replace y_u5 = 20 if y_u5 > 20
replace y_l5 = 0 if y_l5 < 0

twoway (scatter mean_y0 x) (rcap y_u0 y_l0 x), xlabel(1 2 3 4 5 6, valuelabel) title("Trust game - second mover (0 sent)") legend(off) xtitle("") ytitle("Amount sent back") nodraw name(sent0, replace)
twoway (scatter mean_y1 x) (rcap y_u1 y_l1 x), xlabel(1 2 3 4 5 6, valuelabel) title("Trust game - second mover (1 sent)") legend(off) xtitle("") ytitle("Amount sent back") nodraw name(sent1, replace)
twoway (scatter mean_y2 x) (rcap y_u2 y_l2 x), xlabel(1 2 3 4 5 6, valuelabel) title("Trust game - second mover (2 sent)") legend(off) xtitle("") ytitle("Amount sent back") nodraw name(sent2, replace)
twoway (scatter mean_y3 x) (rcap y_u3 y_l3 x), xlabel(1 2 3 4 5 6, valuelabel) title("Trust game - second mover (3 sent)") legend(off) xtitle("") ytitle("Amount sent back") nodraw name(sent3, replace)
twoway (scatter mean_y4 x) (rcap y_u4 y_l4 x), xlabel(1 2 3 4 5 6, valuelabel) title("Trust game - second mover (4 sent)") legend(off) xtitle("") ytitle("Amount sent back") nodraw name(sent4, replace)
twoway (scatter mean_y5 x) (rcap y_u5 y_l5 x), xlabel(1 2 3 4 5 6, valuelabel) title("Trust game - second mover (5 sent)") legend(off) xtitle("") ytitle("Amount sent back") nodraw name(sent5, replace)
*graph combine sent0 sent1 sent2 sent3 sent4 sent5, xsize(10)


*Ultimatum Proposer
use DataUltimatumHuman.dta, clear
append using "DataUltimatumRobotResponder.dta"
keep if role == "proposer"

tab treatment offer, row
table treatment, contents(mean offer)

ranksum offer if treatment == "token" | treatment == "machine earns", by(treatment)
ranksum offer if treatment == "token" | treatment == "burned", by(treatment)

tab treatment belief_minimum, row
table treatment, contents(mean belief_minimum)

collapse (mean) mean_y = offer (semean) se_y = offer, by(treatment)
gen x = 1 if treatment == "human"
replace x = 2 if treatment == "token"
replace x = 3 if treatment == "programmer"
replace x = 4 if treatment == "no info"
replace x = 5 if treatment == "machine earns"
replace x = 6 if treatment == "burned"
sort x
label define x 1 "human" 2 "token" 3 "programmer" 4 "no info" 5 "machine earns" 6 "burned"
label value x x

gen y_u = mean_y + 1.96*se_y 
gen y_l = mean_y - 1.96*se_y
replace y_u = 10 if y_u > 10
replace y_l = 0 if y_l < 0

twoway (scatter mean_y x) (rcap y_u y_l x), xlabel(1 2 3 4 5 6, valuelabel) title("Ultimatum game - Proposer") legend(off) xtitle("") ytitle("Offer") name(offer, replace) nodraw


*Ultimatum Responder
use DataUltimatumHuman.dta, clear
append using "DataUltimatumRobotProposer.dta"
keep if role == "responder"

tab treatment accept_minimum, row
table treatment, contents(mean accept_minimum)

collapse (mean) mean_y = accept_minimum (semean) se_y = accept_minimum, by(treatment)
gen x = 1 if treatment == "human"
replace x = 2 if treatment == "token"
replace x = 3 if treatment == "programmer"
replace x = 4 if treatment == "no info"
replace x = 5 if treatment == "machine earns"
replace x = 6 if treatment == "burned"
sort x
label define x 1 "human" 2 "token" 3 "programmer" 4 "no info" 5 "machine earns" 6 "burned"
label value x x

gen y_u = mean_y + 1.96*se_y 
gen y_l = mean_y - 1.96*se_y
replace y_u = 10 if y_u > 10
replace y_l = 0 if y_l < 0

twoway (scatter mean_y x) (rcap y_u y_l x), xlabel(1 2 3 4 5 6, valuelabel) title("Ultimatum game - Responder") legend(off) xtitle("") ytitle("Minimum acceptable amount") name(minimum, replace) nodraw


*Overview graphs
graph combine kept sent sent5 offer minimum, xsize(10) name(overview, replace)
*graph combine kept sent sent0 sent1 sent2 sent3 sent4 sent5 offer minimum, xsize(10) name(overview_complete, replace) 


*Algorithm accuracy
use DataTrustRobotSecond.dta, clear
append using "DataTrustRobotFirst.dta"
append using "DataUltimatumRobotResponder.dta"
append using "DataUltimatumRobotProposer.dta"

gen game = "Trust Second" if send_back_belief != .
replace game = "Trust First" if sent_back0 != .
replace game = "Ultimatum Responder" if belief_minimum != .
replace game = "Ultimatum Proposer" if game == ""

table game, contents(mean rmse)

gen nrmse = rmse
replace nrmse = rmse*2 if game == "Trust First"
replace nrmse = rmse*10/(5 + sent_amount*3) if game == "Trust Second"

table game, contents(mean nrmse)
