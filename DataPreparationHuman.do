xxx
clear
cd "C:/Users/`=c(username)'/Seafile/Research/Beh Econ of AI/Data"


*Dictator Human
import delimited "dictator_human.csv", clear 

keep if sessioncode == "qd0ydl2t"
keep if participant_index_in_pages == 24
keep if groupid_in_subsession > 1

drop participant_is_bot participant_index_in_pages participant_max_page_index participant_current_app_name participant_current_page_name participanttime_started_utc participantvisited participantmturk_worker_id participantmturk_assignment_id sessionlabel sessionmturk_hitid sessionmturk_hitgroupid sessioncomment sessionis_demo participantpayoff playerrole subsessionround_number
renpfix participant
renpfix player
renpfix group
rename ïparticipantid_in_session id_in_session
rename id_in_subsession group_id
replace group_id = group_id-1
drop if label == ""
sort group_id id_in_group

gen role = "dictator" if id_in_group == 1
replace role = "receiver" if id_in_group == 2
gen treatment = "human"

save "DataDictatorHuman.dta", replace


*Trust Human
import delimited "trust_human_strategy.csv", clear 

keep if sessioncode == "qd0ydl2t"
keep if participant_index_in_pages == 24
keep if groupid_in_subsession > 1

drop participant_is_bot participant_index_in_pages participant_max_page_index participant_current_app_name participant_current_page_name participanttime_started_utc participantvisited participantmturk_worker_id participantmturk_assignment_id sessionlabel sessionmturk_hitid sessionmturk_hitgroupid sessioncomment sessionis_demo participantpayoff playerrole subsessionround_number grouptreatment
renpfix participant
renpfix player
renpfix group
rename ïparticipantid_in_session id_in_session
rename id_in_subsession group_id
replace group_id = group_id-1
drop if label == ""
sort group_id id_in_group

gen role = "first" if id_in_group == 1
replace role = "second" if id_in_group == 2
gen treatment = "human"
bys group_id: replace sent_back0 = sent_back0[_n+1] if id_in_group == 1
bys group_id: replace sent_back1 = sent_back1[_n+1] if id_in_group == 1
bys group_id: replace sent_back2 = sent_back2[_n+1] if id_in_group == 1
bys group_id: replace sent_back3 = sent_back3[_n+1] if id_in_group == 1
bys group_id: replace sent_back4 = sent_back4[_n+1] if id_in_group == 1
bys group_id: replace sent_back5 = sent_back5[_n+1] if id_in_group == 1

save "DataTrustHuman.dta", replace


*Ultimatum Human
import delimited "ultimatum_human_strategy.csv", clear 

keep if sessioncode == "qd0ydl2t"
keep if participant_index_in_pages == 24
keep if groupid_in_subsession > 1

drop participant_is_bot participant_index_in_pages participant_max_page_index participant_current_app_name participant_current_page_name participanttime_started_utc participantvisited participantmturk_worker_id participantmturk_assignment_id sessionlabel sessionmturk_hitid sessionmturk_hitgroupid sessioncomment sessionis_demo participantpayoff playerrole subsessionround_number grouptreatment
renpfix participant
renpfix player
renpfix group
rename ïparticipantid_in_session id_in_session
rename id_in_subsession group_id
replace group_id = group_id-1
drop if label == ""
sort group_id id_in_group

gen role = "proposer" if id_in_group == 1
replace role = "responder" if id_in_group == 2
gen treatment = "human"

save "DataUltimatumHuman.dta", replace
