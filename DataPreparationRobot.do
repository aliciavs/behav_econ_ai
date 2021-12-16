*xxx
clear
cd "C:/Users/`=c(username)'/Seafile/Research/Beh Econ of AI/Data"


*Dictator Robot
import delimited "dictator_robot.csv", clear 

keep if sessioncode == "al9gerxp" | sessioncode == "edqsn7rj" | sessioncode == "xsp8cw0b"
keep if participant_index_in_pages == 24
keep if playertreatment != ""

drop participant_is_bot participant_index_in_pages participant_max_page_index participant_current_app_name participant_current_page_name participanttime_started_utc participantvisited participantmturk_worker_id participantmturk_assignment_id sessionlabel sessionmturk_hitid sessionmturk_hitgroupid sessioncomment sessionis_demo participantpayoff playerrole subsessionround_number
renpfix participant
renpfix player
renpfix group
rename ïparticipantid_in_session id_in_session
rename id_in_subsession group_id
replace group_id = group_id-1
drop if label == ""
replace id_in_group = 0

gen role = "dictator"

save "DataDictatorRobot.dta", replace


*Trust Robot First Mover
import delimited "trust_robot_firstmover_strategy.csv", clear 

keep if sessioncode == "al9gerxp" | sessioncode == "edqsn7rj" | sessioncode == "xsp8cw0b"
keep if participant_index_in_pages == 24
keep if playertreatment != ""

drop participant_is_bot participant_index_in_pages participant_max_page_index participant_current_app_name participant_current_page_name participanttime_started_utc participantvisited participantmturk_worker_id participantmturk_assignment_id sessionlabel sessionmturk_hitid sessionmturk_hitgroupid sessioncomment sessionis_demo participantpayoff playerrole subsessionround_number
renpfix participant
renpfix player
renpfix group
rename ïparticipantid_in_session id_in_session
rename id_in_subsession group_id
replace group_id = group_id-1
drop if label == ""
replace id_in_group = 0

gen role = "second"

save "DataTrustRobotFirst.dta", replace


*Trust Robot Second Mover
import delimited "trust_robot_secondmover.csv", clear 

keep if sessioncode == "al9gerxp" | sessioncode == "edqsn7rj" | sessioncode == "xsp8cw0b"
keep if participant_index_in_pages == 24
keep if playertreatment != ""

drop participant_is_bot participant_index_in_pages participant_max_page_index participant_current_app_name participant_current_page_name participanttime_started_utc participantvisited participantmturk_worker_id participantmturk_assignment_id sessionlabel sessionmturk_hitid sessionmturk_hitgroupid sessioncomment sessionis_demo participantpayoff playerrole subsessionround_number
renpfix participant
renpfix player
renpfix group
rename ïparticipantid_in_session id_in_session
rename id_in_subsession group_id
replace group_id = group_id-1
drop if label == ""
replace id_in_group = 0

gen role = "first"

save "DataTrustRobotSecond.dta", replace


*Ultimatum Robot Proposer
import delimited "ultimatum_robot_proposer_strategy.csv", clear 

keep if sessioncode == "al9gerxp" | sessioncode == "edqsn7rj" | sessioncode == "xsp8cw0b"
keep if participant_index_in_pages == 24
keep if playertreatment != ""

drop participant_is_bot participant_index_in_pages participant_max_page_index participant_current_app_name participant_current_page_name participanttime_started_utc participantvisited participantmturk_worker_id participantmturk_assignment_id sessionlabel sessionmturk_hitid sessionmturk_hitgroupid sessioncomment sessionis_demo participantpayoff playerrole subsessionround_number
renpfix participant
renpfix player
renpfix group
rename ïparticipantid_in_session id_in_session
rename id_in_subsession group_id
replace group_id = group_id-1
drop if label == ""
replace id_in_group = 0

gen role = "responder"

save "DataUltimatumRobotProposer.dta", replace


*Ultimatum Robot Responder
import delimited "ultimatum_robot_responder_strategy.csv", clear 

keep if sessioncode == "al9gerxp" | sessioncode == "edqsn7rj" | sessioncode == "xsp8cw0b"
keep if participant_index_in_pages == 24
keep if playertreatment != ""

drop participant_is_bot participant_index_in_pages participant_max_page_index participant_current_app_name participant_current_page_name participanttime_started_utc participantvisited participantmturk_worker_id participantmturk_assignment_id sessionlabel sessionmturk_hitid sessionmturk_hitgroupid sessioncomment sessionis_demo participantpayoff playerrole subsessionround_number
renpfix participant
renpfix player
renpfix group
rename ïparticipantid_in_session id_in_session
rename id_in_subsession group_id
replace group_id = group_id-1
drop if label == ""
replace id_in_group = 0

gen role = "proposer"

save "DataUltimatumRobotResponder.dta", replace

