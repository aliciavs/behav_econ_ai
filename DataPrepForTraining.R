library(fastDummies)
setwd(paste("C:/Users/",Sys.getenv("USERNAME"),"/Seafile/Research/Beh Econ of AI/Data", sep=""))
rm(list=ls()) # Clear the memory

#load and clean survey
data_survey <- read.csv("survey.csv")
data_survey <- data_survey[data_survey$participant._index_in_pages == 24,]
data_survey <- data_survey[,c(3,18:24)]
names(data_survey) <- sub('^player.', '', names(data_survey))
data_survey <- data_survey[,c(1,2,4,7,8,3,5,6)]
data_survey <- dummy_cols(data_survey, remove_selected_columns = TRUE, select_columns=c("gender", "field_of_study","employment"))


#load trust data and keep only relevant observations
data_trust <- read.csv("trust_human_strategy.csv")
data_trust <- data_trust[data_trust$participant._index_in_pages == 24,]
data_trust <- data_trust[data_trust$group.id_in_subsession > 1,]

#first mover
data_trust_first <- data_trust[data_trust$player.id_in_group == 1,]
data_trust_first <- data_trust_first[,c(3,25)]
data_trust_first <- merge(data_survey, data_trust_first, by="participant.label")
data_trust_first <- data_trust_first[,-1]
names(data_trust_first)[21] <- "sent_amount"
write.table(data_trust_first, file = "Upload/data_trust_first.csv", row.names = FALSE, sep = ",")

#second mover
data_trust_second <- data_trust[data_trust$player.id_in_group == 2,]
data_trust_second <- data_trust_second[,c(3,17:22)]
data_trust_second <- merge(data_survey, data_trust_second, by="participant.label")
data_trust_second <- data_trust_second[,-1]
names(data_trust_second) <- sub('^player.', '', names(data_trust_second))
write.table(data_trust_second, file = "Upload/data_trust_second.csv", row.names = FALSE, sep = ",")


#load ultimatum data and keep only relevant observations
data_ultimatum <- read.csv("ultimatum_human_strategy.csv")
data_ultimatum <- data_ultimatum[data_ultimatum$participant._index_in_pages == 24,]
data_ultimatum <- data_ultimatum[data_ultimatum$group.id_in_subsession > 1,]

#proposer
data_ultimatum_prop <- data_ultimatum[data_ultimatum$player.id_in_group == 1,]
data_ultimatum_prop <- data_ultimatum_prop[,c(3,19)]
data_ultimatum_prop <- merge(data_survey, data_ultimatum_prop, by="participant.label")
data_ultimatum_prop <- data_ultimatum_prop[,-1]
names(data_ultimatum_prop)[21] <- "offer"
write.table(data_ultimatum_prop, file = "Upload/data_ult_prop.csv", row.names = FALSE, sep = ",")

#receiver
data_ultimatum_rec <- data_ultimatum[data_ultimatum$player.id_in_group == 2,]
data_ultimatum_rec <- data_ultimatum_rec[,c(3,21)]
data_ultimatum_rec <- merge(data_survey, data_ultimatum_rec, by="participant.label")
data_ultimatum_rec <- data_ultimatum_rec[,-1]
names(data_ultimatum_rec)[21] <- "accept_minimum"
write.table(data_ultimatum_rec, file = "Upload/data_ult_rec.csv", row.names = FALSE, sep = ",")





