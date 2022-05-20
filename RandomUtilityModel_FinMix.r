# ===========================================================================================================================================
# Finite Mixture Models
# ===========================================================================================================================================

# Calculate start value from aggregate estimation
rm(list=ls()) # Clear the memory
setwd(paste("C:/Users/",Sys.getenv("USERNAME"),"/Seafile/Research/Beh Econ of AI/Data/Binary", sep=""))
source("99_estim_functions_aggregate_individual.r") # Load the required functions for performing the estimations
set.seed(111)
data <- read.table("decisions.csv",  sep=",", header=T) # Load choice data
pe.exp1 <- f.estim(data, "choice_x", c("s_x","r_x","q","v"), c("s_y","r_y","q","v"), "self_x", "other_x", "self_y", "other_y", silent=F, comp.se=T) # Aggregate estimation
write.table(pe.exp1$out[,1],"StartValues_FinMix.csv",col.names=F, row.names=F) # Save point estimates as start values for finite mixture model


# Perform finite mixture estimation on whole dataset
rm(list=ls()) # Clear the memory
source("BruhinFehrSchunk/99_estim_functions_finmix.r") # Load the required functions for performing the estimations
set.seed(111)
data <- read.table("decisions.csv",  sep=",", header=T) # Load choice data
data$sid <- as.numeric(as.factor(data$sid)) + 100000
pe1 <- f.estim(data, "StartValues_FinMix.csv", "sid", nc=3, "choice_x", c("s_x","r_x","q","v"), c("s_y","r_y","q","v"), "self_x", "other_x", "self_y", "other_y", saem=T, loglik1=-10805.2505, maxemiter=30, diffcrit=1e-3,sortrow=1)
rse1 <- f.clusterse(pe1, data, "sid", "choice_x", c("s_x","r_x","q","v"), c("s_y","r_y","q","v"), "self_x", "other_x", "self_y", "other_y", 3, sortrow=1)

assignment <- as.data.frame(pe1$tau)
assignment$sid <- rownames(assignment)

data_merged <- merge(data, assignment, by = "sid")
rm(assignment)
data_merged <- data_merged[data_merged$order==1,]
data_merged$max <- pmax(data_merged$comp1, data_merged$comp2, data_merged$comp3)
data_merged$group <- ifelse(data_merged$max==data_merged$comp1, 1, ifelse(data_merged$max==data_merged$comp2, 2, 3))
table(data_merged$group)/nrow(data_merged)
data_merged$treatment <- ifelse(data_merged$treatment=="human", "1 Fellow Human", data_merged$treatment)
data_merged$treatment <- ifelse(data_merged$treatment=="programmer", "2 Programmer", data_merged$treatment)
data_merged$treatment <- ifelse(data_merged$treatment=="token", "3 Human Behind Machine", data_merged$treatment)
data_merged$treatment <- ifelse(data_merged$treatment=="machine earns", "4 Machine Earns", data_merged$treatment)
data_merged$treatment <- ifelse(data_merged$treatment=="burned", "5 Nobody Earns", data_merged$treatment)
data_merged$treatment <- ifelse(data_merged$treatment=="no info", "6 No Info", data_merged$treatment)
table(data_merged$treatment, data_merged$group)/150*100
