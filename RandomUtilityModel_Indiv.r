# ===========================================================================================================================================
# Individual results
# ===========================================================================================================================================

library(ggplot2)
library(ggpubr)
library(ggstatsplot)
library(dplyr)
rm(list=ls()) # Clear the memory
setwd(paste("C:/Users/",Sys.getenv("USERNAME"),"/Seafile/Research/Beh Econ of AI/Data/Binary", sep=""))
source("99_estim_functions_aggregate_individual.r") # Load the required functions for performing the estimations
set.seed(51)

data <- read.table("decisions.csv",  sep=",", header=T) # Load choice data
data_hd <- data[data$treatment=="human" & data$game=="dictator",]
data_ht <- data[data$treatment=="human" & data$game=="trust",]

data_rd_prog <- data[data$treatment=="programmer" & data$game=="dictator",]
data_rd_toke <- data[data$treatment=="token" & data$game=="dictator",]
data_rd_mach <- data[data$treatment=="machine earns" & data$game=="dictator",]
data_rd_burn <- data[data$treatment=="burned" & data$game=="dictator",]
data_rd_noin <- data[data$treatment=="no info" & data$game=="dictator",]

data_rt_prog <- data[data$treatment=="programmer" & data$game=="trust",]
data_rt_toke <- data[data$treatment=="token" & data$game=="trust",]
data_rt_mach <- data[data$treatment=="machine earns" & data$game=="trust",]
data_rt_burn <- data[data$treatment=="burned" & data$game=="trust",]
data_rt_noin <- data[data$treatment=="no info" & data$game=="trust",]


# Estimate on individual Data
# NOTE: This takes very long, load results for quicker access below
iest_hd <- as.data.frame(f.indivest_dict(data_hd))
iest_ht <- as.data.frame(f.indivest(data_ht))

iest_rd_prog <- as.data.frame(f.indivest_dict(data_rd_prog))
iest_rd_toke <- as.data.frame(f.indivest_dict(data_rd_toke))
iest_rd_mach <- as.data.frame(f.indivest_dict(data_rd_mach))
iest_rd_burn <- as.data.frame(f.indivest_dict(data_rd_burn))
iest_rd_noin <- as.data.frame(f.indivest_dict(data_rd_noin))

iest_rt_prog <- as.data.frame(f.indivest(data_rt_prog))
iest_rt_toke <- as.data.frame(f.indivest(data_rt_toke))
iest_rt_mach <- as.data.frame(f.indivest(data_rt_mach))
iest_rt_burn <- as.data.frame(f.indivest(data_rt_burn))
iest_rt_noin <- as.data.frame(f.indivest(data_rt_noin))


load("IndivEstim.RData")

iest_h_all <- rbind(iest_hd, iest_ht[,-c(3,4)])
iest_h_all$incons <- (iest_h_all$alpha > 0 & iest_h_all$beta < 0) | iest_h_all$alpha > 1  | iest_h_all$alpha < -1  | iest_h_all$beta > 1  | iest_h_all$beta < -1
table(iest_h_all$incons)

iest_r_prog_all <- rbind(iest_rd_prog, iest_rt_prog[,-c(3,4)])
iest_r_prog_all$incons <- (iest_r_prog_all$alpha > 0 & iest_r_prog_all$beta < 0) | iest_r_prog_all$alpha > 1  | iest_r_prog_all$alpha < -1  | iest_r_prog_all$beta > 1  | iest_r_prog_all$beta < -1
table(iest_r_prog_all$incons)

iest_r_toke_all <- rbind(iest_rd_toke, iest_rt_toke[,-c(3,4)])
iest_r_toke_all$incons <- (iest_r_toke_all$alpha > 0 & iest_r_toke_all$beta < 0) | iest_r_toke_all$alpha > 1  | iest_r_toke_all$alpha < -1  | iest_r_toke_all$beta > 1  | iest_r_toke_all$beta < -1
table(iest_r_toke_all$incons)

iest_r_mach_all <- rbind(iest_rd_mach, iest_rt_mach[,-c(3,4)])
iest_r_mach_all$incons <- (iest_r_mach_all$alpha > 0 & iest_r_mach_all$beta < 0) | iest_r_mach_all$alpha > 1  | iest_r_mach_all$alpha < -1  | iest_r_mach_all$beta > 1  | iest_r_mach_all$beta < -1
table(iest_r_mach_all$incons)

iest_r_burn_all <- rbind(iest_rd_burn, iest_rt_burn[,-c(3,4)])
iest_r_burn_all$incons <- (iest_r_burn_all$alpha > 0 & iest_r_burn_all$beta < 0) | iest_r_burn_all$alpha > 1  | iest_r_burn_all$alpha < -1  | iest_r_burn_all$beta > 1  | iest_r_burn_all$beta < -1
table(iest_r_burn_all$incons)

iest_r_noin_all <- rbind(iest_rd_noin, iest_rt_noin[,-c(3,4)])
iest_r_noin_all$incons <- (iest_r_noin_all$alpha > 0 & iest_r_noin_all$beta < 0) | iest_r_noin_all$alpha > 1  | iest_r_noin_all$alpha < -1  | iest_r_noin_all$beta > 1  | iest_r_noin_all$beta < -1
table(iest_r_noin_all$incons)


iest_ht$incons <- (iest_ht$alpha > 0 & iest_ht$beta < 0) | iest_ht$alpha > 1  | iest_ht$alpha < -1  | iest_ht$beta > 1  | iest_ht$beta < -1 | iest_ht$gamma > 1 | iest_ht$gamma < -1 | iest_ht$delta > 1 | iest_ht$delta < -1 
table(iest_ht$incons)

iest_rt_prog$incons <- (iest_rt_prog$alpha > 0 & iest_rt_prog$beta < 0) | iest_rt_prog$alpha > 1  | iest_rt_prog$alpha < -1  | iest_rt_prog$beta > 1  | iest_rt_prog$beta < -1 | iest_rt_prog$gamma > 1 | iest_rt_prog$gamma < -1 | iest_rt_prog$delta > 1 | iest_rt_prog$delta < -1 
table(iest_rt_prog$incons)

iest_rt_toke$incons <- (iest_rt_toke$alpha > 0 & iest_rt_toke$beta < 0) | iest_rt_toke$alpha > 1  | iest_rt_toke$alpha < -1  | iest_rt_toke$beta > 1  | iest_rt_toke$beta < -1 | iest_rt_toke$gamma > 1 | iest_rt_toke$gamma < -1 | iest_rt_toke$delta > 1 | iest_rt_toke$delta < -1 
table(iest_rt_toke$incons)

iest_rt_mach$incons <- (iest_rt_mach$alpha > 0 & iest_rt_mach$beta < 0) | iest_rt_mach$alpha > 1  | iest_rt_mach$alpha < -1  | iest_rt_mach$beta > 1  | iest_rt_mach$beta < -1 | iest_rt_mach$gamma > 1 | iest_rt_mach$gamma < -1 | iest_rt_mach$delta > 1 | iest_rt_mach$delta < -1 
table(iest_rt_mach$incons)

iest_rt_burn$incons <- (iest_rt_burn$alpha > 0 & iest_rt_burn$beta < 0) | iest_rt_burn$alpha > 1  | iest_rt_burn$alpha < -1  | iest_rt_burn$beta > 1  | iest_rt_burn$beta < -1 | iest_rt_burn$gamma > 1 | iest_rt_burn$gamma < -1 | iest_rt_burn$delta > 1 | iest_rt_burn$delta < -1 
table(iest_rt_burn$incons)

iest_rt_noin$incons <- (iest_rt_noin$alpha > 0 & iest_rt_noin$beta < 0) | iest_rt_noin$alpha > 1  | iest_rt_noin$alpha < -1  | iest_rt_noin$beta > 1  | iest_rt_noin$beta < -1 | iest_rt_noin$gamma > 1 | iest_rt_noin$gamma < -1 | iest_rt_noin$delta > 1 | iest_rt_noin$delta < -1 
table(iest_rt_noin$incons)


iest_h_all$treatment <- "Fellow Human"
iest_r_prog_all$treatment <- "Programmer"
iest_r_toke_all$treatment <- "Human behind Machine"
iest_r_mach_all$treatment <- "Machine Earns"
iest_r_burn_all$treatment <- "Nobody Earns"
iest_r_noin_all$treatment <- "No Info"

iest_all <- rbind(iest_h_all, iest_r_prog_all, iest_r_toke_all, iest_r_mach_all, iest_r_burn_all, iest_r_noin_all)
iest_all <- iest_all[!iest_all$incons,-6]


ggplot(iest_h_all[!iest_h_all$incons,], aes(x=alpha, y=beta)) + 
  geom_point(colour="blue", size=3, position = position_jitter(height = 0.05, width = 0.05, seed = 42)) + 
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0) + 
  xlim(-1.05,1.05) + 
  ylim(-1.05,1.05) +
  ggtitle("Fellow Human")


ab_h_prog <- ggplot(iest_h_all[!iest_h_all$incons,], aes(x=alpha, y=beta)) + 
  geom_point(colour="blue", size=3, position = position_jitter(height = 0.05, width = 0.05, seed = 42)) + 
  geom_point(colour="red", size=3, data=iest_r_prog_all[!iest_r_prog_all$incons,], shape=15, position = position_jitter(height = 0.05, width = 0.05, seed = 42)) + 
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0) + 
  xlim(-1.05,0.55) + 
  ylim(-0.55,1.05) +
  ggtitle("Fellow Human vs. Programmer")

ab_h_toke <- ggplot(iest_h_all[!iest_h_all$incons,], aes(x=alpha, y=beta)) + 
  geom_point(colour="blue", size=3, position = position_jitter(height = 0.05, width = 0.05, seed = 42)) + 
  geom_point(colour="red", size=3, data=iest_r_toke_all[!iest_r_toke_all$incons,], shape=15, position = position_jitter(height = 0.05, width = 0.05, seed = 42)) + 
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0) + 
  xlim(-1.05,0.55) + 
  ylim(-0.55,1.05) +
  ggtitle("Fellow Human vs. Human behind Machine")

ab_h_mach <- ggplot(iest_h_all[!iest_h_all$incons,], aes(x=alpha, y=beta)) + 
  geom_point(colour="blue", size=3, position = position_jitter(height = 0.05, width = 0.05, seed = 42)) + 
  geom_point(colour="red", size=3, data=iest_r_mach_all[!iest_r_mach_all$incons,], shape=15, position = position_jitter(height = 0.05, width = 0.05, seed = 42)) + 
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0) + 
  xlim(-1.05,0.55) + 
  ylim(-0.55,1.05) +
  ggtitle("Fellow Human vs. Machine Earns")

ab_h_burn <- ggplot(iest_h_all[!iest_h_all$incons,], aes(x=alpha, y=beta)) + 
  geom_point(colour="blue", size=3, position = position_jitter(height = 0.05, width = 0.05, seed = 42)) + 
  geom_point(colour="red", size=3, data=iest_r_burn_all[!iest_r_burn_all$incons,], shape=15, position = position_jitter(height = 0.05, width = 0.05, seed = 42)) + 
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0) + 
  xlim(-1.05,0.55) + 
  ylim(-0.55,1.05) +
  ggtitle("Fellow Human vs. Nobody Earns")

ab_h_noin <- ggplot(iest_h_all[!iest_h_all$incons,], aes(x=alpha, y=beta)) + 
  geom_point(colour="blue", size=3, position = position_jitter(height = 0.05, width = 0.05, seed = 42)) +  
  geom_point(colour="red", size=3, data=iest_r_noin_all[!iest_r_noin_all$incons,], shape=15, position = position_jitter(height = 0.05, width = 0.05, seed = 42)) + 
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0) + 
  xlim(-1.05,0.55) + 
  ylim(-0.55,1.05) +
  ggtitle("Fellow Human vs. No Info")

ggarrange(ab_h_prog, ab_h_toke, ab_h_mach, ab_h_burn, ab_h_noin,
          ncol = 3, nrow = 2)



ggplot(iest_ht, aes(x=gamma, y=delta)) + 
  geom_point(colour="blue", size=3, position = position_jitter(height = 0.05, width = 0.05, seed = 42)) +  
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0) + 
  xlim(-1.05,1.05) + 
  ylim(-1.05,1.05) +
  ggtitle("Fellow Human")


dg_h_prog <- ggplot(iest_ht, aes(x=gamma, y=delta)) + 
  geom_point(colour="blue", size=3, position = position_jitter(height = 0.05, width = 0.05, seed = 42)) +  
  geom_point(colour="red", size=3, data=iest_rt_prog, shape=15, position = position_jitter(height = 0.05, width = 0.05, seed = 42)) +  
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0) + 
  xlim(-1.05,1.05) + 
  ylim(-1.05,1.05) +
  ggtitle("Fellow Human vs. Programmer")

dg_h_toke <- ggplot(iest_ht, aes(x=gamma, y=delta)) + 
  geom_point(colour="blue", size=3, position = position_jitter(height = 0.05, width = 0.05, seed = 42)) +  
  geom_point(colour="red", size=3, data=iest_rt_toke, shape=15, position = position_jitter(height = 0.05, width = 0.05, seed = 42)) +  
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0) + 
  xlim(-1.05,1.05) + 
  ylim(-1.05,1.05) +
  ggtitle("Fellow Human vs. Human behind Machine")

dg_h_mach <- ggplot(iest_ht, aes(x=gamma, y=delta)) + 
  geom_point(colour="blue", size=3, position = position_jitter(height = 0.05, width = 0.05, seed = 42)) +  
  geom_point(colour="red", size=3, data=iest_rt_mach, shape=15, position = position_jitter(height = 0.05, width = 0.05, seed = 42)) +  
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0) + 
  xlim(-1.05,1.05) + 
  ylim(-1.05,1.05) +
  ggtitle("Fellow Human vs. Machine Earns")

dg_h_burn <- ggplot(iest_ht, aes(x=gamma, y=delta)) + 
  geom_point(colour="blue", size=3, position = position_jitter(height = 0.05, width = 0.05, seed = 42)) +  
  geom_point(colour="red", size=3, data=iest_rt_burn, shape=15, position = position_jitter(height = 0.05, width = 0.05, seed = 42)) +  
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0) + 
  xlim(-1.05,1.05) + 
  ylim(-1.05,1.05) +
  ggtitle("Fellow Human vs. Nobody Earns")

dg_h_noin <- ggplot(iest_ht, aes(x=gamma, y=delta)) + 
  geom_point(colour="blue", size=3, position = position_jitter(height = 0.05, width = 0.05, seed = 42)) +  
  geom_point(colour="red", size=3, data=iest_rt_noin, shape=15, position = position_jitter(height = 0.05, width = 0.05, seed = 42)) + 
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0) + 
  xlim(-1.05,1.05) + 
  ylim(-1.05,1.05) +
  ggtitle("Fellow Human vs. No Info")

ggarrange(dg_h_prog, dg_h_toke, dg_h_mach, dg_h_burn, dg_h_noin,
          ncol = 3, nrow = 2)
