# ===========================================================================================================================================
# Aggregate results
# ===========================================================================================================================================

rm(list=ls()) # Clear the memory
setwd(paste("C:/Users/",Sys.getenv("USERNAME"),"/Seafile/Research/Beh Econ of AI/Data/Binary", sep=""))
source("99_estim_functions_aggregate_individual.r") # Load the required functions for performing the estimations
set.seed(111)

data <- read.table("decisions.csv",  sep=",", header=T) # Load choice data
data_h      <- data[data$treatment=="human",]
data_r_prog <- data[data$treatment=="programmer",]
data_r_toke <- data[data$treatment=="token",]
data_r_mach <- data[data$treatment=="machine earns",]
data_r_burn <- data[data$treatment=="burned",]
data_r_noin <- data[data$treatment=="no info",]

pe.exp1 <- f.estim(data_h, "choice_x", c("s_x","r_x","q","v"), c("s_y","r_y","q","v"), "self_x", "other_x", "self_y", "other_y", silent=T, comp.se=T) # Aggregate estimation
res_h <- f.clusterse(pe.exp1, data_h, "choice_x", "self_x", "other_x", "self_y", "other_y", c("s_x","r_x","q","v"), c("s_y","r_y","q","v"), "sid")

pe.exp1 <- f.estim(data_r_prog, "choice_x", c("s_x","r_x","q","v"), c("s_y","r_y","q","v"), "self_x", "other_x", "self_y", "other_y", silent=T, comp.se=T) # Aggregate estimation
res_r_prog <- f.clusterse(pe.exp1, data_r_prog, "choice_x", "self_x", "other_x", "self_y", "other_y", c("s_x","r_x","q","v"), c("s_y","r_y","q","v"), "sid")

pe.exp1 <- f.estim(data_r_toke, "choice_x", c("s_x","r_x","q","v"), c("s_y","r_y","q","v"), "self_x", "other_x", "self_y", "other_y", silent=T, comp.se=T) # Aggregate estimation
res_r_toke <- f.clusterse(pe.exp1, data_r_toke, "choice_x", "self_x", "other_x", "self_y", "other_y", c("s_x","r_x","q","v"), c("s_y","r_y","q","v"), "sid")

pe.exp1 <- f.estim(data_r_mach, "choice_x", c("s_x","r_x","q","v"), c("s_y","r_y","q","v"), "self_x", "other_x", "self_y", "other_y", silent=T, comp.se=T) # Aggregate estimation
res_r_mach <- f.clusterse(pe.exp1, data_r_mach, "choice_x", "self_x", "other_x", "self_y", "other_y", c("s_x","r_x","q","v"), c("s_y","r_y","q","v"), "sid")

pe.exp1 <- f.estim(data_r_burn, "choice_x", c("s_x","r_x","q","v"), c("s_y","r_y","q","v"), "self_x", "other_x", "self_y", "other_y", silent=T, comp.se=T) # Aggregate estimation
res_r_burn <- f.clusterse(pe.exp1, data_r_burn, "choice_x", "self_x", "other_x", "self_y", "other_y", c("s_x","r_x","q","v"), c("s_y","r_y","q","v"), "sid")

pe.exp1 <- f.estim(data_r_noin, "choice_x", c("s_x","r_x","q","v"), c("s_y","r_y","q","v"), "self_x", "other_x", "self_y", "other_y", silent=T, comp.se=T) # Aggregate estimation
res_r_noin <- f.clusterse(pe.exp1, data_r_noin, "choice_x", "self_x", "other_x", "self_y", "other_y", c("s_x","r_x","q","v"), c("s_y","r_y","q","v"), "sid")


# z-tests for differences
p_alpha_prog <- 2*(1-pnorm(abs(res_r_prog$out[1,1] - res_h$out[1,1])/sqrt(res_r_prog$out[1,2]^2 + res_h$out[1,2]^2)))
p_beta_prog  <- 2*(1-pnorm(abs(res_r_prog$out[2,1] - res_h$out[2,1])/sqrt(res_r_prog$out[2,2]^2 + res_h$out[2,2]^2)))
p_gamma_prog <- 2*(1-pnorm(abs(res_r_prog$out[3,1] - res_h$out[3,1])/sqrt(res_r_prog$out[3,2]^2 + res_h$out[3,2]^2)))
p_delta_prog <- 2*(1-pnorm(abs(res_r_prog$out[4,1] - res_h$out[4,1])/sqrt(res_r_prog$out[4,2]^2 + res_h$out[4,2]^2)))

p_alpha_toke <- 2*(1-pnorm(abs(res_r_toke$out[1,1] - res_h$out[1,1])/sqrt(res_r_toke$out[1,2]^2 + res_h$out[1,2]^2)))
p_beta_toke  <- 2*(1-pnorm(abs(res_r_toke$out[2,1] - res_h$out[2,1])/sqrt(res_r_toke$out[2,2]^2 + res_h$out[2,2]^2)))
p_gamma_toke <- 2*(1-pnorm(abs(res_r_toke$out[3,1] - res_h$out[3,1])/sqrt(res_r_toke$out[3,2]^2 + res_h$out[3,2]^2)))
p_delta_toke <- 2*(1-pnorm(abs(res_r_toke$out[4,1] - res_h$out[4,1])/sqrt(res_r_toke$out[4,2]^2 + res_h$out[4,2]^2)))

p_alpha_mach <- 2*(1-pnorm(abs(res_r_mach$out[1,1] - res_h$out[1,1])/sqrt(res_r_mach$out[1,2]^2 + res_h$out[1,2]^2)))
p_beta_mach  <- 2*(1-pnorm(abs(res_r_mach$out[2,1] - res_h$out[2,1])/sqrt(res_r_mach$out[2,2]^2 + res_h$out[2,2]^2)))
p_gamma_mach <- 2*(1-pnorm(abs(res_r_mach$out[3,1] - res_h$out[3,1])/sqrt(res_r_mach$out[3,2]^2 + res_h$out[3,2]^2)))
p_delta_mach <- 2*(1-pnorm(abs(res_r_mach$out[4,1] - res_h$out[4,1])/sqrt(res_r_mach$out[4,2]^2 + res_h$out[4,2]^2)))

p_alpha_burn <- 2*(1-pnorm(abs(res_r_burn$out[1,1] - res_h$out[1,1])/sqrt(res_r_burn$out[1,2]^2 + res_h$out[1,2]^2)))
p_beta_burn  <- 2*(1-pnorm(abs(res_r_burn$out[2,1] - res_h$out[2,1])/sqrt(res_r_burn$out[2,2]^2 + res_h$out[2,2]^2)))
p_gamma_burn <- 2*(1-pnorm(abs(res_r_burn$out[3,1] - res_h$out[3,1])/sqrt(res_r_burn$out[3,2]^2 + res_h$out[3,2]^2)))
p_delta_burn <- 2*(1-pnorm(abs(res_r_burn$out[4,1] - res_h$out[4,1])/sqrt(res_r_burn$out[4,2]^2 + res_h$out[4,2]^2)))

p_alpha_noin <- 2*(1-pnorm(abs(res_r_noin$out[1,1] - res_h$out[1,1])/sqrt(res_r_noin$out[1,2]^2 + res_h$out[1,2]^2)))
p_beta_noin  <- 2*(1-pnorm(abs(res_r_noin$out[2,1] - res_h$out[2,1])/sqrt(res_r_noin$out[2,2]^2 + res_h$out[2,2]^2)))
p_gamma_noin <- 2*(1-pnorm(abs(res_r_noin$out[3,1] - res_h$out[3,1])/sqrt(res_r_noin$out[3,2]^2 + res_h$out[3,2]^2)))
p_delta_noin <- 2*(1-pnorm(abs(res_r_noin$out[4,1] - res_h$out[4,1])/sqrt(res_r_noin$out[4,2]^2 + res_h$out[4,2]^2)))

cat("\nHuman vs. Programmer\np-value for alpha: p =",p_alpha_prog,
    "\np-value for beta:  p =",p_beta_prog,
    "\np-value for gamma: p =",p_gamma_prog,
    "\np-value for delta: p =",p_delta_prog)

cat("\nHuman vs. Human behind Machine\np-value for alpha: p =",p_alpha_toke,
    "\np-value for beta:  p =",p_beta_toke,
    "\np-value for gamma: p =",p_gamma_toke,
    "\np-value for delta: p =",p_delta_toke)

cat("\nHuman vs. Machine Earns\np-value for alpha: p =",p_alpha_mach,
    "\np-value for beta:  p =",p_beta_mach,
    "\np-value for gamma: p =",p_gamma_mach,
    "\np-value for delta: p =",p_delta_mach)

cat("\nHuman vs. Nobody Earns\np-value for alpha: p =",p_alpha_burn,
    "\np-value for beta:  p =",p_beta_burn,
    "\np-value for gamma: p =",p_gamma_burn,
    "\np-value for delta: p =",p_delta_burn)

cat("\nHuman vs. No Info\np-value for alpha: p =",p_alpha_noin,
    "\np-value for beta:  p =",p_beta_noin,
    "\np-value for gamma: p =",p_gamma_noin,
    "\np-value for delta: p =",p_delta_noin)


#Raw behavioral data and rank sum tests
library(dplyr)
data_h <- data_h %>% select(c(sid, choice_x)) %>% group_by(sid) %>% summarise_all(funs(mean))
data_r_prog <- data_r_prog %>% select(c(sid, choice_x)) %>% group_by(sid) %>% summarise_all(funs(mean))
data_r_toke <- data_r_toke %>% select(c(sid, choice_x)) %>% group_by(sid) %>% summarise_all(funs(mean))
data_r_mach <- data_r_mach %>% select(c(sid, choice_x)) %>% group_by(sid) %>% summarise_all(funs(mean))
data_r_burn <- data_r_burn %>% select(c(sid, choice_x)) %>% group_by(sid) %>% summarise_all(funs(mean))
data_r_noin <- data_r_noin %>% select(c(sid, choice_x)) %>% group_by(sid) %>% summarise_all(funs(mean))

mean(data_h$choice_x)
mean(data_r_prog$choice_x)
mean(data_r_toke$choice_x)
mean(data_r_mach$choice_x)
mean(data_r_burn$choice_x)
mean(data_r_noin$choice_x)

wilcox.test(data_h$choice_x, data_r_prog$choice_x)
wilcox.test(data_h$choice_x, data_r_toke$choice_x)
wilcox.test(data_h$choice_x, data_r_mach$choice_x)
wilcox.test(data_h$choice_x, data_r_burn$choice_x)
wilcox.test(data_h$choice_x, data_r_noin$choice_x)



# ===========================================================================================================================================
# Individual results
# ===========================================================================================================================================

library(ggplot2)
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


iest_h_all <- rbind(iest_hd, iest_ht[,-c(3,4)])
iest_h_all$incons <- (iest_h_all$alpha > 0 & iest_h_all$beta < 0)
table(iest_h_all$incons)

iest_r_prog_all <- rbind(iest_rd_prog, iest_rt_prog[,-c(3,4)])
iest_r_prog_all$incons <- (iest_r_prog_all$alpha > 0 & iest_r_prog_all$beta < 0)
table(iest_r_prog_all$incons)

iest_r_toke_all <- rbind(iest_rd_toke, iest_rt_toke[,-c(3,4)])
iest_r_toke_all$incons <- (iest_r_toke_all$alpha > 0 & iest_r_toke_all$beta < 0)
table(iest_r_toke_all$incons)

iest_r_mach_all <- rbind(iest_rd_mach, iest_rt_mach[,-c(3,4)])
iest_r_mach_all$incons <- (iest_r_mach_all$alpha > 0 & iest_r_mach_all$beta < 0)
table(iest_r_mach_all$incons)

iest_r_burn_all <- rbind(iest_rd_burn, iest_rt_burn[,-c(3,4)])
iest_r_burn_all$incons <- (iest_r_burn_all$alpha > 0 & iest_r_burn_all$beta < 0)
table(iest_r_burn_all$incons)

iest_r_noin_all <- rbind(iest_rd_noin, iest_rt_noin[,-c(3,4)])
iest_r_noin_all$incons <- (iest_r_noin_all$alpha > 0 & iest_r_noin_all$beta < 0)
table(iest_r_noin_all$incons)



ggplot(iest_h_all[!iest_h_all$incons,], aes(x=alpha, y=beta)) + 
  geom_point(colour="blue", size=3) + 
  geom_point(colour="red", size=3, data=iest_r_prog_all[!iest_r_prog_all$incons,], shape=15) + 
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0) + 
  xlim(-1,1) + 
  ylim(-1,1) +
  ggtitle("Fellow Human vs. Programmer")

ggplot(iest_h_all[!iest_h_all$incons,], aes(x=alpha, y=beta)) + 
  geom_point(colour="blue", size=3) + 
  geom_point(colour="red", size=3, data=iest_r_toke_all[!iest_r_toke_all$incons,], shape=15) + 
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0) + 
  xlim(-1,1) + 
  ylim(-1,1) +
  ggtitle("Fellow Human vs. Human behind Machine")

ggplot(iest_h_all[!iest_h_all$incons,], aes(x=alpha, y=beta)) + 
  geom_point(colour="blue", size=3) + 
  geom_point(colour="red", size=3, data=iest_r_mach_all[!iest_r_mach_all$incons,], shape=15) + 
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0) + 
  xlim(-1,1) + 
  ylim(-1,1) +
  ggtitle("Fellow Human vs. Machine Earns")

ggplot(iest_h_all[!iest_h_all$incons,], aes(x=alpha, y=beta)) + 
  geom_point(colour="blue", size=3) + 
  geom_point(colour="red", size=3, data=iest_r_burn_all[!iest_r_burn_all$incons,], shape=15) + 
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0) + 
  xlim(-1,1) + 
  ylim(-1,1) +
  ggtitle("Fellow Human vs. Nobody Earns")

ggplot(iest_h_all[!iest_h_all$incons,], aes(x=alpha, y=beta)) + 
  geom_point(colour="blue", size=3) + 
  geom_point(colour="red", size=3, data=iest_r_noin_all[!iest_r_noin_all$incons,], shape=15) + 
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0) + 
  xlim(-1,1) + 
  ylim(-1,1) +
  ggtitle("Fellow Human vs. No Info")



ggplot(iest_ht, aes(x=gamma, y=delta)) + 
  geom_point(colour="blue", size=3) + 
  geom_point(colour="red", size=3, data=iest_rt_prog, shape=15) + 
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0) + 
  xlim(-1,1) + 
  ylim(-1,1) +
  ggtitle("Fellow Human vs. Programmer")

ggplot(iest_ht, aes(x=gamma, y=delta)) + 
  geom_point(colour="blue", size=3) + 
  geom_point(colour="red", size=3, data=iest_rt_toke, shape=15) + 
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0) + 
  xlim(-1,1) + 
  ylim(-1,1) +
  ggtitle("Fellow Human vs. Human behind Machine")

ggplot(iest_ht, aes(x=gamma, y=delta)) + 
  geom_point(colour="blue", size=3) + 
  geom_point(colour="red", size=3, data=iest_rt_mach, shape=15) + 
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0) + 
  xlim(-1,1) + 
  ylim(-1,1) +
  ggtitle("Fellow Human vs. Machine Earns")

ggplot(iest_ht, aes(x=gamma, y=delta)) + 
  geom_point(colour="blue", size=3) + 
  geom_point(colour="red", size=3, data=iest_rt_burn, shape=15) + 
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0) + 
  xlim(-1,1) + 
  ylim(-1,1) +
  ggtitle("Fellow Human vs. Nobody Earns")

ggplot(iest_ht, aes(x=gamma, y=delta)) + 
  geom_point(colour="blue", size=3) + 
  geom_point(colour="red", size=3, data=iest_rt_noin, shape=15) + 
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0) + 
  xlim(-1,1) + 
  ylim(-1,1) +
  ggtitle("Fellow Human vs. No Info")

