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

pe.exp1 <- f.estim(data, "choice_x", c("s_x","r_x","q","v"), c("s_y","r_y","q","v"), "self_x", "other_x", "self_y", "other_y", silent=T, comp.se=T) # Aggregate estimation
res <- f.clusterse(pe.exp1, data, "choice_x", "self_x", "other_x", "self_y", "other_y", c("s_x","r_x","q","v"), c("s_y","r_y","q","v"), "sid")

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


#Bar plots
library(ggplot2)
library(ggpubr)
library(scales)
dodgewidth <- position_dodge(width=0.9)

estimates <- as.data.frame(matrix(c("Fellow Human", "Programmer", "Human behind Machine", "Machine Earns", "Nobody Earns", "No Information", 
                                    mean(data_h$choice_x), mean(data_r_prog$choice_x), mean(data_r_toke$choice_x), mean(data_r_mach$choice_x), mean(data_r_burn$choice_x), mean(data_r_noin$choice_x),
                                    res_h$out[1,1], res_r_prog$out[1,1], res_r_toke$out[1,1], res_r_mach$out[1,1], res_r_burn$out[1,1], res_r_noin$out[1,1],
                                    res_h$out[2,1], res_r_prog$out[2,1], res_r_toke$out[2,1], res_r_mach$out[2,1], res_r_burn$out[2,1], res_r_noin$out[2,1],
                                    res_h$out[3,1], res_r_prog$out[3,1], res_r_toke$out[3,1], res_r_mach$out[3,1], res_r_burn$out[3,1], res_r_noin$out[3,1],
                                    res_h$out[4,1], res_r_prog$out[4,1], res_r_toke$out[4,1], res_r_mach$out[4,1], res_r_burn$out[4,1], res_r_noin$out[4,1]), 
                                  nrow = 6))
colnames(estimates) <- c("Treatment", "Share_X", "alpha", "beta", "gamma", "delta")
estimates$Treatment <- factor(estimates$Treatment, levels = estimates$Treatment)
estimates[,-1] <- sapply(estimates[,-1], as.numeric)

estimates$note_share1 <- c(
    paste(formatC(mean(data_h$choice_x)*100, width = 4), "%", sep=""),
    rep("", 5)
)
estimates$note_share2 <- c(
    "",
    paste(formatC(mean(data_r_prog$choice_x)*100, width = 4), "%\n[Diff: p<0.001]", sep=""),
    paste(formatC(mean(data_r_toke$choice_x)*100, width = 2), "%\n[Diff: p<0.001]", sep=""),
    paste(formatC(mean(data_r_mach$choice_x)*100, width = 2), "%\n[Diff: p<0.001]", sep=""),
    paste(formatC(mean(data_r_burn$choice_x)*100, width = 2), "%\n[Diff: p<0.001]", sep=""),
    paste(formatC(mean(data_r_noin$choice_x)*100, width = 2), "%\n[Diff: p<0.001]", sep="")
)

estimates$note_alpha1 <- c(
    paste("\U03B1=", formatC(res_h$out[1,1], digits=4, format="f"), "\n(p", ifelse(round(res_h$out[1,4], 3)<0.001, "<0.001", paste("=", formatC(res_h$out[1,4], digits=3, format="f"), sep="")), ")", sep=""),
    rep("", 5)
)
estimates$note_alpha2 <- c(
    "",
    paste("\U03B1=", formatC(res_r_prog$out[1,1], digits=4, format="f"), "\n(p", ifelse(round(res_r_prog$out[1,4], 3)<0.001, "<0.001", paste("=", formatC(res_r_prog$out[1,4], digits=3, format="f"), sep="")), ")\n[Diff: p", ifelse(round(p_alpha_prog, 3)<0.001, "<0.001", paste("=", formatC(p_alpha_prog, digits=3, format="f"), sep="")), "]", sep=""),
    paste("\U03B1=", formatC(res_r_toke$out[1,1], digits=4, format="f"), "\n(p", ifelse(round(res_r_toke$out[1,4], 3)<0.001, "<0.001", paste("=", formatC(res_r_toke$out[1,4], digits=3, format="f"), sep="")), ")\n[Diff: p", ifelse(round(p_alpha_toke, 3)<0.001, "<0.001", paste("=", formatC(p_alpha_toke, digits=3, format="f"), sep="")), "]", sep=""),
    paste("\U03B1=", formatC(res_r_mach$out[1,1], digits=4, format="f"), "\n(p", ifelse(round(res_r_mach$out[1,4], 3)<0.001, "<0.001", paste("=", formatC(res_r_mach$out[1,4], digits=3, format="f"), sep="")), ")\n[Diff: p", ifelse(round(p_alpha_mach, 3)<0.001, "<0.001", paste("=", formatC(p_alpha_mach, digits=3, format="f"), sep="")), "]", sep=""),
    paste("\U03B1=", formatC(res_r_burn$out[1,1], digits=4, format="f"), "\n(p", ifelse(round(res_r_burn$out[1,4], 3)<0.001, "<0.001", paste("=", formatC(res_r_burn$out[1,4], digits=3, format="f"), sep="")), ")\n[Diff: p", ifelse(round(p_alpha_burn, 3)<0.001, "<0.001", paste("=", formatC(p_alpha_burn, digits=3, format="f"), sep="")), "]", sep=""),
    paste("\U03B1=", formatC(res_r_noin$out[1,1], digits=4, format="f"), "\n(p", ifelse(round(res_r_noin$out[1,4], 3)<0.001, "<0.001", paste("=", formatC(res_r_noin$out[1,4], digits=3, format="f"), sep="")), ")\n[Diff: p", ifelse(round(p_alpha_noin, 3)<0.001, "<0.001", paste("=", formatC(p_alpha_noin, digits=3, format="f"), sep="")), "]", sep="")
)

estimates$note_beta1 <- c(
    paste("\U03B2=", formatC(res_h$out[2,1], digits=4, format="f"), "\n(p", ifelse(round(res_h$out[2,4], 3)<0.001, "<0.001", paste("=", formatC(res_h$out[2,4], digits=3, format="f"), sep="")), ")", sep=""),
    rep("", 5)
)
estimates$note_beta2 <- c(
    "",
    paste("\U03B2=", formatC(res_r_prog$out[2,1], digits=4, format="f"), "\n(p", ifelse(round(res_r_prog$out[2,4], 3)<0.001, "<0.001", paste("=", formatC(res_r_prog$out[2,4], digits=3, format="f"), sep="")), ")\n[Diff: p", ifelse(round(p_beta_prog, 3)<0.001, "<0.001", paste("=", formatC(p_beta_prog, digits=3, format="f"), sep="")), "]", sep=""),
    paste("\U03B2=", formatC(res_r_toke$out[2,1], digits=4, format="f"), "\n(p", ifelse(round(res_r_toke$out[2,4], 3)<0.001, "<0.001", paste("=", formatC(res_r_toke$out[2,4], digits=3, format="f"), sep="")), ")\n[Diff: p", ifelse(round(p_beta_toke, 3)<0.001, "<0.001", paste("=", formatC(p_beta_toke, digits=3, format="f"), sep="")), "]", sep=""),
    "",
    "",
    paste("\U03B2=", formatC(res_r_noin$out[2,1], digits=4, format="f"), "\n(p", ifelse(round(res_r_noin$out[2,4], 3)<0.001, "<0.001", paste("=", formatC(res_r_noin$out[2,4], digits=3, format="f"), sep="")), ")\n[Diff: p", ifelse(round(p_beta_noin, 3)<0.001, "<0.001", paste("=", formatC(p_beta_noin, digits=3, format="f"), sep="")), "]", sep="")
)
estimates$note_beta3 <- c(
    "",
    "",
    "",
    paste("\U03B2=", formatC(res_r_mach$out[2,1], digits=4, format="f"), "\n(p", ifelse(round(res_r_mach$out[2,4], 3)<0.001, "<0.001", paste("=", formatC(res_r_mach$out[2,4], digits=3, format="f"), sep="")), ")\n[Diff: p", ifelse(round(p_beta_mach, 3)<0.001, "<0.001", paste("=", formatC(p_beta_mach, digits=3, format="f"), sep="")), "]", sep=""),
    paste("\U03B2=", formatC(res_r_burn$out[2,1], digits=4, format="f"), "\n(p", ifelse(round(res_r_burn$out[2,4], 3)<0.001, "<0.001", paste("=", formatC(res_r_burn$out[2,4], digits=3, format="f"), sep="")), ")\n[Diff: p", ifelse(round(p_beta_burn, 3)<0.001, "<0.001", paste("=", formatC(p_beta_burn, digits=3, format="f"), sep="")), "]", sep=""),
    ""
)

estimates$note_gamma1 <- c(
    paste("\U03B3=", formatC(res_h$out[3,1], digits=4, format="f"), "\n(p", ifelse(round(res_h$out[3,4], 3)<0.001, "<0.001", paste("=", formatC(res_h$out[3,4], digits=3, format="f"), sep="")), ")", sep=""),
    rep("", 5)
)
estimates$note_gamma2 <- c(
    "",
    paste("\U03B3=", formatC(res_r_prog$out[3,1], digits=4, format="f"), "\n(p", ifelse(round(res_r_prog$out[3,4], 3)<0.001, "<0.001", paste("=", formatC(res_r_prog$out[3,4], digits=3, format="f"), sep="")), ")\n[Diff: p", ifelse(round(p_gamma_prog, 3)<0.001, "<0.001", paste("=", formatC(p_gamma_prog, digits=3, format="f"), sep="")), "]", sep=""),
    paste("\U03B3=", formatC(res_r_toke$out[3,1], digits=4, format="f"), "\n(p", ifelse(round(res_r_toke$out[3,4], 3)<0.001, "<0.001", paste("=", formatC(res_r_toke$out[3,4], digits=3, format="f"), sep="")), ")\n[Diff: p", ifelse(round(p_gamma_toke, 3)<0.001, "<0.001", paste("=", formatC(p_gamma_toke, digits=3, format="f"), sep="")), "]", sep=""),
    "",
    "",
    ""
)
estimates$note_gamma3 <- c(
    "",
    "",
    "",
    paste("\U03B3=", formatC(res_r_mach$out[3,1], digits=4, format="f"), "\n(p", ifelse(round(res_r_mach$out[3,4], 3)<0.001, "<0.001", paste("=", formatC(res_r_mach$out[3,4], digits=3, format="f"), sep="")), ")\n[Diff: p", ifelse(round(p_gamma_mach, 3)<0.001, "<0.001", paste("=", formatC(p_gamma_mach, digits=3, format="f"), sep="")), "]", sep=""),
    paste("\U03B3=", formatC(res_r_burn$out[3,1], digits=4, format="f"), "\n(p", ifelse(round(res_r_burn$out[3,4], 3)<0.001, "<0.001", paste("=", formatC(res_r_burn$out[3,4], digits=3, format="f"), sep="")), ")\n[Diff: p", ifelse(round(p_gamma_burn, 3)<0.001, "<0.001", paste("=", formatC(p_gamma_burn, digits=3, format="f"), sep="")), "]", sep=""),
    paste("\U03B3=", formatC(res_r_noin$out[3,1], digits=4, format="f"), "\n(p", ifelse(round(res_r_noin$out[3,4], 3)<0.001, "<0.001", paste("=", formatC(res_r_noin$out[3,4], digits=3, format="f"), sep="")), ")\n[Diff: p", ifelse(round(p_gamma_noin, 3)<0.001, "<0.001", paste("=", formatC(p_gamma_noin, digits=3, format="f"), sep="")), "]", sep="")
)

estimates$note_delta1 <- c(
    paste("\U03B4=", formatC(res_h$out[4,1], digits=4, format="f"), "\n(p", ifelse(round(res_h$out[4,4], 3)<0.001, "<0.001", paste("=", formatC(res_h$out[4,4], digits=3, format="f"), sep="")), ")", sep=""),
    rep("", 5)
)
estimates$note_delta2 <- c(
    "",
    paste("\U03B4=", formatC(res_r_prog$out[4,1], digits=4, format="f"), "\n(p", ifelse(round(res_r_prog$out[4,4], 3)<0.001, "<0.001", paste("=", formatC(res_r_prog$out[4,4], digits=3, format="f"), sep="")), ")\n[Diff: p", ifelse(round(p_delta_prog, 3)<0.001, "<0.001", paste("=", formatC(p_delta_prog, digits=3, format="f"), sep="")), "]", sep=""),
    paste("\U03B4=", formatC(res_r_toke$out[4,1], digits=4, format="f"), "\n(p", ifelse(round(res_r_toke$out[4,4], 3)<0.001, "<0.001", paste("=", formatC(res_r_toke$out[4,4], digits=3, format="f"), sep="")), ")\n[Diff: p", ifelse(round(p_delta_toke, 3)<0.001, "<0.001", paste("=", formatC(p_delta_toke, digits=3, format="f"), sep="")), "]", sep=""),
    "",
    "",
    ""
)
estimates$note_delta3 <- c(
    "",
    "",
    "",
    paste("\U03B4=", formatC(res_r_mach$out[4,1], digits=4, format="f"), "\n(p", ifelse(round(res_r_mach$out[4,4], 3)<0.001, "<0.001", paste("=", formatC(res_r_mach$out[4,4], digits=3, format="f"), sep="")), ")\n[Diff: p", ifelse(round(p_delta_mach, 3)<0.001, "<0.001", paste("=", formatC(p_delta_mach, digits=3, format="f"), sep="")), "]", sep=""),
    paste("\U03B4=", formatC(res_r_burn$out[4,1], digits=4, format="f"), "\n(p", ifelse(round(res_r_burn$out[4,4], 3)<0.001, "<0.001", paste("=", formatC(res_r_burn$out[4,4], digits=3, format="f"), sep="")), ")\n[Diff: p", ifelse(round(p_delta_burn, 3)<0.001, "<0.001", paste("=", formatC(p_delta_burn, digits=3, format="f"), sep="")), "]", sep=""),
    paste("\U03B4=", formatC(res_r_noin$out[4,1], digits=4, format="f"), "\n(p", ifelse(round(res_r_noin$out[4,4], 3)<0.001, "<0.001", paste("=", formatC(res_r_noin$out[4,4], digits=3, format="f"), sep="")), ")\n[Diff: p", ifelse(round(p_delta_noin, 3)<0.001, "<0.001", paste("=", formatC(p_delta_noin, digits=3, format="f"), sep="")), "]", sep="")
)

range_share = c(0.6,0.95)
ggplot(estimates, aes(x=Treatment, y=Share_X)) + 
    geom_col(colour = "black", fill="navy", position = dodgewidth) +
    scale_y_continuous(limits=range_share, oob = rescale_none) + 
    xlab("") +
    ylab("Share Option X") +
    ggtitle("Treatment Comparison: Share Option X") +
    geom_text(position= dodgewidth, aes(x=Treatment, y=Share_X, label=note_share1), vjust=-1) +
    geom_text(position= dodgewidth, aes(x=Treatment, y=Share_X, label=note_share2), vjust=-0.4)

plot_alpha <- ggplot(estimates, aes(x=Treatment, y=alpha)) + 
    geom_col(colour = "black", fill="navy", position = dodgewidth) +
    scale_y_continuous(limits=c(-0.25,0), oob = rescale_none) +
    xlab("") +
    ggtitle("Treatment Comparison: alpha") +
    geom_text(position= dodgewidth, aes(x=Treatment, y=alpha, label=note_alpha1), vjust=1.1) +
    geom_text(position= dodgewidth, aes(x=Treatment, y=alpha, label=note_alpha2), vjust=1.1)

plot_beta <- ggplot(estimates, aes(x=Treatment, y=beta)) + 
    geom_col(colour = "black", fill="navy", position = dodgewidth) +
    scale_y_continuous(limits=c(-0.13,0.4), oob = rescale_none) +
    xlab("") +
    ggtitle("Treatment Comparison: beta") +
    geom_text(position= dodgewidth, aes(x=Treatment, y=beta, label=note_beta1), vjust=-0.3) +
    geom_text(position= dodgewidth, aes(x=Treatment, y=beta, label=note_beta2), vjust=-0.2) +
    geom_text(position= dodgewidth, aes(x=Treatment, y=beta, label=note_beta3), vjust=1.1)

plot_gamma <- ggplot(estimates, aes(x=Treatment, y=gamma)) + 
    geom_col(colour = "black", fill="navy", position = dodgewidth) +
    scale_y_continuous(limits=c(-0.07,0.14), oob = rescale_none) +
    xlab("") +
    ggtitle("Treatment Comparison: gamma") +
    geom_text(position= dodgewidth, aes(x=Treatment, y=gamma, label=note_gamma1), vjust=-0.3) +
    geom_text(position= dodgewidth, aes(x=Treatment, y=gamma, label=note_gamma2), vjust=1.1) +
    geom_text(position= dodgewidth, aes(x=Treatment, y=gamma, label=note_gamma3), vjust=-0.2)

plot_delta <- ggplot(estimates, aes(x=Treatment, y=delta)) + 
    geom_col(colour = "black", fill="navy", position = dodgewidth) +
    scale_y_continuous(limits=c(-0.08,0.08), oob = rescale_none) +
    xlab("") +
    ggtitle("Treatment Comparison: delta") +
    geom_text(position= dodgewidth, aes(x=Treatment, y=delta, label=note_delta1), vjust=-0.3) +
    geom_text(position= dodgewidth, aes(x=Treatment, y=delta, label=note_delta2), vjust=1.1) +
    geom_text(position= dodgewidth, aes(x=Treatment, y=delta, label=note_delta3), vjust=-0.2)

ggarrange(plot_alpha, plot_beta, plot_gamma, plot_delta, nrow = 2, ncol = 2)


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

wilcox.test(data_r_noin$choice_x, data_r_prog$choice_x)
