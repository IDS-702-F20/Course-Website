
###########################################################################
###########################################################################
#################################### START ################################
###########################################################################
###########################################################################


##### Load the data
library(ggplot2)
library(cobalt)
library(MatchIt)
library(randomForest)
# Read in the data
RHC <- read.table("rhc.txt",head=T)
RHC <- RHC[,-which(names(RHC)=="surv2md1")]
RHC$treatment <- as.numeric(RHC$treatment)
RHC$dth30 <- as.numeric(RHC$dth30)

dim(RHC)
head(RHC)


p_score_model <- glm(treatment ~.-dth30,data=RHC,family = bsinomial)
summary(p_score_model)
RHC$p_score <- predict(p_score_model,type="response")

sum(RHC$p_score < max(min(RHC$p_score[RHC$treatment==0]),min(RHC$p_score[RHC$treatment==1])) |
      RHC$p_score > min(max(RHC$p_score[RHC$treatment==0]),max(RHC$p_score[RHC$treatment==1])))
