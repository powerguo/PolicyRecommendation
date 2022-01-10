library(dplyr)
library(tidyr)
library(googleAuthR)
library(googledrive)
library(googlesheets4)
library(shinyjs)
library(lubridate)
library(rstanarm)
library(mlogit)
library(Hmisc)
library(survival)
library(DoE.base)
library(DoE.wrapper)
library(Metrics)
library(mgcv)
library(support.CEs)
library(mlmRev)
library(lme4)
library(rlist)
library(Matrix)
library(matrixStats)
library(pROC)
library(purrr)
library(magrittr)
library(gargle)
library(httr)
library(readxl)
library(fmsb)
library(gridExtra)
library(ggpubr)


#Read in participant voting results for those who participated in Stage 3

indi.result.all <- c()

for(i in 1:54){
  
  indi.result.all[[i]] <-read_excel(paste0("Data/Group Result/Voting Result", i, ".xlsx"), range = cell_rows(17:17), col_names = FALSE)
  
  
}

indi.group <- c()

indi.extract <- function(x){
  
  p1 <- as.numeric(ifelse(x[1] == "Yes", 1, 0))
  p2 <- as.numeric(ifelse(x[2] == "Yes", 1, 0))
  p3 <- as.numeric(ifelse(x[3] == "Yes", 1, 0))
  p4 <- as.numeric(ifelse(x[4] == "Yes", 1, 0))
  
  return(rbind(p1, p2, p3, p4))
  
}

a <- indi.extract(indi.result.all[[1]])

for(i in 1:54){
  
  indi.group1 <- indi.extract(indi.result.all[[i]])
  indi.group <- rbind(indi.group, indi.group1)
  
}

group.number <- rep(1:54, each = 4)

treat.vector.total <- rep(treat.vector, each = 4)

#Combining individual recommendation voting result with model indicator
indi.consensus <- as.data.frame(cbind(indi.group, group.number, treat.vector.total))

#filter out weighted sum results
T1.indi.consensus <- indi.consensus%>%
  filter(treat.vector.total == 0)

#filter out mean variance results
T2.indi.consensus <- indi.consensus%>%
  filter(treat.vector.total == 1)

#Individual model for recommendation
consensus <- lmer(V1 ~ as.factor(treat.vector.total) + (1|group.number), control=lmerControl(optimizer = "optimx",optCtrl=list(method= "L-BFGS-B")),  data = indi.consensus)

#Fixed effect reported in Table 2
fixef(consensus)

#Individual validation result
indi.valid <- c()

for(i in 1:54){
  
  indi.valid[[i]] <-read_excel(paste0("Data/Group Result/Voting Result", i, ".xlsx"), range = cell_rows(18:27), col_names = FALSE)
  
  
}

indi.valid.total <- c()

indi.calc.valid <- function(x){
  
  result <- c()
  
  temp <- as.data.frame(x)
  
  names(temp) <- c("P1", "P2", "P3", "P4")
  
  temp <- temp%>%
    mutate(P1 = ifelse(P1 == "Left", 1, 0))%>%
    mutate(P2 = ifelse(P2 == "Left", 1, 0))%>%
    mutate(P3 = ifelse(P3 == "Left", 1, 0))%>%
    mutate(P4 = ifelse(P4 == "Left", 1, 0))
  
  result <- as.vector(as.matrix(temp))
  
  return(result)
  
  
}

total.valid <- c()

for(i in 1:54){
  
  temp1 <- indi.calc.valid(indi.valid[[i]])  
  total.valid <- append(total.valid, temp1)
  
}

person.count <- rep(1:216, each = 10)
group.count <- rep(1:54, each = 40)
treat.count <- rep(treat.vector, each = 40)

total.validation <- as.data.frame(cbind(total.valid, person.count, group.count, treat.count))

#Validation Model with random intercept for individual and group
indi.validation <- lmer(data = total.validation, total.valid ~ as.factor(treat.count) + (1|group.count) + (1|person.count), 
                        control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#Fixed effect of model selection reported in Table 2
fixef(indi.validation)
