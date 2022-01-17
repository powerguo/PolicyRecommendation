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

#Optimization function for mean-variance model 
constGLM1multiple <- function(X, y, Xtest){
  # constraints
  # Ubar: 0.999 < theta1 < 1.001
  # SD: -1/sqrt(2) < theta2 < 1/sqrt(2)
  ui <- matrix(c(1, 0, -1, 0, 0, 1, 0, -1), nrow = 4, ncol = 2, byrow = TRUE)
  ci <- c(0.999, -1.001, -1/sqrt(4), -1/sqrt(4))
  
  # constrained log-likelihood
  CLL <- function(par, X, y){
    eta <- X %*% par
    p <- exp(eta)/(1 + exp(eta))
    ll <- sum(y*log(p) + (1-y)*log(1-p))
    return(-ll)
  }
  
  # Optimize
  optims <- constrOptim(c(1, 0), CLL, ui = ui, ci = ci, X = X, y = y, grad = NULL)
  pars <- optims$par
  print(pars)
  
  # Return predicted probabilities
  etatest <- Xtest %*% pars
  output.list = list(pars, exp(etatest)/(1+exp(etatest)), etatest)
  return(output.list)
}

#Read in individual cluster results

clustered.indi <- read.csv("Data/Group Result/cluster_assignment_join.csv")%>%
  arrange(Id)

#Mean, Median, and St Dev calculations are performed on all clusters for all attributes

attribute_list <- c("intercept", "Nuc1", "Nuc2", "CO2", "Ban_fuel1", "Ban_fuel2", "RFS1", "RFS2")
descriptive_list <- c("mean", "median", "sd")
cluster_list <- c("1", "2", "3", "4")

for (i in 1:8){ #8 attributes
  
  for(j in 1:3){# 3 descriptive statistics
    
    for(k in 1:4){# 4 clusters
      
      temp_stat <- clustered.indi%>%
        filter(assigned == cluster_list[k])
      
      temp_stat_op <- get(descriptive_list[j])(temp_stat[,1+i])
      
      assign(paste0(attribute_list[i], "_", descriptive_list[j], "_", cluster_list[k]), temp_stat_op)
      
    }
    
  }   
}

#Values Reported in Appendix C, Table 3

#Calculate K values for the mean-variance models

kvalues <- c()

#Calculate K values based on group responses in the group discussion stage. 
#Important to note that K values are deterministic based on group responses.

vote.result.all <- c()

#Keep track of which model the group used
treat.vector <- c()

for(m in 1:54){
  
  treat.vector[m] <- ifelse(colnames(prob.all[[m]][15]) == "predicted.probability", 1,0)
  
}

#Read in Group results
for(j in 1:54){
  
    vote.result.all[[j]] <- read_excel(paste0("Data/Group Result/Voting Result", j, ".xlsx"), range = cell_rows(2:16), col_names = FALSE)
  
  
}

kvalues <- c()

for(i in 1:54){
  
  all.responses <- vote.result.all[[i]]
  
  if(treat.vector[i] == 1){
    y.recommendation <- array(dim = c(15,4))
    
    for(userResponseCount in 1:15){
      for(userCount in 1:4){
        
        y.recommendation[userResponseCount, userCount] <- ifelse(all.responses[userResponseCount, userCount] == "Left", 1,0)
        
      }
      
    }
    
    y.test <- as.vector(y.recommendation)
    
    left.uber <- design.all[[i]]
    #uber.tot <- left_join(left.uber, group.utility, by = c("nuc.power1", "nuc.power2", "CO2Price", "ban.fuel.explore1", "ban.fuel.explore2", "RFS1", "RFS2"))
    
    ubar <- as.data.frame(left.uber)[,"group.score"]
    
    sdrec <- as.data.frame(left.uber)[,"group.sd"]
    
    ubar_test <- rep(ubar, times = 4)
    sd_test <- rep(sdrec, times =4)
    
    ubar_total <- as.data.frame(prob.all[[i]])[,13]
    sd_total <- as.data.frame(prob.all[[i]])[,14]
    
    ubar_total_repeat <- rep(ubar_total, times  = 4)
    sd_total_repeat <- rep(sd_total, times = 4)
    cGLM1 <- constGLM1multiple(X=cbind(ubar_test, sd_test), y = y.test, Xtest = cbind(ubar_total_repeat, sd_total_repeat))
    
    kvalues[i] <- cGLM1[[1]][2]
    
    print(i)
    
  } else{
    kvalues[i] <- 100
    print("nope")
    
  }
  
  
}

kvalues.bind <- as.data.frame(cbind(kvalues, treat.vector))

#Filtering only groups that used the mean-variance model
T2.kvalue <- kvalues.bind%>%
  filter(treat.vector == 1)

#Probability density plot of all k values

ggplot(data = T2.kvalue, aes(x = kvalues))+
  geom_histogram(aes(y = ..density..),  position = "identity", bins = 10, alpha = 0.5)+
  geom_density(alpha = 0.6)+
  ggtitle("Estimated k Values")+
  xlab("Estimated k Values")+
  ylab("Density")+
  theme_bw()+
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title = element_text(size = 16),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)
  )

ggsave("Figures/AppendixFigure2.png", dpi = 100)

#Filtering to groups that agreed with the recommendation

#Group Voting results
group.consensus <- c()

for(k in 1:54){
  
  group.consensus[k] <- ifelse(vote.result[[k]][1] == "Yes" & vote.result[[k]][2] == "Yes" & vote.result[[k]][3] == "Yes" & vote.result[[k]][4] == "Yes", 1, 0)
  
}

kvalues.bind <- as.data.frame(cbind(kvalues, treat.vector,group.consensus))

T2_consensus_kvalues <- kvalues.bind%>%
  filter(treat.vector ==1 & group.consensus == 1)

ggplot(data = T2_consensus_kvalues, aes(x = kvalues))+
  geom_histogram(aes(y = ..density..),  position = "identity", bins = 10, alpha = 0.5)+
  geom_density(alpha = 0.6)+
  ggtitle("Estimated k Values for Groups with Consensus")+
  xlab("Estimated k Values")+
  ylab("Density")+
  theme_bw()+
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title = element_text(size = 16),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)
  )
#Figure 3 in Appendix E
ggsave("Figures/AppendixFigure2B.png", dpi = 100)

#Correlation against equal weight social welfare function

#Estimated Utility Values for All Alternatives for each group

prob.all <- c()

for(l in 1:54){
  
  prob.all[[l]] <- read_excel(paste0("Data/Group Result/predictedprob", l, ".xlsx"), col_names = TRUE)
  
}

team.cor <- c()

for (i in 1:54){
  
  if(treat.vector[i] == 1){
    
    team.cor[i] <- cor(prob.all[[i]][,"group.score"], prob.all[[i]][,"predicted.probability"])

    
  } else{
    team.cor[i] <- cor(prob.all[[i]][,"group.score"], prob.all[[i]][,"new.utility"])
  }  
  
}

team.cor.bind <- as.data.frame(cbind(team.cor, treat.vector, group.consensus))

T2.cor <- team.cor.bind%>%
  filter(treat.vector == 1)

mean(T2.cor$team.cor)

T2.cor_consensus <- T2.cor%>%
  filter(group.consensus == 1)

mean(T2.cor_consensus$team.cor)

#Find top policies for equal weighting versus mean-variance

is.top5 <- c()
is.top <- c()

for (i in 1:54){
  
  if(treat.vector[i] == 1){
    
    temp_list <- as.data.frame(prob.all[[i]])%>%
      arrange(desc(group.score))
    
    top5_equal <- temp_list[1:5,]
    top1_equal <- temp_list[1,]
    top1_equal_id <- top1_equal$id
    top_5_equal_id <- top5_equal$id
    
    top5_meanvariance <- as.data.frame(prob.all[[i]][1:5,])
    top1_meanvariance <- as.data.frame(prob.all[[i]][1,])
    top5_meanvariance_id <- top5_meanvariance$id
    top1_meanvariance_id <- top1_meanvariance$id
    
    equals.num <- equals(top_5_equal_id, top5_meanvariance_id)
    
    is.top5[i] <- sum(equals.num)
    
    is.top[i] <- equals(top1_equal_id, top1_meanvariance_id)
    
  } else{
    team.cor[i] <- cor(prob.all[[i]][,"group.score"], prob.all[[i]][,"new.utility"])
  }  
  
}

#Average number of top 5 being the same for equal weights and mean-variance
mean(is.top5, na.rm = TRUE)

#Number of times the top recommendation is same between mean-variance and equal weights
sum(is.top, na.rm = TRUE)

#Appendix F

#Simulation Results

#Helper Functions


create.col.nuc <- function(df, n){
  varname <- paste0("nuc.power0", n)
  mutate(df, !!varname := ifelse(nuc.power0 == n, 1,0))
}

create.col.ban <- function(df, n){
  varname <- paste0("ban.fuel.explore0", n)
  mutate(df, !!varname := ifelse(ban.fuel.explore0 == n, 1,0))
}

create.col.RFS <- function(df, n){
  varname <- paste0("RFS0", n)
  mutate(df, !!varname := ifelse(RFS0 == n, 1,0))
}

create.col.nuc1 <- function(df, n){
  varname <- paste0("nuc.power1", n)
  mutate(df, !!varname := ifelse(nuc.power1 == n, 1,0))
}

create.col.ban1 <- function(df, n){
  varname <- paste0("ban.fuel.explore1", n)
  mutate(df, !!varname := ifelse(ban.fuel.explore1 == n, 1,0))
}

create.col.RFS1 <- function(df, n){
  varname <- paste0("RFS1", n)
  mutate(df, !!varname := ifelse(RFS1 == n, 1,0))
}

#Generating the full alternative set

alt1.test <- fac.design(factor.names = list(nuc.power =c("Provide financial support to build nuclear power plants that have been planned and proposed while maintaining the existing nuclear power plants. This will add ~13 GW of nuclear power to the existing 92 GW of nuclear capacity", 
                                                         "Provide support and maintain the existing nuclear power capacity of 92 GW (~20%) in the electricity grid. No support for any new or proposed nuclear power plants in the country.", 
                                                         "Phase out unprofitable and scheduled to close nuclear power in the country that will reduce the generation capacity by 13.7-26.8 GW with no planned replacement in the next eight years."),
                                            CO2Price = c("0", "30", "60", "90", "120", "150"),
                                            ban.fuel.explore = c("Reinstate 2015 Bureau of Land Management executive order for hydraulic fracturing on public lands that increases storage safety standards and transparency of the chemicals used.", 
                                                                 "Fully ban fossil fuel exploration on public lands", 
                                                                 "No change to status quo"),
                                            RFS = c("Starting from 2021, reach 100% clean energy by 2035 with a yearly increase of 7.5% of clean energy in the grid", 
                                                    "Starting from 2021, reach 100% clean energy by 2050 with a yearly increase of 3.5% of clean energy in the grid", 
                                                    "Starting from 2021, reach 100% clean energy by 2100 with a yearly increase of 1.3% of clean energy in the grid")))


alt.left.num.test <- alt1.test%>%
  mutate(nuc.power = ifelse(nuc.power == "Provide financial support to build nuclear power plants that have been planned and proposed while maintaining the existing nuclear power plants. This will add ~13 GW of nuclear power to the existing 92 GW of nuclear capacity", 0, ifelse(
    nuc.power == "Provide support and maintain the existing nuclear power capacity of 92 GW (~20%) in the electricity grid. No support for any new or proposed nuclear power plants in the country.", 1, 2)
  ))%>%
  mutate(CO2Price = ifelse(CO2Price == "0", 0, ifelse(
    CO2Price == "30", 1, ifelse(
      CO2Price == "60", 2, ifelse(
        CO2Price =="90", 3, ifelse(
          CO2Price == "120", 4, 5
        )
      )
    ) 
  )))%>%
  mutate(ban.fuel.explore = ifelse(ban.fuel.explore == "Reinstate 2015 Bureau of Land Management executive order for hydraulic fracturing on public lands that increases storage safety standards and transparency of the chemicals used.", 0, ifelse(
    ban.fuel.explore == "Fully ban fossil fuel exploration on public lands", 1, 2
  )))%>%
  mutate(RFS = ifelse(RFS == "Starting from 2021, reach 100% clean energy by 2035 with a yearly increase of 7.5% of clean energy in the grid", 0, ifelse(
    RFS == "Starting from 2021, reach 100% clean energy by 2050 with a yearly increase of 3.5% of clean energy in the grid", 1, 2)))

alt.right.num.test <- alt.left.num.test%>%
  mutate(nuc.power = (nuc.power + 1)%%3)%>%
  mutate(ban.fuel.explore = (ban.fuel.explore + 1)%%3)%>%
  mutate(RFS = (RFS + 1)%%3)%>%
  mutate(CO2Price = (CO2Price + 1)%%6)%>%
  as.data.frame()%>%
  dplyr::select(nuc.power, CO2Price, ban.fuel.explore, RFS)

alt.left.txt <- alt.left.num.test%>%
  mutate(CO2Price = ifelse(CO2Price == "0", 0, ifelse(
    CO2Price == "1", 30, ifelse(
      CO2Price == "2", 60, ifelse(
        CO2Price =="3", 90, ifelse(
          CO2Price =="4", 120, 150
        )
      )
    )
  )))%>%
  mutate(nuc.power0 = nuc.power)%>%
  mutate(CO2Price0 = CO2Price)%>%
  mutate(ban.fuel.explore0 = ban.fuel.explore)%>%
  mutate(RFS0 = RFS)%>%
  as.data.frame()%>%
  dplyr::select(nuc.power0, CO2Price0, ban.fuel.explore0, RFS0)


alt.right.txt <- alt.right.num.test%>%
  mutate(CO2Price = ifelse(CO2Price == "0", 0, ifelse(
    CO2Price == "1", 30, ifelse(
      CO2Price == "2", 60, ifelse(
        CO2Price =="3", 90, ifelse(
          CO2Price =="4", 120, 150
        )
      )
    )
  )))%>%
  mutate(nuc.power1 = nuc.power)%>%
  mutate(CO2Price1 = CO2Price)%>%
  mutate(ban.fuel.explore1 = ban.fuel.explore)%>%
  mutate(RFS1 = RFS)%>%
  as.data.frame()%>%
  dplyr::select(nuc.power1, CO2Price1, ban.fuel.explore1, RFS1)

total.design.test <- cbind(alt.left.txt, alt.right.txt)

total.design.test.1 <- total.design.test%>%
  create.col.nuc(1)%>%
  create.col.nuc(2)%>%
  create.col.ban(1)%>%
  create.col.ban(2)%>%
  create.col.RFS(1)%>%
  create.col.RFS(2)%>%
  create.col.nuc1(1)%>%
  create.col.nuc1(2)%>%
  create.col.ban1(1)%>%
  create.col.ban1(2)%>%
  create.col.RFS1(1)%>%
  create.col.RFS1(2)%>%
  dplyr::select(nuc.power01, nuc.power02, CO2Price0, ban.fuel.explore01, ban.fuel.explore02, RFS01, RFS02, nuc.power11, nuc.power12, CO2Price1, ban.fuel.explore11, ban.fuel.explore12, RFS11, RFS12)

ut.matrix <- total.design.test.1%>%
  dplyr::select(nuc.power01, nuc.power02, CO2Price0, ban.fuel.explore01, ban.fuel.explore02, RFS01, RFS02)%>%
  mutate(id = rep(1:nrow(total.design.test.1)))



colnames(ut.matrix) <- c("nuc.power1", "nuc.power2", "CO2Price", "ban.fuel.explore1", "ban.fuel.explore2", "RFS1", "RFS2", "id")

utility.matrix <- as.matrix(total.design.test.1[,1:7] - total.design.test.1[,8:14])

colnames(utility.matrix) <- c("nuc.power1", "nuc.power2", "CO2Price", "ban.fuel.explore1", "ban.fuel.explore2", "RFS1", "RFS2")

#Generating individual  utility values given simulated parameter values 
utility.generation <- function(utility.matrix, b1, b2, b3, b4, b5, b6, b7){
  
  b0value <- c()
  b1value <- c()
  b2value <- c()
  b3value <- c()
  b4value <- c()
  b5value <- c()
  b6value <- c()
  b7value <- c()
  
  b1 <- b1
  b2 <- b2
  b3 <- b3/150
  b4 <- b4
  b5 <- b5
  b6 <- b6
  b7 <- b7
  
  total.design.ut <- as.data.frame(utility.matrix)%>%
    mutate(id = rep(1:nrow(utility.matrix)))%>%
    mutate(utility = b1*nuc.power1 + b2*nuc.power2 + b3*CO2Price + b4*ban.fuel.explore1 + b5*ban.fuel.explore2 + b6*RFS1 + b7*RFS2)%>%
    mutate(p = (exp(utility)/(1+exp(utility))))
  
  p.values <- total.design.ut$p
  id.sim <- rep(1, nrow(utility.matrix))
  Choice <- rbinom(nrow(utility.matrix), 1, total.design.ut$p)
  
  total.design.simulate <- cbind(total.design.test, p.values, Choice, id.sim)%>%
    mutate(Choices = ifelse(Choice == "0", 1, 0))%>%
    mutate(nuc.power_0 = as.factor(nuc.power0))%>%
    mutate(nuc.power_1 = as.factor(nuc.power1))%>%
    mutate(CO2Price_0 = CO2Price0)%>%
    mutate(CO2Price_1 = CO2Price1)%>%
    mutate(ban.fuel.explore_0 = as.factor(ban.fuel.explore0))%>%
    mutate(ban.fuel.explore_1 = as.factor(ban.fuel.explore1))%>%
    mutate(RFS_0 = as.factor(RFS0))%>%
    mutate(RFS_1 = as.factor(RFS1))%>%
    dplyr::select(id.sim, Choices, nuc.power_0, nuc.power_1, CO2Price_0, CO2Price_1, ban.fuel.explore_0, ban.fuel.explore_1, RFS_0, RFS_1)
  
  dataset.sim <- mlogit.data(total.design.simulate, shape = "wide", choice = "Choices", varying = c(3:10), sep = "_")
  
  sim.utility <- glm(Choices ~ nuc.power + CO2Price + ban.fuel.explore + RFS, data = dataset.sim, family = binomial(link = "logit"))
  
  
  b0t <- sim.utility$coefficients[2]
  b1t <- (sim.utility$coefficients[3])
  b2t <- (sim.utility$coefficients[4]/150)
  b3t <- sim.utility$coefficients[5]
  b4t <- (sim.utility$coefficients[6])
  b5t <- sim.utility$coefficients[7] 
  b6t <- sim.utility$coefficients[8] 
  b7t <- sim.utility$coefficients[1]
  
  
  mylist <- list(b0t, b1t, b2t, b3t, b4t, b5t, b6t, b7t)
  return(mylist)
  
}

#Generate simulated individual utilities for each policy alternative given simulated parameters
utility.calculation <- function(utility.matrix, b1, b2, b3, b4, b5, b6, b7, b8){
  
  total.design.ut <- as.data.frame(utility.matrix)%>%
    mutate(id = rep(1:nrow(utility.matrix)))%>%
    mutate(utility = b1*nuc.power1 + b2*nuc.power2 + b3*CO2Price + b4*ban.fuel.explore1 + b5*ban.fuel.explore2 + b6*RFS1 + b7*RFS2 + b8)
  
  return(total.design.ut)
  
}

#Simulate four participants
p1 <- utility.generation(utility.matrix, 0.5, 0.7, 10, 0.3, 0.6, 0.2, 1)
p2 <- utility.generation(utility.matrix, 0.1, 1, 25, 0, 1, 0.5, 1.5)
p3 <- utility.generation(utility.matrix, -0.2, 0.5, 30, -0.5, 1, 0.3, 1)
p4 <- utility.generation(utility.matrix, 0.1, -0.5, -10, -0.4, 0, -0.2, -0.3)

p1.utility <- utility.calculation(ut.matrix, p1[[1]], p1[[2]], p1[[3]]*150, p1[[4]], p1[[5]], p1[[6]], p1[[7]], p1[[8]])%>%
  dplyr::select(id, utility)
p2.utility <- utility.calculation(ut.matrix, p2[[1]], p2[[2]], p2[[3]]*150, p2[[4]], p2[[5]], p2[[6]], p2[[7]], p2[[8]])%>%
  dplyr::select(id, utility)
p3.utility <- utility.calculation(ut.matrix, p3[[1]], p3[[2]], p3[[3]]*150, p3[[4]], p3[[5]], p3[[6]], p3[[7]], p3[[8]])%>%
  dplyr::select(id, utility)
p4.utility <- utility.calculation(ut.matrix, p4[[1]], p4[[2]], p4[[3]]*150, p4[[4]], p4[[5]], p4[[6]], p4[[7]], p4[[8]])%>%
  dplyr::select(id, utility)

group.utility <- ut.matrix%>%
  inner_join(p1.utility, by = "id")%>%
  inner_join(p2.utility, by = "id")%>%
  inner_join(p3.utility, by = "id")%>%
  inner_join(p4.utility, by = "id")

colnames(group.utility) <- c("nuc.power1", "nuc.power2", "CO2Price", "ban.fuel.explore1", "ban.fuel.explore2", "RFS1", "RFS2", "id", "p1.utility", "p2.utility", "p3.utility", "p4.utility")

group.utility <- group.utility%>%
  mutate(group.score = (0.25*p1.utility + 0.25*p2.utility + 0.25 * p3.utility + 0.25 * p4.utility))%>%
  arrange(desc(group.score))

#Weighted sum model - randomly selecting t pairs of alternatives for pairwise comparison
group.recommendation.random <- function(t){
  
  new.data <- group.utility[c(1, sample(2:162, 14)),]
  new.data <- new.data%>%
    mutate(p1.vote = exp(p1.utility)/(1+exp(p1.utility)))%>%
    mutate(p2.vote = exp(p2.utility)/(1+exp(p2.utility)))%>%
    mutate(p3.vote = exp(p3.utility)/(1+exp(p3.utility)))%>%
    mutate(p4.vote = exp(p4.utility)/(1+exp(p4.utility)))%>%
    mutate(p1.choice = as.numeric(rbinom(n(), 1, p1.vote)))%>%
    mutate(p2.choice = as.numeric(rbinom(n(), 1, p2.vote)))%>%
    mutate(p3.choice = as.numeric(rbinom(n(), 1, p3.vote)))%>%
    mutate(p4.choice = as.numeric(rbinom(n(), 1, p4.vote)))
  
  
  u.diagnostic <- c()
  u.ranking <- c()
  u.id <- c()
  
  all.utility <- c()
  all.models <- c()
  all.covmat <- c()
  
  for (j in 2:t){
    
    id.chosen <- new.data$id
    
    outcome.choice <- as.matrix(new.data[,c("p1.choice", "p2.choice", "p3.choice", "p4.choice")])
    
    o.choice <- as.data.frame(outcome.choice)%>%
      mutate(yes = p1.choice + p2.choice + p3.choice + p4.choice)%>%
      mutate(no = 4 - yes)%>%
      dplyr::select(yes, no)
    
    o.choice <- as.matrix(o.choice)
    
    p1.ut <- group.utility$p1.utility
    p2.ut <- group.utility$p2.utility
    p3.ut <- group.utility$p3.utility
    p4.ut <- group.utility$p4.utility
    
    m1.model <- stan_glm(formula = o.choice ~ 1 + (p1.utility) + (p2.utility) + (p3.utility) + (p4.utility), data = new.data, family = binomial(), 
                         prior = normal())
    
    all.models[[j]] <- m1.model$coefficients
    all.covmat[[j]] <- m1.model$covmat
    
    new.alt.id <- sample(1:162, 1)
    new.alt <- group.utility%>%
      dplyr::filter(id == new.alt.id)
    
    u.id[j] <- new.alt.id
    u.diagnostic[j] <- new.ranking$new.utility[1]
    u.ranking[j] <- which(new.ranking$id == new.alt.id)
    
    
    new.alter <- new.alt%>%
      mutate(p1.vote = exp(p1.utility)/(1+exp(p1.utility)))%>%
      mutate(p2.vote = exp(p2.utility)/(1+exp(p2.utility)))%>%
      mutate(p3.vote = exp(p3.utility)/(1+exp(p3.utility)))%>%
      mutate(p4.vote = exp(p4.utility)/(1+exp(p4.utility)))%>%
      mutate(p1.choice = as.numeric(rbinom(n(), 1, p1.vote)))%>%
      mutate(p2.choice = as.numeric(rbinom(n(), 1, p2.vote)))%>%
      mutate(p3.choice = as.numeric(rbinom(n(), 1, p3.vote)))%>%
      mutate(p4.choice = as.numeric(rbinom(n(), 1, p4.vote)))
    
    new.data <- rbind(new.data, new.alter)
    
    iteration.vector <- rep(j, 162)
    
    diag.util <- cbind(new.utility, iteration.vector)
    
    if(j == 2){
      
      all.utility <- diag.util
      
    }
    else{
      
      all.utility <- rbind(all.utility, diag.util)
      
    }
    
    
  }
  
  
  my.list <- list(new.data, all.models, all.covmat, counter, id.chosen, new.utility, u.id, u.ranking, u.diagnostic, all.utility)
  
  return(my.list)
  
  
}

#Random group selection testing function, specify number of runs and number of t pairs
random.group.search <- function(nruns, t){
  
  correlation <- c()
  regret <- c()
  
  for(i in 1:nruns){
    
    random.search <- group.recommendation.random(t)
    
    random.ranking <- as.matrix(people.utility)%*%as.matrix(random.search[2][[1]][[15]][2:5])
    
    correlation[i] <- cor(random.ranking, group.utility$group.score)
    
    print(correlation)
    
    group.utility1 <- group.utility%>%
      mutate(new.score = random.search[2][[1]][[15]][2]*p1.utility + random.search[2][[1]][[15]][3]*p2.utility + random.search[2][[1]][[15]][4] * p3.utility 
             + random.search[2][[1]][[15]][5] * p4.utility)%>%
      arrange(desc(new.score))
    
    regret[i] <- group.utility$group.score[1] - group.utility1$group.score[1]
    print(regret)
  }
  
  mylist <- list(correlation, regret)
  
  return(mylist)
}

#Running the random pairwise selection algorithm 
test.random <- random.group.search(5, 15)
test.random2 <- random.group.search(10, 15)
test.random3 <- random.group.search(10, 15)
test.random4 <- random.group.search(10, 15)
test.random5 <- random.group.search(10, 15)
test.random6 <- random.group.search(10, 15)
test.random7 <- random.group.search(10, 15)
test.random8 <- random.group.search(15, 15)

random.correlation <- list.append(test.random[[1]], test.random2[[1]], test.random3[[1]], test.random4[[1]], test.random5[[1]], test.random6[[1]], test.random7[[1]], test.random8[[1]])

mean(random.correlation)
var(random.correlation)

random.regret <- list.append(test.random[[2]], test.random2[[2]], test.random3[[2]], test.random4[[2]], test.random5[[2]], test.random6[[2]], test.random7[[2]], test.random8[[2]])

mean(random.regret)
var(random.regret)


#Weighted sum model - using stanglm to find all 15 group pairwise comparisons using a greedy approach
group.recommendation <- function(t){
  
  
  new.data <- group.utility[c(1, sample(2:162, 1)),]
  new.data <- new.data%>%
    mutate(p1.vote = exp(p1.utility)/(1+exp(p1.utility)))%>%
    mutate(p2.vote = exp(p2.utility)/(1+exp(p2.utility)))%>%
    mutate(p3.vote = exp(p3.utility)/(1+exp(p3.utility)))%>%
    mutate(p4.vote = exp(p4.utility)/(1+exp(p4.utility)))%>%
    mutate(p1.choice = as.numeric(rbinom(n(), 1, p1.vote)))%>%
    mutate(p2.choice = as.numeric(rbinom(n(), 1, p2.vote)))%>%
    mutate(p3.choice = as.numeric(rbinom(n(), 1, p3.vote)))%>%
    mutate(p4.choice = as.numeric(rbinom(n(), 1, p4.vote)))
  
  
  u.diagnostic <- c()
  u.ranking <- c()
  u.id <- c()
  
  all.utility <- c()
  all.covmat <- c()
  all.models <- c()
  
  for (j in 2:t){
    
    id.chosen <- new.data$id
    
    outcome.choice <- as.matrix(new.data[,c("p1.choice", "p2.choice", "p3.choice", "p4.choice")])
    
    o.choice <- as.data.frame(outcome.choice)%>%
      mutate(yes = p1.choice + p2.choice + p3.choice + p4.choice)%>%
      mutate(no = 4 - yes)%>%
      dplyr::select(yes, no)
    
    o.choice <- as.matrix(o.choice)
    
    p1.ut <- group.utility$p1.utility
    p2.ut <- group.utility$p2.utility
    p3.ut <- group.utility$p3.utility
    p4.ut <- group.utility$p4.utility
    
    m1.model <- stan_glm(formula = o.choice ~ 1 + (p1.utility) + (p2.utility) + (p3.utility) + (p4.utility), data = new.data, family = binomial(), 
                         prior = normal())
    
    all.models[[j]] <- m1.model$coefficients
    
    all.covmat[[j]] <- m1.model$covmat
    
    coef.model <- m1.model$coefficients
    
    var.model <- diag(m1.model$covmat)
    
    
    var.left <- p1.ut^2*var.model[2]+p2.ut^2*var.model[3] + p3.ut^2 * var.model[4] + p4.ut^2 * var.model[5]
    
    var.right <- p1.ut*p2.ut*m1.model$covmat[2, 3] + p1.ut*p3.ut*m1.model$covmat[2, 4] + p1.ut * p4.ut * m1.model$covmat[2, 5] +
      p2.ut*p3.ut*m1.model$covmat[3, 4] + p2.ut*p4.ut*m1.model$covmat[3, 5] + p3.ut*p4.ut * m1.model$covmat[4, 5]
    
    var.total <- var.left + var.right
    
    people.utility <- group.utility[, 9:12]
    
    mean.total <- as.matrix(people.utility)%*%m1.model$coefficients[2:5]
    
    beta.init <- 2* log((4*j^2*pi^2)/(6))
    
    new.utility <- as.data.frame(cbind(group.utility, var.total, mean.total))%>%
      mutate(new.utilities = mean.total + sqrt(beta.init)*sqrt(var.total))
    
    
    max.utility.row <- which.max(new.utility$new.utilities)
    
    # for (i in 1:length(id.chosen)){
    #   
    #   if (id.chosen[i] == new.utility$id[max.utility.row]){
    #     counter <- counter + 1
    #     
    #   }
    #   else{
    #     counter <- counter + 0
    #   }
    # }
    # 
    new.ranking <- new.utility%>%
      arrange(desc(new.utilities))
    
    new.alt.id <- new.ranking$id[1]
    new.alt <- group.utility%>%
      dplyr::filter(id == new.alt.id)
    
    u.id[j] <- new.alt.id
    u.diagnostic[j] <- new.ranking$new.utility[1]
    u.ranking[j] <- which(new.ranking$id == new.alt.id)
    
    
    new.alter <- new.alt%>%
      mutate(p1.vote = exp(p1.utility)/(1+exp(p1.utility)))%>%
      mutate(p2.vote = exp(p2.utility)/(1+exp(p2.utility)))%>%
      mutate(p3.vote = exp(p3.utility)/(1+exp(p3.utility)))%>%
      mutate(p4.vote = exp(p4.utility)/(1+exp(p4.utility)))%>%
      mutate(p1.choice = as.numeric(rbinom(n(), 1, p1.vote)))%>%
      mutate(p2.choice = as.numeric(rbinom(n(), 1, p2.vote)))%>%
      mutate(p3.choice = as.numeric(rbinom(n(), 1, p3.vote)))%>%
      mutate(p4.choice = as.numeric(rbinom(n(), 1, p4.vote)))
    
    new.data <- rbind(new.data, new.alter)
    
    iteration.vector <- rep(j, 162)
    
    diag.util <- cbind(new.utility, iteration.vector)
    
    if(j == 2){
      
      all.utility <- diag.util
      
    }
    else{
      
      all.utility <- rbind(all.utility, diag.util)
      
    }
    
    
  }
  
  
  my.list <- list(new.data, all.models, all.covmat, counter, id.chosen, new.utility, u.id, u.ranking, u.diagnostic, all.utility)
  
  return(my.list)
  
}

#Weighted sum model - using doptimal approach with four individual utility vectors as variabels for the d-optimal experiment design
group.recommendation.doptimalonly <- function(t){
  
  
  new.data <- group.utility
  optimal.design.matrix <- as.matrix(group.utility[8:12])
  
  doptimaldesign <- Dopt.design(t, data = optimal.design.matrix, formula = ~p1.utility + p2.utility + p3.utility + p4.utility)
  
  id.optimal <- doptimaldesign$id
  
  optimal.data <- subset(group.utility, id %in% id.optimal)
  
  new.data <- optimal.data%>%
    mutate(p1.vote = exp(p1.utility)/(1+exp(p1.utility)))%>%
    mutate(p2.vote = exp(p2.utility)/(1+exp(p2.utility)))%>%
    mutate(p3.vote = exp(p3.utility)/(1+exp(p3.utility)))%>%
    mutate(p4.vote = exp(p4.utility)/(1+exp(p4.utility)))%>%
    mutate(p1.choice = as.numeric(rbinom(n(), 1, p1.vote)))%>%
    mutate(p2.choice = as.numeric(rbinom(n(), 1, p2.vote)))%>%
    mutate(p3.choice = as.numeric(rbinom(n(), 1, p3.vote)))%>%
    mutate(p4.choice = as.numeric(rbinom(n(), 1, p4.vote)))
  
  
  outcome.choice <- as.matrix(new.data[,c("p1.choice", "p2.choice", "p3.choice", "p4.choice")])
  
  o.choice <- as.data.frame(outcome.choice)%>%
    mutate(yes = p1.choice + p2.choice + p3.choice + p4.choice)%>%
    mutate(no = 4 - yes)%>%
    dplyr::select(yes, no)
  
  o.choice <- as.matrix(o.choice)
  
  m1.model <- stan_glm(formula = o.choice ~ 1 + (p1.utility) + (p2.utility) + (p3.utility) + (p4.utility), data = new.data, family = binomial(), 
                       prior = normal())
  
  all.models[[j]] <- m1.model$coefficients
  all.covmat[[j]] <- m1.model$covmat
  
  
  my.list <- list(new.data, model.coefficients, model.fitted, m1.model)
  
  return(my.list)
  
}


test.random <- random.group.search(5, 15)
test.random2 <- random.group.search(10, 15)
test.random3 <- random.group.search(10,15)
test.random4 <- random.group.search(10, 15)
test.random5 <- random.group.search(10, 15)
test.random6 <- random.group.search(10, 15)
test.random7 <- random.group.search(10, 15)
test.random8 <- random.group.search(15, 15)

random.correlation <- list.append(test.random[[1]], test.random2[[1]], test.random3[[1]], test.random4[[1]], test.random5[[1]], test.random6[[1]], test.random7[[1]], test.random8[[1]])

mean.correlation <- cbind(mean(random.correlation), mean(doptimal.correlation), mean(upperconfidence.correlation), mean(hybrid.correlation), mean(test.hybrid2.1[[1]]))
sd.correlation <- cbind(sd(random.correlation), sd(doptimal.correlation), sd(upperconfidence.correlation), sd(hybrid.correlation), sd(test.hybrid2.1[[1]]))

boxplot(random.correlation, doptimal.correlation, upperconfidence.correlation)

mean.regret <- cbind(mean(random.regret), mean(doptimal.regret), mean(upperconfidence.regret), mean(hybrid.regret), mean(test.hybrid2.1[[2]]))
sd.regret <- cbind(sd(random.regret), sd(doptimal.regret), sd(upperconfidence.regret), sd(hybrid.regret))

boxplot(random.correlation, doptimal.correlation, upperconfidence.correlation, hybrid.correlation, test.hybrid2.1[[1]],
        main = "Correlation of Recommendation Algorithm",
        names = c("Random", "Information Maximation", "UCB", "Hybrid (5)", "Hybrid (2)"),
        xlab = "Algorithms",
        ylab = "Correlation with group truth")


boxplot(random.regret, doptimal.regret, upperconfidence.regret, hybrid.regret, test.hybrid2.1[[2]],
        main = "Regret of Recommendation Algorithm",
        names = c("Random", "Information Maximation", "UCB", "Hybrid (5)", "Hybrid (2)"),
        xlab = "Algorithms",
        ylab = "Regret with group truth")
