library(tidyverse)


#Clunky but effective, one could alter the sample size or call them from other variables if you wanted
#Make 1200 random outcomes (sample size is about 1100)
#HERE ARM 1 Has a True outcome Frequency of 25% and 600 participants
arm_1 <- as.data.frame(c(rep(1,150),rep(0,450)))
arm_1$arm <- 1
colnames(arm_1) <- c("Outcome","Arm")
#HERE ARM 2 Has a True outcome Frequency of 20% and 600 participants
arm_2 <- as.data.frame(c(rep(1,120),rep(0,480)))
arm_2$arm <- 2
colnames(arm_2) <- c("Outcome","Arm")
study <- bind_rows(arm_1,arm_2)


#Make a function which takes a random sub-sample of size N
#Do a prop test comparing within that sample the two arms
#Store the p_value
#Store the confidence interval
#Store the input size
f <- function (N) {
  arm_1_df <- sample_n(arm_1, N/2)
  arm_2_df <- sample_n(arm_2, N/2)
  result <- prop.test(c(length(arm_1_df$Outcome[arm_1_df$Outcome==1 ]),length(arm_2_df$Outcome[arm_2_df$Outcome==1])),c(length(arm_1_df$Outcome),length(arm_2_df$Outcome)))
  p_valee <- as.data.frame(as.numeric(result$p.value))
  conf1 <- as.data.frame(as.numeric(result$conf.int)[1])
  conf2 <- as.data.frame(as.numeric(result$conf.int)[2])
  size<-as.data.frame(N)
  outcome <- bind_cols(p_valee,conf1,conf2,size)
  return(outcome)
}

#Test the function at different sizes of sub-sample
#Make a list 20/50/100/200/500/1000
Nlist <- c(200,300,400,500,600,800,1000)
#Make a 10000 of each of these numbers  - this means we will independently replicate each sub-sample 10,000 times
Nlist <- rep(Nlist, each=10000)
#Run the f function on each of these replicates

#Turn everything into a data frame
p <- sapply(Nlist,f)
p <- t(p)
p <- as.data.frame(p)
names(p)[1] <- "P_Value"
names(p)[2] <- "LowerCI"
names(p)[3] <- "UpperCI"
names(p)[4] <- "SampleSize"
p$P_Value <- unlist(p$P_Value)
p$SampleSize <- unlist(p$SampleSize)

#Simplistically if the lower and upper bounds of the CI >0 then there is a true difference
#If they cross 0 then there is no difference
#If both are below 0 then we have incorrectly assumed a worse outcome
p <- mutate(p, decision=case_when(LowerCI>0 & UpperCI >0 ~ "Correct",
                      LowerCI<0 & UpperCI >0 ~ "Incorrect No Diff",
                      LowerCI<0 & UpperCI <0 ~ "Incorrect Wrong Direction"))

#Group by sample size and calculate the proportion of calls which are correct
decisions <- p %>% group_by(SampleSize) %>%
  summarise(Correct = (100/10000)*sum(decision=="Correct"), Incorrect_No_Diff= (100/10000)*sum(decision=="Incorrect No Diff"), Incorrect_Wrong_Direct= (100/10000)*sum(decision=="Incorrect Wrong Direction"))
