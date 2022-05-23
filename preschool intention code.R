library(tidyverse)
library(ggplot2)
library(lme4)
library(dplyr)
library(esc)
library(ggsignif)
library(aod)
library(DescTools)
library(BayesFactor)
library(ggpubr)
rm(list=ls())

setwd("~/Dropbox/Research Projects/Developmental Work/preschool trolley paper/final analysis_github")


##############
##  STUDY 1 ###
##############

data.w<-read.csv("study1.csv")
data<-data.w %>%
  gather(question,answer,-c(1:24,27:32),na.rm=TRUE) 
data<-data[data$excluded==0,]
data$condition[data$condition=="taking"]<-"Taking"
data$condition[data$condition=="Taking"]<-"Means+Stealing"
data$condition[data$condition=="Anvil"]<-"SideEffect"
data$condition[data$condition=="Loop"]<-"Means"
data$question = factor(data$question)
data$condition = factor(data$condition)

data$agebin<-NA
data$agebin[data$age>3 & data$age<=4]<-3
data$agebin[data$age>4 & data$age<=5]<-4
data$agebin[data$age>5 & data$age<=6]<-5
data$agebin[data$age>6 & data$age<=7]<-6

mean(data.w$age[data.w$excluded==0],na.rm=T)
#mean age = 4.9


######descriptive stats of participants in study 1#######

#ages
data.ages1 <- data %>%
  filter(question=="should")%>%
  summarize(
    n = length(answer),
    mean = mean(age, na.rm = T),
    sd = sd(age, na.rm=T),
    min = min(age, na.rm=T),
    max = max(age, na.rm=T)
  )

#ages grouped by condition
data.ages2 <- data %>%
  filter(question=="should")%>%
  group_by(condition) %>%
  summarize(
    n = length(answer),
    mean = mean(age, na.rm = T)
  )

#ages grouped by agebin
data.ages3 <-data %>%
  filter(question=="should")%>%
  group_by(agebin) %>%
  summarize(
    n = length(answer)
  )


#is there a significant difference in ages across conditions?
df5<-data[data$question=="should",]
df5<-df5[c(29,26)]
df5<-df5[complete.cases(df5),]
age.model<- aov(age ~ condition, data=df5) #p=0.0416 

#how many subjects of each gender in the data set?
data.gender <-data %>%
  filter(question=="should")%>%
  group_by(gender) %>%
  summarize(
    n = length(answer)
  )

#summary statistics for DVs

#summarize data, should questions
data.sum.should <- data %>%
  filter(question=="should")%>%
  group_by(condition) %>%
  summarize(
    n = length(answer),
    sum = sum(answer, na.rm=T),
    mean = mean(answer, na.rm = T), 
    se = sqrt(mean*(1-mean)/n),
    error = qnorm(0.975)*se, 
    CI.left = mean-error,
    CI.right = mean+error
  )

#summarize data, likert scale ("pink scale")
data.sum.pink <- data %>%
  filter(question=="pinkscale")%>%
  group_by(condition) %>%
  summarize(
    n = length(answer),
    mean = mean(answer, na.rm = T),
    sd = sd(answer, na.rm = T), 
    se = sd/sqrt(n),
    error = qnorm(0.975)*sd/sqrt(n),
    CI.left = mean-error,
    CI.right = mean+error
  )



####Study 1 Analysis and Graphs######

#graph of should question, final for paper
data.sum.should %>%
  ggplot(aes(y=mean, x=condition, fill="blue")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_bar(position="dodge", stat="identity", width = 0.7, color="black")+
  coord_cartesian(ylim = c(0, 1))+  
  geom_errorbar(aes(ymin=mean-error, ymax=mean+error), width=.2,position=position_dodge(.9))+  
  xlab("Condition")+
  ylab("Proportion of subjects answering 'yes'")+
  theme(legend.position = "none")+
  geom_text(x = 2, y = .93, label = "*", size=10)+
  geom_text(x = 1.5, y = .52, label = "n.s.", size=5)+
  geom_segment(aes(x = 1, y = .48, xend = 2, yend = .48))+
  geom_segment(aes(x = 1, y = .88, xend = 3, yend = .88))+
  theme(axis.title.x = element_text(face="bold", size=14, margin = margin(t = 15)),
        axis.text.x  = element_text(vjust=0.5, size=14),
        axis.title.y = element_text(face="bold", size=14, margin = margin(r = 15)),
        axis.text.y  = element_text(vjust=0.5, size=14))+
  scale_fill_manual(values="#6666FF")

#logistic regression for should question, including age as a predictor

data2<-data[data$question=="should",]
mean=mean(data2$age,na.rm=T)
data2$age[is.na(data2$age)]<-mean

logit1<- glm(answer ~ condition, data = data2, family = "binomial") 
summary(logit1)

logit2<- glm(answer ~ condition + age, data = data2, family = "binomial") #adds interaction
summary(logit2)

logit3<- glm(answer ~ condition*age, data = data2, family = "binomial") 
summary(logit3)


# visualizing the three models to compare them
ggplot2::ggsave(
  plot = ggstatsplot::combine_plots(
    ggstatsplot::ggcoefstats(logit1, title = "model-1"),
    ggstatsplot::ggcoefstats(logit2, title = "model-2"),
    ggstatsplot::ggcoefstats(logit3, title = "model-3"),
    nrow = 1
  ),
  filename = "model_comparison.png",
  height = 6,
  width = 15,
  units = "in",
  dpi = 300
)

#using ggstatsplots to visualize and analyze data

#should question, all conditions
ggplot2::ggsave(
  plot = ggstatsplot::ggbarstats(
    data = data2,
    x = answer,
    y = condition
    #bar.proptest = FALSE
  ),
  filename = "study_1_barchart.png"
) 


#should question, means vs side effect
data3<-data2[data2$condition!="Means+Stealing",]
ggplot2::ggsave(
  plot = ggstatsplot::ggbarstats(
    data = data3,
    x = answer,
    y = condition
    #bar.proptest = FALSE
  ),
  filename = "study_1_barchart3.png"
)

#should question, means vs means+stealing
data4<-data2[data2$condition!="SideEffect",]
ggplot2::ggsave(
  plot = ggstatsplot::ggbarstats(
    data = data4,
    x = answer,
    y = condition
    #bar.proptest = FALSE
  ),
  filename = "study_1_barchart4.png"
)


#Pinkscale (action rating) analysis

#regression including age
data5<-data[data$question=="pink.rating",]
lm1<- lm(answer ~ condition, data = data5) 
lm2<- lm(answer ~ condition + age, data = data5) 
lm3<- lm(answer ~ condition*age, data = data5) 
summary(lm1)
summary(lm2)
summary(lm3)

# visualizing models to compare them
ggplot2::ggsave(
  plot = ggstatsplot::combine_plots(
    ggstatsplot::ggcoefstats(lm1, title = "model-1"),
    ggstatsplot::ggcoefstats(lm2, title = "model-2"),
    ggstatsplot::ggcoefstats(lm3, title = "model-3"),
    nrow = 1
  ),
  filename = "model_comparison_likert1.png",
  height = 6,
  width = 15,
  units = "in",
  dpi = 300
)


set.seed(123)
# plot all conditions
ggplot2::ggsave(
  plot = ggstatsplot::ggbetweenstats(
    data = data5,
    x = condition,
    y = answer,
    xlab = "Condition",
    ylab = "Action Rating",    
    messages = FALSE
  ) + # further modification outside of ggstatsplot
    ggplot2::coord_cartesian(ylim = c(-2, 2)) +
    ggplot2::scale_y_continuous(breaks = seq(-2, 2, by = 1)),
  filename = "likert_dist1a.png",
  height = 5,
  width = 5,
  units = "in",
  dpi = 300)


#comparison of 2 conditions at a time
data6<-data5[data5$condition!="Means+Stealing",]
data7<-data5[data5$condition!="SideEffect",]

t.test(data6$answer[data6$condition=="Means"],data6$answer[data6$condition=="SideEffect"])

set.seed(123)
# plot
ggplot2::ggsave(
  plot = ggstatsplot::ggbetweenstats(
    data = data6,
    x = condition,
    y = answer,
    messages = FALSE
  ) + 
    ggplot2::coord_cartesian(ylim = c(-2, 2)) +
    ggplot2::scale_y_continuous(breaks = seq(-2, 2, by = 1)),
  filename = "likert_dist1b.png",
  height = 5,
  width = 5,
  units = "in",
  dpi = 300)

# plot
ggplot2::ggsave(
  plot = ggstatsplot::ggbetweenstats(
    data = data7,
    x = condition,
    y = answer,
    messages = FALSE
  ) + 
    ggplot2::coord_cartesian(ylim = c(-2, 2)) +
    ggplot2::scale_y_continuous(breaks = seq(-2, 2, by = 1)),
  filename = "likert_dist1c.png",
  height = 5,
  width = 5,
  units = "in",
  dpi = 300)


##############
#  Study 1: Re-analysis for Supplement
#  the analysis is repeated from above without excluding anyone for failing controls
##############

data.full<-data.w %>%
  gather(question,answer,-c(1:24,27:32),na.rm=TRUE) 
data.full$condition[data.full$condition=="taking"]<-"Taking"
data.full$condition[data.full$condition=="Taking"]<-"Means+Stealing"
data.full$condition[data.full$condition=="Anvil"]<-"SideEffect"
data.full$condition[data.full$condition=="Loop"]<-"Means"
data.full$question = factor(data.full$question)
data.full$condition = factor(data.full$condition)

data.full$agebin<-NA
data.full$agebin[data.full$age>3 & data.full$age<=4]<-3
data.full$agebin[data.full$age>4 & data.full$age<=5]<-4
data.full$agebin[data.full$age>5 & data.full$age<=6]<-5
data.full$agebin[data.full$age>6 & data.full$age<=7]<-6

mean(data.w$age,na.rm=T)
#mean age = 4.9

#summarize data.full, should questions
data.full.sum.should <- data.full %>%
  filter(question=="should")%>%
  group_by(condition) %>%
  summarize(
    n = length(answer),
    sum = sum(answer, na.rm=T),
    mean = mean(answer, na.rm = T), 
    se = sqrt(mean*(1-mean)/n),
    error = qnorm(0.975)*se, 
    CI.left = mean-error,
    CI.right = mean+error
  )

#summarize data.full, likert scale ("pink scale")
data.full.sum.pink <- data.full %>%
  filter(question=="pink.rating")%>%
  group_by(condition) %>%
  summarize(
    n = length(answer),
    mean = mean(answer, na.rm = T),
    sd = sd(answer, na.rm = T), 
    se = sd/sqrt(n),
    error = qnorm(0.975)*sd/sqrt(n),
    CI.left = mean-error,
    CI.right = mean+error
  )

###graphs and analysis###

#graph of should data
data.full.sum.should %>%
  ggplot(aes(y=mean, x=condition, fill="blue")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_bar(position="dodge", stat="identity", width = 0.7, color="black")+
  coord_cartesian(ylim = c(0, 1))+  
  geom_errorbar(aes(ymin=mean-error, ymax=mean+error), width=.2,position=position_dodge(.9))+  
  xlab("Condition")+
  ylab("Proportion of subjects answering 'yes'")+
  theme(legend.position = "none")+
  geom_text(x = 2, y = .93, label = "*", size=10)+
  geom_text(x = 1.5, y = .52, label = "n.s.", size=5)+
  geom_segment(aes(x = 1, y = .48, xend = 2, yend = .48))+
  geom_segment(aes(x = 1, y = .88, xend = 3, yend = .88))+
  theme(axis.title.x = element_text(face="bold", size=14, margin = margin(t = 15)),
        axis.text.x  = element_text(vjust=0.5, size=14),
        axis.title.y = element_text(face="bold", size=14, margin = margin(r = 15)),
        axis.text.y  = element_text(vjust=0.5, size=14))+
  scale_fill_manual(values="#6666FF")


#regression
data.full2<-data.full[data.full$question=="should",]
logit1<- glm(answer ~ condition, data = data2, family = "binomial") 
summary(logit1)

#graphs created with ggstatsplots

#all conditions, should question
ggplot2::ggsave(
  plot = ggstatsplot::ggbarstats(
    data = data.full2,
    x = answer,
    y = condition
    #bar.proptest = FALSE
  ),
  filename = "study_1supp_barchart.png"
) 


d.full2<-data.full
d.full2<-d.full2[d.full2$question=="should",]
d.full3<-d.full2[d.full2$condition!="Means+Stealing",]
d.full4<-d.full2[d.full2$condition!="SideEffect",]

#means vs side-effect
ggplot2::ggsave(
  plot = ggstatsplot::ggbarstats(
    data = d.full3,
    x = answer,
    y = condition,
    bar.proptest = FALSE,
    ggstatsplot.layer = FALSE, # turn off ggstatsplot theme layer
    package = "wesanderson", # package from which color palette is to be taken
    palette = "Royal1", # choosing a different color palette
  ),
  filename = "bayes5.png",
  height = 4,
  width = 5.2,
  units = "in",
  dpi = 300
)

#means vs means+stealing
ggplot2::ggsave(
  plot = ggstatsplot::ggbarstats(
    data = d.full4,
    x = answer,
    y = condition,
    bar.proptest = FALSE,
    ggstatsplot.layer = FALSE, # turn off ggstatsplot theme layer
    package = "wesanderson", # package from which color palette is to be taken
    palette = "Royal1", # choosing a different color palette
  ),
  filename = "bayes6.png",
  height = 4,
  width = 5.2,
  units = "in",
  dpi = 300
)

#pink scale analysis and plots
data.full5<-data.full[data.full$question=="pink.rating",]
lm1<- lm(answer ~ condition, data = data.full5) 


set.seed(123)
# plot
ggplot2::ggsave(
  plot = ggstatsplot::ggbetweenstats(
    data = data.full5,
    x = condition,
    y = answer,
    xlab = "Condition",
    ylab = "Action Rating",    
    messages = FALSE
  ) + # further modification outside of ggstatsplot
    ggplot2::coord_cartesian(ylim = c(-2, 2)) +
    ggplot2::scale_y_continuous(breaks = seq(-2, 2, by = 1)),
  filename = "likert_dist1a_supp.png",
  height = 5,
  width = 5,
  units = "in",
  dpi = 300)


#comparison of 2 conditions at a time
data.full6<-data.full5[data.full5$condition!="Means+Stealing",]
data.full7<-data.full5[data.full5$condition!="SideEffect",]

t.test(data.full6$answer[data.full6$condition=="Means"],data.full6$answer[data.full6$condition=="SideEffect"])

set.seed(123)
# plot
ggplot2::ggsave(
  plot = ggstatsplot::ggbetweenstats(
    data.full = data.full6,
    x = condition,
    y = answer,
    messages = FALSE
  ) + 
    ggplot2::coord_cartesian(ylim = c(-2, 2)) +
    ggplot2::scale_y_continuous(breaks = seq(-2, 2, by = 1)),
  filename = "likert_dist1b.png",
  height = 5,
  width = 5,
  units = "in",
  dpi = 300)

# plot
ggplot2::ggsave(
  plot = ggstatsplot::ggbetweenstats(
    data.full = data.full7,
    x = condition,
    y = answer,
    messages = FALSE
  ) + 
    ggplot2::coord_cartesian(ylim = c(-2, 2)) +
    ggplot2::scale_y_continuous(breaks = seq(-2, 2, by = 1)),
  filename = "likert_dist1c.png",
  height = 5,
  width = 5,
  units = "in",
  dpi = 300)


####################
###  STUDY 2
###################

rep.w<-read.csv("study2.csv")
length(rep.w$subjectnumber) #n=92
rep.exclude<-rep.w[rep.w$excluded==1,]
rep.w<-rep.w[rep.w$excluded==0,]
length(rep.w$subjectnumber) #n=74
rep<-rep.w %>%
  gather(question,answer,-c(1:25),na.rm=TRUE) 
rep$question = factor(rep$question)
rep$condition = factor(rep$condition)
rep$agebin<-NA
rep$agebin[rep$age>3 & rep$age<=4]<-3
rep$agebin[rep$age>4 & rep$age<=5]<-4
rep$agebin[rep$age>5 & rep$age<=6]<-5
rep$agebin[rep$age>6 & rep$age<=7]<-6

#descriptive stats for subject ages
mean(rep.w$age) #5.40
sd(rep.w$age) #.91  
min(rep.w$age) #3.8
max(rep.w$age) #6.9

#gender count
rep.sex <- rep.w %>%
  group_by(Gender) %>%
  summarize(
    n = length(subjectnumber)
  )


#age count
rep.age <- rep %>%
  filter(question=="should")%>%
  group_by(agebin) %>%
  summarize(
    n = length(answer)
  )

#age by condition
rep.age2 <- rep %>%
  group_by(condition) %>%
  summarize(
    age = mean(age)
  )

#t test for difference in ages
t.test(rep.w$age[rep.w$condition=="1"],rep.w$age[rep.w$condition=="6"])

###descriptive stats for DVs#####

#summarize data, pinkscale 
rep.sum.pink <- rep %>%
  filter(question=="likert")%>%
  group_by(condition) %>%
  summarize(
    n = length(answer),
    mean = mean(answer, na.rm = T),
    sd = sd(answer, na.rm = T), 
    se = sd/sqrt(n),
    error = qnorm(0.975)*sd/sqrt(n),
    CI.left = mean-error,
    CI.right = mean+error
  )

#summarize data, should
rep.sum.should <- rep %>%
  filter(question=="should")%>%
  group_by(condition) %>%
  summarize(
    n = length(answer),
    mean = mean(answer, na.rm = T), 
    se = sqrt(mean*(1-mean)/n),
    error = qnorm(0.975)*se, 
    CI.left = mean-error,
    CI.right = mean+error
  )


rep2 <- rep %>%
  filter(question=="should")

#analysis reported in paper from this plot
#ggplot2 - should data
ggplot2::ggsave(
  plot = ggstatsplot::ggbarstats(
    data = rep2,
    x = answer,
    y = condition,
    bar.proptest = FALSE,
    ggstatsplot.layer = FALSE, # turn off ggstatsplot theme layer
    package = "wesanderson", # package from which color palette is to be taken
    palette = "Royal1", # choosing a different color palette
  ),
  filename = "bayes3.png",
  height = 4,
  width = 5.2,
  units = "in",
  dpi = 300
)

##HINT
#use 1/exp(x) to find bayes factor in favor of alternative hypothesis (H1) 
#where x is the ln of the BF in favor of the null (which is at the bottom of the ggstatsplots output)


rep3 <- rep %>%
  filter(question=="likert")
rep3$answer<-as.numeric(rep3$answer)

#ggplot2 - likert scale data ("pink scale")
ggplot2::ggsave(
  plot = ggstatsplot::ggbetweenstats(
    data = rep3,
    x = condition,
    y = answer,
    xlab = "Condition",
    ylab = "Action Rating",    
    messages = FALSE
  ) + # further modification outside of ggstatsplot
    ggplot2::coord_cartesian(ylim = c(-2, 2)) +
    ggplot2::scale_y_continuous(breaks = seq(-2, 2, by = 1)),
  filename = "bayes4.png",
  height = 5,
  width = 5,
  units = "in",
  dpi = 300) #spot

###Study 2 - Graph for paper####
rep.sum.should$label<-NA
rep.sum.should$label[rep.sum.should$condition==1]<-"Helps Victim"
rep.sum.should$label[rep.sum.should$condition==6]<-"Helps Others"

rep.sum.should %>%
  ggplot(aes(y=mean, x=label, fill="blue")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_bar(position="dodge", stat="identity", width = 0.7, color="black")+
  coord_cartesian(ylim = c(0, 1))+  
  geom_errorbar(aes(ymin=pmax(mean-error,0), ymax=mean+error), width=.2,position=position_dodge(.9))+  
  xlab("Condition")+
  ylab("Proportion of subjects answering 'yes'")+
  theme(legend.position = "none")+
  geom_text(x = 1.5, y = .65, label = "*", size=10)+
  geom_segment(aes(x = 1, y = .6, xend = 2, yend = .6))+
  theme(axis.title.x = element_text(face="bold", size=14, margin = margin(t = 15)),
        axis.text.x  = element_text(vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=14, margin = margin(r = 15)),
        axis.text.y  = element_text(vjust=0.5, size=12))+
  scale_fill_manual(values="#6666FF")

rep.should<-rep[rep$question=="should",]

logit7a<- glm(answer ~ condition, data = rep.should, family = "binomial")
logit7b<- glm(answer ~ condition + age, data = rep.should, family = "binomial")
logit7c<- glm(answer ~ condition*age, data = rep.should, family = "binomial")
summary(logit7a)
summary(logit7b)
summary(logit7c)


# visualizing models
ggplot2::ggsave(
  plot = ggstatsplot::combine_plots(
    ggstatsplot::ggcoefstats(logit7a, title = "model-1"),
    ggstatsplot::ggcoefstats(logit7b, title = "model-2"),
    ggstatsplot::ggcoefstats(logit7c, title = "model-3"),
    nrow = 1
  ),
  filename = "model_comparison5.png",
  height = 6,
  width = 15,
  units = "in",
  dpi = 300
)

#pink scale, study 2 
rep.pink<-rep[rep$question=="likert",]

lm7a<- lm(answer ~ condition, data = rep.pink) 
lm7b<- lm(answer ~ condition + age, data = rep.pink) 
lm7c<- lm(answer ~ condition*age, data = rep.pink) 
summary(lm7a)
summary(lm7b)
summary(lm7c)

#confidence intervals
confint(lm7b)

# visualizing models
ggplot2::ggsave(
  plot = ggstatsplot::combine_plots(
    ggstatsplot::ggcoefstats(lm7a, title = "model-1"),
    ggstatsplot::ggcoefstats(lm7b, title = "model-2"),
    ggstatsplot::ggcoefstats(lm7c, title = "model-3"),
    nrow = 1
  ),
  filename = "model_comparison_likert2_rep.png",
  height = 6,
  width = 15,
  units = "in",
  dpi = 300
)


set.seed(123)
# plot
rep.pink$label<-NA
rep.pink$label[rep.pink$condition==6]<-"Means Helps Others"
rep.pink$label[rep.pink$condition==1]<-"Means Helps Victim"
ggplot2::ggsave(
  plot = ggstatsplot::ggbetweenstats(
    data = rep.pink,
    x = label,
    y = answer,
    xlab = "Condition",
    ylab = "Action Rating",
    messages = FALSE
  ) + # further modification outside of ggstatsplot
    ggplot2::coord_cartesian(ylim = c(-2, 2)) +
    ggplot2::scale_y_continuous(breaks = seq(-2, 2, by = 1)),
  filename = "likert_dist7.png",
  height = 5,
  width = 5,
  units = "in",
  dpi = 300)

#graph of likert scale judgments by age
ggscatter(rep.pink, x = "age", y = "answer", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Age (years)", ylab = "Action Rating")


#########
# Study 2 replication
# appears in Section 4 of supplemental materials
##########


study2b<-read.csv("study2b.csv")
study2b$pinkscale<-as.numeric(study2b$pinkscale)

#gather data
study2b.l<- study2b %>%
  gather(question,answer,-c(1,4:9))
  

#summarize data, should question
study2b.should <- study2b.l %>%
  filter(question=="should")%>%
  group_by(condition) %>%
  summarize(
    n = length(answer),
    sum = sum(answer, na.rm=T),
    mean = mean(answer, na.rm = T), 
    se = sqrt(mean*(1-mean)/n),
    error = qnorm(0.975)*se, 
    CI.left = mean-error,
    CI.right = mean+error
  )

#summarize data, likert scale ("pink scale")
study2b.pink <- study2b.l %>%
  filter(question=="pinkscale")%>%
  group_by(condition) %>%
  summarize(
    n = length(answer),
    mean = mean(answer, na.rm = T),
    sd = sd(answer, na.rm = T), 
    se = sd/sqrt(n),
    error = qnorm(0.975)*sd/sqrt(n),
    CI.left = mean-error,
    CI.right = mean+error
  )


#should data
ggplot2::ggsave(
  plot = ggstatsplot::ggbarstats(
    data = study2b,
    x = should,
    y = condition,
    bar.proptest = FALSE,
    ggstatsplot.layer = FALSE, # turn off ggstatsplot theme layer
    package = "wesanderson", # package from which color palette is to be taken
    palette = "Royal1", # choosing a different color palette
  ),
  filename = "study2b-1.png",
  height = 4,
  width = 5.2,
  units = "in",
  dpi = 300
)


#pink scale data
ggplot2::ggsave(
  plot = ggstatsplot::ggbetweenstats(
    data = study2b,
    y = pinkscale,
    x = condition,
    xlab = "Condition",
    ylab = "Action Rating",    
    messages = FALSE
  ) + # further modification outside of ggstatsplot
    ggplot2::coord_cartesian(ylim = c(-2, 2)) +
    ggplot2::scale_y_continuous(breaks = seq(-2, 2, by = 1)),
  filename = "study2b-2.png",
  height = 5,
  width = 5,
  units = "in",
  dpi = 300)



#Study 2 comparisons

logit4<- glm(should ~ condition, data = study2b, family = "binomial")
logit5<- glm(should ~ condition + age, data = study2b, family = "binomial")
logit6<- glm(should ~ condition*age, data = study2b, family = "binomial")


# visualizing models
ggplot2::ggsave(
  plot = ggstatsplot::combine_plots(
    ggstatsplot::ggcoefstats(logit4, title = "model-1"),
    ggstatsplot::ggcoefstats(logit5, title = "model-2"),
    ggstatsplot::ggcoefstats(logit6, title = "model-3"),
    nrow = 1
  ),
  filename = "model_comparison2.png",
  height = 6,
  width = 15,
  units = "in",
  dpi = 300
)

#pink scale, study 2
lm4<- lm(pinkscale ~ condition, data = means_se) 
lm5<- lm(pinkscale ~ condition + age, data = means_se) 
lm6<- lm(pinkscale ~ condition*age, data = means_se) 


# visualizing models
ggplot2::ggsave(
  plot = ggstatsplot::combine_plots(
    ggstatsplot::ggcoefstats(lm4, title = "model-1"),
    ggstatsplot::ggcoefstats(lm5, title = "model-2"),
    ggstatsplot::ggcoefstats(lm6, title = "model-3"),
    nrow = 1
  ),
  filename = "model_comparison_likert2.png",
  height = 6,
  width = 15,
  units = "in",
  dpi = 300
)


#STUDY 2 GRAPH (for sup mats)

study2b.should %>%
  ggplot(aes(y=mean, x=condition, fill="blue")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_bar(position="dodge", stat="identity", width = 0.7, color="black")+
  coord_cartesian(ylim = c(0, 1))+  
  geom_errorbar(aes(ymin=mean-error, ymax=mean+error), width=.2,position=position_dodge(.9))+  
  xlab("Study")+
  ylab("Percent of subjects answering 'yes'")+
  theme(legend.position = "none")+
  geom_text(x = 1.5, y = .65, label = "*", size=10)+
  geom_segment(aes(x = 1, y = .6, xend = 2, yend = .6))+
  theme(axis.title.x = element_text(face="bold", size=14, margin = margin(t = 15)),
        axis.text.x  = element_text(vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=14, margin = margin(r = 15)),
        axis.text.y  = element_text(vjust=0.5, size=12))+
  scale_fill_manual(values="#6666FF")


