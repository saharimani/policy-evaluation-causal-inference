# Does free tutoring raise test scores? Policy evaluation with DiD, fixed effects, IV and fuzzy RDD
# EC2209 Econometrics 2 project, Royal Holloway, University of London (April 2023)
# Data: the three course-issued files are expected in data/ (not included; see README)

#Loading necessary libraries
library(readr)
library(openxlsx)
library(haven)
library(dplyr)
library(tidyr)
library(summarytools)
library(stargazer)
library(ggplot2)
library(knitr)
library(magrittr)
library(RColorBrewer)
library(sandwich)
library(lmtest)
library(glmnet)
library(plm)
library(AER)
library(Matrix)
library(namespace)
library(rdd)

#Question 1
#a
#Loading the three data sets into R
DOE_data<-read_csv("data/DOE_data.csv")
DOE_data<-DOE_data %>%
  select(-c(...1))#Removing unnecessary column from DOE_data

Child_data<-read_dta("data/Child_data.dta")

load("data/NED_data.RData")

#Checking column names
colnames(DOE_data)
colnames(Child_data)
colnames(NED_data)

#Merging the three datasets into one
merged_Child_DOE_datasets<-Child_data %>%
  left_join(DOE_data,by="personid")

merged_dataset<-merged_Child_DOE_datasets %>%
  left_join(NED_data,by="personid")

#b
#Converting the wide format dataset to long format
long_dataset<-merged_dataset%>%
  pivot_longer(cols= c(examgrade4,examgrade5),
               names_to="grade",
               values_to="exam_score")%>%
  mutate(grade=ifelse(grade== "examgrade4",4,5))

#c
#Counting number of observations
n_merged_datasets<-nrow(merged_dataset)
n_long_dataset<-nrow(long_dataset)

#Question 2
#a
#Creating descriptive summary statistics table
summary_stats<-descr(long_dataset)
stargazer(summary_stats,
          type="text",
          title="Summary Statistics",
          label="tab:summary_stats",
          table.placement = "ht",
          digits=2,
          align=TRUE)

#d
#Drawing histogram of exam scores for both grade 4 & 5
ggplot(long_dataset,aes(x=exam_score,fill = as.factor(grade)))+
  geom_histogram(binwidth=10,position="dodge",color="black")+
  ggtitle("Distribution of Exam Scores for Grades 4 and 5, Represented by Different Colours ") +
  labs(x="Exam Scores",y="Frequency",fill ="Grade")+ 
  scale_fill_brewer(palette="Pastel1")+
  theme_bw()

#Histogram of exam scores for grade 4
ggplot(data=long_dataset %>% 
         filter(grade==4),aes(x=exam_score))+
  geom_histogram(binwidth=10,color="black",fill ="orange")+
  ggtitle("Distribution of Exam Scores for Grade 4")+
  xlab("Exam Score")+
  ylab("Frequency")+
  theme_bw()

#Histogram of exam scores for grade 5
ggplot(data=long_dataset %>% 
         filter(grade== 5),aes(x = exam_score)) +
  geom_histogram(binwidth=10,color="black",fill ="green") +
  ggtitle("Distribution of Exam Scores for Grade 5") +
  xlab("Exam Score") +
  ylab("Frequency")+
  theme_bw()

#e
#Normalizing exam scores
long_dataset<-long_dataset%>%
  group_by(grade) %>%
  mutate(exam_score_norm=scale(exam_score))%>%
  ungroup()

#f
#Getting descriptive statistics for exam scores in grade 4 & 5
summary_stats_grade4<-descr(long_dataset %>% 
                              filter(grade==4)%>% 
                              select(exam_score_norm))

summary_stats_grade5<-descr(long_dataset %>% 
                              filter(grade ==5) %>% 
                              select(exam_score_norm))
#Binding summary statistics
summary_stats_combined<-bind_cols(Grade4=summary_stats_grade4,Grade5 =summary_stats_grade5)
summary_stats_combined<-tibble::tibble(Statistic=rownames(summary_stats_grade4),
                                         Grade4= summary_stats_combined$Grade4,
                                         Grade5= summary_stats_combined$Grade5)

#Creating table for normalized exam scores summary statistics
stargazer(summary_stats_combined,
          type="text",
          title="Summary Statistics for Normalized Exam Scores for each Grade",
          digits= 2,
          align =TRUE,
          summary=FALSE,
          column.labels = c("Grade 4","Grade 5"))

#Question 3
#a
#Defining dummy treatment variable
long_dataset$treat<-ifelse(long_dataset$tutor==1,1,0)

#b
#Fraction of obs in treatment group
frac_treatment <-mean(long_dataset$treat)
frac_treatment

#c
#Defining dummy post treatment variable
long_dataset$post<-ifelse(long_dataset$grade==5,1,0)

#d
#Reporting fraction of obs in post treatment period
frac_post_treatment<- mean(long_dataset$post)
frac_post_treatment

#e
#Creating crosstabulation to summarise number of obs
crosstab_n <-xtabs(~treat +post,data =long_dataset)
crosstab_n

#Tranforming crosstab to data frame and table
crosstab_df<-as.data.frame.matrix(crosstab_n)
rownames(crosstab_df)<- c("Control","Treatment")
colnames(crosstab_df) <-c("Pre-Policy(Grade 4)","Post-Policy(Grade 5)")

#Using kable library to present the results nicely
kable(crosstab_df,caption ="Crosstabulation of Observations by Treatment and Policy Period")

#f
#Calculating corr and drawing boxplot for Correlation Between Free Tutoring and Test Scores
correlation_treat_examscorenorm<-cor(long_dataset$treat,long_dataset$exam_score_norm)
cat("Correlation between free tutoring and exam grades:",correlation_treat_examscorenorm)

ggplot(long_dataset,aes(x=factor(treat),y=exam_score_norm,fill=factor(treat))) +
  geom_boxplot()+
  labs(title ="Correlation Between Free Tutoring and Test Scores",
       x ="Free Tutoring",
       y= "Normalized Test Scores",
       fill="Free Tutoring")+
  scale_x_discrete(labels=c("Control","Treatment"))+
  scale_fill_brewer(palette="Pastel1",labels=c("Control","Treatment")) +
  theme_bw()

#h
#Calculating average norm test score
average_norm_exam_scores<-long_dataset %>%
  group_by(treat, post)%>%
  summarise(avg_exam_score =mean(exam_score_norm))

#Creating cross table using xtabs, converting it to data frame matrix and using kable to present findings
crosstable_avg<- xtabs(avg_exam_score~treat+post,data=average_norm_exam_scores)
crosstable_avg_df<-as.data.frame.matrix(crosstable_avg)

rownames(crosstable_avg_df)<-c("Control","Treatment")
colnames(crosstable_avg_df)<-c("Pre Policy (Grade 4)","PostPolicy (Grade 5)")

kable(crosstable_avg_df,caption="Crosstable of the Average Test Scores for Each Group and Period ")

#Calculating Simple DiD Manually
#DiD=(Post Policy_Treatment-Pre Policy_Treatment)-(Post Policy_Control-Pre Policy_Control)
DiD = (0.07965408-(-0.06052893))-((-0.02232925)- 0.01696794)
DiD


#Question 5
#d
#Running DiD regression 
DiD_regression<-lm(exam_score_norm~treat*post,data =long_dataset)
summary(DiD_regression)
robust_se<-coeftest(DiD_regression,vcov=vcovHC(DiD_regression,type ="HC1"))

#Displaying DiD results in table
stargazer(DiD_regression,
          se = list(robust_se[, 2]),
          type="text",
          title ="DiD Estimation Results",
          align=TRUE,
          dep.var.caption="Dependent variable: Normalized Exam Score",
          dep.var.labels="Exam Score (Normalized)",
          column.labels=c("DiD Model"))


#Question 6
#a
#Creating "invite" dummy variable based on the parental income
threshold <- 20800 #mentioned threshold in the project description
long_dataset$invite<-ifelse(long_dataset$income<=threshold,1,0)

#b
#Finding fraction of students received invitation 
fraction_invite_grade4<-long_dataset %>%
  filter(grade==4)%>%
  summarize(fraction=mean(invite))%>%
  pull(fraction)
fraction_invite_grade4

#c
#creating cross tabulation of eligibility vs received tutoring 
grade_4<-subset(long_dataset, grade == 4)
invite_tutoring_cross<-table(grade_4$invite,grade_4$treat)
colnames(invite_tutoring_cross)<-c("No Tutoring","Tutoring")
rownames(invite_tutoring_cross)<-c("Not Eligible","Eligible")
kable(invite_tutoring_cross,caption="Comparison of eligibility and free tutoring")

#e
#Running logit regression to check endogenity
grade_4_sub<-subset(long_dataset,grade==4&invite==1)
logit_model <-glm(treat~age+male+income+peduc+distance,family=binomial(link = "logit"),data=grade_4_sub)
coeftest(logit_model,vcov=vcovHC(logit_model,type="HC1"))

#f
#Estimating fixed effect regression as an alternative for DiD estimate(controlling unobservant characteristics specific to each student across both grades)
panel_data<-pdata.frame(long_dataset,index=c("personid","grade"))
fixed_effects_model<-plm(exam_score_norm~treat*post,data=panel_data,model="within")
coeftest(fixed_effects_model,vcov=vcovHC(fixed_effects_model,type="HC1"))
summary(fixed_effects_model)

#g
#creating table with the DiD and the fixed-effects model results
stargazer(DiD_regression, 
          fixed_effects_model,
          type="text",
          title="Comparison of DiD Estimation and Fixed-effects Model Results",
          align=TRUE,
          dep.var.caption="Dependent variable: Normalized Exam Score",
          dep.var.labels="Exam Score (Normalized)",
          column.labels=c("DiD Model", "Fixed Effects Model"))

#Question 7
#b
#Computing IV estimate using iverg conditional on the control variables and drawing results in table
distance_iv_model<-ivreg(exam_score_norm ~ treat+age+male+income+peduc+invite|age+male+income+peduc+invite+distance,data = long_dataset)
summary(distance_iv_model)
iv_ser<-coeftest(distance_iv_model,vcov=vcovHC(distance_iv_model,type="HC1"))
dist_iv_model<-ivreg(exam_score~ treat|distance,data=long_dataset)
summary(dist_iv_model)

invite_iv_model<-ivreg(exam_score_norm ~ treat+age+male+income+peduc+distance|age+male+income+peduc+distance+invite,data = long_dataset)
summary(invite_iv_model)
iv_ser<-coeftest(invite_iv_model,vcov=vcovHC(invite_iv_model,type="HC1"))

stargazer(invite_iv_model,
          type="text",
          se=list(iv_ser[, 2]),
          title="IV (invite) estimate using iverg conditional on the control variables",
          align=TRUE,
          dep.var.caption="Dependent variable: Normalized Exam Score",
          dep.var.labels="Exam Score (Normalized)")


#e
#performing first-stage regression to prove relevance condition
first_stage<-lm(treat~invite+age+male+income+peduc+distance,data=long_dataset)
summary(first_stage)
first_ser<-coeftest(first_stage,vcov=vcovHC(first_stage,type="HC1"))
#Additional Wald test to validate relevance
IV_Ftest<- waldtest(first_stage,.~.-invite)
print(IV_Ftest)

#computing Hausman-Wu test to support effectiveness of the IV approach employing the "invite" instrument to address endogenity
invite_Hausman_model<-lm(exam_score_norm~treat+age+male+income+peduc+distance+first_stage$residuals,data=long_dataset)
HausWu_test<-car::linearHypothesis(invite_Hausman_model,"first_stage$residuals = 0")
HausWu_test
#different approach
HausWutest<-waldtest(invite_Hausman_model,.~.-first_stage$residuals)
HausWutest

#Table for relevance and Hausman test
stargazer(first_stage,invite_Hausman_model,
          type="text",
          se=list(first_ser[, 2]),
          title=c("First stage model (instrument relevance) and HausmanWu test (addressing endogenity)"),
          align=TRUE,
          notes=c("first stage model supports invite as a relevant instrument",
                  "Effectiveness of IV approach in addressing endougenity by using invite instrument"))

#Question 8
#b
#Filtering grade 5 and defining bin and binned data set to properly visualize
data_grade5<-long_dataset %>% filter(grade==5)
bin_width<-20
binned_dataset<-data_grade5%>%
  mutate(income_bin=floor(income/bin_width)*bin_width)%>%
  group_by(income_bin,treat)%>%
  summarise(mean_normalized_exam_score=mean(exam_score_norm,na.rm=TRUE),
            n=n())%>%
  ungroup()

#Setting threshold and upper and lower bound as there is large variation in income
threshold<-20800
lower_bound<-threshold-4000
upper_bound<-threshold+4000

#ploting discontinuity 
ggplot(binned_dataset %>%filter(income_bin>=lower_bound&income_bin<=upper_bound),
       aes(x=income_bin,y=mean_normalized_exam_score,color=as.factor(treat))) +
  geom_point() +
  geom_vline(xintercept=threshold,linetype="dashed",color="black")+
  labs(title="Discontinuity in Normalized Exam Scores",
       x="Parental Income",
       y="Mean Normalized Exam Score",
       color="Tutoring") +
  scale_color_brewer(palette = "Set1", labels = c("Control", "Treatment")) +
  theme_minimal()

#d
#ploting RD by exam score 
#Filtering grade 5 and defining bin and binned data set to properly visualize
data_grade5<-long_dataset %>% filter(grade==5)
bin_width_ex<-20
binned_dataset_ex<-data_grade5%>%
  mutate(income_binn=floor(income/bin_width_ex)*bin_width_ex)%>%
  group_by(income_binn,treat)%>%
  summarise(mean_exam_score=mean(exam_score,na.rm=TRUE),
            n=n())%>%
  ungroup()

#Setting threshold and upper and lower bound as there is large variation in income
threshold<-20800
lower_bound<-threshold-4000
upper_bound<-threshold+4000

#ploting discontinuity 
ggplot(binned_dataset_ex %>%filter(income_binn>=lower_bound&income_binn<=upper_bound),
       aes(x=income_binn,y=mean_exam_score,color=as.factor(treat))) +
  geom_point() +
  geom_vline(xintercept=threshold,linetype="dashed",color="black")+
  labs(title="Discontinuity in Exam Scores",
       x="Parental Income",
       y="Mean Exam Score",
       color="Tutoring") +
  scale_color_brewer(palette = "Set1", labels = c("Control", "Treatment")) +
  theme_minimal()

#e
#Estimating the causal impact using fuzzy RDD using ivreg, accounting for control variables
data_grade5<-data_grade5%>%
  mutate(invite = ifelse(income<= 20800, 1, 0),
         IncomeDist = income - 20800,
         Interaction = invite * IncomeDist)
iv_model_rd <- ivreg(exam_score_norm ~ treat + IncomeDist + Interaction + age + male + peduc | 
                       invite + IncomeDist + Interaction + age + male + peduc, data = data_grade5)
summary(iv_model_rd)

#g
#Creating a new dataset with a smaller bandwidth around the threshold and estimating the rd to check robustness
data_grade5_narrow <- data_grade5[abs(data_grade5$IncomeDist) <= 2000, ]
iv_model_rd_narrow <- ivreg(exam_score_norm ~ treat + IncomeDist + Interaction + age + male + peduc | 
                              invite + IncomeDist + Interaction + age + male + peduc, data = data_grade5_narrow)
summary(iv_model_rd_narrow)

#creating table with main estimate and narrow estimate
stargazer(iv_model_rd,iv_model_rd_narrow,
          type="text",
          title = "Fuzzy Regression Discontinuity Estimates",
          align = TRUE,
          dep.var.caption="exam_score_norm",
          column.labels=c("Main Estimate", "Robustness Check (Narrow Bandwidth)"))
