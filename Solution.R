# Objective: 
# To develop a model for predicting customer churn at "Cell2Cell"
# ___________________________________________________________________
rm(list = ls())
#Set Directory
setwd("C:\\Users\\anubh\\Desktop\\BA360\\R\\Day 14\\Regression Case Studies - Linear & Logistic\\Proactive Attrition Management-Logistic Regression Case Study")

# Read CSV file
mydata<-read.csv("Proactive Attrition Management-Logistic Regression Case Study.csv", header = TRUE)

# View(mydata)
View(mydata)
str(mydata)

#________________________ DATA AUDIT _____________________
# EDA EXERCISE
#Create user defined function for descriptive analysis
var_Summ=function(x){
  if(class(x)=="numeric"){
    Var_Type=class(x)
    n<-length(x)           # number of rows in each column
    nmiss<-sum(is.na(x))   # N# of mising values
    mean<-mean(x, na.rm=T)
    std<-sd(x, na.rm=T)
    var<-var(x,na.rm=T)
    min<-min(x,na.rm=T)
    p1<-quantile(x,0.01,na.rm=T) # 1st percentile
    p5<-quantile(x,0.05,na.rm=T)
    p10<-quantile(x,0.1,na.rm=T)
    q1<-quantile(x,0.25,na.rm=T)
    q2<-quantile(x,0.5,na.rm=T)     # P50 - 50th percentile/ Q2/ D5/ Median/ 
    q3<-quantile(x,0.75,na.rm=T)  
    p90<-quantile(x,0.9,na.rm=T)
    p95<-quantile(x,0.95,na.rm=T)
    p99<-quantile(x,0.99,na.rm=T)
    max<-max(x,na.rm=T)
    UC1=mean(x,na.rm=T)+3*sd(x,na.rm=T) # outlier upper range
    LC1=mean(x,na.rm=T)-3*sd(x,na.rm=T)  # outlier lower range
    UC2=quantile(x,0.99,na.rm=T)
    LC2=quantile(x,0.01,na.rm=T)
    iqr=IQR(x,na.rm=T)
    UC3=q3+1.5*iqr # Q3 - Q1 [50% of the data]
    LC3=q1-1.5*iqr
    ot1<-max>UC1 | min<LC1 
    ot2<-max>UC2 | min<LC2 
    ot3<-max>UC3 | min<LC3
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,mean=mean,std=std,var=var,min=min,p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max,ot_m1=ot1,ot_m2=ot2,ot_m2=ot3))
  }
  else{
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    fre<-table(x)   
    prop<-prop.table(table(x)) 
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss, freq=fre, proportion=prop))
  }
}

# Converting continuous variable to numeric and removing id column.

mydata$MONTHS=as.numeric(mydata$MONTHS)
mydata$PHONES=as.numeric(mydata$PHONES)
mydata$MODELS=as.numeric(mydata$MODELS)
mydata$EQPDAYS=as.numeric(mydata$EQPDAYS)
mydata$CHURNDEP=as.integer(mydata$CHURNDEP)
mydata$CUSTOMER=NULL
mydata$CSA=NULL
str(mydata)

# Converting Categorical to Factor.

oth_var=sapply(mydata,is.integer)
xc=names(mydata[oth_var])
mydata[,xc]=lapply(mydata[,xc],factor)
str(mydata)

#Vector of numerical variables
num_var= sapply(mydata,is.numeric)  # suitable apply function - gives vector most of the times
Other_var= !sapply(mydata,is.numeric)


#Applying above defined function on numerical variables
#
my_num_data <- t(data.frame(apply(mydata[num_var], 2, var_Summ)))

my_cat_data<- data.frame(t(apply(mydata[Other_var], 2, var_Summ)))

View(my_num_data)
View(my_cat_data)

# missing values

Dev <- mydata[!is.na(mydata$CHURNDEP),]

Val <- mydata[is.na(mydata$CHURNDEP),]

View(Dev)
View(Val)

#Missing Value Treatment

Dev[,num_var] <- apply(data.frame(Dev[,num_var]), 
                           2, 
                           function(x){x <- replace(x, is.na(x), mean(x, na.rm=TRUE))})

# Replacing the categorical missing values with the MODE 

Dev[,Other_var] <-apply(data.frame(Dev[,Other_var]), 
                            2, 
                            function(x){x <- replace(x, is.na(x), which.max(prop.table(table(x))))})


# OUTLIERS TREATMENT - WHERE P1 AND P99 ARE THE MAX AND MIN LIMITS
#____________________________________

outlier <- function(x){
  quantiles <- quantile(x, c(.01, .99 ),na.rm=TRUE )
  # Above line will calc the P1 and P99
  
  x[x < quantiles[1] ] <- quantiles[1]  # if value < P1, then P1
  x[ x > quantiles[2] ] <- quantiles[2]  # if value > P99, then P99
  x
}

# Applying the func for Outlier Treatment
Dev[,num_var] <- apply(data.frame(Dev[,num_var]), 2, outlier) 

#Splitting data into Training, Validaton and Testing Dataset
#_________________________________________________________________________

set.seed(999)

train_ind <- sample(1:nrow(Dev), size = floor(0.70 * nrow(Dev)))

training <- Dev[train_ind,]
testing <- Dev[-train_ind,]

nrow(training)
nrow(testing) 

# Factor Analysis
tr=training[,Other_var]
xtr=names(tr)
training[,xtr]=lapply(training[,xtr],factor)
tt=testing[,Other_var]
xtt=names(tt)
testing[,xtt]=lapply(testing[,xtt],factor)

## Correlation matrix

corr= cor(Dev[,num_var]) 
#View(corrm)
write.csv(corr, file = "corrm.csv")

#Chisquare Test for categorical data
cat_tbl=Dev[,Other_var]
xd=names(cat_tbl)
cat_tbl[,xd]=lapply(cat_tbl[,xd],factor)
cat_tbl$CALIBRAT=NULL
str(cat_tbl)
chsq=function(i){
  name_col <- colnames(cat_tbl)[i]
  pval <- chisq.test(cat_tbl[,i],cat_tbl$CHURNDEP)$p.value
  
   if(pval < 0.05){
     stri <- paste(name_col, " : ")
     stri <- paste(stri, pval)
  }
}

sapply(1:ncol(cat_tbl), chsq)

# Significant categorical-cols UNIQSUBS+ACTVSUBS+AGE1+AGE2+CREDITA+CREDITAA+CREDITB+CREDITC+CREDITDE+PRIZMUB+PRIZMTWN+REFURB+WEBCAP+OWNRENT+MARRYUN+MARRYNO+MAILORD+MAILRES+CREDITCD+RETCALLS+RETACCPT+NEWCELLY+REFER+INCMISS+INCOME+SETPRCM+RETCALL

# Factor analysis for continuous data
x=Dev[,num_var]
x$churn=cat_tbl$CHURNDEP
str(x)
num_fit<-glm(churn ~ REVENUE+MOU+RECCHRGE+DIRECTAS+OVERAGE+ROAM+CHANGEM+CHANGER+DROPVCE+
               BLCKVCE+UNANSVCE+CUSTCARE+THREEWAY+MOUREC+OUTCALLS+INCALLS+PEAKVCE+OPEAKVCE+
               DROPBLK+CALLFWDV+CALLWAIT+MONTHS+PHONES+MODELS+EQPDAYS+SETPRC,
             data=x,
             family = binomial(logit))
require(MASS)
n_st=step(num_fit,direction = "both")

# significant vars
#churn ~ REVENUE + MOU + RECCHRGE + OVERAGE + ROAM + CHANGEM + 
#  CHANGER + BLCKVCE + UNANSVCE + CUSTCARE + THREEWAY + OUTCALLS + 
#  INCALLS + PEAKVCE + DROPBLK + MONTHS + PHONES + MODELS + 
#  EQPDAYS + SETPRC


# Modelling

Fit1 <- glm(CHURNDEP ~ REVENUE + MOU + RECCHRGE + OVERAGE + ROAM + CHANGEM + 
              CHANGER + BLCKVCE + UNANSVCE + CUSTCARE + THREEWAY + OUTCALLS + 
              INCALLS + PEAKVCE + DROPBLK + MONTHS + PHONES + MODELS + 
              EQPDAYS + SETPRC+UNIQSUBS+ACTVSUBS+AGE1+AGE2+CREDITA+CREDITAA+
              CREDITB+CREDITC+CREDITDE+PRIZMUB+PRIZMTWN+REFURB+WEBCAP+OWNRENT+MARRYUN+
              MARRYNO+MAILORD+MAILRES+CREDITCD+RETCALLS+RETACCPT+NEWCELLY+REFER+INCMISS+
              INCOME+SETPRCM+RETCALL,
            data = training,
            family = binomial(logit))

step1<-step(Fit1,direction = "both")

final_model<- glm(CHURNDEP ~ REVENUE + MOU + RECCHRGE + OVERAGE + ROAM + CHANGEM + 
                    CHANGER + BLCKVCE + THREEWAY + OUTCALLS + INCALLS + PEAKVCE + 
                    DROPBLK + MONTHS + PHONES + EQPDAYS + SETPRC + UNIQSUBS + 
                    ACTVSUBS + AGE1 + CREDITAA + CREDITB + CREDITDE + REFURB + 
                    WEBCAP + MARRYNO + MAILRES + RETCALLS + NEWCELLY + SETPRCM,
                  data = training,
                  family = binomial(logit))

summary(final_model)
source("Concordance.R")
Concordance(final_model)  #0.626361





################################ VALIDATION and scoring of my model ##############################
#Decile Scoring for 
##Training dataset

train1<- cbind(training, Prob = predict(final_model, type="response")) 
View(train1)
str(train1)


##Creating Deciles
(decLocations <- quantile(train1$Prob, probs = seq(0.1,0.9,by=0.1)))

train1$decile <- findInterval(train1$Prob,c(-Inf,decLocations, Inf))

View(train1)

require(dplyr)

train1$decile<-factor(train1$decile)

tr <- train1[order(train1$decile),]

decile_grp<-group_by(train1,decile)
decile_grp$CHURNDEP<-as.numeric(as.character(decile_grp$CHURNDEP))
View(decile_grp$CHURNDEP)
decile_summ_train<-summarise(decile_grp, 
                             total_cnt = n(), 
                             min_prob = min(p=Prob),
                             max_prob = max(Prob), 
                             churn_cnt= sum(CHURNDEP), 
                             non_churn_cnt = total_cnt - churn_cnt)


# SORTING THE DATA IN DESCEINDING  ORDER BASED ON dECILES
decile_summ_train<-arrange(decile_summ_train, desc(decile))

View(decile_summ_train)

write.csv(decile_summ_train,"Dev_Train_DA1.csv",row.names = F)

train1$decile=as.numeric(as.character(train1$decile))
#Decile Analysis Reports
require(sqldf)
fit_train_DA <- sqldf("select decile, min(Prob) as Min_prob
                       , max(Prob) as max_prob
                       , sum(CHURNDEP) as Churn
                       , (count(decile)-sum(CHURNDEP)) as Non_churn 
                      from train1
                      group by decile
                      order by decile desc")



write.csv(fit_train_DA,"Dev_Train_DA1.csv",row.names = F)





##Testing dataset
testing$ACTVSUBS<-factor(testing$ACTVSUBS,levels=levels(training$ACTVSUBS))
test1<- cbind(testing, Prob = predict(final_model,testing, type="response"))
View(test1)
##Creating Deciles
decLocations <- quantile(test1$Prob, probs = seq(0.1,0.9,by=0.1),na.rm=T)
test1$decile <- findInterval(test1$Prob,c(-Inf,decLocations, Inf))
test1$decile<-factor(test1$decile)
decile_grp<-group_by(test1,decile)
decile_grp$CHURNDEP<-as.numeric(as.character(decile_grp$CHURNDEP))
decile_summ_test<-summarize(decile_grp, 
                            total_cnt = n(), 
                            min_prob = min(p=Prob),
                            max_prob = max(Prob), 
                            churn_cnt= sum(CHURNDEP), 
                            non_churn_cnt = total_cnt - churn_cnt )


# SORTING THE DATA IN DESCEINDING  ORDER BASED ON dECILES
decile_summ_test<-decile_summ_test[complete.cases(decile_summ_test),]
decile_summ_test<-arrange(decile_summ_test, desc(decile))
View(decile_summ_test)
write.csv(decile_summ_test,"dev_test_DA2.csv",row.names = F)



#Validation Data
#Prediction

Val$UNIQSUBS<-factor(Val$UNIQSUBS,levels=levels(training$UNIQSUBS))
Val$ACTVSUBS<-factor(Val$ACTVSUBS,levels=levels(training$ACTVSUBS))
Val1<-cbind(Val, Prob=predict(final_model, Val, type="response"))
View(Val1)
Val1$CHURNDEP <- ifelse(Val1$Prob>0.47, 1,0)



#Confusion matrix

table(train1$Prob>0.47, train1$CHURNDEP)

table(test1$Prob>0.50, test1$CHURNDEP)

#Performance of the model

train1<- cbind(training, Prob=predict(fit, type="response")) 
install.packages("ROCR")
require(ROCR)
pred_train_fit <- prediction(train1$Prob, train1$CHURNDEP)
perf_fit2 <- performance(pred_train_fit, "tpr", "fpr")
plot(perf_fit2)
abline(0, 1)
performance(pred_train_fit, "auc")@y.values  # 0.6283436
