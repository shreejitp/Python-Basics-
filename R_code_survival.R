data("UCBAdmissions")
summary(UCBAdmissions)



library(readr)
whmcs_surv <- read_csv("~/Downloads/whmcs_surv.csv")

library(pacman)

# install.packages("survival")
library(survival)

library(readr)

# using subset function 
newdata <- subset(mydata, age >= 20 | age < 10, 
                  select=c(ID, Weight))

# using subset function (part 2)
newdata <- subset(mydata, sex=="m" & age > 25,
                  select=weight:income)



mydata <- subset(whmcs_surv,a.brand=='SEOH' & a.signupbillterm==1) 

str(mydata)

mydata <- subset(whmcs_surv,a.brand=='SEOH' & a.signupbillterm==1 & yr==2015 &  ten_in_months >= 0)  # & yr==2012



#mydata<- read.csv("C:/Econometrics/Data/survival_unemployment.csv")
attach(mydata)

# Define variables 
time <- mydata$ten_in_months
event <- mydata$canc_flag
X <- cbind(logwage, ui, age)
group <- ui

# Descriptive statistics
summary(time)
summary(event)
summary(X)
summary(group)

# Kaplan-Meier non-parametric analysis

kmsurvival <- survfit(Surv(time,event) ~ 1)
summary(kmsurvival)
plot(kmsurvival, xlab="Tenure in months", ylab="Survival Probability")

# Drawing curves
library("survminer")
ggsurvplot(kmsurvival1, color = "#2E9FDF")


############ Pacman Package###################

library(pacman)
pacman::p_load(plotly)
pacman::p_load(GGally)
pacman::p_load(survival)
pacman::p_load(cowplot)
pacman::p_load(broom)
pacman::p_load_current_gh("sahirbhatnagar/casebase")
pacman::p_load(Epi)
"%ni%" <- Negate("%in%")


install.packages("devtools")
library(devtools)
install_github("hadley/scales")


p1 <- GGally::ggsurv(kmsurvival, main = "Kaplan-Meier Curve for SEOH for 2012 Cohort",plot.cens=FALSE)
plotly::ggplotly(p1)



p1 <- GGally::ggsurv(kmsurvival1, main = "Survival Curve for SEOH monthly Subs by Year of Signup",plot.cens=FALSE,
                     order.legend = FALSE, xlab = "Tenure in months"
                     )
plotly::ggplotly(p1)




#### SurvMiner
fit <- survfit(Surv(time, status) ~ 1, data = lung)
# Drawing curves
ggsurvplot(kmsurvival, color = "#2E9FDF")


kmsurvival <- survfit(Surv(time,event) ~ 1,data=mydata)
install.packages("survminer")
library("survminer")

# Drawing curves
ggsurvplot(kmsurvival1)

kmsurvival1 <- survfit(Surv(time, event) ~ yr_month,data=mydata)






# Kaplan-Meier non-parametric analysis by group
kmsurvival1 <- survfit(Surv(time, event) ~ mydata$yr)
summary(kmsurvival1)
plot(kmsurvival1, xlab="Time", ylab="Survival Probability")
title(main = "Survival Curve by Signup Month")

plot(fit, lty = 1:2, mark.time = FALSE, ylim=c(.75,1), xlab = 'Days since Subscribing', ylab = 'Percent Surviving')
legend(20, .8, c('Male', 'Female'), lty=1:2, bty = 'n', ncol = 2)
title(main = "NetLixx Survival Curves by Gender")



# Nelson-Aalen non-parametric analysis
nasurvival <- survfit(coxph(Surv(time,event)~1), type="aalen")
summary(nasurvival)
plot(nasurvival, xlab="Time", ylab="Survival Probability")


# Cox proportional hazard model - coefficients and hazard rates
coxph <- coxph(Surv(time,event) ~ X, method="breslow")
summary(coxph)


# Exponential, Weibull, and log-logistic parametric model coefficients
# Opposite signs from Stata results, Weibull results differ; same as SAS
exponential <- survreg(Surv(time,event) ~ X, dist="exponential")
summary(exponential)

weibull <- survreg(Surv(time,event) ~ X, dist="weibull")
summary(weibull)

loglogistic <- survreg(Surv(time,event) ~ X, dist="loglogistic")
summary(loglogistic)



## Machine learning Class - 


# Creating a Vector in R 
a <- c(2,1,3)
a

# Transposing the Vector
t(a)




b <- c(2,8,9)
# Adding Vector 1 and 2 
a+b


sqrt(SUM(a^2))

length(a)
length(b)


# Creating a matrix 

A <- matrix(c(1,2,3,4), ncol=2)
A
# Multiply the matrix with 2 
A*2




A <- matrix(c(2,1,1,5,4,2),ncol=3)
A


B <- matrix(c(3,-1,1,2,4,2),ncol=2)
B


A*B
A%*%B


B%*%A


det(A)
det(A%*%B)

# Euclidean Length of the Vector 
rank(A)

# Solving a set of Linear Equations 


A <- matrix(c(5,4,3,2),ncol=2)
A 

Ainv=solve(A)
Ainv



b=c(19,14)


x= Ainv%*%b
x





library(datasets)
ir_data<- iris
head(ir_data)

str(ir_data)

levels(ir_data$Species)

# Checking for any Null Values 
sum(is.na(ir_data))


ir_data<-ir_data[1:100,]

# Splitting the dataset into test and control
set.seed(100)
samp<-sample(1:100,80)
ir_test<-ir_data[samp,]
ir_ctrl<-ir_data[-samp,]



library(ggplot2); 
library(GGally)


ggpairs(ir_test)


y<-ir_test$Species; 
x<-ir_test$Sepal.Length
glfit<-glm(y~x, family = 'binomial')

summary(glfit)

newdata<- data.frame(x=ir_ctrl$Sepal.Length)
predicted_val<-predict(glfit, newdata, type="response")
prediction<-data.frame(ir_ctrl$Sepal.Length, ir_ctrl$Species,predicted_val)
prediction


qplot(prediction[,1], round(prediction[,3]), col=prediction[,2], xlab = 'Sepal Length', ylab = 'Prediction using Logistic Reg.')













