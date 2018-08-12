## Notes 






install.packages("sqldf")
library(sqldf)
################
# Make fake Data Set
################
strat_n <- 10

strata <- seq(1, strat_n, by=1)
test_mbr <- floor(runif(strat_n, min = 0 , max = 1000))   # Generates 10 random numbers between and 1000 (Uniformly distributed)
control_mbr <- floor(runif(strat_n, min = 0 , max = 1000))

test_prop <- runif(strat_n, min = 0 , max = 1)
control_prop <- runif(strat_n, min = 0 , max = 1)

test_resp <- floor(test_mbr*test_prop)
control_resp <- floor(control_mbr*control_prop)

strata_summary <- as.data.frame(cbind(strat=strata, test_mbr=test_mbr, control_mbr=control_mbr, 
                                      test_prop=test_prop, control_prop=control_prop,
                                      test_resp=test_resp, control_resp=control_resp))


test_lst1 <- list()
test_lst2 <- list()
test_lst3 <- list()
test_lst4 <- list()
test_lst5 <- list()
test_lst6 <- list()
test_lst7 <- list()
test_lst8 <- list()

for( strata_i in 1:strat_n){
  test_lst1[[strata_i]] <- rep("test", test_mbr[strata_i])  
  test_lst2[[strata_i]] <- rep(strata_i, test_mbr[strata_i])
  test_lst3[[strata_i]] <- seq(from=1,to=test_mbr[strata_i], by=1)
  test_lst4[[strata_i]] <- sample(c(0,1), size=test_mbr[strata_i], replace=TRUE, prob=c(1-test_prop[strata_i], test_prop[strata_i]))
  
  test_lst5[[strata_i]] <- rep("control", control_mbr[strata_i])  
  test_lst6[[strata_i]] <- rep(strata_i, control_mbr[strata_i])
  test_lst7[[strata_i]] <- seq(from=1,to=control_mbr[strata_i], by=1)
  test_lst8[[strata_i]] <- sample(c(0,1), size=control_mbr[strata_i], replace=TRUE, prob=c(1-control_prop[strata_i], control_prop[strata_i]))
  
}

tbl_make_test <- as.data.frame(cbind(testarm=unlist(test_lst1), 
                                     strata=unlist(test_lst2), 
                                     client=unlist(test_lst3),
                                     response=unlist(test_lst4)))

tbl_make_control <- as.data.frame(cbind(testarm=unlist(test_lst5), 
                                        strata=unlist(test_lst6), 
                                        client=unlist(test_lst7),
                                        response=unlist(test_lst8)))
campaign_mbr_tbl <- rbind(tbl_make_test,tbl_make_control)

campaign_mbr_tbl$strata <- as.numeric(as.character(campaign_mbr_tbl$strata))

#str(campaign_mbr_tbl)
## Write into database

table(campaign_mbr_tbl$testarm, campaign_mbr_tbl$strata)

#head(campaign_mbr_tbl)
#table(campaign_mbr_tbl$response, campaign_mbr_tbl$testarm, campaign_mbr_tbl$strata )
#strata_summary_old <- strata_summary

strata_summary <- sqldf("
                        select strata
                        , count(case when testarm = 'test' then client end) as test_mbr
                        , count(case when testarm = 'control' then client end) as control_mbr
                        , sum(case when testarm = 'test' then response else 0 end) as test_resp
                        , sum(case when testarm = 'control' then response else 0 end) as control_resp
                        from campaign_mbr_tbl
                        group by strata
                        ")

strata_summary$test_prop <- strata_summary$test_resp / strata_summary$test_mbr
strata_summary$control_prop <-  strata_summary$control_resp / strata_summary$control_mbr

strata_summary$test_mbr_dist <- unlist(lapply(strata_summary$test_mbr , function(x) { x/ sum(strata_summary$test_mbr)}))
strata_summary$control_mbr_dist <- unlist(lapply(strata_summary$control_mbr , function(x) { x/ sum(strata_summary$control_mbr)}))

strata_summary$test_mbr_total <- rep(sum(strata_summary$test_mbr), strat_n)
strata_summary$control_mbr_total <- rep(sum(strata_summary$control_mbr), strat_n)

##########
# Initial Tests
###########

##########
### test distribution equality accross Strata
##########

chisq.test(strata_summary[ , c("test_mbr", "control_mbr")])

##########
# test total
###########
x <- c(sum(strata_summary$test_resp) , sum(strata_summary$control_resp)) ## storing the response from test and control
n <- c(sum(strata_summary$test_mbr) , sum(strata_summary$control_mbr))   ## storing the total from test and control

## More details on prop.test 
# prop.test can be used for testing the null that the proportions (probabilities of success) 
# in several groups are the same, or that they equal certain given values.

## H0: The null hypothesis is that the four populations from which
##     the patients were drawn have the same true proportion of smokers 



prop_res <- prop.test(x, n, p = NULL                   ## Understand why prop test is used ? 
                      , alternative = "two.sided"
                      ,conf.level = 0.9
                      ,correct = FALSE)
prop_res

#strata_summary
#sum(strata_summary$test_mbr_dist)
#sum(strata_summary$control_mbr_dist)

## Determine balancing rules per strata

control_mbr_adj_act <- rep(0, strat_n)
control_mbr_adj <- rep(0, strat_n)


for( strata_i in 1:strat_n){
  
  #strata_i <- 4   
  
  strata_summary[strata_i,] 
  
  control_mbr_need <- strata_summary[strata_i,c("test_mbr_dist")] * strata_summary[strata_i,c("control_mbr_total")]
  
  
  if( strata_summary[strata_i,c("test_mbr")] == 0  || strata_summary[strata_i,c("control_mbr")] == 0){
    
    ######################################
    ## SCENARIO: No observations in Test Strata
    ######################################
    
    message(paste0("exclude strata for strata:", strata_i))
    control_mbr_adj_act[strata_i] <- "exclude strata"
    control_mbr_adj[strata_i] <- 0
    
  } else if( strata_summary[strata_i,c("control_mbr")] < 10 || (strata_summary[strata_i,c("control_mbr")] / strata_summary[strata_i,c("test_mbr")]) < 0.1 ) {
    
    ######################################
    ## SCENARIO: Insufficient observations in control group strata
    ######################################
    
    message(paste0("exclude strata for strata:", strata_i))
    control_mbr_adj_act[strata_i] <- "exclude strata"
    control_mbr_adj[strata_i] <- 0
    
  } else if( abs((control_mbr_need - strata_summary[strata_i,c("control_mbr")]) / strata_summary[strata_i,c("control_mbr")]) < 0.01 ) {
    
    ######################################
    ## SCENARIO: Close enough - do nothing
    ######################################
    
    message(paste0("Close enough do nothing to strata:", strata_i))
    control_mbr_adj_act[strata_i] <- "sufficent balancce"
    control_mbr_adj[strata_i] <- strata_summary[strata_i,c("control_mbr")]
    
  } else if(control_mbr_need > strata_summary[strata_i,c("control_mbr")] ){
    
    ######################################
    ## SCENARIO: Bootstrap to get more control group observations
    ######################################
    
    message(paste0("random bootstrap for strata:", strata_i))
    #sample(x=strata, size=11, replace = TRUE)
    control_mbr_adj_act[strata_i] <- "random bootstrap"
    control_mbr_adj[strata_i] <- control_mbr_need
    
  } else if(control_mbr_need < strata_summary[strata_i,c("control_mbr")] ){
    
    ######################################
    ## SCENARIO: sample to get fewer control group observations
    ######################################
    
    message(paste0("random sample for strata:", strata_i))
    #sample(x=strata, size=3, replace = FALSE, prob = NULL)
    control_mbr_adj_act[strata_i] <- "random sample"
    control_mbr_adj[strata_i] <- control_mbr_need
    
  }
  
  
}

strata_summary$control_mbr_adj_act <- control_mbr_adj_act

strata_summary$control_mbr_adj <- control_mbr_adj
strata_summary$control_mbr_adj_dist <- unlist(lapply(strata_summary$control_mbr_adj , function(x) { x/ sum(strata_summary$control_mbr_adj)}))


strata_summary$test_mbr_adj <- ifelse(strata_summary$control_mbr_adj == 0, 0 , strata_summary$test_mbr)
strata_summary$test_mbr_adj_dist <- unlist(lapply(strata_summary$test_mbr_adj , function(x) { x/ sum(strata_summary$test_mbr_adj)}))

strata_summary$strata <- as.numeric(strata_summary$strata )
strata_summary <- strata_summary[with(strata_summary, order(strata)),]
strata_summary

#
#Rebalance Control Group membership based on Test Group
#

test_mbr_ref <- list()
control_mbr_ref <- list()
test_mbr_filter <- list()
control_mbr_filter <- list()


for( strata_i in 1:strat_n){
  
  #strata_i <- 5
  
  strata_summary[strata_i,] 
  
  
  if(control_mbr_adj_act[strata_i] == "exclude strata"){
    
    test_mbr_ref[[strata_i]] <- seq(from=1,to=strata_summary[strata_i,c("test_mbr")], by=1)
    control_mbr_ref[[strata_i]] <- seq(from=1,to=strata_summary[strata_i,c("control_mbr")], by=1)
    
    test_mbr_filter[[strata_i]] <- rep("exclude", strata_summary[strata_i,c("test_mbr")])
    control_mbr_filter[[strata_i]] <- rep("exclude", strata_summary[strata_i,c("control_mbr")])
    
  } else if( control_mbr_adj_act[strata_i] == "random bootstrap" ) {
    
    test_mbr_ref[[strata_i]] <- seq(from=1,to=strata_summary[strata_i,c("test_mbr")], by=1)
    
    control_mbr_set <- seq(from=1,to=strata_summary[strata_i,c("control_mbr")], by=1)
    control_mbr_ref[[strata_i]] <- sample(x=control_mbr_set, size=strata_summary[strata_i,c("control_mbr_adj")], replace = TRUE)
    
    
    test_mbr_filter[[strata_i]] <- rep("include", strata_summary[strata_i,c("test_mbr")])
    control_mbr_filter[[strata_i]] <- rep("include", strata_summary[strata_i,c("control_mbr_adj")])    
    
    
  } else if( control_mbr_adj_act[strata_i] == "random sample" ) {
    
    test_mbr_ref[[strata_i]] <- seq(from=1,to=strata_summary[strata_i,c("test_mbr")], by=1)
    control_mbr_ref[[strata_i]] <- seq(from=1,to=strata_summary[strata_i,c("control_mbr")], by=1)
    
    sample_prob <- strata_summary[strata_i,c("control_mbr_adj")] / strata_summary[strata_i,c("control_mbr")]
    test_mbr_filter[[strata_i]] <- rep("include", strata_summary[strata_i,c("test_mbr")])
    control_mbr_filter[[strata_i]] <- sample(c("exclude","include"), size=strata_summary[strata_i,c("control_mbr")], replace=TRUE, prob=c(1-sample_prob,sample_prob))   
    
  }
  
}



names(test_mbr_ref) <- seq(from=1,to=strat_n,by=1)
names(control_mbr_ref) <- seq(from=1,to=strat_n,by=1)

df_test <- data.frame( testarm = rep("test", sum(unlist(lapply(test_mbr_ref, length)))),
                       strata = rep(names(test_mbr_ref), lapply(test_mbr_ref, length)),
                       client = unlist(test_mbr_ref),
                       client_filter = unlist(test_mbr_filter)
)

df_control <- data.frame( testarm = rep("control", sum(unlist(lapply(control_mbr_ref, length)))),
                          strata = rep(names(control_mbr_ref), lapply(control_mbr_ref, length)),
                          client = unlist(control_mbr_ref),
                          client_filter = unlist(control_mbr_filter)
)

campaign_balance_filters <- rbind(df_test,df_control)


campaign_balance_filters$strata <- as.numeric(as.character(campaign_balance_filters$strata))
str(campaign_balance_filters)

#str(df_campagin_balanced)
#str(campaign_mbr_tbl)
#strata_summary
#table(df_campagin_balanced$strata, df_campagin_balanced$testarm)
#table(campaign_mbr_tbl$strata, campaign_mbr_tbl$testarm)

campaign_mbr_tbl_balanced <- sqldf("
                                   select  a.*, b.response
                                   from campaign_balance_filters a
                                   left join campaign_mbr_tbl b ON a.strata = b.strata AND a.client = b.client AND a.testarm = b.testarm
                                   ")


df_campagin_balanced$strata <- as.numeric(df_campagin_balanced$strata)


#table(campaign_mbr_tbl$strata, campaign_mbr_tbl$testarm)
#table(campaign_mbr_tbl_balanced$strata, campaign_mbr_tbl_balanced$testarm)
#summary(campaign_mbr_tbl_balanced)
#library(data.table)

#setwd("C:/Users/mshump/Documents/Projects/scripted testing/output")

#fwrite(campaign_mbr_tbl, file="campaign_mbr_tbl.csv")
#fwrite(df_campagin_balanced, file="df_campagin_balanced.csv")
#fwrite(campaign_mbr_tbl_balanced, file="campaign_mbr_tbl_balanced.csv")


strata_summary_balanced <- sqldf("
                                 select strata
                                 , count(case when testarm = 'test' then client end) as test_mbr
                                 , count(case when testarm = 'control' then client end) as control_mbr
                                 , sum(case when testarm = 'test' then response else 0 end) as test_resp
                                 , sum(case when testarm = 'control' then response else 0 end) as control_resp
                                 from campaign_mbr_tbl_balanced
                                 where client_filter = 'include' 
                                 group by strata
                                 ")


strata_summary_balanced$test_mbr_dist <- unlist(lapply(strata_summary_balanced$test_mbr , function(x) { x/ sum(strata_summary_balanced$test_mbr)}))
strata_summary_balanced$control_mbr_dist <- unlist(lapply(strata_summary_balanced$control_mbr , function(x) { x/ sum(strata_summary_balanced$control_mbr)}))

strata_summary_balanced$test_prop <- strata_summary_balanced$test_resp / strata_summary_balanced$test_mbr
strata_summary_balanced$control_prop <- strata_summary_balanced$control_resp / strata_summary_balanced$control_mbr

strata_summary_balanced



####################
##### Start testing
####################



##########
### test distribution equality accross Strata
##########
chisq.test(strata_summary_balanced[ , c("test_mbr", "control_mbr")])


##########
# test total
###########
x <- c(sum(strata_summary_balanced$test_resp) , sum(strata_summary_balanced$control_resp))
n <- c(sum(strata_summary_balanced$test_mbr) , sum(strata_summary_balanced$control_mbr))

prop_res <- prop.test(x, n, p = NULL
                      , alternative = "two.sided"
                      ,conf.level = 0.9
                      ,correct = FALSE)
prop_res


##########
# test total - POWER
# Target POWER >= 0.8
###########

n_pergrp <- sum(n)/2  # assume even distribution - farther from equal the lower POWER
prop1 <- prop_res$estimate[1]
prop2 <- prop_res$estimate[2]

power_prop_res <- power.prop.test(n_pergrp, prop1, prop2, sig.level = 0.1,
                                  power = NULL,
                                  alternative = c("two.sided"))

## output for power (inverse of risk of false negatives) looking for >= 0.8
power_prop_res



##########
# test within strat - Differences and Power
###########

pvals <- rep(0, nrow(strata_summary_balanced))
powers <- rep(0, nrow(strata_summary_balanced))

for( strata_i in 1:nrow(strata_summary_balanced)){
  strata_summary_balanced[strata_i,]
  
  x <- as.numeric(strata_summary_balanced[strata_i,c("test_resp","control_resp")]) #success or failure
  n <- as.numeric(as.list(strata_summary_balanced[strata_i,c("test_mbr","control_mbr")]))  # total samples
  
  prop_res <- prop.test(x, n, p = NULL
                        , alternative = "two.sided"
                        ,conf.level = 0.9
                        ,correct = FALSE)
  
  pvals[strata_i] <- round(prop_res$p.value,5)
  
  
  n_pergrp <- sum(n)/2  # assume even distribution - farther from equal the lower POWER
  prop1 <- prop_res$estimate[1]
  prop2 <- prop_res$estimate[2]
  
  power_prop_res <- power.prop.test(n_pergrp, prop1, prop2, sig.level = 0.1,
                                    power = NULL,
                                    alternative = c("two.sided"))
  
  
  powers[strata_i] <- round(power_prop_res$power,5) 
  
}

strata_summary_balanced$pvals <- pvals
strata_summary_balanced$powers <- powers
strata_summary_balanced
