library(ggplot2)
library(lubridate)
library(stringr)
library(tm)
library(plotly)
library(data.table)
library(dplyr)
library(ggplot2)
#library(stringr)
#library(DT)
library(tidyr)
library(knitr)
library(rmarkdown)
library(data.table)
library(dplyr)
library(readr)
library(knitr)
library(stringr)
library(DT)
library(plotly)
library(arules)
library(arulesViz)
library(visNetwork)
library(igraph)
library(kableExtra)

# How many distinct products were there? 
# What is the price range for these products? 
# Average number of transactions per day ?

# Readings: https://www.listendata.com/2015/12/market-basket-analysis-with-r.html
# Understand the logic behind the algorithm. Know what Support, Confidence and lift is 



# Loading the raw data file 
data <- read_csv("~/Downloads/data2.csv")

glimpse(data)

# Replacing all Negative values for Quantity and Unit Price 
data <- data %>% 
  mutate(Quantity = replace(Quantity, Quantity<=0, NA),
         UnitPrice = replace(UnitPrice, UnitPrice<=0, NA))

# Dropping all rows with NA Values 
data <- data %>%
  drop_na()

# Data for United Kingdom from 2010 and 2011
data <- subset(data,Country=="United Kingdom")


# Using only product description and Invoice number for the analysis

dt <- split(data$Description, data$InvoiceNo)

# Installing the arules package 
if(!require(arules)) install.packages("arules")


# Convert data to transaction level
dt2 = as(dt,"transactions")
summary(dt2)
 

# Most Frequent Items bought in UK 
library("RColorBrewer")
arules::itemFrequencyPlot(dt2,
                          topN=10,
                          col=brewer.pal(8,'Pastel2'),
                          main='Relative Item Frequency Plot',
                          type="relative",
                          ylab="Item Frequency (Relative)") 





# aggregated data
rules = apriori(dt2, parameter=list(support=0.005, confidence=0.8))


# Scatter plot for the rules 
plot(rules, measure=c("support", "lift"), 
     shading = "confidence",
     interactive = FALSE)



#rules = apriori(dt2, parameter=list(support=0.005, confidence=0.8, minlen = 3))
#rules = apriori(dt2, parameter=list(support=0.005, confidence=0.8, maxlen = 4))



# Get Summary Information
summary(rules)

detach(package:tm, unload=TRUE)

# Sort by Lift
rules<-sort(rules, by="lift", decreasing=TRUE)
inspect(rules[1:50])

# Storing the top 50 rules in r
r <- rules[1:50]


plot(r, method="grouped",   
     control=list(col = rev(brewer.pal(9, "Greens")[4:9]))) + coord_flip()


#Convert rules into data frame
rules_frame = as(rules, "data.frame")

head(rules_frame)



# What are customers likely to buy before they purchase "PARTY BUNTING"
rules2<-apriori(data=dt2, parameter=list(supp=0.001,conf = 0.8), 
               appearance = list(default="lhs",rhs="PARTY BUNTING"),
               control = list(verbose=F))
rules2<-sort(rules2, decreasing=TRUE,by="confidence")
inspect(rules2[1:5])



# What are customers likely to buy if they purchased "POPPY'S PLAYHOUSE BATHROOM"
rules3<-apriori(data=dt2, parameter=list(supp=0.001,conf = 0.8), 
               appearance = list(default="rhs",lhs="POPPY'S PLAYHOUSE BATHROOM"),
               control = list(verbose=F))
rules3<-sort(rules3, decreasing=TRUE,by="confidence")
inspect(rules3[1:1])










