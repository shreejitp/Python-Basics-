rm(s5_insites)
s5_insites <- read_csv("~/Downloads/s5_2.csv")

library(dplyr)
# using subset function 
nd <- subset(s5_insites, is_content_published==TRUE) 
str(nd)

# Converting Images Count to Numeric 
nd$images_count <- as.numeric(as.character(nd$images_count))                  
# Getting the deciles for Image Count 
nd$img_dc <-ntile(as.numeric(as.character(nd$images_count)),4)

# Converting Word Count to Numeric 
nd$word_count <- as.numeric(as.character(nd$word_count))                  
# Getting the deciles for Image Count 
nd$wc_dc <-ntile(as.numeric(as.character(nd$word_count)),4)


# Converting Form Count to Numeric 
nd$form_count <- as.numeric(as.character(nd$form_count))                  
# Getting the deciles for Image Count 
nd$fc_dc <-ntile(as.numeric(as.character(nd$form_count)),4)



# What is the correlation among word count image count ?

cor(nd$images_count,nd$word_count)
cor(nd$word_count,nd$form_count)

cor(nd$fc_dc,nd$wc_dc)
cor(nd$img_dc,nd$wc_dc)
cor(nd$fc_dc,nd$img_dc)

# Can't do correlation as the data is string  
cor(nd$uses_ga,nd$uses_wp)

nd$wp_uses  <- as.integer(as.logical(nd$uses_wp))  # Uses_wp
nd$ga_uses  <- as.integer(as.logical(nd$uses_ga))  # Uses_ga
nd$shop_uses <- as.integer(as.logical(nd$has_shopping_cart))  # Has Shopping Cart 
nd$serv_biz <- as.integer(as.logical(nd$is_service_business))  # Is a Service business
nd$has_email <- as.integer(as.logical(nd$has_email_list))  # HAs Email List 
nd$mailchimp_use <- as.integer(as.logical(nd$uses_mailchimp))  #Uses mailchimp 


# Selecting only the columns required  // https://stackoverflow.com/questions/10085806/extracting-specific-columns-from-a-data-frame
library(dplyr)
cor_data <- nd %>%
  select(images_count, form_count,word_count,wp_uses,ga_uses,shop_uses,serv_biz,has_email,mailchimp_use)



# Visualizing Correlations in R   
library(corrgram)
corrgram(cor_data, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corr plot for S5")

# Second Correlogram Example
library(corrgram)
corrgram(cor_data, order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax, 
         main="Car Mileage Data in PC2/PC1 Order")


# Using the CorrPlot function 
install.packages("corrplot")

library(corrplot)


# // http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
#Computing the Correlation Matrix 
res <- cor(cor_data)
res

install.packages("Hmisc")
library(Hmisc)

# rcorr Returns the correlation and the the p-values corresponding to the significance levels of correlations.
res2 <- rcorr(as.matrix(cor_data))
res2

# Correlation plot 
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


# Insignificant correlation are crossed
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.01, insig = "blank")

# Insignificant correlations are leaved blank
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.01, insig = "blank")


## Output data frome to a CSV 

write.csv(nd, file = "/Users/spillai/Downlooads/a.csv", row.names = TRUE)







# Creating a two way table # 2-Way Frequency Table
mytable <- table(nd$producttype,nd$img_dc)

margin.table(mytable, 1)   # Summed at the product level or Row level 
margin.table(mytable, 2)       # Summed at the column level 


prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages  // Getting proportions at the row level
prop.table(mytable, 2) # column percentages












prop.table(ftable(nd, row.vars = 3:3))

prop.table(ftable(UCBAdmissions, row.vars = 3:3))



