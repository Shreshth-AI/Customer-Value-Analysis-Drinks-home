#R CODE FOR Customer Value Analysis: 
#1 Set Directory 
setwd("C:/Users/shres") 
#2 Data Reading 
data <- read.csv("Transactions_Customer.csv") 
# To understand the data form 
head(data) 
#3 Install Essential Packages 
# dplyr : for data manipulation and transformation. 
install.packages("dplyr") 
# ggplot2 : for creating visualizations. 
install.packages("ggplot2") 
# mice : for imputing missing values in data-sets through multiple imputation. 
install.packages("mice") 
# corrgram : for produces a graphical display of a correlation matrix 
install.packages("corrgram") 
#4 Load libraries to be used 
library("dplyr") 
library("ggplot2") 
library("mice") 
library("corrgram") 
#5 To check missing data 
sum(complete.cases(data)) 
sum(!complete.cases(data)) 
#6 Creating new data variable to do manipulations without altering original data 

dataplot <- data 
head(dataplot) 
#7 No. of transactions through all Advertisement Channels & Seen Voucher 
# Advertisement Channel 
table(dataplot$Advertisement_Channel) 
# Seen Voucher 
table(dataplot$Seen_Voucher) 
#8 Dummy variables for Linear Regression model 
dataplot$Leaflet <- ifelse(dataplot$Advertisement_Channel == 1 , 1 , 0) 
dataplot$Social_Media <- ifelse(dataplot$Advertisement_Channel == 2 , 1 , 0) 
dataplot$Search_Engine <- ifelse(dataplot$Advertisement_Channel == 3 , 1 , 0) 
dataplot$Influencer <- ifelse(dataplot$Advertisement_Channel == 4 , 1 , 0) 
#9 Only to be used for Corrgram 
corrgram(data) 
# effect of all advertisement channels on rest of the variables 
corrgram(dataplot) 
# effect of all advertisement channels on variable advertisement 
corrgram(dataplot[c(5,7,8,9,10)]) 
# effect of all adv channels on revenue separately 
corrgram(dataplot[c(6,7,8,9,10)]) 
# Nullifying the unnecessary column for the computation 
# as we have to use leaflet as base so we would nullify it for the model 
dataplot$Leaflet <- NULL 
# as we have to check the impact of all variables on revenue so we nullify- 
# -this as well for model's sake 

dataplot$Advertisement_Channel <- NULL 
# PLOTS 
# Let's analyse the data with the plots !! 
# Create ggplot variable 
p <- ggplot(data=dataplot) 
# Study 1 : Revenue behavior for Different AD Channels 
# Scatter-Plot 
p + geom_point(aes(as.factor(Advertisement_Channel), 
                   Revenue, color = as.factor(Advertisement_Channel))) + 
  labs(title = "Revenue vs AD Channels", 
       caption = "Data from Transaction Customers", 
       tag = "Figure 5",x = "AD Channels",y = "Revenue") + 
  scale_color_discrete(name = "Advertisement Channels", 
                       labels = c("Leaflet", "Social Media", 
                                  "Search Engine", "Influencer")) 
# Box-Plot 
p + geom_boxplot(aes(as.factor(Advertisement_Channel), Revenue))+ 
  labs(title = "Revenue Vs Ad Channels", 
       caption = "Data from Transaction Customers", 
       tag = "Figure 6",x = "AD Channels",y = "Revenue") 
# Box-Plot + Scatter-Plot 
p + geom_boxplot(aes(as.factor(Advertisement_Channel), 
                     Revenue, color = as.factor(Seen_Voucher)))+ 
  labs(title = "Revenue Vs Ad Channels", 
       
       caption = "Data from Transaction Customers", 
       tag = "Figure 7",x = "AD Channels",y = "Revenue") + 
  scale_color_discrete(name = "Vouchers", 
                       labels = c("Voucher Not Seen", "Voucher Seen")) 
# Study : Revenue behavior for Seen Vouchers 
# Box-Plot - Seen Vouchers 
p + geom_boxplot(aes(as.factor(Seen_Voucher), Revenue))+ 
  labs(title = "Revenue VS Seen Vouchers", 
       caption = "Data from Transaction Customers", 
       tag = "Figure 8",x = "Seen Voucher",y = "Revenue") 
# Box-Plot - AD Channels 
p + geom_boxplot(aes(as.factor(Seen_Voucher), Revenue, 
                     color = as.factor(Advertisement_Channel)))+ 
  labs(title = "Revenue VS Seen Vouchers", 
       caption = "Data from Transaction Customers", 
       tag = "Figure 9",x = "Seen Voucher",y = "Revenue") + 
  scale_color_discrete(name = "Advertisement Channels", 
                       labels = c("Leaflet", "Social Media", 
                                  "Search Engine", "Influencer")) 
# Study : Revenue behavior with Estimated Income 
# Scatter-Plot - Seen Vouchers 
p + geom_point(aes(Estimated_Age, Revenue, color = as.factor(Seen_Voucher)))+ 
  labs(title = "Revenue vs Estimated Age", 
       caption = "Data from Transaction Customers", 
       tag = "Figure 10 ",x = "Estimated Age",y = "Revenue") +  
  
  scale_color_discrete(name = "Vouchers", 
                       labels = c("Voucher Not Seen", "Voucher Seen")) 
# Scatter-Plot - Advertisement Channels 
p + geom_point(aes(Estimated_Age, Revenue, 
                   color = as.factor(Advertisement_Channel)))+ 
  labs(title = "Revenue vs Estimated Age", 
       caption = "Data from Transaction Customers", 
       tag = "Figure 11",x = "Estimated Age",y = "Revenue") + 
  scale_color_discrete(name = "Advertisement Channels", 
                       labels = c("Leaflet", "Social Media", 
                                  "Search Engine", "Influencer")) 
# Study : Revenue behavior on the basis of Time on Site 
# Scatter-Plot - Basic 
p + geom_point(aes(Time_On_Site, Revenue))+ 
  labs(title = "Revenue vs Time on Site", 
       caption = "Data from Transaction Customers", 
       tag = "Figure 12",x = "Time on Site",y = "Revenue") 
# Scatter-Plot - Seen Voucher 
p + geom_point(aes(Time_On_Site, Revenue, color = as.factor(Seen_Voucher)))+ 
  labs(title = "Revenue vs Time on Site", 
       caption = "Data from Transaction Customers", 
       tag = "Figure 13",x = "Time On Site",y = "Revenue") + 
  scale_color_discrete(name = "Seen Voucher", 
                       labels = c("Voucher not Seen", "Voucher Seen")) 

# Study : Revenue behavior on the basis of Estimated Income 
p + geom_point(aes(Estimated_Income, Revenue)) + 
  labs(title = "Revenue vs Estimated Income", 
       caption = "Data from Transaction Customers", 
       tag = "Figure 14",x = "Estimated Income",y = "Revenue") 
# Study : Revenue behavior on the basis of Estimated Income and Seen Voucher 
p + geom_point(aes(Estimated_Income, Revenue, color=as.factor(Seen_Voucher))) + 
  labs(title = "Revenue vs Estimated Income", 
       caption = "Data from Transaction Customers", 
       tag = "Figure 15",x = "Estimated Income",y = "Revenue")+ 
  scale_color_discrete(name = "Seen Voucher", 
                       labels = c("Voucher not Seen", "Voucher Seen")) 
#Assumption 3 : Checking for Linear correlation 
# Revenue vs Estimated Age 
p + geom_point(aes(Estimated_Age, Revenue))+ 
  labs(title = "Revenue vs Estimated Age", 
       caption = "Data from Transaction Customers", 
       tag = "Figure 16",x = "Estimated Age",y = "Revenue") + 
  geom_abline(slope=0, intercept=83) 
# Revenue vs Time on Site 
p + geom_point(aes(Time_On_Site, Revenue))+ 
  labs(title = "Revenue vs Time on Site", 
       caption = "Data from Transaction Customers", 
       tag = "Figure 17",x = "Time on Site",y = "Revenue") + 
  geom_abline(slope=0, intercept=80) 

# Revenue vs Estimated Income 
p + geom_point(aes(Estimated_Income, Revenue, color=as.factor(Seen_Voucher)))+ 
  geom_abline(slope=0.0095, intercept=-123) + 
  geom_abline(slope=0.0060, intercept=-70) + 
  labs(title = "Revenue vs Estimated Income", 
       caption = "Data from Transaction Customers", 
       tag = "Figure 18",x = "Estimated Income",y = "Revenue")+ 
  scale_color_discrete(name = "Seen Voucher", 
                       labels = c("Voucher not Seen", "Voucher Seen")) 
# Model : Linear Regression 
model1 <- lm(Revenue ~ ., data=dataplot) 
summary(model1) 
# Model 2 : Excluding the variables having negligible impact 
model2 <- lm(Revenue ~ Seen_Voucher + Estimated_Income + 
               Social_Media + Search_Engine + Influencer, data=dataplot) 
summary(model2) 
# Task 2 
# Top 5 Rows of data 
head(dataplot) 
# Option 1 : Advertisement for Age Group >= 45 
age_45 <- ifelse(dataplot$Estimated_Age>=45,1,0) 
table(age_45) 
# Option 2 : Provide a voucher for 20GBP 
table(dataplot$Seen_Voucher) 

# Option 3 : Advertisement through Influencer 
table(dataplot$Influencer)