
library(data.table)
library(ggplot2)
library(sqldf)

packages<- c ("data.table", "ggplot2", "sqldf")

sapply(packages,library, character.only = TRUE)

# Read in the data for the project
BasketData<-read.csv("Data/BasketData.csv")
colnames(BasketData)<-c("TransactionID", "ClassDesc", "DeptDesc", "Brand", "Units")
BasketData$Units<-as.numeric(gsub(",", "", as.character(BasketData$Units)))

# remove the negatives from the overall DF

BasketData<- BasketData[!BasketData$Units <=0,]


# Since our objective is to figure out market baskets that include redbull, let's create a dataset that only includes rebull
# but in a format that we can play with for data visualization first

# print colnames
colnames(BasketData)

# summarize the dataset
summary(BasketData)
str(BasketData)

# how many unique brands?
length(unique(BasketData$Brand))
# looks like 1959 unique brands in the dataset

# how many unique department descriptions? # show bargraph count of these items but products purchased
length(unique(BasketData$DeptDesc)) # 25

# how many unique classes?
length(unique(BasketData$ClassDesc)) # 746 classes

# which ones are most popular?
dt <- data.table(BasketData)
dt <- dt[,sum(Units),by = Brand]
dt <- dt[order(V1)]
tail(dt,20)
# it looks like the store brand, clover valley is the most popular, to no surprise
# we also see that unbranded and DG HOME are some of the more popular products as well.  
# Additionally, we see that the blank records are the second most popular.  WE should probalby delete those
# least popular?
head(dt,20)
# dude...no way funfetti is not that popular!!!

# looking at blanks
df_analysis<- BasketData[BasketData$Brand == "",]
#df_analysis<- BasketData[which(is.na(BasketData$Brand)),] # doesnt work because it is not an NA

# going up one level to get an idea of the breakdown
length(unique(df_analysis$DeptDesc)) # looks like we have values from all departments with no brand

dfa_dt = data.table(df_analysis)
dfa_dt[,sum(Units),by = DeptDesc]
# it looks like most transactions without brand are candy.  We dont necessarily want to filter these out since
# we will be looking at the data at a much higher level for the RedBull market basket.




# what portion of transactions include redbull?

df_analysis_redbull<- BasketData[BasketData$Brand == "RED BULL",]

length(unique(df_analysis_redbull$TransactionID)) # 15732 transactions have redbull
length(unique(BasketData$TransactionID)) # 2928914

head(df_analysis_redbull)
# interesting that this is in the noncarbonated from the looks of things.
# there are not other classes, so this must be the class it is placed in.

# how many redbulls is someone purchasing on average?
mean(df_analysis_redbull$Units) # looks like about 1.33 units per transaction
max(df_analysis_redbull$Units)# dang!  14
min(df_analysis_redbull$Units) # -2 ? must include all refunded transactions?

# we will drop those out of the dataset

df_analysis_redbull<- df_analysis_redbull[!df_analysis_redbull$Units <=0,]



# let's look at a distribution

qplot(Units, data = df_analysis_redbull, geom = 'histogram')
# the overwhelming majority of transactions occur 1 can at a time


# what percent of transactions contain redbull?

length(unique(df_analysis_redbull$TransactionID))/
  length(unique(BasketData$TransactionID)) # looks like about 0.5% of all transactions have a redbull

# that's nice, but what does the Redbull share look like? Need to see of the candy and snacks trans and non-carb

length(unique(df_analysis_redbull$TransactionID)) # 15732 transactions with redbull

length(unique(BasketData$TransactionID[BasketData$DeptDesc=='CANDY & SNACKS'])) # 1,447,064 with candy or snacks
length(unique(BasketData$TransactionID)) #2,928,914 total transactions

length(unique(BasketData$TransactionID[BasketData$DeptDesc=='CANDY & SNACKS']))/length(unique(BasketData$TransactionID))
# wow!  50% of all transactions contain candy or snacks

length(unique(df_analysis_redbull$TransactionID))/length(unique(BasketData$TransactionID[BasketData$DeptDesc=='CANDY & SNACKS']))
# 1.1% of candy or snack transactions contain redbull


# I wonder what the most popular things are that are purchased in redbull transactions beside redbull?

# get the redbull transactions items without redbull in the dataset
redbull_plus_snack_df<-sqldf('select TransactionID, ClassDesc, DeptDesc, Brand, Units  From BasketData Where 
      TransactionID in (select distinct TransactionID From BasketData Where Brand = "RED BULL")
                             And Brand<> "RED BULL"')

length(unique(redbull_plus_snack_df$TransactionID)) 
# alright, we know that 15732 transactions contained redbull; However, only 13058 contained other products
# looking deeper into those products

# what Brands are most often purchased with RedBull?
  # frequency
  agg_df<- sqldf('Select Brand, Count(*) As trans, Sum(Units) As units_purch  From redbull_plus_snack_df Group By 1 Order By 3')
  
  
  barplot(tail(agg_df$units_purch,20), names.arg = tail(agg_df$Brand,20), cex.names = 0.7, las = 2 )

# number of units

# How many units do these transactions contain?

# Which department do they come from?

# which classes are most popular?

# Of the snacks and candy classes which do people purchase together?

