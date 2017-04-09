
library(data.table)

# Read in the data for the project
BasketData<-read.csv("Data/BasketData.csv")
colnames(BasketData)<-c("TransactionID", "ClassDesc", "DeptDesc", "Brand", "Units")
BasketData$Units<-as.numeric(gsub(",", "", as.character(BasketData$Units)))

# Since our objective is to figure out market baskets that include redbull, let's create a dataset that only includes rebull
# but in a format that we can play with for data visualization first

# print colnames
colnames(BasketData)

# summarize the dataset
summary(BasketData)
str(BasketData)

# how many unique brands?
length(unique(BasketData$Brand))
# looks like 1963 unique brands in the dataset

# how many unique department descriptions? # show bargraph count of these items but products purchased
length(unique(BasketData$DeptDesc)) # 25

# how many unique classes?
length(unique(BasketData$ClassDesc)) # 747 classes

# which ones are most popular?
dt <- data.table(BasketData)
head(dt[,sum(Units),by = Brand],20)
# it looks like DG body is the most popular item here.  So...the dataset looks like it is Dollar General
# WE also see that the 12th most purchased product has no brand
# let's look at these values to see if there is an issue here

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
# interesting that this is in the noncarbonated from the looks of things...let's make sure this is the only class
unique(df_analysis_redbull$ClassDesc)
# this seems really odd...


# what percent of transactions contain redbull?

length(unique(df_analysis_redbull$TransactionID))/
  length(unique(BasketData$TransactionID)) # looks like about 0.5% of all transactions have a redbull

# that's nice, but what does the Redbull share look like? Need to see of the 


# average cans purchased?
