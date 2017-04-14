

packages<- c ("data.table", "ggplot2", "sqldf")

sapply(packages,library, character.only = TRUE)

# Read in the data for the project
BasketData<-read.csv("Data/BasketData.csv")
colnames(BasketData)<-c("TransactionID", "ClassDesc", "DeptDesc", "Brand", "Units")
BasketData$Units<-as.numeric(gsub(",", "", as.character(BasketData$Units)))


#################################
# Initial EDA -validation #####
###############################


# remove the negatives from the overall DF

BasketData<- BasketData[!BasketData$Units <=0,]




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
head(dt,40)
# dude...no way funfetti is not that popular!!!

# looking at blanks
df_analysis<- BasketData[BasketData$Brand == "",]
#df_analysis<- BasketData[which(is.na(BasketData$Brand)),] # doesnt work because it is not an NA

# going up one level to get an idea of the breakdown
length(unique(df_analysis$DeptDesc)) # looks like we have values from all departments with no brand

dfa_dt <- data.table(df_analysis)
dfa_dt <- dfa_dt[,sum(Units),by = DeptDesc]
dfa_dt <- dfa_dt[order(-V1),]
print(dfa_dt)
# it looks like most transactions without brand are stationary
# Stationary is the most "keyed in" item; 
# in this case, we would probably want to filter, maybe we should look at the classes
dfa_dt<- data.table(df_analysis)
dfa_dt<- dfa_dt[,length(Units),by =ClassDesc]
dfa_dt<- dfa_dt[order(-V1),]
print(dfa_dt)

############################################
##### MAIN FiLTERING OF DATASET ############
############################################
# The first thing we will do is reread in the dataset and set everything up

BasketData<-read.csv("Data/BasketData.csv")
colnames(BasketData)<-c("TransactionID", "ClassDesc", "DeptDesc", "Brand", "Units")
BasketData$Units<-as.numeric(gsub(",", "", as.character(BasketData$Units)))


# let's take out the transactions that contain negative values
  # notice that we are taking out distinct transactions and not just those items. 
  # this ensures that we get whole itemsets that are not incomplete
BasketData<- sqldf('Select TransactionID, ClassDesc, DeptDesc, Brand, Units
                    From BasketData Where TransactionID Not In  
                      (Select Distinct
                        TransactionID
                        From BasketData
                        Where 
                        Units<=0)')



# ok, that looks like it worked.  Now, we need to see what removal of the transactions without any brand looks like

BasketData<- sqldf('Select TransactionID, ClassDesc, DeptDesc, Brand, Units
                    From BasketData Where TransactionID Not In  
                      (Select Distinct
                        TransactionID
                        From BasketData
                        Where 
                        Brand="")')








##########################
#### REDBULL ANALYSIS ####
##########################

# what portion of transactions include redbull?

df_analysis_redbull<- BasketData[BasketData$Brand == "RED BULL",]

length(unique(df_analysis_redbull$TransactionID)) # 14046 transactions have redbull
length(unique(BasketData$TransactionID)) # 2493458

head(df_analysis_redbull)
unique(df_analysis_redbull$ClassDesc)
# interesting that this is in the noncarbonated from the looks of things.
# there are not other classes, so this must be the class it is placed in.

# how many redbulls is someone purchasing on average?
mean(df_analysis_redbull$Units) # looks like about 1.32 units per transaction
max(df_analysis_redbull$Units)# dang!  14
min(df_analysis_redbull$Units) # -2 ? must include all refunded transactions? - Fixed
summary(df_analysis_redbull$Units)
# we will drop those out of the dataset

#df_analysis_redbull<- df_analysis_redbull[!df_analysis_redbull$Units <=0,]
#### No lOnger needed


# let's look at a distribution

qplot(Units, data = df_analysis_redbull, geom = 'histogram')
# the overwhelming majority of transactions occur 1 can at a time


# what percent of transactions contain redbull?

length(unique(df_analysis_redbull$TransactionID))/
  length(unique(BasketData$TransactionID)) # looks like about 0.56% of all transactions have a redbull

# that's nice, but what does the Redbull share look like? Need to see of the candy and snacks trans and non-carb

length(unique(df_analysis_redbull$TransactionID))# 14046 transactions with redbull

length(unique(BasketData$TransactionID[BasketData$DeptDesc=='CANDY & SNACKS'])) # 1,217,781 with candy or snacks
length(unique(BasketData$TransactionID)) #2,493,458 total transactions

length(unique(BasketData$TransactionID[BasketData$DeptDesc=='CANDY & SNACKS']))/length(unique(BasketData$TransactionID))
# wow!  49% of all transactions contain candy or snacks

length(unique(df_analysis_redbull$TransactionID))/length(unique(BasketData$TransactionID[BasketData$DeptDesc=='CANDY & SNACKS']))
# 1.1% of candy or snack transactions contain redbull

# What about competitors?
df_analysis_monster<- BasketData[BasketData$Brand == "MONSTER",]


length(unique(df_analysis_monster$TransactionID)) # 25,338 transactions have Monster


# how many redbulls is someone purchasing on average?
mean(df_analysis_monster$Units) # looks like about 1.32 units per transaction
max(df_analysis_monster$Units)# dang!  14
min(df_analysis_monster$Units) # -2 ? must include all refunded transactions? - Fixed

# could be skewed by the 47 can dude
median(df_analysis_monster$Units)
median(df_analysis_redbull$Units)
  # ok, that was pointless

# let's look at side by side boxplots
par(mfrow = c(1,2))

# create the dataframe
rb <- data.frame(rep('RedBull', length(unique(df_analysis_redbull$TransactionID))))
df1<- cbind(rb, data.frame(df_analysis_redbull$Units))
colnames(df1) = c('brand','units')

mon <- data.frame(rep('Monster', length(unique(df_analysis_monster$TransactionID))))
df2<- cbind(mon, data.frame(df_analysis_monster$Units))
colnames(df2) = c('brand','units')


df3<- rbind(df1,df2)

boxplot(units~brand, data = df3)

# looks like 42 from Monster skews things a bit, let's filter it

df3<- df3[df3$units<30,]

boxplot(units~brand, data = df3)

# ok, not the best representation of the data, but we can see that the realy average transaction is slightly higher for monster than RedBull

# some more redbull analysis...

# How many items are purchased in each redbull transaction?

redbull_items_purchased<-sqldf('select avg(brands) As num_brands, avg(total_units) As avg_units From
(select TransactionID,count(Distinct(Brand)) As brands,
Sum(Units) AS total_units  From BasketData Where 
      TransactionID in (select distinct TransactionID From BasketData Where Brand = "RED BULL") Group By 1)' )

print(redbull_items_purchased)

# the average basket with redbull items is about 5 different items and about 4 brands.  


# get the redbull transactions items without redbull in the dataset
redbull_plus_snack_df<-sqldf('select TransactionID, ClassDesc, DeptDesc, Brand, Units  From BasketData Where 
      TransactionID in (select distinct TransactionID From BasketData Where Brand = "RED BULL")
                             And Brand<> "RED BULL"')

length(unique(redbull_plus_snack_df$TransactionID)) 
# alright, we know that 14046 transactions contained redbull; However, only 11,375 contained other products
# looking deeper into those products

# what Brands are most often purchased with RedBull?
  # frequency
brand_df<- sqldf('Select Brand, Count(*) As trans, Sum(Units) As units_purch  From redbull_plus_snack_df Group By 1 Order By 3')
  
  
barplot(tail(brand_df$units_purch,20), names.arg = tail(brand_df$Brand,20), cex.names = 0.7, las = 2 )



# Which department do they come from?

department_df<- sqldf('Select DeptDesc, Count(*) As trans, Sum(Units) As units_purch  From redbull_plus_snack_df Group By 1 Order By 3')

barplot(tail(department_df$units_purch,20), names.arg = tail(department_df$DeptDesc,20), cex.names = 0.7, las = 2 )

# which classes are most popular?

class_df<- sqldf('Select ClassDesc, Count(*) As trans, Sum(Units) As units_purch  From redbull_plus_snack_df Group By 1 Order By 3')

barplot(tail(class_df$units_purch,20), names.arg = tail(class_df$ClassDesc,20), cex.names = 0.7, las = 2 )


# which candy and snacks are most popular with Redbull
  # df including all candy and snack items purchased with no redbull
candy_snack_df<- sqldf('Select Brand, Count(*) As trans, Sum(Units) As units_purch  
                      From redbull_plus_snack_df Where DeptDesc ="CANDY & SNACKS" Group By 1 Order By 3')

barplot(tail(candy_snack_df$units_purch,20), names.arg = tail(candy_snack_df$Brand,20), cex.names = 0.7, las = 2 )



# Let's look at the multi item transactions to see what the most purchased products are by bin


# we will do this by brand for simplicity, so 2 brand purchases with redbull? what is the most popular? etc.


redbull_brands_in_trans<-sqldf('select T1.TransactionID As TransactionID ,brands, ClassDesc, DeptDesc, Brand, Units  
From BasketData As T1
Inner Join ( Select TransactionID, Count(Distinct(Brand)) As brands From BasketData Group By 1) As T2
On T1.TransactionID = T2.TransactionID
Where T1.TransactionID in (select distinct TransactionID From BasketData Where Brand = "RED BULL")
 
                               ')
  
# ok, now that we got the aggregation done, let's group by brands and produce the same graphs

# 2 brand transactions (filter out RB)

two_brand_trans<- sqldf('select brand, sum(Units) As units From redbull_brands_in_trans
                        Where
                        Brand <> "RED BULL"
                        And brands = 2
                        Group By 1
                        Order By 2

                        ')

barplot(tail(two_brand_trans$units,20), names.arg = tail(two_brand_trans$Brand,20), cex.names = 0.7, las = 2 )

# 3 brand transactions (filter out RB)

three_brand_trans<- sqldf('select brand, sum(Units) As units From redbull_brands_in_trans
                        Where
                        Brand <> "RED BULL"
                        And brands = 3
                        Group By 1
                        Order By 2
                        
                        ')

barplot(tail(three_brand_trans$units,20), names.arg = tail(three_brand_trans$Brand,20), cex.names = 0.7, las = 2 )


# 4 brand transactions (filter out RB)

four_brand_trans<- sqldf('select brand, sum(Units) As units From redbull_brands_in_trans
                          Where
                          Brand <> "RED BULL"
                          And brands = 4
                          Group By 1
                          Order By 2
                          
                          ')

barplot(tail(four_brand_trans$units,20), names.arg = tail(four_brand_trans$Brand,20), cex.names = 0.7, las = 2 )

# 5 brand transactions (filter out RB)

five_brand_trans<- sqldf('select brand, sum(Units) As units From redbull_brands_in_trans
                          Where
                         Brand <> "RED BULL"
                         And brands = 5
                         Group By 1
                         Order By 2
                         
                         ')

barplot(tail(five_brand_trans$units,20), names.arg = tail(five_brand_trans$Brand,20), cex.names = 0.7, las = 2 )


