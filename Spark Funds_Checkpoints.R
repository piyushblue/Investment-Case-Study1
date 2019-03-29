#Investment Case Study: Spark Funds
library(dplyr)

#CHECKPOINT-1 DATA CLEANING 1

#Load companies data in R
companies <- read.delim("companies.txt", header = TRUE, stringsAsFactors = FALSE, sep = "\t")

#convert all upper case values to Lower case to remove duplication
companies$permalink <- tolower(companies$permalink)
str(companies)
summary(companies)

#Load Rounds2 data in R
rounds2 <- read.csv("rounds2.csv", header = TRUE, stringsAsFactors = FALSE)

#convert all upper case values to Lower case to remove duplication
rounds2$company_permalink <- tolower(rounds2$company_permalink)
str(rounds2)
summary(rounds2)

#unique count of companies in companies dataframe

unique_permalink <- n_distinct(companies$permalink)


#unique count of companies in rounds2 dataframe

unique_rounds2 <- n_distinct(rounds2$company_permalink)


# comparison of companies and rounds2 data frame based on permalink.

compare_permalink <- length(rounds2$company_permalink %in% companies$permalink)
compare_permalink

compare_permalink1<- length(rounds2$company_permalink[!rounds2$company_permalink %in% companies$permalink])
compare_permalink1

#For compare_permalink the output achieved is equal to number of observation in rounds2
#For compare_permalink1 the output achieved is "0" which indicates that there are no rogues values
# hence there are no companies in rounds2 dataframe that are not present in companies data frame.

#Merge the two data frames so that all variables (columns) in the companies frame 
#are added to the rounds2 data frame with left outer join.

master_frame <- merge(rounds2,companies, by.x = c("company_permalink"), by.y = c("permalink"), all.x = TRUE)
str(master_frame)
summary(master_frame)
masterframe_obvs <- nrow(master_frame)


#CHECKPOINT-2 FUNDING TYPE ANALYSIS

#Creatind individual databased based on target funding types

venture <- master_frame[master_frame$funding_round_type == "venture", ]
seed <-master_frame[master_frame$funding_round_type == "seed", ]
angel <- master_frame[master_frame$funding_round_type == "angel", ]
private_equity <- master_frame[master_frame$funding_round_type == "private_equity", ]

#Calculations to decide the treatement of NA values for funding type.

Venture_percentage <- sum(is.na(venture$raised_amount_usd))/nrow(venture)*100
seed_percentage <- sum(is.na(seed$raised_amount_usd))/nrow(seed)*100
angle_percentage <- sum(is.na(angel$raised_amount_usd))/nrow(angel)*100
pe_percentage <- sum(is.na(private_equity$raised_amount_usd))/nrow(private_equity)*100

#The percentage NA values for all target funding types seems close to or greater than 5% 
#Hence NA values need to be replaced with either mean, median or mode.
#checking to see any outliers in the respective funding types

plot(venture$raised_amount_usd)
boxplot(venture$raised_amount_usd)
plot(seed$raised_amount_usd)
boxplot(seed$raised_amount_usd)
plot(angel$raised_amount_usd)
boxplot(angel$raised_amount_usd)
plot(private_equity$raised_amount_usd)
boxplot(private_equity$raised_amount_usd)


#All funding types seem to have outliers hence in order to maintain data integrity
#choosing to replace all NA's values of respective funding type groups with respective group medians


#Calculating Median funding for all funding types by grouping
median_funding <- aggregate(raised_amount_usd~funding_round_type, data=master_frame, median)


#Replacing NA values group wise with respective medians

temp_w <- as.integer(median_funding[which(median_funding$funding_round_type =="venture"),2])
venture[which(is.na(venture$raised_amount_usd)),"raised_amount_usd"] <- temp_w

temp_x <- as.integer(median_funding[which(median_funding$funding_round_type =="seed"),2])
seed[which(is.na(seed$raised_amount_usd)),"raised_amount_usd"] <- temp_x

temp_y <- as.integer(median_funding[which(median_funding$funding_round_type =="angel"),2])
angel[which(is.na(angel$raised_amount_usd)),"raised_amount_usd"] <- temp_y

temp_z <- as.integer(median_funding[which(median_funding$funding_round_type =="private_equity"),2])
private_equity[which(is.na(private_equity$raised_amount_usd)),"raised_amount_usd"] <- temp_z


#Calculating Average of Venture Funding

avg_venture <- aggregate(raised_amount_usd~funding_round_type, data = venture, mean)


#Calculating Average of seed Funding

avg_seed <- aggregate(raised_amount_usd~funding_round_type, data = seed, mean)


#Calculating Average of angel Funding

avg_angel <- aggregate(raised_amount_usd~funding_round_type, data = angel, mean)


#Calculating Average of private equity Funding

avg_pe <- aggregate(raised_amount_usd~funding_round_type, data = private_equity, mean)

#Combining individual data frames into one data frame

target_avg_funding <- rbind(avg_venture,avg_seed,avg_angel,avg_pe)

# Considering that Spark Funds wants to invest between 
#5 to 15 million USD per investment round, 
#which investment type is the most suitable for it?


suit_invesment <- target_avg_funding$funding_round_type[which(target_avg_funding$raised_amount_usd>=5000000 & target_avg_funding$raised_amount_usd<=15000000)]
suit_invesment

#Output based on applied parameters clearly shows that "VENTURE" is the most suitable funding type

#CHECKPOINT-3 Country Analysis

#summarizing top9 countries based on total investment funds received excluding countries with null values

top9<- aggregate(raised_amount_usd~country_code, data = venture, sum)
top9 <- top9[which((top9$country_code!="")) , ]
top9 <- head(top9[order(-top9$raised_amount_usd), ], n=9)

#Importing external English speaking country list
Eng_Cntry_list <- read.csv ("Country code list.csv", header = TRUE, stringsAsFactors = FALSE)

#Filtering Top3 English speaking countries with "Venture" Funding

top3_eng_venture <- head(top9[(top9$country_code %in% Eng_Cntry_list$Country_code), ], n=3)


#CHECKPOINT 4 SECTOR ANALYSIS

mapping <- read.csv("mapping.csv", header = TRUE, stringsAsFactors = FALSE)
mapping$Blanks <- NULL
mapping <- mapping[-1, ]

library(tidyr)

long_mapping <- gather(mapping, main_sector, my_val,Automotive...Sports:Social..Finance..Analytics..Advertising,na.rm = FALSE)

#remove all varialbes with "0" values

long_mapping <- long_mapping[!(long_mapping$my_val ==0), ]
long_mapping$my_val <- NULL

#data cleaning by replacing "0" in category list values with "na"

long_mapping$category_list<-gsub("0","na", long_mapping$category_list)
long_mapping$category_list<-gsub("\\.na",".0", long_mapping$category_list)


#setting up data for merging

long_mapping$category_list <- tolower(long_mapping$category_list)
venture$primary_sector <- gsub("\\|.*","",venture$category_list)
venture$primary_sector <- tolower(venture$primary_sector)

#merging the long format of mapping data frame & venture type funding database using common data column
venture_masterframe <- merge(venture, long_mapping, by.x = c("primary_sector"), by.y = ("category_list"), all.x = TRUE)
venture_masterframe <- na.omit(venture_masterframe,cols = "venture_masterframe$main_sector")


#Checkpoint 5 Sector Analysis 2

D1 <- subset(venture_masterframe, venture_masterframe$country_code == "USA" & venture_masterframe$raised_amount_usd >= 5000000 & venture_masterframe$raised_amount_usd <= 15000000)

D2 <- subset(venture_masterframe, venture_masterframe$country_code == "GBR" & venture_masterframe$raised_amount_usd >= 5000000 & venture_masterframe$raised_amount_usd <= 15000000)

D3 <- subset(venture_masterframe, venture_masterframe$country_code == "IND" & venture_masterframe$raised_amount_usd >= 5000000 & venture_masterframe$raised_amount_usd <= 15000000)



# For D1 data frame

D1_count_total_investment <- nrow(D1)
D1_sum_total_investment <- sum(D1$raised_amount_usd)

D1_sector_sum <- aggregate(raised_amount_usd~main_sector, data = D1, sum)
D1_sector_count <- aggregate(primary_sector~main_sector,data=D1, length)
D1_sector_sum_count <- merge(D1_sector_sum, D1_sector_count, by ="main_sector")
names(D1_sector_sum_count)[names(D1_sector_sum_count) == "raised_amount_usd"] <- "sum_investment_bysector"
names(D1_sector_sum_count)[names(D1_sector_sum_count) == "primary_sector"] <- "count_investment_bysector"
D1 <- merge(D1,D1_sector_sum_count, by = "main_sector",all.x = TRUE)
D1_top3 <- head(arrange(D1_sector_sum_count, desc(count_investment_bysector)),n=3)

#Company with highest investment for TOP Sector for D1
t1_D1 <- subset(D1,D1$main_sector == D1_top3[1,1])
t1_D1_group_by <- group_by(t1_D1,company_permalink,name)
t1_D1_sum <- summarise(t1_D1_group_by,sum(raised_amount_usd))
t1_D1_order<-head(t1_D1_sum[order(-t1_D1_sum$`sum(raised_amount_usd)`), ], n=1)

#Company with highest investment for SECOND BEST Sector for D1
t2_D1 <- subset(D1,D1$main_sector == D1_top3[2,1])
t2_D1_group_by <- group_by(t2_D1,company_permalink,name)
t2_D1_sum <- summarise(t2_D1_group_by,sum(raised_amount_usd))
t2_D1_order<-head(t2_D1_sum[order(-t2_D1_sum$`sum(raised_amount_usd)`), ], n=1)



#For D2 data Frame


D2_count_total_investment <- nrow(D2)
D2_sum_total_investment <- sum(D2$raised_amount_usd)
D2_sum_total_investment
D2_sector_sum <- aggregate(raised_amount_usd~main_sector, data = D2, sum)
D2_sector_count <- aggregate(primary_sector~main_sector,data=D2, length)
D2_sector_sum_count <- merge(D2_sector_sum, D2_sector_count, by ="main_sector")
names(D2_sector_sum_count)[names(D2_sector_sum_count) == "raised_amount_usd"] <- "sum_investment_bysector"
names(D2_sector_sum_count)[names(D2_sector_sum_count) == "primary_sector"] <- "count_investment_bysector"
D2 <- merge(D2,D2_sector_sum_count, by = "main_sector",all.x = TRUE)
D2_top3 <- head(arrange(D2_sector_sum_count, desc(count_investment_bysector)),n=3)

#Company with highest investment for TOP Sector for D2
t1_D2 <- subset(D2,D2$main_sector == D2_top3[1,1])
t1_D2_group_by <- group_by(t1_D2,company_permalink,name)
t1_D2_sum <- summarise(t1_D2_group_by,sum(raised_amount_usd))
t1_D2_order<-head(t1_D2_sum[order(-t1_D2_sum$`sum(raised_amount_usd)`), ], n=1)

#Company with highest investment for SECOND BEST Sector for D2
t2_D2 <- subset(D2,D2$main_sector == D2_top3[2,1])
t2_D2_group_by <- group_by(t2_D2,company_permalink,name)
t2_D2_sum <- summarise(t2_D2_group_by,sum(raised_amount_usd))
t2_D2_order<-head(t2_D2_sum[order(-t2_D2_sum$`sum(raised_amount_usd)`), ], n=1)

#For D3 data Frame
D3_count_total_investment <- nrow(D3)
D3_sum_total_investment <- sum(D3$raised_amount_usd)
D3_sum_total_investment
D3_sector_sum <- aggregate(raised_amount_usd~main_sector, data = D3, sum)
D3_sector_count <- aggregate(primary_sector~main_sector,data=D3, length)
D3_sector_sum_count <- merge(D3_sector_sum, D3_sector_count, by ="main_sector")
names(D3_sector_sum_count)[names(D3_sector_sum_count) == "raised_amount_usd"] <- "sum_investment_bysector"
names(D3_sector_sum_count)[names(D3_sector_sum_count) == "primary_sector"] <- "count_investment_bysector"
D3 <- merge(D3,D3_sector_sum_count, by = "main_sector", all.x = TRUE)
D3_top3 <- head(arrange(D3_sector_sum_count, desc(count_investment_bysector)),n=3)

#Company with highest investment for TOP Sector for D3
t1_D3 <- subset(D3,D3$main_sector == D3_top3[1,1])
t1_D3_group_by <- group_by(t1_D3,company_permalink,name)
t1_D3_sum <- summarise(t1_D3_group_by,sum(raised_amount_usd))
t1_D3_order<-head(t1_D3_sum[order(-t1_D3_sum$`sum(raised_amount_usd)`), ], n=1)

#Company with highest investment for SECOND BEST Sector for D3
t2_D3 <- subset(D3,D3$main_sector == D3_top3[2,1])
t2_D3_group_by <- group_by(t2_D3,company_permalink,name)
t2_D3_sum <- summarise(t2_D3_group_by,sum(raised_amount_usd))
t2_D3_order<-head(t2_D3_sum[order(-t2_D3_sum$`sum(raised_amount_usd)`), ], n=1)

#Extracting all required tables for Tableaus plots

write.csv(master_frame, "master_frame.csv", row.names = FALSE)
write.csv(top9, "top9.csv",row.names = FALSE)
write.csv(D1_top3, "D1_top3.csv",row.names = FALSE)
write.csv(D2_top3, "D2_top3.csv",row.names = FALSE)
write.csv(D3_top3, "D3_top3.csv",row.names = FALSE)
write.csv(D1, "D1.csv",row.names = FALSE)
write.csv(D2, "D2.csv",row.names = FALSE)
write.csv(D3, "D3.csv",row.names = FALSE)
write.csv(top3_eng_venture, "top3_eng.csv", row.names = FALSE)
