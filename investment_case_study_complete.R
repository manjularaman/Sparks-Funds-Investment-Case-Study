#Working directory to local directory
#setwd ("C:/Users/MANJULA/datascience/investment_case_study")
##Packages
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("stringr")
        library(tidyr)
        library(dplyr)
        library(stringr)

#######cHECKPOINT1########
#Reading data into data frames
companies <- read.delim("companies.txt",header=TRUE,sep="\t" , stringsAsFactors = F)
rounds2 <- read.csv("rounds2.csv",stringsAsFactors = F)
#How many unique companies are present in rounds2?
#First convert both companies$permalink and rounds2$company_permalink to lowercase
companies$permalink <- str_to_lower(companies$permalink)
rounds2$company_permalink <- str_to_lower(rounds2$company_permalink)

length(unique(rounds2$company_permalink))
#66368
#How many unique companies are present in companies?
length(unique(companies$permalink))
#66368

#In the companies data frame, which column can be used as the unique key for each company? Write the name of the column.

#Answer is "permalink" because
# 1)any(duplicated(companies$permalink)) is False
# and 2) any(is.na(companies$permalink)) is False
#The "name" is not a unique key because
# 1)any(duplicated(companies$name)) is True

#Are there any companies in the rounds2 file which are not present in companies ? Answer Y/N.

#no
#Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame.
#Name the merged frame master_frame.#How many observations are present in master_frame ?
master_frame <- merge(companies,rounds2,by.x = "permalink", by.y = "company_permalink")
length(master_frame$permalink)
#114949 0bservations after mergign
####master_frame after removing the data where the amount invested id null or na
master_frame <- master_frame[!(master_frame$raised_amount_usd == "" | is.na(master_frame$raised_amount_usd)), ]
length(master_frame$permalink)
####94959 Observations after removing the null values in the investment amout

#######CHECKPOINT2########
#Average funding amount of each type
funding_round_type_group<-group_by(master_frame,funding_round_type)
summarise(funding_round_type_group,mean(raised_amount_usd,na.rm=T))
# 1 angel                                              958694.
# 2 convertible_note                                  1453439.
# 3 debt_financing                                   17043526.
# 4 equity_crowdfunding                                538368.
# 5 grant                                             4300576.
# 6 non_equity_assistance                              411203.
# 7 post_ipo_debt                                   168704572.
# 8 post_ipo_equity                                  82182494.
# 9 private_equity                                   73308593.
# 10 product_crowdfunding                              1363131.
# 11 secondary_market                                 79649630.
# 12 seed                                               719818.
# 13 undisclosed                                      19242370.
# 14 venture                                          11748949. 
# venture is most suited for Sparks avg funding is 11.75 million
#For the chosen investment type,
funding_summary <- summarise(funding_round_type_group,mean(raised_amount_usd,na.rm=T))
write.csv(funding_summary,file="funding_summary.csv")
#######CHECKPOINT3#######
#make a data frame named top9 with the top nine countries
#first filter based on the funding_round_type 

venture_funding_type<-filter(master_frame,funding_round_type=="venture")
venture_country_group <- group_by(venture_funding_type,country_code)
venture_country_summary <- summarise(venture_country_group,total_raised_amount_usd=sum(raised_amount_usd,na.rm=T))
####Arranging in descending order 
venture_country_summary <- arrange(venture_country_summary,desc(total_raised_amount_usd))
######Removing the Rows with NA and blanks
venture_country_summary <- venture_country_summary[!(venture_country_summary$country_code == "" | is.na(venture_country_summary$country_code)), ]
####top9######
top9 <- venture_country_summary[1:9,]
write.csv(venture_country_summary,file="venture_country_summary.csv")

#1 USA 422510842796
#2 CHN 39835418773
#3 GBR 20245627416
#4 IND 14391858718
#5 CAN 9583332317
#6 FRA 7259536732
#7 ISR 6907514579
#8 DEU 6346959822
#9 JPN 3363676611

#top 3 english speaking countries are USA,GBR,IND


#########CHECKPOINT 4 SECTOR ANALYSIS 1
###for sectorwise analysis remove the entries where category_list is NA or null
master_frame <- master_frame[!(master_frame$category_list == "" | is.na(master_frame$category_list)), ]
length(master_frame$permalink)
#Extract the primary_sector of each category_list from category_list
master_frame_primary_sector <- separate(master_frame,category_list,c("primary_sector","other1","other2"),sep="\\|",extra="merge",fill="right")
#Use the mapping file 'mapping.csv' to map each primary sector to one of the eight main sectors 
#(Note that 'Others' is also considered one of the main sectors)
mapping_wide <- read.csv("mapping.csv",stringsAsFactors = F)
#library(tidyr)
mapping <- gather(mapping_wide,main_sector,my_val,2:10)
mapping <- filter(mapping,my_val!=0)
master_frame_mapped <- merge(master_frame_primary_sector,mapping,by.x="primary_sector",by.y="category_list")
#87190 observations
#for full outer left merge 
#master_frame_mapped_full <- merge(master_frame_primary_sector,mapping,by.x="primary_sector",by.y="category_list",all.x=T)
#Removing rows with NA or blank 


########CHECKPOINT 5 SECTOR ANALYSIS 2#########
###Filter the funding_type venture and the range of investments
master_frame_mapped_venture_5to15M <- filter(master_frame_mapped,funding_round_type=="venture",between(raised_amount_usd,5000000,15000000))
write.csv(master_frame_mapped_venture_5to15M,"mastermapped1.csv")
D1 <- filter(master_frame_mapped_venture_5to15M,country_code=="USA")
D1_main_sector_group <- group_by(D1,main_sector)
D1_summary <- summarise(D1_main_sector_group,total_number_of_investments=n(),total_amount_invested=sum(raised_amount_usd))
D1 <- merge(D1,D1_summary,by = "main_sector")

D2 <- filter(master_frame_mapped_venture_5to15M,country_code=="GBR")
D2_main_sector_group <- group_by(D2,main_sector)
D2_summary <- summarise(D2_main_sector_group,total_number_of_investments=n(),total_amount_invested=sum(raised_amount_usd))
D2 <- merge(D2,D2_summary,by = "main_sector")

D3 <- filter(master_frame_mapped_venture_5to15M,country_code=="IND")
D3_main_sector_group <- group_by(D3,main_sector)
D3_summary <- summarise(D3_main_sector_group,total_number_of_investments=n(),total_amount_invested=sum(raised_amount_usd))
D3 <- merge(D3,D3_summary,by = "main_sector")
#write.csv(D1_summary,file="D1_summary.csv",sep=",")
#write.csv(D2_summary,file="D2_summary.csv",sep=",")
#write.csv(D3_summary,file="D3_summary.csv",sep=",")
#####Country 1 
total_investments_USA <- sum(D1$raised_amount_usd)
total_number_of_investments_USA <- length(D1$raised_amount_usd)
#top_sector_no_of_investments_USA
arrange(D1_summary,desc(total_number_of_investments))[1:3, ]
##Others,Cleantech...Semiconductors,Social..Finance..Analytics..Advertising
##filter Others and find the company which received highest investement
D1_others <- filter(D1,main_sector=="Others")
D1_others_group <- group_by(D1_others,permalink)
D1_others_summary <- summarise(D1_others_group,total_investment=sum(raised_amount_usd))
D1_others <- arrange(D1_others_summary,desc(total_investment))
D1_others[1, ]
###filter Cleantech...Semiconductors
D1_Cleantech <- filter(D1,main_sector=="Cleantech...Semiconductors")
D1_Cleantech_group <- group_by(D1_Cleantech,permalink)
D1_Cleantech_summary <- summarise(D1_Cleantech_group,total_investment=sum(raised_amount_usd))
D1_Cleantech <- arrange(D1_Cleantech_summary, desc(total_investment))
D1_Cleantech[1, ]
#######Country2
total_investments_GBR <- sum(D2$raised_amount_usd)
total_number_of_investments_GBR <- length(D2$raised_amount_usd)
#top_sector_no_of_investments_GBR
arrange(D2_summary,desc(total_number_of_investments))[1:3, ]
##Others,Cleantech...Semiconductors,Social..Finance..Analytics..Advertising
##filter Others and find the company which received highest investement
D2_others <- filter(D2,main_sector=="Others")
D2_others_group <- group_by(D2_others,permalink)
D2_others_summary <- summarise(D2_others_group,total_investment=sum(raised_amount_usd))
D2_others <- arrange(D2_others_summary,desc(total_investment))
D2_others[1, ]
###filter Cleantech...Semiconductors
D2_Cleantech <- filter(D2,main_sector=="Cleantech...Semiconductors")
D2_Cleantech_group <- group_by(D2_Cleantech,permalink)
D2_Cleantech_summary <- summarise(D2_Cleantech_group,total_investment=sum(raised_amount_usd))
D2_Cleantech <- arrange(D2_Cleantech_summary,desc(total_investment))
D2_Cleantech[1, ]
#####Country3
total_investments_IND <- sum(D3$raised_amount_usd)
total_number_of_investments_IND <- length(D3$raised_amount_usd)
#top_sector_no_of_investments_CAN
arrange(D3_summary,desc(total_number_of_investments))[1:3, ]
##Others,News..Search.and.Messaging,Entertainment
###filter Others
D3_others <- filter(D3,main_sector=="Others")
D3_others_group <- group_by(D3_others,permalink)
D3_others_summary <- summarise(D3_others_group,total_investment=sum(raised_amount_usd))
D3_others <- arrange(D3_others_summary,desc(total_investment))
D3_others[1, ]
##filter News..Search.and.Messaging  and find the company which received highest investement
D3_News <- filter(D3,main_sector=="News..Search.and.Messaging")
D3_News_group <- group_by(D3_News,permalink)
D3_News_summary <- summarise(D3_News_group,total_investment=sum(raised_amount_usd))
D3_News <- arrange(D3_News_summary,desc(total_investment))
D3_News[1, ]