#attach file 
data <- read.csv('C:/Users/rc124/Desktop/anes_pilot_2019.csv')
library(ggplot2)
library(waffle)

#list of variables 

#race
table(identity)


#social media 

#1facebook

facebook <- data$socmed_1

#2twitter

twitter <- data$socmed_2

#3insta

instagram <- data$socmed_3

#4 youtube 

youtube <- data$socmed_5

#recode for just only 1

table(facebook) # selected # 2 is not selected
facebook_only <- ifelse(facebook ==2 | twitter == 1 | instagram ==1 | youtube ==1 , 0, 1)
table1 <- table(facebook_only, identity)


table(twitter) # selected # 2 is not selected
twitter_only <- ifelse(twitter ==2 | facebook == 1 | instagram ==1 | youtube ==1 , 0, 1)
table2 <- table(twitter_only, identity)
#not many

table(instagram) # selected # 2 is not selected
instagram_only <- ifelse(instagram ==2 | facebook == 1 | twitter ==1 | youtube ==1 , 0, 1)
table3 <- table(instagram_only, identity)

table(youtube)
youtube_only <- ifelse(youtube ==2 | facebook == 1 | twitter ==1 | instagram ==1 , 0, 1)
table4 <- table(youtube_only, identity)


#prop tables 
prop1 <- prop.table(table1, margin = 1) #facebook
prop2 <- prop.table(table2 , margin =1) #twitter
prop3 <- prop.table(table3, margin = 1) #instagram
prop4 <- prop.table(table4, margin = 1) #youtube

#waffle plots
prop1
parts1 <- c(Not_very_important = 0.2619647 *100, A_little_important = 0.1586902 *100,  Moderately_important = 0.2443325 * 100,  Very_important = 0.1385390* 100,  Extremely_important = 0.1964736* 100)
chart1 <- waffle(parts1, rows = 8, title = 'Facebook Only')
print(chart1)

prop2
parts2 <- c(Not_very_important = 0.3333333 *100, A_little_important = 0.1481481 *100,  Moderately_important = 0.2592593 * 100,  Very_important = 0.1481481* 100,  Extremely_important = 0.1111111* 100)
chart2 <- waffle(parts2, rows = 8, title = 'Twitter Only')
print(chart2)

prop3
parts3 <- c(Not_very_important = 0.3154122 *100, A_little_important = 0.1397849 *100,  Moderately_important = 0.2329749 * 100,  Very_important = 0.1254480* 100,  Extremely_important = 0.1863799* 100)
chart3 <- waffle(parts3, rows = 8, title = 'Instagram Only')
print(chart3)

prop4
parts4 <- c(Not_very_important = 0.3259669 *100, A_little_important = 0.1270718 *100,  Moderately_important = 0.2265193 * 100,  Very_important = 0.1381215* 100,  Extremely_important = 0.1823204* 100)
chart4 <- waffle(parts4, rows = 8, title = 'Youtube Only')
print(chart4)

cor.test(facebook_only, identity)
cor.test(twitter_only, identity)
cor.test(instagram_only, identity)
cor.test(youtube_only, identity)

