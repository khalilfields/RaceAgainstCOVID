library(readr)
library(ggplot2)
library(tidyr)
Detroit <- read.csv("C:/Users/17044/Downloads/zipcode_detroit_merged.csv")

library(dplyr)
Detroit <- drop_na(Detroit)
attach(Detroit)
Detroit$white_pop_n1000<- (Detroit$white_pop/Detroit$total_pop) * 1000
Detroit$black_pop_n1000<- (black_pop/total_pop) * 1000
Detroit$hispanic_pop_n1000<- (hispanic_pop/total_pop) * 1000
Detroit$poverty_n1000<- (poverty/total_pop) * 1000
Detroit$employed_pop_n1000<- (employed_pop/total_pop) * 1000
Detroit$commuters_by_public_transportation_n1000<- (commuters_by_public_transportation/total_pop) * 1000
Detroit$employed_education_health_social_n1000<- (employed_education_health_social/total_pop) * 1000
Detroit$employed_arts_entertainment_recreation_accommodation_food_n1000<- (employed_arts_entertainment_recreation_accommodation_food/total_pop) * 1000
Detroit$employed_transportation_warehousing_utilities_n1000<- (employed_transportation_warehousing_utilities/total_pop) * 1000



lm.fit<-lm(rate_p100000 ~ black_pop_n1000, data=Detroit)
summary(lm.fit)

library(car)
print(vif(lm.fit))
detach(Detroit)


Boston <- read.csv("C:/Users/17044/Downloads/zipcode_boston_merged.csv")
Boston <- drop_na(Boston)
attach(Boston)
Boston$white_pop_n1000<- (white_pop/total_pop) * 1000
Boston$black_pop_n1000<- (black_pop/total_pop) * 1000
Boston$hispanic_pop_n1000<- (hispanic_pop/total_pop) * 1000
Boston$poverty_n1000<- (poverty/total_pop) * 1000
Boston$employed_pop_n1000<- (employed_pop/total_pop) * 1000
Boston$commuters_by_public_transportation_n1000<- (commuters_by_public_transportation/total_pop) * 1000
Boston$employed_education_health_social_n1000<- (employed_education_health_social/total_pop) * 1000
Boston$employed_arts_entertainment_recreation_accommodation_food_n1000<- (employed_arts_entertainment_recreation_accommodation_food/total_pop) * 1000
Boston$employed_transportation_warehousing_utilities_n1000<- (employed_transportation_warehousing_utilities/total_pop) * 1000


lm.fit2<-lm(rate_p100000~ white_pop_n1000 + poverty_n1000 + percent_income_spent_on_rent, data=Boston)
summary(lm.fit2)
print(vif(lm.fit2))
detach(Boston)

Chicago <- read.csv("C:/Users/17044/Downloads/zipcode_chicago_merged.csv")
Chicago <- drop_na(Chicago)
attach(Chicago)
Chicago$white_pop_n1000<- (white_pop/total_pop) * 1000
Chicago$black_pop_n1000<- (black_pop/total_pop) * 1000
Chicago$hispanic_pop_n1000<- (hispanic_pop/total_pop) * 1000
Chicago$poverty_n1000<- (poverty/total_pop) * 1000
Chicago$employed_pop_n1000<- (employed_pop/total_pop) * 1000
Chicago$commuters_by_public_transportation_n1000<- (commuters_by_public_transportation/total_pop) * 1000
Chicago$employed_education_health_social_n1000<- (employed_education_health_social/total_pop) * 1000
Chicago$employed_arts_entertainment_recreation_accommodation_food_n1000<- (employed_arts_entertainment_recreation_accommodation_food/total_pop) * 1000
Chicago$employed_transportation_warehousing_utilities_n1000<- (employed_transportation_warehousing_utilities/total_pop) * 1000
Chicago_latest <- filter(Chicago, Week.Number == 19)
detach(Chicago)


lm.fit3<- lm(Case.Rate...Cumulative~ hispanic_pop_n1000 + poverty_n1000, data=Chicago_latest)
summary(lm.fit3)
vif(lm.fit3)

LA <- read.csv("C:/Users/17044/Downloads/zipcode_la_merged (3).csv")
LA <- drop_na(LA)
attach(LA)
LA <- filter(LA, num_zc_in_area == 1)
LA$white_pop_n1000<- (white_pop/acs_zipcode_pop) * 1000
LA$black_pop_n1000<- (black_pop/acs_zipcode_pop) * 1000
LA$hispanic_pop_n1000<- (hispanic_pop/acs_zipcode_pop) * 1000
LA$poverty_n1000<- (poverty/acs_zipcode_pop) * 1000
LA$employed_pop_n1000<- (acs_zipcode_employed_pop/acs_zipcode_pop) * 1000
LA$commuters_by_public_transportation_n1000<- (commuters_by_public_transportation/acs_zipcode_pop) * 1000
LA$employed_education_health_social_n1000<- (employed_education_health_social/acs_zipcode_pop) * 1000
LA$employed_arts_entertainment_recreation_accommodation_food_n1000<- (employed_arts_entertainment_recreation_accommodation_food/acs_zipcode_pop) * 1000
LA$employed_transportation_warehousing_utilities_n1000<- (employed_transportation_warehousing_utilities/acs_zipcode_pop) * 1000
detach(LA)


lm.fit4<-lm(adj_case_rate_final~ hispanic_pop_n1000 + poverty_n1000 + employed_transportation_warehousing_utilities_n1000, data=LA)
summary(lm.fit4)
vif(lm.fit4)

NYC <- read.csv("C:/Users/17044/Downloads/Cases_by_ZipCode_cleaned.csv")
NYC <- drop_na(NYC)
attach(NYC)
NYC$white_pop_n1000<- (white_pop/total_pop) * 1000
NYC$black_pop_n1000<- (black_population/total_pop) * 1000
NYC$hispanic_pop_n1000<- (hispanic_pop/total_pop) * 1000
NYC$poverty_n1000<- (poverty/total_pop) * 1000
NYC$employed_pop_n1000<- (employed_pop/total_pop) * 1000
NYC$commuters_by_public_transportation_n1000<- (commuters_by_public_transportation/total_pop) * 1000
NYC$employed_education_health_social_n1000<- (employed_education_health_social/total_pop) * 1000
NYC$employed_arts_entertainment_recreation_accommodation_food_n1000<- (employed_arts_entertainment_recreation_accommodation_food/total_pop) * 1000
NYC$employed_transportation_warehousing_utilities_n1000<- (employed_transportation_warehousing_utilities/total_pop) * 1000

detach(NYC)


lm.fit5<-lm(Positives_per_1000people~ hispanic_pop_n1000 + black_pop_n1000 + poverty_n1000 + commuters_by_public_transportation_n1000 + employed_education_health_social_n1000 + employed_transportation_warehousing_utilities_n1000 + percent_income_spent_on_rent, data=NYC)
summary(lm.fit5)
vif(lm.fit5)



county_merged <- read.csv("C:/Users/17044/Downloads/county_merged.csv")
county_merged_sum <- county_merged %>% select(1:16, 128)
attach(county_merged_sum)
county_merged_sum <- drop_na(county_merged_sum)
county_merged_sum$case_rate_n1000 <- (May_12/total_pop) * 1000

lm.fit_county<-lm(case_rate_n1000~ pc_black + pc_income_below_poverty_line + percent_income_spent_on_rent + pc_pub_trans_commuters + pc_emp_education_health_social + pc_emp_arts_entertainment_recreation_accommodation_food, data=county_merged_sum)
summary(lm.fit_county)
vif(lm.fit_county)


detach(county_merged_sum)
New_York <- filter(county_merged_sum, state == "New York")
attach(New_York)
lm.fit_NY<-lm(case_rate_n1000~ pc_hispanic + pc_income_below_poverty_line + percent_income_spent_on_rent, data=New_York)
summary(lm.fit_NY)
vif(lm.fit_NY)
detach(New_York)

CA <- filter(county_merged_sum, state == "California")
attach(CA)
lm.fit_CA<-lm(case_rate_n1000~ total_pop + pc_hispanic + pc_pub_trans_commuters, data=CA)
summary(lm.fit_CA)
vif(lm.fit_CA)
detach(CA)


IL <- filter(county_merged_sum, state == "Illinois")
attach(IL)
lm.fit_IL<-lm(case_rate_n1000~ total_pop + pc_white + pc_income_below_poverty_line, data=IL)
summary(lm.fit_IL)
vif(lm.fit_IL)
detach(IL)

MA <- filter(county_merged_sum, state == "Massachusetts")
attach(MA)
lm.fit_MA<-lm(case_rate_n1000~ total_pop + pc_pub_trans_commuters + pc_emp_transportation_warehousing_utilities, data=MA)
summary(lm.fit_MA)
vif(lm.fit_MA)
detach(MA)


MI <- filter(county_merged_sum, state == "Michigan")
attach(MI)
lm.fit_MI<-lm(case_rate_n1000~ total_pop + pc_black + pc_emp_transportation_warehousing_utilities + pc_emp_arts_entertainment_recreation_accommodation_food, data=MI)
summary(lm.fit_MI)
vif(lm.fit_MI)
detach(MI)



