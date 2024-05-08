#install.packages(c('data.table', 'reshape2'))
#install.packages('stringr')
#install.packages("openxlsx")
library(dplyr)
library(data.table)
library(reshape2)
library(readr)
library(stringr)
library(openxlsx)

rm(list = ls())
cwur <- read_csv('Downloads/archive/cwurData.csv')
education_expenditure_supplementary_data <- read_csv("Downloads/archive/education_expenditure_supplementary_data.csv")
educational_attainment_supplementary_data <- read_csv("Downloads/archive/educational_attainment_supplementary_data.csv")
school_and_country_table <- read_csv("Downloads/archive/school_and_country_table.csv")
shanghaiData <- read_csv("Downloads/archive/shanghaiData.csv")
TIMES_WorldUniversityRankings_2024 <- read_csv("University rankings/Data/TIMES_WorldUniversityRankings_2024.csv") #2024 DATA
QS <- read_csv("University rankings/Data/2024 QS World University Rankings 1.1 (For qs.com).csv") #2024 DATA
unique(cwur$country)

#2015

# cwur DATA --------------------
cwur_adj <- cwur %>% 
  filter(year == 2015) %>% 
  filter(country != "USA") %>% 
  filter(world_rank <= 100)

table(cwur_adj$country)

#SHANGHAI DATA PROCESSING----------
shanghaiData_adj <- shanghaiData %>% 
  left_join(school_and_country_table, by = c('university_name'='school_name')) %>% 
  mutate(country = if_else(is.na(country),"-",country)) %>% 
  filter(country != "United States of America") %>% 
  mutate(world_rank = as.numeric(world_rank)) %>% 
  filter(world_rank <= 100) %>% 
  filter(year == max(year))

table(shanghaiData_adj$country)
#2024
#TIMES RANKING DATA PROCESSING------------------
times_adj <- TIMES_WorldUniversityRankings_2024 %>% 
  filter(location != "United States") %>% 
  mutate(rank = str_replace(rank,"=",""),
         rank = as.numeric(rank)) %>% 
  filter(rank <= 100) %>% 
  arrange(scores_research_rank) %>% 
  slice(1:50)

table(times_adj$location)

#QS RANKING DATA PROCESSING-----------------
QS_adj <- QS %>% 
  filter(Country != "United States") %>% 
  mutate(`2024 RANK` = str_replace(`2024 RANK`,"=",""),
         `2024 RANK` = as.numeric(`2024 RANK`)) %>% 
  filter(`2024 RANK` <= 100) %>% 
  mutate(`Employment Outcomes Rank` = as.numeric(`Employment Outcomes Rank`),
         `Academic Reputation Rank` = as.numeric(`Academic Reputation Rank`)) %>% 
  arrange(`Academic Reputation Rank`, `Employment Outcomes Rank` ) %>% 
  slice(1:50)

table(QS_adj$Country)  


# output-----------------
write.xlsx(times_adj, "University rankings/Output/2024 ex-US times ranking(<=100).xlsx")
write.xlsx(QS_adj,"University rankings/Output/2024 ex-US QS ranking(<=100).xlsx" )
write.xlsx(cwur_adj, "University rankings/Output/2015 ex-US cwur ranking(<=100).xlsx")
write.xlsx(shanghaiData_adj, "University rankings/Output/2015 ex-US shanghai ranking(<=100).xlsx")
