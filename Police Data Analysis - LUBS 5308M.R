rm(list=ls(all=TRUE))
library(dplyr)
library(ggplot2)
library(stats)
library(tidyr)
library(stringr)
library(Rcpp)
library(devtools)
devtools::install_github("dkahle/ggmap")
library(ggmap)
register_google(key = "AIzaSyAbPUUx7pg_u_Pk2_wQHa3cPsPMatRkfqI")

WY_street_data <- read.csv('2022-09-west-yorkshire-street.csv',header = TRUE,stringsAsFactors = T)
#The 'context' column was remove`d from the excel sheet before reading the file as all its values are NULLS
WY_outcomes_data <- read.csv('2022-09-west-yorkshire-outcomes.csv',header = TRUE,stringsAsFactors = T)

#The difference between the "street_data" files and the "outcomes_data" files are that the latter 
#mentions the final outcome of the case while the former mentions the last up to date status of the case

#merged the two data frames
solved_WY <- merge(WY_outcomes_data,WY_street_data,by = 'Crime.ID',stringAsFactors = T)

#viewing a summary of the data
summary(solved_WY)

#removing the duplicate columns and columns with 1 unique value
solved_WY <- select(solved_WY,-c(Longitude.y,Latitude.y,Location.y,LSOA.code.y,LSOA.name.y))
solved_WY <- rename(solved_WY,Longitude=Longitude.x,Latitude=Latitude.x,Location=Location.x,
                    LSOA.code=LSOA.code.x,LSOA.name=LSOA.name.x)

#Looking at the unique LSOA names 
LSOA_areas <- gsub(" .*$","",solved_WY$LSOA.name)
LSOA_areas

#Adding the Area column to the data frame
solved_WY$Area <- LSOA_areas

#Grouping crime count by area
crimes_by_area <- solved_WY %>% group_by(Area) %>% summarise('Crime_Count'=n())
crimes_by_area

#Viewing the columns of the data frame
columns_solved_WY <- colnames(solved_WY)

#Viewing the unique crime types
crime_types_WY <- unique(solved_WY$Crime.type)
crime_types_WY
num_crime_types_WY <- length(crime_types_WY) #13 different crime types

#Grouping crime count by type
crimes_by_type <- solved_WY %>% group_by(Crime.type) %>% summarise('Crime_Count'=n())
crimes_by_type

LSOA_areas_unique = unique(LSOA_areas)
LSOA_areas_unique

#Splitting the dataframe to crime data for each area
leeds_crimes <- solved_WY %>% filter(Area == 'Leeds')
kirklees_crimes <- solved_WY %>% filter(Area == 'Kirklees')
bradford_crimes <- solved_WY %>% filter(Area == 'Bradford')
wakefield_crimes <- solved_WY %>% filter(Area == 'Wakefield')
calderdale_crimes <- solved_WY %>% filter(Area == 'Calderdale')
unknown_area_crimes <- solved_WY %>% filter(Area == 'Unknown')
harrogate_crimes <- solved_WY %>% filter(Area == 'Harrogate')
selby_crimes <- solved_WY %>% filter(Area == 'Selby')
barnsely_crimes <- solved_WY %>% filter(Area == 'Barnsley')


leeds_crimes_by_type <- leeds_crimes %>% group_by(Crime.type) %>% 
  summarise('Crime_Count'=n())
sum_count_leeds <- sum(leeds_crimes_by_type$Crime_Count)
leeds_crimes_by_type$Proportion <- round(leeds_crimes_by_type$Crime_Count/sum_count_leeds,2)

kirklees_crimes_by_type <- kirklees_crimes %>% group_by(Crime.type) %>% 
  summarise('Crime Count'=n())
sum_count_kirklees <- sum(kirklees_crimes_by_type$`Crime Count`)
kirklees_crimes_by_type$Proportion <- round(kirklees_crimes_by_type$`Crime Count`/sum_count_kirklees,2)

bradford_crimes_by_type <- bradford_crimes %>% group_by(Crime.type) %>% 
  summarise('Crime Count'=n())
sum_count_bradford <- sum(bradford_crimes_by_type$`Crime Count`)
bradford_crimes_by_type$Proportion <- round(bradford_crimes_by_type$`Crime Count`/sum_count_bradford,2)

wakefield_crimes_by_type <- wakefield_crimes %>% group_by(Crime.type) %>% 
  summarise('Crime Count'=n())
sum_count_wakefield <- sum(wakefield_crimes_by_type$`Crime Count`)
wakefield_crimes_by_type$Proportion <- round(wakefield_crimes_by_type$`Crime Count`/sum_count_wakefield,2)

calderdale_crimes_by_type <- calderdale_crimes %>% group_by(Crime.type) %>% 
  summarise('Crime Count'=n())
sum_count_calderdale <- sum(calderdale_crimes_by_type$`Crime Count`)
calderdale_crimes_by_type$Proportion <- round(calderdale_crimes_by_type$`Crime Count`/sum_count_calderdale,2)


unknown_area_crimes_by_type <- unknown_area_crimes %>% group_by(Crime.type) %>% 
  summarise('Crime Count'=n())
sum_count_unknown <- sum(unknown_area_crimes_by_type$`Crime Count`)
unknown_area_crimes_by_type$Proportion <- round(unknown_area_crimes_by_type$`Crime Count`/sum_count_unknown,2)


harrogate_crimes_by_type <- harrogate_crimes %>% group_by(Crime.type) %>% 
  summarise('Crime Count'=n())
selby_crimes_by_type <- selby_crimes %>% group_by(Crime.type) %>% 
  summarise('Crime Count'=n())
barnsely_crimes_by_type <- barnsely_crimes %>% group_by(Crime.type) %>% 
  summarise('Crime Count'=n())

crimes_by_type_area <- solved_WY %>% group_by(Crime.type,Area) %>% 
  summarise('Crime Count' = n())
print(crimes_by_type_area,n=100)

crime_types_WY

bicycle_thefts <- crimes_by_type_area %>% filter(Crime.type == 'Bicycle theft')
sum_bicycle_thefts <- sum(bicycle_thefts$`Crime Count`)
bicycle_thefts$Proportion <- round(bicycle_thefts$`Crime Count`/sum_bicycle_thefts,2)

violence_and_sexual <- crimes_by_type_area %>% filter(Crime.type == 'Violence and sexual offences')
sum_violence_and_sexual <- sum(violence_and_sexual$`Crime Count`)
violence_and_sexual$Proportion <- round(violence_and_sexual$`Crime Count`/sum_violence_and_sexual,2)

criminal_arson <- crimes_by_type_area %>% filter(Crime.type == 'Criminal damage and arson')
sum_criminal_arson <- sum(criminal_arson$`Crime Count`)
criminal_arson$Proportion <- round(criminal_arson$`Crime Count`/sum_criminal_arson,2)

public_order <- crimes_by_type_area %>% filter(Crime.type == 'Public order')
sum_public_order <- sum(public_order$`Crime Count`)
public_order$Proportion <- round(public_order$`Crime Count`/sum_public_order,2)

shoplifting <- crimes_by_type_area %>% filter(Crime.type == 'Shoplifting')
sum_shoplifting <- sum(shoplifting$`Crime Count`)
shoplifting$Proportion <- round(shoplifting$`Crime Count`/sum_shoplifting,2)

vehicle_crime <- crimes_by_type_area %>% filter(Crime.type == 'Vehicle crime')
sum_vehicle_crime <- sum(vehicle_crime$`Crime Count`)
vehicle_crime$Proportion <- round(vehicle_crime$`Crime Count`/sum_vehicle_crime,2)

weapons <- crimes_by_type_area %>% filter(Crime.type == 'Possession of weapons')
sum_weapons <- sum(weapons$`Crime Count`)
weapons$Proportion <- round(weapons$`Crime Count`/sum_weapons,2)

theft_from_person <- crimes_by_type_area %>% filter(Crime.type == 'Theft from the person')
sum_theft_from_person <- sum(theft_from_person$`Crime Count`)
theft_from_person$Proportion <- round(theft_from_person$`Crime Count`/sum_theft_from_person,2)

other_theft <- crimes_by_type_area %>% filter(Crime.type == 'Other theft')
sum_other_theft <- sum(other_theft$`Crime Count`)
other_theft$Proportion <- round(other_theft$`Crime Count`/sum_other_theft,2)

burglary <- crimes_by_type_area %>% filter(Crime.type == 'Burglary')
sum_burglary <- sum(burglary$`Crime Count`)
burglary$Proportion <- round(burglary$`Crime Count`/sum_burglary,2)

drugs <- crimes_by_type_area %>% filter(Crime.type == 'Drugs')
sum_drugs <- sum(drugs$`Crime Count`)
drugs$Proportion <- round(drugs$`Crime Count`/sum_drugs,2)

robbery <- crimes_by_type_area %>% filter(Crime.type == 'Robbery')
sum_robbery <- sum(robbery$`Crime Count`)
robbery$Proportion <- round(robbery$`Crime Count`/sum_robbery,2)

other_crime <- crimes_by_type_area %>% filter(Crime.type == 'Other crime')
sum_other_crime <- sum(other_crime$`Crime Count`)
other_crime$Proportion <- round(other_crime$`Crime Count`/sum_other_crime,2)


#chart for which areas have most crimes
#chart for which crimes are most popular overall
#chart for which crimes are most popular in each area

crimes_by_area <- subset(crimes_by_area, !(Area %in% c('Barnsley','Harrogate','Selby')))
crimes_by_area

sum_crimes <- sum(crimes_by_area$Crime_Count)
crimes_percent_area <- round(crimes_by_area$Crime_Count/sum_crimes,3)*100
crimes_percent_area

crimes_by_area$Proportion = crimes_percent_area
crimes_by_area

crimes_by_type$Proportion <- round(crimes_by_type$Crime_Count/sum(crimes_by_type$Crime_Count),2)

pie_chart_label <- paste0(crimes_by_area$Area,' = ',crimes_by_area$Proportion,'%')

pie_chart_label
crimes_by_area_pie_chart <- pie(crimes_by_area$Crime_Count,labels = pie_chart_label,radius = 1,
                                col = heat.colors(6),main = 'Overall Crime Proportion by Area in West Yorkshire')

crimes_by_type_bar <- ggplot(data = crimes_by_type) + geom_col(aes(x=Crime.type,y=Proportion),fill = "blue") + 
  theme(axis.text.x = element_text(size=10,angle = 90,vjust = 0.5)) + labs(x = 'Crime Type',y='Crime Count',
                                                                          title='Crime Proportion by Type - West Yorkshire') + ylim(0,0.4)
crimes_by_type_bar

crimes_by_type_leeds_bar <-  ggplot(data = leeds_crimes_by_type) + geom_col(aes(x=Crime.type,y=Proportion),fill = "red2") + 
  theme(axis.text.x = element_text(size=10,angle = 90,vjust = 0.5)) + labs(x = 'Crime Type',y='Crime Proportion',
                                                                          title='Crime Proportion  by Type - Leeds')


crimes_by_type_kirklees_bar <-  ggplot(data = kirklees_crimes_by_type) + geom_col(aes(x=Crime.type,y=Proportion),fill = "red4") + 
  theme(axis.text.x = element_text(size=10,angle = 90,vjust = 0.5)) + labs(x = 'Crime Type',y='Crime Proportion',
                                                                          title='Crime Proportion  by Type - Kirklees')

crimes_by_type_bradford_bar <-  ggplot(data = bradford_crimes_by_type) + geom_col(aes(x=Crime.type,y=Proportion),fill = "orange2") + 
  theme(axis.text.x = element_text(size=10,angle = 90,vjust = 0.5)) + labs(x = 'Crime Type',y='Crime Proportion',
                                                                          title='Crime Proportion  by Type - Bradford')

crimes_by_type_wakefield_bar <-  ggplot(data = wakefield_crimes_by_type) + geom_col(aes(x=Crime.type,y=Proportion),fill = "orange4") + 
  theme(axis.text.x = element_text(size=10,angle = 90,vjust = 0.5)) + labs(x = 'Crime Type',y='Crime Proportion',
                                                                          title='Crime Proportion  by Type - Wakefield')

crimes_by_type_calderdale_bar <-  ggplot(data = calderdale_crimes_by_type) + geom_col(aes(x=Crime.type,y=Proportion),fill = "yellow3") + 
  theme(axis.text.x = element_text(size=10,angle = 90,vjust = 0.5)) + labs(x = 'Crime Type',y='Crime Proportion',
                                                                          title='Crime Proportion by Type - Calderdale')


crimes_by_type_unknown_bar <-  ggplot(data = unknown_area_crimes_by_type) + geom_col(aes(x=Crime.type,y=Proportion),fill = "yellow4") + 
  theme(axis.text.x = element_text(size=10,angle = 90,vjust = 0.5)) + labs(x = 'Crime Type',y='Crime Proportion',
                                                                          title='Crime Proportion  by Type - Unknown Area')

columns_solved_WY
outcome_types_unique <- unique(solved_WY$Outcome.type)
#outcomes by area
outcomes_by_area <- solved_WY %>% group_by(Area,Outcome.type) %>% summarise('Outcome Count'=n())
print(outcomes_by_area,n=70)


bradford_outcomes <- outcomes_by_area %>% filter(Area == 'Bradford',`Outcome Count` > 20)
sum_outcomes_bradford <- sum(bradford_outcomes$`Outcome Count`)
bradford_outcomes$Proportion <- round(bradford_outcomes$`Outcome Count`/sum_outcomes_bradford,2)

calderdale_outcomes <- outcomes_by_area %>% filter(Area == 'Calderdale',`Outcome Count` > 13)
sum_outcomes_calderdale <- sum(calderdale_outcomes$`Outcome Count`)
calderdale_outcomes$Proportion <- round(calderdale_outcomes$`Outcome Count`/sum_outcomes_calderdale,2)

kirklees_outcomes <- outcomes_by_area %>% filter(Area == 'Kirklees',`Outcome Count` > 20)
sum_outcomes_kirklees <- sum(kirklees_outcomes$`Outcome Count`)
kirklees_outcomes$Proportion <- round(kirklees_outcomes$`Outcome Count`/sum_outcomes_kirklees,2)

leeds_outcomes <- outcomes_by_area %>% filter(Area == 'Leeds',`Outcome Count` > 37)
sum_outcomes_leeds <- sum(leeds_outcomes$`Outcome Count`)
leeds_outcomes$Proportion <- round(leeds_outcomes$`Outcome Count`/sum_outcomes_leeds,2)


unknown_outcomes <- outcomes_by_area %>% filter(Area == 'Unknown',`Outcome Count` > 9)
sum_outcomes_unknown <- sum(unknown_outcomes$`Outcome Count`)
unknown_outcomes$Proportion <- round(unknown_outcomes$`Outcome Count`/sum_outcomes_unknown,2)


wakefield_outcomes <- outcomes_by_area %>% filter(Area == 'Wakefield',`Outcome Count` > 20)
sum_outcomes_wakefield <- sum(unknown_outcomes$`Outcome Count`)
wakefield_outcomes$Proportion <- round(wakefield_outcomes$`Outcome Count`/sum_outcomes_wakefield,2)


bradford_outcomes$Outcome.type <-  c('Inv. Complete / no suspect ID','Local Res.','Suspect Changed','Prosec. Failed')
calderdale_outcomes$Outcome.type <-  c('Inv. Complete / no suspect ID','Suspect Changed','Prosec. Failed')
kirklees_outcomes$Outcome.type <-  c('Inv. Complete / no suspect ID','Local Res','Suspect Changed','Prosec. Failed')
leeds_outcomes$Outcome.type <-  c('Inv. Complete / no suspect ID','Local Res','Suspect Changed','Prosec. Failed')
unknown_outcomes$Outcome.type <- c('Inv. Complete / no suspect ID','Suspect Changed','Prosec. Failed')
wakefield_outcomes$Outcome.type <- c('Inv. Complete / no suspect ID','Suspect Changed','Prosec. Failed')



bradford_outcomes_bar <- ggplot(data=bradford_outcomes) + geom_col(aes(x=Outcome.type,y=Proportion),width = 0.3,fill = 'red4') +
  theme(axis.text.x = element_text(size=10)) + labs(y='Outcome Proportion',
                                                                      title = 'Outcome Proportion  by Type - Bradford')

calderdale_outcomes_bar <- ggplot(data=calderdale_outcomes) + geom_col(aes(x=Outcome.type,y=Proportion),width = 0.3,fill = 'red4') +
  theme(axis.text.x = element_text(size=10)) + labs(y='Outcome Proportion',
                                                                      title = 'Outcome Proportion  by Type - Calderdale')

kirklees_outcomes_bar <- ggplot(data=kirklees_outcomes) + geom_col(aes(x=Outcome.type,y=Proportion),width = 0.3,fill = 'red4') +
  theme(axis.text.x = element_text(size=10)) + labs(x='Outcome Type',y='Outcome Proportion',
                                                                      title = 'Outcome Proportion  by Type - Kirklees')


leeds_outcomes_bar <- ggplot(data=leeds_outcomes) + geom_col(aes(x=Outcome.type,y=Proportion),width = 0.3,fill = 'red4') +
  theme(axis.text.x = element_text(size=10)) + labs(x='Outcome Type',y='Outcome Proportion',
                                                                      title = 'Outcome Proportion  by Type - Leeds')


unknowns_outcomes_bar <- ggplot(data=unknown_outcomes) + geom_col(aes(x=Outcome.type,y=Proportion),width = 0.3,fill = 'red4') +
  theme(axis.text.x = element_text(size=10)) + labs(x='Outcome Type',y='Outcome Proportion',
                                                                      title = 'Outcome Proportion  by Type - Unknown')


wakefield_outcomes_bar <- ggplot(data=wakefield_outcomes) + geom_col(aes(x=Outcome.type,y=Proportion),width = 0.3,fill = 'red4') +
  theme(axis.text.x = element_text(size=10)) + labs(x='Outcome Type',y='Outcome Proportion',
                                                                      title = 'Outcome Proportion  by Type - Wakefield')


crimes_by_type_bradford_bar
crimes_by_type_calderdale_bar
crimes_by_type_kirklees_bar
crimes_by_type_leeds_bar
crimes_by_type_unknown_bar
crimes_by_type_wakefield_bar

bradford_outcomes_bar
calderdale_outcomes_bar
kirklees_outcomes_bar
leeds_outcomes_bar
unknowns_outcomes_bar
wakefield_outcomes_bar

crime_types_WY
vehicle_crime <- vehicle_crime %>% filter(Proportion>0)
public_order <- public_order %>% filter(Proportion>0)
violence_and_sexual_bar <- ggplot(data=violence_and_sexual) + geom_col(aes(x=Area,y=Proportion),fill = 'red2') +
  labs(title = 'Violence and Sexual Assault Proportions per Area') + ylim(0,0.7)
criminal_arson_bar <- ggplot(data=criminal_arson) + geom_col(aes(x=Area,y=Proportion),fill = 'red3') +
  labs(title = 'Criminal Arson Proportions per Area') + ylim(0,0.7)
public_order_bar <- ggplot(data=public_order) + geom_col(aes(x=Area,y=Proportion), fill = 'red4') +
  labs(title = 'Public Order Crimes Proportions per Area')+ ylim(0,0.7)
shoplifting_bar <- ggplot(data=shoplifting) + geom_col(aes(x=Area,y=Proportion), fill = 'yellow3') +
  labs(title = 'Shoplifting Crimes Proportions per Area')+ ylim(0,0.7)
vehicle_crime_bar <- ggplot(data=vehicle_crime) + geom_col(aes(x=Area,y=Proportion),fill = 'yellow4') +
  labs(title = 'Vehicle Crimes Proportions per Area')+ ylim(0,0.7)
weapons_bar <- ggplot(data=weapons) + geom_col(aes(x=Area,y=Proportion),fill = 'blue1') +
  labs(title = 'Possession of Weapons Crimes Proportions per Area')+ ylim(0,0.7)
theft_from_person_bar <- ggplot(data=theft_from_person) + geom_col(aes(x=Area,y=Proportion),fill='blue2') +
  labs(title = 'Theft from Person Crimes Proportions per Area')+ ylim(0,0.7)
other_theft_bar <- ggplot(data=other_theft) + geom_col(aes(x=Area,y=Proportion),fill='blue3') + 
  labs(title = 'Other Theft Proportions per Area')+ ylim(0,0.7)
bicycle_thefts_bar <- ggplot(data=bicycle_thefts) + geom_col(aes(x=Area,y=Proportion),fill='blue4') + 
  labs(title = 'Bicycle Thefts Proportions per Area')+ ylim(0,0.7)
burglary_bar <- ggplot(data=burglary) + geom_col(aes(x=Area,y=Proportion),fill='orange2') + 
  labs(title = 'Burglaries Proportions per Area')+ ylim(0,0.7)
drugs_bar <- ggplot(data=drugs) + geom_col(aes(x=Area,y=Proportion),fill='orange3') + 
  labs(title = 'Drugs Crimes Proportions per Area')+ ylim(0,0.7)
robbery_bar <- ggplot(data=robbery) + geom_col(aes(x=Area,y=Proportion),fill='orange1') + 
  labs(title = 'Robberies Proportions per Area')+ ylim(0,0.7)
other_crime_bar <- ggplot(data=other_crime) + geom_col(aes(x=Area,y=Proportion),fill='orange4') + 
  labs(title = 'Other Crimes Proportions per Area')+ ylim(0,0.7)

violence_and_sexual_bar
criminal_arson_bar
public_order_bar
shoplifting_bar
vehicle_crime_bar
weapons_bar
theft_from_person_bar
other_theft_bar
bicycle_thefts_bar
burglary_bar
drugs_bar
robbery_bar
other_crime_bar


DUR_street_data <- read.csv('2022-09-durham-street.csv',header = TRUE,stringsAsFactors = T)
DUR_outcomes_data <- read.csv('2022-09-durham-outcomes.csv',header = TRUE,stringsAsFactors = T)
solved_DUR <- merge(DUR_street_data,DUR_outcomes_data,by = 'Crime.ID',stringAsFactors = T)

solved_DUR <- select(solved_DUR,-c(Month.y,Reported.by.y,Falls.within.y,Longitude.y,
                                   Latitude.y,Location.y,LSOA.code.y,LSOA.name.y))
solved_DUR <- rename(solved_DUR,Month=Month.x,Reported.by=Reported.by.x,Falls.within=Falls.within.x,
                     Longitude=Longitude.x,Latitude=Latitude.x,Location=Location.x,LSOA.code=LSOA.code.x,
                     LSOA.name=LSOA.name.x)


solved_DUR <- select(solved_DUR,-c(Month,Reported.by,Falls.within,Context))

unique(solved_DUR$LSOA.name)
colnames(solved_DUR)

LSOA_areas_DUR <- sub("0.*","",solved_DUR$LSOA.name)
LSOA_areas_DUR <- trimws(LSOA_areas_DUR)
unique(LSOA_areas_DUR)

solved_DUR$Area <- LSOA_areas_DUR

crimes_by_area_DUR <- solved_DUR %>% group_by(Area) %>% summarise('Crime_Count'=n())
crimes_by_area_DUR <- crimes_by_area_DUR %>% filter(Crime_Count>400)
sum_crimes_DUR <- sum(crimes_by_area_DUR$Crime_Count)
crimes_by_area_DUR$Proportion <- round(crimes_by_area_DUR$Crime_Count/sum_crimes_DUR,2)

durham_crimes <- solved_DUR %>% filter(Area=='County Durham')
darlington_crimes <- solved_DUR %>% filter(Area=='Darlington')

durham_crimes_by_type <- durham_crimes %>% group_by(Crime.type) %>% summarise('Crime_Count'=n())
durham_crimes_by_type$Proportion <- round(durham_crimes_by_type$Crime_Count/(sum(durham_crimes_by_type$Crime_Count)),2)
durham_crimes_by_type <- filter(durham_crimes_by_type,Crime_Count>9)

darlington_crimes_by_type <- darlington_crimes %>% group_by(Crime.type) %>% summarise('Crime_Count'=n())
darlington_crimes_by_type$Proportion <- round(darlington_crimes_by_type$Crime_Count/(sum(darlington_crimes_by_type$Crime_Count)),2)
darlington_crimes_by_type <- filter(darlington_crimes_by_type,Crime_Count > 5)

colnames(solved_DUR)
crimes_by_type_area_DUR <- solved_DUR %>% group_by(Area,Crime.type) %>% summarise('Crime_Count'=n())
print(crimes_by_type_area_DUR,n=50)

unique(crimes_by_type_area_DUR$Crime.type)

bicycle_theft_DUR <- crimes_by_type_area_DUR %>% filter(Crime.type=='Bicycle theft')
sum_bicycle_theft <- sum(bicycle_theft_DUR$Crime_Count)
bicycle_theft_DUR$Proportion <- round(bicycle_theft_DUR$Crime_Count/sum_bicycle_theft,2)

burglary_theft_DUR <- crimes_by_type_area_DUR %>% filter(Crime.type=='Burglary')
sum_burglary_theft <- sum(burglary_theft_DUR$Crime_Count)
burglary_theft_DUR$Proportion <- round(burglary_theft_DUR$Crime_Count/sum_burglary_theft,2)

criminal_arson_DUR <- crimes_by_type_area_DUR %>% filter(Crime.type=='Criminal damage and arson',Crime_Count>1)
sum_criminal_arson <- sum(criminal_arson_DUR$Crime_Count)
criminal_arson_DUR$Proportion <- round(criminal_arson_DUR$Crime_Count/sum_criminal_arson,2)

drugs_DUR <- crimes_by_type_area_DUR %>% filter(Crime.type=='Drugs',Crime_Count>1)
sum_drugs_DUR <- sum(drugs_DUR$Crime_Count)
drugs_DUR$Proportion <- round(drugs_DUR$Crime_Count/sum_drugs_DUR,2)

other_crime_DUR <- crimes_by_type_area_DUR %>% filter(Crime.type=='Other crime')
sum_other_crime_DUR <- sum(other_crime_DUR$Crime_Count)
other_crime_DUR$Proportion <- round(other_crime_DUR$Crime_Count/sum_other_crime_DUR,2)

other_theft_DUR <- crimes_by_type_area_DUR %>% filter(Crime.type=='Other theft',Crime_Count>1)
sum_other_theft_DUR <- sum(other_crime_DUR$Crime_Count)
other_theft_DUR$Proportion <- round(other_theft_DUR$Crime_Count/sum_other_theft_DUR,2)

weapons_DUR <- crimes_by_type_area_DUR %>% filter(Crime.type=='Possession of weapons')
sum_weapons_DUR <- sum(weapons_DUR$Crime_Count)
weapons_DUR$Proportion <- round(weapons_DUR$Crime_Count/sum_weapons_DUR,2)

public_order_DUR <- crimes_by_type_area_DUR %>% filter(Crime.type=='Public order')
sum_public_order_DUR <- sum(public_order_DUR$Crime_Count)
public_order_DUR$Proportion <- round(public_order_DUR$Crime_Count/sum_public_order_DUR,2)

robbery_DUR <- crimes_by_type_area_DUR %>% filter(Crime.type=='Robbery')
sum_robbery_DUR <- sum(robbery_DUR$Crime_Count)
robbery_DUR$Proportion <- round(robbery_DUR$Crime_Count/sum_robbery_DUR,2)

shoplifting_DUR <- crimes_by_type_area_DUR %>% filter(Crime.type=='Shoplifting')
sum_shoplifting_DUR <- sum(shoplifting_DUR$Crime_Count)
shoplifting_DUR$Proportion <- round(shoplifting_DUR$Crime_Count/sum_shoplifting_DUR,2)

theft_from_person_DUR <- crimes_by_type_area_DUR %>% filter(Crime.type=='Theft from the person')
sum_theft_from_person_DUR <- sum(theft_from_person_DUR$Crime_Count)
theft_from_person_DUR$Proportion <- round(theft_from_person_DUR$Crime_Count/sum_theft_from_person_DUR,2)

vehicle_crime_DUR <- crimes_by_type_area_DUR %>% filter(Crime.type=='Vehicle crime')
sum_vehicle_crime_DUR <- sum(vehicle_crime_DUR$Crime_Count)
vehicle_crime_DUR$Proportion <- round(vehicle_crime_DUR$Crime_Count/sum_vehicle_crime_DUR,2)

violence_DUR <- crimes_by_type_area_DUR %>% filter(Crime.type=='Violence and sexual offences',Crime_Count>3)
sum_violenece_DUR <- sum(violence_DUR$Crime_Count)
violence_DUR$Proportion <- round(violence_DUR$Crime_Count/sum_violenece_DUR,2)


DUR_pie_chart_label <- paste0(crimes_by_area_DUR$Area,' = ',crimes_by_area_DUR$Proportion*100,'%')
crimes_by_area_DUR_piechart <- pie(crimes_by_area_DUR$Crime_Count,labels = DUR_pie_chart_label,radius = 1,
                                   col=heat.colors(2),main = 'Overall Crime Proportion by Area in Durham',
                                   init.angle = 60)


crimes_by_type_bar_DUR <- ggplot(data=crimes_by_type_area_DUR) + geom_col(aes(x=Crime.type,y=Proportion/100),fill='blue') +
 theme(axis.text.x = element_text(size=10,angle = 90,vjust = 0.5)) + labs(x="Crime Type",y='Crime Proportion',
                                                                        title = 'Crime Proportion by Type - Durham')


crimes_by_type_durham_bar <- ggplot(data=durham_crimes_by_type) + geom_col(aes(x=Crime.type,y=Proportion),fill='red2') +
  theme(axis.text.x = element_text(size=10,angle = 90,vjust = 0.5)) + labs(x = 'Crime Type',y='Crime Proportion',
                                                                           title='Crime Proportion  by Type - County Durham')



crimes_by_type_darlington_bar <- ggplot(data=darlington_crimes_by_type) + geom_col(aes(x=Crime.type,y=Proportion),fill='red4') +
  theme(axis.text.x = element_text(size=10,angle = 90,vjust = 0.5)) + labs(x = 'Crime Type',y='Crime Proportion',
                                                                           title='Crime Proportion  by Type - Darlington')  

colnames(solved_DUR)
outcomes_by_area_DUR <- solved_DUR %>% group_by(Area,Outcome.type) %>% summarise('Outcome_Count' = n()) %>% 
  filter(Outcome_Count > 5)
print(outcomes_by_area_DUR,n=40)
unique(outcomes_by_area_DUR$Area)

durham_outcomes <- outcomes_by_area_DUR %>% filter(Area == 'County Durham',Outcome_Count>4)
darlington_outcomes <- outcomes_by_area_DUR %>% filter(Area == 'Darlington',Outcome_Count>4)

sum_durham_outcomes <- sum(durham_outcomes$Outcome_Count)
sum_darlington_outcomes <- sum(darlington_outcomes$Outcome_Count)
durham_outcomes$Proportion <- round(durham_outcomes$Outcome_Count/sum_durham_outcomes,2)
darlington_outcomes$Proportion <- round(darlington_outcomes$Outcome_Count/sum_darlington_outcomes,2)

durham_outcomes$Outcome.type <- c('Action to be taken by other org.','Formal act. not in publ. int',
                                  'Further action not in publ. int','Further inv. not in publ. int',
                                  'Inv. complete; no suspect','Local res.',
                                  'Offender given caution','Suspect charged','Umable to prosecute')
durham_outcomes_bar <- ggplot(durham_outcomes) + geom_col(aes(x=Outcome.type,y=Proportion),width = 0.4,fill='red4') +
  theme(axis.text.x = element_text(size=10,angle = 90,vjust = 0.5)) + labs(y='Outcome Proportion',
                                                                           title = 'Outcome Proportion  by Type - County Durham')


darlington_outcomes$Outcome.type <- c('Action to be taken by other org.','Further action not in publ. int',
                                      'Further inv. not in publ. int',
                                      'Inv. complete; no suspect','Local res.',
                                      'Offender given caution','Suspect charged','Umable to prosecute')
darlington_outcomes_bar <- ggplot(darlington_outcomes) + geom_col(aes(x=Outcome.type,y=Proportion),width = 0.4,fill='red3') +
  theme(axis.text.x = element_text(size=10,angle = 90,vjust = 0.5)) + labs(y='Outcome Proportion',
                                                                           title = 'Outcome Proportion  by Type - Darlington')


violence_and_sexual_bar_DUR <- ggplot(data=violence_DUR) + geom_col(aes(x=Area,y=Proportion),fill = 'red2',width = 0.3) +
  labs(title = 'Violence and Sexual Assault Proportions per Area - Durham')
criminal_arson_bar_DUR <- ggplot(data=criminal_arson_DUR) + geom_col(aes(x=Area,y=Proportion),fill = 'red3',width = 0.3) +
  labs(title = 'Criminal Arson Proportions per Area - Durham')
public_order_bar_DUR <- ggplot(data=public_order_DUR) + geom_col(aes(x=Area,y=Proportion), fill = 'red4',width = 0.3) +
  labs(title = 'Public Order Crimes Proportions per Area - Durham')
shoplifting_bar_DUR <- ggplot(data=shoplifting_DUR) + geom_col(aes(x=Area,y=Proportion), fill = 'yellow3',width = 0.3) +
  labs(title = 'Shoplifting Crimes Proportions per Area - Durham')
vehicle_crime_bar_DUR <- ggplot(data=vehicle_crime_DUR) + geom_col(aes(x=Area,y=Proportion),fill = 'yellow4',width = 0.3) +
  labs(title = 'Vehicle Crimes Proportions per Area - Durham')
weapons_bar_DUR <- ggplot(data=weapons_DUR) + geom_col(aes(x=Area,y=Proportion),fill = 'blue1',width = 0.3) +
  labs(title = 'Possession of Weapons Crimes Proportions per Area - Durham')
theft_from_person_bar_DUR <- ggplot(data=theft_from_person_DUR) + geom_col(aes(x=Area,y=Proportion),fill='blue2',width = 0.3) +
  labs(title = 'Theft from Person Crimes Proportions per Area - Durham')
other_theft_bar_DUR <- ggplot(data=other_theft_DUR) + geom_col(aes(x=Area,y=Proportion),fill='blue3',width = 0.3) + 
  labs(title = 'Other Theft Proportions per Area - Durham')
bicycle_thefts_bar_DUR <- ggplot(data=bicycle_theft_DUR) + geom_col(aes(x=Area,y=Proportion),fill='blue4',width = 0.3) + 
  labs(title = 'Bicycle Thefts Proportions per Area - Durham')
burglary_bar_DUR <- ggplot(data=burglary_theft_DUR) + geom_col(aes(x=Area,y=Proportion),fill='orange2',width = 0.3) + 
  labs(title = 'Burglaries Proportions per Area - Durham')
drugs_bar_DUR <- ggplot(data=drugs_DUR) + geom_col(aes(x=Area,y=Proportion),fill='orange3',width = 0.3) + 
  labs(title = 'Drugs Crimes Proportions per Area - Durham')
robbery_bar_DUR <- ggplot(data=robbery_DUR) + geom_col(aes(x=Area,y=Proportion),fill='orange1',width = 0.3) + 
  labs(title = 'Robberies Proportions per Area - Durham')
other_crime_bar_DUR <- ggplot(data=other_crime_DUR) + geom_col(aes(x=Area,y=Proportion),fill='orange4',width = 0.3) + 
  labs(title = 'Other Crimes Proportions per Area - Durham')


crimes_by_type_darlington_bar
crimes_by_type_durham_bar
violence_and_sexual_bar_DUR
criminal_arson_bar_DUR
public_order_bar_DUR
shoplifting_bar_DUR
vehicle_crime_bar_DUR
weapons_bar_DUR
theft_from_person_bar_DUR
other_crime_bar_DUR
bicycle_thefts_bar_DUR
other_theft_bar_DUR
burglary_bar_DUR
drugs_bar_DUR
robbery_bar_DUR
durham_outcomes_bar
darlington_outcomes_bar



colnames(solved_DUR)
colnames(solved_WY)
crime_types_WY

leeds <- get_map(location=c(-1.6758144,53.8059209),zoom = 10)


solved_WY$Latitude <- as.numeric(as.character(solved_WY$Latitude))
solved_WY$Longitude <- as.numeric(as.character(solved_WY$Longitude))
map_leeds_all_crimes <- ggmap(leeds) + geom_point(data=solved_WY %>% filter(Crime.type=='Bicycle theft'),aes(x=Longitude,y=Latitude),alpha = 0.5,color="red",size=1) +
  geom_point(data=solved_WY %>% filter(Crime.type=='Burglary'),aes(x=Longitude,y=Latitude),alpha = 0.5,color="blue",size=1) + 
  geom_point(data=solved_WY %>% filter(Crime.type=='Shoplifting'),aes(x=Longitude,y=Latitude),alpha = 0.5,color="green",size=1) + 
  geom_point(data=solved_WY %>% filter(Crime.type=='Other crime'),aes(x=Longitude,y=Latitude),alpha = 0.5,color="yellow",size=1) +
  geom_point(data=solved_WY %>% filter(Crime.type=='Theft from the person'),aes(x=Longitude,y=Latitude),alpha = 0.5,color="purple",size=1) +
  geom_point(data=solved_WY %>% filter(Crime.type=='Vehicle crime'),aes(x=Longitude,y=Latitude),alpha = 0.5,color="orange",size=1) + 
  geom_point(data=solved_WY %>% filter(Crime.type=='Robbery'),aes(x=Longitude,y=Latitude),alpha = 0.5,color="grey",size=1) + 
  geom_point(data=solved_WY %>% filter(Crime.type=='Other theft'),aes(x=Longitude,y=Latitude),alpha = 0.5,color="black",size=1)

crimes_by_type_area <- crimes_by_type_area %>% filter(Area != "Unknown")
crime_types_WY
crimes_by_area_DUR$Latitude <- c(54.7294,54.5236)
crimes_by_area_DUR$Longitude <- c(-1.8812,-1.5595)
crimes_by_area_DUR$Proportion <- crimes_by_area_DUR$Proportion*100


crimes_by_area <- crimes_by_area %>% filter(Area != 'Unknown')
crimes_by_area$latitude <- c(53.7938,53.7248,53.5933,53.8008,53.6833)
crimes_by_area$longitude <- c(-1.7564,-1.8658,-1.8010,-1.5491,-1.5059)


crimes_by_area_DUR
crimes_by_area
crime_counts_combined <- c(crimes_by_area$Crime_Count,crimes_by_area_DUR$Crime_Count)
areas_combined <- c(crimes_by_area$Area,crimes_by_area_DUR$Area)
crimes_by_area_combined <- data.frame(areas_combined,crime_counts_combined)
crimes_by_area_combined$Longitude <- c(crimes_by_area$longitude,crimes_by_area_DUR$Longitude)
crimes_by_area_combined$Latitude <- c(crimes_by_area$latitude,crimes_by_area_DUR$Latitude)
crimes_by_area_combined$Proportion <- round(crimes_by_area_combined$crime_counts_combined/sum(crime_counts_combined),2)*100


UK_map <- get_map(location = c(-1.7,54.3 ),zoom = 8)
UK_crime_sizes <- ggmap(UK_map) + geom_point(data=crimes_by_area_combined,aes(x=Longitude,y=Latitude),alpha=0.5,color="red",size=crimes_by_area_combined$Proportion) 
UK_crime_sizes

WY_map <- get_map(location = c(-1.7626,53.8108),zoom=10)
WY_crime_sizes <- ggmap(WY_map) + geom_point(data=crimes_by_area,aes(x=longitude,latitude),alpha=0.5,color='blue',size=crimes_by_area$Proportion)
WY_crime_sizes

durham_map <- get_map(location = c(-1.5849,54.7753),zoom=9)
durham_map_crime_sizes <- ggmap(durham_map) + geom_point(data=crimes_by_area_DUR,aes(Longitude,Latitude),alpha=0.5,color='purple',size=crimes_by_area_DUR$Proportion)
durham_map_crime_sizes
