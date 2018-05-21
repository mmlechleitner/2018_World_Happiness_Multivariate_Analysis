#### INITIALIZE ####
# Require Libraries:
library(tidyverse)
library(feather)
library(skimr)

# Load Custom Functions:
function_files <- paste0("SRC/FUNCTIONS/" , (dir(path = "SRC/FUNCTIONS/")))
for(i in seq_along(function_files)) {
  source(file = function_files[[i]])
}
rm(function_files, i)

#### WORLD HAPPINESS REPORT ####
# World-happiness-report as the basis df, to which we gonna add the other features..
happiness <- read.csv("DATA/RAW_DATA/0. happiness_report2015.csv", stringsAsFactors = FALSE)
happiness <- happiness %>% 
  select(
     Country
    ,Region
    ,"Happiness_Rank" = Happiness.Rank
    ,"Happiness_Score" = Happiness.Score
    ,"Standard_Error" = Standard.Error
    ,"Econonmy_GDP_Per_Capita" = Economy..GDP.per.Capita.
    ,Family
    ,"Health_Life_Expectancy" = Health..Life.Expectancy.
    ,Freedom
    ,"Trust_Government_Corruption" = Trust..Government.Corruption.
    ,Generosity
    ,"Dystopia_Residual" = Dystopia.Residual
  )

#### 1. Air Pollution ####
airpollution <- read.csv("DATA/RAW_DATA/1.PM25-air-pollution.csv", stringsAsFactors = FALSE)

airpollution <- airpollution %>% 
  select(
    "Country" = Entity,
    "Air_Pollution" = PM2.5.air.pollution..mean.annual.exposure..micrograms.per.cubic.meter...micrograms.per.cubic.meter.
    ) %>%
  filter(airpollution$Year == 2015)

happiness <- left_join(x = happiness, y = airpollution, by = "Country")

rm(airpollution)

#### 2. Share of Individuals Using Internet ####
internetusers <- read.csv("DATA/RAW_DATA/2. 2015 - share-of-individuals-using-the-internet.csv", stringsAsFactors = FALSE)
internetusers <- internetusers %>%
  filter(internetusers$Year == 2015)

internetusers <- internetusers %>% 
  select(
    "Country" = Entity,
    "Internet_Usage_Index" = Individuals.using.the.Internet....of.population.....of.population.
    ) 

happiness <- left_join(x = happiness, y = internetusers, by = "Country")

rm(internetusers)

#### 3. Mobile User Percentage ####

mobile <- read.csv("DATA/RAW_DATA/3. 2015 - mobile-cellular-subscriptions-per-100-people.csv", stringsAsFactors = FALSE)

mobile <- mobile %>% filter(mobile$Year == 2015)

mobile <- mobile %>% 
  select(
    "Country" = Entity
    ,"Cellular_Subscriber_Index" = Mobile.cellular.subscriptions..per.100.people...per.100.people.)

happiness <- left_join(x = happiness, y = mobile, by = "Country")

rm(mobile)


#check and remove NA rows

happiness <- happiness[complete.cases(happiness[ , 1:13]),]
sum(is.na(happiness))


#### 4. Population ####

population <- read.csv("DATA/RAW_DATA/4. population-by-country-gapminder+un.csv", stringsAsFactors = FALSE)
population <- population %>% filter(population$Year == 2015)

population <- population %>% 
  select(
     "Country" = Entity
    ,"Total_Population" = Total.population..Gapminder..UN.Population.Division.
    )

happiness <- left_join(x = happiness, y = population, by = "Country")

rm(population)

#### 6. Child Mortality ####

child <- read.csv("DATA/RAW_DATA/6. child-mortality.csv", stringsAsFactors = FALSE)
child <- child %>% filter(child$Year == 2015)
child <- child %>% 
  select(
     "Country" = Entity
    ,"Child_Mortality_Index" = Under.five.mortality.rate....of.children.that.die.before.they.are.5.years.old.
    )

happiness <- left_join(x = happiness, y = child, by = "Country")

rm(child)

#check and remove NA rows

happiness <- happiness[complete.cases(happiness[ , 1:13]),]
sum(is.na(happiness))


#### 7. Alcohol consumption ####

alcohol <- read.csv("DATA/RAW_DATA/7. total-alcohol-consumption-per-capita-litres-of-pure-alcohol.csv", stringsAsFactors = FALSE)
alcohol <- alcohol %>% filter(alcohol$Year == 2015)
alcohol <- alcohol %>% 
  select(
     "Country" = Entity
    ,"Alcohol_Consumption_Index" = Alcohol.consumption..litres.per.capita...liters.of.pure.alcohol..projected.estimates..15..years.of.age.
    )

happiness <- left_join(x = happiness, y = alcohol, by = "Country")

rm(alcohol)

#### 8. Children born per woman (indeed per woman...) ####

newborn <- read.csv("DATA/RAW_DATA/8. children-born-per-woman.csv", stringsAsFactors = FALSE) #no... really.. per woman.. wow
newborn <- newborn %>% filter(newborn$Year == 2015)
newborn <- newborn %>% 
  select(
     "Country" = Entity
    ,"Birth_Rate_Index" = X.children.per.woman.
    )

happiness <- left_join(x = happiness, y = newborn, by = "Country")

rm(newborn)

#### 9. share-with-mental-and-substance-disorders.csv ####

disorder <- read.csv("DATA/RAW_DATA/9. share-with-mental-and-substance-disorders.csv", stringsAsFactors = FALSE)
disorder <- disorder %>% filter(disorder$Year == 2015)
disorder <- disorder %>% 
  select(
     "Country" = Entity
    ,"Mental_and_Substance_Disorder_Index" = Prevalence...Mental.and.substance.use.disorders...Sex..Both...Age..Age.standardized..Percent.....
    )

happiness <- left_join(x = happiness, y = disorder, by = "Country")

rm(disorder)

#### 10. 10. suicide-death-rates.csv ####

suicide <- read.csv("DATA/RAW_DATA/10. suicide-death-rates.csv", stringsAsFactors = FALSE)
suicide <- suicide %>% filter(suicide$Year == 2015)
suicide <- suicide %>% 
  select(
       "Country" = Entity
      ,"Suicide_Index" = Deaths...Self.harm...Sex..Both...Age..Age.standardized..Rate...deaths.per.100.000.individuals.
      )

happiness <- left_join(x = happiness, y = suicide, by = "Country")

rm(suicide)

#### 14. share-of-the-population-with-access-to-improved-drinking-water ####

Drinking_water <- read.csv("DATA/RAW_DATA/14. share-of-the-population-with-access-to-improved-drinking-water.csv", stringsAsFactors = FALSE)
Drinking_water <- Drinking_water %>% filter(Drinking_water$Year == 2015)
Drinking_water <- Drinking_water %>% 
  select(
     "Country" = Entity
    ,"Clean_Water_Index" = Improved.water.source....of.population.with.access.....of.population.with.access. )

happiness <- left_join(x = happiness, y = Drinking_water, by = "Country")

rm(Drinking_water)

#### 16. population-growth-rates ####

temp_df <- read.csv("DATA/RAW_DATA/16. population-growth-rates.csv", stringsAsFactors = FALSE)
temp_df <- temp_df %>% filter(temp_df$Year == 2015)
temp_df <- temp_df %>% 
  select(
     "Country" = Entity
    ,"Population_Growth_Rate" = Medium.fertility.variant..2015...2099..Demographic.Indicators...Population.growth.rate..percentage...percentage.
    )

happiness <- left_join(x = happiness, y = temp_df, by = "Country")

rm(temp_df)

#### 17. share-with-depression 2016 ####

temp_df <- read.csv("DATA/RAW_DATA/17. share-with-depression 2016.csv", stringsAsFactors = FALSE)
temp_df <- temp_df %>% filter(temp_df$Year == 2015)
temp_df <- temp_df %>% 
  select(
     "Country" = Entity
    ,"Depression_Index" = Prevalence...Depressive.disorders...Sex..Both...Age..Age.standardized..Percent.....
    )

happiness <- left_join(x = happiness, y = temp_df, by = "Country")

rm(temp_df)

#### 18. maternal-mortality ####

temp_df <- read.csv("DATA/RAW_DATA/18. maternal-mortality.csv", stringsAsFactors = FALSE)
temp_df <- temp_df %>% filter(temp_df$Year == 2015)
temp_df <- temp_df %>% 
  select(
     "Country" = Entity
    ,"ChildBirth_Maternal_Mortality_Index" = Maternal.Mortality.Ratio..Gapminder..2010..and.World.Bank..2015....deaths.per.100.000.live.births.
  )

happiness <- left_join(x = happiness, y = temp_df, by = "Country")

rm(temp_df)

#### 20. share-of-pop-infected-with-hiv 2016 ####
temp_df <- read.csv("DATA/RAW_DATA/20. share-of-pop-infected-with-hiv 2016.csv", stringsAsFactors = FALSE)
temp_df <- temp_df %>% filter(temp_df$Year == 2015)
temp_df <- temp_df %>% 
  select(
    "Country" = Entity
    ,"HIV_Disease_Index" = Prevalence...HIV.AIDS...Sex..Both...Age..15.49.years..Percent.....
  )

happiness <- left_join(x = happiness, y = temp_df, by = "Country")

rm(temp_df)

#### 21. share-of-adults-defined-as-obese 2016 ####

temp_df <- read.csv("DATA/RAW_DATA/21. share-of-adults-defined-as-obese 2016.csv", stringsAsFactors = FALSE)
temp_df <- temp_df %>% filter(temp_df$Year == 2015)

temp_df <- temp_df %>% 
  select(
    "Country" = Entity
    ,"Obesity_Index" = Indicator.Prevalence.of.obesity.among.adults..BMI..GreaterEqual..30..age.standardized.estimate........Age.Group.18...years...Sex.Both.sexes....
  )

happiness <- left_join(x = happiness, y = temp_df, by = "Country")

rm(temp_df)

#### 24. annual-co-emissions-per-country ####

temp_df <- read.csv("DATA/RAW_DATA/24. annual-co-emissions-per-country.csv", stringsAsFactors = FALSE)
temp_df <- temp_df %>% filter(temp_df$Year == 2015)

temp_df <- temp_df %>% 
  select(
    "Country" = Entity
    ,"CO2_Emission_Index" = Annual.COâ...emissions..Global.Carbon.Project..2017....million.tonnes.
  )

happiness <- left_join(x = happiness, y = temp_df, by = "Country")

rm(temp_df)


#### Change Country Names for Joining #####

old_country_name = c("Syria", "Egypt", "Iran", "Kyrgyzstan", "Laos", "Russia", "Macedonia", "Slovakia", "South Korea", "Sudan")
new_country_name = c("Syrian Arab Republic", "Egypt, Arab Rep.", "Iran, Islamic Rep.", "Kyrgyz Republic", "Lao PDR", "Russian Federation",
                     "Macedonia, FYR", "Slovak Republic", "Korea, Rep.", "South Sudan")

for(i in 1:nrow(happiness)) {
  for(j in 1:length(old_country_name)) {
    if(happiness$Country[[i]] == old_country_name[[j]]) {
      happiness$Country[[i]] <- new_country_name[[j]]
    }
  }
}

#### Electricity Access ####

temp_df <- read.csv("DATA/RAW_DATA/Electricity_Access.csv", stringsAsFactors = FALSE)

temp_df <- temp_df %>%
  filter(ï..Series.Name == "Access to electricity (% of population)")

temp_df <- temp_df %>% 
  select(
    "Country" = Country.Name
    ,"Electricity_Access_Population_Percentage" = X2015..YR2015.
  )

happiness <- left_join(x = happiness, y = temp_df, by = "Country")

rm(temp_df)


#### Military Forces Personnel ####

temp_df <- read.csv("DATA/RAW_DATA/Military_Forces.csv", stringsAsFactors = FALSE)

temp_df <- temp_df %>%
  filter(ï..Series.Name == "Armed forces personnel (% of total labor force)")

temp_df <- temp_df %>% 
  select(
    "Country" = Country.Name
    ,"Military_Personnel_Index" = X2015..YR2015.
  )

happiness <- left_join(x = happiness, y = temp_df, by = "Country")

rm(temp_df)


#### Compulsory Education in Years ####

temp_df <- read.csv("DATA/RAW_DATA/Compulsory_Education_Years.csv", stringsAsFactors = FALSE)

temp_df <- temp_df %>%
  filter(ï..Series.Name == "Compulsory education, duration (years)")

temp_df <- temp_df %>% 
  select(
    "Country" = Country.Name
    ,"Compulsory_Education_in_Years" = X2015..YR2015.
  )

happiness <- left_join(x = happiness, y = temp_df, by = "Country")

rm(temp_df)

#### Agricultural Land Percentage ####

temp_df <- read.csv("DATA/RAW_DATA/Agricultural_Land_Percentage.csv", stringsAsFactors = FALSE)

temp_df <- temp_df %>%
  filter(ï..Series.Name == "Agricultural land (% of land area)")

temp_df <- temp_df %>% 
  select(
    "Country" = Country.Name
    ,"Agricultural_Land_Percentage" = X2015..YR2015.
  )

happiness <- left_join(x = happiness, y = temp_df, by = "Country")

rm(temp_df)


#### Entrepreneurship Cost ####
temp_df <- read.csv("DATA/RAW_DATA/Entrepreneurship_Cost.csv", stringsAsFactors = FALSE)

temp_df <- temp_df %>%
  filter(ï..Series.Name == "Cost of business start-up procedures (% of GNI per capita)")

temp_df <- temp_df %>% 
  select(
    "Country" = Country.Name
    ,"Entrepreneurship_Cost_Index" = X2015..YR2015.
  )

happiness <- left_join(x = happiness, y = temp_df, by = "Country")

rm(temp_df)

#### Foreign Direct Investment Net Funds Flow ####

temp_df <- read.csv("DATA/RAW_DATA/Foreign_Direct_Investment_Net_Flow.csv", stringsAsFactors = FALSE)

temp_df <- temp_df %>%
  filter(ï..Series.Name == "Foreign direct investment, net (BoP, current US$)")

temp_df <- temp_df %>% 
  select(
    "Country" = Country.Name
    ,"FDI_Net_Flow" = X2015..YR2015.
  )

happiness <- left_join(x = happiness, y = temp_df, by = "Country")

rm(temp_df)


#### Forest Area Land Percentage ####
temp_df <- read.csv("DATA/RAW_DATA/Forest_Area_Land_Percentage.csv", stringsAsFactors = FALSE)

temp_df <- temp_df %>%
  filter(ï..Series.Name == "Forest area (% of land area)")

temp_df <- temp_df %>% 
  select(
    "Country" = Country.Name
    ,"Forest_Area_Land_Percentage" = X2015..YR2015.
  )

happiness <- left_join(x = happiness, y = temp_df, by = "Country")

rm(temp_df)

#### Immunization Measles ####
temp_df <- read.csv("DATA/RAW_DATA/Immunization_Measles.csv", stringsAsFactors = FALSE)

temp_df <- temp_df %>%
  filter(ï..Series.Name == "Immunization, measles (% of children ages 12-23 months)")

temp_df <- temp_df %>% 
  select(
    "Country" = Country.Name
    ,"Infant_Immunization_Measles_Index" = X2015..YR2015.
  )

happiness <- left_join(x = happiness, y = temp_df, by = "Country")

rm(temp_df)

#### Imports of Goods and Services GDP Percentage ####
temp_df <- read.csv("DATA/RAW_DATA/Imports_of_Goods_and_Services_Percentage_of_GDP.csv", stringsAsFactors = FALSE)

temp_df <- temp_df %>%
  filter(ï..Series.Name == "Imports of goods and services (% of GDP)")

temp_df <- temp_df %>% 
  select(
    "Country" = Country.Name
    ,"GDP_Imports_Percentage" = X2015..YR2015.
  )

happiness <- left_join(x = happiness, y = temp_df, by = "Country")

rm(temp_df)

#### Military Expenditures GDP Percentage ####
temp_df <- read.csv("DATA/RAW_DATA/Military_Expenditure_GDP_Percentage.csv", stringsAsFactors = FALSE)

temp_df <- temp_df %>%
  filter(ï..Series.Name == "Military expenditure (% of GDP)")

temp_df <- temp_df %>% 
  select(
    "Country" = Country.Name
    ,"Military_Expenditure_GDP_Percentage" = X2015..YR2015.
  )

happiness <- left_join(x = happiness, y = temp_df, by = "Country")

rm(temp_df)


#### Road Traffic Mortality ####
temp_df <- read.csv("DATA/RAW_DATA/Mortality_Rate_Caused_by_Road_Traffic_Injury_per_100_People.csv", stringsAsFactors = FALSE)

temp_df <- temp_df %>%
  filter(ï..Series.Name == "Mortality caused by road traffic injury (per 100,000 people)")

temp_df <- temp_df %>% 
  select(
    "Country" = Country.Name
    ,"Automotive_Mortality_Index" = X2015..YR2015.
  )

happiness <- left_join(x = happiness, y = temp_df, by = "Country")

rm(temp_df)

#### Urban Population Percentage ####
temp_df <- read.csv("DATA/RAW_DATA/Urban_Population_Percentage_of_Total.csv", stringsAsFactors = FALSE)

temp_df <- temp_df %>%
  filter(ï..Series.Name == "Urban population (% of total)")

temp_df <- temp_df %>% 
  select(
    "Country" = Country.Name
    ,"Urban_Population_Percentage" = X2015..YR2015.
  )

happiness <- left_join(x = happiness, y = temp_df, by = "Country")

rm(temp_df)

#### Legal Rights Index ####
temp_df <- read.csv("DATA/RAW_DATA/Strength_of_Legal_Rights_Index.csv", stringsAsFactors = FALSE)

temp_df <- temp_df %>%
  filter(ï..Series.Name == "Strength of legal rights index (0=weak to 12=strong)")

temp_df <- temp_df %>% 
  select(
    "Country" = Country.Name
    ,"Legal_Rights_Index" = X2015..YR2015.
  )

happiness <- left_join(x = happiness, y = temp_df, by = "Country")

rm(temp_df)
#### CLEANUP ####

# Remove variables we can't use

happiness_transformed <- happiness

happiness_transformed <- happiness_transformed %>% 
  select(
     -Happiness_Rank
    ,-Standard_Error
    ,-Military_Expenditure_GDP_Percentage
  )

# Change ".." values to NA
for(i in seq_along(happiness_transformed)) {
  for(j in 1:nrow(happiness_transformed)) {
    ifelse(test = happiness_transformed[j,i] == "..", yes = NA, no = happiness_transformed[j,i])
  }
}

# Convert character vectors to numeric
for(i in 3:nrow(happiness_transformed)) {
  if(is.character(happiness_transformed[1,i]) == TRUE) {
    happiness_transformed[i] <- as.numeric(happiness_transformed[,i])
  }
}

happiness <- happiness_transformed
rm(happiness_transformed)

# Remove Rejected Variables
happiness <- happiness %>%
  select(
     -Dystopia_Residual
    ,-GDP_Imports_Percentage
    ,-FDI_Net_Flow
    ,-Military_Personnel_Index
    ,-Depression_Index
    ,-CO2_Emission_Index
    ,-ChildBirth_Maternal_Mortality_Index
  )

# Remove incomplete cases
happiness <- happiness[complete.cases(happiness),]

#### Write Final CSV File ####
write.csv(happiness, file="DATA/WorldAnalysis.csv")


#### Test Correlation Matrix ####
# corr_matrix <- happiness %>%
#   select(-Country, -Region)
# 
# corr_matrix <- round(cor(corr_matrix), 1)
# 
# require(ggcorrplot)
# ggcorrplot(corr_matrix, hc.order = TRUE, 
#            type = "lower", 
#            lab = TRUE, 
#            lab_size = 3, 
#            method="circle", 
#            colors = c("tomato2", "white", "springgreen3"), 
#            title="Correlation Matrix of Country Indexes", 
#            ggtheme=theme_bw)
