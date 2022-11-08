## Coffee and Coding topic:war data##
#Sverke R. Saxegaard
#10.11.22

#the purpose of this script is to show how one might use peacesciencer and r to combine data from different sources.

#packages used in this script
library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(readxl)

#UCDP data
ucdp_e <- read_csv("R_projects/GEDEvent_v22_1.csv")
View(ucdp_e)

ucdp_c <- read_csv("R_projects/ucdp-prio-acd-221.csv")
View(ucdp_c)

ucdp_bd <- read_csv("R_projects/ucdp-brd-conf-221.csv")
View(ucdp_bd)

#SIPRI data
#military expenditure per country (in constant 2020 USD)
sipri_me <- read_excel("R_projects/sipri_me9021.xls")
View(sipri_me)


#IPI data
#troop contributing countries, monthly
ipi_tcc <- read_csv("R_projects/data_tcc.csv")
View(ipi_tcc)


#set up your own data with data from these sources?
#by using the package peacesciencer
#install.packages("peacesciencer")

library(peacesciencer)

#enables us to amke an empty panel data that we can fill with data from various sources

data<- create_stateyears(system = "gw") %>%
  filter(year %in% c(1990:2021))
view(data)

#peacesciencer have few built in datasets (f.ex ucdp armed conflicts and some democracy metrics)

data <- create_stateyears(system = "gw") %>%
  filter(year %in% c(1990:2021))%>%
  add_ucdp_acd ()%>%
  add_democracy()
#problem: only updated to 2019 :( 

#lets try to do it manually (big task!)

#doing some joins with dplyr
?anti_join #shows you the observations 
?left_join #keeps all observation in first dataset, and matching observations from second dataset
?full_join #keeps all observations from both datasets
?inner_join #keeps only observations which are present in both datasets

#create an empty panel:

data<- create_stateyears(system = "gw") %>%
  filter(year %in% c(1990:2021))

#first, conflict data
#lets limit ourselves to intrastate wars

ucdp_c1 <- ucdp_c%>%
  select(conflict_id, location, year, side_a_2nd, side_b, side_b_2nd, intensity_level, type_of_conflict, region, gwno_a)%>%
  filter(type_of_conflict> 2, year > 1989)%>%
  mutate(gwcode = as.numeric(gwno_a))

anti_join(data, ucdp_c1, by = c("gwcode", "year"))
# a lot of country-years do not have conflicts, not surprising
anti_join(ucdp_c1, data, by = c("gwcode", "year"))
#all conflicts found a country year to match to, great!

#joing!
data1 <- left_join(data, ucdp_c1, by = c("gwcode", "year"))
view(data1)
#problem! Some country-years show up multiple times because some country years have multiple conflicts!!
#what to do?
#(easy solution is to download a dataset with the correct datastructure)
#but let us try the hard solution

#making a variable that counts the number of observations per country year
ndata <- data1%>%
  group_by(gwcode, year)%>%
  count()

#joining the new variable to the datset
data2<-
  left_join(data1, ndata, by = c("gwcode", "year"))
  
#making new variables which will be the same for all instances of a unique country-year
data3<- data2%>%
  group_by(gwcode, year)%>%
  mutate(side_b_list = list(side_b))%>%
  mutate(side_a_2nd_list = list(side_a_2nd))%>%
  mutate(side_b_2nd_list = list(side_b_2nd))%>%
  mutate(intensity_level_new = max(intensity_level))%>%
  mutate(conflict_id_list = list(conflict_id))%>%
  mutate(type_of_conflict_new = max(type_of_conflict))%>%
  ungroup()%>%
  mutate(ongoing_conflicts = ifelse(is.na(data2$region), 0, n))%>%
  group_by(gwcode)%>%
  mutate(region_new = max(region, na.rm = TRUE))


view(data3)
#looks good! now lets get rid of the duplicates

#making a new dataset with only the new variables
data4 <- data3%>%
  select(-c(side_b, side_a_2nd,side_b_2nd, intensity_level, conflict_id, type_of_conflict, region))

data5 <- unique(data4)
#notice that data5 has exactly 5477 obs!

#lets add a separate variable that counts the number of conflicts starting and ending

data6 <- data5%>%
  group_by(gwcode)%>%
  arrange(year)%>%
  mutate(L1_ongoing_conflicts = dplyr::lag(ongoing_conflicts,1))%>%
  mutate(change = ongoing_conflicts - L1_ongoing_conflicts)

#some simple plots

data6%>%
  group_by(year)%>%
  mutate(cf = sum(ongoing_conflicts))%>%
  ggplot()+
  geom_line(aes(x= year, y= cf))+
  labs(y= "Number of ongoing conflicts",
       x = "Year",
       title = "Number of ongoing conflicts per Year")+
  theme_minimal()

data6%>%
  group_by(year, region_new)%>%
  mutate(cf = sum(ongoing_conflicts))%>%
  ggplot()+
  geom_line(aes(x= year, y= cf, color = region_new))+
  labs(y= "number of ongoing conflicts",
       x = "Year",
       title = "Number of ongoing conflicts per Year by region",
       color = "Region")+
  theme_minimal()

data6%>%
  group_by(year, region_new)%>%
  mutate(cf = sum(ongoing_conflicts))%>%
  ggplot()+
  geom_line(aes(x= year, y= cf))+
  labs(y= "Number of ongoing conflicts",
       x = "Year",
       title = "Number of ongoing conflicts per Year, by region")+
  facet_wrap(~region_new) +
  theme_minimal()




#second, military expenditure

view(sipri_me)

#ahh so we have data for each country year, but we do not have gwcode, and the data is in a different structure. Let us see what we can do

#changing the structure of the data
sipri2 <- sipri_me%>%
  pivot_longer(cols = c("1990":"2021"),
               names_to = "year",
               values_to = "mil_exp")
view(sipri2) #success!

#trying to merge...
#first, making sure we have variables to match with
sipri3<- sipri2%>%
  mutate(statename = Country)%>%
  mutate(year = as.numeric(year))

anti_join (data6, sipri3, by = c("statename", "year"))

#oh wow, 34 countries with dissimilar names...

testdata <- anti_join (data6, sipri3, by = c("statename", "year"))
view(testdata)
list<- testdata%>%
  count(statename)
print(list, n =34)
view(sipri3)

#some of these are not included in SIPRIs data because they are too small (Bahamas, Barbados, Solomon Islands)
#the rest just have different names...
#there is probably as clever way to do this...
#f.ex the country code package (countrycode)
#But I am not that clever right now, so lets do it manually

sipri4<- sipri3%>%
  mutate(statename = case_when(
    statename == "Germany" ~ "German Federal Republic",
    statename == "Italy" ~ "Italy/Sardinia",
    statename == "Romania"~"Rumania",
    statename == "USSR" & year < 1991 ~"Russia (Soviet Union)",
    statename == "Russia" & year > 1991 ~"Russia (Soviet Union)",
    statename == "Gambia, The"~"Gambia",
    statename == "Cote d'Ivoire" ~"Cote D'Ivoire",
    statename == "Burkina Faso"~"Burkina Faso (Upper Volta)",
    statename == "Congo, Republic"~"Congo",
    statename == "Congo, DR"~"Congo, Democratic Republic of (Zaire)",
    statename == "Tanzania" ~"Tanzania/Tanganyika",
    statename == "Zimbabwe" ~ "Zimbabwe (Rhodesia)",
    statename == "Eswatini" ~"Swaziland",
    statename == "Iran"~"Iran (Persia)",
    statename == "Turkey"~"Turkey (Ottoman Empire)",
    statename == "Yemen" ~"Yemen (Arab Republic of Yemen)",
    statename == "Korea, North" ~"Korea, People's Republic of",
    statename == "Korea, South" ~"Korea, Republic of",
    statename == "Myanmar"~"Myanmar (Burma)",
    statename == "Sri Lanka"~"Sri Lanka (Ceylon)",
    statename == "Cambodia"~"Cambodia (Kampuchea)",
    statename == "Viet Nam" ~"Vietnam, Democratic Republic of",
    statename == "North Macedonia"~"Macedonia (Former Yugoslav Republic of)",
    statename == "Belarus"~"Belarus (Byelorussia)",
    statename == "Bosnia and Herzegovina"~"Bosnia-Herzegovina",
    statename == "Czechia"~"Czech Republic",
    statename == "Timor Leste"~"East Timor",
    TRUE~ statename))
  

#did it work?

testdata <- anti_join (data6, sipri4, by = c("statename", "year"))
list<- testdata%>%
  count(statename)
print(list, n =34)

#acceptable
#lets do the proper merge

data7 <- left_join(data6, sipri4,by = c("statename", "year"))
view(data7)
str(data7)
#looks good! (except some variables really should not be charachter, but numbers)

data7b<- data7%>%
  ungroup()%>%
  mutate(mil_exp = as.numeric(mil_exp),
         region = as.numeric(region_new))
view(data7b)
str(data7b)
#looks good!

#third, troop contribution

view(ipi_tcc) # okay what do we have here? Looks like monthly data

#then lets bring out the 'lubridate' package

ipi_tcc$date <- as.POSIXct(ipi_tcc2$Date, format = "%Y-%m-%d ") #making r understand the Date variable is actually a date
ipi_tcc$year <- as.numeric(format(ipi_tcc2$date, format="%Y"))#extracting the year 


#making new variables on a yearly level
ipi_tcc2 <- ipi_tcc%>%
  group_by(year, Contributor)%>%
  mutate(missions = max(`Number of Missions Contributed To`),
         meantroops = mean(Troops),
         meanpolice = mean(Civilian_Police),
         meanobservers = mean(Observers),
         meantotal = mean(Total))

ipi_tcc2<- ipi_tcc2%>%
  mutate(statename = Contributor)

#keeping only the variables on yearly level (aka making a lot of duplicates)
ipi_tcc3 <- ipi_tcc2%>%
  ungroup()%>%
  select(statename, year, missions, meantroops, meanpolice, meanobservers, meantotal)

#removing the duplicates

ipi_tcc4<- unique(ipi_tcc3)
view(ipi_tcc4)

#looks good!
#time to merge

#First, what observations in ipi_tcc4 does not exist in data7??
testdata <- anti_join (ipi_tcc4, data7b, by = c("statename", "year"))
list<- testdata%>%
  count(statename)
print(list, n =200)

#ahhh, we know this problem right?
#note the subtle differences from last case_when code...
ipi_tcc5<- ipi_tcc4%>%
  mutate(statename = case_when(
    statename == "Germany" ~ "German Federal Republic",
    statename == "Italy" ~ "Italy/Sardinia",
    statename == "Romania"~"Rumania",
    statename == "Russian Federation"~"Russia (Soviet Union)",
    statename == "Gambia, The"~"Gambia",
    statename == "Cote d Ivoire" ~"Cote D'Ivoire", 
    statename == "Burkina Faso"~"Burkina Faso (Upper Volta)",
    statename == "Congo, Republic"~"Congo",
    statename == "DR Congo"~"Congo, Democratic Republic of (Zaire)",
    statename == "Tanzania" ~"Tanzania/Tanganyika",
    statename == "Zimbabwe" ~ "Zimbabwe (Rhodesia)",
    statename == "Eswatini" ~"Swaziland",
    statename == "Iran"~"Iran (Persia)",
    statename == "Turkey"~"Turkey (Ottoman Empire)",
    statename == "Yemen" ~"Yemen (Arab Republic of Yemen)",
    statename == "Korea, North" ~"Korea, People's Republic of",
    statename == "Republic of Korea" ~"Korea, Republic of",
    statename == "Myanmar"~"Myanmar (Burma)",
    statename == "Sri Lanka"~"Sri Lanka (Ceylon)",
    statename == "Cambodia"~"Cambodia (Kampuchea)",
    statename == "Vietnam" ~"Vietnam, Democratic Republic of",
    statename == "Macedonia"~"Macedonia (Former Yugoslav Republic of)",
    statename == "The former Yugoslav Republic of Macedonia"~"Macedonia (Former Yugoslav Republic of)",
    statename == "Belarus"~"Belarus (Byelorussia)",
    statename == "Bosnia and Herzegovina"~"Bosnia-Herzegovina",
    statename == "Timor Leste"~"East Timor",
    statename == "Kyrgyzstan"~"Kyrgyz Republic",
    TRUE~ statename))

testdata <- anti_join (ipi_tcc5, data7b, by = c("statename", "year"))
list<- testdata%>%
  count(statename)
print(list, n =200)

#still some issues, but i do not have time to fix that now...

#lets do the merge!!!

data8 <- left_join(data7b, ipi_tcc5, by = c("statename", "year"))
view(data8)

#FINALY!
#now we have a dataset with data on intrastate conflict, military expenditure and troop contribution
#anything we would like to find out?

#lets remove all the datasets and objects we are no longer using
rm(list=setdiff(ls(), "data8"))

#Do states with larger military budget send more peacekeepers?
#note that we have a lot of non-random missing here so take the results with a huge pinch of salt

#basic ols
model1 <- lm (meantroops~mil_exp, data = data8)
summary(model1)  

#no effect

model2 <- lm (meantroops~mil_exp+ongoing_conflicts, data = data8)
summary(model2)

#more civil wars = more troops contributed!

data8%>%
  ggplot()+
  geom_point(aes(mil_exp, meantroops))+
  theme_minimal()

data8%>%
  ggplot()+
  geom_point(aes(meantroops, ongoing_conflicts))+
  theme_minimal()

#lets add some country-fixed effects to see if more conflicts and more military expenditure leads to more troops contribution within single countries
library(fixest)

model3<-feols(meantroops ~ mil_exp + ongoing_conflicts | statename, cluster = "statename", data = data8)
summary(model3)

#with fixed effects: more conflicts = less troops contributed



