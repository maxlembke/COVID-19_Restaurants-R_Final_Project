#' Title: Eating out in the Pandemic: Supporting local business or super spreading?'
#' Author: Tuty Chau and Max Lembke

# Set-up

library(tidyverse) # Standard package 
library(lubridate) # A package that makes dealing with date formats significantly easier
library(reshape2) # Enables improved pivots

# Load data

Org_C_Data <- read_csv("COVID-19_Cases__Tests__and_Deaths_by_ZIP_Code.csv")
Org_Chi_Data <- read_csv("Food_Inspections.csv")

covid_df <- data.frame(Org_C_Data)
chi_df <- data.frame(Org_Chi_Data)

# Data cleaning 

# Dropping un-useful variables
covid_df <- covid_df[-c(20:21)]
chi_df <- subset(chi_df, select = -c(City, State, Location))

# Renaming variables for COVID and Chicago data
covid_df = covid_df %>% 
  rename(
    zip = ZIP.Code,
    week_num = Week.Number,
    week_st = Week.Start,
    week_end = Week.End,
    cases_wkl = Cases...Weekly,
    cases_cum = Cases...Cumulative,
    case_rate_wkl = Case.Rate...Weekly,
    case_rate_cum = Case.Rate...Cumulative,
    test_wkl = Tests...Weekly,
    test_cum = Tests...Cumulative,
    test_rate_wkl = Test.Rate...Weekly,
    test_rate_cum = Test.Rate...Cumulative,
    prcnt_pos_wkl = Percent.Tested.Positive...Weekly,
    prcnt_pos_cum = Percent.Tested.Positive...Cumulative,
    deaths_wkl = Deaths...Weekly,
    deaths_cum = Deaths...Cumulative,
    death_rate_wkl = Death.Rate...Weekly,
    death_rate_cum = Death.Rate...Cumulative
  )

chi_df = chi_df %>% 
  rename(
    ins_id = Inspection.ID,
    d_name = DBA.Name, 
    a_name = AKA.Name,
    lic_num = License..,
    type = Facility.Type,
    risk = Risk,
    addr = Address,
    zip = Zip,
    ins_date = Inspection.Date,
    ins_type = Inspection.Type,
    results = Results,
    vio = Violations,
    lat = Latitude,
    long = Longitude
  )


# Filling NA's with 0 to avoid bugs
covid_df[is.na(covid_df)] <- 0
chi_df[is.na(chi_df)] <- 0

# Establishing type cleaning
covid_df$zip <- as.numeric(covid_df$zip)
covid_df$week_st <- as.Date(covid_df$week_st, format = "%m/%d/%y", origin = "%m/%d/%y")
covid_df$week_end <- as.Date(covid_df$week_end, format = "%m/%d/%y", origin = "%m/%d/%y")

chi_df$ins_date <- as.Date(chi_df$ins_date, format = "%m/%d/%y", origin = "%m/%d/%y")

# Converting Risk Class to numeric for easier analysis 
chi_df$risk [chi_df$risk == "Risk 1 (High)"] <- 1
chi_df$risk [chi_df$risk == "Risk 2 (Medium)"] <- 2
chi_df$risk [chi_df$risk == "Risk 3 (Low)"] <- 3
chi_df$risk <- as.integer(chi_df$risk)

#Cleaning different classes of restaurant to one unified spelling 
chi_df$type <-tolower(chi_df$type) #Changing the case 
chi_df$type2 = chi_df$type # Copying the column to preserve original data 
chi_df$type2 <- str_replace_all(chi_df$type2, "^(rest).*", "restaurant") #Replacing all strings starting with rest to restaurant 
chi_df$type2[chi_df$type2 != "restaurant"] <- 0 #Converting all other business types to 0 


# Creating a count of the violations per inspection 

data_vio <- select(chi_df, c(ins_id, vio)) 

# Creating a function that counts the amount of violations per inspection
get_violations = function(x){
  n_violations = vector("double", length = length(x))
  for(i in seq_along(x)){
    n_violations[i] = x[i] %>% 
      str_match_all("[0-9]+\\.\\s") %>% 
      unlist() %>% 
      length()
  }
  return(n_violations)
}

# Mapping the data and creating the new column 
data_vio$n_violations = data_vio %>% 
  select(vio) %>% 
  map(get_violations) %>% 
  unlist()

# Dropping the violation before joining as already present 
data_vio <- subset(data_vio, select = -c(vio))

# Left join on the original dataframe with all inspection data 
chi_df <- left_join(data_vio, chi_df, by = "ins_id")

# Changing name
chi_df = chi_df %>% 
  rename(vio_n = n_violations)

# Creating week column 
chi_df$ins_date2 = chi_df$ins_date
chi_df$ins_date2 <- isoweek(chi_df$ins_date2)

# Creating week column 
chi_df$ins_date3 = chi_df$ins_date
chi_df$ins_date3 <- isoyear(chi_df$ins_date3)

# Creating the COVID/Food violation df 
df_com = chi_df %>%
  filter(ins_date3 == 2020 & ins_date2 >= 10)%>% #Only pre, not after trend and weeks >= as COVID records start in 10 
  group_by(ins_date2, zip)%>%
  summarise(number_vio = sum(vio_n))

df_com = df_com %>% 
  rename(week_num = ins_date2)

df_com <- left_join(df_com,covid_df, by = c("zip" = "zip", "week_num" = "week_num"))


# Ceating df violations over time + as percentace

#Violations in 2020 
new_df = chi_df %>% 
  filter(chi_df$ins_date > as.Date("2019-12-31"))

#Violations all years
df2 = chi_df %>% 
  group_by(ins_date3, ins_date2)%>%
  summarise(count_insp = n(),number = sum(vio_n))

#Calculations of percentage 
df2$vio_as_per = NA
df2$vio_as_per = (df2$number/df2$count_insp)*100

# DF for out-of-business creation 

# New df with all types, counts of inspections, excluding the post trend 
count_insp_y <- chi_df %>%
  filter(chi_df$ins_date2 < 50)%>%
  group_by(year = isoyear(ins_date)) %>%
  summarise(count=n())

# New df with all types, with count of out of business, excluding the post trend 
count_out_y <- chi_df %>%
  filter(chi_df$ins_date2 < 50)%>%
  filter(results == "Out of Business") %>%
  group_by(results,year = isoyear(ins_date)) %>%
  summarise(number = n())

# Joining the two df together, on year
out_all_df <- left_join(count_insp_y, count_out_y, by = "year")
out_all_df <- data.frame(out_all_df)
out_all_df$all_out_per = NA #Creating new column  
out_all_df$all_out_per <- (out_all_df$number/out_all_df$count)*100   #Percentage calc
out_all_df <- subset(out_all_df, select = -c(count, results, number)) #Dropping unneeded columns 

# New df only restaurants, counts of inspections, excluding the post trend 
count_insp_y <- chi_df %>%
  filter(chi_df$type2 != 0 & chi_df$ins_date2 < 50)%>%
  group_by(year = isoyear(ins_date)) %>%
  summarise(count=n())

# New df only restaurants, counts of out of business, excluding the post trend 
count_out_y <- chi_df %>%
  filter(chi_df$type2 != 0 & chi_df$ins_date2 < 50)%>%
  filter(results == "Out of Business") %>%
  group_by(results,year = isoyear(ins_date)) %>%
  summarise(number = n())

# Joining the two df together, on year
out_re_df <- left_join(count_insp_y, count_out_y, by = "year")
out_re_df$res_out_per = NA #Creating new column  
out_re_df <- data.frame(out_re_df) 
out_re_df$res_out_per <- (out_re_df$number/out_re_df$count)*100 #Percentage calc
out_re_df <- subset(out_re_df, select = -c(count, results, number))#Dropping unneeded columns 

# Joining all the df together
out_of_b <- left_join(out_re_df, out_all_df, by = "year")

#Modeling Df

`%notin%` <- Negate(`%in%`) #Creating not in

#Creating df for modelling (log)
df_com_2 = df_com %>% 
  filter(week_num < 49)%>% #Filtering out other weeks that have not submitted data
  filter(zip %notin% c("60666","0","60201","60455"))%>% #Exlcuding zip without data (collection error)
  group_by(zip)%>% 
  summarise(vio = sum(number_vio), cases = sum(case_rate_wkl))

df_com_2$l_vio = log(df_com_2$vio)  
df_com_2$l_cases = log(df_com_2$cases)

#Plot: COVID-19 Cases over time in Chicago

covid_df %>% 
  group_by(week_num) %>% 
  summarise(sum_cases = sum(cases_wkl)) %>% 
  ggplot(data = ., mapping = aes(x = week_num, y = sum_cases)) +
  geom_line() +
  geom_point() + 
  geom_point(aes(x=26,y=1336),colour="green", size=4) + 
  geom_point(aes(x=12,y=882),colour="red", size=4) +
  labs(title = "COVID cases over time",
       x = "Week number",
       y = "Total cases")

#Plot: Restaurants out of business

chi_df %>%
  filter(chi_df$type2 != 0 & chi_df$ins_date2 < 50)%>%
  filter(results == "Out of Business") %>%
  group_by(results,year = isoyear(ins_date)) %>%
  summarise(number = n())%>%
  ggplot(data = ., mapping = aes(as.factor(year), y = number, fill = results )) + 
  geom_col(fill = "lightblue") + 
  labs(title = "Restaurants out of business",
       x = "Year",
       y = "Number of businesses")

#Plot: Out of business percentage

data_long <- melt(out_of_b, id="year")

ggplot(data_long, aes(x=factor(year), y= value, color = variable, group = variable)) +
  geom_line() +
  labs(title = "Out of business as a percentage of inspections",
       x = "Year",
       y = "Percentage")

#Plot: Cumulative Violations over time

myColors <- c("#51397F", "#3950E5", "#396B7F","#0CF4FC")
df2 %>% 
  filter(ins_date3 %in% c("2020","2019","2018","2017"))%>%
  group_by(ins_date3)%>%
  mutate(ins_date2, number = cumsum(number))%>% 
  ggplot(data = ., mapping = aes(x = ins_date2, y = number, color = factor(ins_date3))) +
  geom_line() + 
  geom_point()+
  scale_color_manual(values=myColors)+
  labs(title = "Cumulative violations over time",
       x = "Week number",
       y = "Number of violations")

#Plot: Violations over time

myColors <- c("#51397F", "#3950E5", "#396B7F","#0CF4FC")
df2 %>% 
  filter(ins_date3 %in% c("2020","2019","2018","2017"))%>%
  group_by(ins_date3)%>%
  mutate(ins_date2, number = number)%>% 
  ggplot(data = ., mapping = aes(x = ins_date2, y = number, color = factor(ins_date3))) +
  geom_line() + 
  geom_point()+
  scale_color_manual(values=myColors)+
  labs(title = "Violations over time",
       x = "Week number",
       y = "Number of violations")

#Model 

#Plot: Relationship between cases and violations per zip

df_com_2 %>% 
  filter(l_cases > 0)%>%
  ggplot(data = ., aes(x = l_vio, y = l_cases)) +
  geom_point() +
  geom_smooth(method ="lm")

#Model Summary 

model = df_com_2 %>%
  lm(formula = l_vio ~ l_cases, data = .)

model %>% 
  summary()

#Plot: Residual 

model %>% 
  broom::augment()%>% 
  ggplot(.,aes(x=.resid))+ 
  geom_histogram()