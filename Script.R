install.packages("tidyverse")
install.packages("readODS")
install.packages("patchwork")
library(tidyverse)
library(readODS)
library(dplyr)
library(ggplot2)
library(stringr)
library(scales)
library(patchwork)
library(tibble)

#----------------------------------------------------------------------------------------------------------------------------------

# 1st Question:
# To what extent has the COVID-19 pandemic fundamentally altered passenger demand?

# First lets check the dataset of passenger usage

pass_journey_by_oper_annual <- read_ods("Passenger-journeys-by-operator-annual-structured.ods")
pass_journey_by_oper_quarterly <- read_ods("Passenger-journeys-by-operator-quarterly-structured.ods")

# replacing all [z] values with NA (those were values that data was not taken for whatever reason)

pass_journey_by_oper_annual[pass_journey_by_oper_annual=="[z]"]<-NA
pass_journey_by_oper_quarterly[pass_journey_by_oper_quarterly=="[z]"]<-NA


# changing the time period to years

pass_journ_oper_annual_str<-pass_journey_by_oper_annual %>%
  mutate(`Time period`= c("2011","2012","2013","2014","2015","2016",
                          "2017","2018","2019","2020","2021","2022","2023","2024"))


# Adding an average column for totals

pass_journ_oper_annual_str<-pass_journ_oper_annual_str %>%
  mutate(AverageTotal = `Total
(million)
[note 1]`/25)

# selecting our 5 chosen train operators
famous_railways_annual<-pass_journ_oper_annual_str %>%
  select(`Time period`,`Avanti West Coast
(million)`, `CrossCountry
(million)`, `East Midlands Railway
(million)`, `Northern Trains
(million)`, `TransPennine Express
(million)`)

#change the format of the table to long format so that we can plot

railways_long <- famous_railways_annual %>%
  pivot_longer(
    cols = -`Time period`, 
    names_to = "RailwayOperator",
    values_to = "Kilometers (million)")

# changing names to remove the "(million)" (it annoys me)

railways_long<-railways_long %>%
  mutate(RailwayOperator=str_remove(RailwayOperator, fixed("(million)")))


#plotting
railways_long %>%
  ggplot(aes(`Time period`, `Kilometers (million)`, colour = RailwayOperator, group = RailwayOperator)) +
  geom_point() +
  geom_line(linewidth=2.5, alpha=0.5) +
  labs(x="Time Period", y="Kilometers in millions", title = "Kilometers travelled by the five chosen train
       operators during the years") +
  theme_minimal()

#adding more data


# Number of passenger journeys for England, Scotland and Wales

pass_journ_eng_sc_wls <- read_ods("Number of passenger journeys for England, Scotland and Wales.ods")

# changing the time period column


pass_journ_eng_sc_wls<-pass_journ_eng_sc_wls %>%
  mutate(`Time period`= c("1995","1996","1997","1998","1999","2000",
                          "2001","2002","2003","2004","2005","2006","2007","2008", "2009", "2010", "2011",
                          "2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023"))

# selecting dates from 2015 instead of 1995 (too old)

pass_journ_eng_sc_wls_dates<-pass_journ_eng_sc_wls %>%
  filter(`Time period`>2014)


#making it long again

pass_journ_eng_sc_wls_long <- pass_journ_eng_sc_wls_dates %>%
  pivot_longer(
    cols = -`Time period`,
    names_to = "Journeys",
    values_to = "No of People (in thousands)")

# changing names to remove the "(thousands)" (it annoys me-again)

pass_journ_eng_sc_wls_long<-pass_journ_eng_sc_wls_long %>%
  mutate(Journeys=str_remove(Journeys, fixed("(thousands)")))

#Plotting timeee

pass_journ_eng_sc_wls_long %>%
  mutate(Journeys = trimws(Journeys)) %>% 
  filter(Journeys != "Total journeys") %>%
  ggplot(aes(`Time period`, `No of People (in thousands)`, colour = Journeys, group = Journeys)) +
  geom_point() +
  geom_line(linewidth=2.5, alpha=0.5) +
  labs(x="Time Period", y="No of People (in thousands)", 
       title = "Number of passenger journeys for England, Scotland and Wales") +
  theme_minimal()

#-------------------------------------------------------------------------------------------------------------------------------------------------------

# 2nd Question:

# How has the real cost of travel evolved across different operators and regions?

# first we load the dataset of change in fares

change_in_fares <- read_ods("Average change in fares by ticket type, Great Britain, annual data, 2004 to 2025.ods")

# replacing all [z] and [x] values with NA 

change_in_fares[change_in_fares=="[z]"]<-NA
change_in_fares[change_in_fares=="[x]"]<-NA

# removing "other' and "revenue per journey" because too many NAs

cleaned_fares_data <- change_in_fares %>%
  filter(`Ticket type` != "Other" & `Ticket type` != "Revenue per journey" & `Ticket type` !="Revenue per journey [r]")

#reshape the data

fares_long <- cleaned_fares_data %>%
  mutate(across( matches("^[0-9]{4}$"), as.character)) %>%
  
  pivot_longer(
    cols = matches("^[0-9]{4}$"),
    names_to = "Year",
    values_to = "FareChange"
  ) %>%
  mutate(
    Year = as.numeric(Year),           
    FareChange = as.numeric(FareChange) 
  )


  
# plot only to see one ticket type and all regions
  
  fares_long %>% filter(`Ticket type` == "All tickets") %>%
    
    ggplot(aes(x = Year, y = FareChange, color = Sector, group = Sector)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(
      title = "Fare Changes by Sector (All tickets)",
      x = "Year",
      y = "Fare Change (Index, 2004=100)",
      color = "Sector") +
    theme_minimal()
  

#-------------------------------------------------------------------------------------------------------------------------------------------------------

# 3rd Question:

# How has rail service reliability in the UK changed over the years?

# load the data first
  
train_canc <- read_ods("Trains planned and cancellations by operator and cause, Great Britain, quarterly data, April 2019 to June 2025.ods")
  

# Selecting specific operators

my_operators <- c('Avanti West Coast', 
                  'CrossCountry', 
                  'East Midlands Railway', 
                  'Northern Trains', 
                  'TransPennine Express')


operators_train_canc <- train_canc %>%
  filter(`National or Operator` %in% my_operators)

# Creating new Year variable as numeric

operators_train_canc <- operators_train_canc %>%
  mutate(
    Year = as.numeric(stringr::str_extract(`Time period`, "\\d{4}"))
  )

# putting it first

operators_train_canc <- operators_train_canc %>%
  select(Year, everything())

# Selecting only some columns

trains_arrv_canc <- operators_train_canc %>%
  select(`Year`, `National or Operator`, `Number of trains planned`, 
         `Cancellations`)


# creating new percentage column

trains_arrv_canc <- trains_arrv_canc %>%
  mutate(
    Percentage_Cancelled = (`Cancellations` / `Number of trains planned`) * 100
  )


# Calculating avrg for each year
avg_cancellations_per_year <- trains_arrv_canc %>%
  
  # 1. Group only by the Year
  group_by(Year, `National or Operator`) %>%
  
  # 2. Calculate the average for your new column
  summarize(
    Avg_Percentage_Cancelled = mean(Percentage_Cancelled, na.rm = TRUE)
  ) %>%
  
  ungroup()


# bar chart

ggplot(data = avg_cancellations_per_year, 
       aes(x = factor(Year),
           y = Avg_Percentage_Cancelled)) +
  geom_col(fill = "#0072B2") + 
  labs(
    title = "Average Train Cancellations per Year (All Operators)",
    x = "Year",
    y = "Average Cancellation Percentage"
  ) +
  theme_bw()


# facetted years for 2019 and 2023 to compare


data_comparison <- trains_arrv_canc %>%
  filter(Year %in% c(2019, 2023)) 

ggplot(data = data_comparison, 
       aes(x = `National or Operator`, 
           y = Percentage_Cancelled,
           fill = `National or Operator`)) + 
  
  geom_col() +
  facet_wrap(~ Year) + 
  scale_fill_viridis_d(option = "plasma") +
  labs(
    title = "Train Cancellations by Operator: 2019 vs 2023",
    x = "Train Operator",
    y = "Cancellation Percentage"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold") 
  )

#----------------------------------------------------------------------------------------------------------------------------------------------------

# to check for correlations we need to make a master dataset to connect all of them

# connect avg_cancellations_per_year, fix the fares_long one and make it fares_main, pass_journ_eng_sc_wls_long
# and railways_long

view(avg_cancellations_per_year)
view(fares_long)
view(pass_journ_eng_sc_wls_long)
view(railways_long)

# cleaning fares_long

fares_main <- fares_long %>%
  select(`Year`, `FareChange`, `Sector`, 
         `Ticket type`)


operator_map <- tibble(
  operator_name = c("Avanti West Coast", 
                    "CrossCountry", 
                    "East Midlands Railway", 
                    "Northern Trains", 
                    "TransPennine Express"),
  sector = c("Long distance",  
             "Long distance", 
             "Regional", 
             "Regional", 
             "Regional")
) %>%
  mutate(
    operator_name = str_squish(as.character(operator_name)), 
    sector = str_squish(as.character(sector))
  )


# clean datasets


# Clean operator demand data

demand_clean <- railways_long %>%
  rename(
    obs_year = `Time period`, 
    operator_name = RailwayOperator,
    kilometers_m = `Kilometers (million)`) %>%
  mutate(obs_year = as.numeric(obs_year)) %>%
  mutate(
    operator_name = as.character(operator_name),
    operator_name = str_replace_all(operator_name, "\\n", ""),
    operator_name = str_squish(operator_name))

# Clean Cancellations data
cancellations_clean <- avg_cancellations_per_year %>%
  rename(
    obs_year = Year,
    operator_name = `National or Operator`,
    cancellation_rate = Avg_Percentage_Cancelled)%>%
  mutate(obs_year = as.numeric(obs_year)) %>%
  mutate(
    operator_name = as.character(operator_name),
    operator_name = str_replace_all(operator_name, "\\n", ""), 
    operator_name = str_squish(operator_name))

# Clean fares data
fares_clean <- fares_main %>%
  filter(`Ticket type` == "All tickets") %>% 
  rename(
    obs_year = Year,
    fare_index = FareChange,
    sector = Sector
  )%>%
  mutate(obs_year = as.numeric(obs_year)) %>%
  mutate(
    sector = as.character(sector), 
    sector = str_squish(sector)     
  )

view(demand_clean)
view(cancellations_clean)
view(fares_clean)
names(demand_clean)
names(cancellations_clean)
names(fares_clean)

# Joining everything into one dataset


# Join demand and cancellations
master_data <- left_join(
  demand_clean, 
  cancellations_clean, 
  by = c("operator_name", "obs_year")
)

# Add the 'Sector' column and clean it
master_data <- left_join(
  master_data,
  operator_map,
  by = "operator_name"
) %>%
  mutate(
    sector = as.character(sector), 
    sector = str_squish(sector)     
  )

# Join the fares data
master_data <- left_join(
  master_data,
  fares_clean,
  by = c("obs_year", "sector")
)

View(master_data)

# Doing correlation test

#Get pre pandemic 2019 baseline
baseline_data <- master_data %>%
  filter(obs_year == 2019) %>%
  group_by(operator_name) %>%
  summarise(baseline_km = mean(kilometers_m, na.rm = TRUE))

#Get post pandemic 2023 recovery data
recovery_data <- master_data %>%
  filter(obs_year == 2023) %>%
  group_by(operator_name) %>%
  summarise(
    recovery_km = mean(kilometers_m, na.rm = TRUE),
    avg_cancellations = mean(cancellation_rate, na.rm = TRUE)
  )


operator_summary <- left_join(baseline_data, recovery_data, by = "operator_name")

#Calculate the recovery ratio
operator_summary <- operator_summary %>%
  mutate(demand_recovery_ratio = recovery_km / baseline_km)


# Plot

summary_with_sector <- left_join(
  operator_summary, 
  operator_map, 
  by = "operator_name"
)

ggplot(summary_with_sector, aes(x = avg_cancellations, y = demand_recovery_ratio)) +
  
  geom_point(aes(color = sector), size = 5) +
  
  geom_text(aes(label = operator_name), vjust = -1, size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 1.0, linetype = "dotted", color = "blue") +
  labs(
    title = "Poorer Reliability is Linked to Slower Passenger Recovery",
    subtitle = "Color-coded by train sector",
    x = "Average Cancellation Rate (2023)",
    y = "Demand Recovery Ratio (2023 km / 2019 km)",
    color = "Sector" 
  ) +
  theme_minimal()

# regression

master_data_final <- master_data %>%
  mutate(Is_Post_COVID = ifelse(obs_year >= 2020, 1, 0))

model_with_sector <- lm(
  kilometers_m ~ cancellation_rate + fare_index + Is_Post_COVID + sector, 
  data = master_data_final
)

# RESULTS
summary(model_with_sector)
