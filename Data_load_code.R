library(tidyverse)
library(haven)


Household_Roster <-read_dta("sec1.dta")
Employment_history <-read_dta("sec4h.dta")





#Land Plot details 
#Rope = 1 | Pole = 2 | Chain = 3  | Acre = 4 | Other = 5
#Acre = 4,840 square yards or 43,560 square feet
#Convert each to feet squared, then to meters 
plot_details <-read_dta("sec8b.dta")

#Convert Plot Sizes to Meters Squared 
plot_details_conv <- plot_details %>%
  rename(plot_size = s8bq4a) %>%
  mutate(mes_converted = case_when(
    s8bq4b == 1 ~ (plot_size * 20)^2 * 0.092903,
    s8bq4b == 2 ~ (plot_size * 16.5)^2 *0.092903,
    s8bq4b == 3 ~ (plot_size * 66)^2 * 0.092903,
    s8bq4b == 4 ~ plot_size * 43560 * 0.092903    
  )) %>%
    relocate(mes_converted, .after = plot_size)
   

#Total Meters Squared per Household
land_summary <- plot_details_conv %>%
  select(nh,mes_converted,clust) %>%
  group_by(clust,nh) %>%
  summarise(mes_converted = sum(mes_converted))


# Data Containing Household Roster Information
Household_Roster <-read_dta("sec1.dta")


#Data Containing Employment data
Employment_history <-read_dta("sec4h.dta")

#Data Containing Educational data
Education <-read_dta("sec2a.dta")

#Expenditure on renting farm land (at level of individual farm)
Land_expense <-read_dta("exp3.dta")  
  


#Agricultural aggregate income 
agr_agg_income2 <-read_dta("agg2.dta")
agr_agg_income1 <-read_dta("agg1.dta")

Job1_inc <-read_dta("inc2.dta")

#House Hold 
# Y Agricultural Profit per HH ~ x =  Avg education per HH + avg age per HH  + area of each HH + other factors 
#explore if higher household education results in lower or higher agricultural income


#Profit per acre - expense

