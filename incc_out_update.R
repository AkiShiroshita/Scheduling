
####Mixed integer programming model####

# Set-up ------------------------------------------------------------------

# involking the necessary libraries
suppressPackageStartupMessages({
library(readr)
library(purrr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(calendR)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(googlesheets4)
})

# Data preparation --------------------------------------------------------

## Demo------

# Preferences
# 1: Shiroshita,
# 2: Suzuki,
# 3: Dodo,
# 4: Fujii(Y),
# 5: Nakai,
# 6, Moriyama,
# 7: Kita,
# 8: Ishida,
# 9: Iwasaka,
# 10: Okano,
# 11: Itahashi,
# 12: Nakano,
# 13: Koide,
# 14: Fujii(M)

## Here set the "target"
target_hms <- ymd_hms("2021-09-01-01:01:01", tz = "Asia/Tokyo")

## Process
target <- as.Date(target_hms, tz = "Asia/Tokyo")
target_m <- month(target)
target_y <- year(target)
target_end <- as.Date(add_with_rollback(target_hms, months(1), roll_to_first = TRUE, preserve_hms = FALSE) - days(1), tz = "Asia/Tokyo")
target_w <- weekdays(target)
target_interval <- as.numeric(target_end - target + 1)

# Creating matrix
Days <- rep(1:target_interval, length = target_interval*15)
times <- rep(target_interval, length = 15)
StaffId <- rep(c(1:15), times = times)
Pref <- rep(1, times = target_interval * 15)
StaffShiftPref <- data.frame(StaffId, Days, Pref)

## Preference
# 1: Shiroshita, Monday, Friday
# 2: Suzuki, Monday, Friday
# 3: Dodo, Wednesday
# 4: Fujii(Y), Friday
# 5: Nakai, Monday, Tuesday, Thursday, Friday
# 6, Moriyama, Monday
# 7: Kita, Monday, Tuesday
# 8: Ishida, Tuesday
# 9: Iwasaka, Wednesday, Thursday
# 10: Okano, Monday, Thursday, Friday
# 11: Itahashi, Monday, Tuesday, Thursday, Friday
# 12: Nakano, Monday, Tuesday, Wednesday, Thursday, Friday
# 13: Koide, Monday, Tuesday, Wednesday, Thursday, Friday
# 14: Fujii(M): Saturday
# 15: Off: Monday

StaffShiftPref
target_w
shift_change <- function(name, number, StaffShiftPref){ # name = 1-14, number = unavailable - first (Monday - Sunday =1), 0-6
  change <- seq(1+number, target_interval, by = 7)
  StaffShiftPref[(target_interval * (name - 1) + change), 3] <- 0
  return(StaffShiftPref)
}

# 1: Shiroshita, Monday, Friday, number:2,5
StaffShiftPref <- shift_change(1, 0, StaffShiftPref)
StaffShiftPref <- shift_change(1, 1, StaffShiftPref)
StaffShiftPref <- shift_change(1, 3, StaffShiftPref)
StaffShiftPref <- shift_change(1, 4, StaffShiftPref)
StaffShiftPref <- shift_change(1, 6, StaffShiftPref)

id = 1
inconv = c(1,2,3,24,27,28,29,30)
StaffShiftPref[target_interval * (id-1) + inconv, 3] <- 0

# 2: Suzuki, Monday, Friday, number: 2,5
StaffShiftPref <- shift_change(2, 0, StaffShiftPref)
StaffShiftPref <- shift_change(2, 1, StaffShiftPref)
StaffShiftPref <- shift_change(2, 3, StaffShiftPref)
StaffShiftPref <- shift_change(2, 4, StaffShiftPref)
StaffShiftPref <- shift_change(2, 6, StaffShiftPref)

id = 2
inconv = c(1,2,3,24,27, 28, 29, 30)
StaffShiftPref[target_interval * (id-1) + inconv, 3] <- 0

# 3: Dodo, Wednesday, number: 0
StaffShiftPref <- shift_change(3, 1, StaffShiftPref)
StaffShiftPref <- shift_change(3, 2, StaffShiftPref)
StaffShiftPref <- shift_change(3, 3, StaffShiftPref)
StaffShiftPref <- shift_change(3, 4, StaffShiftPref)
StaffShiftPref <- shift_change(3, 5, StaffShiftPref)
StaffShiftPref <- shift_change(3, 6, StaffShiftPref)

id = 3
inconv = c(1,15)
StaffShiftPref[target_interval * (id-1) + inconv, 3] <- 0

# 4: Fujii(Y), Friday, number:2
StaffShiftPref <- shift_change(4, 0, StaffShiftPref)
StaffShiftPref <- shift_change(4, 1, StaffShiftPref)
StaffShiftPref <- shift_change(4, 3, StaffShiftPref)
StaffShiftPref <- shift_change(4, 4, StaffShiftPref)
StaffShiftPref <- shift_change(4, 5, StaffShiftPref)
StaffShiftPref <- shift_change(4, 6, StaffShiftPref)

id = 4
inconv = 19
StaffShiftPref[target_interval * (id-1) + inconv, 3] <- 0

# 5: Nakai, Monday, Tuesday, Thursday, Friday, number: 1-7
StaffShiftPref <- shift_change(5, 0, StaffShiftPref)

# 6, Moriyama, Monday, number:5
StaffShiftPref <- shift_change(6, 0, StaffShiftPref)
StaffShiftPref <- shift_change(6, 1, StaffShiftPref)
StaffShiftPref <- shift_change(6, 2, StaffShiftPref)
StaffShiftPref <- shift_change(6, 3, StaffShiftPref)
StaffShiftPref <- shift_change(6, 4, StaffShiftPref)
StaffShiftPref <- shift_change(6, 6, StaffShiftPref)

id = 6
fixed = c(13,27)
StaffShiftPref[target_interval * (id-1) + fixed, 3] <- 2

# 7: Kita, Monday, Tuesday, number: 5,6
StaffShiftPref <- shift_change(7, 0, StaffShiftPref)
StaffShiftPref <- shift_change(7, 1, StaffShiftPref)
StaffShiftPref <- shift_change(7, 2, StaffShiftPref)
StaffShiftPref <- shift_change(7, 3, StaffShiftPref)
StaffShiftPref <- shift_change(7, 4, StaffShiftPref)

# 8: Ishida, Tuesday, number:7
StaffShiftPref <- shift_change(8, 0, StaffShiftPref)
StaffShiftPref <- shift_change(8, 1, StaffShiftPref)
StaffShiftPref <- shift_change(8, 2, StaffShiftPref)
StaffShiftPref <- shift_change(8, 3, StaffShiftPref)
StaffShiftPref <- shift_change(8, 4, StaffShiftPref)
StaffShiftPref <- shift_change(8, 5, StaffShiftPref)

# 9: Iwasaka, Wednesday, Thursday, number:0,1
StaffShiftPref <- shift_change(9, 2, StaffShiftPref)
StaffShiftPref <- shift_change(9, 3, StaffShiftPref)
StaffShiftPref <- shift_change(9, 4, StaffShiftPref)
StaffShiftPref <- shift_change(9, 5, StaffShiftPref)
StaffShiftPref <- shift_change(9, 6, StaffShiftPref)

# 10: Okano, Monday, Thursday, Friday, number: 1,2,5
StaffShiftPref <- shift_change(10, 0, StaffShiftPref)
StaffShiftPref <- shift_change(10, 3, StaffShiftPref)
StaffShiftPref <- shift_change(10, 4, StaffShiftPref)
StaffShiftPref <- shift_change(10, 6, StaffShiftPref)

# 11: Itahashi, Monday, Tuesday, Thursday, Friday, number: 1,2,5,6
StaffShiftPref <- shift_change(11, 2, StaffShiftPref)
StaffShiftPref <- shift_change(11, 3, StaffShiftPref)
StaffShiftPref <- shift_change(11, 4, StaffShiftPref)
StaffShiftPref <- shift_change(11, 5, StaffShiftPref)
StaffShiftPref <- shift_change(11, 6, StaffShiftPref)

id = 11
inconv = c(1,3,6,7,8,13,15,16,20,21,22,24,27,28,29)
StaffShiftPref[target_interval * (id-1) + inconv, 3] <- 0

# 12: Nakano, Monday, Tuesday, Wednesday, Thursday, Friday, number: 0,1,2,5,6
StaffShiftPref <- shift_change(12, 3, StaffShiftPref)
StaffShiftPref <- shift_change(12, 4, StaffShiftPref)

# 13: Koide, Monday, Tuesday, Wednesday, Thursday, Friday, number: 0,1,2,5,6
StaffShiftPref <- shift_change(13, 3, StaffShiftPref)
StaffShiftPref <- shift_change(13, 4, StaffShiftPref)

# 14: Fujii(M): Saturday, number:3
StaffShiftPref <- shift_change(14, 0, StaffShiftPref)
StaffShiftPref <- shift_change(14, 1, StaffShiftPref)
StaffShiftPref <- shift_change(14, 2, StaffShiftPref)
StaffShiftPref <- shift_change(14, 4, StaffShiftPref)
StaffShiftPref <- shift_change(14, 5, StaffShiftPref)
StaffShiftPref <- shift_change(14, 6, StaffShiftPref)

# 15: Off: Sunday, number:4
StaffShiftPref <- shift_change(15, 0, StaffShiftPref)
StaffShiftPref <- shift_change(15, 1, StaffShiftPref)
StaffShiftPref <- shift_change(15, 2, StaffShiftPref)
StaffShiftPref <- shift_change(15, 3, StaffShiftPref)
StaffShiftPref <- shift_change(15, 5, StaffShiftPref)
StaffShiftPref <- shift_change(15, 6, StaffShiftPref)

id = 15
fixed = c(20,23)
StaffShiftPref[target_interval * (id-1) + fixed, 3] <- 2

# Adjustment
id = 1
inconv = c(6)
StaffShiftPref[target_interval * (id-1) + inconv, 3] <- 0

id = 8
inconv = c(29)
StaffShiftPref[target_interval * (id-1) + inconv, 3] <- 0
## from google spreadsheet-----
# sun_start = function(date){
#   date_fm = as.Date(date,format="%Y-%m-%d")+1
#   
#   week_W2 = as.numeric(
#    strftime(date_fm,"%W")
#  )
#  
#  # If the first day is week 00, add 1
#  week_W2 = ifelse(
#    strftime(as.Date(paste(lubridate::year(date_fm),"01","01",sep = "-"))+1,"%W") == "00",
#    week_W2 + 1,
#    week_W2
#   ) 
#   
#   # return result
#   week_W2
# }
# 
# sun_start(target)  

#gs4_auth()
#sheet_id <- "https://docs.google.com/spreadsheets/d/18UV22-0k5sbWOUQdRWU-E1vRW7R3xLcpp3lsFICg6yM/edit#gid=0"
#submitted <- read_sheet(sheet_id, sheet = 1)
#submitted <- submitted %>% 
#  mutate(across(date1:date3, ~ ymd(.x))) %>% 
#  mutate(number1 = as.numeric(date1 - target) %% 7) %>% 
#  mutate(weekday1 = weekdays(date1)) %>% 
#  mutate(number2 = as.numeric(date2 - target) %% 7) %>% 
#  mutate(weekday2 = weekdays(date2)) %>% 
#  mutate(number3 = as.numeric(date3 - target) %% 7) %>% 
#  mutate(weekday3 = weekdays(date3))

# September



# Optimization ------------------------------------------------------------

## Our objective is to build a schedule with just the optimal number of staff for each schedule 
## Constraint
### 1. Each day, one staff
### 2. Fixed number of outpatients

## function to check if staff is available for a given day and shift----
checkPref <- function(staff, day)
{
  staffShiftSubset <- StaffShiftPref[StaffShiftPref$StaffId == staff & StaffShiftPref$Days == day,]
  staffShiftDaySubset <- staffShiftSubset[which(!names(staffShiftSubset) %in% c("StaffId", "Days"))]
  isAvail <- staffShiftDaySubset[,"Pref"]
  #to ensure that non-preferred shifts are given least preference, place a high penalty. If needed this step could be made a constraint.
  isPref <- case_when(isAvail == 0 ~  -10000,
                      isAvail == 2 ~ +10000,
                      isAvail == 1 ~ 1)
  return(isPref)
}

## set the number of rows(staff) and columns(weekday) for the matrix----
numOfStaff <- length(unique(StaffShiftPref$StaffId))
numOfDays <- target_interval

## build integer programming model----
model <- MIPModel() %>%
  add_variable(x[i,j], i = 1:numOfStaff, j = 1:numOfDays, type = "binary") %>%
  # optimize the number of staff based on availability and fixed dates
  set_objective(sum_expr(checkPref(staff = i, day = j) * x[i, j],
                         i = 1:numOfStaff,
                         j = 1:numOfDays),
                sense = "max") %>%
  
  # each day must have 1 staff
  add_constraint(sum_expr(x[i,j], i = 1:numOfStaff) == 1, j =  1:numOfDays) %>%
  
  # shiroshita
  add_constraint(sum_expr(x[1,j], j = 1:numOfDays) == 2) %>% 
  
  # suzuki
  add_constraint(sum_expr(x[2,j], j = 1:numOfDays) == 2) %>%
  
  # dodo
  add_constraint(sum_expr(x[3,j], j = 1:numOfDays) == 1) %>%
  
  # fujii(y)
  add_constraint(sum_expr(x[4,j], j = 1:numOfDays) == 1) %>%
  
  # nakai
  add_constraint(sum_expr(x[5,j], j = 1:numOfDays) == 1) %>%
  
  # moriyama
  add_constraint(sum_expr(x[6,j], j = 1:numOfDays) == 1) %>%
  
  # kita
  add_constraint(sum_expr(x[7,j], j = 1:numOfDays) == 1) %>%
  #add_constraint(sum_expr(x[7,j], j = 1:numOfDays) <= 2) %>%
  
  # ishida
  add_constraint(sum_expr(x[8,j], j = 1:numOfDays) >= 1) %>%
  add_constraint(sum_expr(x[8,j], j = 1:numOfDays) <= 2) %>%
  
  # isawaka
  add_constraint(sum_expr(x[9,j], j = 1:numOfDays) >= 1) %>%
  add_constraint(sum_expr(x[9,j], j = 1:numOfDays) <= 2) %>%
  
  # okano
  add_constraint(sum_expr(x[10,j], j = 1:numOfDays) == 1) %>%
  #add_constraint(sum_expr(x[10,j], j = 1:numOfDays) <= 2) %>%
  
  # itabashi
  add_constraint(sum_expr(x[11,j], j = 1:numOfDays) >= 1) %>%
  add_constraint(sum_expr(x[11,j], j = 1:numOfDays) <= 2) %>%
  
  # nakano
  add_constraint(sum_expr(x[12,j], j = 1:numOfDays) >= 1) %>%
  add_constraint(sum_expr(x[12,j], j = 1:numOfDays) <= 2) %>%
  
  # koide
  add_constraint(sum_expr(x[13,j], j = 1:numOfDays) >= 1) %>%
  add_constraint(sum_expr(x[13,j], j = 1:numOfDays) <= 2) %>%
  
  # fujii(m)
  add_constraint(sum_expr(x[14,j], j = 1:numOfDays) == 4) %>% 
  
  # Off
  add_constraint(sum_expr(x[15,j], j = 1:numOfDays) == 6)

## inspect model
model

## solve integer programming model----
result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

## for display----

roster <- result %>% 
  get_solution(x[i,j])
roster <- roster[,c("i", "j","value")]
colnames(roster) <- c("staff", "day" , "rostered") 
roster <- roster %>% 
  select(day, rostered, staff) %>%
  filter(rostered == 1) %>% 
  arrange(day, rostered) %>% 
  select(-rostered) 
roster
#roster %>% write.csv("roster.csv")

# Calendar
calendar <- roster %>% 
  mutate(staff = case_when(staff == 1 ~ "城下",
                           staff == 2 ~ "鈴木",
                           staff == 3 ~ "百々",
                           staff == 4 ~ "藤井（将）",
                           staff == 5 ~ "中井",
                           staff == 6 ~  "森山",
                           staff == 7 ~  "喜多",
                           staff == 8 ~  "石田",
                           staff == 9 ~  "岩坂",
                           staff == 10 ~  "岡野",
                           staff == 11 ~ "板橋",
                           staff == 12 ~ "中野",
                           staff == 13 ~ "小出",
                           staff == 14 ~ "藤井（真）",
                           staff == 15 ~ "休み"))
num <- as.numeric(target_end - target) + 1
calendR(year = target_y,
        month = target_m,
        title = "INCC外来9月",
        text = calendar$staff,
        text.pos = 1:num)

