
####Mixed integer programming model####

# Set-up ------------------------------------------------------------------

# involking the necessary libraries
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


# Data preparation --------------------------------------------------------

## Demo------

# Preferences
# 1: shiroshita,
# 2: suzuki,
# 3: fujii,
# 4: nakai,
# 5: nakano,
# 6, itabashi,
# 7: koide,
# 8: ishida,
# 9: okano,
# 10: kita,
# 11: iwasaka,
# 12: dodo,
# 13: moriyama
# Saturday: "Fujii"

StaffShiftPref <- data.frame(
  "StaffId" = c(1,1,1,1,
                2,2,2,2,
                3,3,3,3,
                4,4,4,4,
                5,5,5,5,
                6,6,6,6,
                7,7,7,7,
                8,8,8,8,
                9,9,9,9,
                10,10,10,10,
                11,11,11,11,
                12,12,12,12,
                13,13,13,13),
  
  "Week" = c(1,2,3,4,
             1,2,3,4,
             1,2,3,4,
             1,2,3,4,
             1,2,3,4,
             1,2,3,4,
             1,2,3,4,
             1,2,3,4,
             1,2,3,4,
             1,2,3,4,
             1,2,3,4,
             1,2,3,4,
             1,2,3,4),
  
  "Monday" = c(1,1,1,1, # shiroshita
               0,0,0,0, # suzuki
               0,0,0,0, # fujii
               0,0,0,0, # nakai
               1,1,1,1, # nakano
               1,1,1,1, # itabashi
               1,1,1,1, # koide
               0,0,0,0, # ishida
               1,1,1,1, # okano
               0,0,0,0, # kita
               1,1,1,1, # iwasaka
               0,0,0,0, # dodo
               1,1,1,1), # moriyama
  
  "Tuesday" = c(0,0,0,0, # shiroshita
                0,0,0,0, # suzuki
                0,0,0,0, # fujii
                0,0,0,0, # nakai
                1,1,1,1, # nakano
                1,1,1,1, # itabashi
                1,1,1,1, # koide
                1,1,1,1, # ishida
                1,1,1,1, # okano
                1,1,1,1, # kita
                1,1,1,1, # iwasaka
                0,0,0,0, # dodo
                1,1,1,1), # moriyama
  
  "Wednesday" = c(0,0,0,0, # shiroshita
                  0,0,0,0, # suzuki
                  0,0,0,0, # fujii
                  0,0,0,0, # nakai
                  1,1,1,1, # nakano
                  0,0,0,0, # itabashi
                  1,1,1,1, # koide
                  0,0,0,0, # ishida
                  1,1,1,1, # okano
                  0,0,0,0, # kita
                  1,1,1,1, # iwasaka
                  1,1,1,1, # dodo
                  1,1,1,1), # moriyama
  
  "Thursday" = c(0,0,0,0, #shiroshita
                 0,0,0,0, # suzuki
                 0,0,0,0, # fujii
                 1,1,1,1, # nakai
                 1,1,1,1, # nakano
                 1,1,1,1, # itabashi
                 1,1,1,1, # koide
                 0,0,0,0, # ishida
                 1,1,1,1, # okano 
                 0,0,0,0, # kita
                 1,1,1,1, # iwasaka
                 0,0,0,0, # dodo
                 1,1,1,1), # moriyama
  "Friday" = c(0,0,0,0, # shiroshita
               1,1,1,1, # suzuki
               0,0,0,0, # fujii
               1,1,1,1, # nakai
               1,1,1,1, # nakano
               1,1,1,1, # itabashi
               0,0,0,0, # koide
               1,1,1,1, # ishida
               0,0,0,0, # okano
               0,0,0,0, # kita
               0,0,0,0, # iwasaka
               1,1,1,1, # dodo
               1,1,1,1) # moriyama
)
StaffShiftPref
x <- 2
y <- 2
StaffShiftPref[4*(x-1)+1,1+y] <- 0

## from google spreadsheet-----
# set "target"
target_hms <- ymd_hms("2021-08-01-01:01:01", tz = "Asia/Tokyo")
target <- as.Date(target_hms, tz = "Asia/Tokyo")
target_m <- month(target)
target_y <- year(target)
target_end <- as.Date(add_with_rollback(target_hms, months(1), roll_to_first = TRUE, preserve_hms = FALSE) - days(1), tz = "Asia/Tokyo")
target_w <- weekdays(target)
target_interval <- as.numeric(target_end - target)
number_week <- target_interval %/% 7  

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

gs4_auth()
sheet_id <- "https://docs.google.com/spreadsheets/d/1uLBJ-0l9g-1EpC8tiPHjBY9F64qtYXK-b48XdC6UpHw/edit#gid=0"
submitted <- read_sheet(sheet_id, sheet = 1)
submitted <- submitted %>% 
  filter(secret == "takeshita") %>% 
  filter(date1 >= target) %>% 
  mutate(across(date1:date3, ~ ymd(.x))) %>% 
  mutate(number1 = as.numeric(date1 - target)%/%7) %>% 
  mutate(weekday1 = weekdays(date1)) %>% 
  mutate(number2 = as.numeric(date2 - target)%/%7) %>% 
  mutate(weekday2 = weekdays(date2)) %>% 
  mutate(number3 = as.numeric(date3 - target)%/%7) %>% 
  mutate(weekday3 = weekdays(date3))

# Optimization ------------------------------------------------------------

## Our objective is to build a schedule with just the optimal number of staff for each schedule 
## Constraint
### 1. Each day, one staff
### 2. Fixed number of outpatients
# 1: shiroshita, 2
# 2: suzuki, 2
# 3: fujii, 1
# 4: nakai, 1
# 5: nakano, 3
# 6, itabashi, 3
# 7: koide, 2
# 8: ishida, 2
# 9: okano, 2
# 10: kita, 2
# 11: iwasaka, 2
# 12: dodo, 1
# 13: moriyama, 1

## function to check if staff is available for a given day and shift----
checkPref <- function(staff, day, week)
{
  staffShiftSubset <- StaffShiftPref[StaffShiftPref$StaffId == staff & StaffShiftPref$Week == week,]
  staffShiftDaySubset <- staffShiftSubset[which(!names(staffShiftSubset) %in% c("StaffId", "Week"))]
  staffShiftDaySubset <- staffShiftDaySubset[,day]
  isAvail <- staffShiftDaySubset
  #to ensure that non-preferred shifts are given least preference, place a high penalty. If needed this step could be made a constraint.
  isPref <- ifelse(isAvail == 0, -10000, isAvail)
  return(isPref)
}

## set the number of rows(staff) and columns(weekday) for the matrix----
numOfStaff <- length(unique(StaffShiftPref$StaffId))
numOfDays <- 5
numOfWeeks <- length(unique(StaffShiftPref$Week))

## build integer programming model----
model <- MIPModel() %>%
  add_variable(x[i,j,k], i = 1:numOfStaff, j = 1:numOfDays, k = 1:numOfWeeks, type = "binary") %>%
  # optimize the number of staff based on availability and fixed dates
  set_objective(sum_expr(checkPref(staff = i, day = j, week = k) * x[i, j, k],
                         i = 1:numOfStaff,
                         j = 1:numOfDays,
                         k = 1:numOfWeeks),
                sense = "max") %>%
  
  # each day must have at least 1 staff
  add_constraint(sum_expr(x[i,j,k], i = 1:numOfStaff) >= 1, j =  1:numOfDays, k = 1:numOfWeeks) %>%
  
  # each day must have maximum 1 staff
  add_constraint(sum_expr(x[i,j,k], i = 1:numOfStaff) <= 1, j =  1:numOfDays, k = 1:numOfWeeks) %>% 
  
  # shiroshita
  add_constraint(sum_expr(x[1,j,k], k = 1:numOfWeeks, j = 1:numOfDays) == 2) %>% 
  
  # suzuki
  add_constraint(sum_expr(x[2,j,k], k = 1:numOfWeeks, j = 1:numOfDays) == 2) %>%
  
  # fujii
  add_constraint(sum_expr(x[3,j,k], k = 1:numOfWeeks, j = 1:numOfDays) <= 2) %>%
  
  add_constraint(sum_expr(x[3,j,k], k = 1:numOfWeeks, j = 1:numOfDays) >= 1) %>%
  
  # nakai
  add_constraint(sum_expr(x[4,j,k], k = 1:numOfWeeks, j = 1:numOfDays) <= 2) %>%
  
  add_constraint(sum_expr(x[4,j,k], k = 1:numOfWeeks, j = 1:numOfDays) >= 1) %>%
  
  # nakano
  add_constraint(sum_expr(x[5,j,k], k = 1:numOfWeeks, j = 1:numOfDays) <= 3) %>%
  
  add_constraint(sum_expr(x[5,j,k], k = 1:numOfWeeks, j = 1:numOfDays) >= 1) %>%
  
  # itabashi
  add_constraint(sum_expr(x[6,j,k], k = 1:numOfWeeks, j = 1:numOfDays) <= 3) %>%
  
  add_constraint(sum_expr(x[6,j,k], k = 1:numOfWeeks, j = 1:numOfDays) >= 1) %>%
  
  # koide
  add_constraint(sum_expr(x[7,j,k], k = 1:numOfWeeks, j = 1:numOfDays) <= 3) %>%
  
  add_constraint(sum_expr(x[7,j,k], k = 1:numOfWeeks, j = 1:numOfDays) >= 1) %>%
  
  # ishida
  add_constraint(sum_expr(x[8,j,k], k = 1:numOfWeeks, j = 1:numOfDays) <= 2) %>%
  
  add_constraint(sum_expr(x[8,j,k], k = 1:numOfWeeks, j = 1:numOfDays) >= 1) %>%
  
  # okano
  add_constraint(sum_expr(x[9,j,k], k = 1:numOfWeeks, j = 1:numOfDays) <= 2) %>%
  
  add_constraint(sum_expr(x[9,j,k], k = 1:numOfWeeks, j = 1:numOfDays) >= 1) %>%
  
  # kita
  add_constraint(sum_expr(x[10,j,k], k = 1:numOfWeeks, j = 1:numOfDays) <= 2) %>%
  
  add_constraint(sum_expr(x[10,j,k], k = 1:numOfWeeks, j = 1:numOfDays) >= 1) %>%
  
  # iwasaka
  add_constraint(sum_expr(x[11,j,k], k = 1:numOfWeeks, j = 1:numOfDays) <= 2) %>%
  
  add_constraint(sum_expr(x[11,j,k], k = 1:numOfWeeks, j = 1:numOfDays) >= 1) %>%
  
  # dodo
  add_constraint(sum_expr(x[12,j,k], k = 1:numOfWeeks, j = 1:numOfDays) <= 2) %>%
  
  add_constraint(sum_expr(x[12,j,k], k = 1:numOfWeeks, j = 1:numOfDays) >= 1) %>%
  
  # moriyama
  add_constraint(sum_expr(x[13,j,k], k = 1:numOfWeeks, j = 1:numOfDays) <= 2) %>% 
  
  add_constraint(sum_expr(x[13,j,k], k = 1:numOfWeeks, j = 1:numOfDays) >= 1) 

## inspect model
model

## solve integer programming model----
result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

## for display----

roster <- result %>% 
  get_solution(x[i,j,k])
roster <- roster[,c("i", "j", "k","value")]
colnames(roster) <- c("staff", "day" , "week" , "rostered") 
roster <- roster %>% 
  select(week, day, rostered, staff) %>%
  filter(rostered == 1) %>% 
  arrange(week, day, rostered) %>% 
  select(-rostered) 
roster
roster %>% write.csv("roster.csv")

# Calendar
calendar <- roster %>% 
  mutate(staff = case_when(staff == 1 ~ "城下",
                           staff == 2 ~ "鈴木",
                           staff == 3 ~ "藤井",
                           staff == 4 ~ "中井",
                           staff == 5 ~ "中野",
                           staff == 6 ~  "板橋",
                           staff == 7 ~  "小出",
                           staff == 8 ~  "石田",
                           staff == 9 ~  "岡野",
                           staff == 10 ~  "北",
                           staff == 11 ~ "岩坂",
                           staff == 12 ~ "百々",
                           staff == 13 ~ "森山"))
# 3: fujii,
# 4: nakai,
# 5: nakano,
# 6, itabashi,
# 7: koide,
# 8: ishida,
# 9: okano,
# 10: kita,
# 11: iwasaka,
# 12: dodo,
# 13: moriyama
num <- as.numeric(target_end - target) + 1
calendR(year = target_y,
        month = target_m,
        text = calendar$staff,
        text.pos = 1:num)
