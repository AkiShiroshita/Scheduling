
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
# 3: Takeshita

## Here set the parameters 
target_hms <- ymd_hms("2021-09-01-01:01:01", tz = "Asia/Tokyo")

## Process
target <- as.Date(target_hms, tz = "Asia/Tokyo")
target_m <- month(target)
target_y <- year(target)
target_end <- as.Date(add_with_rollback(target_hms, months(1), roll_to_first = TRUE, preserve_hms = FALSE) - days(1), tz = "Asia/Tokyo")
target_w <- weekdays(target)
target_interval <- as.numeric(target_end - target + 1)

# Creating matrix
Days <- rep(1:target_interval, length = target_interval*3)
times <- rep(target_interval, length = 3)
StaffId <- rep(c(1:3), times = times)
Pref <- rep(1, times = target_interval * 3)
StaffShiftPref <- data.frame(StaffId, Days, Pref)

## Preference
# 1: Shiroshita,
# 2: Suzuki,
# 3: Takeshita,

StaffShiftPref
target_w
shift_change <- function(name, number, StaffShiftPref){ # name = 1-14, number = unavailable - first (Monday - Sunday =1), 0-6
  change <- seq(1+number, target_interval, by = 7)
  StaffShiftPref[(target_interval * (name - 1) + change), 3] <- 0
  return(StaffShiftPref)
}

# 1: Shiroshita
StaffShiftPref <- shift_change(1, 1, StaffShiftPref)
id = 1
inconv1 <- c(1,2,3,4,5,8,11,12,19,23)
StaffShiftPref[target_interval * (id-1) + inconv1, 3] <- 0
fixed1 <- c(10,26)
StaffShiftPref[target_interval * (id-1) + fixed1, 3] <- 2

# 2: Suzuki
id = 2
inconv2 <- c(25,26,27,28,29,30)
StaffShiftPref[target_interval * (id-1) + inconv2, 3] <- 0

# 3: Takeshita
StaffShiftPref <- shift_change(3, 1, StaffShiftPref)
StaffShiftPref <- shift_change(3, 2, StaffShiftPref)
StaffShiftPref <- shift_change(3, 3, StaffShiftPref)
#StaffShiftPref <- shift_change(3, 4, StaffShiftPref)
StaffShiftPref <- shift_change(3, 5, StaffShiftPref)
StaffShiftPref <- shift_change(3, 6, StaffShiftPref)
id = 3
inconv3 <- c(20,25)
StaffShiftPref[target_interval * (id-1) + inconv3, 3] <- 0
id = 3
fixed3 <- c(1,8,15,22,29)
StaffShiftPref[target_interval * (id-1) + fixed3, 3] <- 2

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
  add_constraint(sum_expr(x[1,j], j = 1:numOfDays) == 12) %>% 
  
  # suzuki
  add_constraint(sum_expr(x[2,j], j = 1:numOfDays) == 12) %>%
  
  # takeshita
  add_constraint(sum_expr(x[3,j], j = 1:numOfDays) == 6)

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
                           staff == 3 ~ "竹下"))
num <- as.numeric(target_end - target) + 1
events <- rep(NA, target_interval)
inconv_dual12 <- union(intersect(inconv1, inconv2), intersect(inconv2, inconv3))
inconv_dual123 <- union(intersect(inconv1, inconv3), inconv_dual12)
inconv1_unique <- setdiff(inconv1, inconv_dual123)
events[inconv1_unique] <- "shiroshita"
inconv2_unique <- setdiff(inconv2, inconv_dual123)
events[inconv2] <- "Suzuki"
inconv3_unique <- setdiff(inconv3, inconv_dual123)
events[inconv3] <- "Takeshita"
events[inconv_dual123] <- "Double"
calendR(year = target_y,
        month = target_m,
        title = "INCCオンコール9月",
        subtitle = "青: 城下対応不可、オレンジ：竹下対応不可、緑：鈴木対応不可、赤：他2人とも対応不可",
        text = calendar$staff,
        special.days = events,
        special.col = c("red","lightblue","lightgreen", "lightsalmon"),
        text.pos = 1:num)
#calendR(year = target_y,
#        month = target_m,
#        title = "INCCオンコール9月",
#        subtitle = "青: 城下対応不可、オレンジ：竹下対応不可、緑：鈴木対応不可、赤：他2人とも対応不可",
#        text = calendar$staff,
#        special.days = events,
#        special.col = c("red","lightblue","lightgreen", "lightsalmon"),
#        text.pos = 1:num,
#        orientation = "portrait",
#        pdf = TRUE,
#        doc_name = "INCCオンコール9月")

# for google calendar
roster <- roster %>%
  mutate("Start Date" = ymd(2021-9-1) + day)
colnames(roster) <- c("staff", "day" , "rostered") 