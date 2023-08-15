### proxy to access internet while using City of Cape Town (CoCT) sandbox, replace with Personal Username and Password
Sys.setenv(http_proxy='http://USERNAME:PASSWORD@internet.capetown.gov.za:8080') 
# note: username and password are removed for privacy purposes

start_time <- Sys.time()

# imported libraries to use
library(tidyverse)
library(readxl)
library(httr)
library(hms)
library(lubridate)
library(ggplot2)



###################### test data table retrieved from 'COS Tariff...xlsm' with 10 meters #######################
# dataset <- read_excel("~/COS Tariff Cat Meter Data Extract_v1.xlsm", sheet = "Meters", range = "A3:D13")
# meters <- dataset$`Meter Number`
# string_ids <- dataset$`String ID`
# start_dates <- substr(dataset$`Start Date`, 1, 10)
# end_dates <- substr(dataset$`End Date`, 1, 10)

#################### test data table with ~1600 meters ########################
# dataset <- read_excel("~/Metering.xlsx", range="B4:M1632")
# meters <- dataset$`New Meter No`
# string_ids <- dataset$Name
# end_dates <- as.Date(dataset$`Last Reading Date`, origin="1899-12-30")
# start_dates <- as.Date(end_dates) - months(12)

#################### ALL metering data with ~8200 meters ########################
dataset <- read_excel("~/Active AMI's 01.07.2021_30.06.2022_Final.xlsx", sheet = "Final")

### if you want to run n meters, put [1:n] after the following two lines
meters <- dataset$`Serial Number` #[1:n]
string_ids <- dataset$`Rate Category` #[1:n]

###################### Use API to retrieve meter data ######################

### use when NOT on CoCT network
# link <- "https://coct.pnpscada.com/Profile2SAP.jsp?" 

### use when ON CoCT network
link <- "http://citymdus01.capetown.gov.za/Profile2SAP.jsp?"

# loop through list of IDs, make a vector of each meter response (csv format) from the API call
num_ids <- length(meters)
data_vec <- vector("list", length = num_ids) # create empty
count <- 1 # for looping purposes 
missing_meters <- vector("character")
missing_data <- vector("character")

############################## while loop to retrieve data ######################################

while (count != num_ids + 1) {
  curr_meter <- meters[count] # grab the current meter we are on
  curr_str_id <- string_ids[count] # grab the rate category
  
  # some meters start with '000F...'-- we need to shorten these meters in order to make an API call
  if (nchar(curr_meter) > 8 & substr(curr_meter, 1, 4) != "000F"){
    meter_shortened <- substr(curr_meter, 5, 12)
  } else{
    meter_shortened <- curr_meter
  }
  
  # create the parameters for the API call
  # note: login and password are removed for privacy purposes
  parameters <- list(
    LOGIN = #USERNAME, 
    PWD = #PASSWORD ,
    SNUMBER = meter_shortened,
    # sDate = start_dates[count], # these are for the variable start and end dates for the 'COS Tariff...xlsm' file
    # eDate = end_dates[count]
    sDate = "2021-07-01", # financial year
    eDate = "2022-07-01"
  )
  
  api_response <- GET(link, query = parameters) # raw api response
  response_content <- content(api_response, "text") # convert the response into a usable format
  
  ################## ERROR TRAPPING ##################
  
  # this is the start of the error pattern for each scenario in which an API call is not fulfilled
  error_pattern <- 'window\\.alert\\("An error occurred:'  
  
  # search for the errors in the content of the api_response
  if (!grepl(error_pattern, response_content, perl = TRUE)) {
    temp_data <- read.csv(text = response_content)
    
    # handle the case in which the API call goes through, BUT the csv file is empty
    if (nrow(temp_data) > 0) {
      # if rows exist in the file, add that information to our data_vec
      temp_data$SNUMBER <- curr_meter
      temp_data$STR_ID <- curr_str_id
      data_vec[[count]] <- temp_data
    } else {
      # if csv file empty, add that meter number to our list of missing data
      missing_data <- c(missing_data, curr_meter)
    }
    
  } else {
    # if API call is not fulfilled, add that meter number to our list of missing meters
    missing_meters <- c(missing_meters, curr_meter)
  }
  
  # counting the while loop in the console
  count <- count + 1
  message(paste0(count-1))
  
}

api_time <- Sys.time()

## prints in the console which meters don't exist or don't have data (that were inputted)
if (length(missing_meters) > 0) {
  message(paste0(length(missing_meters), " Meter(s) We Couldn't Find: ", paste(missing_meters, collapse = ", ")))
}

if (length(missing_data) > 0) {
  message(paste0(length(missing_data), " Meter(s) That Didn't Have Data: ", paste(missing_data, collapse = ", ")))
}


################################ Data Manipulation ################################

### function to check if the meter is 
is_15min_interval <- function(inp_data){
  time_diff <- diff(inp_data$Period.End.Date.and.Time) # a list of all the time differences between dates
  
  ### note for troubleshooting: maybe just check first few
  ret_bool <- sum(time_diff == 15) >= (0.8 * (nrow(inp_data) - 1)) # checks if 80% of the time_diff is 15min
  return(ret_bool)
}

### an empty list of all frames that we need to manipulate
seasonal_frames <- list() # Stores daily average intervals

real_power_frames <- list() # Stores P Values
reactive_power_frames <- list() # Stores Q Values
apparent_power_frames <- list() # Stores S Values

## create a single column data frame of 17520 rows for each 30min interval in a year
start_inc <- as.POSIXct("2021-07-01 00:30")
end_inc <- as.POSIXct("2022-07-01 00:00")
calendar_dates <- data.frame(`Period.End.Date.and.Time` = format(seq(start_inc, end_inc, by = "30 mins"), "%Y-%m-%d %H:%M"))


for (i in seq_along(data_vec)){
  
  # only runs when data received isn't NULL
  if (!is.null(data_vec[[i]])){
    
    ################## seasonal_frames data manipulation ##################
    # removes unused columns and creates 30min intervals
    read_data <- data_vec[[i]] %>%
      select(-Q1, -Status, -P2, -Q2, -Q3, -Q4, -S) %>%
      mutate(`Period.End.Date.and.Time` = as.POSIXct(`Period.End.Date.and.Time`, format = "%Y-%m-%d %H:%M"),
             `Interval` = substr(cut(Period.End.Date.and.Time, breaks = "30 min"), 12, 16))
    
    # troubleshooting minor problem
    minute_of_first <- minute(first(read_data$`Period.End.Date.and.Time`))
    check_first_minute <- minute_of_first == 30 | minute_of_first == 0
    
    # check if the data we are on is by intervals of 15min, if so, manipulate
    if (is_15min_interval(read_data) & !check_first_minute) {
      read_data$Interval <- format(as.POSIXlt(read_data$Interval, format = "%H:%M") + 900, "%H:%M")
    }
    
    #### splitting the data into SIX categories: 
    ### High Demand Weekdays, High Demand Saturdays, High Demand Sundays
    ### Low Demand Weekdays, Low Demand Saturdays, Low Demand Sundays
    weekday_names <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    read_data <- read_data %>%
      mutate(`HighLow` = ifelse(Period.End.Date.and.Time >= as.POSIXct("2021-07-01 00:00:00") &
                                  Period.End.Date.and.Time <= as.POSIXct("2021-10-01 00:00:00"), "High", "Low"),
             `Days` = ifelse(weekdays(Period.End.Date.and.Time) %in% weekday_names[1:5], "Weekday",
                             ifelse(weekdays(Period.End.Date.and.Time) == weekday_names[6], "Saturday", "Sunday")),
             `Season` = paste(HighLow, Days, sep = " Demand Season Average of kW: ")) %>%
      select(-HighLow, -Days) %>%
      mutate(Interval = ifelse(Interval == "", "00:00", Interval)) %>%
      group_split(Season)
    
    # continuously adds newly manipulated data into seasonal_frames
    seasonal_frames <- c(seasonal_frames, read_data)

    ################## P, Q, S value manipulation ##################
    index <- i
    read_data_2 <- data_vec[[i]] %>%
      select(-Status, -S, -SNUMBER) %>%
      
      # P = P1 - P2
      # Q = Q1 + Q2 - Q3 - Q4
      # S^2 = P^2 + Q^2
      mutate(`Period.End.Date.and.Time` = format(`Period.End.Date.and.Time`, format = "%Y-%m-%d %H:%M"),
             `P.Sum` = P1-P2,
             `Q.Sum` = Q1+Q2-Q3-Q4,
             `S` = (P.Sum^2 + Q.Sum^2)^0.5) %>%
      select(-P1, -P2, -Q1, -Q2, -Q3, -Q4)

    # merge the single-column data frame of the 17520 time intervals with the newly manipulated data
    merged <- left_join(calendar_dates, read_data_2, by = "Period.End.Date.and.Time")
    
    # change format of dates to Date object
    read_data_2$Period.End.Date.and.Time <- as.POSIXct(read_data_2$Period.End.Date.and.Time, format = "%Y-%m-%d %H:%M")

    ######### if the data is in 15min intervals, have to average #########
    
    ## note: BOTTLENECK, THIS SECTION SLOWS DOWN THE CODE
    ## possible solution: right in the beginning when we get an API response, manipulate the 
    ## 15min interval meters right away so we don't have to run into more problems later
    
    if (is_15min_interval(read_data_2)){
      read_data_2$Period.End.Date.and.Time <- format(read_data_2$Period.End.Date.and.Time, format = "%Y-%m-%d %H:%M")
      unmatched_dates <- setdiff(read_data_2$Period.End.Date.and.Time, merged$Period.End.Date.and.Time)
      nearest_intervals <- cut(as.numeric(as.POSIXct(unmatched_dates)), breaks = as.numeric(as.POSIXct(calendar_dates$Period.End.Date.and.Time)), labels = FALSE, right = FALSE)

      for (i in nearest_intervals) {
        P_values_to_add <- read_data_2$P.Sum[unmatched_dates >= calendar_dates$Period.End.Date.and.Time[i]
                                        & unmatched_dates < calendar_dates$Period.End.Date.and.Time[i + 1]]
        Q_values_to_add <- read_data_2$Q.Sum[unmatched_dates >= calendar_dates$Period.End.Date.and.Time[i]
                                             & unmatched_dates < calendar_dates$Period.End.Date.and.Time[i + 1]]
        S_values_to_add <- read_data_2$S[unmatched_dates >= calendar_dates$Period.End.Date.and.Time[i]
                                             & unmatched_dates < calendar_dates$Period.End.Date.and.Time[i + 1]]
        merged$P.Sum[i] <- mean(P_values_to_add)
        merged$Q.Sum[i] <- mean(Q_values_to_add)
        merged$S[i] <- mean(S_values_to_add)
        }
    }

    real_power_merged <- merged %>%
      select(-Q.Sum, -S) %>%
      pivot_wider(names_from = Period.End.Date.and.Time, values_from = P.Sum)
    real_power_frames[[index]] <- real_power_merged
    
    reactive_power_merged <- merged %>%
      select(-P.Sum, -S) %>%
      pivot_wider(names_from = Period.End.Date.and.Time, values_from = Q.Sum)
    reactive_power_frames[[index]] <- reactive_power_merged

    apparent_power_merged <- merged %>%
      select(-P.Sum, -Q.Sum) %>%
      pivot_wider(names_from = Period.End.Date.and.Time, values_from = S)
    apparent_power_frames[[index]] <- apparent_power_merged

    message(paste0(index)) # prints index in console to check where we are
    
  }
}

####################### create data frames for P, Q, S values #######################

real_power_df <- bind_rows(real_power_frames) %>%
  group_by(STR_ID) %>%
  summarize(across(all_of(calendar_dates$Period.End.Date.and.Time), sum, na.rm = TRUE)) %>%
  column_to_rownames("STR_ID")
real_power_df <- as.data.frame(t(real_power_df))

reactive_power_df <- bind_rows(reactive_power_frames) %>%
  group_by(STR_ID) %>%
  summarize(across(all_of(calendar_dates$Period.End.Date.and.Time), sum, na.rm = TRUE)) %>%
  column_to_rownames("STR_ID")
reactive_power_df <- as.data.frame(t(reactive_power_df))

apparent_power_df <- bind_rows(apparent_power_frames) %>%
  group_by(STR_ID) %>%
  summarize(across(all_of(calendar_dates$Period.End.Date.and.Time), sum, na.rm = TRUE)) %>%
  column_to_rownames("STR_ID")
apparent_power_df <- as.data.frame(t(apparent_power_df))

write.csv(real_power_df, "~/results2021_07_27_REAL_POWER.csv", row.names = FALSE)
write.csv(reactive_power_df, "~/results2021_07_27_REACTIVE_POWER.csv", row.names = FALSE)
write.csv(apparent_power_df, "~/results2021_07_27_APPARENT_POWER.csv", row.names = FALSE)

  
############################ Creation of Data Frame for seasonal_frames ####################################

intervals <- c("00:00-00:30", "00:30-01:00", "01:00-01:30", "01:30-02:00", 
               "02:00-02:30",	"02:30-03:00", "03:00-03:30",	"03:30-04:00", "04:00-04:30", 
               "04:30-05:00", "05:00-05:30", "05:30-06:00", "06:00-06:30", "06:30-07:00", 
               "07:00-07:30", "07:30-08:00", "08:00-08:30",	"08:30-09:00", "09:00-09:30",	
               "09:30-10:00", "10:00-10:30", "10:30-11:00", "11:00-11:30", "11:30-12:00", 
               "12:00-12:30", "12:30-13:00", "13:00-13:30", "13:30-14:00", "14:00-14:30", 
               "14:30-15:00", "15:00-15:30", "15:30-16:00", "16:00-16:30", "16:30-17:00", 
               "17:00-17:30",	"17:30-18:00", "18:00-18:30",	"18:30-19:00", "19:00-19:30", 
               "19:30-20:00", "20:00-20:30", "20:30-21:00", "21:00-21:30", "21:30-22:00", 
               "22:00-22:30", "22:30-23:00", "23:00-23:30", "23:30-00:00")

# used to find compound sum per time interval (last six rows of final data frame)
compounded <- list()

for (i in seq_along(seasonal_frames)){
  
  # gathering data before full manipulation
  copy_data <- seasonal_frames[[i]] %>%
    summarise(Sum = sum(P1), Count1 = sum(Interval == "00:00"), Count2 = sum(Interval == "23:30"))  
  
  # finding compound sum
  compounded[[i]] <- seasonal_frames[[i]] %>%
    group_by(Interval) %>%
    summarise(Sum = sum(P1)) %>%
    mutate(`Rate Category` = "Compound", 
           `Meter Number` = "All Meters", 
           `Season` = first(seasonal_frames[[i]]$Season),
           `Start Date` = NA,
           `End Date` = NA, 
           `Interval` = paste(Interval, format(as.POSIXlt(strptime(Interval, format = "%H:%M")) + 1800, "%H:%M"), sep = "-"),
           `Count of Data Points: 00:00-00:30` = NA, 
           `Count of Data Points: 23:30-00:00` = NA) %>%
    select(`Rate Category`, `Meter Number`, `Season`, `Start Date`, `End Date`, `Interval`,
           `Count of Data Points: 00:00-00:30`, `Count of Data Points: 23:30-00:00`, everything()) %>%
    pivot_wider(names_from = Interval, values_from = Sum)
  
  # finding averages per time interval
  seasonal_frames[[i]] <- seasonal_frames[[i]] %>%
    group_by(Interval) %>%
    summarise(Average = mean(P1)) %>%
    mutate(`Rate Category` = first(seasonal_frames[[i]]$STR_ID),
           `Meter Number` = first(seasonal_frames[[i]]$SNUMBER),
           `Season` = first(seasonal_frames[[i]]$Season),
           `Start Date` = first(seasonal_frames[[i]]$`Period.End.Date.and.Time`),
           `End Date` = last(seasonal_frames[[i]]$`Period.End.Date.and.Time`), 
           `Interval` = paste(Interval, format(as.POSIXlt(strptime(Interval, format = "%H:%M")) + 1800, "%H:%M"), sep = "-"),
           `Count of Data Points: 00:00-00:30` = copy_data$Count1,
           `Count of Data Points: 23:30-00:00` = copy_data$Count2,
           `Energy Consumed (kWh)` = if_else(
             is_15min_interval(seasonal_frames[[i]]), copy_data$Sum/4, copy_data$Sum/2)) %>%
    pivot_wider(names_from = Interval, values_from = Average)
}

# combine each row of the averages we found into one data frame
meter_df <- bind_rows(seasonal_frames) %>%
  relocate(`Energy Consumed (kWh)`, .after = last_col())

# combine each row of the compound sums we found into one data frame
compound_df <- bind_rows(compounded) %>%
  group_by(Season)%>%
  summarize(across(all_of(intervals), sum, na.rm = TRUE)) %>%
  mutate(`Rate Category` = "Compound", `Meter Number` = "All Meters", `Start Date` = NA, `End Date` = NA, 
         `Count of Data Points: 00:00-00:30` = NA, `Count of Data Points: 23:30-00:00` = NA) %>%
  select(`Rate Category`, `Meter Number`, `Season`, `Start Date`, `End Date`, 
         `Count of Data Points: 00:00-00:30`, `Count of Data Points: 23:30-00:00`, everything())

# combine BOTH these data frames together
product_1_df <- bind_rows(meter_df, compound_df)
# write.csv(product_1_df, "~/results2021_07_24_TEST.csv", row.names = FALSE)

end_time <- Sys.time()
api_time-start_time
end_time-start_time

end_time 

########################## Graph Seasonal Compound Load Curves ##########################

# create a simplified data frame to graph
graph_data <- compound_df %>%
  select(-`Meter Number`, -`Start Date`, -`End Date`, 
         -`Count of Data Points: 00:00-00:30`, -`Count of Data Points: 23:30-00:00`, -`Rate Category`) %>%
  pivot_longer(cols = all_of(intervals), names_to = "Interval", values_to = "Sum (kW)")
  
ggplot(graph_data, aes(x=Interval, y=`Sum (kWh)`, group=Season, color=Season)) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))


