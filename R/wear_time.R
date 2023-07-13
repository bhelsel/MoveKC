#' @title calculate_wear_time
#' @description Calculates wear time for several .agd or .csv files
#' @param directory Path name to the location of the .agd or .csv files
#' @param valid_wear_time Number of hours to be considered a valid day, Default: c(8, 10)
#' @return A data frame containing the number of valid days, 
#' @details Calculates wear time for several .agd or .csv files
#' @seealso 
#'  \code{\link[dplyr]{rename}}
#'  \code{\link[PhysicalActivity]{wearingMarking}}
#'  \code{\link[plyr]{rbind.fill}}
#' @rdname calculate_wear_time
#' @export 
#' @importFrom dplyr rename
#' @importFrom PhysicalActivity wearingMarking
#' @importFrom plyr rbind.fill

calculate_wear_time <- function(directory, valid_wear_time = c(8, 10)){
  
  main_data <- data.frame()
  
  files <- list.files(directory, full.names = TRUE, pattern = ".csv$|.agd$")
  
  for(file in files){
    id <- strsplit(basename(file), " ")[[1]][1]
    print(sprintf("Processing data from %s", id))
    
    # Check to see if .csv file
    if(substr(file, nchar(file)-3, nchar(file)) == ".csv"){
      data <- read.csv(file)
    }
    
    # Check to see if .agd file  
    if(substr(file, nchar(file)-3, nchar(file)) == ".agd"){
      data <- read_agd(file)
      data$time <- as.POSIXct(paste(data$Date, data$` Time`), format = "%m/%d/%Y %H:%M:%S", tz = "UTC")
      data <- dplyr::rename(data, "Axis1" = " Axis1")
    }
    
    if(length(data[is.na(data$time), "time"]) > 0){
      data[is.na(data$time), "time"] <-
        seq(data$time[1], data$time[nrow(data)], "1 min") %>%
        setdiff(., data$time) %>%
        as.POSIXct(., origin = "1970-01-01", tz = "UTC")
    }
    
    data <- PhysicalActivity::wearingMarking(dataset = data, frame = 90, perMinuteCts = 1, TS = "time", cts = "Axis1", allowanceFrame = 2, newcolname = "wear", tz = "UTC")
    data$weekend <- ifelse(data$weekday %in% c("Saturday", "Sunday"), 1, 0)
    data$wear <- ifelse(data$wear=="w", 1, 0)
    
    data %<>%
      group_by(Date = as.Date(time, format = "%m/%d/%Y")) %>%
      summarise(wear = sum(wear),
                weekend = mean(weekend), .groups = "drop")
    
    for(i in 1:length(valid_wear_time)){
      data[, paste0("valid_day_", valid_wear_time[i])] <- ifelse(data$wear >= valid_wear_time[i] * 60, 1, 0)
      data[, paste0("weekend_", valid_wear_time[i])] <- ifelse(data$weekend==1 & data[, paste0("valid_day_", valid_wear_time[i])]==1, 1, 0)
      data[, paste0("wear_", valid_wear_time[i])] <- ifelse(data[, paste0("valid_day_", valid_wear_time[i])] == 1, data$wear, 0)
    }
    
    data %<>%
      mutate(id = id) %>%
      select(-c("Date", "weekend", "wear")) %>%
      group_by(id) %>%
      summarise_all("sum")
    
    for(i in 1:length(valid_wear_time)){
      data[, paste0("wear_", valid_wear_time[i])] <- data[, paste0("wear_", valid_wear_time[i])] / data[, paste0("valid_day_", valid_wear_time[i])]
    }
    
    main_data <- plyr::rbind.fill(main_data, data)
  }
  return(main_data)
}


#' @title wear_time_tbl_summary
#' @description Print out a gt table with the number of valid observations
#' @param data Data with wear time information from the calculate_wear_time function
#' @param days Number of days to consider for valid wear time, Default: c()
#' @param weekend Whether the wear time criteria consider the difference between valid weekend days, Default: TRUE
#' @return A gt table with the number of valid observation
#' @details Print out a gt table with the number of valid observations
#' @seealso 
#'  \code{\link[dplyr]{arrange}}
#'  \code{\link[dplyr]{filter}}
#'  \code{\link[gt]{gt}}
#'  \code{\link[gtExtras]{gt_theme_538}}
#' @rdname wear_time_tbl_summary
#' @export 
#' @importFrom dplyr arrange filter
#' @importFrom gt gt


wear_time_tbl_summary <- function(data, days = c(), weekend = TRUE){
  
  if(length(days) == 0) stop("Please add the number of days to consider for valid wear time")
  
  hours <- unique(as.numeric(gsub("\\D", "", names(data))))
  hours <- hours[!is.na(hours)]
  
  rows <- length(days) * length(hours) * (sum(weekend) + 1)
  
  summary <- data.frame(Days = sort(rep(days, rows / length(days))),
                        Hours = rep(hours, rows / length(hours)))
  
  summary %<>% dplyr::arrange(Days, Hours)
  
  if(weekend) {
    summary$Weekend <- rep(0:1, rows / 2)
  } else{
    summary$Weekend <- rep(0, rows)
    }

  wearCount <- function(d, h, w){
    vd <- paste0("valid_day_", h)
    if(w == 0) c <- data %>% dplyr::filter(!!sym(vd) >= d) %>% count() %>% as.integer()
    if(w == 1) {
      wknd <- paste0("weekend_", h)
      c <- data %>% dplyr::filter(!!sym(vd) >= d & !!sym(wknd) >= 1) %>% count() %>% as.integer()
    }
    return(c)
  }
  
  summary$N <- sapply(1:nrow(summary), function(x) wearCount(d = summary$Days[x], h = summary$Hours[x], w = summary$Weekend[x]))
  
  gt::gt(summary)
}


