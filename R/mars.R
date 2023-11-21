# Copyright © 2023 University of Kansas. All rights reserved.
#
# Creative Commons Attribution NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

#' @title Movement Analysis for Remote Sensors (MARS)
#' @description Main function to execute MARS accelerometer processing program.
#' @param study.name Abbreviation of the Study, Default: study
#' @param datadir Directory where the AGD files are stored.
#' @param results Directory where the results should be stored.
#' @param adult.cp Adult cut-point to be used (see cutpoint.list.R for list of available cut-points), Default: ''
#' @param child.cp Child cut-point to be used (see cutpoint.list.R for list of available cut-points), Default: ''
#' @param spurious A maximum value for vertical or vector magnitude counts to not exceed, Default: 20000
#' @param axis Number of axes to be used (1: Vertical Axis; 3: Vector Magnitude), Default: 1
#' @param overwrite Overwrite the individual files if individual.file.save is set to TRUE, Default: FALSE
#' @param person.time Export the accelerometer summary by person time categories, Default: FALSE
#' @param person.date Export the accelerometer summary by person date, Default: TRUE
#' @param person.month Export only valid days to a summary by person time point of the trial, Default: FALSE
#' @param valid Set the number of minutes for a valid day, Default: 480
#' @param return.timestamped.dataframe Return a timestamped data set into R for all combined time point and id data, Default: FALSE
#' @param individual.file.save Save all of the individual timestamped files to a CSV, Default: FALSE
#' @param do.parallel Process dates in parallel to speed up the processing of the accelerometer data
#' @return Individual and/or summary files to a Results folder located in the designated directory.
#' @details Main function to execute MARS accelerometer processing program.
#' @seealso
#'  \code{\link[readr]{write_delim}}
#'  \code{\link[plyr]{rbind.fill}}
#' @rdname mars.main
#' @export
#' @importFrom readr write_csv
#' @importFrom plyr rbind.fill

mars.main <- function(study.name = "study",
                      datadir, results, adult.cp = "", child.cp = "", spurious = 20000,
                      axis = 1, overwrite=FALSE, person.time=FALSE,
                      person.date=TRUE, person.month=FALSE, valid = 480,
                      return.timestamped.dataframe=FALSE, individual.file.save=FALSE,
                      do.parallel = TRUE) {

  files <- sort(list.files(datadir, pattern = ".agd$"))

  newdatadir <- agd_to_csv(datadir)

  csv.files <- sort(list.files(newdatadir, pattern = ".csv$", full.names = TRUE))

  if(!dir.exists(results)) dir.create(results)

  if (!dir.exists(paste0(results, "/Individual Files"))) dir.create(paste0(results, "/Individual Files"))

  individual.files <- paste0(results, "/Individual Files")

  if(!dir.exists(paste0(results, "/Summary Files"))) dir.create(paste0(results, "/Summary Files"))

  summary.files <- paste0(results, "/Summary Files")

  if(child.cp == "freedson.child"){
    demographics <- birth.date(datadir)
  } else {
    demographics <- NULL
  }

  load(system.file("extdata/cutpointList.RData", package = "MoveKC"))

  main <- function(file, study.name, demographics, child.cp, cutpoint.list,
                   adult.cp, spurious, axis, individual.file.save){

    record.id = strsplit(basename(file), split = " ")[[1]][1]
    print(paste0("Processing accelerometer data from: ", record.id))
    data <- AGread.csv(demo=demographics, file=file, record.id)
    data <- AG.temporal(data, season=TRUE, weekday=TRUE, time=TRUE)

    # Only child
    if(child.cp != "" & adult.cp == ""){
      if(child.cp == "freedson.child" & exists("age", data)){
        data <- cutpoints(data = data, sets = cutpoint.list, set.name=child.cp, n.axis=as.character(axis), spurious = spurious)
      }
      if (child.cp == "freedson.child" & !exists("age", data)) {
        stop("Age-specific cut-point specified, but could not find age in the data.")
      }
    }

    # Only adult
    if(adult.cp != "" & child.cp == ""){
      data <- cutpoints(data = data, sets = cutpoint.list, set.name=adult.cp, n.axis=as.character(axis), spurious = spurious)
    }

    # Both adult and child
    if(child.cp != "" & adult.cp != ""){

      if(!exists("age", data)){
        stop("Both child and adult cut-points specified. Age needs to be included in the data.")
      }

      data.under.18 <- data[data$age < 18, ]
      data.18.over <- data[data$age >= 18, ]

      if (dim(data.under.18)[1]!=0) {
        data.under.18 <- cutpoints(data = data.under.18, sets = cutpoint.list, set.name=child.cp, n.axis=as.character(axis), spurious = spurious)
      }

      if (dim(data.18.over)[1]!=0) {
        data.18.over <- cutpoints(data = data.18.over, sets = cutpoint.list, set.name=adult.cp, n.axis=as.character(axis), spurious = spurious)
      }

      data = rbind(data.under.18, data.18.over)
    }

    # if(individual.file.save==TRUE){
    #
    #   individual.filename <- paste0(individual.files, "/", record.id, ".", name, ".csv")
    #   }
    #
    #   if(file.exists(individual.filename)==TRUE & overwrite==TRUE){
    #     readr::write_csv(data, individual.filename, append=FALSE, col_names = TRUE)
    #   }
    #
    #   if(file.exists(individual.filename)==FALSE){
    #     readr::write_csv(data, individual.filename, append=FALSE, col_names = TRUE)
    #   }

    return(data)
  }

  # if parallel
  if(do.parallel) {
    cores <- parallel::detectCores()
    Ncores = cores - 1
    cl = parallel::makeCluster(Ncores)
    doParallel::registerDoParallel(cl)
    `%dopar%` <- foreach::`%dopar%`
    accel.data <- foreach::foreach(i = csv.files, .packages = "MoveKC", .errorhandling = "pass") %dopar% {
      data <- tryCatch({
        main(file = i, study.name = study.name, demographics = demographics,
             cutpoint.list, child.cp = child.cp, adult.cp = adult.cp,
             spurious = spurious, axis = axis,
             individual.file.save = individual.file.save)},
        error = function(err){
          return(basename(i))
        }
        )
    }
    errors_no <- which(do.call("c", lapply(accel.data, is.character)))
    errors <- do.call("c", accel.data[errors_no])
    accel.data <- dplyr::bind_rows(accel.data[-errors_no])
    parallel::stopCluster(cl)
  } else{
    accel.data <- list()
    for(i in csv.files){
      data <- tryCatch({
        df <- main(file = i, study.name = study.name, demographics = demographics,
                   cutpoint.list, child.cp = child.cp, adult.cp = adult.cp,
                   spurious = spurious, axis = axis,
                   individual.file.save = individual.file.save)},

        error = function(err){
          return(basename(i))
        })
      accel.data <- append(accel.data, list(data))
    }
    errors_no <- which(do.call("c", lapply(accel.data, is.character)))
    errors <- do.call("c", accel.data[errors_no])
    accel.data <- dplyr::bind_rows(accel.data[-errors_no])
  }

  `%>%` <- dplyr::`%>%`

  # add age back at some point
  if("age" %in% colnames(accel.data)) accel.data$age <- NULL

  data.by.person.time <- accel.data %>%
    dplyr::group_by(record.id, date=format(accel.data$time.stamp, "%m/%d/%Y"), days, weekday, time.category, season) %>%
    dplyr::summarise_at(names(dplyr::select(accel.data, counts:mvpa.bout.counts)), sum, na.rm=TRUE)

  data.by.person.date <- accel.data %>%
    dplyr::group_by(record.id, date=format(accel.data$time.stamp, "%m/%d/%Y"), days, weekday, season) %>%
    dplyr::summarise_at(names(dplyr::select(accel.data, counts:mvpa.bout.counts)), sum, na.rm=TRUE) %>%
    dplyr::ungroup()

  data.by.person.date$valid_days <- ifelse(data.by.person.date$wear >= valid, 1, 0)

  data.by.person <- data.by.person.date %>%
    dplyr::filter(wear >= valid) %>%
    dplyr::group_by(record.id) %>%
    dplyr::summarise_at(names(dplyr::select(data.by.person.date, counts:mvpa.bout.counts)), mean, na.rm=TRUE)

  valid.days <- data.by.person.date %>%
    dplyr::filter(wear >= valid) %>%
    dplyr::group_by(record.id) %>%
    dplyr::summarise(valid_days = sum(valid_days, na.rm=TRUE), .groups = "keep")

  data.by.person <- merge(data.by.person, valid.days, by="record.id", all = TRUE)

  # Set File Name

  if (child.cp != "" & adult.cp != ""){
    name = paste0(study.name,".", child.cp, ".", adult.cp, ".", axis, "axis")
  }

  if (child.cp != "" & adult.cp == ""){
    name = paste0(study.name,".", child.cp, ".", axis, "axis")
  }

  if (child.cp == "" & adult.cp != ""){
    name = paste0(study.name,".", adult.cp, ".", axis, "axis")
  }

  if(person.time==TRUE){
    readr::write_csv(data.by.person.time, paste0(summary.files, "/", name, ".person.time.csv"), append=FALSE, col_names=TRUE)
  }

  if(person.date==TRUE){
    readr::write_csv(data.by.person.date, paste0(summary.files, "/", name, ".person.date.csv"), append=FALSE, col_names=TRUE)
  }

  if(person.month==TRUE){
    readr::write_csv(data.by.person, paste0(summary.files, "/", name, ".person.csv"), append=FALSE, col_names=TRUE)
  }

  if(length(errors) != 0) cat(sprintf("Error processing the following files:\n %s", paste(errors, collapse = "\n ")))

  if(return.timestamped.dataframe==TRUE){
    return(accel.data)
  }
}
