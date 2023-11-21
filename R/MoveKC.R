# Copyright © 2023 University of Kansas. All rights reserved.
#
# Creative Commons Attribution NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

if(getRversion() >= "2.15.1")  utils::globalVariables(
  c(".", " Axis1", "counts", "Vector Magnitude",
    "days", "interrupts", "mvpa", "mvpa.bout.counts", "mvpa.bout.length", 
    "record.id", "season", "sedentary", "steps", "temp", "time.category", 
    "valid_days", "vector.magnitude", "wear", "weekday", "time", "weekend",
    "data", "Days", "Hours", "cutpoint.list"))

#' @title MoveKC: A package for physical activity and weight management research.
#'
#' @description The MoveKC package provides several important functions:
#' 
#' \code{\link{ilr_transform}}
#' 
#' \code{\link{mars.main}}
#' 
#' \code{\link{read_agd}}
#' 
#' \code{\link{agd_to_csv}}
#' 
#' \code{\link{gt3x2csv}}
#' 
#' \code{\link{AGread.csv}}
#' 
#' \code{\link{birth.date}}
#' 
#' \code{\link{AG.temporal}}
#' 
#' \code{\link{cutpoints}}
#' 
#' \code{\link{detect.bouts}}
#'  
#' @docType package
#' @name MoveKC
#' @import dplyr
#' @import magrittr

NULL
