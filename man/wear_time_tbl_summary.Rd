% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wear_time.R
\name{wear_time_tbl_summary}
\alias{wear_time_tbl_summary}
\title{wear_time_tbl_summary}
\usage{
wear_time_tbl_summary(data, days = c(), weekend = TRUE)
}
\arguments{
\item{data}{Data with wear time information from the calculate_wear_time function}

\item{days}{Number of days to consider for valid wear time, Default: c()}

\item{weekend}{Whether the wear time criteria consider the difference between valid weekend days, Default: TRUE}
}
\value{
A gt table with the number of valid observation
}
\description{
Print out a gt table with the number of valid observations
}
\details{
Print out a gt table with the number of valid observations
}
\seealso{
\code{\link[dplyr]{arrange}}
\code{\link[dplyr]{filter}}
\code{\link[gt]{gt}}
\code{\link[gtExtras]{gt_theme_538}}
}
