% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CT_one_cal.R
\name{CT_one_cal}
\alias{CT_one_cal}
\title{Calculate temperature-compensated Specific Conductance and one-point calibration for CT logger data}
\usage{
CT_one_cal(
  data,
  date,
  temp,
  EC.logger,
  cal.ref,
  cal.ref.temp,
  startCal,
  endCal,
  EC.cal = FALSE
)
}
\arguments{
\item{data}{The dataframe containing raw Electrical Conductivity values}

\item{date}{Date and time column used to filter out calibration times}

\item{temp}{Temperature at which electrical conductivity values were logged}

\item{EC.logger}{Electrical conductivity logged at time of calibration}

\item{cal.ref}{The conductivity calibration solution specific conductance value, uS/cm at 25degC, or the electrical conductivity value of a secondary probe}

\item{cal.ref.temp}{The temperature of the conductivity standard or in situ water read by a secondary probe}

\item{startCal}{Date and time at the start of the calibration}

\item{endCal}{Date and time at the end of the calibration}

\item{EC.cal}{Logical parameter indicating whether the logger should be calibrated to an electrical conductivity value, then TRUE, or a specific conductance value, then FALSE. Default = FALSE.}
}
\value{
The original dataframe with the newly calculated Specific Conductance and calibrated SC values of CT logger data
}
\description{
This function reads in a dataframe containing electrical conductivity logs from a conductivity-temperature logger
and calculates specific conductance from in situ electrical conductivity and temperature readings.
}
