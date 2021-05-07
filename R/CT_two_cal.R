#' Calculate temperature-compensated Specific Conductance and two-point calibration for CT logger data
#'
#' This function reads in a dataframe containing electrical conductivity logs from a conductivity-temperature logger
#' and calculates specific conductance from in situ electrical conductivity and temperature readings.
#'
#'
#' @param data The dataframe containing raw Electrical Conductivity values
#' @param date Date and time column used to filter out calibration times
#' @param temp Temperature at which electrical conductivity values were logged
#' @param Abs_pressure Absolute pressure, measured in dbar, used to calculate salinity using the PSS-78 equation.  Default = 10dbar.
#' @param EC Electrical conductivity logged at time of calibration
#' @param high.Ref The high range conductivity calibration solution specific conductance value, uS/cm at 25degC
#' @param low.Ref The low range conductivity calibration solution specific conductance value, uS/cm at 25degC
#' @param startHigh Date and time at the start of the high range calibration
#' @param endHigh Date and time at the end of the high range calibration
#' @param startLow Date and time at the start of the low range calibration
#' @param endLow Date and time at the end of the low range calibration
#' @return The original dataframe with the calibrated Electrical Conductivity values of CT logger data
#' @export
CT_two_cal<-function(data, date, temp, Abs_pressure = 10, EC, high.Ref, low.Ref, startHigh, endHigh, startLow, endLow) {


  ############################################################
  ### Two Point Calibration
  ############################################################

  # mean temperature at calibration intervals
  high.mean.temp<-data %>%
    filter(between({{date}}, {{startHigh}}, {{endHigh}})) %>%
    summarise(mean = mean({{temp}})) %>%
    as.numeric()
  low.mean.temp<-data %>%
    filter(between({{date}}, {{startLow}}, {{endLow}})) %>%
    summarise(mean = mean({{temp}})) %>%
    as.numeric()

  # use mean temperature of calibrations with PSS-78 and gsw package
  # Get EC of conductivity standard at logged temperature to calibrate logged EC readings
  highCal.sp<-gsw_SP_from_C(C = high.Ref*0.001, t = 25, p = Abs_pressure)
  highCal<-1000*gsw_C_from_SP(SP = highCal.sp, t = {{temp}}, p = Abs_pressure)

  lowCal.sp<-gsw_SP_from_C(C = low.Ref*0.001, t = 25, p = Abs_pressure)
  lowCal<-1000*gsw_C_from_SP(SP = lowCal.sp, t = {{temp}}, p = Abs_pressure)


    # mean EC at calibration interval
    rawHigh<-data %>%
      filter(between({{date}}, {{startHigh}}, {{endHigh}})) %>%
      summarise(mean = mean({{EC}})) %>%
      as.numeric()
    rawLow<-data %>%
      filter(between({{date}}, {{startLow}}, {{endLow}})) %>%
      summarise(mean = mean({{EC}})) %>%
      as.numeric()

    # calibration
    rawRange<-rawHigh - rawLow
    refRange<-highCal - lowCal

    data<-data %>%
      mutate(EC_Cal = ((({{EC}} - rawLow) * refRange) / rawRange) + lowCal)


  return(data)
}




