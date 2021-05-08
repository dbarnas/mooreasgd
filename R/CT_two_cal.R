#' Calculate temperature-compensated Specific Conductance and two-point calibration for CT logger data
#'
#' This function reads in a dataframe containing electrical conductivity logs from a conductivity-temperature logger
#' and calculates specific conductance from in situ electrical conductivity and temperature readings.
#'
#'
#' @param data The dataframe containing raw Electrical Conductivity values
#' @param date Date and time column used to filter out calibration times
#' @param temp Temperature at which electrical conductivity values were logged
#' @param EC Electrical conductivity logged at time of calibration
#' @param high.Ref The high range conductivity calibration solution specific conductance value, uS/cm at 25degC
#' @param low.Ref The low range conductivity calibration solution specific conductance value, uS/cm at 25degC
#' @param startHigh Date and time at the start of the high range calibration
#' @param endHigh Date and time at the end of the high range calibration
#' @param startLow Date and time at the start of the low range calibration
#' @param endLow Date and time at the end of the low range calibration
#' @return The original dataframe with the calibrated Electrical Conductivity values of CT logger data
#' @export
CT_two_cal<-function(data, date, temp, EC, high.Ref, low.Ref, startHigh, endHigh, startLow, endLow) {


  ############################################################
  ### Two Point Calibration
  ############################################################

  # mean temperature at calibration intervals
  high.mean.temp<-data %>%
    dplyr::filter(dplyr::between({{date}}, {{startHigh}}, {{endHigh}})) %>%
    dplyr::rename(TempInSitu = temp) %>%
    dplyr::summarise(mean = mean(TempInSitu)) %>%
    as.numeric()
  low.mean.temp<-data %>%
    dplyr::filter(dplyr::between({{date}}, {{startLow}}, {{endLow}})) %>%
    dplyr::rename(TempInSitu = temp) %>%
    dplyr::summarise(mean = mean(TempInSitu)) %>%
    as.numeric()

  # use mean temperature of calibrations with PSS-78 and gsw package
  # Get EC of conductivity standard at logged temperature to calibrate logged EC readings
  highCal.sp<-gsw::gsw_SP_from_C(C = high.Ref*0.001, t = 25, p = 10) # calculate salinity from specific conductance value of standard
  highCal<-1000*gsw::gsw_C_from_SP(SP = highCal.sp, t = high.mean.temp, p = 10) # calculate electrical conductivity of standard at logged temperature

  lowCal.sp<-gsw::gsw_SP_from_C(C = low.Ref*0.001, t = 25, p = 10)
  lowCal<-1000*gsw::gsw_C_from_SP(SP = lowCal.sp, t = low.mean.temp, p = 10)


  # mean EC at calibration interval
  rawHigh<-data %>%
    dplyr::filter(dplyr::between({{date}}, {{startHigh}}, {{endHigh}})) %>%
    dplyr::rename(EC = EC) %>%
    dplyr::summarise(mean = mean(EC)) %>%
    as.numeric()
  rawLow<-data %>%
    dplyr::filter(dplyr::between({{date}}, {{startLow}}, {{endLow}})) %>%
    dplyr::rename(EC = EC) %>%
    dplyr::summarise(mean = mean(EC)) %>%
    as.numeric()

  # calibration
  rawRange<-rawHigh - rawLow
  refRange<-highCal - lowCal

  data<-data %>%
    dplyr::mutate(EC_Cal = (((EC - rawLow) * refRange) / rawRange) + lowCal)


  return(data)

}




