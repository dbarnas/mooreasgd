#' Calculate temperature-compensated Specific Conductance and two-point calibration for CT logger data
#'
#' This function reads in a dataframe containing electrical conductivity logs from a conductivity-temperature logger
#' and calculates specific conductance from in situ electrical conductivity and temperature readings.
#'
#'
#' @param data The dataframe containing raw Electrical Conductivity values
#' @param date Date and time column used to filter out calibration times
#' @param temp Temperature at which electrical conductivity values were logged
#' @param EC.logger Electrical conductivity logged at time of calibration
#' @param high.ref The high range conductivity calibration solution specific conductance value, uS/cm at 25degC
#' @param low.ref The low range conductivity calibration solution specific conductance value, uS/cm at 25degC
#' @param high.ref.temp High range in situ temperature, value from a secondary probe
#' @param low.ref.temp Low range in situ temperature, value from a secondary probe
#' @param startHigh Date and time at the start of the high range calibration
#' @param endHigh Date and time at the end of the high range calibration
#' @param startLow Date and time at the start of the low range calibration
#' @param endLow Date and time at the end of the low range calibration
#' @param EC.cal Logical parameter indicating whether the logger should be calibrated to an electrical conductivity value, then TRUE, or a specific conductance value, then FALSE. Default = FALSE.
#' @return The original dataframe with the calibrated Electrical Conductivity values of CT logger data
#' @export
CT_two_cal<-function(data, date,temp, EC.logger, high.ref, low.ref, high.ref.temp, low.ref.temp, startHigh, endHigh, startLow, endLow, EC.cal = FALSE) {


  ############################################################
  ### Two Point Calibration
  ############################################################

  # mean temperature at calibration intervals
  high.mean.temp<-data %>%
    dplyr::filter(dplyr::between({{date}}, {{startHigh}}, {{endHigh}})) %>%
    dplyr::rename(TempInSitu = contains("temp") | contains("Temp")) %>%
    dplyr::summarise(mean = mean(TempInSitu)) %>%
    as.numeric()
  low.mean.temp<-data %>%
    dplyr::filter(dplyr::between({{date}}, {{startLow}}, {{endLow}})) %>%
    dplyr::rename(TempInSitu = contains("temp") | contains("Temp")) %>%
    dplyr::summarise(mean = mean(TempInSitu)) %>%
    as.numeric()

  if(EC.cal == FALSE){

  # use mean temperature of calibrations with PSS-78 and gsw package
  # Get EC of conductivity standard at logged temperature to calibrate logged EC readings
  highCal.sp<-gsw::gsw_SP_from_C(C = high.Ref*0.001, t = 25, p = 10) # calculate salinity from specific conductance value of standard
  highCal<-1000*gsw::gsw_C_from_SP(SP = highCal.sp, t = high.mean.temp, p = 10) # calculate electrical conductivity of standard at logged temperature

  lowCal.sp<-gsw::gsw_SP_from_C(C = low.Ref*0.001, t = 25, p = 10)
  lowCal<-1000*gsw::gsw_C_from_SP(SP = lowCal.sp, t = low.mean.temp, p = 10)

  # assuming the temperature of the standard solution is equivalent to the temperature read by the logger
  high.ref.temp = high.mean.temp
  low.ref.temp = low.mean.temp
  } # else, use cal.ref = ec reading from secondary probe and cal.ref.temp = temperature reading from probe


  # mean EC at calibration interval
  rawHigh<-data %>%
    dplyr::filter(dplyr::between({{date}}, {{startHigh}}, {{endHigh}})) %>%
    dplyr::rename(EC.logger = contains("EC") | contains("E_C")) %>%
    dplyr::summarise(mean = mean(EC.logger)) %>%
    as.numeric()
  rawLow<-data %>%
    dplyr::filter(dplyr::between({{date}}, {{startLow}}, {{endLow}})) %>%
    dplyr::rename(EC.logger = contains("EC") | contains("E_C")) %>%
    dplyr::summarise(mean = mean(EC.logger)) %>%
    as.numeric()

  # calibration
  rawRange<-rawHigh - rawLow
  refRange<-highCal - lowCal

  data<-data %>%
    dplyr::mutate(EC_Cal = (((EC.logger - rawLow) * refRange) / rawRange) + lowCal)


  return(data)

}




