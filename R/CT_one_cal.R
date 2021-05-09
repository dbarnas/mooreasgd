#' Calculate temperature-compensated Specific Conductance and one-point calibration for CT logger data
#'
#' This function reads in a dataframe containing electrical conductivity logs from a conductivity-temperature logger
#' and calculates specific conductance from in situ electrical conductivity and temperature readings.
#'
#'
#' @param data The dataframe containing raw Electrical Conductivity values
#' @param date Date and time column used to filter out calibration times
#' @param temp Temperature at which electrical conductivity values were logged
#' @param EC Electrical conductivity logged at time of calibration
#' @param cal.ref The conductivity calibration solution specific conductance value, uS/cm at 25degC
#' @param startCal Date and time at the start of the calibration
#' @param endCal Date and time at the end of the calibration
#' @return The original dataframe with the newly calculated Specific Conductance and calibrated SC values of CT logger data
#' @export
CT_one_cal<-function(data, date, temp, EC, cal.ref, startCal, endCal) {


  ############################################################
  ### One Point Calibration
  ############################################################

  # mean temperature at calibration
  mean.temp<-data %>%
    dplyr::filter(dplyr::between({{date}}, {{startCal}}, {{endCal}})) %>%
    dplyr::rename(TempInSitu = contains("temp") | contains("Temp")) %>%
    dplyr::summarise(mean = mean(TempInSitu)) %>%
    as.numeric()

  # use mean temperature of calibrations with PSS-78 and gsw package
  # Get EC of conductivity standard at logged temperature to calibrate logged EC readings
  cal.sp<-gsw::gsw_SP_from_C(C = cal.ref*0.001, t = 25, p = 10)
  cal.ref<-1000*gsw::gsw_C_from_SP(SP = cal.sp, t = mean.temp, p = 10)

  # Logger data in pre-deployment calibration
  mean.ec<-data%>%
    dplyr::filter(dplyr::between({{date}},{{startCal}},{{endCal}}))%>%
    dplyr::rename(EC = contains("EC") | contains("E_C")) %>%
    dplyr::summarise(mean = mean(EC)) %>%
    as.numeric()

  # Offset between the calibration reference and the logger reading
  offset<-cal.ref - mean.ec

  # Apply offset to logger data
  data<-data%>%
    dplyr::mutate(EC_Cal = EC + offset)

  return(data)
}
