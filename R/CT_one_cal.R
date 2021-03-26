#' Calculate temperature-compensated Specific Conductance and one-point calibration for CT logger data
#'
#' This function reads in a dataframe containing electrical conductivity logs from a conductivity-temperature logger
#' and calculates specific conductance from in situ electrical conductivity and temperature readings.
#'
#' Nonlinear temperature compensation is calculated using the EC to SC equation from
#' https://www.aqion.de/site/112
#'
#' @param data The dataframe containing raw Electrical Conductivity values
#' @param date Date column used to filter for calibration data only
#' @param temp Temperature column used to calculate specific conductance from electrical conductivity
#' @param EC Electrical conductivity column used to calculate specific conductance
#' @param cal_reference The conductivity calibration solution specific conductance value, uS/cm at 25degC
#' @param startCal Date and time at the start of the calibration
#' @param endCal Date and time at the end of the calibration
#' @return The original dataframe with the newly calculated Specific Conductance and calibrated SC values of CT logger data
#' @export
CT_one_cal<-function(data, date, temp, EC, cal_reference, startCal, endCal) {

  ############################################################
  ### Nonlinear Temperature Compensation
  ############################################################

  # source: https://www.aqion.de/site/112#:~:text=The%20electrical%20conductivity%20(EC)%20of,the%20reference%20temperature%20at%2025.
  data<-data %>%
    mutate(A = (1.37023 * ({{temp}} - 20)) + 8.36 * (10^(-4) * (({{temp}} - 20)^2))) %>%
    mutate(B = 109 + {{temp}}) %>%
    mutate(Sp_Conductance = 0.889 * (10^(A/B)) * {{EC}})


  ############################################################
  ### One Point Calibration
  ############################################################

  # Logger data in pre-deployment calibration
  Cal.Log<-data%>%
    filter(between({{date}},{{startCal}},{{endCal}}))%>%
    summarise(mean(Sp_Conductance))%>%
    as.numeric

  # Offset between the calibration reference and the logger reading
  offset<-cal_reference-Cal.Log

  # Apply offset to logger data
  data<-data%>%
    mutate(Sp_Conductance_cal=Sp_Conductance+offset)

  return(data)
}
