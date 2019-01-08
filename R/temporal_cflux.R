#' Function to calculate the carbon fluxes using IPCC Tier 1 and 2 methods
#'
#' This function is intended for calculating the CO2 emissions from LULUCF changes
#' following the IPCC guidelines.  -GWP Licence:GPL3
#'
#' @return  data.frame containing carbon emission estimates for specified locations and time periods
#' temporal_cflux()
#' @name temporal_cflux
#'
#' @author Lyna LaPointe, \email{lyna.lapointe@canada.ca} and Arumugam Thiagarajan, \email{arumugam.thiagarajan@canada.ca} and
#'
#' @param tier_level specified the IPCC tier methodology, 1,2 or 3 are valid entries, it is an integer
#' @param activity_data_df data frame containing the activity data,
#' structure of data frame;
#' @param disturbance_matrix_df data frame
#' @param emission_factor_df data frame
#' @param stand_flag standardized rate flag boolean
#' @param full_time_series_flag flag to indicate completion of full time series data, boolean
#' @param start_year starting year for estimation, integer
#' @param end_year ending year for estimation, integer
#'
#' @import tidyverse
#' @import readxl
#' @import ggplot2
#' @import ggthemes
#' @examples
#' temporal_cflux()
#'

library(roxygen2)
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(compare)



# 1. Activity Data
activity_data_df <- read_excel("~/lulucfcarbon/Processed_Input/GM_data_input_T1_NLUC.xlsx", sheet=1)
# 2. Disturbance Matrix
disturbance_matrix_df <- read_excel("~/lulucfcarbon/Processed_Input/GM_data_input_T1_NLUC.xlsx", sheet=2)
# 3. Emission Factors
emission_factor_df <- read_excel("~/lulucfcarbon/Processed_Input/GM_data_input_T1_NLUC.xlsx", sheet=3)



temporal_cflux=function(tier_level,
                        activity_data_df,
                        disturbance_matrix_df,
                        emission_factor_df,
                        stand_flag,
                        full_time_series_flag,
                        start_year,
                        end_year)
{

  # Melt - Location breakdown and Rename columns
  activity_data_df%>%melt(id=c("yr","event","std","cts","int"))%>%
    rename(location=variable, area_ha=value)%>%
    merge(disturbance_matrix_df, by="event")%>%
    merge(emission_factor_df, by="location")%>%
    mutate(affected_area_ha=area_ha/100*affected_area_pourc)->dat# Apply DM to define affected areas

  #Give preprocessing messages
  dat%>%distinct(location)->locations_ad
  emission_factor_df%>%distinct(location)%>%mutate(location=as.factor(location))->locations_ef
  print(paste0("Number of locations found in Activity data: ", nrow(locations_ad)))
  print(paste0("Number of locations found in Emission factors data: ", nrow(locations_ef)))

  #Check whether all emission factors are availablef or all locations
  compared=compare(locations_ad,locations_ef, allowAll = T)
  if(compared$result){
    print("Emission factors found for all locations")
  }else{print("Emission factors not found for all locations. Therefore, corresponding locations will be dropped frome estimation")}

  #Check for start_year and end_year validity
  if(start_year>end_year){
    print("End year is lower than Start year; Program quitting")
    quit()
  }


  # Standardizes rate of change when we don't know the initial year of disturbances (manual selection...)
  dat$affected_area_ha <- case_when(
    dat$std=="yes" ~ dat$affected_area_ha/9.024463,
    dat$std=="no" ~ dat$affected_area_ha)

  # Apply EF to define C (CO2) pool affected, and unc.
  dat$emission_kt <- dat$affected_area_ha*dat$SOC
  dat$emission_unc_kt <- dat$affected_area_ha*dat$SOC_unc

  # Create the time series
  time_series <- data.frame(matrix(ncol = length(c(start_year:end_year))))
  colnames(time_series) <- c(start_year:end_year)

  # Combine dataset and time series
  dat2 <- cbind(dat, time_series)

  # Melt
  m_dat2 <- melt(dat2, id=c(1:21))

  m_dat2$variable=as.numeric(as.character(m_dat2$variable))
  m_dat2%>%rename(emission_yr=variable)%>%select(-c(value))->m_dat2


  # Subset
  #dat3a <- subset(m_dat2, (m_dat2$cts=="no" & m_dat2$emission_yr>=(m_dat2$yr-(m_dat2$int-1)) & (m_dat2$emission_yr<=m_dat2$yr)))
  m_dat2%>%filter(cts=='no'& emission_yr>=yr-int-1 & emission_yr<=yr)->dat3a

  #dat3b <- subset(m_dat2, (m_dat2$cts=="yes" & m_dat2$emission_yr>=(m_dat2$yr-(m_dat2$int-1))))
  m_dat2%>%filter(cts=='yes'& emission_yr>=yr-int-1)->dat3b

  #dat3c <- subset(m_dat2, (m_dat2$cts=="all"))
  m_dat2%>%filter(cts=='all')->dat3c

  # Combine dataset
  dat3 <- rbind(dat3a, dat3b, dat3c)

  # Create empty column for each year of the time series (To be filled with CO2 emissions)
  time_series <- data.frame(matrix(ncol = length(c(start_year,end_year))))
  colnames(time_series) <- c(start_year,end_year)

  # Combine dataset and time series
  dat4 <- cbind(dat3, time_series)

  # Melt
  dat4%>%melt(id=c(1:22))%>%rename(event_yr=variable) %>%
    mutate(emission_kt=emission_kt/int)%>%mutate(event_yr=as.numeric(as.character(event_yr)))->m_dat4 # Distibution of data based on yr



  # Residual effect over the time series (add unc)
  library(stringr)
  m_dat4$value <- case_when(
    m_dat4$event_yr<m_dat4$emission_yr ~ 0,
    m_dat4$event_yr<=(m_dat4$emission_yr+(m_dat4$duration-1))& str_detect(m_dat4$desc, "T1") ~ m_dat4$emission_kt/100*m_dat4$EF_a,
    m_dat4$event_yr<=(m_dat4$emission_yr+(m_dat4$duration-1))& str_detect(m_dat4$desc, "CS") ~ m_dat4$emission_kt/100*(m_dat4$EF_a*exp(m_dat4$EF_b*((m_dat4$event_yr-m_dat4$emission_yr)+1)))
  )

  # Sum
  dat5_sum=aggregate(value~location+event+yr+emission_yr+event_yr, data=m_dat4, FUN=sum)

  # dcast
  dat5_dcast=dcast(dat5_sum, location+event+yr+event_yr~emission_yr)

  # Graph all - bar; # fill can be event or location
  dat5_dcast$event_yr=as.numeric(as.character(dat5_dcast$event_yr))

  print("Estimation process completed successfully")
  return(dat5_dcast)

}


cflux=temporal_cflux(1,activity_data_df,disturbance_matrix_df,emission_factor_df,T,T, 1970,2019)


