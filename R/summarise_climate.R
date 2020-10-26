#' Summarises nichemapper microclimate data to cumulative hours
#'
#' Summarises nichemapper microclimate data to cumulative degree hours or
#' cumulative moisture hours between set of census dates
#' 
#' @param file Path to csv file
#' @param census_dates A vector of census dates
#' @param wilting_pt The soil moisture at wilting point. If not included output
#' will be cumulative moisture hours. If included it will be the number of hours
#' below wilting point
#' @param temp_threshold A temperature threshold below which to ignore when
#' accumulating degrees
#' @return tibble
#' @author James Camac (\email{james.camac@gmail.com})
#' @importFrom readr read_csv  cols_only
#' @importFrom dplyr mutate filter group_by ungroup select summarise_all
#' @importFrom magrittr '%>%'
#' @importFrom forcats as_factor fct_inorder
#' @export

summarise_climate <- function(file, census_dates, wilting_pt, temp_threshold) {
  climate <-  readr::read_csv(file = file,
                              col_types = readr::cols_only(
                                site = "i",
                                year = "i",
                                DOY = "i",
                                time = "d",
                                D0cm = "d",
                                D2.5cm = "d",
                                D5cm = "d",
                                D10cm = "d",
                                D15cm = "d",
                                WC10cm = "d",
                                WC15cm = "d",
                                WC20cm = "d",
                                WC30cm = "d")) %>%
    dplyr::mutate(
      site_id = as.integer(forcats::fct_inorder(forcats::as_factor(site))),
      date = as.Date(paste0(year,DOY), format = "%Y%j")) %>%
    # Subset climate to be between experimental dates
    dplyr::filter(date >= min(census_dates) & date <= max(census_dates)) %>%
    # Create census ID
    dplyr::mutate(census =  findInterval(date, 
                                         census_dates, 
                                         rightmost.closed=TRUE))
  
  if(!missing(wilting_pt)) {
    climate <- climate %>%
      dplyr::mutate(Wiltpt_hours_10cm = ifelse(WC10cm < wilting_pt,1,0),
                    Wiltpt_hours_15cm = ifelse(WC15cm < wilting_pt,1,0),
                    Wiltpt_hours_20cm = ifelse(WC20cm < wilting_pt,1,0),
                    Wiltpt_hours_30cm = ifelse(WC30cm < wilting_pt,1,0))
  }
  
  if(!missing(temp_threshold)) {
    climate <- climate %>%
      dplyr::mutate(!!paste0(D0cm_DH_,temp_threshold) := 
                      ifelse(D0cm < temp_threshold,0,D0cm),
                    !!paste0(D2.5cm_DH_,temp_threshold) := 
                      ifelse(D2.5cm < temp_threshold,0, D2.5cm),
                    !!paste0(D5cm_DH_,temp_threshold) := 
                      ifelse(D5cm < temp_threshold,0, D5cm),
                    !!paste0(D10cm_DH_,temp_threshold) := 
                      ifelse(D10cm < temp_threshold,0, D10cm),
                    !!paste0(D15cm_DH_,temp_threshold) := 
                      ifelse(D15cm < temp_threshold,0, D15cm))
  }
  
  climate %>%
    dplyr::select(-c(site, time, DOY, year, date)) %>%
    dplyr::group_by(site_id, census) %>%
    dplyr::summarise_all(sum) %>%
    dplyr::ungroup()
  
}