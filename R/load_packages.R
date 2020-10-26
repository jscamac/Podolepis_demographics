#' loads packages and installs missing packages
#'
#' loads packages and installs missing packages
#' @param packages Character vector of required package names
#' @param install_missing Logical. Installs missing packages. Default = TRUE.
#' @param pkg_date Character. The date used to install missing packages. This ensures package versions present on that particular date are installed. 
#' Dates must be of the following format "YYYY-MM-DD". Defaults to "2019-07-23". 
#' @return Loads and installs required packages
#' @details Note this package installs missing packages using the version present on MRAN on the pkg_date. 
#' This function will not overwrite preinstalled packages. 
#' @author James Camac (\email{james.camac@gmail.com})
#' @export
load_packages <- function(packages, pkg_date = "2019-09-23", install_missing=TRUE) {
	# Turn off AWT. This causes bugs with some java based packages such as OpenStreetMap
	Sys.setenv(NOAWT=1)

	# What packages have been installed?
  installed_pkgs <- rownames(installed.packages())
  
  # Install devtools (required by function)
  if("devtools" %in% installed_pkgs == FALSE) {
    install.packages('devtools', repos = "cran.csiro.au")
  }

# Install missing packages
for(p in packages) {
  if(p %in% installed_pkgs == FALSE) {
    install.packages(p, repos=paste0("https://mran.revolutionanalytics.com/snapshot/",pkg_date,"/"))
  }
}

# Load packages
sapply(packages, require, character.only = TRUE)
}
