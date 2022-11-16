#Contains functions for estimating the leaf osmotic parameters from the dataset with saturated and relative water content 
#already determined. 
#' @description Estimates the osmotic potential at full turgor from the linear relationship
#'     between relative water deficit and inverse leaf water potential. 
#'
#' @param data 
#' @param fw.index 
#' @param wp.index 
#'
#' @return Numeric list containing the slope, intercept and osmotic potential at full turgor, in that order. 
#'
#' @export
#'

OsmoticPotFullTurgor <- function(data, fw.index, wp.index) {
  
  output<-list()
  
  slope <- sma_slope(x = data[, fw.index], y = data[, wp.index])
  
  intercept <- sma_intercept(x = data[, fw.index], y = data[, wp.index], slope = -slope)
  
  pi.o <- -1/intercept
  
  output<-c(slope, intercept, pi.o)
  
  return(output)
  
}

NULL

#' @description Estimate the osmotic potential at full turgor, pressure potential at full turgor for leaves 
#'     as well as the osmotic potential and pressure potential for each hydration state
#'     
#'     This function is written on the values \strong{below} turgor loss point. Here we assume that the data points at
#'     last 4 hydration states all represent points below turgor loss point. 
#'     
#'     It is implemented \strong{after} estimation of leaf saturated water content, relative water content and relative 
#'     water deficit. @seealso [RelativeWaterCD] for information on the estimation of those parameters. 
#'     
#'     Here, we estimate the osmotic potential at full turgor $\pi_{O}$ as the x-intercept of the relationship between
#'     inverse leaf water potential and relative water deficit below turgor loss point. The slope needs to be negative 
#'     within the sma_intercept function(@seealso [sma_intercept]) because of the negative inverse of leaf water potential.
#'     Also, note that unlike in the saturated water content estimation the x values is inverse $\Psi_{leaf}$. 
#'     
#'
#' @param data data frame containing the data set of the last 4 hydration states for a given leaf
#' @param wp.index data frame index which contains the leaf water potential data
#' @details The data frame must contain four points below turgor loss points. These values of relative water deficit
#' 
#' @return Returns data frame with new columns containing the osmotic and pressure potential variables namely: 
#'     * osmotic potential at full turgor
#'     * maximum pressure potential 
#'     * osmotic potential (for each hydration state)
#'     * pressure potential (for each hydration state)
#'     * apoplastic fraction
#'     * symplastic relative water content(sym.rwc; for each hydration state)
#'     
#' @export OsmoticEstimates
#'

OsmoticEstimates<- function(data, fw.index, wp.index) {
   
    pi.o_list <- OsmoticPotFullTurgor(data, fw.index, wp.index)
    
    data$osm.pot.fullturgor <- pi.o_list[3]
    data$max.psip <- data$osm.pot.fullturgor * -1
    data$osmotic.potential <- -1/(pi.o_list[2]+pi.o_list[1]*data$relative.water.deficit)
    data$pressure.potential <- data[,wp.index]-data$osmotic.potential
    data$apoplastic.fraction<-100+(pi.o_list[2]/pi.o_list[1])
    data$sym.rwc<-(data$relative.water.content-(data$apoplastic.fraction/100))/(1-(data$apoplastic.fraction/100))*100
    
    return(data)
}
