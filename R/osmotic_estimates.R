#Contains functions for estimating the leaf osmotic parameters from the dataset with saturated and relative water content 
#already determined. 
#' Osmotic potential at full turgor estimate
#' 
#' @description Estimates the osmotic potential at full turgor from the linear relationship
#'     between relative water deficit and inverse leaf water potential. 
#'
#' @param data A data frame. A data frame containing the data set of the last 4 hydration states for a given leaf 
#' @param wc.index A double. Data frame index which contains the relative water content/deficit
#' @param wp.index A double. Data frame index which contains the leaf water potential data
#'
#' @return Numeric list containing the slope, intercept and osmotic potential at full turgor, in that order. 
#'
#' @export
#' 
#' @seealso \code{\link{sma_slope}}, \code{\link{sma_intercept}}
#'

OsmoticPotFullTurgor <- function(data, wc.index, wp.index) {
  
  output<-list()
  
  slope <- -sma_slope(x = data[, wp.index], y = data[, wc.index])
  
  intercept <- sma_intercept(x = data[, wp.index], y = data[, wc.index], slope = slope)
  
  pi.o <- -1/intercept
  
  output<-c(slope, intercept, pi.o)
  
  return(output)
  
}

NULL

#' Osmotic and pressure parameter estimation
#'@description Estimate the osmotic potential at full turgor, pressure potential at full turgor for leaves 
#'     as well as the osmotic potential and pressure potential for each hydration state.
#' 
#' @param data A data frame. A data frame containing the data set of the last 4 hydration states for a given leaf
#' @param wc.index A double. A data frame index which contains the water content data passed through OsmoticPotFullTurgor function
#' @param wp.index A double. A data frame index which contains the leaf water potential data
#' @param n_row A double. Value which indicates the number of rows for estimating parameters. Default to 4 rows. 
#'     
#'@details
#'This function estimates osmotic variables from the values \strong{below} turgor loss point. Here we assume that the data points at
#'last 4 hydration states all represent points below turgor loss point. 
#'     
#'It is implemented \strong{after} estimation of leaf saturated water content, relative water content and relative 
#'water deficit.See `RelativeWaterCD` for information on the estimation of those parameters. 
#'     
#'Here, we estimate the osmotic potential at full turgor as the x-intercept of the relationship between
#'inverse leaf water potential and relative water deficit below turgor loss point. The slope needs to be negative 
#'within the sma_intercept function(\code{\link{sma_intercept}}) because of the negative inverse of leaf water potential.
#'Also, note that unlike in the saturated water content estimation the x values is inverse. 
#'
#'      
#' @return Returns data frame with new columns containing the osmotic and pressure potential variables namely: 
#'    \itemize{
#'     \item osmotic potential at full turgor
#'     \item maximum pressure potential 
#'     \item osmotic potential (for each hydration state)
#'     \item pressure potential (for each hydration state)
#'     \item apoplastic fraction
#'     \item symplastic relative water content(sym.rwc; for each hydration state)
#'     }
#'
#' @import dplyr 
#' @export OsmoticEstimates
#' @seealso [RelativeWaterCD()], [sma_intercept()]

OsmoticEstimates<- function(data, wc.index="relative.water.deficit", wp.index="inv.water.potential", n_row=4) {
   
   data_belowtlp<-data%>% 
    dplyr::arrange(desc(wp.index))%>%
    dplyr::slice_tail(n=n_row)%>%as.data.frame()
  
    pi.o_list <- OsmoticPotFullTurgor(data_belowtlp, wc.index, wp.index)
    
    data$osm.pot.fullturgor <- pi.o_list[3]
    data$max.psip <- data$osm.pot.fullturgor * -1
    data$osmotic.potential <- -1/(pi.o_list[2]+pi.o_list[1]*data$relative.water.deficit)
    data$pressure.potential <- data$water.potential-data$osmotic.potential
    data$apoplastic.fraction<-100+(pi.o_list[2]/pi.o_list[1])
    data$sym.rwc<-((data$relative.water.content-(data$apoplastic.fraction))/(100-(data$apoplastic.fraction)))*100
    data$sym.rwd<-100-data$sym.rwc
    
    return(data)
}
