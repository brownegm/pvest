#' Stat function for plotting pv curve on data

#'@importFrom ggplot2 ggplot ggproto
#@inheritParams ggplot2::stat_smooth
#'@export

## Create ggproto object

statPV <- ggproto("statPV", Stat, 
            compute_group = function(pvest){
              
              pvStats <- pvest$stat
            } )
