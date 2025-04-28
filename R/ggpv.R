#' Stat function for plotting pv curve on data

#'@importFrom ggplot2 ggplot ggproto Stat
#@inheritParams ggplot2::stat_smooth
#'@export

## Create ggproto object

StatPV <- ggplot2::ggproto("StatPV", ggplot2::Stat, 
            compute_group = function(pvest){
              pvStats <- pvest$stat
            } )
