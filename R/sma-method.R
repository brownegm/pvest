
library(S7)

smaMod <- S7::new_class(name = "sma_model",
  properties = list(slope = S7::class_numeric,
                    intercept = S7::class_numeric)
)

smaSlope <- S7:: new_class(name = "smaSlope",
  properties = list(slope = S7::class_numeric))

smaIntercept <- S7:: new_class(name = "smaIntercept",
                           properties = list(intercept = S7::class_numeric))

sma_model <- new_generic('sma_model', "x")



method(sma_model, swc) <- function(x) {

  x@slope <- slope
  x@intercept <- intercept
}

swc <- S7::new_class(name= "satrwc", 
                     properties = list(swm = class_double,
                                       swc = class_double, 
                                       units = class_list)
                     )

osm <- S7::new_class(name= "osm", 
                     parent = sma,
                     properties = list("slope" = smaSlope, 
                                       "intercept" = smaIntercept,
                                       "pi.o" = class_double)
)

# display structure
print(sma)
swc.setunits <- new_generic('swc.setunits', "x")
method(swc.setunits, swc) <- function(x) {
  x@units <- list("swm"="g", "swc"="g/g")
}
data <- data.frame(fw = seq(0.450,0.2, length.out = 10),
                   wp = seq(-0.5,-2.5, length.out = 10),
                   dm = 0.5)


t <- 
swc.setunits(swc)
# set sma method for satwatercontent
sma.swc <- new_generic('sma.swc', "x")

sma.setunits<- new_generic('sma.setunits', "x")

method(sma.setunits, osm) <- function(x) {
  x@slope <- -x@slope
}

sma.swc(c(1:10),c(1:10))

sma.osm <- new_generic('sma.osm', "slope", "intercept")

method(sma.swc, sma) <- function(slope, intercept) {
  
  slope <- -slope
  intercept <- intercept
  
}
