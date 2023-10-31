#' Determine best number of points below turgor loss 
#' 
#' @description
#' This function aims to determine the best number of points by coefficients of determination, coefficients of variation, and estimated leaf osmotic potential at full turgor. 
#' @param data a data frame
#' @param wp.index index number or string containing the column name of the water potential data
#' @param wm.index index number or string containing the column name of the water mass data (i.e., water mass or relative water content)
#' @param max_row set cap on the number of rows to test for best r-squared value. Default to 10 rows of data.
#'
#' @details An alternative to the default number of rows would be to set `max_rows` to number of rows in the `data`. Note that more rows require greater computational time especially if computing parameters across many species. Suggest running the function with the default then, if a greater number of rows need to be tested, rerun the function for a subset of species. 
#' 
#' @return returns a list of 6 containing r2 values, osmotic potentials and coefficients of variation. See below for further information.
#' 
#' \bold{Return values}
#' 
#' \itemize{
#' \item all_r2. Coefficient of determination (R^2^) for each number of rows
#' \item cv. Coefficient of variation (CV) in the slope between increasing number of rows
#' \item r2. N rows where there is the greatest R^2^
#' \item pi_o. N rows that predicts the most negative osmotic potential at full turgor.
#' \item cv10. Rows where the CV in slope in less than 10%
#' } 
#' 
#' 
#' @family internal
#' @importFrom smatr sma
#'
#'@export

check_n_pts<- function(data, wp.index, wm.index, max_row=10, method=c("r2", "pio", "cv10", "all")) {
  
# create vectors to store the values 
r2<-vector()
pio<-vector()
slope<-vector()
slope.cv<-vector()

# for each number of rows estimate model parameters, collect slope and r2, estimate pio and cv of slope.

#for (i in 4:max_row)
mods <- lapply(4:max_row, function(ntail){
  
  tmp<- leaf_estimate %>%
    dplyr::arrange(desc(wp.index)) %>%
    dplyr::slice_tail(n = ntail) %>%
    as.data.frame()
  
  return(smatr::sma(formula=tmp[,wp.index]~tmp[,wm.index], 
             data=tmp))
}
)

if(method=="all"){
  
  r2_vals<-unlist(mods[[mod]]$r2)
  pio[i]<- -1/mod$coef[[1]][1,1]
  slope[i]<-mod$coef[[1]][2,1]
  slope.cv[i]<-sd(c(slope[i], slope[i-1]), na.rm = T)/mean(c(slope[i], slope[i-1]))
  
  
  lowest_pio<-which.min(pio)
  cv.10<-which(abs(slope.cv)<0.10)

    # return a named list of values. 
  return(list(all_r2=r2, 
              all_pio=pio,
              cv=slope.cv,
              r2=greatest_r2,
              pi_o=lowest_pio, 
              cv10=cv.10))
  
}else if(method=="r2"){
  
  r2<-lapply(1:length(mods), function(mod) {
   
     r2_vals<-unlist(mods[[mod]]$r2)

  })

  return(which.max(r2))
  
}else if (method=="pio") {
  
  pio<-lapply(1:length(mods), function(mod) {
    
    pio_vals <- -1/(mods[[mod]]$coef[[1]][1,1])
    
  })

  return(which.min(pio))
  
}else if (method=="cv"){
  
  slopes<-lapply(1:length(mods), function(mod) {
    
    slope<-mods[[mod]]$coef[[1]][2,1]%>%unlist

  })%>%
    dplyrsummarize()
  # make it summarize the slopes list getting the cv between the first and next object 
  
  slopes<-unlist(slopes)
  
  
  
  cv<-sd(c(slope[i], slope[i-1]), na.rm = T)/mean(c(slope[i], slope[i-1]))
  
  
  
  cv.10<-which(abs(slope.cv)<0.10)
  
}

}
