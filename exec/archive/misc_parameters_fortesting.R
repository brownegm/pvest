
  
#roco1<-pv_dat%>%filter(species=="roco" & leaf=="1")

data<-pv_dat%>%filter(species=="roco"& leaf=="4")

#data<-pv_dat%>%filter(species=="alma"& leaf=="2")

fw.index = 5; wp.index = 4; dm.index = 3; n_pts = F

fw.index="fresh.weight"; wp.index= "water.potential"; 
#wp.index="inv.water.potential"; wm.index="relative.water.deficit"; 

# getting leaf estimate ---------------------------------------------------

# create unique ID and add inverse psi
data$unique_id <- paste(data$species, data$leaf, sep = "_")
data$inv.water.potential <- -1 / (data[[wp.index]])

unique_ids <- unique(data$unique_id)

output_est <- list() # list of estimates for each unique id

d_names <- names(data)

i<-unique_ids[1]

leaf_estimate <- data[data$unique_id == i, ]
  

swc_swm_est <- SaturatedWaterContent(leaf_estimate, fw.index = fw.index, wp.index = wp.index, dm.index = dm.index)
  
  leaf_estimate[, "saturated.water.mass"] <- swc_swm_est[[1]]
  
  leaf_estimate[, "saturated.water.content"] <- swc_swm_est[[2]]
  
  leaf_estimate[, c("relative.water.content", "relative.water.deficit")] <- RelativeWaterCD(leaf_estimate, fw.index = fw.index)
  
  
  
  # pts.vec = check_n_pts(leaf_estimate, wp.index="inv.water.potential", wm.index="relative.water.deficit", method = metho)  

  pts.vec = 4
  pts =4 
  
  tlp_est <- OsmoticEstimates(data = leaf_estimate, wc.index = "relative.water.deficit", wp.index = "inv.water.potential", n_row = pts)%>%
    EstimateTLP(., wc.index = "relative.water.deficit", wp.index = "inv.water.potential", n_row_below=pts) %>%
    dplyr::pull(leaf.waterpotential.attlp) %>%
    unique()
  
  
  ## n rows above and below this tlp estimate
  row_above_tlp <- leaf_estimate %>%
    dplyr::filter(water.potential> tlp_est) %>%
    nrow()
  row_below_tlp <- leaf_estimate %>%
    dplyr::filter(water.potential< tlp_est) %>%
    nrow()
 
   if(row_above_tlp|row_below_tlp>nrow(data)-4){
    row_below_tlp = 4
    row_above_tlp = nrow(data)-row_below_tlp
  }else{
    
  }
  
  leaf_estimate <- OsmoticEstimates(data = leaf_estimate, wc.index = "relative.water.deficit", wp.index = "inv.water.potential", n_row = row_below_tlp) # osmotic variables are estimated based on inv.psi vs RWD below TLP
  
  leaf_estimate <- EstimateTLP(df = leaf_estimate, wc.index = "relative.water.deficit", wp.index = "inv.water.potential", n_row_above = row_above_tlp, n_row_below = row_below_tlp)
  
  
## 
  df<-leaf_estimate
  # define function that returns the SSE
  calcSSE <- function(x){
    loessMod <- try(loess(inv.water.potential ~ relative.water.deficit, data=df, span=x), silent=F)
    res <- try(loessMod$residuals, silent=T)
    if(class(res)!="try-error"){
      if((sum(res, na.rm=T) > 0)){
        sse <- sum(res^2)  
      }
    }else{
      sse <- 99999
    }
    return(sse)
  }
  
  # Run optim to find span that gives min SSE, starting at 0.5
  optim(par=c(0.5), calcSSE, method="SANN")

## loess fit
  
  # Assuming your data is a dataframe named 'df'
  # Replace 'x' and 'y' with your actual column names
  
  # Fit a LOESS curve to the data
  loess_fit <- loess(inv.water.potential ~ relative.water.deficit, data = leaf_estimate)
  
  # Predict values using the LOESS model
  y_pred <- predict(loess_fit)
  
  z<- diff(y_pred)/diff(leaf_estimate$relative.water.deficit)
  
  
  second_deriv_loess <- sec_der (fit = loess_fit, leaf_estimate$relative.water.deficit)

  # Visualize the LOESS fit
  plot(leaf_estimate$relative.water.deficit, leaf_estimate$inv.water.potential, main = "LOESS Fit", xlab = "X", ylab = "Y", col = "blue")
  lines(leaf_estimate$relative.water.deficit, y_pred, col = "red", lwd = 2)
  
 
  
  
  best_fit <- NULL
  best_metric <- 0
  
  for (i in 2:(nrow(leaf_estimate) - 1)) {
    
    model1 <- fit_linear_model(leaf_estimate, 1, i)
    model2 <- fit_linear_model(leaf_estimate, i + 1, nrow(leaf_estimate))
    
    metric1 <- assess_fit(model1)
    metric2 <- assess_fit(model2)
    
    # Use a metric to compare the fits (e.g., sum of R-squared values)
    total_metric <- metric1 + metric2
    
    if (total_metric > best_metric) {
      best_metric <- total_metric
      best_fit <- list(above = model1, below = model2)
    }
  }
  

# test piecewise regression -----------------------------------------------
#source(here("R", "piecewise_regression.R"))

  mod_bestfit <- piecewise_reg(leaf_estimate)

  ?piecewise_reg
  
  
  
# Visualize the best fit
  plot(leaf_estimate$relative.water.deficit, leaf_estimate$inv.water.potential, main = "Piecewise Linear Regression", xlab = "X", ylab = "Y", col = "blue")
  abline(mod_bestfit$above, col = "red", lwd = 2)
  abline(mod_bestfit$below, col = "green", lwd = 2) 
  
  abline(best_fit$above, col = "red", lwd = 2)
  abline(best_fit$below, col = "green", lwd = 2)
# other -------------------------------------------------------------------


# ROCO 1 & 4 , ALMA 2 problems as of 20230719


max_row=nrow(data)
plot(leaf_estimate$relative.water.deficit, leaf_estimate$inv.water.potential)

ggplot(data=leaf_estimate, mapping = aes(x=relative.water.deficit, y=inv.water.potential))+
  geom_point(color="red",fill="blue", size=5, shape=21)+
  labs(y="Inverse Water Potential (1/Psi)", 
       x="Relative Water Deficit (%)")+
  geom_smooth()+
  theme_bw()

vignette("ggplot2-specs")

leaf_estimate

t<-check_n_pts(pv_params_r2, wp.index="inv.water.potential", wm.index="relative.water.deficit", method = "all")#$cv10


pv_params_fil<-pv_params%>%filter(is.na(modulus))

plot

kl