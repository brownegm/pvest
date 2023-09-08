#' Check the number of rows that produces the highest R^2 values
#'
#' @param data a dataframe
#' @param wp.index index number or string containing the column name of the water potential data
#' @param wm.index index number or string containing the column name of the water mass data (i.e., water mass or relative water contentt)
#' @param max_row set cap on the number of rows to test for best r-squared value. Default to number of rows in the `data`
#'
#' @return returns a list of two with the number of rows that produces the greatest r-squared (r2) or lowest pi_o value
#' 
#' @family internal
#'
#' @export

check_n_pts <- function(data, wp.index, wm.index, max_row = nrow(data)) {
    # create vectors to store the values
    r2 <- vector()
    pio <- vector()
    slope <- vector()
    slope.cv <- vector()
    
    # for each number of rows estimate model parameters, collect slope and r2, estimate pio and cv of slope.
    mods<-lapply(4:max_row, function(rows) smatr::sma(formula = inv.water.potential ~ relative.water.deficit, 
                                                   data = slice_tail(leaf_estimate, n = rows)))
    
    output<-lapply(mods,
                   function(mod) {
                     list(
                     r2 = unlist(mod$r2),
                     pio = -1 / mod$coef[[1]][1, 1],
                     slope = mod$coef[[1]][2, 1] )
                     #slope.cv <- sd(c(slope, slope[mod - 1]), na.rm = T) / mean(c(slope[mod], slope[mod - 1]))
                   
                    # list()
                     }
                     )
    # 
    for (i in 4:max_row) {
      tmp <- data %>%
        #dplyr::arrange(desc(wp.index)) %>%
        dplyr::slice_tail(n = i) %>%
        as.data.frame()
      
      mod <- smatr::sma(formula = tmp[, wp.index] ~ tmp[, wm.index],
                        data = tmp)
      
      r2[i] <- mod$r2 %>% unlist
      pio[i] <- -1 / mod$coef[[1]][1, 1]
      slope[i] <- mod$coef[[1]][2, 1]
      slope.cv[i] <- sd(c(slope[i], slope[i - 1]), na.rm = T) / mean(c(slope[i], slope[i - 1]))
      
      greatest_r2 <- which.max(r2)
      lowest_pio <- which.min(pio)
      cv.10 <- which(abs(slope.cv) < 0.10)
      
    }
    # return a named list of values.
    return(
      list(
        all_r2 = r2,
        all_pio = pio,
        cv = slope.cv,
        r2 = greatest_r2,
        pi_o = lowest_pio,
        cv10 = cv.10
      )
    )
    
  }
