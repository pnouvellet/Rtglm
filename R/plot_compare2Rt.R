#' Hello World
#'
#' This is an example of how to create and document exported functions.
#'
#' @param input you should always document the paramters.
#'              Including the expected data type.
#'
#' @export

plot_compare2Rt <- function(a, b, t_window, overlap, 
                            Corr = FALSE, threshold = NA){
  # for title
  o <- 'non-overlapping'
  o[overlap] <- 'overlapping'
  mtit <- paste0('tw = ',t_window,'; ',o)
  
  # remove infinite value (replaced by 1e4)
  y_Inf <- which(!is.finite(b$high_Quantile) &
                   !is.na(b$high_Quantile))
  b$high_Quantile[y_Inf] <- 1e4
  
  # plot a
  plot(a$t, a$Mean, 
       xlim = range(c(a$t,b$t)),
       ylim = c(0,max(c(b$high_Quantile[-y_Inf],
                        a$high_Quantile),na.rm = TRUE)), 
       bty = 'n',type = 'l', xlab = 'time',ylab = 'Rt')
  abline(h=1,lty=2,col='blue3')
  
  polygon(c(a$t,rev(a$t)), 
          c(a$low_Quantile,rev(a$high_Quantile)),
          border = NA,col = rgb(0,0,0,.1))
  
  # plot b
  lines(b$t,b$Mean,col = 'red3')
  polygon(c(b$t,rev(b$t)), 
          c(b$low_Quantile,rev(b$high_Quantile)),
          border = NA,col = rgb(1,0,0,.1))
  
  mtext(text = mtit, side = 3, line = 1)
  legend('topright',legend = c('EpiEstim','GLM'), 
         lwd = 2, col = c('black','red3'), bty='n')
  
  
  
  if(Corr){
    # correlations
    y_Inf <- which(b$high_Quantile==1e4)
    b$high_Quantile[y_Inf] <- NA
    data_m <- merge(a,b,by='t')
    Corr <- data.frame( mean = cor(x = data_m$Mean.x, 
                                   y = data_m$Mean.y, 
                                   method = c("pearson"),use = "pairwise.complete.obs"),
                        low = cor(x = data_m$low_Quantile.x, 
                                  y = data_m$low_Quantile.y, 
                                  method = c("pearson"),use = "pairwise.complete.obs"),
                        high = cor(x = data_m$high_Quantile.x, 
                                   y = data_m$high_Quantile.y, 
                                   method = c("pearson"),use = "pairwise.complete.obs") )
    return(Corr)
  }
}