#' @export
plot_subs_cov <- function(dat, color = NULL){
  
  dat <- as.data.table(dat)
  
  dat <- dat[, .(Start, End, Sequence, common)]
  
  dat <- dat[!duplicated(dat)]
  dat[, Len := - End + Start]
  setorderv(dat, cols = c("Start", "Len"))
  
  levels <- rep(NA, (nrow(dat)))
  levels[1] <- 1
  
  start <- dat[["Start"]]
  end <- dat[["End"]]
  
  for(i in 1:(nrow(dat) - 1)) {
    
    for(level in 1:max(levels, na.rm = TRUE)) {
      
      if(all(start[i + 1] > end[1:i][levels == level] | end[i + 1] < start[1:i][levels == level], na.rm = TRUE)) {
        
        levels[i + 1] <- level
        break
        
      } else {
        if(level == max(levels, na.rm = TRUE)) {
          levels[i + 1] <- max(levels, na.rm = TRUE) + 1
        } 
      }
    }
  }
  
  dat[, ID := levels]
  
  coverage_plot <- ggplot(data = dat) +
    geom_rect(data = dat, 
              mapping = aes(xmin = Start, 
                            xmax = End, 
                            ymin = ID, 
                            ymax = ID - 1,
                            fill = common), 
              colour = "black", 
              alpha = 0.8) +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "bottom") +
    labs(title = "Peptide coverage",
         x = "Position",
         y = "")
  
  return(HaDeXify(coverage_plot))

}

