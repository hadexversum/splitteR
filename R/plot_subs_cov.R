#' @importFrom data.table as.data.table
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom ggiraph girafe geom_rect_interactive
#' @importFrom glue glue
#' 
#' @examples
#' subsections <- create_subsections(alpha_dat)
#' plot_subs_cov(dat = rename(subsections, Sequence = sub_sequence, Start = sub_start, End = sub_end))
#' 
#' 
#' 
#' 
#' @export
plot_subs_cov <- function(dat,
                          interactive = FALSE){
  
  dat <- as.data.table(dat)
  
  # dat <- dat[, .(Start, End, Sequence, common)]
  
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
  
  if(interactive){
    
    chosen_rect <- geom_rect_interactive(data = dat[!is.na(id)], 
                            mapping = aes(xmin = Start, 
                                          xmax = End +1, 
                                          ymin = ID, 
                                          ymax = ID - 1,
                                          tooltip = glue("Sequence: {Sequence}
                                                      Position: {Start}-{End}
                                                      Original peptide"),
                                          fill = common),
                            colour = "black", 
                            alpha = 0.8) 
    
    chosen_rect_2 <- geom_rect_interactive(data = dat[is.na(id)], 
                            mapping = aes(xmin = Start, 
                                          xmax = End +1, 
                                          ymin = ID, 
                                          ymax = ID - 1,
                                          tooltip = glue("Sequence: {Sequence}
                                                      Position: {Start}-{End}
                                                      Longer peptide: {longer_sequence} ({longer_start}-{longer_end})
                                                      Shorter peptide: {shorter_sequence} ({shorter_start}-{shorter_end})"),
                                          fill = common),
                            colour = "black", 
                            alpha = 0.8)
    
    
  } else {
    chosen_rect <- geom_rect(data = dat, 
                             mapping = aes(xmin = Start, 
                                           xmax = End +1, 
                                           ymin = ID, 
                                           ymax = ID - 1,
                                           fill = common), 
                             colour = "black", 
                             alpha = 0.8) 
    chosen_rect_2 <- geom_rect()
  }
  
  coverage_plot <- ggplot(data = dat) +
    chosen_rect +
    chosen_rect_2 + 
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "bottom") +
    labs(title = "Peptide coverage",
         x = "Position",
         y = "")
  
  return(HaDeXify(coverage_plot))

}

