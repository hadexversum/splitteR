#' 
#' 
#' @export
plot_comparison_backexchange <- function(bex_dat){
  
  state <- unique(bex_dat[["State"]])
  
  ggplot(bex_dat) + 
    geom_segment(aes(x = Start, xend = End, y = back_exchange, color = nchar(Sequence)), size = 2) +
    labs(x = "Sequence position", 
         y = "back-exchange [%]",
         title = paste0("Back-exchange for ", state),
         color = "Peptide length") +
    ylim(c(0, NA)) +
    theme_bw(base_size = 18) +
    theme(legend.position = "bottom")
  
}