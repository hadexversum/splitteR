#' 
#' @examples
#' bex_dat <- HaDeX2::calculate_back_exchange(alpha_dat, state = "Alpha_KSCN") 
#' 
#' 
#' @export
plot_comparison_backexchange <- function(bex_dat, 
                                         interactive = FALSE){
  
  state <- unique(bex_dat[["State"]])
  
  geom_segment_chosen <- if(interactive){
    geom_segment_interactive(aes(x = Start, xend = End, 
                                 y = back_exchange, yend = back_exchange,
                                 tooltip = glue(
                                   "Sequence: {Sequence}
                                   Position: {Start}-{End}
                                   BEX: {round(back_exchange, 2)}
                                   Err(BEX): {round(err_back_exchange, 2)}"
                                 ),
                                 color = nchar(Sequence)), linewidth = 2) 
    
  } else {
    geom_segment(aes(x = Start, xend = End, y = back_exchange, color = nchar(Sequence)), linewidth = 2) 
  }
  
  ggplot(bex_dat) + 
    geom_segment_chosen + 
    labs(x = "Protein sequence", 
         y = "back-exchange [%]",
         title = paste0("Back-exchange for ", state),
         color = "Peptide length") +
    ylim(c(0, NA)) +
    theme_bw(base_size = 18) +
    theme(legend.position = "bottom")
  
}