#' Calculates back-exchange based 
#' on the Hamuro table
#' 
#' @examples
#' calculate_hrate("GFGDLKSPAGL")
#' 
#' @export
calculate_hrate <- function(sequence){
  
  n_res <- nchar(sequence)
  residues <- strsplit(sequence, split = "")[[1]]
  
  v_hrates <- lapply(1:n_res-1, function(i){
    if(i == 1){
      rate_n[residues[i], residues[i+1]]
      
    } else if(i == n_res-1) {
      rate_c[residues[i], residues[i+1]]
    } else {
      # if(residues[i-1] == "P" |residues[i] == "P") {
      #   0
      # }
      # else {
      #   
      # }
      rate_m[residues[i], residues[i+1]]
      
    }
  }) %>% unlist(.)
  
  return(sum(v_hrates))
  
}

#' Calculates h rates for all residues 
#' within the peptide
#' 
#' @param sequence peptide sequence
#' @param start start position of the peptide
#' @param end end position of the peptide
#' 
#' @description
#' If the start value is not provided, it is assumed that the 
#' start position is 1, with the end position depending on the 
#' sequence length.
#' 
#'  
#' @examples
#' get_peptide_hrates("GFGDLKSPAGL")
#' 
#' 
#' @export
get_peptide_hrates <- function(sequence,
                               start = 1,
                               end = nchar(sequence)){
  
  n_res <- nchar(sequence)
  residues <- strsplit(sequence, split = "")[[1]]
  
  v_hrates <- lapply(1:n_res, function(i){

   x <- if(i == 1){ 
      0 
   } else if(residues[i] == "P") {
     0
    } else if(i == 2){ 
        rate_n[residues[i-1], residues[i]] 
    } else if(i == n_res){
        rate_c[residues[i-1], residues[i]]
      } else {  rate_m[residues[i-1], residues[i]] }
   
    # print(paste0(i, " ", residues[i]))
    # print(x)
    
   x
   
  }) %>% unlist(.)
 
  data.frame(res = residues, 
             h_rate = v_hrates,
             pos = start:end)
   
}

#' @export
plot_hrates_heatmap <- function(dat){
  
  pep_dat <- dat %>%
    select(Sequence, Start, End) %>% unique(.) %>% mutate(id = 1:nrow(.)) %>%
    filter(End < 100)
  
  plot_dat <- lapply(1:nrow(pep_dat), function(i){
    
    get_peptide_hrates(sequence = pep_dat[[i, "Sequence"]],
                       start = pep_dat[[i, "Start"]],
                       end = pep_dat[[i, "End"]]) %>%
      mutate(id = pep_dat[[i, "id"]])
    
  }) %>% bind_rows()
  
  ggplot(plot_dat) +
    geom_rect(aes(xmin = pos, xmax = pos+1,
                  ymin = id, ymax = id+1,
                  fill = h_rate)) +
    geom_text(aes(x = pos + 0.5, y = 0, label = res)) +
    theme(legend.position = "bottom",
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) +
    labs(y = "",
         x = "Protein sequence") +
    scale_fill_gradient2(low = "deeppink1", mid = "white", high = "deepskyblue", midpoint = 0.3) 
  
  
}

plot_sequence_hrates_heatmap <- function(dat){
  
  ## to slow
  residues <- HaDeX2::get_residue_positions(dat)

  v_hrates <- lapply(1:nrow(residues), function(i){
    
    # print(paste0(i, residues[i, "aa"]))
    
    if(i == 1){ 
      0 
    } else if(is.na(residues[i, "aa"])){ 
      NA
      } else if(residues[i, "aa"] == "P") {
      0
    } else if(i == 2){ 
      rate_n[residues[i-1, "aa"], residues[i, "aa"]] 
    } else if(i == n_res){
      rate_c[residues[i-1, "aa"], residues[i, "aa"]]
    } else if(is.na(residues[i-1, "aa"])){
      NA
    } else {  rate_m[residues[i-1, "aa"], residues[i, "aa"]] }
    
    
  })  
  
  residues["h_rate"] <- unlist(v_hrates)
  
  ggplot(residues) +
    geom_rect(aes(xmin = position-0.5, xmax = position+0.5,
                  ymin = 1, ymax = 2,
                  fill = h_rate)) +
    geom_text(aes(x = position, y = 1, label = aa)) +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    labs(y = "",
         x = "Protein sequence") +
    scale_fill_gradient2(low = "deeppink1", mid = "white", high = "deepskyblue", midpoint = 0.3) 
  
}

