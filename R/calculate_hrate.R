#' Calculates back-exchange based 
#' on the Hamuro table
#' 
#' @examples
#' calculate_hrate("GFGDLKSPAGL")
#' calculate_hrate(toupper("gfGDLKSpAGL"))
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
    
    if(i == 1){ 
      NA 
    } else if(is.na(residues[i])){ 
      NA
    } else if(residues[i] == "P") {
      NA
    } else if(i == 2){ 
      rate_n[residues[i-1], residues[i]] 
    } else if(i == n_res){
      rate_c[residues[i-1], residues[i]]
    } else if(is.na(residues[i-1])){
      NA
    } else {  rate_m[residues[i-1], residues[i]] }
   
  }) %>% unlist(.)
 
  data.frame(res = residues, 
             h_rate = v_hrates,
             pos = start:end)
   
}

#' @importFrom patchwork wrap_plots
#' 
#' @examples
#' plot_hrates_heatmap(filter(alpha_dat, End < 100))
#' 
#' 
#' @export
plot_hrates_heatmap <- function(dat,
                                hamuro_threshold = 0.3,
                                panel_length = 50){
  
  
  
  pep_dat <- dat %>%
    select(Sequence, Start, End) %>% unique(.) %>% mutate(id = 1:nrow(.)) 
  
  plot_dat <- lapply(1:nrow(pep_dat), function(i){
    
    get_peptide_hrates(sequence = pep_dat[[i, "Sequence"]],
                       start = pep_dat[[i, "Start"]],
                       end = pep_dat[[i, "End"]]) %>%
      mutate(id = pep_dat[[i, "id"]])
    
  }) %>% bind_rows()
  
  n_panels = ceiling(max(plot_dat[["pos"]])/panel_length)
  
  plts <- lapply(1:n_panels, function(i){
    
    panel_start <- (i-1)*panel_length
    panel_end <- i*panel_length
    
    tmp_plot_dat <- filter(plot_dat, 
                           pos >= panel_start, 
                           pos <= panel_end)
    
    min_id = min(tmp_plot_dat[["id"]])
    max_id = max(tmp_plot_dat[["id"]])
    
    plt <- ggplot(tmp_plot_dat) +
      geom_rect(aes(xmin = pos - 0.5, xmax = pos + 0.5,
                    ymin = id, ymax = id+1,
                    fill = h_rate)) +
      geom_text(aes(x = pos, y = min_id - 2.5, label = res)) +
      geom_text(aes(x = pos, y = max_id + 3 , label = res)) +
      theme_minimal() +
      ylim(c(min_id - 3, max_id + 3)) +
      xlim(c(panel_start - 1, panel_end + 1)) + 
      theme(legend.position = "none",
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            text = element_text(size = 14),
            panel.background = element_rect(fill = "grey98", color = NA)) +
      labs(y = "",
           x = "") +
      scale_fill_gradient2(low = "deeppink1", mid = "pink", high = "deepskyblue1", midpoint = hamuro_threshold,
                           na.value = "grey90") 
    
    if(i == n_panels){
      plt <- plt + 
        theme(legend.position = "bottom") +
        labs(x = "Protein sequence",
             fill = "Retention rate")
    }
    plt
    
  }) 
  
  wrap_plots(plts, ncol = 1)
  
}


#' @examples
#' plot_hrates_heatmap(filter(alpha_dat, End < 100))
#' 
#' @description
#' first version 
#' 
#' @export
plot_hrates_heatmap_v2 <- function(dat,
                                hamuro_threshold = 0.3){
  
  pep_dat <- dat %>%
    select(Sequence, Start, End) %>% unique(.) %>% mutate(id = 1:nrow(.)) 
  
  plot_dat <- lapply(1:nrow(pep_dat), function(i){
    
    # print(pep_dat[[i, "Sequence"]])
    get_peptide_hrates(sequence = pep_dat[[i, "Sequence"]],
                       start = pep_dat[[i, "Start"]],
                       end = pep_dat[[i, "End"]]) %>%
      mutate(id = pep_dat[[i, "id"]])
    
  }) %>% bind_rows()
 
  seq_length <- max(plot_dat[["pos"]])
  new_length <- ceiling(seq_length / 50) * 50
  
  max_id <- max(plot_dat[["id"]])
  
  
  plot_dat$range <- cut(
    plot_dat$pos,
    breaks = seq(0, new_length, by = 50),
    right = FALSE,
    include.lowest = TRUE
  )
  
  ##
  ## to fix the length of last panel
  
  null_positions <- new_length - seq_length
  
  sequence <- plot_dat %>%
    select(pos, res) %>%
    unique() %>%
    bind_rows(data.frame(pos = (seq_length+1):new_length,
                         res = rep("", null_positions))) %>%
    .[["res"]]
  
  mocked_dat <- data.frame(
    res = rep("", null_positions),
    h_rate = rep(NA_real_, null_positions),
    pos = (seq_length+1):new_length,
    id = rep(max_id+1, null_positions)
  )
  
  mocked_dat$range <- cut(
    mocked_dat$pos,
    breaks = seq(0, new_length, by = 50),
    right = FALSE,
    include.lowest = TRUE
  )
  
  ##
  
  plt <- ggplot(plot_dat) +
    geom_rect(aes(xmin = pos - 0.5, xmax = pos + 0.5,
                  ymin = id, ymax = id+1,
                  fill = h_rate)) +
    geom_rect(data = mocked_dat, aes(xmin = pos - 0.5, xmax = pos + 0.5,
                                     ymin = max_id - 1, ymax = max_id - 1), fill = "grey98") +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = "grey98", color = NA),
          strip.text = element_blank(),
          axis.text = element_text(size = 14)) +
    labs(y = "",
         x = "Protein sequence") +
    scale_fill_gradient2(low = "deeppink1", mid = "white", high = "deepskyblue", midpoint = hamuro_threshold,
                         na.value = "grey90") +
    scale_x_continuous(breaks = 1:new_length, labels = sequence) +
    facet_wrap(~ range, ncol = 1, scales = "free", drop = FALSE) 
  
  return(plt)
}

#' @description
#' not used.
#' 
#' 
#' @export
plot_sequence_hrates_heatmap <- function(dat){
  
  ## to slow
  residues <- HaDeX2::get_residue_positions(dat)

  v_hrates <- lapply(1:nrow(residues), function(i){
    
    # print(paste0(i, residues[i, "aa"]))
    
    if(i == 1){ 
      NA 
    } else if(is.na(residues[i, "aa"])){ 
      NA
      } else if(residues[i, "aa"] == "P") {
      NA
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

