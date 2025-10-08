#' to test
#' 
#' dat <- filter(alpha_dat, State == "Alpha_KSCN", End < 35)
#' subsections <- create_subsections(dat)
#' subs_dats <- create_subsections_dataset(dat, subsections)
#' 
#' @export

create_subsections_dataset <- function(dat, 
                                       subsections){
  
  mass_dat <- HaDeX::calculate_exp_masses_per_replicate(dat)
  
  peptides <- dat %>%
    select(Sequence, Start, End) %>%
    unique(.) %>%
    arrange(Start, End) %>%
    mutate(id = 1:nrow(.)) 
  
  times <- unique(mass_dat[["Exposure"]])
  
  subsections <- unique(subsections)

  res_uc <- lapply(1:nrow(subsections), function(i){
    
    sub_start =   subsections[i, "sub_start"]
    sub_end = subsections[i, "sub_end"]
    sub_sequence = subsections[i, "sub_sequence"]
    
    sub_type = subsections[i, "common"]
    
    longer_peptide <- peptides[peptides[["id"]] == subsections[i, "longer_id"], ]
    shorter_peptide <- peptides[peptides[["id"]] == subsections[i, "shorter_id"], ]
    
    lapply(times, function(time){
      
      if(sub_type == "origin"){
        origin_dat <- filter(mass_dat, Start == sub_start, End == sub_end, Exposure == time) %>%
          rename(Center = avg_exp_mass) %>%
          mutate(Fragment = "",
                 MaxUptake = nchar(sub_sequence),
                 z = 1,
                 RT = 1,
                 Inten = 1)
        
      } else {
        
        longer_dat_mass <- merge(mass_dat, longer_peptide, by = c("Sequence", "Start", "End")) %>%
          filter(Exposure == time)
        
        shorter_dat_mass <- merge(mass_dat, shorter_peptide, by = c("Sequence", "Start", "End")) %>%
          filter(Exposure == time)
        
        merge(longer_dat_mass, shorter_dat_mass, by = c("Protein", "Modification", "State", "Exposure", "File")) %>%
          select( -MaxUptake.x, -MaxUptake.y, - Sequence.x, -Sequence.y, -MHP.x, -MHP.y, -id.x, -id.y) %>%
          mutate(mass = avg_exp_mass.x - avg_exp_mass.y) %>%
          mutate(MaxUptake = nchar(sub_sequence), 
                 MHP = 1,
                 Start = sub_start, 
                 End = sub_end,
                 Sequence = sub_sequence,
                 Fragment = "",
                 z = 1,
                 RT = 1,
                 Inten = 1) %>%
          select(-Start.x, -End.x, -Start.y, -End.y, -avg_exp_mass.x, -avg_exp_mass.y) %>%
          rename(Center = mass)
        
      }
      
    }) %>% bind_rows()
  }) %>% bind_rows()
  
  return(res_uc)
  
}
