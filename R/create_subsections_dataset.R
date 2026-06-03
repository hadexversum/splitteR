#' to test
#' 
#' @param dat raw data
#' @param subsections subsections created from 
#' the dat coverage
#' 
#' @description
#' use get_subsection_data
#' 
#' @examples
#' dat <- filter(alpha_dat, State == "Alpha_KSCN", End < 35)
#' subsections <- create_subsections(dat)
#' subs_dats <- create_subsections_dataset(dat, subsections)
#' 
#' @export

create_subsections_dataset <- function(dat, 
                                       subsections,
                                       time_0 = min(dat[["Exposure"]])){
  
  states <- unique(dat[["State"]])

  res_uc <- lapply(1:nrow(subsections), function(i){

    lapply(states, function(state){
      
      if(subsections[i, "common"] == "origin"){
        
        calculate_deut_uptake(dat, 
                              state = state,
                              sequence = subsections[i, "sub_sequence"],
                              start = subsections[i, "sub_start"],
                              end = subsections[i, "sub_end"],
                              time_0 = time_0) %>%
          rename(Center = deut_uptake) %>%
          select(-err_deut_uptake) %>%
          mutate(Fragment = "",
                 z = 1,
                 RT = 1,
                 Inten = 1,
                 File = paste0("file_", Exposure)) %>%
          select(Protein,Start,End,Sequence,Modification,Fragment,MaxUptake,MHP,State,Exposure,File,z,RT,Inten,Center) %>%
          arrange(Start, End, Exposure)
        
      } else {
        
        longer_kin_dat <- calculate_deut_uptake(dat, 
                                                state = state, 
                                                sequence = subsections[i, "longer_sequence"],
                                                start = subsections[i, "longer_start"],
                                                end = subsections[i, "longer_end"],
                                                time_0 = time_0)
        
        shorter_kin_dat <- calculate_deut_uptake(dat, 
                                                 state = state,
                                                 sequence = subsections[i, "shorter_sequence"],
                                                 start = subsections[i, "shorter_start"],
                                                 end = subsections[i, "shorter_end"],
                                                 time_0 = time_0)
        
        
        sub_kin_dat <- calculate_sub_deut_uptake(subsection = subsections[i, ], 
                                                 kin_dat_longer = longer_kin_dat,
                                                 kin_dat_shorter = shorter_kin_dat)
        
        sub_kin_dat %>%
          rename(Center = deut_uptake) %>%
          select(-err_deut_uptake) %>%
          mutate(Fragment = "",
                 MHP = 0,
                 z = 1,
                 RT = 1,
                 Inten = 1,
                 File = paste0("file_", Exposure)) %>%
          select(Protein,Start,End,Sequence,Modification,Fragment,MaxUptake,MHP,State,Exposure,File,z,RT,Inten,Center) %>%
          arrange(Start, End, Exposure)
          
      }
      
    }) %>% bind_rows()
  }) %>% bind_rows()
  
  return(res_uc)
  
}

# create_subsections_dataset_old <- function(dat, 
#                                        subsections){
#   
#   mass_dat <- HaDeX2::calculate_exp_masses_per_replicate(dat)
#   
#   peptides <- dat %>%
#     select(Sequence, Start, End) %>%
#     unique(.) %>%
#     arrange(Start, End) %>%
#     mutate(id = 1:nrow(.)) 
#   
#   times <- unique(mass_dat[["Exposure"]])
#   
#   subsections <- unique(subsections)
#   
#   res_uc <- lapply(1:nrow(subsections), function(i){
#     
#     sub_start =   subsections[i, "sub_start"]
#     sub_end = subsections[i, "sub_end"]
#     sub_sequence = subsections[i, "sub_sequence"]
#     
#     sub_type = subsections[i, "common"]
#     
#     longer_peptide <- peptides[peptides[["id"]] == subsections[i, "longer_id"], ]
#     shorter_peptide <- peptides[peptides[["id"]] == subsections[i, "shorter_id"], ]
#     
#     lapply(times, function(time){
#       
#       if(sub_type == "origin"){
#         
#         origin_dat <- filter(mass_dat, Start == sub_start, End == sub_end, Exposure == time) %>%
#           rename(Center = avg_exp_mass) %>%
#           mutate(Fragment = "",
#                  MaxUptake = stringr::str_count(sub_sequence, "[A-Z]"),
#                  z = 1,
#                  RT = 1,
#                  Inten = 1)
#         
#       } else {
#         
#         longer_dat_mass <- merge(mass_dat, longer_peptide, by = c("Sequence", "Start", "End")) %>%
#           filter(Exposure == time)
#         
#         shorter_dat_mass <- merge(mass_dat, shorter_peptide, by = c("Sequence", "Start", "End")) %>%
#           filter(Exposure == time)
#         
#         merge(longer_dat_mass, shorter_dat_mass, by = c("Protein", "Modification", "State", "Exposure", "File")) %>%
#           select( -MaxUptake.x, -MaxUptake.y, - Sequence.x, -Sequence.y, -MHP.x, -MHP.y, -id.x, -id.y) %>%
#           mutate(mass = avg_exp_mass.x - avg_exp_mass.y) %>%
#           mutate(MaxUptake = stringr::str_count(sub_sequence, "[A-Z]"),
#                  MHP = 1,
#                  Start = sub_start, 
#                  End = sub_end,
#                  Sequence = sub_sequence,
#                  Fragment = "",
#                  z = 1,
#                  RT = 1,
#                  Inten = 1) %>%
#           select(-Start.x, -End.x, -Start.y, -End.y, -avg_exp_mass.x, -avg_exp_mass.y) %>%
#           rename(Center = mass)
#         
#         
#         
#         
#       }
#       
#     }) %>% bind_rows()
#   }) %>% bind_rows()
#   
#   return(res_uc)
#   
# }

#' @description
#' This function changes negative Center values to zeros.
#' 
#' 
#' @export
filter_negative_values <- function(subsections_dat){

  subsection_dat %>%
    mutate(Center = if_else(Center<0, 0, Center))
}

#' calculate mass uptake for one subsection
#' 
#' @examples
#' subsections <- create_subsections(alpha_dat)
#' subsection <- subsections[3, ]
#' get_subsection_data(dat = alpha_dat, subsection = subsection)
#' 
#' @export
get_subsection_data <- function(dat,
                                state = dat[["State"]][1],
                                subsection){
  
  assert(subsection[["common"]]!="origin")

  ##
  
  longer_dat_mass <- dat %>%
    filter(Sequence == subsection[, "longer_sequence"],
           Start == subsection[, "longer_start"],
           End == subsection[, "longer_end"],
           State == state) %>%
    calculate_exp_masses_per_replicate()
  
  shorter_dat_mass <- dat %>%
    filter(Sequence == subsection[, "shorter_sequence"],
           Start == subsection[, "shorter_start"],
           End == subsection[, "shorter_end"],
           State == state) %>%
    calculate_exp_masses_per_replicate()
  
  
  ##
  
  merge(longer_dat_mass, shorter_dat_mass, by = c("Protein", "Modification", "State", "Exposure", "File")) %>%
    select( -MaxUptake.x, -MaxUptake.y, - Sequence.x, -Sequence.y, -MHP.x, -MHP.y) %>%
    mutate(Center = avg_exp_mass.x - avg_exp_mass.y) %>%
    mutate(MaxUptake = stringr::str_count(subsection[, "sub_sequence"], "[A-Z]"),
           MHP = 1,
           Start = subsection[, "sub_start"], 
           End = subsection[, "sub_end"],
           Sequence = subsection[, "sub_sequence"],
           Fragment = "",
           z = 1,
           RT = 1,
           Inten = 1) %>%
    select(-Start.x, -End.x, -Start.y, -End.y, -avg_exp_mass.x, -avg_exp_mass.y) 
  
  
}


#' calculate mass uptake for one subsection with error
#' 
#' @description
#' This is function similar to get_subsection_data but with 
#' propaged error. Currently it is in a separate function 
#' as it may broke the exisitng workflow.
#' 
#' 
#' @examples
#' subsections <- create_subsections(alpha_dat)
#' subsection <- subsections[10, ]
#' get_subsection_data_w_error(dat = alpha_dat, subsection = subsection)
#' 
#' @export
get_subsection_data_w_error <- function(dat,
                                        state = dat[["State"]][1],
                                        subsection){
  
  assert(subsection[["common"]]!="origin")
  
  ##
  
  longer_dat_mass <- dat %>%
    filter(Sequence == subsection[, "longer_sequence"],
           Start == subsection[, "longer_start"],
           End == subsection[, "longer_end"],
           State == state) %>%
    calculate_exp_masses_per_replicate()
  
  shorter_dat_mass <- dat %>%
    filter(Sequence == subsection[, "shorter_sequence"],
           Start == subsection[, "shorter_start"],
           End == subsection[, "shorter_end"],
           State == state) %>%
    calculate_exp_masses_per_replicate()
  
  
  ##
  
  merge(longer_dat_mass, shorter_dat_mass, by = c("Protein", "Modification", "State", "Exposure", "File")) %>%
    select( -MaxUptake.x, -MaxUptake.y, - Sequence.x, -Sequence.y, -MHP.x, -MHP.y) %>%
    mutate(Center = avg_exp_mass.x - avg_exp_mass.y) %>%
    mutate(MaxUptake = stringr::str_count(subsection[, "sub_sequence"], "[A-Z]"),
           MHP = 1,
           Start = subsection[, "sub_start"], 
           End = subsection[, "sub_end"],
           Sequence = subsection[, "sub_sequence"],
           Fragment = "",
           z = 1,
           RT = 1,
           Inten = 1) %>%
    select(-Start.x, -End.x, -Start.y, -End.y, -avg_exp_mass.x, -avg_exp_mass.y) 
  
  
}
