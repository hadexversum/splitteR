#' Create subsections from peptides
#' 
#' @importFrom checkmate assert

#' @param dat peptides data
#' 
#' @examples
#' dat <- filter(alpha_dat, State == "Alpha_KSCN", End < 35)
#' subsections <- create_subsections(dat)
#' 
#' @export
create_subsections <- function(dat){
  assert(all(c("Sequence", "Start", "End") %in% colnames(dat)))
  
  peptides <- dat %>%
    select(Sequence, Start, End) %>%
    unique(.) %>%
    arrange(Start, End) %>%
    mutate(id = 1:nrow(.)) 
  
  starts <- unique(peptides[["Start"]])
  ends <- unique(peptides[["End"]])
  
  results_starts <- lapply(starts, function(start){
    
    peptides %>%
      filter(Start == start) %>%
      find_subs_start(.)
    
  }) %>% bind_rows()
  
  results_ends <- lapply(ends, function(end){
    
    peptides %>%
      filter(End == end) %>%
      find_subs_end(.)
    
  }) %>% bind_rows()
  
  res <- bind_rows(results_starts, results_ends) %>%
    unique() %>%
    arrange(sub_start, sub_end) 
  
  pep_out <- res %>% filter(!is.na(longer_id)) %>% select(longer_id) %>% unique(.) %>% .[[1]]
  
  return(filter(res, !id%in%pep_out))
}


#' @export
#

find_subs_start <- function(tmp_dat){
  
  tmp_dat <- arrange(tmp_dat, desc(End))
  
  subs <- data.frame(sub_sequence = character(), 
                     sub_start = double(), sub_end = double(), 
                     longer_id = double(), longer_sequence = character(), longer_start = double(), longer_end = double(),
                     shorter_id = double(), shorter_sequence = character(), shorter_start = double(), shorter_end = double(),
                     common = character(), id = double())  
  
  n_pep <- nrow(tmp_dat)
  
  if(n_pep == 1){
    
    subs <- data.frame(sub_sequence = tmp_dat[["Sequence"]], sub_start = tmp_dat[["Start"]], sub_end = tmp_dat[["End"]], 
                       longer_id = NA, shorter_id = NA, common = "origin", id = tmp_dat[["id"]],
                       longer_sequence = NA, longer_start = NA, longer_end = NA, shorter_sequence = NA, shorter_start = NA, shorter_end = NA)
    
  } else {
    
    for(i in 1:(n_pep-1)){
      
      longer_pep = tmp_dat[i,]
      shorter_pep = tmp_dat[i+1, ]
      
      # step by step for now
      longer_sequence <- longer_pep[["Sequence"]]
      sub_sequence_length <- longer_pep[["End"]]-shorter_pep[["End"]]
      sub_sequence <- substring(longer_sequence, nchar(longer_sequence) - sub_sequence_length + 1, nchar(longer_sequence))
      
      subs <- rbind(subs, data.frame(sub_sequence = sub_sequence, sub_start = shorter_pep[["End"]] +1 , sub_end = longer_pep[["End"]], 
                                     longer_id = longer_pep[["id"]], shorter_id = shorter_pep[["id"]], common = "start", id = NA,
                                     longer_sequence = longer_pep[["Sequence"]], longer_start = longer_pep[["Start"]], longer_end = longer_pep[["End"]], 
                                     shorter_sequence = shorter_pep[["Sequence"]], shorter_start = shorter_pep[["Start"]], shorter_end = shorter_pep[["End"]]
                                     ))
      
    }
    
    last_pep <- tmp_dat[n_pep, ]
    subs <- rbind(subs, data.frame(sub_sequence = last_pep[["Sequence"]], sub_start = last_pep[["Start"]], sub_end = last_pep[["End"]], 
                                   longer_id = NA, shorter_id = NA, common = "origin", id = last_pep[["id"]],
                                   longer_sequence = NA, longer_start = NA, longer_end = NA, 
                                   shorter_sequence = NA, shorter_start = NA, shorter_end = NA
                                   ))
    
  }
  
  subs
}

#' @export
find_subs_end <- function(tmp_dat){
  
  tmp_dat <- arrange(tmp_dat, Start)
  
  subs <- data.frame(sub_sequence = character(), 
                     sub_start = double(), sub_end = double(), 
                     longer_id = double(), longer_sequence = character(), longer_start = double(), longer_end = double(),
                     shorter_id = double(), shorter_sequence = character(), shorter_start = double(), shorter_end = double(),
                     common = character(), id = double())  
  
  n_pep <- nrow(tmp_dat)
  
  if(n_pep == 1){
    
    subs <- data.frame(sub_sequence = tmp_dat[["Sequence"]], sub_start = tmp_dat[["Start"]], sub_end = tmp_dat[["End"]], 
                       longer_id = NA, shorter_id = NA, common = "origin", id = tmp_dat[["id"]],
                       longer_sequence = NA, longer_start = NA, longer_end = NA, shorter_sequence = NA, shorter_start = NA, shorter_end = NA)
    
  } else {
    
    for(i in 1:(n_pep-1)){
      
      longer_pep = tmp_dat[i,]
      shorter_pep = tmp_dat[i+1, ]
    
      
      # step by step for now
      longer_sequence <- longer_pep[["Sequence"]]
      sub_sequence_length <- nchar(longer_pep[["Sequence"]])-nchar(shorter_pep[["Sequence"]])
      sub_sequence <- substring(longer_sequence, 1, sub_sequence_length)
      
      
      subs <- rbind(subs, data.frame(sub_sequence = sub_sequence, sub_start = longer_pep[["Start"]], sub_end = shorter_pep[["Start"]] -1, 
                                     longer_id = longer_pep[["id"]], shorter_id = shorter_pep[["id"]], common = "end", id = NA,
                                     longer_sequence = longer_pep[["Sequence"]], longer_start = longer_pep[["Start"]], longer_end = longer_pep[["End"]], 
                                     shorter_sequence = shorter_pep[["Sequence"]], shorter_start = shorter_pep[["Start"]], shorter_end = shorter_pep[["End"]]))
      
    }
    
    last_pep <- tmp_dat[n_pep, ]
    subs <- rbind(subs, data.frame(sub_sequence = last_pep[["Sequence"]], sub_start = last_pep[["Start"]], sub_end = last_pep[["End"]], 
                                   longer_id = NA, shorter_id = NA, common = "origin", id = last_pep[["id"]],
                                   longer_sequence = NA, longer_start = NA, longer_end = NA, shorter_sequence = NA, shorter_start = NA, shorter_end = NA))
    
  }
  
  subs
  
  
}

