#' Calculates back-exchange based 
#' on the Hamuro table
#' 
#' @examples
#' calculate_hrate("GFGDLKSPAGL")
#' 
#' @export
calculate_hrate <- function(sequence){
  
  n <- nchar(sequence)
  residues <- strsplit(sequence, split = "")[[1]]
  
  v_hrates <- lapply(1:n, function(i){
    if(i == 1){
        rate_n[residues[i], residues[i+1]]
      
    } else if(i == n) {
        rate_c[residues[i], residues[i-1]]
    } else {
      if(residues[i-1] == "P" |residues[i] == "P") {
        0
      }
      else {
        rate_m[residues[i], residues[i-1]]
      }
        
    }
  }) %>% unlist(.)
  
  sum(v_hrates)
  
}