#' @export
#' 
nicer_table <- function(tbl_dat, filename = "data", dom = "tBip", selection = "none"){
  
  DT::datatable(
    data = tbl_dat,
    class = "table-bordered table-condensed",
    extensions = "Buttons",
    selection = selection,
    options = list(
      dom = dom,
      autoWidth = TRUE,
      buttons = list(
        list(extend = "excel", filename = filename),
        list(extend = "pdf", filename = filename),
        list(extend = "csv", filename = filename)
      )
    ),
    filter = "bottom",
    rownames = FALSE
  )
  
}
