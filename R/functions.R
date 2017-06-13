#' Transform topic model reports to the input object for heatmaps
#'
#' Transform topic model reports to the input object for heatmaps
#' @param a_data_frame a dataframe created with topic_model_5.R
#' @keywords read, topic models
#' @export
#' @examples
#' keyword_to_list(a_data_frame)
keywords_to_list <- function(a_data_frame) {
  serie <- seq(1, ncol(a_data_frame)-1, 2)
  lodf <- lapply(serie, function(x){
    term <- a_data_frame[, x, with=FALSE]
    relevance <- a_data_frame[, x+1, with=FALSE]
    return(data.frame(term, relevance))
  })
  return(lodf)
}
