R <- 100

#' Transform list of n_grams to unigrams
#'
#' Transform list of n_grams to unigrams
#' @param x a dataframe of keywords
#' @keywords read, topic models
#' @export
#' @examples
#' keyword_to_list(a_data_frame)
unigrams <-  function(x) {
  terms <- x$TERM
  terms <- strsplit(terms, split =" ")
  index <- which(sapply(terms, length) == 1)[1:R]
  return (x[index,c(1,2)])}


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


#' Transform fukan system keyword files to the input object for heatmaps
#'
#' Transform fukan system keyword files to the input object for heatmaps
#' @param path a directory having the "mission.keyword.tsv" files
#' @param report whether to save the unified keyword report as .csv
#' @keywords read, clusters
#' @export
#' @examples
#' fukan_keywords_to_list(path)
fukan_Keywords_to_list <- function(path, report = FALSE) {
  # select the directory with the files, mission.keywords for each cluster
  file_names = list.files(path = path, full.names= TRUE, pattern = "*.tsv")

  #Get the cluster number as they were read
  cluster_number <- sapply(file_names, function(x){
    t <- as.numeric(str_extract(x, "[0-9]+"))
  })

  # Read each file and store them in a list
  cluster_keywords <- lapply(file_names, function(g){
    read.table(g, header=T, sep="\t", fill= T, quote ="",
               row.names=NULL, stringsAsFactors = F, check.names=FALSE)
  })

  # Put files in data frame format, order them, and name them accrodingly
  ALL_Clusters <- lapply(cluster_keywords, unigrams)
  ALL_Clusters <- ALL_Clusters[order(unname(cluster_number))]
  names(ALL_Clusters) <- 1:length(ALL_Clusters)

  # write file
  if (report == TRUE) {write.csv(ALL_Clusters, file="cluster_words.csv", row.names = F)}

  return(ALL_Clusters)
}
