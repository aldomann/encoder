#' One-hot Encoder
#'
#' @param data Input data frame
#' @param feature Unquoted of the feature/column to encode
#'
#' @return
#' @export
#'
#' @examples
#' set.seed(11)
#' one_hot_encoder(iris[sample(1:150, 10),], Species)
one_hot_encoder <- function(data, feature) {

  # Deparse feature name
  feature_name <- deparse(substitute(feature))

  # Process labels
  labels_raw <- data %>%
    dplyr::pull({{ feature }}) %>%
    as.character() %>%
    base::unique() %>%
    base::sort()

  labels <- paste0(
    # Keep original name
    feature_name, "_",
    # Processed labels
    labels_raw %>%
      # Replace spaces
      stringr::str_replace_all(" ", "_") %>%
      # Replace odd symbols (WIP)
      stringr::str_replace_all("/", "_")
  )

  detection_labels <- labels_raw

  # Raw processed feature
  processed_feature <- data %>%
    dplyr::pull({{ feature }}) %>%
    as.character()

  # MLB encoding
  for (i in 1:length(labels)) {
    data[[labels[i]]] <- stringr::str_detect(processed_feature, detection_labels[i])
  }

  return(data %>% dplyr::select(-{{ feature }}))
}
