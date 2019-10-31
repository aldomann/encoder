#' One-hot Encoder
#'
#' @param data Input data frame
#' @param feature Unquoted form of the feature/column to encode
#' @param as_integer Wether the boolean should be converted to integer or not
#'
#' @return
#' @export
#'
#' @examples
#' set.seed(11)
#' one_hot_encoder(iris[sample(1:150, 10),], Species)
one_hot_encoder <- function(data, feature, as_integer = FALSE) {

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

  if (as_integer) {
    data <- data %>%
      dplyr::mutate_at(
        .vars = dplyr::vars(labels),
        .funs = as.integer
      )
  }

  return(data %>% dplyr::select(-{{ feature }}))
}
