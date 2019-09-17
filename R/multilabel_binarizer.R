#' Multi-label Binarizer
#'
#' @param data Input data frame
#' @param feature Unquoted of the feature/column to encode
#' @param sep Inner separator of the labels
#'
#' @return
#' @export
#'
#' @examples
multi_label_binarizer <- function(data, feature, sep = "#") {
  pad_data_with_sep <- function(data, feature, sep = "#") {
    data %>%
      dplyr::mutate(
        # Add separator to start and end of string
        processed_feature = stringr::str_replace_all(
          {{ feature }}, "^|$", sep
        ),
        # Remove duplicate separator
        processed_feature = stringr::str_replace_all(
          processed_feature, rep(sep, 2) %>% paste(sep = "", collapse = ""), sep
        ),
        # Fix NAa
        processed_feature = stringr::str_replace_na(processed_feature)
      ) %>%
      dplyr::select(-{{ feature }})
  }

  # Deparse feature name
  feature_name <- deparse(substitute(feature))

  # Process labels
  labels_raw <- data %>%
    dplyr::pull({{ feature }}) %>%
    stringr::str_split(sep) %>%
    base::unlist() %>%
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

  detection_labels <- labels_raw %>%
    stringr::str_replace_all("^|$", sep)

  # Raw processed feature
  processed_feature <- pad_data_with_sep(data, {{ feature }}, sep) %>%
    dplyr::pull(processed_feature)

  # MLB encoding
  for (i in 1:length(labels)) {
    data[[labels[i]]] <- stringr::str_detect(processed_feature, detection_labels[i])
  }

  return(data %>% dplyr::select(-{{ feature }}))
}

#' @rdname multi_label_binarizer
#' @export
multi_label_binariser <- multi_label_binarizer
