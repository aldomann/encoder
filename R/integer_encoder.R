#' Integer Encoder
#'
#' @param data Input data frame
#' @param feature Unquoted form of the feature/column to encode
#' @param as_factor Whether the feature data type should be kept as factor or pure integer
#'
#' @return
#' @export
#'
#' @examples
#' set.seed(11)
#' integer_encoder(iris[sample(1:150, 10),], Species)
integer_encoder <- function(data, feature, as_factor = TRUE) {

  # Process levels and labels
  levels_raw <- data %>%
    dplyr::pull({{ feature }}) %>%
    base::as.character() %>%
    base::unique() %>%
    base::sort()

  labels_raw <- base::seq(1, base::length(levels_raw), 1)

  # Encoding
  data <- data %>%
    dplyr::mutate_at(
      .vars = dplyr::vars({{ feature }}),
      .funs = function(x) {
        base::factor(
          x,
          levels = levels_raw,
          labels = labels_raw
        )
      }
    )

  if(!as_factor) {
    data <- data %>%
      dplyr::mutate_at(
        .vars = dplyr::vars({{ feature }}),
        .funs = as.integer
      )
  }

  return(data)
}
