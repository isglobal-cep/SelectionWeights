#' Generate a random dataset
#'
#' @param num_rows The number of rows to add. A positive integer.
#' @param num_numeric_cols The number of numerical variables to add. A positive integer.
#' @param num_factor_cols The number of categorical variables to add. A positive integer.
#' @param id_str The string indicating IDs. A string.
#'
#' @return A random dataset. A tibble.
#' @export
#'
#' @examples
#' dat <- generate_fake_data(10, 5, 3, "HelixID")
generate_fake_data <- function(num_rows,
                               num_numeric_cols,
                               num_factor_cols,
                               id_str) {
  # Assertions
  checkmate::assert_count(num_rows, positive = TRUE, null.ok = FALSE)
  checkmate::assert_count(num_numeric_cols, positive = TRUE, null.ok = FALSE)
  checkmate::assert_count(num_factor_cols, positive = TRUE, null.ok = FALSE)
  checkmate::assert_string(id_str, null.ok = FALSE)

  # Generate dataset
  dat <- tibble::tibble(
    id = paste0(id_str, "_", 1:num_rows)
  )
  ## Adding Normally distributed numerical variables
  for (i in 1:num_numeric_cols) {
    dat <- dat |>
      tibble::add_column(
        !! paste0("num_var_", i) := rnorm(num_rows)
      )
  }
  ## Adding random integers as categorical variables
  for (i in 1:num_factor_cols) {
    dat <- dat |>
      tibble::add_column(
        !! paste0("cat_var_", i) := as.factor(sample(
          1:num_rows, num_rows, replace = TRUE
        ))
      )
  }

  return(dat)
} # End function generate_fake_data
################################################################################
