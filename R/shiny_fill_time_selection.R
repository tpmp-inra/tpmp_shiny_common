library(tidyverse)
library(shiny)
library(gtools)

#' fill_time_selection
#'
#' @param df
#' @param input_id
#' @param label_id
#'
#' @return
#' @export
#'
#' @examples
fill_time_selection <- function(df, input_id = "cbDateTimeSelector", label_id = "Select days to be displayed") {
  df <- df %>% select(trunc_day_after_start)
  df <- df[!(duplicated(df) | duplicated(df, fromLast = FALSE)), ]

  cb_options = as.list(levels(df))
  if(length(cb_options) == 0) {
    cb_options <- as.list(df$trunc_day_after_start)
  }
  cb_options <- cb_options[mixedorder(unlist(cb_options),decreasing=F)]
  pickerInput(
    inputId = input_id,
    label = label_id,
    choices = cb_options,
    options = list(
      `selected-text-format` = "count > 3",
      `count-selected-text` = "{0} attributes selelcted",
      `actions-box` = TRUE,
      `deselect-all-text` = "Select none",
      `select-all-text` = "Select all"
    ),
    selected = cb_options,
    multiple = TRUE
  )
}
