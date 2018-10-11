library(tidyverse)
library(shiny)
library(gtools)

#' build_discret_selectImput
#'
#' @param df A dataframe
#' @param input_id A string
#' @param label_id A string
#' @param selected_item A string
#' @param add_items A vector
#'
#' @return A combobox filled with the names of the discret columns of the dataframe
#' @export
#'
#' @examples
build_discret_selectImput <- function(df,
                                      input_id,
                                      label_id,
                                      selected_item,
                                      add_items = c("none")) {
  stringDf <- select_if(df, function(col) {is.integer(col) | is.character(col)})
  stringDf <- stringDf[, !(colnames(stringDf) %in% c("date_time"))]
  dsnames <- names(stringDf)
  if (length(add_items) > 0) {
    cb_choices <- c(add_items, as.list(dsnames))
  } else {
    cb_choices <- as.list(dsnames)
  }
  cb_options <- cb_choices[mixedorder(unlist(cb_choices),decreasing=F)]
  if (!selected_item %in% cb_options) {
    selected_item <- NULL
  }
  selectInput(inputId =  input_id,
              label = label_id,
              choices = cb_choices,
              selected = selected_item)
}
