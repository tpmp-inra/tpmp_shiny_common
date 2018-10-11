library(tidyverse)
library(shiny)
library(gtools)

#' fill_plant_selection
#'
#' @param df
#' @param input_id
#' @param label_id
#' @param selected_text_format
#' @param preseselct_all
#'
#' @return
#' @export
#'
#' @examples
fill_plant_selection <- function(df,
                                 input_id="cbPlantSelection",
                                 label_id="Select plants to be displayed",
                                 selected_text_format="count > 3",
                                 preseselct_all=T) {
  if ("treatment" %in% colnames(df)) {
    df <- df %>%
      select(plant, treatment)
    df <- df[!(duplicated(df) | duplicated(df, fromLast = FALSE)), ]
    df <- df %>% mutate(desc = sprintf("Plant: %s, Treatment: %s", plant, treatment))
  } else {
    df <- df %>%  select(plant)
    df <- df[!(duplicated(df) | duplicated(df, fromLast = FALSE)), ]
    df <- df %>% mutate(desc = sprintf("Plant: %s", plant))
  }

  cb_options = setNames(as.list(df$plant), as.list(df$desc))
  cb_options <- cb_options[mixedorder(unlist(cb_options),decreasing=F)]
  if (preseselct_all) {
    selected_set <- cb_options
  } else {
    selected_set <- cb_options[1]
  }

  pickerInput(
    inputId = input_id,
    label = label_id,
    choices = cb_options,
    options = list(
      `selected-text-format` = selected_text_format,
      `count-selected-text` = "{0} attributes selelcted",
      `actions-box` = TRUE,
      `deselect-all-text` = "Select none",
      `select-all-text` = "Select all"
    ),
    selected = selected_set,
    multiple = TRUE
  )
}
