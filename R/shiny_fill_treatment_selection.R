library(tidyverse)
library(shiny)
library(gtools)

fill_treatment_selection <- function(df,
                                     input_id="cbTreatmentSelection",
                                     label_id="Select treatments to be displayed",
                                     selected_text_format="count > 3") {
  df <- df %>%
    select(treatment)
  df <- df[!(duplicated(df) | duplicated(df, fromLast = FALSE)), ]

  cb_options = as.list(levels(df))
  if(length(cb_options) == 0) {
    cb_options <- as.list(df$treatment)
  }
  cb_options <- cb_options[mixedorder(unlist(cb_options),decreasing=F)]
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
    selected = cb_options,
    multiple = TRUE
  )

}
