library(tidyverse)
library(shiny)
library(gtools)

#' load_experience_csv
#'
#' @param input
#'
#' @return
#' @export
#'
#' @examples
load_experience_csv <- function(input) {

  infile <- input$datafile
  if (is.null(infile)) {
    # User has not uploaded a file yet
    return(NULL)
  }
  tmp <- read_csv(infile$datapath)
  names(tmp) <- tolower(names(tmp))

  # Factorize all and set all strings to lower case


  # If file is really raw use the median
  if ("series_id" %in% colnames(tmp)) {
    columns_to_remove = c()
    if ("view_option" %in% colnames(tmp)) {
      columns_to_remove <- c('view_option', columns_to_remove)
    }
    if ("experiment" %in% colnames(tmp)) {
      columns_to_remove <- c('experiment', columns_to_remove)
    }
    if ("camera" %in% colnames(tmp)) {
      columns_to_remove <- c('camera', columns_to_remove)
    }
    tmp <-
      tmp %>%
      select(-col_to_remove) %>%
      mutate_if(is.character, str_to_lower) %>%
      mutate_if(is.character, as.factor) %>%
      group_by(series_id) %>%
      mutate_if(is.numeric, median) %>%
      ungroup() %>%
      select(-c(series_id)) %>%
      distinct()
  }

  if (("treatment" %in% colnames(tmp)) &
      ("genotype" %in% colnames(tmp)) &
      (!"condition" %in% colnames(tmp))) {
    tmp <-
      tmp %>%
      mutate(condition = treatment) %>%
      mutate(treatment = sprintf("%s - %s", genotype, condition))
  } else {
    # If needed ad a treatment column
    if (!"treatment" %in% colnames(tmp)) {
      tmp <- tmp %>% mutate(treatment = "No Data")
    }
  }

  # Build day after start if not present
  if (!("day_after_start" %in% colnames(tmp))) {
    if("date_time" %in% colnames(tmp)) {
      tmp <- tmp %>% mutate(day_after_start = as.numeric(date_time - min(date_time)) / (60*60*24))
    } else {
      if ("date" %in% colnames(tmp)) {
        tmp <- tmp %>% mutate(day_after_start = date - min(date))
      }
    }
  }

  # If treatment exists create foctor
  if ("treatment" %in% colnames(tmp)) {
    if (!("treatment_value" %in% colnames(tmp))) {
      tmp <- tmp %>% mutate(treatment_value = as.numeric(as.factor(treatment)))
    }
  }

  # If disease index exists create natural version
  if ("disease_index" %in% colnames(tmp)) {
    if (!("trunc_disease_index" %in% colnames(tmp))) {
      tmp <- tmp %>% mutate(trunc_disease_index = as.character(trunc(disease_index)))
    }
    if (!("disease_index_max" %in% colnames(tmp))) {
      tmp <-
        tmp %>%
        group_by(plant) %>%
        mutate(disease_index_max = max(disease_index)) %>%
        ungroup()
    }
    if (!("trunc_disease_index_max" %in% colnames(tmp))) {
      tmp <- tmp %>% mutate(trunc_disease_index_max = as.character(trunc(disease_index_max)))
    }
  }

  # Remove experiment if only one is present
  if ("experiment" %in% colnames(tmp)) {
    df <- tmp %>% select(experiment)
    df <- df[!(duplicated(df) | duplicated(df, fromLast = FALSE)), ]
    cb_options = as.list(levels(df))
    if(length(cb_options) == 1) {
      tmp <- tmp %>% select(-c(experiment))
    }
  }

  # Create natural version of day after start
  tmp <- tmp %>%
    mutate(trunc_day_after_start = round(day_after_start))
}
