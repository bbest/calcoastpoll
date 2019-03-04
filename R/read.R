#' Tidy poll data from a wide to long format for easy querying
#'
#' @param data_xlsx input path to original poll data Excel spreadsheet
#' @param headers_xlsx input path to cleaned headers Excel spreadsheet, with
#'   comments aligned to question and no in between text
#' @param row_end last row of poll data in original Excel spreadsheet
#' @param cols_chr vector of columns that are character
#' @param dir_diagnostic_csvs directory to output diagnostic csvs (questions,
#'   answers, todo_data-not-numeric)
#'
#' @description Besides the data in the original Excel spreadsheet, this
#'   function needs clean headers to fill the answers with associated headings,
#'   questions and comments. It also needs to know how many rows contain the
#'   actual data (not any summary information at the bottom), and which columns
#'   are legitimately character data types whereas the rest of the columns it
#'   will attempt to convert to numeric. Finally an output directory can be
#'   optionally be specified to output diagnostic csvs for cleaning the data,
#'   especially for values that could not convert to numeric or ones that should
#'   be bound to a different numeric range (eg 0 or 1, not -1 to 11).
#'
#'   In future, all the necessary information would be contained in the header.
#'   For instance, \code{data_xslx} is composed already of a clean \code{headers_xlsx},
#'   all summary information is removed so no need for \code{row_end}, and data type
#'   along with acceptable range is appended to each answer heading, eg
#'   \code{County [chr]} for character or \code{score [int;0-5]} for integer ranging from
#'   0 to 5 and the default fields without attribution would be assumed to be
#'   like \code{answered [int;0,1]} so only 0 or 1 is allowed.
#'
#' @return tidy data frame
#' @export
#' @import readxl readr dplyr stringr purrr
#'
#' @examples
tidy_poll <- function(data_xlsx, headers_xlsx, row_end, cols_chr, dir_diagnostic_csvs = NULL){

  if (!is.null(dir_diagnostic_csvs)){
    questions_csv     <- file.path(dir_diagnostic_csvs, "questions.csv")
    answers_csv       <- file.path(dir_diagnostic_csvs, "answers.csv")
    todo_chr2num_csv  <- file.path(dir_diagnostic_csvs, "todo_data-not-numeric.csv")
  }

  library(readxl)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)

  # gather headers, fill empty headers and questions from previous
  headers  <- read_excel(headers_xlsx, col_types="text") %>%
    gather(column, value, -row) %>%
    spread(row, value) %>%
    mutate(
      column = str_replace(column, fixed(".."), "") %>% as.numeric() - 1) %>%
    arrange(column) %>%
    select(column, heading, question, answer, comment1, comment2, comment3) %>%
    fill(heading, question)
  #View(headers)

  questions <- headers %>%
    group_by(heading, question) %>%
    summarise(
      column1  = first(column),
      comment1 = first(comment1),
      comment2 = first(comment2),
      comment3 = first(comment3))

  # write questions_csv
  if (!is.null(dir_diagnostic_csvs))
    write_csv(questions, questions_csv)

  nrow_header <- nrow(read_excel(headers_xlsx, col_types="text"))
  n_max <- row_end - nrow_header + 1

  data <- read_excel(
    data_xlsx,
    n_max=n_max, guess_max=n_max, skip=8, col_names=F)
  #View(head(data))

  col_class <- map_chr(data, class)
  #table(col_class)

  data_chr <- data[, c(T, col_class[-1] %in% c("character"))]  %>%
    rename(survey_id = "..1") %>%
    gather(column, value_chr, -survey_id) %>%
    mutate(
      column = str_replace(column, fixed(".."), "") %>% as.numeric()) %>%
    filter(!is.na(value_chr)) %>%
    left_join(headers, by="column")
  #View(data_chr)

  # check to see if value_chr should be value_num
  data_chr_ck <- data_chr %>%
    group_by(column, question, answer) %>%
    summarize(
      n = n())
  # View(data_chr_ck)

  # columns confirmed to be ok as character
  # cols_chr <- c(
  #   2,4:7,10:13,46,167,256,263,434,437,438,447,455,460,487)
  # TODO: name NA answers: columns 256,438,
  # TODO: fix column 455 (QM) Zip Code : "answerd"
  # - eg values of Leo Carrillo State Park row 10887, not 1 or 0

  # convert character to numeric
  data_chr_num <- data_chr %>%
    filter(!column %in% cols_chr) %>%
    mutate(
      value_num = as.numeric(value_chr))
  # NAs introduced by coercion

  # flag data to clean that didn't convert
  if (!is.null(dir_diagnostic_csvs)){
    data_chr_num %>%
      filter(is.na(value_num)) %>%
      write_csv(todo_chr2num_csv)
  }

  # remove converted numeric data from data_chr
  data_chr <- data_chr %>%
    filter(column %in% cols_chr)

  # cleanup converted numeric data
  data_chr_num <- data_chr_num %>%
    select(-value_chr) %>%
    filter(!is.na(value_num))

  data_num <- data[, c(T, col_class[-1] %in% c("logical","numeric"))]  %>%
    rename(survey_id = "..1") %>%
    gather(column, value_num, -survey_id) %>%
    mutate(
      column = str_replace(column, fixed(".."), "") %>% as.numeric()) %>%
    filter(!is.na(value_num)) %>%
    left_join(headers, by="column")
  #View(data_num)

  d <- bind_rows(data_num, data_chr, data_chr_num) %>%
    select(survey_id, column, question, answer, value_num, value_chr) %>%
    arrange(survey_id, column) %>%
    filter(!str_detect(answer, "^x"))
  #View(d)

  # TODO: check for expected 1s or 0s

  answers <- questions %>%
    left_join(
      d, by="question") %>%
    group_by(heading, question, answer, column) %>%
    summarize(
      value_min = min(value_num, na.rm = T),
      value_max = max(value_num, na.rm = T),
      n_surveys = length(unique(survey_id))) %>%
    ungroup() %>%
    mutate(
      value_min = ifelse(is.infinite(value_min), NA, value_min),
      value_max = ifelse(is.infinite(value_max), NA, value_max)) %>%
    arrange(column)

  # add heading back to data
  d <- questions %>%
    select(heading, question) %>%
    left_join(d, by="question") %>%
    select(heading, question, answer, survey_id, value_num, value_chr) %>%
    arrange(heading, question, answer, survey_id) %>%
    ungroup()

  if (!is.null(dir_diagnostic_csvs))
    write_csv(answers, answers_csv)

  d
}
