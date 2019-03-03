#' Plot percent bars by year for given yes/no/unsure question
#'
#' @param d data frame from \code{\link{tidy_poll()}}
#' @param q question with yes/no/unsure answers
#'
#' @return a ggplot2 graph object
#' @export
#' @importFrom lubridate year
#' @importFrom RColorBrewer brewer.pal
#' @import dplyr ggplot2
#'
#' @examples
plot_pctbar_qyn_year <- function(d, q){

  library(lubridate)
  library(RColorBrewer)
  library(dplyr)
  library(ggplot2)

  #q <- "CA ocean health better?"
  o_q <- "Metadata"
  o_a <- "year"
  f_a <- "answered"

  d_o <- d %>%
    dplyr::filter(
      question == !!o_q,
      answer   == !!o_a) %>%
    dplyr::select(survey_id, value_num) %>%
    dplyr::rename(!!o_a := value_num)
  #d_o

  d_q <- d %>%
    dplyr::filter(question == !!q) %>%
    dplyr::left_join(d_o, by="survey_id") %>%
    dplyr::arrange(survey_id, answer)


  d_a <- d_q %>%
    dplyr::group_by(question, year, answer) %>%
    dplyr::summarize(
      sum = sum(value_num)) %>%
    dplyr::filter(answer != !!f_a) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      year = lubridate::ymd(year, truncated = 2))
  #d_a

  #browser()

  # Stacked Percent
  rdylgn <- RColorBrewer::brewer.pal(5,"RdYlGn")

  g <- ggplot2::ggplot(d_a, ggplot2::aes(fill=answer, y=sum, x=year)) +
    ggplot2::geom_bar( stat="identity", position="fill") +
    ggplot2::scale_fill_manual(values=c(rdylgn[1], "grey50", rdylgn[5])) +
    ggplot2::ylab("%") +
    ggplot2::theme_minimal()

  g
}

#' Plot percent bars by category (from other question) for given yes/no/unsure question
#'
#' @param d data frame from \code{\link{tidy_poll()}}
#' @param q question with yes/no/unsure answers
#' @param o_q other question with categorical answers
#'
#' @return a ggplot2 graph object
#' @importFrom RColorBrewer brewer.pal
#' @import dplyr ggplot2
#' @export
#'
#' @examples
plot_pctbar_qyn_qc <- function(d, q, o_q){

  library(RColorBrewer)
  library(dplyr)
  library(ggplot2)

  # q="Climate change problem?"; o_q="Education"

  o_answers <- d %>%
    filter(
      question == !!o_q,
      answer   != "answered") %>%
    select(answer) %>%
    distinct() %>%
    arrange(answer) %>%
    pull(answer)

  d_o <- d %>%
    filter(
      question == !!o_q,
      answer   != "answered",
      value_num == 1) %>%
    select(survey_id, answer) %>%
    rename(other = answer) %>%
    mutate(
      other = factor(other, levels=o_answers, ordered=T))
  #d_o
  #d_o$other

  d_q <- d %>%
    filter(
      question == !!q,
      answer   != "answered",
      value_num == 1) %>%
    select(-value_num, -value_chr) %>%
    left_join(d_o, by="survey_id") %>%
    filter(!is.na(other)) %>%
    arrange(survey_id, answer)
  #d_q

  d_a <- d_q %>%
    group_by(question, answer, other) %>%
    summarize(
      sum = n())
  #d_a

  # Stacked Percent
  rdylgn <- brewer.pal(5,"RdYlGn")

  g <- ggplot(d_a, aes(fill=answer, y=sum, x=other)) +
    geom_bar( stat="identity", position="fill") +
    #facet_wrap(~condition) +
    scale_fill_manual(values=c(rdylgn[1], "grey50", rdylgn[5])) +
    ylab("%") +
    xlab(o_q) +
    theme_minimal()

  g
}


#' Plot treemap for categorical question
#'
#' @param d data frame from \code{\link{tidy_poll()}}
#' @param q question with categorical answers
#'
#' @return a ggplot2 graph object
#' @import dplyr ggplot2 treemapify
#' @export
#'
#' @examples
plot_treemap_qc <- function(d, q){

  library(dplyr)
  library(ggplot2)
  library(treemapify)

  # q <- "Recreational Activities"

  d_a <- d %>%
    filter(
      question == !!q,
      !answer %in% c("answered", "*response"),
      value_num > 0) %>%
    group_by(answer) %>%
    summarize(
      n = n())
  #d_a

  ggplot(d_a, aes(area = n, fill=answer, label=answer)) +
    geom_treemap() +
    geom_treemap_text(
      fontface = "italic", color = "white",
      place = "centre", grow = TRUE) +
    guides(fill=FALSE)
}

#' Animate treemap for categorical question over years, write to gif
#'
#' @param d data frame from \code{\link{tidy_poll()}}
#' @param q question with categorical answers
#' @param gif path to output gif
#'
#' @return True
#' @import dplyr ggplot2 treemapify gganimate
#' @export
#'
#' @examples
animate_treemap_qc_year <- function(d, q, gif){

  q   <- "Recreational Activities"
  gif <- paste(q, "animated_treemap.gif")

  library(dplyr)
  library(ggplot2)
  library(treemapify)
  library(gganimate)

  d_yr <- d %>%
    filter(
      question == "Metadata",
      answer   == "year") %>%
    select(survey_id, year=value_num) %>%
    mutate(
      year = as.integer(year))
  #d_yr

  d_a <- d %>%
    filter(
      question == !!q,
      !answer %in% c("answered", "*response"),
      value_num > 0) %>%
    left_join(
      d_yr, by = "survey_id") %>%
    group_by(year, answer) %>%
    summarize(
      n = n())
  #d_a

  g <- ggplot(d_a, aes(area = n, fill=answer, label=answer)) +
    geom_treemap() +
    geom_treemap(layout = "fixed") +
    geom_treemap_text(
      layout = "fixed",
      fontface = "italic", color = "white",
      place = "centre", grow = TRUE) +
    guides(fill=FALSE) +
    transition_time(year) +
    ease_aes('linear') +
    labs(title = "Year: {frame_time}")

  anim_save(gif, g, nframes = 50)
}
