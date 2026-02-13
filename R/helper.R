## HEADER #######################################################
# Project: Some utility scripts for my Quarto books
# Author: Peter Baumgartner
# Last edit date: May 15, 2025
# CONTENT:
## - load glossary package
## - get_skimmers.sfc: skimmer for geometry column class
## - my_glance_data: glance at a specified number of random data
## - my_create_folder: create folder at path if it not already exists
## - my_save_data_file: save data file
## - my_excel_as_csv_and_rds:
##            save content of stored Excel file with all sheets
##            as CSV snapshots and RDS objects
## - my_ls_region: well being ladder score line chart for regions
## - my_pkgs_dl: Get number of downloads from RStudio CRAN Mirror
## - my_as_tibble_sf: Convert sf class to sf and tbl_df / tbl class
## - my_plot_beta: Plot a Beta Model for pi
## - my_plot_gamma: Plot a Gamma Model for lambda

## glossary #####################################################
library(glossary)

glossary::glossary_path("../glossary-pb/glossary.yml")
glossary_popup("hover")

##get_skimmers.sfc ##############################################
# get_skimmers.sfc:
# Purpose:
# provide skimmers for special column `geometry`
# of `sfc` data type for geospatial data in {sf}
# General procedure:
#   https://docs.ropensci.org/skimr/articles/extending_skimr.html#defining-sfls-for-a-package
# Specifics the simple feature list column (sfc) in the {sf} package
#   https://github.com/ropensci/skimr/issues/88

get_skimmers.sfc <- function(column) {
  skimr::sfl(
    skim_type = "sfc",
    missing = skimr::n_missing,
    complete = skimr::n_complete,
    n = length,
    n_unique = purrr::compose(length, skimr::n_unique),
    valid = purrr::compose(sum, sf::st_is_valid)
  )
}


### my_glance_data ##############################################
# my_glance_data: Glance at a specified number of random data
# Purpose:
# To prevent possible bias with head()/tail() or
# other function that print some data excerpts
# Used in "Statistics with R"
# See: https://bookdown.org/pbaumgartner/swr-harris/

# df   = dataframe or tibble
# N    = number of records chosen randomly
# seed = set.seed for reproducibility

my_glance_data <- function(df, N = 8, seed = 42){
  df_temp <- first_and_last_row(df)

  set.seed(seed)
  df |>
    dplyr::mutate(obs = dplyr::row_number()) |>
    dplyr::relocate(obs) |>
    dplyr::slice_sample(n = N) |>
    dplyr::bind_rows(df_temp) |>
    dplyr::arrange(obs)
}

first_and_last_row <-  function(df) {
  df |>
    dplyr::mutate(obs = dplyr::row_number()) |>
    dplyr::filter(dplyr::row_number() %in% base::c(1, dplyr::n()))
}


## my_create_folder #############################################
# my_create_folder:
# Purpose:
# check if folder already exists at parameter "path"
# if not, then create folder
# Author: Peter Baumgartner
# path = character string:
#                example: "/Users/xxyyzz/Documents/my-data/"

my_create_folder <- function(path){

  if (!base::file.exists(path))
  {base::dir.create(path)}
}


## my_save_data_file ############################################
# my_save_data_file: Save data file for the specified chapter
# Purpose:
# If folder not exists, create it and save object as .rds file
# Author: Peter Baumgartner
# chapter_folder = character: folder inside "data" folder
#                  example "chap05"
# object = data to save
# file_name = character: example "xy_object.rds"

my_save_data_file <- function(chapter_folder, object, file_name){
  data_folder <- base::paste0(here::here(), "/data/")
  if (!base::file.exists(data_folder))
  {base::dir.create(data_folder)}

  chap_folder <-
    base::paste0(
      here::here(),
      paste0("/data/", chapter_folder, "/")
    )
  if (!base::file.exists(chap_folder))
  {base::dir.create(chap_folder)}

  base::saveRDS(object = object,
                file = paste0(chap_folder, "/", file_name))
}



## my_excel_as_csv_and_rds ######################################
# my_excel_as_csv_and_rds:
# Purpose:
#            save content of stored Excel file with all sheets
#            as CSV snapshots and RDS objects
# Author: Peter Baumgartner


# sheet: vector of sheet names
# path_excel: path to the already saved excel file
# path_csv: folder path where to store all Excel sheets as .csv files
# path_rds: folder path where to store all Excel sheets as .rds objects

my_excel_as_csv_and_rds <- function(
    sheet, path_excel, path_csv, path_rds) {
  path_base <- path_excel |>
    base::basename()  |>
    tools::file_path_sans_ext()
  path_excel  |>
    readxl::read_excel(sheet = sheet) |>
    readr::write_csv(base::paste0(path_csv, path_base, "-", sheet, ".csv")) |>
    readr::write_rds(base::paste0(path_rds, path_base, "-", sheet, ".rds"))
}

class_scheme <- function(df, sel1, sel2) {
  ## df = dataframe to show
  ## sel1 = name of the first column (country names) to select
  ## sel2 = name of the column with the regional indicator
  df |>
    dplyr::select(!!sel1, !!sel2) |>
    dplyr::nest_by(!!sel2) |>
    dplyr::mutate(data = as.vector(data)) |>
    dplyr::mutate(data = stringr::str_c(data, collapse = "; ")) |>
    dplyr::mutate(data = paste(data, ";")) |>
    dplyr::mutate(N = lengths(gregexpr(";", data))) |>
    dplyr::rename(Country = data) |>
    dplyr::arrange(!!sel2) |>
    DT::datatable(class = 'cell-border compact stripe',
                  options = list(
                    pageLength = 25,
                    lengthMenu = c(5, 10, 15, 20, 25, 50),
                    columnDefs = list(
                      list(className = 'dt-body-left', targets = 2)
                    )
                  )
    )
}

class_scheme2 <- function(df, sel1, sel2) {
  ## df = dataframe to show
  ## sel1 = name of the first column (country names) to select
  ## sel2 = name of the column with the regional indicator
  df |>
    dplyr::select(!!sel1, !!sel2) |>
    dplyr::nest_by(!!sel2) |>
    dplyr::mutate(data = as.vector(data)) |>
    dplyr::mutate(data = stringr::str_c(data, collapse = "; ")) |>
    dplyr::mutate(data = paste(data, ";")) |>
    dplyr::mutate(N = lengths(gregexpr(";", data))) |>
    dplyr::rename(Country = data) |>
    dplyr::arrange(!!sel2)
}


## my_ls_region ######################################
# Create plotly line chart with well being ladder scores (ls)
# Data from the World Happiness Report (WHR)
# Using as origin data my `whr_final`
# combining WHR data and M49/World Bank classification dataset
# Author: Peter Baumgartner


my_ls_region <- function(
    df,                     # dataset with three or four columns
    #   - year (numeric)
    #   - region (factor)
    #   - score (numeric)
    fig_title,              # figure title (character)
    mean_column  = TRUE,    #  is there a mean column (logical)
    legend_title = "Region" # legend title (character)
) {

  if (mean_column) {
    df_mean <- df |>
      dplyr::group_by(year) |>
      dplyr::summarize(
        mean = mean(score)
      )

    df <- dplyr::left_join(
      x = df,
      y = df_mean,
      by = dplyr::join_by(year)
    )
  }

  p <- df |>
    plotly::plot_ly(
      x = ~year,
      y = ~score
    ) |>
    plotly::add_trace(
      color = ~ forcats::fct_reorder2(
        region, year, score, .fun = forcats::last2
      ),
      type = "scatter",
      mode = "lines+markers",
      colors = my_colors13()
    )

  (function(x) if (mean_column) {
    plotly::add_lines(
      p,
      y = ~mean,
      name = "<b>mean</b>",
      mode = "lines+marker",
      line = list(
        width = 6,
        dash = "dot",
        color = "black",
        opacity = 0
      )
    )
  }else (x))() |>

    plotly::layout(
      title = fig_title,
      legend = base::list(title =
                            base::list(text =
                                         base::paste0('<b>', legend_title, '</b>'))
      ),
      xaxis = base::list(title = "Year"),
      yaxis = base::list(title = "Cantril Ladder Score")
    ) |>
    plotly::config(doubleClickDelay = 1000)
}


my_get_ls_data <-  function(
    group_name,
    region_name,
    world_bank_group,
    filter_string
) {

  base::readRDS(
    base::paste0(here::here(),
                 "/data/whr-cantril/rds/whr_final.rds")
  ) |>
    dplyr::filter(
      stringr::str_detect(
        !!group_name, filter_string) &
        group48 == world_bank_group
    ) |>
    dplyr::select(year, !!region_name, ladder_score) |>
    dplyr::summarize(
      score = mean(ladder_score), .by = c(year, !!region_name)
    ) |>
    dplyr::rename(region = !!region_name) |>
    base::droplevels()
}


## my_colors13 ######################################
# Southern Europe has 13 categories
# add 13th color to RColorBrewer "Set3" with 12 colors
# idea from Brave-KI "RColorBrewer what to do with 13 categories"
# Author: Peter Baumgartner

my_colors13 <-  function() {
  c(RColorBrewer::brewer.pal(12, "Paired"), "#999999")
}

## my_colors ######################################
# Sometimes I need more than colors for max. 12 categories
# Combine different palettes 'Paired' and 'Set3'
# resulting in 24 colors
# idea from Brave-KI "RColorBrewer combining multiple palettes"
# see also: https://stackoverflow.com/a/70739992/7322615
# and: https://stackoverflow.com/questions/15282580/
# Author: Peter Baumgartner

my_colors <-  function() {
  c(
    RColorBrewer::brewer.pal(12,'Paired'),
    RColorBrewer::brewer.pal(12,'Set3')
  )
}


##### my_pkgs_dl ###########################################
# my_pkgs_dl: Get number of downloads from RStudio CRAN Mirror
# Purpose:
# Compare popularity of different packages
# Author: Peter Baumgartner
# pkgs = character vector of package names
# period = "last-day" "last-week", "last-month"
# days: period days = 1, 7, 30
# returns: tibble with packages sorted by download figures
# I have used the function in my notes on "Statistics with R"
# # See: https://bookdown.org/pbaumgartner/swr-harris/

my_pkgs_dl <-  function(pkgs, period = "last-week", days = 7) {
  dl_pkgs <- cranlogs::cran_downloads(when = period, packages = pkgs)

  start_date = base::min(dl_pkgs$date)
  end_date = base::max(dl_pkgs$date)

  dl_pkgs |>
    dplyr::group_by(package) |>
    dplyr::summarize(average = trunc(sum(count) / days)) |>
    dplyr::arrange(desc(average)) |>
    dplyr::mutate(
      from = start_date,
      to = end_date
    )
}


##### my_as_tibble_sf ###########################################
# my_as_tibble_sf: Add `"tbl_df" "tbl"` to `"sf" "data.frame"`
# Purpose:
# Provide tibble functionality to sf class
# Author: Peter Baumgartner
# df = data.frame class sf to convert

my_as_tibble_sf <- function(df) {

  sf_class <- class(df)
  if (sf_class[1] == "sf" & sf_class[2] == "data.frame") {
    df <- tibble::as_tibble(df) |>
      sf::st_as_sf()

  } else {
    return("failed")
  }
}

##### my_plot_beta ##############################################
# my_plot_beta: Plot a Beta Model for π
# Purpose:
# Plots the probability density function (pdf)
# for a Beta(alpha, beta) model of variable π
# alpha = alpha value, beta = beta value
# show_mean = draw vertical line for mean: default = FALSE
# show_mode = draw vertical line for mode: default = FALSE
# show_legend = show or hide legend: default = FALSE
# mean_color and mode_color: default = "darkblue"
# mean_linetype: default = "solid"
# mode_linetype: default = dashed
# x_lab = x-axis value: default = expression(pi)
# y_lab = y-axis value: default = expression(paste("f(", pi, ")")))
my_plot_beta <- function(alpha, beta,
                         show_mean = FALSE,
                         show_mode = FALSE,
                         show_legend = FALSE,  # Default is FALSE
                         mean_color = "darkblue",
                         mode_color = "darkblue",
                         mean_linetype = "solid",
                         mode_linetype = "dashed",
                         x_lab = expression(pi),
                         y_lab = expression(paste("f(", pi, ")")))
{
  # Calculate mean and mode
  mean_val <- alpha / (alpha + beta)
  mode_val <- (alpha - 1) / (alpha + beta - 2)

  # Create base plot
  p <- ggplot2::ggplot(data = tibble::tibble(x = base::c(0, 1)), ggplot2::aes(x)) +
    ggplot2::stat_function(fun = stats::dbeta, n = 101,
                  args = base::list(shape1 = alpha, shape2 = beta)) +
    ggplot2::labs(x = x_lab, y = y_lab) +
    ggplot2::theme_minimal()

  # Create data frame for lines to show
  lines_data <- tibble::tibble()

  # Add mean line if requested
  if (show_mean) {
    lines_data <- dplyr::bind_rows(lines_data,
                            tibble::tibble(
                              x = mean_val,
                              xend = mean_val,
                              y = 0,
                              yend = dbeta(mean_val, alpha, beta),
                              type = "Mean",
                              color = mean_color,
                              linetype = mean_linetype
                            ))
  }

  # Add mode line if requested and valid
  if (show_mode && alpha > 1 && beta > 1) {
    lines_data <- dplyr::bind_rows(lines_data,
                            tibble::tibble(
                              x = mode_val,
                              xend = mode_val,
                              y = 0,
                              yend = dbeta(mode_val, alpha, beta),
                              type = "Mode",
                              color = mode_color,
                              linetype = mode_linetype
                            ))
  }

  # Add lines if any
  if (base::nrow(lines_data) > 0) {
    p <- p +
      ggplot2::geom_segment(data = lines_data,
            ggplot2::aes(x = x, xend = xend, y = y, yend = yend,
                linetype = type, color = type), linewidth = 1) +
      ggplot2::scale_linetype_manual(
        values = rlang::set_names(lines_data$linetype, lines_data$type),
        name = NULL
      ) +
      ggplot2::scale_color_manual(
        values = rlang::set_names(lines_data$color, lines_data$type),
        name = NULL
      )

    if (show_legend) {
      p <- p + ggplot2::theme(
        legend.position = c(0.9, 0.9),
        legend.background = ggplot2::element_rect(fill = "white", color = "gray80")
      )
    } else {
      p <- p + ggplot2::theme(legend.position = "none")
    }
  }

  p
}



my_plot_gamma <- function (shape, rate, show_mean = FALSE, show_mode = FALSE,
          show_legend = FALSE,
          mean_color = "darkblue",
          mode_color = "darkblue",
          mean_linetype = "solid",
          mode_linetype = "dashed")
{
  x_min <- qgamma(1e-25, shape, rate)
  x_max <- qgamma(0.99999, shape, rate)

  # Add small buffer to x_min to prevent clipping at boundary
  x_range <- x_max - x_min
  x_min_plot <- x_min - 0.01 * x_range

  p <- ggplot2::ggplot(data = data.frame(x = c(x_min, x_max)), ggplot2::aes(x)) +
    ggplot2::stat_function(fun = dgamma, n = 101, args = list(shape = shape,
                                                              rate = rate)) +
    ggplot2::labs(x = expression(lambda), y = expression(paste("f(", lambda, ")")))

  # Create dataframe for segments
  segments_df <- data.frame()

  # Add mean line if requested
  if (show_mean == TRUE) {
    mean_val <- shape/rate
    segments_df <- rbind(segments_df,
                         data.frame(x = mean_val, y = 0, xend = mean_val,
                                    yend = dgamma(mean_val, shape, rate),
                                    label = "mean", color = mean_color, linetype = mean_linetype))
  }

  # Add mode line if requested
  if (show_mode == TRUE) {
    if (shape < 1) {
      stop("In order to plot the mode the shape parameter must be greater than\n           or equal to 1.")
    }
    mode_val <- (shape - 1)/rate
    mode_yend <- dgamma(mode_val, shape, rate)

    segments_df <- rbind(segments_df,
                         data.frame(x = mode_val, y = 0, xend = mode_val,
                                    yend = mode_yend,
                                    label = "mode", color = mode_color, linetype = mode_linetype))
  }

  # Add segments if any exist
  if (nrow(segments_df) > 0) {
    p <- p + ggplot2::geom_segment(data = segments_df,
                                   ggplot2::aes(x = x, y = y, xend = xend, yend = yend,
                                                color = label, linetype = label),
                                   show.legend = show_legend) +
      ggplot2::scale_color_manual(values = setNames(segments_df$color, segments_df$label)) +
      ggplot2::scale_linetype_manual(values = setNames(segments_df$linetype, segments_df$label))
  }

  p
}
## END

