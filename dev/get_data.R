library(tidyverse)
data_piezo <- httr::GET("https://hubeau.eaufrance.fr/api/v1/niveaux_nappes/chroniques?code_bss=07548X0009%2FF&size=2000") %>%
  httr::content(as = "text") %>%
  jsonlite::fromJSON() %>%
  purrr::pluck("data")

tidy_piezo <- data_piezo %>%
  tibble::as_tibble() %>%
  dplyr::select(date_mesure, niveau_nappe_eau) %>%
  dplyr::mutate(date_mesure = lubridate::ymd(date_mesure))


hydrotibble <- function(.data, date, value, type = c("daily", "monthly", "yearly"),
                        format = "ymd", impute_na = c("mean", "median", "linear", "spline", "none")){
  input_data <- .data %>%
    dplyr::select({{date}}, {{value}}) %>%
    tibble::as_tibble()

  if (type == "daily"){

    colnames(input_data) <- c("date", "value")

    if (format == "ymd"){
      input_data$date <- lubridate::ymd(input_data$date)
    }

    complete_data <- tibble::tibble(
      date = seq.Date(from = min(input_data$date), to = max(input_data$date), by = "1 day")
    ) %>%
      dplyr::left_join(input_data, by = "date")

    if (impute_na == "linear"){
      complete_data$value <- imputeTS::na_interpolation(complete_data$value, option = "linear")
    } else if (impute_na == "mean"){
      complete_data$value <- imputeTS::na_mean(complete_data$value, option = "mean")
    } else if (impute_na == "median"){
      complete_data$value <- imputeTS::na_mean(complete_data$value, option = "median")
    } else if (impute_na == "spline"){
      complete_data$value <- imputeTS::na_interpolation(complete_data$value, option = "spline")
    }

    class(complete_data) <- c("daily", "hydrotibble", class(complete_data))
  } else if (type == "monthly"){
    colnames(input_data) <- c("year", "month", "value")

    complete_data <- tidyr::crossing(
      year = seq(min(input_data$year), max(input_data$year)),
      month = 1:12
    ) %>%
      dplyr::left_join(input_data, by = c("year", "month")) %>%
      zoo::na.trim()

    if (impute_na == "linear"){
      complete_data$value <- imputeTS::na_interpolation(complete_data$value, option = "linear")
    } else if (impute_na == "mean"){
      complete_data$value <- imputeTS::na_mean(complete_data$value, option = "mean")
    } else if (impute_na == "median"){
      complete_data$value <- imputeTS::na_mean(complete_data$value, option = "median")
    } else if (impute_na == "spline"){
      complete_data$value <- imputeTS::na_interpolation(complete_data$value, option = "spline")
    }
    class(complete_data) <- c("monthly", "hydrotibble", class(complete_data))

  }

  return(complete_data)

}

hydrofilter <- function(.data, start = NA, end = NA){

  if(sum(class(.data) == "hydrotibble") == 0){
    stop("Pas un objet hydrotibble")
  }

  if (is.na(start) & is.na(end)){
    warning("Pas de filtre")
    filter_ts <- .data
  } else if (!is.na(start) & is.na(end)){
    filter_ts <- .data %>%
      dplyr::filter(date >= lubridate::ymd(start))
  } else if (is.na(start) & !is.na(end)){
    filter_ts <- .data %>%
      dplyr::filter(date <= lubridate::ymd(end))
  } else if (!is.na(start) & !is.na(end)){
    filter_ts <- .data %>%
      dplyr::filter(date >= lubridate::ymd(start) & date <= lubridate::ymd(end))
  }

  class(filter_ts) <- class(.data)
  return(filter_ts)

}

hydroMK <- function(.data, sen = FALSE){

  if(sum(class(.data) == "hydrotibble") == 0){
    stop("Pas un objet hydrotibble")
  }

  res <- trend::mk.test(.data$value)

  if (sen == TRUE){
    res_sen <- trend::sens.slope(.data$value)
    out <- tibble::tibble(
      mk_z = res$statistic,
      mk_pvalue = res$p.value,
      mk_result = dplyr::case_when(
        res$p.value >= 0.05 ~ "Pas de tendance",
        res$p.value <= 0.05 & res$statistic > 0 ~ "Tendance à la hausse",
        res$p.value <= 0.05 & res$statistic < 0 ~ "Tendance à la baisse"
      ),
      sen_slope = unname(res_sen$estimates),
      sen_slope_low = res_sen$conf.int[1],
      sen_slope_high = res_sen$conf.int[2]
    )
  } else {
    out <- tibble::tibble(
      mk_z = res$statistic,
      mk_pvalue = res$p.value,
      mk_result = dplyr::case_when(
        res$p.value >= 0.05 ~ "Pas de tendance singificative",
        res$p.value <= 0.05 & res$statistic > 0 ~ "Tendance singificative à la hausse",
        res$p.value <= 0.05 & res$statistic < 0 ~ "Tendance singificative à la baisse"
      )
    )
  }

  class(out) <- c("hydroMK", class(out))
  return(out)
}

monthlysum <- function(.data, fun = "mean"){

  if(sum(class(.data) == "hydrotibble") == 0){
    stop("Pas un objet hydrotibble")
  }

  sum_data <- .data %>%
    dplyr::mutate(year = lubridate::year(date), month = lubridate::month(date)) %>%
    dplyr::group_by(year, month) %>%
    dplyr::summarise(
      value = dplyr::case_when(
        fun == "mean" ~ mean(value, na.rm = TRUE),
        fun == "min" ~ min(value, na.rm = TRUE),
        fun == "max" ~ max(value, na.rm = TRUE),
        fun == "median" ~ median(value, na.rm = TRUE)
      ), .groups = "drop"
    ) %>%
    dplyr::mutate(
      date = lubridate::ymd(paste0(year,"-",month,"-15"))
    ) %>%
    dplyr::select(date, year, month, value)

  class(sum_data) <- c("monthly", "hydrotibble", "tbl_df", "tbl", "data.frame")

  return(sum_data)
}

hydroSMK <- function(.data, sen = FALSE){

  if(sum(class(.data) == "hydrotibble") == 0){
    stop("Pas un objet hydrotibble")
  }

  if (sum(class(.data) == "monthly") != 1){
    stop("Dois être un objet de classe montly")
  }


  result <- .data %>%
    dplyr::mutate(month_name = month.abb[month]) %>%
    dplyr::arrange(month, year) %>%
    dplyr::select(month, month_name, value) %>%
    tidyr::nest(data = c(value)) %>%
    dplyr::mutate(
      mk = purrr::map(data, ~hydroMK(.x, sen))
    ) %>%
    tidyr::unnest(mk) %>%
    dplyr::select(-data)

  class(result) <- c("hydroSMK", class(result))
  return(result)
}

plot_hydrotrend <- function(.data, type = c("mk", "smk")){
  if(sum(class(.data) == "hydrotibble") == 0){
    stop("Pas un objet hydrotibble")
  }

  if (type == "mk"){
    data_plot <- .data
    res_mk <- data_plot %>%
      hydroMK(sen = TRUE)

    data_line <- tibble::tibble(
      slope = c(res_mk$sen_slope, res_mk$sen_slope_low, res_mk$sen_slope_high),
      intercept = c(
        median(data_plot$value) - res_mk$sen_slope * median(as.numeric(data_plot$date)),
        median(data_plot$value) - res_mk$sen_slope_low * median(as.numeric(data_plot$date)),
        median(data_plot$value) - res_mk$sen_slope_high * median(as.numeric(data_plot$date))
      ),
      type = c("average", "CI", "CI")
    )


    p <- ggplot2::ggplot(data_plot, ggplot2::aes(x=date, y=value)) +
      geom_line() +
      geom_abline(ggplot2::aes(slope = res_mk$sen_slope, intercept = median(data_plot$value) - res_mk$sen_slope * median(as.numeric(data_plot$date)))) +
      theme_light()
  } else if (type == "smk"){
    if (sum(class(.data) == "monthly") != 1){
      stop("Dois être un objet de classe montly")
    }

    data_plot <- .data %>%
      dplyr::mutate(month_name = month.name[month])

    res_sen <- data_plot %>%
      hydroSMK(sen = TRUE)

    table_intercept <- data_plot %>%
      dplyr::group_by(month) %>%
      dplyr::summarise(date = as.numeric(median(year)), value = median(value), .groups = "drop") %>%
      dplyr::inner_join(res_sen, by = "month") %>%
      dplyr::mutate(
        intercept = value - sen_slope * date,
        month_name = forcats::fct_reorder(month.name[month], month)
      )

    p <- data_plot %>%
      dplyr::mutate(month_name = forcats::fct_reorder(month_name, month)) %>%
      ggplot2::ggplot(ggplot2::aes(x=year, y=value)) +
      geom_line() +
      geom_abline(
        data = table_intercept, ggplot2::aes(slope = sen_slope, intercept = intercept)
      ) +
      facet_wrap(~month_name, scales = "free") +
      theme_light()


  }

  return(p)

}


data_piezo %>%
  hydrotibble(date_mesure, niveau_nappe_eau, format = "ymd", impute_na = "linear") %>%
  hydrofilter(start = "2000-01-01") %>%
  plot_hydrotrend(type = "mk")

data_piezo %>%
  hydrotibble(date_mesure, niveau_nappe_eau, format = "ymd", impute_na = "linear") %>%
  hydrofilter(start = "2000-01-01") %>%
  monthlysum(fun = "min") %>%
  plot_hydrotrend(type = "smk")
