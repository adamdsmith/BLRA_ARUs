aru_forecast <- function(lat, lon,
                         max_wsp_kmh = 20, 
                         tzone = "America/New_York") {

  if (!requireNamespace("lubridate", quietly = TRUE))
    install.packages("lubridate", quiet = TRUE)
  if (!requireNamespace("ggplot2", quietly = TRUE))
    install.packages("ggplot2", quiet = TRUE)

  wx <- get_hourly(lat, lon, tz = tzone)
  dts <- unique(wx$date_str)[1:5] # Only want forecast 4 days out
  sun <- nrsmisc::get_sun(lon, lat, start = min(dts), end = max(dts),
                          direction = c("sunrise", "sunset"), out_tz = tzone)
  wx <- wx %>%
    filter(date_str %in% dts) %>%
    left_join(sun, by = "date_str") %>%
    rowwise() %>%
    mutate(AM_survey = sunrise + as.difftime(30, units = "mins"),
           PM_survey = sunset - as.difftime(30, units = "mins"),
           window = factor(
             case_when(
               hour %in% (lubridate::hour(AM_survey) + -2:2) ~ "Sunrise",
               hour %in% (lubridate::hour(PM_survey) + -2:2) ~ "Sunset",
               hour %in% c(23, 0:3)                          ~ "Midnight",
               TRUE                                          ~ NA_character_),
             levels = c("Midnight", "Sunrise", "Sunset")),
           rain = case_when(
             grepl("Slight Chance Rain Showers", forecast) ~ 0.5,
             grepl("Chance Rain Showers", forecast)        ~ 0.25,
             grepl("Showers", forecast)                    ~ 0,
             TRUE                                          ~ 1),
           date = as.Date(ifelse(hour > 22, 
                                 as.character(as.Date(date_str) + as.difftime(1, units = "days")), 
                                 date_str))) %>%
    filter(!is.na(window)) %>% ungroup() %>%
    group_by(date, window) %>%
    summarise(n = n(),
              wind_OK = sum(wspd_kmh <= max_wsp_kmh) / n,
              rain_OK = sum(rain) / n) %>%
    filter(n == 5) 

  p <- ggplot2::ggplot(wx, ggplot2::aes(date, window)) + 
    ggplot2::geom_tile(ggplot2::aes(fill = wind_OK * rain_OK)) +
    ggplot2::scale_fill_distiller(palette = 8, type = "div", direction = 1, guide = "none",
                                  limits = c(0, 1)) +
    ggplot2::geom_label(ggplot2::aes(label = sprintf("%.2f", round(wind_OK * rain_OK, 2))), size = 3) + 
    ggplot2::scale_y_discrete("Survey window", expand=c(0,0), limits = rev(levels(wx$window))) + 
    ggplot2::scale_x_date("", position = "top", expand=c(0,0), 
                          date_breaks = "1 day", date_labels = "%a%n%d %b") +
    ggplot2::geom_vline(xintercept = as.numeric(unique(wx$date)) + 0.5, color = "black") +
    ggplot2::geom_hline(yintercept = seq(from = 0.5, by = 1, length = 4), color = "black") +
    ggplot2::theme_bw() + 
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(), 
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank()) +
    ggplot2::ggtitle(paste0("ARU Deployment Outlook", ": ", lat, ", ", lon))
  p
}

get_hourly <- function(lat, lon, tz) {
  if (!requireNamespace("httr", quietly = TRUE))
    install.packages("httr", quiet = TRUE)
  if (!requireNamespace("jsonlite", quietly = TRUE))
    install.packages("jsonlite", quiet = TRUE)
  res <- httr::GET(paste0("https://api.weather.gov/points/", lat, ",", lon))
  httr::stop_for_status(res)
  con <- httr::content(res, "text", encoding = "UTF-8")
  fch <- jsonlite::fromJSON(con)$properties$forecastHourly
  res <- httr::GET(fch)
  httr::stop_for_status(res)
  con <- httr::content(res, "text", encoding = "UTF-8")
  wx <- jsonlite::fromJSON(con)$properties$periods
  out_wx <- wx %>%
    mutate(dt = lubridate::ymd_hms(startTime, tz = tz),
           date = as.Date(dt, tz = tz),
           date_str = as.character(date),
           hour = lubridate::hour(dt),
           wspd_kmh = round(as.integer(sub(" mph", "", windSpeed)) * 1.60934)) %>%
    select(date, date_str, hour, wspd_kmh, forecast = shortForecast)
  out_wx
}
