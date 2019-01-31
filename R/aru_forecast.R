aru_forecast <- function(lat, lon, key = Sys.getenv("wu_key"),
                         max_wsp_kmh = 20, max_pop = 30) {
  
  stopifnot(is.character(key))
  if (identical(key, "")) 
    stop("Missing environmental variable 'wu_key' containing Weather Underground API key.")
  if (!requireNamespace("rwunderground", quietly = TRUE))
    install.packages("rwunderground", quiet = TRUE)
  if (!requireNamespace("lubridate", quietly = TRUE))
    install.packages("lubridate", quiet = TRUE)
  if (!requireNamespace("ggplot2", quietly = TRUE))
    install.packages("ggplot2", quiet = TRUE)

  wx <- get_hourly(lat, lon, key)
  tzone <- ifelse(grepl("EDT|EST", unique(wx$wu_tz)), "America/New_York", unique(wx$wu_tz))
  dts <- unique(wx$date_str)[1:8] # Only want forecast one week out
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
               hour %in% 10:14                               ~ "Midday",
               hour %in% c(23, 0:3)                          ~ "Midnight",
               TRUE                                          ~ NA_character_),
             levels = c("Midnight", "Sunrise", "Midday", "Sunset")),
           date = as.Date(ifelse(hour > 22, 
                                 as.character(as.Date(date_str) + as.difftime(1, units = "days")), 
                                 date_str))) %>%
    filter(!is.na(window)) %>% ungroup() %>%
    group_by(date, window) %>%
    summarise(n = n(),
              wind_OK = sum(wspd_kmh <= max_wsp_kmh) / n,
              rain_OK = sum(pop <= max_pop) / n) %>%
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

get_hourly <- function(lat, lon, key) {
  location <- rwunderground::set_location(lat_long = paste(lat, lon, sep = ","))
  parsed_req <- rwunderground:::wunderground_request(
    request_type = "hourly10day", location = location, date = NULL, key, message = FALSE)
  rwunderground:::stop_for_error(parsed_req)
  if (!("hourly_forecast" %in% names(parsed_req))) 
    stop("Cannot parse hourly forecast for: ", location)
  hourly_forecast <- parsed_req$hourly_forecast
  tzone <- strsplit(hourly_forecast[[1]]$FCTTIME$pretty, split = " ")[[1]][3]
  wx <- lapply(hourly_forecast, function(x) {
    data.frame(date_str = paste(x$FCTTIME$year, x$FCTTIME$mon_padded, x$FCTTIME$mday_padded, sep = "-"),
               wu_tz = tzone,
               hour = as.integer(x$FCTTIME$hour),
               tmp_c = as.numeric(x$temp[["metric"]]), 
               rh = as.numeric(x$humidity), 
               wspd_kmh = as.numeric(x$wspd[["english"]]), 
               wdir = x$wdir$dir, 
               pop = as.numeric(x$pop), 
               mslp = as.numeric(x$mslp[["english"]]),
               stringsAsFactors = FALSE)
  })
  wx <- rwunderground:::encode_NA(do.call("rbind", wx))
}
