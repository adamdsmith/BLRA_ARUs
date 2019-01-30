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

#' Retrieve possible sequences for Black Rail broadcasts by geographic location
#'  and start date
#'  
#' Assumes  
#'
#'
#' @param lon numeric scalar of position longitude (decimal degrees; WGS84)
#' @param lat numeric scalar of position latitude (decimal degrees; WGS84)
#' @param start character or Date scalar of start date ("YYYY-MM-DD"); default is
#'  tomorrow's date
#' @param out_tz time zone specification to be used for output. System-specific
#'   (see \link[base]{timezones}. Default is "America/New_York".

aru_sequences <- function(lat, lon, start = Sys.Date() + 1,
                         out_tz = "America/New_York") {
  if (!requireNamespace("remotes", quietly = TRUE))
    install.packages("remotes", quiet = TRUE)
  if (!requireNamespace("nrsmisc", quietly = TRUE))
    remotes::install_github("adamdsmith/nrsmisc")
  start <- as.Date(start)
  end <- start + 4
  sequence <- seq(from = start, to = end, by = "days")
  
  sun <- nrsmisc::get_sun(lon, lat, start, end, direction = c("sunrise", "sunset"),
                          out_tz = out_tz) %>%
    mutate(day = seq(length(sequence)),
           midnight = as.POSIXct(paste(date_str, "00:00:00"), tz = out_tz),
           noon = as.POSIXct(paste(date_str, "12:00:00"), tz = out_tz),
           sunrise = sunrise + as.difftime(30, units = "mins"),
           sunset = sunset - as.difftime(30, units = "mins")) %>%
    select(date_str, day, midnight, sunrise, noon, sunset)

  aru_seq_nms <- c("SR_start", "SS_start", "Midnight_start", "Noon_start")
  aru_seqs <- list(
    SR_start = list(day = c(1, 1, 3, 3),
                    window = c("sunrise", "sunset", "midnight", "noon")),
    SS_start = list(day = c(1, 2, 3, 4),
                    window = c("sunset", "noon", "sunrise", "midnight")),
    Midnight_start = list(day = c(1, 3, 4, 5),
                    window = c("midnight", "sunrise", "noon", "sunset")),
    Noon_start = list(day = c(1, 2, 2, 4),
                      window = c("noon", "midnight", "sunset", "sunrise")))
  
  ARU_sequence_programs <- lapply(aru_seq_nms, function(s) {
    seq_id <- which(aru_seq_nms == s)
    days <- aru_seqs[[s]]$day
    windows <- aru_seqs[[s]]$window
    progs <- sapply(seq_along(days), function(i) {
      as.character(format(sun[sun$day == days[i], windows[i]], format = "%d %b %H:%M"))})
    tmp <- data.frame(seq_nm = s, 
                      seq_id,
                      start_date = start,
                      prog1 = progs[1],
                      prog2 = progs[2],
                      prog3 = progs[3],
                      prog4 = progs[4], stringsAsFactors = FALSE)
  })
  ARU_sequence_programs <- bind_rows(ARU_sequence_programs)
  View(ARU_sequence_programs)
}
