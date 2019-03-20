#' Retrieve possible sequences for Black Rail broadcasts by geographic location
#'  and start date
#'  
#' Assumes  
#'
#' @param lon numeric scalar of position longitude (decimal degrees; WGS84)
#' @param lat numeric scalar of position latitude (decimal degrees; WGS84)
#' @param start character of start date ("YYYY-MM-DD"); default is
#'  tomorrow's date
#' @param out_tz time zone specification to be used for output. System-specific
#'   (see \link[base]{timezones}. Default is "America/New_York".

aru_sequences <- function(lat, lon, start = Sys.Date() + 1,
                          out_tz = "America/New_York") {
  if (!requireNamespace("remotes", quietly = TRUE))
    install.packages("remotes", quiet = TRUE)
  if (!requireNamespace("nrsmisc", quietly = TRUE))
    remotes::install_github("adamdsmith/nrsmisc")
  out <- lapply(start, function(s) {
    s <- as.POSIXct(as.character(s), tz = out_tz)
    end <- s + as.difftime(4, units = "days")
    
    sun <- nrsmisc::get_sun(lon, lat, s, end, direction = c("sunrise", "sunset"),
                            out_tz = out_tz) %>%
      mutate(day = seq(5),
             midnight = as.POSIXct(paste(date_str, "00:00:00"), tz = out_tz),
             # noon = as.POSIXct(paste(date_str, "12:00:00"), tz = out_tz),
             sunrise = sunrise + as.difftime(30, units = "mins"),
             sunset = sunset - as.difftime(30, units = "mins")) %>%
      select(date_str, day, midnight, sunrise, sunset) # noon
    
    aru_seq_nms <- c("SrSsMn", "SrMnSs", "SsSrMn", "SsMnSr", "MnSrSs", "MnSsSr")
    aru_seqs <- list(
      SrSsMn = list(day = c(1,1,3), window = c("sunrise", "sunset", "midnight")),
      SrMnSs = list(day = c(1,2,2), window = c("sunrise", "midnight", "sunset")),
      SsSrMn = list(day = c(1,3,4), window = c("sunset", "sunrise", "midnight")),
      SsMnSr = list(day = c(1,3,4), window = c("sunset", "midnight", "sunrise")),
      MnSrSs = list(day = c(1,2,2), window = c("midnight", "sunrise", "sunset")),
      MnSsSr = list(day = c(1,1,3), window = c("midnight", "sunset", "sunrise")))
    
    tmp <- lapply(aru_seq_nms, function(sq) {
      seq_id <- which(aru_seq_nms == sq)
      days <- aru_seqs[[sq]]$day
      windows <- aru_seqs[[sq]]$window
      progs <- sapply(seq_along(days), function(i) {
        as.character(format(sun[sun$day == days[i], windows[i]], format = "%a %H:%M (%d %b)"))})
      prog_df <- data.frame(start_date = s,
                            day_of_week = format(s, "%A"),
                            seq_nm = sq, 
                            seq_id,
                            prog1 = progs[1],
                            prog2 = progs[2],
                            prog3 = progs[3],
                            # prog4 = progs[4], 
                            stringsAsFactors = FALSE)
    })
    tmp <- bind_rows(tmp)
  })
  out <- bind_rows(out) %>% arrange(start_date)
  out
}
