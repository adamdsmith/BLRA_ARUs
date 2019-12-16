pacman::p_load(googlesheets,lubridate, dplyr, openxlsx, tuneR)
segfiles <- dir("E:/2019_ARU_files/Training_Files/Segmented/",
                pattern = ".wav$", full.names = TRUE)
audio <- pbapply::pblapply(segfiles, function(i) {
  tmp <- readWave(i, header = TRUE)
  sample_rate <- tmp$sample.rate
  length_s <- round(tmp$samples / sample_rate)
  tibble(length_s = as.integer(length_s), sample_rate)
})
audio <- bind_rows(audio)

log <- tibble(fn = basename(segfiles)) %>%
  mutate(rec_base_dt = ymd_hms(gsub("^.*_[0,1]_(\\d{8})_(\\d{6})_.*$", "\\1 \\2", fn), tz = "America/New_York"),
         rec_min = as.integer(gsub("^.*_[0,1]_\\d{8}_\\d{6}_\\d{3}_(\\d{3}).*$", "\\1", fn)) - 1L,
         rec_start_dt = rec_base_dt + as.difftime(rec_min, units = "mins")) %>%
  bind_cols(audio)

# Load known vocalization log
fullfiles <- dir("E:/2019_ARU_files/Training_Files", pattern = ".wav$", full.names = TRUE)
audio <- pbapply::pblapply(fullfiles, function(i) {
  tmp <- readWave(i, header = TRUE)
  sample_rate <- tmp$sample.rate
  length_s <- round(tmp$samples / sample_rate)
  tibble(length_s = as.integer(length_s), sample_rate)
})
audio <- bind_rows(audio)
full_log <- bind_cols(tibble(filename = basename(fullfiles)), audio)

gs <- googlesheets::gs_title("BLRA known vocalization log", verbose = FALSE)
blra <- googlesheets::gs_read(gs, verbose = FALSE, col_types = "c______________cc") %>%
  left_join(full_log, by = "filename") %>%
  filter(!is.na(length_s))

# Separate out handful of files targeting specific calls
# All are <= 1 minute files
one_offs <- filter(blra, length_s <= 60) %>%
  mutate(kkd = !is.na(kkd_mins),
         non_kkd = !is.na(non_kkd_mins),
         fn_seg = filename) %>%
  select(filename, fn_seg, kkd, non_kkd)

# Separate all 1-min segments containing non-KKD call types
# These are gold and must be included in training...
non_kkd <- filter(blra, !is.na(non_kkd_mins), length_s > 60) %>%
  tidyr::separate_rows(non_kkd_mins, sep = ",", convert = TRUE) %>%
  mutate(non_kkd = TRUE,
         fn_seg = sprintf(paste0(tools::file_path_sans_ext(filename), "_%03d", ".wav"), non_kkd_mins + 1)) %>%
  select(filename, fn_seg, non_kkd)

# On to the KKDs
# The approach is to enumerate how many minutes contain KKD within a file
# and keep 5 KKD segments at random (all KKD segments will be retained from 
# those files with < 5)
kkd <- filter(blra, !is.na(kkd_mins), length_s > 60) %>%
  tidyr::separate_rows(kkd_mins, sep = ",", convert = TRUE) %>%
  mutate(kkd = TRUE,
         fn_seg = sprintf(paste0(tools::file_path_sans_ext(filename), "_%03d", ".wav"), kkd_mins + 1)) %>%
  select(filename, fn_seg, kkd)

(kkd_summary <- group_by(kkd, filename) %>% tally())

# Extract files/segments with <= 5 KKD segments
kkd_lte5_fn <- filter(kkd_summary, n <= 5) %>% pull(filename)
kkd_lte5 <- filter(kkd, filename %in% kkd_lte5_fn)

set.seed(704732947)
kkd_gt5 <- filter(kkd, !filename %in% kkd_lte5_fn) %>%
  group_by(filename) %>%
  sample_n(5) %>%
  ungroup()

kkd <- bind_rows(kkd_gt5, kkd_lte5)

na_false <- function(x) ifelse(is.na(x), FALSE, x)
training <- full_join(kkd, non_kkd, by = c("filename", "fn_seg")) %>%
  mutate_if(is.logical, na_false) %>%
  bind_rows(one_offs) %>%
  arrange(filename, fn_seg)
training_log <- filter(log, fn %in% training$fn_seg)
write.xlsx(training_log, file = "Output/BLRA_classifier_training_segments.xlsx")

training_segs <- pull(training_log, fn)

for (f in training_segs) {
  from <- segfiles[grep(f, segfiles, fixed = TRUE)]
  to <- file.path(dirname(from), "Classifier_training", f)
  file.rename(from, to)
}
