process_tile_pair <- function(r_2019, r_evo) {
  # --- Process WSF 2019 ---
  r_2019[r_2019 == 0] <- NA         # Mask out non-built-up
  r_2019[r_2019 == 255] <- 2019     # Recode built-up as 2019

  # --- Process WSF Evolution ---
  r_evo[r_evo < 1985] <- NA         # Mask out invalid / early values

  # --- Align WSF Evolution to WSF 2019 ---
  r_evo_upsampled <- resample(r_evo, r_2019, method = "near")

  # --- Identify overlapping built-up areas ---
  already_built <- !is.na(r_2019) & !is.na(r_evo_upsampled)

  # --- Separate new vs previously built in 2019 ---
  r_2019_newest <- mask(r_2019, already_built, maskvalue = TRUE)
  r_2019_old    <- mask(r_2019, already_built, maskvalue = FALSE)

  # --- Optional: Binary classification (1 = built-up) ---
  r_2019_newest_bin <- classify(r_2019_newest, matrix(c(2019, 1), ncol = 2), right = NA)
  r_2019_old_bin    <- classify(r_2019_old, matrix(c(2019, 1), ncol = 2), right = NA)

  # --- Return all outputs needed downstream ---
  return(list(
    r_evo_upsampled     = r_evo_upsampled,
    r_2019_newest       = r_2019_newest
  ))
}
