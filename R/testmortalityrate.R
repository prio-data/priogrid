# # --- TEST ---

start_read <- Sys.time()
r_data <- read_sedac_global_infant_mortality_rates()
time_read <- Sys.time() - start_read
print(paste("Time to read data:", time_read))

start_gen <- Sys.time()
r_processed <- gen_sedac_global_infant_mortality_rates(data = r_data)
time_gen <- Sys.time() - start_gen
print(paste("Time to process data:", time_gen))

pg <- prio_blank_grid()
df <- as.data.frame(c(pg, r_processed))
plot(r_processed)
