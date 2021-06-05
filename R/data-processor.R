dataFileFromFit = function(fit_file) {
  rda_file = paste0(strsplit(fit_file, split = "-ELEMNT")[[1]][1], ".Rda")
  rda_file = strsplit(rda_file, split = "/")[[1]][2]
  rda_file = paste0("data/", rda_file)
  return(rda_file)
}

powerFileFromFit = function(fit_file) {
  rda_file = paste0(strsplit(fit_file, split = "-ELEMNT")[[1]][1], "-power.Rda")
  rda_file = strsplit(rda_file, split = "/")[[1]][2]
  rda_file = paste0("data/", rda_file)
  return(rda_file)
}

processFitFile = function(fit_file) {
  if (! dir.exists("data")) dir.create("data")

  if (! file.exists(fit_file)) stop("Fit file", fit_file, "does not exist!")

  rda_file = dataFileFromFit(fit_file)
  if (! file.exists(rda_file)) {

    fit = FITfileR::readFitFile(fit_file)

    rcrds = arrange(do.call(bind_rows, FITfileR::records(fit)), timestamp) %>%
      mutate(
        seconds_interval = c(1, as.integer(difftime(timestamp[-1], timestamp[-length(timestamp)], units = "secs"))),
        seconds    = as.integer(difftime(timestamp, timestamp[1], units = "secs")),
        gradient   = diff(c(0, altitude)) / diff(c(0, distance)),
        mgained    = c(altitude[1], diff(altitude)),
        pause      = ifelse(speed < threshold_pause, TRUE, FALSE),
        speed      = speed * 3.6,
        heart_rate = ifelse(heart_rate > hr_max, NA, heart_rate),
        training_zones = cut(x = heart_rate, breaks = training_zones$breaks, label = training_zones$zone)
    )
    if (! "power" %in% names(rcrds)) {
      rcrds$power = NA_real_
      rcrds$power_zone = NA_character_
    } else {
      rcrds = rcrds %>%
        mutate(power_zone = cut(x = power, breaks = power_zones$breaks * ftp(power), labels = power_zones$zone))
    }
    save(rcrds, file = rda_file)
    message("Save processed ", fit_file, " as ", rda_file)
  } else {
    message("File ", fit_file, " already processed as ", rda_file)
  }
}

updateSummary = function() {
  if (! dir.exists("data")) dir.create("data")
  fit_files = list.files("fit-files", full.names = TRUE)
  summary_file = "data/summary.Rda"

  if (file.exists(summary_file)) {
    load(summary_file)
  } else {
    smr = NULL
  }

  for (fit_file in fit_files) {
    processFitFile(fit_file)
    if (! fit_file %in% smr$source) {
      data_file = dataFileFromFit(fit_file)
      load(data_file)

      date  = format(rcrds$timestamp[1], format = "%Y-%m-%d")
      np    = normalizedPower(rcrds)
      np    = ifelse(is.nan(np), NA, np)
      ftpow = ftp(rcrds$power)
      ftpow = ifelse(is.infinite(ftpow), NA, ftpow)
      km    = max(rcrds$distance, na.rm = TRUE) / 1000
      secs_paused  = sum(rcrds$seconds_interval[rcrds$seconds_interval > 1])
      hours_active = (sum(rcrds$seconds_interval) - secs_paused) / 60^2
      avg_speed    = km / hours_active
      climbed      = sum(rcrds$mgained[(rcrds$mgained > 0) & (rcrds$mgained < 10) & (rcrds$seconds_interval == 1)], na.rm = TRUE)
      max_speed    = max(rcrds$speed, na.rm = TRUE)
      max_watt     = max(rcrds$power, na.rm = TRUE)
      max_watt     = ifelse(is.infinite(max_watt), NA, max_watt)
      avg_hr       = mean(rcrds$heart_rate, na.rm = TRUE)
      max_hr       = max(rcrds$heart_rate, na.rm = TRUE)

      # How hard the ride was. Calculated as effective power / FTP, where FTP is xxxW
      # for this ride. Shorter rides can have higher intensities. A one hour time trial
      # will have by definition an intensity of 100%. Andrew Coggan came up with this.
      intensity = np / ftpow

      # How smooth or variable the ride was. Calculated as (effective power - average power) / average power.
      # A ride at a constant power output will have a variability of 0%, while hillier courses and
      # interval training will result in higher values.
      variability = (np - mean(rcrds$power, na.rm = TRUE)) / mean(rcrds$power, na.rm = TRUE)

      # How taxing on the body the ride was. Calculated as intensity2 × time riding. A one hour time
      # trial will by definition have a training load of 100. Andrew Coggan also came up with this.
      training_load = intensity^2 * (hours_active * 60)

      new_entry = data.frame(
        date   = date,
        source = fit_file,
        np   = np,
        ftp  = ftpow,
        km   = km,
        secs_paused  = secs_paused,
        hours_active = hours_active,
        avg_speed    = avg_speed,
        climbed      = climbed,
        max_speed    = max_speed,
        max_watt     = max_watt,
        avg_hr       = avg_hr,
        max_hr       = max_hr,
        intensity    = intensity,
        variability  = variability,
        load         = training_load)

      smr = rbind(smr, new_entry)
      save(smr, file = summary_file)
    }
  }
}

updatePowerCurves = function() {
  if (! dir.exists("data")) dir.create("data")

  fit_files = list.files("fit-files", full.names = TRUE)
  pc_secs   = c(seq_len(5 * 60), seq(65, 30 * 60, 5), seq(1830, 120 * 60, 30), seq(7320, 4 * 60 * 60, 120))

  for (fit_file in fit_files) {
    power_file = powerFileFromFit(fit_file)
    if (! power_file %in% list.files("data", full.names = TRUE)) {
      data_file = dataFileFromFit(fit_file)
      load(data_file)

      if (! all(is.na(rcrds$power))) {
        date  = format(rcrds$timestamp[1], format = "%Y-%m-%d")
        pc    = powerCurve(rcrds$power, pc_secs)
        df_pc = data.frame(date = date, max_avg_power = pc, secs = pc_secs)

        save(df_pc, file = power_file)
        message("Save power curve from ", fit_file, " as ", power_file)
      } else {
        message("No power in file ", fit_file)
      }
    } else {
      message("Power file ", power_file, " already processed")
    }
  }
}

gatherPowerCurves = function() {
  pfs = list.files("data", full.names = TRUE)
  pfs = pfs[grepl("-power", pfs)]

  ll_power = list()
  for (pf in pfs) {
    load(pf)
    ll_power[[pf]] = df_pc
  }
  dpc = do.call(rbind, ll_power)
  dates = unique(dpc$date)
  df_dates = data.frame(date = dates, order = order(dates, decreasing = TRUE))

  return(left_join(dpc, df_dates, by = "date"))
}

extractTextBetween = function(html, tag) {
  lines = readLines(html)
  idx_tag = grep(paste0("<", tag, ">"), lines)
  lines[seq(idx_tag[1] + 1, idx_tag[2] - 1)]
}
