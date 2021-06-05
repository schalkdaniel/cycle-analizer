normalizedPower = function(records) {
  timestamp = records$timestamp
  power = records$power
  idx = timestamp >= timestamp[1] + 30

  roll_mean = vapply(X = which(idx), FUN.VALUE = numeric(1L), FUN = function(i) {
    idx_current = rep(FALSE, length(timestamp))
    idx_current[seq_len(i)] = TRUE
    idx_last30 = timestamp > timestamp[i] - 30
    mean(power[idx_last30 & idx_current], na.rm = TRUE)
  })
  mean(roll_mean^4, na.rm = TRUE)^0.25
}

ma = function(x, k = 3) zoo::rollmeanr(x, k = k, na.pad = TRUE, na.rm = TRUE)

maxMeanSecs = function(power, window) {
  if (window > length(power)) return(NA)
  if (window == 1) return(max(power, na.rm = TRUE))

  rms = ma(power, k = window)
  return(max(rms, na.rm = TRUE))
}

ftp = function(power) maxMeanSecs(power, 3600)

powerCurve = function(power, pc_secs, ncores = parallel::detectCores() - 1) {
  idx_split = as.integer(seq(1, length(pc_secs), length.out = ncores + 1))
  ll_idx = lapply(seq_len(length(idx_split) - 1), function(i) seq(idx_split[i], idx_split[i + 1] - 1))
  ll_idx[[length(ll_idx)]] = c(ll_idx[[length(ll_idx)]], length(pc_secs))

  cl = makeCluster(ncores)
  registerDoSNOW(cl)

  pc = foreach(idx = ll_idx, .combine = "c", .export = c("maxMeanSecs", "ma")) %dopar% {
    vapply(X = idx, FUN.VALUE = numeric(1L), FUN = function(i) maxMeanSecs(power, pc_secs[i]))
  }
  stopCluster(cl)
  return(pc)
}

