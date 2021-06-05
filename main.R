source("R/meta.R")
source("R/cycle-analytics.R")
source("R/data-processor.R")
source("R/widget-processor.R")
source("R/site-processor.R")
source("R/figures.R")

updateSummary()
updatePowerCurves()

if (FALSE) file.remove(summary_file)
summary_file = "data/summary.Rda"
load(summary_file)
smr = smr[order(smr$date), ]

# Create html subsite menu:
html_sub_dir = "html-diary/sub-sites/"
diary_files_dir = "diary-files/"
template_dir = "templates/"
index = "html-diary/index.html"
fit_files = rev(list.files("fit-files"))

index_menu = siteMenu(fit_files, html_sub_dir, diary_files_dir, smr, "sub-sites/")
includeLinesInDiv(index, index_menu, "<SUBSITES-MENU>", index)
createSubSites(fit_files, html_sub_dir, diary_files_dir, template_dir, smr)



pc_labels = c(paste0(c(1, 2, 5, 10, 30), "Sec."), paste0(c(1, 2, 5, 10, 20), "Min."), paste0(c(1, 2), "H"))
pc_breaks = c(1, 2, 5, 10, 30, 60 * c(1, 2, 5, 10, 20, 60, 120))


pcs = gatherPowerCurves()
n_last = 10
pcs = pcs %>%
  filter(order <= n_last) %>%
  mutate(alpha = 1 - (order - 1) / (n_last * 2)) %>%
  mutate(Power = max_avg_power, Seconds = secs, Date = date)

gg = ggplot() +
  geom_line(data = pcs %>%
      group_by(Seconds) %>%
      summarize(Power = max(Power, na.rm = TRUE)),
    aes(x = Seconds, y = Power),
    size = 1.3,
    show.legend = FALSE,
    color = "white") +
  geom_line(data = pcs, aes(x = Seconds, y = Power, color = Date), show.legend = FALSE) + #, alpha = alpha)) +
  scale_x_continuous(trans = "log10", breaks = pc_breaks, labels = pc_labels) +
  scale_y_continuous(breaks = seq(0, 1200, by = 200)) +
  ggsci::scale_color_tron() +
  theme_minimal()

pfont = list(
  family = "'Work Sans', 'Cardo', serif",
  size = 12,
  color = "white"
)
tickfont = list(
  family = "'Work Sans', 'Cardo', serif",
  size = 10,
  color = "white"
)
x = function(title = "") {
  list(
    title = title,
    titlefont = pfont,
    tickangle = 45,
    tickfont = tickfont
  )
}
y = function(title = "") {
  list(
    title = title,
    titlefont = pfont,
    tickfont = tickfont
  )
}
ggplotly(gg, width = 400, height = 300) %>%
  layout(
    plot_bgcolor  = "rgba(0, 0, 0, 0)",
    paper_bgcolor = "rgba(0, 0, 0, 0)",
    plot_bgcolor   = "rgba(0, 0, 0, 0)",
    showlegend = FALSE,
    xaxis = x(),
    yaxis = y("Maximum Average Power")) %>%
  saveWidget("power-curves.html", selfcontained = FALSE, libdir = "lib")


## UPDATE INDEX
new_lines = extractWidget("power-curves.html", lib = "html-diary/lib")
includeLinesInDiv("html-diary/index.html",
  new_lines,
  div = "<POWERCURVE>",
  write_to_file = "html-diary/index-temp.html")
includeLinesInDiv("html-diary/index-temp.html",
  as.character(trunc(sum(smr$km))),
  div = "<KMTOTAL>",
  write_to_file = "html-diary/index-temp.html")
includeLinesInDiv("html-diary/index-temp.html",
  as.character(trunc(sum(smr$climbed))),
  div = "<ALTITUDETOTAL>",
  write_to_file = "html-diary/index-temp.html")


# Create html subsite menu:
html_sub_dir = "html-diary/sub-sites/"
diary_files_dir = "diary-files/"
template_dir = "templates/"
index = "html-diary/index-test.html"
fit_files = rev(list.files("fit-files"))

menu = siteMenu(fit_files, html_sub_dir, diary_files_dir, smr, "sub-sites/")
includeLinesInDiv(index, menu, "<SUBSITES-MENU>", index)
createSubSites(fit_files, html_sub_dir, diary_files_dir, template_dir, smr)

#menu = lapply(rev(fit_files), function(ff) {
  #ffile = strsplit(ff, split = "-ELEMNT")[[1]][1]
  #sub_file = paste0(html_sub_dir, ffile, ".html")
  #if (file.exists(sub_file)) {
    #message("File ", sub_file, " already exists!")
  #} else {

    ## Get title:
    #dfile = paste0(diary_files_dir, ffile, ".txt")
    #if (file.exists(dfile)) {
      #tt = extractTextBetween(dfile, "TITLE")
    #} else {
      #tt = ffile
    #}

    ## Get Meta:
    #meta_idx = grep(ffile, smr$source)
    #km = smr$km[meta_idx]
    #dte = smr$date[meta_idx]
    #hrs = smr$hours_active[meta_idx]
    #hrs = paste0(trunc(hrs), ":", trunc((hrs - trunc(hrs)) * 60), "h")
    #grid_date = paste0("  <p class='grid-date'><b>", dte, " ", hrs, "</b></p>")
    #grid_meta = paste0("  <p class='grid-date'>", round(km, 2), "km - ", tt, "</p>")
    #return(c(
      #paste0("<div class='grid-item' id='", sub_file, "'>"),
      #grid_date,
      #grid_meta,
      #"</div>"
    #))
  #}
#})





smr %>%
  na.omit() %>%
  mutate(FTP = ftp, Date = date)
  gg = ggplot(aes(x = Date, y = FTP, color = "FTP")) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    ggsci::scale_color_tron() +
    theme_minimal()

ggplotly(gg, width = 400, height = 300) %>%
  layout(
    plot_bgcolor  = "rgba(0, 0, 0, 0)",
    paper_bgcolor = "rgba(0, 0, 0, 0)",
    plot_bgcolor   = "rgba(0, 0, 0, 0)",
    showlegend = FALSE,
    xaxis = x(),
    yaxis = y("Functional-Threshold-Power (FTP)")) %>%
  saveWidget("ftp.html", selfcontained = FALSE, libdir = "lib")





load("data/2021-06-01-112555.Rda")

rcrds %>%
  filter(gradient < grad_max, gradient > -grad_max) %>%
  ggplot(aes(x = seconds, y = altitude, color = training_zones)) +
    geom_point()

rcrds %>%
  filter(gradient < grad_max, gradient > -grad_max) %>%
  ggplot(aes(x = gradient, y = power, color = training_zones)) +
    geom_point()



### =============================================================== ###
###
###                              SETUP
###
### =============================================================== ###

library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(doSNOW)
library(htmlwidgets)

options(browser = "/usr/bin/chromium")

kg = 73
age = as.numeric(difftime(Sys.Date(), as.Date("1994-02-24"), unit = "weeks")) / 52.25
hr_max = 220 - trunc(age)

grad_max = 0.3
grad_min = -0.5
threshold_pause = 0.3

power_zones = list(
  zone = c("Z1: Recovery", "Z2: Endurance", "Z3: Tempo", "Z4: Lactate Threshold",
    "Z5: VO2 Max", "Z6: Anaerobic Capacity", "Z7: Neuromuscular Power"),
  breaks = c(-Inf, 0.55, 0.75, 0.9, 1.05, 1.2, 2, Inf)
)

## Determined on 29.05.2021:
training_zones = list(
  zone = paste0("Zone", seq_len(5L)),
  breaks = c(-Inf, 122, 132, 143, 149, Inf)
) # 111-122, 122-132, 132-143, 143-149, 149-157
lactate_thresholds = list(LT1 = 129, LT2 = 150)

### =============================================================== ###
###
###                            FUNCTIONS
###
### =============================================================== ###

normalizedPower = function(records) {
  timestamp = records$timestamp
  power = records$power
  idx = timestamp >= timestamp[1] + 30

  roll_mean = vapply(X = which(idx), FUN.VALUE = numeric(1L), FUN = function(i) {
    idx_current = rep(FALSE, length(timestamp))
    idx_current[seq_len(i)] = TRUE
    idx_last30 = timestamp > timestamp[i] - 30
    mean(power[idx_last30 & idx_current])
  })
  mean(roll_mean^4)^0.25
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
tssSec = function(sec, np, intf, ftp) {
  return((sec * np * intf) / (ftp * 3600) * 100)
}

### =============================================================== ###
###
###                              DATA
###
### =============================================================== ###

if (FALSE) remotes::install_github("grimbough/FITfileR")

fit_file = "~/Downloads/2021-05-21-082730-ELEMNT BOLT 9493-34-0.fit"
fit = FITfileR::readFitFile(fit_file)

rcrds = arrange(do.call(bind_rows, FITfileR::records(fit)), timestamp) %>%
  mutate(
    seconds_interval = c(1, as.integer(difftime(timestamp[-1], timestamp[-length(timestamp)], units = "secs"))),
    seconds    = as.integer(difftime(timestamp, timestamp[1], units = "secs")),
    gradient   = diff(c(0, altitude)) / diff(c(0, distance)),
    mgained    = c(altitude[1], diff(altitude)),
    pause      = ifelse(speed < threshold_pause, TRUE, FALSE),
    speed      = speed * 3.6,
    power_zone = cut(x = power, breaks = power_zones$breaks * ftp(power), labels = power_zones$zone),
    heart_rate = ifelse(heart_rate > hr_max, NA, heart_rate),
    training_zones = cut(x = heart_rate, breaks = training_zones$breaks, label = training_zones$zone)
  )


### =============================================================== ###
###
###                        SUMMARY VALUES
###
### =============================================================== ###

date = format(rcrds$timestamp[1], format = "%Y-%m-%d")

np    = normalizedPower(rcrds)
ftpow = ftp(rcrds$power)
km    = max(rcrds$distance) / 1000
secs_paused  = sum(rcrds$seconds_interval[rcrds$seconds_interval > 1])
hours_active = (sum(rcrds$seconds_interval) - secs_paused) / 60^2
avg_speed    = km / hours_active
climbed      = sum(rcrds$mgained[rcrds$mgained > 0], na.rm = TRUE)
max_speed    = max(rcrds$speed, na.rm = TRUE)
max_watt     = max(rcrds$power, na.rm = TRUE)
avg_hr       = mean(rcrds$heart_rate, na.rm = TRUE)
max_hr       = max(rcrds$heart_rate, na.rm = TRUE)


# How hard the ride was. Calculated as effective power / FTP, where FTP is xxxW
# for this ride. Shorter rides can have higher intensities. A one hour time trial
# will have by definition an intensity of 100%. Andrew Coggan came up with this.
intensity = np / ftp

# How smooth or variable the ride was. Calculated as (effective power - average power) / average power.
# A ride at a constant power output will have a variability of 0%, while hillier courses and
# interval training will result in higher values.
variability = (np - mean(rcrds$power, na.rm = TRUE)) / mean(rcrds$power, na.rm = TRUE)

# How taxing on the body the ride was. Calculated as intensity2 × time riding. A one hour time
# trial will by definition have a training load of 100. Andrew Coggan also came up with this.
training_load = intensity^2 * (hours_active * 60)
tss = tssSec(hours_active * 3600, np, intensity, ftpow)

### =============================================================== ###
###
###                            FIGURES
###
### =============================================================== ###

### POWER CURVES
### ============================================

pc_labels = c(paste0(c(1, 2, 5, 10, 30), "Sec."), paste0(c(1, 2, 5, 10, 20), "Min."), paste0(c(1, 2), "H"))
pc_breaks = c(1, 2, 5, 10, 30, 60 * c(1, 2, 5, 10, 20, 60, 120))
pc_secs   = c(seq_len(5 * 60), seq(65, 30 * 60, 5), seq(1830, 120 * 60, 30), seq(7320, 4 * 60 * 60, 120))

#power = rcrds$power
#maxMeanSecs(power = power, window = 7200)
#maxMeanSecs(power = power, window = 20 * 60)

time = proc.time()
pc = powerCurve(rcrds$power, pc_secs) ## 2 M 30 Sec for 4:10 H ride
time = proc.time() - time

gg = data.frame(time = pc_secs, power = pc) %>%
  ggplot(aes(x = time, y = power)) +
    geom_line() +
    scale_x_continuous(trans = "log10", breaks = pc_breaks, labels = pc_labels) +
    scale_y_continuous(breaks = seq(0, 1200, by = 200)) +
    xlab("") +
    ylab("Maximum Average Power") +
    theme_minimal()

p = plotly::ggplotly(gg, width = 500, height = 300)
p

#saveWidget(p, "p1.html", selfcontained = TRUE)
saveWidget(p, "p1.html", selfcontained = FALSE, libdir = "lib")


### RIDE
### ============================================


rcrds_normalized = rcrds %>%
    select(timestamp, altitude, power, speed, cadence, temperature, heart_rate, gradient) %>%
    filter(gradient <= grad_max, gradient >= grad_min) %>%
    pivot_longer(cols = altitude:gradient)

tmp = rcrds_normalized %>% group_by(name) %>% summarize(rmean = ma(value, 30), timestamp = timestamp)

tmp %>%
  ungroup() %>%
  na.omit() %>%
  transform(id = as.integer(factor(name))) %>%
  plot_ly(x = ~timestamp, y = ~rmean, color = ~name, colors = "Dark2", yaxis = ~paste0("y", id)) %>%
  add_lines() %>%
  layout(hovermode = "x unified") %>%
  subplot(nrows = 7, shareX = TRUE)

rcrds %>%
  filter(gradient <= grad_max, gradient >= grad_min) %>%
  ggplot(aes(x = timestamp, y = altitude)) +
    #geom_vline(aes(xintercept = timestamp, color = power_zone), alpha = 0.5) +
    #geom_segment(aes(y = 0, xend = timestamp, yend = altitude, color = power_zone)) +
    geom_segment(aes(y = 0, xend = timestamp, yend = altitude, color = gradient), alpha = 0.2) +
    geom_line()

rcrds %>%
  filter(gradient <= grad_max, gradient >= grad_min) %>%
  ggplot(aes(x = timestamp, y = altitude)) +
    #geom_vline(aes(xintercept = timestamp, color = power_zone), alpha = 0.5) +
    geom_segment(aes(y = 0, xend = timestamp, yend = altitude, color = power_zone)) +
    #geom_segment(aes(y = 0, xend = timestamp, yend = altitude, color = training_zones)) +
    geom_line() +
    theme_minimal() +
    ggsci::scale_color_jco()

### MAP
### ============================================

#pal_base_colors = rev(RColorBrewer::brewer.pal(5, "RdYlGn"))
#pal_base_colors = rev(ggsci::pal_startrek()(3))
pal_base_colors = rev(ggsci::pal_uchicago()(3))

nreduced = 2000L
rsmall = rcrds[unique(as.integer(seq(from = 1, to = nrow(rcrds), length.out = nreduced))), ]
rsmall$gradient = zoo::rollmean(rsmall$gradient, k = 3, align = "right", fill = rsmall$gradient[1:2])

idx_grad = (rsmall$gradient <= grad_max) & (rsmall$gradient >= grad_min)
myPal = colorNumeric(pal_base_colors, na.omit(rsmall$gradient[rsmall$gradient < grad_max]))
rsmall$clr = myPal(rsmall$gradient)
rsmall$lab = paste0("<p>Altitude: ", rsmall$altitude, "\n</p><p>Speed: ", rsmall$speed, "</p>",
  "<p>Gradient: ", round(rsmall$gradient * 100, 2), "% </p><p>Power: ", rsmall$power, "</p><p>Distance: ",
  round(rsmall$distance / 1000, 2), " km</p>")

m = rsmall %>%
  filter(gradient <= grad_max, gradient >= grad_min) %>%
  leaflet() %>%
  addProviderTiles("Esri.WorldStreetMap") %>%
  addLegend("topright",
    pal = myPal,
    values = ~na.omit(gradient),
    title = "Slope")

for (i in seq_len(nrow(rsmall))[-1]) {
  m = addPolylines(m, lng = ~position_long, lat = ~position_lat,
    data = rsmall[c(i - 1, i), ], color = ~clr, label = ~htmltools::HTML(lab[1]),
    opacity = 0.7, labelOptions(noHide = FALSE))
}
m
saveWidget(m, paste0("map-", date, ".html"), selfcontained = TRUE)


### DATA:
### ============================================

rcrds %>%
  filter(gradient <= grad_max, gradient >= grad_min) %>%
  ggplot(aes(x = gradient, y = speed, color = power_zone)) +
    geom_point(alpha = 0.5, size = 3) +
    ggsci::scale_color_jco() +
    theme_minimal() +
    xlab("Slope") +
    ylab("Speed")

rcrds %>%
  filter(gradient <= grad_max, gradient >= grad_min) %>%
  ggplot(aes(x = gradient, y = speed, color = training_zones)) +
    geom_point(alpha = 0.5, size = 3) +
    ggsci::scale_color_jco() +
    theme_minimal() +
    xlab("Slope") +
    ylab("Speed")

rcrds %>%
  ggplot(aes(x = heart_rate)) +
    geom_histogram(alpha = 0.2, bins = 60L, color = "black") +
    geom_vline(xintercept = training_zones$breaks)

rcrds %>%
  ggplot(aes(x = gradient, y = power, color = training_zones)) +
    geom_point(alpha = 0.5, size = 3) +
    ggsci::scale_color_jco() +
    theme_minimal()

rcrds %>%
  ggplot(aes(x = training_zones)) + geom_bar()

tssSec = function(sec, np, intf, ftp) {
  return((sec * np * intf) / (ftp * 3600) * 100)
}
