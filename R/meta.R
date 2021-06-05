library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(doSNOW)
library(htmlwidgets)

#options(browser = "/usr/bin/chromium")

kg = 73
age = as.numeric(difftime(Sys.Date(), as.Date("1994-02-24"), unit = "weeks")) / 52.25
#hr_max = 220 - trunc(age)
hr_max = 172

grad_max = 0.3
grad_min = -0.5
threshold_pause = 0.3

power_zones = list(
  zone = c("Z1: Recovery", "Z2: Endurance", "Z3: Tempo", "Z4: Lactate Threshold",
    "Z5: VO2 Max", "Z6: Anaerobic Capacity", "Z7: Neuromuscular Power"),
  breaks = c(-Inf, 0.55, 0.75, 0.9, 1.05, 1.2, 2, Inf)
)

training_zones = list(
  zone = paste0("Zone", seq_len(5L)),
  breaks = c(-Inf, 122, 132, 143, 149, Inf)
) # 111-122, 122-132, 132-143, 143-149, 149-157
lactate_thresholds = list(LT1 = 129, LT2 = 150)


