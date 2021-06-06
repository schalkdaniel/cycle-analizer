monthSeparator = function(df, month) {
  df_sub = df[df$month == month, ]
  edges = list(x0 = min(df_sub$woy))
  tb = table(df_sub$woy)
  tb = data.frame(woy = as.integer(names(tb)), count = as.integer(tb))

  xmin = min(df_sub$woy)
  xmax = max(df_sub$woy)
  ymin = 1
  ymax = 7

  lleft = list()
  if (df_sub$dow[1] != 7) {
    lleft[["up0"]] = list(x0 = xmin, y0 = ymin, x1 = xmin, y1 = df_sub$dow[1] + 1)
    lleft[["right0"]] = list(x0 = xmin, y0 = df_sub$dow[1] + 1, x1 = xmin + 1, y1 = df_sub$dow[1] + 1)
    lleft[["up1"]] = list(x0 = xmin + 1, y0 = df_sub$dow[1] + 1, x1 = xmin + 1, y1 = ymax + 1)
    xleft = xmin + 1
  } else {
    lleft[["up0"]] = list(x0 = xmin, y0 = ymin, x1 = xmin, y1 = ymax + 1)
    xleft = xmin
  }
  lright = list()
  if (tail(df_sub$dow, 1) != 1) {
    lright[["down0"]] = list(x0 = xmax + 1, y0 = ymax + 1, x1 = xmax + 1, y1 = tail(df_sub$dow, 1))
    lright[["left0"]] = list(x0 = xmax + 1, y0 = tail(df_sub$dow, 1), x1 = xmax, y1 = tail(df_sub$dow, 1))
    lright[["down1"]] = list(x0 = xmax, y0 = tail(df_sub$dow, 1), x1 = xmax, y1 = ymin)
    xright = xmax
  } else {
    lright[["down0"]] = list(x0 = xmax + 1, y0 = ymin, x1 = xmax + 1, y1 = ymax + 1)
    xright = xmax + 1
  }
  lup = list(data.frame(x0 = xleft, y0 = ymax + 1, x1 = xmax + 1, y1 = ymax + 1))
  ldown = list(data.frame(x0 = xmin, y0 = ymin, x1 = xright, y1 = ymin))

  lleft = list(do.call(rbind, lleft))
  lright = list(do.call(rbind, lright))

  do.call(rbind, c(lleft, lup, lright, ldown))
}




#' Calendar Heatmap
#'
#' Creates a colour coded calendar visualising time series data
#'
#' Copied from: https://dominikkoch.github.io/Calendar-Heatmap/
#'
#' @param dates A vector containing the dates in `Date` format.
#' @param values A vector containing the corresponding values as numeric.
#' @param title Main plot title (optional).
#' @param subtitle Main plot subtitle (optional).
#' @param legendtitle Legend title (optional).
#'
#' @return ggplot object
calendarHeatmap = function(dates, values, title = "", subtitle = "", legendtitle = ""){

  require(lubridate)
  require(ggplot2)
  require(plotly)

  # Parameter checks
  if (missing(dates)) stop("Need to specify a dates vector.")
  if (missing(values)) stop("Need to specify a values vector.")
  if (!is.Date(dates)) stop("dates vector need to be in Date format.")
  if (length(dates) != length(values)) stop("dates and values need to have the same length.")

  my_theme = function() {

    # Colors
    color.background = "white"
    color.text = "#22211d"

    # Begin construction of chart
    theme_bw(base_size = 15) +

      # Format background colors
      theme(panel.background = element_rect(fill = color.background, color = color.background)) +
      theme(plot.background  = element_rect(fill = color.background, color = color.background)) +
      theme(panel.border     = element_rect(color = color.background)) +
      theme(strip.background = element_rect(fill = color.background, color = color.background)) +

      # Format the grid
      theme(panel.grid.major = element_blank()) +
      theme(panel.grid.minor = element_blank()) +
      theme(axis.ticks       = element_blank()) +

      # Format the legend
      theme(legend.position = "bottom") +
      theme(legend.text     = element_text(size = 8, color = color.text)) +
      theme(legend.title    = element_text(size = 10, face = "bold", color = color.text)) +

      # Format title and axis labels
      theme(plot.title       = element_text(color = color.text, size = 20, face = "bold")) +
      theme(axis.text.x      = element_text(size = 12, color = "black")) +
      theme(axis.text.y       = element_text(size = 12, color = "black")) +
      theme(axis.title.x     = element_text(size = 14, color = "black", face = "bold")) +
      theme(axis.title.y     = element_text(size = 14, color = "black", vjust = 1.25)) +
      theme(axis.text.x      = element_text(size = 10, hjust = 0, color = color.text)) +
      theme(axis.text.y      = element_text(size = 10, color = color.text)) +
      theme(strip.text       = element_text(face = "bold")) +

      # Plot margins
      theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
  }

  # create empty calendar
  min.date = as.Date(paste(format(min(dates), "%Y"), "-1-1", sep = ""))
  max.date = as.Date(paste(format(max(dates), "%Y"), "-12-31", sep = ""))
  df = data.frame(date = seq(min.date, max.date, by = "days"), value = NA)

  # fill in values
  df$value[match(dates, df$date)] = values

  df$year  =  as.factor(format(df$date, "%Y"))
  df$month = as.numeric(format(df$date, "%m"))
  df$doy   = as.numeric(format(df$date, "%j"))
  df$dow = as.numeric(format(df$date, "%w"))
  df$woy = as.numeric(format(df$date, "%U")) + 1

  weekdys = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  weekyear = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

  df$dow = 7 - df$dow
  df$dowmapped = factor(df$dow, rev(weekdys))

  x = (df$value - min(df$value, na.rm = TRUE)) / (max(df$value, na.rm = TRUE) - min(df$value, na.rm = TRUE))
  scale_max = 24
  scale_min = 12
  x = x * (scale_max - scale_min) + scale_min
  df$scalesize = x

  rects = list()
  for (i in seq_len(nrow(df))) {
    rects[[i]] = list(type = "rect", fillcolor = "transparent", line = list(color = "darkgrey", width = 0.5),
      x0 = df$woy[i], x1 = df$woy[i] + 1, y0 = df$dow[i], y1 = df$dow[i] + 1, xref = "x", yref = "y",
      layer = "below")
  }

  month_lines = as.data.frame(do.call(rbind, lapply(1:12, function(i) monthSeparator(df, i))))
  rownames(month_lines) = NULL

  lines = list()
  for (i in seq_len(nrow(month_lines))) {
    lines[[i]] = list(type = "line", line = list(color = "white"), x0 = month_lines$x0[[i]],
      x1 = month_lines$x1[[i]], y0 = month_lines$y0[[i]], y1 = month_lines$y1[[i]], xref = "x", yref = "y",
      layer = "below")
  }


  ybrks = c(vapply(unique(df$month), function(m) min(df[df$month == m, "woy"]), numeric(1L)), max(df$woy) + 1)
  ybrks = vapply(seq_len(12L), function(i) mean(ybrks[c(i, i + 1)]), numeric(1L))

  #fig = plot_ly(df[!is.na(df$value), ], x = ~woy + 0.5, y = ~dow + 0.5,
      #name = "km", type = "scatter", mode = "markers", ,
      #color = ~value, width = 600, height = 150, markers = list(size = ~scalesize, opacity = 1)) %>%

  tickfont = list(
    family = "'Work Sans', 'Cardo', serif",
    size = 10,
    color = "white"
  )

  fig = plot_ly(type = "scatter", mode = "markers", width = 600, height = 150) %>%
    layout(
      title = title,
      shapes = c(rects, lines),
      yaxis = list(
        ticktext = paste0(rev(weekdys), "  "),
        tickvals = (1:7) + 0.5,
        tickmode = "array",
        tickfont = tickfont,
        scaleratio = 1,
        title = "",
        showgrid = FALSE,
        zeroline = FALSE
      ),
      xaxis = list(
        ticktext = weekyear,
        tickvals = ybrks,
        tickfont = tickfont,
        tickmode = "array",
        title = "",
        showgrid = FALSE
      ),
      plot_bgcolor  = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)",
      plot_bgcolor   = "rgba(0, 0, 0, 0)"
    ) %>%
    add_trace(data = df[!is.na(df$value), ],
      x = ~woy + 0.5, y = ~dow + 0.5,
      text = ~paste(date, " ", round(value, 2), "km"),
      marker = list(
        color = ~value,
        colorscale = "Greens",
        size = ~scalesize,
        opacity = 0.8,
        line = list(
          color = 'rgb(231, 99, 250)',
          width = 0
        )
      ),
      hovertemplate = "%{marker.color:.2f} km<extra></extra>",
      showlegend = FALSE
    )

  return(fig)
}
