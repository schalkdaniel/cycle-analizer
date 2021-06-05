extractWidget = function(widget, lib, remove_widget = FALSE, remove_lib = FALSE) {
  if (! file.exists(widget)) stop("No ", widget, " available!")

  lines = readLines(widget)
  idx_body = grep("body>", lines)
  wlines = lines[seq(idx_body[1] + 1, idx_body[2] - 1)]

  if (remove_widget) file.remove(widget)
  if (dir.exists(lib)) {
    if (remove_lib) unlink(lib, TRUE)
  } else {
    stop("No directory ", lib, ".")
  }
  return(wlines)
}

includeLinesInDiv = function(html, lines_include, div, write_to_file = NULL) {
  lines = readLines(html)
  idx_div = grep(div, lines)

  lines_pre = lines[seq(1, idx_div[1])]
  lines_after = lines[seq(idx_div[2], length(lines))]
  lines_out = c(lines_pre, lines_include, lines_after)

  if (is.null(write_to_file)) {
    return(lines_out)
  } else {
    writeLines(lines_out, write_to_file)
  }
}

