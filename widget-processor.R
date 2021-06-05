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
