includeLinesInDiv = function(html, lines_include, div, write_to_file = NULL) {
  lines = readLines(html)
  idx_div = grep(div, lines)
  if (length(idx_div) == 0) stop("Could not find ", div, " in ", html, ".")

  lines_pre = lines[seq(1, idx_div[1])]
  lines_after = lines[seq(idx_div[2], length(lines))]
  lines_out = c(lines_pre, lines_include, lines_after)

  if (is.null(write_to_file)) {
    return(lines_out)
  } else {
    writeLines(lines_out, write_to_file)
  }
}

updateIndex = function() {

}


createSubSites = function(fit_files, html_sub_dir, diary_files_dir, template_dir, smr) {
  submenu = siteMenu(fit_files, html_sub_dir, diary_files_dir, smr)
  for (ff in fit_files) {
    message("Process ", ff)
    createSubSite(ff, html_sub_dir, diary_files_dir, template_dir, smr, submenu)
  }
}

createSubSite = function(ff, html_sub_dir, diary_files_dir, template_dir, smr, menu, link_base = "") {
  ffile = strsplit(ff, split = "-ELEMNT")[[1]][1]
  sub_file = paste0(html_sub_dir, ffile, ".html")
  template = paste0(template_dir, "subsite.html")
  dfile = paste0(diary_files_dir, ffile, ".txt")

  file.copy(template, sub_file, overwrite = TRUE)
  includeLinesInDiv(sub_file, menu, "<SUBSIDES-MENU>", sub_file)

  meta_idx = grep(ffile, smr$source)
  km = smr$km[meta_idx]
  dte = smr$date[meta_idx]
  hrs = smr$hours_active[meta_idx]
  hrs = paste0(trunc(hrs), ":", trunc((hrs - trunc(hrs)) * 60), "h")

  ## TITLE:
  if (file.exists(dfile)) {
    tt = paste0("<p class='subtitles'>", dte, ": ", extractTextBetween(dfile, "TITLE"), "</p>")
  } else {
    tt = ffile
  }
  includeLinesInDiv(sub_file, tt, "<TITLE>", sub_file)
  ## TEXT:
  if (file.exists(dfile)) {
    tx = extractTextBetween(dfile, "TEXT")
  } else {
    tx = ""
  }
  includeLinesInDiv(sub_file, tx, "<TEXT>", sub_file)
}

siteMenu = function(fit_files, html_sub_dir, diary_files_dir, smr, link_base = "") {

  menu = lapply(fit_files, function(ff) {
    ffile = strsplit(ff, split = "-ELEMNT")[[1]][1]
    sub_file = paste0(html_sub_dir, ffile, ".html")
    link_file = paste0(link_base, ffile, ".html")

    ### Get title:
    dfile = paste0(diary_files_dir, ffile, ".txt")
    if (file.exists(dfile)) {
      tt = extractTextBetween(dfile, "TITLE")
    } else {
      tt = ffile
    }

    ### Get Meta:
    meta_idx = grep(ffile, smr$source)
    km = smr$km[meta_idx]
    dte = smr$date[meta_idx]
    hrs = smr$hours_active[meta_idx]
    hrs = paste0(trunc(hrs), ":", trunc((hrs - trunc(hrs)) * 60), "h")
    grid_date = paste0("  <p class='grid-date'><b>", dte, " ", hrs, "</b></p>")
    grid_meta = paste0("  <p class='grid-date'>", round(km, 2), "km - ", tt, "</p>")

    ## Return item:
    return(c(
      paste0("<div class='grid-item' id='", link_file, "'>"),
      grid_date,
      grid_meta,
      "</div>"
    ))
  })
  return(unlist(menu))
}
