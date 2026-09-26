# leanRide -- see leanRide.md for what this does and how to set it up.
# Add this file's contents to ~/.Rprofile (or source() it from there).

.rsend_file  <- path.expand("~/.rsend/pending.R")
.rsend_mtime <- 0

rsend_watch <- function(interval = 0.3) {
  dir.create(dirname(.rsend_file), showWarnings = FALSE, recursive = TRUE)
  if (file.exists(.rsend_file)) {
    .rsend_mtime <<- as.numeric(file.info(.rsend_file)$mtime)
  }
  check <- function() {
    if (file.exists(.rsend_file)) {
      mt <- as.numeric(file.info(.rsend_file)$mtime)
      if (!is.na(mt) && mt > .rsend_mtime) {
        .rsend_mtime <<- mt
        cat("\n--- sourcing from CotEditor ---\n")
        tryCatch(
          source(.rsend_file, echo = TRUE, max.deparse.length = 100000L),
          error = function(e) message("rsend error: ", conditionMessage(e))
        )
        cat("\n", getOption("prompt"), sep = "")
      }
    }
    later::later(check, interval)
  }
  later::later(check, interval)
  invisible(NULL)
}

.chromium_app_path <- function() {
  candidates <- c("/Applications/Chromium.app",
                   path.expand("~/Applications/Chromium.app"))
  found <- candidates[file.exists(candidates)]
  if (length(found)) found[1] else NA_character_
}

.chromium_binary <- function(app_path) {
  bin_dir <- file.path(app_path, "Contents", "MacOS")
  files <- list.files(bin_dir, full.names = TRUE)
  if (length(files) == 0) return(NA_character_)
  files[1]
}

# lsof, not socketConnection(server=TRUE) -- see leanRide.md
.port_available <- function(port, host = "127.0.0.1") {
  out <- suppressWarnings(tryCatch(
    system2("lsof", args = c("-nP", sprintf("-iTCP:%d", port), "-sTCP:LISTEN"),
            stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)
  ))
  length(out) == 0
}

.find_free_port <- function(start, tries = 20) {
  for (p in start + seq(0, tries - 1)) {
    if (.port_available(p)) return(p)
  }
  stop("leanRide: could not find a free port near ", start,
       " after ", tries, " tries.")
}

.screen_bounds <- function() {
  scr <- tempfile(fileext = ".applescript")
  writeLines('tell application "Finder" to get bounds of window of desktop', scr)
  out <- system2("osascript", args = shQuote(scr), stdout = TRUE)
  as.numeric(strsplit(gsub("[[:space:]]", "", out), ",")[[1]])
}

# calls the Chromium binary directly, not `open` -- see leanRide.md
.launch_chromium_app <- function(binary, url, x, y, w, h, profile_dir) {
  args <- c(sprintf("--user-data-dir=%s", profile_dir),
            "--no-first-run", "--no-default-browser-check",
            sprintf("--app=%s", url),
            sprintf("--window-size=%d,%d", w, h),
            sprintf("--window-position=%d,%d", x, y))
  # stdout/stderr = FALSE (discard), not NULL (inherit) -- see leanRide.md
  system2(binary, args = args, wait = FALSE, stdout = FALSE, stderr = FALSE)
}

.navigate_chromium_help <- function(url, hport) {
  prefix <- sprintf("http://127.0.0.1:%d/", hport)
  as_nav <- tempfile(fileext = ".applescript")
  writeLines(sprintf(
'tell application "Chromium"
  activate
  set winCount to count of windows
  repeat with i from 1 to winCount
    set w to window i
    set tabCount to count of tabs of w
    repeat with j from 1 to tabCount
      set t to tab j of w
      if (URL of t starts with "%s") then
        set URL of t to "%s"
        set index of w to 1
        return
      end if
    end repeat
  end repeat
end tell', prefix, url), as_nav)
  system2("osascript", args = shQuote(as_nav))
}

.navigate_safari_help <- function(url, hport) {
  prefix <- sprintf("http://127.0.0.1:%d/", hport)
  as_nav <- tempfile(fileext = ".applescript")
  writeLines(sprintf(
'tell application "Safari"
  activate
  set winCount to count of windows
  repeat with i from 1 to winCount
    set w to window i
    set tabCount to count of tabs of w
    repeat with j from 1 to tabCount
      set t to tab j of w
      if (URL of t starts with "%s") then
        set current tab of w to t
        set URL of t to "%s"
        set index of w to 1
        return
      end if
    end repeat
  end repeat
end tell', prefix, url), as_nav)
  system2("osascript", args = shQuote(as_nav))
}

.launch_safari_tabs <- function(help_url, plot_url) {
  b <- .screen_bounds()
  win_w <- floor(b[3] / 3)
  win_h <- floor(b[4] / 2)
  as_open <- tempfile(fileext = ".applescript")
  # "set newWin to make new document ..." captures a direct reference to the
  # window this call just created, so the resize/tab setup below (tell
  # newWin ...) is guaranteed to act on that specific window -- never on any
  # Safari window you already had open -- regardless of window ordering.
  # Referring to it positionally instead (tell window 1 ...) would rely on
  # the new window happening to be frontmost at that instant, which is true
  # almost always but not guaranteed (e.g. Safari still restoring windows
  # from its last session when "activate" launches it).
  writeLines(sprintf(
'tell application "Safari"
  activate
  set newWin to make new document with properties {URL:"%s"}
  tell newWin
    make new tab with properties {URL:"%s"}
    set current tab to tab 1
    set bounds to {0, 0, %d, %d}
  end tell
end tell', help_url, plot_url, win_w, win_h), as_open)
  system2("osascript", args = shQuote(as_open))
}

leanRide <- function(plot_port = 8892, watch = TRUE, force = FALSE, use_chromium = TRUE, epobj = TRUE) {
  if (isTRUE(getOption(".leanRide_ran")) && !force) {
    message("leanRide: already running this session (use leanRide(force = TRUE) to reopen).")
    return(invisible(NULL))
  }

  # snapshot pre-existing objects for vObjects() to exclude -- see leanRide.md.
  # envir = .GlobalEnv, not the default pos = -1L, since -1 inside a function
  # means that function's own frame, not the global environment (same
  # gotcha as vObjects()'s own pos argument below). Guarded by exists() so a
  # later leanRide(force = TRUE) call doesn't reset the snapshot to include
  # objects your analysis has created since the first call.
  if (isTRUE(epobj) && !exists("envExclude", envir = .GlobalEnv, inherits = FALSE)) {
    envExclude <<- objects(envir = .GlobalEnv, all.names = TRUE)
  }

  options(help_type = "html")
  hport <- tools::startDynamicHelp()
  if (hport <= 0) stop("Could not start R's dynamic HTML help server.")
  help_url <- sprintf("http://127.0.0.1:%d/doc/html/index.html", hport)

  if (!requireNamespace("httpgd", quietly = TRUE))
    stop("Please install.packages('httpgd') first.")

  start_hgd <- !isTRUE(getOption(".leanRide_hgd"))
  if (start_hgd && !.port_available(plot_port)) {
    free_port <- .find_free_port(plot_port + 1)
    message(sprintf(
      "leanRide: port %d is already in use (leftover httpgd server from an earlier session?); using %d instead.",
      plot_port, free_port))
    plot_port <- free_port
  }
  plot_url <- sprintf("http://127.0.0.1:%d/live", plot_port)

  # browser windows launched before httpgd::hgd() opens its socket -- see leanRide.md
  chromium <- if (isTRUE(use_chromium)) .chromium_app_path() else NA_character_
  if (!is.na(chromium)) {
    binary <- .chromium_binary(chromium)
    profile_dir <- tempfile("leanRide-chromium-")
    dir.create(profile_dir, recursive = TRUE)
    b <- .screen_bounds()
    sw <- b[3]; sh <- b[4]
    w <- round(sw * 0.4)
    h <- round(sh * 0.5)
    .launch_chromium_app(binary, help_url, x = 0,      y = 0, w = w, h = h, profile_dir = profile_dir)
    Sys.sleep(1.2)
    .launch_chromium_app(binary, plot_url, x = sw - w, y = 0, w = w, h = h, profile_dir = profile_dir)
    help_prefix <- sprintf("http://127.0.0.1:%d/", hport)
    options(browser = function(url) {
      if (startsWith(url, help_prefix)) .navigate_chromium_help(url, hport)
      else system2("open", args = shQuote(url))
    })
    message("leanRide: bare Chromium windows opened for Help (left) and Plots (right).")
  } else {
    .launch_safari_tabs(help_url, plot_url)
    help_prefix <- sprintf("http://127.0.0.1:%d/", hport)
    options(browser = function(url) {
      if (startsWith(url, help_prefix)) .navigate_safari_help(url, hport)
      else system2("open", args = shQuote(url))
    })
    if (isTRUE(use_chromium)) {
      message("leanRide: Chromium not found; opened Safari with Help/Plots tabs (resized to half-screen).")
    } else {
      message("leanRide: use_chromium = FALSE; opened Safari with Help/Plots tabs (resized to half-screen).")
    }
  }

  if (start_hgd) {
    httpgd::hgd(host = "127.0.0.1", port = plot_port, token = FALSE, silent = TRUE)
    options(.leanRide_hgd = TRUE)
  }

  if (isTRUE(watch) && !isTRUE(getOption(".leanRide_rsend"))) {
    if (!requireNamespace("later", quietly = TRUE))
      stop("Please install.packages('later') first.")
    rsend_watch()
    options(.leanRide_rsend = TRUE)
  }

  options(.leanRide_ran = TRUE)
  if (isTRUE(watch)) message("leanRide: CotEditor-send is watching.")
  invisible(list(help_url = help_url, plot_url = plot_url))
}

.vObjects_attr_str <- function(v) {
  if (is.function(v)) return("<function>")
  if (is.environment(v)) return("<environment>")
  s <- tryCatch(as.character(v), error = function(e) NA_character_)
  if (length(s) == 0 || all(is.na(s))) return("<unprintable>")
  if (length(s) > 5) s <- c(s[1:5], "...")
  paste(s, collapse = ",")
}

# see leanRide.md for what this shows, the pos argument, and envExclude
vObjects <- function(pos = -1L) {
  if (!requireNamespace("DT", quietly = TRUE))
    stop("Please install.packages('DT') first.")
  if (!requireNamespace("htmltools", quietly = TRUE))
    stop("Please install.packages('htmltools') first.")
  if (!requireNamespace("htmlwidgets", quietly = TRUE))
    stop("Please install.packages('htmlwidgets') first.")

  env_label <- if (is.character(pos)) pos else search()[abs(pos)]
  if (identical(env_label, ".GlobalEnv")) env_label <- "Global Environment"

  # -1 must mean vObjects()'s caller, not vObjects()'s own frame -- see leanRide.md
  if (identical(pos, -1L) || identical(pos, -1)) pos <- parent.frame()
  envir <- as.environment(pos)
  all_names <- objects(pos = pos, all.names = TRUE)

  # envExclude, if present (see leanRide.md -- leanRide()'s epobj argument
  # creates it by default), is a snapshot of objects() to hide: everything
  # present right when leanRide() ran, including ~/.Rprofile's own objects
  # and anything it source()d in
  if (exists("envExclude", envir = envir, inherits = FALSE)) {
    ex <- get("envExclude", envir = envir, inherits = FALSE)
    keep <- setdiff(all_names, c(ex, "envExclude"))
  } else {
    keep <- all_names
  }
  title <- sprintf("Objects (%s)", env_label)

  if (!length(keep)) {
    message("vObjects: no objects to show.")
    return(invisible(NULL))
  }

  structural <- c("names", "dim", "dimnames", "class", "row.names",
                   "label", "units", "srcref", "srcfile", "wholeSrcref")

  rows <- lapply(sort(keep), function(nm) {
    obj <- tryCatch(get(nm, envir = envir), error = function(e) NULL)
    if (is.null(obj) && !exists(nm, envir = envir, inherits = FALSE)) {
      return(data.frame(Name = nm, Class = "?", `Dim/Length` = "?",
                         Size = "?", Label = "", Units = "",
                         `Other attributes` = "(could not access)",
                         stringsAsFactors = FALSE, check.names = FALSE))
    }
    cls  <- paste(class(obj), collapse = ", ")
    dm   <- dim(obj)
    dims <- if (!is.null(dm)) paste(dm, collapse = " x ") else as.character(length(obj))
    sz   <- format(utils::object.size(obj), units = "auto")
    lab <- attr(obj, "label")
    uni <- attr(obj, "units")
    at <- attributes(obj)
    extra <- at[setdiff(names(at), structural)]
    extra_str <- if (length(extra)) {
      paste(sprintf("%s=%s", names(extra), vapply(extra, .vObjects_attr_str, character(1))),
            collapse = "; ")
    } else ""
    data.frame(
      Name = nm,
      Class = cls,
      `Dim/Length` = dims,
      Size = sz,
      Label = if (is.null(lab)) "" else paste(as.character(lab), collapse = " "),
      Units = if (is.null(uni)) "" else paste(as.character(uni), collapse = " "),
      `Other attributes` = extra_str,
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })

  tab <- do.call(rbind, rows)

  col_is_blank <- function(v) all(!nzchar(trimws(ifelse(is.na(v), "", as.character(v)))))
  for (col in c("Label", "Units", "Other attributes")) {
    if (col %in% names(tab) && col_is_blank(tab[[col]])) tab[[col]] <- NULL
  }

  widget <- DT::datatable(
    tab,
    caption = title,
    rownames = FALSE,
    options = list(pageLength = 100, autoWidth = TRUE)
  )
  widget <- htmlwidgets::prependContent(
    widget,
    htmltools::tags$style(htmltools::HTML(
      "table.dataTable td { white-space: normal !important; word-wrap: break-word; }"
    ))
  )

  out <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(widget, out, selfcontained = TRUE, title = title)
  utils::browseURL(out)
  invisible(tab)
}

# strips a column down to a plain type DT/jsonlite know how to serialize --
# see leanRide.md. Hmisc's label()/units() add classes like "labelled" (and
# the label/units themselves as attributes) on top of the real storage type;
# left in place, they can confuse DT's column-type detection and corrupt the
# whole table (a "DataTables warning ... unknown parameter" is the symptom).
.vdata_plain_col <- function(col) {
  dt_classes <- c("factor", "ordered", "Date", "POSIXct", "POSIXt", "difftime")
  cls <- class(col)
  if (any(cls %in% dt_classes)) {
    class(col) <- cls[cls %in% dt_classes]
  } else if (!is.null(attr(col, "class"))) {
    class(col) <- NULL
  }
  attr(col, "label") <- NULL
  attr(col, "units") <- NULL
  col
}

# see leanRide.md for what this shows
vData <- function(x) {
  if (!requireNamespace("DT", quietly = TRUE))
    stop("Please install.packages('DT') first.")
  if (!requireNamespace("htmltools", quietly = TRUE))
    stop("Please install.packages('htmltools') first.")
  if (!requireNamespace("htmlwidgets", quietly = TRUE))
    stop("Please install.packages('htmlwidgets') first.")

  nm <- deparse(substitute(x))
  d  <- dim(x)

  if (is.null(d)) {
    # plain vector -- not the primary use case, but handled for convenience
    tab <- if (is.null(names(x))) data.frame(Value = .vdata_plain_col(x))
           else data.frame(Name = names(x), Value = .vdata_plain_col(x), row.names = NULL)
  } else if (length(d) == 2) {
    tab0 <- as.data.frame(x)
    tab <- as.data.frame(lapply(tab0, .vdata_plain_col), stringsAsFactors = FALSE, check.names = FALSE)
    rownames(tab) <- rownames(tab0)
  } else {
    stop("vData: x must be a data frame, data table, matrix, or vector (not a >2-D array).")
  }

  title <- sprintf("Data (%s)", nm)

  # filter = "top" deliberately omitted -- see leanRide.md: its per-column
  # widgets (Selectize, ionRangeSlider) don't reliably bundle into a
  # selfcontained file opened via file://, and corrupt the whole table when
  # they fail to load. DT's built-in global search box still works fine.
  widget <- DT::datatable(
    tab,
    caption = title,
    rownames = TRUE,
    options = list(pageLength = 25, scrollX = TRUE, autoWidth = TRUE)
  )
  widget <- htmlwidgets::prependContent(
    widget,
    htmltools::tags$style(htmltools::HTML(
      "table.dataTable td { white-space: normal !important; word-wrap: break-word; }
       .dataTables_scrollBody table.dataTable { margin-left: 0 !important; }"
    ))
  )

  out <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(widget, out, selfcontained = TRUE, title = title)
  utils::browseURL(out)
  invisible(tab)
}

# see leanRide.md for what this shows and the noCache fix
vPackages <- function(update = FALSE) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  # lib.loc/lib pinned to .libPaths()[1] (your own writable library), and
  # noCache = TRUE -- see leanRide.md. Without pinning a single library,
  # a package installed in more than one library path (e.g. a "recommended"
  # package -- MASS, Matrix, survival, etc. -- that ships bundled with R
  # itself in a separate system library) shows up as a SEPARATE row per
  # library, and install.packages() (which only ever writes to
  # .libPaths()[1]) can never touch the other one, so that row falsely
  # reports as needing an update forever. noCache = TRUE separately guards
  # against installed.packages()'s own session cache still reporting a
  # package's pre-update version right after install.packages() updated it.
  lib <- .libPaths()[1]
  z <- old.packages(lib.loc = lib, instPkgs = installed.packages(lib.loc = lib, noCache = TRUE))
  # old.packages() returns a plain character matrix when several packages
  # are listed, but a "list matrix" (typeof "list") when exactly one is --
  # see leanRide.md. Rebuilding as a genuine character matrix makes z[,...]
  # and z[pkg, ...] behave consistently either way.
  if (!is.null(z) && is.list(z))
    z <- matrix(as.character(z), nrow = nrow(z), dimnames = dimnames(z))
  print(z[, c('Installed', 'ReposVer'), drop = FALSE], quote = FALSE)
  if (update && NROW(z) > 0) {
    install.packages(rownames(z), lib = lib)
    # A package can keep reappearing here even after "updating" it: CRAN's
    # source index bumps a package's version immediately on submission, but
    # the macOS binary build can lag behind by hours to days -- see
    # leanRide.md. install.packages() then (correctly) installs the newest
    # binary that actually exists, which is still the old version, so
    # nothing changes. Say so explicitly rather than leaving that silent.
    after <- installed.packages(lib.loc = lib, noCache = TRUE)
    unchanged <- vapply(rownames(z), function(p) {
      av <- if (p %in% rownames(after)) after[p, "Version"] else NA_character_
      identical(unname(z[p, "Installed"]), unname(av))
    }, logical(1))
    if (any(unchanged))
      message("vPackages: no newer macOS binary yet on CRAN for: ",
              paste(rownames(z)[unchanged], collapse = ", "),
              " -- still at the version you had; try again later.")
  }
  invisible(z)
}
