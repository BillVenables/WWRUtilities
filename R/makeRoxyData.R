#' #' Make Roxygen Template for Data
#' #'
#' #' Generate Roxygen template for a data frame object
#' #'
#' #' @param dat A character string giving the name of the data frame
#' #' @param x An object of class "RoxyData"
#' #' @param ... Additional arguments (currently not used)
#' #'
#' #' @return A character string vector, of class "RoxyData", with the roxygen template
#' #' @export
#' #'
#' #' @examples
#' #' makeRoxyData("janka")
#' makeRoxyData <- function(dat) {
#'   ndat <- dat
#'   dat <- eval(substitute(DAT, list(DAT = as.name(ndat))))
#'   d <- dim(dat)
#'   n <- names(dat)
#'   x <- sapply(dat, function(x) class(x)[1])
#'   out <- c(ndat, "", "",
#'            paste("@format A data frame with", d[1], "rows and", d[2], "columns:"),
#'            "\\describe{",
#'            paste0("    \\item{", n, "}{", x, ": ... }"),
#'            "}")
#'   out <- c(paste("#'", out), paste0('"', ndat, '"'), "")
#'   class(out) <- "RoxyData"
#'    out
#' }
#'
#' #' @rdname makeRoxyData
#' #' @export
#' print.RoxyData <- function(x, ...) {
#'   to_clip(x, sep = "\n")
#'   cat(x, sep = "\n")
#'   invisible(x)
#' }
#'
#'
#' to_clip <- function (x, ..., file = con) {  ## copy_to_clipboard() from bannerCommenter
#'   if (missing(file)) {
#'     oldOpt <- options(warn = -1)
#'     on.exit(options(oldOpt))
#'     con <- switch(Sys.info()["sysname"],
#'                   Linux = {
#'                     xclip <- system("which xclip", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
#'                     if (xclip) {
#'                       pipe("xclip -selection clipboard -i", open = "w")
#'                     } else NULL
#'                   },
#'                   Windows = {
#'                     base::file("clipboard")
#'                   },
#'                   {
#'                     pbcopy <- system("which pbcopy", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
#'                     if (pbcopy) {
#'                       pipe("pbcopy", "w")
#'                     } else NULL
#'                   })
#'     if (!is.null(con))
#'       on.exit(close(con), add = TRUE)
#'   }
#'   if (interactive())
#'     cat(x, file = file, ...)
#'   invisible(x)
#' }
