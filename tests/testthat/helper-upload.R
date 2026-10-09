# A file where Shiny keeps an upload, for the tests of a session with no
# browser (#97). The app reads a file only from there, so a test that tells a
# session a reader chose a file puts the file where Shiny would have: in a
# folder of this R process's temporary folder named by 24 hexadecimal digits,
# under the name `0` with the extension of the reader's name for it
# (shiny:::FileUploadContext and shiny:::FileUploadOperation). Nothing here is
# exported.

# A folder named as Shiny names an upload's: 24 hexadecimal digits, made of
# this process's number, the clock and a count, so that no test's random
# numbers are drawn on.
nUploadFolders <- 0L
strUploadFolder <- function() {
  repeat {
    nUploadFolders <<- nUploadFolders + 1L
    strId <- sprintf("%08x%08x%08x", Sys.getpid(), as.integer(Sys.time()), nUploadFolders)
    strDir <- file.path(tempdir(), strId)
    if (!dir.exists(strDir) && dir.create(strDir)) {
      return(strDir)
    }
  }
}

# What a session is told when a reader chooses a file: one row, with the
# reader's name for the file and the place Shiny copied it to.
dfUploaded <- function(strFile, strName = basename(strFile)) {
  strType <- regmatches(strName, regexpr("\\.[^./]*$", strName))
  strKept <- file.path(strUploadFolder(), paste0("0", if (length(strType) == 1L) strType))
  if (!file.copy(strFile, strKept)) {
    stop("the file could not be put where Shiny keeps an upload: ", strFile, call. = FALSE)
  }
  data.frame(name = strName, size = file.size(strKept), type = "", datapath = strKept, stringsAsFactors = FALSE)
}
