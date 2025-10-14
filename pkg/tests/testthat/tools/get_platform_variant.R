get_platform_variant <- function() {
  os <- Sys.info()["sysname"]

  if (os == "Linux") {
    # Try to get distribution info
    if (file.exists("/etc/os-release")) {
      os_release <- readLines("/etc/os-release")
      id_line <- grep("^ID=", os_release, value = TRUE)
      if (length(id_line) > 0) {
        distro <- gsub("ID=|\"", "", id_line[1])
        return(paste0("linux-", distro))
      }
    }
    # Fallback to generic linux
    return("linux-unknown")
  }

  return(tolower(os))
}
