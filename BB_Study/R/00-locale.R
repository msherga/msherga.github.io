# Ensure text functions such as gsub() can process UTF-8 values on shinyapps.io.
locale_result <- try(Sys.setlocale("LC_CTYPE", "C.UTF-8"), silent = TRUE)

if (inherits(locale_result, "try-error") || identical(locale_result, "")) {
  try(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"), silent = TRUE)
}

options(encoding = "UTF-8")
message("BB_Study LC_CTYPE: ", Sys.getlocale("LC_CTYPE"))
