print_ob <- function(units = c("KB", "MB")){
  require(dplyr)
  obs_list <- ls(envir = .GlobalEnv)
  res <- NULL
  for (i in 1:length(obs_list)) {
    print(length(obs_list))
    res <- c(res, get(obs_list[i]) %>% 
              object.size %>%
              as.numeric)
  }
  if (units == "KB"){
    data.frame(object = obs_list, size_KB = (res / 1024) %>% as.integer)
  } else if (units == "MB") {
    data.frame(object = obs_list, size_MB = (res / 1024^2) %>% as.integer)
  }
}
