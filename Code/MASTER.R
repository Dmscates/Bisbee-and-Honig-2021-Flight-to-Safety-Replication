# Master script
for(f in list.files('./')) {
  if(grepl('helper_functions|MASTER|LOG',f)) { next }
  if(file.exists(paste0('./LOG/log_',gsub('.R','.txt',f)))) { next }
  # stop()
  con <- file(paste0('./LOG/log_',gsub('.R','.txt',f)))
  cat(f,'\n')
  sink(con,append = TRUE)
  sink(con,append = TRUE,type = 'message')
  source(f,echo = T,max.deparse.length = 10000)
  sink()
  sink(type = 'message')
}
