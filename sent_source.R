sourceFiles<-list.files(pattern="text_.*.R$")

for(i in 2){
  start<-Sys.time()
  source(sourceFiles[i])
  stop<-Sys.time()
  timeDiff<- difftime(stop,start, units='secs')
  message<-paste(sourceFiles[i], "started at", start,
                 "and finished at", stop, "(Time:",
                 timeDiff, "seconds)")
  message
  write(message, 'Time_log.txt',append = T)
}

