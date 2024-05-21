constrict_time <- function(time.units) {
  incidents = c(rep(0,23))
  for (i in 1:24) {
    current_char = paste(as.character(i-1), ":", sep="")
    incident_index = which(startsWith(time.units$Incident.Time,current_char))
    incidents[i] = sum(time.units$n[incident_index])
  }
  hours = c(0:23)
  constricted_time = data.frame(hours,incidents)
  return(constricted_time)
}