## Summarise PPP log from OSX
## Tasks:
# Read connection log;
# Turn it into a dataframe
# Summarise mean / median / boxplot for received and sent;
# Graph over time

ppplog <- function (fromdate = "1990-01-01 00:00:00") {
  # Get the current ppplog. This may need work / better regex:
  system("cat /private/var/log/ppp.log | grep bytes. > ~/workspace/ppplog.txt")
  since <- strptime(fromdate, "%Y-%m-%d %H:%M:%S") # summarise since when?
  
  # Convert to a table
  ppplog <- read.table("~/workspace/ppplog.txt")
  
  # build dataframe
  p.date <- strptime(paste(ppplog$V3,ppplog$V2, ppplog$V5), "%d %b %Y")
  p.time <- strptime(paste(ppplog$V3,ppplog$V2, ppplog$V5, ppplog$V4), "%d %b %Y %H:%M:%s")
  p.sent <- as.numeric(ppplog$V8)
  p.rec <- as.numeric(ppplog$V11)
  plog <- data.frame(date = p.date, time = p.time, sent = p.sent, received = p.rec)
  plog <- plog[plog$time > since,]
  
  date.received <- aggregate(plog$received / 1024, by = list(Date=plog$date), sum)
  date.sent <- aggregate(plog$sent / 1024, by = list(Date=plog$date), sum)
  
  plog.daysum <- cbind(date.received, date.sent$x)
  names(plog.daysum) <- c("Date", "Received", "Sent")
  
  # since <- strptime("2012-10-24 15:08:00", "%Y-%m-%d %H:%M:%s") # summarise since when?
  
  # Plot daily summary of all data in log
  par(mfrow=c(1,2))
  plot(plog.daysum$Date, plog.daysum$Received / (1024^2), type = "h", lwd = 10, col = 'green')
  lines(plog.daysum$Date, plog.daysum$Sent / (1024^2), type = "h", lwd = 10, col = 'red')
  legend("topright", c("Received", "Sent"), col = c('green', 'red'), lwd = 10)
  boxplot(plog.daysum$Received / (1024 ^ 2), plog.daysum$Sent / (1024 ^ 2), col = c('green', 'red'))
  legend("topright", c(paste("Received: ", format(sum(plog.daysum$Received / (1024^2)), digits = 3), "GB"), paste("Sent: ", format(sum(plog.daysum$Sent / (1024^2)) , digits = 3), "GB"), "", paste("Average: ", format((mean(plog.daysum$Received)+mean(plog.daysum$Sent)) / (1024), digits = 3), "MB/d"), paste("Median: ", format((median(plog.daysum$Received)+median(plog.daysum$Sent)) / (1024), digits = 3), "MB/d")), cex = 0.7)
  par(mfrow=c(1,1))
}