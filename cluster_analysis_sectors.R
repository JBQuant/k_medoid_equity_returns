rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}
needed <- c('cluster', 'colorspace', 'factoextra', 'tseries')  
installIfAbsentAndLoad(needed)
start_date <- Sys.Date()-365*5
end_date <- Sys.Date()-1

sp_lab <- read.csv("SPY_All_Holdings.csv", header = T)
str(sp_lab)
lab <- as.vector(sp_lab$ticker)

create_price_dir <- function(labels, start_date="2018-06-30", end_date="2019-06-30") {
  path <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(path)
  #days <- length(get.hist.quote('AAPL', start = start_date, end = end_date, quote = 'Adj'))
  len <- length(labels)
  try(dir.create("data"))
  k <- 1
  for (i in labels) {
    try({
      prices <- as.vector(get.hist.quote(i, start = start_date, end = end_date, quote = 'Adj'))
      #print(prices)
      dates <- time(get.hist.quote(i, start = start_date, end = end_date, quote = 'Adj'))
      bound <- cbind(as.character(dates), prices)
      file_name <- paste(path, "/data/", "cached", i, ".csv", sep = '')
      write.csv(bound, file = file_name, row.names = F)
      print(paste(k, "of", len, "done", sep = " "))
      k <- k+1
    }, silent = T)
  }
  return(paste("Done with", len-k, "errors"))
}

create_price_dir(labels = lab, start_date = start_date)

files_list <- c(list.files(paste(dirname(rstudioapi::getSourceEditorContext()$path),"/data/", sep = '')))
length(files_list)

# remove the securities that don't have the correct amount of trading days over the given period
files_list_complete <- c()
n <- 1244
k <- 1
for (i in files_list) {
  mydata <- read.csv(paste(dirname(rstudioapi::getSourceEditorContext()$path),"/data/", i, sep = ''))
  print(nrow(mydata))
  if (nrow(mydata) == n) {
    files_list_complete[k] <- i
    k <- k+1
  }
}           

df1 <- read.csv(paste(dirname(rstudioapi::getSourceEditorContext()$path),"/data/", "cachedAAPL.csv", sep = ''))
head(df1)

# function that takes a dataframe of two columns, Date and Price of
# length n and converts it to a vector of log n-1 price returns
calc_returns2 <- function(df, days_per_period=125) {
  date_lims <- range(as.Date(df$X))
  yrs <- as.numeric(range(as.Date(df$X))[2] - range(as.Date(df$X))[1])/365.25
  trde_days <- ceiling(252*yrs)
  segs <- ceiling(trde_days/days_per_period)
  date_cuts <- seq.Date(from = date_lims[1], to = date_lims[2], length.out = segs+1)
  ret_list <- c()
  for (i in 1:length(date_cuts)-1) {
    ret_list[i] <- mean(diff(log(df$prices[as.Date(df$X) > as.Date(date_cuts[i]) & as.Date(df$X) < as.Date(date_cuts[i+1])])))
  }
  return(ret_list)
}

# testing to make sure our new function works
test <- calc_returns2(df = df1, days_per_period = 252/2)
length(test)

# Getting a list of tickers from the files in the sub directory
setwd(paste(dirname(rstudioapi::getSourceEditorContext()$path),"/data/", sep = ''))
tickers <- c()
dates <- df1$X
df <- data.frame(Date=dates)
k <- 1
for (i in 1:length(files_list_complete)) {
  j <- read.csv(file = files_list_complete[i], header = T)
  if (length(j$prices == nrow(df))) {
    #df[as.character(substr(files_list_complete[1], start = 7, stop = nchar(files_list_complete[i])-4))] <- j$prices
    tickers[i] <- substr(files_list_complete[i], start = 7, stop = nchar(files_list_complete[i])-4)
    k <- k+1
    #print("done")
  }
}

final.df <- data.frame(row.names = tickers, 
                       "period_1"=rep(NA, length(tickers)), 
                       "period_2"=rep(NA, length(tickers)), 
                       "period_3"=rep(NA, length(tickers)), 
                       "period_4"=rep(NA, length(tickers)), 
                       "period_5"=rep(NA, length(tickers)), 
                       "period_6"=rep(NA, length(tickers)), 
                       "period_7"=rep(NA, length(tickers)), 
                       "period_8"=rep(NA, length(tickers)), 
                       "period_10"=rep(NA, length(tickers)))

for (i in 1:length(files_list_complete)) {
  elem <- read.csv(paste(dirname(rstudioapi::getSourceEditorContext()$path),"/data/", files_list_complete[i], sep = ''), header = T)
  final.df[i,] <- calc_returns2(elem, days_per_period = 125)
}

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

write.csv(x = final.df, file = "sp_period_returns.csv")         

k <- 12
final.df.clean <- na.omit(final.df)
my_clusters <- pam(x = final.df.clean, k)
my_clusters$clustering
hist(my_clusters$clustering, breaks = k, col = "royalblue4")
plot(final.df.clean,col=rainbow_hcl(max(my_clusters$clustering))[my_clusters$clustering],main="k-Medoids Clusters")

plot(final.df.clean[,3:4],col=rainbow_hcl(max(my_clusters$clustering))[my_clusters$clustering],main="k-Medoids Clusters")
points(my_clusters$medoids,pch=19,cex=1.5,col="navyblue")


plot(my_clusters)
my_clusters$clusinfo
round(aggregate(final.df.clean, by=list(cluster=my_clusters$cluster), mean),5)

dd <- cbind(final.df.clean, cluster = my_clusters$clustering)
dd[order(dd$cluster),]
head(dd)
dd[dd$cluster == 10,1:ncol(dd)-1]
dd <- cbind(ticker=tickers, dd$cluster)
dd_sectors <- merge(x = dd, y = sp_lab, by = "ticker")
dd_sectors <- dd_sectors[order(dd_sectors$V2),]
View(dd_sectors)

write.csv(x = dd_sectors, file = "spy_holdings_clusters.csv", row.names = F)

fviz_cluster(object = my_clusters[1:10], data = final.df.clean[1:10])
