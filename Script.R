rm(list=ls(all=TRUE))
# Download .asc file
#Download of NOAA Merged Land Ocean Global Surface Temperature Analysis Dataset (NOAAGlobalTemp) Data in the root/working directory of RStudio: 
#NOAAGlobalTemp Dataset (8.7 MB)
#[ftp://ftp.ncdc.noaa.gov/pub/data/noaaglobaltemp/operational/gridded/NOAAGlobalTemp.gridded.v4.0.1.201810.asc]
setwd("/Users/Paul/Climate")
da1=scan("NOAAGlobalTemp.gridded.v4.0.1.201809.asc.gz")
length(da1)
#[1] 4319010

da1[1:30]
#[1] 1.0 1880.0 -999.9 #means mon, year, temp

#It is about a vector (0 dimensions), a sequence of numbers: mon, year, temp, temp....
#Here numerous temperatures follow according to a net over the whole globe in a distance of 5 x 5 degrees.
#At the 2595 and 2596th position appears the month February and the year 1880
#2595-2 = 2593 Temperaturvalues follow after each month and year values 
da1[2590:2600]
# [1] -999.9 -999.9 -999.9 -999.9 -999.9    2.0 1880.0 -999.9 -999.9 -999.9 -999.9
da1[5180:5200]
#[1] -999.9 -999.9 -999.9 -999.9 -999.9 -999.9 -999.9 -999.9 -999.9    3.0 1880.0 -999.9 -999.9 -999.9 -999.9 -999.9 -999.9
#[18] -999.9 -999.9 -999.9 -999.9

#Data in 72 rows (2.5, ..., 357.5), Latitude, and
#Data in 36 columns (-87.5, ..., 87.5), Longitude

#Years 2018-1880=138 years; 138 * 12 months = 1656 months
#From inclusive: Thursday, 1. January 1880
#To Saturday, 1. September 2018
#(Enddate not counted)
#
#Result: 50 647 Days
#50 647 Days between the metioned dates, enddate not included.
#Or 138 years, 9 months (exclusive Enddate), 138*12 +9 = 1665

#We build a sequence for the years and months
tm1=seq(1,4319009, by=2594) #Sequence of months
tm2=seq(2,4319010, by=2594) #Sequence of years
length(tm1)
#[1] 1665
length(tm2)
#[1] 1665
mm1=da1[tm1] #Extract months
yy1=da1[tm2] #Extract years
head(mm1)
head(yy1)
length(mm1)
length(yy1)
rw1<-paste(yy1, sep="-", mm1) #Combine YYYY with MM
head(rw1)
# "1880-1" "1880-2" "1880-3" "1880-4" "1880-5" "1880-6"
head(tm1)
head(tm2)
tm3=cbind(tm1,tm2)
head(tm3)
tm4=as.vector(t(tm3))
head(tm4)
#[1] 1 2 2595 2596 5189 5190
da2<-da1[-tm4] #Remove the months and years data from the scanned data
length(da2)/(36*72)
#[1] 1665 
#months
#36*72 = 2592, that is the number of gridded (5? ? 5?) global surface temperature datasets and these grids
# deliver measured values for 1665 months
2592*1665
#[1] 4315680 
#Temperature values from 1880 to 2018 in these 2592 grids
#138 yrs 9 mon: Jan 1880-Sep 2018
#1656 months + 9 = 1665, 138 yrs 9 mon: Jan 1880 - Sep 2018
da3<-matrix(da2,ncol=1665) #Generate the space-time data
#2592 (=36*72) rows and 1665 months (=138 yrs 9 mon)
dim(da3)
colnames(da3)<-rw1
lat1=seq(-87.5, 87.5, length=36)
lon1=seq(2.5, 357.5, length=72)
LAT=rep(lat1, each=72)
LON=rep(lon1,36)
gpcpst=cbind(LAT, LON, da3)
head(gpcpst)
dim(gpcpst)
#[1] 2592 1667 #The first two columns are Lat and Lon
#-87.5 to 87.5, Latitude, and then 2.5 to 375.5, Longitude
#The first row for time is header, not counted as data.
write.csv(gpcpst,file="NOAAGlobalT.csv")
#Output the data as a csv file

#Plot the temperature data map of a given month on a map from Baffin Bay to Egypt
#With this space-time data, one can plot a data map for a given month or a data time series
#for a given location. For example, the following R code plots the temperature data map for
#September 2018 on a map from Baffin Bay to Egypt.
#Install maps package if not done before
#install.packages("maps")
library(maps)
#Baffin Bay
#DG (Dezimalgrad)*
#Latitude: 74.69324929999999
#Longitude: -68.49280190000002
#Egypt
#Latitude 26.820553
#Longitude 30.802498000000014
Lat= seq(26.820553,74.69324929999999, length=36)
Lon=seq(-68.49280190000002, 30.802498000000014, length=72)
mapmat=matrix(gpcpst[,1665],nrow=72)
#column 1665 corresponding to Sep 2018
#Convert the vector into a lon-lat matrix for R map plotting
mapmat=pmax(pmin(mapmat,6),-6)
#This command compresses numbers larger than 6 to 6
plot.new()
par(mar=c(4,5,3,0))
int=seq(-6,6,length.out=81)
rgb.palette=colorRampPalette(c("black","blue", "darkgreen","green","yellow","pink","red","maroon"), interpolate="spline")
mapmat= mapmat[,seq(length(mapmat[1,]),1)]
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               plot.title=title(main="NOAAGlobalTemp Anomalies Sep 2018 [deg C]",
                                xlab="Longitude",ylab="Latitude", cex.lab=1.5),
               plot.axes={axis(1, cex.axis=1.5);
                       axis(2, cex.axis=1.5);map("world", add=TRUE);grid()},
               key.title=title(main="[oC]"),
               key.axes={axis(4, cex.axis=1.5)})

#Extract the data for the tropical Pacific region (20S-20N, 160E-120W) from 1951-2000
#The following code studies the data over a particular region, the tropical Pacific for El
#Nino characteristics, it extracts the data for the region for the given time interval.
#Keep only the data for the Pacific region
n2<-which(gpcpst[,1]>-20&gpcpst[,1]<20&gpcpst[,2]>160&gpcpst[,2]<260)
dim(gpcpst)
length(n2)
#[1] 160 #4 latitude bends and 20 longitude bends
pacificdat=gpcpst[n2,855:1454] #from 1951-2000
#(1951-1880)*12 + lat col + lon col =854
#Thus, Jan 1951 data from column 855
Lat=seq(-17.5,17.5, by=5)
Lon=seq(162.5, 257.5, by=5)
plot.new()
par(mar=c(4,5,3,0))
mapmat=matrix(pacificdat[,564], nrow=20)
int=seq(-5,5,length.out=81)
rgb.palette=colorRampPalette(c("black","blue", "darkgreen",
                               "green", "yellow","pink","red","maroon"),interpolate="spline")
#mapmat= mapmat[,seq(length(mapmat[1,]),1)]
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               xlim=c(120,300),ylim=c(-40,40),
               plot.title=title(main="Tropic Pacific SAT Anomalies [deg C]: Dec 1997",
                                xlab="Longitude",ylab="Latitude", cex.lab=1.5),
               plot.axes={axis(1, cex.axis=1.5); axis(2, cex.axis=1.5);
                       map("world2", add=TRUE);grid()},
               key.title=title(main="[oC]"),
               key.axes={axis(4, cex.axis=1.5)})

#Extract data from only one grid box
#A special case is to extract data for a specified grid box with given latitude and longitude for a given interval,
#e.g., the Iceland Region box (62.5, 337.5). This can be easily done by the
#following R code with a simple plotting command.
#Extract data for a specified box with given lat and lon
#Iceland Region
#Latitude 62.5
#Longitude 337.5
#For a survey of Latitude (LAT) and Longitude (LON)
#> gpcpst[1:100,1:10]

n2 <- which(gpcpst[,1]==62.5&gpcpst[,2]==337.5) #Latitude and Longitude
#Iceland <- gpcpst[n2,855:1454] #Interval 1880-2018
IcelandData <- gpcpst[n2,3:1667]
plot(seq(1880,2018, len=length(IcelandData)),
     IcelandData, type="l",
     xlab="Year", ylab="Temp [oC]",
     main="Iceland Region temperature anomalies history")

#Spatial averages and their trends
#36-by-72 boxes and Jan 1880 - Jan 2017 = 1645 months (Jan 1880-Sep 2018,1667 months (=138 yrs 9 mon))+ lat and lon
temp=gpcpst
areaw=matrix(0,nrow=2592,ncol = 1667)
dim(areaw)
#[1] 2592 1667
areaw[,1]=temp[,1]
areaw[,2]=temp[,2]
veca=cos(temp[,1]*pi/180)
#Here, convert degrees to radians
#Area-weight matrix equal to cosine lat of the boxes with data
#and to zero for the boxes of missing data -999.9
for(j in 3:1667)
{
        for (i in 1:2592)
        {if(temp[i,j]> -290.0) {areaw[i,j]=veca[i]} }
}
#Area-weight data matrixs first two columns as lat-lon
tempw=areaw*temp
tempw[,1:2]=temp[,1:2]
#create monthly global average vector for 1667 months
#Jan 1880- Sep 2018
avev=colSums(tempw[,3:1667])/colSums(areaw[,3:1667])
#Spatial average of the monthly temperature data from NOAAGlobalTemp
#from January 1880 -January 2018 and can be generated by the following R code
timemo=seq(1880,2018,length=1665)
plot(timemo,avev,type="l", cex.lab=1.4,
     xlab="Year", ylab="Temperature anomaly [oC]",
     main="Area-weighted global average of monthly SAT anomalies: Jan 1880-Sep 2018")
abline(lm(avev ~ timemo),col="blue",lwd=2)
text(1930,0.7, "Linear trend: 0.69 [oC] per century",
     cex=1.4, col="blue")

#Spatial average of annual data
plot.new()
avem = matrix(avev[1:1665], ncol=12, byrow=TRUE)
#compute annual average
annv=rowMeans(avem)
#Plot the annual mean global average temp
timeyr<-seq(1880, 2018)
plot(timeyr,annv,type="s",
     cex.lab=1.4, lwd=2,
     xlab="Year", ylab="Temperature anomaly [oC]",
     main="Area-weighted global average of annual SAT anomalies: 1880-2018")
abline(lm(annv ~ timeyr),col="blue",lwd=2)
text(1940,0.4, "Linear trend: 0.69 [oC] per century",
     cex=1.4, col="blue")
text(1900,0.07, "Base line",cex=1.4, col="red")
lines(timeyr,rep(0,139), type="l",col="red")
