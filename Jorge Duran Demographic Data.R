#*** Jorge Eduardo Duran Vasquez***
#*** Introductory Case Studies SS 2021 ***
#*** Project 1: Demographic Data
# Import the data from the csv
Data <- read.csv("census_2020_2000.csv");

#1 Frequency Distribution of the variables
#***Total Fertility Rate****#

# Layout to split the screen, here we are creating one column and 2 rows in the visualization.
layout(mat = matrix(c(1,2),2,1, byrow=TRUE), height = c(8,3))
# Now we set up the margin of the first graph that will be in the in the Layout function
par(mar=c(0, 5, 1.1, 1))
#Draw the histogram of the variable
h1<- hist(Data$Total.Fertility.Rate[Data$Year==2020], breaks = 10, plot = FALSE)
h1$counts=(h1$counts)/sum(h1$counts) #Relative frequencies
plot(h1, main= "", xaxt="n", xlim=c(1,7),
     ylim=c(0,0.40),
     col="dodgerblue3", ylab="Relative Frecuency",
     las= 2, #The style of axis labels is always perpendicular to the axis
     panel.first=grid(NA, 6, col = "gray", lty = "dotted",lwd = par("lwd")))
# Now we set up the margin of the second graph that will be in the Layout function
par(mar=c(5, 5, 0, 1))
boxplot(Data$Total.Fertility.Rate[Data$Year==2020], horizontal = TRUE, frame=FALSE, ylim=c(1,7), col= "goldenrod1", xlab="Average number of children",xaxt='n')
axis(1,at = seq(1,7,1))


min(Data$Total.Fertility.Rate[Data$Year==2020])
quantile(Data$Total.Fertility.Rate[Data$Year==2020])
median(Data$Total.Fertility.Rate[Data$Year==2020])
mean(Data$Total.Fertility.Rate[Data$Year==2020])
max(Data$Total.Fertility.Rate[Data$Year==2020])
sd(Data$Total.Fertility.Rate[Data$Year==2020])

#***Life Expectancy of Both Sexes****#

# Layout to split the screen, here we are creating one column and 2 rows in the visualization.
layout(mat = matrix(c(1,2),2,1, byrow=TRUE), height = c(8,3))
# Now we set up the margin of the first graph that will be in the in the Layout function
par(mar=c(0, 5, 1.1, 1))
#Draw the histogram of the variable
h2 <- hist(Data$Life.Expectancy.at.Birth..Both.Sexes[Data$Year==2020], breaks = 15, plot = FALSE)
h2$counts=(h2$counts)/sum(h2$counts) #Relative frequencies
plot(h2, main= "", xaxt="n",
     ylim=c(0,0.2),
     xlim=c(50,90),
     col="dodgerblue3", ylab="Relative Frecuency",
     las= 2, #The style of axis labels is always perpendicular to the axis
     panel.first=grid(NA, 6, col = "gray", lty = "dotted",lwd = par("lwd")))
# Now we set up the margin of the second graph that will be in the Layout function
par(mar=c(5, 5, 0, 1))
boxplot(Data$Life.Expectancy.at.Birth..Both.Sexes[Data$Year==2020], horizontal = TRUE, frame=FALSE, ylim=c(50,90), col= "goldenrod1", xlab="Average number of years",xaxt='n')
axis(1,at = seq(50,90,5))

min(Data$Life.Expectancy.at.Birth..Both.Sexes[Data$Year==2020])
quantile(Data$Life.Expectancy.at.Birth..Both.Sexes[Data$Year==2020])
median(Data$Life.Expectancy.at.Birth..Both.Sexes[Data$Year==2020])
mean(Data$Life.Expectancy.at.Birth..Both.Sexes[Data$Year==2020])
max(Data$Life.Expectancy.at.Birth..Both.Sexes[Data$Year==2020])
sd(Data$Life.Expectancy.at.Birth..Both.Sexes[Data$Year==2020])

#***Life Expectancy at Birth for Males****#

# Layout to split the screen, here we are creating one column and 2 rows in the visualization.
layout(mat = matrix(c(1,2),2,1, byrow=TRUE), height = c(8,3))
# Now we set up the margin of the first graph that will be in the in the Layout function
par(mar=c(0, 5, 1.1, 1))
#Draw the histogram of the variable
h3 <- hist(Data$Life.Expectancy.at.Birth..Males[Data$Year==2020], breaks = 15, plot = FALSE)
h3$counts=(h3$counts)/sum(h3$counts) #Relative frequencies
plot(h3, main= "", xaxt="n",
     ylim=c(0,0.15),
     xlim=c(50,90),
     col="dodgerblue3", ylab="Relative Frecuency",
     las= 2, #The style of axis labels is always perpendicular to the axis
     panel.first=grid(NA, 6, col = "gray", lty = "dotted",lwd = par("lwd")))
# Now we set up the margin of the second graph that will be in the Layout function
par(mar=c(5, 5, 0, 1))
boxplot(Data$Life.Expectancy.at.Birth..Males[Data$Year==2020], horizontal = TRUE, frame=FALSE, ylim=c(50,90), col= "goldenrod1", xlab="Average number of years Males",xaxt='n')
axis(1,at = seq(50,90,5))

min(Data$Life.Expectancy.at.Birth..Males[Data$Year==2020])
quantile(Data$Life.Expectancy.at.Birth..Males[Data$Year==2020])
median(Data$Life.Expectancy.at.Birth..Males[Data$Year==2020])
mean(Data$Life.Expectancy.at.Birth..Males[Data$Year==2020])
max(Data$Life.Expectancy.at.Birth..Males[Data$Year==2020])


#***Expectancy at Birth for Females****#

# Layout to split the screen, here we are creating one column and 2 rows in the visualization.
layout(mat = matrix(c(1,2),2,1, byrow=TRUE), height = c(8,3))
# Now we set up the margin of the first graph that will be in the Layout function
par(mar=c(0, 5, 1.1, 1))
h4 <- hist(Data$Life.Expectancy.at.Birth..Females[Data$Year==2020], breaks = 15, plot = FALSE)
h4$counts=(h4$counts)/sum(h4$counts) #Relative frequencies
plot(h4, main= "", xaxt="n",
     ylim=c(0,0.15),
     xlim=c(50,95),
     col="dodgerblue3", ylab="Relative Frecuency",
     las= 2, #The style of axis labels is always perpendicular to the axis
     panel.first=grid(NA, 6, col = "gray", lty = "dotted",lwd = par("lwd")))

# Now we set up the margin of the second graph that will be in the Layout function
par(mar=c(5, 5, 0, 1))
boxplot(Data$Life.Expectancy.at.Birth..Females[Data$Year==2020], horizontal = TRUE, frame=FALSE, ylim=c(50,95), col= "goldenrod1", xlab="Average number of years (Females)",xaxt='n')
axis(1,at = seq(50,95,5))

min(Data$Life.Expectancy.at.Birth..Females[Data$Year==2020])
quantile(Data$Life.Expectancy.at.Birth..Females[Data$Year==2020])
median(Data$Life.Expectancy.at.Birth..Females[Data$Year==2020])
mean(Data$Life.Expectancy.at.Birth..Females[Data$Year==2020])
max(Data$Life.Expectancy.at.Birth..Females[Data$Year==2020])

#****** Comparison between both sexes****#

# Layout to split the screen, here we are creating one column and 2 rows in the visualization.
layout(mat = matrix(c(1,2),2,1, byrow=TRUE), height = c(8,3))
# Now we set up the margin of the first graph that will be in the Layout function
par(mar=c(0, 5, 1.1, 1))
differences <- ((Data$Life.Expectancy.at.Birth..Females[Data$Year==2020])-Data$Life.Expectancy.at.Birth..Males[Data$Year==2020])
h5 <- hist(differences, breaks = 15, plot = FALSE)
h5$counts=(h5$counts)/sum(h5$counts) #Relative frequencies
plot(h5, main= "", xaxt="n",
     ylim=c(0,0.30),
     xlim=c(-3,13),
     col="dodgerblue3", ylab="Relative Frecuency",
     las= 2, #The style of axis labels is always perpendicular to the axis
     panel.first=grid(NA, 6, col = "gray", lty = "dotted",lwd = par("lwd")))
# Now we set up the margin of the second graph that will be in the Layout function
par(mar=c(5, 5, 0, 1))
boxplot(differences, horizontal = TRUE, frame=FALSE, ylim=c(-3,13), col= "goldenrod1", xlab="Difference Life Expectancy", xaxt='n')
axis(1,at = seq(-3,13,2))

median(differences)
mean(differences)



# 2. Correlation
# Correlation between Total Fertility Rate 2020 and Life Expectancy at Birth Both Sexes 2020

mcor<-round(cor(data.frame(Data$Total.Fertility.Rate[Data$Year==2020],Data$Life.Expectancy.at.Birth..Both.Sexes[Data$Year==2020],Data$Life.Expectancy.at.Birth..Males[Data$Year==2020],Data$Life.Expectancy.at.Birth..Females[Data$Year==2020])),2)
lower<-mcor
lower[lower.tri(mcor, diag=FALSE)]<-""
lower<-as.data.frame(lower)
print(lower)

par(mgp=c(2.5,0.8,0), mar=c(4, 4, 1, 1)) #bottom, left, top and right
plot(Data$Life.Expectancy.at.Birth..Both.Sexes[Data$Year==2020],Data$Total.Fertility.Rate[Data$Year==2020], 
     xlab= "Life Expectancy at Birth Both Sexes 2020",
     ylab = "Total Fertility Rate 2020",
     xlim=c(50,90),
     ylim=c(0,9),
     col= "grey2",
     pch = 19)


#* 3. Comparison Between regions and Subregions
#***** Box plot of the whole data **** 

# we sort the for the visualization
Data$Subregion <- factor(Data$Subregion,levels=c("Middle Africa","Western Africa","Eastern Africa","Northern Africa","Southern Africa",
                                                 "Melanesia",
                                                 "Micronesia",
                                                 "Polynesia",
                                                 "Australia/New Zealand",
                                                 "Western Asia",
                                                 "South-Central Asia",
                                                 "South-Eastern Asia",
                                                 "Eastern Asia",
                                                 "Central America",
                                                 "South America",
                                                 "Caribbean",
                                                 "Northern America",
                                                 "Northern Europe",
                                                 "Western Europe",
                                                 "Southern Europe",
                                                 "Eastern Europe"))

## Assing the color to the subregions
mycolors<-ifelse(levels(Data$Subregion)=="Northern Africa","olivedrab",
                 ifelse(levels(Data$Subregion)=="Middle Africa","olivedrab1" ,
                        ifelse(levels(Data$Subregion)=="Western Africa","olivedrab3",
                               ifelse(levels(Data$Subregion)=="Southern Africa","palegreen4",
                                      ifelse(levels(Data$Subregion)=="Eastern Africa","springgreen4",
                                             ifelse(levels(Data$Subregion)=="Caribbean","orchid1",
                                                    ifelse(levels(Data$Subregion)=="South America","mediumpurple1",
                                                           ifelse(levels(Data$Subregion)=="Central America","mediumpurple2",
                                                                  ifelse(levels(Data$Subregion)=="Northern America","mediumpurple4",
                                                                         ifelse(levels(Data$Subregion)=="South-Central Asia","indianred1",
                                                                                ifelse(levels(Data$Subregion)=="Western Asia","red1",
                                                                                       ifelse(levels(Data$Subregion)=="South-Eastern Asia","red3",
                                                                                              ifelse(levels(Data$Subregion)=="Eastern Asia","red4",
                                                                                                     ifelse(levels(Data$Subregion)=="Southern Europe","lightskyblue" ,
                                                                                                            ifelse(levels(Data$Subregion)=="Western Europe","royalblue1",
                                                                                                                   ifelse(levels(Data$Subregion)=="Eastern Europe","royalblue3",
                                                                                                                          ifelse(levels(Data$Subregion)=="Northern Europe","navyblue",
                                                                                                                                 ifelse(levels(Data$Subregion)=="Polynesia","palegoldenrod",
                                                                                                                                        ifelse(levels(Data$Subregion)=="Australia/New Zealand","Orange1",
                                                                                                                                               ifelse(levels(Data$Subregion)=="Melanesia","Orange3",
                                                                                                                                                      ifelse(levels(Data$Subregion)=="Micronesia","Orange4", "gray")))))))))))))))))))))

# Now we plot the graph multi-boxplot for Fertility Rate
layout(mat = matrix(c(1,2),2,1, byrow=TRUE), height = c(15,7))                                                                                             
par(mar=c(1, 5, 1, 1),mgp=c(3,0.8,0),
    xpd=FALSE)
boxplot(Data$Total.Fertility.Rate[Data$Year==2020]~Data$Subregion[Data$Year==2020], col= mycolors, xaxt="n", xlab="" , ylab=("Fertility Rate [Average Number of childen]"))
grid(NA, 6, col = "gray", lty = "dotted",lwd = par("lwd"))
axis(1, at = c(2.5,7.5,11.5,15.5,19.5),  c("Africa", "Ocenia", "Asia", "Americas", "Europe") , tick=FALSE , cex=0.2)
abline(v=5.5,lty=1, col="grey")
abline(v=9.5,lty=1, col="grey")
abline(v=13.5,lty=1, col="grey")
abline(v=17.5,lty=1, col="grey")
par(mar=c(3, 2, 3, 1))
plot.new()
par(xpd=TRUE)
legend("center", legend = c("Middle Africa","Western Africa","Eastern Africa","Northern Africa","Southern Africa","Melanesia ","Micronesia ","Polynesia ","Australia/New Zealand","Western Asia","South-Central Asia","South-Eastern Asia","Eastern Asia","Central America","South America","Caribbean","Northern America","Northern Europe","Western Europe","Southern Europe","Eastern Europe"),  col= mycolors, pch = 15, pt.cex = 1.5, cex = 0.8, inset = c(-0.12, 0) , ncol = 4)
par(xpd=FALSE)

# Now we plot the graph multi-boxplot Life Expectancy
layout(mat = matrix(c(1,2),2,1, byrow=TRUE), height = c(15,7))                                                                                             
par(mar=c(1, 5, 1, 1),mgp=c(3,0.8,0),
    xpd=FALSE)
boxplot(Data$Life.Expectancy.at.Birth..Both.Sexes[Data$Year==2020]~Data$Subregion[Data$Year==2020], col= mycolors, xaxt="n", xlab="" , ylab=("Life Expectancy [Average Number of Years]"))
grid(NA, 6, col = "gray", lty = "dotted",lwd = par("lwd"))
axis(1, at = c(2.5,7.5,11.5,15.5,19.5),  c("Africa", "Ocenia", "Asia", "Americas", "Europe") , tick=FALSE , cex=0.2)
abline(v=5.5,lty=1, col="grey")
abline(v=9.5,lty=1, col="grey")
abline(v=13.5,lty=1, col="grey")
abline(v=17.5,lty=1, col="grey")
par(mar=c(3, 2, 3, 1))
plot.new()
par(xpd=TRUE)
legend("center", legend = c("Middle Africa","Western Africa","Eastern Africa","Northern Africa","Southern Africa","Melanesia ","Micronesia ","Polynesia ","Australia/New Zealand","Western Asia","South-Central Asia","South-Eastern Asia","Eastern Asia","Central America","South America","Caribbean","Northern America","Northern Europe","Western Europe","Southern Europe","Eastern Europe"),  col= mycolors, pch = 15, pt.cex = 1.5, cex = 0.8, inset = c(-0.12, 0) , ncol = 4)
par(xpd=FALSE)

# Now we plot the graph multi-boxplot Life Expectancy Females
layout(mat = matrix(c(1,2),2,1, byrow=TRUE), height = c(15,7))                                                                                             
par(mar=c(1, 5, 1, 1),mgp=c(3,0.8,0),
    xpd=FALSE)
boxplot(Data$Life.Expectancy.at.Birth..Females[Data$Year==2020]~Data$Subregion[Data$Year==2020], col= mycolors, xaxt="n", xlab="" , ylab=("Life Expectancy [Average Number of Years]"))
grid(NA, 6, col = "gray", lty = "dotted",lwd = par("lwd"))
axis(1, at = c(2.5,7.5,11.5,15.5,19.5),  c("Africa", "Ocenia", "Asia", "Americas", "Europe") , tick=FALSE , cex=0.2)
abline(v=5.5,lty=1, col="grey")
abline(v=9.5,lty=1, col="grey")
abline(v=13.5,lty=1, col="grey")
abline(v=17.5,lty=1, col="grey")
par(mar=c(3, 3, 3, 1))
plot.new()
par(xpd=TRUE)
legend("center", legend = c("Middle Africa","Western Africa","Eastern Africa","Northern Africa","Southern Africa","Melanesia ","Micronesia ","Polynesia ","Australia/New Zealand","Western Asia","South-Central Asia","South-Eastern Asia","Eastern Asia","Central America","South America","Caribbean","Northern America","Northern Europe","Western Europe","Southern Europe","Eastern Europe"),  col= mycolors, pch = 15, pt.cex = 1.5, cex = 0.8, inset = c(-0.12, 0) , ncol = 4)
par(xpd=FALSE)

# Now we plot the graph multi-boxplot Life Expectancy Males
layout(mat = matrix(c(1,2),2,1, byrow=TRUE), height = c(15,7))                                                                                             
par(mar=c(1, 5, 1, 1),mgp=c(3,0.8,0),
    xpd=FALSE)
boxplot(Data$Life.Expectancy.at.Birth..Males[Data$Year==2020]~Data$Subregion[Data$Year==2020], col= mycolors, xaxt="n", xlab="" , ylab=("Life Expectancy [Average Number of Years]"))
grid(NA, 6, col = "gray", lty = "dotted",lwd = par("lwd"))
axis(1, at = c(2.5,7.5,11.5,15.5,19.5),  c("Africa", "Ocenia", "Asia", "Americas", "Europe") , tick=FALSE , cex=0.2)
abline(v=5.5,lty=1, col="grey")
abline(v=9.5,lty=1, col="grey")
abline(v=13.5,lty=1, col="grey")
abline(v=17.5,lty=1, col="grey")
par(mar=c(3, 3, 3, 1))
plot.new()
par(xpd=TRUE)
legend("center", legend = c("Middle Africa","Western Africa","Eastern Africa","Northern Africa","Southern Africa","Melanesia ","Micronesia ","Polynesia ","Australia/New Zealand","Western Asia","South-Central Asia","South-Eastern Asia","Eastern Asia","Central America","South America","Caribbean","Northern America","Northern Europe","Western Europe","Southern Europe","Eastern Europe"),  col= mycolors, pch = 15, pt.cex = 1.5, cex = 0.8, inset = c(-0.12, 0) , ncol = 4)
par(xpd=FALSE)



# 4. * Change of Total Fertility Rate 2000 - 2020 *·


naval <- data.frame() 
i=1;
for(j in 1:nrow(Data)){
  if(is.na(Data[j,7])) {
    naval[i,1] <- Data[j,1]
    i=i+1 }
}
naval
wmv <- Data[!Data$Country.Area.Name %in% naval$V1,]

graphdatah <- data.frame()
i=1
for(j in 1:221){
  graphdatah[j,1]= unique(wmv[,1])[j]
  i=(j*2)
  graphdatah[j,2]= wmv$Subregion[i]
  graphdatah[j,3]= wmv$Total.Fertility.Rate[wmv$Year==2020][j]
  graphdatah[j,4]= wmv$Total.Fertility.Rate[wmv$Year==2000][j]
  graphdatah[j,5]= wmv$Life.Expectancy.at.Birth..Both.Sexes[wmv$Year==2020][j]
  graphdatah[j,6]= wmv$Life.Expectancy.at.Birth..Both.Sexes[wmv$Year==2000][j]
  graphdatah[j,7]= wmv$Life.Expectancy.at.Birth..Females[wmv$Year==2020][j]
  graphdatah[j,8]= wmv$Life.Expectancy.at.Birth..Females[wmv$Year==2000][j]
  graphdatah[j,9]= wmv$Life.Expectancy.at.Birth..Males[wmv$Year==2020][j]
  graphdatah[j,10]= wmv$Life.Expectancy.at.Birth..Males[wmv$Year==2000][j]
  graphdatah[j,11]= wmv$FIPS[i]
  
}

#*** Variation by subregion ***
variation<-data.frame()
variation<-data.frame(Subregions=unique(graphdatah$V2))
for (j in 1:nrow(variation)){
  variation[j,2] <- sd(graphdatah$V5[graphdatah$V2==variation$Subregions[j]])
  variation[j,3] <- IQR(graphdatah$V5[graphdatah$V2==variation$Subregions[j]])
  variation[j,4] <- sd(graphdatah$V3[graphdatah$V2==variation$Subregions[j]])
  variation[j,5] <- IQR(graphdatah$V3[graphdatah$V2==variation$Subregions[j]])
}

print(variation)

#*** Change of Total Fertility Rate 2000 - 2020 Country, subregion
layout(mat = matrix(c(1,2),2,1, byrow=TRUE), height = c(15,7))                                                                                             
par(mgp=c(2.5,0.8,0), mar=c(3.4, 4, 1, 1)) #bottom, left, top and right
plot(graphdatah$V4,
     graphdatah$V3,
     #main = "Change of Total Fertility Rate 2000 - 2020",
     xlab="Total Fertility Rate 2000",
     ylab = "Total Fertility Rate 2020",
     xlim=c(0,8.2),
     ylim=c(0,8.2),
     col= ifelse(graphdatah$V2=="Northern Africa","olivedrab",
                 ifelse(graphdatah$V2=="Middle Africa","olivedrab1" ,
                        ifelse(graphdatah$V2=="Western Africa","olivedrab3",
                               ifelse(graphdatah$V2=="Southern Africa","palegreen4",
                                      ifelse(graphdatah$V2=="Eastern Africa","springgreen4",
                                             ifelse(graphdatah$V2=="Caribbean","orchid1",
                                                    ifelse(graphdatah$V2=="South America","mediumpurple1",
                                                           ifelse(graphdatah$V2=="Central America","mediumpurple2",
                                                                  ifelse(graphdatah$V2=="Northern America","mediumpurple4",
                                                                         ifelse(graphdatah$V2=="South-Central Asia","indianred1",
                                                                                ifelse(graphdatah$V2=="Western Asia","red1",
                                                                                       ifelse(graphdatah$V2=="South-Eastern Asia","red3",
                                                                                              ifelse(graphdatah$V2=="Eastern Asia","red4",
                                                                                                     ifelse(graphdatah$V2=="Southern Europe","lightskyblue" ,
                                                                                                            ifelse(graphdatah$V2=="Western Europe","royalblue1",
                                                                                                                   ifelse(graphdatah$V2=="Eastern Europe","royalblue3",
                                                                                                                          ifelse(graphdatah$V2=="Northern Europe","navyblue",
                                                                                                                                 ifelse(graphdatah$V2=="Polynesia","palegoldenrod",
                                                                                                                                        ifelse(graphdatah$V2=="Australia/New Zealand","Orange1",
                                                                                                                                               ifelse(graphdatah$V2=="Melanesia","Orange3",
                                                                                                                                                      ifelse(graphdatah$V2=="Micronesia","Orange4", "gray"))))))))))))))))))))),
     pch = 19)
legend("bottomright", legend=c("0% change","20% change", "40% change"),
       col=c("gray47", "gray47"), lty=c("solid","dashed","dotted"), cex=0.8)
abline(coef=c(0,1))
abline(coef=c(0,1.2),lty="dashed",col="gray")
abline(coef=c(0,0.8),lty="dashed",col="gray")
abline(coef=c(0,0.6),lty="dotted",col="gray")
text(8.09,7, 'Niger', cex=0.7, pos=3)
text(8,4.82, 'Afghanistan', cex=0.7, pos=1)
text(1.68,1.0603, 'Taiwan', cex=0.7, pos=1)
text(1.2105,0.8924, 'Hong Kong', cex=0.7, pos=2)
par(mar=c(3, 3, 3, 2),mgp=c(2.5,0.8,0))
plot.new()
par(xpd=TRUE)
legend("center", legend = c("Middle Africa","Western Africa","Eastern Africa","Northern Africa","Southern Africa","Melanesia ","Micronesia ","Polynesia ","Australia/New Zealand","Western Asia","South-Central Asia","South-Eastern Asia","Eastern Asia","Central America","South America","Caribbean","Northern America","Northern Europe","Western Europe","Southern Europe","Eastern Europe"),  col= mycolors, pch = 15, pt.cex = 1.5, cex = 0.8, inset = c(-0.12, 0) , ncol = 4)
par(xpd=FALSE)



#*** Change of Life Expectancy rate 2000 - 2020 Country, subregion
layout(mat = matrix(c(1,2),2,1, byrow=TRUE), height = c(15,7))                                                                                             
par(mgp=c(2.5,0.8,0), mar=c(3.4, 4, 1, 1)) #bottom, left, top and right
plot(graphdatah$V6,
     graphdatah$V5,
     #main = "Change of Total Fertility Rate 2000 - 2020",
     xlab="Life Expectancy 2000 [Years]",
     ylab = "Life Expectancy  2020 [Years]",
     xlim=c(34,92),
     ylim=c(34,92),
     col= ifelse(graphdatah$V2=="Northern Africa","olivedrab",
                 ifelse(graphdatah$V2=="Middle Africa","olivedrab1" ,
                        ifelse(graphdatah$V2=="Western Africa","olivedrab3",
                               ifelse(graphdatah$V2=="Southern Africa","palegreen4",
                                      ifelse(graphdatah$V2=="Eastern Africa","springgreen4",
                                             ifelse(graphdatah$V2=="Caribbean","orchid1",
                                                    ifelse(graphdatah$V2=="South America","mediumpurple1",
                                                           ifelse(graphdatah$V2=="Central America","mediumpurple2",
                                                                  ifelse(graphdatah$V2=="Northern America","mediumpurple4",
                                                                         ifelse(graphdatah$V2=="South-Central Asia","indianred1",
                                                                                ifelse(graphdatah$V2=="Western Asia","red1",
                                                                                       ifelse(graphdatah$V2=="South-Eastern Asia","red3",
                                                                                              ifelse(graphdatah$V2=="Eastern Asia","red4",
                                                                                                     ifelse(graphdatah$V2=="Southern Europe","lightskyblue" ,
                                                                                                            ifelse(graphdatah$V2=="Western Europe","royalblue1",
                                                                                                                   ifelse(graphdatah$V2=="Eastern Europe","royalblue3",
                                                                                                                          ifelse(graphdatah$V2=="Northern Europe","navyblue",
                                                                                                                                 ifelse(graphdatah$V2=="Polynesia","palegoldenrod",
                                                                                                                                        ifelse(graphdatah$V2=="Australia/New Zealand","Orange1",
                                                                                                                                               ifelse(graphdatah$V2=="Melanesia","Orange3",
                                                                                                                                                      ifelse(graphdatah$V2=="Micronesia","Orange4", "gray"))))))))))))))))))))),
     pch = 19)
legend("bottomright", legend=c("0% change","20% change", "40% change"),
       col=c("gray47", "gray47"), lty=c("solid","dashed","dotted"), cex=0.8)
abline(coef=c(0,1))
abline(coef=c(0,1.2),lty="dashed",col="gray")
abline(coef=c(0,0.8),lty="dashed",col="gray")
abline(coef=c(0,1.4),lty="dotted",col="gray")
text(87.65,89.27, 'Monaco', cex=0.7, pos=3)
text(45.49,52.84, 'Afghanistan', cex=0.7, pos=1)
text(39.84,59.79, 'Sierra Leone', cex=0.7, pos=2)
text(72.25,71, 'Venezuela', cex=0.7, pos=1)
par(mar=c(3, 3, 3, 2),mgp=c(2.5,0.8,0))
plot.new()
par(xpd=TRUE)
legend("center", legend = c("Middle Africa","Western Africa","Eastern Africa","Northern Africa","Southern Africa","Melanesia ","Micronesia ","Polynesia ","Australia/New Zealand","Western Asia","South-Central Asia","South-Eastern Asia","Eastern Asia","Central America","South America","Caribbean","Northern America","Northern Europe","Western Europe","Southern Europe","Eastern Europe"),  col= mycolors, pch = 15, pt.cex = 1.5, cex = 0.8, inset = c(-0.12, 0) , ncol = 4)
par(xpd=FALSE)


#*** Change of Life Expectancy rate Famales 2000 - 2020 by Country and subregion
layout(mat = matrix(c(1,2),2,1, byrow=TRUE), height = c(15,7))                                                                                             
par(mgp=c(2.5,0.8,0), mar=c(3.4, 4, 1, 1)) #bottom, left, top and right
plot(graphdatah$V8,
     graphdatah$V7,
     #main = "Change of Total Fertility Rate 2000 - 2020",
     xlab="Life Expectancy Females 2000 [Years]",
     ylab = "Life Expectancy Females 2020 [Years]",
     xlim=c(35,95),
     ylim=c(35,95),
     col= ifelse(graphdatah$V2=="Northern Africa","olivedrab",
                 ifelse(graphdatah$V2=="Middle Africa","olivedrab1" ,
                        ifelse(graphdatah$V2=="Western Africa","olivedrab3",
                               ifelse(graphdatah$V2=="Southern Africa","palegreen4",
                                      ifelse(graphdatah$V2=="Eastern Africa","springgreen4",
                                             ifelse(graphdatah$V2=="Caribbean","orchid1",
                                                    ifelse(graphdatah$V2=="South America","mediumpurple1",
                                                           ifelse(graphdatah$V2=="Central America","mediumpurple2",
                                                                  ifelse(graphdatah$V2=="Northern America","mediumpurple4",
                                                                         ifelse(graphdatah$V2=="South-Central Asia","indianred1",
                                                                                ifelse(graphdatah$V2=="Western Asia","red1",
                                                                                       ifelse(graphdatah$V2=="South-Eastern Asia","red3",
                                                                                              ifelse(graphdatah$V2=="Eastern Asia","red4",
                                                                                                     ifelse(graphdatah$V2=="Southern Europe","lightskyblue" ,
                                                                                                            ifelse(graphdatah$V2=="Western Europe","royalblue1",
                                                                                                                   ifelse(graphdatah$V2=="Eastern Europe","royalblue3",
                                                                                                                          ifelse(graphdatah$V2=="Northern Europe","navyblue",
                                                                                                                                 ifelse(graphdatah$V2=="Polynesia","palegoldenrod",
                                                                                                                                        ifelse(graphdatah$V2=="Australia/New Zealand","Orange1",
                                                                                                                                               ifelse(graphdatah$V2=="Melanesia","Orange3",
                                                                                                                                                      ifelse(graphdatah$V2=="Micronesia","Orange4", "gray"))))))))))))))))))))),
     pch = 19)
legend("bottomright", legend=c("0% change","20% change", "40% change"),
       col=c("gray47", "gray47"), lty=c("solid","dashed","dotted"), cex=0.8)
abline(coef=c(0,1))
abline(coef=c(0,1.2),lty="dashed",col="gray")
abline(coef=c(0,0.8),lty="dashed",col="gray")
abline(coef=c(0,1.4),lty="dotted",col="gray")
par(mar=c(3, 3, 3, 2),mgp=c(2.5,0.8,0))
plot.new()
par(xpd=TRUE)
legend("center", legend = c("Middle Africa","Western Africa","Eastern Africa","Northern Africa","Southern Africa","Melanesia ","Micronesia ","Polynesia ","Australia/New Zealand","Western Asia","South-Central Asia","South-Eastern Asia","Eastern Asia","Central America","South America","Caribbean","Northern America","Northern Europe","Western Europe","Southern Europe","Eastern Europe"),  col= mycolors, pch = 15, pt.cex = 1.5, cex = 0.8, inset = c(-0.12, 0) , ncol = 4)
par(xpd=FALSE)


#*** Change of Life Expectancy rate Males 2000 - 2020 by Country and subregion
layout(mat = matrix(c(1,2),2,1, byrow=TRUE), height = c(15,7))                                                                                             
par(mgp=c(2.5,0.8,0), mar=c(3.4, 4, 1, 1)) #bottom, left, top and right
plot(graphdatah$V10,
     graphdatah$V9,
     #main = "Change of Total Fertility Rate 2000 - 2020",
     xlab="Life Expectancy Males 2000 [Years]",
     ylab = "Life Expectancy Males 2020 [Years]",
     xlim=c(35,95),
     ylim=c(35,95),
     col= ifelse(graphdatah$V2=="Northern Africa","olivedrab",
                 ifelse(graphdatah$V2=="Middle Africa","olivedrab1" ,
                        ifelse(graphdatah$V2=="Western Africa","olivedrab3",
                               ifelse(graphdatah$V2=="Southern Africa","palegreen4",
                                      ifelse(graphdatah$V2=="Eastern Africa","springgreen4",
                                             ifelse(graphdatah$V2=="Caribbean","orchid1",
                                                    ifelse(graphdatah$V2=="South America","mediumpurple1",
                                                           ifelse(graphdatah$V2=="Central America","mediumpurple2",
                                                                  ifelse(graphdatah$V2=="Northern America","mediumpurple4",
                                                                         ifelse(graphdatah$V2=="South-Central Asia","indianred1",
                                                                                ifelse(graphdatah$V2=="Western Asia","red1",
                                                                                       ifelse(graphdatah$V2=="South-Eastern Asia","red3",
                                                                                              ifelse(graphdatah$V2=="Eastern Asia","red4",
                                                                                                     ifelse(graphdatah$V2=="Southern Europe","lightskyblue" ,
                                                                                                            ifelse(graphdatah$V2=="Western Europe","royalblue1",
                                                                                                                   ifelse(graphdatah$V2=="Eastern Europe","royalblue3",
                                                                                                                          ifelse(graphdatah$V2=="Northern Europe","navyblue",
                                                                                                                                 ifelse(graphdatah$V2=="Polynesia","palegoldenrod",
                                                                                                                                        ifelse(graphdatah$V2=="Australia/New Zealand","Orange1",
                                                                                                                                               ifelse(graphdatah$V2=="Melanesia","Orange3",
                                                                                                                                                      ifelse(graphdatah$V2=="Micronesia","Orange4", "gray"))))))))))))))))))))),
     pch = 19)
legend("bottomright", legend=c("0% change","20% change", "40% change"),
       col=c("gray47", "gray47"), lty=c("solid","dashed","dotted"), cex=0.8)
abline(coef=c(0,1))
abline(coef=c(0,1.2),lty="dashed",col="gray")
abline(coef=c(0,0.8),lty="dashed",col="gray")
abline(coef=c(0,1.4),lty="dotted",col="gray")
par(mar=c(3, 3, 3, 2),mgp=c(2.5,0.8,0))
plot.new()
par(xpd=TRUE)
legend("center", legend = c("Middle Africa","Western Africa","Eastern Africa","Northern Africa","Southern Africa","Melanesia ","Micronesia ","Polynesia ","Australia/New Zealand","Western Asia","South-Central Asia","South-Eastern Asia","Eastern Asia","Central America","South America","Caribbean","Northern America","Northern Europe","Western Europe","Southern Europe","Eastern Europe"),  col= mycolors, pch = 15, pt.cex = 1.5, cex = 0.8, inset = c(-0.12, 0) , ncol = 4)
par(xpd=FALSE)