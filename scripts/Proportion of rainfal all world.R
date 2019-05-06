rm(list = ls())

library(ncdf4)
library(fields)
library(Hmisc)
library(mapdata)
library(FactoMineR)
library (TTR)
setwd("~/Google Drive File Stream/My Drive/Macrosystems/Climate_data/")


pre <- nc_open("~/Documents/Climate Data/cru_ts4.01.1901.2016.pre.dat.nc")




pre$dim[[1]]$vals#longitude
pre$dim[[2]]$vals#latitude
pre$dim[[3]]$vals



#seq((-120+359.75), (-100+359.75), by= 0.5)   ## set the long boundaries 
xlim=seq((-135.75), (170.75), by= 0.5)   ## set the long boundaries 
ylim=seq(-70.25,70.25, by= 0.5)  ## set the lat boundaries 
length(ylim)/length(xlim)


x1 <- which(pre$dim[[1]]$vals == range(xlim)[1])[1] 
x_length <- length(xlim)
longitude <- pre$dim[[1]]$vals[pre$dim[[1]]$vals%in%(xlim)]



y1 <- which(pre$dim[[2]]$vals == range(ylim)[1])[1]
y_length <- length(ylim)
latitude <- rev(pre$dim[[2]]$vals[pre$dim[[2]]$vals%in%(ylim)])

zlim=1:12 #time...
zdim <- pre$dim[[3]]$vals # time
dim(zdim)

#temp <- get.var.ncdf(tmp, start=c(150,200,1), count = c(130, 100,1))
prec <-  ncvar_get(pre, start=c(x1,y1,1), count = c(x_length,y_length,dim(zdim)))



library(caTools)

yr <- as.integer(dim(prec)[3]/12)

DE.annual<- vector(mode = "numeric", length = dim(prec)[1]*dim(prec)[2]*yr)
JJ.annual<- vector(mode = "numeric", length = dim(prec)[1]*dim(prec)[2]*yr)
MA.annual<- vector(mode = "numeric", length = dim(prec)[1]*dim(prec)[2]*yr)
SO.annual<- vector(mode = "numeric", length = dim(prec)[1]*dim(prec)[2]*yr)
sols.annual<- vector(mode = "numeric", length = dim(prec)[1]*dim(prec)[2]*yr)
equi.annual<- vector(mode = "numeric", length = dim(prec)[1]*dim(prec)[2]*yr)

dim(DE.annual) <- c( dim(prec)[1],dim(prec)[2],yr)
dim(JJ.annual) <- c( dim(prec)[1],dim(prec)[2],yr)
dim(MA.annual) <- c( dim(prec)[1],dim(prec)[2],yr)
dim(SO.annual) <- c( dim(prec)[1],dim(prec)[2],yr)
dim(sols.annual) <- c( dim(prec)[1],dim(prec)[2],yr)
dim(equi.annual) <- c( dim(prec)[1],dim(prec)[2],yr)

count <- 1

for(i in 1:yr){
  a <- prec[,,count:(count+11)] 
  annualmean <- apply(a, c(1,2), sum, na.rm = T)  
  DE_rmean <- apply(a[,,c(12,1,2)], c(1,2), sum, na.rm = T)
  JJ_rmean <- apply(a[,,c(6,7,8)], c(1,2), sum, na.rm = T)
  MA_rmean <- apply(a[,,c(3,4,5)], c(1,2), sum, na.rm = T)
  SO_rmean <- apply(a[,,c(9,10,11)], c(1,2), sum, na.rm = T)
  sols_rmean <- apply(a[,,c(12,1,6,7)], c(1,2), sum, na.rm = T)
  equi_rmean <- apply(a[,,c(3,4,9,10)], c(1,2), sum, na.rm = T)
  
  DE.annual[,,i] <-( DE_rmean/annualmean)*100
  JJ.annual[,,i] <-( JJ_rmean/annualmean)*100
  MA.annual[,,i] <-( MA_rmean/annualmean)*100
  SO.annual[,,i] <-( SO_rmean/annualmean)*100
  sols.annual[,,i] <-( sols_rmean/annualmean)*100
  equi.annual[,,i] <-( equi_rmean/annualmean)*100
  count <- count+12
}
str(DE.annual)


DE.annual.mean <- apply(DE.annual, c(1,2), mean, na.rm = F) 
JJ.annual.mean <- apply(JJ.annual, c(1,2), mean, na.rm = F) 
MA.annual.mean <- apply(MA.annual, c(1,2), mean, na.rm = F) 
SO.annual.mean <- apply(SO.annual, c(1,2), mean, na.rm = F) 
sols.annual.mean <- apply(sols.annual, c(1,2), mean, na.rm = F) 
equi.annual.mean <- apply(equi.annual, c(1,2), mean, na.rm = F) 

#w_s <- (sprec.annual.mean+wprec.annual.mean)/2

source("~/Google Drive File Stream/My Drive/Macrosystems/Data Analysis/Colors_paulszejner.R")


# color bar ---------------------------------------------------------------
library(fields)
color.bar <- function(lut, min, max=-min, nticks=11, ticks=seq(min, max, len=nticks), title='') {
  scale = (length(lut))/(max-min)
  
  #dev.new(width=1.75, height=5)
  plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
  for (i in 1:(length(lut))) {
    y = (i-1)/scale + min
    rect(0,y,10,y+1/scale, col=lut[i], border=NA)
  }
  #box()
  axis(2, ticks, las=1, tcl=0.2,hadj = 0.5, lwd=0.5)
  axis(4,labels = F, ticks, las=1, tcl=0.2, lwd=0.5)
  
}
color.bar_h <- function(lut, min, max=-min, nticks=11, ticks=seq(min, max, len=nticks), title='') {
  scale = (length(lut))/(max-min)
  
  #dev.new(width=1.75, height=5)
  plot(y=c(0,10), x=c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
  for (i in 1:(length(lut))) {
    y = (i-1)/scale + min
    rect(y,0,y+1/scale,10, col=lut[i], border=NA)
  }
  #box()
  axis(3, labels =F,ticks, las=1,padj = 1, lwd=0.5)
  axis(1, labels =T ,ticks, las=1,padj = -1, lwd=0.5)
  
}




library("astsa")
col.N_South  <-  colorRampPalette(c(rgb(0/255,0/255,0/255,0.5),rgb(3/255,95/255,160/255,0.6), rgb(86/255,151/255,204/255, 0.6),
                                    rgb(0/255,180/255,12/255, 0.6),
                                    rgb(230/255,230/255,16/255, 0.6),rgb(245/255,180/255,12/255, 0.6)),alpha = TRUE)
Col.7     <- col.N_South (n = length(0))                           
ye <- rgb(255/255,252/255,4/255 )
rd <- rgb(217/255,0/255,255/255 )
or <-rgb(255/255,162/255,0/255 )
pu <- rgb(217/255,0/255,255/255 )
gr <- rgb(10/255,200/255,60/255 )
bl <- rgb(40/255,25/255,160/255 ) #(21/255,84/255,105/255 ), (52/255, 122/255, 145255),,, (129/255, 193/255, 214/255)
skb <- rgb(40/255,210/255,230/255 )
sk <- rgb(160/255,240/255,250/255 )
colfunc.5 <- colorRampPalette(c(rgb(100/255, 70/255, 5/255), 
                                rgb(166/255, 115/255, 20/255),
                                rgb(214/255, 159/255, 58/255), 
                                rgb(240/255,172/255,46/255 ), 
                                "white",     "white",
                                rgb(129/255, 193/255, 214/255), 
                                rgb(52/255, 122/255, 145/255), 
                                rgb(21/255,84/255,105/255 ), 
                                rgb(6/255, 70/255, 92/255)))
Col.5 <-  colfunc.5(20)






colfunc.5 <- colorRampPalette(c( 
  "darkorange", "orange", "orange", rgb(248/255, 31/255, 245/255)  , 
  rgb(82/255, 142/255, 165/255),
  rgb(52/255, 122/255, 145/255),
  rgb(52/255, 122/255, 145/255),
  rgb(21/255,84/255,105/255 ),
  rgb(21/255,84/255,105/255 ),
  rgb(6/255, 70/255, 92/255),
  rgb(6/255, 70/255, 92/255),
  "black"))


Col.br <- (colfunc.5(100))




# plot --------------------------------------------------------------------



library(mapdata)
#png("~/Downloads/proportion of sumer sols equi rainfall world.png",15,8,units = "in",res = 600, pointsize=15, family= "helvetica") 
#png("~/Google Drive File Stream/My Drive/proportion of SEASONAL  world.png",height = 9, width = 7,units = "in",res = 600, pointsize=15, family= "helvetica") 
#png("~/Documents/Macrosystems/proportion of sumer solstice  world.png",5.5,7,units = "in",res = 600, pointsize=12, family= "helvetica") 
pdf("~/Google Drive File Stream/My Drive/proportion of sumer JAS rainfall world.pdf",height = 9,width = 7, pointsize=13) 


layout(mat = matrix(c(1,1,1,1,1,2,3,4,5,6),nrow = 5, ncol = 2, byrow = F),widths = c(1,6),heights = c(2.5,2.5,2.5,2.5,0.3),respect = T)
par(mar=c(5,2,5,0.7),lwd=0.5, cex=0.8)
color.bar((Col.br), min =0 , max =90 , nticks=10)
mtext("% Annual precipitation" ,line=1,cex=0.9,bty="n", side = 2, padj = -1)      


#DE

par(mar=c(0,0.1,0.3,1), lwd= 0.3, cex.axis=0.7)
image(xlim,ylim, DE.annual.mean, col=(Col.br), zlim= c(0,90), las=1 ,lwd=0.2, axes=F, xlab="", ylab="")
legend("bottom",legend = "December-February", cex=0.7, bty="n")
#map("worldHires",interior = T,add=T,xlim=c(-135,170), ylim=c(-70,70), lwd=1, col="black" , resolution=0.1)
box(lwd=0.6)
axis(1, lwd=0.5,tck= -0.01, labels = F)
axis(4, lwd=0.5, las=2, tck= -0.01, hadj=1)

# MA

image(xlim,ylim, MA.annual.mean, col=(Col.br), zlim= c(0,90), las=1 ,lwd=0.2, axes=F, xlab="", ylab="")
legend("bottom",legend = "March-May", cex=0.7, bty="n")
#map("worldHires",interior = T,add=T,xlim=c(-135,170), ylim=c(-70,70), lwd=1, col="black" , resolution=0.1)
box(lwd=0.6)

axis(1, lwd=0.5,tck= -0.01, labels = F)
axis(4, lwd=0.5, las=2, tck= -0.01, hadj=1)

#JJ


image(xlim,ylim, JJ.annual.mean, col=(Col.br), zlim= c(0,90), las=1 ,lwd=0.2, axes=F, xlab="", ylab="")
legend("bottom",legend = "June-August", cex=0.7, bty="n")
#map("worldHires",interior = T,add=T,xlim=c(-135,170), ylim=c(-70,70), lwd=1, col="black" , resolution=0.1)
box(lwd=0.6)

axis(1, lwd=0.5,tck= -0.01 , labels = F)
axis(4, lwd=0.5, las=2, tck= -0.01, hadj=1)

#SO

image(xlim,ylim, SO.annual.mean, col=(Col.br), zlim= c(0,90), las=1 ,lwd=0.2, axes=F, xlab="", ylab="")
legend("bottom",legend = "September-November", cex=0.7, bty="n")
#map("worldHires",interior = T,add=T,xlim=c(-135,170), ylim=c(-70,70), lwd=1, col="black" , resolution=0.1)
box(lwd=0.6)

axis(1, lwd=0.5,tck= -0.01, padj=-2.5)
axis(4, lwd=0.5, las=2, tck= -0.01, hadj=1)

dev.off()
