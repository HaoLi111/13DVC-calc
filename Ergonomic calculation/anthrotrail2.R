#Generate anthropometric with z score from height

#Male

#stature 
setwd('E:/DVC')
(data<-read.csv('anthro.csv',header=T))

myheight=192

Z_anthro =function(x,...) UseMethod('Z_anthro')
Z_anthro.numeric<-function(x,mean,sd) (x-mean)/sd

Z_anthro.data.frame<-function(x,sample, target = NA){
  me=Z_anthro(sample,mean = data[target,'mean'],sd = data[target,'sd'])[1]#target is the 'type'
  me
}

ApplyAnthro<-function(z,data,type = 'largeN'){
  data[,'sd']*z + data['mean']
}

m = subset(data,sex=='m')
(z=Z_anthro(m,myheight,target = (m$type=='stature')))
(myself=ApplyAnthro(z,m))
(myself = data.frame(type=m[,'type'],me=myself))
  
#-------------------------------------------------fold this------

fxCA=function(ATA=pi/12,yShoulder,AL,AU,L=279.4 ,Lk=100) (L - Lk)*cos(ATA) + sqrt(AL^2 + AU^2)*sin(ATA + atan(AL/AU))
fyCA=function(ATA=pi/12,yShoulder,AL,AU,L=279.4 ,Lk=100) (L - Lk)*sin(ATA) - sqrt(AL^2 + AU^2)*cos(ATA + atan(AL/AU)) + yShoulder
fxCS=function(ATE=pi/12,D=500,L=279.4) D*cos(ATE) 
fyCS=function(ATE=pi/12,yEye,D=500,L=279.4)  - D*sin(ATE) + yEye

fCA = function(ATA=pi/12,yShoulder,AL,AU,L=279.4 ,Lk=100){
  c(x = fxCA(ATA,yShoulder,AL,AU,L,Lk),y=fyCA(ATA,yShoulder,AL,AU,L,Lk))
}
#keypad optimization

fCS = function(ATE=pi/12,yEye,D=500,L=279.4){
  c(x = fxCS(ATE,D,L),y = fyCS(ATE,yEye,D,L))
}#screen optimization



fS = function(FCS,L,ATE){
  c(FCS[1]- 1/2*L*sin(ATE),FCS[2]-1/2*L*cos(ATE))
}


fCF = function(FC,L=279.4,ATA=pi/12) {
  c(x = FC['x'] + L*cos(ATA),
    y = FC['y'] + L*sin(ATA))
}

fxCF = function(FCx,L=279.4,ATA=pi/12) FCx - L*cos(ATA)
fyCF = function(FCy,L=279.4,ATA = pi/12) FCy - L* sin(ATA)


#Calculate for male average and female average

yChair = 500
yTable = 750

#M
fma = fCA(yShoulder = yChair + 597.8,
          AL = 269.9,
          AU = 340.8)
fms = fCS(yEye = yChair + 792.0,ATE = 10/180*pi)#viewpoint absolute
#----------------------------------------------------------------

#trail 1 normal chair and table

yChair=50
yTable=75
#tilt  = 10 degree
ATA = 0
ATE =pi/12
Le=50
#computer
L=22
Lk=12.5

(myKeypad<-fCA(ATA,yShoulder = yChair+myself[myself$type=='shoulder_sitting',2],
              AU=myself[myself$type=='forearm',2],
              AL =myself[myself$type=='lowerarm',2],L,Lk))
(myPCfromKeyPad<-fCF(myKeypad,L,ATA))

(myScreen<-fCS(ATE,yEye = yChair + myself[myself$type=='eye_sitting',2],
                     Le,L))
(myPCfromScreen<-fS(myScreen,L,ATE))
library(MFVN)
MapR(myPCfromKeyPad,myPCfromScreen)

plot(c(0,60),c(0,150),type='n',asp=1)


#-----------------------------------------------------------------------

abline(h = yTable)
abline(h = yChair)
text(yChair,'Chair')
text(yTable,'Table')
points(myKeypad[1],myKeypad[2],col='red')
points(myPCfromKeyPad[1],myPCfromKeyPad[2],col = 'green')
points(myPCfromScreen[1],myPCfromScreen[2],col='blue')
points(0,myself[myself$type=='eye_sitting',2]+ yChair)
text(myself[myself$type=='eye_sitting',2]+ yChair,'eye')
points(0,myself[myself$type=='shoulder_sitting',2]+ yChair)
text(myself[myself$type=='shoulder_sitting',2]+ yChair,'shoulder')
abline(c(myself[myself$type=='eye_sitting',2]+ yChair,-atan(ATE)),col = 'blue')
points(myScreen[1],myScreen[2],col = 'orange')

#-----------------------------------------------------------------------


(hback = myPCfromScreen['y'] - yTable)
(m = (hback)/myPCfromScreen['x'])
tilt  = atan(m)

(dh= sin(tilt)* L)
(hfront =hback - dh)
(depth = cos(tilt)*L)
(tilt = tilt*180/pi)
(stand1 = list(hback=hback,m=m,tilt=tilt,dh=dh,depth=depth,hfront=hfront))

parameterize.stand<-function(myself,
                             ATA=0,
                             ATE=15/180*pi,
                             yChair=50,
                             yTable=75,
                             Le=50,
                             L=22,
                             Lk=12.5,plot=F){
  myKeypad<-fCA(ATA,yShoulder = yChair+myself[myself$type=='shoulder_sitting',2],
                 AU=myself[myself$type=='forearm',2],
                 AL=myself[myself$type=='lowerarm',2],L,Lk)
  myPCfromKeyPad<-fCF(myKeypad,L,ATA)
  
  myScreen<-fCS(ATE,yEye = yChair + myself[myself$type=='eye_sitting',2],
                 Le,L)
  myPCfromScreen<-fS(myScreen,L,ATE)
  PCxy=list(myKeyPad=myKeypad,myPCfromKeyPad=myPCfromKeyPad,
            myScreen=myScreen,myPCfromScreen)
  require(MFVN)
  MapR(myPCfromKeyPad,myPCfromScreen)
  if(plot==TRUE){
  plot(c(0,60),c(0,150),type='n',asp=1)
  
  
  #-----------------------------------------------------------------------
  
  abline(h = yTable)
  abline(h = yChair)
  text(yChair,'Chair')
  text(yTable,'Table')
  points(myKeypad[1],myKeypad[2],col='red')
  points(myPCfromKeyPad[1],myPCfromKeyPad[2],col = 'green')
  points(myPCfromScreen[1],myPCfromScreen[2],col='blue')
  points(0,myself[myself$type=='eye_sitting',2]+ yChair)
  text(myself[myself$type=='eye_sitting',2]+ yChair,'eye')
  points(0,myself[myself$type=='shoulder_sitting',2]+ yChair)
  text(myself[myself$type=='shoulder_sitting',2]+ yChair,'shoulder')
  abline(c(myself[myself$type=='eye_sitting',2]+ yChair,-atan(ATE)),col = 'blue')
  points(myScreen[1],myScreen[2],col = 'orange')
  }
  #-----------------------------------------------------------------------
  
  
  hback = myPCfromScreen['y'] - yTable
  m = (hback)/myPCfromScreen['x']
  tilt  = atan(m)
  
  dh= sin(tilt)* L
  hfront =hback - dh
  depth = cos(tilt)*L
  tilt = tilt*180/pi
  stand1 = c(hback=hback,m=m,tilt=tilt,dh=dh,depth=depth,hfront=hfront)
  list(Standl=stand1,PCxy=PCxy)
}

parameterize.stand(myself = myself)


parameterize.stand(myself,ATE=pi*11/180)$Standl


ATE=(0:30)/180*pi

ATE_criticality<-foreach(i=ATE,.combine = rbind) %dopar% {
  parameterize.stand(myself,ATE=i)$Standl
}
ATE_criticality
matplot(ATE_criticality[,1],ATE_criticality[,-1],type='l')
legend(10,legend=2:ncol(ATE_criticality))


Le_criticality<-foreach(i=)
parameterize.stand(myself,ATE=30/180*pi,Le=60)
