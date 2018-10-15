#

#link population

sample = rnorm(10000)

setwd('H:/13DVC/Laptop Stand Calculations/')

anthroId = c('armLength_lower','armLength_upper','eyeHeight_sitting','shoulderHeight_sitting')

sapply(anthroId,function(anthro) assign(anthro,read.csv(paste(anthro,'.csv',sep='')),envir = .GlobalEnv))

for(i in c('Male','Female')){
    for(j in anthroId){
        set=get(j)
        sd. = sqrt(as.vector(set$Var)[set$Sex ==i]) 
        mean.=set$Mean[set$Sex==i]
        assign(paste(i,j,sep = '_'),sample*sd.+ mean.,
        envir = .GlobalEnv)
    }
}

layout(matrix(1:8,2))

for(i in c('Male','Female')){
    for(j in anthroId){
        print(paste(i,'_',j,'=...',sep=''))
        print(head((get(paste(i,j,sep = '_')))))
        hist((get(paste(i,j,sep = '_'))),main = paste(i,j,sep = '_'))
        print(summary(get(paste(i,j,sep = '_'))))
    }
}






#---------------------------------
fxCA=function(ATA=pi/12,yShoulder,AL,AU,L=279.4 ,Lk=100) (L - Lk)*cos(ATA) + sqrt(AL^2 + AU^2)*sin(ATA + atan(AL/AU))
fyCA=function(ATA=pi/12,yShoulder,AL,AU,L=279.4 ,Lk=100) (L - Lk)*sin(ATA) - sqrt(AL^2 + AU^2)*cos(ATA + atan(AL/AU)) + yShoulder
fxCS=function(ATE=pi/12,D=500,L=279.4) D*cos(ATE) - 1/2*L*sin(ATE)
fyCS=function(ATE=pi/12,yEye,D=500,L=279.4) -1/2*L*cos(ATE) - D*sin(ATE) + yEye

fCA = function(ATA=pi/12,yShoulder,AL,AU,L=279.4 ,Lk=100){
    c(x = fxCA(ATA,yShoulder,AL,AU,L,Lk),y=fxCA(ATA,yShoulder,AL,AU,L,Lk))
}
#keypad optimization

fCS = function(ATE=pi/12,yEye,D=500,L=279.4){
    c(x = fxCS(ATE,D,L),y = fyCS(ATE,yEye,D,L))
}#screen optimization

fCF = function(FC,L=279.4,ATA=pi/12) {
    c(x = FC['x'] - L*cos(ATA),
     y = FC['y'] - L*sin(ATA))
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

fms - yTable

fma
fms
fmfa = fCF(fma)
fmfs = fCF(fms)



fmfa - c(0,yTable)
fmfs - c(0,yTable)
#F
ffa = fCA(yShoulder = yChair + 555.5,
   AL = 243.4,
   AU = 311.9)
ffs = fCS(yEye = yChair + 738.7,ATE = 10/180*pi)
ffa
ffs

ffs - yTable

fffa = fCF(ffa)
fffs = fCF(ffs)

fffa - c(0,yTable)
fffs - c(0,yTable)
#-------------



#Calculate for point cloud for male

Male<-data.frame(AU =Male_armLength_upper,AL = Male_armLength_lower,yShoulder = yChair + Male_shoulderHeight_sitting,yEye = yChair + Male_eyeHeight_sitting)
Female<-data.frame(AL = Female_armLength_lower,AU = Female_armLength_upper,yShoulder = yChair + Female_shoulderHeight_sitting,yEye = yChair + Female_eyeHeight_sitting)

Male<-within(Male,{
    fxCA = fxCA(AU=AU,AL=AL,yShoulder=yShoulder)
    fyCA = fyCA(AU=AU,AL=AL,yShoulder=yShoulder)
    fxCS = rep(fxCS(),by = nrow(Male))
    fyCS = fyCS(yEye=yEye)
    fxCF = fxCF(fxCS)
    fyCF = fyCF(fyCS)
    
})

Female<-within(Female,{
    fxCA = fxCA(AU=AU,AL=AL,yShoulder=yShoulder)
    fyCA = fyCA(AU=AU,AL=AL,yShoulder=yShoulder)
    fxCS = rep(fxCS(),by = nrow(Male))
    fyCS = fyCS(yEye=yEye)
    fxCF = fxCF(fxCS)
    fyCF = fyCF(fyCS)
    
})

layout(matrix(1,1))

plot(c(0,1000),c(600,1300),main = 'Workspace',type = 'n',asp = 1)

abline(h = yChair,lty = 2,col = 'purple')
abline(h = yTable,lty = 2,col = 'purple')

points(Male$fxCA,Male$fyCA,col = 'blue')
points(Male$fxCS,Male$fyCS,col = 'green')

points(Female$fxCA,Female$fyCA,col = 'red')
points(Female$fxCS,Female$fyCS,col = 'orange')

#-------------------------------------------------------------------
#check for inconsistency

plot_locationInconsistency<-function(data,tarX1,tarY1,tarX2,tarY2,type = 'l', col = 'red'){
    for (i in seq_along(tarX1)){
    points(c(tarX1[i],tarX2[i]),c(tarY1[i],tarY2[i]),type = type,col=col)
    }
}


plot(c(300,800),c(750,1150),main = 'Workspace',type = 'n',asp = 1)
plot_locationInconsistency(tarX1 = Male$fxCA,tarX2=Male$fxCS,tarY1 = Male$fyCA,tarY2 = Male$fyCS,col = 'blue')

plot_locationInconsistency(tarX1 = Female$fxCA,tarX2=Female$fxCS,tarY1 = Female$fyCA,tarY2 = Female$fyCS)

#
Male = within(Male,{
    R = sqrt((fxCS - fxCA)^2+(fyCA-fyCS)^2)
})

Female = within(Male,{
    R = sqrt((fxCS - fxCA)^2+(fyCA-fyCS)^2)
})

layout(matrix(1:2,1))
hist(Male$R)
hist(Female$R)

summary(Male$R)
summary(Female$R)



#loop for armTilt

ATA = seq(from=0,to=15,by=.1)
RM=NULL
RF=NULL

system.time(    for(a in ATA){
    Male<-within(Male,{
    fxCA = fxCA(AU=AU,AL=AL,yShoulder=yShoulder,ATA=a)
    fyCA = fyCA(AU=AU,AL=AL,yShoulder=yShoulder,ATA=a)
    fxCS = rep(fxCS(),by = nrow(Male))
    fyCS = fyCS(yEye=yEye)
    fxCF = fxCF(fxCS)
    fyCF = fyCF(fyCS)
    
    })

    Female<-within(Female,{
    fxCA = fxCA(AU=AU,AL=AL,yShoulder=yShoulder,ATA=a)
    fyCA = fyCA(AU=AU,AL=AL,yShoulder=yShoulder,ATA=a)
    fxCS = rep(fxCS(),by = nrow(Male))
    fyCS = fyCS(yEye=yEye)
    fxCF = fxCF(fxCS)
    fyCF = fyCF(fyCS)
    
    })
    Male = within(Male,{
    R = sqrt((fxCS - fxCA)^2+(fyCA-fyCS)^2)
    })
    
    Female = within(Male,{
    R = sqrt((fxCS - fxCA)^2+(fyCA-fyCS)^2)
    })
    RM<-rbind(RM,summary(Male$R))
    RF<-rbind(RF,summary(Female$R))
    })
    
    RM<-cbind(ATA,RM)
    RF<-cbind(ATA,RF)
summary(RM)
summary(RF)

write.csv(RM,'Male_sensitivity_ATA.csv')
write.csv(RF,'Female_sensitivity_ATA.csv')


persp(as.matrix(RM))