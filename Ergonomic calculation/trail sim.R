
rnormAnthro = function(n,data){
	MEAN = data$mean
	SD = data$sd
	RR = rnorm(n)
	re = matrix()
	for (i in 1:nrow(data)){
	
	re[,i]= re * SD[i] +  MEAN[i] 
	}
re
}

rnormAnthro_mf = function (n,data){
	M = subset(data,type == 'm')
	F = subset(data,type == 'f')
	list(M = rnormAnthro(n,data = M),F = rnormAnthro(n,data = F))
}

satis = function(){}

sampleAnthro_m=list()
for(i in 1:1000){
	sampleAnthro[[i]] = ApplyAnthro(Z_anthro(m,sampleHeight[i],
		 target =  (m$type=='stature)))
	sampleAnthro[[i]] = data.frame(type = m[,'type'],me = )
}

sampleAnthro_f=list()
for(i in 1:1000){
	sampleAnthro_f[[i]] = ApplyAnthro(Z_anthro(m,sampleHeight[i],
		 target =  (m$type=='stature)))
	sampleAnthro_f[[i]] = data.frame(type = f[,'type'],me = )
}

