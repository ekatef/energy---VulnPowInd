#=================================================================================================
#	Technical file: influence of different avaraging approaches was used
#	The target was the sensitivity function, the parameters included a, d_k and m
#	Functions from "CondSensModel.r" and "Sensitivity visualisation.r" were used
#=================================================================================================
# average along all water velocity values
#			t_ind_range is the index of the temperature range corresponding
#			to the maximum temperature value under certain conditions
Sens_avr_byW<-function(a_avr,m_avr,d_k_range_avr,t_avr) {
	# in reality, the velocity of cooling water is usually >0.8..0.9 m/s
	# so, we have to narrow the velocity range for the impact study
	x<-mean(Sens(a=a_avr,m_nom=m_avr,d_k=d_k_range_avr,W_nom=W_range[14],
		W=W_range[10:length(W_range)],t=t_avr))
	return(x)
}
Sens_avr_byW_alongT<-function(a_avrT,m_avrT,d_k_range_avrT,t_ind_range=1:250) {
	x<-sapply(FUN=Sens_avr_byW,a_avr=a_avrT,m_avr=m_avrT,
		d_k_range_avr=d_k_range_avrT,X=t_range[c(min(t_ind_range):max(t_ind_range))])
	return(x)
}
Sens_avr_byW_byD_k<-function(a_avrD,m_avrD,t_ind_rangeD=1:250) {
	Sens_alongT_list<-lapply(FUN=Sens_avr_byW_alongT,a_avrT=a_avrD,m_avrT=m_avrD,
		t_ind_range=t_ind_rangeD,X=d_k_range)
	Sens_alongT_df<- data.frame(matrix(unlist(Sens_alongT_list), nrow=length(Sens_alongT_list), 
		byrow=TRUE),stringsAsFactors=FALSE)
	Sens_byDk<-apply(Sens_alongT_df,2,mean)
	return(Sens_byDk)
}
Sens_avr_byW_byA<-function(m_avrA,d_k_avrA,t_ind_rangeA=1:250) {
	Sens_alongT_list<-lapply(FUN=Sens_avr_byW_alongT,m_avrT=m_avrA,d_k_range_avrT=d_k_avrA,
		t_ind_range=t_ind_rangeA,X=a_range)
	Sens_alongT_df<- data.frame(matrix(unlist(Sens_alongT_list), nrow=length(Sens_alongT_list),
		byrow=TRUE),stringsAsFactors=FALSE)
	Sens_byA<-apply(Sens_alongT_df,2,mean)
	return(Sens_byA)
}
Sens_avr_byW_byM<-function(a_avrM,d_k_avrM,t_ind_rangeM=1:250) {
	Sens_alongT_list<-lapply(FUN=Sens_avr_byW_alongT,a_avrT=a_avrM,d_k_range_avrT=d_k_avrM,
		t_ind_range=t_ind_rangeM,X=m_range)
	Sens_alongT_df<- data.frame(matrix(unlist(Sens_alongT_list), nrow=length(Sens_alongT_list),
		byrow=TRUE),stringsAsFactors=FALSE)
	Sens_byM<-apply(Sens_alongT_df,2,mean)
	return(Sens_byM)
}
#	TEST OF ORDER OF AVERAGING
# Averaging along a by mean values of other parameters
pdf("Infl of averaging order.pdf")
	legend_text<-c("along a by mean others","along d_k by mean others","along m by mean others","all means")
	compar_colors<-c("darkorange1","firebrick3","forestgreen","gray5")
	plot(x=t_range[1:250],y=Sens_avr_byW_byA(m_avrA=mean(m_range),d_k_avrA=mean(d_k_range),t_ind_rangeA=1:250),
		type="l",col=compar_colors[1],lwd=3,lty=1,ylab="Sensitivity",main="Influence of averaging order of averaging",
		panel.first=grid(col="gray15"))
	points(x=t_range[1:250],y=Sens_avr_byW_byA(m_avrA=mean(m_range),d_k_avrA=mean(d_k_range),t_ind_rangeA=1:250),
		pch=19,col=compar_colors[1])
	lines(x=t_range[1:250],y=Sens_avr_byW_byD_k(a_avrD=mean(a_range),m_avrD=mean(m_range),t_ind_range=1:250),
		type="l",col=compar_colors[2],lwd=3,lty=1)
	lines(x=t_range[1:250],y=Sens_avr_byW_byM(a_avrM=mean(a_range),d_k_avrM=mean(d_k_range)),
		type="l",col=compar_colors[3],lwd=3,lty=1)
	points(x=t_range[1:250],y=Sens_avr_byW_byM(a_avrM=mean(a_range),d_k_avrM=mean(d_k_range)),
		pch=3,col=compar_colors[3])
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=mean(d_k_range)),type="l",col=compar_colors[4],lwd=5,lty=1)
	legend("topright",legend=legend_text,lty=1,lwd=3,col=compar_colors,bg="white")
	dev.off()
#***********************************************************************************************************************
#								Illustration
#***********************************************************************************************************************
pdf("Sensitivity_parametric.pdf")
# I1) Effect of a by mean values of other parameters
	# dev.new()
	plot(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=a_range[1],m_avrT=mean(m_range),
		d_k_range_avrT=mean(d_k_range),t_ind_range=1:250),mgp=c(2.5,1,0),
		type="l",col="darkred",lwd=5,main="All parameters are mean",
		xlab="Water temperature, °C",ylab="Condenser temperature sensitivity coefficient",
		panel.first=grid(col="gray30"),sub="Effect of a",cex.axis=1.5,cex.lab=1.7)
	#
	for (i in seq(from=2,along.with=a_range[-1])) {
		lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=a_range[i],m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=mean(d_k_range)),type="l",col="coral",lwd=2)
	}
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=a_range[length(a_range)],m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=mean(d_k_range)),type="l",col="darkred",lwd=5)
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=a_range[1],m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=mean(d_k_range)),type="l",col="darkblue",lwd=5)
#	tiny difference between averaging of the functions by different a values
	# lines(x=t_range[1:250],y=Sens_avr_byW_byA(m_avrA=mean(m_range),d_k_avrA=mean(d_k_range),t_ind_rangeA=1:250),
	# 	type="l",col="chartreuse4",lwd=3,lty=1)
#	... and function of mean a
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=mean(d_k_range)),type="l",col="darkgreen",lwd=2,lty=5)
	text(x=t_range[190],y=0.12+(Sens_avr_byW_alongT(a_avrT=a_range[length(a_range)],m_avrT=mean(m_range),
			d_k_range_avrT=mean(d_k_range),t_ind_range=190)),labels="max(a)",cex=2)
	text(x=t_range[50],y=-0.275+(Sens_avr_byW_alongT(a_avrT=a_range[length(a_range)],m_avrT=mean(m_range),
			d_k_range_avrT=mean(d_k_range),t_ind_range=50)),labels="min(a)",cex=2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# I2) Effect of d_k by mean values of other parameters
	# dev.new()
	plot(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=mean(m_range),
		t_ind_range=1:250,d_k_range_avrT=d_k_range[1]),mgp=c(2.5,1,0),
		type="l",col="darkred",lwd=5,main="All parameters are mean",
		xlab="Water temperature, °C",ylab="Condenser temperature sensitivity coefficient",
		panel.first=grid(col="gray30"),sub="Effect of d_k",cex.axis=1.5,cex.lab=1.7)
		for (i in seq(from=2,along.with=a_range[-1])) {
			lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=mean(m_range),t_ind_range=1:250,
			d_k_range_avrT=d_k_range[i]),type="l",col="coral",lwd=2)
		}
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=d_k_range[length(d_k_range)]),type="l",col="darkred",lwd=5)
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=d_k_range[1]),type="l",col="darkblue",lwd=5)
	#
	# lines(x=t_range[1:250],y=Sens_avr_byW_byD_k(a_avrD=mean(a_range),m_avrD=mean(m_range),t_ind_range=1:250),
	# 	type="l",col="chartreuse4",lwd=3,lty=1)
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=mean(d_k_range)),type="l",col="darkgreen",lwd=2,lty=5)
	text(x=t_range[200],y=0.2+(Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=mean(m_range),
		t_ind_range=200,d_k_range_avrT=d_k_range[1])),labels="max(a)",cex=2)
	text(x=t_range[80],y=-0.1+(Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=mean(m_range),
		t_ind_range=80,d_k_range_avrT=d_k_range[1])),labels="min(a)",cex=2)	
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# I3) Effect of m by mean values of other parameters
m_range_param<-seq(from=min(m_range),to=max(m_range),length.out=20)
	# dev.new()
	plot(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=m_range_param[1],
		t_ind_range=1:250,d_k_range_avrT=mean(d_k_range)),mgp=c(2.5,1,0),
		type="l",col="darkblue",lwd=5,main="All parameters are mean",
		xlab="Water temperature, °C",ylab="Condenser temperature sensitivity coefficient",
		panel.first=grid(col="gray30"),sub="Effect of m_nom",cex.axis=1.5,cex.lab=1.7)
	for (i in seq(from=2,along.with=a_range[-1])) {
		lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=m_range_param[i],t_ind_range=1:250,
			d_k_range_avrT=mean(d_k_range)),type="l",col="coral",lwd=1)
	}
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=m_range_param[1],t_ind_range=1:250,
		d_k_range_avrT=mean(d_k_range)),type="l",col="darkblue",lwd=5)
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=m_range_param[length(m_range_param)],
		t_ind_range=1:250,d_k_range_avrT=mean(d_k_range)),type="l",col="darkred",lwd=5)
	# lines(x=t_range[1:250],y=Sens_avr_byW_byM(a_avrM=mean(a_range),d_k_avrM=mean(d_k_range)),
	# 	type="l",col="chartreuse4",lwd=3,lty=1)
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=mean(d_k_range)),type="l",col="darkgreen",lwd=2,lty=5)
	text(x=t_range[200],y=0.14+(Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=m_range_param[1],
		t_ind_range=200,d_k_range_avrT=mean(d_k_range))),labels="max(a)",cex=2)
	text(x=t_range[70],y=-0.17+(Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=m_range_param[1],
		t_ind_range=70,d_k_range_avrT=mean(d_k_range))),labels="min(a)",cex=2)
dev.off()
#***********************************************************************************************************************
#								DETAILS
#***********************************************************************************************************************
pdf("Sensitiv_ParametrDependence.pdf")
# Effect of a on Sensitivity 
# by mean values of other parameters
	# dev.new()
	plot(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=a_range[1],m_avrT=mean(m_range),
		d_k_range_avrT=mean(d_k_range),t_ind_range=1:250),
		type="l",col="darkred",lwd=5,main="All parameters are mean",
		xlab="t_water, C",ylab="Condenser temperature sensitivity coefficient, C/C",
		panel.first=grid(col="black"),sub="Effect of a")
	#
	for (i in seq(from=2,along.with=a_range[-1])) {
		lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=a_range[i],m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=mean(d_k_range)),type="l",col="red",lwd=2)
	}
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=a_range[length(a_range)],m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=mean(d_k_range)),type="l",col="darkblue",lwd=5)
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=a_range[1],m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=mean(d_k_range)),type="l",col="darkred",lwd=5)
#	the difference between averaging of the functions by different a values
	lines(x=t_range[1:250],y=Sens_avr_byW_byA(m_avrA=mean(m_range),d_k_avrA=mean(d_k_range),t_ind_rangeA=1:250),
		type="l",col="chartreuse4",lwd=3,lty=1)
#	... and function of mean a
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=mean(d_k_range)),type="l",col="darkgreen",lwd=2,lty=2)
#***********************************************************************************************************************
# Effect of d_k on Sensitivity
# D1) D2 is min, other parameters are mean
	# dev.new()
	plot(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=a_range[1],m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=min(d_k_range)),type="l",col="darkred",lwd=5,main="D2 is min, other parameters are mean",
		xlab="t_water, C",ylab="Condenser temperature sensitivity coefficient, C/C",panel.first=grid(col="black"),
		sub="Effect of a")
		for (i in seq(from=2,along.with=a_range[-1])) {
			lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=a_range[i],m_avrT=mean(m_range),
			d_k_range_avrT=min(d_k_range)),type="l",col="red",lwd=2)
		}
	#
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=a_range[length(a_range)],m_avrT=mean(m_range),
		d_k_range_avrT=min(d_k_range)),type="l",col="darkblue",lwd=5)
	#
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=a_range[1],m_avrT=mean(m_range),
		d_k_range_avrT=min(d_k_range)),type="l",col="darkred",lwd=5)
	lines(x=t_range[1:250],y=Sens_avr_byW_byA(m_avrA=mean(m_range),d_k_avrA=min(d_k_range)),
		type="l",col="chartreuse4",lwd=3,lty=1)
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=mean(m_range),
		d_k_range_avrT=min(d_k_range)),type="l",col="darkgreen",lwd=2,lty=2)
# D2) D2 is max, other parameters are mean
	# dev.new()
	plot(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=a_range[1],m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=max(d_k_range)),type="l",col="darkred",lwd=5,main="D2 is max, other parameters are mean",
		xlab="t_water, C",ylab="Condenser temperature sensitivity coefficient, C/C",panel.first=grid(col="black"),
		sub="Effect of a")
		for (i in seq(from=2,along.with=a_range[-1])) {
			lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=a_range[i],m_avrT=mean(m_range),t_ind_range=1:250,
			d_k_range_avrT=max(d_k_range)),type="l",col="red",lwd=2)
		}
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=a_range[length(a_range)],m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=max(d_k_range)),type="l",col="darkblue",lwd=5)
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=a_range[1],m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=max(d_k_range)),type="l",col="darkred",lwd=5)
	lines(x=t_range[1:250],y=Sens_avr_byW_byA(m_avrA=mean(m_range),d_k_avrA=max(d_k_range)),
		type="l",col="chartreuse4",lwd=3,lty=1)
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=max(d_k_range)),type="l",col="darkgreen",lwd=2,lty=2)
# D3) All parameters are mean
	# dev.new()
	plot(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=d_k_range[1]),type="l",col="darkred",lwd=5,main="All parameters are mean",
		xlab="t_water, C",ylab="Condenser temperature sensitivity coefficient, C/C",panel.first=grid(col="black"),
		sub="Effect of d_k")
		for (i in seq(from=2,along.with=a_range[-1])) {
			lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=mean(m_range),t_ind_range=1:250,
			d_k_range_avrT=d_k_range[i]),type="l",col="red",lwd=2)
		}
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=d_k_range[length(d_k_range)]),type="l",col="darkblue",lwd=5)
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=d_k_range[1]),type="l",col="darkred",lwd=5)
	#
	lines(x=t_range[1:250],y=Sens_avr_byW_byD_k(a_avrD=mean(a_range),m_avrD=mean(m_range),t_ind_range=1:250),
		type="l",col="chartreuse4",lwd=3,lty=1)
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=mean(d_k_range)),type="l",col="darkgreen",lwd=2,lty=2)
# D4) a is min, other parameters are mean
	# dev.new()
	plot(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=min(a_range),m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=d_k_range[1]),type="l",col="darkred",lwd=5,main="a is min, other parameters are mean",
		xlab="t_water, C",ylab="Condenser temperature sensitivity coefficient, C/C",panel.first=grid(col="black"),
		sub="Effect of d_k")
	for (i in seq(from=2,along.with=a_range[-1])) {
		lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=min(a_range),m_avrT=mean(m_range),t_ind_range=1:250,
			d_k_range_avrT=d_k_range[i]),type="l",col="red",lwd=2)
	}
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=min(a_range),m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=d_k_range[length(d_k_range)]),type="l",col="darkblue",lwd=5)
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=min(a_range),m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=d_k_range[1]),type="l",col="darkred",lwd=5)
	#
	lines(x=t_range[1:250],y=Sens_avr_byW_byD_k(a_avrD=min(a_range),m_avrD=mean(m_range)),
		type="l",col="chartreuse4",lwd=3,lty=1)
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=min(a_range),m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=mean(d_k_range)),type="l",col="darkgreen",lwd=2,lty=2)
# D5) a is max, other parameters are mean
	# dev.new()
	plot(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=max(a_range),m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=d_k_range[1]),type="l",col="darkred",lwd=5,main="a is max, other parameters are mean",
		xlab="t_water, C",ylab="Condenser temperature sensitivity coefficient, C/C",panel.first=grid(col="black"),
		sub="Effect of d_k")
	for (i in seq(from=2,along.with=a_range[-1])) {
		lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=max(a_range),m_avrT=mean(m_range),t_ind_range=1:250,
			d_k_range_avrT=d_k_range[i]),type="l",col="red",lwd=2)
	}
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=max(a_range),m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=d_k_range[length(d_k_range)]),type="l",col="darkblue",lwd=5)
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=max(a_range),m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=d_k_range[1]),type="l",col="darkred",lwd=5)
	#
	lines(x=t_range[1:250],y=Sens_avr_byW_byD_k(a_avrD=max(a_range),m_avrD=mean(m_range)),
		type="l",col="chartreuse4",lwd=3,lty=1)
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=max(a_range),m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=mean(d_k_range)),type="l",col="darkgreen",lwd=2,lty=2)
#***********************************************************************************************************************
# let's show what is an effect of m_nom on Sensitivity
m_range_param<-seq(from=min(m_range),to=max(m_range),length.out=20)
# D1) All parameters are mean
	# dev.new()
	plot(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=m_range_param[1],t_ind_range=1:250,
		d_k_range_avrT=mean(d_k_range)),type="l",col="darkred",lwd=5,main="All parameters are mean",
		xlab="t_water, C",ylab="Condenser temperature sensitivity coefficient, C/C",panel.first=grid(col="black"),
		sub="Effect of m_nom")
	for (i in seq(from=2,along.with=a_range[-1])) {
		lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=m_range_param[i],t_ind_range=1:250,
			d_k_range_avrT=mean(d_k_range)),type="l",col="red",lwd=1)
	}
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=m_range_param[1],t_ind_range=1:250,
		d_k_range_avrT=mean(d_k_range)),type="l",col="darkblue",lwd=5)
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=m_range_param[length(m_range_param)],
		t_ind_range=1:250,d_k_range_avrT=mean(d_k_range)),type="l",col="darkred",lwd=5)
	lines(x=t_range[1:250],y=Sens_avr_byW_byM(a_avrM=mean(a_range),d_k_avrM=mean(d_k_range)),
		type="l",col="chartreuse4",lwd=3,lty=1)
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=mean(d_k_range)),type="l",col="darkgreen",lwd=2,lty=2)
# D2) D2 is min, al other parameters are mean
	# dev.new()
	plot(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=m_range_param[1],t_ind_range=1:250,
		d_k_range_avrT=min(d_k_range)),type="l",col="darkred",lwd=5,main="D2 is min, al other parameters are mean",
		xlab="t_water, C",ylab="Condenser temperature sensitivity coefficient, C/C",panel.first=grid(col="black"),
		sub="Effect of m_nom")
	for (i in seq(from=2,along.with=a_range[-1])) {
		lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=m_range_param[i],t_ind_range=1:250,
			d_k_range_avrT=min(d_k_range)),type="l",col="red",lwd=1)
	}
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=m_range_param[1],t_ind_range=1:250,
		d_k_range_avrT=min(d_k_range)),type="l",col="darkblue",lwd=5)
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=m_range_param[length(m_range_param)],
		t_ind_range=1:250,d_k_range_avrT=min(d_k_range)),type="l",col="darkred",lwd=5)
	lines(x=t_range[1:250],y=Sens_avr_byW_byM(a_avrM=mean(a_range),d_k_avrM=min(d_k_range)),
		type="l",col="chartreuse4",lwd=3,lty=1)
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=min(d_k_range)),type="l",col="darkgreen",lwd=2,lty=2)
# D3) D2 is max, al other parameters are mean
	# dev.new()
	plot(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=m_range_param[1],t_ind_range=1:250,
		d_k_range_avrT=max(d_k_range)),type="l",col="darkred",lwd=5,main="D2 is max, al other parameters are mean",
		xlab="t_water, C",ylab="Condenser temperature sensitivity coefficient, C/C",panel.first=grid(col="black"),
		sub="Effect of m_nom")
	for (i in seq(from=2,along.with=a_range[-1])) {
		lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=m_range_param[i],t_ind_range=1:250,
			d_k_range_avrT=max(d_k_range)),type="l",col="red",lwd=1)
	}
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=m_range_param[1],t_ind_range=1:250,
		d_k_range_avrT=max(d_k_range)),type="l",col="darkblue",lwd=5)
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=m_range_param[length(m_range_param)],
		t_ind_range=1:250,d_k_range_avrT=max(d_k_range)),type="l",col="darkred",lwd=5)
	lines(x=t_range[1:250],y=Sens_avr_byW_byM(a_avrM=mean(a_range),d_k_avrM=max(d_k_range)),
		type="l",col="chartreuse4",lwd=3,lty=1)
	lines(x=t_range[1:250],y=Sens_avr_byW_alongT(a_avrT=mean(a_range),m_avrT=mean(m_range),t_ind_range=1:250,
		d_k_range_avrT=max(d_k_range)),type="l",col="darkgreen",lwd=2,lty=2)
dev.off()
#***********************************************************************************************************************
