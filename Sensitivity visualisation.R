#
plot_by_W_Sens<-function(a_plot,m_nom_plot,d_k_plot,W_nom_plot,W_plot,t_ind_range=1:205){
	m_clcd<-m_real(m_nom_plot,W_nom_plot,W_plot)
	# t_plot<-seq(from=min(t_range),to=max(t_range),along=W_range)
	t_plot<-t_range[c(min(t_ind_range):max(t_ind_range))]
	plot_palette<-brewer.pal(9,"YlOrRd")
	n_rainbow<-seq(from=3,to=9,along=W_range)
	i_rainbow<-n_rainbow[match(W_plot,W_range)]
	#
	x_text_pos<-t_plot[match(W_plot,W_range)]
	y_text_pos<-Sens(a_plot,m_clcd,d_k_plot,W_nom_plot,W_plot,
		t_plot[match(W_plot,W_range)])
	#
	lines(t_range[c(min(t_ind_range):max(t_ind_range))],Sens(a_plot,m_clcd,d_k_plot,W_nom_plot,W_plot,
		t_range[c(min(t_ind_range):max(t_ind_range))]),type="l",lty=1,lwd=2,col=plot_palette[i_rainbow],
		panel.first=grid(col="black"))
	#	TODO adjust text to be clerly visible
	# text(x=x_text_pos,y=y_text_pos,labels=paste(round(w_range[match(W_plot,W_range)],2),
	# 	" m/s",sep=""),bg="white",cex=0.95)
	return(invisible(1+1))
}
#
main_plot_Sens_Ppr<-function(a_plot,m_nom_plot,d_k_plot,W_nom_plot,W_plot,t_ind_range=1:250){
	sub_text<-paste("a=",round(a_plot,1),", m=",round(m_nom_plot,0),", d_k=",round(d_k_plot,0),
		" g/(s m2), w_border=",round(w_range[match(W_plot,W_range)],1)," m/s" ,sep="")
	# plot(t_range,Sens(a_plot,m_nom_plot,d_k_plot,W_nom=W_range[14],W=W_plot,t_range),type="l",lty=1,lwd=5,
	plot(t_range[c(min(t_ind_range):max(t_ind_range))],Sens(a_plot,m_nom_plot,d_k_plot,W_nom=W_range[14],
		W=W_plot,t_range[c(min(t_ind_range):max(t_ind_range))]),type="l",lty=1,lwd=5,
		col="firebrick4",panel.first=grid(col="black"),main="",xlab="Water temperature, °C",
		ylab="Condenser temperature sensitivity coefficient",sub=sub_text,ylim=c(1,2),
		cex.axis=1.23,cex.lab=1.45)
}
#	picture by the typical values of the parameters
pdf("Fig6.pdf")
	main_plot_Sens_Ppr(mean(a_range),mean(m_range),mean(d_k_range),W_nom_plot=W_range[14],W_plot=W_range[10],t_ind_range=1:250)
	sapply(FUN=plot_by_W_Sens,X=W_range,a_plot=mean(a_range),m_nom_plot=mean(m_range),
		W_nom_plot=W_range[14],d_k_plot=mean(d_k_range),t_ind_range=1:250)
dev.off()	
#