#=================================================================================================
# Construction of the sensitivity function of the temperature in the steam turbine condenser 
# to the temperature of the cooling water. The The model was based on a classical VTI approach 
# developed as a generalization of a comprehensive amount of experimental data obtained for steam condensation 
# processes with the parameters corresponding to the industry-scale power units.
#
# References:
# 1. Berman, LD. Heat transfer with steam condensation on a bundle of horizontal tubes. 
# Thermal Engineering 1981;28; 218-224.
# 2. Berman LD, Zernova EP. Directions to calculation of the surface condensers of large steam turbines
# of thermal and nuclear power plants. Moscow: Soyuztechenergo; 1982. [in Russian]. 
#=================================================================================================
rm(list=ls())
library("RColorBrewer")
#	physical properties
#		* heat capacity of water
# [Rivkin, Aleksandrov; 1984], p=0.1 MPa
c_water_to_interp<-c(4.217,4.192,4.182,4.178,4.179,4.181)
t_to_interp<-seq(from=0,to=50,by=10)
c_water<-approxfun(x=t_to_interp,y=c_water_to_interp,rule=1)
		#	graphical check
		#plot(x=0:50,y=c_water(0:50),type="p",col="red",panel.first=grid(col="gray"))
		#points(x=t_to_interp,y=c_water_to_interp,pch=16,col="darkred")
#-----------------------------------
#	condenser parameters
#-----------------------------------
#	increase of the cooling water temperature in the condenser
#	typical values for the two-pass design of the condenser
#	source of the values [Heat transfer equipment of the power units, ed. Brodov; 2010]
delta_t<-8.5
delta_t_test<-10
#	the steam to water ratio
#		NB m depends on the velocity;
#		the value m_range is just the values range
#		corresponding to the nominal velocity
m_range<-seq(from=50,to=70,by=10)
# the specific steam load
d_k_range<-seq(from=5,to=20,length.out=20)
#	a is a coefficient accounting for the surface parameters of the condencer tubes:
#	the clealiness and the material/thickness of the tubes wall
#		(0.75-0.9)					(0.85-0.99)		
# 	source of values: [Berman LD, Zernova EP. Directions to calculation of the surface condensers 
# 	of large steam turbines of thermal and nuclear power plants. Moscow: Soyuztechenergo; 1982]
a_min<-min(seq(from=0.75,to=0.9,length.out=20)*seq(from=0.85,to=0.99,length.out=20))
a_max<-max(seq(from=0.75,to=0.9,length.out=20)*seq(from=0.85,to=0.99,length.out=20))
a_range=seq(from=a_min,to=a_max,length.out=20)
# the velocity of the cooling water inside the condenser tubes
# 	source of values: [Berman LD, Zernova EP. Directions to calculation...: 1982]
w_range<-seq(from=1,to=2.5,length.out=20)
# the outer diameter of the condenser tubes
# source of values: [Brodov Yu.M, Savel’ev RZ. Condensing units of the steam turbines. Moscow: Energoatomizdat; 1994]
d_outer<-seq(from=16,to=30,length.out=20)
# the nondimentional parameter used in the heat transfer calculations
W_min<-min((1.1*w_range)/(d_outer^0.25))
W_max<-max((1.1*w_range)/(d_outer^0.25))
W_range=seq(from=W_min,to=W_max,length.out=20)
#	the tempearture of the cooling water
#		source of values: [Berman LD, Zernova EP. Directions to calculation...: 1982]
#	NB the experimental data used to derive the calculation method
#	included the water temperature values up to 45C only
t_range=seq(from=0,to=45,by=0.1)
#-----------------------------------
#	calculation method
#	Heat transfer coefficient is factorised in the method
#	Ft and Fw represent a temperature-sensitive part
#	Fz is 1 for the most popular two-pass condenser design (z=2)
#-----------------------------------
#	supplementary value (b*a^0.5)/1000
B<-function(a,d_k){
	((0.52-0.0072*d_k)*sqrt(a))/1e3
}
#	influence of the temperature of the cooling water
Ft<-function(a,d_k,t){
	# !!! the sighn of 2e-3 was corrected form "-" to "+"
	ifelse( (t<35),(1-B(a,d_k)*(35-t)^2),(1+2e-3*(t-35)))
}
#	influence of the velocity of the cooling water
Fw<-function(a,W,t){
	ifelse( (t<27),(W^(0.12*a*(1+0.15*t))),(W^(0.6*a)))
}
#	the product of Ft and Fw
FtFw<-function(a,d_k,W,t){
	Ft(a,d_k,t)*Fw(a,W,t)
}
#--------------------------------------------------------------------------------
#	the governing equation is delta_t=Delta_t/(exp(n)-1)
#	n is linearly connected with the heat transfer coefficient
#	our task is to find d(delta_t)/d(t_water)
#	Delta_t determines the enthalpy change used by the turbine
#	and may be assumed to be constant; change of the heat transfer processes
#	between the condensing steam and the cooling water impacts n as well
#	the temperature and the pressure in the condenser
#--------------------------------------------------------------------------------
#	4070 [W/(m^2*K)] is the empirical coefficient found as a result of generalization of the experiment data
#	4.19 [kJ/(kg*K)] is the water heat capacity
#	the heat capacity of water is changing between 0C and 50C less than on 1%
#	TO DO: however, it would be better to approximate dependence c_p(t_water)
#			the problem is N0 stands under exp...->c_water(t) is constructed
N0_func<-function(a,m,d_k,W,t){
	(a*4070*FtFw(a,d_k,W,t))/(c_water(t)*m*d_k)
}
#	(1/Ft)*(dFt/dt)
#	TO DO: a temperature condition should be added
ft<-function(a,d_k,t){
	#(2*B(a,d_k)*(35-t))/(1-B(a,d_k)*(35-t)^2)
	#ifelse(t>35,(2*B(a,d_k)*(35-t))/(1-B(a,d_k)*(35-t)^2),0)
	ifelse(t<35,(2*B(a,d_k)*(35-t))/(1-B(a,d_k)*(35-t)^2),(2e-3/(1+2e-3*(t-35))))
}
#	(1/Fw)*(dFw/dt)
#	TO DO: a temperature condition should be added
fw<-function(a,W,t){
	#1.8e-2*t*log(W)
	# actually, it should be a condition
	# but it results in quite unphysical behaiviour
	# TO DO: what is the physiacl sence of such a jumping?
	ifelse(t<26.7,(1.8e-2*a)*log(W),0)
}
#	a supplementary function for calculation of d(delta_t)/d(t_water) 
f_exp_verschacht<-function(a,m,d_k,W,t){
((Ft(a,d_k,t)*Fw(a,W,t)*N0_func(a,m,d_k,W,t))*exp(Ft(a,d_k,t)*Fw(a,W,t)*N0_func(a,m,d_k,W,t)))/((exp(Ft(a,d_k,t)*Fw(a,W,t)*N0_func(a,m,d_k,W,t))-1)^2)
}
#	a supplementary function A* for calculation of d(delta_t)/d(t_water)	
A_<-function(a,m,d_k,W,t){
	f_exp_verschacht(a,m,d_k,W,t)*(ft(a,d_k,t)+fw(a,W,t))
}
#	m value itself depends on the velocity value
m_real<-function(m_nom,W_nom,W) {
	m_nom*(W/W_nom)
}
# the result: the function representing the sensitivity of the condenser temperature
# 	to the water temperature
Sens<-function(a,m_nom,d_k,W_nom,W,t){
	m_clcd<-m_real(m_nom,W_nom,W)
	return(1+delta_t*A_(a,m_clcd,d_k,W,t))
}
#