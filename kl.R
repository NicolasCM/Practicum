# Simulación ar

gen_ar <- function(observaciones, varianza, ...) {
	thetas <- unlist(list(...))
	n <- length(thetas)
	z<-rep(0, times=n)
	e<-rnorm(observaciones-n,0,varianza)
	for (i in 1:(observaciones-n)) {
		# print(length(z[i:(n+i-1)]))
		z<- c(z,sum(z[i:(n+i-1)]*thetas)+e[i])
		# print(z[n+i])
		# print(e[i])
	}
	return(z)
} 

# Similar am
gen_ma <- function(observaciones, varianza,...){
	phis <- unlist(list(...))
	n <- length(phis)
	e <- c(rep(0,times=n),rnorm(observaciones,0,varianza))
	z<-c()
	for (i in 1:observaciones) {
		z <- c(z,sum(e[i:(n+i-1)]*phis)+e[n+i])
	}
	return(z)
} 

# Proceso raro 
gen_proceso_1 <- function(observaciones, varianza,theta1, theta2, theta12){
	z<-rep(0, times=2)
	e<-rnorm(observaciones-2,0,varianza)
	for (i in 1:(observaciones-2)) {
		z_aux<-theta1*z[i+1]+theta2*z[i]+theta12*z[i]*z[i+1]+e[i]
		z<-c(z,z_aux)
	}
	# print(e)
	return(z)
}


funcion_autocovarianza <- function(serie) {
	n <- length(serie)
	media_serie <- mean(serie)
	# print(media_serie)
	r<-c()
	for (i in 0:(n-1)) {
		serie_izq <- serie[1:(n-i)]-media_serie
		serie_der <- serie[(1+i):n]-media_serie
		# print(length(serie_izq))
		# print(length(serie_der))
		r_aux <- sum(serie_izq*serie_der)/n
		r<- c(r, r_aux)
	}
	return(r)
}

funcion_autocorrelacion <- function(serie) {
	n <- length(serie)
	media_serie <- mean(serie)
	# print(media_serie)
	r<-c()
	for (i in 0:(n-1)) {
		serie_izq <- serie[1:(n-i)]-media_serie
		serie_der <- serie[(1+i):n]-media_serie
		# print(length(serie_izq))
		# print(length(serie_der))
		r_aux <- sum(serie_izq*serie_der)/n
		r<- c(r, r_aux)
	}
	r_aux<-r[1]
	return(r/r_aux)
}

funcion_autocorrelacion_doble <- function(serie) {
	n <- length(serie)
	media_serie <- mean(serie)
	# print(media_serie)
	r<-c()
	for (i in 0:(n-1)) {
		serie_izq <- serie[1:(n-i)]-media_serie
		serie_der <- serie[(1+i):n]-media_serie
		# print(length(serie_izq))
		# print(length(serie_der))
		r_aux <- sum(serie_izq*serie_der)/n
		r<- c(r, r_aux)
	}
	r_aux<-r[1]
	r<- c(r[n:2],r)
	return(r/r_aux)
}





funcion_d<- function(serie) {
	n <- length(serie)
	d <- c()
	for (i in 0:(n-1)) {
	frecuencia <- 2*i*pi/n
	exponentes <- exp((1:n)*complex(real=0,imaginary=-i*frecuencia))
	d<- c(d, sum(serie*exponentes))
	}
	return(d)
}
periodograma<- function(serie) {
	n <- length(serie)
	d <- c()
	for (i in 0:(n-1)) {
	frecuencia <- 2*i*pi/n
	exponentes <- exp((1:n)*complex(real=0,imaginary=-i*frecuencia))
	d<- c(d, sum(serie*exponentes))
	}
	d<-(abs(d)^2)/(2*pi*n)
	d<-
	return(d)
}

periodograma_priestley<- function(serie) {
	n <- length(serie)
	d <- c()
	for (i in 0:(n-1)) {
	frecuencia <- 2*i*pi/n
	exponentes <- exp((1:n)*complex(real=0,imaginary=-i*frecuencia))
	d<- c(d, sum(serie*exponentes))
	}
	d<-d/(2*pi)
	return(d)
}

funcion_densidad_espectral_periodograma_priestley<- function(serie) {
	n <- length(serie)
	# fun_autocor<- funcion_autocorrelacion(serie)
	fun_autocor<- funcion_autocorrelacion_doble(serie)
	factor_aux <-2*pi
	d <- c()
	for (i in 0:(n-1)) {
	frecuencia <- 2*i*pi/n
	exponentes <- exp(complex(real=0,imaginary=i*frecuencia*((1-n):(n-1))))
	d<- c(d, sum(fun_autocor*exponentes))
	}
	# factor <-2*pi*d[1]
	d<-d/factor_aux
	return(d)
}


kl_divergence<-function(serie1, serie2) {
	kl_div <- 0
	n <- length(serie1)
	for (i in 1:n) {
		if(serie2[i]==0 && serie1[i]==0) next
		aux <- serie1[i]*log(serie1[i]/serie2[i]) 
		kl_div<- kl_div+aux
	}
	return(kl_div)
}

j_divergence<-function(serie1, serie2) {
	j_div <- 0
	n <- length(serie1)
	for (i in 1:n) {
		if(serie2[i]==0 || serie1[i]==0) next
		aux <- (serie1[i]-serie2[i])*log(serie1[i]/serie2[i]) 
		j_div<- j_div+aux
	}
	return(j_div)
}

proceso_11<-gen_proceso_1(1002, 1, .9, .9, .05)
# plot(proceso_11[5:1000], type="l")
# print(proceso_11)


ma1<-gen_ma(1000,1,.5,.5,.5,.5)
ma2<-gen_ma(1000,1,.2,.5,.1,.2)
ma3<-gen_ma(1000,1,.5,.1,.5,.1)
ma4<-gen_ma(1000,1,.2,.5,.2)


ar1 <- gen_ar(1000,1,.1, .5, -.1)
ar2 <- gen_ar(1000,1,.5, -.1)
ar3 <- gen_ar(1000,1,.5, .5, -.1,-.2)
ar4 <- gen_ar(1000,1,.5, .5, .5, .5)



fac_ar1<- funcion_autocorrelacion(ar1)
fac_ma1<- funcion_autocorrelacion(ma1)
fac_ar2<- funcion_autocorrelacion(ar2)
fac_ma2<- funcion_autocorrelacion(ma2)
fac_ar3<- funcion_autocorrelacion(ar3)
fac_ma3<- funcion_autocorrelacion(ma3)
fac_ar4<- funcion_autocorrelacion(ar4)
fac_ma4<- funcion_autocorrelacion(ma4)



periodograma_ar1 <- periodograma(fac_ar1)
periodograma_ma1 <- periodograma(fac_ma1)
periodograma_ar2 <- periodograma(fac_ar2)
periodograma_ma2 <- periodograma(fac_ma2)
periodograma_ar3 <- periodograma(fac_ar3)
periodograma_ma3 <- periodograma(fac_ma3)
periodograma_ar4 <- periodograma(fac_ar3)
periodograma_ma4 <- periodograma(fac_ma3)

periodograma_ar1 <- periodograma(ar1)
periodograma_ma1 <- periodograma(ma1)
periodograma_ar2 <- periodograma(ar2)
periodograma_ma2 <- periodograma(ma2)
periodograma_ar3 <- periodograma(ar3)
periodograma_ma3 <- periodograma(ma3)
periodograma_ar4 <- periodograma(ar3)
periodograma_ma4 <- periodograma(ma3)

periodogramas <- list (periodograma_ar1,periodograma_ar2, periodograma_ar3, periodograma_ar4, periodograma_ma1,periodograma_ma2, periodograma_ma3, periodograma_ma4)

# j_divergence(periodograma_ar1, periodograma_ma1)
# j_divergence(periodograma_ar2, periodograma_ma2)
# j_divergence(periodograma_ar3, periodograma_ma3)

# j_divergence(periodograma_ar1, periodograma_ar2)
# j_divergence(periodograma_ar1, periodograma_ar3)
# j_divergence(periodograma_ar2, periodograma_ar3)

# j_divergence(periodograma_ma1, periodograma_ma2)
# j_divergence(periodograma_ma1, periodograma_ma3)
# j_divergence(periodograma_ma2, periodograma_ma3)


str(periodogramas)
distancias<-function(periodogramas) {
	# str(periodogramas)
	n<- length(periodogramas)
	# str(unlist(periodogramas[1]))
	distancias<- matrix(data=0.0, nrow = n, ncol = n)
	for (i in 1:(n-1)) {
		for (j in (i+1):n) {
			# distancias[i,j] <- j_divergence(periodogramas[i], periodogramas[j])
			distancias[i,j]<-j_divergence(unlist(periodogramas[i]), unlist(periodogramas[j]))
		}
	}
	return(t(distancias))
}

distanc <- as.dist(distancias(periodogramas))
print(distanc)

conglomerado <-hclust(distanc)
str(conglomerado)
 # dendrogram(conglomerado)
plot(conglomerado)
# plot(periodograma_fac, type="l")

gen_mulp_ar <- function(n, observaciones, varianza, ...) {
	ars<- matrix(NA, nrow = n, ncol = observaciones)
	for (i in 1:n) {
		ars[i,]<- gen_ar(observaciones, varianza, ...)
	}
	return(ars)
}
# clase_ar<- gen_mulp_ar(20, 1000,1,.1, .5, -.1)

gen_mulp_ma <- function(n, observaciones, varianza, ...) {
	mas<- matrix(NA, nrow = n, ncol = observaciones)
	for (i in 1:n) {
		mas[i,]<- gen_ma(observaciones, varianza, ...)
	}
	return(mas)
}

gen_mulp_proceso1 <- function(n, observaciones, varianza, ...) {
	proceso1s<- matrix(NA, nrow = n, ncol = observaciones)
	for (i in 1:n) {
		proceso1s[i,]<- gen_proceso1(observaciones, varianza, ...)
	}
	return(proceso1s)
}


# str(t(as.data.frame(clase_ar)))
promedio_espectro <- function(clase) {
	return(colMeans(clase))
}

clase_ar<- gen_mulp_ar(100, 1000,1,.1, .5, -.1, .1, .1)
espectro_clase_ar<- t(apply(clase_ar, 1, funcion_densidad_espectral_periodograma_priestley))
# str(clase_ar)
# str(espectro_clase_ar)
# funcion_densidad_espectral_periodograma_priestley(clase_ar[1,])
espectro_prom_ar<-promedio_espectro(espectro_clase_ar)
# plot(espectro_ma)


clase_ma<-gen_mulp_ma(100, 1000,1,.1, .5, -.1, -.2)
espectro_clase_ma<- t(apply(clase_ma, 1, funcion_densidad_espectral_periodograma_priestley))
espectro_prom_ma<-promedio_espectro(espectro_clase_ma)
# abs(espectro_prom_ma)
# kl_divergence(abs(espectro_prom_ma), abs(espectro_clase_ma[2,]))
# kl_divergence(abs(espectro_prom_ar), abs(espectro_clase_ma[2,]))
# kl_divergence(abs(espectro_prom_ma), abs(espectro_clase_ma[3,]))
# kl_divergence(abs(espectro_prom_ar), abs(espectro_clase_ma[3,]))
# kl_divergence(abs(espectro_prom_ma), abs(espectro_clase_ma[4,]))
# kl_divergence(abs(espectro_prom_ar), abs(espectro_clase_ma[5,]))
# kl_divergence(abs(espectro_prom_ma), abs(espectro_clase_ma[6,]))
# kl_divergence(abs(espectro_prom_ar), abs(espectro_clase_ma[6,]))
# kl_divergence(abs(espectro_prom_ma), abs(espectro_clase_ma[7,]))
# kl_divergence(abs(espectro_prom_ar), abs(espectro_clase_ma[7,]))

entrenamiento<- rbind(espectro_clase_ar, espectro_clase_ma)

clasificar<- function(series, espectro_1, espectro_2) {
	n<- nrow(series)
	clase_asignanda<-numeric(n)
	for (i in 1:n) {
		if(kl_divergence(abs(espectro_1),abs(series[i,]))<=kl_divergence(abs(espectro_2),abs(series[i,]))){
			clase_asignanda[i]<-1
		}else{
			clase_asignanda[i]<-2
		}
	}
	return(clase_asignanda)
}
clasi <- data.frame(clasificar(entrenamiento, espectro_prom_ar, espectro_prom_ma), c(rep(1, 100), rep(2, 100)))
# clasi
# clasificar(entrenamiento, espectro_prom_ar, espectro_prom_ma)
confusion<- matrix(0,nrow = 2, ncol = 2)
for (i in 1:200) {
	confusion[clasi[i, 2], clasi[i, 1]]<-confusion[clasi[i, 2], clasi[i, 1]]+1
}
confusion




