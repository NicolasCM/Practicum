
library(seewave)
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
	print("densidad")
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

# proceso_11<-gen_proceso_1(1002, 1, .9, .9, .05)
# # plot(proceso_11[5:1000], type="l")
# # print(proceso_11)


# ma1<-gen_ma(1000,1,.5,.5,.5,.5)
# ma2<-gen_ma(1000,1,.2,.5,.1,.2)
# ma3<-gen_ma(1000,1,.5,.1,.5,.1)
# ma4<-gen_ma(1000,1,.2,.5,.2)


# ar1 <- gen_ar(1000,1,.1, .5, -.1)
# ar2 <- gen_ar(1000,1,.5, -.1)
# ar3 <- gen_ar(1000,1,.5, .5, -.1,-.2)
# ar4 <- gen_ar(1000,1,.5, .5, .5, .5)



# fac_ar1<- funcion_autocorrelacion(ar1)
# fac_ma1<- funcion_autocorrelacion(ma1)
# fac_ar2<- funcion_autocorrelacion(ar2)
# fac_ma2<- funcion_autocorrelacion(ma2)
# fac_ar3<- funcion_autocorrelacion(ar3)
# fac_ma3<- funcion_autocorrelacion(ma3)
# fac_ar4<- funcion_autocorrelacion(ar4)
# fac_ma4<- funcion_autocorrelacion(ma4)



# periodograma_ar1 <- periodograma(fac_ar1)
# periodograma_ma1 <- periodograma(fac_ma1)
# periodograma_ar2 <- periodograma(fac_ar2)
# periodograma_ma2 <- periodograma(fac_ma2)
# periodograma_ar3 <- periodograma(fac_ar3)
# periodograma_ma3 <- periodograma(fac_ma3)
# periodograma_ar4 <- periodograma(fac_ar3)
# periodograma_ma4 <- periodograma(fac_ma3)

# periodograma_ar1 <- periodograma(ar1)
# periodograma_ma1 <- periodograma(ma1)
# periodograma_ar2 <- periodograma(ar2)
# periodograma_ma2 <- periodograma(ma2)
# periodograma_ar3 <- periodograma(ar3)
# periodograma_ma3 <- periodograma(ma3)
# periodograma_ar4 <- periodograma(ar3)
# periodograma_ma4 <- periodograma(ma3)

# periodogramas <- list (periodograma_ar1,periodograma_ar2, periodograma_ar3, periodograma_ar4, periodograma_ma1,periodograma_ma2, periodograma_ma3, periodograma_ma4)

# j_divergence(periodograma_ar1, periodograma_ma1)
# j_divergence(periodograma_ar2, periodograma_ma2)
# j_divergence(periodograma_ar3, periodograma_ma3)

# j_divergence(periodograma_ar1, periodograma_ar2)
# j_divergence(periodograma_ar1, periodograma_ar3)
# j_divergence(periodograma_ar2, periodograma_ar3)

# j_divergence(periodograma_ma1, periodograma_ma2)
# j_divergence(periodograma_ma1, periodograma_ma3)
# j_divergence(periodograma_ma2, periodograma_ma3)


# str(periodogramas)


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


distancias_series<-function(series) {
	# str(periodogramas)
	# espectros<-espectro_clase(series[classes==un_classes[1],])
	periodogramas<-espectro_clase(series)
	n<- nrow(series)
	# str(unlist(periodogramas[1]))
	distancias<- matrix(data=0.0, nrow = n, ncol = n)
	for (i in 1:(n-1)) {
		for (j in (i+1):n) {
			# distancias[i,j] <- j_divergence(periodogramas[i], periodogramas[j])
			distancias[i,j]<-j_divergence(abs(periodogramas[i,]), abs(periodogramas[j,]))
		}
	}
	return(t(distancias))
}



# distanc <- as.dist(distancias(periodogramas))
# print(distanc)

# conglomerado <-hclust(distanc)
# str(conglomerado)
#  # dendrogram(conglomerado)
# plot(conglomerado)
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



# clase_ar<- gen_mulp_ar(100, 1000,1,.1, .5, -.1, .1, .1)
# espectro_clase_ar<- t(apply(clase_ar, 1, funcion_densidad_espectral_periodograma_priestley))

# espectro_prom_ar<-promedio_espectro(espectro_clase_ar)



# clase_ma<-gen_mulp_ma(100, 1000,1,.1, .5, -.1, -.2)
# espectro_clase_ma<- t(apply(clase_ma, 1, funcion_densidad_espectral_periodograma_priestley))
# espectro_prom_ma<-promedio_espectro(espectro_clase_ma)

# entrenamiento<- rbind(espectro_clase_ar, espectro_clase_ma)

clasificar<- function(espectro_series, espectro_1, espectro_2) {
	n<- nrow(espectro_series)
	clase_asignanda<-numeric(n)
	for (i in 1:n) {
		if(kl_divergence(abs(espectro_1),abs(espectro_series[i,]))<=kl_divergence(abs(espectro_2),abs(espectro_series[i,]))){
			clase_asignanda[i]<-1
		}else{
			clase_asignanda[i]<-2
		}
	}
	return(clase_asignanda)
}


# clasificacion<- clasificar(entrenamiento, espectro_prom_ar, espectro_prom_ma)
# clasi <- data.frame(clasificacion, c(rep(1, 100), rep(2, 100)))
# confusion<- matrix(0,nrow = 2, ncol = 2)
# for (i in 1:200) {
# 	confusion[clasi[i, 2], clasi[i, 1]]<-confusion[clasi[i, 2], clasi[i, 1]]+1
# }
# confusion

gen_clase_ar_al<-function(max.par=1, n.series=1,n.observaciones=1, varianza=1) {
	ars<- matrix(NA, nrow = n.series, ncol = n.observaciones)
	n.par<- floor(runif(n.series, min=1, max=max.par+1))
	for (i in 1:n.series) {
		ars[i,]<- gen_ar(n.observaciones, varianza, runif(n.par[i], -.3, .3))
	}
	return(ars)
}

gen_clase_ma_al<-function(max.par=1, n.series=1,n.observaciones=1, varianza=1) {
	ars<- matrix(NA, nrow = n.series, ncol = n.observaciones)
	n.par<- floor(runif(n.series, min=1, max=max.par+1))
	# print(n.par)
	for (i in 1:n.series) {
		ars[i,]<- gen_ma(n.observaciones, varianza, runif(n.par[i], -.3, .3))
	}
	return(ars)
}

espectro_clase<- function(clase) {
espectro_clase<- t(apply(clase, 1, funcion_densidad_espectral_periodograma_priestley))
return(espectro_clase)
}
# clase_ar_genera<- gen_clase_ar_al(5, 100, 100)
# clase_ma_genera<- gen_clase_ma_al(5, 100, 100)
# espectro_ar<-espectro_clase(clase_ar_genera)
# espectro_ma<-espectro_clase(clase_ma_genera)
# espectro_prom_ma<-colMeans(espectro_ma)
# espectro_prom_ar<-colMeans(espectro_ar)
# entrenamiento<-rbind(espectro_ar, espectro_ma)
# clasificacion<- clasificar(entrenamiento, espectro_prom_ar, espectro_prom_ma)
# clasi <- data.frame(clasificacion, c(rep(1, 100), rep(2, 100)))
# 	confusion<- matrix(0,nrow = 2, ncol = 2)
# for (i in 1:200) {
# 	confusion[clasi[i, 2], clasi[i, 1]]<-confusion[clasi[i, 2], clasi[i, 1]]+1
# }
# confusion






# The classification algorithm
# It needs a series 

clasify<-function(series, centers, classes, is.spectrum=F) {
	if(is.spectrum){
		spectrum<-series
	}else{
		spectrum<-spectro_clase(series)
	}
	n<-nrow(spectrum)
	# print(length(classes))
	class <- numeric(n)
	ci<-0
	for (k in 1:n) {
		aux<-spectrum[k,]
		i<-1
		while(i<length(classes)){
			for (j in (i+1):length(classes)) {
				ci<-j
				# print(j)
				if(kl_divergence(abs(centers[i,]),abs(aux))>kl_divergence(abs(centers[j,]), abs(aux))){
					i<-ci
					break
				}
			}
			# print(ci)
			class[k]<-classes[i]
			if(ci==length(classes)) break
		}
		# for (i in 1:(length(classes)-1)) {
			
		# }
	}
	return(class)
}
confusion_matrix<- function(real_class, assigned_class) {
	un_classes<- unique(real_class)
	n<- length(un_classes)
	n2<- length(real_class)
	confusion_m<-matrix(0, nrow=n, ncol=n)
	real_index<-match(real_class,un_classes)
	assigned_index<-match(assigned_class,un_classes)
	# print("real")
	# print(real_index)
	# print("assign")
	# print(assigned_index)
	for (i in 1:n2) {
		confusion_m[real_index[i], assigned_index[i]]<-confusion_m[real_index[i], assigned_index[i]]+1
		# print(confusion_m[real_index[i], assigned_index[i]])
	}
	confusion_m<-as.data.frame(confusion_m)
	colnames(confusion_m)<-un_classes
	rownames(confusion_m)<-un_classes
	return(confusion_m)
}

classification_problem<-function(series, classes, series_names=NA) {
	n<-nrow(series)
	un_classes<- unique(classes)
	espectros<-espectro_clase(series[classes==un_classes[1],])
	centers <- colMeans(espectros)
	for (cla in un_classes[-1]) {
		aux<-espectro_clase(series[classes==cla,])
		espectros<- rbind(espectros, aux)
		centers<- rbind(centers, colMeans(aux))
	}
	new_classes<- clasify(espectros, centers, un_classes, T)
	confusion_m<- confusion_matrix(classes, new_classes)
	return(list(spectrums=espectros, centers=centers, new_class=new_classes, confusion_mat=confusion_m))
}

classification_problem2<-function(series, classes, series_names=NA) {
	n<-nrow(series)
	un_classes<- unique(classes)
	espectros<-espectro_clase2(series[classes==un_classes[1],])
	centers <- colMeans(espectros)
	for (cla in un_classes[-1]) {
		aux<-espectro_clase2(series[classes==cla,])
		espectros<- rbind(espectros, aux)
		centers<- rbind(centers, colMeans(aux))
	}
	new_classes<- clasify(espectros, centers, un_classes, T)
	confusion_m<- confusion_matrix(classes, new_classes)
	return(list(spectrums=espectros, centers=centers, new_class=new_classes, confusion_mat=confusion_m))
}


espectro_clase2<- function(series) {
	n<- nrow(series)
	espectros<-t(data.frame(spectrum(series[1,])$spec), )
	for (i in 2:n) {
		espectros<-rbind(espectros, t(spectrum(series[i,])$spec))
	}
	return (espectros)
}

espectro_clase3<- function(series, fre=1) {
	n<- length(names(series))
	espectros_aux<- list()
	frecs<-numeric(n)
	# espectros_aux[[1]]<-
	for (i in 1:n) {
		espectros_aux[[i]]<-spec(series[[i]], fre, PSD = TRUE)
		frecs[i]<-espectros_aux[[i]][length(espectros_aux[[i]][,2]),2]
		if(anyNA(espectros_aux[[i]])){
			print(espectros_aux[[i]])
		}
	}
	# min_frec<- min(frecs)
	# for (i in 1:n) {
	# 	espectros_aux[[i]]<-cutspec(espectros_aux[[i]],fre, flim=c(0, min_frec))
	# }

	espectros<-t(data.frame(espectros_aux[[1]][,2]))
	for (i in 2:n) {
		espectros<-rbind(espectros, t(espectros_aux[[i]][,2]))
	}
	return (espectros)
	# spectros<- list()
	# for(i in names(listaseries)){
	# 	spectros[[i]]<-spec(listaseries[[i]])
	# }
	# return(spectros)
}

clasify3<-function(series, centers, classes, is.spectrum=F) {
	if(is.spectrum){
		spectrum<-series
	}else{
		spectrum<-spectro_clase(series)
	}
	n<-nrow(spectrum)
	# print(length(classes))
	class <- numeric(n)
	ci<-0
	for (k in 1:n) {
		aux<-spectrum[k,]
		i<-1
		while(i<length(classes)){
			for (j in (i+1):length(classes)) {
				ci<-j
				# print(j)
				if(kl.dist((centers[i,]),(aux))$D1>kl.dist((centers[j,]), (aux))$D1){
					i<-ci
					break
				}
			}
			# print(ci)
			class[k]<-classes[i]
			if(ci==length(classes)) break
		}
		# for (i in 1:(length(classes)-1)) {
			
		# }
	}
	return(class)
}

clasify4<-function(series, centers, classes, is.spectrum=F) {
	if(is.spectrum){
		spectrum<-series
	}else{
		spectrum<-spectro_clase(series)
	}
	n<-nrow(spectrum)
	# print(length(classes))
	class <- numeric(n)
	ci<-0
	for (k in 1:n) {
		aux<-spectrum[k,]
		i<-1
		while(i<length(classes)){
			for (j in (i+1):length(classes)) {
				ci<-j
				# print(j)
				if(kl.dist((centers[i,]),(aux))$D2>kl.dist((centers[j,]), (aux))$D2){
					i<-ci
					break
				}
			}
			# print(ci)
			class[k]<-classes[i]
			if(ci==length(classes)) break
		}
		# for (i in 1:(length(classes)-1)) {
			
		# }
	}
	return(class)
}

clasify5<-function(series, centers, classes, is.spectrum=F) {
	if(is.spectrum){
		spectrum<-series
	}else{
		spectrum<-spectro_clase(series)
	}
	n<-nrow(spectrum)
	# print(length(classes))
	class <- numeric(n)
	ci<-0
	for (k in 1:n) {
		aux<-spectrum[k,]
		i<-1
		while(i<length(classes)){
			for (j in (i+1):length(classes)) {
				ci<-j
				# print(j)
				if(kl.dist((centers[i,]),(aux))$D>kl.dist((centers[j,]), (aux))$D){
					i<-ci
					break
				}
			}
			# print(ci)
			class[k]<-classes[i]
			if(ci==length(classes)) break
		}
		# for (i in 1:(length(classes)-1)) {
			
		# }
	}
	return(class)
}

classification_problem3<-function(series, classes, series_names=NA, fre=1) {
	n<-nrow(series)
	un_classes<- unique(classes)
	espectros<-espectro_clase3(series[classes==un_classes[1]],fre)
	centers <- colMeans(espectros)
	for (cla in un_classes[-1]) {
		aux<-espectro_clase3(series[classes==cla])
		espectros<- rbind(espectros, aux)
		centers<- rbind(centers, colMeans(aux))
	}
	new_classes<- clasify3(espectros, centers, un_classes, T)
	confusion_m<- confusion_matrix(classes, new_classes)
	return(list(spectrums=espectros, centers=centers, new_class=new_classes, confusion_mat=confusion_m))
}


# Problema de clasificacion donde recortamos las series y obtenemos el espectro promedio
espectro_clase_promedio<- function(series, duraciones, porcentajes_over, fre=1) {
	n<- length(names(series))
	espectros_aux<- list()
	frecs<-numeric(n)
	print("ok")
	for (i in 1:n) {
		espectros_aux[[i]]<-meanspec(series[[i]], fre, PSD = TRUE, to=duraciones[i], ovlp=porcentajes_over[i])
		frecs[i]<-espectros_aux[[i]][length(espectros_aux[[i]][,2]),2]
		if(anyNA(espectros_aux[[i]])){
			print(espectros_aux[[i]])
		}
	}
	espectros<-t(data.frame(espectros_aux[[1]][,2]))
	for (i in 2:n) {
		espectros<-rbind(espectros, t(espectros_aux[[i]][,2]))
	}
	return (espectros)
}


classification_problem_promedio<-function(series, classes, duraciones, series_names=NA, fre=1) {
	duracion_min<- min(duraciones)
	porcentajes<-duracion_min/duraciones
	n<-nrow(series)
	un_classes<- unique(classes)
	indicador <-classes==un_classes[1]
	espectros<-espectro_clase_promedio(series[indicador], duraciones[indicador], porcentajes[indicador],fre)
	centers <- colMeans(espectros)
	for (cla in un_classes[-1]) {
		indicador<-classes==cla
		aux<-espectro_clase_promedio(series[indicador], duraciones[indicador], porcentajes[indicador],fre)
		espectros<- rbind(espectros, aux)
		centers<- rbind(centers, colMeans(aux))
	}
	new_classes<- clasify3(espectros, centers, un_classes, T)
	confusion_m<- confusion_matrix(classes, new_classes)
	return(list(spectrums=espectros, centers=centers, new_class=new_classes, confusion_mat=confusion_m))
}

clasify3_umbral<-function(series, centers, classes, umbral=0, is.spectrum=F) {
	if(is.spectrum){
		spectrum<-series
	}else{
		spectrum<-spectro_clase(series)
	}
	n<-nrow(spectrum)
	# print(length(classes))
	class <- numeric(n)
	ci<-0
	for (k in 1:n) {
		aux<-spectrum[k,]
		i<-1
		while(i<length(classes)){
			for (j in (i+1):length(classes)) {
				ci<-j
				# print(j)
				if(kl.dist((centers[i,]),(aux))$D1+umbral>kl.dist((centers[j,]), (aux))$D1){
					i<-ci
					break
				}
			}
			# print(ci)
			class[k]<-classes[i]
			if(ci==length(classes)) break
		}
		# for (i in 1:(length(classes)-1)) {
			
		# }
	}
	return(class)
}





distancias_series<-function(series, duraciones, fre=1) {
	duracion_min<- min(duraciones)
	porcentajes<-duracion_min/duraciones
	n<-length(series)
	periodogramas<-espectro_clase_promedio(series, duraciones, porcentajes, fre)
	
	# str(unlist(periodogramas[1]))
	distancias<- matrix(data=0.0, nrow = n, ncol = n)
	for (i in 1:(n-1)) {
		for (j in (i+1):n) {
			# distancias[i,j] <- j_divergence(periodogramas[i], periodogramas[j])
			# distancias[i,j]<-j_divergence(abs(periodogramas[i,]), abs(periodogramas[j,]))
			# print("ok2")
			distancias[i,j]<-kl.dist(periodogramas[i,],periodogramas[j,])$D
		}
	}
	return(t(distancias))
}


# kmeans
within_sum_squares<-function(series, centro) {
	suma<-0
	for(i in 1:nrow(series)){
		suma<-suma+kl.dist(centro,series[i,])$D
	}
	return(suma)
}


kmeans_ts<- function(series, duraciones, k=1, series_names=NA, fre=1, iteraciones=1) {
	duracion_min<- min(duraciones)
	porcentajes<-duracion_min/duraciones
	todos.espectros<-espectro_clase_promedio(series, duraciones, porcentajes,fre)
	# print(str(todos.espectros))
	# i<-1
	centros.t<-list()
	clases.t<-list()
	wihtin.ss<-numeric(iteraciones)
	for (i in 1:iteraciones) {
	centers<- todos.espectros[sample(1:length(duraciones),k),]
	
	clases.v<-clasify5(todos.espectros, centers, 1:k,T)
	print(i)
	while(1){
		# i<-i+1
		for (j in 1:k) {
			centers[j,]<-colMeans(todos.espectros[clases.v==j,])
		}

		clases.n<-clasify5(todos.espectros, centers, 1:k,T)
		# print(sum(clases.v==clases.n))
		if(all(clases.v==clases.n)){
			break
		}
		clases.v<-clases.n
	}
	for (s in 1:k) {
			wihtin.ss[i]<-wihtin.ss[s]+within_sum_squares(todos.espectros[clases.v==s,],centers[s,])
		}
	# print(which.max(wihtin.ss))
	clases.t[[i]]<- clases.v
	centros.t[[i]]<-centers
	}
	return(list(centros=centros.t, ultimac=clases.t, wss=wihtin.ss))
}
