library(tuneR)
library(zoo)
 library(dendextend)
# Importamos la tabla que identifica el conjunto de entramineto y el de prueba
entramiento_prueba <- read.table("audio-cats-and-dogs/train_test_split.csv", sep=",")
str(entramiento_prueba)
str(levels(entramiento_prueba$V2))
cat_test<-levels(droplevels(entramiento_prueba$V2[2:50]))
dog_test<-levels(droplevels(entramiento_prueba$V3[2:50]))
cat_train<-levels(droplevels(entramiento_prueba$V4[2:116]))
dog_train<-levels(droplevels(entramiento_prueba$V5[2:65]))

nombres <- c(cat_test, cat_train, dog_train, dog_test)

# p1<- readWave(paste0("audio-cats-and-dogs/cats_dogs/",cat_train[1]))
# str(p1)
load_waves<-function(names) {
	loaded.waves<-list()
	for (i in names) {
		loaded.waves[[i]]<-readWave(paste0("audio-cats-and-dogs/cats_dogs/",i))
	}
	loaded.waves
}

get_minimum_length<-function(lista) {
	n<-length(lista)
	largos<-numeric(n)
	for (i in 1:n) {
		largos[i]<-length(lista[[i]]@left)
	}
	# print(largos)
	return(min(largos))
}

get_maximum_length<-function(lista) {
	n<-length(lista)
	largos<-numeric(n)
	for (i in 1:n) {
		largos[i]<-length(lista[[i]]@left)
	}
	# print(largos)
	return(max(largos))
}
get_lengths<-function(lista) {
	n<-length(lista)
	largos<-numeric(n)
	for (i in 1:n) {
		largos[i]<-length(lista[[i]]@left)
	}
	# print(largos)
	return((largos))
}

get_truncated_wave<-function(lista, minimum) {
	new_lista<-list()
	# nombres<-names(lista)
	for (i in names(lista)) {
		new_lista[[i]]<-lista[[i]]@left[1:minimum]
		# print(lista[[i]]@left[1:minimum])
	}
	return(new_lista)
}
get_truncated_wave2<-function(lista, minimum) {
	new_lista<-list()
	mino2<-minimum/2
	# nombres<-names(lista)
	for (i in names(lista)) {
		aux<-lista[[i]]@left
		n<-floor(length(aux)/2-mino2)+1
		new_lista[[i]]<-aux[n:(n+minimum-1)]
		# print(lista[[i]]@left[1:minimum])
	}
	return(new_lista)
}

convert_to_training_set<-function(lista,nombres) {
	nr<-length(nombres)
	nc<-length(lista[[1]])
	# print(nr)
	# print(nc)
	train_mat<-matrix(0, nrow=nr, ncol=nc)
	for (i in 1:length(nombres)) {
		train_mat[i,]<-lista[[nombres[i]]]
	}
	# for (i in (length(nombres_c)+1):nr) {
	# 	train_mat[i,]<-lista[[nombres_d[i]]]
	# }
	return(train_mat)
}
convert_to_training_set2<-function(lista,nombres) {
	nr<-length(nombres)
	nc<-length(lista[[1]])
	# print(nr)
	# print(nc)
	lista2<-list()
	# train_mat<-matrix(0, nrow=nr, ncol=nc)
	for (i in 1:length(nombres)) {
		lista2[[nombres[[i]]]]<-lista[[nombres[i]]]
	}
	# for (i in (length(nombres_c)+1):nr) {
	# 	train_mat[i,]<-lista[[nombres_d[i]]]
	# }
	return(lista2)
}

swapset<-function(original_class, new_class) {
	un_or<-unique(original_class)
	un_new<-unique(new_class)
	n<-length(un_or)
	m<- length(un_new)
	matriz_swap<- matrix(nrow = n, ncol = m)
	for (i in 1:n) {
		for (j in 1:m) {
			matriz_swap[i,j]<-sum(original_class==un_or[i] & new_class==un_new[j] )
		}
	}
	matriz_swap<-as.data.frame(matriz_swap)
	colnames(matriz_swap)<- un_new
	rownames(matriz_swap)<- un_or
	return(matriz_swap)
}


# Cargamos los datos 
todas.waves<-load_waves(nombres)
mini<-get_minimum_length(todas.waves)
maxi<-get_maximum_length(todas.waves)
lens<-get_lengths(todas.waves)
modulos<- floor(lens / mini)* (mini/16000)
porcentajes<-min(modulos)/modulos
duration<-lens/16000
clase_test1<-c(rep("cat", length(cat_test)), rep("dog", length(dog_test)))
clase_entre<-c(rep("cat", length(cat_train)), rep("dog", length(dog_train)))
which.min(lens)
# Esta esta es la parte que se queda
# sp21<- spec(trincadas4[["cat_68.wav"]],f=2, PSD=TRUE)
# sampling, obtenemos la parte de enmedio de las series de tiempo
trincadas4<-get_truncated_wave2(todas.waves, mini)

# Entrenamos el algoritmo
# gatos.vs.perros3<-classification_problem3(trincadas4[c(cat_train, dog_train)[-89]], clase_entre[-89], fre=16000)
# gatos.vs.perros3$confusion_mat
# probamos con el conjunto de prueba xD
# c(cat_train, dog_train)[-89][clase_entre[-89]!=gatos.vs.perros3$new_class]
# espectro_test4<- espectro_clase3(trincadas4[c(cat_test,dog_test)])
# clase_test4<-clasify3(espectro_test4, gatos.vs.perros3$centers,c("cat", "dog"), T)
# conf2<-confusion_matrix(clase_test1,clase_test4)
# conf2
# plot(gatos.vs.perros3$centers[1,], type="l")
# plot(gatos.vs.perros3$centers[2,], type="l")

# spectro(trincadas4[[1]], f=16)

# prs <- meanspec(todas.waves[[34]], f=16000)
# prs2 <- spec(todas.waves[[34]], f=16000)
# str(prs)

# Vamos a hacer la prueba con el espector medio de cada serie
# gatos.vs.perros.promedio<-classification_problem_promedio(todas.waves[c(cat_train, dog_train)], 
# 	clase_entre, modulos[nombres%in%c(cat_train, dog_train)], fre=16000)
# gatos.vs.perros.promedio$confusion_mat
# str(gatos.vs.perros.promedio$centers)

# probemos con el conjunto de prueba
# indicador<-match(c(cat_test,dog_test),nombres)
# espectro_test_promedio<- espectro_clase_promedio(todas.waves[c(cat_test,dog_test)], modulos[indicador], porcentajes[indicador], fre=16000)
# clase_test_promedio<-clasify3(espectro_test_promedio, gatos.vs.perros.promedio$centers,c("cat", "dog"), T)
# conf_test_promedio<-confusion_matrix(clase_test1,clase_test_promedio)
# conf_test_promedio
# plot(prs[,1],gatos.vs.perros.promedio$centers[1,], type="l")
# plot(prs[,1],gatos.vs.perros.promedio$centers[2,], type="l")

# ploteamos lo que tenemos a la manos
# pdf("resultados.pdf")
# Espectrogramas nitidos
# spectro(todas.waves[["cat_67.wav"]], f=16000, colbg="#09333e")
# spectro(todas.waves[["dog_barking_105.wav"]], f=16000,colbg="#09333e")
# dev.off()
# pdf("resultados2.pdf")

# Primer ejemplo, corte, espectros
# plot(prs2[,1],gatos.vs.perros3$centers[1,], type="l")
# plot(prs2[,1],gatos.vs.perros3$centers[2,], type="l")

#segundo ejemplo, promedio, espectros
# plot(prs[,1],gatos.vs.perros.promedio$centers[1,], type="l")
# plot(prs[,1],gatos.vs.perros.promedio$centers[2,], type="l")
# dev.off()


# clasificacion considerando un umbral de decisi\'on distinto

curva_roc<- function(spectrums, centers, t_classes, from=0, to=1, by=1) {
classes_u<- unique(t_classes)
positive<- sum(t_classes==classes_u[1])
negative<- sum(t_classes==classes_u[2])
umbrales<- seq(from, to, by)
n<-length(umbrales)
roc<-matrix(0,nrow=n, ncol=2)
for (i in 1:n) {
	clasificacion_aux<-clasify3_umbral(spectrums, centers,classes_u, umbrales[i], T)
	matriz_conf<-confusion_matrix(t_classes, clasificacion_aux)
	roc[i,1]<-1-matriz_conf[2,2]/negative
	roc[i,2]<-matriz_conf[1,1]/positive
}
return(roc)
}
# clase_entrenamiento_promedio_umbral<-clasify3_umbral(gatos.vs.perros.promedio$spectrums, gatos.vs.perros.promedio$centers,c("cat", "dog"), 1, T)
# conf.entrenamiento.promedio.umbral<-confusion_matrix(clase_entre,clase_entrenamiento_promedio_umbral)
# roca<-curva_roc(gatos.vs.perros.promedio$spectrums, gatos.vs.perros.promedio$centers, clase_entre, -5, 5, .1)
# str(roca)
# plot(roca, type="l")
# oed<-order(roca[,1])
# AUC <- sum(diff(roca[oed,1])*rollmean(roca[oed,2],2))

# roca_test<- curva_roc(espectro_test_promedio, gatos.vs.perros.promedio$centers, clase_test1, -5, 5, .1)
# plot(roca_test, type="l")
# oed_test<-order(roca_test[,1])
# AUC_test <- sum(diff(roca_test[oed_test,1])*rollmean(roca_test[oed_test,2],2))






# prueba con balance de la muestra

# gatos<- c(cat_test, cat_train)
# perros<-c(dog_test, dog_train)
# gatos_entrenamiento<-sample(gatos,60)
# perros_entrenamiento<-sample(perros, 60)
# gatos_prueba<-setdiff(gatos, gatos_entrenamiento)
# perros_prueba<-setdiff(perros, perros_entrenamiento)
# clase_entrenamiento<- c(rep("cat", 60), rep("dog", 60))
# clase_prueba<- c(rep("cat", length(gatos_prueba)), rep("dog", length(perros_prueba)))

# Vamos a hacer la prueba con el espector medio de cada serie
# g.vs.p<-classification_problem_promedio(todas.waves[c(gatos_entrenamiento, perros_entrenamiento)], 
# 	clase_entrenamiento, modulos[match(c(gatos_entrenamiento, perros_entrenamiento),nombres)], fre=16000)
# g.vs.p$confusion_mat
# str(g.vs.p$centers)

# Prueba
# indicador<-match(c(gatos_prueba, perros_prueba),nombres)
# g.vs.p_espt_promedio<- espectro_clase_promedio(todas.waves[indicador], modulos[indicador], porcentajes[indicador], fre=16000)
# clase_test_promedio<-clasify3(g.vs.p_espt_promedio, g.vs.p$centers,c("cat", "dog"), T)
# conf_test_promedio<-confusion_matrix(clase_prueba,clase_test_promedio)
# conf_test_promedio


# Curva rock entrenamiento
# roca_entrenamiento<-curva_roc(g.vs.p$spectrums, g.vs.p$centers, clase_entrenamiento, -5, 5, .1)
# str(roca_entrenamiento)
# plot(roca_entrenamiento, type="l")
# oed<-order(roca_entrenamiento[,1])
# AUC_entrenamiento <- sum(diff(roca_entrenamiento[oed,1])*rollmean(roca_entrenamiento[oed,2],2))


# roca_prueba<-curva_roc(g.vs.p_espt_promedio, g.vs.p$centers, clase_prueba, -5, 5, .1)
# str(roca_prueba)
# plot(roca_prueba, type="l")
# oed<-order(roca_prueba[,1])
# AUC_prueba <- sum(diff(roca_prueba[oed,1])*rollmean(roca_prueba[oed,2],2))

# pdf("resultados3.pdf")

# Primer ejemplo, corte, espectros
# plot(prs[,1],g.vs.p$centers[1,], type="l")
# plot(prs[,1],g.vs.p$centers[2,], type="l")
#segundo ejemplo, promedio, espectros
# dev.off()


# Mejor clasificacion y umbral de decisis\'on'
mejor_umbral<-function(spectrums, centers, t_classes, from=0, to=1, by=1) {
classes_u<- unique(t_classes)
positive<- sum(t_classes==classes_u[1])
negative<- sum(t_classes==classes_u[2])
total<- length(t_classes)
umbrales<- seq(from, to, by)
n<-length(umbrales)
tasa_error<-numeric(n)
for (i in 1:n) {
	clasificacion_aux<-clasify3_umbral(spectrums, centers,classes_u, umbrales[i], T)
	matriz_conf<-confusion_matrix(t_classes, clasificacion_aux)
	tasa_error[i]<-(matriz_conf[1,2]+matriz_conf[2,1])/total 
	# roc[i,1]<-1-matriz_conf[2,2]/negative
	# roc[i,2]<-matriz_conf[1,1]/positive
}
objeto <- list("Umbral"=umbrales[which.min(tasa_error)], "Tasa"=tasa_error[which.min(tasa_error)])
return(objeto)	
}


mejor_umbral_minimax<-function(spectrums, centers, t_classes, from=0, to=1, by=1) {
classes_u<- unique(t_classes)
positive<- sum(t_classes==classes_u[1])
negative<- sum(t_classes==classes_u[2])
total<- length(t_classes)
umbrales<- seq(from, to, by)
n<-length(umbrales)
tasa_error<-numeric(n)
sens<-numeric(n)
spec<-numeric(n)
for (i in 1:n) {
	clasificacion_aux<-clasify3_umbral(spectrums, centers,classes_u, umbrales[i], T)
	matriz_conf<-confusion_matrix(t_classes, clasificacion_aux)
	sens[i]<-matriz_conf[1,1]/(matriz_conf[1,1]+ matriz_conf[1,2])
	spec[i]<-matriz_conf[2,2]/(matriz_conf[2,1]+ matriz_conf[2,2])
	tasa_error[i]<-abs(sens[i]-spec[i])
	# roc[i,1]<-1-matriz_conf[2,2]/negative
	# roc[i,2]<-matriz_conf[1,1]/positive
}
objeto <- list("Umbral"=umbrales[which.min(tasa_error)], "Tasa"=tasa_error[which.min(tasa_error)], "Sensibilidad"=sens, "Especificidad"=spec)
return(objeto)	
}
# mej_umb <- mejor_umbral(g.vs.p$spectrums, g.vs.p$centers, clase_entrenamiento, -5, 5, .1)
# clasificacion_entrenamiento_m<-clasify3_umbral(g.vs.p$spectrums, g.vs.p$centers,c("cat", "dog"), mej_umb$Umbral, T)
# matriz_conf_entrenamiento_m<-confusion_matrix(clase_entrenamiento, clasificacion_entrenamiento_m)



# clase_test_promedio_m<-clasify3_umbral(g.vs.p_espt_promedio, g.vs.p$centers,c("cat", "dog"),mej_umb$Umbral, T)
# conf_test_promedio_m<-confusion_matrix(clase_prueba,clase_test_promedio_m)
# conf_test_promedio_m



 # Sobre muestreando los perros, este es el ejercicio final
# Designamos clases y el indicador de cada serie
cat_test2<-cat_test
dog_test2<-dog_test
cat_train2<-cat_train
set.seed(25)
dog_train2<-c(dog_train, sample(dog_train, length(cat_train)-length(dog_train),replace = T))
clase_entrenamiento<- c(rep("cat", length(cat_train2)), rep("dog", length(dog_train2)))
clase_prueba<- c(rep("cat", length(cat_test2)), rep("dog", length(dog_test2)))

indicador<-match(c(cat_train2, dog_train2),nombres)

# Entrenamos el algoritmo y vemos que resultados arroja
g.vs.p<-classification_problem_promedio(todas.waves[indicador], 
	clase_entrenamiento, modulos[indicador], fre=16000)
g.vs.p$confusion_mat
(g.vs.p$confusion_mat[2,1]+g.vs.p$confusion_mat[1,2])/sum(g.vs.p$confusion_mat)

# best_umb<- mejor_umbral(g.vs.p$spectrums, g.vs.p$centers, clase_entrenamiento, -5, 5, .1)
best_umb<- mejor_umbral_minimax(g.vs.p$spectrums, g.vs.p$centers, clase_entrenamiento, -5, 5, .1)
str(g.vs.p$centers)
clasificacion_entrenamiento_m<-clasify3_umbral(g.vs.p$spectrums, g.vs.p$centers,c("cat", "dog"), best_umb$Umbral, T)
matriz_conf_entrenamiento_m<-confusion_matrix(clase_entrenamiento, clasificacion_entrenamiento_m)
matriz_conf_entrenamiento_m
(matriz_conf_entrenamiento_m[2,1]+matriz_conf_entrenamiento_m[1,2])/sum(matriz_conf_entrenamiento_m)
nombres[indicador[clase_entrenamiento != clasificacion_entrenamiento_m]]

# Resultados con el conjunto de prueba
indicador<-match(c(cat_test2, dog_test2),nombres)
g.vs.p_espt_promedio<- espectro_clase_promedio(todas.waves[indicador], modulos[indicador], porcentajes[indicador], fre=16000)
clase_test_promedio_m<-clasify3_umbral(g.vs.p_espt_promedio, g.vs.p$centers,c("cat", "dog"),best_umb$Umbral, T)
conf_test_promedio_m<-confusion_matrix(clase_prueba,clase_test_promedio_m)
conf_test_promedio_m
(conf_test_promedio_m[2,1]+conf_test_promedio_m[1,2])/sum(conf_test_promedio_m)
nombres[indicador[clase_prueba != clase_test_promedio_m]]



# Curva rock entrenamiento
roca_entrenamiento<-curva_roc(g.vs.p$spectrums, g.vs.p$centers, clase_entrenamiento, -5, 5, .1)
str(roca_entrenamiento)
dev.off()
plot(roca_entrenamiento, type="l")
oed<-order(roca_entrenamiento[,1])
AUC_entrenamiento <- sum(diff(roca_entrenamiento[oed,1])*rollmean(roca_entrenamiento[oed,2],2))

# Curva rock entrenamiento
roca_prueba<-curva_roc(g.vs.p_espt_promedio, g.vs.p$centers, clase_prueba, -5, 5, .1)
str(roca_prueba)
plot(roca_prueba, type="l")
oed<-order(roca_prueba[,1])
AUC_prueba <- sum(diff(roca_prueba[oed,1])*rollmean(roca_prueba[oed,2],2))
AUC_prueba





# Ejemplo de conglomerados

distancias_congo<-distancias_series(todas.waves, modulos, fre=16000)
conglomerado1 <-hclust(as.dist(distancias_congo), method="ward.D")
dendrogramas1<-as.dendrogram(conglomerado1)
conglomerado2 <-hclust(as.dist(distancias_congo), method="ward.D2")
dendrogramas2<-as.dendrogram(conglomerado2)
conglomerado3 <-hclust(as.dist(distancias_congo), method="complete")
dendrogramas3<-as.dendrogram(conglomerado3)
conglomerado4 <-hclust(as.dist(distancias_congo), method="single")
dendrogramas4<-as.dendrogram(conglomerado4)
conglomerado5 <-hclust(as.dist(distancias_congo), method="centroid")
dendrogramas5<-as.dendrogram(conglomerado5)
clase<-c(rep(1, 164),rep(2,113))
# colorres <- c("#a4093a", "#01bcf0")
colorres2<-c(rep("#a4093a", 164),rep("#01bcf0",113))

n<-4
colores <- rainbow(n)
dendrogramas2 <- dendrogramas2 %>%
          color_branches(k = n)
labels_colors(dendrogramas1)<-colorres2[conglomerado1$order]
labels(dendrogramas1)<-c(rep(".", length(c(cat_test,cat_train))), rep(".", length(c(dog_train,dog_test))))[conglomerado1$order]
labels_colors(dendrogramas2)<-colorres2[conglomerado2$order]
labels(dendrogramas2)<-c(rep(".", length(c(cat_test,cat_train))), rep(".", length(c(dog_train,dog_test))))[conglomerado2$order]
labels_colors(dendrogramas3)<-colorres2[conglomerado3$order]
labels(dendrogramas3)<-c(rep(".", length(c(cat_test,cat_train))), rep(".", length(c(dog_train,dog_test))))[conglomerado3$order]
labels_colors(dendrogramas4)<-colorres2[conglomerado4$order]
labels(dendrogramas4)<-c(rep(".", length(c(cat_test,cat_train))), rep(".", length(c(dog_train,dog_test))))[conglomerado4$order]
labels_colors(dendrogramas5)<-colorres2[conglomerado5$order]
labels(dendrogramas5)<-c(rep(".", length(c(cat_test,cat_train))), rep(".", length(c(dog_train,dog_test))))[conglomerado5$order]

plot(dendrogramas1)
plot(dendrogramas2)
plot(dendrogramas3)
plot(dendrogramas4)
plot(dendrogramas5)

pdf("resultados4.pdf")
plot(dendrogramas1)
plot(dendrogramas2)
plot(dendrogramas3)
dev.off()
# plot(color_branches(dendrogramas, col=colorres, groupLabels = clase))

# Make the analysis more complete, what does de ward.D2 stans for
n<-4
colores <- rainbow(n)
dendrogramas2 <- dendrogramas2 %>%
          color_branches(k = n)
labels(dendrogramas2)<-c(rep(".", length(c(cat_test,cat_train))), rep(".", length(c(dog_train,dog_test))))[conglomerado2$order]
plot(dendrogramas2)
clasificacion_conglomerados<-cutree(conglomerado2, n)
vercadero.vs.conglomerado<- data.frame(c(rep("cat", length(c(cat_test,cat_train))), rep("dog", length(c(dog_train,dog_test)))),
	clasificacion_conglomerados)
swapset(c(rep("cat", length(c(cat_test,cat_train))), rep("dog", length(c(dog_train,dog_test)))),
	clasificacion_conglomerados)



# Clasificacion enlace completo

# clasificacion_conglomerados<-cutree(conglomerado3, 5)
# vercadero.vs.conglomerado<- data.frame(c(rep("cat", length(c(cat_test,cat_train))), rep("dog", length(c(dog_train,dog_test)))),
# 	clasificacion_conglomerados)
# swapset(c(rep("cat", length(c(cat_test,cat_train))), rep("dog", length(c(dog_train,dog_test)))),
# 	clasificacion_conglomerados)


espectro_promedio_congo <- list()
for (i in 1:n) {
	indicador<-clasificacion_conglomerados==i
	espectro_promedio_congo[[i]]<-colMeans(espectro_clase_promedio(todas.waves[indicador], modulos[indicador], porcentajes[indicador], fre=16000))

}
indicador<-clasificacion_conglomerados==2 | clasificacion_conglomerados==3
usa<-colMeans(espectro_clase_promedio(todas.waves[indicador], modulos[indicador], porcentajes[indicador], fre=16000))
plot(usa, type="l")

plot(espectro_promedio_congo[[1]], type="l")
plot(espectro_promedio_congo[[2]], type="l")
plot(espectro_promedio_congo[[3]], type="l")
plot((espectro_promedio_congo[[2]]+espectro_promedio_congo[[3]])/2, type="l")
plot(espectro_promedio_congo[[4]], type="l")



names(todas.waves)[indicador<-clasificacion_conglomerados==1]
names(todas.waves)[indicador<-clasificacion_conglomerados==2]
names(todas.waves)[indicador<-clasificacion_conglomerados==3]
names(todas.waves)[indicador<-clasificacion_conglomerados==4]

d<-6:10
s<-4
for (i in d) {
	play(todas.waves[[names(todas.waves)[indicador<-clasificacion_conglomerados==s][i]]])
}

# K-medias 
set.seed(25)
clases.kmeans<-kmeans_ts(todas.waves, modulos, 2, fre=16000, iteraciones=20)
clases.kmeans$wss
clases.kmeans$centros[which.min(clases.kmeans$wss)]
clases.kmeans$ultimac[which.min(clases.kmeans$wss)]
swapset(c(rep("cat", length(c(cat_test,cat_train))), rep("dog", length(c(dog_train,dog_test)))),
	clases.kmeans$ultimac[[which.min(clases.kmeans$wss)]])
unique(clases.kmeans$ultimac[which.min(clases.kmeans$wss)])


# Todo lo que viene al final en el paper o presentacion

# seleccionemos un gato al azar 75, 101

spectro(todas.waves[["dog_barking_101.wav"]])
setWavPlayer('/usr/bin/afplay')
play(todas.waves[["dog_barking_94.wav"]])
play(todas.waves[["cat_89.wav"]])
pdf("Resultados_F1.pdf")
plot(todas.waves[["cat_75.wav"]], xlab="Tiempo", ylab="db")
plot(todas.waves[["dog_barking_101.wav"]], xlab="Tiempo", ylab="db")
spectro(todas.waves[["cat_75.wav"]], f=16000, tlab="Tiempo", flab="Frecuencia(kHz)",scalelab = "Amplitud\n(dB)")
spectro(todas.waves[["dog_barking_101.wav"]], f=16000, tlab="Tiempo", flab="Frecuencia(kHz)", scalelab = "Amplitud\n(dB)")
dev.off()

# tlab = "Time (s)",
# flab = "Frequency (kHz)",
prs <- meanspec(todas.waves[[34]], f=16000)
pdf("Resultados_F2.pdf")
plot(prs[,1],g.vs.p$centers[1,], type="l", xlab="Frecuencia", ylab="")
plot(prs[,1],g.vs.p$centers[2,], type="l", xlab="Frecuencia", ylab="")
dev.off()
pdf("Resultados_F3.pdf")
plot(roca_entrenamiento, type="l", xlab="1-especificidad", ylab="Sensibilidad")
abline(0,1)
plot(roca_prueba, type="l", xlab="1-especificidad", ylab="Sensibilidad")
abline(0,1)
plot(dendrogramas1)
plot(dendrogramas2)
plot(dendrogramas3)
plot(prs[,1],espectro_promedio_congo[[1]], type="l",xlab="Frecuencia", ylab="", col=unique(get_leaves_branches_col(dendrogramas2))[4])
plot(prs[,1],espectro_promedio_congo[[2]], type="l",xlab="Frecuencia", ylab="", col=unique(get_leaves_branches_col(dendrogramas2))[2])
plot(prs[,1],espectro_promedio_congo[[3]], type="l", xlab="Frecuencia", ylab="", col=unique(get_leaves_branches_col(dendrogramas2))[3])
plot(prs[,1],usa, type="l", xlab="Frecuencia", ylab="")
plot(prs[,1],clases.kmeans$centros[[which.min(clases.kmeans$wss)]][1,], type="l", xlab="Frecuencia", ylab="", col="#a4093a")
plot(prs[,1],clases.kmeans$centros[[which.min(clases.kmeans$wss)]][2,], type="l", xlab="Frecuencia", ylab="", col="#01bcf0")
plot(prs[,1],clases.kmeans$centros[[which.min(clases.kmeans$wss)]][1,], type="l", xlab="Frecuencia", ylab="", col="#a4093a")
lines(prs[,1],clases.kmeans$centros[[which.min(clases.kmeans$wss)]][2,], type="l", col="#01bcf0")
dev.off()
# colorres2<-c(rep("#a4093a", 164),rep("#01bcf0",113))

# plot(clases.kmeans$centros[[which.min(clases.kmeans$wss)]][1,], type="l")
# plot(clases.kmeans$centros[[which.min(clases.kmeans$wss)]][2,], type="l")
# Resultados conglomerados jerarquicos
# pdf("Resultados_F4.pdf")

pdf("Resultados_F1_2.pdf")
par(bg="#09333e", col="#d9d9d9", col.axis="#d9d9d9", col.lab="#d9d9d9", col.main="#d9d9d9", col.sub="#d9d9d9")
plot(todas.waves[["cat_75.wav"]], xlab="Tiempo", ylab="db",col="#d9d9d9")
plot(todas.waves[["dog_barking_101.wav"]], xlab="Tiempo", ylab="db", col="#d9d9d9")
spectro(todas.waves[["cat_75.wav"]], f=16000, tlab="Tiempo", flab="Frecuencia(kHz)",scalelab = "Amplitud\n(dB)",
	colbg="#09333e",  colaxis="#d9d9d9", collab="#d9d9d9")
spectro(todas.waves[["dog_barking_101.wav"]], f=16000, tlab="Tiempo", flab="Frecuencia(kHz)", scalelab = "Amplitud\n(dB)",
	colbg="#09333e",  colaxis="#d9d9d9", collab="#d9d9d9")
dev.off()

pdf("Resultados_F2_2.pdf")
par(bg="#09333e", col="#d9d9d9", col.axis="#d9d9d9", col.lab="#d9d9d9", col.main="#d9d9d9", col.sub="#d9d9d9")
plot(prs[,1],g.vs.p$centers[1,], type="l", xlab="Frecuencia", ylab="", col="#a4093a")
plot(prs[,1],g.vs.p$centers[2,], type="l", xlab="Frecuencia", ylab="", col="#01bcf0")
dev.off()

pdf("Resultados_F3_2.pdf")
par(bg="#09333e", col="#d9d9d9", col.axis="#d9d9d9", col.lab="#d9d9d9", col.main="#d9d9d9", col.sub="#d9d9d9")
plot(roca_entrenamiento, type="l", xlab="1-especificidad", ylab="Sensibilidad")
abline(0,1)
plot(roca_prueba, type="l", xlab="1-especificidad", ylab="Sensibilidad")
abline(0,1)
plot(dendrogramas1)
plot(dendrogramas2)
plot(dendrogramas3)
plot(prs[,1],espectro_promedio_congo[[1]], type="l",xlab="Frecuencia", ylab="", col=unique(get_leaves_branches_col(dendrogramas2))[4])
plot(prs[,1],espectro_promedio_congo[[2]], type="l",xlab="Frecuencia", ylab="", col=unique(get_leaves_branches_col(dendrogramas2))[2])
plot(prs[,1],espectro_promedio_congo[[3]], type="l", xlab="Frecuencia", ylab="", col=unique(get_leaves_branches_col(dendrogramas2))[3])
plot(prs[,1],usa, type="l", xlab="Frecuencia", ylab="")
plot(prs[,1],clases.kmeans$centros[[which.min(clases.kmeans$wss)]][1,], type="l", xlab="Frecuencia", ylab="", col="#a4093a")
plot(prs[,1],clases.kmeans$centros[[which.min(clases.kmeans$wss)]][2,], type="l", xlab="Frecuencia", ylab="", col="#01bcf0")
plot(prs[,1],clases.kmeans$centros[[which.min(clases.kmeans$wss)]][1,], type="l", xlab="Frecuencia", ylab="", col="#a4093a")
lines(prs[,1],clases.kmeans$centros[[which.min(clases.kmeans$wss)]][2,], type="l", col="#01bcf0")
dev.off()
plot()
str(best_umb)
plot(best_umb$Sensibilidad, type="l")
lines(best_umb$Especificidad, type="l")
AUC_entrenamiento
AUC_prueba


# best_umb<- mejor_umbral_minimax(g.vs.p$spectrums, g.vs.p$centers, clase_entrenamiento, -5, 5, .1)
# str(best_umb)