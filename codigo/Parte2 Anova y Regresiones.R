
setwd("C:/Users/Orlando/Documents/BEDU/Modulo 2- R y Python/R/proyecto")

# Librerias ----
library("ggplot2")
library("pals")
library(RColorBrewer)
library(normtest)
library(nortest)
library(agricolae)
library(knitr)
# Define the number of colors you want

# ----

# Preprocesamiento ----
datos<-read.csv("dat.economi.csv")
datos$educa_jefe<-as.factor(datos$educa_jefe)
datos$Escolaridad.jefe<-factor(datos$Escolaridad.jefe,
                                  levels=c("Sin estudios","Preescolar",
                                           "Primaria incompleta","Primaria completa","Secundaria incompleta",
                                           "Secundaria completa","Preparatoria incompleta","Preparatoria completa",
                                           "Profesional incompleta","Profesional completa","Posgrado"))


# ----


#Exploración ----
nb.cols <- length(unique(datos$Escolaridad.jefe))
mycolors <- colorRampPalette(brewer.pal(8, "Spectral"))(nb.cols)
ggplot(datos)+geom_bar(aes(Escolaridad.jefe,fill=Escolaridad.jefe))+coord_flip()+scale_fill_manual(values = mycolors) #+ geom_text(data = datos,aes(x=x))

# outliers
ggplot(datos,aes(y=sueldos, fill=Tipo.Localidad))+geom_boxplot() # se observa que hay una cantidad considerable de datos atípicos, que
# representan la desigualdad salarial de las viviendas en méxico.
quantiles<-as.data.frame(quantile(datos$sueldos,seq(0,.99,.01)))
names(quantiles)<-"Cuantiles"
quantiles$P<-seq(0,.99,.01)
ggplot(quantiles,aes(P,Cuantiles))+geom_point()+geom_segment(aes(x=P,xend=P,y=0,yend=Cuantiles))
# OBSERVAMOS QUE HAY UN GRAN SALTO EN EL ULTIMO PERCENTIL (99%), POR LO QUE NOS QUEDAMOS CON EL 97% DE LOS DATOS POR REPRESENTATIVIDAD.
cuantil<-quantile(datos$sueldos,.97)
datos<-datos[datos$sueldos<cuantil,]

#nuevo boxplot
ggplot(datos,aes(y=sueldos, fill=Tipo.Localidad))+geom_boxplot() 
# ----



# ANOVAS ----
ggplot(datos,aes(y=sueldos, fill=Escolaridad.jefe))+geom_boxplot() 
#Procederemos a realizar ANOVA, discriminamos las escolaridades incompletas
datos_anova<-datos
datos_anova$Escolaridad.jefe<-as.character(datos_anova$Escolaridad.jefe)

#Sustituimos valores
datos_anova$Escolaridad.jefe[datos_anova$Escolaridad.jefe=="Primaria incompleta"]<-"Preescolar"
datos_anova$Escolaridad.jefe[datos_anova$Escolaridad.jefe=="Primaria completa"]<-"Primaria"
datos_anova$Escolaridad.jefe[datos_anova$Escolaridad.jefe=="Secundaria incompleta"]<-"Primaria"
datos_anova$Escolaridad.jefe[datos_anova$Escolaridad.jefe=="Secundaria completa"]<-"Secundaria"
datos_anova$Escolaridad.jefe[datos_anova$Escolaridad.jefe=="Preparatoria incompleta"]<-"Secundaria"
datos_anova$Escolaridad.jefe[datos_anova$Escolaridad.jefe=="Preparatoria completa"]<-"Preparatoria"
datos_anova$Escolaridad.jefe[datos_anova$Escolaridad.jefe=="Profesional incompleta"]<-"Preparatoria"
datos_anova$Escolaridad.jefe[datos_anova$Escolaridad.jefe=="Profesional completa"]<-"Profesional"
unique(datos_anova$Escolaridad.jefe)

datos_anova$Escolaridad.jefe<-factor(datos_anova$Escolaridad.jefe,
                               levels=c("Sin estudios","Preescolar",
                                        "Primaria","Secundaria",
                                        "Preparatoria",
                                        "Profesional","Posgrado"))

ggplot(datos_anova,aes(y=sueldos, fill=Escolaridad.jefe))+geom_boxplot() 




# Realizamos una prueba anova general. Lo más probable es que se rechace la igualdad de medias de salarios según nivel de estudios
# Nuestro interés está en realizar las comparaciones múltiples para determinar si existen diferencias de salarios entre los niveles de estudios consecutivos.

anov<-aov(sueldos~Escolaridad.jefe,data = datos_anova)
summary(anov)


#obtenemos un p-valor (muy pequeño) que rechaza la hipotesis nula con cualquier nivel de significancia, por lo que procedemos a realizar comparaciones multiples
scheffe<-scheffe.test(anov,"Escolaridad.jefe",group = F)
scheffe<-as.data.frame(scheffe$comparison)
kable(scheffe)


#### OTRAS VARIABLES


#En el siguiente apartado se intentará explicar el efecto que tienen ciertas variables en el salario que recibe el jefe de familia de una vivienda.


#Decidimos utilizar la variable cuantitativa edad, y las variables cualitativas escolaridad, nivel socioeconómico y tipo de localidad.


#definicion de variables

variables<-seq(0,6,1)
variables<-as.data.frame(variables)
names(variables)<-"Valor"
variables$Escolaridad<-c("Sin estudios","Preescolar","Primaria","Secundaria","Preparatioria","Profesional","Posgrado")
variables$NivelSocioeconómico<-c("Bajo","Medio Bajo","Medio Alto","Alto","-","-","-")
variables$Localidad<-c("No ciudad","Ciudad","-","-","-","-","-")
kable(variables,caption = "Definición de variables categóricas utilizadas en el modelo")




#Ajustamos 4 modelos diferentes:
  

modelo1<-lm(sueldos~edad_jefe,data = datos_anova)
modelo2<-lm(sueldos~edad_jefe+Escolaridad.jefe,data = datos_anova)
modelo3<-lm(sueldos~edad_jefe+factor(Tipo.Localidad),data = datos_anova)
modelo4<-lm(sueldos~edad_jefe+factor(est_socio),data = datos_anova)
modelo5<-lm(sueldos~edad_jefe+Escolaridad.jefe+factor(est_socio),data = datos_anova)

summary(modelo1)
summary(modelo2)
summary(modelo3)
summary(modelo4)
summary(modelo5)

#Analisis de residuales
par(mfrow = c(2, 3))
ggplot(datos_anova,aes(sueldos,modelo1$residuals))+ geom_point() + geom_smooth(color="firebrick")
ggplot(datos_anova,aes(sueldos,modelo2$residuals))+ geom_point() + geom_smooth(color="firebrick")
ggplot(datos_anova,aes(sueldos,modelo3$residuals))+ geom_point() + geom_smooth(color="firebrick")
ggplot(datos_anova,aes(sueldos,modelo4$residuals))+ geom_point() + geom_smooth(color="firebrick")
ggplot(datos_anova,aes(sueldos,modelo5$residuals))+ geom_point() + geom_smooth(color="firebrick")



