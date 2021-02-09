#Script creado para analizar la tabla obtenida de los datos concentrados de
#vivienda, correspondientes al senso Hogar y vivienda del inegi 2018.
#El script separará la informacion para obtener datos con la menor cantidad
#de ruido posible.

#===========================================================================
#se importan librerias
library(ggplot2)
library(pals)
library(dplyr)
library(LearnBayes)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(knitr)
library(agricolae)

#===========================================================================
#Preprocesado de datos
#---------------------------------------------------------------------------
#se carga el archivo.
#nota, es posible que el archivo pese demasiado y algunos ordenadores no sean capaces de descargarlos.
#en caso de no poderse, ejecutar el codigo desde R cloud
dat_economi <- read.csv("https://raw.githubusercontent.com/Mario-Encinas/analisis-relacion-estudios-ingresos/main/dat.economi.csv")


#---------------------------------------------------------------------------
#Se crean columnas con cadenas de valores ordinales
dat_economi$educa_jefe <- as.factor(dat_economi$educa_jefe)
dat_economi$Escolaridad.jefe <- factor(dat_economi$educa_jefe,
                               labels = c("Sin estudios","Preescolar",
                                        "Primaria incompleta","Primaria completa"
                                        ,"Secundaria incompleta","Secundaria completa"
                                        ,"Preparatoria incompleta",
                                        "Preparatoria completa",
                                        "Profesional incompleta",
                                        "Profesional completa","Posgrado"))

dat_economi$est_socio <- as.factor(dat_economi$est_socio)
dat_economi$Estado.socio <- factor(dat_economi$est_socio,
                                       labels = c("bajo","Medio bajo",
                                                  "Medio alto", "Alto"))

#Se comvierten a tipos de datos numericos las colunas correspondientes al
#salario y los ingresos.
dat_economi$ing_cor <- as.integer(dat_economi$ing_cor)
dat_economi$sueldos <- as.integer(dat_economi$sueldos)

#reducimos muestra a percentil 97
cuantil<-quantile(dat_economi$ing_cor,.97)
dat_economi <- dat_economi[dat_economi$sueldos<cuantil,]

#calculamos la diferencia entre ingresos totales y sueldo para obtener el total
#de ingresos extra.
dat_economi <- mutate(dat_economi, extra = ing_cor - sueldos)

#calculamos que porcentaje de los ingresos corresponde al extra
dat_economi <- mutate(dat_economi, porcentaje.extra = (100/ing_cor)*extra)

#Separamos los hogares donde solamente vive una persona
#este conjunto es el principal enfoque de analisis en este script
solo <- subset(dat_economi, tot_integ == 1)
rm(dat_economi)

#Separamos los que viven en una ciudad a los que viven fuera de la ciudad
#poblaciones con mas de 15,000 habitantes se consideran ciudades
solo.ciudad <- subset(solo, tam_loc <= 2)
solo.campo <- subset(solo, tam_loc > 2)
rm(solo)

#Separamos a los jovenes en edad escolar del resto del grupo
#son un grupo que posee mucho ruido en los datos.
ciudad.joven <- subset(solo.ciudad, edad_jefe <= 25)
ciudad.adulto <- subset(solo.ciudad, edad_jefe > 25)
#rm(solo.ciudad)

campo.joven <- subset(solo.campo, edad_jefe <= 25)
campo.adulto <- subset(solo.campo, edad_jefe > 25)
#rm(solo.campo)

#===========================================================================
#visualizacion de datos
#---------------------------------------------------------------------------

#Configuramos variable para permitir visualizar colores en las graficas
#los colores facilitarán el entnedimiento de las mismas
nb.cols <- length(unique(ciudad.adulto$Escolaridad.jefe))
mycolors <- colorRampPalette(brewer.pal(8, "Spectral"))(nb.cols)
rm(nb.cols)

#---------------------------------------------------------------------------
#Ploteamos graficos de distribucion acorde con el tipo de localidad

ggplot(solo.ciudad)+geom_bar(aes(x = educa_jefe,fill=Escolaridad.jefe))+
  scale_fill_manual(values = mycolors)+
  labs(x = "Escolaridad", y = "Numero individuos",
       title = "Nivel de escolaridad en la ciudad")

ggplot(solo.campo)+geom_bar(aes(x = educa_jefe,fill=Escolaridad.jefe))+
  scale_fill_manual(values = mycolors)+
  labs(x = "Escolaridad", y = "Numero individuos",
       title = "Nivel de escolaridad fuera de la ciudad")

#---------------------------------------------------------------------------
#ploteamos plotbox de sueldos

#ggplot(solo.ciudad, aes(y=sueldos, fill=Escolaridad.jefe))+
#  geom_boxplot(outlier.shape = NA) +
#  coord_cartesian(ylim = c(0,100000))+
#  labs(title = "Sueldos en la ciudad")


#ggplot(solo.campo, aes(y=sueldos, fill=Escolaridad.jefe))+
#  geom_boxplot(outlier.shape = NA) +
#  coord_cartesian(ylim = c(0,100000))+
#  labs(title = "Sueldos fuera de la ciudad")

#---------------------------------------------------------------------------
#plot box de sueldos, discriminando los jovenes en edad estudiantil.

ggplot(subset(ciudad.adulto, sueldos > 0), aes(y=sueldos, fill=Escolaridad.jefe))+
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,100000))+  
  labs(title = "Sueldos en la ciudad (individuos mayores a 25 años)")

ggplot(subset(ciudad.joven, sueldos > 0), aes(y=sueldos, fill=Escolaridad.jefe))+
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,100000))+
  labs(title = "Sueldos en la ciudad (individuos menores a 25 años)")

ggplot(subset(campo.adulto, sueldos > 0), aes(y=sueldos, fill=Escolaridad.jefe))+
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,100000))+
  labs(title = "Sueldos fuera de la ciudad (individuos mayores a 25 años)")

ggplot(subset(campo.joven, sueldos > 0), aes(y=sueldos, fill=Escolaridad.jefe))+
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,100000))+
  labs(title = "Sueldos fuera de la ciudad (individuos menores a 25 años)")

#---------------------------------------------------------------------------
#se plotea boxplot de ingresos totales
ggplot(ciudad.adulto, aes(y=ing_cor, fill=Escolaridad.jefe))+
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,125000))+
  labs(y = "Ingresos", title = "Ingresos totales mensuales en la ciudad (individuos mayores a 25 años)")


ggplot(campo.adulto, aes(y=ing_cor, fill=Escolaridad.jefe))+
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,100000))+
  labs(y = "Ingresos",title = "Ingresos totales mensuales fuera de la ciudad (individuos mayores a 25 años)")

#---------------------------------------------------------------------------
#ingresos extra
ggplot(ciudad.adulto, aes(y=extra, fill=Escolaridad.jefe))+
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,100000))+
  labs(title = "Ingresos no salariales mensuales en la ciudad (individuos mayores a 25 años)")


ggplot(campo.adulto, aes(y=extra, fill=Escolaridad.jefe))+
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,100000))+
  labs(title = "Ingresos no salariales mensuales fuera de la ciudad (individuos mayores a 25 años)")

#---------------------------------------------------------------------------
#relacion ingresos extra y salarios
ggp1 <- ggplot(subset(ciudad.adulto, sueldos > 0), aes(y=sueldos))+
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,60000))+
  labs(title = "Salarios")

ggp2 <- ggplot(ciudad.adulto, aes(y=extra))+
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,60000))+
  labs(title = "Ingresos extra")

grid.arrange(ggp1, ggp2, ncol = 2, top = textGrob("Promedio tipo de ingresos ciudad (individuos mayores a 25 años)"))

ggp1 <- ggplot(subset(campo.adulto, sueldos > 0), aes(y=sueldos))+
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,60000))+
  labs(title = "Salarios")

ggp2 <- ggplot(campo.adulto, aes(y=extra))+
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,60000))+
  labs(title = "Ingresos extra")

grid.arrange(ggp1, ggp2, ncol = 2, top = textGrob("Promedio tipo de ingresos fuera de la ciudad (individuos mayores a 25 años)"))

#---------------------------------------------------------------------------
#mostramos boxplot con porcentaje de ingresos no salariales frente a total ingresos
ggplot(ciudad.adulto, aes(y=porcentaje.extra, fill=Escolaridad.jefe))+
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,100))+
  labs(y = "%", title = "Porcentaje de los ingresos totales no correspondiente a salario en la ciudad (individuos mayores a 25 años)")

ggplot(campo.adulto, aes(y=porcentaje.extra, fill=Escolaridad.jefe))+
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,100))+
  labs(y = "%", title = "Porcentaje de los ingresos totales no correspondiente a salario fuera de la ciudad (individuos mayores a 25 años)")

#---------------------------------------------------------------------------
#relacion entre edad y sueldo separado por nivel de estudios
ggplot(subset(ciudad.adulto, sueldos > 0), 
                       aes(x=edad_jefe,y=sueldos)) + 
   geom_point() + 
   geom_smooth(method = "lm")+
   facet_wrap( ~ Escolaridad.jefe)+
    labs(x = "Edad", y = "Sueldo",
         title = "Relacion entre edad y sueldo en ciudad separado por nivel de estudios (individuos mayores a 25 años)")

campo.adulto$sueldos <- as.integer(campo.adulto$sueldos)
ggplot(subset(campo.adulto, sueldos > 0), 
       aes(x=edad_jefe,y=sueldos)) + 
  geom_point() + 
  #geom_smooth(method = "lm")+
  facet_wrap( ~ Escolaridad.jefe)+
  labs(x = "Edad", y = "Sueldo",
       title = "Relacion entre edad y sueldo en ciudad separado por nivel de estudios (individuos mayores a 25 años)")
#===========================================================================
#ANOVA de relacion entre ingresos extra y escolaridad
anov<-aov(extra~Escolaridad.jefe,data = ciudad.adulto)
summary(anov)

scheffe <- scheffe.test(anov,"Escolaridad.jefe",group = F)
scheffe<-as.data.frame(scheffe$comparison)
kable(scheffe)

anov<-aov(extra~Escolaridad.jefe,data = campo.adulto)
summary(anov)

scheffe <- scheffe.test(anov,"Escolaridad.jefe",group = F)
scheffe<-as.data.frame(scheffe$comparison)
kable(scheffe)

#ANOVA de relacion entre dependencia a ingresos extra y escolaridad
attach(ciudad.adulto)
ciudad <- lm(sueldos ~ Escolaridad.jefe + tam_loc + edad_jefe)
summary(ciudad)

attach(campo.adulto)
campo <- lm(sueldos ~ Escolaridad.jefe + tam_loc + edad_jefe)
summary(ciudad)

#Calculamos rango intercuartil para demostraar que la variabilidad de los datos es muy alta
#eso dificulta ajustar modelos precisos
IQR(subset(ciudad.adulto, educa_jefe == 8)$sueldos)
IQR(subset(ciudad.adulto, educa_jefe == 10)$sueldos)
IQR(subset(ciudad.adulto, educa_jefe == 11)$sueldos)

IQR(subset(campo.adulto, educa_jefe == 8)$sueldos)
IQR(subset(campo.adulto, educa_jefe == 10)$sueldos)
IQR(subset(campo.adulto, educa_jefe == 11)$sueldos)



#---------------------------------------------------------------------------
#se plotean graficas usando como parametro el nivel socioeconomico proporcionado por la encuesta
#del inegi
ggplot(ciudad.adulto)+geom_bar(aes(x = educa_jefe, fill=Escolaridad.jefe)) +
  scale_fill_manual(values = mycolors)+
  labs(x = "Escolaridad", y = "Numero individuos",
       title = "Escolaridad en la ciudad separado por nivel socioecónomico (individuos mayores a 25 años)")+
  facet_wrap( ~ Estado.socio)

ggplot(campo.adulto)+geom_bar(aes(x = educa_jefe, fill=Escolaridad.jefe)) +
  scale_fill_manual(values = mycolors)+
  labs(x = "Escolaridad", y = "Numero individuos",
       title = "Escolaridad fuera de la ciudad separado por nivel socioecónomico (individuos mayores a 25 años)")+
  facet_wrap( ~ Estado.socio)

#Se llegó a la concusion que el utilizar los campos de nivel socioeconomico proporcionados por el
#inegi proporcionaria resultados sesgados debido a la manera en que determinan dicho campo.
ggplot(subset(ciudad.adulto, sueldos > 0), aes(y=sueldos, fill=Escolaridad.jefe))+
  geom_boxplot(outlier.shape = NA) +
  facet_wrap( ~ Estado.socio)+
  coord_cartesian(ylim = c(0,100000))+
  labs(title = "Sueldos en la ciudad separados por nivel socioecónomico (individuos mayores a 25 años)")

ggplot(subset(campo.adulto, sueldos > 0), aes(y=sueldos, fill=Escolaridad.jefe))+
  geom_boxplot(outlier.shape = NA) +
  facet_wrap( ~ Estado.socio)+
  coord_cartesian(ylim = c(0,100000))+
  labs(title = "Sueldos fuera de la ciudad separados por nivel socioecónomico (individuos mayores a 25 años)")
