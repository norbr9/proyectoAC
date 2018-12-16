#Primero cargaremos las librerías necearias para la ejecución de las funciones que necesitaremos.
library(ggplot2)
library(lattice)
library(caret)
library(e1071)
library(ellipse)
library(doParallel)
library(rpart)
library(gbm)
library(MLmetrics)

```{r}
##para que no se ejecuten el cacho de código
library(ggplot2)
library(lattice)
library(caret)
library(e1071)
library(ellipse)
library(doParallel)
library(rpart)
library(gbm)
library(MLmetrics)
```

#Cargad de la base de datos



credit <-read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data", sep = ",", na.strings = "?", header = F)
colnames(credit) <- c("Male", "Age", "Debt", "Married", "BankCustomer", "EducationLevel", "Ethnicity", "YearsEmployed", "PriorDefaul", "Employed", "CreditScore", "DriversLicense", "Citizen", "ZipCode", "Income","Approved")

# Informacion de los atributos. Columnas
# Male:	b, a. 
# Age:	continuous. 
# Debt:	continuous. 
# Married:	u, y, l, t. 
# BankCustomer:	g, p, gg. 
# EducationLevel:	c, d, cc, i, j, k, m, r, q, w, x, e, aa, ff. 
# Ethnicity:	v, h, bb, j, n, z, dd, ff, o. 
# YearsEmployed:	continuous. 
# PriorDefaul:	t, f. 
# Employed:	t, f. 
# CreditScore: continuous. 
# DriversLicense:	t, f. 
# Citizen: g, p, s. 
# ZipCode: continuous. 
# Income: continuous. 
# Approved: +,- (Si ha sido aprobado, esta es la variable de salida)

# Resumen de los todos los datos
summary(credit)

###########Funciones auxiliares###########
qqplot.data <- function (vec)   
{        
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))   
  x <- qnorm(c(0.25, 0.75))                       
  slope <- diff(y)/diff(x)          
  int <- y[1L] - slope * x[1L]       
  d <- data.frame(resids = vec)     
  ggplot(d, aes(sample = resids)) + stat_qq() + geom_abline(slope = slope,intercept = int)   
}                                                                                               


####### Análisis variable Male #######
# Variable No Numérica                                                                                
# Resumen                          
summary(credit[1], maxsum = Inf)
# Numero de ocurrencias (porcentaje)    
porcent <- prop.table(table(credit$Male)) * 100
cbind(total=table(credit$Male), porcentaje=porcent) 
# Gráfico de barras    
barplot(table(credit$Male))

####### Análisis variable Age #######
# Variable Continua  
# Resumen    
summary(credit[2], maxsum = Inf)                                                             
# Numero de ocurrencias (porcentaje)     
porcent <- prop.table(table(credit$Age)) * 100   
cbind(total=table(credit$Age), porcentaje=porcent)                                             
# Histograma con frecuencias de aparicion


# Estimacion de la fucnión de desidad de probabilidad          
dens<-density(credit$Age,na.rm=T)                             
hist(credit$Age,xlab="",main="Age",ylim=c(0,max(dens$y)*1.1),probability =T) 
lines(dens)                                                                 
# rug() apra la repersentacion de lso valores reales del atributo, bajo el eje x                      
# jitter() para añadir un poco de ruido aleatorio a los valores verdaderos,                           
# por si hubiese valores repetidos                                                                    
rug(jitter(credit$Age))                                         
# Diagrama Box-Whisker. Informacion sobre la dispersion, asimetria estadistica y posibles valores atípicos
boxplot(credit$Age,main="Age")                                       
# Representacion de los percentiles con respecto a los de una normal centrada en cero 
qqplot.data(credit$Age)+ggtitle("Gráfica Q-Q para Age")

