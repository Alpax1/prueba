library(readr)
library(FactoMineR)
library(factoextra)
library(dplyr)
library(readxl)
indicadores3 <- read_excel("indicadores3.xlsx")

#Para la realización de la práctica final de la asignatura, debéis utiliza la base de datos
#denominada indicadores3.xlsx. Se trata de un conjunto de indicadores de carácter socioeconómicos por países, expresados en distintas unidades de medidas (porcentaje,
#índices, dólares, etc.). Se pide:
  #I. Realizar una reducción de la dimensionalidad, realizando la interpretación de
  #los factores. Se debe justificar la elección del número de factores a partir de
  #la aplicación del test de Barlett, KMO, etc. Se deben extraer conclusiones
  #sobre los resultados obtenidos.

indicadores <- data.frame(indicadores3[,-1], row.names = indicadores3$country)
indicadores$Class <- as.factor(indicadores$Class)
indicadores$Region = NULL
indicadores <- na.omit(indicadores)

acp <- PCA(indicadores %>% select_if(is.numeric))
acp$eig

#Podemos ver los valores propios y su porcentaje de varianza, a priori, vemos
#que 8 componentes podemos explicar hasta un 80% de la varianza

cor.mat = round(cor(indicadores %>% select_if(is.numeric)),2)
View(cor.mat)

require(corrplot)
corrplot(cor.mat, type="lower", order="original", 
         tl.col="black", tl.cex=0.7, tl.srt=45)
#El gráfico nos muestra que las variables se encuentran considerablemente correlacionadas
#por lo que si que es conveniente el analisis de factores

require(Hmisc)
cor.mat.nds= rcorr(as.matrix(indicadores%>% select_if(is.numeric)))
cor.mat.nds
a <- as.data.frame(cor.mat)

fviz_eig(acp, addlabels=TRUE, hjust = -0.3)+
  labs(title="Scree plot / Gráfico de sedimentacion", x="Dimensiones", y="% Varianza explicada")+
  theme_minimal()

#La gráfica nos muestra la varianza explicada por cada componente, podemos ver que con
#tan solo 3 componentes somos capaces de explicar más del 50% de la varianza

##Test de Bartlett
require(psych)
print(cortest.bartlett(cor.mat, n=173))

#Con un p-value = 0 rechazamos la hipótesis nula de que las variables no se encuentran
#correlacionadas, y existen factores comunes
KMO(cor.mat)

#El MSA para la muestra completa es de 0,7, por lo que se lo que podemos considerarla
#una muestra adecuada para el análisis factorial; aun así, el KMA individual de cada variable
#nos indica que hay ciertas variables que estarán peor representadas en el análisis
#factorial, que serán las que su MSA individual sea menor que 0,7.

indicadores2 <- indicadores
indicadores2$Class = NULL
library(paran)
paran(indicadores2, iterations=5000,graph=TRUE,color=FALSE)

#El test de Horn que retengamos 5 componentes, pero al saber que con 8 componentes
#podemos explicar hasta el 80%, rechazamos dicho test.








  #II. Análisis clustering, en el que se tendrá especial atención a las diferentes
#unidades de medida. Se desarrollará tanto un análisis jerárquico, como un no
#jerárquico, para una submuestra de los países europeos, así como un análisis
#no jerárquico para todos los países. Se deben extraer conclusiones sobre los
#resultados obtenidos.

indicadores_europe <- filter(indicadores3, Region == "Europe")
europe <- data.frame(indicadores_europe[,-1], row.names = indicadores_europe$country)
europe_tip <- as.data.frame(scale(europe %>% select_if(is.numeric)))


matriz.dis.euclid<- dist(europe_tip, method = "euclidean", diag=TRUE)
                         
##Realizamos un análisis jerárquico de los países europeos mediante el método del encadenamiento promedio

library(NbClust)
res.average<-NbClust(europe_tip, distance = "euclidean", min.nc=2, max.nc=15, method = "average", index = "alllong")

#Vemos que el número de clusters mas adecuado es dos

hclust.average<-hclust(matriz.dis.euclid,method="average")
hclust_average <- round(data.frame(hclust.average[2:1]), 2)

rect.hclust(hclust.average, k = 2, border = "red")

fviz_dend(hclust.average, k = 2, cex = 0.5, main = "Average Method")

#El primer cluster se encuentra formado por Rusia, Bielorrusia y Ucrania. Bielorrusia y
#Ucrania se unen a una altura
#de 5'7, siendo esta la distancia promedio existente entre estos dos países. 
#Posteriormente, los dos países se unen a Rusia, y podemos ver que la distancia promedio
#entre Rusia, y el grupo ya formado por Bielorrusia y Ucrania, es de 
#aproximadamente 7'4, con la que terminan formando el primer cluster.

names <- c("Russia", "Belarus", "Ukraine")
x <- subset(europe, rownames(europe) %in%  names)

#Nos resulta interesante aislar los países que conforman el primer cluster del resto de países, en un nuevo
#dataframe, en el que nos podemos fijar en que comparten valores similares en ciertas variables como pueden ser
#la carga fiscal (tax_burden), la libertad de comercio exterior (trade freedom), 
#la libertad de inversión (investent freedom),  o el equilibrio fiscal (fiscal health)

#A su vez, podemos ver que el segundo cluster esta formado por los 41 países europeos restantes, agrupados en 
#diferentes subgrupos. 40 de los 41 países conforman un subgrupo que se encuentra subdividido, al que posteriormente 
#se le une Irlanda, que se encuentra aislada, a una distancia superior a 7,5. Sabemos que en el caso de haber cogido
#un cluster adicional para hacer este análisis, dicho cluster habría estado conformado tan solo por Irlanda.
#Irlanda posee una carga fiscal muy inferior a muchos países, y posee el mayor ratio de crecimiento del PIB de todo
#Europa



##Ward 
res.wardD2<-NbClust(europe_tip, distance = "euclidean", min.nc=2, max.nc=10, method = "ward.D2", index = "alllong")

hclust.ward<-hclust(matriz.dis.euclid,method="ward.D2")
hclust_ward <- round(data.frame(hclust.ward[2:1]),2)

rect.hclust(hclust.ward, k = 3, border = "red")


fviz_dend(hclust.ward, k = 3, cex = 0.5, main = "Ward Method")

#Vemos que los 3 clusters seleccionados se distribuyen de manera que queda un cluster de 15 países, otro cluster de
#21 países, y un cluster de 8 países. Los clusters mostrados realizan cada uno una agrupación de los países 
#que maximiza la homogeneidad intra-cluster. Asíe cada cluster se encuentra formado de manera que se
#minimice la suma de cuadrados de cada cluster, es decir, la distancia euclídea de cada país respecto a la 
#media de su grupo es mínima.
#Vemos que tanto el primer como el tercer cluster, unifican sus grupos a una distancia de 12,02, mientras que el 
#segundo cluster se termina de agrupar a una distancia inferior, de 11'4.
#Posteriormente, el segundo cluster y el tercero se unen a una distancia aproximada de 17.

#No jerarquico Europa
grupo_ward <- cutree(hclust.ward, k = 3, h = NULL)

no_jerarquico<-cbind(europe_tip,grupo_ward)
no_jerarquico <- print(round((no_jerarquico),2))
print(no_jerarquico)

centroides <- round(aggregate(no_jerarquico,list(grupo_ward), mean ),2)

list2env(setNames(split(as.matrix(centroides),
                        row(centroides)),
                  paste0("C",1:3)), 
         envir=.GlobalEnv)


no_jerarquico.kmeans <- no_jerarquico
no_jerarquico.kmeans$grupo_ward<-NULL
no_jerarquico.kmeans

a <- rbind(C1, C2, C3)
b <- a[,-28]
c <- b[,-1]

c[is.na(c)] <- 0

solucion <- kmeans(na.omit(no_jerarquico.kmeans), c)
print(solucion)
no_jerarquico.kmeans2 <- data.frame(na.omit(no_jerarquico.kmeans, solucion$cluster))
fviz_cluster(solucion, data = na.omit(europe_tip))



#El gráfico mostrado agrupa en un mismo cluster a Rusia, Turquía, Ucrania, Moldavia y 
#Bielorrusia, que se caracterizan por un índice bajo de libertad de inversión.
#Estos países comparten características económicas, así como una cercanía geográfica.

#En el cluster compuesto por España, Reino Unido, Portugal y el resto de países, sabemos 
#que son países con un nivel alto de gasto público. Siendo la mayoría países pertenecientes
#a la Unión Europea que se encuentran en una situación económica favorable.

#El cluster de color verde incluye países como Armenia, Irlanda, o Albania, que poseen
#un nivel de gasto público más elevado que el resto de países; y que poseen una presión 
#fiscal alta, a excepción de Irlanda, donde la presión fiscal no es tan elevada, por eso
#se encuentra más alejada del resto de países.


#Analisis no jerarquico de todos los países
indicadores_tip <- as.data.frame(scale(indicadores %>% select_if(is.numeric)))

matriz2<- dist(indicadores_tip, method = "euclidean", diag=TRUE)
res.wardD2<-NbClust(indicadores_tip, distance = "euclidean", min.nc=2, max.nc=10, method = "ward.D2", index = "alllong")

hclust.ward<-hclust(matriz2 ,method="ward.D2")
hclust_ward <- round(data.frame(hclust.ward[2:1]),2)

grupo_ward2 <- cutree(hclust.ward, k = 5, h = NULL)

no_jerarquico<-cbind(indicadores_tip,grupo_ward2)
no_jerarquico <- print(round((no_jerarquico),2))
print(no_jerarquico)

centroides2 <- round(aggregate(no_jerarquico,list(grupo_ward2), mean ),2)

list2env(setNames(split(as.matrix(centroides2),
                        row(centroides2)),
                  paste0("C",1:5)), 
         envir=.GlobalEnv)


no_jerarquico.kmeans <- no_jerarquico
no_jerarquico.kmeans$grupo_ward2<-NULL
no_jerarquico.kmeans

z <- rbind(C1, C2, C3, C4, C5)
y <- z[,-28]
x <- y[,-1]

is.na(c) <- 0

solucion <- kmeans(na.omit(no_jerarquico.kmeans), x)
print(solucion)
no_jerarquico.kmeans2 <- data.frame(na.omit(no_jerarquico.kmeans, solucion$cluster))
fviz_cluster(solucion, data = na.omit(indicadores_tip))

#Podemos ver que existe un cluster formado tan solo por Estados Unidos, China, 
#e India, que son los países que poseen mayor población y mayor PIB.

#Mientras que la gran mayoría de países de África se encuentra agrupada en un 
#cluster, que recoge países poco desarrollados, junto a otros países como Filipinas o 
#Argentina, que son países en los que existe un elevado nivel de desempleo, 
#y no atraen a la inversión

#El cluster de color amarillo recoge países tanto de Latinoamérica, como países 
#Asiáticos, algún país Europeo y ciertos países de África. Estos se pueden corresponder
#con países que se encuentran en vías de desarrollo pero que todavía no se consideran
#de primer mundo, y en los que existe una gran desigualdad entre los ricos y los pobres.
#En la mayoría de estos países existe una carga fiscal bastante elevada

#En el cluster de color verde se encuentran los países que podemos considerar más
#desarrollados, a excepción de EEUU, China e India que se encuentran en un cluster aislado
#Los países de este cluster tienen un nivel económico elevado, se caracterizan por la 
#existencia de buenas relaciones comerciales (la mayoría son países de la UE) siendo estos
#países son los que mayor libertad económica tienen. A su vez, el cluster incluye países
#como pueden ser Grecia, Portugal e Italia, que poseen un índice alto de deuda pública.

#Por último podemos ver que Venezuela se encuentra aislada, al ser un país que 
#se enceuntra en una profunda crisis económica, teniendo la mayor tasa de inflación,
#y un índice muy bajo de protección de los derechos a la propiedad privada
#debido a las expropiaciones.






##III. Estimar un árbol de clasificación, sobre la variable Class. Se deberá justificar e
#interpretar adecuadamente el podado y la evaluación. Se debe analizar las
#distintas medidas de ajuste y precisión a partir de la matriz de confusión e
#interpretar los resultados. Se debe valorar la existencia de sobreajuste del
#árbol estimado. Asimismo se deben extraer conclusiones sobre la
#interpretación del árbol. Nota: se debe considerar la semilla aleatoria
#seed(1234) y un reparto 80-20 en muestra de entrenamiento y muestra de
#validación.

set.seed(1234)
indicadores3<- read_excel("indicadores3.xlsx")
datos <- na.omit(indicadores3 %>% dplyr::select(-GDP_per_Capita))
datos$Class <- factor(datos$Class)
datos <- data.frame(datos[,-1], row.names = datos$country)
datos$Region = NULL

sample_ind <- sample(nrow(datos),nrow(datos)*0.80)
train<-datos[sample_ind,]
test<-datos[-sample_ind,]

library(rpart.plot)
arbol <- rpart(Class ~ ., data = train, method = "class",
               control = rpart.control(cp = 0))

rpart.plot(arbol)
arbol

#En el árbol obtenido podemos observar que la primera división tiene lugar
#sobre los derechos de propiedad, que nos indica cómo es el índice de derechos
#de propiedad en cada país. Cuanto mayor sea este índice
#existirá una mayor eficiencia económica en un país.
#Este criterio divide la muestra entre los que poseen un índice menor o 
#mayor de 50.

#Para un índice de derechos de la propiedad superior a 50, el 84% de países 
#serán ricos, y el 16% serán pobres. Posteriormente, para este grupo existe
#un nodo que divide países según la libertad de comercio, si este índice es
#mayor que 74, el 4,9% de los países serán pobres, y el 95,1% serán ricos,
#mientras que si el índice es menor que 74, el 20% serán ricos y el 80% pobres

#Para un índice de derechos de la propiedad inferior a 50, el 21% de países 
#serán ricos, y el 79% serán pobres.
#Existe un nodo inferior que divide los países con un índice de derechos de propiedad
#menor de 50 según el índice de libertad de negocio
#Si este índice es mayor que de 66, 41% de los países serán ricos; 
#mientras que si el índice es inferior a 66, obtenemos que tan solo el 12% serán
#ricos, y el 88% pobres.
#Este último grupo de países con derechos de propiedad menor que 50, y libertad
#de negocio inferior a 66, se subdivide en un nodo que distingue entre países
#con Impuesto de Sociedades menor o mayor que 24.
#Para un IS mayor de 24, tan solo el 6% de los países serán ricos, mientras que
#para un IS menor de 24 habrá un 42% de países pobres.

printcp(arbol)
plotcp(arbol)

# Calculamos la precisión del árbol
test$pred <- predict(arbol, test, type = "class")
base_accuracy <- mean(test$pred == test$Class)

#Obtenemos una precisión del 60% 


# Podamos el arbol a partir del parametro de complejidad, optimo cp
arbol.podado <- prune(arbol, cp=0)

prp(arbol.podado, type = 2, extra = 104,
    fallen.leaves = TRUE, main="Decision Tree")
rpart.plot(arbol.podado)

summary(arbol.podado)

#Con la funcion summary, observamos la importancia a la hora de predecir de cada 
#variable independiente sobre la distinción de un pais como rico o pobre. 
#Podemos ver que las variables con mayor capacidad de predecir son los derechos
#de propiedad, libertad de empresa y libertad de comercio, dado que son factores relevantes
#sobre la economía de un país.

test$pred <- predict(arbol.podado, test, type = "class")
accuracy_postprun <- mean(test$pred == test$Class)
data.frame(base_accuracy, accuracy_postprun)

#Obtenemos una precisión del 60% tanto para el árbol podado como el árbol sin podar.

library(caret)
class <- as.factor(test$Class)
confusionMatrix(test$pred,test$Class)

#Podemos ver que en la matriz de confusión existe una especificidad de 0,75, que nos indica 
#que existe una proporción entre los casos negativos bien clasificados por el modelo 
#respecto del total de negativos del 75%
#La sensibilidad es de 0,47, lo que nos dice que la proporción de casos positivos bien 
#clasificados por el modelo respecto al total de positivos es del 47%.
#El modelo tiene una precisión del 60%. Es decir, la proporción entre los positivos reales
#estimados por el algoritmo y todos los casos positivos es del 60%.

library(Epi)
ROC(data=train, form=Class~.)
#Hacemos uso de la curva ROC para ver la capacidad predictiva de la muestra de entreno.
#Obtenemos la medida de Área debajo de la curva, con un 0,933
#Esta medida nos indica la precisión, por lo que cuanto más cerca esté de 1 será más precisa

#El punto de corte óptimo llega a una sensibilidad del 87.8% y una especificidad de 89.1% 
#Esto último nos indica que el modelo sobre la muestra de entrenamiento tiene una alta capacidad
#predictora.

#La diferencia entre la sensibilidad y especificidad de la matriz de confusión
#(0,47 y 0,75 respectivamente), y las de la curva ROC, nos indica que no es posible 
#generalizar el modelo ROC, ya que la diferencia entre estas medidas es muy alta.

#Observamos un problema de sobreajuste dado que la muestra de entrenamiento 
#no se no está bien reflejada en la muestra validación. 
#Podemos ver que esto ocurre dado que el modelo encaja perfectamente tan solo
#sobre  la muestra de entrenamiento y NO lo hace sobre la de validación.
