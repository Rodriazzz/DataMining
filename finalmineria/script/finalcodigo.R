rm(list = ls())
gc()
library(ggplot2)
library(dplyr)


data <- read.csv("C:/Users/rodri/Desktop/finalmineria/datos/e-shop clothing 2008.csv", sep=";")

#Cómo se distribuyen los clicks por sesión

#cantidad de click por session
frecuencia_session <- data %>%
  count(session.ID)

#ver cuartiles frecuencia_session
summary(frecuencia_session$n)
# me quedo con los datos que estan por debajo del tercer cuartil
frecuencia_productos_filtrado_2 <- frecuencia_session %>%
  filter(n <= 9)
# Crear un gráfico de barras para visualizar las frecuencias
ggplot(frecuencia_productos_filtrado_2, aes(x = n, y = session.ID)) +
  geom_bar(stat = "identity") +
  xlab("Session ID") + 
  ylab("Frequency") +
  ggtitle("Frequency of Each Session ID") +
  theme_minimal()


#Cómo se distribuyen las sesiones por país. Además de las sesiones provenientes de
#Polonia, ¿de qué países visitan en mayor medida el sitio?

#eliminar las filas donde se repite la session.id en data
paises <- data[!duplicated(data$session.ID),]

#ver el porcentaje de sesiones de los paises 29 9 y 46
paises %>%
  count(country) %>%
  mutate(percentage = n / sum(n) * 100)

#grafico de barras con session.id y country
ggplot(paises, aes(x=country)) + geom_bar()

#ver los 3 paises con mas sesiones
paises %>%
  count(country) %>%
  arrange(desc(n)) %>%
  head(3)

#filtrar los paises 29 9 y 46
paises_filtrados <- paises %>%
  filter(country == 29 | country == 9 | country == 46)
#convertir a factor
paises_filtrados$country <- as.factor(paises_filtrados$country)
ggplot(paises_filtrados, aes(x=country)) + geom_bar()

#ver el porcentaje de sesiones de los paises 29 9 y 46 
paises %>%
  count(country) %>%
  mutate(percentage = n / sum(n) * 100)
#CONCLUSION:polonia es el pais con mas sesion con el %81 seguido por 
#el pais 9(Czech Republic) con el %0.28 y por ultimo el pais 46 con el %0.9


#La evolución de los clicks en el transcurso de los meses estudiados. 
#Agrupamos la cantidad de clicks por mes
clicks_por_mes <- data %>%
  group_by(month) %>%
  summarise(clicks = n())

#Graficar la evolución de la cantidad de clicks por mes
ggplot(clicks_por_mes, aes(x = month, y = clicks)) +
  geom_line() +
  xlab("Month") +
  ylab("Clicks") +
  ggtitle("Evolution of Clicks per Month") +
  theme_minimal()

#La evolución de los clicks en el transcurso de los meses estudiados se ve 
#una disminucion de los clicks a medida que avanzan los meses

#b) Explore los datos. ¿Qué tipo de variables contiene? (describa y grafique)

#ver los tipos de variables
str(data)
#tiene 165474 transacciones
#contar valores unicos de la variable session.id
length(unique(data$session.ID))
#contiene 24026 sesiones

#nombre de las columnas
colnames(data)
#graficar page.1..main.category.
ggplot(data, aes(x=page.1..main.category.)) + geom_bar()
#las categorias tienen cantidades similares, solo la categoria 1 sobresale un poco
#ver los 3 item mas repetidos de la columna page.2..clothing.model.
data %>%
  count(page.2..clothing.model.) %>%
  arrange(desc(n)) %>%
  head(3)
#los 3 productos mas usados son b4 A2 y A11
#las transacciones tienen un formato single
#year/mont/day son variables de tipo datetime
#location,page1, colour, country y page2 es de tipo factor
rm(list = ls())
gc()
library(data.table)
library(caret)
library(dplyr)
library(arulesSequences)
library(ggplot2)
#library(ggthemes)
library(arules)
#library(treemapify)
library(stringr)


data <- read.csv("C:/Users/rodri/Desktop/finalmineria/datos/e-shop clothing 2008.csv", sep=";")
#data <- read.csv(".datos/e-shop clothing 2008.csv", sep=";")


#d) Encuentre el número de transacciones e ítems de los datos correspondientes a
#Polonia, utilizando un procesamiento adecuado. A partir del objeto de transacciones obtenido,
#encuentre los itemsets frecuentes para un soporte mínimo de 1,5% y una longitud mínima de 2
#ítems.
#29-Poland

data0 <- data[data$country == "29",]
#quedarme con las columnas sessin_id y page.1..main.category.
data1 <- data0[,c("session.ID","page.2..clothing.model.")]
summary(data1)
#verifico la cantidad de trnasacciones
#eliminar duplicados de la columna session.ID
data1_prue <- data1[!duplicated(data1$session.ID),]

transactions <- as(split(data1$page.2..clothing.model., data1$session.ID), "transactions")
transactions
#transacciones: 19582
#ítems 217
soporte <- 0.015
frecuencia_polaca <- apriori(transactions, 
                         parameter = list(support = soporte,
                                          minlen = 2, 
                                          target = "frequent itemsets"), 
                         control = list(verbose=F))

itemsets <- sort(frecuencia_polaca, by = "support", decreasing = TRUE)
inspect(itemsets)


#e) Encuentre las reglas de asociación para las sesiones que provienen solamente de
#Polonia y tienen una confianza mínima de 60%, un soporte mínimo de 2% y una longitud de al
#menos 2 ítems. Muestre las 10 reglas de mayor lift.

reglas_polacas <- apriori(transactions, 
                         parameter = list(support = 0.02,
                                          
                                          confidence = 0.3,
                                          target = "rules"), 
                         control = list(verbose=F))
itemsets_pol <- sort(reglas_polacas, by = "lift", decreasing = TRUE)
inspect(itemsets_pol)

#f) Sobre las reglas obtenidas en el punto anterior, encuentre las que tienen un soporte
#mínimo de 2,5% y al elemento “A2” en el consecuente. Muestre las 10 reglas de mayor lift

reglas_polacas2 <- apriori(transactions, 
                         parameter = list(support = 0.025,
                                          
                                          confidence = 0.3,
                                          target = "rules"), 
                         control = list(verbose=F))
#RHS = consecuente
#LHS = antecedente

itemsets_pol_2 <- subset(reglas_polacas2, subset=rhs%in%c("A2"))
itemsets_pol_2 <- sort(itemsets_pol_2, by = "lift", decreasing = TRUE)
inspect(itemsets_pol_2)
