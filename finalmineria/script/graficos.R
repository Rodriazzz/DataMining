
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
