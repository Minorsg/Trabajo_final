# Proyecto final
# Curso: Herramientas prácticas para investigación reproducible
# Estudiante: Minor Solano Gutiérrez
# II-2020


# Tema: Influencia de la complejidad estructural de la comunidad vegetal del entorno en la composición de especies de escamas armadas (Hemiptera: Diaspididae) y sus parasitoides, en palmas ornamentales en ambientes urbanos en Costa Rica


# Pregunta comleta de investigación
# ¿Cuál impacto tiene la complejidad estructural de la comunidad vegetal del entorno en la abundancia y diversidad de avispas parasitoides de escamas armadas (Hemiptera: Diaspididae) en palmas ornamentales en ambientes urbanos?


# Hipótesis
# La diversidad de especies de escamas Diaspididae y sus parasitoides Hymenoptera en palmas ornamentales variará según la complejidad estructural de la comunidad vegetal entorno, 
# donde en hábitats más complejos (p. e. parques) habrá una mayor diversidad (H') de especies, tanto de escamas, como de parasitoides, respecto a un hábitat más simple (p. e. bordes de calle o parqueos). 
# Pero no una mayor uniformidad de especies (J') (i.e. la proporción de la diversidad observada en cada hábitat, respecto a máxima diversidad posible, i.e. riqueza total), debido a las especies de especialistas de cada hábitat. 
# Donde, se espera menos diversidad, pero con abundancias más estables de las especies de especialistas en hábitats simples, respecto a mayor diversidad, pero con abundancias menores y más heterogéneas en hábitat complejos. 
# Esto podría explicar las razones por las cuáles zonas con una baja complejidad vegetal del entorno son más propensas a experimentar grandes incrementos en las poblaciones de escamas 
# y proponer métodos de manejo más sostenibles de poblaciones de estos insectos basados en aumentar la complejidad estructural de la comunidad vegetal del entorno.


#El número de plantas (V) influencia la complejidad estructural de la comunidad vegetal del entorno (x), 
#tanto el número de plantas como la complejidad estructural influye en la diversidad de escamas (HE) y esta en la abundancia (E) de estos insectos herbívoros. 
#Las escamas son hospederos de los parasitoides y causan mayor abundancia (A) y diversidad de parasitoides (y). 
#La diversidad de parasitoides y escamas a su vez permite una mayor abundancia de parasitoides (A), ya que se reduce la competencia por hospederos.
#La abundacia influye sobre la proporción de especialistas (S) y este a su vez sobre la uniformidad de especies (J)

# En este trabajo daremos énfasis a nuestra pregunta principalmente que busca responder si la complejidad estructural causa una mayor diversidad de parasitoides.

# Los datos se tomarán en 24 sitios en 9 momentos distintos (frecuencia mensual)
sitios1 <- rep("s", 216)
sitios2 <- rep(1:24, each = 1)
sitios3 <- rep(sitios2, 9)
sitio <- paste(sitios1, sitios3)

muestreos1 <- rep("m", 216)
muestreos2 <- rep(1:9, each = 24)
muestreo <- paste(muestreos1, muestreos2)

datos <- data.frame(muestreo, sitio)
str(datos)

#Crear DAG

library(ggdag) #cargar libreria ggdag

escamas <- dagify(x ~ V,
                  HE ~ V,
                  HE ~ x,
                  E ~ HE,
                  A ~ E,
                  y ~ HE,
                  S ~ E,
                  S ~ A,
                  J ~ S,
                  J ~ HE,
                  J ~ y,
                   A ~ y,
                 
                  
                  exposure = "x",
                  outcome = "y")
tidy_dagitty(escamas)
ggdag(escamas, layout = "circle") + theme_dag()


# enumerar covariables
adjustmentSets(x = escamas, exposure = "x", outcome = "y", type="all", effect = "total")

# Vemos que existe una relacion entre x y y a traves del pipe HE

# Datos simulados

# Compactar
td_dag <- tidy_dagitty(escamas)


# Controlamos por el fork V

# d relativos
d_node <- node_dseparated(td_dag, "x", "y", controlling_for = "V")

# grafico DAG compactado
ggplot(d_node, aes(
  x = x, 
  y = y, 
  xend = xend, 
  yend = yend, 
  shape = adjusted, 
  col= d_relationship
)) +
  geom_dag_edges_link()+
  geom_dag_point() +
  geom_dag_text(col = "white") +
  theme_dag() + 
  scale_adjusted() +
  expand_plot(expand_y = expansion(c(0.2, 0.2))) +
  scale_color_manual(values = c("Black", "White"), 
                     name = "d-relationship", 
                     na.value = "grey85"
  )

# Implicaciones de independencia.

impliedConditionalIndependencies(x = escamas, type = "missing.edge")

impliedConditionalIndependencies(x = escamas, type = "basis.set")


# Simulación de las relaciones de dependencia específicadas:

# Los datos son conteos, por lo que son datos discretos y dado esto no simularemos datos siguiendo una distribución normal
# Importante aclarar que en los datos reales podría haber sobre dispersión, lo que es más posible por lo que se simulará con una distribución binomial negativa

# V = número de palmas (plantas hospederas del insecto fitófago) en el sitio de muestreo 
#En el caso de V, no pueden haber datos con cero palmas, porque haber palmas es un requisito

library(extraDistr)

V <- rtpois(24, lambda = 4, a = 1, b = 6)


# x = complejidad estructural de la comunidad vegetal del entorno
# Se utilizará se utilizará un índice de complejidad estructural ("SCI", por sus siglas en inglés; Shrewsbury & Raupp, 2000, 2006) 
# Este es un índice tridimensional del hábitat en estudio el cual consiste de un área de 10 x 10 m alrededor de la planta en estudio. 
# La tercera dimensión consiste en cinco estratos vegetales comunes en los paisajes urbanos (Figura 1). Estos estratos son: 
# (1) cobertura del suelo o césped, 
# (2) plantas herbáceas, 
# (3) arbustos, 
# (4) árboles de piso inferior y 
# (5) árboles del piso superior.  
# Esto hace una cuadrícula tridimensional de 10 x 10 m (100 m2) por cinco estratos de vegetación (100 m2 x 5 estratos = 500 cuadrados en total). 
# El número total de cuadrados con vegetación representará el SCI para ese paisaje (SCI máximo = 500). Un SCI < 125 = un paisaje simple y un SCI > 175 = un paisaje complejo. 

x <- rnbinom(n = 24, mu = 40, size = 5)*V # x depende de V 


# Los muestreos se harán durante 9 momentos distintos, por lo que se simularán datos de diversidad para cada momento de muestreo
# HE = diversidad de escamas, en este caso diversidad si se podría simular con una distribución normal, ya que si se tiene un conjunto de índices de diversidad replicados de diferentes sitios, el índice de diversidad podría considerarse como observaciones

HE.err1 <- rnorm(n = 24, mean = 0, sd = 0.5)
HE1 <- 0 + 0.5*V + 0.5*x + HE.err1 # E depende de V y de x

HE.err2 <- rnorm(n = 24, mean = 0, sd = 0.6)
HE2 <- 0 + 0.5*V + 0.5*x + HE.err2

HE.err3 <- rnorm(n = 24, mean = 0, sd = 0.4)
HE3 <- 0 + 0.5*V + 0.5*x + HE.err3

HE.err4 <- rnorm(n = 24, mean = 0, sd = 0.5)
HE4 <- 0 + 0.5*V + 0.5*x + HE.err4

HE.err5 <- rnorm(n = 24, mean = 0, sd = 0.3)
HE5 <- 0 + 0.5*V + 0.5*x + HE.err5

HE.err6 <- rnorm(n = 24, mean = 0, sd = 0.7)
HE6 <- 0 + 0.5*V + 0.5*x + HE.err6

HE.err7 <- rnorm(n = 24, mean = 0, sd = 0.5)
HE7 <- 0 + 0.5*V + 0.5*x + HE.err7

HE.err8 <- rnorm(n = 24, mean = 0, sd = 0.6)
HE8 <- 0 + 0.5*V + 0.5*x + HE.err8

HE.err9 <- rnorm(n = 24, mean = 0, sd = 0.4)
HE9 <- 0 + 0.5*V + 0.5*x + HE.err9

HE <- c(HE1, HE2, HE3, HE4, HE5, HE6, HE7, HE8, HE9)

datos$HE <- HE
head(datos)

# y = diversidad de parasitoides 
y.err1 <- rnorm(n = 24, mean = 0, sd = 0.5)
y1 <- 0 + 0.75*HE1 + y.err1 # y depende de HE

y.err2 <- rnorm(n = 24, mean = 0, sd = 0.5)
y2 <- 0 + 0.75*HE1 + y.err2

y.err3 <- rnorm(n = 24, mean = 0, sd = 0.5)
y3 <- 0 + 0.75*HE1 + y.err3

y.err4 <- rnorm(n = 24, mean = 0, sd = 0.5)
y4 <- 0 + 0.75*HE1 + y.err4

y.err5 <- rnorm(n = 24, mean = 0, sd = 0.5)
y5 <- 0 + 0.75*HE1 + y.err5

y.err6 <- rnorm(n = 24, mean = 0, sd = 0.5)
y6 <- 0 + 0.75*HE1 + y.err6

y.err7 <- rnorm(n = 24, mean = 0, sd = 0.5)
y7 <- 0 + 0.75*HE1 + y.err7

y.err8 <- rnorm(n = 24, mean = 0, sd = 0.5)
y8 <- 0 + 0.75*HE1 + y.err8

y.err9 <- rnorm(n = 24, mean = 0, sd = 0.5)
y9 <- 0 + 0.75*HE1 + y.err9

y <- c(y1, y2, y3, y4, y5, y6, y7, y8, y9)

datos$y <- y
head(datos)

# Como se consideró en la simulación de diversidad, la complejidad de la comunidad vegetal del entorno y el número plantas hospederas del fitófago posible se mantenga en cada momento de muestreo
#Las incluímos al data frame
V <- rep(V, 9)
datos$V <- V
x <- rep(x, 9)
datos$x <- x

head(datos)
str(datos)
summary(datos)

#Importante recordar que según nuestro DAG, hay relacion entre x y y a traves del pipe HE y que deberiamos poder detectar si controlamos por V:

# Exploración de distribución con histográmas

par(mfrow = c(1, 3))
hist(datos$x)
hist(datos$V)
hist(datos$y)

#Se observa un distribución no normal




