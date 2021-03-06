# Proyecto final
# Curso: Herramientas pr�cticas para investigaci�n reproducible
# Estudiante: Minor Solano Guti�rrez
# II-2020


# Tema: Influencia de la complejidad estructural de la comunidad vegetal del entorno en la composici�n de especies de escamas armadas (Hemiptera: Diaspididae) y sus parasitoides, en palmas ornamentales en ambientes urbanos en Costa Rica


# Pregunta comleta de investigaci�n
# �Cu�l impacto tiene la complejidad estructural de la comunidad vegetal del entorno en la abundancia y diversidad de avispas parasitoides de escamas armadas (Hemiptera: Diaspididae) en palmas ornamentales en ambientes urbanos?


# Hip�tesis
# La diversidad de especies de escamas Diaspididae y sus parasitoides Hymenoptera en palmas ornamentales variar� seg�n la complejidad estructural de la comunidad vegetal entorno, 
# donde, en h�bitats m�s complejos (p. e. parques) habr� una mayor diversidad (H') de especies, tanto de escamas, como de parasitoides, respecto a un h�bitat m�s simple (p. e. bordes de calle o parqueos). 
# Pero no una mayor uniformidad de especies (J') (i.e. la proporci�n de la diversidad observada en cada h�bitat, respecto a m�xima diversidad posible, i.e. riqueza total), debido a las especies de especialistas de cada h�bitat. 
# Por lo que, se espera menos diversidad, pero con abundancias relativas m�s estables por la proporci�n de especialistas en h�bitats simples, respecto a mayor diversidad, pero con abundancias relativas menores y m�s heterog�neas en h�bitat complejos. 
# Esto podr�a explicar las razones por las cu�les zonas con una baja complejidad vegetal del entorno son m�s propensas a experimentar grandes incrementos en las poblaciones de escamas 
# y proponer m�todos de manejo m�s sostenibles de poblaciones de estos insectos basados en aumentar la complejidad estructural de la comunidad vegetal del entorno.


#El n�mero de plantas (V) influencia la complejidad estructural de la comunidad vegetal del entorno (x), 
#tanto el n�mero de plantas como la complejidad estructural influye en la diversidad de escamas (HE) y esta en la abundancia (E) de estos insectos herb�voros. 
#Las escamas son hospederos de los parasitoides y una mayor abundanacia (E) causan mayor abundancia de parasitoides (A) y una mayor diversidad de escamas (HE) y mayor diversidad de parasitoides (y). 
#La abundacia influye sobre la proporci�n de especialistas (S) y este a su vez sobre la uniformidad de especies (J)

# En este trabajo daremos �nfasis a nuestra pregunta principalmente que busca responder si la complejidad estructural causa una mayor diversidad de parasitoides.



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

library(dagitty) #cargar libreria dagitty 

adjustmentSets(x = escamas, exposure = "x", outcome = "y", type="all", effect = "total")

# Vemos que existe una relacion entre x y y a traves del pipe HE


# Compactar
td_dag <- tidy_dagitty(escamas)


# Controlaremos por el fork V

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


#Los datos se tomar�n en 24 sitios en 9 momentos distintos (frecuencia mensual), por lo que, los datos ser�n simulados partiendo de esta premisa

#Simularemos los datos

#Primero generar un data frame con la informaci�n de sitios y momentos de muestreo
sitios1 <- rep("s", 216)
sitios2 <- rep(1:24, each = 1)
sitios3 <- rep(sitios2, 9)
sitio <- paste(sitios1, sitios3)

muestreos1 <- rep("m", 216)
muestreos2 <- rep(1:9, each = 24)
muestreo <- paste(muestreos1, muestreos2)

datos <- data.frame(muestreo, sitio)
str(datos)

#Hay un espacio innecesario en los datos de muestreo

quitarespacio <- function(x, y, datos){
  datos
  searchString <- (x) 
  replacementString <- (y)
  datos = gsub(searchString,replacementString,datos)
}
datos$muestreo <- quitarespacio("m 1", "m1", datos$muestreo) 
datos$muestreo <- quitarespacio("m 2", "m2", datos$muestreo) 
datos$muestreo <- quitarespacio("m 3", "m3", datos$muestreo) 
datos$muestreo <- quitarespacio("m 4", "m4", datos$muestreo) 
datos$muestreo <- quitarespacio("m 5", "m5", datos$muestreo) 
datos$muestreo <- quitarespacio("m 6", "m6", datos$muestreo)
datos$muestreo <- quitarespacio("m 7", "m7", datos$muestreo)
datos$muestreo <- quitarespacio("m 8", "m8", datos$muestreo) 
datos$muestreo <- quitarespacio("m 9", "m9", datos$muestreo) 



# Simulaci�n de las relaciones de dependencia espec�ficadas:

# Los datos son conteos, por lo que son datos discretos y dado esto no simularemos datos siguiendo una distribuci�n normal
# Importante aclarar que en los datos reales podr�a haber sobre dispersi�n, lo que es m�s posible por lo que se simular� con una distribuci�n binomial negativa

# V = n�mero de palmas (plantas hospederas del insecto fit�fago) en el sitio de muestreo 
#En el caso de V, no pueden haber datos con cero palmas, porque haber palmas es un requisito

library(extraDistr)

V <- rtpois(24, lambda = 4, a = 1, b = 6)


# x = complejidad estructural de la comunidad vegetal del entorno
# Se utilizar� se utilizar� un �ndice de complejidad estructural ("SCI", por sus siglas en ingl�s; Shrewsbury & Raupp, 2000, 2006) 
# Este es un �ndice tridimensional del h�bitat en estudio el cual consiste de un �rea de 10 x 10 m alrededor de la planta en estudio. 
# La tercera dimensi�n consiste en cinco estratos vegetales comunes en los paisajes urbanos (Figura 1). Estos estratos son: 
# (1) cobertura del suelo o c�sped, 
# (2) plantas herb�ceas, 
# (3) arbustos, 
# (4) �rboles de piso inferior y 
# (5) �rboles del piso superior.  
# Esto hace una cuadr�cula tridimensional de 10 x 10 m (100 m2) por cinco estratos de vegetaci�n (100 m2 x 5 estratos = 500 cuadrados en total). 
# El n�mero total de cuadrados con vegetaci�n representar� el SCI para ese paisaje (SCI m�ximo = 500). Un SCI < 125 = un paisaje simple y un SCI > 175 = un paisaje complejo. 

x <- rnbinom(n = 24, mu = 40, size = 5)*V # x depende de V 


# Los muestreos se har�n durante 9 momentos distintos, por lo que se simular�n datos de diversidad para cada momento de muestreo
# HE = diversidad de escamas, en este caso diversidad si se podr�a simular con una distribuci�n normal, ya que si se tiene un conjunto de �ndices de diversidad replicados de diferentes sitios, el �ndice de diversidad podr�a considerarse como observaciones
# Le damos mayor peso a su dependencia con V

HE.err1 <- rnorm(n = 24, mean = 0, sd = 0.5)
HE1 <- 0 + 0.06*V + 0.05*x + HE.err1 # E depende de V y de x

HE.err2 <- rnorm(n = 24, mean = 0, sd = 0.6)
HE2 <- 0 + 0.06*V + 0.05*x + HE.err2

HE.err3 <- rnorm(n = 24, mean = 0, sd = 0.4)
HE3 <- 0 + 0.06*V + 0.05*x + HE.err3

HE.err4 <- rnorm(n = 24, mean = 0, sd = 0.5)
HE4 <- 0 + 0.06*V + 0.05*x + HE.err4

HE.err5 <- rnorm(n = 24, mean = 0, sd = 0.3)
HE5 <- 0 + 0.06*V + 0.05*x + HE.err5

HE.err6 <- rnorm(n = 24, mean = 0, sd = 0.7)
HE6 <- 0 + 0.06*V + 0.05*x + HE.err6

HE.err7 <- rnorm(n = 24, mean = 0, sd = 0.5)
HE7 <- 0 + 0.06*V + 0.05*x + HE.err7

HE.err8 <- rnorm(n = 24, mean = 0, sd = 0.6)
HE8 <- 0 + 0.06*V + 0.05*x + HE.err8

HE.err9 <- rnorm(n = 24, mean = 0, sd = 0.4)
HE9 <- 0 + 0.06*V + 0.05*x + HE.err9

HE <- c(HE1, HE2, HE3, HE4, HE5, HE6, HE7, HE8, HE9)

datos$HE <- HE
head(datos)

# y = diversidad de parasitoides 
y.err1 <- rnorm(n = 24, mean = 0, sd = 0.4)
y1 <- 0 + 0.75*HE1 + y.err1 # y depende de HE

y.err2 <- rnorm(n = 24, mean = 0, sd = 0.5)
y2 <- 0 + 0.75*HE1 + y.err2

y.err3 <- rnorm(n = 24, mean = 0, sd = 0.7)
y3 <- 0 + 0.75*HE1 + y.err3

y.err4 <- rnorm(n = 24, mean = 0, sd = 0.5)
y4 <- 0 + 0.75*HE1 + y.err4

y.err5 <- rnorm(n = 24, mean = 0, sd = 0.3)
y5 <- 0 + 0.75*HE1 + y.err5

y.err6 <- rnorm(n = 24, mean = 0, sd = 0.7)
y6 <- 0 + 0.75*HE1 + y.err6

y.err7 <- rnorm(n = 24, mean = 0, sd = 0.5)
y7 <- 0 + 0.75*HE1 + y.err7

y.err8 <- rnorm(n = 24, mean = 0, sd = 0.4)
y8 <- 0 + 0.75*HE1 + y.err8

y.err9 <- rnorm(n = 24, mean = 0, sd = 0.6)
y9 <- 0 + 0.75*HE1 + y.err9

y <- c(y1, y2, y3, y4, y5, y6, y7, y8, y9)

datos$y <- y
head(datos)

# Como se consider� en la simulaci�n de diversidad, la complejidad de la comunidad vegetal del entorno y el n�mero plantas hospederas del fit�fago posible se mantenga en cada momento de muestreo
#Las inclu�mos al data frame
V <- rep(V, 9)
datos$V <- V
x <- rep(x, 9)
datos$x <- x

head(datos)
str(datos)
summary(datos)


#Importante recordar que seg�n nuestro DAG, hay relacion entre x y y a traves del pipe HE y que deberiamos poder detectar si controlamos por V:

# Exploraci�n de distribuci�n con histogr�mas

par(mfrow = c(1, 3))
hist(datos$x)
hist(datos$V)
hist(datos$y)

#Se observa un distribuci�n no normal

# Modelo binomial negativo

library(MASS) #cargar paquete MASS
library(lme4) #cargar paquete lme4
# Crear modelo log�stico mixto, considerando el momento de muestreo
escamas.glmer.nb <- glmer.nb(y ~ x + V + (1|muestreo),
                    data=datos)
summary(escamas.glmer.nb)


# Sin considerar el momento del muestreo 
escamas.glm.nb <- glm.nb(y ~ x + V, link = "log",
                             data=datos)
summary(escamas.glm.nb)




# Comparamos nuestro modelo con un modelo nulo considerando el momento de muestreo
nb.null <- glmer.nb(y ~ 1 + (1|muestreo), data = datos)
summary(nb.null)

AIC(escamas.glmer.nb, nb.null)

anova(escamas.glmer.nb, nb.null, test = "Chi")

# El modelo es mucho mejor que el nulo 


# Seg�n nuestros datos simulados, s� hay un efecto de la complejidad vegetal de la comunidad vegetal del entorno (x), pero del n�mero de plantas hospederas de escamas (Fit�fago) (V) sobre la diversidad de parasitoides (y)

# �Qu� tanto explica el modelo los datos? Lo evaluamos en base a deviance de nuestro modelo y Null deviance
1 - (escamas.glm.nb$deviance / escamas.glm.nb$null.deviance)

#Nuestro modelo explica un 93% los datos

# Figuras

#exploramos residuales
plot(escamas.glmer.nb)


# Predicci�n del modelo
datos1 <- data.frame(x = seq(min(datos$x), max(datos$x), by = 1.91), y = mean(datos$y), V = mean(datos$V))                                                                             

# Predicci�n del modelo sobre nuestros datos simulados

datos2 <- cbind(datos1, predict(escamas.glm.nb, datos1, re.form=NA, type = "link", se = T))
str(datos2)
# Objeto con valores predichos por el modelo
fit <- exp(datos2$fit)
LL <- exp(datos2$fit- 1.96 * datos2$se.fit)
UL <- exp(datos2$fit+ 1.96 * datos2$se.fit)

# Guardar valores predichos en nueva base de datos
nd <- data.frame(x = datos2$x, y = datos2$y, V = datos2$V,
                      fit, LL, UL)
# Figura modelo

par(mfrow = c(1, 1))

f1 <- ggplot(nd, aes(x, fit)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), fill = "gray", alpha = 0.30) +
  geom_line(colour = "black", size = 1, lty = 1) +
  labs(x = "Complejidad vegetal", y = "Diversidad parasitoides") +
  theme_classic() +
  theme(panel.border= element_blank())+
  theme(axis.line.x = element_line(color="black", size = .7),
        axis.line.y = element_line(color="black", size = .7)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  theme(text = element_text(size=12))

f1

# En la figura se observa el efecto de la complejidad vegetal de la comunidad vegetal del entorno (x), controlando por el n�mero de plantas hospederas de escamas (Fit�fago) sobre la diversidad de parasitoides (y), seg�n nuestros datos simulados siguiendo nuestra hip�tesis de causalidad esperada
