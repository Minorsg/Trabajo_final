# Proyecto final
# Curso: Herramientas pr�cticas para investigaci�n reproducible
# Estudiante: Minor Solano Guti�rrez
# II-2020


# Tema: Influencia de la complejidad estructural de la comunidad vegetal del entorno en la composici�n de especies de escamas armadas (Hemiptera: Diaspididae) y sus parasitoides, en palmas ornamentales en ambientes urbanos en Costa Rica


# Pregunta comleta de investigaci�n
# �Cu�l impacto tiene la complejidad estructural de la comunidad vegetal del entorno en la abundancia y diversidad de avispas parasitoides de escamas armadas (Hemiptera: Diaspididae) en palmas ornamentales en ambientes urbanos?


# Hip�tesis
# La diversidad de especies de escamas Diaspididae y sus parasitoides Hymenoptera en palmas ornamentales variar� seg�n la complejidad estructural de la comunidad vegetal entorno, 
# donde en h�bitats m�s complejos (p. e. parques) habr� una mayor diversidad (H') de especies, tanto de escamas, como de parasitoides, respecto a un h�bitat m�s simple (p. e. bordes de calle o parqueos). 
# Pero no una mayor uniformidad de especies (J') (i.e. la proporci�n de la diversidad observada en cada h�bitat, respecto a m�xima diversidad posible, i.e. riqueza total), debido a las especies de especialistas de cada h�bitat. 
# Donde, se espera menos diversidad, pero con abundancias m�s estables de las especies de especialistas en h�bitats simples, respecto a mayor diversidad, pero con abundancias menores y m�s heterog�neas en h�bitat complejos. 
# Esto podr�a explicar las razones por las cu�les zonas con una baja complejidad vegetal del entorno son m�s propensas a experimentar grandes incrementos en las poblaciones de escamas 
# y proponer m�todos de manejo m�s sostenibles de poblaciones de estos insectos basados en aumentar la complejidad estructural de la comunidad vegetal del entorno.


#El n�mero de plantas (V) influencia la complejidad estructural de la comunidad vegetal del entorno (x), 
#tanto el n�mero de plantas como la complejidad estructural influye en la diversidad de escamas (HE) y esta en la abundancia (E) de estos insectos herb�voros. 
#Las escamas son hospederos de los parasitoides y causan mayor abundancia (A) y diversidad de parasitoides (y). 
#La diversidad de parasitoides y escamas a su vez permite una mayor abundancia de parasitoides (A), ya que se reduce la competencia por hospederos.
#La abundacia influye sobre la proporci�n de especialistas (S) y este a su vez sobre la uniformidad de especies (J)

# Nuestra pregunta principalmente busca responder si la complejidad estructural causa una mayor diversidad de parasitoides.


library(ggdag) #cargar libreria ggdag

#Crear DAG

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

# grafico
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

#Para "probar" el modelo podr�a interesarnos saber cuales implicaciones de independencia deber�an cumplirse si es que el modelo es correcto.
impliedConditionalIndependencies(x = escamas, type = "missing.edge")

impliedConditionalIndependencies(x = escamas, type = "basis.set")

# Simulemos las relaciones de dependencia que especificamos:

# Los datos son conteos, por lo que son datos discretos y dado esto no simularemos datos siguiendo una distribuci�n normal
# Importante aclarar que en los datos reales podr�a haber sobre dispersi�n, lo que es m�s posible por lo que se simular� con una distribuci�n binomial negativa



# V = n�mero de palmas (plantas hospederas del insecto fit�fago) en el sitio de muestreo 
#En el caso de V, no pueden haber datos con cero palmas, porque haber palmas es un requisito

library(extraDistr)

V <- rtpois(24, lambda = 4, a = 1, b = 6)
barplot(table(V))

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
barplot(table(x))

# HE = diversidad de escamas, en este caso diversidad si podr�a tener distribuci�n normal, ya que si se tiene un conjunto de �ndices de diversidad replicados de diferentes sitios, el �ndice de diversidad podr�a considerarse como observaciones
HE.err <- rnorm(n = 24, mean = 0, sd = 0.5)
HE <- 0 + 0.5*V + 0.5*x + HE.err # E depende de V y de x

# y = diversidad de parasitoides 
y.err <- rnorm(n = 24, mean = 0, sd = 0.5)
y <- 0 + 0.75*E + y.err # y depende de E




