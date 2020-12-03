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

#Para "probar" el modelo podría interesarnos saber cuales implicaciones de independencia deberían cumplirse si es que el modelo es correcto.
impliedConditionalIndependencies(x = escamas, type = "missing.edge")

impliedConditionalIndependencies(x = escamas, type = "basis.set")

# Simulemos las relaciones de dependencia que especificamos:

# Los datos son conteos, por lo que son datos discretos y dado esto no simularemos datos siguiendo una distribución normal
# Importante aclarar que en los datos reales podría haber sobre dispersión, lo que es más posible por lo que se simulará con una distribución binomial negativa



# V = número de palmas (plantas hospederas del insecto fitófago) en el sitio de muestreo 
#En el caso de V, no pueden haber datos con cero palmas, porque haber palmas es un requisito

library(extraDistr)

V <- rtpois(24, lambda = 4, a = 1, b = 6)
barplot(table(V))

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
barplot(table(x))

# HE = diversidad de escamas, en este caso si podrían tener distribución normal, ya que si se tiene un conjunto de índices de diversidad replicados de diferentes sitios, el índice de diversidad podría considerarse como observaciones
E.err <- rnorm(n = 24, mean = 0, sd = 0.5)
E <- 0 + 0.5*V + 0.5*x + E.err # E depende de V y de x

# y = 
y.err <- rnorm(n = 24, mean = 0, sd = 0.5)
y <- 0 + 0.75*E + y.err # y depende de E

A.err <- rnorm(n = 100, mean = 0, sd = 0.5)
A <- 0 + 0.5*E + 0.5*y + A.err # A depende de E



