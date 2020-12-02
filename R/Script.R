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


#El número de plantas (V) influencia la complejidad estructural de la vegetación (x), 
#tanto el número de plantas como la complejidad estructural influye en la diversidad de escamas (HE) y esta la abundancia (E) de estos insectos herbívoros. 
#Las escamas son hospederos de los parasitoides y causan mayor abundancia (A) y diversidad de parasitoides (y). 
#La diversidad de parasitoides y escamas a su vez permite una mayor abundancia de parasitoides (A), ya que se reduce la competencia por hospederos.
#La abundacia influye sobre la proporción de especialistas (S) y este a su vez sobre la uniformidad de especies (J)

# Nuestra pregunta principalmente busca responder si la complejidad estructural causa una mayor diversidad de parasitoides.


library(ggdag) #cargar libreria ggdag

#modificado considerando escamas 1
escamas <- dagify(x ~ V,
                  HE ~ V,
                  HE ~ x,
                  E ~ HE,
                  A ~ E,
                  y ~ HE,
                  S ~ E,
                  S ~ A,
                  J ~ S,
                   A ~ y,
                 
                  
                  exposure = "x",
                  outcome = "y")
tidy_dagitty(escamas)
ggdag(escamas, layout = "circle") + theme_dag()


# Datos

# enumerar covariables
adjustmentSets(x = escamas, exposure = "x", outcome = "y", type="all", effect = "total")


# compactar
td_dag <- tidy_dagitty(escamas)




