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


#El n�mero de plantas (V) influencia la complejidad estructural de la vegetaci�n (x), 
#tanto el n�mero de plantas como la complejidad estructural influye en la diversidad de escamas (HE) y esta la abundancia (E) de estos insectos herb�voros. 
#Las escamas son hospederos de los parasitoides y causan mayor abundancia (A) y diversidad de parasitoides (y). 
#La diversidad de parasitoides y escamas a su vez permite una mayor abundancia de parasitoides (A), ya que se reduce la competencia por hospederos.
#La abundacia influye sobre la proporci�n de especialistas (S) y este a su vez sobre la uniformidad de especies (J)

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



