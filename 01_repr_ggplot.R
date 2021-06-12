
# Estos paquetes serán necesarios a lo largo de la clase
# 
paquetes <- c("rstudioapi", "ggpubr","gganimate","gifsk","av",
              "ggthemes","grid","gridExtra","rsvg","magick",
              "wesanderson", "MASS", "plotly","car","scatterplot3d",
              "MASS","dslabs","HSAUR","RColorBrewer","plot3D")
nuevos <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]
if(length(nuevos)>0) install.packages(nuevos)
# representación interactiva
library(plotly)
# paletas generales
library(RColorBrewer)

library(ggplot2)
# ggplot listo para publicaciones
library(ggpubr)
# fondos
library(ggthemes)
# paletas de películas
library(wesanderson)
# situación de workspace en directorio
library(rstudioapi) 
# lectura de datos
library(data.table)

# library(dplyr)
# library(magrittr)

library(tidyverse)




####################################################################
# RColorBrewer dispone de una lista bastante completa de paletas 
display.brewer.all()

# Podemos ver alguna en particular
display.brewer.pal(n = 5, name = 'Set2')
display.brewer.pal(n = 9, name = 'Set2')
# y obtener su codigo hexadecimal
brewer.pal(n = 5, name = 'Set2')

display.brewer.pal(n = 5, name = 'Accent')
brewer.pal(n = 5, name = 'Accent')


# DEGRADADOS
display.brewer.pal(n = 5, name = 'YlOrRd')
display.brewer.pal(n = 5, name = 'GnBu')
# COMPLEMENTARIOS
display.brewer.pal(n = 5, name = 'Spectral')
display.brewer.pal(n = 5, name = 'RdYlBu')
display.brewer.pal(n = 5, name = 'RdBu')

# El paquete wesanderson contiene paletas de color de las 
# peliculas de este director

names(wes_palettes)
# Asi podemos ver los codigos de los colores
wes_palettes$Zissou1
# pero es mejor ver los colores en si
wes_palette("Zissou1")
wes_palette("Darjeeling1")
wes_palette("Darjeeling2")
wes_palette("GrandBudapest1")
wes_palette("BottleRocket2")
wes_palette("Chevalier1")
wes_palette("IsleofDogs2")



###################################################################
# situo el path en la direccion actual de este archivo
setwd(dirname(getActiveDocumentContext()$path))
# lo podia haber hecho a mano:
#setwd("C:/Users/jmartinezmo/Desktop/IFFE/s3_representacion_ggplot2/scripts")
getwd()

###################################################################
################# Representacion Analitica de Datos

# Teniendo vectores
x=seq(0,10,length.out = 1000)
y=sin(x)
z=sin(0.5*x)

# paquete base
plot(x,y)
lines(x,z)

# quickplot pertenece a ggplot
qplot(x,y)

qplot(x, y, geom=c("point", "line"))+
  geom_line(aes(x,z,color="red"))

# puede leer de un df
qplot(mpg, wt, data=mtcars)
?mtcars
colnames(mtcars)

# el paquete base tambien puede leer de un df
plot(mtcars$wt,mtcars$mpg,col="red")

plot(mtcars$wt, mtcars$mpg,
     main="Autonomia y Peso del Coche",
     xlab="Peso", ylab="Autonomia", pch=18, col="red")
text(mtcars$wt, mtcars$mpg,
     row.names(mtcars), cex=0.6, pos=4, col="blue")

# mpg es otro df de coches
?mpg
boxplot(mpg$cty~mpg$class,
        main="Boxplot consumo ciudad - tipo coche",
        xlab="Tipo de coche",
        ylab="consumo",
        pch=18, col="blue")

# es util para hacer representaciones rapidas sobre la marcha
# pero no es tan presentable como lo que se puede obtener con 
# ggplot2 y similares
###

############################################################
################### TIPO CORRELACION #######################
## DISPERSION
## CORRELOGRAMAS 
## HISTOGRAMAS Y DENSIDAD
## BOXPLOT Y VIOLINPLOT

# gapminder es un dataset muy famoso
# que presenta datos socioeconómicos mundiales 
# a nivel país y a través de los años
gp<-dslabs::gapminder
?dslabs::gapminder

## Elegimos los colores con que representaremos 
# los datos
names(wes_palettes)

# pero es importante saber el tipo de 
# representacion de antemano
# Si hay factores -> Contraste
# Si queremos ver variaciones - > Degradados

# Vamos a representar caracteristivas 
# por continente
# 
wes_palette("Darjeeling1")
wes_palettes$Darjeeling1

wes_palette("BottleRocket2")
wes_palettes$BottleRocket2

# puedo hacer una paleta eligiendo colores:
paleta_continentes=c(
      wes_palettes$BottleRocket2[c(1:3)],
      wes_palettes$Darjeeling1[2],
      wes_palettes$Darjeeling1[5])

barplot(1:5, 
        col=paleta_continentes, 
        main="Mis colores personalizados")


glimpse(gp)
View(gp)

# Elegiremos el ano 2010
# Pero primero realizaremos una modificacion 
# para disponer del PIB per capita
gp<-gp %>% mutate(gdp_pc=gdp/population)

gp_2010<-gp %>% filter(year==2010)

########################### Dispersion    

# Si no especifico la geometria:
ggplot(data=gp_2010,
       aes(x=gdp_pc,
           y=life_expectancy))

# Diagrama de dispersion (scatter) va con 
# geom_point()
gp_2010 %>%
  ggplot(aes(x=gdp_pc,
             y=life_expectancy))+
  geom_point()

## Capa de colores por continente
gp_2010 %>%
  ggplot(aes(x=gdp_pc,
             y=life_expectancy,
           color=continent))+
geom_point()

## Uso mi paleta  
gp_2010 %>%
ggplot(
       aes(x=gdp_pc,
           y=life_expectancy,
           color=continent)
       )+
geom_point()+
scale_colour_manual(values=paleta_continentes)

## Cambio de escala  
gp_2010 %>%
  ggplot(
    aes(x=gdp_pc,
        y=life_expectancy,
        color=continent)
  )+
  geom_point()+
  scale_colour_manual(values=paleta_continentes)+
  scale_x_log10()
  
gp_2010 %>%
  ggplot(
    aes(x=gdp_pc,
        y=life_expectancy,
        color=continent)
  )+
  geom_point()+
  scale_colour_manual(values=paleta_continentes)+
  scale_x_log10()+
  scale_y_log10()

## Cambio los titulos de eje
gp_2010 %>%
  ggplot(
    aes(x=gdp_pc,
        y=life_expectancy,
        color=continent)
  )+
  geom_point()+
  scale_colour_manual(values=paleta_continentes)+
  scale_x_log10()+
  scale_y_log10()+
  xlab("PIB/Capita $")+
  ylab("Años de vida")

## Cambio el tamaño de los puntos 
#  en funcion de la poblacion
## de cada pais
gp_2010 %>%
  ggplot(
    aes(x=gdp_pc,
        y=life_expectancy,
        color=continent)
  )+
  #
  geom_point(aes(size=population))+
  #
  scale_colour_manual(values=paleta_continentes)+
  scale_x_log10()+
  scale_y_log10()+
  xlab("PIB/Capita $")+
  ylab("Años de vida")

## Tambien puedo asociar formas de 
#  los puntos con el grupo de 
## pertenencia, en este caso continentes
gp_2010 %>%
  ggplot(
    aes(x=gdp_pc,
        y=life_expectancy,
        color=continent)
  )+
  #
  geom_point(aes(size=population,
                 shape=continent))+
  #
  scale_colour_manual(values=paleta_continentes)+
  scale_x_log10()+
  scale_y_log10()+
  xlab("PIB/Capita $")+
  ylab("Años de vida")


## Puedo añadir lineas de tendencia 
#  con geom_smooth(method='lm')
g_lm<-gp_2010 %>%
  ggplot(
    aes(x=gdp_pc,
        y=life_expectancy,
        color=continent,
        group=continent,
        text=country)
  )+
  #
  geom_point(aes(size=population,
                 shape=continent))+
  #
  scale_colour_manual(values=paleta_continentes)+
  scale_x_log10()+
  scale_y_log10()+
  xlab("PIB/Capita $")+
  ylab("Años de vida")+
  geom_smooth(method = 'lm')

g_lm


## Le pongo titulo a la figura
gp_2010 %>%
  ggplot(
    aes(x=gdp_pc,
        y=life_expectancy,
        color=continent)
  )+
  geom_point(aes(size=population,shape=continent))+
  scale_colour_manual(values=paleta_continentes)+
  scale_x_log10()+
  scale_y_log10()+
  xlab("PIB/Capita $")+
  ylab("Años de vida")+
  ggtitle("Esperanza de vida y PIB per capita 2010")


## Le pongo titulo a la figura y
## coloco la leyenda en la parte superior 
pib_life_exp<-gp_2010 %>%
  ggplot(
    aes(x=gdp_pc,
        y=life_expectancy,
        color=continent,
        text=country)
  )+
  geom_point(aes(size=population,
                 shape=continent))+
  scale_colour_manual(values=paleta_continentes)+
  scale_x_log10()+
  scale_y_log10()+
  xlab("PIB/Capita $")+
  ylab("Años de vida")+
  ggtitle("Esperanza de vida y PIB per capita 2010")+
  theme(legend.position = "top")

pib_life_exp





################################# E1
# ¿Qué sucede si no filtramos por año?





## Al no filtrar por año es posible 
## ver las trazas de evolucion

gp %>%
  ggplot(
    aes(x=gdp_pc,
        y=life_expectancy,
        color=continent)
  )+
  geom_point(aes(size=population,
                 shape=continent))+
  scale_colour_manual(values=paleta_continentes)+
  scale_x_log10()+
  scale_y_log10()+
  xlab("PIB/Capita $")+
  ylab("Años de vida")+
  ggtitle("Esperanza de vida y PIB per capita 2010")+
  theme(legend.position = "top")


################################# E2
# Cambia el gráfico para estudiar el mismo
# comportamiento del año 2010 pero en Europa 
# y diferenciando por regiones
# ¿Qué observas?







# Existe una clara diferencia entre paises del norte y del sur

gp_2010 %>% filter(continent=="Europe") %>%
  ggplot(
    aes(x=gdp_pc,
        y=life_expectancy,
        color=region)
  )+
  geom_point(aes(size=population,
                 shape=region))+
  scale_colour_manual(values=paleta_continentes)+
  scale_x_log10()+
  scale_y_log10()+
  xlab("PIB/Capita $")+
  ylab("Años de vida")+
  ggtitle("Europa: Esperanza de vida y PIB per capita 2010")+
  theme(legend.position = "top")


# Es posible añadir etiquetas para ver los paises
gp_2010 %>% filter(continent=="Europe") %>%
  ggplot(
    aes(x=gdp_pc,
        y=life_expectancy,
        color=region)
  )+
  geom_point(aes(size=population,
                 shape=region))+
  #
  geom_text(aes(label=country),hjust=0,vjust=0)+
  #
  scale_colour_manual(values=paleta_continentes)+
  scale_x_log10()+
  scale_y_log10()+
  xlab("PIB/Capita $")+
  ylab("Años de vida")+
  ggtitle("Europa: Esperanza de vida y PIB per capita 2010")+
  theme(legend.position = "top")


################################# E3
# Realiza una gráfica de dispersión con 
# los datos del dataset "mammals" del 
# paquete "MASS" que relaciona la masa
# del animal con su masa encefálica

?MASS::mammals
## MASS tiene una funcion "select" que 
# enmascararía la de dplyr
## por eso extraigo el dataset sin cargar 
# el paquete

mamiferos<-MASS::mammals

View(mamiferos)
# Por comodidad hago una columna con los nombres
# y calculo los logaritmos decimales de masa corporal 
# y encefálica para poder manejar mejor el rango de 
# información disponible, ya que el estudio abarca desde 
# ratones hasta elefantes

mamiferos<-mamiferos %>%
  mutate(animal=row.names(mamiferos),
         log_cerebro=log10(brain),
         log_cuerpo=log10(body))

## Represento
mamiferos %>% 
  ggplot(aes(x=body,y=brain,color=log_cerebro))+
  geom_point()+
  scale_color_gradient(name="log(Masa cerebral)",
                       high = "blue", low="orange")+
  xlab("Masa corporal en kg")+
  ylab("Masa encefalica en g")+
  ggtitle("Masa corporal y masa encefálica en mamíferos")+
  geom_smooth(method='lm')+
  theme(legend.position = "top")


## Asi no se ve nada
# por eso es util el empleo de logaritmos
# en el caso 
# de tener rangos de datos muy amplios
mamiferos %>% 
  ggplot(aes(x=log_cuerpo,
             y=log_cerebro,color=log_cerebro))+
  geom_point()+
  scale_color_gradient(name="log(Masa cerebral)",
                       high = "blue", low="orange")+
  xlab("Log Masa corporal en kg")+
  ylab("Log Masa encefálica en g")+
  ggtitle("Masa corporal y masa encefálica en mamíferos")+
  geom_smooth(method='lm')+
  stat_cor(method = "pearson",
           label.x = -1,
           label.y=3.5)+
  theme(legend.position = "top")

# Represento el logaritmo de masa 
# encefálica en el eje y frente 
# a la masa coroporal en el X, pero 
# en escala logarítmica
mamiferos %>% 
  ggplot(aes(x=body,y=brain,color=log_cerebro))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  scale_color_gradient(name="log(Masa cerebral)",
                       high = "blue", 
                       low="orange")+
  xlab("Masa corporal en kg")+
  ylab("Masa encefálica en g")+
  ggtitle("Masa corporal y masa encefálica en mamíferos")+
  geom_smooth(method='lm')+
  stat_cor(method = "pearson", 
           label.x = 0.01,
           label.y=3.5)+
  theme(legend.position = "top")


####################################################################
####################### 
############## Histogramas, densidad y Boxplot

# Este tipo de gráficos ayudan a visualizar 
# la distribución de 
# los datos que estamos estudiando
gp_2010 %>% 
  ggplot(aes(life_expectancy,color=continent))+
  geom_histogram(fill="white")

gp_2010 %>% 
  ggplot(aes(life_expectancy,color=continent))+
  geom_histogram(fill="white")+
  scale_color_manual(values=paleta_continentes)

# titulo el gráfico
gp_2010 %>% 
  ggplot(aes(life_expectancy,color=continent))+
  geom_histogram(fill="white")+
  scale_color_manual(values=paleta_continentes)+
  ggtitle("2010 - Histograma de la esperanza de vida por continente")+
  xlab("Años de vida")+
  ylab("conteo")
  
# recoloco la leyenda
gp_2010 %>% 
  ggplot(aes(life_expectancy,color=continent))+
  geom_histogram(fill="white")+
  scale_color_manual(values=paleta_continentes)+
  ggtitle("2010 - Histograma de la esperanza de vida por continente")+
  xlab("Años de vida")+
  ylab("conteo")+
  theme(legend.position = "top",
        legend.title = element_blank())

# o en densidad, 
# cambiando geom_histogram por geom_density
gp_2010 %>% 
  ggplot(aes(life_expectancy,color=continent))+
  geom_density(fill="white")+
  scale_color_manual(values=paleta_continentes)+
  ggtitle("2010 - Histograma de la esperanza de vida por continente")+
  xlab("Años de vida")+
  ylab("conteo")+
  theme(legend.position = "top",
        legend.title = element_blank())

# mejor con transparencia alpha
gp_2010 %>% 
  ggplot(aes(life_expectancy,color=continent,fill=continent))+
  geom_density(alpha=0.3)+
  scale_color_manual(values=paleta_continentes)+
  scale_fill_manual(values=paleta_continentes)+
  ggtitle("2010 - Densidades de la esperanza de vida por continente")+
  xlab("Años de vida")+
  ylab("conteo")+
  theme(legend.position = "top",
        legend.title = element_blank())



# Existe una extensión de ggplot2 llamada "ggpubr" 
# que funciona de modo similar a "Seaborn" 
# en Python, encapsulando las posibles capas 
# en una única función para los tipos 
# de representación más comunes:

?gghistogram
?ggdensity

gp_2010 %>% 
  gghistogram(x = "life_expectancy",
              color = "continent",
              fill = "continent",
              alpha = 0.5,
              rug = T,
              palette = paleta_continentes,
              add = "mean",
              add_density = T,
              ylab = "conteo",
              xlab = "Esperanza de vida",
              title = "2010 - Histograma de Esperanzas de vida con ggpubr")

gp_2010 %>% 
  ggdensity( x = "life_expectancy",
             add = "median", 
             rug = TRUE,
             palette = paleta_continentes,
             color = "continent",
             fill = "continent")+
  ggtitle("Esperanza de Vida por continente - Año 2010")+
  xlab("años")+
  theme(legend.title = element_blank())


################################## BOXPLOT

# Evita solapamientos 
# Ayuda a identificar outliers

gp %>% filter(year==2010) %>%
  ggplot(aes(x=continent,y=life_expectancy))+
  geom_boxplot()

gp_2010 %>% filter(
            continent %in% c("Europe",
                             "Americas",
                             "Oceania")) %>%
           ggplot(aes(x=continent,
                      y=life_expectancy,
                      label=country,
                      color=continent))+
           xlab("")+
           ylab("Años de vida")+
           ggtitle("2010 - Esperanza de Vida por Continente")+
           geom_boxplot()+
           scale_colour_manual(name="",
                               values=brewer.pal(n = 5, name = 'Accent'))+
           theme_economist()

## Geom_jitter presenta los puntos de 
#  forma aleatoria en las cajas
## pero manteniendo la distribucion 
# existente en los datos

gp_2010 %>% 
  filter(continent %in% c("Europe",
                          "Americas",
                          "Oceania")) %>%
  ggplot(aes(x=continent,
             y=life_expectancy,
             label=country,
             color=continent))+
  xlab("")+
  ylab("Años de vida")+
  ggtitle("2010 - Esperanza de Vida por Continente")+
  geom_boxplot()+
  geom_jitter()+
  scale_colour_manual(name="",
                      values=brewer.pal(n = 5, name = 'Accent'))+
  theme_economist()

## con ggpubr es fácil presentar contrastes de hipótesis en los 
## gráficos

gp_2010_cont<-gp_2010 %>% 
  filter(continent %in% c("Europe",
                          "Americas",
                          "Oceania"))


p<-gp_2010_cont %>%
  ggplot(aes(x=continent,
             y=life_expectancy,
             label=country,
             color=continent))+
  geom_boxplot()+
  geom_jitter()+
  ggtitle("Esperanza de Vida por Continente - Año 2010")+
  ylab("Años")+
  xlab("")+
  scale_colour_manual(name="",
                      values=wes_palette("Darjeeling1",
                                         n=5))+
  theme_economist()

p

# luego hago comparaciones dos a dos 
mis_comparaciones <- list( c("Americas", "Europe"),
                        c("Americas", "Oceania"),
                        c("Europe", "Oceania") )

# por defecto usa test no paramétrico
p + stat_compare_means(label.y = 50)

# ######################## CUIDADO:
# puedo elegir anova, pero hay que verificar 
# que se cumplan sus 
# asunciones: 

# normalidad de los datos -> shapiro sobre cada grupo
# homocedasticidad entre grupos -> test de Fligner 
# o test de Bartlett o test de Levene 
# si hay normalidad

p + stat_compare_means(label.y = 50,
                       method="anova")

# por detrás está haciendo esto:
compare_means(life_expectancy ~ continent,
              data = gp_2010_cont,
              method = "anova")

# PERO no dice entre qué grupos hay diferencias,
# por lo que 
# tambien se pueden mostrar los pvalores dos a dos 
# especificados por mis_comparaciones

p + stat_compare_means(comparisons = mis_comparaciones)+ 
    stat_compare_means(label.y = 50)

# En las comparaciones dos a dos
# puedo usar el test de Student (t-test)
# pero también hay que verificar la normalidad
# de los datos
# y la homocedasticidad entre grupos 
# para que sus resultados 
# sean fiables con seguridad

p + stat_compare_means(comparisons = mis_comparaciones,
                       method="t.test")+ 
    stat_compare_means(label.y = 50)


# se pueden poner los niveles como indicadores *
p + stat_compare_means(comparisons = mis_comparaciones,
                       label = "p.signif")+ 
    stat_compare_means(label.y = 50)

# o establecer un grupo de referencia 
# sobre el que realizar 
# las comparaciones
p + stat_compare_means(label = "p.format",
                       label.y = 50,
                       ref.group = "Americas",
                       method="t.test")

# el test de wilcoxon es no parametrico,
# por lo que no necesita 
# verificar condiciones como t-test o anova
p + stat_compare_means(label = "p.format",
                       label.y = 50,
                       ref.group = "Americas",
                       method="wilcox.test")

p + stat_compare_means(comparisons = mis_comparaciones,
                       label = "p.format")+ 
  stat_compare_means(label.y = 50)

# de hecho: 
continentes = gp_2010_cont %>%
              select(continent) %>%
              distinct() %>% pull()

tests_norm=data.frame()

for(continente in continentes){
  
  lexp=gp_2010_cont %>%
        filter(continent==continente) %>% 
        select(life_expectancy) %>% pull()
  
  sh<-shapiro.test(lexp)
  df=data.frame(continent=continente,pval=sh$p.value,
                test="normalidad shapiro",
                es_normal=if_else(sh$p.value<0.05,F,T))
  
  tests_norm=rbind(tests_norm,df)
    
}
View(tests_norm)

# Comparo las varianzas con el test de fligner
# H0: comparten misma varianza
fligner.test(life_expectancy~continent,
             data=gp_2010_cont)

### Por lo tanto en este ejemplo debemos 
# desconfiar de los 
##  resultados de anova y t.test 
## (aunque tengan cierta robustez ante 
# incumplimiento de condiciones)


# Violin: junta la idea del boxplot con el diagrama de densidad
gp %>% filter(year==2016,
              continent %in% c("Europe","Americas","Oceania")) %>%
  ggplot(aes(x=continent,
             y=life_expectancy,
             label=country,
             color=continent,
             fill=continent))+
  geom_violin()+
  geom_jitter()+
  ggtitle("Esperanza de Vida por Continente - Año 2016")+
  ylab("Años")+xlab("")+
  scale_colour_manual(values=c("blue","yellow","red"))+
  scale_fill_manual(values=c("darkgoldenrod1","blue1","cyan1"))+
  theme(legend.position = "top",legend.title = element_blank())+
  stat_compare_means(comparisons = mis_comparaciones,
                     label = "p.format")+ 
  stat_compare_means(label.y = 55)

                   

#################### E1
# Estudia la distribución de PIB per capita
# para el año 2010 en Europa, Americas y Oceania,
# y comprueba si existen diferencias estadísiticas
# entre la media de los grupos

gp_2010_cont %>%
  ggplot(aes(x=continent,
             y=gdp_pc,
             label=country,
             color=continent))+
  geom_boxplot()+
  geom_jitter()+
  ggtitle("PIB/cap por Continente - Año 2010")+
  ylab("PIB per capita ($)")+
  xlab("")+
  scale_colour_manual(name="",
                      values=wes_palette("Darjeeling1",
                                         n=5))+
  theme_economist()+
  stat_compare_means(comparisons = mis_comparaciones,
                     label = "p.format")+ 
  stat_compare_means(label.y = 70000)

##################### E2
# Empleando los datos de "ToothGrowth",
# ¿hay evidencia estadística para afirmar que
# la administración de vitamina C favorece el 
# crecimiento dental?

?ToothGrowth
dientes<-ToothGrowth

shapiro.test(dientes$len[dientes$supp=="OJ"])
# NO hay normalidad en el grupo zumo

shapiro.test(dientes$len[dientes$supp=="VC"])
# SI hay normalidad en el grupo vitamina C

fligner.test(len~supp,data=dientes)
car::leveneTest(len~supp,
                data=dientes,
                center=median)
# hay homocedasticidad

dientes %>%
  ggplot(aes(x=supp,
             y=len,
             label=dose,
             color=dose))+
  geom_boxplot()+
  geom_jitter()+
  ggtitle("Longitud de dientes según tratamiento con Zumo de naranja o Vit.C")+
  ylab("longitud en mm")+
  xlab("Suplemento")+
  scale_colour_gradient(name="dosis",
                        high = "blue",low="red")+
  stat_compare_means(label = "p.format")
  

compare_means(len ~ supp,
              data = dientes,
              method = "anova")
compare_means(len ~ supp,
              data = dientes,
              method = "t.test")

# Estrictamente sólo el test de Wilcoxon
# sería válido aquí
compare_means(len ~ supp,
              data = dientes,
              method = "wilcox.test")
# NO hay evidencia de que la administración de
# vitamina C afecte al crecimiento dental 
# en función de si procede de Zumo o Suplemento



###################### E3
# Para el dataset del ejercicio anterior 
# ¿hay evidencia de que a mayor dosis de
# suplemento de vitamina C haya un aumento 
# del crecimiento dental?


# Hay estos grupos de dosis
dientes %>% select(dose) %>% distinct()

# establezco las comparaciones dos a dos
comparaciones <- list( c("0.5", "1"),
                       c("1", "2"),
                       c("0.5", "2") )
# represento

ggboxplot(dientes,
          x = "dose",
          y = "len",
          color = "dose",
          palette = c("grey","orange","blue"),
          title = "Crecimiento dental por dosis de Vit.C administrada")+ 
# Establezco las comparaciones
  stat_compare_means(comparisons = comparaciones)+ 
# Test sobre la existencia de diferencias entre los 3 grupos
  stat_compare_means(label.y = 50)     

# Examino los tests
compare_means(len ~ dose,
              data = dientes,
              method = "anova")
compare_means(len ~ dose,
              data = dientes,
              method = "wilcox.test")
compare_means(len ~ dose,
              data = dientes,
              method = "t.test")

# hay evidencias muy fuertes de que un aumento de 
# la Vitamina C administrada afecta al crecimiento dental 

# existe correlación lineal?
cor.test(dientes$len,dientes$dose)
cor.test(dientes$len,dientes$dose,method="spearman")
cor.test(dientes$len,dientes$dose,method = "kendall")
# Además la correlación lineal es positiva, 
# fuerte y significativa











######################### BARRAS Y DIVERGENCIA #######################

# Datos de Europa:
gp_2010_eu<-gp_2010_cont %>% filter(continent=="Europe")

# Tipico diagrama de barras
# Falta orden para legibilidad inmediata
gp_2010_eu %>% 
  ggplot(aes(x=country,
             y=gdp_pc,
             fill=region))+
  scale_fill_manual(values=paleta_continentes)+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle=90, hjust=1))

# Así está ordenado, pero se puede mejorar
gp_2010_eu %>% 
  ggplot(aes(x=reorder(country,-gdp_pc),
             y=gdp_pc,
             fill=region))+
  scale_fill_manual(values=paleta_continentes)+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("PIB/capita ($)")+
  ggtitle("Europa: PIB per capita por regiones, año 2010")+
  theme(axis.text.x = element_text(angle=90, hjust=1),
        legend.position = "top",
        legend.title = element_blank())

# En horizontal es más sencillo de leer
gp_2010_eu %>% arrange(desc(gdp_pc)) %>%
  ggplot(aes(x=reorder(country,gdp_pc),
             y=gdp_pc,
             fill=region))+
  scale_fill_manual(values=paleta_continentes)+
  geom_bar(stat="identity")+
  coord_flip()+
  xlab("")+
  ylab("PIB/capita ($)")+
  ggtitle("Europa: PIB per capita por regiones, año 2010")+
  theme(axis.text.x = element_text(angle=90, hjust=1),
        legend.position = "top",
        legend.title = element_blank())

# Añado una línea mediante geom_hline 
# para referenciar a España 
# (Es geom_hline, porque en realidad es horizontal,
# no vertical,
# debido al intercambio de coordenadas que hacemos)
gp_2010_eu %>% arrange(desc(gdp_pc)) %>%
  ggplot(aes(x=reorder(country,gdp_pc),
             y=gdp_pc,
             fill=region))+
  scale_fill_manual(values=paleta_continentes)+
  geom_bar(stat="identity")+
  
  coord_flip()+
  
  xlab("")+
  ylab("PIB/capita ($)")+
  ggtitle("Europa: PIB per capita por regiones, año 2010")+
  
  theme(axis.text.x = element_text(angle=90, hjust=1),
        legend.position = "top",
        legend.title = element_blank())+
  
  geom_hline(yintercept = gp_2010_eu %>%
                          filter(country=="Spain") %>% 
                          select(gdp_pc) %>% 
                          pull(),
             linetype="dashed",
             color="red")

# Con ggpubr es algo mas sencillo debido a 
# las opciones sort para ordenar
# y recolocar texto en los ejes
gp_2010_eu %>%
ggbarplot(x = "country",
          y = "gdp_pc",
          fill = "region",              
          color = "white",            
          palette = paleta_continentes,
          sort.val = "asc",           
          sort.by.groups = TRUE,      
          x.text.angle = 90,
          rotate = T,
          xlab = "",
          ylab = "",
          title = "Europa: PIB per capita por regiones, año 2010",
          ggtheme = theme_minimal()
)+geom_hline(yintercept = gp_2010_eu %>%
               filter(country=="Spain") %>% 
               select(gdp_pc) %>% 
               pull(),
             linetype="dashed",
             color="darkblue")

# Existe una variante llamada diagrama de puntos o lollipop
gp_2010_eu %>%
  ggdotchart(x = "country",
             y = "gdp_pc",
             color = "region",                                
             palette = paleta_continentes, 
             sorting = "descending",                       
             add = "segments",                             
             rotate = F,                                
             group = "region",
             ylab = "PIB/capita ($)",
             xlab = "",
             title = "Europa, PIB per capita - 2010",
             dot.size = 12,                                 
             label = round(gp_2010_eu$gdp_pc),             
             font.label = list(color = "white", size = 8, 
                               vjust = 0.5),               
             ggtheme = theme_pubr()                        
  )


gp_2010_eu %>%
ggdotchart(x = "country",
           y = "gdp_pc",
           color = "region",                                
           palette = paleta_continentes, 
           sorting = "descending",                       
           add = "segments",                             
           rotate = TRUE,                                
           group = "region",
           ylab = "PIB/capita ($)",
           xlab = "",
           title = "Europa, PIB per capita - 2010",
           dot.size = 12,                                 
           label = round(gp_2010_eu$gdp_pc),             
           font.label = list(color = "white", size = 8, 
                             vjust = 0.5),               
           ggtheme = theme_pubr()                        
)




### Veamos ahora un gráfico de desviaciones 

# Primero crearemos una métrica que normalice
# los valores disponibles
# para el PIB per capita de cada pais respecto 
# a su continente:

gp_2010_cont <- gp_2010_cont %>%
                group_by(continent) %>% 
                mutate(PIB_norm=(gdp_pc-mean(gdp_pc,
                                             na.rm = T))/sd(gdp_pc,na.rm = T))
# para ello también existe la función scale
gp_2010_cont<- gp_2010_cont %>%
  group_by(continent) %>% 
  mutate(PIB_norm_scale=scale(gdp_pc))


gp_2010_cont %>% 
  filter(continent %in% c("Europe","Americas")) %>%
  ggbarplot(x = "country",
            y = "PIB_norm",
            fill = "continent",              
            color = "white",            
            palette = paleta_continentes,
            sort.val = "asc",           
            sort.by.groups = TRUE,      
            x.text.angle = 90,
            rotate = F,
            xlab = "",
            ylab = "",
            title = "Desviaciones PIB respecto a la media continental - año 2010",
            ggtheme = theme_minimal()
  )

# es mejor en vertical para aumentar 
# la legibilidad
gp_2010_cont %>% 
  filter(continent %in% c("Europe","Americas")) %>%
  ggbarplot(x = "country",
            y = "PIB_norm",
            fill = "continent",              
            color = "white",            
            palette = paleta_continentes,
            sort.val = "asc",           
            sort.by.groups = TRUE,      
            x.text.angle = 90,
            rotate = T,
            xlab = "",
            ylab = "",
            title = "Desviaciones PIB respecto a la media continental - año 2010",
            ggtheme = theme_minimal()
            )


############### SERIES TEMPORALES ####################
seleccion_paises<-c("Spain","Portugal",
                    "France","Germany","United Kingdom")
gp_sel <- gp %>% 
            filter(country %in% seleccion_paises)

# Extraigo una muestra de países y represento su 
# variación en el tiempo del PIB

gp_sel %>% 
  ggplot(aes(x=year,
             y=gdp_pc,
             color=country))+
  geom_point()+
  geom_line()+
  scale_color_manual(values=paleta_continentes)+
  xlab("Años")+
  ylab("PIB per capita")+
  ggtitle("Evolución temporal del PIB per capita")+
  theme_minimal()+
  theme(legend.position = "top",
        legend.title = element_blank())


# Esperanza de vida
gp_sel %>% 
  ggplot(aes(x=year,
             y=life_expectancy,
             color=country))+
  geom_point()+
  geom_line()+
  scale_color_manual(values=paleta_continentes)+
  xlab("Año")+
  ylab("Años de Vida")+
  ggtitle("Evolución temporal de la esperanza de vida")+
  geom_hline(yintercept = 80,
             color="firebrick",
             linetype="dashed")+
  theme_minimal()+
  theme(legend.position = "top",
        legend.title = element_blank())



# Modifico el dataset original para tener valores 
# máximos y mínimos por año/continente y año/región
gp_life_exp <- gp %>%
  group_by(continent,year) %>% 
  
  mutate(cont_min_life_exp=min(life_expectancy,na.rm=T),
         cont_max_life_exp=max(life_expectancy,na.rm=T),
         cont_mean_life_exp=mean(life_expectancy,na.rm=T)
         ) %>% 
  
  group_by(region,year) %>% 
  
  mutate(region_min_life_exp=min(life_expectancy,na.rm=T),
         region_max_life_exp=max(life_expectancy,na.rm=T),
         region_mean_life_exp=mean(life_expectancy,na.rm=T)
  ) %>% ungroup()

View(gp_life_exp)

# Así es posible tener intervalos en el gráfico
# que sirven para dar una idea de los márgenes 
# de comportamiento de una forma limpia
gp_life_exp %>% 
  ggplot(aes(x=year,
             y=cont_mean_life_exp,
             color=continent,
             group=continent,
             fill=continent))+
  geom_line(aes(x=year,
                y=cont_mean_life_exp))+
  geom_ribbon(aes(ymin = cont_min_life_exp,
                  ymax = cont_max_life_exp),
              alpha=0.1)+
  scale_color_manual(values=paleta_continentes)+
  xlab("Año")+
  ylab("Años de vida")+
  ggtitle("Evolución temporal de la esperanza de vida por continentes")+
  geom_hline(yintercept = 80,color="firebrick",linetype="dashed")+
  theme_minimal()+
  theme(legend.position = "top",
        legend.title = element_blank())

gp_life_exp %>% filter(continent=="Europe") %>%
  ggplot(aes(x=year,
             y=region_mean_life_exp,
             color=region,
             group=region,
             fill=region))+
  geom_line(aes(x=year,
                y=region_mean_life_exp))+
  geom_ribbon(aes(ymin = region_min_life_exp,
                  ymax = region_max_life_exp),
              alpha=0.1)+
  scale_color_manual(values=paleta_continentes)+
  xlab("Año")+
  ylab("Años de vida")+
  ggtitle("Europa: Evolución temporal de la esperanza de vida por regiones")+
  geom_hline(yintercept = 80,
             color="firebrick",
             linetype="dashed")+
  theme_minimal()+
  theme(legend.position = "top",
        legend.title = element_blank())

# Otra forma de presentar la variación 
# en un determinado intervalo
# es empleando boxplots

gp_life_exp %>% filter(continent=="Europe") %>%
  mutate(year=as.factor(year)) %>%
  ggplot(aes(x=year,
             y=life_expectancy,
             color=region))+
  geom_boxplot()+
  scale_color_manual(values=paleta_continentes)+
  xlab("Año")+
  ylab("Años de vida")+
  ggtitle("Europa: Evolución temporal de la esperanza de vida por regiones")+
  geom_hline(yintercept = 80,
             color="firebrick",
             linetype="dashed")+
  theme_minimal()+
  theme(legend.position = "top",
        axis.text.x = element_text(angle=45, hjust=1),
        legend.title = element_blank())


gp_life_exp %>% 
  filter(continent=="Europe" & year > 1989) %>%
  
  mutate(year=as.factor(year)) %>%
  ggplot(aes(x=year,
             y=life_expectancy,
             color=region))+
  geom_boxplot()+
  scale_color_manual(values=paleta_continentes)+
  xlab("Año")+
  ylab("Años de vida")+
  ggtitle("Europa: Evolución temporal de la esperanza de vida por regiones")+
  geom_hline(yintercept = 80,
             color="firebrick",
             linetype="dashed")+
  theme_minimal()+
  theme(legend.position = "top",
        axis.text.x = element_text(angle=45, hjust=1),
        legend.title = element_blank())




##### Con fechas, no años
library(lubridate)
# lubridate es una libreria excelente para el 
# manejo de fechas en general

url_world_data="https://covid.ourworldindata.org/data/owid-covid-data.csv"

covid_mundo<-fread(url_world_data,sep=",",encoding = "UTF-8")
glimpse(covid_mundo)

###
covid_mundo<-covid_mundo %>% 
  transform(date=as.Date(date,"%Y-%m-%d"),
            fecha_n=ymd(date)) %>% 
  mutate(dia=day(date),
         dia=wday(date,
                  getOption("lubridate.week.start", 7)),
         dia_semana=wday(date,label = T,
                         abbr = F,
                         getOption("lubridate.week.start", 7))
         )



### casos por paises
covid_mundo %>% 
  filter(location %in% 
           c("Spain","Italy","France",
             "United Kingdom","Germany")) %>%
  filter(date>"2020-07-30") %>%
  ggplot(aes(x=date,
             y=new_cases_smoothed,
             color=location))+
  geom_line()+
  scale_color_manual(values=paleta_continentes)+
  xlab("")+
  ylab("Casos confirmados")+
  ggtitle("COVID-19: Evolución en Europa")+
  theme(legend.position = "top",
        legend.title = element_blank())

# comparando con otros continentes
covid_mundo %>% 
  filter(location %in% 
           c("Spain","United Kingdom",
             "India","Brazil","United States")) %>%
  filter(date>"2020-07-30") %>%
  ggplot(aes(x=date,
             y=new_cases_smoothed,
             color=location))+
  geom_line()+
  scale_color_manual(values=paleta_continentes)+
  xlab("")+
  ylab("Casos confirmados")+
  ggtitle("COVID-19: Evolución en Europa")+
  theme(legend.position = "top",
        legend.title = element_blank())

# normalizando a millón de habitantes
covid_mundo %>% 
  filter(location %in% 
           c("Spain","United Kingdom",
             "India","Brazil","United States")) %>%
  filter(date>"2020-07-30") %>%
  ggplot(aes(x=date,
             y=new_cases_smoothed_per_million,
             color=location))+
  geom_line()+
  scale_color_manual(values=paleta_continentes)+
  xlab("")+
  ylab("Casos confirmados por millón")+
  ggtitle("COVID-19: Evolución en Europa - Casos por millón de habitantes")+
  theme(legend.position = "top",
        legend.title = element_blank())

# podemos rellenar el área bajo la curva
covid_mundo %>% 
  filter(location %in% 
           c("Spain")) %>%
  ggplot(aes(x=date,
             y=new_cases_smoothed,
             color=location))+
  geom_line()+
  geom_area(fill="gold",alpha=0.5)+
  
  xlab("")+
  ylab("Casos confirmados")+
  ggtitle("COVID-19: Evolución en España")+
  theme(legend.position = "top",
        legend.title = element_blank())


### Vamos a hacer un diagrama de areas 
#   apiladas en serie temporal
lista_paises=c("Spain","United Kingdom",
               "France","Italy",
               "Germany",
               "Mexico",
               "United States","Brazil",
               "India","China","Japan")

# selecciono los paises
covid_selec <- covid_mundo %>%
               filter(location %in% lista_paises) %>% 
               filter(date>"2020-01-01")

# para cada día calculo los casos totales 
# en esta selección de países y la parte porcentual 
# del total de casos con que contribuye cada país
covid_selec <- covid_selec %>% 
               select(location,date,new_cases_smoothed) %>% 
                 
               arrange(date) %>% 
  
               group_by(date) %>% 
               mutate(casos_dia=sum(new_cases_smoothed,
                                    na.rm = T)) %>% 
  
               group_by(location) %>%
               mutate(pct_casos_dia=100*round((new_cases_smoothed)/casos_dia,4)) %>% 
  ungroup()

# represento
ggplot(covid_selec,
       aes(x=date,
           y=new_cases_smoothed,
           fill=location)) + 
  geom_area()+
  xlab("")+ylab("Casos confirmados diarios")+
  ggtitle("COVID-19: Evolución diaria de casos")+
  scale_fill_manual(name="País",
                    values=brewer.pal(n=11,name = 'RdYlBu'))
  
## ¿Qué porcentaje diario es provocado por cada país?
ggplot(covid_selec,
       aes(x=date, 
           y=pct_casos_dia,
           fill=location)) + 
    geom_area(alpha=0.7 , size=0.5, colour="black")+
xlab("")+ylab("% de casos")+
  ggtitle("COVID-19: % de casos por pais respecto al total diario")+
  scale_fill_manual(name="País",
                    values=brewer.pal(n=11,name = 'RdYlBu'))





######################### Graficos Avanzados ################################

######################## Facetas y agrupaciones #############################

pib_life_exp

# Simplemente añadiendo una capa más de facetas tenemos la agrupación
pib_life_exp+facet_grid(~continent)
pib_life_exp+facet_wrap(~continent)

# Así es posible ver evoluciones temporales
gp_anos<-gp %>% filter(year %in% seq(1970,2010,by=5) &
                       continent=="Europe")

evo_pib_life_exp<-ggplot(gp_anos,aes(gdp_pc,
                   life_expectancy,
                   color=region,
                   label=country)
                   )+
 geom_point(aes(size=population))+
 scale_color_manual(name="regiones: ",
                    values=paleta_continentes)+
 scale_x_log10()+
 scale_y_log10()+    
 xlab("PIB/capita $")+
 ylab("Años de Vida")+
 geom_hline(yintercept = 80,
            color="red",
            linetype="dashed")+  
 ggtitle("Europa: Evolución PIB per capita y Esperanza de vida")+  
 theme(legend.position = "top")

evo_pib_life_exp
evo_pib_life_exp+facet_wrap(~year)

## Mediante grid_Extra es posible combinar
# diferentes visualizaciones

#  esperanza de vida y pib per capita
g0<-gp_2010 %>%
  ggplot(
    aes(x=gdp_pc,
        y=life_expectancy,
        color=continent,
        label=country)
  )+
  geom_point(aes(size=population,shape=continent))+
  scale_colour_manual(values=paleta_continentes)+
  scale_x_log10()+
  scale_y_log10()+
  xlab("PIB/Capita $")+
  ylab("Años de vida")+
  theme(legend.position = "top")+
  ggtitle("Esperanza de vida y PIB per capita 2010")
g0

# mortalidad infantil por cada 1000 y pib per cap
g1<-gp_2010 %>%
  ggplot(aes(x=gdp_pc,
             y=infant_mortality,
             color=continent,
             label=country))+
  geom_point(aes(size=population,shape=continent))+
  scale_color_manual(name="",values=paleta_continentes)+
  scale_x_log10()+
  scale_y_log10()+
  xlab("PIB/capita $")+
  ylab("Mortalidad infantil")+
  ggtitle("Tasa de Mortalidad infantil y PIB per capita")+
  theme(legend.position = "top",legend.title = element_blank())

g1

# podemos cambiar la representación matricial
gridExtra::grid.arrange(g0,g1,ncol=1,nrow=2)  
gridExtra::grid.arrange(g0,g1,ncol=2,nrow=1)  


# Es posible hacer gráficos combinados aún más complejos

# fijo una semilla para que el generador aleatorio sea siempre igual
set.seed(123)
# calculo dos distribuciones normales bidimensionales
x <- c(rnorm(500, mean = -1), rnorm(500, mean = 1.5))
y <- c(rnorm(500, mean = 1), rnorm(500, mean = 1.7))
# les asigno un grupo
group <- as.factor(rep(c(1,2), each=500))
# las almaceno en un dataframe
df2 <- data.frame(x, y, group)
head(df2)

# Dispersion de cada grupo
scatterPlot <- ggplot(df2,aes(x, y, color=group)) + 
  geom_point() + 
  scale_color_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position=c(0,1),
        legend.justification=c(0,1))
# Distribucion marginal de x
xdensity <- ggplot(df2, aes(x, fill=group)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position = "none")
# Distribucion marginal de y
ydensity <- ggplot(df2, aes(y, fill=group)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position = "none")
# Creo una hoja en blanco
blankPlot <- ggplot()+geom_blank(aes(1,1))+
  theme(
    plot.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()
  )
# Junto todo sobre la hoja en blanco
gridExtra::grid.arrange(xdensity,
                        blankPlot,
                        scatterPlot,
                        ydensity, 
             ncol=2, nrow=2, 
             widths=c(4, 1.4),
             heights=c(1.4, 4))

# Otra forma, con viewport
library(grid)
# Creo una nueva página en blanco
grid.newpage()
# Creo un lienzo matricial : nrow = 2, ncol = 2
pushViewport(viewport(layout = grid.layout(2, 2)))
# Función de definición de posición en el lienzo
define_region <- function(row, col){
  viewport(layout.pos.row = row, 
           layout.pos.col = col)
} 
# Ordeno los diagramas en la matriz creada
print(scatterPlot, vp=define_region(1, 1:2))
print(xdensity, vp = define_region(2, 1))
print(ydensity, vp = define_region(2, 2))

############################################################################
######################## Gráficos Interactivos #############################

g1<-gp_2010 %>%
  ggplot(aes(x=gdp_pc,
             y=infant_mortality,
             color=continent,
             # establezco una etiqueta 
             label=country))+
  
  geom_point(aes(size=population,
                 shape=continent))+
  scale_color_manual(name="",
                     values=paleta_continentes)+
  scale_x_log10()+
  scale_y_log10()+
  xlab("PIB/capita $")+
  ylab("Mortalidad infantil")+
  ggtitle("Tasa de Mortalidad infantil y PIB per capita")+
  theme(legend.position = "top",legend.title = element_blank())

g1
ggplotly(g1)

ggplotly(pib_life_exp)

ggplotly(pib_life_exp+facet_wrap(~continent))

ggplotly(evo_pib_life_exp+facet_wrap(~year))

p
ggplotly(p)


############################################################################
################# Graficos 3D ###################################

require(scatterplot3d)
scatterplot3d(x=gp_2010$gdp_pc,
              y=gp_2010$life_expectancy,
              z=gp_2010$infant_mortality,
              xlab = "PIB/cap",
              ylab = "Esp. de Vida",
              zlab = "Mortalidad infantil",
              color = "blue",
              type = "h")

require(plot3D)
scatter3D(x = gp_2010$gdp_pc,
          y = gp_2010$life_expectancy,
          z = gp_2010$infant_mortality,
          col.var = gp$continent,
          col = paleta_continentes,
          ticktype="detailed",
          addlines=T,
          pch=18,
          xlab = "PIB/cap",
          ylab = "Esp. de Vida",
          zlab = "Mortalidad infantil",
          d=2,theta=45,phi=25,
          type = "h")


# La funcion scatter3d de car es mejor
car::scatter3d(x=gp_2010$gdp_pc,
              y=gp_2010$life_expectancy,
              z=gp_2010$infant_mortality,
              xlab = "PIB/cap",
              ylab = "Esp. de Vida",
              zlab = "Mortalidad infantil",
              color = "blue",
              type = "h")

car::scatter3d(x=gp_2010$gdp_pc,
               y=gp_2010$life_expectancy,
               z=gp_2010$infant_mortality,
               groups=gp_2010$continent,
               xlab = "PIB/cap",
               ylab = "Esp. de Vida",
               zlab = "Mortalidad infantil",
               grid=T)


car::scatter3d(x=gp_2010$gdp_pc,
               y=gp_2010$life_expectancy,
               z=gp_2010$infant_mortality,
               groups=gp_2010$continent,
               xlab = "PIB/cap",
               ylab = "Esp. de Vida",
               zlab = "Mortalidad infantil",
               grid=T,
               fit="smooth")

car::scatter3d(x=gp_2010$gdp_pc,
               y=gp_2010$life_expectancy,
               z=gp_2010$infant_mortality,
               groups=gp_2010$continent,
               xlab = "PIB/cap",
               ylab = "Esp. de Vida",
               zlab = "Mortalidad infantil",
               surface = F,
               ellipsoid = T,
               surface.col=paleta_continentes)


############################################################################
############## Animaciones ###################################

# En R es también relativamente fácil 
# crear animaciones
library(gganimate)

g_evo<-gp %>%
  ggplot(aes(x=gdp_pc,
             y=infant_mortality,
             color=continent,
             label=country))+
  geom_point(aes(size=population,shape=continent))+
  scale_color_manual(name="",values=paleta_continentes)+
  scale_x_log10()+
  scale_y_log10()+
  xlab("PIB/capita $")+
  ylab("Mortalidad infantil")+
  ggtitle("Tasa de Mortalidad infantil y PIB per capita")+
  theme(legend.position = "top",legend.title = element_blank())

g_evo
animacion1<-g_evo + transition_time(year) +
  labs(title = "Mortalidad infantil y PIB/capita - Año: {frame_time}")
animacion1

# la función anim_save funciona de modo análogo
# a la función ggsave 
# guardando en este caso la animación deseada 
# en el path especificado. 

# -Si no se indica la animación a guardar 
# la función toma por defecto la última disponible.

# -Si no se especifica el path toma el del workspace actual

anim_save("evo_mort.gif")
getwd()
anim_save("evo_mort.gif",
          animacion1,
          path ="C:/Users/jmartinezmo/Desktop/IFFE/s3_representacion_ggplot2")

g_evo + transition_time(year) +
  labs(title = "Mortalidad infantil y PIB/capita - Año: {frame_time}") +
  shadow_wake(wake_length = 0.1, alpha = FALSE)

### aplicable también a diagramas de densidad y boxplots

# BOXPLOT
evo_box<-gp %>% 
  ggplot(aes(x=continent,
             y=life_expectancy,
             color=continent))+
  geom_boxplot()+
  geom_jitter()+
  scale_color_manual(name="",
                     values=paleta_continentes)+
  xlab("")+
  ylab("Años de vida")+
  theme(legend.position = "top",
        legend.title = element_blank())
evo_box

evo_box + transition_time(year) +
  labs(title = "Evolución de la esperanza de vida por continentes - Año: {frame_time}")
anim_save("evo_esp_vida_box.gif")

# DENSIDAD
evo_dens<-gp %>%
  ggplot(aes(life_expectancy,color=continent,fill=continent))+
  geom_density(alpha=0.3)+
  scale_color_manual(values=paleta_continentes)+
  scale_fill_manual(values=paleta_continentes)+
  xlab("Años de vida")+
  ylab("conteo")+
  theme(legend.position = "top",
        legend.title = element_blank())

evo_dens + transition_reveal(year) +
  labs(title = "Evolución de la esperanza de vida por continentes - Año: {frame_time}")
anim_save("evo_esp_vida_dens.gif")

### útil también para ver la evolución en series temporales

covid_selec %>% head()

paleta_covid<-c("red","orange","darkblue",
                "brown","cyan","black")

evo_casos<-covid_selec %>% 
  filter(location %in% c("Spain","Italy","United States",
                         "India","United Kingdom","Brazil")) %>%
  ggplot(aes(date,
             new_cases_smoothed,
             color=location))+
  geom_line()+
  xlab("")+ylab("Casos diarios confirmados")+
  theme(legend.position = "top")+
  ggtitle("Casos diarios confirmados COVID-19")+
  scale_color_manual(name="",values = paleta_covid)

evo_casos

evo_casos + transition_reveal(date)
anim_save("evo_covid.gif")

# podemos desglosar por facetas para aumentar
# la visibilidad individual
evo_casos<-covid_selec %>% 
  filter(location %in% c("Spain","Italy","United States",
                         "India","United Kingdom","Brazil")) %>%
  ggplot(aes(date,
             new_cases_smoothed,
             color=location))+
  geom_line()+
  xlab("")+ylab("Casos diarios confirmados")+
  theme(legend.position = "top")+
  ggtitle("Casos diarios confirmados COVID-19")+
  scale_color_manual(name="",values = paleta_covid)+
  facet_wrap(~location)

evo_casos

evo_casos + transition_reveal(date)
anim_save("evo_covid_facet.gif")

# rangos similares
evo_casos<-covid_selec %>% 
  filter(location %in% c("Spain","Italy","Germany",
                         "United Kingdom","France")) %>%
  ggplot(aes(date,
             new_cases_smoothed,
             color=location))+
  geom_line()+
  xlab("")+ylab("Casos diarios confirmados")+
  theme(legend.position = "top")+
  ggtitle("Casos diarios confirmados COVID-19")+
  scale_color_manual(name="",values = paleta_covid)+
  facet_wrap(~location)

evo_casos

evo_casos + transition_reveal(date)
anim_save("evo_covid_facet_eu.gif")


##### Otra forma de realizar animaciones es mediante bucles:

# Creo los vértices de un triángulo 
# equilátero de lado unidad
vertice_0x=0
vertice_0y=0

vertice_Ax=1
vertice_Ay=0

vertice_Bx=0.5
vertice_By=sqrt(1-0.5^2)

# elijo un punto inicial al azar dentro del triángulo según 
# una distribución uniforme
# 
x_ini<-runif(n=1,min=0,max=1)
y_ini<-runif(n=1,min=0,max = x_ini)

# creo un df para guardar los datos 
# de cada iteración del proceso
simulacion<-data.frame(iteracion=0,
                       px=x_ini,py=y_ini)

# 3500 simulaciones
for(i in 1:3500){
  
# tomo el valor anterior
  x_anterior=simulacion$px[i]
  y_anterior=simulacion$py[i]
  
# en cada iteración tiro el dado
  dado=ceiling(runif(n=1,min=0,max=6))
  
# Calculo en cada iteración el punto de la 
# iteración siguiente mediante el punto medio
# del segmento que une el punto actual 
# con el vértice elegido por 
# el dado
  
  if(dado==1|dado==2){
    x_nuevo=(x_anterior+vertice_0x)/2
    y_nuevo=(y_anterior+vertice_0y)/2
    
  }else if(dado==3|dado==4){
    x_nuevo=(x_anterior+vertice_Ax)/2
    y_nuevo=(y_anterior+vertice_Ay)/2
    
  }else{
    x_nuevo=(x_anterior+vertice_Bx)/2
    y_nuevo=(y_anterior+vertice_By)/2
  }

  # almaceno el resultado de la iteración actual
  
  paso=data.frame(iteracion=i,
                  px=x_nuevo,
                  py=y_nuevo)
  
  simulacion=rbind(simulacion,paso)
  
  # en las iteraciones múltiplo de 100 guardo la imagen
  if(i%%100==0){
    
    plot_simulacion<-simulacion%>%
      ggplot(aes(x=px,
                 y=py,
                 color=py))+
      geom_point()+
      scale_color_gradient(high="orange",
                           low="blue")+
      ggtitle(paste("Simulación número: ",i))
    
    ggsave(file=paste("simulacion_",i,".jpeg",sep=""),
           path="C:/Users/jmartinezmo/Desktop/IFFE/s3_representacion_ggplot2/simulacion",
           width = 14,
           height = 10,
           units = "cm")
  }
    
}
# una vez guardadas las imágenes pueden ser tratadas
# por cualquier programa gráfico (GIMP o Photoshop)
# El paquete magick puede ser una buena opción si no se dispone de uno


# Otra forma es emplear gganimate como en los ejemplos anteriores
sierpinsky<-simulacion%>%
  ggplot(aes(x=px,y=py,color=py))+
  geom_point()+
  scale_color_gradient(high="orange",low="blue")

anim<-sierpinsky +
  transition_time(iteracion) +
  labs(title = "Simulación Triángulo de Sierpinsky 3500 iteraciones")+
  shadow_mark(alpha = 0.9, size = 0.9)

anim
anim_save("sierpinsky.gif",
          path="C:/Users/jmartinezmo/Desktop/IFFE/s3_representacion_ggplot2")




