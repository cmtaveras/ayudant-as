
# Ayundantía 2: Visualización de Datos ------------------------------------

# INTRODUCCIÓN A LOS MÉTODOS CUANTITATIVOS EN EDUCACIÓN
# EDU4045
# Facultad de Educación, UC 
# Profesor: Ernesto Treviño
# Ayudantes: Carmen Taveras, Catalina Miranda 


# Objetivos ----------------------------------------------------------------

# Interpretar estadísticos descriptivos 

# Visualizar datos y estadísticos descriptivos


# Librerías  -----------------------------------------------------------

# Ojo: Los paquetes deben ser descargados para que las librerías puedan abrir

library(readxl) # sirve abrir archivos .xls
library(dplyr) # sirve para hacer manipulaciones a los datos
library(tidyverse) # sirve para hacer manipulaciones a los datos
library(ggplot2) # sirve para hacer figuras y gráficos 
library (writexl) # sirve para guardar archivos de R a .xlsx
library(psych) #sirve para correr estadisticos descriptivos
library(RColorBrewer) #sirve para ver los colores en R

# Verificar directorio de trabajo y project -------------------------------

# Verificamos el directorio del trabajo y que tengamos el project abierto 

# Es importante saber en que directorio estás para cargar datos correctamente

getwd()

# Cargar Base de Datos ----------------------------------------------------

# Cargamos base de datos "votación municipal" que contiene seis variables

data <- read_excel("votacion_municipal.xlsx")

data |> glimpse()   #inspeccionar datos
str(data) # inspeccionar datos

# Objetivo 1: Explorar Estadísticos Descriptivos (ED) --------------------------------------

# Primero, recordemos que son los ED: 

# Media: valor promedio de los datos. ej, cuál es la edad promedio curso? 

# Mediana: valor que se encuentra justo en el "medio". ej, cuál es la mediana de la edad del curso? 

# Moda: valor que más se repite. ej, cuál es la edad que más se repite? 

# Desviación Estándar: mide que tanto se alejan los datos de la media. ej, cuál es la desviación estándar de la edad? 

# Varianza: mide la dispersión de los datos respecto de la media. ej, cuál es la varianza de la edad?

# No incluimos a la varianza en la tabla de estadísticos descriptivos pues no la interpretamos; interpretamos la desviación estándar 

# Segundo, creamos nuestros propios estadísticos descriptivos

# Entonces, creemos una tabla de estadísticos descriptivos para variables "conoc_civic" y "edad" 

base1 <- data %>% 
  select(conoc_civic,edad)

base1 |> glimpse() 

descriptivos <- describe(base1) 

write_xlsx(descriptivos,"descriptivos.xlsx") # esta tabla la pueden editar en excel al formato APA 7 

# Pensemos: 

# Cuál es el puntaje promedio de conocimiento cívico? 

# Cuál es la desviación estándar de la edad? 

# Ahora veamos estadísticos descriptivos según género 

# Creamos las etiquetas adecuadas: 

base2<- data %>% 
  select(genero,conoc_civic,edad)

# Hablar sobre el tipo de variable factor (0,1, "hombre", "mujer")

base2 <- mutate(base2, genero_factor = factor(base2$genero, 
                                            labels = c("Hombre", "Mujer")))

base2_des <- base2 %>%  
  group_by(genero_factor) %>%  
  summarise(
    media = mean(conoc_civic, na.rm = TRUE), # Agregar media/promedio
    mediana = median(conoc_civic, na.rm = TRUE), # Agregar mediana
    q1 = quantile(conoc_civic, probs = 0.25, na.rm = TRUE), # Agregar cuartil 1 (Q1)
    des = sd(conoc_civic, na.rm = TRUE), # Agregar desviacion estandar
    max = max(conoc_civic, na.rm = TRUE), # Agregar maximo
    min = min(conoc_civic, na.rm = TRUE), # Agregar minimo
    rango = max(conoc_civic) - min(conoc_civic), #Agregar rango
    N = n(), # Agregar N total  (muestra total)
    porcentaje = n()/30 #Agregar porcentaje, por qué dividimos entre 30? 
  )

write_xlsx(base2_des,"descriptivos_genero.xlsx") # esta tabla la pueden editar en excel al formato APA 7 

# Pensemos 

# Cuál es el puntaje promedio de conocimiento cívico de las mujeres? 

# Cuál puntaje se aleja más del promedio? 

# Cuál es el N de hombres? Y de mujeres? 


# Objetivo 2: Visualización de datos: Histograma --------------------------------------------------

# Histograma: representación de la distribución 

# Vamos a graficar la distribución del puntaje de conocimiento cívico 

# Primero, calculamos los límites superiores e inferiores para 1 y 2 DE.

# Para 1 Desviacion Estandar (DE)----

sup <-mean(base2$conoc_civic, na.rm = T)+ sd(base2$conoc_civic, na.rm = T)
inf <-mean(base2$conoc_civic, na.rm = T)- sd(base2$conoc_civic, na.rm = T)

# Para 2 DE

sup2<-mean(base2$conoc_civic, na.rm = T)+2*sd(base2$conoc_civic, na.rm = T)
inf2<-mean(base2$conoc_civic, na.rm = T)-2*sd(base2$conoc_civic, na.rm = T)

hist_preg2 <- ggplot(base2, aes(x=conoc_civic)) +
  geom_histogram(fill="pink",colour="pink4", bins = 30) +  # Adjust 'bins' as needed
  theme_minimal() +
  geom_vline(aes(xintercept = mean(conoc_civic, na.rm = TRUE)),
             linetype="dotdash", size=1, colour="violet") +
  geom_vline(aes(xintercept = median(conoc_civic, na.rm = TRUE)),
             linetype="dotdash", size=1, colour="hotpink") +
  geom_vline(aes(xintercept = sup),
             linetype="dotdash", size=1, colour="plum") +
  geom_vline(aes(xintercept = inf),
             linetype="dotdash", size=1, colour="orchid") +
  geom_vline(aes(xintercept = sup2),
             linetype="dotdash", size=1, colour="mediumpurple") +
  geom_vline(aes(xintercept = inf2),
             linetype="dotdash", size=1, colour="plum4") +
  labs(title="Distribución de puntaje de conocimiento cívico", x="Puntaje de conocimiento cívico", y="Frecuencia")

print(hist_preg2)

# Tips para ver paletas de colores en R

# display.brewer.all() con library(RColorBrewer) 

# https://www.nceas.ucsb.edu/sites/default/files/2020-04/colorPaletteCheatsheet.pdf 

# Ahora veamos un histograma según género. 

hist_preg3 <- ggplot(base2, aes(x = conoc_civic, fill = genero_factor)) +
  geom_histogram(position = "identity", bins = 30, alpha = 0.7, color = "black") +  
  theme_minimal() +
  geom_vline(aes(xintercept = mean(conoc_civic, na.rm = TRUE)), 
             linetype = "dotdash", size = 1, colour = "red") +
  geom_vline(aes(xintercept = median(conoc_civic, na.rm = TRUE)), 
             linetype = "dotdash", size = 1, colour = "magenta") +
  geom_vline(aes(xintercept = sup), 
             linetype = "dotdash", size = 1, colour = "paleturquoise4") +
  geom_vline(aes(xintercept = inf), 
             linetype = "dotdash", size = 1, colour = "royalblue") +
  geom_vline(aes(xintercept = sup2), 
             linetype = "dotdash", size = 1, colour = "mediumturquoise") +
  geom_vline(aes(xintercept = inf2), 
             linetype = "dotdash", size = 1, colour = "royalblue4") +
  labs(title = "Distribución de puntaje de conocimiento cívico", 
       x = "Puntaje de conocimiento cívico", y = "Frecuencia", fill = "genero_factor")

print(hist_preg3)

# Otra forma de hacer el Histograma : sin crear un objeto y con opción facet_grid

ggplot(base2, aes(x=conoc_civic, color=genero_factor, fill=genero_factor)) +
  geom_histogram(position="identity", alpha=0.2)+
  scale_color_manual(values=c("yellow", "Pink"))+
  scale_fill_manual(values=c("yellow", "Pink"))+
  theme_classic()+
  geom_vline(aes(xintercept = mean(conoc_civic, na.rm = T)),
             linetype="dotted", size=1, colour="navyblue")+
  geom_vline(aes(xintercept = median(conoc_civic, na.rm = T)),
             linetype="dotted", size=1, colour="skyblue")+
  facet_grid(~genero_factor)+
  labs(title="Escala de Conocimiento Cívico por Género",
       #subtitle= "O=hombres 1=mujeres",
       x="puntaje de conocimiento cívico",
       y="cantidad de hombres o mujeres",
       caption = "Fuente de datos: ICCS 2016")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))


# Visualización de datos: Boxplot -----------------------------------------

if(!require(devtools)) install.packages("devtools") #devtools es un paquete que descarga desde repositorios
devtools::install_github("kassambara/ggpubr")

library(ggpubr) #para hacer boxplots 

# Para hacer el boxplot

# Boxplot: Se ordenan el conjunto de datos y se identifica el valor que está en la posición central.

# El ancho de la caja nos indica la dispersión de los datos

ggplot(base2, aes(x=genero_factor, y=conoc_civic)) +
  geom_boxplot(color= c("palegreen", "turquoise")) +
  stat_summary(fun.y = mean, color="hotpink")+
  theme_classic() +
  labs(title="Puntaje de Conocimiento Cívico Según Género",
       #subtitle= "O=hombres 1=mujeres",
       x="Género",
       y="Puntaje de conocimiento cívico",
       caption = "Fuente de datos: ICCS 2016")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))


# Resumen de la Sesión ----------------------------------------------------

# Hicimos una tabla con estadísticos descriptivos y la exportamos a excel. 

# Interpretamos la distribución de datos a través de un histograma. 

# Convertimos la variable género a genero_factor, para poder categorizar la muestra. 

# Interpretamos la distribución de datos a través de un histograma según género 

# Visualizamos el promedio o la media a través de un boxplot según género 

# Con esta información podemos tener una idea de como se distribuyen los datos para más adelante continuar con análisis más específicos 
