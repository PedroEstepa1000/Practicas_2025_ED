
# Librerias ---------------------------------------------------------------
# install.packages("dplyr")
# install.packages("data.table")
# install.packages("readxl")
library(dplyr)
library(data.table)
library(readxl)

options(scipen = 999) #para evitar notacion cientifica

# Carga datos -------------------------------------------------------------
datos <- fread(input="Data/CNA2014_ENCABEZADO_15.csv",
               sep = ",") %>%
  select(COD_VEREDA,S05_TENENCIA,TIPO_UC,P_S5PAUTOS,P_S7P82,P_S7P84F,P_S7P85B) %>% 
  filter(TIPO_UC == 1) %>% 
  mutate(S05_TENENCIA = as.character(S05_TENENCIA))
str(datos)
glimpse(datos)
# Limpieza de datos -------------------------------------------------------

t_homologacion_7 <- readxl::read_excel(path = "Data/Tablasdehomologacion.xlsx",
                                       sheet = "Hoja2") %>% 
  mutate(S05_TENENCIA = as.character(S05_TENENCIA))
str(t_homologacion_7)

#el left_join permite unir las colomnas con el principio de union de conjuntos,
#otra funcion similar es el inner_join, que usa el principio de interseccion
datos_dep <- datos %>%
  left_join(t_homologacion_7, by = c("S05_TENENCIA"="S05_TENENCIA")) %>% 
  select(Predominancia_de_tenencia_en_la_Unidad_productora, P_S7P85B) %>% 
  na.omit()

# TDF Cualitativa ---------------------------------------------------------
#tabla de distribución de frecuencia (TDF)

tdf_S05_TENENCIA <- datos_dep %>% 
  group_by(Predominancia_de_tenencia_en_la_Unidad_productora) %>% 
  summarise(n_i = n()) %>% 
  arrange(desc(n_i)) %>% 
  mutate(N_i = cumsum(n_i),
         f_i = n_i/sum(n_i),
         F_i = cumsum(f_i))

# Grafico
barplot(table(datos_dep$Predominancia_de_tenencia_en_la_Unidad_productora))

#Usando esquisse
# install.packages("esquisse")
# install.packages("plotly")
library(esquisse)
library(plotly)
#esquisse::esquisser(viewer = "browser")  #permite conectarse con la web para el grafico

ggplot(tdf_S05_TENENCIA) +
  aes(
    x = f_i,
    y = Predominancia_de_tenencia_en_la_Unidad_productora
  ) +
  geom_bar(stat = "summary", fun = "sum", fill = "#112446") +
  labs(
    y = "Tenencia en la unidad productora",
    title = "Predominancia de tenencia",
    subtitle = "CNA"
  ) +
  theme_minimal()


#Jugar con la funcion esquisse para mejorar el grafico. 

# TDF - Variable cuanti ---------------------------------------------------


#Leer paquete DT, datatable

# Numero de clases

# k = 1 + 3.3 * log10(nrow(datos_dep))
# k
k = round(1 + 3.3 * log10(nrow(datos_dep)))  
k

# Rango

# rango = max(datos$P_S7P85B) - min(datos$P_S7P85B)
# rango
rango = max(datos_dep$P_S7P85B, na.rm = T) - 
  min(datos$P_S7P85B, na.rm =T)
rango

# Longitud
longitud = rango/k
longitud

cortes <- min(datos_dep$P_S7P85B, na.rm = T) + c(seq(0,k,1))*longitud
cortes


# TDF - Leche -------------------------------------------------------------
 
# tdf_P_S7P85B <- datos_dep %>% 
#   mutate(P_S7P85B_c = (cut(P_S7P85B,
#                            breaks = cortes,
#                            include.lowest = T,
#                            dig.lab = 6))) %>% 
#   group_by(P_S7P85B_c) %>% 
#   summarise(n_i = n()) #En este caso con esta forma la nueva tabla de datos
# #contiene solamente los intervalos donde si hay registros, para que aparezcan
# #los intervalos donde no esta la siguiente forma


# tdf_P_S7P85B <- datos_dep %>%
#   mutate(P_S7P85B_c = as.factor(cut(P_S7P85B,
#                            breaks = cortes,
#                            levels = cortes,
#                            include.lowest = T,
#                            dig.lab = 6))) %>%
#   group_by(P_S7P85B_c, .drop = F, .add = F) %>%
#   summarise(n_i = n())

#la siguiente es el codigo mas corto, pero se le agrega todas las columnas 
# de una tabla de frecuencias


tdf_P_S7P85B <- datos_dep %>% 
  mutate(P_S7P85B_c = (cut(P_S7P85B,
                          breaks = cortes,
                          include.lowest = T,
                          dig.lab = 6))) %>% 
  group_by(P_S7P85B_c, .drop = F, .add = F) %>% 
  summarise(n_i = n()) %>% 
  mutate(N_i = cumsum(n_i),
         f_i = n_i/sum(n_i),
         F_i = cumsum(f_i),
         x_i = cortes[1:k] + longitud/2,  #la marca de clase
         c_i = abs(cortes[1:k] - cortes [2 : (k+1)]),
         d_i = n_i/c_i) 
sum(tdf_P_S7P85B$f_i)
sum(tdf_P_S7P85B$n_i) %>% 
 

# Histograma

hist(datos_dep$P_S7P85B)

mean(datos_dep$P_S7P85B)
median(datos_dep$P_S7P85B)
