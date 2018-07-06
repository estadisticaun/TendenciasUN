# ANÁLISIS DE RESULTADOS DE LOS EXAMENES SABER PRO

# LIBRERÍAS REQUERIDAS
library(readxl)
library(tidyverse)


# IMPORTAR DATOS SABER PRO 2017

SBPRO_2017_GEN <- read_excel("Datos/SBPRO-2017-GEN.xlsx")

# SELECCIONAR VARIABLES DE INTERÉS

SBPRO_2017_GEN <- SBPRO_2017_GEN %>% select(c(INST_COD_INSTITUCION:GRUPOREFERENCIA, 
  MOD_RAZONA_CUANTITAT_PUNT, MOD_RAZONA_CUANTITATIVO_PNAL, MOD_RAZONA_CUANTITATIVO_PGREF,
  MOD_LECTURA_CRITICA_PUNT, MOD_LECTURA_CRITICA_PNAL, MOD_LECTURA_CRITICA_PGREF,
  MOD_COMPETEN_CIUDADA_PUNT, MOD_COMPETEN_CIUDADA_PNAL, MOD_COMPETEN_CIUDADA_PGREF,
  MOD_INGLES_PUNT, MOD_INGLES_PNAL, MOD_INGLES_PGREF, MOD_COMUNI_ESCRITA_PUNT,
  MOD_COMUNI_ESCRITA_PNAL, MOD_COMUNI_ESCRITA_PGREF, PUNT_GLOBAL, PERCENTIL_GLOBAL))

# BOX PLOT DE PUNTAJES GLOBALES POR GRUPOS DE REFERENCIA

# remover datos faltantes de puntaje global

SBPRO_2017_GEN <- SBPRO_2017_GEN %>% filter(!is.na(GRUPOREFERENCIA))

# Distribución puntajes globales por área

ggplot(data = SBPRO_2017_GEN, aes(x= GRUPOREFERENCIA, y = PUNT_GLOBAL)) + geom_boxplot(outlier.color = "green") +
  ylim(0, 300) + geom_hline(yintercept = 146, col = "red", size = 1) + coord_flip()+
geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") 

# BOX PLOT DE PUNTAJES GLOBALES POR GRUPOS DE REFERENCIA CON UNAL

# Crear Variable Unal

SBPRO_2017_GEN <- SBPRO_2017_GEN %>% 
  mutate(Unal =  case_when(.$INST_COD_INSTITUCION %in% c(1101, 1102, 1103, 1104, 1124, 1125, 1126, 9920) ~ "UN",
                                                 TRUE ~ "Resto IES"))

# Distribución puntajes globales por área Unal

ggplot(data = SBPRO_2017_GEN, aes(y = PUNT_GLOBAL, x = Unal)) + geom_boxplot(outlier.color = "green", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = 146, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") 

ggplot(data = SBPRO_2017_GEN, aes(x= GRUPOREFERENCIA, y = PUNT_GLOBAL, fill = Unal)) + geom_boxplot(outlier.color = "green") +
  ylim(0, 300) + geom_hline(yintercept = 146, col = "red", size = 1) + coord_flip()+
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") 


table(SBPRO_2017_GEN$Unal)




# SNIES	Sede
# 1101	Bogotá
# 1102	Medellín
# 1103	Manizales
# 1104	Palmira
# 1124	Orinoquía
# 1125	Amazonía
# 1126	Caribe
# 9920	Tumaco



