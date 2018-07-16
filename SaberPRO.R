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

# CREAR VARIABLES DE INTERÉS

# Crear Variable Unal

SBPRO_2017_GEN <- SBPRO_2017_GEN %>% 
  mutate(Unal =  case_when(.$INST_COD_INSTITUCION %in% c(1101, 1102, 1103, 1104, 1124, 1125, 1126, 9920) ~ "UN",
                           TRUE ~ "Resto IES"))
# Crear Sedes Unal

SBPRO_2017_GEN <- SBPRO_2017_GEN %>% 
  mutate(Sedes =  case_when(.$INST_COD_INSTITUCION == 1101 ~ "UN-Bogotá",
                            .$INST_COD_INSTITUCION == 1102 ~ "UN-Medellín",
                            .$INST_COD_INSTITUCION == 1103 ~ "UN-Manizales",
                            .$INST_COD_INSTITUCION == 1104 ~ "UN-Palmira",
                            .$INST_COD_INSTITUCION == 1124 ~ "UN-Orinoquía",
                            .$INST_COD_INSTITUCION == 1125 ~ "UN-Amazonía",
                            .$INST_COD_INSTITUCION == 1126 ~ "UN-Caribe",
                            .$INST_COD_INSTITUCION == 9920 ~ "UN-Tumaco",
                            TRUE ~ "Resto IES"))

# Crear G12

SBPRO_2017_GEN <- SBPRO_2017_GEN %>% 
  mutate(G12 =  case_when(.$INST_COD_INSTITUCION == 1813  ~ "U. de los Andes",
                          .$INST_COD_INSTITUCION == 1714  ~ "U. del Rosario",
                          .$INST_COD_INSTITUCION == 1706  ~ "U. Externado",
                          .$INST_COD_INSTITUCION == 1828  ~ "ICESI",
                          .$INST_COD_INSTITUCION %in% c(1201, 1219, 1222, 1223, 9125) ~ "U. de Antioquia",
                          .$INST_COD_INSTITUCION == 1712  ~ "EAFIT",
                          .$INST_COD_INSTITUCION == 1203  ~ "U. del Valle",
                          .$INST_COD_INSTITUCION %in% c(1701, 1702)  ~ "U. Javeriana",
                          .$INST_COD_INSTITUCION == 1204  ~ "UIS",
                          .$INST_COD_INSTITUCION == 1713  ~ "U. del Norte",
                          .$INST_COD_INSTITUCION %in% c(1710, 1723, 1727, 1730)  ~ "U. Bolivariana",
                          .$INST_COD_INSTITUCION %in% c(1101, 1102, 1103, 1104)  ~ "U. Nacional",
                          TRUE ~ "Resto IES"))

# Crear G16 (sedes Unal)
SBPRO_2017_GEN <- SBPRO_2017_GEN %>% 
  mutate(G15 =  case_when(.$INST_COD_INSTITUCION == 1813  ~ "U. de los Andes",
                          .$INST_COD_INSTITUCION == 1714  ~ "U. del Rosario",
                          .$INST_COD_INSTITUCION == 1706  ~ "U. Externado",
                          .$INST_COD_INSTITUCION == 1828  ~ "ICESI",
                          .$INST_COD_INSTITUCION %in% c(1201, 1219, 1222, 1223, 9125) ~ "U. de Antioquia",
                          .$INST_COD_INSTITUCION == 1712  ~ "EAFIT",
                          .$INST_COD_INSTITUCION == 1203  ~ "U. del Valle",
                          .$INST_COD_INSTITUCION %in% c(1701, 1702)  ~ "U. Javeriana",
                          .$INST_COD_INSTITUCION == 1204  ~ "UIS",
                          .$INST_COD_INSTITUCION == 1713  ~ "U. del Norte",
                          .$INST_COD_INSTITUCION %in% c(1710, 1723, 1727, 1730)  ~ "U. Bolivariana",
                          .$INST_COD_INSTITUCION == 1101  ~ "UN-Bogotá",
                          .$INST_COD_INSTITUCION == 1102  ~ "UN-Medellín",
                          .$INST_COD_INSTITUCION == 1103  ~ "UN-Manizales",
                          .$INST_COD_INSTITUCION == 1104  ~ "UN-Palmira",
                          TRUE ~ "Resto IES"))

# Remover datos faltantes de grupos de referencia

SBPRO_2017_GEN <- SBPRO_2017_GEN %>% filter(!is.na(GRUPOREFERENCIA))

#######################################################
################# ANÁLISIS POR IES
#######################################################

################ BOX PLOT DE PUNTAJES GLOBALES 

# Puntos mínimos y máximos (SaberPRO - Interpretación)

ggplot(data = SBPRO_2017_GEN, aes(y = PUNT_GLOBAL, x = Unal)) +
  ylim(0, 300) + geom_hline(yintercept = 180, col = "red", size = 1)+
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") 

# Distribución puntajes globales 

ggplot(data = SBPRO_2017_GEN, aes(y = PUNT_GLOBAL, x = 1)) + geom_boxplot(outlier.color = "green", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = 180, col = "red", size = 1)+
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") 

# Distribución puntajes globales y Unal

ggplot(data = SBPRO_2017_GEN, aes(y = PUNT_GLOBAL, x = Unal)) + geom_boxplot(outlier.color = "green", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = 180, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") 

# Distribución puntajes globales, unal y sedes

### crear base de datos 

Unal <- SBPRO_2017_GEN %>% select(PUNT_GLOBAL, Unal) %>% rename(Categoria = Unal)
sedes <- SBPRO_2017_GEN %>% select(PUNT_GLOBAL, Sedes) %>% 
  filter(Sedes != "Resto IES") %>% rename(Categoria = Sedes)
Global <- bind_rows(Unal, sedes)

### Puntajes Sedes Unal y Resto

ggplot(data = Global, aes(y = PUNT_GLOBAL, x = fct_reorder(Categoria, PUNT_GLOBAL, fun = median, .desc =TRUE))) + 
  geom_boxplot(outlier.color = "green", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = 180, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") +
  ylab("Puntaje Promedio Global SaberPRO 2017")+
  xlab("Categorías")


# Distribución G12

ggplot(data = SBPRO_2017_GEN, aes(y = PUNT_GLOBAL, x = fct_reorder(G12, PUNT_GLOBAL, fun = median))) + 
  geom_boxplot(outlier.color = "green", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = 180, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") +
  ylab("Puntaje Promedio Global SaberPRO 2017")+
  xlab("Universidades")+coord_flip()

# Distribución G15

ggplot(data = SBPRO_2017_GEN, aes(y = PUNT_GLOBAL, x = fct_reorder(G15, PUNT_GLOBAL, fun = median))) + 
  geom_boxplot(outlier.color = "green", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = 180, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") +
  ylab("Puntaje Promedio Global SaberPRO 2017")+
  xlab("Universidades")+coord_flip()

###### DISTRIBUCIÓN POR PRUEBAS GENÉRICAS

### Competencias Ciudadanas

ggplot(data = SBPRO_2017_GEN, aes(y = MOD_COMPETEN_CIUDADA_PUNT, x = fct_reorder(G15, MOD_COMPETEN_CIUDADA_PUNT, fun = median(na.rm = TRUE)))) + 
  geom_boxplot(outlier.color = "green", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = 180, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") +
  ylab("Puntaje Promedio Global SaberPRO 2017")+
  xlab("Universidades")+coord_flip()+
  theme(axis.text.y = element_text(size = 13, face = "bold"),
        axis.title = element_text(face="bold", color="black", size=16)
  )

### Lectura Crítica

ggplot(data = SBPRO_2017_GEN, aes(y = MOD_LECTURA_CRITICA_PUNT, x = fct_reorder(G15, MOD_LECTURA_CRITICA_PUNT, fun = median(na.rm = TRUE)))) + 
  geom_boxplot(outlier.color = "green", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = 180, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") +
  ylab("Puntaje Promedio Global SaberPRO 2017")+
  xlab("Universidades")+coord_flip()+
  theme(axis.text.y = element_text(size = 13, face = "bold"),
        axis.title = element_text(face="bold", color="black", size=16)
  )

### Razonamiento Cuantitativo

ggplot(data = SBPRO_2017_GEN, aes(y = MOD_RAZONA_CUANTITAT_PUNT, x = fct_reorder(G15, MOD_RAZONA_CUANTITAT_PUNT, fun = median(na.rm = TRUE)))) + 
  geom_boxplot(outlier.color = "green", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = 180, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") +
  ylab("Puntaje Promedio Global SaberPRO 2017")+
  xlab("Universidades")+coord_flip()+
  theme(axis.text.y = element_text(size = 13, face = "bold"),
        axis.title = element_text(face="bold", color="black", size=16)
  )

### Comunicación Escrita

SBPRO_2017_GEN1 <- SBPRO_2017_GEN %>% filter(!is.na(MOD_COMUNI_ESCRITA_PUNT))

ggplot(data = SBPRO_2017_GEN1, aes(y = MOD_COMUNI_ESCRITA_PUNT, x = fct_reorder(G15, MOD_COMUNI_ESCRITA_PUNT, fun = median(na.rm = TRUE)))) + 
  geom_boxplot(outlier.color = "green", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = 180, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") +
  ylab("Puntaje Promedio Global SaberPRO 2017")+
  xlab("Universidades")+coord_flip()+
  theme(axis.text.y = element_text(size = 13, face = "bold"),
        axis.title = element_text(face="bold", color="black", size=16)
  )

### Inglés

ggplot(data = SBPRO_2017_GEN, aes(y = MOD_INGLES_PUNT, x = fct_reorder(G15, MOD_INGLES_PUNT, fun = median(na.rm = TRUE)))) + 
  geom_boxplot(outlier.color = "green", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = 180, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") +
  ylab("Puntaje Promedio Global SaberPRO 2017")+
  xlab("Universidades")+coord_flip()+
  theme(axis.text.y = element_text(size = 13, face = "bold"),
        axis.title = element_text(face="bold", color="black", size=16)
  )

#######################################################
################# ANÁLISIS POR PROGRAMAS ACADÉMICOS
#######################################################


QCuantitativo <- SBPRO_2017_GEN %>% 
  mutate(QRazCuant =  case_when(.$MOD_RAZONA_CUANTITATIVO_PNAL <= 20  ~ "Quintil 1",
                                .$MOD_RAZONA_CUANTITATIVO_PNAL > 20 & .$MOD_RAZONA_CUANTITATIVO_PNAL <= 40 ~ "Quintil 2",
                                .$MOD_RAZONA_CUANTITATIVO_PNAL > 40 & .$MOD_RAZONA_CUANTITATIVO_PNAL <= 60 ~ "Quintil 3",
                                .$MOD_RAZONA_CUANTITATIVO_PNAL > 60 & .$MOD_RAZONA_CUANTITATIVO_PNAL <= 80 ~ "Quintil 4",
                                TRUE ~ "Quintil 5"
                                )) %>% 
  select(G12,G15, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  filter(G12 == "U. Nacional") %>% 
  group_by(G15, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  summarise(Total = n()) %>% spread(QRazCuant, Total) %>% 
  replace(., is.na(.), 0)





















names(SBPRO_2017_GEN)
  
  MOD_RAZONA_CUANTITATIVO_PNAL
  
  
  mutate(G12 =  case_when(.$INST_COD_INSTITUCION == 1813  ~ "U. de los Andes",
                          .$INST_COD_INSTITUCION == 1714  ~ "U. del Rosario",
                          .$INST_COD_INSTITUCION == 1706  ~ "U. Externado",
                          .$INST_COD_INSTITUCION == 1828  ~ "ICESI",
                          .$INST_COD_INSTITUCION %in% c(1201, 1219, 1222, 1223, 9125) ~ "U. de Antioquia",
                          .$INST_COD_INSTITUCION == 1712  ~ "EAFIT",
                          .$INST_COD_INSTITUCION == 1203  ~ "U. del Valle",
                          .$INST_COD_INSTITUCION %in% c(1701, 1702)  ~ "U. Javeriana",
                          .$INST_COD_INSTITUCION == 1204  ~ "UIS",
                          .$INST_COD_INSTITUCION == 1713  ~ "U. del Norte",
                          .$INST_COD_INSTITUCION %in% c(1710, 1723, 1727, 1730)  ~ "U. Bolivariana",
                          .$INST_COD_INSTITUCION %in% c(1101, 1102, 1103, 1104)  ~ "U. Nacional",
                          TRUE ~ "Resto IES"))
MOD_RAZONA_CUANTITATIVO_PNAL






SBPRO_2017_GEN1 <- SBPRO_2017_GEN %>% filter(!is.na(MOD_COMPETEN_CIUDADA_PUNT))

ggplot(data = SBPRO_2017_GEN1, aes(y = MOD_COMPETEN_CIUDADA_PUNT, x = fct_reorder(G15, MOD_COMPETEN_CIUDADA_PUNT, fun = median(na.rm = TRUE)))) + 
  geom_boxplot(outlier.color = "green", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = 180, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") +
  ylab("Puntaje Promedio Global SaberPRO 2017")+
  xlab("Universidades")+coord_flip()+
  theme(axis.text.y = element_text(size = 13, face = "bold"),
        axis.title = element_text(face="bold", color="black", size=16)
                )

sum(is.na(SBPRO_2017_GEN$MOD_COMUNI_ESCRITA_PUNT))


median(c(1,2,3, NA))

###############################
### Grupos de Referencia
###############################

# Distribución puntajes globales por área

ggplot(data = SBPRO_2017_GEN, aes(x= GRUPOREFERENCIA, y = PUNT_GLOBAL)) + geom_boxplot(outlier.color = "green") +
  ylim(0, 300) + geom_hline(yintercept = 146, col = "red", size = 1) + coord_flip()+
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") 


# Distribución puntajes globales por grupos de referencia

ggplot(data = SBPRO_2017_GEN, aes(x= GRUPOREFERENCIA, y = PUNT_GLOBAL, fill = Unal)) + geom_boxplot(outlier.color = "green") +
  ylim(0, 300) + geom_hline(yintercept = 146, col = "red", size = 1) + coord_flip()+
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") 


ggplot(data = SBPRO_2017_GEN, aes(x= Unal, y = PUNT_GLOBAL)) + geom_boxplot(outlier.color = "green") +
  ylim(0, 300) + geom_hline(yintercept = 146, col = "red", size = 1) +
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

G10

# 1813 Universidad de los Andes
# 1714 Universidad del Rosario
# 1706 Universidad Externado
# 1828 ICESI
# 1201 Universidad de Antioquia
# 1712 EAFIT
# 1203 Universidad del Valle
# 1701 1702 Universidad Javeriana
# 1204 UIS
# 1713 U del Norte
# 1710 1723 1727 1730 U pont Bolivariana
########  Unal
# 1101	Bogotá
# 1102	Medellín
# 1103	Manizales
# 1104	Palmira
# 1124	Orinoquía
# 1125	Amazonía
# 1126	Caribe
# 9920	Tumaco


#######################################################
################# ANÁLISIS POR PROGRAMAS ACADÉMICOS
#######################################################



defaul_locale


stocks <- tibble(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)

gather(stocks, stock, price, -time)
stocks %>% gather(stock, price, -time)
