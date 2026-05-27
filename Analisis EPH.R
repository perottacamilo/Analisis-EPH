library(tidyverse)    # Para el manejo de variables
library(ggplot2)      # Está incluído en tidyverse (para hacer gráficos)
library(dplyr)        # Está incluído en tidyverse (para el manejo de variables)
library(haven)        # Para importar archivos de tipo .dta o .sav  
library(readxl)       # Para importar archivos de excel
library(writexl)      # Para exportar archivos de excel
library(skimr)        # Genera resúmenes estadísticos más completos que
library(janitor)      # Limpieza de nombre de ariables
library(broom)        # Convierte resultados de modelos en data frames ordenados.
library(sandwich)     # Genera errores estandar
library(lmtest)       # Sirve para tests estadísticos para modelos lineales (heter., autocorr., etc.).
library(modelsummary) # Formatos más visibles de regresiones
library(AER)
library(lmtest)
library(eph)

rm(list = ls())


eph <- get_microdata(
  year = 2025,
  period = 4,
  type = "individual")

eph_hogar <- get_microdata(
  year = 2025,
  period = 4,
  type = "hogar")

names(eph) <- tolower(names(eph))

#Etiquetas de las variables de interes
eph <- eph %>% 
  mutate(ingreso = p21,
         parentesco = ch03,
         sexo = ch04,
         edad = ch06,
         estado_civil = ch07,
         horas_trabajadas = pp3e_tot)


eph$region <- factor(eph$region, levels = c(01, 40, 41, 42, 43, 44), 
                     labels = c("Gran Buenos Aires", "Noroeste", "Noreste", "Cuyo", "Pampeana", "Patagonia")) 

eph <- eph %>% 
  mutate(sexo = ifelse(sexo == 2, 0, 1))

eph$sexo <- factor(eph$sexo, levels = c(0, 1),
                   labels = c("Mujer", "Hombre"))

eph$estado_civil <- factor(eph$estado_civil, levels = c(1, 2, 3, 4, 5),
                   labels = c("unido", "casado", "separado", "viudo", "soltero"))

eph$cat_ocup <- factor(eph$cat_ocup, levels = c(1, 2, 3, 4, 9),
                      labels = c("patron", "cuenta propia", "obrero o empleado",
                                "trab_familiar", "ns_nr."))

#Brecha salarial
eph <- eph %>% 
  mutate(ingreso2 = p21) %>% 
  filter(ingreso2 > 0)

#Salario promedio mensual
tabla1 <- eph %>% 
  summarise(
  "Tabla 1: Salario Promedio Mensual" = weighted.mean(as.numeric(ingreso2), as.numeric(pondiio, na.rm = TRUE)))

print(tabla1)

#Separando por sexo
tabla_sexo <- eph %>%
  group_by(sexo) %>%
  summarise(
    "Conteo" = n(),
    "Ingreso Promedio" = weighted.mean(as.numeric(ingreso2), as.numeric(pondiio, na.rm = TRUE)))

print(tabla_sexo)

#Para obtener la brecha salarial es mas preciso utilzar una regresion multiple
#Incluyendo variables, como edad (18-65) o experiencia, categoria , estado civil, region, etc.

#Se utiliza una nueva base filtrada
eph_2 <- eph %>% 
  filter(edad >= 18 & edad <= 65,
         estado == 1,
         horas_trabajadas > 0 & horas_trabajadas <= 168)

#Para obtener la educacion como variable continua 

eph_2 <- eph_2 %>%
  mutate(aeduc = case_when(
    
    # Nunca asistió / menores de 2 / educación especial
    ch10 == 3                          ~ 0,
    edad < 2                           ~ 0,
    ch12 == 9                          ~ 0,
    
    # No completaron el nivel
    # Jardín/preescolar
    ch13 == 2 & ch12 == 1             ~ 0,
    
    # Primaria incompleta
    ch13 == 2 & ch12 == 2 & ch14 == 0 ~ 1,
    ch13 == 2 & ch12 == 2 & ch14 == 1 ~ 2,
    ch13 == 2 & ch12 == 2 & ch14 == 2 ~ 3,
    ch13 == 2 & ch12 == 2 & ch14 == 3 ~ 4,
    ch13 == 2 & ch12 == 2 & ch14 == 4 ~ 5,
    ch13 == 2 & ch12 == 2 & ch14 == 5 ~ 6,
    ch13 == 2 & ch12 == 2 & ch14 == 6 ~ 7,
    
    # EGB incompleto
    ch13 == 2 & ch12 == 3 & ch14 == 0 ~ 1,
    ch13 == 2 & ch12 == 3 & ch14 == 1 ~ 2,
    ch13 == 2 & ch12 == 3 & ch14 == 2 ~ 3,
    ch13 == 2 & ch12 == 3 & ch14 == 3 ~ 4,
    ch13 == 2 & ch12 == 3 & ch14 == 4 ~ 5,
    ch13 == 2 & ch12 == 3 & ch14 == 5 ~ 6,
    ch13 == 2 & ch12 == 3 & ch14 == 6 ~ 7,
    ch13 == 2 & ch12 == 3 & ch14 == 7 ~ 8,
    ch13 == 2 & ch12 == 3 & ch14 == 8 ~ 9,
    
    # Secundaria incompleta
    ch13 == 2 & ch12 == 4 & ch14 == 0 ~ 8,
    ch13 == 2 & ch12 == 4 & ch14 == 1 ~ 9,
    ch13 == 2 & ch12 == 4 & ch14 == 2 ~ 10,
    ch13 == 2 & ch12 == 4 & ch14 == 3 ~ 11,
    ch13 == 2 & ch12 == 4 & ch14 == 4 ~ 12,
    ch13 == 2 & ch12 == 4 & ch14 == 5 ~ 13,
    
    # Polimodal incompleto
    ch13 == 2 & ch12 == 5 & ch14 == 0 ~ 10,
    ch13 == 2 & ch12 == 5 & ch14 == 1 ~ 11,
    ch13 == 2 & ch12 == 5 & ch14 == 2 ~ 12,
    ch13 == 2 & ch12 == 5 & ch14 == 3 ~ 13,
    
    # Terciario incompleto
    ch13 == 2 & ch12 == 6 & ch14 == 0          ~ 13,
    ch13 == 2 & ch12 == 6 & ch14 == 1          ~ 14,
    ch13 == 2 & ch12 == 6 & ch14 >= 2 & ch14 < 98 ~ 15,
    
    # Universitario incompleto
    ch13 == 2 & ch12 == 7 & ch14 == 0          ~ 13,
    ch13 == 2 & ch12 == 7 & ch14 == 1          ~ 14,
    ch13 == 2 & ch12 == 7 & ch14 == 2          ~ 15,
    ch13 == 2 & ch12 == 7 & ch14 == 3          ~ 16,
    ch13 == 2 & ch12 == 7 & ch14 == 4          ~ 17,
    ch13 == 2 & ch12 == 7 & ch14 >= 5 & ch14 < 98 ~ 18,
    
    # Posgrado incompleto
    ch13 == 2 & ch12 == 8 & ch14 == 0          ~ 19,
    ch13 == 2 & ch12 == 8 & ch14 == 1          ~ 20,
    ch13 == 2 & ch12 == 8 & ch14 == 2          ~ 21,
    ch13 == 2 & ch12 == 8 & ch14 >= 3 & ch14 < 98 ~ 22,
    
    # Completaron el nivel
    ch13 == 1 & ch12 == 1                      ~ 1,
    ch13 == 1 & ch12 == 2                      ~ 8,
    ch13 == 1 & ch12 == 3                      ~ 11,
    ch13 == 1 & (ch12 == 4 | ch12 == 5)        ~ 13,
    ch13 == 1 & ch12 == 6                      ~ 16,
    ch13 == 1 & ch12 == 7                      ~ 19,
    ch13 == 1 & ch12 == 8                      ~ 23,
    
    TRUE ~ NA_real_))

#Para calcular la experiencia se utiliza la formula de: edad - aeduc - 6
eph_2 <- eph_2 %>% 
  mutate(experiencia = (edad-aeduc-6))

eph_2 <- eph_2 %>% 
  filter(experiencia >= 0)

eph_2 <- eph_2 %>% 
  mutate(log_ingreso = log(ingreso))

#Regresion para obtener la brecha salarial

brecha <- lm(log_ingreso ~ aeduc + sexo + experiencia + I(experiencia^2) + region +
             cat_ocup + as.factor(estado_civil) + horas_trabajadas, data = eph_2, weights = pondiio)

#El coeficiente sexo indica cuanto % mas (o menos) ganan los hombres en comparacion con las mujeres
#Para una persona con la misma cantidad de educacion, experiencia, region, categoria ocupacional y horas trabajadas


summary(brecha)

#utilizando salario por hora en vez de mensual
eph_2 <- eph_2 %>% 
  mutate(salario_hora = ingreso/(horas_trabajadas*4.3)) %>% 
  mutate(log_salario_hora = log(salario_hora))

#Si quisiera verlo en CABA
eph_caba <- eph_2 %>% 
  filter(aglomerado == 32)

#Ya no es necesario incluir la variable region
brecha.caba <- lm(log_ingreso ~ aeduc + sexo + experiencia + I(experiencia^2)  +
                    cat_ocup + as.factor(estado_civil) + horas_trabajadas, data = eph_caba, weights = pondiio)

summary(brecha_caba)


#Brecha utilizando el salario por hora
brecha_nac <- lm(log_salario_hora ~ aeduc + sexo + experiencia + I(experiencia^2) + region +
                   cat_ocup + as.factor(estado_civil), data = eph_2, weights = pondiio)
summary(brecha_nac)

brecha_caba <- lm(log_salario_hora ~ aeduc + sexo + experiencia + I(experiencia^2) +
                cat_ocup + as.factor(estado_civil), data = eph_caba, weights = pondiio)
summary(brecha_caba)

eph_gba <- eph_2 %>% 
  filter(region == "Gran Buenos Aires")

brecha_gba <- lm(log_salario_hora ~ aeduc + sexo + experiencia + I(experiencia^2) +
                   cat_ocup + as.factor(estado_civil), data = eph_gba, weights = pondiio)
summary(brecha_gba)


eph_no <- eph_2 %>% 
  filter(region == "Noroeste")

brecha_no <- lm(log_salario_hora ~ aeduc + sexo + experiencia + I(experiencia^2) +
                   cat_ocup + as.factor(estado_civil), data = eph_no, weights = pondiio)
summary(brecha_no)



eph_ne <- eph_2 %>% 
  filter(region == "Noreste")

brecha_ne <- lm(log_salario_hora ~ aeduc + sexo + experiencia + I(experiencia^2) +
                   cat_ocup + as.factor(estado_civil), data = eph_ne, weights = pondiio)
summary(brecha_ne)


eph_cuyo <- eph_2 %>% 
  filter(region == "Cuyo")

brecha_cuyo <- lm(log_salario_hora ~ aeduc + sexo + experiencia + I(experiencia^2) +
                   cat_ocup + as.factor(estado_civil), data = eph_cuyo, weights = pondiio)
summary(brecha_cuyo)


eph_pat <- eph_2 %>% 
  filter(region == "Patagonia")

brecha_pat <- lm(log_salario_hora ~ aeduc + sexo + experiencia + I(experiencia^2) +
                   cat_ocup + as.factor(estado_civil), data = eph_pat, weights = pondiio)
summary(brecha_pat)



#Se ve como la brecha a nivel nacional es muy alta alcanzando el 38% cuando se utiliza el salario mensual
#En cambio en CABA la diferencia es mucho menor y llega al 18%
#En cambio cuando se utiliza el salario por hora a nivel nacional la brecha cae al 20%
#Y en CABA directamente el coeficiente no es significtativo (95%)
#Por lo que no se puede asegurar estadisticamente que haya brecha salarial en CABA

#Esto sucede porque una de las principales razones que explican la diferencia de los salarios
#Es que el hombre trabaja mas horas en el mes que las mujeres
#Lo que hace que su salario/ingreso mensual sea mayor
#Al compararse dos personas midiendo el salario por horas la brecha disminuye mucho
#Ya que ahora se elimina el control de cuantas horas trabajaron
#Sin embargo para medir la brecha de genero, es decir 
#Cual es la diferencia entre el hombre y la mujer para la misma tarea, la misma cantidad de horas
#Se utiliza el segundo modelo y no el primero que compara con el ingreso mensual


coef_nac <- brecha_nac$coefficients["sexoHombre"]
coef_caba <- brecha_caba$coefficients["sexoHombre"]
coef_gba <- brecha_gba$coefficients["sexoHombre"]
coef_no <- brecha_no$coefficients["sexoHombre"]
coef_ne <- brecha_ne$coefficients["sexoHombre"]
coef_cuyo <- brecha_cuyo$coefficients["sexoHombre"]
coef_pat <- brecha_pat$coefficients["sexoHombre"]


tabla_brecha <- data.frame(
  Region = c("Nivel Nacional", "CABA", "Gran Buenos Aires", "Noroeste", "Noreste", "Cuyo", "Patagonia"),
  Brecha_Salarial = c(coef_nac, coef_caba, coef_gba, coef_no, coef_ne, coef_cuyo, coef_pat))

print(tabla_brecha)

brechas <- list("Nacional" = brecha_nac,
                "CABA" = brecha_caba,
                "Gran Buenos Aires" = brecha_gba,
                "Noroeste" = brecha_no,
                "Noreste" = brecha_ne,
                "Cuyo" = brecha_cuyo,
                "Patagonia" = brecha_pat)
modelsummary(brechas,
             title = "Brecha salarial en el pais",
             stars = TRUE,
             coef_map = c("sexoHombre" = "Brecha de genero"),
             gof_map = c("nobs", "adj.r.squared"))

