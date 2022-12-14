#
rm(list=ls())
#
library(openxlsx)
library(tidyverse)
#
base <- read.xlsx("insumos/2022-05-10 Identificación del estrés en Docentes (respuestas)_JN.xlsx",
                  sheet = 1,
                  colNames = T) %>% 
  select(71:98)

colnames(base) <- paste0("v", str_pad(1:28, 2, "left", "0"))
rownames(base) <- paste0("p", str_pad(1:40, 2, "left", "0"))

base1 <- base %>% 
  mutate(across(.cols = c(1:28), substr, start = 1, stop = 1)) %>% 
  t(.) %>% 
  data.frame() %>% 
  mutate(variable = rownames(.)) %>% 
  cbind(estrategia = paste0("e", c(1, 2, 1, 1, 1, 2, 3, 1, 1, 2, 3, 3, 1, 2, 3, 1, 1, 2, 3, 1, 1, 1, 3, 3, 3, 2, 1, 1))) %>% 
  pivot_longer(cols = starts_with("p"),
               values_to = "respuesta",
               names_to = "persona")

aggr1 <- base1 %>% 
  mutate(respuesta = as.numeric(respuesta)) %>% 
  group_by(estrategia) %>% 
  summarise(media = mean(respuesta),
            DT = sd(respuesta),
            min = min(respuesta),
            max = max(respuesta))

aggr2 <- base1 %>% 
  mutate(respuesta = as.numeric(respuesta)) %>% 
  group_by(estrategia, persona) %>% 
  summarise(respuesta = sum(respuesta)) %>% 
  group_by(estrategia) %>% 
  summarise(media = mean(respuesta),
            sd = sd(respuesta),
            min = min(respuesta),
            max = max(respuesta))
