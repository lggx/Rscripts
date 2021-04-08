library(readxl)
library(tidyr)
library(ggplot2)
library(glue)
library(knitr)
library(openxlsx)

#importamos datos ----


resultados_FF3_UE <- read_excel("resultados_FF3_UE.xlsx")


datos_FF3_UE <- read_excel("resultados_FF3_UE.xlsx", sheet = "datos")



#table & pic ----------

tabla_resultados <- resultados_FF3_UE [, 1:3]
head(tabla_resultados)

tabla_resultados <- pivot_wider(tabla_resultados, names_from = term, values_from = estimate)



ultimo_año <- tail(EUFactors, 1)

ultimo_año

prima <- tabla_resultados$`(Intercept)`+tabla_resultados$Mkt.RF*ultimo_año$Mkt.RF + tabla_resultados$SMB*ultimo_año$SMB+ tabla_resultados$HML*ultimo_año$HML
head(prima)

coste_rp <- prima+ultimo_año$RF

tabla_resultados <- cbind(tabla_resultados, prima) %>% 
  cbind(coste_rp)

#tabla_resultados <- cbind (tabla_resultados, coste_rp)

head(tabla_resultados)


hoja_de_calculo <- createWorkbook()

addWorksheet(wb = hoja_de_calculo, sheetName = "tabla de resultados")

writeData(wb = hoja_de_calculo, sheet = 'tabla de resultados', x = tabla_resultados)

saveWorkbook(wb = hoja_de_calculo, file = 'resultados_prima_&_coste.xlsx', overwrite = TRUE)

#descripción de la muestra _----  

figura_muestra <- ggplot(datos_FF3_UE,aes(x = ticker, y = monthly.returns))+
  facet_wrap(~ticker, scale = "free", ncol = 5 ) + 
  geom_violin(adjust = 1.0, scale ='width', color = "light grey",fill = 'light grey') +
  geom_boxplot(color = "black", fill = "dark grey", width = 0.2) +
  labs(title = "Descripción de la muestra de empresas", 
       subtitle = "", 
       x = " Tickers de empresas del IBEX-35", 
       y = "Rendimiento (%)")


 # stat_boxplot(geom = 'errorbar', width = 0.2)
  
figura_muestra

ggsave(filename = paste0("descripcion muestra1",'.png'),
       plot = figura_muestra,
       device = 'png',
       units = 'cm',
       width = 25, 
       height = 35,
       dpi = 300)
