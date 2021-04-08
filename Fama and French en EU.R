
# descarga de paquetes --------------

library(tidyquant)
library(tidyverse)
library(timetk)
library(broom)
library(glue)
library(readxl)
library(BatchGetSymbols)
library(apaTables)
library(MBESS)
library(openxlsx)

#importamos los factores Europeos--------------

temp <- tempfile()
base <-
  "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"


factor <- 
  "Europe_3_Factors"

format<-
  "_CSV.zip"

full_url <-
  glue(base,factor,format,sep ="")

download.file(full_url,temp,quiet = TRUE)

EUFactors<-read.csv(unz(temp,"Europe_3_Factors.csv",open = ""),skip = 6, nrows = 346)

#arreglamos las fechas
colnames(EUFactors)[1] <- "date"
EUFactors$date <- as.character(EUFactors$date)
EUFactors <- EUFactors %>%
  mutate_at(vars(-date),as.numeric) %>% 
  mutate(date = ymd(parse_date_time(date, "Y%m%"))) 

EUFactors%>% 
  select(date) %>%
  mutate(date = lubridate::rollback(date, roll_to_first = TRUE)) %>%
  head(1)

#importamos precios---------------


tickers_ibex <- read_excel("C:/Users/lenovo/Downloads/tickers ibex.xlsx")

#tickers_ibex <- read_excel("tickers ibex.xlsx") # esta es mi ruta, usando getwd() del archivo, tu usa la tuya, la de arriba.


precios <- BatchGetSymbols(tickers_ibex$ticker, first.date = '2000-01-01',
                           thresh.bad.data = 0.10)

df.precios <- precios$df.tickers 


returns_longs <- df.precios %>% group_by(ticker) %>% 
  tq_transmute(select = price.adjusted, mutate_fun = periodReturn, 
               period = "monthly", type = "log") %>% 
  na.omit()

#arreglamos las fechas
colnames(returns_longs)[2] <- "date"

returns_longs <-returns_longs %>% 
  mutate(date = lubridate::rollback(date, roll_to_first = TRUE))%>% 
  filter(date <= last(EUFactors$date))


#Ajustamos las fechas de los factores y los precios
EUFactors <- EUFactors %>%
  filter(date >= first(returns_longs$date))


#juntamos los factores con los precios-----------

relacion_de_tickers <- tickers_ibex$ticker #relacion_de_tickers vector con los tickers
df.vacio <- data.frame()
df.model <- data.frame()




# comienzo del bucle ------------------------------------------------------



for (i in relacion_de_tickers) {  
  #filtro por ticker y junto a factores
  returns <- returns_longs %>% 
    filter(ticker == i) 
  
  
  # Si un ticker no esta en returns_longs tiene que pasar al siguiente
  
  if(dim(returns) == 0) {next} 
  
  
  datos <- returns %>% 
    left_join(EUFactors, by = "date") 
  
  datos <- datos %>% 
    mutate( Mkt.RF= Mkt.RF/100,
            SMB = SMB/100,
            HML = HML/100,
            RF = RF/100,
            R_excess = monthly.returns - RF) 
  
  # modelo para arreglar coeficientes y pegarlo en data.frame de acumulacion de todos los tickers
  FF_model <- datos %>%  
    do(model = lm(R_excess ~ Mkt.RF + SMB + HML, 
                  data = .)) %>% 
    tidy(model, conf.int = T, conf.level = .95) 
  
  
  #mismo modelo con un formato adecuado para la presentacion de las tablas
  FF_model_para_apa <- lm(data = datos, R_excess ~ Mkt.RF + SMB + HML)
  
  
  # generacion de tabla apa para cada regresion en formato doc
  apa.reg.table(FF_model_para_apa, filename = paste0(i,'.doc')) 
  
  
  
  # figura valor coeficientes
  
  figure <- FF_model %>% 
    mutate_if(is.numeric, funs(round(., 3))) %>% 
    filter(term != "(Intercept)") %>% 
    ggplot(aes(x = term, y = estimate, shape = term, color = term)) + 
    geom_point() +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
    labs(title = paste("Coeficientes Fama y French:" ,i),
         x = "",
         y = "Coeficiente",
         caption = "Fuente de datos: Fama French website y yahoo! Finance") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption  = element_text(hjust = 0),
          legend.title = element_blank())
  
  # exportacion figura 
  ggsave(filename = paste0(i,'.png'),
         plot = figure,
         device = 'png',
         units = 'cm',
         width = 25, # quizas 25 cm sea demasiado grande
         height = 25/1.41,
         dpi = 300)
  
  #incorporo al data frame vacio
  df.vacio <- rbind.data.frame(df.vacio, datos)
  df.model <- rbind.data.frame(df.model, FF_model)
  
} # final de bucle principal



# exportar tabla coeficientes regresiones FF ------------------------------

# creo  una hoja de calculo con dos objetos (dos hojas)

hoja_de_calculo <- createWorkbook()
addWorksheet(wb = hoja_de_calculo, sheetName = "modelos")
addWorksheet(wb = hoja_de_calculo, sheetName = 'datos')


writeData(wb = hoja_de_calculo, sheet = 'modelos', x = df.model)
writeData(wb = hoja_de_calculo, sheet = 'datos', x = df.vacio)


saveWorkbook(wb = hoja_de_calculo, file = 'resultados_FF3_UE.xlsx', overwrite = TRUE)






view(EUFactors)