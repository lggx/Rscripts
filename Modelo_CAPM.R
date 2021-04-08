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
  
  m_CAPM <- datos %>%  
    do(model = lm(R_excess ~ Mkt.RF, 
                  data = .))%>% 
    tidy(model, conf.int = T, conf.level = .95)
  
  df.vacio <- rbind.data.frame(df.vacio, datos)
  df.model <- rbind.data.frame(df.model, m_CAPM)
}



hoja_de_calculo <- createWorkbook()
addWorksheet(wb = hoja_de_calculo, sheetName = "modelo")
writeData(wb = hoja_de_calculo, sheet = 'modelo', x = df.model)
saveWorkbook(wb = hoja_de_calculo, file = 'resultados_CAPM.xlsx', overwrite = TRUE)

coef_capm <- df.model [,1:3]

coef_capm <- coef_capm %>% 
  pivot_wider(names_from = term, values_from = estimate)
head(coef_capm)

ultimo_año_capm <- tail(EUFactors, 1)
ultimo_año_capm

prima2 <- coef_capm$`(Intercept)`+coef_capm$Mkt.RF*ultimo_año_capm$Mkt.RF

coste_CAPM <- prima2 + ultimo_año_capm$RF

modelo_CAPM <- cbind.data.frame(coef_capm, prima2) %>% 
  cbind.data.frame(coste_CAPM)
  

modelo_CAPM

hoja_de_calculo2 <- createWorkbook()
addWorksheet(wb = hoja_de_calculo2, sheetName = "prima y coste CAPM")
writeData(wb = hoja_de_calculo2, sheet = 'prima y coste CAPM', x = modelo_CAPM)
saveWorkbook(wb = hoja_de_calculo2, file = 'coste_CAPM.xlsx', overwrite = TRUE)
