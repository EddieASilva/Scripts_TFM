pkgs <- c("data.table", "dplyr", "stringr", "tidyr", "readxl", "openxlsx", "lubridate", "ggplot2", "RODBC",
          "tictoc", "excel.link", "plotly", "Hmisc")
lapply(pkgs, library, character.only = TRUE)

options(scipen = 999)

setwd('C:/Users/eddysimo/OneDrive - Caja Colombiana de Subsidio Familiar - Colsubsidio/Documents/Personal')

#base general----
data <- read_excel('TFM/2020-09-26.xlsx') %>% 
  mutate(
    Estado_2=case_when(
      !is.na(Fecha_muerte) ~ 'Fallecido',
      !is.na(Fecha_recuperado) ~ 'Recuperado',
      is.na(Fecha_muerte) ~ 'Activo',
      is.na(Fecha_recuperado) ~ 'Activo'
      ),
    Fallecido=ifelse(
      Estado_2=='Fallecido', 1, 0
    ),
    Recuperado=ifelse(
      Estado_2=='Recuperado', 1, 0
    ),
    Activo=ifelse(
      Estado_2=='Activo', 1,0
    ),
    Casos_totales=1,
    Sexo=toupper(Sexo),
    Fuente_tipo_contagio=toupper(Fuente_tipo_contagio),
    Ubicacion...10=toupper(Ubicacion...10),
    Estado=toupper(Estado),
    nombre_depa=toupper(nombre_depa),
    nombre_mun=toupper(nombre_mun),
    nombre_depa=ifelse(
      nombre_depa=='BARRANQUILLA D.E.', 'ATL?NTICO', nombre_depa),
    nombre_depa=ifelse(
      nombre_depa=='CARTAGENA D.T. Y C.', 'BOL?VAR', nombre_depa),
    nombre_depa=ifelse(
      nombre_depa=='BUENAVENTURA D.E.', 'VALLE DEL CAUCA', nombre_depa),
    nombre_depa=ifelse(
      nombre_depa=='SANTA MARTA D.T. Y C.', 'MAGDALENA', nombre_depa
    )
    ) %>% 
  select(-c(
    `Fecha Not`,
    Pais_viajo_1_cod,
    Pais_viajo_1_nom,
    per_etn_2,
    nom_grupo_,
    Ubicacion...19
  )
  ) 


base_edad <- data %>% 
  mutate(grupo_etario=cut2(data$Edad, cuts = c(0, 20, 40, 60, 80,100))) %>%
  group_by(Fecha_diagnostico, grupo_etario) %>% 
  summarise(Infectados=n()) %>% 
  spread(grupo_etario, Infectados) %>% 
  ungroup()

base_genero <- data %>% 
  group_by(Fecha_diagnostico, Sexo) %>% 
  summarise(Infectados=n()) %>% 
  spread(Sexo, Infectados) %>% 
  ungroup()


base_muertos <- data %>% 
  # mutate(grupo_etario=cut2(data$Edad, cuts = c(20, 40, 60, 80,100))) %>% 
  group_by(Fecha_muerte) %>% 
  summarise(Muertos=n())


base_recuperados <- data %>% 
  filter(!is.na(Fecha_recuperado)) %>% 
  group_by(Fecha_recuperado) %>% 
  summarise(Recuperados=n())

base_muertos <- data %>% 
  # mutate(grupo_etario=cut2(data$Edad, cuts = c(20, 40, 60, 80,100))) %>% 
  group_by(Fecha_muerte) %>% 
  summarise(Muertos=n())

mobilidad <- read.csv('TFM/2020_CO_Region_Mobility_Report.csv', sep = ';')[,8:14] %>% 
  mutate(date=as.Date(date, '%d/%m/%Y'))

base_final <- read.csv("TFM/owid-covid-data.csv", sep = ';') %>% 
  mutate(Fecha=as.Date(date, '%d/%m/%Y')) %>% 
  filter(Fecha>='2020-03-07' & Fecha<='2020-09-25') %>% 
  select(Fecha, 
         population,
         stringency_index,
         total_tests,
         new_tests,
         new_deaths,
         total_deaths,
         new_cases, 
         total_cases) %>% 
  left_join(base_recuperados, by=c('Fecha'= 'Fecha_recuperado')) %>% 
  left_join(base_muertos, by=c('Fecha'='Fecha_muerte')) %>% 
  left_join(base_genero, by=c('Fecha'= 'Fecha_diagnostico')) %>% 
  left_join(base_edad, by=c('Fecha'= 'Fecha_diagnostico')) %>% 
  mutate(Susceptibles=ifelse(
    is.na(population-total_cases),
    population,
    population-total_cases)
    ) %>% 
  select(-c(population))

base_final[is.na(base_final)] <- 0

base_final %>% 
  plot_ly() %>% 
  add_lines(
    x=~Fecha,
    y=~stringency_index)


base_final <- base_final %>% 
  left_join(mobilidad, by=c('Fecha'='date'))

fwrite(base_final, "TFM/base_final.csv")

#mapa----
mapa <- data %>% 
  group_by(nombre_depa, nombre_mun, Estado_2) %>%
  summarise(Activo=sum(Activo, na.rm = T),
            Recuperado=sum(Recuperado, na.rm = T),
            Fallecido=sum(Fallecido, na.rm = T),
            Casos=n())

fwrite(mapa, "TFM/mapa.txt")


#bases individuales----
 #casos acontagiados
 plot_casos_diarios <- data %>%
   arrange(Fecha_diagnostico) %>% 
   group_by(Fecha_diagnostico) %>% 
   summarise(Casos_acum=sum(Casos_totales, na.rm = T)) %>% 
   filter(!is.na(Fecha_diagnostico) & Fecha_diagnostico<='2020-09-25')
 
 fechas <- data.frame(plot_casos_diarios$Fecha_diagnostico)
 colnames(fechas) <- 'Fecha'
 
 plot_casos_acum <- plot_casos_diarios %>% 
   group_by(Anio=year(Fecha_diagnostico)) %>% 
   summarise( Casos_acum=cumsum(Casos_acum)) %>% 
   bind_cols(fechas) %>% 
   ungroup() 
 
 #casos recuperados
 
 plot_recuperados_diarios <- data %>%
   arrange(Fecha_recuperado) %>% 
   group_by(Fecha_recuperado) %>% 
   summarise(Recuperados_acum=sum(Recuperado, na.rm = T))  %>% 
   filter(!is.na(Fecha_recuperado) & Fecha_recuperado<='2020-09-25')
 
 fechas <- data.frame(plot_recuperados_diarios$Fecha_recuperado)
 colnames(fechas) <- 'Fecha'
 
 plot_recuperados_acum <- plot_recuperados_diarios %>%
   arrange(Fecha_recuperado) %>% 
   group_by(Anio=year(Fecha_recuperado)) %>% 
   summarise(Recuperados_acum=cumsum(Recuperados_acum)) %>% 
   bind_cols(fechas) %>% 
   ungroup() 
 
 #casos fallecidos
 
 plot_fallecidos_diarios <- data %>%
   arrange(Fecha_muerte) %>% 
   group_by(Fecha_muerte) %>% 
   summarise(Fallecidos_acum=sum(Fallecido, na.rm = T)) %>% 
   filter(!is.na(Fecha_muerte) & Fecha_muerte<='2020-09-25')
 
 fechas <- data.frame(plot_fallecidos_diarios$Fecha_muerte)
 colnames(fechas) <- 'Fecha'
 
 plot_fallecidos_acum <- plot_fallecidos_diarios %>%
   group_by(Anio=year(Fecha_muerte)) %>% 
   summarise(Fallecidos_acum=cumsum(Fallecidos_acum)) %>% 
   bind_cols(fechas) %>% 
   ungroup() 
 
##activos 
plot_activos_diarios_acum <- plot_casos_diarios %>% 
  left_join(plot_recuperados_diarios, by=c('Fecha_diagnostico'='Fecha_recuperado')) %>% 
  left_join(plot_fallecidos_diarios, by=c('Fecha_diagnostico'='Fecha_muerte')) %>% 
  mutate(Casos_acum=ifelse(
    is.na(Casos_acum), 0, Casos_acum),
    Recuperados_acum=ifelse(
      is.na(Recuperados_acum), 0, Recuperados_acum),
    Fallecidos_acum=ifelse(
      is.na(Fallecidos_acum), 0, Fallecidos_acum),
    Activos_acum=cumsum(Casos_acum-Recuperados_acum-Fallecidos_acum)) %>% 
  select(Fecha_diagnostico, Activos_acum)



#plot compilado  
plot_compilado_acum <- plot_casos_acum %>% 
  left_join(plot_activos_diarios_acum, by=c('Fecha'='Fecha_diagnostico')) %>% 
  left_join(plot_recuperados_acum) %>% 
  left_join(plot_fallecidos_acum)


plot_compilado_diario <- plot_casos_diarios %>% 
  left_join(plot_activos_diarios_acum, by='Fecha_diagnostico') %>% 
  left_join(plot_recuperados_diarios, by=c('Fecha_diagnostico'='Fecha_recuperado')) %>% 
  left_join(plot_fallecidos_diarios, by=c('Fecha_diagnostico'='Fecha_muerte'))

          


#bases individuales por departamento----
#casos acontagiados
plot_casos_diarios_dep <- data %>%
  arrange(Fecha_diagnostico) %>% 
  group_by(Fecha_diagnostico, nombre_depa) %>% 
  summarise(Casos_acum=sum(Casos_totales, na.rm = T)) %>% 
  filter(!is.na(Fecha_diagnostico) & Fecha_diagnostico<='2020-09-25')


#casos recuperados

plot_recuperados_diarios_dep <- data %>%
  arrange(Fecha_recuperado) %>% 
  group_by(Fecha_recuperado, nombre_depa) %>% 
  summarise(Recuperados_acum=sum(Recuperado, na.rm = T))  %>% 
  filter(!is.na(Fecha_recuperado) & Fecha_recuperado<='2020-09-25')


#casos fallecidos

plot_fallecidos_diarios_dep <- data %>%
  arrange(Fecha_muerte) %>% 
  group_by(Fecha_muerte, nombre_depa) %>% 
  summarise(Fallecidos_acum=sum(Fallecido, na.rm = T)) %>% 
  filter(!is.na(Fecha_muerte) & Fecha_muerte<='2020-09-25')




#plot compilado  

plot_compilado_diario_dep <- plot_casos_diarios_dep %>% 
  left_join(plot_recuperados_diarios_dep, by=c('Fecha_diagnostico'='Fecha_recuperado', 
                                               'nombre_depa')) %>% 
  left_join(plot_fallecidos_diarios_dep, by=c('Fecha_diagnostico'='Fecha_muerte',
                                              'nombre_depa'))

#graficas----

plot_compilado_diario %>% 
  plot_ly() %>% 
  add_lines(
    x=~Fecha_diagnostico,
    y=~Casos_acum,
    name=list(tile='Casos'),
    line=list(
      color='blue'
    )
  ) %>% 
  add_lines(
    x=~Fecha_diagnostico,
    y=~Recuperados_acum,
    name=list(tile='Recuperados'),
    line=list(
      color='green'
    )
  ) %>% 
  add_lines(
    x=~Fecha_diagnostico,
    y=~Fallecidos_acum,
    name=list(tile='Fallecidos'),
    line=list(
      color='red'
    )
  ) %>% 

  layout(
    title='Comportamiento diario',
    xaxis=list(
      title='Fecha'),
    yaxis=list(
      title='Casos diarios'
    ),
    legend = list(x = 0.1, y = 0.9)
  )





plot_acum %>%
  plot_ly() %>% 
   add_lines(
     x=~Fecha,
     y=~Casos_acum,
     name=list(tile='Casos acumulados'),
     line=list(
       color='blue'
     )
   ) %>% 
   add_lines(
     x=~Fecha,
     y=~Recuperados_acum,
     name=list(tile='Recuperados acumulados'),
     line=list(
       color='green'
     )
   ) %>% 
   add_lines(
     x=~Fecha,
     y=~Fallecidos_acum,
     name=list(tile='Fallecidos acumulados'),
     line=list(
       color='red'
     )
   ) %>% 
   add_lines(
     x=~Fecha,
     y=~Activos_acum,
     name=list(tile='Activos acumulados'),
     line=list(
       color='orange'
     )
   ) %>% 
  layout(
    title='Comportamiento acumulado',
    xaxis=list(
      title='Fecha'),
    yaxis=list(
      title='Casos acumulados'
    ),
    legend = list(x = 0.1, y = 0.9)
  )

#piramide----

piramide <- data %>% 
  mutate(Sexo=as.factor(Sexo)) %>% 
  group_by(Sexo, Edad) %>% 
  summarise(Pacientes=n()) %>% 
  mutate(Pacientes=ifelse(Sexo=="M", Pacientes*-1, Pacientes),
         abs_pop=format(abs(Pacientes), big.mark = ","))

piramide %>% 
  plot_ly(x=~Pacientes,
          y = ~Edad, 
          color = ~Sexo, 
          colors = c("pink", "blue"), 
          type = 'bar', 
          orientation = 'h', 
          hoverinfo = 'y+text+name',
          text=~abs_pop) %>% 
  layout(title="Piramide poblacional pacientes con COVID-19 Colombia",
         barmode='overlay',
         xaxis=list(title="Cantidad de pacientes"),
         yaxis=list(title="Edad"))


data %>% group_by(Sexo) %>% summarise(pob=sum(Casos_totales))
396484/(396484+409554)*100
data %>% group_by(Sexo) %>% summarise(eadd=`mode<-`(Edad))


piramide <- data %>% 
  filter(!is.na(Fecha_muerte)) %>% 
  mutate(Sexo=as.factor(Sexo)) %>% 
  group_by(Sexo, Edad) %>% 
  summarise(Pacientes=n()) %>% 
  mutate(Pacientes=ifelse(Sexo=="M", Pacientes*-1, Pacientes),
         abs_pop=format(abs(Pacientes), big.mark = ","))

piramide %>% 
  plot_ly(x=~Pacientes,
          y = ~Edad, 
          color = ~Sexo, 
          colors = c("pink", "blue"), 
          type = 'bar', 
          orientation = 'h', 
          hoverinfo = 'y+text+name',
          text=~abs_pop) %>% 
  layout(title="Piramide poblacional pacientes fallecidos con COVID-19 Colombia",
         barmode='overlay',
         xaxis=list(title="Cantidad de pacientes"),
         yaxis=list(title="Edad"))


data %>% filter(!is.na(Fecha_muerte)) %>% group_by(Sexo) %>% summarise(pob=sum(Casos_totales))
9931/(9931+17283)*100
data %>% filter(!is.na(Fecha_muerte)) %>% group_by(Sexo) %>% summarise(eadd=mean(Edad))

piramide <- data %>% 
  filter(!is.na(Fecha_recuperado)) %>% 
  mutate(Sexo=as.factor(Sexo)) %>% 
  group_by(Sexo, Edad) %>% 
  summarise(Pacientes=n()) %>% 
  mutate(Pacientes=ifelse(Sexo=="M", Pacientes*-1, Pacientes),
         abs_pop=format(abs(Pacientes), big.mark = ","))

piramide %>% 
  plot_ly(x=~Pacientes,
          y = ~Edad, 
          color = ~Sexo, 
          colors = c("pink", "blue"), 
          type = 'bar', 
          orientation = 'h', 
          hoverinfo = 'y+text+name',
          text=~abs_pop) %>% 
  layout(title="Piramide poblacional pacientes recuperados de COVID-19 Colombia",
         barmode='overlay',
         xaxis=list(title="Cantidad de pacientes"),
         yaxis=list(title="Edad"))



data %>% filter(!is.na(Fecha_recuperado)) %>% group_by(Sexo) %>% summarise(pob=sum(Casos_totales))
347470/(347470+352642)*100
data %>% filter(!is.na(Fecha_recuperado)) %>% group_by(Sexo) %>% summarise(eadd=mean(Edad))

piramide <- data %>% 
  filter(is.na(Fecha_muerte) & is.na(Fecha_recuperado)) %>% 
  mutate(Sexo=as.factor(Sexo)) %>% 
  group_by(Sexo, Edad) %>% 
  summarise(Pacientes=n()) %>% 
  mutate(Pacientes=ifelse(Sexo=="M", Pacientes*-1, Pacientes),
         abs_pop=format(abs(Pacientes), big.mark = ","))

piramide %>% 
  plot_ly(x=~Pacientes,
          y = ~Edad, 
          color = ~Sexo, 
          colors = c("pink", "blue"), 
          type = 'bar', 
          orientation = 'h', 
          hoverinfo = 'y+text+name',
          text=~abs_pop) %>% 
  layout(title="Piramide poblacional pacientes activos con COVID-19 Colombia",
         barmode='overlay',
         xaxis=list(title="Cantidad de pacientes"),
         yaxis=list(title="Edad"))


#casos por departamento----
dep_grandes <- c('BOGOT? D.C.', 'ATL?NTICO', 'ANTIOQUIA','CUNDINAMARCA' )


plot_compilado_diario_dep %>% 
  filter(nombre_depa!= 'BOGOT? D.C.' &  
           nombre_depa!= 'ATL?NTICO' & 
           nombre_depa!= 'ANTIOQUIA' &
           nombre_depa!= 'VALLE DEL CAUCA' &
           nombre_depa!= 'C?RDOBA' & 
           nombre_depa!= 'SANTANDER' &
           nombre_depa!= 'CUNDINAMARCA' &
           nombre_depa!= 'META' &
           nombre_depa!= 'CESAR' &
           nombre_depa!= 'BOL?VAR' &
           nombre_depa!= 'MAGDALENA' &
           nombre_depa!= 'NARI?O' &
           nombre_depa!= 'NORTE DE SANTANDER' &
           nombre_depa!= 'SUCRE') %>% 
  ggplot() +
  geom_line(aes(x=Fecha_diagnostico, y=Casos_acum ,color='Casos nuevos')) +
  geom_line(aes(x=Fecha_diagnostico, y=Recuperados_acum, color='Casos recuperados')) +
  geom_line(aes(x=Fecha_diagnostico, y=Fallecidos_acum, color='Casos fallecidos')) +
  facet_wrap(~nombre_depa) +
  ggtitle("Casos por departamento") +
  xlab("Fecha") + 
  ylab("N?mero de casos") + 
  scale_color_manual(values = c(
    'Casos nuevos' = 'blue',
    'Casos recuperados' = 'green',
    'Casos fallecidos' = 'red')
    ) + theme_bw()



plot_compilado_diario_dep %>% 
  filter(nombre_depa== 'BOGOT? D.C.' |  
           nombre_depa== 'ATL?NTICO' | 
           nombre_depa== 'ANTIOQUIA' |
           nombre_depa== 'VALLE DEL CAUCA'
         ) %>% 
  ggplot() +
  geom_line(aes(x=Fecha_diagnostico, y=Casos_acum ,color='Casos nuevos')) +
  geom_line(aes(x=Fecha_diagnostico, y=Recuperados_acum, color='Casos recuperados')) +
  geom_line(aes(x=Fecha_diagnostico, y=Fallecidos_acum, color='Casos fallecidos')) +
  facet_wrap(~nombre_depa) +
  ggtitle("Casos por departamento") +
  xlab("Fecha") + 
  ylab("N?mero de casos") + 
  scale_color_manual(values = c(
    'Casos nuevos' = 'blue',
    'Casos recuperados' = 'green',
    'Casos fallecidos' = 'red')
  )+ theme_bw()


plot_compilado_diario_dep %>% 
  filter(
           nombre_depa== 'C?RDOBA' |
           nombre_depa== 'SANTANDER' |
           nombre_depa== 'CUNDINAMARCA' |
           nombre_depa== 'META' |
           nombre_depa== 'CESAR' |
           nombre_depa== 'BOL?VAR' |
           nombre_depa== 'MAGDALENA' |
           nombre_depa== 'NARI?O' |
           nombre_depa== 'NORTE DE SANTANDER' |
           nombre_depa== 'SUCRE') %>% 
  ggplot() +
  geom_line(aes(x=Fecha_diagnostico, y=Casos_acum ,color='Casos nuevos')) +
  geom_line(aes(x=Fecha_diagnostico, y=Recuperados_acum, color='Casos recuperados')) +
  geom_line(aes(x=Fecha_diagnostico, y=Fallecidos_acum, color='Casos fallecidos')) +
  facet_wrap(~nombre_depa) +
  ggtitle("Casos por departamento") +
  xlab("Fecha") + 
  ylab("N?mero de casos") + 
  scale_color_manual(values = c(
    'Casos nuevos' = 'blue',
    'Casos recuperados' = 'green',
    'Casos fallecidos' = 'red')
  )+ theme_bw()

#tasas de crecimiento----

plot_compilado_diario %>% 
  mutate(Mes=month(Fecha_diagnostico)) %>% 
  group_by(Mes) %>% 
  summarise(Casos_acum=sum(Casos_acum, na.rm = T),
            Recuperados_acum=sum(Recuperados_acum, nna.rm = T),
            Fallecidos_acum=sum(Fallecidos_acum, na.rm = T),
            Tasa_mortalidad=Fallecidos_acum/Casos_acum,
            Tasa_recuperados=Recuperados_acum/Casos_acum) %>% 
  filter(Mes!=9) %>% 
  plot_ly() %>% 
  add_lines(
    x=~Mes,
    y=~Tasa_mortalidad,
    line=list(
      color='red'
    ), 
    name='Tasa de mortalidad',
    text =~round(Tasa_mortalidad*100, digits = 1),
    textfont = t, 
    textposition= "top"
  ) %>% 
  add_lines(
    x=~Mes,
    y=~Tasa_recuperados,
    line=list(
      color='green'
    ),
    name='Tasa de recuperados',
    text =~round(Tasa_recuperados*100, digits = 1),
    textfont = t, 
    textposition= "top"
  ) %>% 
  layout(
    title='Tasa de recuperados y mortalidad mensual',
    xaxis=list(
      title='Fecha'),
    yaxis=list(
      title='Tasa',
      tickformat='%'
    ),
    legend = list(x = 0.1, y = 0.9)
  )


#datos clinicos

data %>%
  filter(Ubicacion...10=='CASA' | Ubicacion...10=='HOSPITAL' | Ubicacion...10=='HOSPITAL UCI') %>% 
  group_by(Ubicacion...10) %>%
  summarise(Valor=round(
    sum(
      Casos_totales, na.rm = T), digits = 0), .groups='drop'
  ) %>% 
  ungroup() %>% 
  plot_ly(
    labels =~Ubicacion...10,
    values =~Valor,
    type = 'pie') %>% 
  layout(
    title='Ubicaci?n de pacientes activos'
    )

data %>%
  filter(Tipo_recuperacion!='null') %>% 
  group_by(Tipo_recuperacion) %>%
  summarise(Valor=round(
    sum(
      Casos_totales, na.rm = T), digits = 0), .groups='drop'
  ) %>% 
  ungroup() %>% 
  plot_ly(
    labels =~Tipo_recuperacion,
    values =~Valor,
    type = 'pie') %>% 
  layout(
    title='Pruebas de recuperaci?n'
  )