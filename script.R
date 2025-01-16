# Limpiar el entorno
rm(list=ls(all=TRUE))

# Ver directorio de trabajo actual y cambiar
getwd()
setwd("/.../...") 


############################# Cargar y explorar datos ############################
datos = read.table("datos_grupo_1.txt", sep="\t", header=TRUE)
datos$Tipo.de.proyecto = as.factor(datos$Tipo.de.proyecto)

# Exploración de datos
dim(datos)
str(datos)
summary(datos)
names(datos)


################ Calcular e interpretar intervalos de confianza ##################
# Intervalo de confianza del 96% para la media teórica de Tiempo
tiempo = datos$Tiempo
mean_tiempo = mean(tiempo)
sd_tiempo = sd(tiempo)
n_tiempo = length(tiempo)
error = qnorm(0.98) * sd_tiempo / sqrt(n_tiempo)
intervalo_96 = c(mean_tiempo - error, mean_tiempo + error)
intervalo_96

# Intervalo de confianza del 95% para la varianza teórica de Ingresos
ingresos = datos$Ingresos
varianza_ingresos = var(ingresos)
n_ingresos = length(ingresos)
chisq_lower = qchisq(0.025, df = n_ingresos - 1)
chisq_upper = qchisq(0.975, df = n_ingresos - 1)
intervalo_95_varianza = c((n_ingresos - 1) * varianza_ingresos / chisq_upper, (n_ingresos - 1) * varianza_ingresos / chisq_lower)
intervalo_95_varianza

# Intervalo de confianza del 98% para el porcentaje teórico del Tipo Comercio Eléctrónico
tipo = datos$Tipo.de.proyecto
n_comercio = sum(tipo == "Comercio Eléctrónico")
n_total = length(tipo)
p_comercio = n_comercio / n_total
error = qnorm(0.99) * sqrt((p_comercio * (1 - p_comercio)) / n_total)
intervalo_98 = c(p_comercio - error, p_comercio + error)
intervalo_98


################### Contrastes de hipótesis #####################################
# Porcentaje teórico del Tipo Comercio Eléctrónico inferior al 71%
prop.test(x = n_comercio, n = n_total, p = 0.71, alternative = "less") 
t.test(ingresos, mu = 1283, alternative = "less")

# Media de Tiempo en Tipo Página web superior a 11.7
tiempo_pagina_web = tiempo[tipo == "Página web"]
t.test(tiempo_pagina_web, mu = 11.7, alternative = "greater")


########## Diferencias entre Tipo Comercio Eléctrónico y Página web ##############
# Media de Tiempo superior en Tipo Comercio Eléctrónico
tiempo_comercio = tiempo[tipo == "Comercio Eléctrónico"]
t.test(tiempo_comercio, tiempo_pagina_web, alternative = "greater")

# Diferencias significativas en Ingresos entre Tipo Página web y Tipo Comercio Eléctrónico
ingresos_comercio = ingresos[tipo == "Comercio Eléctrónico"]
ingresos_pagina_web = ingresos[tipo == "Página web"]
t.test(ingresos_comercio, ingresos_pagina_web)
