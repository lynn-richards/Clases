# Tarea 2: Lab 02 Data Management en R

#instalar y cargar el paquete "car"
install.packages("car")
help(package="car")
library(car)
library(data.table)

#cargar los datos preinstalados "Chile"
?Chile
Chile
plebicito<-Chile

plot(Chile)

# 0. ¿De qué clase es el objeto Chile?


# 1. ¿De qué clase es cada variable?

# 2. Cree una nueva variable que se llame income1000, que sea igual al ingreso en miles de pesos

# 3. Cuente cuántas personas votaron por el si, y cuántas por el no

# 4. Cree un nuevo objeto tipo data.table en base al objeto Chile, que se llame Chile2

# 5. Borre la variable statusquo

# 7. Cree una nueva variable de ingreso per cápita
  # 7.1. Reemplace los NAs de income por ceros

# 8. Cree una variable que tenga un 1 si age>65 y 0 en el caso contrario

# 9. Cree un nuevo objeto que muestre los valores promedio de ingreso y edad por region y sexo
#     Nota: Si tiene observaciones con NA, por qué es esto? Cómo lo arreglamos?


#10. loops: Solo para propósitos demostrativos, no entra en la tarea!

for(i in 1:10){
  print(paste("Estoy contando de 1 en 1 al 10, y voy en el", i))
  Sys.sleep(3)
}


regiones<-names(table(Chile$region))
sexo<-names(table(Chile$sex))

Chile2<-data.table(Chile)
for(r in 1:length(regiones)){
  for(s in 1:length(sexo)){
    prom.inc<-Chile2[region==regiones[r] & sex==sexo[s],mean(income,na.rm = T)]
    prom.age<-Chile2[region==regiones[r] & sex==sexo[s],mean(age,na.rm = T)]
    plot(Chile2[region==regiones[r] & sex==sexo[s],.(age,income)])
    title(main = paste("Region", regiones[r], "y Sexo", sexo[s]),sub = paste("Ingreso Promedio:", prom.inc, "| Edad Promedio:", prom.age))
    abline(h = prom.inc, col='red')
    abline(v = prom.age, col='blue')
    Sys.sleep(2)
  }
}