# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# ANALISIS EXLORATORIO DE DATOS: GERMAN CREDIT DATA
# --------------------------------------------------------------------------------------- #

# --------------------------------------------------------------------------------------- #
# Importar librerias
# --------------------------------------------------------------------------------------- #
library(readr)  # funciones para leer archivos
library(dplyr)  # funciones para muestreo de datos
library(crayon) # funciones para texto en consola

# --------------------------------------------------------------------------------------- #
# Importar datos desde archivo CSV
# --------------------------------------------------------------------------------------- #

  # Cambiar el directorio de trabajo actual
  current_dir = getwd()
  setwd(current_dir)

  # Importar el contenido del archivo german_credit_data.csv
  data <- read_csv("german_credit_data.csv")


#########################################################################################
# ......................................................................................#
# Analisis univariante del 100% del dataset
# ......................................................................................#
#########################################################################################

  # --------------------------------------------------------------------------------------- #
  # Analisis preliminar de los datos, exploracion del dataset
  # --------------------------------------------------------------------------------------- #

    # Dimension del dataset
    dim_data <- dim(data)
    cat("El dataset tiene una dimension de", bold(dim_data[1]),"filas y",bold(dim_data[2]),"columnas.\n")
    
    # Chequeo de la estructura del archivo importado
    str(data)
    
    # Visualizacion del contenido del archivo 
    View (data)
    
    # Elimino la columna X1 que parece ser un ID autoincremental no necesario para este analisis
    data <- subset (data, select = -X1)
    
    # Analisis estadistico descriptivo de todo el dataset
    summary(data)
    
    # --------------------------------------------------------------------------------------- #
    # Definicion de columnas del dataset (fuente: https://www.kaggle.com/uciml/german-credit)
    # --------------------------------------------------------------------------------------- #
    # 1. Age (numeric)
    # 2. Sex (text: male, female)
    # 3. Job (numeric: 0 - unskilled and non-resident, 1 - unskilled and resident, 2 - skilled, 3 - highly skilled)
    # 4. Housing (text: own, rent, or free)
    # 5. Saving accounts (text - little, moderate, quite rich, rich)
    # 6. Checking account (numeric, in DM - Deutsch Mark)
    # 7. Credit amount (numeric, in DM)
    # 8. Duration (numeric, in month)
    # 9. Purpose (text: car, furniture/equipment, radio/TV, domestic appliances, repairs, education, business, vacation/others)
    
    # Para mejorar la legibilidad de las variables, renombro las columnas a minusculas y palabras separadas por _
    colnames(data) <- c("age", "gender", "job", "housing", "savings_account", "checking_account","credit_amount","duration", "purpose")
    
    # --------------------------------------------------------------------------------------- #
    # Columnas renombradas
    # --------------------------------------------------------------------------------------- #
    # 1. age
    # 2. gender
    # 3. job
    # 4. housing
    # 5. savings_account
    # 6. checking_account
    # 7. credit_amount
    # 8. duration
    # 9. purpose
    
    # --------------------------------------------------------------------------------------- #
    # --------------------------------------------------------------------------------------- #
    # NOTA IMPORTANTE
    # --------------------------------------------------------------------------------------- #
    # A pesar que la definicion dice que la columna 6 es "Checking account (numeric, in DM)", 
    # explorando el dataset se puede verificar que sus valores corresponden a una variable 
    # categorica similar a la columna 5 "Saving accounts"
    # --------------------------------------------------------------------------------------- #
    # --------------------------------------------------------------------------------------- #

    # --------------------------------------------------------------------------------------- #
    # Analisis de tipos de variable dentro del data set
    # --------------------------------------------------------------------------------------- #

    # Descripcion de los tipos de dato por variable, cantidad y porcentaje segun sean 
    # cualitativas o cuantitativas
    col_names <- names(data)  # Nombres de las variables del data set
    cont_var <- 0             # Contador de variables cuantitativas, inicializado en cero
    disc_var <- 0             # Contador de variables cualitativas, inicializado en cero
    
    for (i in col_names)
    {
      cat("La variable", bold(i), "es del tipo", bold(typeof(data[[i]])), "\n")
      if (is.character(data[[i]])){
        disc_var = disc_var + 1
      } 
      else {
        cont_var = cont_var + 1
      }
    }
    
    cat("El data set contiene", bold(cont_var), "variables cuantitativas y", bold(disc_var), "variables discretas")
    
    # --------------------------------------------------------------------------------------- #
    # Analisis de datos nulos
    # --------------------------------------------------------------------------------------- #
    
    # Total y porcentaje de valores nulos de cada variable del data set
    for (i in col_names)
    {
      na_sum = sum(is.na(data[i]))
      na = na_sum
      na_percent = na_sum
      if (na_sum > 0) {
        na = red(na_sum)
        na_percent = red(100 * na_sum / length(data[[i]]) )
      }
      cat("La variable",bold(i),"tiene",bold(na),"registros nulos, el",bold(na_percent),"% del total de sus registros.\n")
    }
    
    
    
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # --------------------------------------------------------------------------------------- #
  # Analisis de variables cualitativas
  # --------------------------------------------------------------------------------------- #
  # Utilizaremos herramientas visuales como tablas, diagrama de torta, diagrama de barras   
  # y tabla de contingencia para hacer comparaciones.                                       
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    
    # Analisis univariante
    
    # Chequeo de los distintos valores para las siguientes variables:
    # 2. gender
    # 3. job
    # 4. housing
    # 5. savings_account
    # 6. checking_account
    # 9. purpose

    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
    # Variable: gender
    # Type: caracter
    
      table(data$gender, useNA="always")  # usamos useNA="always" para visualizar si existen datos en NULL
      pie(table(data$gender, useNA="always"))
    
      # Transformacion: No aplica. Solo hay 2 valores ["female","male"] 
      # Valores NULL: No hay.
    
      
    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
    # Variable: job
    # Type: caracter
      
      # Visualizacion de datos  
      table(data$job, useNA="always")
      pie(table(data$job, useNA="always"))
      
      #.............................
      # Transformacion: 
      #     Relizamos una tranformacion de datos para mejor comprension de los valores de la variable. 
      #     Reemplazando los valores numericos a sus valores literales, siguiendo el siguiente criterio:
      #     [0 - unskilled and non-resident, 1 - unskilled and resident, 2 - skilled, 3 - highly skilled ]
      
      data$job[data$job == 0] <- "unskilled and non-resident"
      data$job[data$job == 1] <- "unskilled and resident"
      data$job[data$job == 2] <- "skilled"
      data$job[data$job == 3] <- "highly skilled"
      
      # Visualizacion nuevamente de los datos transformados
      table(data$job, useNA="always")
      pie(table(data$job, useNA="always"))
      
      #.............................
      # Valores NULL: No hay.
    
    
    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
    # Variable: housing
    # Type: caracter
      table(data$housing, useNA="always")
      pie(table(data$housing, useNA="always"))
      
      #.............................
      # Transformacion: No aplica. Solo hay 3 valores ["free","own","rent"]
      
      #.............................
      # Valores NULL: No hay.
      
    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
    # Variable: savings_account
    # Type: caracter
      table(data$savings_account, useNA="always")
      pie(table(data$savings_account, useNA="always"))
      
      #.............................
      # Transformacion: No aplica. Solo hay 4 valores ["little","moderate","quite rich","rich"]
      
      #.............................
      # Valores NULL: No hay.
      
    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
    # Variable: checking_account
    # Type: caracter
      table(data$checking_account, useNA="always")
      pie(table(data$checking_account, useNA="always"))
      
      #.............................
      # Transformacion: No aplica. Solo hay 3 valores ["little","moderate","rich"]
      
      #.............................
      # Valores NULL: Hay valores NULL.
      
    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
    # Variable: purpose
    # Type: caracter
      table(data$purpose, useNA="always")
      pie(table(data$purpose, useNA="always"))
      
      #.............................
      # Transformacion: No aplica. Solo hay 8 valores ["business","car","domestic appliances","education","furniture/equipement","radio/TV","repairs","vacation/others"]
      
      #.............................
      # Valores NULL: No hay.
      

  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # --------------------------------------------------------------------------------------- #
  # Analisis de variables cuantitativas
  # --------------------------------------------------------------------------------------- #
  # Utilizaremos herramientas visuales como histogramas, boxplots y diagramas de dispersion.                                       
  # --------------------------------------------------------------------------------------- #
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
      
      # Chequeo de los valores para las siguientes variables:
      # 1. age
      # 7. credit_amount
      # 8. duration
        
      # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
      # Variable: age
      # Type: numeric
      
        # ...........................
        # Resumen estadistico formal
        summary(data$age)
        # La mediana es distinta a 50, por lo tanto es una medicion sesgada.
        
        # Analisis de la distribucion
        hist(data$age, main = "Age distribution", xlab = "age [years]", ylab="frequency")
        
        # Agregamos la leyenda al histograma indicando que se identifica una distrubucion asimetrica a la derecha, 
        # la asimetria positiva implica que hay mas valores distintos a la derecha de la media.
        legend("topright", legend=c(""), title = "Distribucion con sesgo positivo o asimetrica a la derecha", text.font=2, bg='lightgreen',cex=0.85)
        
        #.............................
        # Analisis de datos outliers
        
        # Visualizacion de la variable mediante un boxplot, para estudiar la distribucion de la variable e identificar datos outliers
        boxplot(data$age, main="Age distrubution")
        
        # Valores outliers (lista)
        age_outliers <- sort(boxplot(data$age)$out)
        cat("La variable", bold("age"), "tiene", red(length(age_outliers)), "datos outliers y son los siguientes valores:", red(paste(age_outliers, collapse = ",")))
        
        #.............................
        # Transformacion: No aplica.
        
        #.............................
        sum(is.na(data$age))
        # Valores NULL: No hay.
        
      # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
      # Variable: credit_amount
      # Type: numeric
        
        # ...........................
        # Resumen estadistico formal
        summary(data$credit_amount)
        # La mediana es distinta a 50, por lo tanto es una medicion sesgada.
        
        # Analisis de la distribucion
        hist(data$credit_amount, main = "Credit amount distribution", xlab = "credit_amount [DM]", ylab="frequency")
        
        # Agregamos la leyenda al histograma indicando que se identifica una distrubucion asimetrica a la derecha, 
        # la asimetria positiva implica que hay mas valores distintos a la derecha de la media.
        legend("topright", legend=c(""), title = "Distribucion con sesgo positivo o asimetrica a la derecha", text.font=2, bg='lightgreen',cex=0.85)
        
        #.............................
        # Analisis de datos outliers
        
        # Visualizacion de la variable mediante un boxplot, para estudiar la distribucion de la variable e identificar datos outliers
        boxplot(data$credit_amount, main="Credit amount distrubution")
        
        # Valores outliers (lista)
        credit_amount_outliers <- sort(boxplot(data$credit_amount)$out)
        cat("La variable", bold("credit amount"), "tiene", red(length(credit_amount_outliers)), "datos outliers y son los siguientes valores:", red(paste(credit_amount_outliers, collapse = ",")))
        
        #.............................
        # Transformacion: No aplica.
        
        #.............................
        sum(is.na(data$credit_amount))
        # Valores NULL: No hay.
      
      # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
      # Variable: duration
      # Type: numeric
        
        # ...........................
        # Resumen estadistico formal
        summary(data$duration)
        # La mediana es distinta a 50, por lo tanto es una medicion sesgada.
        
        # Analisis de la distribucion
        hist(data$duration, main = "Credit amount distribution", xlab = "duration", ylab="frequency")
        
        # Agregamos la leyenda al histograma indicando que se identifica una distrubucion asimetrica a la derecha, 
        # la asimetria positiva implica que hay mas valores distintos a la derecha de la media.
        legend("topright", legend=c(""), title = "Distribucion con sesgo positivo o asimetrica a la derecha", text.font=2, bg='lightgreen',cex=0.85)
        
        #.............................
        # Analisis de datos outliers
        
        # Visualizacion de la variable mediante un boxplot, para estudiar la distribucion de la variable e identificar datos outliers
        boxplot(data$duration, main="Duration distrubution")
        
        # Valores outliers (lista)
        duration_outliers <- sort(boxplot(data$duration)$out)
        cat("La variable", bold("duration"), "tiene", red(length(duration_outliers)), "datos outliers y son los siguientes valores:", red(paste(duration_outliers, collapse = ",")))
        
        #.............................
        # Transformacion: No aplica.
        
        sum(is.na(data$duration))
        #.............................
        # Valores NULL: No hay.
  

#########################################################################################
# ......................................................................................#
# Analisis univariante del 50% del dataset
# ......................................................................................#
#########################################################################################
  
    # Obtener solo el 50% del set de datos
    data_sample <- sample_frac(data, 0.5)
    
    # --------------------------------------------------------------------------------------- #
    # Analisis preliminar de los datos, exploracion del dataset
    # --------------------------------------------------------------------------------------- #

    # Dimension del dataset de muestra
    dim_sample<-dim(data_sample)
    cat("La muestra tiene una dimension de", bold(dim_sample[1]),"filas y",bold(dim_sample[2]),"columnas")
    
    # Chequeo de la estructura del dataset de muestra
    str(data)
    
    # Visualizacion del contenido del dataset de muestra 
    View (data)
    
    # Analisis estadistico descriptivo del dataset de muestra
    summary(data)

    # --------------------------------------------------------------------------------------- #
    # Analisis de tipos de variable dentro del data set
    # --------------------------------------------------------------------------------------- #
    
    # Descripcion de los tipos de dato por variable, cantidad y porcentaje segun sean 
    # cualitativas o cuantitativas
    col_names <- names(data_sample)  # Nombres de las variables del data set
    cont_var <- 0             # Contador de variables cuantitativas, inicializado en cero
    disc_var <- 0             # Contador de variables cualitativas, inicializado en cero
    
    for (i in col_names)
    {
      cat("La variable", bold(i), "es del tipo", bold(typeof(data_sample[[i]])), "\n")
      if (is.character(data_sample[[i]])){
        disc_var = disc_var + 1
      } 
      else {
        cont_var = cont_var + 1
      }
    }
    
    cat("El data set contiene", bold(cont_var), "variables cuantitativas y", bold(disc_var), "variables discretas")
    
    # --------------------------------------------------------------------------------------- #
    # Analisis de datos nulos
    # --------------------------------------------------------------------------------------- #
    
    # Total y porcentaje de valores nulos de cada variable del data set
    for (i in col_names)
    {
      na_sum = sum(is.na(data_sample[i]))
      na = na_sum
      na_percent = na_sum
      if (na_sum > 0) {
        na = red(na_sum)
        na_percent = red(100 * na_sum / length(data_sample[[i]]) )
      }
      cat("La variable",bold(i),"tiene",bold(na),"registros nulos, el",bold(na_percent),"% del total de sus registros.\n")
    }
    
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    # --------------------------------------------------------------------------------------- #
    # Analisis de variables cualitativas
    # --------------------------------------------------------------------------------------- #
    # Utilizaremos herramientas visuales como tablas, diagrama de torta, diagrama de barras   
    # y tabla de contingencia para hacer comparaciones.                                       
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    
    # Analisis univariante
    
    # Chequeo de los distintos valores para las siguientes variables:
    # 2. gender
    # 3. job
    # 4. housing
    # 5. savings_account
    # 6. checking_account
    # 9. purpose

    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
    # Variable: gender
    # Type: caracter
    
      table(data_sample$gender, useNA="always")  # usamos useNA="always" para visualizar si existen datos en NULL
      pie(table(data_sample$gender, useNA="always"))
    
      # Transformacion: No aplica. Solo hay 2 valores ["female","male"] 
      # Valores NULL: No hay.
    
      
    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
    # Variable: job
    # Type: caracter
      
      # Visualizacion de datos  
      table(data_sample$job, useNA="always")
      pie(table(data_sample$job, useNA="always"))
      
      #.............................
      # Transformacion: No aplica, ya fue realizado con anterioridad.
      #.............................
      # Valores NULL: No hay.
    
    
    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
    # Variable: housing
    # Type: caracter
      table(data_sample$housing, useNA="always")
      pie(table(data_sample$housing, useNA="always"))
      
      #.............................
      # Transformacion: No aplica. Solo hay 3 valores ["free","own","rent"]
      
      #.............................
      # Valores NULL: No hay.
      
    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
    # Variable: savings_account
    # Type: caracter
      table(data_sample$savings_account, useNA="always")
      pie(table(data_sample$savings_account, useNA="always"))
      
      #.............................
      # Transformacion: No aplica. Solo hay 4 valores ["little","moderate","quite rich","rich"]
      
      #.............................
      # Valores NULL: No hay.
      
    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
    # Variable: checking_account
    # Type: caracter
      table(data_sample$checking_account, useNA="always")
      pie(table(data_sample$checking_account, useNA="always"))
      
      #.............................
      # Transformacion: No aplica. Solo hay 3 valores ["little","moderate","rich"]
      
      #.............................
      # Valores NULL: Hay valores NULL.
      
    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
    # Variable: purpose
    # Type: caracter
      table(data_sample$purpose, useNA="always")
      pie(table(data_sample$purpose, useNA="always"))
      
      #.............................
      # Transformacion: No aplica. Solo hay 8 valores ["business","car","domestic appliances","education","furniture/equipement","radio/TV","repairs","vacation/others"]
      
      #.............................
      # Valores NULL: No hay.
      

  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # --------------------------------------------------------------------------------------- #
  # Analisis de variables cuantitativas
  # --------------------------------------------------------------------------------------- #
  # Utilizaremos herramientas visuales como histogramas, boxplots y diagramas de dispersion.                                       
  # --------------------------------------------------------------------------------------- #
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
      
    # Chequeo de los valores para las siguientes variables:
    # 1. age
    # 7. credit_amount
    # 8. duration
    
    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
    # Variable: age
    # Type: numeric
    
    # ...........................
    # Resumen estadistico formal
    summary(data_sample$age)
    # La mediana es distinta a 50, por lo tanto es una medicion sesgada.
    
    # Analisis de la distribucion
    hist(data_sample$age, main = "Age distribution (50% sample data)", xlab = "age [years]", ylab="frequency")
    
    # Agregamos la leyenda al histograma indicando que se identifica una distrubucion asimetrica a la derecha, 
    # la asimetria positiva implica que hay mas valores distintos a la derecha de la media.
    legend("topright", legend=c(""), title = "Distribucion con sesgo positivo o asimetrica a la derecha", text.font=2, bg='lightgreen',cex=0.85)
    
    #.............................
    # Analisis de datos outliers
    
    # Visualizacion de la variable mediante un boxplot, para estudiar la distribucion de la variable e identificar datos outliers
    boxplot(data_sample$age, main="Age distrubution (50% sample data)")
    
    # Valores outliers (lista)
    age_outliers <- boxplot(data_sample$age)$out
    cat("La variable", bold("age"), "tiene", red(length(age_outliers)), "datos outliers y son los siguientes valores:", red(paste(age_outliers, collapse = ",")))
    
    #.............................
    # Transformacion: No aplica.
    
    #.............................
    sum(is.na(data_sample$age))
    # Valores NULL: No hay. Obviamente si en la muestra completa no habia NULLs, en la muestra tampoco los hay.
    
    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
    # Variable: credit_amount
    # Type: numeric
    
    # ...........................
    # Resumen estadistico formal
    summary(data_sample$credit_amount)
    # La mediana es distinta a 50, por lo tanto es una medicion sesgada.
    
    # Analisis de la distribucion
    hist(data_sample$credit_amount, main = "Credit amount distribution (50% sample data)", xlab = "credit_amount [DM]", ylab="frequency")
    
    # Agregamos la leyenda al histograma indicando que se identifica una distrubucion asimetrica a la derecha, 
    # la asimetria positiva implica que hay mas valores distintos a la derecha de la media.
    legend("topright", legend=c(""), title = "Distribucion con sesgo positivo o asimetrica a la derecha", text.font=2, bg='lightgreen',cex=0.85)
    
    #.............................
    # Analisis de datos outliers
    
    # Visualizacion de la variable mediante un boxplot, para estudiar la distribucion de la variable e identificar datos outliers
    boxplot(data_sample$credit_amount, main="Credit amount distrubution (50% sample data)")
    
    # Valores outliers (lista)
    credit_amount_outliers <- boxplot(data_sample$credit_amount)$out
    cat("La variable", bold("credit amount"), "tiene", red(length(credit_amount_outliers)), "datos outliers y son los siguientes valores:", red(paste(credit_amount_outliers, collapse = ",")))
    
    #.............................
    # Transformacion: No aplica.
    
    #.............................
    sum(is.na(data_sample$credit_amount))
    # Valores NULL: No hay. Obviamente si en la muestra completa no habia NULLs, en la muestra tampoco los hay.
    
    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
    # Variable: duration
    # Type: numeric
    
    # ...........................
    # Resumen estadistico formal
    summary(data_sample$duration)
    # La mediana es distinta a 50, por lo tanto es una medicion sesgada.
    
    # Analisis de la distribucion
    hist(data_sample$duration, main = "Credit amount distribution (50% sample data)", xlab = "duration", ylab="frequency")
    
    # Agregamos la leyenda al histograma indicando que se identifica una distrubucion asimetrica a la derecha, 
    # la asimetria positiva implica que hay mas valores distintos a la derecha de la media.
    legend("topright", legend=c(""), title = "Distribucion con sesgo positivo o asimetrica a la derecha", text.font=2, bg='lightgreen',cex=0.85)
    
    #.............................
    # Analisis de datos outliers
    
    # Visualizacion de la variable mediante un boxplot, para estudiar la distribucion de la variable e identificar datos outliers
    boxplot(data_sample$duration, main="Duration distrubution (50% sample data)")
    
    # Valores outliers (lista)
    duration_outliers <- boxplot(data_sample$duration)$out
    cat("La variable", bold("duration"), "tiene", red(length(duration_outliers)), "datos outliers y son los siguientes valores:", red(paste(duration_outliers, collapse = ",")))
    
    #.............................
    # Transformacion: No aplica.
    
    sum(is.na(data_sample$duration))
    #.............................
    # Valores NULL: No hay. Obviamente si en la muestra completa no habia NULLs, en la muestra tampoco los hay.
    
    
#########################################################################################
# ......................................................................................#
# Analisis comparativo de los resultados del dataset completo y de la muestra del 50%
# ......................................................................................#
#########################################################################################

  # Comparacion de resultados con herramientas visuales
    par(mfrow=c(1,2)) # Dos columnas para ver los resultados en el mismo plot

    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
    # Variables cualitativas
    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
    # Variable: gender
    # Type: caracter
      pie(table(data$gender, useNA="always"))           
      pie(table(data_sample$gender, useNA="always"))    
      
      # Total
      x_sex_value<-c(table(data$gender))
      x_sex_label<-unique(c(data$gender))
      pie_percent_sex<-c(round(prop.table(table(data$gender))*100),2)
      etiquetas_sex <- paste0(x_sex_label, " = ", pie_percent_sex, "%")
      pie(x_sex_value, labels = etiquetas_sex, main = "Sex PieChart DF",col = rainbow(length(x_sex_label)))  
      
      # Muestra 50%
      x_sex_value<-c(table(data_sample$gender))
      x_sex_label<-unique(c(data_sample$gender))
      pie_percent_sex<-c(round(prop.table(table(data_sample$gender))*100),2)
      etiquetas_sex <- paste0(x_sex_label, " = ", pie_percent_sex, "%")
      pie(x_sex_value, labels = etiquetas_sex, main = "Sex PieChart DF",col = rainbow(length(x_sex_label)))  
      
    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
    # Variable: job
    # Type: caracter
      pie(table(data$job, useNA="always"))            # Total
      pie(table(data_sample$job, useNA="always"))     # Muestra 50%

      
    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
    # Variable: housing
    # Type: caracter
      pie(table(data$housing, useNA="always"))           # Total
      pie(table(data_sample$housing, useNA="always"))    # Muestra 50%

    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
    # Variable: savings_account
    # Type: caracter
      pie(table(data$savings_account, useNA="always"))           # Total
      pie(table(data_sample$savings_account, useNA="always"))    # Muestra 50%

    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
    # Variable: checking_account
    # Type: caracter
      pie(table(data$checking_account, useNA="always"))           # Total
      pie(table(data_sample$checking_account, useNA="always"))    # Muestra 50%

    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
    # Variable: purpose
    # Type: caracter
      pie(table(data$purpose, useNA="always"))           # Total
      pie(table(data_sample$purpose, useNA="always"))    # Muestra 50%

    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
    # Variables cuantitativas
    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
    # Variable: age
    # Type: numeric

      # Histograma
      hist(data$age, main = "Age distribution", xlab = "age [years]", ylab="frequency")
      hist(data_sample$age, main = "Age distribution (50% sample data)", xlab = "age [years]", ylab="frequency")
    
      # Boxplot
      boxplot(data$age, main="Age distrubution")
      boxplot(data_sample$age, main="Age distrubution (50% sample data)")
    
    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
    # Variable: credit_amount
    # Type: numeric

      # Histograma
      hist(data$credit_amount, main = "Credit amount distribution", xlab = "age [years]", ylab="frequency")
      hist(data_sample$credit_amount, main = "Credit amount distribution (50% sample data)", xlab = "age [years]", ylab="frequency")
    
      # Boxplot
      boxplot(data$credit_amount, main="Credit amount distrubution")
      boxplot(data_sample$credit_amount, main="Credit amount distrubution (50% sample data)")

    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
    # Variable: duration
    # Type: numeric

      # Histograma
      hist(data$duration, main = "Duration distribution", xlab = "age [years]", ylab="frequency")
      hist(data_sample$duration, main = "Duration distribution (50% sample data)", xlab = "age [years]", ylab="frequency")
    
      # Boxplot
      boxplot(data$duration, main="Duration distrubution")
      boxplot(data_sample$duration, main="Duration distrubution (50% sample data)")

      #Conclusiones
      #Se observa que al hacer las comparaciones con los datos de las variables categóricas del Dataset completo con sus respectivas variables del Dataset de la muestra, se mantienen las mismas proporciones en la relación de datos.
      #Se podría desarrollar una análisis bivariado entre las variables Credit Amount y Saving Account, y entre Credit Amount y Checking Account, para clasificar las etiquetas faltantes.
      # De la visualización de la variable Gender, se observa que aproximadamente el 70% de solicitantes al crédito son femeninos y aproximadamente el 30% son masculinos
      # De la visualización de la variable Housing, se observa que aproximadamente el 70% de los solicitantes, son propietarios de al menos una vivienda.
      # De la visualización de la variable Saving Account, se observa que el 60% de los solicitantes, tienen un monto de ahorros bajos y el 5% elevados.
      # De la visualización de la variable Checking Account, se observa que aproximadamente el 40% de los solicitantes no cuenta con una categoría establecida, mientras que aproximadamente un 27% está tipificado con Checking Account Bajo, y aproximadamente el 6% con Checking Account Alto.
      # De la visualización de la variable Purpose se observa que el principal destino del crédito esta destinado al Sector Automotriz. Mientras que, en segundo lugar se destina a la adquisición de bienes relacionados al Sector Audio y Video y en tercer lugar a Remodelaciones.Sector Audio y Video y en tercer lugar a Remodelaciones.ón de la variable Purpose se observa que el principal destino del crédito esta destinado al Sector Automotriz. Mientras que, en segundo lugar se destina a la adquisición de bienes relacionados al Sector Audio y Video y en tercer lugar a Remodelaciones.bserva que el principal destino del crédito esta destinado al Sector Automotriz. Mientras que, en segundo lugar se destina a la adquisición de bienes relacionados al Sector Audio y Video y en tercer lugar a Remodelaciones.