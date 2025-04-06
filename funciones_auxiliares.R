library(tidyverse)


agregar_componentes_principales <- function(df, variables, sufix="", escalar = TRUE){
  df_reducido <- df |> 
    select(all_of(c("mes", variables) ) )|> drop_na()
  
  componentes <-df_reducido |>
    select(-mes) |> 
    prcomp(scale. = escalar)
  
  df_final <- df_reducido |> 
    bind_cols(as_tibble(componentes$x)|> rename_with(~paste0(.x, sufix)))
  
  return(df_final)
}


calcular_impulso <- function(df, variable_impulso, impulso_pct){

  if (variable_impulso %in% c("antibioticos_real_dif1",
                              "antisepticos_real_dif1",
                              "insecticidas_real_dif1",
                              "medicamentos_real_dif1",
                              "sales_y_minerales_real_dif1")){
    
    #Nombre de la variable en el dataframe (sin diferenciar)
    variable_insumo <- str_replace(variable_impulso, "_dif1", "") 
    
    # Calcula el valor del impulso en la variable con una diferencia
    valor_variable <- impulso_pct * df |> 
      select(all_of(variable_insumo)) |> tail(1) |> pull()
    
  } else if (variable_impulso == "acopio_vs_ventas_industria") {
    
    valor_variable <- (1+impulso_pct) * df |> 
      select(acopio_vs_ventas_industria) |> tail(1) |> pull()
    
  } else {
    valor_variable <- impulso_pct
  } 
  return(valor_variable)
}


predecir_valors <- function(df, 
                            variable_impulso, 
                            impulso_pct, 
                            modelo_lineal, 
                            componentes_principales, 
                            iteraciones = 36, 
                            ultimas_obs = 2,
                            duracion_impulso = 1
                            ){
  
  valor_inicial_acopio_vs_ventas_industria <-  df |> slice_tail(n=1) |> pull(acopio_vs_ventas_industria)
  
  valor_variable_impulso <- df |> calcular_impulso(variable_impulso, impulso_pct)
  
  results <- 
    df |>
    select(mes,
           precio,
           precio_dif1,
           precio_dif1_lag_1,
           precio_dif1_lag_2,
           
           acopio_vs_ventas_industria,
           
           precio_pct_alimentos,
           precio_pct_fertilizantes,
           
           antibioticos_real_dif1,
           antisepticos_real_dif1,
           insecticidas_real_dif1,
           medicamentos_real_dif1,
           sales_y_minerales_real_dif1) |> 
    tail(ultimas_obs) |> 
    mutate(tipo_variable = "Actual")
  
  
  
  for (iteracion in 1:iteraciones){
    filas <- nrow(results)
    
    # Valores por defecto si no hay cambios en ninguna variable
    nueva_fila <- list(
      mes = results[filas, "mes"] |> pull() + 1,
      precio_dif1_lag_1 = results[filas, "precio_dif1"] |> pull(),
      precio_dif1_lag_2 = results[filas, "precio_dif1_lag_1"] |> pull(),
      acopio_vs_ventas_industria = valor_inicial_acopio_vs_ventas_industria,
      precio_pct_alimentos = 0,
      precio_pct_fertilizantes = 0,
      antibioticos_real_dif1 = 0,
      antisepticos_real_dif1 = 0,
      insecticidas_real_dif1 = 0,
      medicamentos_real_dif1 = 0,
      sales_y_minerales_real_dif1 = 0,
      tipo_variable = "Prediccion"
    ) |> as_tibble()
    
    # Cambia el valor de la variable que recibe el impulso. Solo ocurre en la primer iteracion
    if (iteracion <= duracion_impulso){
      nueva_fila <- 
        nueva_fila |> 
        mutate({{variable_impulso}} := valor_variable_impulso)
    }
    
    # Calcular Componentes principales
    componentes_fila <- 
      predict(componentes_principales, 
              nueva_fila |> select(antibioticos_real_dif1,
                                   antisepticos_real_dif1,
                                   insecticidas_real_dif1,
                                   medicamentos_real_dif1,
                                   sales_y_minerales_real_dif1)) |> as_tibble() |>
      rename_with(~paste0(.x, "_dif1"))
    
    # Inserta el valor de los compenentes principales en la fila
    nueva_fila <- bind_cols(nueva_fila, componentes_fila)
    # Predice la diferencia en el precio con los datos de la nueva fila
    precio_dif1 <-  predict(modelo_lineal,  nueva_fila)
    # Calcula el precio sumando el valor del precio de la ultima fila con la variacion recien calculada
    precio <- results[filas, "precio"] |> pull() + precio_dif1
    # Inserta los resultados en el dataframe de resultados
    results <- results |> bind_rows(bind_cols(precio = precio, precio_dif1 = precio_dif1, nueva_fila) )
  }
  
  return(results)
}


simulacion_niveles <- function(df, 
                               variable_impulso,
                               label_variable,
                               modelo_lineal, 
                               componentes_principales,
                               magnitud_impulsos = c(0.1, 0, -0.1, -0.2),
                               labels_impulsos = c("Incremento 10%", "Nivel Actual", "Decrecimiento 10%", "Decrecimiento 20%"),
                               iteraciones = 12,
                               ultimas_obs = 48,
                               duracion_impulso = 12,
                               colores = c("Actual" = "blue", "Prediccion" = "red") ,
                               tipo_linea= c("Actual" = 1, "Prediccion" = 5) ){
   
  for (i in 1:length(magnitud_impulsos)){
    impulso <- predecir_valors(df = df, 
                               variable_impulso = variable_impulso, 
                               impulso_pct = magnitud_impulsos[i], 
                               modelo_lineal = modelo_lineal, 
                               componentes_principales = componentes_principales,
                               iteraciones = iteraciones, ultimas_obs = ultimas_obs, duracion_impulso = duracion_impulso) |> 
      mutate(tipo_impulso = labels_impulsos[i])
    
    if (i == 1){
      results <- impulso
    } else {
      results <- bind_rows(results, impulso)
    }
  }
  grafico <- results |>  
    select(mes, 
           `Precio` = precio, 
           {{label_variable}} := {{variable_impulso}}, 
           tipo_variable, 
           tipo_impulso) |>
    pivot_longer(-all_of(c("mes", "tipo_variable", "tipo_impulso")) )|> 
    ggplot(aes(x = mes, 
               y = value, 
               color = tipo_variable,
               linetype = tipo_variable))+
    geom_line()+
    facet_grid(rows= vars(name), cols = vars(tipo_impulso)
               , scales = "free_y")+
    ggtitle(paste("Efecto en el precio de cambiar", label_variable))+
    scale_color_manual(values = colores)+
    scale_linetype_manual(values = tipo_linea)+
    labs(x="", y = "")+
    theme_bw()+
    theme(legend.position = "top", legend.title = element_blank())
  
  return(list(results, grafico))
}