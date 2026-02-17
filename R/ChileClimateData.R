if(getRversion() >= "2.15.1")  utils::globalVariables(c("date_posix", "momento"))

#' Title ChileClimateData
#' @description function that compiles climate data from Climate direction of Chile (D.M.C.)
#' @param Estaciones data vector containing the  codes of the monitoring
#'  stations. To see the table with the monitoring stations use ChileClimateData()
#' @param Parametros data vector containing the names of the climate parameters.
#'  Available parameters: "Temperatura", "PuntoRocio", "Humedad","Viento", "PresionQFE", "PresionQFF".
#'
#' @param inicio text string containing the start year of the data request.
#' @param fin text string containing the end year of the data request.
#' @param Region logical parameter. If region is true it allows to enter the administrative region in which the station is located instead of the station code.
#' @return A data frame with climate data of Chile.
#'
#' @source <http://www.meteochile.gob.cl/>
#' @export
#' @import data.table
#' @import utils
#'
#'
#' @examples
#'
#' try({ChileClimateData()}, silent = TRUE)
#'
#' try({
#' data <- ChileClimateData(Estaciones = "180005",
#'  Parametros = c("Temperatura", "Humedad"),
#'   inicio = "2020", fin = "2020")
#' }, silent = TRUE)
#'
#' try({
#' head(ChileClimateData(Estaciones = "II",
#'  Parametros = "Temperatura", inicio = "2020",
#'   fin = "2020", Region = TRUE))
#' }, silent = TRUE)

ChileClimateData <- function(Estaciones = "INFO", Parametros, inicio, fin, Region = FALSE){
  
  sysEstaciones   <- system.file("extdata", "Estaciones.csv", package = "AtmChile")
  tablaEstaciones <- read.csv(sysEstaciones, sep = "," , dec =".", encoding = "UTF-8")
  
  if(Estaciones[1] == "INFO") return(tablaEstaciones)
  if(fin < inicio) stop("Verificar fechas de inicio y fin")
  
  col_idx <- if(Region) 7 else 1
  estaciones_target <- tablaEstaciones[tablaEstaciones[, col_idx] %in% Estaciones, ]
  
  if(nrow(estaciones_target) == 0) stop("No se encontraron estaciones.")
  
  url_base <- "https://climatologia.meteochile.gob.cl/application/datos/getDatosSaclim/"
  parametros_validos <- c("Temperatura", "PuntoRocio", "Humedad", "Viento", "PresionQFE", "PresionQFF")
  params_to_fetch <- intersect(Parametros, parametros_validos)
  
  # Generamos secuencia de fechas maestra
  start_date <- as.POSIXct(paste0(inicio, "-01-01 00:00:00"), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  end_date   <- as.POSIXct(paste0(fin, "-12-31 23:00:00"), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  date_seq   <- seq(start_date, end_date, by = "hour")
  
  lista_final <- list()
  
  # Directorio temporal para esta ejecución
  temp_dir <- tempdir()
  
  for(j in 1:nrow(estaciones_target)){
    
    codigo_estacion <- estaciones_target[j, 1]
    nombre_estacion <- estaciones_target[j, 4]
    latitud         <- estaciones_target[j, 5]
    longitud        <- estaciones_target[j, 6]
    
    dt_base <- data.table::data.table(
      date_posix = date_seq,
      Nombre = nombre_estacion,
      Latitud = latitud,
      Longitud = longitud,
      key = "date_posix"
    )
    
    for(param in params_to_fetch){
      data_param_years <- list()
      
      for(year in inicio:fin){
        url_query <- paste0(url_base, codigo_estacion, "_", year, "_", param, "_")
        zip_file  <- tempfile(fileext = ".zip") # El zip ya se crea en temp
        
        tryCatch({
          download.file(url_query, zip_file, mode = "wb", quiet = TRUE)
          csv_name_internal <- paste0(codigo_estacion, "_", year, "_", param, "_.csv")
          
          # --- CAMBIO IMPORTANTE ---
          # 1. Descomprimimos explícitamente en el directorio temporal (exdir = temp_dir)
          # unzip devuelve la ruta completa del archivo extraído
          extracted_csv <- unzip(zip_file, files = csv_name_internal, exdir = temp_dir)
          
          if(!is.null(extracted_csv) && length(extracted_csv) > 0){
            dt_year <- tryCatch({
              data.table::fread(extracted_csv, 
                                sep = ";", dec = ".", encoding = "UTF-8", 
                                header = TRUE, showProgress = FALSE)
            }, error = function(e) NULL)
            
            # Eliminamos el CSV inmediatamente después de leerlo para no ocupar espacio
            unlink(extracted_csv)
          } else {
            dt_year <- NULL
          }
          
          if(!is.null(dt_year) && nrow(dt_year) > 0){
            
            if(param == "Viento"){
              dt_year <- dt_year[, c(2, 3, 4, 5), with = FALSE]
              names(dt_year) <- c("momento", "dd_Valor", "ff_Valor", "VRB_Valor")
            } else {
              dt_year <- dt_year[, c(2, 3), with = FALSE]
              names(dt_year) <- c("momento", param)
            }
            
            dt_year[, date_posix := as.POSIXct(momento, format = "%Y-%m-%d %H:%M", tz = "UTC")]
            
            if(all(is.na(dt_year$date_posix))){
              dt_year[, date_posix := as.POSIXct(momento, format = "%d-%m-%Y %H:%M:%S", tz = "UTC")]
            }
            
            dt_year[, momento := NULL]
            dt_year <- dt_year[!is.na(date_posix)]
            
            data_param_years[[paste(year)]] <- dt_year
          }
        }, error = function(e) {}, finally = { 
          # Limpiamos el ZIP
          if(file.exists(zip_file)) unlink(zip_file) 
        })
      } 
      
      if(length(data_param_years) > 0){
        dt_param_full <- data.table::rbindlist(data_param_years)
        dt_param_full <- unique(dt_param_full, by = "date_posix")
        dt_base <- merge(dt_base, dt_param_full, by = "date_posix", all.x = TRUE)
      } else {
        if(param == "Viento"){
          dt_base[, `:=`(dd_Valor = NA_real_, ff_Valor = NA_real_, VRB_Valor = NA_real_)]
        } else {
          dt_base[, (param) := NA_real_]
        }
      }
    }
    lista_final[[j]] <- dt_base
  }
  
  if(length(lista_final) == 0) return(NULL)
  data_total <- data.table::rbindlist(lista_final)
  
  data_total[, date := format(date_posix, "%d/%m/%Y %H:%M")]
  data_total[, date_posix := NULL] 
  
  setcolorder(data_total, c("date", "Nombre", "Latitud", "Longitud"))
  
  cols_to_numeric <- setdiff(names(data_total), c("date", "Nombre"))
  suppressWarnings({
    data_total[, (cols_to_numeric) := lapply(.SD, as.numeric), .SDcols = cols_to_numeric]
  })
  
  return(as.data.frame(data_total))
}