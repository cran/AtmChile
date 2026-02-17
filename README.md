# AtmChile

[![](https://www.r-pkg.org/badges/version/AtmChile?color=green)](https://cran.r-project.org/package=AtmChile)
![]([https://github.com/franciscoxaxo/AtmChile/actions/workflows/r.yml/badge.svg](https://github.com/franciscoxaxo/AtmChile/actions/workflows/r.yml/badge.svg))
![]([https://cranlogs.r-pkg.org/badges/grand-total/AtmChile](https://cranlogs.r-pkg.org/badges/grand-total/AtmChile))

**AtmChile** is an R package designed to download and compile information on air quality and meteorological parameters in Chile. It sources data from:
* **SINCA**: National Air Quality System (Ministry of the Environment).
* **DMC**: Meteorological Directorate of Chile (Directorate General of Civil Aeronautics).

> **Project developed by:** Department of Chemistry, Faculty of Sciences of the University of Chile.
> **Funding:** FONDECYT Project 1200674.

## Installation

You can install the stable version from CRAN:

```r
install.packages("AtmChile")
```

Or the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("franciscoxaxo/AtmChile")
```

## Usage

Load the library:

```r
library(AtmChile)
```

### 1. ChileAirQuality

Retrieves air quality data from the National Air Quality System ([SINCA](https://sinca.mma.gob.cl/)).

**Available Parameters:**

| Parameter | Description | Units |
| :--- | :--- | :--- |
| **PM10** | Particulate matter less than 10 microns | ug/m3N |
| **PM25** | Particulate matter less than 2.5 microns | ug/m3N |
| **SO2** | Sulfur dioxide | ug/m3N |
| **NOX** | Nitrogen oxides | ppb |
| **NO** | Nitrogen monoxide | ppb |
| **NO2** | Nitrogen dioxide | ppb |
| **O3** | Tropospheric ozone | ppb |
| **CO** | Carbon monoxide | ppb |
| **temp** | Temperature | °C |
| **ws** | Wind speed | m/s |
| **wd** | Wind direction | ° |
| **HR** | Relative humidity | % |

**How to find station codes:**
To view the full table of available monitoring stations and their codes (e.g., "SA" for Parque O'Higgins), run the function without arguments:

```r
# View available stations
stations <- ChileAirQuality()
print(stations)
```

**Arguments:**

* `Comunas`: Vector containing names or codes of the monitoring stations (e.g., "Cerrillos", "SA"). Use `"all"` for all stations.
* `Parametros`: Vector containing the names of air quality parameters. Use `"all"` for all parameters.
* `fechadeInicio`: Start date (format "dd/mm/yyyy").
* `fechadeTermino`: End date (format "dd/mm/yyyy").
* `Curar`: Logical (`TRUE`/`FALSE`). Activates data curation for particulate matter, NOx, RH, and wind direction. Default: `TRUE`.
* `Site`: Logical. If `TRUE`, allows searching by station code instead of name in `Comunas`. Default: `FALSE`.
* `st`: Logical. If `TRUE`, includes validation reports from SINCA ("NV": Not Validated, "PV": Pre-Validated, "V": Validated). Default: `FALSE`.

**Examples:**

```r
# Example 1: Basic request by name
data_names <- ChileAirQuality(
  Comunas = "Cerrillos",
  Parametros = c("PM10", "PM25"),
  fechadeInicio = "01/01/2020",
  fechadeTermino = "01/01/2021"
)

# Example 2: Request by Station Code (Site = TRUE) without curation
data_codes <- ChileAirQuality(
  Comunas = c("SA", "CE"),
  Parametros = c("NO2", "O3"),
  fechadeInicio = "01/01/2020",
  fechadeTermino = "01/01/2021",
  Curar = FALSE,
  Site = TRUE
)

# Example 3: Download everything (All stations, all parameters)
data_all <- ChileAirQuality(
  Comunas = "all",
  Parametros = "all",
  fechadeInicio = "01/01/2020",
  fechadeTermino = "01/01/2021"
)
```

---

### 2. ChileClimateData

Retrieves climate data from the Meteorological Directorate of Chile ([DMC](https://www.meteochile.gob.cl/)).

**Available Parameters:**
* `Temperatura` (Temperature)
* `PuntoRocio` (Dew point)
* `Humedad` (Humidity)
* `Viento` (Wind)
* `PresionQFF` (Pressure at sea level)
* `PresionQFE` (Pressure at station level)

**How to find station codes:**
To see the table with the available weather stations:

```r
# View available climate stations
climate_stations <- ChileClimateData()
print(climate_stations)
```

**Arguments:**

* `Estaciones`: Vector containing codes of the monitoring stations (e.g., "180005").
* `Parametros`: Vector containing the names of climate parameters.
* `inicio`: Text string containing the **start year** (e.g., "2020").
* `fin`: Text string containing the **end year** (e.g., "2021").
* `Region`: Logical. If `TRUE`, allows entering the administrative region code (e.g., "XV", "RM") in `Estaciones` instead of the station code.

**Examples:**

```r
# Example 1: Request by specific station codes
climate_data <- ChileClimateData(
  Estaciones = c("180005", "200006"),
  Parametros = c("Temperatura", "Humedad", "Viento"),
  inicio = "2020",
  fin = "2021"
)

# Example 2: Request by Administrative Region
region_data <- ChileClimateData(
  Estaciones = "II",
  Parametros = "Temperatura",
  inicio = "2020",
  fin = "2021",
  Region = TRUE
)
```

---

### 3. ChileAirQualityApp

A Shiny dashboard that provides a graphical interface for the package. It allows users to download data, perform analysis, visualize trends, and calculate descriptive statistics without writing code.

```r
# Launch the local app
ChileAirQualityApp()
```

This dashboard is also hosted online at:
[**ChileAirQualityApp on ShinyApps.io**](https://chileairquality.shinyapps.io/chileairquality/)