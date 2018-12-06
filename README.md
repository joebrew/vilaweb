
<!-- README.md is generated from README.Rmd. Please edit that file -->
vilaweb: The R package of Joe Brew's VilaWeb data analysis
==========================================================

Installation
------------

``` r
if(!require(devtools)) install.packages("devtools")
install_github('joebrew/vilaweb')
```

About
-----

`vilaweb` is...

Developer details
-----------------

In order to reproduce the entire package, raw data will need to be downloaded from various sources

### Downloading raw data

#### 1. Download CIS data

Download into `data-raw/cis` the "Barómetro Mensual - 2000-2018 all data" file from <http://analisis.cis.es/fid/fidHistorico.jsp>. This requires an account. Following log-in, go to the "Ficheros Integrados de Datos" page. Download the full data file. This will be downloaded as `FID_637_06bcee7b-ea6b-4f41-a74e-37a941519966.zip` (or similar, depending on date downloaded). Extract the data as is in the folder in which it was downloaded.

#### 2. Download CEO data

Download into `data-raw/ceo` the "Matriu de dades fusionada a partir de 2014 (presdencial)" file as `2014_Microdades_anonimitzades_fusio_cine_pres.rar` from <http://ceo.gencat.cat/ca/barometre/matrius-fusionada-BOP/>. Extract the data as is in the folder in which it was downloaded.

#### 3. Download ICPS data

Download into `data-raw/icps` the "Sondeig d'opinió Catalunya 2018" data from <https://www.icps.cat/recerca/sondeigs-i-dades/sondeigs/sondeigs-d-opinio-catalunya>. This will require creating an account and password. Download both the 2017 and 2018 data into the 'data' sub-folder.
