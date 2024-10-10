
library(dplyr)
library(leaflet)
library(plotly)

substituir_nan_inf <- function(df) {
  df %>%
    mutate(across(where(is.numeric), ~ ifelse(is.nan(.) | is.infinite(.), NA, .)))
}

# -------------------------------------------------------------------------

dados_rj <- readRDS("~/git/dados_obs/data/dados_rj.rds")

dados_rj <- dados_rj %>%
  select(ano, municipio, carga_tributaria, depedencia_petroleo, gasto_sociais, gasto_urbanos) %>%
  substituir_nan_inf()

# -------------------------------------------------------------------------

geo_br <- readRDS("~/git/dados_obs/data/geo_br.rds")

geo_br$name_muni <- tolower(geo_br$name_muni)
geo_br$name_muni <- stringi::stri_trans_general(geo_br$name_muni, 'latin-ascii')

anos <- 2004:2023

geo_br_expanded <- geo_br[rep(1:nrow(geo_br), each = length(anos)), ]
geo_br_expanded$ano <- rep(anos, times = nrow(geo_br))

geo_br_dados <- left_join(geo_br_expanded, dados_rj, by = c("ano", "name_muni" = "municipio"))
choice_var <- names(dados_rj)
choice_var <- choice_var[-c(1,2)]

municipios <- unique(dados_rj$municipio)
anos <- unique(geo_br_dados$ano)

# -------------------------------------------------------------------------

dados_rj$carga_tributaria
dados_rj$depedencia_petroleo
dados_rj$gasto_sociais
dados_rj$gasto_urbanos

mean_rj <- dados_rj |> 
  group_by(ano) |> 
  summarise(
    carga_tributaria    = mean(carga_tributaria, na.rm = T),
    depedencia_petroleo = mean(depedencia_petroleo, na.rm = T),
    gasto_sociais       = mean(gasto_sociais, na.rm = T),
    gasto_urbanos       = mean(gasto_urbanos, na.rm = T)
  ) |> 
  mutate(municipio = "Media Estadual")


