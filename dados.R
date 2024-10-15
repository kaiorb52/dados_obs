
library(dplyr)
library(tidyr)
library(sf)
library(leaflet)
library(plotly)

substituir_nan_inf <- function(df) {
  df %>%
    mutate(across(where(is.numeric), ~ ifelse(is.nan(.) | is.infinite(.), NA, .)))
}

# -------------------------------------------------------------------------

dados_rj <- readRDS("dados_rj.rds")
geo_br <- readRDS("geo_br.rds")

Dados_Populacionais_Censo <- readRDS("~/git/dados_obs/Dados_Populacionais_Censo.rds")

# 1. Fiscal ================================================================

dados_rj <- dados_rj %>%
  select(ano, municipio, carga_tributaria, depedencia_petroleo, gasto_sociais, gasto_urbanos) %>%
  substituir_nan_inf()

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

rm(geo_br_expanded)

# -------------------------------------------------------------------------

# dados_rj$carga_tributaria
# dados_rj$depedencia_petroleo
# dados_rj$gasto_sociais
# dados_rj$gasto_urbanos

mean_rj <- dados_rj |> 
  group_by(ano) |> 
  summarise(
    carga_tributaria    = mean(carga_tributaria, na.rm = T),
    depedencia_petroleo = mean(depedencia_petroleo, na.rm = T),
    gasto_sociais       = mean(gasto_sociais, na.rm = T),
    gasto_urbanos       = mean(gasto_urbanos, na.rm = T)
  ) |> 
  mutate(municipio = "Media Estadual")

# 2. Demografia =============================================================

dados_censo <- Dados_Populacionais_Censo |> 
  pivot_longer(cols = Tx_Popul_2022:DOMI_Total_2010) |> 
  #filter(!(grepl("POP_", name))) |> 
  mutate(
    ano = case_when(
      grepl("2010", name) ~ 2010,
      grepl("2022", name) ~ 2022
    ),
    name = stringr::str_replace(name, "_(2010|2022)", "")
  ) |> 
  select(ano, everything()) |> 
  pivot_wider(names_from = name, values_from = value) |> 
  mutate(
    TAXA_Hab_per_Domi = (POP_Total / DOMI_Total),
    PERC_04a14  = (POP_04a14 / (POP_04a14 + POP_15a24 + POP_25a49 + POP_50a64 + POP_65mais)) * 100,
    PERC_15a24  = (POP_15a24 / (POP_04a14 + POP_15a24 + POP_25a49 + POP_50a64 + POP_65mais)) * 100,
    PERC_25a49  = (POP_04a14 / (POP_04a14 + POP_15a24 + POP_25a49 + POP_50a64 + POP_65mais)) * 100,
    PERC_50a64  = (POP_04a14 / (POP_04a14 + POP_15a24 + POP_25a49 + POP_50a64 + POP_65mais)) * 100,
    PERC_65mais = (POP_04a14 / (POP_04a14 + POP_15a24 + POP_25a49 + POP_50a64 + POP_65mais)) * 100
  ) |> 
  mutate(PERC_Indigenas = coalesce(PERC_Indigena, PERC_Indigenas)) |> 
  select(ano, Cod_07, contains("PERC"), -PERC_Indigena, TAXA_Hab_per_Domi)

# dados_censo$per
# dados_censo$POP_15a24
# dados_censo$POP_25a49
# dados_censo$POP_50a64
# dados_censo$POP_65mais

demo_vars <- names(dados_censo)

dados_censo2 <- left_join(geo_br, dados_censo, by = c("code_muni" = "Cod_07"))

