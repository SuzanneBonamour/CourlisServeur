###
####
# MAREE ------------------------------------------------------------------------
####
###

## Prédites ----

# Chargement des données marées
tides <- read_csv("~/Courlis/Data/1) data/Maree/tides.csv")
tides$DateTime <- paste0(tides$y_m_d, " ", tides$time)

tides$sunrise_UTC <- ymd_hms(paste0(tides$y_m_d, tides$sunrise), tz = "Europe/Paris")
tides$sunrise_UTC <- with_tz(tides$sunrise_UTC, "UTC")
tides$sunset_UTC <- ymd_hms(paste0(tides$y_m_d, tides$sunset), tz = "Europe/Paris")
tides$sunset_UTC <- with_tz(tides$sunset_UTC, "UTC")

tides <- tides %>% 
  na.omit() %>% 
  distinct()

tides <- tides %>%
  mutate(date_UTC = as.character(DateTime),  # Convertir si besoin
         date_UTC = if_else(grepl("^\\d{2}-\\d{2}-\\d{4}", date_UTC), 
                            dmy_hms(date_UTC),  # Format DD-MM-YYYY
                            ymd_hms(date_UTC)),
         date_UTC = with_tz(date_UTC, "UTC")) # Format YYYY-MM-DD

tides <- tides %>% 
  mutate(date_rounded = round_date(ymd_hms(date_UTC), "30 mins"))

## Observées ----

# prendre en priorité #3 "validé temps différé", 
# puis #2 "brute temps différé", 
# puis #1 "brute hautes fréquences"  

# remplissage des na dans le marégraphe de l'ile d'aix avec les infos des corrélations 
# en priorité de la rochelle, puis de la cotinière

# Chargement des données marées

# Ile d'aix
maree_path_aix <- paste0(data_path_serveur, "Maree/maregraphie/Ile_d_aix/ok/")
files_maree_aix <- paste0(maree_path_aix, list.files(path = maree_path_aix, pattern = "*.txt"))
dt_maree_aix <- lapply(files_maree_aix, fread, sep = ";")
maree_aix <- rbindlist(dt_maree_aix)
maree_aix$maregraphe <- "aix"

maree_aix_2 <- maree_aix %>% 
  filter(Source !=4) %>% 
  pivot_wider(names_from = Source, values_from = Valeur) %>% 
  rename("valide_temps_diff" = "3", "brute_temps_diff" = "2", "brute_haute_freq" = "1") %>% 
  mutate(hauteur_eau = coalesce(valide_temps_diff, brute_temps_diff, brute_haute_freq)) %>% 
  select(Date, maregraphe, hauteur_eau) %>% 
  distinct()

# La cotiniere
maree_path_coti <- paste0(data_path_serveur, "Maree/maregraphie/La_cotiniere/ok/")
files_maree_coti <- paste0(maree_path_coti, list.files(path = maree_path_coti, pattern = "*.txt"))
dt_maree_coti <- lapply(files_maree_coti, fread, sep = ";")
maree_coti <- rbindlist(dt_maree_coti)
maree_coti$maregraphe <- "cotiniere"

maree_coti_2 <- maree_coti %>% 
  rename("brute_haute_freq" = "Source", "hauteur_eau" = "Valeur") %>% 
  select(Date, maregraphe, hauteur_eau) %>% 
  distinct()

# La Rochelle
maree_path_roch <- paste0(data_path_serveur, "Maree/maregraphie/La_rochelle/ok/")
files_maree_roch <- paste0(maree_path_roch, list.files(path = maree_path_roch, pattern = "*.txt"))
dt_maree_roch <- lapply(files_maree_roch, fread, sep = ";")
maree_roch <- rbindlist(dt_maree_roch)
maree_roch$maregraphe <- "rochelle"

maree_roch_2 <- maree_roch %>% 
  filter(Source !=4) %>% 
  pivot_wider(names_from = Source, values_from = Valeur) %>% 
  rename("valide_temps_diff" = "3", "brute_temps_diff" = "2", "brute_haute_freq" = "1") %>% 
  mutate(hauteur_eau = coalesce(valide_temps_diff, brute_temps_diff, brute_haute_freq)) %>% 
  select(Date, maregraphe, hauteur_eau) %>% 
  distinct()

# all maregraphes
maree <- rbind(maree_aix_2, maree_coti_2, maree_roch_2) %>% 
  na.omit()

maree2 <- maree %>% 
  mutate(date_2 = gsub("/", "-", Date))

maree3 <- maree2 %>%
  mutate(date_UTC = as.character(date_2),  # Convertir si besoin
         date_UTC = if_else(grepl("^\\d{2}-\\d{2}-\\d{4}", date_UTC), 
                            dmy_hms(date_UTC),  # Format DD-MM-YYYY
                            ymd_hms(date_UTC)),
         date_UTC = with_tz(date_UTC, "UTC")) # Format YYYY-MM-DD

str(maree3$date_UTC)  # Vérifie le type
tz(maree3$date_UTC)

maree4 <- maree3 %>% 
  mutate(date_rounded = round_date(date_UTC, "30 mins"))

# comparaison entre marégraphe
maree_sum <- maree4 %>%  
  group_by(maregraphe, date_rounded) %>% 
  summarise(mean_hauteur_eau = mean(hauteur_eau))

maree_spread <- maree_sum %>% 
  pivot_wider(names_from = maregraphe, values_from = c(mean_hauteur_eau))

model_aix_roch <- lm(maree_spread$aix ~ maree_spread$rochelle)
model_aix_coti <- lm(maree_spread$aix ~ maree_spread$cotiniere)

# estimation des prédictions selon la corrélation aix ~ rochelle et aix ~ cotiniere
maree_spread <- maree_spread %>% 
  mutate(pred_aix_roch = aix*model_aix_roch$coefficients[2] + model_aix_roch$coefficients[1],
         pred_aix_coti = aix*model_aix_coti$coefficients[2] + model_aix_coti$coefficients[1])

# selection de la valeur observé, puis la prédiction via la rochelle, puis via cotiniere
maree_correl <- maree_spread %>% 
  mutate(hauteur_eau_and_pred = coalesce(aix, pred_aix_roch, pred_aix_coti)) %>% 
  select(date_rounded, hauteur_eau_and_pred) %>% 
  distinct()

## Prédites et observées ---- 

tides <- left_join(tides, maree_correl)

tides <- tides %>% 
  dplyr::rename(mean_height_obs = hauteur_eau_and_pred)

tides <- tides %>%
  mutate(high_type = case_when(
    type == "High" & mean_height_obs <= 4.8 ~ "mortes_eaux",
    type == "High" & between(mean_height_obs, 4.8, 6.4) ~ "vives_eaux",
    type == "High" & mean_height_obs >= 6.4 ~ "submersion" # 6.9
  ))

table(tides$high_type)

# save 
where <-paste0("D:/Projets_Suzanne/Courlis/Data/1) data/Maree/", "tides_donnees_complete.csv")
write.csv(x=tides, file=where)
