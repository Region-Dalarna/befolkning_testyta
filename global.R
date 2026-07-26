## Globala inställningar för Shinyappen: befolkning_testyta

# Ladda nödvändiga paket
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(ggiraph)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(sf)
library(leaflet)

# Allmänna options - TRUE = visa inte R-felmeddelanden i appen, FALSE = visa felmeddelanden från R på webben
options(shiny.sanitize.errors = FALSE)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_shinyappar.R", encoding = "utf-8", echo = FALSE)

# ==== 1. Designtokens (speglar regiondalarna_ruf.css) ========================
rd_farg <- c(
  primary      = "#158daf",
  primary_dark = "#0f7090",
  bla_klar     = "#00b4e4",
  bla_ljus     = "#8edded",
  bla_blek     = "#b6f0fd",
  bla_mork     = "#0074a2",
  gra          = "#969696",
  gra_mork     = "#424242",
  gra_ljus     = "#e6e6e6",
  rod          = "#db3747",   # endast status/varning
  gron         = "#1a7f4e"    # endast status
)

# Färger för befolkningspyramidens kön (önskemål från verksamheten)
farg_kon <- c('Kvinnor' = "#e2a855", 'Män' = "#459079")

# ==== 2. Hämta data ==========================================================
# Riket ("00"), Dalarnas län ("20") samt samtliga Dalakommuner ("20xx")
con <- shiny_uppkoppling_las("oppna_data")

hamta_scb <- function(tabell, kodkol = "regionkod") {
  tbl(con, dbplyr::in_schema("scb", tabell)) %>%
    filter(!!sym(kodkol) == "00" | substr(!!sym(kodkol), 1L, 2L) == "20") %>%
    collect()
}

totfolkmangd       <- hamta_scb("totfolkmangd")
fodda              <- hamta_scb("fodda")                        # OBS: alder = moderns ålder, ingen könsuppdelning
doda               <- hamta_scb("doda")
invandring         <- hamta_scb("invandring")
utvandring         <- hamta_scb("utvandring")
inrikes_inflyttade <- hamta_scb("inrikes_inflyttade")
inrikes_utflyttade <- hamta_scb("inrikes_utflyttade")
inflytt_lansgrans  <- hamta_scb("inflyttningar_lansgrans_raw")
utflytt_lansgrans  <- hamta_scb("utflyttningar_lansgrans_raw")
folkmangd_modrar   <- hamta_scb("totfolkmangd_modrar")          # kvinnor i barnafödande ålder
prognos_utfall     <- hamta_scb("prognos_utfall")
prognos_historisk  <- hamta_scb("prognos_historisk")            # äldre prognosomgångar

# Flyttar per åldersgrupp (inrikes flytt + in-/utvandring) och födelseland
flyttar_aldersgrp <- hamta_scb("flyttar_aldersgrupper", kodkol = "region_kod") %>%
  rename(regionkod = region_kod, alder = `ålder`, kon = `kön`,
         tabellinnehall = `tabellinnehåll`, ar = `år`, varde = value)

bef_fodelseland <- hamta_scb("bef_fodelseland", kodkol = "region_kod") %>%
  rename(regionkod = region_kod, fodelseregion = `födelseregion`, kon = `kön`,
         tabellinnehall = `tabellinnehåll`, ar = `år`, varde = value)

# Flyttrelationer mellan kommuner (mikrodatabaserad) - kommun <-> relationskommun
flyttrelationer <- tbl(con, dbplyr::in_schema("mikro_db", "flyttrelationer")) %>%
  collect()

DBI::dbDisconnect(con)

# Prognostabellerna har svenska kolumnnamn - standardisera till samma som
# övriga tabeller. Normaliseringen är tolerant: den döper bara om kolumner
# som faktiskt finns, så den klarar att prognos_historisk skiljer sig något.
normalisera_prognos <- function(df) {
  namnkarta <- c(ar = "år", kon = "kön", alder = "ålder", varde = "folkmängd")
  for (nytt_namn in names(namnkarta)) {
    if (namnkarta[[nytt_namn]] %in% names(df)) {
      df <- rename(df, !!nytt_namn := !!sym(namnkarta[[nytt_namn]]))
    }
  }
  df %>% mutate(ar = as.integer(ar), prognos_ar = as.integer(prognos_ar))
}

prognos_utfall    <- normalisera_prognos(prognos_utfall)
prognos_historisk <- normalisera_prognos(prognos_historisk)

# Slå ihop till ETT prognosobjekt. Om samma prognosomgång skulle finnas i
# båda tabellerna har den aktuella tabellen företräde (skydd mot dubbletter).
prognos_utfall <- bind_rows(
  prognos_utfall,
  prognos_historisk %>%
    filter(!prognos_ar %in% unique(prognos_utfall$prognos_ar))
)
rm(prognos_historisk)

# Säkerställ att år alltid är heltal (kommer ibland som character från databasen)
stada_ar <- function(df) mutate(df, ar = as.integer(ar))

totfolkmangd       <- stada_ar(totfolkmangd)
fodda              <- stada_ar(fodda)
doda               <- stada_ar(doda)
invandring         <- stada_ar(invandring)
utvandring         <- stada_ar(utvandring)
inrikes_inflyttade <- stada_ar(inrikes_inflyttade)
inrikes_utflyttade <- stada_ar(inrikes_utflyttade)
inflytt_lansgrans  <- stada_ar(inflytt_lansgrans)
utflytt_lansgrans  <- stada_ar(utflytt_lansgrans)
folkmangd_modrar   <- stada_ar(folkmangd_modrar)
prognos_utfall     <- stada_ar(prognos_utfall)
flyttar_aldersgrp  <- stada_ar(flyttar_aldersgrp)
bef_fodelseland    <- stada_ar(bef_fodelseland)
flyttrelationer    <- stada_ar(flyttrelationer)

# Skydd mot dubbelräkning: varna om en tabell oväntat innehåller flera variabler
for (nm in c("totfolkmangd", "fodda", "doda", "invandring", "utvandring",
             "inrikes_inflyttade", "inrikes_utflyttade", "folkmangd_modrar")) {
  df_tmp <- get(nm)
  if ("variabel" %in% names(df_tmp) && n_distinct(df_tmp$variabel) > 1) {
    warning("Tabellen '", nm, "' innehåller flera variabler - kontrollera aggregeringen!")
  }
}
rm(df_tmp, nm)

# ==== 3. Hämta geodata (kommun- och länskarta) ===============================
con_geo <- shiny_uppkoppling_las("geodata")

kommuner_sf <- st_read(con_geo, layer = Id(schema = "karta", table = "kommun_scb"), quiet = TRUE) %>%
  st_simplify(dTolerance = 100) %>%     # förenkla i SWEREF (meter) för snabbare leaflet
  st_transform(4326)

lan_sf <- st_read(con_geo, layer = Id(schema = "karta", table = "lan_scb"), quiet = TRUE) %>%
  st_simplify(dTolerance = 200) %>%
  st_transform(4326)

DBI::dbDisconnect(con_geo)

# Kolumnnamnen i kartlagren varierar - hitta kod- och namnkolumn robust
hitta_kol <- function(sf_obj, monster) {
  namn <- names(sf_obj)
  traff <- namn[str_detect(str_to_lower(namn), monster)]
  if (length(traff) == 0) stop("Hittar ingen kolumn som matchar '", monster, "' i kartlagret.")
  traff[1]
}
kommuner_sf <- kommuner_sf %>%
  rename(kommunkod  = !!hitta_kol(kommuner_sf, "kod"),
         kommunnamn = !!hitta_kol(kommuner_sf, "namn")) %>%
  mutate(kommunkod = as.character(kommunkod))

lan_sf <- lan_sf %>%
  rename(lankod  = !!hitta_kol(lan_sf, "kod"),
         lannamn = !!hitta_kol(lan_sf, "namn")) %>%
  mutate(lankod  = as.character(lankod),
         # Kartlagrets länsnamn står i genitiv ("Dalarnas", "Stockholms") -
         # ta bort avslutande s så att det blir "Dalarna", "Stockholm" osv.
         lannamn = str_remove(str_trim(lannamn), "s$"))

# ==== 4. Hjälpfunktioner =====================================================

# TRUE för rader av typen "totalt ålder" / "totalt" så att de kan rensas bort
ar_totalrad <- function(x) str_detect(str_to_lower(as.character(x)), "tot")

# TRUE för "övriga"-kategorier (t.ex. datasetets egna "övriga födelseländer")
ar_ovrigrad <- function(x) str_detect(str_to_lower(as.character(x)), "övrig")

# "100+" -> 100, "0 år" -> 0, "20-24 år" -> 20 osv.
parse_alder <- function(x) suppressWarnings(parse_number(as.character(x)))

# Filtrerar på kön om kolumnen finns; "Totalt" = summera båda könen senare
filtrera_kon <- function(df, vald_kon = "Totalt") {
  if (!"kon" %in% names(df)) return(df)
  df <- filter(df, !ar_totalrad(kon))
  if (vald_kon != "Totalt") {
    forsta_bokstav <- str_to_lower(str_sub(vald_kon, 1, 1))   # "k" eller "m"
    df <- filter(df, str_starts(str_to_lower(kon), forsta_bokstav))
  }
  df
}

# Summerar till en rad per region och år (rensar ev. totalrader för ålder/kön)
summera_ar <- function(df, vald_kon = "Totalt") {
  df %>%
    filter(!ar_totalrad(alder)) %>%
    filtrera_kon(vald_kon) %>%
    group_by(regionkod, region, ar) %>%
    summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop")
}

# Svensk talformatering: 12 345,6
fmt <- function(x, dec = 0) {
  formatC(x, format = "f", digits = dec, big.mark = " ", decimal.mark = ",")
}

# Sekretessmaskning av låga tal: 1-2 (eller -1 till -2) visas som "färre än 3"
maska_tal <- function(x, visa_plus = FALSE) {
  ifelse(abs(x) > 0 & abs(x) < 3,
         "färre än 3",
         paste0(ifelse(x < 0, "\u2212", ifelse(visa_plus & x > 0, "+", "")),
                fmt(abs(x))))
}

# Grupperar ettårsåldrar i 10-årsklasser (0-9 ... 80-89, 90+)
gor_aldersklass <- function(alder_num) {
  start <- pmin(floor(alder_num / 10) * 10, 90)
  factor(if_else(start >= 90, "90+", paste0(start, "-", start + 9)),
         levels = c(paste0(seq(0, 80, 10), "-", seq(9, 89, 10)), "90+"))
}

# ==== 5. Förberäknade data ===================================================

# Summerad fruktsamhet (TFR) per region och år.
# Antagande: fodda$alder avser MODERNS ålder och totfolkmangd_modrar är
# medelfolkmängd kvinnor per ålder - verifiera mot källtabellerna i SCB.
tfr_data <- fodda %>%
  filter(!ar_totalrad(alder)) %>%
  mutate(alder_num = parse_alder(alder)) %>%
  select(regionkod, region, ar, alder_num, antal_fodda = varde) %>%
  inner_join(
    folkmangd_modrar %>%
      filter(!ar_totalrad(alder)) %>%
      mutate(alder_num = parse_alder(alder)) %>%
      select(regionkod, ar, alder_num, antal_kvinnor = varde),
    by = c("regionkod", "ar", "alder_num")
  ) %>%
  filter(antal_kvinnor > 0) %>%
  group_by(regionkod, region, ar) %>%
  summarise(tfr = sum(antal_fodda / antal_kvinnor, na.rm = TRUE), .groups = "drop")

# ==== 6. Gemensamma värden för UI ============================================
ar_min <- min(totfolkmangd$ar, na.rm = TRUE)
ar_max <- max(totfolkmangd$ar, na.rm = TRUE)

# Regionval: Dalarnas län, Riket och därefter Dalakommunerna i bokstavsordning
kommunlista <- totfolkmangd %>%
  distinct(regionkod, region) %>%
  filter(nchar(regionkod) == 4) %>%
  arrange(region)
regionval <- c("Dalarnas län" = "20", "Riket" = "00",
               setNames(kommunlista$regionkod, kommunlista$region))

prognosar_val <- sort(unique(prognos_utfall$prognos_ar), decreasing = TRUE)

# Åldersgrupper för flikens "Flyttnetto per åldersgrupp" (sorterade på startålder)
aldersgrupp_val <- flyttar_aldersgrp %>%
  filter(!ar_totalrad(alder)) %>%
  distinct(alder) %>%
  arrange(parse_alder(alder)) %>%
  pull(alder)
# Förvalda grupper: barn och unga (prioritet A2 i kommundialogen)
aldersgrupp_barn <- aldersgrupp_val[parse_alder(aldersgrupp_val) < 20]

# Val för flyttrelationsfliken (mikro_db.flyttrelationer) - endast Dalakommuner
rel_kommun_val <- flyttrelationer %>%
  filter(substr(kommun, 1, 2) == "20") %>%
  distinct(kommun, kommun_namn) %>%
  arrange(kommun_namn) %>%
  { setNames(.$kommun, .$kommun_namn) }
rel_alder_val <- flyttrelationer %>%
  distinct(alder_grp) %>%
  mutate(sort_tal = if_else(str_starts(str_trim(alder_grp), "-"),
                            -Inf,                       # "- 5 år" först
                            parse_alder(alder_grp))) %>%
  arrange(sort_tal) %>%
  pull(alder_grp)
rel_ar_min <- min(flyttrelationer$ar, na.rm = TRUE)
rel_ar_max <- max(flyttrelationer$ar, na.rm = TRUE)

# Årsval för "alla åldersgrupper ett år" (senaste året förvalt)
flyttgrp_ar_val <- sort(unique(flyttar_aldersgrp$ar), decreasing = TRUE)

# Utfallsår där det finns både prognos och utfall (för avvikelse per åldersklass)
avvikelse_ar_val <- sort(intersect(unique(prognos_utfall$ar),
                                   unique(totfolkmangd$ar)),
                         decreasing = TRUE)

# ==== 7. ggplot-tema och girafe-inställningar ================================
# OBS: kräver att Poppins finns installerat på servern för korrekta textmått i
# SVG:n. Saknas typsnittet renderar webbläsaren ändå Poppins via CSS:en, men
# byt till base_family = "" om ni får varningar vid rendering.
theme_rd <- function(bas = 11.5) {
  theme_minimal(base_size = bas, base_family = "Poppins") +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major   = element_line(color = "#e6e6e6", linewidth = 0.4),
      axis.title         = element_text(color = "#6c757d", size = rel(0.9)),
      axis.text          = element_text(color = "#212529"),
      legend.position    = "bottom",
      legend.title       = element_blank(),
      legend.text        = element_text(size = rel(0.9)),
      plot.margin        = margin(8, 12, 4, 4)
    )
}

skapa_girafe <- function(p, hojd = 4.5, bredd = 9) {
  girafe(
    ggobj = p,
    width_svg = bredd,
    height_svg = hojd,
    options = list(
      opts_tooltip(css = paste0(
        "background:#fff;color:#212529;border:1px solid #e3e8ed;",
        "border-radius:6px;padding:8px 10px;",
        "font-family:Poppins,Arial,sans-serif;font-size:0.85rem;",
        "box-shadow:0 2px 6px rgba(0,0,0,0.12);"
      )),
      opts_hover(css = "filter:brightness(1.08);"),
      opts_hover_inv(css = "opacity:0.4;"),
      opts_selection(type = "none"),
      opts_toolbar(saveaspng = TRUE, hidden = c("lasso_select", "lasso_deselect"))
    )
  )
}
