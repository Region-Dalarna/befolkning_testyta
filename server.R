shinyServer(function(input, output, session) {

  # ==== Gemensamma reaktiva uttryck ==========================================

  ar_span <- reactive(seq(input$ar_intervall[1], input$ar_intervall[2]))

  regionnamn     <- reactive(names(regionval)[match(input$region, regionval)])
  rel_kommunnamn <- reactive(names(rel_kommun_val)[match(input$rel_kommun, rel_kommun_val)])

  # Dynamiska rubriker: vald region läggs till där den inte framgår av
  # teckenförklaringen i diagrammet
  rubriker <- list(
    rubrik_folkmangd        = 'Folkmängdens utveckling',
    rubrik_komponenter      = 'Förändringens komponenter',
    rubrik_netton           = 'Netton över tid',
    rubrik_inrikes_flytt    = 'Flyttningar inom och utanför länet',
    rubrik_flyttnetto_alder = 'Inrikes flyttnetto per ålder',
    rubrik_netto_aldersgrupp = 'Flyttnetto per åldersgrupp över tid',
    rubrik_utrikes          = 'Invandring och utvandring',
    rubrik_utrikes_alder    = 'Utrikes flyttnetto per ålder',
    rubrik_fodelseland_tid  = 'Utrikes födda efter födelseland',
    rubrik_fodda_doda       = 'Födda och döda',
    rubrik_prognos          = 'Prognoser mot faktisk utveckling',
    rubrik_avvikelse        = 'Prognosavvikelse över tid'
  )
  for (rubrik_id in names(rubriker)) {
    local({
      id_lokal <- rubrik_id
      output[[id_lokal]] <- renderText(paste0(rubriker[[id_lokal]], ' - ', regionnamn()))
    })
  }
  # Fruktsamheten visar alltid även riket - speglas i rubriken
  output$rubrik_fruktsamhet <- renderText({
    paste0('Summerad fruktsamhet - ', regionnamn(),
           if (input$region != '00') ' och riket' else '')
  })

  # Komponenterna i befolkningsförändringen. Alltid "Totalt" eftersom
  # födda saknar könsuppdelning - annars blir komponenterna ojämförbara.
  komponenter <- reactive({
    bind_rows(
      summera_ar(fodda)              %>% mutate(komponent = 'Födda',               riktning =  1),
      summera_ar(doda)               %>% mutate(komponent = 'Döda',                riktning = -1),
      summera_ar(invandring)         %>% mutate(komponent = 'Invandring',          riktning =  1),
      summera_ar(utvandring)         %>% mutate(komponent = 'Utvandring',          riktning = -1),
      summera_ar(inrikes_inflyttade) %>% mutate(komponent = 'Inrikes inflyttning', riktning =  1),
      summera_ar(inrikes_utflyttade) %>% mutate(komponent = 'Inrikes utflyttning', riktning = -1)
    ) %>%
      filter(regionkod == input$region, ar %in% ar_span()) %>%
      mutate(
        bidrag = varde * riktning,
        komponent = factor(komponent,
                           levels = c('Födda', 'Döda', 'Invandring', 'Utvandring',
                                      'Inrikes inflyttning', 'Inrikes utflyttning'))
      )
  })

  prognos_summerad <- reactive({
    req(input$valda_prognoser)
    prognos_utfall %>%
      filter(regionkod == input$region,
             prognos_ar %in% as.integer(input$valda_prognoser),
             !ar_totalrad(alder)) %>%
      filtrera_kon(input$kon) %>%
      group_by(prognos_ar, ar) %>%
      summarise(varde = sum(varde, na.rm = TRUE), .groups = 'drop') %>%
      mutate(prognos = paste('Prognos', prognos_ar))
  })

  # ==== Återanvändbara diagramfunktioner =====================================

  # Två linjeserier (t.ex. födda/döda eller invandring/utvandring) + nettostaplar
  fig_par_netto <- function(df_pos, df_neg, namn_pos, namn_neg, namn_netto) {
    df <- bind_rows(
      summera_ar(df_pos, input$kon) %>% mutate(serie = namn_pos),
      summera_ar(df_neg, input$kon) %>% mutate(serie = namn_neg)
    ) %>%
      filter(regionkod == input$region, ar %in% ar_span()) %>%
      mutate(serie = factor(serie, levels = c(namn_pos, namn_neg)))
    validate(need(nrow(df) > 0, 'Ingen data för valt urval.'))

    netto <- df %>%
      select(ar, serie, varde) %>%
      pivot_wider(names_from = serie, values_from = varde) %>%
      mutate(netto = .data[[namn_pos]] - .data[[namn_neg]])

    p <- ggplot() +
      geom_col_interactive(
        data = netto,
        aes(ar, netto,
            tooltip = paste0(namn_netto, ' ', ar, ': ', fmt(netto)),
            data_id = paste('netto', ar)),
        fill = rd_farg[['bla_blek']]) +
      geom_hline(yintercept = 0, color = rd_farg[['gra']], linewidth = 0.4) +
      geom_line(data = df, aes(ar, varde, color = serie), linewidth = 1) +
      geom_point_interactive(
        data = df,
        aes(ar, varde, color = serie,
            tooltip = paste0(serie, ' ', ar, ': ', fmt(varde)),
            data_id = paste(serie, ar)),
        size = 1.8) +
      scale_color_manual(values = unname(rd_farg[c('primary', 'gra_mork')])) +
      scale_y_continuous(labels = fmt) +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      labs(x = NULL, y = 'Antal personer') +
      theme_rd()

    skapa_girafe(p, hojd = 4.6)
  }

  # Flyttnetto per ettårsålder (genomsnitt per år i intervallet)
  fig_netto_alder <- function(df_in, df_ut) {
    prep <- function(df) {
      d <- df %>%
        filter(regionkod == input$region, ar %in% ar_span(), !ar_totalrad(alder)) %>%
        filtrera_kon(input$kon) %>%
        mutate(alder_num = parse_alder(alder))
      n_ar <- max(n_distinct(d$ar), 1)
      d %>%
        group_by(alder_num) %>%
        summarise(antal = sum(varde, na.rm = TRUE) / n_ar, .groups = 'drop')
    }
    df <- full_join(prep(df_in) %>% rename(inflode = antal),
                    prep(df_ut) %>% rename(utflode = antal),
                    by = 'alder_num') %>%
      mutate(across(c(inflode, utflode), ~replace_na(., 0)),
             netto = inflode - utflode,
             tooltip = paste0(alder_num, ' år',
                              '<br>Netto: ', fmt(netto),
                              '<br>Inflöde: ', fmt(inflode),
                              '<br>Utflöde: ', fmt(utflode)))
    validate(need(nrow(df) > 0, 'Ingen data för valt urval.'))

    p <- ggplot(df, aes(alder_num, netto, fill = netto >= 0)) +
      geom_col_interactive(aes(tooltip = tooltip, data_id = alder_num), width = 1) +
      geom_hline(yintercept = 0, color = rd_farg[['gra_mork']], linewidth = 0.3) +
      scale_fill_manual(values = c(`TRUE`  = rd_farg[['primary']],
                                   `FALSE` = rd_farg[['rod']]),
                        guide = 'none') +
      scale_y_continuous(labels = fmt) +
      labs(x = 'Ålder', y = 'Flyttnetto, genomsnitt per år') +
      theme_rd()

    skapa_girafe(p, hojd = 4.6)
  }

  # ==== Flik 1: Översikt =====================================================

  output$kpi_rad <- renderUI({
    folkm_df <- summera_ar(totfolkmangd, input$kon) %>%
      filter(regionkod == input$region)
    req(nrow(folkm_df) > 0)

    ar_sista <- max(folkm_df$ar[folkm_df$ar <= input$ar_intervall[2]])
    folkm    <- folkm_df$varde[folkm_df$ar == ar_sista]
    folkm_fg <- folkm_df$varde[folkm_df$ar == ar_sista - 1]
    delta    <- if (length(folkm_fg) == 1) folkm - folkm_fg else NA_real_

    # Komponent-KPI:er beräknas alltid för samtliga (födda saknar kön)
    arsvarde <- function(df) {
      d <- summera_ar(df) %>% filter(regionkod == input$region, ar == ar_sista)
      if (nrow(d) == 1) d$varde else NA_real_
    }
    fodelsenetto <- arsvarde(fodda) - arsvarde(doda)
    inrikes_netto <- arsvarde(inrikes_inflyttade) - arsvarde(inrikes_utflyttade)
    utrikes_netto <- arsvarde(invandring) - arsvarde(utvandring)

    signerad <- function(x) {
      if (is.na(x)) return('–')
      paste0(if (x >= 0) '+' else '\u2212', fmt(abs(x)))
    }
    kpi <- function(rubrik, varde, undertext = NULL, klass = NULL) {
      div(class = 'rd-kpi',
          div(class = 'rd-kpi__label', rubrik),
          div(class = 'rd-kpi__value', varde),
          if (!is.null(undertext))
            div(class = paste('rd-kpi__delta', klass), undertext))
    }

    div(class = 'rd-kpi-row rd-kpi-row--4',
        kpi(paste('Folkmängd', ar_sista), fmt(folkm),
            paste0(signerad(delta), ' sedan ', ar_sista - 1),
            if (!is.na(delta) && delta >= 0) 'up' else 'down'),
        kpi(paste('Födelsenetto', ar_sista), signerad(fodelsenetto),
            'födda minus döda'),
        kpi(paste('Inrikes flyttnetto', ar_sista), signerad(inrikes_netto),
            'inrikes in- minus utflyttning'),
        kpi(paste('Utrikes flyttnetto', ar_sista), signerad(utrikes_netto),
            'invandring minus utvandring'))
  })

  output$fig_folkmangd <- renderGirafe({
    if (isTRUE(input$jamfor_riket)) {
      visa_regioner <- unique(c(input$region, '20', '00'))
      df <- summera_ar(totfolkmangd, input$kon) %>%
        filter(regionkod %in% visa_regioner, ar %in% ar_span()) %>%
        group_by(region) %>%
        arrange(ar, .by_group = TRUE) %>%
        mutate(visat = 100 * varde / first(varde)) %>%
        ungroup() %>%
        mutate(tooltip = paste0(region, ' ', ar,
                                '<br>Index: ', fmt(visat, 1),
                                '<br>Folkmängd: ', fmt(varde)))
      ytitel <- paste0('Index (', input$ar_intervall[1], ' = 100)')
    } else {
      df <- summera_ar(totfolkmangd, input$kon) %>%
        filter(regionkod == input$region, ar %in% ar_span()) %>%
        mutate(visat = varde,
               tooltip = paste0(region, ' ', ar, '<br>Folkmängd: ', fmt(varde)))
      ytitel <- 'Folkmängd'
    }
    validate(need(nrow(df) > 0, 'Ingen data för valt urval.'))

    p <- ggplot(df, aes(ar, visat, color = region, group = region)) +
      geom_line(linewidth = 1) +
      geom_point_interactive(aes(tooltip = tooltip, data_id = paste(region, ar)),
                             size = 1.8) +
      scale_color_manual(values = unname(rd_farg[c('primary', 'bla_klar', 'gra_mork')])) +
      scale_y_continuous(labels = fmt) +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      labs(x = NULL, y = ytitel) +
      theme_rd() +
      (if (!isTRUE(input$jamfor_riket)) theme(legend.position = 'none') else theme())

    skapa_girafe(p, hojd = 4.6)
  })

  output$rubrik_pyramid <- renderText({
    paste0('Befolkningspyramid - ', regionnamn(), ' ', input$pyramid_ar)
  })

  output$fig_pyramid <- renderGirafe({
    df <- totfolkmangd %>%
      filter(regionkod == input$region, ar == input$pyramid_ar,
             !ar_totalrad(alder), !ar_totalrad(kon)) %>%
      mutate(alder_num = parse_alder(alder),
             kon_grupp = if_else(str_starts(str_to_lower(kon), 'm'), 'Män', 'Kvinnor')) %>%
      group_by(alder_num, kon_grupp) %>%
      summarise(varde = sum(varde, na.rm = TRUE), .groups = 'drop') %>%
      mutate(visat = if_else(kon_grupp == 'Män', -varde, varde),
             tooltip = paste0(kon_grupp, ', ', alder_num, ' år<br>Antal: ', fmt(varde)))
    validate(need(nrow(df) > 0, 'Ingen data för valt år.'))

    brytpunkter <- scales::pretty_breaks(5)(c(-max(df$varde), max(df$varde)))

    p <- ggplot(df, aes(x = visat, y = alder_num, fill = kon_grupp)) +
      geom_col_interactive(aes(tooltip = tooltip, data_id = paste(kon_grupp, alder_num)),
                           orientation = 'y', width = 1) +
      geom_vline(xintercept = 0, color = '#ffffff', linewidth = 0.3) +
      scale_fill_manual(values = farg_kon) +
      scale_x_continuous(breaks = brytpunkter, labels = function(x) fmt(abs(x))) +
      labs(x = 'Antal', y = 'Ålder') +
      theme_rd()

    skapa_girafe(p, hojd = 5.6)
  })

  # ==== Flik 2: Befolkningsförändring ========================================

  output$fig_komponenter <- renderGirafe({
    df <- komponenter()
    validate(need(nrow(df) > 0, 'Ingen data för valt urval.'))
    tot <- df %>% group_by(ar) %>% summarise(netto = sum(bidrag), .groups = 'drop')

    pal_komp <- setNames(
      unname(rd_farg[c('primary', 'gra', 'bla_klar', 'bla_ljus', 'bla_mork', 'bla_blek')]),
      levels(df$komponent)
    )

    p <- ggplot() +
      geom_col_interactive(
        data = df,
        aes(ar, bidrag, fill = komponent,
            tooltip = paste0(komponent, ' ', ar, '<br>Antal: ', fmt(varde)),
            data_id = paste(komponent, ar))) +
      geom_hline(yintercept = 0, color = rd_farg[['gra_mork']], linewidth = 0.3) +
      geom_line(data = tot, aes(ar, netto, group = 1),
                color = rd_farg[['gra_mork']], linewidth = 1) +
      geom_point_interactive(
        data = tot,
        aes(ar, netto,
            tooltip = paste0('Total förändring ', ar, ': ', fmt(netto)),
            data_id = paste('tot', ar)),
        color = rd_farg[['gra_mork']], size = 1.8) +
      scale_fill_manual(values = pal_komp) +
      scale_y_continuous(labels = fmt) +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      labs(x = NULL, y = 'Antal personer') +
      theme_rd()

    skapa_girafe(p, hojd = 5)
  })

  output$fig_netton <- renderGirafe({
    df <- komponenter() %>%
      mutate(grupp = case_when(
        komponent %in% c('Födda', 'Döda')             ~ 'Födelsenetto',
        komponent %in% c('Invandring', 'Utvandring')  ~ 'Utrikes flyttnetto',
        TRUE                                          ~ 'Inrikes flyttnetto')) %>%
      group_by(ar, grupp) %>%
      summarise(netto = sum(bidrag), .groups = 'drop')
    validate(need(nrow(df) > 0, 'Ingen data för valt urval.'))

    p <- ggplot(df, aes(ar, netto, color = grupp)) +
      geom_hline(yintercept = 0, color = rd_farg[['gra']], linewidth = 0.4) +
      geom_line(linewidth = 1) +
      geom_point_interactive(
        aes(tooltip = paste0(grupp, ' ', ar, ': ', fmt(netto)),
            data_id = paste(grupp, ar)),
        size = 1.8) +
      scale_color_manual(values = unname(rd_farg[c('primary', 'bla_klar', 'gra_mork')])) +
      scale_y_continuous(labels = fmt) +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      labs(x = NULL, y = 'Antal personer') +
      theme_rd()

    skapa_girafe(p, hojd = 4.4)
  })

  # ==== Flik 3: Inrikes flyttningar ==========================================

  output$fig_inrikes_flytt <- renderGirafe({
    prep <- function(df, riktning, tecken) {
      df %>%
        filter(regionkod == input$region, ar %in% ar_span(), !ar_totalrad(alder)) %>%
        filtrera_kon(input$kon) %>%
        group_by(ar) %>%
        summarise(`inom länet`  = sum(inom_lan,   na.rm = TRUE),
                  `övriga län`  = sum(ovriga_lan, na.rm = TRUE),
                  .groups = 'drop') %>%
        pivot_longer(-ar, names_to = 'omrade', values_to = 'antal') %>%
        mutate(riktning = riktning,
               visat    = antal * tecken,
               kategori = paste(riktning, omrade))
    }
    df <- bind_rows(prep(inflytt_lansgrans, 'Inflyttning', 1),
                    prep(utflytt_lansgrans, 'Utflyttning', -1))
    validate(need(nrow(df) > 0 && sum(abs(df$antal), na.rm = TRUE) > 0,
                  'Inget underlag för flyttningar inom/utanför länet för vald region.'))

    df <- df %>%
      mutate(kategori = factor(kategori,
                               levels = c('Inflyttning inom länet', 'Inflyttning övriga län',
                                          'Utflyttning inom länet', 'Utflyttning övriga län')))
    netto <- df %>% group_by(ar) %>% summarise(netto = sum(visat), .groups = 'drop')

    pal_flytt <- setNames(
      unname(rd_farg[c('primary', 'bla_mork', 'bla_ljus', 'bla_blek')]),
      levels(df$kategori)
    )

    p <- ggplot() +
      geom_col_interactive(
        data = df,
        aes(ar, visat, fill = kategori,
            tooltip = paste0(kategori, ' ', ar, '<br>Antal: ', fmt(antal)),
            data_id = paste(kategori, ar))) +
      geom_hline(yintercept = 0, color = rd_farg[['gra_mork']], linewidth = 0.3) +
      geom_line(data = netto, aes(ar, netto, group = 1),
                color = rd_farg[['gra_mork']], linewidth = 1) +
      geom_point_interactive(
        data = netto,
        aes(ar, netto,
            tooltip = paste0('Inrikes flyttnetto ', ar, ': ', fmt(netto)),
            data_id = paste('netto', ar)),
        color = rd_farg[['gra_mork']], size = 1.8) +
      scale_fill_manual(values = pal_flytt) +
      scale_y_continuous(labels = function(x) fmt(abs(x))) +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      labs(x = NULL, y = 'Antal personer (utflyttning nedåt)') +
      theme_rd()

    skapa_girafe(p, hojd = 5)
  })

  output$fig_flyttnetto_alder <- renderGirafe({
    fig_netto_alder(inrikes_inflyttade, inrikes_utflyttade)
  })

  output$fig_netto_aldersgrupp <- renderGirafe({
    req(input$valda_aldersgrupper)
    # tabellinnehall förväntas innehålla "inflytt"/"utflytt" för de inrikes
    # serierna och "invandr"/"utvandr" för de utrikes - verifiera mot tabellen!
    df <- flyttar_aldersgrp %>%
      filter(regionkod == input$region, ar %in% ar_span(),
             alder %in% input$valda_aldersgrupper) %>%
      filtrera_kon(input$kon) %>%
      mutate(tecken = case_when(
        str_detect(str_to_lower(tabellinnehall), 'inflytt') ~  1,
        str_detect(str_to_lower(tabellinnehall), 'utflytt') ~ -1,
        TRUE ~ NA_real_)) %>%
      filter(!is.na(tecken)) %>%
      group_by(ar, alder) %>%
      summarise(netto = sum(varde * tecken, na.rm = TRUE), .groups = 'drop') %>%
      mutate(alder = factor(alder, levels = aldersgrupp_val))
    validate(need(nrow(df) > 0, 'Ingen data för valt urval.'))

    pal <- setNames(
      rep(unname(rd_farg[c('primary', 'bla_klar', 'bla_mork', 'bla_ljus', 'gra_mork', 'gra')]),
          length.out = nlevels(droplevels(df$alder))),
      levels(droplevels(df$alder))
    )

    p <- ggplot(df, aes(ar, netto, color = alder)) +
      geom_hline(yintercept = 0, color = rd_farg[['gra']], linewidth = 0.4) +
      geom_line(linewidth = 1) +
      geom_point_interactive(
        aes(tooltip = paste0(alder, ', ', ar, '<br>Flyttnetto: ', fmt(netto)),
            data_id = paste(alder, ar)),
        size = 1.8) +
      scale_color_manual(values = pal) +
      scale_y_continuous(labels = fmt) +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      labs(x = NULL, y = 'Inrikes flyttnetto') +
      theme_rd()

    skapa_girafe(p, hojd = 4.6)
  })

  output$rubrik_aldersgrupp_ar <- renderText({
    paste0('Flyttnetto per åldersgrupp - ', regionnamn(), ' ', input$flyttgrp_ar)
  })

  output$fig_aldersgrupp_ar <- renderGirafe({
    req(input$flyttgrp_ar)
    df <- flyttar_aldersgrp %>%
      filter(regionkod == input$region, ar == as.integer(input$flyttgrp_ar),
             !ar_totalrad(alder)) %>%
      filtrera_kon(input$kon) %>%
      mutate(tecken = case_when(
        str_detect(str_to_lower(tabellinnehall), 'inflytt') ~  1,
        str_detect(str_to_lower(tabellinnehall), 'utflytt') ~ -1,
        TRUE ~ NA_real_)) %>%
      filter(!is.na(tecken)) %>%
      group_by(alder) %>%
      summarise(inflode = sum(varde[tecken == 1],  na.rm = TRUE),
                utflode = sum(varde[tecken == -1], na.rm = TRUE),
                .groups = 'drop') %>%
      mutate(netto = inflode - utflode,
             alder = factor(alder, levels = aldersgrupp_val),
             tooltip = paste0(alder,
                              '<br>Netto: ', fmt(netto),
                              '<br>Inflyttade: ', fmt(inflode),
                              '<br>Utflyttade: ', fmt(utflode)))
    validate(need(nrow(df) > 0, 'Ingen data för valt år.'))

    p <- ggplot(df, aes(alder, netto, fill = netto >= 0)) +
      geom_col_interactive(aes(tooltip = tooltip, data_id = alder), width = 0.75) +
      geom_hline(yintercept = 0, color = rd_farg[['gra_mork']], linewidth = 0.3) +
      scale_fill_manual(values = c(`TRUE`  = rd_farg[['primary']],
                                   `FALSE` = rd_farg[['rod']]),
                        guide = 'none') +
      scale_y_continuous(labels = fmt) +
      labs(x = NULL, y = 'Inrikes flyttnetto') +
      theme_rd() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    skapa_girafe(p, hojd = 4.6)
  })

  # ==== Flik 4: Flyttrelationer ==============================================

  output$rubrik_flyttrel <- renderText({
    paste0('Flyttmönster till och från ', rel_kommunnamn())
  })
  output$rubrik_karta_kommun <- renderText({
    paste0('Relationskommuner - ', rel_kommunnamn())
  })
  output$rubrik_karta_lan <- renderText({
    paste0('Relationslän - ', rel_kommunnamn())
  })
  output$rubrik_flyttrel_tabell <- renderText({
    paste0('Största flyttrelationerna - ', rel_kommunnamn())
  })

  flyttrel_urval <- reactive({
    req(input$rel_kommun, input$rel_alder, input$rel_ar)
    df <- flyttrelationer %>%
      filter(kommun == input$rel_kommun,
             ar >= input$rel_ar[1], ar <= input$rel_ar[2],
             alder_grp %in% input$rel_alder) %>%
      mutate(typ = case_when(
        str_starts(str_to_lower(flytt_typ), 'in') ~ 'Inflyttning',
        str_starts(str_to_lower(flytt_typ), 'ut') ~ 'Utflyttning',
        TRUE ~ NA_character_)) %>%
      filter(!is.na(typ)) %>%
      group_by(relationskommun, relationskommun_namn, typ) %>%
      summarise(antal = sum(antal, na.rm = TRUE), .groups = 'drop') %>%
      pivot_wider(names_from = typ, values_from = antal, values_fill = 0)

    if (!'Inflyttning' %in% names(df)) df$Inflyttning <- 0
    if (!'Utflyttning' %in% names(df)) df$Utflyttning <- 0
    df %>% mutate(Flyttnetto = Inflyttning - Utflyttning)
  })

  # Länsnivå: aggregera relationskommunerna på länskod (två första siffrorna)
  flyttrel_lan <- reactive({
    flyttrel_urval() %>%
      mutate(lankod = substr(relationskommun, 1, 2)) %>%
      group_by(lankod) %>%
      summarise(across(c(Inflyttning, Utflyttning, Flyttnetto), sum),
                .groups = 'drop')
  })

  # ---- Kartorna byggs i två steg så att zoomläget bevaras: ----
  # 1) renderLeaflet ritar en statisk baskarta EN gång (bakgrund, vy, hemknapp)
  # 2) en observer uppdaterar polygoner och teckenförklaring via leafletProxy
  #    när urvalet eller vald flyttyp ändras - utan att röra zoom/panorering

  start_vy <- list(lng = 15.5, lat = 62, zoom = 4)

  bas_karta <- function() {
    leaflet(options = leafletOptions(attributionControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = start_vy$lng, lat = start_vy$lat, zoom = start_vy$zoom) %>%
      addEasyButton(easyButton(
        icon    = '<span style="font-size:17px;line-height:26px;">&#8962;</span>',
        title   = 'Återgå till utgångsläget',
        onClick = htmlwidgets::JS(sprintf(
          'function(btn, map){ map.setView([%f, %f], %d); }',
          start_vy$lat, start_vy$lng, start_vy$zoom))))
  }

  output$karta_flyttrel_kommun <- renderLeaflet(bas_karta())
  output$karta_flyttrel_lan    <- renderLeaflet(bas_karta())

  # Rita baskartorna direkt vid start även om fliken inte är öppnad ännu,
  # annars går proxy-uppdateringarna förlorade
  outputOptions(output, 'karta_flyttrel_kommun', suspendWhenHidden = FALSE)
  outputOptions(output, 'karta_flyttrel_lan',    suspendWhenHidden = FALSE)

  # Gemensam uppdaterare för kommun- och länskartan.
  # geo_joined måste ha kolumnerna Inflyttning/Utflyttning/Flyttnetto + visningsnamn
  uppdatera_flyttkarta <- function(karta_id, geo_joined, markera_vald = FALSE) {
    namn_vald <- rel_kommunnamn()
    vals <- geo_joined[[input$rel_typ]]

    if (input$rel_typ == 'Flyttnetto') {
      maxabs <- max(abs(vals), na.rm = TRUE)
      if (!is.finite(maxabs) || maxabs == 0) maxabs <- 1
      pal  <- colorNumeric(c(rd_farg[['rod']], '#f7f9fa', rd_farg[['primary']]),
                           domain = c(-maxabs, maxabs))
      fyll <- pal(vals)
      legend_pal  <- pal
      legend_vals <- c(-maxabs, maxabs)
      legend_fmt  <- labelFormat(big.mark = ' ')
    } else {
      # kvadratrotsskala så att de största relationerna inte dränker alla andra
      maxv <- max(vals, na.rm = TRUE)
      if (!is.finite(maxv) || maxv == 0) maxv <- 1
      pal  <- colorNumeric(c('#eaf7fb', rd_farg[['bla_mork']]), domain = c(0, sqrt(maxv)))
      fyll <- pal(sqrt(vals))
      legend_pal  <- pal
      legend_vals <- c(0, sqrt(maxv))
      legend_fmt  <- labelFormat(big.mark = ' ', transform = function(x) round(x^2))
    }

    etikett <- sprintf(
      paste0('<strong>%s</strong>',
             '<br>Inflyttning till %s härifrån: %s',
             '<br>Utflyttning från %s hit: %s',
             '<br>Flyttnetto för %s: %s'),
      geo_joined$visningsnamn,
      namn_vald, maska_tal(geo_joined$Inflyttning),
      namn_vald, maska_tal(geo_joined$Utflyttning),
      namn_vald, maska_tal(geo_joined$Flyttnetto, visa_plus = TRUE)
    ) %>% lapply(htmltools::HTML)

    proxy <- leafletProxy(karta_id) %>%
      clearGroup('flyttdata') %>%
      clearGroup('overlagg') %>%
      removeControl('teckenforklaring') %>%
      addPolygons(data = geo_joined, group = 'flyttdata',
                  fillColor = fyll, fillOpacity = 0.85,
                  color = '#ffffff', weight = 0.5,
                  label = etikett,
                  highlightOptions = highlightOptions(
                    weight = 1.5, color = rd_farg[['gra_mork']], bringToFront = TRUE)) %>%
      addLegend(layerId = 'teckenforklaring', position = 'bottomright',
                pal = legend_pal, values = legend_vals,
                title = input$rel_typ, labFormat = legend_fmt, opacity = 0.85)

    if (markera_vald) {
      proxy %>%
        addPolygons(data = lan_sf, group = 'overlagg', fill = FALSE,
                    color = rd_farg[['gra']], weight = 0.8) %>%
        addPolygons(data = filter(kommuner_sf, kommunkod == input$rel_kommun),
                    group = 'overlagg', fill = FALSE,
                    color = rd_farg[['gra_mork']], weight = 2.5)
    }
    invisible(NULL)
  }

  observe({
    df <- flyttrel_urval()
    req(nrow(df) > 0)
    geo <- kommuner_sf %>%
      left_join(df, by = c('kommunkod' = 'relationskommun')) %>%
      mutate(across(c(Inflyttning, Utflyttning, Flyttnetto), ~replace_na(., 0)),
             visningsnamn = kommunnamn)
    uppdatera_flyttkarta('karta_flyttrel_kommun', geo, markera_vald = TRUE)
  })

  observe({
    df <- flyttrel_lan()
    req(nrow(df) > 0)
    geo <- lan_sf %>%
      left_join(df, by = 'lankod') %>%
      mutate(across(c(Inflyttning, Utflyttning, Flyttnetto), ~replace_na(., 0)),
             visningsnamn = lannamn)
    uppdatera_flyttkarta('karta_flyttrel_lan', geo, markera_vald = FALSE)
  })

  output$tab_flyttrel <- renderDT({
    df <- flyttrel_urval()
    validate(need(nrow(df) > 0, 'Ingen data för valt urval.'))

    sma   <- df %>% filter(Inflyttning < 3, Utflyttning < 3)
    stora <- df %>%
      filter(!(Inflyttning < 3 & Utflyttning < 3)) %>%
      arrange(desc(Inflyttning + Utflyttning))

    tabell <- stora %>%
      transmute(Kommun      = relationskommun_namn,
                Inflyttning = maska_tal(Inflyttning),
                Utflyttning = maska_tal(Utflyttning),
                Flyttnetto  = maska_tal(Flyttnetto, visa_plus = TRUE))

    if (nrow(sma) > 0) {
      tabell <- bind_rows(
        tabell,
        tibble(Kommun      = paste0('Övriga kommuner (', nrow(sma),
                                    ' st med färre än 3 flyttar vardera)'),
               Inflyttning = fmt(sum(sma$Inflyttning)),
               Utflyttning = fmt(sum(sma$Utflyttning)),
               Flyttnetto  = maska_tal(sum(sma$Flyttnetto), visa_plus = TRUE))
      )
    }

    datatable(tabell, rownames = FALSE,
              options = list(pageLength = 12, dom = 'tip', ordering = FALSE))
  })

  # ==== Flik 5: In- och utvandring ===========================================

  output$fig_utrikes <- renderGirafe({
    fig_par_netto(invandring, utvandring,
                  'Invandring', 'Utvandring', 'Utrikes flyttnetto')
  })

  output$fig_utrikes_alder <- renderGirafe({
    fig_netto_alder(invandring, utvandring)
  })

  # ==== Flik 6: Födelseland ==================================================

  output$fig_fodelseland_tid <- renderGirafe({
    bas <- bef_fodelseland %>%
      filter(regionkod == input$region, ar %in% ar_span(),
             !ar_totalrad(fodelseregion)) %>%
      filtrera_kon(input$kon)
    validate(need(nrow(bas) > 0, 'Ingen data för valt urval.'))

    befolkning_tot <- bas %>%
      group_by(ar) %>%
      summarise(tot = sum(varde, na.rm = TRUE), .groups = 'drop')

    utrikes <- bas %>% filter(str_to_lower(fodelseregion) != 'sverige')
    validate(need(nrow(utrikes) > 0, 'Ingen data om utrikes födda för valt urval.'))

    # De 7 största födelseländerna det senaste året; resten samlas i "Övriga".
    # Datasetets egen "övriga"-kategori utesluts ur topplistan och slås alltid
    # ihop med vår grupp så att det inte blir två övriga-staplar.
    topp <- utrikes %>%
      filter(!ar_ovrigrad(fodelseregion)) %>%
      filter(ar == max(ar)) %>%
      group_by(fodelseregion) %>%
      summarise(v = sum(varde, na.rm = TRUE), .groups = 'drop') %>%
      slice_max(v, n = 7) %>%
      pull(fodelseregion)

    df <- utrikes %>%
      mutate(grupp = if_else(fodelseregion %in% topp & !ar_ovrigrad(fodelseregion),
                             fodelseregion, 'Övriga födelseländer')) %>%
      group_by(ar, grupp) %>%
      summarise(varde = sum(varde, na.rm = TRUE), .groups = 'drop') %>%
      left_join(befolkning_tot, by = 'ar') %>%
      mutate(andel = varde / tot,
             tooltip = paste0(grupp, ' ', ar,
                              '<br>Antal: ', fmt(varde),
                              '<br>Andel av befolkningen: ', fmt(100 * andel, 1), ' %'))

    nivaer <- df %>%
      group_by(grupp) %>%
      summarise(tot = sum(varde), .groups = 'drop') %>%
      arrange(grupp == 'Övriga födelseländer', desc(tot)) %>%   # Övriga sist
      pull(grupp)
    df <- df %>% mutate(grupp = factor(grupp, levels = nivaer))

    pal <- setNames(
      c(rep(unname(rd_farg[c('primary', 'bla_klar', 'bla_mork', 'bla_ljus')]),
            length.out = length(nivaer) - 1),
        '#c9ced3'),                                              # Övriga = grå
      nivaer
    )

    visa_andel <- input$fodelseland_matt == 'Andel'

    p <- ggplot(df, aes(ar, if (visa_andel) andel else varde, fill = grupp)) +
      geom_col_interactive(aes(tooltip = tooltip, data_id = paste(grupp, ar)),
                           position = 'stack') +
      scale_fill_manual(values = pal) +
      scale_y_continuous(labels = if (visa_andel) {
        function(x) paste0(fmt(100 * x, 1), ' %')
      } else fmt) +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      labs(x = NULL,
           y = if (visa_andel) 'Andel av hela befolkningen' else 'Antal personer') +
      theme_rd()

    skapa_girafe(p, hojd = 4.8)
  })

  fodelseland_ar <- reactive({
    ar_i_span <- bef_fodelseland$ar[bef_fodelseland$ar %in% ar_span()]
    validate(need(length(ar_i_span) > 0, 'Ingen data i valt årsintervall.'))
    max(ar_i_span, na.rm = TRUE)
  })

  output$rubrik_fodelseland_jmf <- renderText({
    paste0('Andel utrikes födda per kommun ', fodelseland_ar())
  })

  output$fig_fodelseland_jmf <- renderGirafe({
    df <- bef_fodelseland %>%
      filter(ar == fodelseland_ar(), !ar_totalrad(fodelseregion)) %>%
      filtrera_kon(input$kon) %>%
      group_by(regionkod, region) %>%
      summarise(
        tot     = sum(varde, na.rm = TRUE),
        utrikes = sum(varde[str_to_lower(fodelseregion) != 'sverige'], na.rm = TRUE),
        .groups = 'drop') %>%
      filter(tot > 0) %>%
      mutate(andel = utrikes / tot,
             vald  = regionkod == input$region,
             region = reorder(region, andel),
             tooltip = paste0(region,
                              '<br>Andel utrikes födda: ', fmt(100 * andel, 1), ' %',
                              '<br>Antal utrikes födda: ', fmt(utrikes),
                              '<br>Folkmängd: ', fmt(tot)))
    validate(need(nrow(df) > 0, 'Ingen data för valt urval.'))

    p <- ggplot(df, aes(x = andel, y = region, fill = vald)) +
      geom_col_interactive(aes(tooltip = tooltip, data_id = region),
                           orientation = 'y', width = 0.75) +
      scale_fill_manual(values = c(`TRUE`  = rd_farg[['primary']],
                                   `FALSE` = rd_farg[['bla_ljus']]),
                        guide = 'none') +
      scale_x_continuous(labels = function(x) paste0(fmt(100 * x), ' %'),
                         expand = expansion(mult = c(0, 0.05))) +
      labs(x = 'Andel utrikes födda', y = NULL) +
      theme_rd()

    skapa_girafe(p, hojd = 5.4)
  })

  output$rubrik_fodelseland_tabell <- renderText({
    paste0('Födelseländer - ', regionnamn(), ' ', fodelseland_ar())
  })

  output$tab_fodelseland <- renderDT({
    df <- bef_fodelseland %>%
      filter(regionkod == input$region, ar == fodelseland_ar(),
             !ar_totalrad(fodelseregion)) %>%
      filtrera_kon(input$kon) %>%
      group_by(fodelseregion) %>%
      summarise(antal = sum(varde, na.rm = TRUE), .groups = 'drop') %>%
      mutate(andel = 100 * antal / sum(antal)) %>%
      arrange(desc(antal)) %>%
      transmute(`Födelseland` = fodelseregion,
                Antal         = antal,
                `Andel (%)`   = andel)
    validate(need(nrow(df) > 0, 'Ingen data för valt urval.'))

    datatable(df, rownames = FALSE,
              options = list(pageLength = 15, dom = 'ftip')) %>%
      formatRound('Antal', digits = 0, mark = ' ') %>%
      formatRound('Andel (%)', digits = 1, mark = ' ', dec.mark = ',')
  })

  # ==== Flik 7: Födda, döda & fruktsamhet ====================================

  output$fig_fodda_doda <- renderGirafe({
    # OBS: födda saknar kön och visar alltid samtliga - vid valt kön blir
    # nettot därför inte könsuppdelat
    fig_par_netto(fodda, doda, 'Födda', 'Döda', 'Födelsenetto')
  })

  output$fig_fruktsamhet <- renderGirafe({
    visa_regioner <- unique(c(input$region, '00'))
    df <- tfr_data %>%
      filter(regionkod %in% visa_regioner, ar %in% ar_span()) %>%
      mutate(tooltip = paste0(region, ' ', ar,
                              '<br>Summerad fruktsamhet: ', fmt(tfr, 2)))
    validate(need(nrow(df) > 0, 'Ingen data för valt urval.'))

    p <- ggplot(df, aes(ar, tfr, color = region)) +
      geom_line(linewidth = 1) +
      geom_point_interactive(aes(tooltip = tooltip, data_id = paste(region, ar)),
                             size = 1.8) +
      scale_color_manual(values = unname(rd_farg[c('primary', 'gra_mork')])) +
      scale_y_continuous(labels = function(x) fmt(x, 1)) +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      labs(x = NULL, y = 'Barn per kvinna') +
      theme_rd()

    skapa_girafe(p, hojd = 4.4)
  })

  # ==== Flik 8: Prognos & utfall =============================================

  output$fig_prognos <- renderGirafe({
    utfall <- summera_ar(totfolkmangd, input$kon) %>%
      filter(regionkod == input$region, ar >= input$ar_intervall[1])
    prog <- prognos_summerad() %>%
      filter(ar >= input$ar_intervall[1])
    validate(need(nrow(prog) > 0, 'Ingen prognosdata för vald region.'))

    prognosnamn <- sort(unique(prog$prognos))
    pal <- c(
      setNames(rep(unname(rd_farg[c('primary', 'bla_klar', 'bla_mork', 'bla_ljus')]),
                   length.out = length(prognosnamn)),
               prognosnamn),
      Utfall = rd_farg[['gra_mork']]
    )

    p <- ggplot() +
      geom_line(data = utfall, aes(ar, varde, color = 'Utfall', group = 1),
                linewidth = 1.2) +
      geom_point_interactive(
        data = utfall,
        aes(ar, varde, color = 'Utfall',
            tooltip = paste0('Utfall ', ar, ': ', fmt(varde)),
            data_id = paste('utfall', ar)),
        size = 1.6) +
      geom_line(data = prog, aes(ar, varde, color = prognos, group = prognos),
                linetype = '42', linewidth = 0.9) +
      geom_point_interactive(
        data = prog,
        aes(ar, varde, color = prognos,
            tooltip = paste0(prognos, ', ', ar, ': ', fmt(varde)),
            data_id = paste(prognos, ar)),
        size = 1.4) +
      scale_color_manual(values = pal) +
      scale_y_continuous(labels = fmt) +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      labs(x = NULL, y = 'Folkmängd') +
      theme_rd()

    skapa_girafe(p, hojd = 4.8)
  })

  output$fig_avvikelse <- renderGirafe({
    utfall <- summera_ar(totfolkmangd, input$kon) %>%
      filter(regionkod == input$region) %>%
      select(ar, utfall = varde)

    df <- prognos_summerad() %>%
      inner_join(utfall, by = 'ar') %>%
      mutate(avvikelse = varde - utfall,
             tooltip = paste0(prognos, ', ', ar,
                              '<br>Avvikelse: ',
                              if_else(avvikelse >= 0, '+', '\u2212'), fmt(abs(avvikelse)),
                              '<br>Prognos: ', fmt(varde),
                              '<br>Utfall: ', fmt(utfall)))
    validate(need(nrow(df) > 0,
                  'Inga år där både prognos och utfall finns för valda prognosomgångar.'))

    pal <- setNames(rep(unname(rd_farg[c('primary', 'bla_klar', 'bla_mork', 'bla_ljus')]),
                        length.out = n_distinct(df$prognos)),
                    sort(unique(df$prognos)))

    p <- ggplot(df, aes(ar, avvikelse, color = prognos)) +
      geom_hline(yintercept = 0, color = rd_farg[['gra_mork']], linewidth = 0.4) +
      geom_line(linewidth = 1) +
      geom_point_interactive(aes(tooltip = tooltip, data_id = paste(prognos, ar)),
                             size = 1.8) +
      scale_color_manual(values = pal) +
      scale_y_continuous(labels = fmt) +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      labs(x = NULL, y = 'Prognos minus utfall') +
      theme_rd()

    skapa_girafe(p, hojd = 4.2)
  })

  output$rubrik_avvikelse_alder <- renderText({
    paste0('Prognosavvikelse per åldersklass - ', regionnamn(),
           if (!is.null(input$avvikelse_ar)) paste0(' ', input$avvikelse_ar) else '')
  })

  output$fig_avvikelse_alder <- renderGirafe({
    validate(need(length(avvikelse_ar_val) > 0,
                  'Det finns ännu inga år med både prognos och utfall i databasen.'))
    req(input$valda_prognoser, input$avvikelse_ar)
    valt_ar <- as.integer(input$avvikelse_ar)

    utfall_alder <- totfolkmangd %>%
      filter(regionkod == input$region, ar == valt_ar, !ar_totalrad(alder)) %>%
      filtrera_kon(input$kon) %>%
      mutate(aldersklass = gor_aldersklass(parse_alder(alder))) %>%
      group_by(aldersklass) %>%
      summarise(utfall = sum(varde, na.rm = TRUE), .groups = 'drop')

    prog_alder <- prognos_utfall %>%
      filter(regionkod == input$region, ar == valt_ar,
             prognos_ar %in% as.integer(input$valda_prognoser),
             !ar_totalrad(alder)) %>%
      filtrera_kon(input$kon) %>%
      mutate(aldersklass = gor_aldersklass(parse_alder(alder))) %>%
      group_by(prognos_ar, aldersklass) %>%
      summarise(prognos_varde = sum(varde, na.rm = TRUE), .groups = 'drop') %>%
      mutate(prognos = paste('Prognos', prognos_ar))

    df <- prog_alder %>%
      inner_join(utfall_alder, by = 'aldersklass') %>%
      mutate(avvikelse = prognos_varde - utfall,
             tooltip = paste0(prognos, ', ', aldersklass, ' år',
                              '<br>Avvikelse: ',
                              if_else(avvikelse >= 0, '+', '\u2212'), fmt(abs(avvikelse)),
                              '<br>Prognos: ', fmt(prognos_varde),
                              '<br>Utfall: ', fmt(utfall)))
    validate(need(nrow(df) > 0,
                  'Ingen prognosdata för valt utfallsår och valda prognosomgångar.'))

    pal <- setNames(rep(unname(rd_farg[c('primary', 'bla_klar', 'bla_mork', 'bla_ljus')]),
                        length.out = n_distinct(df$prognos)),
                    sort(unique(df$prognos)))

    p <- ggplot(df, aes(aldersklass, avvikelse, fill = prognos)) +
      geom_col_interactive(aes(tooltip = tooltip, data_id = paste(prognos, aldersklass)),
                           position = position_dodge(preserve = 'single'),
                           width = 0.75) +
      geom_hline(yintercept = 0, color = rd_farg[['gra_mork']], linewidth = 0.3) +
      scale_fill_manual(values = pal) +
      scale_y_continuous(labels = fmt) +
      labs(x = 'Åldersklass', y = 'Prognos minus utfall') +
      theme_rd()

    skapa_girafe(p, hojd = 4.6)
  })

})
