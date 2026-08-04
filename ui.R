source('global.R')

shinyUI(
  fluidPage(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = 'icon', type = 'image/x-icon', href = 'favicon.ico'),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'regiondalarna_ruf.css'),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'app.css?v=6'),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('rd_stang_tooltips', function(mapId) {
          var el = document.getElementById(mapId);
          var map = el && el.__leaflet__ ? el.__leaflet__ :
                    (HTMLWidgets.find('#' + mapId) && HTMLWidgets.find('#' + mapId).getMap ?
                     HTMLWidgets.find('#' + mapId).getMap() : null);
          if (!map || !map.eachLayer) return;
          map.eachLayer(function(layer) {
            if (layer.closeTooltip) layer.closeTooltip();
          });
        });
      "))
    ),

    # ---- Header (matchar .rd-header i regiondalarna_ruf.css) --------------
    tags$div(
      class = 'rd-header',
      tags$div(class = 'rd-header__title', 'Befolkningsutvecklingen i Dalarna - experimentyta för analyser'),
      tags$a(
        class  = 'rd-header__right',
        href   = 'https://www.regiondalarna.se',
        target = '_blank',
        tags$img(src = 'logo_liggande_fri_vit.png', alt = 'Region Dalarna'),
        tags$span('Samhällsanalys')
      )
    ),

    # ---- Innehåll: sidopanel + flikar --------------------------------------
    tags$div(
      class = 'rd-app',

      # ---- Sidopanel med globala filter ----
      tags$aside(
        class = 'rd-sidebar',
        h3('Filter'),
        div(class = 'rd-field',
            selectizeInput('region', 'Region',
                           choices = regionval, selected = '20',
                           options = list(placeholder = 'Välj region...'))),
        div(class = 'rd-field',
            radioButtons('kon', 'Kön',
                         choices = c('Totalt', 'Kvinnor', 'Män'),
                         selected = 'Totalt')),
        div(class = 'rd-field',
            sliderInput('ar_intervall', 'Årsintervall',
                        min = ar_min, max = ar_max,
                        value = c(max(ar_min, ar_max - 20), ar_max),
                        step = 1, sep = '')),
        div(class = 'rd-info',
            HTML(paste0('<strong>Obs!</strong> Födda saknar könsuppdelning. ',
                        'Diagram som bygger på födda (komponenter, netton, fruktsamhet) ',
                        'visar därför alltid samtliga. Fliken Flyttrelationer har egna filter.'))),
        tags$hr(),
        h3('Ladda ner data'),
        div(class = 'rd-field',
            selectizeInput('nedladdning_val', 'Välj dataset',
                           choices = c('Alla dataset', names(nedladdning_dataset)),
                           selected = 'Alla dataset', multiple = TRUE,
                           options = list(placeholder = 'Välj ett eller flera dataset...'))),
        downloadButton('nedladdning_download', 'Ladda ner Excel',
                       class = 'rd-btn rd-btn--primary', style = 'width: 100%; justify-content: center;'),
        uiOutput('nedladdning_info')
      ),

      # ---- Huvudinnehåll ----
      tags$div(
        class = 'rd-main',
        tabsetPanel(
          id = 'flikar',

          # ================= Flik 1: Översikt =================
          tabPanel('Översikt',
                   uiOutput('kpi_rad'),
                   div(class = 'rd-card',
                       h2(textOutput('rubrik_folkmangd', inline = TRUE)),
                       p(class = 'rd-subtitle',
                         'Folkmängd per år för vald region. Med jämförelsen ikryssad visas indexerad utveckling (första året i intervallet = 100) mot länet och riket.'),
                       checkboxInput('jamfor_riket', 'Jämför med länet och riket (index)', value = FALSE),
                       girafeOutput('fig_folkmangd', height = '420px')),
                   div(class = 'rd-card',
                       h2(textOutput('rubrik_pyramid', inline = TRUE)),
                       p(class = 'rd-subtitle', 'Åldersstruktur uppdelad på kön.'),
                       div(class = 'rd-slider-single',
                           sliderInput('pyramid_ar', 'År',
                                       min = ar_min, max = ar_max, value = ar_max,
                                       step = 1, sep = '', width = '320px',
                                       animate = animationOptions(interval = 1600, loop = FALSE))),
                       girafeOutput('fig_pyramid', height = '520px'))
          ),

          # ================= Flik 2: Befolkningsförändring =================
          tabPanel('Befolkningsförändring',
                   div(class = 'rd-card',
                       h2(textOutput('rubrik_komponenter', inline = TRUE)),
                       p(class = 'rd-subtitle',
                         'Staplar över nollstrecket ökar folkmängden, staplar under minskar den. Linjen visar summan av komponenterna (total förändring). Visas för samtliga oavsett könsfilter.'),
                       girafeOutput('fig_komponenter', height = '460px')),
                   div(class = 'rd-card',
                       h2(textOutput('rubrik_netton', inline = TRUE)),
                       p(class = 'rd-subtitle',
                         'Födelsenetto (födda minus döda), inrikes flyttnetto och utrikes flyttnetto.'),
                       girafeOutput('fig_netton', height = '400px'))
          ),

          # ================= Flik 3: Inrikes flyttningar =================
          tabPanel('Inrikes flyttningar',
                   div(class = 'rd-card',
                       h2(textOutput('rubrik_inrikes_flytt', inline = TRUE)),
                       p(class = 'rd-subtitle',
                         'Inflyttning visas som staplar uppåt och utflyttning som staplar nedåt, uppdelat på flyttar inom länet respektive till/från övriga län. Linjen visar inrikes flyttnetto.'),
                       girafeOutput('fig_inrikes_flytt', height = '460px')),
                   div(class = 'rd-card',
                       h2(textOutput('rubrik_flyttnetto_alder', inline = TRUE)),
                       p(class = 'rd-subtitle',
                         'Genomsnitt per år i valt årsintervall. Visar i vilka åldrar regionen vinner respektive tappar befolkning på inrikes flyttningar.'),
                       girafeOutput('fig_flyttnetto_alder', height = '420px')),
                   div(class = 'rd-card',
                       h2(textOutput('rubrik_netto_aldersgrupp', inline = TRUE)),
                       p(class = 'rd-subtitle',
                         'Inrikes flyttnetto över tid för valda åldersgrupper - förvalda grupper följer barn och unga. För små kommuner kan enstaka år bygga på låga tal; tolka trender snarare än enskilda år.'),
                       div(class = 'rd-controls',
                           selectizeInput('valda_aldersgrupper', 'Åldersgrupper',
                                          choices  = aldersgrupp_val,
                                          selected = aldersgrupp_barn,
                                          multiple = TRUE, width = '420px')),
                       girafeOutput('fig_netto_aldersgrupp', height = '420px')),
                   div(class = 'rd-card',
                       h2(textOutput('rubrik_aldersgrupp_ar', inline = TRUE)),
                       p(class = 'rd-subtitle',
                         'Inrikes flyttnetto för samtliga åldersgrupper ett enskilt år. Staplar uppåt = fler flyttar in än ut.'),
                       div(class = 'rd-controls',
                           selectInput('flyttgrp_ar', 'År',
                                       choices = flyttgrp_ar_val, width = '140px')),
                       girafeOutput('fig_aldersgrupp_ar', height = '650px'))
          ),

          # ================= Flik 4: Flyttrelationer =================
          tabPanel('Flyttrelationer',
                   div(class = 'rd-card',
                       h2(textOutput('rubrik_flyttrel', inline = TRUE)),
                       p(class = 'rd-subtitle',
                         'Se varifrån inflyttarna kommer och vart utflyttarna tar vägen - grannkommuner och övriga Sverige. Kartorna visar summan för valt årsintervall; slå ihop flera år för att hantera låga tal. Avgränsa på åldersgrupp för att t.ex. följa barnfamiljer. Tal under 3 visas som "färre än 3".'),
                       div(class = 'rd-controls',
                           selectInput('rel_kommun', 'Kommun / Län',
                                       choices = rel_kommun_val, width = '260px')),
                       div(class = 'rd-controls',
                           radioButtons('rel_typ', 'Visa',
                                        choices = c('Inflyttning', 'Utflyttning', 'Flyttnetto'),
                                        inline = TRUE),
                           sliderInput('rel_ar', 'Årsintervall',
                                       min = rel_ar_min, max = rel_ar_max,
                                       value = c(max(rel_ar_min, rel_ar_max - 4), rel_ar_max),
                                       step = 1, sep = '', width = '240px'),
                           selectizeInput('rel_alder', 'Åldersgrupper',
                                          choices = rel_alder_val, selected = rel_alder_val,
                                          multiple = TRUE, width = '300px')),
                       div(class = 'rd-map-grid',
                           div(
                             h3(textOutput('rubrik_karta_kommun', inline = TRUE)),
                             leafletOutput('karta_flyttrel_kommun', height = 460)),
                           div(
                             h3(textOutput('rubrik_karta_lan', inline = TRUE)),
                             leafletOutput('karta_flyttrel_lan', height = 460))),
                       p(class = 'rd-kalla', 'Källa: Statistiska centralbyrån (SCB), bearbetning av Samhällsanalys, Region Dalarna')),
                   div(class = 'rd-card',
                       h2(textOutput('rubrik_flyttrel_tabell', inline = TRUE)),
                       p(class = 'rd-subtitle',
                         'Kommuner med störst flyttutbyte med vald kommun, för valt urval ovan. Kommuner där både in- och utflyttning understiger 3 redovisas samlat som "Övriga kommuner".'),
                       DTOutput('tab_flyttrel'),
                       p(class = 'rd-kalla', 'Källa: Statistiska centralbyrån (SCB), bearbetning av Samhällsanalys, Region Dalarna'))
          ),

          # ================= Flik 5: In- och utvandring =================
          tabPanel('In- och utvandring',
                   div(class = 'rd-card',
                       h2(textOutput('rubrik_utrikes', inline = TRUE)),
                       p(class = 'rd-subtitle',
                         'Antal invandrade och utvandrade per år (linjer) samt utrikes flyttnetto (staplar).'),
                       girafeOutput('fig_utrikes', height = '420px')),
                   div(class = 'rd-card',
                       h2(textOutput('rubrik_utrikes_alder', inline = TRUE)),
                       p(class = 'rd-subtitle',
                         'Genomsnitt per år i valt årsintervall. Visar i vilka åldrar in- och utvandringen påverkar befolkningen mest.'),
                       girafeOutput('fig_utrikes_alder', height = '420px'))
          ),

          # ================= Flik 6: Födelseland =================
          tabPanel('Födelseland',
                   div(class = 'rd-card',
                       h2(textOutput('rubrik_fodelseland_tid', inline = TRUE)),
                       p(class = 'rd-subtitle',
                         'De största födelseländerna bland utrikes födda i vald region; övriga länder samlas i en grupp. Växla mellan antal och andel av hela befolkningen.'),
                       div(class = 'rd-controls',
                           radioButtons('fodelseland_matt', NULL,
                                        choices = c('Antal', 'Andel'),
                                        selected = 'Antal', inline = TRUE)),
                       girafeOutput('fig_fodelseland_tid', height = '440px')),
                   div(class = 'rd-card',
                       h2(textOutput('rubrik_fodelseland_jmf', inline = TRUE)),
                       p(class = 'rd-subtitle',
                         'Andel av befolkningen som är född utanför Sverige, per kommun samt för länet och riket. Vald region är markerad.'),
                       girafeOutput('fig_fodelseland_jmf', height = '480px')),
                   div(class = 'rd-card',
                       h2(textOutput('rubrik_fodelseland_tabell', inline = TRUE)),
                       p(class = 'rd-subtitle',
                         'Samtliga födelseländer i underlaget för vald region, med antal och andel av befolkningen.'),
                       DTOutput('tab_fodelseland'),
                       p(class = 'rd-kalla', 'Källa: Statistiska centralbyrån (SCB), bearbetning av Samhällsanalys, Region Dalarna'))
          ),

          # ================= Flik 7: Födda, döda & fruktsamhet =================
          tabPanel('Födda, döda & fruktsamhet',
                   div(class = 'rd-card',
                       h2(textOutput('rubrik_fodda_doda', inline = TRUE)),
                       p(class = 'rd-subtitle',
                         'Antal födda och döda per år (linjer) samt födelsenetto (staplar).'),
                       girafeOutput('fig_fodda_doda', height = '420px')),
                   div(class = 'rd-card',
                       h2(textOutput('rubrik_fruktsamhet', inline = TRUE)),
                       p(class = 'rd-subtitle',
                         'Antal barn en kvinna i genomsnitt skulle föda under sin livstid utifrån årets åldersspecifika fruktsamhetstal. Vald region jämförs med riket. För små kommuner varierar talet kraftigt mellan år.'),
                       girafeOutput('fig_fruktsamhet', height = '400px'))
          ),

          # ================= Flik 8: Prognos =================
          tabPanel('Prognos',
                   div(class = 'rd-card',
                       h2(textOutput('rubrik_prognos_kommuner', inline = TRUE)),
                       p(class = 'rd-subtitle',
                         'Procentuell förändring av folkmängden mellan senaste utfallsåret och det valda antalet år framåt, enligt senaste prognosen. Samtliga Dalakommuner samt Dalarnas län (mörkare stapel).'),
                       div(class = 'rd-slider-graybar',
                           div(class = 'rd-controls',
                               sliderInput('prognos_ar_framat', 'Antal år framåt',
                                           min = 1, max = prognos_ar_max_framat,
                                           value = min(10, prognos_ar_max_framat),
                                           step = 1, sep = '', width = '320px'))),
                       girafeOutput('fig_prognos_kommuner', height = '620px')),
                   div(class = 'rd-card',
                       h2(textOutput('rubrik_prognos_senaste', inline = TRUE)),
                       p(class = 'rd-subtitle',
                         'Heldragen linje visar faktiskt utfall (hela den historiska tidsserien i databasen), streckad linje visar senaste prognosen från senaste utfallsåret och framåt.'),
                       div(class = 'rd-controls',
                           radioButtons('prognos_senaste_matt', NULL,
                                        choices = c('Antal', 'Procent'),
                                        selected = 'Antal', inline = TRUE)),
                       div(class = 'rd-controls',
                           selectInput('prognos_aldersgrupp', 'Åldersgrupper',
                                       choices = names(prognos_aldersgrupper),
                                       selected = 'Alla åldrar', width = '200px'),
                           checkboxInput('prognos_egen_aktiv', 'Skapa egen åldersgrupp', value = FALSE)),
                       conditionalPanel(
                         condition = 'input.prognos_egen_aktiv == true',
                         div(class = 'rd-controls',
                             sliderInput('prognos_egen_alder', 'Egen åldersgrupp (min-max år)',
                                         min = 0, max = prognos_alder_max,
                                         value = c(0, prognos_alder_max),
                                         step = 1, sep = '', width = '420px'))),
                       girafeOutput('fig_prognos_senaste', height = '420px'))
          ),

          # ================= Flik 9: Prognos vs. utfall =================
          tabPanel('Prognos vs. utfall',
                   div(class = 'rd-card',
                       h2(textOutput('rubrik_prognos', inline = TRUE)),
                       p(class = 'rd-subtitle',
                         'Heldragen linje visar faktisk folkmängd, streckade linjer visar valda prognosår.'),
                       div(class = 'rd-controls',
                           selectizeInput('valda_prognoser', 'Prognosår',
                                          choices  = prognosar_val,
                                          selected = head(prognosar_val, 2),
                                          multiple = TRUE, width = '320px')),
                       girafeOutput('fig_prognos', height = '440px')),
                   div(class = 'rd-card',
                       h2(textOutput('rubrik_avvikelse', inline = TRUE)),
                       p(class = 'rd-subtitle',
                         'Skillnad mellan utfall och prognos (utfall minus prognos), en linje per prognosår. Värden över nollstrecket = utfallet blev högre än prognosen, under = utfallet blev lägre.'),
                       girafeOutput('fig_avvikelse', height = '380px')),
                   div(class = 'rd-card',
                       h2(textOutput('rubrik_avvikelse_alder', inline = TRUE)),
                       p(class = 'rd-subtitle',
                         'Prognosavvikelse uppdelad på 10-årsklasser för valt utfallsår - visar i vilka åldrar prognoserna träffar sämst, t.ex. om flyttbenägna unga över- eller underskattas. Över nollstrecket = utfallet blev högre än prognosen, under = lägre. Styrs av Prognosår-filtret ovan.'),
                       div(class = 'rd-controls',
                           selectInput('avvikelse_ar', 'Utfallsår',
                                       choices = avvikelse_ar_val, width = '140px')),
                       girafeOutput('fig_avvikelse_alder', height = '420px'))
          ),

          # ================= Om =================
          tabPanel('Om',
                   div(class = 'rd-card',
                       h2('Om applikationen'),
                       p('Applikationen visar befolkningsutvecklingen i Dalarnas kommuner, Dalarnas län och riket. Underlaget hämtas från SCB via Region Dalarnas databas (scheman ', tags$code('scb'), ' och ', tags$code('mikro_db'), ') och omfattar folkmängd, födda, döda, in- och utvandring, inrikes flyttningar, flyttrelationer mellan kommuner, befolkning efter födelseland samt SCB:s befolkningsprognoser.'),
                       p('Filtren i vänsterspalten styr samtliga flikar utom Flyttrelationer, som har egna filter. Könsfiltret påverkar de diagram där könsuppdelning finns i underlaget; för födda saknas uppdelningen och samtliga visas alltid.'),
                       p('Flyttrelationerna bygger på mikrodata. Tal under 3 visas som "färre än 3" och kommuner med genomgående låga tal redovisas samlat. Vid små urval (enskilda åldersgrupper och enstaka år) kan talen vara låga - slå ihop flera år i årsintervallet för stabilare bilder.'),
                       p('Kontakt: ',
                         tags$a(href = 'mailto:samhallsanalys@regiondalarna.se',
                                'samhallsanalys@regiondalarna.se'))),
                   div(class = 'rd-card',
                       h2('Ändringslogg'),
                       p(class = 'rd-subtitle', 'Senaste uppdateringarna i applikationen, senaste datumet överst.'),
                       tagList(
                         lapply(andringslogg, function(post) {
                           tagList(
                             h3(post$datum),
                             tags$ul(lapply(post$andringar, tags$li))
                           )
                         })
                       )))
        )
      )
    ),

    # ---- Footer (matchar .rd-footer i regiondalarna_ruf.css) --------------
    tags$div(
      class = 'rd-footer',
      'Samhällsanalys, Region Dalarna · ',
      tags$a(
        href = 'mailto:samhallsanalys@regiondalarna.se',
        'samhallsanalys@regiondalarna.se'
      )
    )
  )
)
