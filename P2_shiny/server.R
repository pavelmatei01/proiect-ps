# =============================================================================
# server.R  --  Logica reactiva a aplicatiei Shiny (Problema 2)
# =============================================================================
#
# ROLUL ACESTUI FISIER:
#   Leaga intrarile (input$...) de iesiri (output$...). Toata logica de calcul
#   vine din fisierele din R/ (distributii.R, transformari_1d.R) — server.R doar
#   le APELEAZA si actualizeaza interfata.
#
# NOTA: incarcam pachetele SI aici (nu doar in app.R), fiindca atunci cand Shiny
# detecteaza ui.R + server.R separate, le poate rula DIRECT, fara sa treaca prin
# app.R. Incarcand library() aici, ggplot2 e garantat disponibil indiferent cum
# porneste aplicatia. (library() apelat de mai multe ori e inofensiv.)
library(shiny)
library(ggplot2)

# Incarcam si functiile de logica, defensiv: daca Shiny ruleaza server.R direct
# (fara app.R), functiile din R/ tot trebuie sa fie disponibile. Le incarcam doar
# daca nu sunt deja definite (ca sa nu dublam munca lui app.R).
if (!exists("genereaza_esantion")) {
  for (f in c("R/distributii.R", "R/transformari_1d.R", "R/transformari_2d.R")) {
    if (file.exists(f)) source(f, local = FALSE)
  }
}
#
# DECIZII CHEIE:
#   1) PARAMETRI DINAMICI: campurile de parametri se genereaza in functie de
#      repartitia aleasa (renderUI + descrie_parametri din distributii.R). Cand
#      alegi Normala vezi mu/sigma; cand alegi Exponentiala vezi doar lambda.
#   2) BUTON SIMULEAZA (eventReactive): tot calculul porneste DOAR la apasarea
#      butonului (cerinta 4 Shiny) — nu la fiecare miscare de slider.
#   3) VALIDARE REALA (cerinta 3 Shiny): inainte de simulare verificam parametrii
#      cu valideaza_parametri(); daca pica, afisam mesaj si NU simulam.
# =============================================================================

server <- function(input, output, session) {

  # =========================================================================
  # (0) PARAMETRII DINAMICI  (UI care se schimba per repartitie)
  # =========================================================================
  # Generam campurile numerice corecte pentru repartitia aleasa. Folosim
  # descrie_parametri() din distributii.R, care ne da pentru fiecare repartitie
  # lista de (id, eticheta, valoare implicita).
  output$parametri_ui <- renderUI({
    specs <- descrie_parametri(input$distributie)
    # Pentru fiecare parametru, construim un numericInput cu eticheta lui.
    campuri <- lapply(specs, function(p) {
      numericInput(inputId = p$id, label = p$eticheta, value = p$valoare)
    })
    # tagList combina campurile intr-un singur bloc de interfata.
    do.call(tagList, campuri)
  })

  # =========================================================================
  # (1) STRANGEREA PARAMETRILOR CURENTI intr-o lista numita
  # =========================================================================
  # Parametrii au id-uri diferite per repartitie (mu, sigma, lambda, a, b...).
  # Construim o lista numita citind DOAR id-urile asteptate de repartitia aleasa.
  # Asa evitam sa citim un input inexistent (ex: input$mu cand suntem pe Exp).
  parametri_curenti <- reactive({
    specs <- descrie_parametri(input$distributie)
    par <- list()
    for (p in specs) {
      par[[p$id]] <- input[[p$id]]   # citeste valoarea campului cu acest id
    }
    par
  })

  # =========================================================================
  # (2) VALIDAREA INTRARILOR  (cerinta 3 Shiny)
  # =========================================================================
  # Apeleaza valideaza_parametri() din distributii.R. Returneaza NULL daca e ok,
  # sau un mesaj de eroare (string) altfel.
  valideaza <- reactive({
    par <- parametri_curenti()
    # Daca vreun camp e inca NULL (UI nu s-a randat complet), nu validam inca.
    if (any(vapply(par, is.null, logical(1)))) return(NULL)
    valideaza_parametri(input$distributie, par)
  })

  # Afisam mesajul de validare (gol daca e ok). Se actualizeaza in timp real,
  # ca utilizatorul sa vada problema INAINTE sa apese Simuleaza.
  output$mesaj_validare <- renderText({
    msg <- valideaza()
    if (is.null(msg)) "" else msg
  })

  # =========================================================================
  # (3) SIMULAREA — porneste DOAR la apasarea butonului (eventReactive)
  # =========================================================================
  date_simulate <- eventReactive(input$simuleaza, {

    # Daca validarea pica, nu simulam. Afisam un mesaj si oprim elegant.
    msg <- valideaza()
    validate(need(is.null(msg), msg))   # validate/need = mesaj curat in UI

    par <- parametri_curenti()
    n   <- input$n

    # --- Generarea esantionului X (distributii.R) ---
    X <- genereaza_esantion(input$distributie, par, n)

    # --- Aplicarea transformarii Y = g(X) (transformari_1d.R) ---
    # Intoarce lista cu $y, $n_eliminate, $avertisment.
    rezultat_Y <- aplica_transformare(input$transformare, X)

    # Returnam tot ce au nevoie output-urile.
    list(
      X = X,
      Y = rezultat_Y$y,
      avertisment = rezultat_Y$avertisment
    )
  })

  # =========================================================================
  # (4) AVERTISMENTUL DE LA TRANSFORMARE  (ex: log pe valori <= 0)
  # =========================================================================
  output$avertisment_transf <- renderText({
    d <- date_simulate()
    d$avertisment
  })

  # =========================================================================
  # (5) GRAFICELE  (cerintele 3, 5, 6)
  # =========================================================================
  output$hist_X <- renderPlot({
    d <- date_simulate()
    ggplot(data.frame(x = d$X), aes(x = x)) +
      geom_histogram(bins = 40, fill = "#2E5A87", color = "white") +
      labs(x = "X", y = "Frecventa") +
      theme_minimal(base_size = 12)
  })

  output$hist_Y <- renderPlot({
    d <- date_simulate()
    ggplot(data.frame(y = d$Y), aes(x = y)) +
      geom_histogram(bins = 40, fill = "#1B7340", color = "white") +
      labs(x = "Y = g(X)", y = "Frecventa") +
      theme_minimal(base_size = 12)
  })

  # =========================================================================
  # (6) INDICATORII NUMERICI  (cerintele 3 si 5)
  # =========================================================================
  rezuma <- function(v) {
    q <- quantile(v, probs = c(0.25, 0.5, 0.75))
    paste0(
      "Media:      ", round(mean(v), 4), "\n",
      "Dispersia:  ", round(var(v), 4), "\n",
      "Std. dev.:  ", round(sd(v), 4), "\n",
      "Minim:      ", round(min(v), 4), "\n",
      "Q1 (25%):   ", round(q[1], 4), "\n",
      "Mediana:    ", round(q[2], 4), "\n",
      "Q3 (75%):   ", round(q[3], 4), "\n",
      "Maxim:      ", round(max(v), 4)
    )
  }

  output$stats_X <- renderText({ rezuma(date_simulate()$X) })
  output$stats_Y <- renderText({ rezuma(date_simulate()$Y) })

  # =========================================================================
  # (7) INTERPRETAREA AUTOMATA  (cerinta 7)
  # =========================================================================
  output$interpretare <- renderText({
    d <- date_simulate()
    mesaje <- interpreteaza_transformare(d$X, d$Y)
    # Lipim mesajele cu cate un bullet pe linie.
    paste0("- ", mesaje, collapse = "\n")
  })
}