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

    # Returnam tot ce au nevoie output-urile. Stocam si distributie+par, ca
    # histograma sa poata suprapune densitatea teoretica corespunzatoare
    # (cerinta enunt: "verificati simularea suprapunand densitatea de probabilitate").
    list(
      X = X,
      Y = rezultat_Y$y,
      avertisment = rezultat_Y$avertisment,
      distributie = input$distributie,
      par         = par
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
  # Histograma X cu DENSITATE TEORETICA suprapusa (linie rosie).
  # Folosim aes(y = after_stat(density)) ca histograma sa fie pe scara de
  # densitate, ca sa poata fi comparata direct cu PDF-ul teoretic.
  output$hist_X <- renderPlot({
    d <- date_simulate()
    f_densitate <- densitatea_repartitiei(d$distributie, d$par)
    ggplot(data.frame(x = d$X), aes(x = x)) +
      geom_histogram(aes(y = after_stat(density)),
                     bins = 40, fill = "#2E5A87", color = "white") +
      stat_function(fun = f_densitate, color = "#C1440E", linewidth = 1.1, n = 401) +
      labs(x = "X", y = "Densitate",
           subtitle = "Histograma (empiric) vs. densitatea teoretica (linie rosie)") +
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

  # =========================================================================
  # ===  COMPONENTA 2D  (Z = h(X,Y))  — adaugata de persoana A (Faza 2)  =====
  # =========================================================================

  # --- (2D-0) Parametrii dinamici in functie de modul de generare ---
  # Mod "binormal": cere mu_X, mu_Y, sigma_X, sigma_Y, rho.
  # Mod "independent": cere repartitia + parametrii pentru X si pentru Y.
  output$parametri_2d_ui <- renderUI({
    if (input$mod_2d == "binormal") {
      tagList(
        h4("Parametrii normalei bivariate"),
        fluidRow(
          column(6, numericInput("mu_X", "mu_X", value = 0)),
          column(6, numericInput("mu_Y", "mu_Y", value = 0))
        ),
        fluidRow(
          column(6, numericInput("sigma_X", "sigma_X (>0)", value = 1)),
          column(6, numericInput("sigma_Y", "sigma_Y (>0)", value = 1))
        ),
        sliderInput("rho", "rho (corelatie, -1 < rho < 1):",
                    min = -0.99, max = 0.99, value = 0.5, step = 0.01)
      )
    } else {
      # Mod independent: alegi cate o repartitie pentru X si pentru Y.
      # Pentru simplitate folosim parametri generici p1/p2 per variabila.
      tagList(
        h4("Repartitia lui X"),
        selectInput("dist_X_2d", "X ~", choices = c(
          "Normala" = "normal", "Exponentiala" = "exp",
          "Uniforma" = "unif", "Gamma" = "gamma"), selected = "normal"),
        uiOutput("par_X_2d_ui"),
        h4("Repartitia lui Y"),
        selectInput("dist_Y_2d", "Y ~", choices = c(
          "Normala" = "normal", "Exponentiala" = "exp",
          "Uniforma" = "unif", "Gamma" = "gamma"), selected = "normal"),
        uiOutput("par_Y_2d_ui")
      )
    }
  })

  # Parametrii dinamici pentru X si Y in modul independent (refolosim
  # descrie_parametri din distributii.R, cu id-uri prefixate ca sa nu se
  # ciocneasca intre X si Y).
  output$par_X_2d_ui <- renderUI({
    req(input$mod_2d == "independent", input$dist_X_2d)
    specs <- descrie_parametri(input$dist_X_2d)
    do.call(tagList, lapply(specs, function(p)
      numericInput(paste0("X_", p$id), paste0("X: ", p$eticheta), value = p$valoare)))
  })
  output$par_Y_2d_ui <- renderUI({
    req(input$mod_2d == "independent", input$dist_Y_2d)
    specs <- descrie_parametri(input$dist_Y_2d)
    do.call(tagList, lapply(specs, function(p)
      numericInput(paste0("Y_", p$id), paste0("Y: ", p$eticheta), value = p$valoare)))
  })

  # --- (2D-1) Validarea pentru modul binormal ---
  valideaza_2d <- reactive({
    if (input$mod_2d == "binormal") {
      # Asteptam ca toate campurile sa existe (UI randat).
      if (is.null(input$sigma_X) || is.null(input$sigma_Y) || is.null(input$rho))
        return(NULL)
      valideaza_parametri_2d(input$sigma_X, input$sigma_Y, input$rho)
    } else {
      NULL  # modul independent isi valideaza repartitiile la generare
    }
  })

  output$mesaj_validare_2d <- renderText({
    msg <- valideaza_2d()
    if (is.null(msg)) "" else msg
  })

  # --- (2D-2) Simularea 2D (porneste la butonul Simuleaza 2D) ---
  date_2d <- eventReactive(input$simuleaza_2d, {
    msg <- valideaza_2d()
    validate(need(is.null(msg), msg))

    n <- input$n   # refolosim acelasi slider n din sidebar

    if (input$mod_2d == "binormal") {
      d <- genereaza_pereche("binormal", n,
                             mu_X = input$mu_X, mu_Y = input$mu_Y,
                             sigma_X = input$sigma_X, sigma_Y = input$sigma_Y,
                             rho = input$rho)
      # Marginalele unei normale bivariate sunt N(mu_X, sigma_X^2) si N(mu_Y, sigma_Y^2).
      # Le stocam in formatul uniform (dist + par) ca histogramele sa suprapuna PDF-ul.
      dist_X_used <- "normal"
      par_X_used  <- list(mu = input$mu_X, sigma = input$sigma_X)
      dist_Y_used <- "normal"
      par_Y_used  <- list(mu = input$mu_Y, sigma = input$sigma_Y)
    } else {
      # Strangem parametrii X si Y din campurile prefixate (X_..., Y_...).
      specs_X <- descrie_parametri(input$dist_X_2d)
      par_X <- setNames(lapply(specs_X, function(p) input[[paste0("X_", p$id)]]),
                        vapply(specs_X, function(p) p$id, ""))
      specs_Y <- descrie_parametri(input$dist_Y_2d)
      par_Y <- setNames(lapply(specs_Y, function(p) input[[paste0("Y_", p$id)]]),
                        vapply(specs_Y, function(p) p$id, ""))
      d <- genereaza_pereche("independent", n,
                             dist_X = input$dist_X_2d, par_X = par_X,
                             dist_Y = input$dist_Y_2d, par_Y = par_Y)
      dist_X_used <- input$dist_X_2d
      par_X_used  <- par_X
      dist_Y_used <- input$dist_Y_2d
      par_Y_used  <- par_Y
    }

    # Aplicam transformarea Z = h(X, Y).
    Z <- aplica_transformare_2d(input$transformare_2d, d$X, d$Y)
    list(X = d$X, Y = d$Y, Z = Z,
         dist_X = dist_X_used, par_X = par_X_used,
         dist_Y = dist_Y_used, par_Y = par_Y_used)
  })

  # --- (2D-3) Graficele 2D ---
  # Scatterplot (X, Y): arata vizual corelatia (norul de puncte).
  output$scatter_2d <- renderPlot({
    d <- date_2d()
    ggplot(data.frame(x = d$X, y = d$Y), aes(x = x, y = y)) +
      geom_point(alpha = 0.25, color = "#2E5A87", size = 0.7) +
      labs(x = "X", y = "Y") + theme_minimal(base_size = 12)
  })
  # Histogramele marginale X si Y, cu densitatile teoretice suprapuse (linie rosie).
  # In modul binormal suprapunem N(mu_X, sigma_X^2) si N(mu_Y, sigma_Y^2);
  # in modul independent suprapunem PDF-ul repartitiei alese pentru fiecare.
  output$hist_X_2d <- renderPlot({
    d <- date_2d()
    f_densitate <- densitatea_repartitiei(d$dist_X, d$par_X)
    ggplot(data.frame(x = d$X), aes(x = x)) +
      geom_histogram(aes(y = after_stat(density)),
                     bins = 40, fill = "#2E5A87", color = "white") +
      stat_function(fun = f_densitate, color = "#C1440E", linewidth = 1.0, n = 401) +
      labs(x = "X", y = "Densitate") + theme_minimal(base_size = 11)
  })
  output$hist_Y_2d <- renderPlot({
    d <- date_2d()
    f_densitate <- densitatea_repartitiei(d$dist_Y, d$par_Y)
    ggplot(data.frame(y = d$Y), aes(x = y)) +
      geom_histogram(aes(y = after_stat(density)),
                     bins = 40, fill = "#8B2E5D", color = "white") +
      stat_function(fun = f_densitate, color = "#C1440E", linewidth = 1.0, n = 401) +
      labs(x = "Y", y = "Densitate") + theme_minimal(base_size = 11)
  })
  output$hist_Z_2d <- renderPlot({
    ggplot(data.frame(z = date_2d()$Z), aes(x = z)) +
      geom_histogram(bins = 40, fill = "#1B7340", color = "white") +
      labs(x = "Z = h(X, Y)", y = "") + theme_minimal(base_size = 11)
  })

  # --- (2D-4) Indicatorii 2D (cerinta 5) ---
  output$stats_2d <- renderText({
    d <- date_2d()
    ind <- indicatori_2d(d$X, d$Y, d$Z)
    paste0(
      "Media X: ",  round(ind$media_X, 4), "   Var X: ", round(ind$var_X, 4), "\n",
      "Media Y: ",  round(ind$media_Y, 4), "   Var Y: ", round(ind$var_Y, 4), "\n",
      "Media Z: ",  round(ind$media_Z, 4), "   Var Z: ", round(ind$var_Z, 4), "\n",
      "Covarianta(X,Y): ", round(ind$cov_XY, 4), "\n",
      "Corelatie(X,Y):  ", round(ind$cor_XY, 4)
    )
  })
}