# =============================================================================
# ui.R  --  Interfata aplicatiei Shiny (Problema 2)
# =============================================================================
#
# ROLUL ACESTUI FISIER:
#   Defineste DOAR ce VEDE utilizatorul (controale, taburi, locuri pentru grafice
#   si numere). Nicio logica de calcul aici — aceea e in server.R si in R/.
#   Fisierul ramane "subtire": doar descrie structura interfetei.
#
# STRUCTURA ALEASA (acopera cerinta 1 din enunt):
#   - un panou lateral (sidebar) cu TOATE controalele de intrare;
#   - un panou principal cu TABURI pentru rezultate.
#   Layout-ul sidebar + taburi e standard, clar, si usor de folosit de cineva
#   care nu cunoaste codul (cerinta 2: interfata clara).
#
# DECIZIE DE PROIECTARE — DE CE TABURI?
#   Enuntul are doua mari sectiuni: transformari 1D (Y=g(X)) si transformari 2D
#   (Z=h(X,Y)). Le punem in taburi separate ca interfata sa nu fie aglomerata.
#   In plus, la 1D enuntul recomanda histogramele X si Y "una langa alta sau in
#   taburi" (cerinta 6) — noi le punem una langa alta in acelasi tab.
#
# DECIZIE — DE CE BUTON "Simuleaza"?
#   Cerinta 4 (Shiny) cere un buton de tip Simuleaza, ca simularea sa NU se
#   reactualizeze haotic la fiecare miscare de slider. Folosim actionButton +
#   eventReactive in server, ca tot calculul sa porneasca DOAR la apasarea
#   butonului. (Detaliile reactive sunt in server.R.)
#
# NOTA: parametrii repartitiilor (mu, sigma, lambda, a, b, alpha, theta) ar
# trebui sa apara/dispara in functie de repartitia aleasa. Acel comportament
# dinamic (uiOutput) il implementam cand legam logica reala. Deocamdata punem
# toti parametrii ca un placeholder, ca scheletul sa fie complet si sa ruleze.
# =============================================================================

ui <- fluidPage(

  # --- Titlul aplicatiei ---
  titlePanel("Transformari de variabile aleatoare continue"),

  # --- Layout: bara laterala (controale) + zona principala (rezultate) ---
  sidebarLayout(

    # =======================================================================
    # BARA LATERALA — toate controalele de intrare
    # =======================================================================
    sidebarPanel(
      width = 5,

      # --- Sectiunea: alegerea repartitiei lui X (cerinta 1 + 2) ---
      h4("1. Repartitia lui X"),
      selectInput(
        inputId = "distributie",
        label   = "Alege repartitia:",
        choices = c(
          "Normala"      = "normal",
          "Exponentiala" = "exp",
          "Uniforma"     = "unif",
          "Gamma"        = "gamma"
        ),
        selected = "normal"
      ),

      # --- Sectiunea: parametrii repartitiei (cerinta 2) ---
      # Campurile se genereaza DINAMIC in functie de repartitia aleasa:
      # server.R umple acest uiOutput cu campurile corecte (mu/sigma pentru
      # normala, lambda pentru exponentiala etc.) folosind descrie_parametri().
      h4("2. Parametrii repartitiei"),
      uiOutput("parametri_ui"),

      # --- Sectiunea: dimensiunea esantionului n (cerinta 1) ---
      h4("3. Dimensiunea esantionului"),
      sliderInput("n", "n (numar de valori generate):",
                  min = 100, max = 50000, value = 5000, step = 100),

      # --- Sectiunea: alegerea transformarii g (cerinta 1 + 4) ---
      h4("4. Transformarea Y = g(X)"),
      selectInput(
        inputId = "transformare",
        label   = "Alege transformarea:",
        choices = c(
          "g(x) = x^2"            = "patrat",
          "g(x) = |x|"            = "modul",
          "g(x) = log(x)"         = "log",
          "g(x) = e^x"            = "exp",
          "g(x) = 1/(1+e^(-x))  (sigmoid)" = "sigmoid"
        ),
        selected = "patrat"
      ),

      # --- Butonul Simuleaza (cerinta 4 Shiny) ---
      # Tot calculul porneste DOAR la apasarea acestui buton (vezi server.R).
      br(),
      actionButton("simuleaza", "Simuleaza",
                   class = "btn-primary", width = "100%"),

      # --- Zona de mesaje de validare (cerinta 3 Shiny) ---
      # Aici vom afisa erori prietenoase (ex: "sigma trebuie > 0").
      br(), br(),
      div(style = "color:#b00; font-weight:bold;",
          textOutput("mesaj_validare"))
    ),

    # =======================================================================
    # ZONA PRINCIPALA — rezultatele, organizate in taburi
    # =======================================================================
    mainPanel(
      width = 7,
      tabsetPanel(
        id = "tab_principal",

        # --- TAB 1: Transformari 1D (Y = g(X)) ---
        tabPanel(
          title = "Transformare 1D",

          # Histogramele X si Y una langa alta (cerinta 6).
          # fluidRow + doua coloane = afisare alaturata.
          br(),
          fluidRow(
            column(6,
                   h4("Distributia lui X"),
                   plotOutput("hist_X", height = "300px")),
            column(6,
                   h4("Distributia lui Y = g(X)"),
                   plotOutput("hist_Y", height = "300px"))
          ),

          # Indicatorii numerici pentru X si Y (cerintele 3 si 5).
          fluidRow(
            column(6,
                   h4("Indicatori X"),
                   verbatimTextOutput("stats_X")),
            column(6,
                   h4("Indicatori Y"),
                   verbatimTextOutput("stats_Y"))
          ),

          # Avertisment de la transformare (ex: log pe valori <= 0).
          br(),
          div(style = "color:#b00; font-weight:bold;",
              textOutput("avertisment_transf")),

          # Interpretarea automata (cerinta 7).
          br(),
          h4("Interpretare automata"),
          verbatimTextOutput("interpretare")
        ),

        # --- TAB 2: Transformari 2D (Z = h(X,Y)) ---
        # Controalele 2D stau IN tab (nu in sidebarul comun), fiindca au alti
        # parametri decat 1D si ar aglomera bara laterala. Layout pe doua zone:
        # stanga = controale 2D, dreapta = rezultate (scatterplot + histograme).
        tabPanel(
          title = "Transformare 2D",
          br(),
          fluidRow(
            # --- Coloana de controale 2D ---
            column(4,
              h4("Mod de generare (X, Y)"),
              selectInput("mod_2d", "Cum generam perechea:",
                choices = c(
                  "Independente (repartitii alese)" = "independent",
                  "Normala bivariata (cu rho)"      = "binormal"
                ),
                selected = "binormal"
              ),

              # Parametrii apar dinamic in functie de modul ales (renderUI).
              uiOutput("parametri_2d_ui"),

              h4("Transformarea Z = h(X, Y)"),
              selectInput("transformare_2d", "Alege transformarea:",
                choices = c(
                  "h = X + Y"            = "suma",
                  "h = X - Y"            = "diferenta",
                  "h = X * Y"            = "produs",
                  "h = sqrt(X^2 + Y^2)"  = "norma"
                ),
                selected = "suma"
              ),

              br(),
              actionButton("simuleaza_2d", "Simuleaza 2D",
                           class = "btn-primary", width = "100%"),
              br(), br(),
              div(style = "color:#b00; font-weight:bold;",
                  textOutput("mesaj_validare_2d"))
            ),

            # --- Coloana de rezultate 2D ---
            column(8,
              h4("Scatterplot (X, Y)"),
              plotOutput("scatter_2d", height = "260px"),
              fluidRow(
                column(6, h5("Histograma X"), plotOutput("hist_X_2d", height = "200px")),
                column(6, h5("Histograma Y"), plotOutput("hist_Y_2d", height = "200px"))
              ),
              h5("Histograma Z = h(X, Y)"),
              plotOutput("hist_Z_2d", height = "200px"),
              h4("Indicatori 2D"),
              verbatimTextOutput("stats_2d")
            )
          )
        )
      )
    )
  )
)