# =============================================================================
# 05_cost.R  --  Functia de cost si compararea strategiilor dupa cost
# =============================================================================
#
# ROLUL ACESTUI FISIER (in arhitectura Problemei 1):
#   01_model.R    -> genereaza o zi (total_curse, curse_suspecte).
#   02_strategii.R-> dat (N, S), decide cate verificam si cate ramin nedetectate.
#   03_simulare.R -> ruleaza anul + repetarile; produce tabelele "an".
#   05_cost.R     -> ACEST fisier. Adauga stratul ECONOMIC: cat costa fiecare
#                    strategie, si care minimizeaza costul mediu.
#
# Acoperire cerinte:
#   - Cerinta ulterioara 1: functia de cost C = c1*verificari + c2*nedetectate,
#     si studiul "care strategie minimizeaza costul mediu".
#
# NOTA DE PROIECTARE: ca si celelalte fisiere, acesta DOAR DEFINESTE functii.
# Lucreaza peste data.frame-ul "an" produs de simuleaza_an() (din 03_simulare.R),
# folosind coloanele 'verificate' si 'nedetectate' returnate de strategiile din
# 02_strategii.R. Nu ruleaza nimic la source().
#
# DEPENDENTE: presupune ca 01, 02, 03 au fost incarcate (pentru blocul de test).
# =============================================================================


# -----------------------------------------------------------------------------
#  ASPECTE TEORETICE — DE CE ACEASTA FUNCTIE DE COST?  (citit de C)
# -----------------------------------------------------------------------------
#
#  ## 1. Functia de cost ca un COMPROMIS (trade-off)
#  Enuntul defineste:
#       C = c1 * (numar verificari) + c2 * (numar incidente nedetectate)
#  Cei doi termeni trag in directii OPUSE:
#    - c1 = costul de a verifica o cursa (timp, personal, resurse de inspectie).
#           Cu cat verifici mai mult, cu atat acest termen creste.
#    - c2 = costul de a RATA un incident (defectiune in trafic, intarzieri,
#           interventie de urgenta). Cu cat verifici mai putin, cu atat scapa
#           mai multe incidente si acest termen creste.
#  Verificarea suplimentara reduce al doilea termen DAR il mareste pe primul.
#  Strategia optima minimizeaza SUMA, nu unul dintre termeni izolat. Aici e
#  inima problemei: detectia devine o intrebare economica cu un optim real.
#
#  ## 2. De ce conteaza RAPORTUL c2/c1, nu valorile absolute
#  Inmultirea ambelor costuri cu aceeasi constanta nu schimba CARE strategie e
#  cea mai ieftina (doar scaleaza costul total). Ce conteaza este raportul
#  c2/c1 = "de cate ori e mai scump sa ratezi un incident decat sa verifici o cursa":
#    - raport MARE (ex. c2/c1 = 500): ratarea e foarte scumpa -> merita sa
#      verifici agresiv; costul SCADE cu cresterea verificarilor.
#    - raport MIC  (ex. c2/c1 = 20):  ratarea e ieftina -> verificarea agresiva
#      e risipa; costul CRESTE cu verificarile.
#  De aceea c1 si c2 sunt PARAMETRI: echipa testeaza mai multe rapoarte si vede
#  cum se schimba strategia castigatoare (asta e studiul cerut).
#
#  ## 3. Cost total vs. cost mediu pe zi
#  Raportam si costul TOTAL pe an, si costul MEDIU pe zi (total / n_zile).
#  Costul mediu pe zi e mai usor de interpretat si de comparat intre scenarii
#  cu numar diferit de zile.
# -----------------------------------------------------------------------------


# =============================================================================
# (A) COSTUL UNUI AN SIMULAT  (cerinta ulterioara 1)
# =============================================================================
#
# Primeste un data.frame "an" (din simuleaza_an) si costurile unitare c1, c2,
# si calculeaza costul total si costul mediu pe zi, plus defalcarea pe cele doua
# componente (cat din cost vine din verificari, cat din nedetectari).
#
# @param df_an  data.frame cu coloanele 'verificate' si 'nedetectate'.
# @param c1     costul unitar al unei verificari (> 0).
# @param c2     costul unitar al unui incident nedetectat (> 0).
# @return       lista cu: cost_total, cost_mediu_zi, cost_verificari,
#               cost_nedetectari (ultimele doua = defalcarea totalului).
calculeaza_cost <- function(df_an, c1, c2) {

  # --- Validare defensiva ---
  necesare <- c("verificate", "nedetectate")
  if (!all(necesare %in% names(df_an))) {
    stop("df_an trebuie sa contina coloanele 'verificate' si 'nedetectate'.")
  }
  if (c1 < 0 || c2 < 0) stop("Costurile c1 si c2 trebuie sa fie >= 0.")

  n_zile <- nrow(df_an)

  # --- Cele doua componente ale costului (totalizate pe an) ---
  total_verificari  <- sum(df_an$verificate)    # cate verificari in tot anul
  total_nedetectate <- sum(df_an$nedetectate)   # cate incidente au scapat

  cost_verificari   <- c1 * total_verificari    # primul termen din formula
  cost_nedetectari  <- c2 * total_nedetectate   # al doilea termen din formula

  # --- Costul total = suma celor doua componente (formula din enunt) ---
  cost_total    <- cost_verificari + cost_nedetectari
  cost_mediu_zi <- cost_total / n_zile

  list(
    cost_total       = cost_total,        # C pe tot anul
    cost_mediu_zi    = cost_mediu_zi,     # C / n_zile (mai usor de comparat)
    cost_verificari  = cost_verificari,   # cat din C vine din verificari
    cost_nedetectari = cost_nedetectari   # cat din C vine din incidente ratate
  )
}


# =============================================================================
# (B) COSTUL MEDIU AL UNEI STRATEGII PE MULTE REPETARI (Monte Carlo)
# =============================================================================
#
# Un singur an e o realizare aleatoare -> costul lui e si el aleator. Repetam
# anul de n_rep ori si raportam costul mediu pe zi (MEDIA, estimarea valorii
# adevarate) impreuna cu ABATEREA STANDARD intre repetari (variabilitatea).
# Acelasi principiu Monte Carlo ca in 03_simulare.R.
#
# Strategia e pasata ca FUNCTIE de (N, S) -> list(verificate, nedetectate, ...),
# exact forma produsa de strategie_fixa / strategie_adaptiva (cu parametrii
# "inchisi"). Astfel functia merge cu orice strategie, fara rescriere.
#
# @param strategie_fn  functie(N, S) -> list(verificate, nedetectate, ...).
# @param p             probabilitatea de incident pe cursa.
# @param c1, c2        costurile unitare.
# @param n_rep         numarul de repetari ale anului (>= 1000 recomandat).
# @param ...           parametri pasati spre simuleaza_an (mu, size, n_zile...).
# @return  lista cu media si abaterea standard a costului mediu pe zi.
cost_mediu_strategie <- function(strategie_fn, p, c1, c2, n_rep = 1000, ...) {

  if (n_rep <= 0) stop("n_rep trebuie sa fie strict pozitiv.")

  # Vector care stocheaza costul mediu pe zi din fiecare an simulat.
  costuri_zi <- numeric(n_rep)

  for (i in seq_len(n_rep)) {
    # Simulam un an nou cu strategia data (functie din 03_simulare.R).
    df_an <- simuleaza_an(strategie_fn, p = p, ...)
    # Ii calculam costul si retinem costul mediu pe zi.
    costuri_zi[i] <- calculeaza_cost(df_an, c1 = c1, c2 = c2)$cost_mediu_zi
  }

  list(
    cost_mediu_zi      = mean(costuri_zi),  # estimarea costului adevarat
    abatere_std        = sd(costuri_zi),    # variabilitatea intre ani
    costuri_brute      = costuri_zi         # toate valorile (utile pentru grafice)
  )
}


# =============================================================================
# (C) COMPARAREA STRATEGIILOR DUPA COST  (cerinta ulterioara 1)
# =============================================================================
#
# Ruleaza cost_mediu_strategie pentru o LISTA de strategii (numite) si intoarce
# un tabel ordonat crescator dupa cost. Strategia din capul tabelului este cea
# care minimizeaza costul mediu - exact ce cere enuntul.
#
# @param strategii  lista NUMITA de functii de strategie. Numele = eticheta
#                   strategiei in tabel. Ex:
#                     list(Fixa_5  = function(N,S) strategie_fixa(N,S,0.05),
#                          Adaptiva = function(N,S) strategie_adaptiva(N,S,...))
# @param p          probabilitatea de incident.
# @param c1, c2     costurile unitare.
# @param n_rep      repetari Monte Carlo per strategie.
# @param ...        parametri pentru simuleaza_an.
# @return  data.frame: strategie, cost_mediu_zi, abatere_std, ordonat crescator.
compara_strategii_cost <- function(strategii, p, c1, c2, n_rep = 1000, ...) {

  if (!is.list(strategii) || is.null(names(strategii))) {
    stop("'strategii' trebuie sa fie o lista NUMITA de functii.")
  }

  nume      <- names(strategii)
  cost_med  <- numeric(length(strategii))
  cost_sd   <- numeric(length(strategii))

  for (k in seq_along(strategii)) {
    rez <- cost_mediu_strategie(strategii[[k]], p = p,
                                c1 = c1, c2 = c2, n_rep = n_rep, ...)
    cost_med[k] <- rez$cost_mediu_zi
    cost_sd[k]  <- rez$abatere_std
  }

  tabel <- data.frame(
    strategie     = nume,
    cost_mediu_zi = cost_med,
    abatere_std   = cost_sd,
    row.names     = NULL
  )

  # Ordonam crescator: strategia cea mai ieftina apare prima.
  tabel[order(tabel$cost_mediu_zi), ]
}


# =============================================================================
# (D) STUDIUL SENSIBILITATII LA RAPORTUL c2/c1
# =============================================================================
#
# Punctul teoretic 2 de mai sus spune ca strategia castigatoare depinde de
# raportul c2/c1. Aceasta functie il studiaza sistematic: fixam c1 = 1 si
# variem c2, comparand costul fiecarei strategii pentru fiecare valoare a lui c2.
# Rezultatul arata la ce PRAG de cost al ratarii o strategie o intrece pe alta -
# un rezultat foarte vizual si convingator pentru documentatie (si bonus).
#
# @param strategii  lista numita de functii de strategie (ca la (C)).
# @param p          probabilitatea de incident.
# @param valori_c2  vector de valori pentru c2 (cu c1 fixat la 1).
# @param c1         costul unei verificari (implicit 1, ca referinta).
# @param n_rep      repetari Monte Carlo per (strategie, c2).
# @param ...        parametri pentru simuleaza_an.
# @return  data.frame "lung": c2, strategie, cost_mediu_zi.
studiu_sensibilitate_cost <- function(strategii, p,
                                      valori_c2 = c(10, 20, 50, 100, 200, 500),
                                      c1 = 1, n_rep = 200, ...) {

  if (!is.list(strategii) || is.null(names(strategii))) {
    stop("'strategii' trebuie sa fie o lista NUMITA de functii.")
  }

  rezultate <- list()  # vom acumula randuri si le combinam la final

  for (c2 in valori_c2) {
    for (k in seq_along(strategii)) {
      rez <- cost_mediu_strategie(strategii[[k]], p = p,
                                  c1 = c1, c2 = c2, n_rep = n_rep, ...)
      rezultate[[length(rezultate) + 1]] <- data.frame(
        c2            = c2,
        strategie     = names(strategii)[k],
        cost_mediu_zi = rez$cost_mediu_zi
      )
    }
  }

  do.call(rbind, rezultate)  # combinam toate randurile intr-un singur data.frame
}


# =============================================================================
# (E) BLOC DE TEST (comentat) - exemplu de folosire pentru main_P1.R
# =============================================================================
# Necesita 01, 02, 03 incarcate.
#
# source("01_model.R"); source("02_strategii.R"); source("03_simulare.R")
# set.seed(2026)
#
# # Definim strategiile ca functii de (N, S):
# strat_fixa_5   <- function(N, S) strategie_fixa(N, S, f = 0.05)
# strat_fixa_20  <- function(N, S) strategie_fixa(N, S, f = 0.20)
# strat_adaptiva <- function(N, S) strategie_adaptiva(N, S, 0.05, 0.30, 300, 500)
#
# # Costul unui singur an:
# an <- simuleaza_an(strat_fixa_5, p = 0.005)
# print(calculeaza_cost(an, c1 = 1, c2 = 200))
#
# # Compararea strategiilor dupa cost (cine minimizeaza costul mediu?):
# strategii <- list(Fixa_5   = strat_fixa_5,
#                   Fixa_20  = strat_fixa_20,
#                   Adaptiva = strat_adaptiva)
# print(compara_strategii_cost(strategii, p = 0.005, c1 = 1, c2 = 200, n_rep = 500))
#
# # Studiul sensibilitatii la raportul c2/c1 (date pentru un grafic la C):
# tab_sens <- studiu_sensibilitate_cost(strategii, p = 0.005, n_rep = 200)
# print(tab_sens)
# # In 04_grafice.R se poate desena: x = c2, y = cost_mediu_zi, color = strategie,
# # ca sa se vada la ce prag de c2 o strategie o intrece pe alta.
# =============================================================================