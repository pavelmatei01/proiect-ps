# =============================================================================
# 03_simulare.R  --  Motorul de simulare pentru Problema 1
# =============================================================================
#
# ROLUL ACESTUI FISIER (in arhitectura Problemei 1):
#   01_model.R    -> genereaza o zi (total_curse, curse_suspecte).
#   02_strategii.R-> dat (N, S), decide cate verificam (V) si cate prindem (D).
#   03_simulare.R -> ACEST fisier. Liantul: ruleaza un AN intreg, aplica
#                    strategiile, calculeaza cei 5 indicatori, repeta de 1000 de
#                    ori (Monte Carlo) si studiaza efectul procentului de
#                    verificare. Produce tabelele pe care le foloseste C la
#                    grafice (04_grafice.R) si la cost (05_cost.R).
#
# Acoperire cerinte:
#   - Cerinta 1     : model de simulare pe un an calendaristic.
#   - Cerinta 5     : cei 5 indicatori per strategie.
#   - Cerinta 7     : efectul cresterii procentului de verificare.
#   - Cerinta ult. 2: repetarea de >= 1000 de ori + variabilitate.
#
# NOTA DE PROIECTARE: ca si celelalte fisiere, acesta DEFINESTE in principal
# functii. Reproductibilitatea (set.seed) se face AICI, intr-un singur loc,
# inainte de a apela functiile de simulare (vezi blocul de la final).
#
# DEPENDENTE: presupune ca 01_model.R si 02_strategii.R au fost deja incarcate
# cu source(). main_P1.R se ocupa de ordinea corecta a source-urilor.
# =============================================================================


# -----------------------------------------------------------------------------
#  ASPECTE TEORETICE — DE CE SIMULAM ASA?  (citit de C pentru documentatie)
# -----------------------------------------------------------------------------
#
#  ## 1. De ce simulare Monte Carlo?
#  Indicatorii care ne intereseaza (probabilitate de detectie, proportii, cost)
#  sunt greu sau imposibil de calculat analitic exact, fiindca implica un lant
#  de variabile aleatoare: N ~ NegBinom, S | N ~ Binomial, D | (N,S,V) ~ Hyper.
#  Metoda Monte Carlo ocoleste calculul analitic: generam multe realizari ale
#  sistemului si ESTIMAM marimile de interes prin medii empirice. Justificarea
#  teoretica e LEGEA NUMERELOR MARI: media empirica a unui esantion converge
#  catre valoarea asteptata reala pe masura ce numarul de repetari creste.
#
#  ## 2. De ce un AN si apoi 1000 de repetari ale anului?
#  - Un singur an (365 de zile) e O SINGURA realizare aleatoare a sistemului.
#    Indicatorii lui sunt ei insisi variabile aleatoare - difera daca re-rulezi.
#  - Repetand intregul an de 1000 de ori obtinem 1000 de valori pentru fiecare
#    indicator. MEDIA lor estimeaza valoarea adevarata; ABATEREA STANDARD intre
#    repetari ne spune cat de STABILA / de incredere este estimarea.
#    Aceasta este distinctia ceruta explicit: "rezultate medii vs. variabilitate".
#
#  ## 3. Subtilitate la proportia detectata/nedetectata (ipoteza de declarat)
#  Proportia detectata intr-o zi = D / S. Daca S = 0 (zi fara incidente),
#  raportul e 0/0 = NEDEFINIT. Decizia noastra: calculam proportiile medii DOAR
#  pe zilele cu S >= 1. A trata o zi fara incidente drept "100% detectat" sau
#  "0% detectat" ar denatura artificial media. Aceasta ipoteza trebuie declarata
#  in documentatie (conform regulii 7 din enunt).
#
#  ## 4. Indicatorul de eficienta (definit de echipa - cerinta 5)
#  Definim:   eficienta = (total incidente detectate) / (total verificari)
#  Interpretare: cate incidente prinzi, in medie, per cursa inspectata. O
#  strategie e "eficienta" daca prinde MULT verificand PUTIN. Alternativ se
#  poate raporta la cost (vezi 05_cost.R), dar acest raport e cel mai intuitiv
#  indicator pur de detectie.
# -----------------------------------------------------------------------------


# =============================================================================
# (A) SIMULAREA UNUI SINGUR AN, PENTRU O STRATEGIE DATA
# =============================================================================
#
# Generam `n_zile` zile independente. Pentru fiecare zi:
#   1) generam (total_curse, curse_suspecte) cu genereaza_zi()  [din 01_model.R]
#   2) aplicam strategia primita ca argument -> (verificate, detectate, nedetectate)
#   3) stocam totul intr-un rand de tabel.
#
# Strategia e pasata ca FUNCTIE (programare functionala): astfel acelasi motor
# de simulare merge cu strategie_fixa, strategie_adaptiva sau orice strategie
# viitoare, fara sa rescriem nimic. Functia primita trebuie sa ia (N, S) si sa
# intoarca o lista cu $verificate, $detectate, $nedetectate.
#
# @param strategie_fn  functie(N, S) -> list(verificate, detectate, nedetectate)
# @param p             probabilitatea de incident pe cursa
# @param n_zile        numarul de zile simulate (implicit 365)
# @param model,mu,size parametrii modelului pentru numarul de curse (01_model.R)
# @return  un data.frame cu o linie pe zi si coloanele cerute de enunt.
simuleaza_an <- function(strategie_fn, p,
                         n_zile = 365,
                         model = "nbinom", mu = 400, size = 20) {

  # --- Validare ---
  if (!is.function(strategie_fn)) stop("strategie_fn trebuie sa fie o functie.")
  if (n_zile <= 0)                stop("n_zile trebuie sa fie strict pozitiv.")

  # Pre-alocam vectori pentru viteza (mai rapid decat sa "crestem" un data.frame
  # rand cu rand intr-un for). Fiecare vector are cate o pozitie per zi.
  total_curse    <- integer(n_zile)
  curse_normale  <- integer(n_zile)
  curse_suspecte <- integer(n_zile)
  verificate     <- integer(n_zile)
  detectate      <- integer(n_zile)
  nedetectate    <- integer(n_zile)

  # --- Bucla pe zile ---
  for (zi in seq_len(n_zile)) {

    # 1) Ce se intampla "in realitate" in ziua asta (model, 01_model.R).
    date_zi <- genereaza_zi(p = p, model = model, mu = mu, size = size)
    N <- date_zi$total_curse
    S <- date_zi$curse_suspecte

    # 2) Ce vede operatorul prin strategia de verificare (02_strategii.R).
    rez <- strategie_fn(N, S)

    # 3) Stocam.
    total_curse[zi]    <- N
    curse_normale[zi]  <- date_zi$curse_normale
    curse_suspecte[zi] <- S
    verificate[zi]     <- rez$verificate
    detectate[zi]      <- rez$detectate
    nedetectate[zi]    <- rez$nedetectate
  }

  # Returnam un data.frame "tidy" cu exact coloanele cerute de enunt (cerinta 1).
  data.frame(
    zi             = seq_len(n_zile),
    total_curse    = total_curse,
    curse_normale  = curse_normale,
    curse_suspecte = curse_suspecte,
    verificate     = verificate,
    detectate      = detectate,
    nedetectate    = nedetectate
  )
}


# =============================================================================
# (B) CEI 5 INDICATORI DINTR-UN AN SIMULAT  (cerinta 5)
# =============================================================================
#
# Primeste un data.frame produs de simuleaza_an() si calculeaza cei 5 indicatori
# ceruti. Vezi sectiunea teoretica de mai sus pentru justificarea fiecaruia.
#
# @param df_an  data.frame cu o linie pe zi (output din simuleaza_an).
# @return  o lista numita cu cei 5 indicatori.
calculeaza_indicatori <- function(df_an) {

  # --- 1) Probabilitatea empirica de a detecta CEL PUTIN un incident pe zi ---
  # Estimata ca frecventa relativa a zilelor cu detectate >= 1.
  # mean() peste un vector logic = proportia valorilor TRUE (legea nr. mari).
  prob_detectie_zi <- mean(df_an$detectate >= 1)

  # --- 2) & 3) Proportiile detectata / nedetectata, DOAR pe zile cu S >= 1 ---
  # Excludem zilele fara incidente (S=0) ca sa evitam raportul 0/0 (vezi teorie).
  zile_cu_incident <- df_an[df_an$curse_suspecte >= 1, ]

  if (nrow(zile_cu_incident) > 0) {
    # Proportia detectata pe fiecare astfel de zi, apoi media lor.
    prop_det <- mean(zile_cu_incident$detectate   / zile_cu_incident$curse_suspecte)
    prop_ned <- mean(zile_cu_incident$nedetectate / zile_cu_incident$curse_suspecte)
  } else {
    # Caz limita extrem (p foarte mic + an scurt): niciun incident tot anul.
    prop_det <- NA_real_
    prop_ned <- NA_real_
  }

  # --- 4) Numarul mediu de verificari pe zi ---
  nr_mediu_verificari <- mean(df_an$verificate)

  # --- 5) Indicatorul de eficienta: detectate totale / verificari totale ---
  # Folosim TOTALURILE pe an (nu media raporturilor zilnice) ca sa evitam din
  # nou impartirea la zero in zilele cu 0 verificari, si fiindca raportul de
  # totaluri are interpretare directa: "incidente prinse per cursa inspectata".
  total_verif <- sum(df_an$verificate)
  eficienta <- if (total_verif > 0) sum(df_an$detectate) / total_verif else NA_real_

  list(
    prob_detectie_zi    = prob_detectie_zi,    # P(D >= 1 intr-o zi)
    prop_detectata      = prop_det,            # medie pe zile cu S>=1
    prop_nedetectata    = prop_ned,            # medie pe zile cu S>=1
    nr_mediu_verificari = nr_mediu_verificari, # medie pe an
    eficienta           = eficienta            # detectate/verificari (pe an)
  )
}


# =============================================================================
# (C) REPETAREA MONTE CARLO: ruleaza anul de N_rep ori  (cerinta ulterioara 2)
# =============================================================================
#
# Repeta intreaga simulare a anului de `n_rep` ori (implicit 1000) pentru o
# strategie data, colectand cei 5 indicatori la fiecare repetare. Apoi raporteaza
# MEDIA (estimarea valorii adevarate) si ABATEREA STANDARD (variabilitatea
# estimarii) - exact distinctia ceruta de enunt.
#
# @param strategie_fn  functia de strategie (N,S) -> list(...)
# @param p             probabilitatea de incident
# @param n_rep         numarul de repetari ale anului (>= 1000 recomandat)
# @param ...           parametri pasati mai departe spre simuleaza_an (mu, size...)
# @return  un data.frame cu, pentru fiecare indicator, media si abaterea standard.
repeta_simulare <- function(strategie_fn, p, n_rep = 1000, ...) {

  if (n_rep <= 0) stop("n_rep trebuie sa fie strict pozitiv.")

  # Matrice care va stoca cei 5 indicatori pentru fiecare dintre cele n_rep ani.
  # Linii = repetari, coloane = indicatori.
  nume_indicatori <- c("prob_detectie_zi", "prop_detectata", "prop_nedetectata",
                       "nr_mediu_verificari", "eficienta")
  rezultate <- matrix(NA_real_, nrow = n_rep, ncol = length(nume_indicatori),
                      dimnames = list(NULL, nume_indicatori))

  # Bucla Monte Carlo: la fiecare iteratie simulam un AN NOU si ii luam indicatorii.
  for (i in seq_len(n_rep)) {
    df_an <- simuleaza_an(strategie_fn, p = p, ...)
    ind   <- calculeaza_indicatori(df_an)
    rezultate[i, ] <- c(ind$prob_detectie_zi, ind$prop_detectata,
                        ind$prop_nedetectata, ind$nr_mediu_verificari,
                        ind$eficienta)
  }

  # Rezumam: media si abaterea standard pe coloane.
  # na.rm = TRUE ignora eventualele repetari cu NA (zile fara niciun incident).
  data.frame(
    indicator = nume_indicatori,
    medie     = colMeans(rezultate, na.rm = TRUE),
    abatere_std = apply(rezultate, 2, sd, na.rm = TRUE),
    row.names = NULL
  )
}


# =============================================================================
# (D) STUDIUL EFECTULUI PROCENTULUI DE VERIFICARE  (cerinta 7)
# =============================================================================
#
# Pentru mai multe procente fixe de verificare (1%, 5%, 10%, 20%, 30%),
# estimam probabilitatea de detectie (cel putin un incident pe zi), ca sa vedem
# cum creste detectia cu efortul. Ilustreaza randamentul descrescator: dublarea
# verificarilor NU dubleaza neaparat detectia.
#
# Pentru fiecare procent rulam repeta_simulare (cu strategie fixa la acel procent)
# si extragem media probabilitatii de detectie.
#
# @param p          probabilitatea de incident
# @param procente   vector de procente de verificare de testat
# @param n_rep      repetari Monte Carlo per procent (mai mic decat 1000 e ok aici,
#                   fiindca testam multe procente; ajustati dupa timpul disponibil)
# @return  data.frame cu procentul si probabilitatea medie de detectie estimata.
studiu_procent_verificare <- function(p,
                                      procente = c(0.01, 0.05, 0.10, 0.20, 0.30),
                                      n_rep = 200, ...) {

  prob_detectie <- numeric(length(procente))

  for (k in seq_along(procente)) {
    f <- procente[k]
    # Construim "din mers" o strategie fixa la procentul f. Inchidem f intr-o
    # functie de un singur argument-pereche (N,S) - exact ce asteapta motorul.
    strat_f <- function(N, S) {force(f); strategie_fixa(N, S, f = f) } 

    rez <- repeta_simulare(strat_f, p = p, n_rep = n_rep, ...)
    # Extragem media indicatorului "prob_detectie_zi" din tabelul rezumat.
    prob_detectie[k] <- rez$medie[rez$indicator == "prob_detectie_zi"]
  }

  data.frame(
    procent_verificare = procente,
    prob_detectie      = prob_detectie
  )
}


# =============================================================================
# (E) RULAREA PRINCIPALA (comentata) — exemplul de folosire pentru main_P1.R
# =============================================================================
# Lasam blocul comentat ca source("03_simulare.R") sa NU ruleze nimic automat.
# main_P1.R va contine o varianta a acestui bloc, dupa ce face source la 01 si 02.
#
# # --- 0) Incarca dependentele (in main_P1.R) ---
# source("01_model.R")
# source("02_strategii.R")
#
# # --- 1) Reproductibilitate: UN SINGUR set.seed, aici ---
# set.seed(2026)
#
# # --- 2) Definim cele doua strategii ca functii de (N, S) ---
# # Inchidem parametrii ca sa ramana doar (N, S) - forma ceruta de motor.
# strat_fixa_5   <- function(N, S) strategie_fixa(N, S, f = 0.05)
# strat_adaptiva <- function(N, S) strategie_adaptiva(N, S,
#                                                     f_min = 0.05, f_max = 0.30,
#                                                     N_min = 300,  N_max = 500)
#
# # --- 3) Un an pentru fiecare strategie (date pentru graficele lui C) ---
# an_fix    <- simuleaza_an(strat_fixa_5,   p = 0.005)
# an_adapt  <- simuleaza_an(strat_adaptiva, p = 0.005)
# head(an_fix)
#
# # --- 4) Cei 5 indicatori dintr-un an ---
# print(calculeaza_indicatori(an_fix))
# print(calculeaza_indicatori(an_adapt))
#
# # --- 5) Repetare Monte Carlo (1000 de ani) - medie vs. variabilitate ---
# rezumat_fix   <- repeta_simulare(strat_fixa_5,   p = 0.005, n_rep = 1000)
# rezumat_adapt <- repeta_simulare(strat_adaptiva, p = 0.005, n_rep = 1000)
# print(rezumat_fix)
# print(rezumat_adapt)
#
# # --- 6) Cele 3 scenarii de p cerute (cerinta 3) ---
# for (p_test in c(0.001, 0.005, 0.02)) {
#   cat("\n=== p =", p_test, "===\n")
#   print(repeta_simulare(strat_fixa_5, p = p_test, n_rep = 500))
# }
#
# # --- 7) Efectul procentului de verificare (cerinta 7) ---
# tabel_procente <- studiu_procent_verificare(p = 0.005, n_rep = 200)
# print(tabel_procente)
# =============================================================================