# =============================================================================
# 04_grafice.R  --  Reprezentari grafice pentru Problema 1
# =============================================================================
#
# ROLUL ACESTUI FISIER (in arhitectura Problemei 1):
#   01_model.R    -> genereaza o zi (total_curse, curse_suspecte).
#   02_strategii.R-> dat (N, S), decide cate verificam si cate prindem.
#   03_simulare.R -> ruleaza anul + repetarile; produce TABELELE de mai jos.
#   04_grafice.R  -> ACEST fisier. Transforma tabelele in grafice ggplot2.
#
# Acoperire cerinte:
#   - Cerinta 6 : cele 4 grafice obligatorii (histograme + comparativ + evolutie).
#   - Cerinta 7 : graficul probabilitatii de detectie vs. procentul de verificare.
#
# NOTA DE PROIECTARE: ca si celelalte fisiere, acesta DOAR DEFINESTE functii care
# primesc un data.frame si intorc un obiect ggplot. Nu deseneaza nimic la
# source(). Asa main_P1.R poate genera graficele controlat, in ordinea dorita,
# si le poate salva pe disc. Fiecare functie intoarce obiectul ggplot (NU il
# afiseaza), ca sa poata fi combinat cu patchwork sau salvat cu ggsave.
#
# CONTRACTUL DE DATE (ce coloane asteptam de la 03_simulare.R):
#   - tabel "an" (din simuleaza_an): zi, total_curse, curse_normale,
#     curse_suspecte, verificate, detectate, nedetectate  -> o linie pe zi.
#   - tabel "indicatori" (din repeta_simulare): indicator, medie, abatere_std.
#
# DEPENDENTE: ggplot2 (obligatoriu), patchwork (optional, pentru combinarea
# graficelor). main_P1.R se ocupa de library(...).
# =============================================================================


# -----------------------------------------------------------------------------
#  ASPECTE GRAFICE — DE CE ACESTE ALEGERI?  (citit de C pentru documentatie)
# -----------------------------------------------------------------------------
#
#  ## 1. De ce histograme cu binwidth = 1 pentru incidente?
#  Numarul de incidente pe zi (S) si numarul detectate (D) sunt variabile
#  DISCRETE care iau valori intregi mici (tipic 0..10). O histograma cu
#  binwidth = 1 pune fiecare valoare intreaga in propriul "cos", deci bara de
#  deasupra lui k arata exact CATE zile au avut k incidente. Asta reflecta
#  fidel distributia discreta - un binwidth mai mare ar amesteca valori si ar
#  ascunde forma reala (de ex. importanta lui S=0).
#
#  ## 2. De ce comparam strategiile pe indicatori, cu bare de eroare?
#  03_simulare.R repeta anul de 1000 de ori si raporteaza, pentru fiecare
#  indicator, MEDIA si ABATEREA STANDARD intre repetari. Graficul comparativ
#  arata media ca inaltime a barei si abaterea standard ca bara de eroare
#  (medie +/- 1 sd). Astfel se vede nu doar CARE strategie e mai buna in medie,
#  ci si CAT de stabila e - exact distinctia "rezultate medii vs. variabilitate".
#
#  ## 3. De ce evolutia zilnica suprapune suspecte si detectate?
#  Suprapunand cele doua serii pe acelasi grafic, distanta verticala dintre ele
#  vizualizeaza direct incidentele NEDETECTATE (suspecte - detectate). Cititorul
#  "vede" cat scapa verificarea, zi de zi - mesajul central al proiectului.
# -----------------------------------------------------------------------------


# =============================================================================
# PALETA DE CULORI COMUNA (consistenta vizuala intre toate graficele)
# =============================================================================
# Definim o singura paleta si o refolosim peste tot: incidentele "reale" intr-o
# culoare calda (atentie), detectate in verde (succes), nedetectate evidentiate.
# Consistenta culorilor ajuta cititorul sa lege graficele intre ele.
.culori <- list(
  suspecte    = "#C1440E",  # caramiziu - incidentele reale (ce vrem sa prindem)
  detectate   = "#1B7340",  # verde     - incidentele prinse (succes)
  nedetectate = "#E08A1E",  # portocaliu- incidentele scapate (risc)
  fixa        = "#2E5A87",  # albastru  - strategia fixa
  adaptiva    = "#8B2E5D"   # purpuriu  - strategia adaptiva
)

# Tema comuna minimalista: fundal curat, fara zgomot vizual, text lizibil.
# O aplicam la fiecare grafic ca sa aiba toate acelasi aspect ingrijit.
.tema_proiect <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = 13),
      plot.subtitle = ggplot2::element_text(color = "grey35", size = 10),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()  # mai putine linii = mai curat
    )
}


# =============================================================================
# (A) HISTOGRAMA NUMARULUI DE INCIDENTE PE ZI  (cerinta 6, grafic 1)
# =============================================================================
#
# Arata distributia variabilei S = curse_suspecte pe parcursul anului: cate zile
# au avut 0 incidente, cate 1, cate 2 etc. Pentru p mic, ne asteptam ca bara de
# la 0 (sau valori mici) sa domine - confirma vizual ca evenimentul e "rar".
#
# @param df_an  data.frame din simuleaza_an() (are coloana curse_suspecte).
# @return       obiect ggplot.
grafic_hist_suspecte <- function(df_an) {

  # --- Validare defensiva: coloana asteptata exista? ---
  if (!"curse_suspecte" %in% names(df_an)) {
    stop("df_an trebuie sa contina coloana 'curse_suspecte'.")
  }

  ggplot2::ggplot(df_an, ggplot2::aes(x = curse_suspecte)) +
    # binwidth = 1: fiecare valoare intreaga = o bara (vezi nota teoretica 1).
    # boundary = -0.5 centreaza barele pe intregi (0,1,2,...), nu intre ele.
    ggplot2::geom_histogram(binwidth = 1, boundary = -0.5,
                            fill = .culori$suspecte, color = "white") +
    ggplot2::labs(
      title    = "Distributia incidentelor pe zi",
      subtitle = "Cate zile au avut 0, 1, 2, ... incidente intr-un an",
      x = "Numar de incidente intr-o zi (S)",
      y = "Numar de zile"
    ) +
    .tema_proiect()
}


# =============================================================================
# (B) HISTOGRAMA NUMARULUI DE INCIDENTE DETECTATE  (cerinta 6, grafic 2)
# =============================================================================
#
# Aceeasi idee, dar pentru D = detectate. Comparand-o vizual cu histograma de
# mai sus se vede "deplasarea spre stanga": detectatele sunt sistematic mai
# putine decat suspectele, fiindca verificarea prinde doar o fractiune.
#
# @param df_an  data.frame din simuleaza_an() (are coloana detectate).
# @return       obiect ggplot.
grafic_hist_detectate <- function(df_an) {

  if (!"detectate" %in% names(df_an)) {
    stop("df_an trebuie sa contina coloana 'detectate'.")
  }

  ggplot2::ggplot(df_an, ggplot2::aes(x = detectate)) +
    ggplot2::geom_histogram(binwidth = 1, boundary = -0.5,
                            fill = .culori$detectate, color = "white") +
    ggplot2::labs(
      title    = "Distributia incidentelor DETECTATE pe zi",
      subtitle = "Comparati cu histograma incidentelor reale: detectatele sunt mai putine",
      x = "Numar de incidente detectate intr-o zi (D)",
      y = "Numar de zile"
    ) +
    .tema_proiect()
}


# =============================================================================
# (C) GRAFIC COMPARATIV INTRE STRATEGII  (cerinta 6, grafic 3)
# =============================================================================
#
# Compara cele doua strategii pe indicatorii calculati de repeta_simulare().
# Primeste cele doua tabele rezumat (fiecare cu: indicator, medie, abatere_std),
# le combina, si deseneaza bare grupate cu bare de eroare (medie +/- 1 sd).
#
# Ne concentram pe indicatorii direct comparabili ca proportii/probabilitati;
# 'nr_mediu_verificari' are alta scara (zeci), deci il lasam pe un grafic separat
# daca e nevoie - aici comparam indicatorii in [0,1] ca sa fie pe aceeasi scara.
#
# @param rezumat_fix    data.frame din repeta_simulare() pentru strategia fixa.
# @param rezumat_adapt  data.frame din repeta_simulare() pentru strategia adaptiva.
# @param indicatori     care indicatori sa afisam (implicit cei in [0,1]).
# @return               obiect ggplot.
grafic_comparativ_strategii <- function(rezumat_fix, rezumat_adapt,
                                        indicatori = c("prob_detectie_zi",
                                                       "prop_detectata",
                                                       "prop_nedetectata",
                                                       "eficienta")) {

  # --- Validare: ambele tabele au coloanele asteptate? ---
  necesare <- c("indicator", "medie", "abatere_std")
  for (tab in list(rezumat_fix, rezumat_adapt)) {
    if (!all(necesare %in% names(tab))) {
      stop("Tabelele rezumat trebuie sa aiba coloanele: indicator, medie, abatere_std.")
    }
  }

  # --- Etichetam fiecare tabel cu numele strategiei si le combinam ---
  rezumat_fix$strategie   <- "Fixa"
  rezumat_adapt$strategie <- "Adaptiva"
  df <- rbind(rezumat_fix, rezumat_adapt)

  # --- Pastram doar indicatorii ceruti (cei pe scara [0,1]) ---
  df <- df[df$indicator %in% indicatori, ]

  # Ordonam factorii ca sa apara intr-o ordine logica pe axa.
  df$indicator <- factor(df$indicator, levels = indicatori)
  df$strategie <- factor(df$strategie, levels = c("Fixa", "Adaptiva"))

  ggplot2::ggplot(df, ggplot2::aes(x = indicator, y = medie, fill = strategie)) +
    # Bare grupate (position_dodge): fixa si adaptiva una langa alta per indicator.
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8),
                      width = 0.7) +
    # Bare de eroare = medie +/- 1 abatere standard (variabilitatea intre repetari).
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = medie - abatere_std, ymax = medie + abatere_std),
      position = ggplot2::position_dodge(width = 0.8),
      width = 0.2, color = "grey25"
    ) +
    ggplot2::scale_fill_manual(values = c(Fixa = .culori$fixa,
                                          Adaptiva = .culori$adaptiva)) +
    ggplot2::labs(
      title    = "Comparatie intre strategii",
      subtitle = "Bara = media pe 1000 de repetari; segmentul = +/- 1 abatere standard",
      x = "Indicator", y = "Valoare medie", fill = "Strategie"
    ) +
    .tema_proiect() +
    # Rotim etichetele de pe axa X ca sa nu se suprapuna (nume lungi).
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))
}


# =============================================================================
# (D) EVOLUTIA ZILNICA: SUSPECTE vs. DETECTATE  (cerinta 6, grafic 4)
# =============================================================================
#
# Serie temporala pe parcursul anului: doua linii suprapuse - incidentele reale
# (suspecte) si cele detectate. Distanta dintre linii = incidentele scapate.
# Pentru lizibilitate, oferim optiunea de a netezi cu o medie mobila, fiindca
# datele zilnice brute sunt foarte "zimtate".
#
# IMPLEMENTARE TEHNICA: transformam tabelul din format "lat" (coloane separate
# suspecte/detectate) in format "lung" (o coloana 'tip', o coloana 'valoare'),
# ca ggplot sa poata desena cele doua serii cu o singura comanda si o legenda.
# Folosim doar functii din baza R (reshape) ca sa nu adaugam dependinte.
#
# @param df_an     data.frame din simuleaza_an().
# @param fereastra latimea mediei mobile (zile). 1 = date brute, fara netezire.
# @return          obiect ggplot.
grafic_evolutie_zilnica <- function(df_an, fereastra = 7) {

  necesare <- c("zi", "curse_suspecte", "detectate")
  if (!all(necesare %in% names(df_an))) {
    stop("df_an trebuie sa contina: zi, curse_suspecte, detectate.")
  }
  if (fereastra < 1) stop("fereastra trebuie sa fie >= 1.")

  # --- Functie ajutatoare: medie mobila simpla (centrata) ---
  # Netezeste seria ca tendinta sa fie vizibila. fereastra=1 -> identitate.
  medie_mobila <- function(x, k) {
    if (k <= 1) return(x)
    n <- length(x)
    rez <- numeric(n)
    semi <- floor(k / 2)
    for (i in seq_len(n)) {
      jos <- max(1, i - semi)
      sus <- min(n, i + semi)
      rez[i] <- mean(x[jos:sus])   # media valorilor din fereastra din jurul lui i
    }
    rez
  }

  # --- Construim tabelul in format "lung" pentru cele doua serii ---
  df_lung <- rbind(
    data.frame(zi = df_an$zi,
               valoare = medie_mobila(df_an$curse_suspecte, fereastra),
               tip = "Incidente reale"),
    data.frame(zi = df_an$zi,
               valoare = medie_mobila(df_an$detectate, fereastra),
               tip = "Incidente detectate")
  )
  df_lung$tip <- factor(df_lung$tip,
                        levels = c("Incidente reale", "Incidente detectate"))

  # Subtitlul descrie daca seria e bruta sau netezita.
  sub <- if (fereastra > 1) {
    paste0("Netezit cu medie mobila pe ", fereastra,
           " zile; distanta dintre linii = incidente scapate")
  } else {
    "Date zilnice brute; distanta dintre linii = incidente scapate"
  }

  ggplot2::ggplot(df_lung, ggplot2::aes(x = zi, y = valoare, color = tip)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_color_manual(values = c("Incidente reale" = .culori$suspecte,
                                           "Incidente detectate" = .culori$detectate)) +
    ggplot2::labs(
      title    = "Evolutia zilnica: incidente reale vs. detectate",
      subtitle = sub,
      x = "Ziua din an", y = "Numar de incidente", color = NULL
    ) +
    .tema_proiect()
}


# =============================================================================
# (E) EFECTUL PROCENTULUI DE VERIFICARE  (cerinta 7)
# =============================================================================
#
# Reprezinta cum creste probabilitatea de detectie cand marim procentul de
# verificare. Primeste tabelul produs de studiu_procent_verificare() din
# 03_simulare.R (coloane: procent_verificare, prob_detectie).
#
# Forma curbei e mesajul cheie: tipic concava (randament descrescator) - primele
# procente aduc cel mai mare castig, apoi fiecare procent in plus aduce tot mai
# putin. Argument direct pentru concluzia practica (cerinta 8).
#
# @param df_procente  data.frame din studiu_procent_verificare().
# @return             obiect ggplot.
grafic_efect_procent <- function(df_procente) {

  necesare <- c("procent_verificare", "prob_detectie")
  if (!all(necesare %in% names(df_procente))) {
    stop("df_procente trebuie sa contina: procent_verificare, prob_detectie.")
  }

  ggplot2::ggplot(df_procente,
                  ggplot2::aes(x = procent_verificare, y = prob_detectie)) +
    ggplot2::geom_line(color = .culori$fixa, linewidth = 0.9) +
    ggplot2::geom_point(color = .culori$fixa, size = 2.5) +
    # Afisam procentele pe axa X ca procente (5% in loc de 0.05).
    ggplot2::scale_x_continuous(labels = function(x) paste0(x * 100, "%")) +
    ggplot2::scale_y_continuous(labels = function(y) paste0(round(y * 100), "%")) +
    ggplot2::labs(
      title    = "Efectul procentului de verificare asupra detectiei",
      subtitle = "Tipic concav: primele procente aduc cel mai mare castig (randament descrescator)",
      x = "Procent de curse verificate", y = "Probabilitatea de detectie (cel putin un incident/zi)"
    ) +
    .tema_proiect()
}


# =============================================================================
# (F) AJUTOR: salveaza un grafic pe disc  (folosit de main_P1.R / de C)
# =============================================================================
# Wrapper subtire peste ggsave cu dimensiuni rezonabile pentru documentatie.
# @param plot      obiectul ggplot de salvat.
# @param fisier    calea fisierului (ex: "figuri/hist_suspecte.png").
salveaza_grafic <- function(plot, fisier, latime = 8, inaltime = 5, dpi = 150) {
  ggplot2::ggsave(filename = fisier, plot = plot,
                  width = latime, height = inaltime, dpi = dpi)
}


# =============================================================================
# (G) BLOC DE TEST (comentat) - exemplu de folosire pentru main_P1.R / C
# =============================================================================
# Necesita 01, 02, 03 incarcate si library(ggplot2). Optional library(patchwork).
#
# source("01_model.R"); source("02_strategii.R"); source("03_simulare.R")
# library(ggplot2)
# set.seed(2026)
#
# # Un an cu strategia fixa la 10% (date pentru histograme + evolutie):
# strat_fix <- function(N, S) strategie_fixa(N, S, f = 0.10)
# an <- simuleaza_an(strat_fix, p = 0.005)
#
# g1 <- grafic_hist_suspecte(an)
# g2 <- grafic_hist_detectate(an)
# g4 <- grafic_evolutie_zilnica(an, fereastra = 7)
# print(g1); print(g2); print(g4)
#
# # Grafic comparativ (necesita cele doua tabele rezumat din repeta_simulare):
# strat_adapt <- function(N, S) strategie_adaptiva(N, S, 0.05, 0.30, 300, 500)
# rez_fix   <- repeta_simulare(strat_fix,   p = 0.005, n_rep = 200)
# rez_adapt <- repeta_simulare(strat_adapt, p = 0.005, n_rep = 200)
# g3 <- grafic_comparativ_strategii(rez_fix, rez_adapt)
# print(g3)
#
# # Efectul procentului (cerinta 7):
# tab <- studiu_procent_verificare(p = 0.005, n_rep = 100)
# g5 <- grafic_efect_procent(tab)
# print(g5)
#
# # Combinarea cu patchwork (optional, pentru documentatie):
# # library(patchwork); (g1 | g2) / g4
# =============================================================================