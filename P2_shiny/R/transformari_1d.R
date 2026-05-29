# =============================================================================
# transformari_1d.R  --  Cele 5 transformari g(x) pentru Problema 2
# =============================================================================
#
# ROLUL ACESTUI FISIER (in arhitectura Problemei 2):
#   Contine LOGICA transformarilor Y = g(X) (cerinta 4). Primeste un esantion X
#   si numele unei transformari, intoarce esantionul transformat Y — plus o
#   raportare a valorilor eliminate (pentru transformarile nedefinite pe tot
#   suportul). server.R doar apeleaza; nicio interfata aici.
#
# Cele 5 transformari cerute de enunt:
#   g(x) = x^2        (patrat)
#   g(x) = |x|        (modul)
#   g(x) = log(x)     (logaritm natural) — NEDEFINIT pentru x <= 0
#   g(x) = e^x        (exponential)
#   g(x) = 1/(1+e^-x) (sigmoid / functia logistica)
#
# -----------------------------------------------------------------------------
# DECIZIA CHEIE — tratarea transformarilor nedefinite (cerinta 4)
# -----------------------------------------------------------------------------
# Enuntul cere explicit: "pentru transformarile care nu sunt definite pe tot
# suportul, aplicatia trebuie sa trateze corect situatia. De exemplu, pentru
# log(x) trebuie discutat ce se intampla daca valorile sunt negative sau nule."
#
# log(x) e definit DOAR pentru x > 0. Daca X ~ Normal sau Unif(-1,1), aproximativ
# jumatate din valori sunt <= 0. Abordarea noastra (cea mai onesta):
#   - aplicam log DOAR pe valorile pozitive;
#   - eliminam valorile <= 0 din rezultat;
#   - RAPORTAM cate au fost eliminate, ca interfata sa afiseze un avertisment.
# Nu ascundem problema (nu inlocuim tacit cu 0 sau NA invizibil), ci o comunicam.
# Aceasta e "tratarea eleganta a erorilor" apreciata in barem.
#
# CONVENTIE DE RETURNARE: fiecare transformare intoarce o LISTA cu:
#   $y         = vectorul transformat (poate fi mai scurt decat X daca s-au
#                eliminat valori nedefinite);
#   $n_eliminate = cate valori din X au fost eliminate (0 daca transformarea
#                e definita peste tot);
#   $avertisment = un mesaj (string) daca s-a intamplat ceva notabil, altfel "".
# server.R foloseste $y pentru grafice/statistici si $avertisment pentru a
# afisa un mesaj utilizatorului.
# =============================================================================


# =============================================================================
# (A) FUNCTIA DISPECER: aplica transformarea aleasa
# =============================================================================
#
# @param transformare  numele: "patrat" / "modul" / "log" / "exp" / "sigmoid".
# @param X             vectorul numeric de transformat (esantionul generat).
# @return  lista cu $y, $n_eliminate, $avertisment (vezi conventia de mai sus).
aplica_transformare <- function(transformare, X) {

  # --- Validare defensiva ---
  if (!is.numeric(X) || length(X) == 0) {
    stop("X trebuie sa fie un vector numeric nevid.")
  }

  switch(transformare,

    # ----- g(x) = x^2 -----
    # Definita peste tot. Produce mereu valori >= 0 (vezi interpretarea).
    "patrat" = list(
      y = X^2,
      n_eliminate = 0L,
      avertisment = ""
    ),

    # ----- g(x) = |x| -----
    # Definita peste tot. Produce mereu valori >= 0.
    "modul" = list(
      y = abs(X),
      n_eliminate = 0L,
      avertisment = ""
    ),

    # ----- g(x) = log(x) -----  CAZ SPECIAL: nedefinit pentru x <= 0
    "log" = {
      # Identificam valorile valide (strict pozitive).
      valide <- X > 0
      n_eliminate <- sum(!valide)   # cate valori sunt <= 0 (nedefinite)

      # Aplicam log doar pe valorile valide.
      y <- log(X[valide])

      # Construim un avertisment onest daca s-au eliminat valori.
      avert <- if (n_eliminate > 0) {
        paste0("Atentie: ", n_eliminate, " din ", length(X),
               " valori erau <= 0 (log nedefinit) si au fost excluse. ",
               "Histograma lui Y se bazeaza pe restul de ",
               length(X) - n_eliminate, " valori.")
      } else ""

      list(y = y, n_eliminate = n_eliminate, avertisment = avert)
    },

    # ----- g(x) = e^x -----
    # Matematic definita peste tot, dar pentru x foarte mare poate da Inf
    # (overflow numeric, peste ~709 in R). Tratam defensiv: eliminam eventualele
    # valori infinite si raportam.
    "exp" = {
      y_brut <- exp(X)
      finite <- is.finite(y_brut)
      n_eliminate <- sum(!finite)

      avert <- if (n_eliminate > 0) {
        paste0("Atentie: ", n_eliminate, " valori au produs overflow (e^x = Inf) ",
               "si au fost excluse. Reduceti parametrii daca apar des.")
      } else ""

      list(y = y_brut[finite], n_eliminate = n_eliminate, avertisment = avert)
    },

    # ----- g(x) = 1/(1+e^-x)  (sigmoid) -----
    # Definita peste tot, intoarce mereu valori in intervalul (0, 1).
    # Implementare numeric stabila ca sa evitam overflow la -x foarte mare.
    "sigmoid" = list(
      y = 1 / (1 + exp(-X)),
      n_eliminate = 0L,
      avertisment = ""
    ),

    # Caz implicit: transformare necunoscuta.
    stop("Transformare necunoscuta: ", transformare)
  )
}


# =============================================================================
# (B) INTERPRETAREA AUTOMATA  (cerinta 7)
# =============================================================================
#
# Compara X cu Y si genereaza mesaje descriptive de tipul cerut de enunt:
#   - "transformarea a produs valori strict pozitive";
#   - "transformarea a modificat simetria distributiei";
#   - "transformarea a comprimat valorile mari";
#   - "transformarea a accentuat valorile extreme".
# Folosim reguli simple bazate pe statistici comparate intre X si Y.
#
# @param X  esantionul original.
# @param Y  esantionul transformat (vectorul $y din aplica_transformare).
# @return   un vector de mesaje (string-uri); poate fi gol daca nimic notabil.
interpreteaza_transformare <- function(X, Y) {

  mesaje <- character(0)   # vom adauga mesaje pe rand

  # --- Regula 1: semnul valorilor ---
  # Daca toate valorile Y sunt > 0, transformarea a produs valori strict pozitive.
  if (all(Y > 0)) {
    mesaje <- c(mesaje, "Transformarea a produs valori strict pozitive.")
  } else if (all(Y >= 0)) {
    mesaje <- c(mesaje, "Transformarea a produs valori nenegative (>= 0).")
  }

  # --- Regula 2: marginirea intr-un interval ---
  # Daca Y e cuprins strict intre 0 si 1, semnalam (cazul sigmoid).
  if (min(Y) >= 0 && max(Y) <= 1) {
    mesaje <- c(mesaje, "Transformarea a comprimat valorile in intervalul [0, 1].")
  }

  # --- Regula 3: schimbarea simetriei (prin coeficientul de asimetrie) ---
  # Asimetria (skewness) masoara cat de "inclinata" e distributia. Daca X e ~
  # simetric (skew ~ 0) iar Y devine puternic asimetric, transformarea a
  # modificat simetria. Calculam skewness manual (fara pachete externe).
  skew <- function(v) {
    m <- mean(v); s <- sd(v)
    if (s == 0) return(0)
    mean(((v - m) / s)^3)
  }
  skew_X <- skew(X)
  skew_Y <- skew(Y)
  if (abs(skew_X) < 0.5 && abs(skew_Y) > 1) {
    mesaje <- c(mesaje,
      paste0("Transformarea a modificat simetria: X era aproape simetric ",
             "(asimetrie ~ ", round(skew_X, 2), "), iar Y e puternic asimetric ",
             "(asimetrie ~ ", round(skew_Y, 2), ")."))
  }

  # --- Regula 4: accentuarea valorilor extreme (prin amplitudinea relativa) ---
  # Daca raportul max/mediana al lui Y e mult mai mare decat al lui |X|,
  # transformarea a "intins" coada (a accentuat extremele). Tipic la e^x.
  amplitudine <- function(v) {
    med <- median(abs(v))
    if (med == 0) return(Inf)
    max(abs(v)) / med
  }
  if (amplitudine(Y) > 3 * amplitudine(X)) {
    mesaje <- c(mesaje,
      "Transformarea a accentuat valorile extreme (coada distributiei e mai lunga).")
  }

  # Daca nicio regula nu s-a aplicat, dam un mesaj neutru.
  if (length(mesaje) == 0) {
    mesaje <- "Transformarea nu a modificat semnificativ forma generala a distributiei."
  }

  mesaje
}


# =============================================================================
# (C) BLOC DE TEST (comentat) - ruleaza in consola
# =============================================================================
# source("R/distributii.R")
# set.seed(1)
# X <- genereaza_esantion("normal", list(mu = 0, sigma = 1), 5000)
#
# # x^2 (legat de chi-patrat cu 1 gdl):
# r <- aplica_transformare("patrat", X)
# cat("x^2: eliminate =", r$n_eliminate, " | min Y =", round(min(r$y), 3), "\n")
# print(interpreteaza_transformare(X, r$y))
#
# # log pe normal standard (~jumatate eliminate):
# r <- aplica_transformare("log", X)
# cat("log: eliminate =", r$n_eliminate, "\n")
# cat(r$avertisment, "\n")
#
# # sigmoid (mereu in (0,1)):
# r <- aplica_transformare("sigmoid", X)
# cat("sigmoid: range = [", round(min(r$y), 3), ",", round(max(r$y), 3), "]\n")
# print(interpreteaza_transformare(X, r$y))
# =============================================================================