# =============================================================================
# distributii.R  --  Generarea celor 4 repartitii + validarea parametrilor
# =============================================================================
#
# ROLUL ACESTUI FISIER (in arhitectura Problemei 2):
#   Contine LOGICA de generare a esantioanelor pentru cele 4 repartitii cerute
#   (cerinta 2) si VALIDAREA parametrilor introdusi de utilizator (cerinta 3
#   Shiny). server.R doar apeleaza aceste functii — nicio logica de interfata aici.
#
# Cele 4 repartitii continue cerute de enunt:
#   - Normala       N(mu, sigma^2)   parametri: mu (media), sigma (abatere std)
#   - Exponentiala  Exp(lambda)      parametru:  lambda (rata, > 0)
#   - Uniforma      Unif(a, b)       parametri: a, b cu a < b
#   - Gamma         Gamma(alpha, theta) parametri: alpha (forma), theta (scala)
#
# DECIZIE DE PROIECTARE — o singura functie "dispecer":
#   In loc de 4 functii separate, avem o singura genereaza_esantion() care
#   primeste NUMELE repartitiei si o LISTA de parametri. Asa server.R apeleaza
#   mereu la fel, indiferent de repartitie, iar adaugarea unei noi repartitii
#   inseamna doar un nou caz in switch(). Parametrii vin ca lista numita
#   (ex: list(mu = 0, sigma = 1)) ca sa mearga uniform desi repartitiile au
#   numar diferit de parametri.
# =============================================================================


# =============================================================================
# (A) VALIDAREA PARAMETRILOR  (cerinta 3 Shiny + bonus "tratarea erorilor")
# =============================================================================
#
# Verifica daca parametrii introdusi sunt valizi pentru repartitia aleasa.
# Returneaza:
#   - NULL              daca totul e in regula (niciun mesaj de eroare);
#   - un string (mesaj) daca exista o problema (ex: "sigma trebuie sa fie > 0").
# Aceasta conventie (NULL = ok) e exact ce asteapta server.R: daca primeste un
# mesaj, il afiseaza si NU simuleaza; daca primeste NULL, continua.
#
# @param distributie  numele repartitiei: "normal" / "exp" / "unif" / "gamma".
# @param par          lista numita de parametri (campurile depind de repartitie).
# @return             NULL daca e valid, altfel un mesaj de eroare (string).
valideaza_parametri <- function(distributie, par) {

  # Functie ajutatoare: verifica daca o valoare e un numar finit (nu NA, nu Inf).
  # Utilizatorul poate sterge un camp si valoarea devine NA -> tratam elegant.
  e_numar <- function(x) {
    is.numeric(x) && length(x) == 1 && is.finite(x)
  }

  if (distributie == "normal") {
    # Normala: mu poate fi orice numar real; sigma trebuie STRICT pozitiv.
    if (!e_numar(par$mu))    return("mu (media) trebuie sa fie un numar.")
    if (!e_numar(par$sigma)) return("sigma trebuie sa fie un numar.")
    if (par$sigma <= 0)      return("sigma (abaterea standard) trebuie sa fie strict pozitiv.")

  } else if (distributie == "exp") {
    # Exponentiala: lambda (rata) trebuie STRICT pozitiv.
    if (!e_numar(par$lambda)) return("lambda trebuie sa fie un numar.")
    if (par$lambda <= 0)      return("lambda (rata) trebuie sa fie strict pozitiv.")

  } else if (distributie == "unif") {
    # Uniforma: a si b numere reale, cu conditia a < b.
    if (!e_numar(par$a) || !e_numar(par$b)) return("a si b trebuie sa fie numere.")
    if (par$a >= par$b)                     return("trebuie ca a < b pentru Unif(a, b).")

  } else if (distributie == "gamma") {
    # Gamma: alpha (forma) si theta (scala) trebuie STRICT pozitivi.
    if (!e_numar(par$alpha)) return("alpha (forma) trebuie sa fie un numar.")
    if (!e_numar(par$theta)) return("theta (scala) trebuie sa fie un numar.")
    if (par$alpha <= 0)      return("alpha (forma) trebuie sa fie strict pozitiv.")
    if (par$theta <= 0)      return("theta (scala) trebuie sa fie strict pozitiv.")

  } else {
    return("Repartitie necunoscuta.")
  }

  # Daca am ajuns aici, toti parametrii sunt valizi.
  NULL
}


# =============================================================================
# (B) GENERAREA ESANTIONULUI  (cerintele 2 si 3)
# =============================================================================
#
# Dat numele repartitiei, parametrii si dimensiunea n, genereaza un esantion
# X_1, ..., X_n. Presupune ca parametrii au fost deja validati (server.R cheama
# valideaza_parametri INAINTE), dar pastram un strat defensiv si aici.
#
# @param distributie  "normal" / "exp" / "unif" / "gamma".
# @param par          lista numita de parametri.
# @param n            numarul de valori de generat (intreg pozitiv).
# @return             un vector numeric de lungime n cu valorile generate.
genereaza_esantion <- function(distributie, par, n) {

  # --- Validare defensiva a lui n ---
  if (!is.numeric(n) || n < 1) stop("n trebuie sa fie un intreg >= 1.")

  # --- Strat defensiv: re-validam parametrii (in caz ca cineva apeleaza direct) ---
  msg <- valideaza_parametri(distributie, par)
  if (!is.null(msg)) stop(msg)

  # --- Dispecer: alegem functia de generare in functie de repartitie ---
  switch(distributie,

    # Normala: rnorm(n, mean = mu, sd = sigma).
    "normal" = rnorm(n, mean = par$mu, sd = par$sigma),

    # Exponentiala: rexp(n, rate = lambda). In R, parametrul e RATA (lambda),
    # iar media repartitiei este 1/lambda.
    "exp"    = rexp(n, rate = par$lambda),

    # Uniforma: runif(n, min = a, max = b).
    "unif"   = runif(n, min = par$a, max = par$b),

    # Gamma: rgamma(n, shape = alpha, scale = theta). Folosim 'scale' explicit
    # (= theta) ca sa corespunda notatiei Gamma(alpha, theta) din enunt.
    # ATENTIE: rgamma are si parametrul alternativ 'rate' = 1/scale; noi folosim
    # 'scale' ca sa nu existe confuzie cu notatia ceruta.
    "gamma"  = rgamma(n, shape = par$alpha, scale = par$theta),

    # Caz implicit: repartitie necunoscuta -> eroare clara.
    stop("Repartitie necunoscuta: ", distributie)
  )
}


# =============================================================================
# (C) DESCRIEREA PARAMETRILOR  (ajutor pentru UI dinamic in server.R)
# =============================================================================
#
# Intoarce, pentru o repartitie data, LISTA de parametri pe care trebuie sa-i
# ceara interfata: numele intern, eticheta afisata, si o valoare implicita
# rezonabila. server.R foloseste asta ca sa construiasca dinamic campurile
# numerice (uiOutput) pentru repartitia aleasa — cand schimbi repartitia, se
# schimba si campurile de parametri.
#
# @param distributie  "normal" / "exp" / "unif" / "gamma".
# @return  lista de liste, fiecare cu: id, eticheta, valoare_implicita.
descrie_parametri <- function(distributie) {
  switch(distributie,
    "normal" = list(
      list(id = "mu",    eticheta = "mu (media)",              valoare = 0),
      list(id = "sigma", eticheta = "sigma (abaterea standard)", valoare = 1)
    ),
    "exp" = list(
      list(id = "lambda", eticheta = "lambda (rata, > 0)",     valoare = 1)
    ),
    "unif" = list(
      list(id = "a", eticheta = "a (minim)", valoare = 0),
      list(id = "b", eticheta = "b (maxim)", valoare = 1)
    ),
    "gamma" = list(
      list(id = "alpha", eticheta = "alpha (forma, > 0)", valoare = 2),
      list(id = "theta", eticheta = "theta (scala, > 0)", valoare = 1)
    ),
    list()  # repartitie necunoscuta -> lista goala
  )
}


# =============================================================================
# (D) BLOC DE TEST (comentat) - ruleaza in consola ca sa verifici functiile
# =============================================================================
# set.seed(1)
# # Generare normala:
# x <- genereaza_esantion("normal", list(mu = 0, sigma = 1), 10000)
# cat("Normala: media ~", round(mean(x), 3), " sd ~", round(sd(x), 3), "\n")
#
# # Generare exponentiala (media teoretica = 1/lambda):
# y <- genereaza_esantion("exp", list(lambda = 2), 10000)
# cat("Exp(2): media ~", round(mean(y), 3), " (teoretic 0.5)\n")
#
# # Validare: sigma negativ trebuie respins:
# print(valideaza_parametri("normal", list(mu = 0, sigma = -1)))  # mesaj eroare
# print(valideaza_parametri("normal", list(mu = 0, sigma = 1)))   # NULL (ok)
#
# # Validare uniforma a >= b:
# print(valideaza_parametri("unif", list(a = 5, b = 2)))  # mesaj eroare
# =============================================================================