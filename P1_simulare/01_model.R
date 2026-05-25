# ============================================================================
#  01_model.R
#  ---------------------------------------------------------------------------
#  PROBLEMA 1 — Evenimente rare: detectie si simulare
#  Context ales de echipa: INCIDENTE TEHNICE LA METROU
#
#  Acest fisier contine MODELUL PROBABILISTIC de baza:
#    (1) generarea numarului de curse intr-o zi  -> variabila N
#    (2) generarea numarului de incidente rare    -> variabila S (suspecte)
#
#  Restul proiectului (strategii de verificare, simulare pe un an, costuri,
#  grafice) se construieste DEASUPRA functiilor definite aici.
#
#  NOTA DE PROIECTARE: acest fisier NU ruleaza nimic "de la sine" — doar
#  DEFINESTE functii. Asta permite ca 03_simulare.R sa le apeleze fara efecte
#  secundare nedorite (principiul "cod = functii, nu scripturi care ruleaza
#  singure"). Reproductibilitatea (set.seed) se gestioneaza in 03_simulare.R.
# ============================================================================
install.packages("languageserver")

# ----------------------------------------------------------------------------
#  ASPECTE TEORETICE — DE CE ACESTE DISTRIBUTII?  (citit de C pentru documentatie)
# ----------------------------------------------------------------------------
#
#  ## 1. Numarul de curse pe zi (N)
#
#  Problema cere generarea aleatoare a "numarului total de cereri" (la noi:
#  numarul de curse de metrou intr-o zi). Enuntul sugereaza Poisson, normala
#  trunchiata sau binomiala negativa. Analizam alegerea:
#
#  *** Poisson(lambda) ***
#  - Model clasic pentru "numar de evenimente intr-un interval fix de timp".
#  - Proprietate definitorie:  E[N] = Var[N] = lambda.
#  - Limitare: impune ca imprastierea (varianta) sa fie EXACT egala cu media.
#    In realitate, numarul de curse variaza mai mult de la zi la zi (zile
#    lucratoare vs. weekend, sarbatori, evenimente speciale, vreme), deci
#    datele reale sunt de obicei SUPRA-DISPERSATE: Var[N] > E[N].
#
#  *** Binomiala Negativa(r, p_nb) ***  <-- MODELUL NOSTRU PRINCIPAL
#  - Generalizeaza Poisson permitand Var[N] > E[N] (supra-dispersie).
#  - Interpretare utila: este o Poisson a carei rata "lambda" este ea insasi
#    aleatoare (un amestec Poisson-Gamma). Aceasta surprinde exact ideea ca
#    "intensitatea traficului" difera de la o zi la alta.
#  - Parametrizare folosita in R (rnbinom cu argumentele `mu` si `size`):
#       mu   = media dorita a numarului de curse              (E[N] = mu)
#       size = parametrul de dispersie (r); controleaza cat de
#              "imprastiata" este distributia. Relatia:
#                   Var[N] = mu + mu^2 / size
#       => size MARE  => Var ~ mu  => ne apropiem de Poisson (dispersie mica)
#       => size MIC   => Var >> mu => supra-dispersie puternica
#
#  CONCLUZIE: folosim Binomiala Negativa ca model implicit (mai realista), dar
#  pastram Poisson ca optiune, pentru a putea COMPARA in documentatie efectul
#  supra-dispersiei. Aceasta comparatie teoretic-vs-empiric este punctata bonus.
#
#
#  ## 2. Numarul de incidente rare (S) conditionat de N
#
#  O proportie MICA p din curse au un incident tehnic (p in {0.001, 0.005, 0.02}
#  conform cerintei). Modelam fiecare cursa ca un experiment Bernoulli independent
#  cu probabilitate p de incident. Atunci numarul de incidente intr-o zi este:
#
#       S | N = n  ~  Binomial(n, p)
#
#  adica E[S | N=n] = n*p  si  Var[S | N=n] = n*p*(1-p).
#
#  *** Legatura teoretica (proprietatea de THINNING / subtiere) ***
#  Daca N ~ Poisson(lambda) si fiecare eveniment este pastrat independent cu
#  probabilitate p, atunci marginal:   S ~ Poisson(lambda * p)   (EXACT).
#  Aceasta ne da o PROBABILITATE TEORETICA de comparat cu simularea:
#       P(cel putin un incident intr-o zi) = 1 - P(S = 0) = 1 - exp(-lambda*p)
#  (vezi functia `prob_teoretica_cel_putin_un_incident` mai jos).
#  Pentru Binomiala Negativa rezultatul marginal nu mai este Poisson, dar
#  formula conditionata Binomial(n, p) ramane valabila — de aceea folosim
#  abordarea conditionata (mai generala).
#
#  De ce conteaza p mic ("eveniment rar")? Pentru ca P(detectie) depinde puternic
#  de p: cu p foarte mic, multe zile NU au niciun incident, iar verificarea
#  aleatoare devine ineficienta — exact tensiunea pe care o studiem in proiect.
# ----------------------------------------------------------------------------


# ----------------------------------------------------------------------------
#  (A) GENERAREA NUMARULUI DE CURSE INTR-O ZI
# ----------------------------------------------------------------------------

#' Genereaza numarul de curse pentru O SINGURA zi.
#'
#' @param model  "nbinom" (binomiala negativa, implicit) sau "poisson".
#' @param mu     media dorita a numarului de curse pe zi (E[N]). Ex: 400.
#' @param size   parametrul de dispersie pentru binomiala negativa.
#'               Valoare mare -> aproape Poisson; valoare mica -> supra-dispersie.
#'               Ignorat daca model = "poisson".
#' @return       un intreg >= 0: numarul de curse generat pentru ziua respectiva.
genereaza_curse_zi <- function(model = "nbinom", mu = 400, size = 20) {

  # --- Validarea parametrilor (programare defensiva) ---
  # Media trebuie sa fie strict pozitiva: nu are sens un metrou cu 0 curse/zi.
  if (mu <= 0) {
    stop("Parametrul 'mu' (media curselor) trebuie sa fie strict pozitiv.")
  }

  if (model == "poisson") {
    # rpois genereaza o realizare dintr-o Poisson(lambda = mu).
    # Aici lambda joaca simultan rolul de medie SI de varianta.
    return(rpois(n = 1, lambda = mu))

  } else if (model == "nbinom") {
    # Pentru binomiala negativa, 'size' trebuie sa fie strict pozitiv.
    if (size <= 0) {
      stop("Parametrul 'size' (dispersia) trebuie sa fie strict pozitiv.")
    }
    # rnbinom cu parametrizarea (mu, size):
    #   - mu   = media                       => E[N] = mu
    #   - size = parametrul de dispersie r   => Var[N] = mu + mu^2/size
    # Aceasta parametrizare este cea mai intuitiva pentru modelare aplicata,
    # pentru ca fixam DIRECT media dorita si controlam separat imprastierea.
    return(rnbinom(n = 1, mu = mu, size = size))

  } else {
    # Daca cineva apeleaza functia cu un model necunoscut, semnalam clar eroarea
    # in loc sa returnam tacit un rezultat gresit.
    stop("Model necunoscut. Folositi 'nbinom' sau 'poisson'.")
  }
}


# ----------------------------------------------------------------------------
#  (B) GENERAREA INCIDENTELOR RARE CONDITIONAT DE NUMARUL DE CURSE
# ----------------------------------------------------------------------------

#' Genereaza numarul de incidente (curse "suspecte") pentru o zi, dat fiind N.
#'
#' Model: fiecare cursa are, independent, probabilitatea p de a avea un incident
#'        tehnic  =>  S | N=n ~ Binomial(n, p).
#'
#' @param n_curse  numarul de curse din ziua respectiva (intreg >= 0).
#' @param p        probabilitatea ca o cursa sa aiba un incident (p mic).
#' @return         un intreg in [0, n_curse]: numarul de incidente generat.
genereaza_incidente_zi <- function(n_curse, p) {

  # --- Validare ---
  # p este o probabilitate, deci trebuie sa fie in intervalul [0, 1].
  if (p < 0 || p > 1) {
    stop("Probabilitatea 'p' trebuie sa fie in intervalul [0, 1].")
  }
  # Numarul de curse nu poate fi negativ.
  if (n_curse < 0) {
    stop("Numarul de curse nu poate fi negativ.")
  }

  # Caz limita: daca nu exista curse, nu pot exista incidente.
  # (rbinom cu size=0 returneaza oricum 0, dar tratam explicit pentru claritate.)
  if (n_curse == 0) {
    return(0L)
  }

  # rbinom(n_extrageri = 1, size = n_curse, prob = p):
  #   numara cate dintre cele n_curse "incercari" Bernoulli au reusit (au incident).
  # Aceasta este realizarea variabilei S ~ Binomial(n_curse, p).
  rbinom(n = 1, size = n_curse, prob = p)
}


# ----------------------------------------------------------------------------
#  (C) FUNCTIE COMBINATA: genereaza datele "brute" ale unei zile
# ----------------------------------------------------------------------------
#  Returneaza componentele cerute explicit de enunt pentru fiecare zi:
#    - numarul total de curse        (N)
#    - numarul de curse normale      (N - S)
#    - numarul de curse suspecte     (S = incidente)
#  Verificarea/detectia NU se face aici — ea apartine strategiilor din
#  02_strategii.R. Separam astfel "ce se intampla in sistem" (acest fisier)
#  de "ce vede operatorul prin verificare" (fisierul de strategii). Aceasta
#  separare tine codul modular si usor de testat.
# ----------------------------------------------------------------------------

#' Genereaza componentele de baza ale unei zile de activitate.
#'
#' @param p      probabilitatea de incident pe cursa.
#' @param model  modelul pentru numarul de curse ("nbinom" / "poisson").
#' @param mu     media numarului de curse pe zi.
#' @param size   dispersia (doar pentru "nbinom").
#' @return       o lista cu: total_curse, curse_normale, curse_suspecte.
genereaza_zi <- function(p, model = "nbinom", mu = 400, size = 20) {

  # 1) Cate curse au loc azi?
  total_curse <- genereaza_curse_zi(model = model, mu = mu, size = size)

  # 2) Cate dintre ele au un incident (sunt "suspecte")?
  curse_suspecte <- genereaza_incidente_zi(n_curse = total_curse, p = p)

  # 3) Restul sunt curse normale. Prin constructie, curse_suspecte <= total_curse,
  #    deci diferenta este intotdeauna >= 0.
  curse_normale <- total_curse - curse_suspecte

  # Returnam o lista numita (usor de folosit mai departe in data.frame-uri).
  list(
    total_curse    = total_curse,
    curse_normale  = curse_normale,
    curse_suspecte = curse_suspecte
  )
}


# ----------------------------------------------------------------------------
#  (D) PROBABILITATEA TEORETICA — pentru comparatia teorie vs. simulare
# ----------------------------------------------------------------------------
#  Cerintele ulterioare cer o discutie teorie vs. empiric. Oferim aici formula
#  teoretica pentru cazul Poisson (unde thinning-ul da un rezultat exact).
#
#  Daca N ~ Poisson(lambda) si fiecare cursa are incident cu probabilitate p,
#  atunci S ~ Poisson(lambda * p), deci:
#       P(S >= 1) = 1 - P(S = 0) = 1 - exp(-lambda * p).
#  Aceasta este probabilitatea teoretica de a avea CEL PUTIN un incident intr-o
#  zi (inainte de orice verificare). O comparam cu frecventa empirica obtinuta
#  din simulare in 03_simulare.R.
# ----------------------------------------------------------------------------

#' Probabilitatea teoretica de a avea cel putin un incident intr-o zi (caz Poisson).
#'
#' @param lambda  media numarului de curse pe zi (E[N]).
#' @param p       probabilitatea de incident pe cursa.
#' @return        P(S >= 1) sub modelul Poisson + thinning.
prob_teoretica_cel_putin_un_incident <- function(lambda, p) {
  if (lambda <= 0) stop("lambda trebuie sa fie strict pozitiv.")
  if (p < 0 || p > 1) stop("p trebuie sa fie in [0, 1].")

  # P(S = 0) sub Poisson(lambda*p) este exp(-lambda*p);
  # complementul da P(cel putin un incident).
  1 - exp(-lambda * p)
}


----------------------------------------------------------------------------
 (E) MIC TEST RAPID (rulati manual pentru a verifica modelul)
----------------------------------------------------------------------------
 Lasam aceste linii COMENTATE ca sa nu ruleze automat la `source("01_model.R")`.
 Persoana A le poate de-comenta temporar ca sa verifice ca totul functioneaza.

 set.seed(123)                                   # reproductibilitate
 zi <- genereaza_zi(p = 0.005, mu = 400, size = 20)
 print(zi)                                       # ~400 curse, cateva suspecte

 # Comparatie rapida medie vs. varianta pentru a "vedea" supra-dispersia:
 esantion_nb  <- replicate(5000, genereaza_curse_zi("nbinom",  mu = 400, size = 20))
 esantion_poi <- replicate(5000, genereaza_curse_zi("poisson", mu = 400))
 cat("NBinom : media =", mean(esantion_nb),  " varianta =", var(esantion_nb),  "\n")
 cat("Poisson: media =", mean(esantion_poi), " varianta =", var(esantion_poi), "\n")
 # Veti observa: la Poisson varianta ~ media (~400); la NBinom varianta >> media.

 # Verificare teorie vs. empiric pentru P(cel putin un incident):
 emp <- mean(replicate(10000, genereaza_zi(p = 0.005, mu = 400, size = 20)$curse_suspecte >= 1))
 teo <- prob_teoretica_cel_putin_un_incident(lambda = 400, p = 0.005)
 cat("P(S>=1) empiric =", emp, " | teoretic (Poisson) =", teo, "\n")
 # Nu vor fi IDENTICE: empiricul foloseste NBinom (supra-dispersat), teoreticul
 # presupune Poisson. Aceasta diferenta este un punct bun de discutie!
============================================================================