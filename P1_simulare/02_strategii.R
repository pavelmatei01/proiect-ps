# =============================================================================
# 02_strategii.R  --  Strategii de verificare pentru detectia incidentelor rare
# =============================================================================
#
# ROLUL ACESTUI FISIER (in arhitectura Problemei 1):
#   01_model.R    -> genereaza ce se intampla "in realitate" intr-o zi
#                    (N curse, dintre care S au incident). Functia genereaza_zi().
#   02_strategii.R-> ACEST fisier. Primeste o zi (N, S) si decide CATE curse
#                    verificam (V) si CATE incidente prindem (D, detectate).
#   03_simulare.R -> ruleaza un an intreg + cele 1000 de repetari (fisierul lui A).
#
# Acest fisier DOAR defineste functii. Nu ruleaza nimic la incarcare
# (fara set.seed, fara apeluri directe). Asa 03_simulare.R le poate apela curat.
#
# -----------------------------------------------------------------------------
# TEORIA DIN SPATELE DETECTIEI  (de pus de catre C in documentatie)
# -----------------------------------------------------------------------------
# Stratul de detectie e o problema clasica de ESANTIONARE FARA INTOARCERE.
#
# Intr-o zi avem N curse, dintre care S au un incident ("defectele" ascunse).
# Verificam un subset de V curse, alese fara intoarcere (verifici curse
# DISTINCTE - nu poti inspecta de doua ori aceeasi cursa in aceeasi zi).
# Intrebarea "cate dintre cele S incidente cad in subsetul verificat?" este
# EXACT problema urnei cu bile.
#
# Numarul de incidente detectate D urmeaza o repartitie HIPERGEOMETRICA:
#
#       D ~ Hypergeometric(N, S, V)
#
#   cu media:   E[D] = V * (S / N)
#
# Interpretare: in medie prinzi fractiunea V/N din incidente - adica detectia
# este PROPORTIONALA cu efortul de verificare. Aceasta e intuitia centrala a
# intregului proiect: cu cat verifici mai mult, cu atat prinzi mai mult.
#
# De ce hipergeometrica si NU binomiala (rbinom)?
#   - Binomiala ar presupune ca fiecare incident e prins INDEPENDENT cu
#     probabilitate V/N. Dar verificarile nu sunt independente: daca ai verificat
#     deja o cursa, ramane o cursa mai putin de verificat. Esantionarea e FARA
#     intoarcere, deci modelul corect e hipergeometric.
#   - Pentru S mic si N mare diferenta e mica, dar hipergeometrica e riguros
#     corecta si o folosim ca atare (rhyper() din baza R).
#
# CE SE INTAMPLA CU INCIDENTELE NEDETECTATE:
#   D   = incidente detectate (au fost printre cele verificate)
#   S-D = incidente NEDETECTATE (au scapat verificarii) -> genereaza cost (05_cost.R)
#
# -----------------------------------------------------------------------------


# =============================================================================
# FUNCTIE AJUTATOARE: clamp - limiteaza o valoare intr-un interval [lo, hi]
# =============================================================================
# Folosita la strategia adaptiva ca sa "taiem" procentul de verificare astfel
# incat sa ramana mereu intre f_min si f_max (tratare eleganta a marginilor).
clamp <- function(x, lo, hi) {
  pmax(lo, pmin(hi, x))   # pmax/pmin sunt vectorizate -> merg si pe vectori
}


# =============================================================================
# FUNCTIE COMUNA DE DETECTIE: dat fiind N, S si V -> cate incidente detectam
# =============================================================================
# Aceasta functie incapsuleaza stratul comun (esantionarea hipergeometrica).
# Ambele strategii o apeleaza dupa ce au decis cate curse verifica (V).
#
# Argumente:
#   N - numarul total de curse in ziua respectiva
#   S - numarul de curse cu incident (cele "rare", din genereaza_zi())
#   V - numarul de curse pe care le verificam (decis de strategie)
#
# Returneaza: D = numarul de incidente detectate (variabila aleatoare).
detecteaza <- function(N, S, V) {

  # ---- Validare defensiva a intrarilor (apreciat in barem) ----
  if (N < 0 || S < 0 || V < 0)        stop("N, S si V trebuie sa fie >= 0.")
  if (S > N)                          stop("S (incidente) nu poate depasi N (curse).")
  if (V > N)                          stop("V (verificari) nu poate depasi N (curse).")

  # Cazuri limita care ar produce NA in rhyper daca nu le tratam explicit:
  if (N == 0) return(0L)   # zi fara curse -> nimic de detectat
  if (S == 0) return(0L)   # zi fara incidente -> nu ai ce prinde
  if (V == 0) return(0L)   # nu verifici nimic -> nu prinzi nimic

  # ---- Esantionare hipergeometrica ----
  # rhyper(nn, m, n, k):
  #   nn = cate trageri facem (1, o singura zi)
  #   m  = numarul de bile "albe"  = incidente              -> S
  #   n  = numarul de bile "negre" = curse fara incident    -> N - S
  #   k  = cate bile extragem      = curse verificate        -> V
  # Rezultatul = cate "bile albe" (incidente) am extras = D detectate.
  D <- rhyper(nn = 1, m = S, n = N - S, k = V)

  return(D)
}


# =============================================================================
# STRATEGIA A: verificare aleatoare simpla (PROCENT FIX)
# =============================================================================
# Verifica un procent FIX f din cursele zilei, indiferent de volum.
# Aceasta e cerinta 4(a): "se verifica un procent fix din cererile zilnice".
#
# Matematic:  V = round(f * N)   (numar intreg de curse verificate)
#             D ~ Hypergeometric(N, S, V)
#
# Argumente:
#   N - numarul total de curse in zi
#   S - numarul de incidente in zi
#   f - fractiunea fixa de verificare (ex: 0.05 = 5%)
#
# Returneaza: lista cu numarul de verificari (V) si detectate (D).
strategie_fixa <- function(N, S, f) {

  # ---- Validare ----
  if (f < 0 || f > 1) stop("f (procentul de verificare) trebuie sa fie in [0, 1].")

  # ---- Cate curse verificam ----
  # round() poate da, teoretic, o valoare > N din cauza rotunjirii doar daca
  # f > 1; cum am validat f <= 1, V <= N mereu. Pastram min() ca plasa de
  # siguranta defensiva.
  V <- min(round(f * N), N)

  # ---- Cate incidente prindem ----
  D <- detecteaza(N, S, V)

  # Returnam tot ce are nevoie 03_simulare.R mai departe.
  list(
    verificate = V,        # cate curse am verificat
    detectate  = D,        # cate incidente am prins
    nedetectate = S - D    # cate incidente au scapat (intra in functia de cost)
  )
}


# =============================================================================
# STRATEGIA B: verificare ADAPTIVA (procent care creste cu volumul)
# =============================================================================
# Ideea (cerinta 4(b)): "in zilele cu numar mare de cereri se verifica un
# procent mai mare". In contextul metroului: in orele/zilele aglomerate
# inspectezi proportional mai mult, fiindca acolo e mai probabil sa fie incidente.
#
# Formularea aleasa - procent care creste LINIAR cu N intre doua praguri:
#
#   f_adaptiv(N) = f_min + (f_max - f_min) * clamp( (N - N_min)/(N_max - N_min), 0, 1 )
#
# Interpretare:
#   - sub N_min curse  -> verifici procentul minim f_min  (zi linistita)
#   - peste N_max curse-> verifici procentul maxim f_max  (zi foarte aglomerata)
#   - intre ele        -> interpolare liniara a procentului
# clamp(..., 0, 1) garanteaza ca fractiunea ramane in [0,1] chiar daca N iese
# din intervalul [N_min, N_max] -> tratare eleganta a marginilor.
#
# Argumente:
#   N      - numarul total de curse in zi
#   S      - numarul de incidente in zi
#   f_min  - procentul minim de verificare (zile linistite)
#   f_max  - procentul maxim de verificare (zile aglomerate)
#   N_min  - pragul de volum sub care folosim f_min
#   N_max  - pragul de volum peste care folosim f_max
#
# Returneaza: lista cu verificari (V) si detectate (D).
strategie_adaptiva <- function(N, S, f_min, f_max, N_min, N_max) {

  # ---- Validare ----
  if (f_min < 0 || f_min > 1 || f_max < 0 || f_max > 1) {
    stop("f_min si f_max trebuie sa fie in [0, 1].")
  }
  if (f_min > f_max)          stop("f_min nu poate fi mai mare decat f_max.")
  if (N_min >= N_max)         stop("N_min trebuie sa fie strict mai mic decat N_max.")

  # ---- Calculam procentul adaptiv pentru ziua curenta ----
  # Pozitia relativa a lui N in intervalul [N_min, N_max], limitata la [0,1].
  pozitie <- clamp((N - N_min) / (N_max - N_min), 0, 1)
  f <- f_min + (f_max - f_min) * pozitie

  # ---- Cate curse verificam si cate incidente prindem ----
  V <- min(round(f * N), N)
  D <- detecteaza(N, S, V)

  list(
    verificate  = V,
    detectate   = D,
    nedetectate = S - D,
    f_aplicat   = f       # util pentru grafice/diagnoza: ce procent s-a folosit
  )
}


# =============================================================================
# (OPTIONAL) STRATEGIA C propusa de echipa - cerinta 4(c), aduce bonus
# =============================================================================
# Idee: verificare adaptiva cu PRAG (threshold). Verifici un procent de baza
# in zile normale, dar daca volumul depaseste un prag de "alerta", verifici
# un procent mult mai mare. Util de povestit ("modul de criza").
# O lasam ca schelet - o puteti activa daca vreti puncte de bonus.
#
# strategie_prag <- function(N, S, f_baza, f_alerta, prag) {
#   if (f_baza < 0 || f_alerta > 1) stop("Procentele trebuie in [0,1].")
#   f <- if (N > prag) f_alerta else f_baza
#   V <- min(round(f * N), N)
#   D <- detecteaza(N, S, V)
#   list(verificate = V, detectate = D, nedetectate = S - D, f_aplicat = f)
# }


# =============================================================================
# BLOC DE TEST (comentat) - de-comenteaza ca sa verifici manual functiile
# =============================================================================
# Necesita ca 01_model.R sa fie incarcat (pentru genereaza_zi).
#
# source("01_model.R")
# set.seed(123)
# zi <- genereaza_zi(mu = 400, size = 20, p = 0.005)
# cat("Curse:", zi$total_curse, " Incidente:", zi$curse_suspecte, "\n")
#
# rez_fix <- strategie_fixa(zi$total_curse, zi$curse_suspecte, f = 0.10)
# cat("FIX 10%:    verificate =", rez_fix$verificate,
#     " detectate =", rez_fix$detectate,
#     " nedetectate =", rez_fix$nedetectate, "\n")
#
# rez_adapt <- strategie_adaptiva(zi$total_curse, zi$curse_suspecte,
#                                 f_min = 0.05, f_max = 0.30,
#                                 N_min = 300, N_max = 500)
# cat("ADAPTIV:    verificate =", rez_adapt$verificate,
#     " detectate =", rez_adapt$detectate,
#     " (procent aplicat:", round(rez_adapt$f_aplicat, 3), ")\n")