# =============================================================================
# main_P1.R  --  Scriptul PRINCIPAL al Problemei 1 (incidente tehnice la metrou)
# =============================================================================
#
# ACESTA este SINGURUL fisier pe care il rulezi efectiv pentru Problema 1.
# Celelalte (01-05) doar DEFINESC functii; acest fisier le ORCHESTREAZA:
#   1) incarca dependentele (pachete) si verifica daca sunt instalate;
#   2) face source la 01-05 in ordinea corecta;
#   3) fixeaza UN SINGUR set.seed (reproductibilitate intr-un singur loc);
#   4) ruleaza tot pipeline-ul (simulare, indicatori, cost, grafice);
#   5) salveaza tabelele si graficele finale in foldere dedicate, pentru C.
#
# CUM SE RULEAZA:
#   - din RStudio: deschide acest fisier si apasa "Source";
#   - din terminal: Rscript main_P1.R
#   IMPORTANT: directorul de lucru trebuie sa fie folderul P1_simulare/
#   (unde se afla 01-05). In RStudio: Session > Set Working Directory >
#   To Source File Location.
#
# ACOPERIRE CERINTE (harta completa):
#   Cerinta 1     -> simuleaza_an (model pe un an calendaristic)
#   Cerinta 3     -> bucla pe cele 3 scenarii de p (0.001, 0.005, 0.02)
#   Cerinta 4     -> strategie_fixa + strategie_adaptiva
#   Cerinta 5     -> calculeaza_indicatori (cei 5 indicatori)
#   Cerinta 6     -> cele 4 grafice obligatorii
#   Cerinta 7     -> studiu_procent_verificare + grafic_efect_procent
#   Cerinta ult.1 -> calculeaza_cost + compara_strategii_cost
#   Cerinta ult.2 -> repeta_simulare (1000 de repetari, medie vs. variabilitate)
#   Cerinta ult.3 -> comparatia teorie (prob_teoretica) vs. simulare
# =============================================================================

# =============================================================================
# (0) PARAMETRII GLOBALI AI STUDIULUI  -  schimba-i AICI, intr-un singur loc
# =============================================================================
# Centralizam toti parametrii ca sa fie usor de ajustat si de documentat de C.
# Daca schimbi 'mu' aici, ajusteaza si pragurile adaptive (N_MIN/N_MAX) sub el.

MU_CURSE   <- 400      # media numarului de curse pe zi (E[N])
SIZE_DISP  <- 20       # dispersia binomialei negative (mic => supra-dispersie)
MODEL_N    <- "nbinom" # modelul pentru numarul de curse ("nbinom" / "poisson")

N_ZILE     <- 365      # zile intr-un an calendaristic
N_REP      <- 1000     # repetari Monte Carlo (cerinta ult. 2: cel putin 1000)

# Cele 3 scenarii de probabilitate ceruta explicit (cerinta 3).
P_SCENARII <- c(0.001, 0.005, 0.02)
P_PRINCIPAL <- 0.005   # scenariul "central" folosit pentru graficele detaliate

# Parametrii strategiei adaptive (potriviti la MU_CURSE: sub medie verifici
# putin, peste medie verifici mult).
F_FIXA   <- 0.05       # procentul strategiei fixe (5%)
F_MIN    <- 0.02      # procentul minim al strategiei adaptive
F_MAX    <- 0.08       # procentul maxim al strategiei adaptive
N_MIN    <- 300        # sub acest volum -> F_MIN
N_MAX    <- 500        # peste acest volum -> F_MAX

# Costuri unitare pentru functia de cost (cerinta ult. 1). Ipoteza de echipa:
# ratarea unui incident e mult mai scumpa decat o verificare (raport 200:1).
C1_VERIF <- 1          # costul unei verificari
C2_RATARE <- 200       # costul unui incident nedetectat

SEED <- 2026           # samanta unica pentru reproductibilitate


# =============================================================================
# (1) INCARCAREA PACHETELOR  (cu verificare prietenoasa de instalare)
# =============================================================================
# Verificam fiecare pachet inainte sa-l incarcam; daca lipseste, dam un mesaj
# clar in loc de o eroare criptica la jumatatea rularii.
verifica_pachet <- function(nume, obligatoriu = TRUE) {
  if (!requireNamespace(nume, quietly = TRUE)) {
    mesaj <- paste0("Pachetul '", nume, "' nu este instalat. Ruleaza:  install.packages('", nume, "')")
    if (obligatoriu) stop(mesaj) else message("(optional) ", mesaj)
    return(FALSE)
  }
  TRUE
}

verifica_pachet("ggplot2", obligatoriu = TRUE)
library(ggplot2)

# patchwork e OPTIONAL: doar combina graficele intr-o singura imagine. Daca
# lipseste, salvam graficele individual si mergem mai departe.
ARE_PATCHWORK <- verifica_pachet("patchwork", obligatoriu = FALSE)
if (ARE_PATCHWORK) library(patchwork)


# =============================================================================
# (2) INCARCAREA FUNCTIILOR DIN 01-05  (ordinea conteaza!)
# =============================================================================
# Ordinea e importanta: 03 foloseste functii din 01 si 02; 04 si 05 folosesc
# date produse de 03. Le incarcam de jos in sus pe lantul de dependente.
source("01_model.R")      # model: genereaza_zi, prob_teoretica_...
source("02_strategii.R")  # strategii: strategie_fixa, strategie_adaptiva, ...
source("03_simulare.R")   # motor: simuleaza_an, repeta_simulare, ...
source("04_grafice.R")    # grafice: grafic_hist_*, grafic_comparativ_*, ...
source("05_cost.R")       # cost: calculeaza_cost, compara_strategii_cost, ...


# =============================================================================
# (3) PREGATIREA FOLDERELOR DE IESIRE  (pentru C / documentatie)
# =============================================================================
# Cream folderele unde salvam figurile si tabelele, daca nu exista deja.
if (!dir.exists("figuri")) dir.create("figuri")
if (!dir.exists("tabele")) dir.create("tabele")


# =============================================================================
# (4) REPRODUCTIBILITATE: UN SINGUR set.seed, AICI
# =============================================================================
# Toata aleatoritatea proiectului porneste de aici. Cu aceeasi samanta, rularea
# produce EXACT aceleasi rezultate -> esential pentru documentatie reproductibila.
set.seed(SEED)


# =============================================================================
# (5) DEFINIREA STRATEGIILOR ca functii de (N, S)
# =============================================================================
# Motorul de simulare asteapta strategii de forma functie(N, S). "Inchidem"
# parametrii ca sa ramana doar (N, S) - tehnica de programare functionala.
strat_fixa <- function(N, S) {
  strategie_fixa(N, S, f = F_FIXA)
}
strat_adaptiva <- function(N, S) {
  strategie_adaptiva(N, S, f_min = F_MIN, f_max = F_MAX,
                     N_min = N_MIN, N_max = N_MAX)
}

# Lista numita de strategii (numele devine eticheta in tabele si grafice).
STRATEGII <- list(
  Fixa     = strat_fixa,
  Adaptiva = strat_adaptiva
)


# =============================================================================
# (6) SIMULAREA UNUI AN PENTRU FIECARE STRATEGIE  (cerinta 1)
#     -> date pentru histograme si evolutia zilnica
# =============================================================================
cat("\n[1/6] Simulez cate un an pentru fiecare strategie...\n")

an_fix   <- simuleaza_an(strat_fixa,     p = P_PRINCIPAL, n_zile = N_ZILE,
                         model = MODEL_N, mu = MU_CURSE, size = SIZE_DISP)
an_adapt <- simuleaza_an(strat_adaptiva, p = P_PRINCIPAL, n_zile = N_ZILE,
                         model = MODEL_N, mu = MU_CURSE, size = SIZE_DISP)

# Salvam tabelele "an" pentru ca C sa le poata inspecta / pune in anexa.
write.csv(an_fix,   "tabele/an_strategie_fixa.csv",     row.names = FALSE)
write.csv(an_adapt, "tabele/an_strategie_adaptiva.csv", row.names = FALSE)


# =============================================================================
# (7) CEI 5 INDICATORI + REPETAREA MONTE CARLO  (cerintele 5 si ult. 2)
# =============================================================================
cat("[2/6] Rulez ", N_REP, " repetari Monte Carlo per strategie (poate dura)...\n")

# Indicatorii dintr-un singur an (ilustrativ):
ind_fix_1an   <- calculeaza_indicatori(an_fix)
ind_adapt_1an <- calculeaza_indicatori(an_adapt)

# Rezumatul pe N_REP repetari: medie + abatere standard (cerinta ult. 2).
rezumat_fix   <- repeta_simulare(strat_fixa,     p = P_PRINCIPAL, n_rep = N_REP,
                                 n_zile = N_ZILE, model = MODEL_N,
                                 mu = MU_CURSE, size = SIZE_DISP)
rezumat_adapt <- repeta_simulare(strat_adaptiva, p = P_PRINCIPAL, n_rep = N_REP,
                                 n_zile = N_ZILE, model = MODEL_N,
                                 mu = MU_CURSE, size = SIZE_DISP)

cat("\n--- Indicatori medii pe", N_REP, "repetari (strategia FIXA) ---\n")
print(rezumat_fix)
cat("\n--- Indicatori medii pe", N_REP, "repetari (strategia ADAPTIVA) ---\n")
print(rezumat_adapt)

write.csv(rezumat_fix,   "tabele/rezumat_fixa.csv",     row.names = FALSE)
write.csv(rezumat_adapt, "tabele/rezumat_adaptiva.csv", row.names = FALSE)


# =============================================================================
# (8) CELE 3 SCENARII DE p  (cerinta 3)
# =============================================================================
cat("[3/6] Compar cele 3 scenarii de p (", paste(P_SCENARII, collapse = ", "), ")...\n")

# Pentru fiecare p, rezumam ambele strategii. Adunam totul intr-un tabel lung.
rezultate_p <- list()
for (p_test in P_SCENARII) {
  for (nume_strat in names(STRATEGII)) {
    rez <- repeta_simulare(STRATEGII[[nume_strat]], p = p_test, n_rep = N_REP,
                           n_zile = N_ZILE, model = MODEL_N,
                           mu = MU_CURSE, size = SIZE_DISP)
    rez$p         <- p_test
    rez$strategie <- nume_strat
    rezultate_p[[length(rezultate_p) + 1]] <- rez
  }
}
tabel_scenarii_p <- do.call(rbind, rezultate_p)
write.csv(tabel_scenarii_p, "tabele/scenarii_p.csv", row.names = FALSE)


# =============================================================================
# (9) EFECTUL PROCENTULUI DE VERIFICARE  (cerinta 7)
# =============================================================================
cat("[4/6] Studiez efectul procentului de verificare...\n")

tabel_procente <- studiu_procent_verificare(
  p = P_PRINCIPAL, procente = c(0.01, 0.05, 0.10, 0.20, 0.30),
  n_rep = 200, n_zile = N_ZILE, model = MODEL_N,
  mu = MU_CURSE, size = SIZE_DISP
)
print(tabel_procente)
write.csv(tabel_procente, "tabele/efect_procent.csv", row.names = FALSE)


# =============================================================================
# (10) ANALIZA DE COST  (cerinta ult. 1)
# =============================================================================
cat("[5/6] Analizez costurile strategiilor...\n")

# Compararea strategiilor dupa cost mediu (cine minimizeaza costul?).
tabel_cost <- compara_strategii_cost(
  STRATEGII, p = P_PRINCIPAL, c1 = C1_VERIF, c2 = C2_RATARE, n_rep = N_REP,
  n_zile = N_ZILE, model = MODEL_N, mu = MU_CURSE, size = SIZE_DISP
)
cat("\n--- Cost mediu pe zi (c1 =", C1_VERIF, ", c2 =", C2_RATARE, ") ---\n")
print(tabel_cost)
write.csv(tabel_cost, "tabele/comparatie_cost.csv", row.names = FALSE)

# Studiul sensibilitatii la raportul c2/c1 (bonus, foarte vizual).
tabel_sensibilitate <- studiu_sensibilitate_cost(
  STRATEGII, p = P_PRINCIPAL, n_rep = 200,
  n_zile = N_ZILE, model = MODEL_N, mu = MU_CURSE, size = SIZE_DISP
)
write.csv(tabel_sensibilitate, "tabele/sensibilitate_cost.csv", row.names = FALSE)


# =============================================================================
# (11) COMPARATIA TEORIE vs. SIMULARE  (cerinta ult. 3)
# =============================================================================
cat("[6/6] Compar probabilitatea teoretica cu cea empirica...\n")

# Probabilitatea teoretica (model Poisson + thinning): 1 - exp(-lambda*p).
prob_teoretica <- prob_teoretica_cel_putin_un_incident(lambda = MU_CURSE, p = P_PRINCIPAL)

# Probabilitatea empirica = frecventa zilelor cu cel putin un incident in anul
# simulat (model binomiala negativa, supra-dispersat).
prob_empirica <- mean(an_fix$curse_suspecte >= 1)

cat("\n--- Teorie vs. simulare: P(cel putin un incident intr-o zi) ---\n")
cat("  Teoretic (Poisson, lambda*p):", round(prob_teoretica, 4), "\n")
cat("  Empiric  (Binomiala Negativa):", round(prob_empirica, 4), "\n")
cat("  NOTA: diferenta apare fiindca teoria presupune Poisson, iar simularea\n")
cat("        foloseste Binomiala Negativa (supra-dispersata). Punct de discutie!\n")

# Salvam si asta intr-un mic tabel pentru documentatie.
tabel_teorie <- data.frame(
  marime   = "P(cel putin un incident pe zi)",
  teoretic = prob_teoretica,
  empiric  = prob_empirica
)
write.csv(tabel_teorie, "tabele/teorie_vs_simulare.csv", row.names = FALSE)


# =============================================================================
# (12) GENERAREA SI SALVAREA GRAFICELOR  (cerintele 6 si 7)
# =============================================================================
cat("\nGenerez si salvez graficele in folderul 'figuri/'...\n")

# Cele 4 grafice obligatorii (cerinta 6) + cel de la cerinta 7.
g_hist_susp <- grafic_hist_suspecte(an_fix)
g_hist_det  <- grafic_hist_detectate(an_fix)
g_evolutie  <- grafic_evolutie_zilnica(an_fix, fereastra = 7)
g_comparativ <- grafic_comparativ_strategii(rezumat_fix, rezumat_adapt)
g_efect     <- grafic_efect_procent(tabel_procente)

# Salvam fiecare grafic individual (mereu disponibil, indiferent de patchwork).
salveaza_grafic(g_hist_susp,  "figuri/01_hist_suspecte.png")
salveaza_grafic(g_hist_det,   "figuri/02_hist_detectate.png")
salveaza_grafic(g_evolutie,   "figuri/03_evolutie_zilnica.png")
salveaza_grafic(g_comparativ, "figuri/04_comparativ_strategii.png")
salveaza_grafic(g_efect,      "figuri/05_efect_procent.png")

# Daca avem patchwork, salvam si un "panou" combinat, frumos pentru documentatie.
if (ARE_PATCHWORK) {
  panou <- (g_hist_susp | g_hist_det) / g_evolutie
  salveaza_grafic(panou, "figuri/00_panou_combinat.png",
                  latime = 12, inaltime = 9)
}


# =============================================================================
# (13) GATA
# =============================================================================
cat("\n=============================================================\n")
cat(" Simularea Problemei 1 s-a incheiat cu succes.\n")
cat(" Tabele salvate in:  tabele/\n")
cat(" Grafice salvate in: figuri/\n")
cat(" Samanta folosita (pentru reproductibilitate): ", SEED, "\n")
cat("=============================================================\n")