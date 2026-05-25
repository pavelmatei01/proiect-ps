# ============================================================================
#  03_simulare.R
#  ---------------------------------------------------------------------------
#  PROBLEMA 1 — Evenimente rare: detectie si simulare
#
#  Acest fisier contine MOTORUL DE SIMULARE:
#    (1) simuleaza_an()          -> ruleaza 365 de zile cap la cap
#    (2) simuleaza_monte_carlo() -> repeta un an de N ori (ex: 1000)
#
#  NOTA DE PROIECTARE: Functiile de aici asteapta ca `01_model.R` si
#  `02_strategii.R` sa fie deja incarcate (de regula prin `main_P1.R`).
# ============================================================================

# ----------------------------------------------------------------------------
#  ASPECTE TEORETICE — DE CE SIMULAM ASTFEL? (citit de C pentru documentatie)
# ----------------------------------------------------------------------------
#
#  ## 1. Agregarea pe un an (365 de zile)
#
#  O singura zi este prea zgomotoasa (varianta mare). Unele zile nu au niciun
#  incident, altele pot avea 2-3. Managerii metroului nu iau decizii bazate
#  pe o zi, ci pe bugete si riscuri ANUALE. De aceea, construim "un an" din
#  365 de extrageri independente.
#
#  ## 2. Metoda Monte Carlo (1000 de iteratii)
#
#  De ce nu e de ajuns sa simulam un singur an? Pentru ca un singur an este
#  doar o singura traiectorie posibila (o realizare a procesului stocastic).
#  Daca intr-un an am prins 90% din incidente, poate am fost doar norocosi.
#
#  Repetand anul de M = 1000 de ori (Monte Carlo), obtinem o DISTRIBUTIE a
#  rezultatelor anuale. Conform Legii Numerelor Mari (LLN), media acestor 1000
#  de simulari va converge catre valoarea teoretica asteptata (speranta
#  matematica E[X]).
#
#  Asta ne permite sa raspundem la intrebari probabilistice complexe in
#  `04_grafice.R` si `05_cost.R`:
#    - "Care este probabilitatea ca intr-un an sa ne scape > 10 incidente?"
#    - "Care este costul MEDIU anual si in cel mai rau caz (percentila 95)?"
# ----------------------------------------------------------------------------


# ============================================================================
#  (A) SIMULAREA UNUI SINGUR AN (365 zile)
# ============================================================================

#' Simuleaza activitatea si verificarile pe parcursul unui an intreg.
#'
#' @param zile            Numarul de zile (implicit 365).
#' @param p               Probabilitatea de incident pe cursa.
#' @param model_curse     "nbinom" sau "poisson" (din 01_model.R).
#' @param mu              Media curselor pe zi.
#' @param size            Dispersia curselor (pentru nbinom).
#' @param tip_strategie   Tipul strategiei: "fixa" sau "adaptiva".
#' @param param_strategie O lista numita cu parametrii necesari strategiei.
#'                        Ex pt fixa: list(f = 0.10)
#'                        Ex pt adaptiva: list(f_min=0.05, f_max=0.30, N_min=300, N_max=500)
#' @return Un data.frame cu `zile` randuri, fiecare rand fiind rezultatul unei zile.
simuleaza_an <- function(zile = 365, p = 0.005,
                         model_curse = "nbinom", mu = 400, size = 20,
                         tip_strategie = "fixa", param_strategie = list(f = 0.10)) {
  
  # --- Programare defensiva: pre-alocam vectori pentru performanta ---
  # In R, extinderea unui vector in interiorul unui for-loop este lenta.
  # Este mult mai eficient sa alocam spatiul de la inceput.
  vec_total_curse  <- integer(zile)
  vec_incidente    <- integer(zile)
  vec_verificate   <- integer(zile)
  vec_detectate    <- integer(zile)
  vec_nedetectate  <- integer(zile)
  
  for (i in 1:zile) {
    # 1. Generam realitatea din sistem (ce se intampla)
    zi_curenta <- genereaza_zi(p = p, model = model_curse, mu = mu, size = size)
    
    N <- zi_curenta$total_curse
    S <- zi_curenta$curse_suspecte
    
    # 2. Aplicam strategia pentru a vedea ce detectam
    if (tip_strategie == "fixa") {
      # Extragem parametrul f din lista, cu valoare implicita fallback
      f_val <- if (!is.null(param_strategie$f)) param_strategie$f else 0.10
      rez_strat <- strategie_fixa(N = N, S = S, f = f_val)
      
    } else if (tip_strategie == "adaptiva") {
      # Extragem parametrii adaptivi
      rez_strat <- strategie_adaptiva(
        N = N, S = S,
        f_min = param_strategie$f_min,
        f_max = param_strategie$f_max,
        N_min = param_strategie$N_min,
        N_max = param_strategie$N_max
      )
      
    } else {
      stop("Tip strategie necunoscut. Folositi 'fixa' sau 'adaptiva'.")
    }
    
    # 3. Salvam rezultatele in vectori
    vec_total_curse[i] <- N
    vec_incidente[i]   <- S
    vec_verificate[i]  <- rez_strat$verificate
    vec_detectate[i]   <- rez_strat$detectate
    vec_nedetectate[i] <- rez_strat$nedetectate
  }
  
  # Returnam un tabel (data.frame) cu detaliile anuale zi de zi.
  # Asta ajuta la graficele de detaliu (daca vrem sa plotam evolutia pe zile).
  data.frame(
    zi          = 1:zile,
    total_curse = vec_total_curse,
    incidente   = vec_incidente,
    verificate  = vec_verificate,
    detectate   = vec_detectate,
    nedetectate = vec_nedetectate
  )
}


# ============================================================================
#  (B) SIMULARE MONTE CARLO (M repetari ale unui an)
# ============================================================================

#' Ruleaza simularea anuala de M ori si returneaza agregatele pe fiecare an.
#'
#' @param n_sim     Numarul de repetari (ex: 1000).
#' @param seed      Numarul pentru reproductibilitate (set.seed).
#' @param ...       Orice alte argumente se trimit mai departe catre `simuleaza_an`.
#' @return Un data.frame cu `n_sim` randuri. Fiecare rand = totalurile dintr-un an.
simuleaza_monte_carlo <- function(n_sim = 1000, seed = 123, ...) {
  
  # Asiguram reproductibilitatea (aceleasi rezultate la fiecare rulare)
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Folosim lapply pentru viteza si eleganta in loc de un for-loop greoi.
  # lapply va returna o lista de liste, pe care o legam cu do.call(rbind, ...)
  rezultate_lista <- lapply(1:n_sim, function(i) {
    
    # Rulam un an intreg
    an_curent <- simuleaza_an(...)
    
    # Calculam TOTALURILE pe acel an
    # Aici intervine "reducerea": convertim 365 de zile intr-un singur rand de statistici.
    an_agregat <- data.frame(
      sim_id            = i,
      total_curse_an    = sum(an_curent$total_curse),
      total_incidente_an= sum(an_curent$incidente),
      total_verificate  = sum(an_curent$verificate),
      total_detectate   = sum(an_curent$detectate),
      total_nedetectate = sum(an_curent$nedetectate)
    )
    
    return(an_agregat)
  })
  
  # Combinam toate cele `n_sim` randuri intr-un singur data.frame central
  df_final <- do.call(rbind, rezultate_lista)
  
  return(df_final)
}


# ============================================================================
#  (C) BLOC DE TEST (comentat) - pentru a verifica manual motorul
# ============================================================================
#  De-comenteaza cand esti in RStudio ca sa verifici ca se leaga bine
#  cu 01_model.R si 02_strategii.R.
#
# source("01_model.R")
# source("02_strategii.R")
#
# cat("Rulam test: 1 An, Strategie Fixa 10% ...\n")
# test_an <- simuleaza_an(zile = 365, p = 0.005, tip_strategie = "fixa", param_strategie = list(f = 0.10))
# cat("Total incidente an:", sum(test_an$incidente), "| Prinse:", sum(test_an$detectate), "\n\n")
#
# cat("Rulam test: Monte Carlo (100 ani), Strategie Adaptiva ...\n")
# parametri_adapt <- list(f_min = 0.05, f_max = 0.20, N_min = 350, N_max = 450)
# mc_results <- simuleaza_monte_carlo(
#   n_sim = 100, zile = 365, p = 0.005,
#   tip_strategie = "adaptiva",
#   param_strategie = parametri_adapt
# )
# cat("Media incidentelor scapate (nedetectate) anual:", mean(mc_results$total_nedetectate), "\n")
# ============================================================================