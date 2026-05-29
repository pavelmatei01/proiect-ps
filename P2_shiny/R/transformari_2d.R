# =============================================================================
# transformari_2d.R  --  Componenta bidimensionala: Z = h(X, Y)
# =============================================================================
#
# ROLUL ACESTUI FISIER (in arhitectura Problemei 2):
#   Contine LOGICA pentru sectiunea 2D (obligatorie): generarea perechii (X, Y)
#   in doua moduri si cele 4 transformari Z = h(X, Y). server.R doar apeleaza.
#   Reutilizeaza genereaza_esantion() din distributii.R pentru cazul independent.
#
# DEPENDENTE: pachetul MASS (pentru mvrnorm — generare normala bivariata).
#
# -----------------------------------------------------------------------------
# ASPECTE TEORETICE — NORMALA BIVARIATA  (citit de C pentru documentatie)
# -----------------------------------------------------------------------------
# O pereche (X, Y) normala bivariata e definita de vectorul de medii (muX, muY)
# si de MATRICEA DE COVARIANTA:
#
#       Sigma = | sigmaX^2          rho*sigmaX*sigmaY |
#               | rho*sigmaX*sigmaY  sigmaY^2         |
#
# Diagonala = variantele individuale. Off-diagonala = COVARIANTA dintre X si Y,
# egala cu rho*sigmaX*sigmaY. Coeficientul de corelatie rho "leaga" variabilele
# prin acest termen.
#
# CONSTRANGERE: -1 < rho < 1 (STRICT). La rho = +/-1 matricea e singulara (X si Y
# perfect dependente), iar generarea poate esua. De aceea validam strict.
#
# PROPRIETATI TEORETICE (de comparat cu empiricul - bonus):
#   Var(X + Y) = sigmaX^2 + sigmaY^2 + 2*rho*sigmaX*sigmaY
#   Var(X - Y) = sigmaX^2 + sigmaY^2 - 2*rho*sigmaX*sigmaY
#   E[X*Y]     = rho*sigmaX*sigmaY + muX*muY
# Arata cum corelatia afecteaza DIFERIT suma si diferenta.
# =============================================================================


# =============================================================================
# (A') FUNCTIE DISPECER UNIFICATA: genereaza_pereche(mod, n, ...)
# =============================================================================
# server.R apeleaza un singur punct de intrare genereaza_pereche(mod, n, ...),
# care alege intre cele doua moduri. E mai curat decat sa stie server-ul de
# ambele functii specializate. Argumentele suplimentare se paseaza prin ...:
#   - mod = "binormal":    asteapta mu_X, mu_Y, sigma_X, sigma_Y, rho
#   - mod = "independent": asteapta dist_X, par_X, dist_Y, par_Y
genereaza_pereche <- function(mod, n, ...) {
  args <- list(...)
  if (mod == "binormal") {
    genereaza_pereche_bivariata(
      muX = args$mu_X, muY = args$mu_Y,
      sigmaX = args$sigma_X, sigmaY = args$sigma_Y,
      rho = args$rho, n = n
    )
  } else if (mod == "independent") {
    genereaza_pereche_independenta(
      dist_X = args$dist_X, par_X = args$par_X,
      dist_Y = args$dist_Y, par_Y = args$par_Y, n = n
    )
  } else {
    stop("Mod de generare necunoscut: ", mod)
  }
}

# Alias: server.R foloseste numele 'valideaza_parametri_2d'. Il legam la
# valideaza_2d definita mai jos, ca ambele nume sa functioneze.
valideaza_parametri_2d <- function(sigmaX, sigmaY, rho) {
  valideaza_2d(sigmaX, sigmaY, rho)
}


# =============================================================================
# (A) GENERAREA PERECHII (X, Y) — doua moduri (cerinta 1 sectiunea 2D)
# =============================================================================

# --- Mod 1: X si Y INDEPENDENTE, fiecare cu repartitia aleasa ---
# Refoloseste genereaza_esantion() din distributii.R. Esantioane separate =>
# independente (corelatia teoretica = 0).
genereaza_pereche_independenta <- function(dist_X, par_X, dist_Y, par_Y, n) {
  X <- genereaza_esantion(dist_X, par_X, n)
  Y <- genereaza_esantion(dist_Y, par_Y, n)
  list(X = X, Y = Y)
}

# --- Mod 2: (X, Y) NORMALA BIVARIATA cu corelatie rho ---
# Genereaza din N2((muX,muY), Sigma) folosind MASS::mvrnorm.
genereaza_pereche_bivariata <- function(muX, muY, sigmaX, sigmaY, rho, n) {

  # Validare defensiva.
  if (sigmaX <= 0 || sigmaY <= 0) stop("sigmaX si sigmaY trebuie strict pozitive.")
  if (rho <= -1 || rho >= 1)      stop("rho trebuie strict in intervalul (-1, 1).")

  # Verificam ca MASS e disponibil.
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Pachetul 'MASS' nu e instalat. Ruleaza: install.packages('MASS')")
  }

  # Matricea de covarianta: off-diagonala = rho * sigmaX * sigmaY.
  covXY <- rho * sigmaX * sigmaY
  Sigma <- matrix(c(sigmaX^2, covXY,
                    covXY,    sigmaY^2),
                  nrow = 2, byrow = TRUE)

  # mvrnorm intoarce o matrice n x 2: coloana 1 = X, coloana 2 = Y.
  esantion <- MASS::mvrnorm(n = n, mu = c(muX, muY), Sigma = Sigma)
  list(X = esantion[, 1], Y = esantion[, 2])
}


# =============================================================================
# (B) VALIDAREA PARAMETRILOR 2D  (cerinta 3 Shiny, cazul bivariat)
# =============================================================================
# Returneaza NULL daca e ok, sau un mesaj de eroare (string).
valideaza_2d <- function(sigmaX, sigmaY, rho) {
  e_numar <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x)

  if (!e_numar(sigmaX) || !e_numar(sigmaY)) return("sigmaX si sigmaY trebuie sa fie numere.")
  if (sigmaX <= 0) return("sigmaX trebuie sa fie strict pozitiv.")
  if (sigmaY <= 0) return("sigmaY trebuie sa fie strict pozitiv.")
  if (!e_numar(rho)) return("rho trebuie sa fie un numar.")
  if (rho <= -1 || rho >= 1) return("rho trebuie sa fie strict intre -1 si 1 (exclusiv).")

  NULL
}


# =============================================================================
# (C) CELE 4 TRANSFORMARI Z = h(X, Y)  (cerinta 4 sectiunea 2D)
# =============================================================================
aplica_transformare_2d <- function(transformare, X, Y) {

  if (length(X) != length(Y)) stop("X si Y trebuie sa aiba aceeasi lungime.")

  switch(transformare,
    "suma"      = X + Y,                # h(X,Y) = X + Y
    "diferenta" = X - Y,                # h(X,Y) = X - Y
    "produs"    = X * Y,                # h(X,Y) = X * Y
    "norma"     = sqrt(X^2 + Y^2),      # h(X,Y) = sqrt(X^2 + Y^2)
    stop("Transformare 2D necunoscuta: ", transformare)
  )
}


# =============================================================================
# (D) INDICATORII 2D  (cerinta 5 sectiunea 2D)
# =============================================================================
# Mediile, dispersiile lui X, Y, Z + covarianta si corelatia empirica X-Y.
indicatori_2d <- function(X, Y, Z) {
  list(
    media_X = mean(X), media_Y = mean(Y), media_Z = mean(Z),
    var_X   = var(X),  var_Y   = var(Y),  var_Z   = var(Z),
    cov_XY  = cov(X, Y),
    cor_XY  = cor(X, Y)
  )
}


# =============================================================================
# (E) BLOC DE TEST (comentat)
# =============================================================================
# source("R/distributii.R"); source("R/transformari_2d.R")
# set.seed(42)
# per <- genereaza_pereche_bivariata(0, 0, 1, 1, rho = 0.5, n = 100000)
# Z <- aplica_transformare_2d("suma", per$X, per$Y)
# ind <- indicatori_2d(per$X, per$Y, Z)
# cat("cor empiric =", round(ind$cor_XY, 3), " (cerut 0.5)\n")
# cat("Var(X+Y) =", round(ind$var_Z, 3), " (teoretic 3)\n")
# print(valideaza_2d(1, 1, rho = 1))    # mesaj eroare
# print(valideaza_2d(1, 1, rho = 0.5))  # NULL
# =============================================================================