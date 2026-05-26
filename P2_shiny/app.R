# =============================================================================
# app.R  --  Punctul de pornire al aplicatiei Shiny (Problema 2)
# =============================================================================
#
# ROLUL ACESTUI FISIER:
#   Acesta e fisierul pe care il rulezi ca sa pornesti aplicatia. El doar:
#     1) incarca pachetele necesare;
#     2) face source la fisierele cu LOGICA (din folderul R/);
#     3) face source la ui.R si server.R;
#     4) porneste aplicatia cu shinyApp(ui, server).
#
# De ce structura asta (app.R + ui.R + server.R + R/)?
#   - Tine fisierele SUBTIRI: ui.R = doar interfata, server.R = doar reactivitate.
#     Logica grea (generarea repartitiilor, transformarile) sta separat in R/,
#     ca sa nu se umfle server.R si ca A si B sa nu se calce pe fisiere.
#   - Shiny incarca automat fisierele din R/ la pornire DACA folosesti structura
#     standard, dar le incarcam explicit cu source() ca sa fie clar si portabil.
#
# CUM SE RULEAZA:
#   - din RStudio: deschide app.R si apasa "Run App" (dreapta sus);
#   - din consola: shiny::runApp("P2_shiny")
#   IMPORTANT: directorul de lucru trebuie sa fie folderul P2_shiny/.
# =============================================================================

# --- 1) Pachete ---
# shiny  = framework-ul aplicatiei. ggplot2 = grafice. Le verificam prietenos.
if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Pachetul 'shiny' nu e instalat. Ruleaza: install.packages('shiny')")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Pachetul 'ggplot2' nu e instalat. Ruleaza: install.packages('ggplot2')")
}
library(shiny)
library(ggplot2)

# --- 2) Incarcam LOGICA din folderul R/ ---
# Deocamdata aceste fisiere sunt schelete; le vom umple la pasii urmatori
# (distributii.R, transformari_1d.R, transformari_2d.R). Le incarcam cu source()
# ca functiile lor sa fie disponibile in server.R.
# NOTA: folosim un source "tolerant" — daca un fisier e inca gol, nu pica totul.
fisiere_logica <- c(
  "R/distributii.R",
  "R/transformari_1d.R",
  "R/transformari_2d.R"
)
for (f in fisiere_logica) {
  if (file.exists(f)) source(f, local = FALSE)
}

# --- 3) Incarcam interfata si serverul ---
source("ui.R",     local = FALSE)
source("server.R", local = FALSE)

# --- 4) Pornim aplicatia ---
shinyApp(ui = ui, server = server)