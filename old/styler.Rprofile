# ------------------------------------------------------------------------------
# .Rprofile : configuration utilisateur ou projet
# ------------------------------------------------------------------------------
# Ce fichier est exécuté automatiquement au démarrage de R/RStudio.
# Ici, on active le formatage automatique avec {styler} à chaque sauvegarde.
# ------------------------------------------------------------------------------

if (interactive()) {
  # Hook : quand on sauvegarde un fichier dans RStudio → styler
  setHook("rstudio.onSave", function(...) {
    tryCatch(
      styler::style_active_file(),
      error = function(e) message("⚠️ Erreur styler: ", e$message)
    )
  })
  
  message("✨ Formatage automatique avec {styler} activé (à chaque sauvegarde).")
}
