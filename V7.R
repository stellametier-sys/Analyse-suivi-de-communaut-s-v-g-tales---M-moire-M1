# SCRIPT ANALYSE PROTOCOLE LIFE 
# Par STELLA MÉTIER - CEN Occitanie


# 1. CHARGEMENT DONNEES ----

  ## 1.1 CHARGEMENT DES PACKAGES & FONCTIONS DE BASE ----

# Packages 
local({
  req <- c("rstudioapi", "clusterCrit", "fpc", "pairwiseAdonis", "tidyverse", "vegan", "ggrepel",
           "readxl", "readr","dendextend", "FactoMineR", "factoextra", "tibble", "quarto", "httpuv")
  manquants <- req[!(req %in% installed.packages()[,"Package"])]
  if(length(manquants) > 0) install.packages(manquants, dependencies = TRUE)
})

library(rstudioapi)
library(clusterCrit)
library(fpc)
library(pairwiseAdonis)
library(tidyverse)
library(vegan)
library(ggrepel)
library(readxl)
library(readr)
library(FactoMineR)
library(factoextra)
library(tibble)
library(quarto)
library(httpuv)
library(dendextend)

# Rafraîchit le Viewer avec le contenu HTML + un bouton "Continuer" collé en bas

rafraichir_viewer <- function(texte_html, avec_bouton = FALSE) {
  bouton <- if (avec_bouton) "
  <div id='bouton-wrapper' style='
      position: fixed; bottom: 0; left: 0; right: 0;
      background: rgba(255,255,255,0.96);
      border-top: 3px solid #27ae60;
      padding: 14px 20px;
      text-align: center;
      z-index: 9999;
      box-shadow: 0 -2px 8px rgba(0,0,0,0.1);'>
    <button id='btn-continuer' onclick=\"
      document.getElementById('btn-continuer').disabled = true;
      document.getElementById('btn-continuer').innerText = '⏳ Chargement...';
      fetch('http://127.0.0.1:7894/continuer')
        .then(function() {
          document.getElementById('bouton-wrapper').innerHTML =
            '<p style=\\'color:#27ae60;font-weight:bold;margin:0;font-size:1.1em;\\'>✅ Continuation lancée — vous pouvez continuer à lire.</p>';
        })
        .catch(function() {
          document.getElementById('btn-continuer').disabled = false;
          document.getElementById('btn-continuer').innerText = '▶ Continuer';
        });
    \" style='
      background-color: #27ae60;
      color: white;
      border: none;
      padding: 11px 35px;
      font-size: 1em;
      border-radius: 6px;
      cursor: pointer;
      font-weight: bold;
      letter-spacing: 0.03em;'>
      ▶ Continuer
    </button>
    <p style='margin: 6px 0 0 0; font-size: 0.8em; color: #888;'>
      Faites défiler le Viewer pour lire, puis cliquez quand vous êtes prêt(e).
    </p>
  </div>
  <div style='height: 90px;'></div>" else ""
  
  html_file <- tempfile(fileext = ".html")
  writeLines(paste0(texte_html, bouton, "</body></html>"), html_file, useBytes = TRUE)
  rstudioapi::viewer(html_file)
}

valeur_recuperee <<- ""

# Fonction pause viewver 

pause_viewer <- function(texte_html, champ_saisie = NULL) {
  # 1. Nettoyage
  try(lapply(httpuv::listServers(), httpuv::stopServer), silent = TRUE)
  
  # 2. Structure HTML + Script qui attend le chargement
  # On déplace l'assignation de l'onclick DANS un window.onload
  inject <- if (!is.null(champ_saisie)) {
    paste0("
      <div style='margin-top: 15px; padding: 10px; border-top: 2px solid #27ae60;'>
        <input type='text' id='saisie_txt' placeholder='", champ_saisie, "' style='width: 100%; padding: 10px;'>
      </div>
      <script>
        window.onload = function() {
          document.getElementById('btn-continuer').onclick = function() {
            var v = document.getElementById('saisie_txt').value;
            fetch('http://127.0.0.1:7894/continuer?val=' + encodeURIComponent(v));
          };
        };
      </script>")
  } else {
    "<script>
      window.onload = function() {
        document.getElementById('btn-continuer').onclick = function() {
          fetch('http://127.0.0.1:7894/continuer');
        };
      };
    </script>"
  }
  
  bouton <- "<div id='bouton-wrapper' style='position:fixed; bottom:0; width:100%; padding:15px; background:#fff; border-top:3px solid #27ae60; text-align:center;'>
               <button id='btn-continuer' style='padding:10px 30px; cursor:pointer;'>▶ Continuer</button>
             </div><div style='height:100px;'></div>"
  
  html_file <- tempfile(fileext = ".html")
  writeLines(paste0("<meta charset='UTF-8'>", texte_html, bouton, inject), html_file, useBytes = TRUE)
  rstudioapi::viewer(html_file)
  
# Serveur
  assign("valeur_recuperee", "", envir = .GlobalEnv)
  signal_recu <- FALSE
  serveur <- httpuv::startServer("127.0.0.1", 7894, list(
    call = function(req) {
      params <- httpuv::decodeURI(req$QUERY_STRING)
      if (grepl("val=", params)) assign("valeur_recuperee", sub(".*val=", "", params), envir = .GlobalEnv)
      signal_recu <<- TRUE
      list(status = 200L, body = "ok")
    }
  ))
  
  while (!signal_recu) { httpuv::service(50); Sys.sleep(0.1) }
  httpuv::stopServer(serveur)
}


  ## 1.2 INITIALISATION DU GUIDE ÉVOLUTIF ----

historique_guide <- "
<html>
<head>
  <meta charset='UTF-8'>
 <style>
  body { font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; background-color:#f4f7f6; padding:20px; color: #333; line-height: 1.6; }
  .header-box { background-color: #2c3e50; color: white; padding: 20px; border-radius: 8px; margin-bottom: 25px; }
  .etape { padding: 15px; margin-bottom: 15px; border-radius: 6px; background-color: #fff; border-left: 6px solid #ccc; box-shadow: 0 2px 5px rgba(0,0,0,0.05); }
  .etape-1 { border-left-color: #27ae60; } /* Vert : Succès/Validé */
  .etape-2 { border-left-color: #2980b9; } /* Bleu : Processus/Info */
  .etape-3 { border-left-color: #d35400; } /* Orange : Analyse */
  .etape-4 { border-left-color: #8e44ad; } /* Violet : Taxonomie */
  h2, h3 { margin-top: 0; color: #2c3e50; }
  table { width: 100%; border-collapse: collapse; margin: 10px 0; }
  th { background-color: #f8f9fa; padding: 10px; border: 1px solid #dee2e6; text-align: left; }
  td { padding: 10px; border: 1px solid #dee2e6; }
</style>
</head>
<body>
  <h2>📖 Mode d'emploi</h2>
  
  <div class='box-pdf'>
    ⚠️ <b>RAPPEL IMPORTANT :</b> Avant de continuer, assurez-vous d'avoir bien lu la <b>note PDF</b> qui explique sous quel format précis doivent être vos données terrain et où télécharger les bases de données Baseflor et de Dengler <i>et al.</i> (2023).
  </div>
  
  <div class='box-files'>
    📁 <b>ORGANISATION DES FICHIERS :</b> Une fois vos documents mis au bon format, ils doivent impérativement être placés ensemble dans le même dossier d'ordinateur que ce script R, et nommés très exactement ainsi : <code>Data.xlsx</code>, <code>baseflor.xlsx</code> et <code>Dengler.xlsx</code>.
  </div>
  
  <div class='etape etape-1'>
    <p style='color: #27ae60; margin-top:0;'><b>📍 ÉTAPE 1 : Définir le dossier de travail (Set As Working Directory)</b></p>
    <p>Le script doit savoir où sont cachés vos fichiers Excel. Vous avez deux options pour cela :</p>
    <ul>
      <li><b>Méthode A (Clics RStudio) :</b> Dans le panneau en bas à droite, allez sur l'onglet <b>Files</b>. Naviguez jusqu'à ouvrir votre dossier. Cliquez sur le bouton <b>More</b> (petite roue crantée bleue) et sélectionnez <b>Set As Working Directory</b>.</li>
      <li><b>Méthode B (Ligne de commande) :</b> Vous pouvez écrire directement la commande <code>setwd(\"chemin/de/votre/dossier\")</code> dans votre console en collant le chemin d'accès de votre dossier (attention à remplacer les antislashs <code>\\</code> par des slashs <code>/</code>).</li>
    </ul>
  </div>

  <div class='etape etape-2'>
    <p style='color: #2980b9; margin-top:0;'><b>🚀 ÉTAPE 2 : Comment faire tourner le script (RStudio)</b></p>
    <ul>
      <li>Vous pouvez lancer le script <b>partie par partie</b> (en sélectionnant des blocs de lignes) ou <b>tout d'un coup</b> (en faisant un <b>Ctrl + A</b>).</li>
      <li>Pour exécuter votre sélection, appuyez sur le bouton <b>'Run'</b> en haut à droite de ce panneau (ou utilisez le raccourci <b>Ctrl + Entrée</b>).</li>
      <li><b>Règle d'or :</b> Continuez à lancer le code. Le script s'arrêtera de lui-même dès qu'une action ou une validation sera requise.</li>
      <li>💡 <b>IMPORTANT :</b> Gardez un œil attentif sur cette fenêtre ! De nouvelles étapes s'ajouteront automatiquement <b>tout en bas de ce panneau</b> au fur et à mesure que vous avancerez. Faites défiler vers le bas si nécessaire.</li>
    </ul>
  </div>
"

# Premier affichage avec pause
pause_viewer(historique_guide)


  ## 1.3 CONFIGURATION INITIALE DU SITE ----

site <- rstudioapi::showPrompt(
  title = "Configuration du Site",
  message = "Entrez le nom du site \n\n(ce nom sera affiché en titre sur vos graphiques) :",
  default = ""
)



  ## 1.4 CHARGEMENT DES FICHIERS ----

historique_guide <- paste0(historique_guide, "
  <div class='etape etape-3'>
    <p style='color: #d35400; margin-top:0;'><b>📂 ÉTAPE 3 : Chargement des documents </b></p>
    <p>Le script va tenter de charger vos fichiers (<code>Data.xlsx</code>, <code>baseflor.xlsx</code> et <code>Dengler.xlsx</code>)...</p>
    <p>⚠️ <b>Si un message d'erreur apparaît ou si les tableaux sont vides/bizarres :</b></p>
    <ol style='margin-top: 5px;'>
      <li>Allez dans l'onglet <b>Files</b> (en bas à droite).</li>
      <li>Cliquez directement sur le fichier qui bloque (ex : <code>Data.xlsx</code>) et sélectionnez <b>Import Dataset...</b>.</li>
      <li>Dans la fenêtre, <b>vérifiez impérativement que vos données s'affichent correctement dans le visualiseur central</b> (sinon changez d'onglet avec l'option 'Sheet').</li>
      <li>Vérifiez ensuite en bas à droite que le champ 'Name' contient bien le nom exact sans extension (ex : juste <code>Data</code>, <code>baseflor</code> ou <code>Dengler</code>) puis cliquez sur <b>Import</b>.</li>
      <li>Le fichier réapparaîtra proprement dans 'Environment'. Vous pourrez relancer la suite du script !</li>
    </ol>
    <p>👀 <b>CONTRÔLE VISUEL OBLIGATOIRE :</b><br>
      Une fois le chargement terminé, allez dans l'onglet <b>Environment</b> (panneau en haut à droite) :<br>
      👉 Cliquez un par un sur les 3 documents chargés (<code>Data</code>, <code>baseflor</code>, <code>Dengler</code>) pour les ouvrir et <b>vérifiez qu'ils se sont importés correctement</b> (pas d'erreurs dans le nom des colonnes, format propre, données bien alignées, etc.).</p>
  </div>
")
pause_viewer(historique_guide)

fichiers_manquants <- c()
if (!file.exists("Data.xlsx"))    fichiers_manquants <- c(fichiers_manquants, "Data.xlsx")
if (!file.exists("baseflor.xlsx")) fichiers_manquants <- c(fichiers_manquants, "baseflor.xlsx")
if (!file.exists("Dengler.xlsx"))  fichiers_manquants <- c(fichiers_manquants, "Dengler.xlsx")

if (length(fichiers_manquants) > 0) {
  rstudioapi::showDialog(
    title = "⚠️ Fichiers introuvables !",
    message = paste0(
      "Le script a bloqué car il ne trouve pas le(s) fichier(s) suivant(s) :\n",
      paste("-", fichiers_manquants, collapse = "\n"),
      "\n\n👉 SOLUTIONS :\n",
      "1. Vérifiez que vous avez bien fait 'Set As Working Directory' (Étape 1 du Viewer).\n",
      "2. Vérifiez l'orthographe exacte et l'extension (.xlsx et non .csv).\n",
      "3. Importez vos fichiers manuellement via l'onglet Files (Étape 3 du Viewer)."
    )
  )
  stop("Analyse interrompue : Fichiers manquants dans le Working Directory.")
}

Data     <- read_excel("Data.xlsx")
baseflor <- read_excel("baseflor.xlsx")
Dengler  <- read_excel("Dengler.xlsx", sheet = "mainTable")

historique_guide <- paste0(historique_guide, "
  <div class='etape' style='border-left: 6px solid #2980b9; background-color: #f0f7fb;'>
    <p style='color: #2980b9; margin-top:0;'><b>✅ Étape 3 validée : Données chargées avec succès.</b></p>
    <p>Vos fichiers <code>Data</code>, <code>baseflor</code> et <code>Dengler</code> ont été importés. 
    Le script va maintenant procéder à la vérification de la structure de vos relevés (50 unités par transect et par année).<br>
    <i>👉 Cliquez sur 'Continuer' pour lancer le contrôle qualité.</i></p>
  </div>
")
pause_viewer(historique_guide)

  ## 1.5 STANDARDISATION DES QUADRATS & VÉRIFICATION ----

quadrats_standard <- c(
  "0a","0b","0c","0d", "1a","1b","1c","1d", "2a","2b","2c","2d",
  "3a","3b","3c","3d", "4a","4b","4c","4d", "5a","5b","5c","5d",
  "6a","6b","6c","6d", "7a","7b","7c","7d", "8a","8b","8c","8d",
  "9a","9b","9c","9d", "10a","10b","10c","10d", "11a","11b","11c","11d",
  "12a","12d"
)

standardiser_site <- function(data) {
  # 1. On nettoie tout de suite : on enlève les lignes sans année
  data <- data %>% filter(!is.na(Année))
  
  # Vérification des colonnes nécessaires
  stopifnot(all(c("Année", "id_transect", "id_sous_unité") %in% names(data)))
  
  # On récupère les années restantes (proprement)
  annees    <- unique(data$Année)
  transects <- unique(data$id_transect)
  
  complet <- expand_grid(Année = annees, 
                         id_transect = transects, 
                         id_sous_unité = quadrats_standard)
  
  # Fusion
  data_std <- complet %>%
    left_join(data, by = c("Année", "id_transect", "id_sous_unité")) %>%
    mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>%
    mutate(id = paste(Année, id_transect, id_sous_unité, sep = "-"))
  
  return(data_std)
}

# Application de la standardisation
Data <- standardiser_site(Data)

# Vérification pour l'interface
verif <- Data %>%
  group_by(Année, id_transect) %>%
  summarise(Nb_Sous_Quadrats = n(), .groups = "drop")

# Génération d'un tableau HTML propre pour visualiser chaque transect par année
liens_html <- verif %>%
  mutate(html = paste0(
    "<tr>
       <td style='padding: 5px; border-bottom: 1px solid #eee;'>", Année, "</td>
       <td style='padding: 5px; border-bottom: 1px solid #eee;'>", id_transect, "</td>
       <td style='padding: 5px; border-bottom: 1px solid #eee; text-align: right; color: ", 
    ifelse(Nb_Sous_Quadrats == 50, "#27ae60", "#c0392b"), "; font-weight: bold;'>", 
    Nb_Sous_Quadrats, " / 50</td>
     </tr>"
  )) %>%
  pull(html) %>%
  paste(collapse = "")

liens_html <- paste0(
  "<table style='width: 100%; border-collapse: collapse; font-family: monospace;'>
     <tr style='background-color: #eee;'>
       <th style='text-align: left; padding: 5px;'>Année</th>
       <th style='text-align: left; padding: 5px;'>Transect</th>
       <th style='text-align: right; padding: 5px;'>Unités</th>
     </tr>",
  liens_html,
  "</table>"
)

alerte_structure_excel <- "
  <div style='background-color: #fcf8e3; border-left: 4px solid #f39c12; color: #a04000; padding: 12px; margin-top: 15px; border-radius: 4px;'>
    ⚠️ <b>CONTRÔLE DE LA STRUCTURE DES DONNÉES :</b><br>
    Chaque transect par année doit posséder <b>exactement 50 unités</b>. 
    <p style='margin-top: 8px; margin-bottom: 0;'><i>👉 Si tout est à 50, le chiffre est en vert. Sinon, vérifiez vos saisies dans Data.xlsx pour l'année et le transect concernés.</i></p>
  </div>
"

historique_guide <- paste0(historique_guide, "
  <div class='etape etape-4'>
    <p style='color: #e67e22; margin-top:0;'><b>📊 ÉTAPE 4 : Vérification du nombre d'unités par année — ", site, "</b></p>
    <p>Structure détectée pour vos données de terrain :</p>
    <div style='max-width: 400px;'>", liens_html, "</div>",
                           alerte_structure_excel,
                           "</div>
")
pause_viewer(historique_guide)

# Supprimer les listes et variables temporaires 
rm(verif, liens_html)

  ## 1.6 HARMONISATION TAXONOMIQUE ----


# Dictionnaire des suggestions
synonymes_connus <- list(
  "Vicia amphicarpa" = "Vicia sativa subsp. amphicarpa",
  "Herniaria cinerea" = "Herniaria hirsuta subsp. cinerea",
  "Lysimachia foemina" = "Anagallis foemina",
  "Lysimachia linum-stellatum" = "Asterolinon linum-stellatum",
  "Oloptum miliaceum" = "Piptatherum miliaceum",
  "Petrosedum sediforme" = "Sedum sediforme",
  "Poterium verrucosum" = "Sanguisorba verrucosa",
  "Scorpiurus subvillosus" = "Scorpiurus muricatus",
  "Scorzonera hispanica subsp. crispatula" = "Pseudopodospermum hispanicum subsp. hispanicum",
  "Ervum gracile" = "Vicia parviflora",
  "Knautia collina" = "Knautia purpurea",
  "Lotus dorycnium" = "Dorycnium pentaphyllum subsp. pentaphyllum",
  "Lotus hirsutus" = "Dorycnium hirsutum",
  "Poterium sanguisorba" = "Sanguisorba minor",
  "Scabiosa atropurpurea" = "Sixalix atropurpurea",
  "Vicia segetalis" = "Vicia sativa subsp. nigra"
)

# Fonction standardisation
nettoyer_nom_colonne <- function(nom_vector) {
  clean_func <- function(nom) {
    if (is.na(nom) | nom == "NA") return(NA_character_)
    if (nom %in% c("id", "Année", "id_transect", "id_sous_unité")) return(as.character(nom))
    nom <- str_replace(nom, ",.*| \\(.*| \\d{4}.*", "")
    nom <- str_trim(nom)
    mots <- unlist(str_split(nom, "\\s+"))
    if (length(mots) >= 3 && mots[3] %in% c("subsp.", "subsp", "var.", "var", "f.")) return(paste(mots[1], mots[2], mots[3], mots[4]))
    if (length(mots) >= 2 && str_detect(mots[2], "^[A-Z][a-z]*\\.?$")) return(paste0(mots[1], " sp."))
    if (length(mots) >= 2) return(paste(mots[1], mots[2]))
    return(paste0(mots[1], " sp."))
  }
  return(unname(sapply(nom_vector, clean_func)))
}

names(synonymes_connus) <- nettoyer_nom_colonne(names(synonymes_connus))

# Nettoyage total
colnames(Data) <- nettoyer_nom_colonne(colnames(Data))
baseflor$NOM_SCIENTIFIQUE <- nettoyer_nom_colonne(baseflor$NOM_SCIENTIFIQUE)
Dengler$TaxonConcept <- nettoyer_nom_colonne(Dengler$TaxonConcept)

colonnes_meta <- c("id", "Année", "id_transect", "id_sous_unité")
especes_terrain <- setdiff(colnames(Data), colonnes_meta)
noms_bf <- unique(baseflor$NOM_SCIENTIFIQUE)
noms_dg <- unique(Dengler$TaxonConcept)

manquants_bf <- setdiff(especes_terrain, noms_bf)
manquants_dg <- setdiff(especes_terrain, noms_dg)

# Fonction détection noms incomplets
est_incomplet <- function(noms) { str_detect(noms, "sp\\.?$|spp\\.?$|famille|genre|^[A-Z][a-z]+$") }

# Affichage du bilan dans le Viewer
bilan_taxo_html <- paste0(
  "<div style='display: grid; grid-template-columns: 1fr 1fr; gap: 10px; margin: 10px 0;'>",
  "  <div style='background-color: #fcf3f2; padding: 10px; border-radius: 4px; border: 1px solid #f5b7b1;'>",
  "    <b style='color: #c0392b;'>🌿 Baseflor</b><br>Reconnues : <b>", length(especes_terrain) - length(manquants_bf), "</b><br>Inconnues : <b style='color: #c0392b;'>", length(manquants_bf), "</b></div>",
  "  <div style='background-color: #f5eef8; padding: 10px; border-radius: 4px; border: 1px solid #d7bde2;'>",
  "    <b style='color: #8e44ad;'>🌍 Dengler</b><br>Reconnues : <b>", length(especes_terrain) - length(manquants_dg), "</b><br>Inconnues : <b style='color: #8e44ad;'>", length(manquants_dg), "</b></div>",
  "</div>"
)

historique_guide <- paste0(historique_guide, "<div class='etape etape-6' style='border-left: 6px solid #8e44ad; background-color: #fcfbfd;'>
    <p style='color: #8e44ad; margin-top:0; font-size: 1.1em;'><b>🔍 PARTIE 6 : Harmonisation taxonomique</b></p>", bilan_taxo_html, "</div>")
pause_viewer(historique_guide)

# 2. Affichage des listes avant saisie
pause_viewer(paste0("<h3>📋 Taxons manquants détectés :</h3>",
                    "<div style='display: grid; grid-template-columns: 1fr 1fr; gap: 10px;'>",
                    "<div><b>Baseflor :</b><br><ul><li>", paste(manquants_bf, collapse="</li><li>"), "</li></ul></div>",
                    "<div><b>Dengler :</b><br><ul><li>", paste(manquants_dg, collapse="</li><li>"), "</li></ul></div>",
                    "</div>"))

# Saisie pour les espèces manquantes (Baseflor + Dengler)
manquants_a_traiter <- list(
  Baseflor = manquants_bf[!est_incomplet(manquants_bf)],
  Dengler = manquants_dg[!est_incomplet(manquants_dg)]
)

for (nom_base in names(manquants_a_traiter)) {
  liste_manquants <- manquants_a_traiter[[nom_base]]
  
  if(length(liste_manquants) > 0) {
    for (sp in liste_manquants) {
      sug <- ifelse(!is.null(synonymes_connus[[sp]]), synonymes_connus[[sp]], "")
      
      reponse <- rstudioapi::showPrompt(
        title = paste("Harmonisation", nom_base),
        message = paste0("Taxon '", sp, "' absent de ", nom_base, ".\n\n",
                         "Entrez le nom obsolète (actuellement dans ", nom_base, ") à remplacer par '", sp, "' :"),
        default = sug
      )
      
      if (!is.null(reponse) && str_trim(reponse) != "") {
        vieux_nom <- str_trim(reponse)
        
        # Mise à jour conditionnelle selon la base
        if (nom_base == "Dengler") {
          indices <- which(Dengler$TaxonConcept == vieux_nom)
          if (length(indices) > 0) Dengler$TaxonConcept[indices] <- sp
        } else {
          indices <- which(baseflor$NOM_SCIENTIFIQUE == vieux_nom)
          if (length(indices) > 0) baseflor$NOM_SCIENTIFIQUE[indices] <- sp
        }
        cat("✅ Succès : '", vieux_nom, "' mis à jour par '", sp, "' dans", nom_base, ".\n")
      }
    }
  }
}
# Bilan final post-harmonisation
noms_dg_final <- unique(Dengler$TaxonConcept)
manquants_bf_final <- setdiff(especes_terrain, noms_bf)
manquants_dg_final <- setdiff(especes_terrain, noms_dg_final)

bilan_final_html <- paste0(
  "<h3>✅ Bilan après harmonisation :</h3>",
  "<div style='display: grid; grid-template-columns: 1fr 1fr; gap: 10px; margin: 10px 0;'>",
  "  <div style='background-color: #fcf3f2; padding: 10px; border-radius: 4px; border: 1px solid #f5b7b1;'>",
  "    <b style='color: #c0392b;'>🌿 Baseflor</b><br>Reconnus : <b>", length(especes_terrain) - length(manquants_bf_final), "</b><br>Restants à identifier : <b style='color: #c0392b;'>", length(manquants_bf_final), "</b></div>",
  "  <div style='background-color: #f5eef8; padding: 10px; border-radius: 4px; border: 1px solid #d7bde2;'>",
  "    <b style='color: #8e44ad;'>🌍 Dengler</b><br>Reconnus : <b>", length(especes_terrain) - length(manquants_dg_final), "</b><br>Restants à identifier : <b style='color: #8e44ad;'>", length(manquants_dg_final), "</b></div>",
  "</div>",
  "<p style='font-size: 0.9em; color: #555;'><i>Les taxons restants sont soit des noms incomplets (sp./genre), soit des espèces absentes des bases de référence.</i></p>"
)

# Ajout à l'historique et affichage
historique_guide <- paste0(historique_guide, "<div class='etape' style='border-left: 6px solid #27ae60; background-color: #f9fbf9;'>",
                           "<p style='color: #27ae60; margin-top:0;'><b>✅ Harmonisation terminée</b></p>", bilan_final_html, "</div>")
pause_viewer(historique_guide)

# Supprimer les listes et variables temporaires 
rm(synonymes_connus, manquants_bf, manquants_dg, manquants_a_traiter, 
   manquants_bf_final, manquants_dg_final, bilan_taxo_html, bilan_final_html, 
   colonnes_meta, especes_terrain, noms_bf, noms_dg, noms_dg_final)

  ## 1.7 STATUS DES TRANSECTS -----

# Définissez ici VOTRE liste de choix autorisés
choix_stade <- c("Témoin", "À restaurer n+1", "Restauré n-1")

annotations_all <- list(
  "Fosse" = tibble(
    id_transect = c("F1", "F2", "F3"),
    stade = c("Témoin", "À restaurer n+1", "Restauré n-1")
  ),
  "Salvezines" = tibble(
    id_transect = c("T2", "T3", "T4", "T5"),
    stade = c("À restaurer n+1", "Témoin", "À restaurer n+1", "Témoin")
  ),
  "Claira" = tibble(
    id_transect = c("T1", "T2", "T3", "T4"),
    stade = c("À restaurer n+1", "À restaurer n+1", "Témoin", "Témoin")
  )
)

annotations <- annotations_all[[site]]

# Boucle de saisie avec menu déroulant
cat("--- Modification des stades pour :", site, "---\n")

for (i in 1:nrow(annotations)) {
  current_id <- annotations$id_transect[i]
  current_stade <- annotations$stade[i]
  
  # Utilisation de select.list pour forcer un choix parmi votre liste
  nouveau_stade <- select.list(
    choices = choix_stade,
    preselect = current_stade,
    multiple = FALSE,
    title = paste("Choisir le stade de restauration pour :", current_id),
    graphics = TRUE
  )
  
  # Si vous fermez la fenêtre sans choisir (ou Annuler), on garde l'ancien
  if (nouveau_stade != "") {
    annotations$stade[i] <- nouveau_stade
    cat("✅ Transect", current_id, ":", current_stade, nouveau_stade, "\n")
  }
}

# Enregistrement final
annotations_all[[site]] <- annotations
cat("--- Mise à jour terminée pour le site", site, "---\n")

couleurs_stade <- c(
  "Témoin"="#1b9e77",
  "À restaurer n+1"="#e41a1c",
  "Restauré n-1"="#d95f02"
)


# 2. ANALYSE DES DONNÉES (EFFORT, ALPHA, BÊTA, AFC) ----

# Préparation du dossier de sauvegarde
nom_dossier <- paste0("graphs_", site)

if (!dir.exists(nom_dossier)) {
  dir.create(nom_dossier)
}

# Fonction utilitaire pour enregistrer proprement dans le dossier spécifique
save_graph <- function(plot_obj, filename, width = 10, height = 7) {
  path <- file.path(nom_dossier, paste0(filename, ".png"))
  ggsave(path, plot_obj, width = width, height = height)
  cat("✅ graphique enregistré :", path, "\n")
}


  ## 2.1 COURBES DE RAREFACTION ----

# Titre général et initialisation de l'historique
historique_guide <- "
  <html>
  <head>
    <meta charset='UTF-8'>
    <style>
      body { font-family:sans-serif; background-color:#f4f7f6; padding:15px; line-height: 1.5; color: #333; }
      .header-main { background-color: #2c3e50; color: white; padding: 15px; border-radius: 4px; margin-bottom: 20px; }
      .etape { padding: 12px; margin-top: 15px; border-radius: 4px; background-color: #fff; box-shadow: 0 1px 3px rgba(0,0,0,0.1); }
      .etape-1 { border-left: 6px solid #27ae60; }
    </style>
  </head>
  <body>
    <div class='header-main'>
      <h1 style='margin:0; font-size: 1.5em;'>📊 ANALYSE DES DONNÉES</h1>
      <p style='margin:5px 0 0 0; opacity: 0.9;'>Tout est bien chargé, nous commençons l'exploration.</p>
    </div>
    
    <div class='etape etape-1'>
      <p style='color: #27ae60; margin-top:0;'><b>🚀 ÉTAPE 1 : Analyse de l'effort d'échantillonnage</b></p>
      <p>Nous allons vérifier si votre effort d'échantillonnage est suffisant en ajustant des modèles mathématiques.</p>
    </div>
  </body>
  </html>"

pause_viewer(historique_guide)

# Calcul et stockage des modèles
colonnes_meta <- c("id", "Année", "id_transect", "id_sous_unité")
colonnes_especes <- setdiff(names(Data), colonnes_meta)
Data_pa <- Data %>% mutate(across(all_of(colonnes_especes), ~ ifelse(. > 0, 1, 0)))

groupes <- unique(paste(Data_pa$id_transect, Data_pa$Année, sep="_"))
resultats_analyse <- list()

for(g in groupes) {
  parts <- strsplit(g, "_")[[1]]
  sous_data <- Data_pa %>% filter(id_transect == parts[1], Année == as.numeric(parts[2]))
  mat <- sous_data %>% select(all_of(colonnes_especes)) %>% select(where(~sum(.) > 0))
  if(ncol(mat) >= 2 && nrow(mat) >= 2) {
    acc <- specaccum(mat, method = "exact")
    mods <- list(Arrhenius = fitspecaccum(acc, "arrhenius"), Gleason = fitspecaccum(acc, "gleason"), 
                 Logistic = fitspecaccum(acc, "logis"), Lomolino = fitspecaccum(acc, "lomolino"), 
                 Michaelis = fitspecaccum(acc, "michaelis-menten"))
    resultats_analyse[[g]] <- list(acc = acc, mods = mods, aics = sapply(mods, AIC))
  }
}

# Affichage du tableau AIC et explication
df_aic <- do.call(rbind, lapply(resultats_analyse, function(x) x$aics))

explication_aic <- "
  <div style='margin-top: 20px; font-size: 0.9em; border-top: 1px solid #ccc; padding-top: 10px;'>
    <strong>Comment choisir le modèle ?</strong><br>
    <ul>
      <li>Le <b>critère AIC</b> estime la qualité du modèle : <b>plus l'AIC est faible, meilleur est le modèle</b>.</li>
      <li><b>Conseil :</b> Privilégiez le modèle qui présente l'AIC le plus faible pour le plus grand nombre de vos transects afin d'assurer une cohérence méthodologique sur l'ensemble de votre étude.</li>
    </ul>
  </div>"

html_aic <- paste0("<h3>📊 Valeurs d'AIC par groupe</h3><table><tr><th>Groupe</th><th>Arrhenius</th><th>Gleason</th><th>Logistic</th><th>Lomolino</th><th>Michaelis</th></tr>",
                   paste0("<tr><td>", rownames(df_aic), "</td>", apply(df_aic, 1, function(x) paste0("<td>", round(x, 2), "</td>", collapse="")), "</tr>", collapse=""),
                   "</table>", explication_aic)

historique_guide <- paste0(historique_guide, "<div class='etape'>", html_aic, "</div>")
pause_viewer(historique_guide)

# Sélection du modèle et demande du pourcentage
choix_modele <- select.list(colnames(df_aic), title = "Choisir le modèle pour le tracé et les calculs :", graphics = TRUE)

input_val <- winDialogString(
  "Quel pourcentage de l'asymptote souhaitez-vous atteindre ?\n(ex: 0.80)",
  "0.80"
)

pct_cible <- as.numeric(input_val)
if (is.na(pct_cible)) pct_cible <- 0.80

pct_label <- paste0(round(pct_cible * 100, 0), "%")

tableau_resultats <- do.call(rbind, lapply(names(resultats_analyse), function(g) {
  
  res <- resultats_analyse[[g]]
  modele <- res$mods[[choix_modele]]
  
  richesse_obs <- max(res$acc$richness)
  richesse_asym <- as.numeric(coef(modele)["Asym"])
  
  # Calcul du pourcentage de la richesse asymptotique atteinte
  pct_atteint <- (richesse_obs / richesse_asym) * 100
  
  # Calcul de l'effort pour 80% de l'asymptote
  richesse_cible <- richesse_asym * pct_cible
  effort_test <- 1:5000
  pred <- predict(modele, newdata = effort_test)
  
  idx <- which(pred >= richesse_cible)[1]
  effort_requis <- if (!is.na(idx)) effort_test[idx] else NA
  
  data.frame(
    Groupe = g,
    Richesse_observee = round(richesse_obs, 0),
    Richesse_asymptote = round(richesse_asym, 1),
    Pourcentage_richesse_atteinte = paste0(round(pct_atteint, 1), "%"),
    Effort_pour_80 = ifelse(is.na(effort_requis), ">5000", effort_requis),
    stringsAsFactors = FALSE
  )
  
}))


# Génération du graphique avec légende ajustée
rarefaction_df <- bind_rows(lapply(names(resultats_analyse), function(g) {
  res <- resultats_analyse[[g]]
  parts <- strsplit(g, "_")[[1]]
  df <- data.frame(Surface = res$acc$sites, Richesse = res$acc$richness, SD = res$acc$sd,
                   Transect = parts[1], Année = as.factor(parts[2]))
  df$Pred <- predict(res$mods[[choix_modele]], newdata = df$Surface)
  df$stade <- annotations_all[[site]] %>% filter(id_transect == parts[1]) %>% pull(stade)
  return(df)
}))

p_rarefaction <- ggplot(rarefaction_df, aes(x = Surface, color = stade, fill = stade)) +
  geom_ribbon(aes(ymin = Richesse - SD, ymax = Richesse + SD, group = interaction(Transect, Année)), alpha = 0.15, colour = NA) +
  geom_line(aes(y = Pred, group = interaction(Transect, Année), linetype = Année), linewidth = 1.2) +
  geom_text_repel(data = rarefaction_df %>% group_by(Transect, Année) %>% filter(Surface == max(Surface)),
                  aes(y = Pred, label = Transect), nudge_x = 1, show.legend = FALSE) +
  scale_color_manual(values = couleurs_stade, name = "Stade de restauration") +
  scale_fill_manual(values = couleurs_stade, name = "Stade de restauration") +
  scale_linetype_manual(values = c("2025" = "solid", "2026" = "dashed"), name = "Année") +
  theme_minimal(base_size = 14) +
  labs(title = paste0("Raréfaction (", choix_modele, ") — ", site),
       x = "Nombre de unités", y = "Richesse spécifique cumulée") +
  theme(legend.position = "bottom", 
        legend.box = "vertical",
        legend.box.just = "left",
        panel.grid.minor = element_blank(),
        legend.key.width = unit(3, "line")) +
  guides(color = guide_legend(order = 1), 
         fill = guide_legend(order = 1),
         linetype = guide_legend(order = 2))

print(p_rarefaction)
save_graph(p_rarefaction, paste0("Courbes de raréfaction _ ", site))

# Affichage Viewer
html_res <- paste0(
  "<h3>📊 Effort d'échantillonnage (modèle : ", choix_modele, ")</h3>",
  "<table border='1' style='border-collapse: collapse; width: 100%; text-align: center;'>",
  "<tr><th>Groupe</th><th>Richesse observée</th><th>Richesse asymptote</th><th>% Richesse atteinte</th><th>Effort pour 80%</th></tr>",
  paste0(
    "<tr><td>", tableau_resultats$Groupe,
    "</td><td>", tableau_resultats$Richesse_observee,
    "</td><td>", tableau_resultats$Richesse_asymptote,
    "</td><td><b>", tableau_resultats$Pourcentage_richesse_atteinte, "</b>",
    "</td><td>", tableau_resultats$Effort_pour_80,
    "</td></tr>",
    collapse = ""
  ),
  "</table>"
)

historique_guide <- paste0(historique_guide, "<div class='etape'>", html_res, "</div>")
pause_viewer(historique_guide)

# Supprimer les listes et variables temporaires
rm(sous_data, mat, acc, mods, df_aic, html_aic, input_val)

  ## 2.2  DIVERSITÉ ALPHA (α) ----

# attention à vérifier quel indice utiliser !!!!! ----

# Calcul des indices à l'échelle de l'unité
alpha_data <- Data %>%
  mutate(
    S = rowSums(select(., all_of(colonnes_especes)) > 0),
    H = diversity(select(., all_of(colonnes_especes)), index = "shannon"),
    D = diversity(select(., all_of(colonnes_especes)), index = "simpson")
  )

# Fusion manuelle des stades (évite les erreurs de jointure)
ref_df <- annotations_all[[site]]
alpha_data$stade <- NA
for(i in 1:nrow(ref_df)) {
  id_t <- as.character(ref_df$id_transect[i])
  stade_t <- as.character(ref_df$stade[i])
  alpha_data$stade[alpha_data$id_transect == id_t] <- stade_t
}
alpha_data$stade[is.na(alpha_data$stade)] <- "Inconnu"
alpha_data$stade <- factor(alpha_data$stade, levels = names(couleurs_stade))

# Génération des graphiques (chargés en mémoire avant l'affichage Viewer)
p_richesse <- ggplot(alpha_data, aes(x = id_transect, y = S, fill = stade)) +
  geom_boxplot(alpha = 0.8) + 
  facet_wrap(~Année) + 
  scale_fill_manual(values = couleurs_stade, na.value = "grey80") +
  theme_minimal() + 
  labs(title = paste("Richesse spécifique (S) -", site), y = "Nombre d'espèces", x = "Transect")

p_simpson <- ggplot(alpha_data, aes(x = id_transect, y = D, fill = stade)) +
  geom_boxplot(alpha = 0.8) + 
  facet_wrap(~Année) + 
  scale_fill_manual(values = couleurs_stade, na.value = "grey80") +
  theme_minimal() + 
  labs(title = paste("Indice de Simpson (D) -", site), y = "Indice de diversité", x = "Transect")

# Affichage immédiat pour vérification
print(p_richesse)
print(p_simpson)
save_graph(p_richesse, paste0("Richesse spécifique _ ", site))
save_graph(p_simpson, paste0("Indice de Simpson _ ", site))


# Mise à jour du Viewer avec les explications
explication_alpha <- "
<div class='etape' style='border-left: 6px solid #27ae60;'>
  <p style='color: #27ae60; margin-top:0;'><b>🚀 ÉTAPE 2 : Analyse de la diversité Alpha</b></p>
  <p>Nous avons calculé les indices à l'échelle de vos <b>unités</b> (chiffre + lettre, ex: 1a, 1b...).</p>
</div>

<div class='etape' style='border-left: 6px solid #8e44ad; background-color: #fcfbfd; margin-top: 15px;'>
  <p style='color: #8e44ad; margin-top:0; font-size: 1.1em;'><b>🧬 QU'EST-CE QUE L'ANALYSE ALPHA ?</b></p>
  <p>L'analyse Alpha caractérise la diversité biologique au sein de chaque échantillon local.</p>
  <ul style='line-height: 1.6;'>
    <li><b>Richesse Spécifique (S) :</b> Compte le nombre total d'espèces présentes au sein d'une unité.</li>
    <li><b>Indice de Simpson (D) :</b> Mesure la probabilité que deux individus pris au hasard dans une unité appartiennent à des espèces différentes.</li>
  </ul>
  <p><i><b>Conseil :</b> Observez vos graphiques (onglet Plots). Si vos boîtes 'À restaurer' tendent à se rapprocher des valeurs des 'Témoins', cela illustre visuellement le succès de la dynamique de restauration.</i></p>
</div>"

historique_guide <- paste0(historique_guide, explication_alpha)
pause_viewer(historique_guide)


## 2.3 DIVERSITÉ BÊTA (β) ----

# Préparation des données
mat_num <- alpha_data %>% select(all_of(colonnes_especes)) 
lignes_a_garder <- rowSums(mat_num > 0) > 0
mat_num_filtre <- mat_num[lignes_a_garder, ]
df_filtre <- alpha_data[lignes_a_garder, ] %>% select(id_sous_unité, Année, stade)
dist_mat <- vegdist(mat_num_filtre, method = "jaccard", binary = TRUE)
hc <- hclust(dist_mat, method = "ward.D2")
dend <- as.dendrogram(hc)
dend <- reorder(dend, as.numeric(as.factor(df_filtre$stade[order.dendrogram(dend)])))

# Fonction de tracé ajustée
dessiner_dendrogramme <- function(marge_bas = 30, agrandir = FALSE) {
  # Paramètres agrandis
  cex_main <- if(agrandir) 2.5 else 1.5
  cex_lab  <- if(agrandir) 2.0 else 1.2
  cex_leg  <- if(agrandir) 1.8 else 1.0 
  cex_ann  <- if(agrandir) 1.8 else 0.8 # ANNÉES : plus petit dans le Viewer
  
  par(mar = c(marge_bas, 8, 8, 4)) 
  
  plot(hang.dendrogram(dend, hang = -1), 
       main = paste("Similarité floristique -", site), 
       ylab = "Distance de Jaccard", 
       leaflab = "none",
       cex.main = cex_main,   
       cex.lab = cex_lab)     
  
  ord <- order.dendrogram(dend); n <- length(ord); at <- seq(1, n, length.out = n) 
  ids <- as.character(df_filtre$id_sous_unité[ord])
  couleurs <- couleurs_stade[as.character(df_filtre$stade[ord])]
  est_2025 <- (df_filtre$Année[ord] == min(df_filtre$Année[ord]))

    # Texte des feuilles (utilisez des valeurs plus proches, ex: -0.5 et -0.8)
  text(x = at[est_2025], y = -0.5, labels = ids[est_2025], col = couleurs[est_2025], srt = 90, cex = 0.7, adj = 0, xpd = NA)
  text(x = at[!est_2025], y = -0.8, labels = ids[!est_2025], col = couleurs[!est_2025], srt = 90, cex = 0.7, adj = 0, xpd = NA)
  
  # Texte des années (alignés sur les nouveaux y)
  text(x = 0, y = -0.5, labels = "2025", cex = cex_ann, font = 2, xpd = NA, pos = 2)
  text(x = 0, y = -0.8, labels = "2026", cex = cex_ann, font = 2, xpd = NA, pos = 2)  
  # Légende (agrandie sur les exports)
  legend("topright", legend = names(couleurs_stade), fill = couleurs_stade, 
         title = "stade", bty = "n", cex = cex_leg, inset = c(0.01, 0.01))
}

# Exportations
if (!dir.exists(nom_dossier)) dir.create(nom_dossier)
pdf(file.path(nom_dossier, "dendro_agrandi.pdf"), width = 30, height = 20)
dessiner_dendrogramme(marge_bas = 30, agrandir = TRUE); dev.off()

jpeg(file.path(nom_dossier, "dendro.jpg"), width = 7000, height = 6000, quality = 100, res = 300)
dessiner_dendrogramme(marge_bas = 30, agrandir = TRUE); dev.off() # agrandir = TRUE pour légende grosse

# Affichage Viewer
par(mar = c(10, 6, 6, 4))
dessiner_dendrogramme(marge_bas = 10, agrandir = FALSE)

# Indice de Calinski-Harabasz
k_test <- 2:10

ch_scores <- sapply(k_test, function(k) {
  cl <- cutree(hc, k)
  val <- calinhara(as.matrix(dist_mat), cl)
  if (is.na(val) || !is.finite(val)) return(-Inf)
  as.numeric(val)
})

k_opt <- k_test[which.max(ch_scores)]

# Clusters finaux
df_filtre$cluster <- cutree(hc, k = k_opt)

# Calcul du tableau croisé (Prop.table)
tab_prop <- prop.table(table(df_filtre$cluster, df_filtre$stade), 1)

# Génération dynamique des lignes du tableau HTML
# Utilisation d'une boucle 'for' pour garantir l'affichage des numéros de clusters (1, 2, 3...)
lignes_html <- ""
for(i in 1:nrow(tab_prop)) {
  row_data <- tab_prop[i, ]
  lignes_html <- paste0(lignes_html, 
                        "<tr><td style='border-bottom:1px solid #bdc3c7; padding: 8px; font-weight: bold;'>Cluster ", i, "</td>", 
                        paste0("<td style='padding: 8px;'>", round(row_data, 3), "</td>", collapse = ""), 
                        "</tr>")
}

# Création du bloc HTML complet
etape_beta <- paste0("
<div class='etape' style='border-left: 6px solid #e67e22; background-color: #fdf2e9; padding: 15px; margin-top: 20px;'>
  <p style='color: #e67e22; margin-top:0;'><b>🚀 ÉTAPE 3 : Diversité Bêta (Tableau de correspondance)</b></p>
  <p>Répartition des relevés par cluster vs stade de restauration :</p>
  
  <table style='width:100%; border-collapse: collapse; font-size: 13px; background-color: white;'>
    <tr style='background-color: #e67e22; color: white;'>
      <th>Cluster</th><th>Témoin</th><th>À restaurer</th><th>Restauré</th>
    </tr>", 
                     lignes_html, 
                     "</table>
</div>")

# Mise à jour et affichage
historique_guide <- paste0(historique_guide, etape_beta)
pause_viewer(historique_guide)

# Si on veut tester le clustering selon l'année aussi 
# 1. Calcul du tableau croisé : Cluster vs Année
# On récupère les années uniques dynamiquement pour les en-têtes
annees <- sort(unique(df_filtre$Année))
tab_prop_annee <- prop.table(table(df_filtre$cluster, df_filtre$Année), 1)

# 2. Génération dynamique des lignes du tableau HTML
lignes_html_annee <- ""
for(i in 1:nrow(tab_prop_annee)) {
  row_data <- tab_prop_annee[i, ]
  lignes_html_annee <- paste0(lignes_html_annee, 
                              "<tr><td style='border-bottom:1px solid #bdc3c7; padding: 8px; font-weight: bold;'>Cluster ", i, "</td>", 
                              paste0("<td style='padding: 8px;'>", round(row_data, 3), "</td>", collapse = ""), 
                              "</tr>")
}

# 3. Création du bloc HTML pour l'année
etape_annee <- paste0(
  "<div class='etape' style='border-left: 6px solid #2980b9; background-color: #ebf5fb; padding: 15px; margin-top: 20px;'>",
  "<p style='color: #2980b9; margin-top:0;'><b>🗓️ ÉTAPE 3bis : Diversité Bêta (Répartition par Année)</b></p>",
  "<p>Répartition des relevés par cluster vs Année :</p>",
  
  "<table style='width:100%; border-collapse: collapse; font-size: 13px; background-color: white;'>",
  "<tr style='background-color: #2980b9; color: white;'>",
  "<th>Cluster</th>", paste0("<th>", annees, "</th>", collapse = ""),
  "</tr>",
  lignes_html_annee,
  "</table>",
  "</div>")

# Mise à jour et affichage
historique_guide <- paste0(historique_guide, etape_annee)
pause_viewer(historique_guide)



## 2.4 ANALYSE DES INDICATEURS ÉCOLOGIQUES (EIV) ----

lettres_eco <- c("M", "N", "R", "L", "T")

# 1. Définition des noms (manquants)
noms_complets <- c("M" = "Humidité", "N" = "Nutriments", "R" = "Réaction pH", "L" = "Lumière", "T" = "Température")

analyser_tous_indicateurs_final <- function() {
  # Nettoyage préalable de Dengler pour éviter les doublons (règle le many-to-many)
  Dengler_clean <- Dengler %>% distinct(TaxonConcept, .keep_all = TRUE)
  
  for (lettre in lettres_eco) {
    pattern <- paste0("^EIVEres-", lettre, "$")
    nom_colonne_reel <- names(Dengler)[grep(pattern, names(Dengler))][1]
    
    if(is.na(nom_colonne_reel)) next
    
    message(paste("Analyse en cours :", lettre))
    
    data_agg <- Data %>%
      pivot_longer(cols = -all_of(c("id", "Année", "id_transect", "id_sous_unité")), 
                   names_to = "TaxonConcept", values_to = "abondance") %>%
      filter(abondance > 0) %>%
      # Utilisation de la version propre de Dengler
      left_join(Dengler_clean %>% select(TaxonConcept, all_of(nom_colonne_reel)), by = "TaxonConcept") %>%
      rename(valeur_eco = !!sym(nom_colonne_reel)) %>%
      filter(!is.na(valeur_eco)) %>%
      group_by(Année, id_transect, valeur_eco) %>%
      summarise(abondance_cumulee = sum(abondance), .groups = "drop") %>%
      left_join(annotations %>% select(id_transect, stade), by = "id_transect")
    
    # Graphique
    p <- ggplot(data_agg, aes(x = valeur_eco, y = abondance_cumulee, 
                              color = stade, group = id_transect)) +
      geom_line(linewidth = 0.4, alpha = 0.3, linetype = "dotted") +
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), linewidth = 1.2, se = FALSE) + 
      scale_color_manual(values = couleurs_stade) + 
      facet_wrap(~ Année, ncol = 1, scales = "fixed") + 
      theme_minimal() +
      labs(
        title = paste("Distribution de l'abondance :", noms_complets[lettre]),
        x = paste("Valeur de l'indice écologique", lettre),
        y = "Cumul des abondances",
        color = "stade du transect"
      )
    
    print(p)
    ggsave(filename = file.path(nom_dossier, paste0("Indicateur_", lettre, ".jpg")), 
           plot = p, width = 8, height = 6, dpi = 300)
  }
}

# Lancer
analyser_tous_indicateurs_final()

# Mise à jour pédagogique du Viewer
etape_indicateurs <- "
<div class='etape' style='border-left: 6px solid #27ae60;'>
  <p style='color: #27ae60; margin-top:0;'><b>🌱 ÉTAPE 4 : Analyse des Indicateurs Écologiques (EIV)</b></p>
  <p>Visualisation de la réponse floristique aux gradients environnementaux.</p>
</div>

<div class='etape' style='border-left: 6px solid #27ae60; background-color: #f1fcf4; margin-top: 15px;'>
  <p style='color: #27ae60; margin-top:0;'><b>📈 COMMENT INTERPRÉTER CES GRAPHES ?</b></p>
  <ul style='line-height: 1.6;'>
    <li><b>Cumul des abondances :</b> La valeur sur l'axe Y représente la somme totale des abondances des espèces présentes à un niveau donné de l'indice écologique.</li>
    <li><b>Courbes (GAM) :</b> Elles montrent la tendance centrale. Si les courbes de 2025 et 2026 s'écartent, cela traduit une évolution de la niche écologique.</li>
    <li><b>stade :</b> Les couleurs comparent instantanément si les zones <i>'À restaurer'</i> convergent vers les zones <i>'Témoin'</i>.</li>
  </ul>
</div>"

historique_guide <- paste0(historique_guide, etape_indicateurs)
pause_viewer(historique_guide)


  ## 2.5 ANALYSE TYPE BIOLOGIQUE ----

# Liste des ajouts manuels pour compléter la base de référence
ajouts_manuels <- tribble(
  ~nom, ~type_bio,
  "Allium sp.", "G",
  "Arabis sp.", "H",
  "Campanula sp.", "H",
  "Cephalanthera sp.", "G",
  "Lathyrus sp.", "T",
  "Lotus dorycnium", "C",
  "Lotus hirsutus", "C",
  "Ophrys sp.", "G",
  "Petrosedum sediforme", "C",
  "Trifolium sp.", "T",
  "Homalothecium lutescens", "B",
  "Homalothecium sp.", "B",
  "Syntrichia calcicola", "B",
  "Tortella squarrosa", "B",
  "Tortella tortuosa", "B",
  "Weissia controversa", "B",
  "Rosa sp.", "P",
  "Scabiosa sp.", "H",
  "Rubus sp.", "H",
  "Bunium bulbocastanum", "G",
  "Ferula communis", "H",
  "Festuca sp.", "H",
  "Gladiolus sp.", "G",
  "Muscari baeticum", "G",
  "Muscari matritensis", "G",
  "Vitis sp.", "P",
  "Vicia saxatilis", "T"
)

# Conversion de baseflor et harmonisation
referentiel_bio <- baseflor %>%
  mutate(nom = nettoyer_nom_colonne(NOM_SCIENTIFIQUE)) %>%
  mutate(type_bio = case_when(
    str_starts(TYPE_BIOLOGIQUE, "A|B|a|b") ~ "P",
    TRUE ~ toupper(str_sub(TYPE_BIOLOGIQUE, 1, 1))
  )) %>%
  select(nom, type_bio)

# Fusion : les ajouts manuels sont prioritaires
tableau_reference <- bind_rows(referentiel_bio, ajouts_manuels) %>%
  distinct(nom, .keep_all = TRUE) %>%
  filter(!is.na(type_bio))

# Création du vecteur de référence pour la jointure
typebio <- setNames(tableau_reference$type_bio, tableau_reference$nom)

# Passage en format long
quad_long <- Data %>%
  pivot_longer(
    cols = -all_of(c("id", "Année", "id_transect", "id_sous_unité")), 
    names_to = "nom", 
    values_to = "presence"
  ) %>%
  mutate(type_bio = typebio[nom]) %>%
  filter(!is.na(type_bio))

# Agrégation par année, transect et type bio
stats_typebio <- quad_long %>%
  group_by(Année, id_transect, type_bio, id_sous_unité) %>%
  summarise(abundance_q = sum(presence), .groups = "drop") %>%
  group_by(Année, id_transect, type_bio) %>%
  summarise(
    mean = mean(abundance_q),
    sd = sd(abundance_q),
    n = n(),
    se = sd / sqrt(n),
    .groups = "drop"
  ) %>%
  group_by(Année, id_transect) %>%
  mutate(pourcentage = mean / sum(mean) * 100) %>%
  ungroup() %>%
  mutate(
    IC_low = pmax(0, pourcentage - 1.96 * se),
    IC_high = pourcentage + 1.96 * se
  )

# Graph type biologique

stats_typebio <- stats_typebio %>% left_join(annotations, by = "id_transect")

labels_typebio <- c(
  "H" = "Hémicryptophytes", "T" = "Thérophytes", "G" = "Géophytes",
  "C" = "Chaméphytes", "P" = "Phanérophytes", "B" = "Bryophytes"
)

# PRÉPARATION : On calcule les proportions totales par Année et Stade

stats_empilees <- stats_typebio %>%
  # Conversion des codes (H, T...) en noms complets pour la légende
  mutate(type_bio_nom = recode(type_bio, !!!labels_typebio)) %>%
  group_by(Année, stade) %>%
  # Calcul explicite du pourcentage pour atteindre 100%
  mutate(pourcentage_empile = (mean / sum(mean)) * 100) %>%
  ungroup()

# VISUALISATION AVEC VOTRE GAMME DE COULEURS
p <- ggplot(stats_empilees, aes(x = as.factor(Année), y = pourcentage_empile, fill = type_bio_nom)) + 
  geom_col(position = "stack") + 
  facet_wrap(~ stade) + 
  
  # Utilisation de la palette Set3 (la gamme classique aux couleurs pastels)
  scale_fill_brewer(palette = "Set3") + 
  
  theme_minimal() + 
  labs(
    title = paste("Évolution du pourcentage des types biologiques -", site),
    x = "Année", 
    y = "Pourcentage (%)", 
    fill = "Type biologique"
  ) +
  theme(legend.position = "bottom")

print(p)

# Sauvegarde
ggsave(filename = paste0(site, "_Indicateur_TypeBio_MultiAnnuel.jpg"),
       plot = p, width = 12, height = 8, dpi = 300)

# Mise à jour pédagogique du Viewer
etape_types_bio <- "
<div class='etape' style='border-left: 6px solid #e67e22; margin-top: 20px;'>
  <p style='color: #e67e22; margin-top:0;'><b>📊 ÉTAPE 5 : Analyse des Types Biologiques (Raunkiaer)</b></p>
  <p>Visualisation de la stratégie de survie et de la structure de la végétation.</p>
</div>

<div class='etape' style='border-left: 6px solid #e67e22; background-color: #fff9f4; margin-top: 15px;'>
  <p style='color: #e67e22; margin-top:0;'><b>💡 COMMENT INTERPRÉTER CES BARRES ?</b></p>
  <ul style='line-height: 1.6;'>
    <li><b>Composition relative :</b> Chaque colonne représente 100% de la végétation pour une année donnée. Les changements de hauteur des blocs colorés montrent le remplacement des stratégies biologiques.</li>
    <li><b>Phanérophytes (Bleu) :</b> Une augmentation de leur part indique souvent une fermeture du milieu (embroussaillement).</li>
    <li><b>Thérophytes (Orange) :</b> Une diminution peut traduire une perte de milieux ouverts ou perturbés (succession écologique).</li>
    <li><b>Comparaison :</b> Observez si la structure de la zone <i>'À restaurer'</i> se rapproche de celle du <i>'Témoin'</i> au fil des années.</li>
  </ul>
</div>"

historique_guide <- paste0(historique_guide, etape_types_bio)
pause_viewer(historique_guide)

  ## 2.5 EXPLORATION MULTIDIMENSIONNEL ----
 
       ### 2.5.1 AFC ----

# PRÉPARATION DES DONNÉES (Création de l'objet)
data_afc_detail <- Data %>%
  mutate(id_unique_q = id,
         id_groupe_ellipse = paste(id_transect, Année, sep = "_")) %>%
  left_join(annotations_all[[site]], by = "id_transect")

# CALCUL AFC
data_numeric_only <- data_afc_detail %>% 
  dplyr::select(-matches("EIVEres|Taxon|UUID|AccordingTo|id_groupe|stade|Année|id_transect|id_sous_unité|id_unique_q")) %>% 
  dplyr::select(where(is.numeric))

lignes_valides <- rowSums(data_numeric_only) > 0
data_afc_clean <- data_afc_detail[lignes_valides, ]
matrice_afc_q <- as.data.frame(data_numeric_only[lignes_valides, ])
rownames(matrice_afc_q) <- data_afc_clean$id_unique_q

res_afc_q <- CA(matrice_afc_q, graph = FALSE)

# PRÉPARATION DES COORDONNÉES ET VECTEURS
coord_quadrats <- data.frame(
  Dim.1 = as.numeric(res_afc_q$row$coord[, 1]), 
  Dim.2 = as.numeric(res_afc_q$row$coord[, 2]), 
  id_unique_q = rownames(res_afc_q$row$coord)
) %>%
  left_join(data_afc_clean %>% select(id_unique_q, id_groupe_ellipse, stade, Année, id_transect), by = "id_unique_q") %>%
  filter(!is.na(stade))

centres_ellipses <- coord_quadrats %>% 
  group_by(id_groupe_ellipse) %>% 
  summarise(cx = mean(Dim.1), cy = mean(Dim.2), Année = first(Année), 
            id_transect = first(id_transect), stade = first(stade), .groups = "drop")

coords_especes <- data.frame(nom = rownames(res_afc_q$col$coord), Dim.1 = as.numeric(res_afc_q$col$coord[, 1]), Dim.2 = as.numeric(res_afc_q$col$coord[, 2]), contrib = as.numeric(res_afc_q$col$contrib[, 1]) + as.numeric(res_afc_q$col$contrib[, 2]))
top_15_especes <- coords_especes[order(-coords_especes$contrib), ][1:15, ]

cols_especes <- setdiff(names(data_numeric_only), names(Dengler %>% select(starts_with("EIVEres"))))
data_num_sync <- data_afc_clean %>%
  pivot_longer(cols = all_of(cols_especes), names_to = "TaxonConcept", values_to = "abondance") %>%
  filter(abondance > 0) %>%
  left_join(Dengler %>% select(TaxonConcept, starts_with("EIVEres")), by = "TaxonConcept") %>%
  group_by(id_unique_q) %>%
  summarise(across(starts_with("EIVEres"), ~weighted.mean(., abondance, na.rm = TRUE))) %>%
  filter(id_unique_q %in% rownames(res_afc_q$row$coord)) %>%
  arrange(match(id_unique_q, rownames(res_afc_q$row$coord))) %>% select(-id_unique_q)

# Pour projeter les EIVE
cols_especes <- setdiff(names(data_numeric_only), names(Dengler %>% select(starts_with("EIVEres"))))
data_num_sync <- data_afc_clean %>%
  pivot_longer(cols = all_of(cols_especes), names_to = "TaxonConcept", values_to = "abondance") %>%
  filter(abondance > 0) %>%
  left_join(Dengler %>% select(TaxonConcept, starts_with("EIVEres")), by = "TaxonConcept") %>%
  group_by(id_unique_q) %>%
  summarise(across(starts_with("EIVEres"), ~weighted.mean(., abondance, na.rm = TRUE))) %>%
  filter(id_unique_q %in% rownames(res_afc_q$row$coord)) %>%
  arrange(match(id_unique_q, rownames(res_afc_q$row$coord))) %>% 
  # Sélection stricte des optimums principaux uniquement
  select(id_unique_q, `EIVEres-M`, `EIVEres-N`, `EIVEres-R`, `EIVEres-L`, `EIVEres-T`) %>%
  select(-id_unique_q)

# Calcul des vecteurs avec envfit
env_results <- envfit(res_afc_q$row$coord, data_num_sync, permutations = 999)
vectors <- as.data.frame(scores(env_results, display = "vectors"))
vectors$pvals <- env_results$vectors$pvals
vectors$label <- rownames(vectors)
colnames(vectors)[1:2] <- c("Dim1", "Dim2")

# Filtrage significatif
vectors <- vectors[vectors$pvals < 0.05, ]

# Renommage explicite
vectors$label[vectors$label == "EIVEres-M"] <- "Humidité (M)"
vectors$label[vectors$label == "EIVEres-N"] <- "Fertilité (N)"
vectors$label[vectors$label == "EIVEres-R"] <- "pH (R)"
vectors$label[vectors$label == "EIVEres-L"] <- "Luminosité (L)"
vectors$label[vectors$label == "EIVEres-T"] <- "Température (T)"

# VISUALISATIONS
style_annees <- scale_linetype_manual(values = c("2025" = "solid", "2026" = "dashed"), name = "Année")

custom_color <- scale_color_manual(
  values = couleurs_stade, 
  name = "Stades de restauration"
)

custom_linetype <- scale_linetype_manual(
  values = c("2025" = "solid", "2026" = "dashed"), 
  name = "Année", # IMPORTANT : doit être identique au scale_color
  guide = guide_legend(override.aes = list(color = "black")) # Force le rendu des traits
)


# P1 : Relevés
p1 <- ggplot(coord_quadrats, aes(x = Dim.1, y = Dim.2)) + 
  geom_point(aes(color = stade), size = 2.5, alpha = 0.5) + 
  stat_ellipse(aes(group = id_groupe_ellipse, color = stade, linetype = as.factor(Année)), level = 0.95, type = "norm", linewidth = 0.8) +
  geom_point(data = centres_ellipses, aes(x = cx, y = cy, fill = stade), shape = 23, size = 5, color = "black", stroke = 1.5, show.legend = FALSE) +
  geom_text_repel(data = centres_ellipses, aes(x = cx, y = cy, label = paste0(id_transect, "-", Année)), fontface = "bold") +
  scale_color_manual(values = couleurs_stade, name = "Stades de restauration") + 
  scale_fill_manual(values = couleurs_stade, name = "Stades de restauration") +
  style_annees + 
  custom_linetype +
  theme_minimal() + 
  theme(legend.key.width = unit(3, "line")) +
  guides(linetype = guide_legend(override.aes = list(linetype = c("solid", "dashed")))) +
  labs(title = "AFC : Répartition des relevés",
       x = paste0("Dim.1 (", round(res_afc_q$eig[1, 2], 1), " %)"),
       y = paste0("Dim.2 (", round(res_afc_q$eig[2, 2], 1), " %)")
  )

# P2 : Gradients
p2 <- ggplot() + 
  stat_ellipse(data = coord_quadrats, aes(x = Dim.1, y = Dim.2, color = stade, group = id_groupe_ellipse, linetype = as.factor(Année)), level = 0.95, type = "norm", linewidth = 0.8) +
  geom_segment(data = vectors, aes(x = 0, y = 0, xend = Dim1, yend = Dim2), arrow = arrow(length = unit(0.2, "cm")), color = "black", linewidth = 0.8) +
  geom_text_repel(data = vectors, aes(x = Dim1, y = Dim2, label = label), fontface = "bold") +
  scale_color_manual(values = couleurs_stade, name = "Stades de restauration") +
  scale_linetype_manual(values = c("2025" = "solid", "2026" = "dashed"), name = "Année") +
  theme_minimal() +
  theme(legend.key.width = unit(3, "line")) +
  labs(title = "Gradients environnementaux",
       x = paste0("Dim.1 (", round(res_afc_q$eig[1, 2], 1), " %)"),
       y = paste0("Dim.2 (", round(res_afc_q$eig[2, 2], 1), " %)")
  )

# P3 : Espèces
p3 <- ggplot(top_15_especes, aes(x = Dim.1, y = Dim.2)) + 
  stat_ellipse(data = coord_quadrats, aes(x = Dim.1, y = Dim.2, color = stade, group = id_groupe_ellipse, linetype = as.factor(Année)), level = 0.95, type = "norm", linewidth = 0.8) +
  geom_segment(aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2), arrow = arrow(length = unit(0.2, "cm")), color = "black", linewidth = 0.8) +
  geom_text_repel(aes(label = nom), fontface = "bold", size = 3.5) +
  scale_color_manual(values = couleurs_stade) +
  style_annees + 
  custom_color + 
  custom_linetype +
  theme_minimal() + 
  theme(legend.key.width = unit(3, "line")) +
  labs(title = "Top 15 Espèces",
       x = paste0("Dim.1 (", round(res_afc_q$eig[1, 2], 1), " %)"),
       y = paste0("Dim.2 (", round(res_afc_q$eig[2, 2], 1), " %)")
  )

print(p1); print(p2); print(p3)

save_graph(p1, paste0("AFC _ ", site))
save_graph(p2, paste0("AFC avec projection EIVE _ ", site))
save_graph(p3, paste0("AFC avec projection espèces_ ", site))

# AJOUT DE L'ÉTAPE 5 DANS LE VIEWER
historique_guide <- paste0(historique_guide, "
  <div class='etape etape-5' style='border-left: 6px solid #8e44ad; background-color: #f9f4fb; margin-top: 20px;'>
    <p style='color: #8e44ad; margin-top:0;'><b>🎯 ÉTAPE 5 : Synthèse des résultats</b></p>
    <p>L'analyse multivariée (AFC) est maintenant terminée. Vous pouvez observer les trajectoires de restauration sur les graphiques ci-dessus. 
    Les données ont été exportées dans le dossier <code>graphs_", site, "</code>.</p>
  </div>")

# AJOUT DES BLOCS D'EXPLICATION (Interprétations)
interpr_afc <- "
<div style='background-color: #fdf2e9; border: 1px solid #d35400; padding: 15px; border-radius: 5px; margin-top: 15px;'>
  <h4 style='color: #d35400; margin-top:0;'>🔍 Comment interpréter vos graphiques AFC ?</h4>
  <p>Pour lire ces trois graphiques ensemble, gardez cette logique en tête :</p>
  <ul>
    <li><b>Répartition (P1) :</b> Comparez les ellipses 2026 (pointillées) à 2025 (pleines). Un glissement vers une autre ellipse indique un changement de composition floristique.</li>
    <li><b>Gradients (P2) :</b> Les flèches indiquent la direction des variables (ex: lumière, humidité). Plus la flèche est longue, plus la variable structure la végétation.</li>
    <li><b>Espèces (P3) :</b> Elles sont positionnées selon leurs affinités. Une espèce proche d'un barycentre de transect est typique de ce milieu.</li>
  </ul>
</div>"

historique_guide <- paste0(historique_guide, interpr_afc)
pause_viewer(historique_guide)



      ### 2.5.2 PERMANOVA ----

# Calcul de la distance (si le script de la diversité béta n'a pas tourné)
dist_mat <- vegdist(matrice_afc_q, method = "jaccard", binary = TRUE)

# PERMANOVA Globale
res_global <- adonis2(dist_mat ~ stade * Année, data = df_filtre, permutations = 999)

# Calcul des paires
pairwise_stade_annee <- pairwise.adonis2(dist_mat ~ stade_annee, data = df_filtre)

# Extraction des résultats
liste_res_propre <- list()
for (nom in names(pairwise_stade_annee)) {
  temp <- pairwise_stade_annee[[nom]]
  if (is.data.frame(temp) && nrow(temp) > 0 && "Pr(>F)" %in% names(temp)) {
    liste_res_propre[[nom]] <- data.frame(
      Comparaison = nom,
      p_value = if(!is.null(temp$`Pr(>F)`[1])) temp$`Pr(>F)`[1] else NA,
      stringsAsFactors = FALSE
    )
  }
}
tab_final_stats <- do.call(rbind, liste_res_propre)
tab_final_stats$Sig <- symnum(tab_final_stats$p_value, cutpoints = c(0, 0.001, 0.01, 0.05, 1), symbols = c("***", "**", "*", "ns"))

# Filtrage
sync_res <- tab_final_stats[grepl("_2025_vs_.*_2025$|_2026_vs_.*_2026$", tab_final_stats$Comparaison), ]
diachro_res <- tab_final_stats[grepl("^(.*)_2025_vs_\\1_2026$", tab_final_stats$Comparaison), ]

# Texte viewer
html_final <- paste0("
<div class='etape' style='background-color: #ffffff; border-left: 6px solid #2980b9; margin-top: 20px; padding: 15px;'>
  <h2 style='margin-top: 0; color: #333;'>📊 ÉTAPE 5 : Analyse PERMANOVA</h2>
  <p>Nous évaluons si la composition floristique diffère significativement selon les stades de restauration et leur stabilité au cours du temps.</p>
  
  <h3 style='font-size: 1em; margin-bottom: 5px;'>💡 Comment interpréter ces résultats ?</h3>
  <ul style='font-size: 0.9em;'>
    <li><b>Test Global :</b> Indique si le stade et l'année expliquent globalement la variation de la flore.</li>
    <li><b>Analyse Synchronique :</b> Compare les différences entre stades au sein d'une même année.</li>
    <li><b>Analyse Diachronique :</b> Mesure l'évolution temporelle pour chaque stade.</li>
  </ul>
</div>

<div style='background-color: #ebf5fb; padding: 15px; border-radius: 6px; border: 1px solid #2980b9; margin-top: 15px;'>
  <h3 style='margin-top:0;'>Test Global</h3>
  <p>Modèle (Stade * Année) : <b>R² = ", round(res_global$R2[1], 3), ", p = ", round(res_global$`Pr(>F)`[1], 3), "</b></p>
  
  <h3>Analyse Synchronique</h3>
  <table style='width:100%; border-collapse: collapse; font-size: 11px;'>
    <tr style='border-bottom: 2px solid #2980b9;'><th>Comparaison</th><th>p-value</th><th>Sig</th></tr>",
                     paste0("<tr><td style='border-bottom:1px solid #bdc3c7; padding: 5px;'>", sync_res$Comparaison, "</td><td style='padding: 5px;'>", round(sync_res$p_value, 4), "</td><td style='padding: 5px;'>", sync_res$Sig, "</td></tr>", collapse=""),
                     "</table>

  <h3 style='margin-top: 15px;'>Analyse Diachronique</h3>
  <table style='width:100%; border-collapse: collapse; font-size: 11px;'>
    <tr style='border-bottom: 2px solid #2980b9;'><th>Comparaison</th><th>p-value</th><th>Sig</th></tr>",
                     paste0("<tr><td style='border-bottom:1px solid #bdc3c7; padding: 5px;'>", diachro_res$Comparaison, "</td><td style='padding: 5px;'>", round(diachro_res$p_value, 4), "</td><td style='padding: 5px;'>", diachro_res$Sig, "</td></tr>", collapse=""),
                     "</table>
</div>")

# Mise à jour
historique_guide <- paste0(historique_guide, html_final)
pause_viewer(historique_guide)

