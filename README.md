# Analyse et suivi des communautés végétales - Mémoire M1


Ce dépôt contient le script et les données associés au travail réalisé dans le cadre de mon Mémoire de Master 1, sur le protocole de suivis des communautés végétales testé par le CEN Occitanie. L'objectif étant de proposer une démarche analytique complète, adaptée à la fois aux données produites par ce protocole et aux compétences des gestionnaires.


## 📂 Contenu du dépôt

- `Script_Données/` : Contient les jeux de données bruts (`Baseflor.xlsx`, `Data.xlsx`, `Dengler.xlsx`) ainsi que le script R (`Script.R`).
                                      -> Le jeu de données ici est celui du site de Fosse (66), il est donné à titre d'exmple si vous souhaitez tester le script. 
- `Graphs_Fosse/` : Regroupe les sorties graphiques générées par l'analyse pour le site de Fosse (66).

## 🧪 Analyses effectuées par le script

Le script réalise une analyse complète des données produits par le protocole en suivant plusieurs étapes clés :

- **Évaluation de l'effort d'échantillonnage :** Ajustement de modèles mathématiques **(Arrhenius, Gleason, Lomolino, Michaelis-Menten)**, puis choix d'un modèle pour réaliser des **courbes de raréfaction**. 
- **Analyse de la richesse spacifique :** Calcul de la richesse spécifique, puis tests de comparaison paramétriques **(test de Student)** ou non paramétriques **(test de Wilcoxon)**, selon la normalité de la distribution des données.
- **Analyse par classification hiérarchique :** Réalisation de **CAH (Classification ascendante hiérarchique)** et calcul de l'indice de **Calinski-Harabasz**.
- **Analyse multivariée :** Exécution d'une **AFC (Analyse factorielle des correspondances)**, avec l'utilisation de **tests de permutation (PERMANOVA/Adonis)** et projection des espèces les plus contributives, des types biologiques et d'indicateurs écologiques des espèces **(EIVE)**. 


  
## 📊 Préparation des données
Si vous souhaitez appliquer ces analyses à d'autres sites, il est impératif de respecter le format des fichiers sources :
-  **Format :** Utilisez exclusivement le format `.xlsx`. Et renommez votre fichier `Data.xlsx.
-  **Structure :** Vos fichiers doivent conserver strictement la même structure en format large que le fichier modèle fourni dans ce dépôt.
- <img width="1212" height="352" alt="image" src="https://github.com/user-attachments/assets/257c445d-b64e-4f59-8cf6-98b4d8b0926c" />




## 🚀 Instructions d'utilisation

Pour celles et ceux qui n'ont jamais utilisé R, pas de panique, ce script est adapté et conçu pour être accessible à tout le monde. Seul prérequis : Avoir installé **R** et **RStudio** sur votre ordinateur.

Pour exécuter l'analyse, suivez ces étapes simples :

1. **Préparation :** Enregistrez le fichier `Script.R` et les trois fichiers Excel (`Baseflor.xlsx`, `Data.xlsx`, `Dengler.xlsx`) dans le même dossier sur votre ordinateur.

2. **Ouverture :** Ouvrez le fichier `Script.R` avec le logiciel **RStudio**.

3. **Exécution :** Sélectionnez l'intégralité du script (Ctrl+A), puis cliquez sur le bouton **Run** en haut à droite de la fenêtre de script (ou utilisez le raccourci Ctrl+Enter). 

**Ensuite laissez-vous guider :** le script s'exécute automatiquement, toutes les étapes et explications sont présentées dans la fenêtre **Viewer** et la fonction personnalisée `pause.viewer` suspendra l'analyse aux étapes clés :
- **Affichage des résultats :** Regardez également la fenêtre **Plots** pour les graphiques lorsque cela est nécessaire. 
- **Interactivité :** Des fenêtres contextuelles (*pop-ups*) apparaîtront si vous devez renseigner des informations spécifiques.
- **Progression :** Pour passer à l'étape suivante, il vous suffit de cliquer sur le bouton **"Continuer"** situé en bas de la fenêtre **Viewer**.

4. **Enregistrements automatique des résultats :** Le script est également conçu pour enregistrer automatiquement sur votre ordinateur. 


## 👤 Contact

Pour toute question relative à ce travail ou à l'utilisation des scripts, n'hésitez pas à me contacter via mon profil GitHub.
