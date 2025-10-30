# 📊 README - Atlas dynamique Courlis cendré

Ce README est en lien avec le code utilisé pour les analyses et cartographie présent dans l'atlas "Utilisation de l'espace par le Courlis cendré dans le site fonctionnel de la réserve de Moëze-Oléron"

Étude conduite par la LPO France et la RNN de Moëze-Oléron, en collaboration avec le CNRS (LIENSs) et le MNHN (CESCO).
Projet financé par le Fonds Vert et la Région Nouvelle-Aquitaine (Fonds Européen de Développement Régional, FEDER) dans le cadre du programme « Adaptation des limicoles aux changements climatiques ».

# 🎓 Autrice

[Suzanne Bonamour](https://github.com/SuzanneBonamour), chargée de traitement de données, LPO France

# 📂 Structure du projet

Les données itilisées et produites sont téléchageables ici : XXX

A télécharger en amont (pas assez de place sur github) :
```
📁 Courlis/
│── 📂 1) Data/                 # Jeux de données (autre que point GPS)
  |── 📂 1) data                  # Jeux de données d'origine, sans modification
    |── 📂 1) XXX                   # XXX
    |── 📂 1) XXX                   # XXX
    |── 📂 1) XXX                   # XXX
  |── 📂 2) data_generated        # Jeux de donénes produits après modification au fûr et à mesure des analyses
  |── 📂 3) images                # Figure, graphiques et cartes produites
│── 📂 2) GPS/                  # Jeux de données GPS d'origine, sans modification
```

Tous les scripts pour reproduire ces resultats sont disponibles dans ce répertoire GitHub.

Sur github :
```
📁 CourlisServeur/
│── 📂 1) code/               # Scripts R d'analyse et de visualisation
│── 📂 2) report/             # Rapports html
│── README.md                  # Documentation du projet
```

# 💻 Logiciels et langages programmation utilisé

Toutes les analyses, graphiques et cartes ont été produites à l'aide du logiciel R version 4.4.2.

# 📊 Jeux de données

## 🌊 Marée 

### Horaires des marées

```
📁 1) data/
│── 📂 Maree/ 
```

- **Nom du fichier** : `tides.csv`
- **Source** : [logiciel "wxtide32"](https://wxtide32.informer.com/download/#download_content)
- **Description** : Contient horaine de marée, ryhtme jour/nuit
- **Format** : CSV avec les colonnes suivantes :
  - `ID` : identifiant de la ligne
  - `y_m_d` : date format year-month-day
  - `type` : marée haute (high), marée basse (low)
  - `time` : heure
  - `sunrise` : heure lever du soleil
  - `sunset` : heure coucher du soleil
  - `moonset` : heure coucher de lune
  - `moonrise` : heure lever de lune
 
### Hauteur d'eau

```
📁 1) data/
│── 📂 Maree/               
  │── 📂 maregraphie/
    │── 📂 Ile_d_aix/                 # Marégraphe de l'Ile d'Aix, jeux de données d'origine
      │── 📂 ok/                         # jeux de données modifié pour R
    │── 📂 La_cotiniere/              # Marégraphe de La Cotinière, jeux de données d'origine
      │── 📂 ok/                         # jeux de données modifié pour R
    │── 📂 La_rochelle/               # Marégraphe de La Rochelle, jeux de données d'origine
      │── 📂 ok/                         # jeux de données modifié pour R
```

Hauteur d'eau en m.

Hauteur d'eau arrondie pour chaque période du grain temporelle choisi (5 min). 

Hauteur d'eau "validé temps différé" en priotité, puis "brute temps différé", puis "brute haute fréquence".

Le marégraphe utilisé est celui de l'ile d'Aix en priorité, puis corrélation avec la cotinière et la rochelle quand il y a des trous. 

- **Nom du fichier** : `189_2015.txt` ou même format
- **Source** : [Shom, LIENSs, CG Charente-Maritime / Vigicrues / Shom, GPM La Rochelle](https://data.shom.fr)
- **Station** : ILE_D_AIX / LA_COTINIERE / LA_ROCHELLE
- **Longitude** : -1.174341 / -1.32781 / -1.2206499576568604
- **Latitude** : 46.007357 / 45.913597 / 46.15850067138672
- **Description** : Hauteur d'eau au cours des marées
- **Fuseau horaire** : UTC
- **Référence verticale** : zero_hydrographique
- **Unité** : m
- **Format** : txt avec les colonnes suivantes :
  - `Date` : date et heure
  - `Valeur` : hauteur d'eau en m
  - `Source` : 1 ~ Données brutes temps réel, 2 ~ Données brutes temps différé, 3 ~ Données validées temps différé, 4 ~ Données horaires validées, 5 ~ Données horaires brutes, 6 ~ Pleines et basses mers


 Type de marée hautes en fonction de la hauteur : 
- inférieur à 4.8m ~ marée de mortes eaux
- entre 4.8m & 6.4m ~ marée de vives eaux
- supérieur à 6.4m ~ submersion
  
## ⛅ Météo

- **Nom du fichier** : `meteo_courlis_la_rochelle.xlsx`
- **Source** : [météostat](https://meteostat.net/fr/place/fr/la-rochelle?s=07315&t=2025-03-13/2025-03-20)
- **Description** : Donnée issue pour la station de La Rochelle. 
- **Format** : xlsx avec les colonnes suivantes :
  - `date` : date journalière
  - `tavg` : température journalière moyenne (°c)
  - `tmin` : température journalière minimum (°c)
  - `tmax` : température journalière maximale (°c)
  - `prcp` : précipitation totale
  - `snow` : neige
  - `wdir` : direction du vent (degré)
  - `wspd` : vitesse du vent
  - `wpgt` : pic de Rafale
  - `press` : pression atmosphérique
  - `tsun` : durée de l'ensoleillement
 
Extreme Climatic Event (ECE) = 5% des valeur les plus basses et 5% des valeurs les plus hautes de la période 2015-2024

## 🔪 Chasse

- Tonnes de chasses
- Zone de chasse
- Effort de chasse
- Periode de chasse


- **Nom du fichier** : `biodiversite.csv`
- **Source** : [Nom de la base de données ou de l'organisation]
- **Description** : Contient des observations sur la biodiversité (espèces, localisations, abondance, etc.).
- **Format** : CSV avec les colonnes suivantes :
  - `espece` : Nom de l'espèce
  - `localisation` : Coordonnées géographiques
  - `date_observation` : Date de l'observation  (time zone = UTC/Europe/...)
  - `abondance` : Nombre d'individus observés

## 🌊 Periode de submersion

- Date de submersion

## ♀️ Sexe

- Sexe associé à chaque individus lors du baguage.
- Quand F? ou M?, considéré F ou M certain

## 🪶 Age au baguage

- Age chronologique = juv l'année de baguage si juv, adult l'année de baguage si adult, adult les année suivantes si adult l'année de baguage, adult l'annéez n+2 si juv l'année de baguage
- Age chronologique avec passage de juv à adulte le XX 01/09 de chaque année

## 🎀 Age chronologique

- Age au baguage déterminé par le plumage

## ⏰ Jour & nuit 

- Période jour vs nuit calculés sur la base des lever et coucher du soleil issus du logiciel de marée "wxtide32"

## 🧱 Ouverture de la brèche 

- brèche, ouverture de la digue : variable "brèche" : avant/après 2018 ; "brèche _summary" : digue intacte < 2018, ouverture progressive < 2021/07 ; ouverture complète > 2021/07 ; variable "bèche_detail" : "digue intacte" < 2018, ), "ouverture progressive" < 2020-10-01, "disparition du seuil" < 2021-07-01,"ouverture complète" > 2021-07-01

# 🚀 Installation

*Within a particular ecosystem, there may be a common way of installing things, such as using Yarn, NuGet, or Homebrew. However, consider the possibility that whoever is reading your README is a novice and would like more guidance. Listing specific steps helps remove ambiguity and gets people to using your project as quickly as possible. If it only runs in a specific context like a particular programming language version or operating system or has dependencies that have to be installed manually, also add a Requirements subsection.*

1. **Cloner le dépôt**
   ```sh
   git clone [https://github.com/utilisateur/mon_projet_biodiversite.git](https://github.com/SuzanneBonamour/CourlisServeur.git)  
   ```

2. **Installer les dépendances**

Ouvrez R et exécutez :
   ```r
   install.packages(c("lubridate", "ggplot2", "sf", "classInt",
   "tidyr", "remotes", "leaflet", "adehabitatLT",
   "trip", "extrafont", "ggthemes", "raster",
   "graticule", "data.table", "stringi", "terra",
   "ggalt", "tidyverse", "beepr", "readr"))
   ```
   
# 📜 Utilisation

Afin de repoduire les résultats, faire tourner les scripts les uns après les autres par ordre alphabétique "A_Courlis_GPS_x", puis "B_Courlis_ENV_x", etc...

## 📍 Les données GPS

Les données GPS sont issues des bases de données XXX.

### 🚧 Nettoyage

Le nettoyage des données issues des balises GPS a principalement été effectué à l'aide du package R adehabitat.

Dans un premier temps, les points aberrants avec des barometries très grandes et des longidude/latitude égale à zéro ont été enlevés. 

Ensuite, des types de comportements ont été associés au points GPS. 

## 🦤 Identification des comportements de repos et d'alimentation

Chaque point GPS a été associé à une comportement de repos (roosting), d'alimentation (foraging), ou autre.

Pour différencier les différentes types de comportements à partir des caractéristiques des pints GPS, plusieurs filtres ont été appliqués.

Pour les points associés au compotement d'alimentation :

1) les points "stationnaires" avec une vitesse maximal de 1 km/h
2) les points enregistré autour d'une marée basse, 2h avant et 2h après la niveau le plus bas de la mer

Pour les points associés au compotement de repos :

1) les points "stationnaires" avec une vitesse maximal de 1 km/h
2) les points enregistré autour d'une marée basse, 2h avant et 2h après la niveau le plus bas de la mer
3) les points au dessus de la hauteur d'eau associé à la plus basse marée haute (c'est-à-dire, exclusion des points sous le nivau de la mer)
4) les points hors de la zone intertidale

Une fois les comportements associé aux points GPS, un échantillonnage des points à été fait pour chaque individu.

## ⌛ Echantillonnage des points GPS

Pour que chaque individu ait le même poids dans les analyses, un point toutes les 5 min ont été estimé pour chaque individu. Uniquement les points situés dans la zone d’étude ont été utilisés. Le temps entre chaque point de localisation sauvegardé par individu pouvant varier et provoquer des périodes de carences de données plus ou moins longues, les périodes où la balise GPS de l’oiseau a enregistré plus d’un point par période de 5 min ont été analysés (éviter d’analyser des positions GPS trop peu précises et de résolutions temporelles hétérogènes). Une limite basse de 100 points estimés par individus sur une période supérieure à 28 jours (de deux cycles lunaires) a été appliquée pour maintenir une très haute qualité de suivi des individus pour les analyses.

A vérif ! 
- Interpolation entre chaque points gps enregistré et estimation d'un point toutes les 5 min pour chaque individu
- Assignation de chaque point à un comportement "foraging" (alimentation => points entre 2h avant et après la marée base) ou "roosting" (repos => points entre 2h avant et après la marée haute), ou other
- Filtrage des points interpolés uniquement dans la zone d'étude 
- Filtrage des points interpolés uniquement sur les périodes où la balise gps de l'oiseau a enregistré plus d'un point par periode 5 min (les points avant de après la/les périodes de carence de la balise sont gardés, les points retirés sont seulement ceux interpolés à partir de données trop peu précises)

## 🌍 Utilisation de l'espace

Utilisation Distribution map (UD map)

Package AdehabitatHR

Fonction kernelUD

Règle de Silverman pour estimation de h : 

Estimation de h en supposant que l'échantillon des points est distribué selon une loi Normale, ainsi h = 1.06*var(point)*nb(point) ^-(1/5)

Voir : https://fr.wikipedia.org/wiki/Estimation_par_noyau

Estimation de h pour lat et pour lon independemment

Estimation de h pour chaque kernelUD (pas pour chaque ind, periode, etc) (?)

# 🌼 Remerciements

Je remercie chaleureusement :
- Les meilleures co-bureaux : Anaïs et Marine
- Anais une fois de plus pour nos brainstorming récurrent !
- Les membres sympathiques du Cambouis pour leur bons conseils et nos discussions endiablées à base data et de code <3

# 📜 Licence

Ce projet est sous licence [MIT](https://choosealicense.com/licenses/mit/) - voir le fichier [LICENSE](LICENSE) pour plus de détails.

# 📅 Historique des demandes

- Deadline : juin 2025 

# 🙏 Aides & informations utiles

*Tell people where they can go to for help. It can be any combination of an issue tracker, a chat room, an email address, etc.*

- [makeareadme](https://www.makeareadme.com/)
- [emoji list markdown](https://gist.github.com/rxaviers/7360908)

# ✉️ Contact
Pour toute question, contactez-moi à : `suzanne.bonamour@lpo.fr` ou via [GitHub](https://github.com/SuzanneBonamour)
