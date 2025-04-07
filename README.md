# 📊 Courlis

Utilisation de l'espace par le Courlis cendré aux alentours de la réserve de Moëze-Oléron

# 🎓 Autrice

[Suzanne Bonamour](https://github.com/SuzanneBonamour), chargée de traitement de données, LPO France

# ⏳ Statut du projet

Work in progress

# 📌 Description du projet

Cette étude entre dans le cadre du projet "Adaptation des limicoles aux changements climatiques". 

Problématiques générales de l'étude : 

Mieux comprendre l’utilisation de l’espace dans le bassin de Marennes et le marais de Brouage dans un contexte de recul du trait de côte, menant à une maritimisation de la réseve naturelle de Moëze-Oléron, et de forte pression anthropique (chasse et pêche à pied, ostréïcultures) pour proposer des zones potentielles d’accueil des limicoles à protéger.

Objectifs principaux :

- Analyses de données issues des balises GPS posé sur plusieurs dizaines de courlis cendré dans la réserve de Moëze-Oléron.
- Production d'un atlas dynamique décrivant l'utilisation de l'espace pat les oiseaux et les conséquences des activités anthropiques et de la maritimisation. 

Questions spéficiques abordées :

1. Identification des "zones reposoirs" (roosting) : zone de reposoir vs. foraging ? zone de reposoirs en fonction de la hauteur d'eau (marée vives eaux et mortes eaux) ?
2. Fidélité aux reposoirs : répétabilité intra individuelle au cours d'une même année ? au cours de leur vie ? en fonction de la hateur d'eau (plasticité du reposoir) ?
3. Quelle distance entre les zones d'alimentation et les reposoirs et les zones fonction dans le marais (distance moyenne pour tous les ind, et par individus) ?
4. Quelles zones de report des oiseaux pour les reposoirs et l'alimentation lors des submersions, des périodes de chasses, de pêche, le jours vs. la nuit ?
5. Quelles utilisation de l'espace avant vs. après l'ouverture de la digue, mise en place de la brèche ?
6. Pourcentage home range dans vs. en dehors de la réserve ?
7. Pourcentage de temps de repos passé dans la réserve vs. en dehors ?
8.  Différence d'utilisation de l'espace entre les sexes et les ages (toutes ces analyses sont ventilées par sexe et par age) ?

La zone d'étude est un rectangle de XXX km² qui s'étend :

- au Nord jusqu'à l'estuaire de la Charente,
- à l'Est jusqu'à la limite Est de la ville de Rochefort,
- au Sud jusqu'à l'estuaire de la Seudre,
- et à l'Ouest jusqu'à la facade Est de l'ile d'Oléron,
englobant ainsi zone fonctionnellle du bassin de Marennes, la réserve de Moëze-Oléron et le marais de Brouage.

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

Toutes les analyses, graphiques et cartes ont été produites à l'aide du logiciel R version XXX et RStudio version XXX.

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

## 🔫 Chasse

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

## 🎣 Pêche à pied

- Zone de pêche
- Effort de pêche
- Période de pêche

- **Nom du fichier** : `biodiversite.csv`
- **Source** : [Nom de la base de données ou de l'organisation]
- **Description** : Contient des observations sur la biodiversité (espèces, localisations, abondance, etc.).
- **Format** : CSV avec les colonnes suivantes :
  - `espece` : Nom de l'espèce
  - `localisation` : Coordonnées géographiques
  - `date_observation` : Date de l'observation  (time zone = UTC/Europe/...)
  - `abondance` : Nombre d'individus observés

## ♦️ Periode de submersion

- Date d'innondation

## ♀️ Sexe

- Sexe associé à chaque individus lors du baguage.
- Quand F? ou M?, considéré F ou M certain

## 🪶 Age au baguage

- Age chronologique = juv l'année de baguage si juv, adult l'année de baguage si adult, adult les année suivantes si adult l'année de baguage, adult l'annéez n+2 si juv l'année de baguage
- Age chronologique avec passage de juv à adulte le XX 01/09 de chaque année

## 🎁 Age chronologique

- Age au baguage déterminé par plumage

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

# 📈 Fonctionnalités principales

## 🚧 Nettoyage des données GPS

Le nettoyage des données issues des balises GPS a principalement été effectué à l'aide du package R adehabitat.

- Retrait d'une point aberrant : barometrie très grande et lon/lat = 0
- Filtrage des points "stationnaires" avec une vitesse maximal de 0.5 km/h
- Interpolation entre chaque points gps enregistré et estimation d'une point toutes les 5 min pour chaque individu
- Assignation de chaque point à un comportement "foraging" (alimentation => points entre 2h avant et après la marée base) ou "roosting" (repos => points entre 2h avant et après la marée haute), ou other
- Filtrage des points interpolés uniquement dans la zone d'étude 
- Filtrage des points interpolés uniquement sur les périodes où la balise gps de l'oiseau a enregistré plus d'un point par periode 5 min (les points avant de après la/les périodes de carence de la balise sont gardés, les points retirés sont seulement ceux interpolés à partir de données trop peu précises)
- Filtrage des individus avec au moins 1000 points étalés sur une durée minimum de 2 fois 28 jours (2 cycles lunaires)

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
