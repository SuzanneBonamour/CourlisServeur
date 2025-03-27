# 📊 Nom du projet 

Utilisation de l'espace par le Courlis cendré aux alentours de la réserve de Moëze-Oléron

# 🎓 Authors

[Suzanne Bonamour](https://github.com/SuzanneBonamour), chargée de traitement de données, LPO France

# ⏳ Project status

Work in progress

# 📌 Description du projet

Cette étude entre dans le cadre du projet "Adaptation des limicoles aux changements climatiques". 

Problématiques générales de l'étude : 

Mieux comprendre l’utilisation de l’espace dans le bassin de Marennes et le marais de Brouage dans un contexte de recul du trait de côte, menant à une maritimisation de la réseve naturelle de Moëze-Oléron, et de forte pression anthropique (chasse et pêche à pied, ostréïcultures) pour proposer des zones potentielles d’accueil des limicoles à protéger.

Objectifs principaux :

* Analyses de données issues des balises GPS posé sur plusieurs dizaines de courlis cendré dans la réserve de Moëze-Oléron.
* Production d'un atlas dynamique décrivant l'utilisation de l'espace pat les oiseaux et les conséquences des activités anthropiques et de la maritimisation. 

Questions spéficiques abordées :

1. Identification des "zones reposoirs" (roosting) : zone de reposoir vs. foraging ? zone de reposoirs en fonction de la hauteur d'eau (marée vives eaux et mortes eaux) ?
2. Fidélité aux reposoirs : répétabilité intra individuelle au cours d'une même année ? au cours de leur vie ? en fonction de la hateur d'eau (plasticité du reposoir) ?
3. Quelle distance entre les zones d'alimentation et les reposoirs et les zones fonction dans le marais (distance moyenne pour tous les ind, et par individus) ?
4. Quelles zones de report des oiseaux pour les reposoirs et l'alimentation lors des submersions, des périodes de chasses, de pêche, le jours vs. la nuit ?
5. Quelles utilisation de l'espace avant vs. après l'ouverture de la digue, mise en place de la brèche ?
6. Pourcentage home range dans vs. en dehors de la réserve ?
7. Pourcentage de temps de repos passé dans la réserve vs. en dehors ?
8.  Différence d'utilisation de l'espace entre les sexes et les ages (toutes ces analyses sont ventilées par sexe et par age) ?

La zone d'étude :

La zone d'étude est un rectangle de XXX km² qui s'étend :
- au Nord jusqu'à l'estuaire de la Charente,
- à l'Est jusqu'à la limite Est de la ville de Rochefort,
- au Sud jusqu'à l'estuaire de la Seudre,
- et à l'Ouest jusqu'à la facade Est de l'ile d'Oléron,
englobant ainsi zone fonctionnellle du bassin de Marennes, la réserve de Moëze-Oléron et le marais de Brouage.

## 📂 Structure du projet

```
📁 mon_projet_biodiversite/
│── 📂 data/               # Contient les jeux de données
│── 📂 scripts/            # Scripts R d'analyse et de visualisation
│── 📂 results/            # Résultats des analyses (graphiques, tableaux, etc.)
│── README.md              # Documentation du projet
│── requirements.txt       # Liste des packages R requis
```

## 💻 Logiciels et langages programmation utilisé

Toutes les analyses, graphiques et cartes ont été produites à l'aide du logiciel R version XXX et RStudio version XXX.

## 📊 Jeux de données

### Données de marée 

Les données de marée ont été obtenues à partir du logiciel "wxtide32", téléchargeable [ici](https://wxtide32.informer.com/download/#download_content).
Le marégraphe utilisé est celui de l'ile d'Aix en priorité, puis corrélation avec la cotinière et la rochelle quand il y a des trous 

Type de marée hautes en fonction de la hauteur :
Comme indiqué par Adrien... mais ça ne semble pas être les bonnes hauteurs avec ce que j'ai...
<= 3.57 ~ marée de mortes eaux >>> donc <= 5
Entre 3.57 & 6.9 ~ marée de vives eaux >>>> donc 5 & 6.3
>= 6.9 ~ submersion >>>>> donc 6.3


### Données environnementales

#### Hauteur d'eau

- Hauteur d'eau en m, issue du marégraphe de l'ile d'Aix
- Hauteur d'eau arrondie pour chaque péridoe de 30 min
- Hauteur d'eau "validé temps différé" en priotité, puis "brute temps différé", puis "brute haute fréquence".

#### (Chasse)

- Tonnes de chasses
- Zone de chasse
- Effort de chasse
- Periode de chasse

#### (Pêche à pied)

- Zone de pêche
- Effort de pêche
- Période de pêche

#### (Periode de submersion)

- Date d'innondation

#### Météo

Donnée issue du site météo stat, pour la station de La Rochelle 

- Température journalière moyenne, min et max
- Vitesse du vent
- Pression atmosphérique
- Direction du vent

Extreme Climatic Event (ECE) = 5% des valeur les plus basses et 5% des valeurs les plus hautes de la période 2015-2024





- **Nom du fichier** : `biodiversite.csv`
- **Source** : [Nom de la base de données ou de l'organisation]
- **Description** : Contient des observations sur la biodiversité (espèces, localisations, abondance, etc.).
- **Format** : CSV avec les colonnes suivantes :
  - `espece` : Nom de l'espèce
  - `localisation` : Coordonnées géographiques
  - `date_observation` : Date de l'observation  (time zone = UTC/Europe/...)
  - `abondance` : Nombre d'individus observés
 
- **Nom du fichier** : `biodiversite_2.csv`
- **Source** : [Nom de la base de données ou de l'organisation]
- **Description** : Contient des observations sur la biodiversité (espèces, localisations, abondance, etc.).
- **Format** : object sf avec les colonnes suivantes :
- **Projection** : 2154/4326/...
  - `esp` : Nom de l'espèce
  - `site` : Coordonnées géographiques
  - `hour` : Date de l'observation (time zone = UTC/Europe/...)
  - `nb` : Nombre d'individus observés
  - `geometry` : point/polygon/...

## 🚀 Installation

*Within a particular ecosystem, there may be a common way of installing things, such as using Yarn, NuGet, or Homebrew. However, consider the possibility that whoever is reading your README is a novice and would like more guidance. Listing specific steps helps remove ambiguity and gets people to using your project as quickly as possible. If it only runs in a specific context like a particular programming language version or operating system or has dependencies that have to be installed manually, also add a Requirements subsection.*

1. **Cloner le dépôt**
   ```sh
   git clone https://github.com/utilisateur/mon_projet_biodiversite.git
   cd mon_projet_biodiversite
   ```

2. **Installer les dépendances**
   Ouvrez R et exécutez :
   ```r
   install.packages(c("tidyverse", "ggplot2", "sf", "rmarkdown"))
   ```
   
## 📜 Utilisation

*Use examples liberally, and show the expected output if you can. It's helpful to have inline the smallest example of usage that you can demonstrate, while providing links to more sophisticated examples if they are too long to reasonably include in the README.*

Exécutez le script principal d'analyse :
```r
source("scripts/analyse_biodiversite.R")
```


Tous les scripts pour reproduire ces resultats sont disponibles dans ce répertoire GitHub.
Les données itilisées et produites sont téléchageables ici : XXX

Afin de repoduire les résultats, faire tourner les scripts les uns après les autres par ordre alphabétique "A_Courlis_GPS_x", puis "B_Courlis_ENV_x", etc...







### Utilisation de l'espace

#### Utilisation Distribution map (UD map)

Package AdehabitatHR

Fonction kernelUD

Règle de Silverman pour estimation de h : 

Estimation de h en supposant que l'échantillon des points est distribué selon une loi Normale, ainsi h = 1.06*var(point)*nb(point) ^-(1/5)

Voir : https://fr.wikipedia.org/wiki/Estimation_par_noyau

Estimation de h pour lat et pour lon independemment

Estimation de h pour chaque kernelUD (pas pour chaque ind, periode, etc) (?)





## 📈 Fonctionnalités principales
- Chargement et nettoyage des données 📂
- Analyse exploratoire 📊
- Visualisation des tendances 🌍
- Modélisation statistique 📉




### Nettoyage des données GPS

Le nettoyage des données issues des balises GPS a principalement été effectué à l'aide du package R adehabitat.

- Retrait d'une point aberrant : barometrie très grande et lon/lat = 0

- Filtrage des points "stationnaires" avec une vitesse maximal de 27 km/h
- Interpolation entre chaque points gps enregistré et estimation d'une point toutes les 30 min pour chaque individu
- Assignation de chaque point à un comportement "foraging" (alimentation) ou "roosting" (repos)
- Foraging : points entre 2h avant et après la marée base
- Roosting : points entre 2h avant et après la marée haute (+ avec une hauteur d'eau supérieure ou égale à XX pour les reposoirs, supérieure ou égale à XX pour les pré-reposoirs)
- Filtrage des points interpolés uniquement dans la zone d'étude définie plus haut
- Filtrage des points interpolés uniquement sur les périodes où la balise gps de l'oiseau à enregistré plus d'un point par demie-heure (les points avant de après la/les périodes de carence de la balise sont gardés,les points retirés sont seulement ceux interpolé à partir de données trop peu précises)
- Filtrage des individus avec au moins 1000 points étalés sur une durée minimum de 2 fois 28 jours (2 cycles lunaires)
- Sexe associé à chaque individus, quand F? ou M?, considéré F ou M certain
- Période jour vs nuit calculés sur la base des lever et coucher du soleil issus du logiciel de marée "wxtide32"
- Age au baguage + age chronologique = juv l'année de baguage si juv, adult l'année de baguage si adult, adult_plus les année suivantes si adult l'année de baguage, adult_plus l'annéez n+2 si juv l'année de baguage
- brèche, ouverture de la digue : variable "brèche" : avant/après 2018 ; "brèche _summary" : digue intacte < 2018, ouverture progressive < 2021/07 ; ouverture complète > 2021/07 ; variable "bèche_detail" : "digue intacte" < 2018, ), "ouverture progressive" < 2020-10-01, "disparition du seuil" < 2021-07-01,"ouverture complète" > 2021-07-01


## 🌼 Remerciements

Je remercie :
* Les meilleures co-bureaux : Anaïs et Marine
* Anais une fois de plus pour nos brainstorming récurrent !
* les membres sympathiques du Cambouis pour leur bons conseils et nos discussions endiablées à base data et de code <3

## 📜 Licence

Ce projet est sous licence [MIT](https://choosealicense.com/licenses/mit/) - voir le fichier [LICENSE](LICENSE) pour plus de détails.

## 📅 Historique des demandes

* 2023 : de mars à juin
* 2024 : de février à juin 

## Aides & informations utiles

*Tell people where they can go to for help. It can be any combination of an issue tracker, a chat room, an email address, etc.*

* [makeareadme](https://www.makeareadme.com/)
* [emoji list markdown](https://gist.github.com/rxaviers/7360908)

## ✉️ Contact
Pour toute question, contactez-moi à : `suzanne.bonamour@lpo.fr` ou via [GitHub](https://github.com/SuzanneBonamour)


