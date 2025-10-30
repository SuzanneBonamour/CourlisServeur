# 📢 README

Ce README est en lien avec l'atlas "Utilisation de l'espace par le Courlis cendré dans le site fonctionnel de la réserve de Moëze-Oléron".

L'atlas est [ici](https://suzannebonamour.github.io/Atlas_Courlis/), son répertoire Github est [ici](https://github.com/SuzanneBonamour/Atlas_Courlis). 

Cette étude a été conduite par la LPO France et la RNN de Moëze-Oléron, en collaboration avec le CNRS (LIENSs) et le MNHN (CESCO). Ce projet est financé par le Fonds Vert et la Région Nouvelle-Aquitaine (Fonds Européen de Développement Régional, FEDER) dans le cadre du programme « Adaptation des limicoles aux changements climatiques ».

# 🎓 Autrice

[Suzanne Bonamour](https://github.com/SuzanneBonamour), chargée de traitement de données, LPO France

# 💻 Données et scripts

Dans une démarche de [science ouverte](https://www.science-ouverte.cnrs.fr/fr/) et [*FAIR*](https://www.ouvrirlascience.fr/fair-principles/), l'ensemble des codes et données utilisées et/ou produites dans ce projet sont disponibles ; et les analyses et cartographies sont entièrement reproductives.

Les données utilisées et produites sont téléchageables ici : XXX. Les données doivent être téléchargées en local avant de reproduire les analyses.

Pour connaitre le détail des analyses effectués, tous les scripts de code R sont téléchargeables ici, sur le répertoire Github [CourlisServeur](https://github.com/SuzanneBonamour/CourlisServeur.git). Il est possible de consulter les scripts directement sur le répertoire Github sans téléchager toutes les données.

Afin de repoduire les résultats, 1) faire tourner le script "A_Courlis_GPS_cleaning&behaviors.R" pour nettoyer les données GPS et identifier les comportements ; 2) puis le script "C_Courlis_Maps&Analyses_2.R" pour effectuer les analyses et produire les cartographies et graphiques. 

Attention : certaines étapes sont gourmandes en mémoire vive, puissance de proccesseur et donc en temps ! Pour ces étapes longues, il est possible de lire directement les fichers de résultats associés (ils se trouvent également dans les données ou le Github).

Structure du projet à télécharger pour la reproduction des analyses :

```
📁 Courlis/
│── 📂 1) CourlisServeur/         # équivalent du répertoire Github 
  |── 📂 1) code                  # scripts R
│── 📂 2) GPS/                    # données GPS d'origine, sans modification
│── 📂 3) Data/                   # jeux de données (autre que point GPS)
  |── 📂 1) data                  # jeux de données d'origine, sans modification
  |── 📂 2) data_generated        # jeux de donénes produits après modification au fûr et à mesure des analyses
│── 📂 4) Atlas_Courlis/          # script .qmd and fichiers .png et html pour régérer l'atlas
```

Toutes les analyses, graphiques et cartes ont été produites à l'aide du logiciel R version 4.4.2.

# 🔎 Détail des données utilisées

## Marée 

### Horaires des marées

Structure du dossiers de données marées :
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

Structure du dossiers de données de hauteur d'eau :
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

Le marégraphe utilisé pour obtenir les hauteurs d'eau (en m) est celui de l'ile d'Aix en priorité. Lorsque les données été manquante pour ce marégraphe, les hauteurs d'eau ont été prédites via une corrélation avec la cotinière et la rochelle. La variable choisie pour la hauteur d'eau est la variable "validé temps différé" en priorité, puis "brute temps différé", puis "brute haute fréquence". Les données de hauteurs d'eau ont été téléchargées via le site du [SHOM](https://data.shom.fr/donnees/refmar/189/download#001=eyJjIjpbLTI0Njc0Ni4zNzYyODU2MTMwMiw1NzMzNjYzLjU2NTM3OTgzXSwieiI6OCwiciI6MCwibCI6W3sidHlwZSI6IlJFRk1BUiIsImlkZW50aWZpZXIiOiJSRUZNQVIvUk9OSU0iLCJvcGFjaXR5IjoxLCJ2aXNpYmlsaXR5Ijp0cnVlfV19). La hauteur d'eau est moyennée pour chaque période du grain temporelle choisie (5 min).

XXX a vérif 
Basée sur l'expertise de terrain, les marées hautes ont été classées en fonction de la hauteur mesurée ou prédites. La marée est classifiée de marée de mortes eaux si la hauteur d'eau est inférieure à 4.8m, de marée de vives eaux entre 4.8m et 6.4m. Au-delà de 6.4m de hauteur d'eau, la marée provoque une submersion de la lagune.
XXX a vérif 

Définition des champs dans les données de hauteur d'eau : 
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

## Météorologie

Définition des champs dans les données de météorologie : 
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
 
Les évènements climatiques extrêmes (ECE) sont définis comme les évènements d'intensité supérieure au quartile 95 % des distributions du paramètres météorologiques sur la période 2015-2024. Trois variables ECE ont été calculé à partir de la vitesse et l'orientation moyenne journalière du vent : i) les évènements de vent fort : 5% des vitesses de vent les plus fortes, ii) les évènements de vent de Nord-Ouest : orientation du vent entre 270 et 360 degrés, et iii) les évènements de vent fort de Nord-Ouest : 5% des vitesses de vent les plus fortes et d'orientation entre 270 et 360 degrés. Un ECE est un jour où l'une de ces 3 variables a été détectées. L'utilisation de l'espace pour les comportements de repos et d'alimentation pendant les jours avec ECE détectés sont comparés aux jours j-7 avant les évènements extrêmes et considéré comme jour de référence.

## Chasse

XXX a vérif 
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
XXX a vérif 

## Periode de submersion
XXX a vérif 
- Date de submersion

## Sexe

Le sexe des individus est déterminé au baguage par morphométrie, les femelles étant plus grandes que les mâles. Quand associé à une catégories de sexe incertaines dans la base de donées (c-a-d, F? ou M?), il est considéré comment certain (c-a-d, F ou M).

## Age
Le courlis cendré est considéré comme juvénile de sa sortie du nid au 1er septembre de l'année suivant leur éclosion. Au-delà de cette période, ils arborent leur plumage mature et sont considéré comme adulte. L'âge des individus est déterminé au baguage grâce à leur plumage. Les individus juvéniles lors du baguage et de la pose du GPS deviennent adultes après le 1er septembre de l'année suivante.

## Jour & nuit 

- Période jour vs nuit calculés sur la base des lever et coucher du soleil issus du logiciel de marée "wxtide32"

## Ouverture de la brèche 

- brèche, ouverture de la digue : variable "brèche" : avant/après 2018 ; "brèche _summary" : digue intacte < 2018, ouverture progressive < 2021/07 ; ouverture complète > 2021/07 ; variable "bèche_detail" : "digue intacte" < 2018, ), "ouverture progressive" < 2020-10-01, "disparition du seuil" < 2021-07-01, "ouverture complète" > 2021-07-01

## Les données GPS

Les données GPS sont issues des bases de données XXX.

# 📊 Analyses 

## Identification des comportements de repos et d'alimentation

Dans un premier temps, les points GPS aberrants avec des barometries très grandes et des longidude/latitude égale à zéro ont été enlevés. 

Ensuite, des types de comportements ont été associés au points GPS. 

Chaque point GPS a été associé à une comportement de repos (roosting), d'alimentation (foraging), ou autre.

Pour différencier les différentes types de comportements à partir des caractéristiques des pints GPS, plusieurs filtres ont été appliqués.

Un point GPS est considéré comme correspondant à du repos si : 

i) sa vitesse de déplacement est (quasi)stationnaire et inférieure ou égale à 1 Km/h (estimé par partir de la fonction *speedfilter* du package R "adehabitatHR"), 

ii) il est enregistré entre 2h avant et 2h après une marée haute, 

iii) il est situé au-dessus du plus bas niveau d'eau de marée base (au-dessus du *Lowest Astronomical Tide* en anglais) 

et iv) il est en dehors de la zone intertidal (déterminée par Litto3D data).

Un point GPS est considéré comme correspondant à de la recherche alimentaire si : 

i) sa vitesse de déplacement est (quasi)stationnaire et inférieure ou égale à 1 Km/h (estimé par partir de la fonction *speedfilter* du package R "adehabitatHR"),

ii) il est enregistré entre 2h avant et 2h après une marée basse.

## Interpolation des points GPS 

A vérif ! 
Pour que chaque individu ait le même poids dans les analyses, un point toutes les 5 min ont été estimé pour chaque individu. Uniquement les points situés dans la zone d’étude ont été utilisés. Le temps entre chaque point de localisation sauvegardé par individu pouvant varier et provoquer des périodes de carences de données plus ou moins longues, les périodes où la balise GPS de l’oiseau a enregistré plus d’un point par période de 5 min ont été analysés (éviter d’analyser des positions GPS trop peu précises et de résolutions temporelles hétérogènes). Une limite basse de 100 points estimés par individus sur une période supérieure à 28 jours (de deux cycles lunaires) a été appliquée pour maintenir une très haute qualité de suivi des individus pour les analyses.

A vérif ! 
Le nettoyage des données issues des balises GPS a principalement été effectué à l'aide du package R adehabitat.
- Interpolation entre chaque points gps enregistré et estimation d'un point toutes les 5 min pour chaque individu
- Assignation de chaque point à un comportement "foraging" (alimentation => points entre 2h avant et après la marée base) ou "roosting" (repos => points entre 2h avant et après la marée haute), ou other
- Filtrage des points interpolés uniquement dans la zone d'étude 
- Filtrage des points interpolés uniquement sur les périodes où la balise gps de l'oiseau a enregistré plus d'un point par periode 5 min (les points avant de après la/les périodes de carence de la balise sont gardés, les points retirés sont seulement ceux interpolés à partir de données trop peu précises)
A vérif ! 

## Distribution d'utilisation de l'espace

**Méthodes des kernels** : 

L'ensemble des analyses spatiales sont basées sur des estimations de fonction de "distribution d'utilisation" de l'espace (<em>Utilization Distribution</em>, ou "UD" en anglais) qui décrivent la probabilité de présence d’un individu (ou d'un groupe d'individu) dans l'espace en fonction des points GPS qui lui ont été associés [Worton (1989)](#Worton1989). Les distributions d'utilisation permettent donc d’estimer les zones les plus fréquemment utilisées par un animal. Elles ont été effectuées par la méthode dite du noyau (*kernel* en anglais) et avec les fonctions [kernelUD]{style="font-family: 'Courier';"} et [getverticeshr]{style="font-family: 'Courier';"} du package "adehabitatHR" [Calenge (2006)](#calenge2006).

**Paramètre de lissage (h)** : 

L’estimation par noyau repose sur un paramètre de lissage (<em>bandwidth</em>, en anglais, nommé *h*), ici calculé selon la [règle de Silverman](https://fr.wikipedia.org/wiki/Estimation_par_noyau) adapté à chaque sous jeu de données pour chaque analyse, ajustée par un facteur de 1/2 pour permettre des analyses à grain fin (voir légende de chaque carte pour la valeur de *h* utilisée). En supposant que l'échantillon des points est distribué selon une loi Normale, *h* est donc estimé pour chaque analyse sptatiales comme suit : h = 1.06*var(point)*nb(point) ^-(1/5). Estimation de *h* pour les latitudes et longitude independemment, puis moyenné.

**Echantillonnage aléatoire** : 

Afin de garantir une représentativité (quasi)égale à chaque individu étudié malgré l'hétérogénéïté dans les quantités de point GPS enregistrés pour chacun, un échantillonnage aléatoire des points a été effectué à chaque analyse. Pour chaque individu et catégorie de variables analysées (comportement, zone, sexe, âge, etc...), 1000 points sont échantillonnés aléatoirement sans remise. La probabilité d'échantillonnage est fonction du temps entre chaque point, plus un point représente une période de temps peu enregistré, plus il aura de probabilité d'être échantillonné. Lorsqu'un individu présente moins de 1000 point pour une combinaison de variable données, tous les disponibles pour cette combinaison sont sélectionnés.

**Sous jeu de données utilisé** : 

Suivant les analyses, le jeu de données utilisé pour estimer les distributions d'utilisation de l'espace est différent et restreint à la zone A, B et C, au comportement ciblé (repos ou alimentation), et/ou à la (ou les) variable(s) d'intérêt(s) sans valeur inconnues (c'est-à-dire sans *NA*).

**Grain spatial** : 

Les analyses spatiales ont été effectués avec un grain spatial fin en grille de 10 m x 10 m.

**Domaines vitaux** : 

Les domaines vitaux (<em>home range</em> en anglais) ont été estimés par les mêmes méthodes de distributions spatial d'utilisation et de kernels comme précédemment, mais pour chaque individu séparément sur l’ensemble des points GPS, tous comportements confondus. Deux enveloppes de domaine vital sont calculées pour chaque individu : i) le domaine vital étendu qui correspond à l’enveloppe englobant 95 % de la surface d’utilisation, ii) le noyau d’activité correspondant aux 50 % de surface d'utilisation, représente les zones de fréquentation la plus intense.

**Distance entre les reposoirs et d'alimentations** : 

La distance entre la zones d'alimentation et de repos a été estimé comme la distance entre les paires de centres géographiques individuels des zones d'alimentation et de repos à chaque cycle de marée.

# 📜 Licence

Ce projet est sous licence [MIT](https://choosealicense.com/licenses/mit/) - voir le fichier [LICENSE](LICENSE) pour plus de détails.

# ✉️ Contact
Pour toute question, contactez-moi à : `suzanne.bonamour@lpo.fr` ou via [GitHub](https://github.com/SuzanneBonamour)
