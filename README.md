# Utilisation de l'espace par le Courlis cendré aux alentours de la réserve de Moëze-Oléron

***

## Introduction 

Cette étude entre dans le cadre du projet "Adaptation des limicoles aux changements climatiques". 

### Problématiques générales de l'étude 

Mieux comprendre l’utilisation de l’espace dans le bassin de Marennes et le marais de Brouage dans un contexte de recul du trait de côte, menant à une maritimisation de la réseve naturelle de Moëze-Oléron, et de forte pression anthropique (chasse et pêche à pied, ostréïcultures) pour proposer des zones potentielles d’accueil des limicoles à protéger.

### Objectifs principaux 

Analyses de données issues des balises GPS posé sur plusieurs dizaines de courlis cendré dans la réserve de Moëze-Oléron.
Production d'un atlas dynamique décrivant l'utilisation de l'espace pat les oiseaux et les conséquences des activités anthropiques et de la maritimisation. 

### Questions spéficiques abordées

1. Identification des "zones reposoirs" (roosting) : zone de reposoir vs. foraging ? zone de reposoirs en fonction de la hauteur d'eau (marée vives eaux et mortes eaux) ?
2. Fidélité aux reposoirs : répétabilité intra individuelle au cours d'une même année ? au cours de leur vie ? en fonction de la hateur d'eau (plasticité du reposoir) ?
3. Quelle distance entre les zones d'alimentation et les reposoirs et les zones fonction dans le marais (distance moyenne pour tous les ind, et par individus) ?
4. Quelles zones de report des oiseaux pour les reposoirs et l'alimentation lors des submersions, des périodes de chasses, de pêche, le jours vs. la nuit ?
5. Quelles utilisation de l'espace avant vs. après l'ouverture de la digue, mise en place de la brèche ?
6. Pourcentage home range dans vs. en dehors de la réserve ?
7. Pourcentage de temps de repos passé dans la réserve vs. en dehors ?
8.  Différence d'utilisation de l'espace entre les sexes et les ages (toutes ces analyses sont ventilées par sexe et par age) ?

***

## Matériels & méthodes

Toutes les analyses, graphiques et cartes ont été produites à l'aide du logiciel R version XXX.
Tous les scripts pour reproduire ces resultats sont disponibles dans ce répertoire GitHub.
Les données itilisées et produites sont téléchageables ici : XXX

### La zone d'étude

La zone d'étude est un rectangle de XXX km² qui s'étend :
- au Nord jusqu'à l'estuaire de la Charente,
- à l'Est jusqu'à la limite Est de la ville de Rochefort,
- au Sud jusqu'à l'estuaire de la Seudre,
- et à l'Ouest jusqu'à la facade Est de l'ile d'Oléron,
englobant ainsi zone fonctionnellle du bassin de Marennes, la réserve de Moëze-Oléron et le marais de Brouage.

### Données de marée 

Les données de marée ont été obtenues à partir du logiciel "wxtide32", téléchargeable [ici](https://wxtide32.informer.com/download/#download_content).
Le marégraphe utilisé est celui de l'ile d'Aix.

### Nettoyage des données GPS

Le nettoyage des données issues des balises GPS a principalement été effectué à l'aide du package R adehabitat.

- Filtrage des points "stationnaires" avec une vitesse maximal de 27 km/h
- Interpolation entre chaque points gps enregistré et estimation d'une point toutes les 30 min pour chaque individu
- Assignation de chaque point à un comportement "foraging" (alimentation) ou "roosting" (repos)
- Foraging : points entre 2h avant et après la marée base
- Roosting : points entre 2h avant et après la marée haute + avec une hauteur d'eau supérieure ou égale à XX pour les reposoirs, supérieure ou égale à XX pour les pré-reposoirs
- Filtrage des points interpolés uniquement dans la zone d'étude définie plus haut
- Filtrage des points interpolés uniquement sur les périodes où la balise gps de l'oiseau à enregistré plus d'un point par demie-heure (les points avant de après la/les périodes de carence de la balise sont gardés,les points retirés sont seulement ceux interpolé à partir de données trop peu précises)
- Filtrage des individus avec au moins 1000 points étalés sur une durée minimum de 2 fois 28 jours (2 cycles lunaires)
- Sexe associé à chaque individus, quand F? ou M?, considéré F ou M certain
- Période jour vs nuit calculés sur la base des lever et coucher du soleil issus du logiciel de marée "wxtide32"
- 

### Création du jeu de données de marées






***

## Résultats

***

## Eléments de discussion

***

## Conclusion

***







