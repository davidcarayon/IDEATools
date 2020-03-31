
<!-- README.md is generated from README.Rmd. Please edit that file -->

![](www/ideatools.png)

IDEATools est un programme développé sur le logiciel R (sous forme de
package) dont l’objectif est de proposer des outils et ressources
mobilisables sous R afin de manipuler des données issues d’enquêtes
réalisées dans le cadre de la méthode IDEA4.

# Installation

En attendant sa publication officielle sur le CRAN, vous pouvez
télécharger et utiliser la version en cours de développement depuis
GitHub avec :

``` r
# install.packages("devtools")

devtools::install_github("davidcarayon/IDEATools")
```

# Description des modules

Sept principaux modules ont été développés et s’interconnectent tout au
long de la chaîne de traitement depuis l’import des calculateurs jusqu’à
la visualisation des résultats. Le septième module produit une interface
web interactive mobilisant l’ensemble des 6 autres modules.

![](www/dessin_modules.png)

## `importIDEA()`

Ce premier module sert à l’import des données issues d’un calculateur
IDEA4.

### Aspects techniques

Les formats d’entrée acceptés sont le format natif du calculateur
(.xls), mais également le format excel plus récent (.xlsx) ainsi qu’un
format de fichier standardisé directement exportable depuis les
dernières versions du calculateur \> 4.4.0 (.json).

La logique appliquée par l’algorithme pour récupérer les différentes
données est la suivante :

  - S’il s’agit d’un fichier .json, alors `jsonlite::fromJSON()` permet
    d’importer le fichier sans autres besoins de transformations.

  - S’il s’agit d’un fichier Excel (.xls ou .xlsx), utilisation de
    `readxl::read_excel()`:
    
      - Si le numéro de version indiqué dans l’onglet ‘Notice’ est
        supérieur à la version 4.2.0, alors l’algorithme ira chercher
        les données dans l’onglet ‘Renvoi BDD’. La procédure est dans ce
        cas identique à l’import JSON.
      - Si le numéro de version est inférieur à 4.2.0 ou inexistant,
        alors un algorithme alternatif ira chercher les métadonnées dans
        l’entête de l’onglet “Saisie et calculateur”, puis les scores
        déplafonnés dans les bilan de chaque dimension et les résultats
        plafonnés dans l’onglet du bilan global de la durabilité. Selon
        l’ancienneté dans le développement (2017 à 2020) du calculateur
        inséré, il est donc possible que certaines données soient
        manquantes (en particulier les métadonnées).

Les imports issus de fichiers Excels ont été standardisés pour être
identiques à l’import JSON. Ainsi, quel que soit le format d’entrée, le
fonctionnement des modules suivants reste toujours identique.

> **Attention : L’import sera interrompu dès que la moindre donnée
> relative aux indicateurs ou à certaines métadonnées clés (ex :
> présence d’élevage ou non) sera manquante.**

En ce qui concerne le nom de l’exploitation (ou l’id si le format est de
type json ou calculateur Excel \> 4.2.0), une absence d’information sera
traduite par la création d’un code aléatoire à 5 lettres. Si vous
décidez de forcer un import anonyme (en utilisant le second argument du
module, `anonymous` qui est un booléen), alors ce code aléatoire sera
affecté à l’exploitation même si il existe un identifiant renseigné.

Dans le cadre du développement de ce package, 3 fichiers de tests
(correspondants aux 3 possibilités d’import mentionnées plus haut) ont
été utilisés et sont accessibles dans le répertoire source du package :

``` r

# Renvoie la localisation du fichier. Ce chemin peut directement être inséré dans le module d'import IDEA
chemin <- file.path("example_json.json", package = "IDEATools") 

IDEAdata <- importIDEA(chemin)
```

En plus d’un import “simple”, ce module permet également d’importer un
répertoire de calculateurs entier, mêlant les 3 types de fichiers de
données (xls, xlsx et json).

Dans ce module, comme pour tous les autres, le traitement de fichiers
multiples a été résolue par le développement d’un sous-module de
traitement simple (un fichier à la fois) qui est appliqué de manière
itérative sur chaque fichier grâce aux fonctionnalités du package
`{purrr}`.

Les autres packages R mobilisés dans ce module sont :

  - `{readxl}` pour la lecture de fichiers excels et `{jsonlite}` pour
    les fichiers .json
  - `{dplyr}`, `{tidyr}`, `{stringr}` et `{janitor}` pour la
    manipulation de données
  - `{purrr}` pour les traitement itératifs dans le cas de traitements
    multiples

### Pré-calcul à l’importation.

Les calculateur Excel sont uniquement développés pour traiter l’approche
par les dimensions. De ce fait, à l’import de ces fichiers, aucun
résultat n’est disponible pour l’approche par les propriétés. Plutôt
que de développer un nouveau module qui procéderait à cette évaluation,
il semblait plus judicieux d’implémenter cette évaluation dès
l’importation. De ce fait, en fin d’import, toutes les données liées à
l’évaluation sont disponibles, les modules suivants ne se concentrent
que sur les restitutions graphiques.

Cette évaluation par les propriétés repose sur deux types de tables qui
ont été construites par le CS IDEA et qui sont stockées en tant que
données internes du package. Ces règles de décisions sont donc tout à
fait consultables (et prochainement modifiables) par un utilisateur du
package.

  - Une table fixant les seuils permettant d’attribuer une des quatre
    modalités “Favorable”, “Intermédiaire”, “Défavorable” ou “Très
    défavorable” à chacun des 53 indicateurs :

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#taeslxruus .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  /* table.margin.left */
  margin-right: auto;
  /* table.margin.right */
  color: #333333;
  font-size: 16px;
  /* table.font.size */
  background-color: #FFFFFF;
  /* table.background.color */
  width: auto;
  /* table.width */
  border-top-style: solid;
  /* table.border.top.style */
  border-top-width: 2px;
  /* table.border.top.width */
  border-top-color: #A8A8A8;
  /* table.border.top.color */
  border-bottom-style: solid;
  /* table.border.bottom.style */
  border-bottom-width: 2px;
  /* table.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* table.border.bottom.color */
}

#taeslxruus .gt_heading {
  background-color: #FFFFFF;
  /* heading.background.color */
  border-bottom-color: #FFFFFF;
  /* table.background.color */
  border-left-style: hidden;
  /* heading.border.lr.style */
  border-left-width: 1px;
  /* heading.border.lr.width */
  border-left-color: #D3D3D3;
  /* heading.border.lr.color */
  border-right-style: hidden;
  /* heading.border.lr.style */
  border-right-width: 1px;
  /* heading.border.lr.width */
  border-right-color: #D3D3D3;
  /* heading.border.lr.color */
}

#taeslxruus .gt_title {
  color: #333333;
  font-size: 125%;
  /* heading.title.font.size */
  font-weight: initial;
  /* heading.title.font.weight */
  padding-top: 4px;
  /* heading.top.padding - not yet used */
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  /* table.background.color */
  border-bottom-width: 0;
}

#taeslxruus .gt_subtitle {
  color: #333333;
  font-size: 85%;
  /* heading.subtitle.font.size */
  font-weight: initial;
  /* heading.subtitle.font.weight */
  padding-top: 0;
  padding-bottom: 4px;
  /* heading.bottom.padding - not yet used */
  border-top-color: #FFFFFF;
  /* table.background.color */
  border-top-width: 0;
}

#taeslxruus .gt_bottom_border {
  border-bottom-style: solid;
  /* heading.border.bottom.style */
  border-bottom-width: 2px;
  /* heading.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* heading.border.bottom.color */
}

#taeslxruus .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  padding-top: 4px;
  padding-bottom: 4px;
}

#taeslxruus .gt_col_headings {
  border-top-style: solid;
  /* column_labels.border.top.style */
  border-top-width: 2px;
  /* column_labels.border.top.width */
  border-top-color: #D3D3D3;
  /* column_labels.border.top.color */
  border-bottom-style: solid;
  /* column_labels.border.bottom.style */
  border-bottom-width: 2px;
  /* column_labels.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* column_labels.border.bottom.color */
  border-left-style: none;
  /* column_labels.border.lr.style */
  border-left-width: 1px;
  /* column_labels.border.lr.width */
  border-left-color: #D3D3D3;
  /* column_labels.border.lr.color */
  border-right-style: none;
  /* column_labels.border.lr.style */
  border-right-width: 1px;
  /* column_labels.border.lr.width */
  border-right-color: #D3D3D3;
  /* column_labels.border.lr.color */
}

#taeslxruus .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  /* column_labels.background.color */
  font-size: 100%;
  /* column_labels.font.size */
  font-weight: normal;
  /* column_labels.font.weight */
  text-transform: inherit;
  /* column_labels.text_transform */
  vertical-align: middle;
  padding: 5px;
  margin: 10px;
  overflow-x: hidden;
}

#taeslxruus .gt_sep_right {
  border-right: 5px solid #FFFFFF;
}

#taeslxruus .gt_group_heading {
  padding: 8px;
  /* row_group.padding */
  color: #333333;
  background-color: #FFFFFF;
  /* row_group.background.color */
  font-size: 100%;
  /* row_group.font.size */
  font-weight: initial;
  /* row_group.font.weight */
  text-transform: inherit;
  /* row_group.text_transform */
  border-top-style: solid;
  /* row_group.border.top.style */
  border-top-width: 2px;
  /* row_group.border.top.width */
  border-top-color: #D3D3D3;
  /* row_group.border.top.color */
  border-bottom-style: solid;
  /* row_group.border.bottom.style */
  border-bottom-width: 2px;
  /* row_group.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* row_group.border.bottom.color */
  border-left-style: none;
  /* row_group.border.left.style */
  border-left-width: 1px;
  /* row_group.border.left.width */
  border-left-color: #D3D3D3;
  /* row_group.border.left.color */
  border-right-style: none;
  /* row_group.border.right.style */
  border-right-width: 1px;
  /* row_group.border.right.width */
  border-right-color: #D3D3D3;
  /* row_group.border.right.color */
  vertical-align: middle;
}

#taeslxruus .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  /* row_group.background.color */
  font-size: 100%;
  /* row_group.font.size */
  font-weight: initial;
  /* row_group.font.weight */
  border-top-style: solid;
  /* row_group.border.top.style */
  border-top-width: 2px;
  /* row_group.border.top.width */
  border-top-color: #D3D3D3;
  /* row_group.border.top.color */
  border-bottom-style: solid;
  /* row_group.border.bottom.style */
  border-bottom-width: 2px;
  /* row_group.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* row_group.border.bottom.color */
  vertical-align: middle;
}

#taeslxruus .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
  /* row.striping.background_color */
}

#taeslxruus .gt_from_md > :first-child {
  margin-top: 0;
}

#taeslxruus .gt_from_md > :last-child {
  margin-bottom: 0;
}

#taeslxruus .gt_row {
  padding-top: 8px;
  /* data_row.padding */
  padding-bottom: 8px;
  /* data_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  /* table_body.hlines.style */
  border-top-width: 1px;
  /* table_body.hlines.width */
  border-top-color: #D3D3D3;
  /* table_body.hlines.color */
  border-left-style: none;
  /* table_body.vlines.style */
  border-left-width: 1px;
  /* table_body.vlines.width */
  border-left-color: #D3D3D3;
  /* table_body.vlines.color */
  border-right-style: none;
  /* table_body.vlines.style */
  border-right-width: 1px;
  /* table_body.vlines.width */
  border-right-color: #D3D3D3;
  /* table_body.vlines.color */
  vertical-align: middle;
  overflow-x: hidden;
}

#taeslxruus .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  /* stub.background.color */
  font-weight: initial;
  /* stub.font.weight */
  text-transform: inherit;
  /* stub.text_transform */
  border-right-style: solid;
  /* stub.border.style */
  border-right-width: 2px;
  /* stub.border.width */
  border-right-color: #D3D3D3;
  /* stub.border.color */
  padding-left: 12px;
}

#taeslxruus .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  /* summary_row.background.color */
  text-transform: inherit;
  /* summary_row.text_transform */
  padding-top: 8px;
  /* summary_row.padding */
  padding-bottom: 8px;
  /* summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
}

#taeslxruus .gt_first_summary_row {
  padding-top: 8px;
  /* summary_row.padding */
  padding-bottom: 8px;
  /* summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  /* summary_row.border.style */
  border-top-width: 2px;
  /* summary_row.border.width */
  border-top-color: #D3D3D3;
  /* summary_row.border.color */
}

#taeslxruus .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  /* grand_summary_row.background.color */
  text-transform: inherit;
  /* grand_summary_row.text_transform */
  padding-top: 8px;
  /* grand_summary_row.padding */
  padding-bottom: 8px;
  /* grand_summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
}

#taeslxruus .gt_first_grand_summary_row {
  padding-top: 8px;
  /* grand_summary_row.padding */
  padding-bottom: 8px;
  /* grand_summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  /* grand_summary_row.border.style */
  border-top-width: 6px;
  /* grand_summary_row.border.width */
  border-top-color: #D3D3D3;
  /* grand_summary_row.border.color */
}

#taeslxruus .gt_table_body {
  border-top-style: solid;
  /* table_body.border.top.style */
  border-top-width: 2px;
  /* table_body.border.top.width */
  border-top-color: #D3D3D3;
  /* table_body.border.top.color */
  border-bottom-style: solid;
  /* table_body.border.bottom.style */
  border-bottom-width: 2px;
  /* table_body.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* table_body.border.bottom.color */
}

#taeslxruus .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  /* footnotes.background.color */
  border-bottom-style: none;
  /* footnotes.border.bottom.style */
  border-bottom-width: 2px;
  /* footnotes.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* footnotes.border.bottom.color */
  border-left-style: none;
  /* footnotes.border.lr.color */
  border-left-width: 2px;
  /* footnotes.border.lr.color */
  border-left-color: #D3D3D3;
  /* footnotes.border.lr.color */
  border-right-style: none;
  /* footnotes.border.lr.color */
  border-right-width: 2px;
  /* footnotes.border.lr.color */
  border-right-color: #D3D3D3;
  /* footnotes.border.lr.color */
}

#taeslxruus .gt_footnote {
  margin: 0px;
  font-size: 90%;
  /* footnotes.font.size */
  padding: 4px;
  /* footnotes.padding */
}

#taeslxruus .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  /* source_notes.background.color */
  border-bottom-style: none;
  /* source_notes.border.bottom.style */
  border-bottom-width: 2px;
  /* source_notes.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* source_notes.border.bottom.color */
  border-left-style: none;
  /* source_notes.border.lr.style */
  border-left-width: 2px;
  /* source_notes.border.lr.style */
  border-left-color: #D3D3D3;
  /* source_notes.border.lr.style */
  border-right-style: none;
  /* source_notes.border.lr.style */
  border-right-width: 2px;
  /* source_notes.border.lr.style */
  border-right-color: #D3D3D3;
  /* source_notes.border.lr.style */
}

#taeslxruus .gt_sourcenote {
  font-size: 90%;
  /* source_notes.font.size */
  padding: 4px;
  /* source_notes.padding */
}

#taeslxruus .gt_left {
  text-align: left;
}

#taeslxruus .gt_center {
  text-align: center;
}

#taeslxruus .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#taeslxruus .gt_font_normal {
  font-weight: normal;
}

#taeslxruus .gt_font_bold {
  font-weight: bold;
}

#taeslxruus .gt_font_italic {
  font-style: italic;
}

#taeslxruus .gt_super {
  font-size: 65%;
}

#taeslxruus .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="taeslxruus" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

indicateur

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

nom\_indicateur

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

TD

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

D

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

I

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

F

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_left">

A1

</td>

<td class="gt_row gt_left">

Diversité des espèces cultivées

</td>

<td class="gt_row gt_right">

NA

</td>

<td class="gt_row gt_right">

0

</td>

<td class="gt_row gt_right">

3

</td>

<td class="gt_row gt_right">

4

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_striped">

A2

</td>

<td class="gt_row gt_left gt_striped">

Diversité génétique

</td>

<td class="gt_row gt_right gt_striped">

NA

</td>

<td class="gt_row gt_right gt_striped">

0

</td>

<td class="gt_row gt_right gt_striped">

2

</td>

<td class="gt_row gt_right gt_striped">

4

</td>

</tr>

<tr>

<td class="gt_row gt_left">

A3

</td>

<td class="gt_row gt_left">

Diversité temporelle des cultures

</td>

<td class="gt_row gt_right">

NA

</td>

<td class="gt_row gt_right">

0

</td>

<td class="gt_row gt_right">

3

</td>

<td class="gt_row gt_right">

5

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_striped">

A4

</td>

<td class="gt_row gt_left gt_striped">

Qualité de l’organisation spatiale

</td>

<td class="gt_row gt_right gt_striped">

NA

</td>

<td class="gt_row gt_right gt_striped">

0

</td>

<td class="gt_row gt_right gt_striped">

3

</td>

<td class="gt_row gt_right gt_striped">

5

</td>

</tr>

<tr>

<td class="gt_row gt_left">

A5

</td>

<td class="gt_row gt_left">

Gestion des insectes pollinisateurs et des auxiliaires des cultures

</td>

<td class="gt_row gt_right">

NA

</td>

<td class="gt_row gt_right">

0

</td>

<td class="gt_row gt_right">

2

</td>

<td class="gt_row gt_right">

4

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_striped">

A6

</td>

<td class="gt_row gt_left gt_striped">

Autonomie en énergie, matériaux, matériels, semences et plants

</td>

<td class="gt_row gt_right gt_striped">

NA

</td>

<td class="gt_row gt_right gt_striped">

0

</td>

<td class="gt_row gt_right gt_striped">

3

</td>

<td class="gt_row gt_right gt_striped">

5

</td>

</tr>

</tbody>

</table>

</div>

<!--/html_preserve-->

  - L’ensemble des 46 tableaux de contingence élaborées par le biais du
    logiciel DEXi, puis également stockés sous forme de liste sous R :

<!-- end list -->

``` r
data("decision_rules_total")
```

``` r
names(decision_rules_total)
#>  [1] "node_1"  "node_2"  "node_3"  "node_4"  "node_5"  "node_6"  "node_7" 
#>  [8] "node_8"  "node_9"  "node_10" "node_11" "node_12" "node_13" "node_14"
#> [15] "node_15" "node_16" "node_17" "node_18" "node_19" "node_20" "node_21"
#> [22] "node_22" "node_23" "node_24" "node_25" "node_26" "node_27" "node_28"
#> [29] "node_29" "node_30" "node_31" "node_32" "node_33" "node_34" "node_35"
#> [36] "node_36" "node_37" "node_38" "node_39" "node_40" "node_41" "node_42"
#> [43] "node_43" "node_44" "node_45" "node_46"

head(decision_rules_total$node_1) %>% gt()
```

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#qwewykcmkd .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  /* table.margin.left */
  margin-right: auto;
  /* table.margin.right */
  color: #333333;
  font-size: 16px;
  /* table.font.size */
  background-color: #FFFFFF;
  /* table.background.color */
  width: auto;
  /* table.width */
  border-top-style: solid;
  /* table.border.top.style */
  border-top-width: 2px;
  /* table.border.top.width */
  border-top-color: #A8A8A8;
  /* table.border.top.color */
  border-bottom-style: solid;
  /* table.border.bottom.style */
  border-bottom-width: 2px;
  /* table.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* table.border.bottom.color */
}

#qwewykcmkd .gt_heading {
  background-color: #FFFFFF;
  /* heading.background.color */
  border-bottom-color: #FFFFFF;
  /* table.background.color */
  border-left-style: hidden;
  /* heading.border.lr.style */
  border-left-width: 1px;
  /* heading.border.lr.width */
  border-left-color: #D3D3D3;
  /* heading.border.lr.color */
  border-right-style: hidden;
  /* heading.border.lr.style */
  border-right-width: 1px;
  /* heading.border.lr.width */
  border-right-color: #D3D3D3;
  /* heading.border.lr.color */
}

#qwewykcmkd .gt_title {
  color: #333333;
  font-size: 125%;
  /* heading.title.font.size */
  font-weight: initial;
  /* heading.title.font.weight */
  padding-top: 4px;
  /* heading.top.padding - not yet used */
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  /* table.background.color */
  border-bottom-width: 0;
}

#qwewykcmkd .gt_subtitle {
  color: #333333;
  font-size: 85%;
  /* heading.subtitle.font.size */
  font-weight: initial;
  /* heading.subtitle.font.weight */
  padding-top: 0;
  padding-bottom: 4px;
  /* heading.bottom.padding - not yet used */
  border-top-color: #FFFFFF;
  /* table.background.color */
  border-top-width: 0;
}

#qwewykcmkd .gt_bottom_border {
  border-bottom-style: solid;
  /* heading.border.bottom.style */
  border-bottom-width: 2px;
  /* heading.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* heading.border.bottom.color */
}

#qwewykcmkd .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  padding-top: 4px;
  padding-bottom: 4px;
}

#qwewykcmkd .gt_col_headings {
  border-top-style: solid;
  /* column_labels.border.top.style */
  border-top-width: 2px;
  /* column_labels.border.top.width */
  border-top-color: #D3D3D3;
  /* column_labels.border.top.color */
  border-bottom-style: solid;
  /* column_labels.border.bottom.style */
  border-bottom-width: 2px;
  /* column_labels.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* column_labels.border.bottom.color */
  border-left-style: none;
  /* column_labels.border.lr.style */
  border-left-width: 1px;
  /* column_labels.border.lr.width */
  border-left-color: #D3D3D3;
  /* column_labels.border.lr.color */
  border-right-style: none;
  /* column_labels.border.lr.style */
  border-right-width: 1px;
  /* column_labels.border.lr.width */
  border-right-color: #D3D3D3;
  /* column_labels.border.lr.color */
}

#qwewykcmkd .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  /* column_labels.background.color */
  font-size: 100%;
  /* column_labels.font.size */
  font-weight: normal;
  /* column_labels.font.weight */
  text-transform: inherit;
  /* column_labels.text_transform */
  vertical-align: middle;
  padding: 5px;
  margin: 10px;
  overflow-x: hidden;
}

#qwewykcmkd .gt_sep_right {
  border-right: 5px solid #FFFFFF;
}

#qwewykcmkd .gt_group_heading {
  padding: 8px;
  /* row_group.padding */
  color: #333333;
  background-color: #FFFFFF;
  /* row_group.background.color */
  font-size: 100%;
  /* row_group.font.size */
  font-weight: initial;
  /* row_group.font.weight */
  text-transform: inherit;
  /* row_group.text_transform */
  border-top-style: solid;
  /* row_group.border.top.style */
  border-top-width: 2px;
  /* row_group.border.top.width */
  border-top-color: #D3D3D3;
  /* row_group.border.top.color */
  border-bottom-style: solid;
  /* row_group.border.bottom.style */
  border-bottom-width: 2px;
  /* row_group.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* row_group.border.bottom.color */
  border-left-style: none;
  /* row_group.border.left.style */
  border-left-width: 1px;
  /* row_group.border.left.width */
  border-left-color: #D3D3D3;
  /* row_group.border.left.color */
  border-right-style: none;
  /* row_group.border.right.style */
  border-right-width: 1px;
  /* row_group.border.right.width */
  border-right-color: #D3D3D3;
  /* row_group.border.right.color */
  vertical-align: middle;
}

#qwewykcmkd .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  /* row_group.background.color */
  font-size: 100%;
  /* row_group.font.size */
  font-weight: initial;
  /* row_group.font.weight */
  border-top-style: solid;
  /* row_group.border.top.style */
  border-top-width: 2px;
  /* row_group.border.top.width */
  border-top-color: #D3D3D3;
  /* row_group.border.top.color */
  border-bottom-style: solid;
  /* row_group.border.bottom.style */
  border-bottom-width: 2px;
  /* row_group.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* row_group.border.bottom.color */
  vertical-align: middle;
}

#qwewykcmkd .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
  /* row.striping.background_color */
}

#qwewykcmkd .gt_from_md > :first-child {
  margin-top: 0;
}

#qwewykcmkd .gt_from_md > :last-child {
  margin-bottom: 0;
}

#qwewykcmkd .gt_row {
  padding-top: 8px;
  /* data_row.padding */
  padding-bottom: 8px;
  /* data_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  /* table_body.hlines.style */
  border-top-width: 1px;
  /* table_body.hlines.width */
  border-top-color: #D3D3D3;
  /* table_body.hlines.color */
  border-left-style: none;
  /* table_body.vlines.style */
  border-left-width: 1px;
  /* table_body.vlines.width */
  border-left-color: #D3D3D3;
  /* table_body.vlines.color */
  border-right-style: none;
  /* table_body.vlines.style */
  border-right-width: 1px;
  /* table_body.vlines.width */
  border-right-color: #D3D3D3;
  /* table_body.vlines.color */
  vertical-align: middle;
  overflow-x: hidden;
}

#qwewykcmkd .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  /* stub.background.color */
  font-weight: initial;
  /* stub.font.weight */
  text-transform: inherit;
  /* stub.text_transform */
  border-right-style: solid;
  /* stub.border.style */
  border-right-width: 2px;
  /* stub.border.width */
  border-right-color: #D3D3D3;
  /* stub.border.color */
  padding-left: 12px;
}

#qwewykcmkd .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  /* summary_row.background.color */
  text-transform: inherit;
  /* summary_row.text_transform */
  padding-top: 8px;
  /* summary_row.padding */
  padding-bottom: 8px;
  /* summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
}

#qwewykcmkd .gt_first_summary_row {
  padding-top: 8px;
  /* summary_row.padding */
  padding-bottom: 8px;
  /* summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  /* summary_row.border.style */
  border-top-width: 2px;
  /* summary_row.border.width */
  border-top-color: #D3D3D3;
  /* summary_row.border.color */
}

#qwewykcmkd .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  /* grand_summary_row.background.color */
  text-transform: inherit;
  /* grand_summary_row.text_transform */
  padding-top: 8px;
  /* grand_summary_row.padding */
  padding-bottom: 8px;
  /* grand_summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
}

#qwewykcmkd .gt_first_grand_summary_row {
  padding-top: 8px;
  /* grand_summary_row.padding */
  padding-bottom: 8px;
  /* grand_summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  /* grand_summary_row.border.style */
  border-top-width: 6px;
  /* grand_summary_row.border.width */
  border-top-color: #D3D3D3;
  /* grand_summary_row.border.color */
}

#qwewykcmkd .gt_table_body {
  border-top-style: solid;
  /* table_body.border.top.style */
  border-top-width: 2px;
  /* table_body.border.top.width */
  border-top-color: #D3D3D3;
  /* table_body.border.top.color */
  border-bottom-style: solid;
  /* table_body.border.bottom.style */
  border-bottom-width: 2px;
  /* table_body.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* table_body.border.bottom.color */
}

#qwewykcmkd .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  /* footnotes.background.color */
  border-bottom-style: none;
  /* footnotes.border.bottom.style */
  border-bottom-width: 2px;
  /* footnotes.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* footnotes.border.bottom.color */
  border-left-style: none;
  /* footnotes.border.lr.color */
  border-left-width: 2px;
  /* footnotes.border.lr.color */
  border-left-color: #D3D3D3;
  /* footnotes.border.lr.color */
  border-right-style: none;
  /* footnotes.border.lr.color */
  border-right-width: 2px;
  /* footnotes.border.lr.color */
  border-right-color: #D3D3D3;
  /* footnotes.border.lr.color */
}

#qwewykcmkd .gt_footnote {
  margin: 0px;
  font-size: 90%;
  /* footnotes.font.size */
  padding: 4px;
  /* footnotes.padding */
}

#qwewykcmkd .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  /* source_notes.background.color */
  border-bottom-style: none;
  /* source_notes.border.bottom.style */
  border-bottom-width: 2px;
  /* source_notes.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* source_notes.border.bottom.color */
  border-left-style: none;
  /* source_notes.border.lr.style */
  border-left-width: 2px;
  /* source_notes.border.lr.style */
  border-left-color: #D3D3D3;
  /* source_notes.border.lr.style */
  border-right-style: none;
  /* source_notes.border.lr.style */
  border-right-width: 2px;
  /* source_notes.border.lr.style */
  border-right-color: #D3D3D3;
  /* source_notes.border.lr.style */
}

#qwewykcmkd .gt_sourcenote {
  font-size: 90%;
  /* source_notes.font.size */
  padding: 4px;
  /* source_notes.padding */
}

#qwewykcmkd .gt_left {
  text-align: left;
}

#qwewykcmkd .gt_center {
  text-align: center;
}

#qwewykcmkd .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#qwewykcmkd .gt_font_normal {
  font-weight: normal;
}

#qwewykcmkd .gt_font_bold {
  font-weight: bold;
}

#qwewykcmkd .gt_font_italic {
  font-style: italic;
}

#qwewykcmkd .gt_super {
  font-size: 65%;
}

#qwewykcmkd .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="qwewykcmkd" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

A1 - Diversité des cultures

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

A3 - Diversité temporelle des cultures

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

A4 - Qualité spatiale du territoire

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

Diversité de l’organisation spatiale et temporelle

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_left">

défavorable

</td>

<td class="gt_row gt_left">

défavorable

</td>

<td class="gt_row gt_left">

défavorable

</td>

<td class="gt_row gt_left">

très défavorable

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_striped">

défavorable

</td>

<td class="gt_row gt_left gt_striped">

défavorable

</td>

<td class="gt_row gt_left gt_striped">

intermédiaire

</td>

<td class="gt_row gt_left gt_striped">

très défavorable

</td>

</tr>

<tr>

<td class="gt_row gt_left">

défavorable

</td>

<td class="gt_row gt_left">

défavorable

</td>

<td class="gt_row gt_left">

favorable

</td>

<td class="gt_row gt_left">

défavorable

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_striped">

défavorable

</td>

<td class="gt_row gt_left gt_striped">

intermédiaire

</td>

<td class="gt_row gt_left gt_striped">

défavorable

</td>

<td class="gt_row gt_left gt_striped">

très défavorable

</td>

</tr>

<tr>

<td class="gt_row gt_left">

défavorable

</td>

<td class="gt_row gt_left">

intermédiaire

</td>

<td class="gt_row gt_left">

intermédiaire

</td>

<td class="gt_row gt_left">

défavorable

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_striped">

défavorable

</td>

<td class="gt_row gt_left gt_striped">

intermédiaire

</td>

<td class="gt_row gt_left gt_striped">

favorable

</td>

<td class="gt_row gt_left gt_striped">

défavorable

</td>

</tr>

</tbody>

</table>

</div>

<!--/html_preserve-->

## `MakeTrees()`

Ce module permet la construction de cartes heuristiques colorées à
partir de données importées par le module précédent. Différentes
approches faisant appel à la librairie `{ggplot2}` ont d’abord été
explorées pour cette fonctionnalité, avant d’être remplacées par une
procédure plus complexe s’appuyant sur du dessin vectoriel .svg.

Des modèles (ou canvas) non colorés ont d’abord été dessinés sous le
logiciel Inkscape. Un identifiant unique a été assigné à chaque
rectangle afin de pouvoir lier chaque rectangle à l’indicateur qu’il
représente. Le code source svg (donc type XML) de chacun de ces canvas a
été sauvegardé sous forme de liste comme ressource interne de ce
package.

L’algorithme va ensuite détecter dans le code source de chaque canvas
les balises relatives aux rectangles (`<rect[...]/>`), y chercher
l’identifiant de l’objet et puis la ligne relative au remplissage de
l’objet (`style="fill:#ffffff"`) avant de remplacer ce code
hexadécimal par la couleur correspondant à l’évaluation proposée par
l’IDEA à l’indicateur concerné.

Pour information, une balise de rectangle au format svg s’écrit comme
suit :

    <rect
           style="fill:#ffffff;fill-opacity:1;stroke:#001800;stroke-width:0.14778921"
           id="rect2-16"
           width="17.833706"
           height="4.9643545"
           x="370.30774"
           y="143.9761"
           inkscape:label="#rect2-16" />

Ce code source “modifié” est stocké dans la liste renvoyée par ce
module. Celui-ci sera ensuite transformé en image réelle (png/pdf) par
le dernier module d’export.

Les packages ici utilisés sont principalement `{stringr}` pour la
manipulation des chaînes de caractères puis `{dplyr}` pour la
manipulation des tableaux de manière générale. Enfin, `{purrr}` est ici
aussi utilisé dans le cas du traitement simultané de plusieurs fichiers
de données.

## `dimensionsPlots()`

De la même manière que le module de production des cartes heuristiques,
ce module s’appuie sur les données importées par le premier module. En
revanche, celui-ci est très majoritairement basé sur la librairie
`{ggplot2}` pour la production de 5 graphiques relatifs à l’approche par
les dimensions :

  - Vision globale des 3 dimensions
  - Vision des 13 composantes
  - 3 graphiques de vision des indicateurs de chaque dimension,
    regroupés par composante.

Ce module renvoie une liste de ggplots, qui pourront être exportés en
fichier concret (png/pdf) par le module d’export.

Ici encore, `{purrr}` est utilisé dans le cas du traitement simultané de
plusieurs fichiers de données. `{dplyr}` et `{tidyr}` pour le
remaniement des données.

## `radarPlots()`

> Derrière l’intitulé de “radar”, ce ne sont en fait pas des diagrammes
> radar qui sont réalisés, mais en réalité des diagrammes en barres
> ayant subi une rotation polaire. En effet, les radars sont de moins en
> moins utilisés car il est admis que leur interprétation (notamment en
> termes d’aire) peut totalement varier selon le positionnement des
> variables et créer des différences d’interprétation trompeuses selon
> la façon dont chaque utilisateur construit son radar.

Ce module est très similaire au module précédent car lui aussi basé sur
`{ggplot2}`.

Cette représentation graphique circulaire n’étant pas compatible avec un
affichage du libellé complet des indicateurs, une table a été adjointe à
chaque graphique afin de traduire chaque code indicateur en libellé
complet. Les couleurs des lignes ce ces tables ont été
programmatiquement colorées pour correspondre à la dimension
d’appartenance de l’indicateur.

Pour ces graphiques, les scores de chaque indicateur sont standardisés
en % de la note maximale afin d’être représentés sur une échelle
commune. Par soucis de visibilité, il a également été décidé de ne pas
afficher les pourcentages inférieurs à 5%.

Ce module renvoie une liste de ggplots, qui pourront être exportés en
fichier concret (png/pdf) par le module d’export.

Le package `{purrr}` a de nouveau été utilisé dans le cas du traitement
simultané de plusieurs fichiers de données. `{dplyr}` et `{tidyr}` pour
le remaniement des données.

## `metaIDEA()`

Ce module est le plus récemment développé et est également basé sur
`{ggplot2}`. Il est amené à fortement évoluer.

Ce module est dédié à l’analyse de collectifs et n’est donc utilisable
que dans une situation d’import de plusieurs fichiers en simultané. Par
soucis de cohérence, le nombre minimal de fichiers a été fixé à 3.

Deux matrices (ou heatmap) sont proposés pour l’approche par les
propriétés, ainsi qu’un diagramme en barres empilées pour l’approche
par les dimensions.

Ce module est encore en version provisoire, et sera développé plus en
profondeur au fil des réflexions des concepteurs de la méthode sur les
analyses de groupe à proposer.

Ce module renvoie une liste de ggplots, qui pourront être exportés en
fichier concret (png/pdf) par le module d’export.

Une fois encore, `{tidyr}` et `{dplyr}` ont été utilisés pour la
manipulation des données.

## `exportIDEA()`

Ce dernier module permet d’exporter les résultats issus des 4 précédents
modules sous forme de fichiers graphiques (svg, png ou pdf).

Le module d’export va lancer différents algorithmes en fonction du type
de résultats qui lui sont injectés :

  - S’il s’agit d’une liste de ggplots (produits par
    `dimensionsPlots()`, `radarPlots()` ou `metaIDEA()`), alors les
    différents graphiques vont être exportés dans le dossier défini par
    l’utilisateur dans l’argument `outdir`. Par défaut, si aucune
    indication n’est donnée par l’utilisateur, ce dossier de sortie
    prendra la forme “RES\_{date}/”. A noter que les résultats seront
    ensuite exportés dans un sous-dossier correspondant à l’identifiant
    de l’exploitation (ce qui permet de séparer les résultats dans le
    cas d’un traitement multiple).

  - S’il s’agit de code source svg produit par `MakeTrees()`, alors les
    graphiques sont exportés par des fonctions issues du package
    `{rsvg}`, à savoir `rsvg::rsvg_pdf` et `rsvg::rsvg_png`.

Les dimensions d’export de chaque graphique ont été choisies et
implémentées dans une liste interne au package. L’export est ensuite
réalisé par l’application des fonctions d’export (`ggplot2::ggsave()`
ou `rsvg`) combinées à l’approche itérative de `{purrr}` pour les
traitements simultanés.

# Programmation complète d’un ou plusieurs diagnostic

Voici un exemple concret de diagnostic complet à partir d’un fichier de
données JSON.

``` r

library(IDEATools)

chemin <- file.path("example_json.json", package = "IDEATools") 

IDEAdata <- importIDEA("chemin") 

dimensionsPlots(IDEAdata) %>% exportIDEA()

MakeTrees(IDEAdata) %>% exportIDEA()

radarPlots(IDEAdata) %>% exportIDEA()
```

# Application web interactive

Afin de simplifier l’utilisation de ce package qui, en l’état, réservé
aux utilisateurs du langage R, un applicatif de calcul basé sur la
librairie `{shiny}` a été également développé et intégré dans ce
package. Celui-ci peut être exécuté d’une simple commande qui va ouvrir
une seconde page Rstudio, qui peut être ensuite ouverte dans votre
navigateur web préféré.

Shiny est un package R permettant de produire des applications
interactives pour le web à partir de code R. Pour faire simple, le
package va traduire le code R en mélange de code HTML/CSS/Javascript,
les langages du web et de l’interactivité. Comme tout produit R, il est
donc entièrement personnalisable à condition de disposer de certaines
compétences en HTML/CSS/JS. Son principal avantage est donc de mêler la
puissance calculatoire et statistique de R avec des format de diffusion
et communication bien plus attrayant pour un public non
chercheur/ingénieur.

L’application peut être lancée par cette simple commande :

``` r

runIDEATool()
```

Après avoir importé un calculateur via le bouton d’import en haut à
gauche, l’application devrait après un bref temps de chargement
ressembler à la capture d’écran ci-dessus :

![](www/app1.png)

<br> <br>

Il est alors possible d’explorer la synthèse des résultats (première
page), puis de rentrer dans le détail de chacun de ces résultats par un
clic sur les résultats. L’ensemble des résultats détaillés est
accessible via les différents onglets sur la gauche.

![](www/app2.png)

> Note : chacun des graphiques dans l’application peut être manuellement
> récupéré par un clic droit \> Sauvegarder l’image sous. Néanmoins, il
> convient de privilégier l’approche ci-dessous permettant de
> télécharger l’ensemble des figures.

Une fois l’exploration des résultats terminés, ceux-ci peuvent
maintenant être exportés de deux façons :

  - Par le biais d’un rapport pré-édité (environ 16 pages) qui sera
    automatiquement mis à jour avec les données relatives au calculateur
    importé. Celui-ci présente d’abord les métadonnées générales, puis
    affiche successivement l’ensemble des résultats relatifs aux
    propriétés et aux dimensions :

![](www/app3.png)

La technologie utilisée est `{rmarkdown}` qui fait appel à un moteur
`LaTeX` pour produire le rapport, ce qui peut expliquer un temps de
rendu assez long selon l’ordinateur utilisé (jusqu’à 30 secondes).

  - Par le téléchargement d’une archive .zip contenant l’ensemble des
    figures présentes dans le rapport. Afin d’éviter tout conflit de
    fichiers, les figures sont à nouveau produites avant d’être
    encapsulées et téléchargées, ce qui demande quelques secondes de
    calcul côté serveur. Ce délai reste néanmoins plus court que pour la
    production du rapport automatisé.

Cette application n’est actuellement disponible que pour un utilisateur
du package R. Elle n’est pas encore disponible en ligne car elle
nécessite de disposer d’un serveur pour l’héberger. Les solutions
d’hébergement sont actuellement en cours de prospection.

# A propos

Cet applicatif est un programme libre; vous pouvez le redistribuer ou le
modifier suivant les termes de la GNU General Public License (GPL) telle
que publiée par la Free Software Foundation; soit la version 3 de la
licence, soit (a votre gré) toute version ultérieure. Ce travail est
diffusé dans l’espoir qu’il sera utile, mais sans aucune garantie de
qualité marchande ou d’adéquation à un but particulier.

En cas de problèmes rencontrés avec l’outil, contacter :

  - David Carayon : <david.carayon@inrae.fr>

ou

  - Sydney Girard : <sydney.girard@inrae.fr>
  - Frédéric Zahm : <frederic.zahm@inrae.fr>

# Références

Carayon, D., Girard, S., Zahm, F. (2020). IDEATools: Un applicatif pour
le calcul, l’automatisation et l’exploitation de données IDEA4. R
package version 1.0.

Zahm F., Alonso Ugaglia A., Boureau H., Del’homme B., Barbier J.M.,
Gasselin P., Gafsi M., Girard S., Guichard L., Loyce C., Manneville V.,
Menet A., Redlingshofer B., 2019, Évaluer la durabilité des
exploitations agricoles. La méthode IDEA v4, un cadre conceptuel
mobilisant dimensions et propriétés de la durabilité, Cahiers
Agricultures, 28, 5, <https://doi.org/10.1051/cagri/2019004>.

Zahm et al. (2019). “Évaluer la durabilité des exploitations agricoles.
La méthode IDEA v4, un cadre conceptuel combinant dimensions et
propriétés de la durabilité” in Cahiers Agricultures, 8(5):1-10.
