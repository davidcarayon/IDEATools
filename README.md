
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IDEATools <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:experimental](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![packageversion](https://img.shields.io/badge/Package%20version-3.1.1-orange.svg?style=flat-square)](commits/master)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![R build
status](https://github.com/davidcarayon/IDEATools/workflows/R-CMD-check/badge.svg)](https://github.com/davidcarayon/IDEATools/actions)
<!-- badges: end -->

IDEATools est un package R dédié à la méthode IDEA4, visant à fournir
aux utilisateurs des outils pour le traitement, l’automatisation et le
reporting de diagnostics IDEA.

# Installation & Prérequis

En attendant sa publication officielle sur le CRAN, vous pouvez
télécharger et utiliser la version en cours de développement depuis
GitHub avec :

``` r
install.packages("remotes")
remotes::install_github("davidcarayon/IDEATools")
```

*NB : Le logiciel RTools est parfois nécessaire sur les machines Windows
pour pouvoir installer le package `{remotes}`, puisque l’installation
ici se fait depuis un dépôt de développement (Github) et non un dépôt
officiel R. Vous pouvez l’installer ici :
[Rtools](https://cran.r-project.org/bin/windows/Rtools/)*

Une fois installé, vous pouvez charger le package avec :

``` r
library(IDEATools)
```

## Prérequis pour la production de rapports

Pour la production de rapport PDF, une installation de LaTeX est
requise. Si vous n’avez jamais utilisé LaTeX, vous pouvez utiliser la
fonction `tinytex::install_tinytex()` pour installer une version
minimale de LaTeX vous permettant d’éditer des rapports au format PDF en
utilisant le package IDEATools. Une fois installé, vous n’avez plus
besoin de vous soucier de LaTeX (opération à réaliser seulement lors de
la première utilisation).

# Utilisation

Au total, 5 fonctions ou “modules” ont été développés dans ce package,
allant de l’import des données d’un calculateur à la production de
graphiques puis à la productions de produits de reporting (PDF, Excel,
etc.) :

-   `read_idea()` : Permet d’identifier la validité du fichier d’entrée
    et d’en extraire métadonnées et items.
-   `compute_idea()` : Calcule les
    indicateurs/composantes/dimensions/propriétés à partir des items
-   `old_idea()` : Alternative aux deux fonctions précédentes si le
    calculateur est trop ancien (vise les indicateurs plutôt que les
    items)
-   `plot_idea()` : Produit les graphiques dimensions / propriétés
-   `write_idea()` : Export des graphiques sous forme brute ou sous
    forme de rapports aux formats variés.

Afin de simplifier l’utilisation du package, une fonction globale
`diag_idea()` a été développée. Grâce à cette fonction, selon la saisie
de l’utilisateur, les modules d’IDEATools vont être appelés
séquentiellement afin de produire les résultats demandés. L’utilisateur
peut notamment paramétrer :

-   Le fichier/dossier d’entrée des données `input`
-   Le dossier de sortie des résultats `output_directory`
-   Le type d’analyse (individuelle ou de groupe) `type`
-   Le type de sorties (rapport et/ou graphiques bruts) `export_type`
-   Le types de graphiques qu’il souhaite (dans le cas d’un export brut)
    `plot_choices`
-   Le format de sortie du rapport si désiré (au choix : pdf, docx, odt,
    pptx, xlsx) `report_format`
-   Le préfixe à rajouter aux fichiers de sortie (ex : le nom de la
    ferme) dans le cas d’une analyse individuelle `prefix`
-   La résolution de sortie des graphiques (impacte notamment le poids
    des sorties) `dpi`
-   Si l’algorithme doit afficher sa progression dans la console.
    `quiet`
-   (*nouveau*) Dans le cas particulier ou un calculateur au format
    .xlsx est inséré et qu’un rapport individuel au format xlsx est
    demandé, `append` paramétré en TRUE permet de coller les onglets de
    résultats à la suite des onglets du calculateur initial, créant
    ainsi un calculateur “tout en un” avec données + résultats.

Voici un appel complet à la fonction `diag_idea()` avec toutes les
possibilités de paramétrage :

``` r
diag_idea(input,
          output_directory,
          type = c("single","group"),
          export_type = c("report","local",NULL),
          plot_choices = c("dimensions","trees","radars"),
          report_format = c("pdf","docx","odt","xlsx","pptx"),
          prefix = "EA",
          dpi = 300,
          quiet = FALSE,
          append = FALSE)
```

*Pour information, les utilisateurs les moins habitués à l’écosystème R
peuvent utiliser les commandes suivantes (à condition d’utiliser
RStudio) pour sélectionner les dossier/fichiers via une fenêtre en
presse-bouton:*

``` r
input <- rstudioapi::selectDirectory() # Dans le cas d'un répertoire 
# OU
input<- rstudioapi::selectFile() # Si un seul calculateur

output_directory <- rstudioapi::selectDirectory()
```

On distingue 3 grands types de diagnostics :

## Les analyses individuelles

En premier lieu, l’utilisateur peut avoir besoin d’un diagnostic pour
une seule ferme. Prennons ici l’exemple d’utilisateur qui souhaite
récupérer ses résultats pour sa ferme, mais uniquement ses arbres
éclairés. Le code sera alors :

``` r
diag_idea(input = "chemin_calculateur",
          output_directory = "mes_résultats",
          type = "single",
          export_type = "local"
          prefix = "MaFerme",
          plot_choices = "trees"
          quiet = FALSE)
```

## Les analyses multi-individuelles

Ensuite, certains utilisateurs ont besoin de traiter plusieurs
calculateurs en même temps.

Ici par exemple, l’utilisateur n’a pas besoin des figures “brutes”, mais
a juste besoin pour chaque exploitation d’un rapport au format word
qu’il pourra commenter ainsi qu’une présentation powerpoint contenant
toutes les figures et prête à projeter. Le code sera alors :

``` r
diag_idea(input = "chemin_vers_dossier",
          output_directory = "mes_résultats",
          type = "single",
          export_type = "report"
          report_format = c("docx","pptx")
          quiet = FALSE)
```

## Les analyses de groupe

Enfin, certains utilisateurs souhaitent traiter un ensemble de
calculateurs en même temps et ont besoin d’avoir une vision globale sur
le groupe.

Dans cet exemple, l’utilisateur va donc demander à la fois des
graphiques bruts, mais aussi des rapports prêts à être imprimés (PDF)
ainsi qu’un support excel qu’il pourra re-traiter à sa guise pour son
analyse de group. Le code sera alors :

``` r
diag_idea(input = "chemin_vers_dossier",
          output_directory = "mes_résultats",
          type = "group",
          export_type = c("report","local")
          report_format = c("pdf","xlsx")
          quiet = FALSE)
```

Notons qu’il peut demander, en plus de son analyse de groupe, des
rapports individuels qu’il pourra donner à chaque exploitation (par
exemple au format Libreoffice ODT) :

``` r
diag_idea(input = "chemin_vers_dossier",
          output_directory = "mes_résultats",
          type = c("group","single")
          export_type = c("report")
          report_format = c("odt")
          quiet = FALSE)
```

**Note : Une analyse de groupe nécessite un nombre d’exploitations au
moins égal à 3.**

# Contact

<div align="center">

   :e-mail:
[Email](mailto:david.carayon@inrae.fr)   \|   :speech_balloon:
[Twitter](https://twitter.com/david_carayon)   \|   :necktie:
[LinkedIn](https://www.linkedin.com/in/carayon-david/)

<!--
Quick Link
-->

</div>

# Références

Carayon, D., Girard, S., Zahm, F. (2020). IDEATools: Un applicatif pour
le calcul, l’automatisation et l’exploitation de données IDEA4. R
package version 2.0.

Zahm F., Alonso Ugaglia A., Boureau H., Del’homme B., Barbier J.M.,
Gasselin P., Gafsi M., Girard S., Guichard L., Loyce C., Manneville V.,
Menet A., Redlingshofer B., 2019, Évaluer la durabilité des
exploitations agricoles. La méthode IDEA v4, un cadre conceptuel
mobilisant dimensions et propriétés de la durabilité, Cahiers
Agricultures, 28, 5, <https://doi.org/10.1051/cagri/2019004>.

Zahm et al. (2019). “Évaluer la durabilité des exploitations agricoles.
La méthode IDEA v4, un cadre conceptuel combinant dimensions et
propriétés de la durabilité” in Cahiers Agricultures, 8(5):1-10.
