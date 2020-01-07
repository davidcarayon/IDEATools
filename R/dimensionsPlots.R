#' Plot visual outputs linked to the "Dimension" approach for IDEA4
#'
#' @param IDEAdata output of the `importIDEA()` function
#' @param output_dir If a directory path is given, each plot will be exported as a png file in this directory. If the directory doesn't exist yet, it will be created.
#'
#' @return a named list of plots
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' path <- system.file("example.xls", package = "RIDEATools")
#' dat <- importIDEA(path, anonymous = FALSE)
#' plots <- dimensionsPlots(dat, output_dir = NULL)
dimensionsPlots <- function(IDEAdata){

  singleplots <- function(res_dim){

    splotlist <- list()

    dim_dimensions <- res_dim %>%
      dplyr::group_by(dimension) %>%
      dplyr::summarise(score_dim = unique(score_dim), max_dim = 100) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(dplyr::desc(dimension))

    critiq <- min(res_dim$score_dim)

    splotlist$dimensions <- ggplot2::ggplot(dim_dimensions,ggplot2::aes(x = dimension, y = score_dim, group = factor(dimension))) +
      ggplot2::geom_bar(ggplot2::aes(x = dimension, y = max_dim,fill = dimension), alpha = 0.3,color = "black", position = ggplot2::position_dodge(width = 0.8),stat = "identity")+
      ggplot2::geom_bar(ggplot2::aes(fill = dimension), color = "black", position = ggplot2::position_dodge(width = 0.8),stat = "identity")+
      ggplot2::geom_hline(yintercept = critiq, color = "red", size = 1.5, linetype = 5)+
      ggplot2::scale_fill_manual(values = c("#2e9c15","#5077FE","#FE962B")) +
      ggplot2::geom_label(ggplot2::aes(label = paste0(score_dim,"/",max_dim)), fill = "white", size = 5) +
      ggplot2::theme(axis.line = ggplot2::element_blank())+
      ggplot2::theme_bw()+
      ggplot2::theme(panel.grid.major = ggplot2::element_line(color = "grey75")) +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     legend.text = ggplot2::element_text(size = 13),
                     legend.title = ggplot2::element_text(size = 15),
                     strip.text = ggplot2::element_text(size = 9, face = "bold")) +
      ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white", color = "black")) +
      ggplot2::theme(axis.text = ggplot2::element_text(size = 15, color = "black"), axis.title = ggplot2::element_text(size = 15, face = "bold"))+
      ggplot2::labs(fill = "Dimension", y = "Valeur de la dimension / valeur max") +
      ggplot2::theme(plot.caption = ggplot2::element_text(face = "bold"))+
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::guides(fill = FALSE)



    dim_compo <- res_dim %>%
      dplyr::mutate(dim = sub("^([[:alpha:]]*).*", "\\1", indicateur)) %>%
      dplyr::mutate(composante = ifelse(composante == "Assurer des conditions favorables à la production à moyen et long terme",
                                        yes = "Assurer des conditions favorables à la production\n à moyen et long terme", no = composante)) %>%
      dplyr::mutate(composante = ifelse(composante == "Bouclage de flux \nde matières et d'énergie \npar une recherche d'autonomie",
                                        yes = "Bouclage de flux de matières et d'énergie \npar une recherche d'autonomie", no = composante)) %>%
      dplyr::group_by(composante) %>%
      dplyr::summarise(dim = unique(dim),dimension = unique(dimension),valeur_compo = unique(valeur_compo), max_compo = unique(max_compo)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(dplyr::desc(dim)) %>%
      dplyr::mutate(composante = factor(composante, levels = unique(composante)))

    splotlist$composantes <- ggplot2::ggplot(dim_compo,ggplot2::aes(x = composante, y = valeur_compo, group = factor(dimension))) +
      ggplot2::geom_bar(ggplot2::aes(x = composante, y = max_compo,fill = dimension), alpha = 0.3,color = "black", position = ggplot2::position_dodge(width = 0.8),stat = "identity")+
      ggplot2::geom_bar(ggplot2::aes(fill = dimension), color = "black", position = ggplot2::position_dodge(width = 0.8),stat = "identity")+
      ggplot2::geom_label(ggplot2::aes(label = paste0(valeur_compo,"/",max_compo)), fill = "white", size = 5.5)+
      ggplot2::scale_fill_manual(values = c("#2e9c15","#5077FE","#FE962B")) +
      ggplot2::theme(axis.line = ggplot2::element_blank())+
      ggplot2::theme_bw()+
      ggplot2::theme(panel.grid.major = ggplot2::element_line(color = "grey75")) +
      ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                     legend.text = ggplot2::element_text(size = 15),
                     legend.title = ggplot2::element_text(size = 17),
                     strip.text = ggplot2::element_text(size = 9, face = "bold")) +
      ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white", color = "black")) +
      ggplot2::theme(axis.text = ggplot2::element_text(size = 17, color = "black"), axis.title = ggplot2::element_text(size = 17, face = "bold"))+
      ggplot2::labs(fill = "Dimension", y = "Valeur de la composante / valeur max") +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::coord_flip()

    dim_indic <- res_dim %>%
      dplyr::inner_join(label_nodes, by = c("indicateur"="code_indicateur", "composante", "dimension")) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(composante = ifelse(composante == "Assurer des conditions favorables à la production à moyen et long terme",
                                        yes = "Assurer des conditions favorables à la production\n à moyen et long terme", no = composante)) %>%
      dplyr::mutate(composante = ifelse(composante == "Bouclage de flux \nde matières et d'énergie \npar une recherche d'autonomie",
                                        yes = "Bouclage de flux de matières et d'énergie \npar une recherche d'autonomie", no = composante)) %>%
      dplyr::mutate(composante = ifelse(composante == "Réduire les impacts sur la santé humaine et les écosystèmes",
                                        yes = "Réduire les impacts sur la santé humaine\n et les écosystèmes", no = composante)) %>%
      dplyr::mutate(nom_indicateur = wrapit(nom_complet)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(indicateur %in% liste_indicateurs)  %>%
      dplyr::mutate(num_indic = readr::parse_number(indicateur)) %>%
      dplyr::arrange(dplyr::desc(dimension),dplyr::desc(num_indic)) %>%
      dplyr::mutate(indicateur = factor(indicateur, levels = unique(indicateur))) %>%
      dplyr::mutate(nom_indicateur = factor(nom_indicateur, levels = unique(nom_indicateur))) %>%
      dplyr::mutate(composante = paste0("Composante : ",composante)) %>%
      dplyr::mutate(dimension2 = dimension) %>%
      dplyr::group_by(dimension2) %>%
      tidyr::nest()


    indic_plot <- function(df){
      ggplot2::ggplot(df, ggplot2::aes(x = nom_indicateur, y = valeur, fill = dimension)) +
        ggplot2::geom_bar(ggplot2::aes(x = nom_indicateur, y = valeur_max,fill = dimension), alpha = 0.3,color = "black", position = ggplot2::position_dodge(width = 0.8),stat = "identity")+
        ggplot2::geom_bar(ggplot2::aes(fill = dimension), color = "black", position = ggplot2::position_dodge(width = 0.8),stat = "identity")+
        ggplot2::facet_wrap(~composante, ncol = 1, scales = "free_y")+
        ggplot2::scale_fill_manual(values = c("Agroécologique" = "#2e9c15", "Socio-Territoriale" = "#5077FE", "Economique" = "#FE962B"))+
        ggplot2::geom_label(ggplot2::aes(label = paste0(valeur,"/",valeur_max)), fill = "white", size = 5)+
        ggplot2::theme(axis.line = ggplot2::element_blank())+
        ggplot2::theme_bw()+
        ggplot2::theme(panel.grid.major = ggplot2::element_line(color = "grey75")) +
        ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                       legend.text = ggplot2::element_text(size = 13),
                       legend.title = ggplot2::element_text(size = 15),
                       strip.text = ggplot2::element_text(size = 12, face = "bold")) +
        ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white", color = "black", size = 2)) +
        ggplot2::theme(axis.text = ggplot2::element_text(size = 12, color = "black"), axis.title = ggplot2::element_text(size = 15, face = "bold"))+
        ggplot2::guides(fill = FALSE) +
        ggplot2::labs(fill = "Dimension", y = "Valeur de l'indicateur / valeur max") +
        ggplot2::theme(legend.position = "top") +
        ggplot2::coord_flip()
    }

    list_indicators <- purrr::map(dim_indic$data, indic_plot)
    names(list_indicators) <- paste0("indic_",unique(dim_indic$dimension2))



    indicateurs_ancrage <- c("B10","B3","B9","B8","B7","B6","B15","B14","B19")
    indicateurs_autonomie <- c("B13","B15","B18","B8","C5","C3","C6","A7","A8","A6")
    indicateurs_robustesse <- c("A1","A3","A4","A14","C5","C4","C7","A2","C8","C9","A15","B22","B13","B15","B18","B16")
    indicateurs_responsabilite <- c("B20","B5","B19","B11","B1","B2","B4","A10","A9","A11","C11","B17","B14","B16","B21","B23","A5","A16","A17","A18","A19","B12")
    indicateurs_capacite <- c("A5","A12","A13","B14","B15","B16","B13","B18","B1","B3","C1","C2","C3","C10")

    prop_radar <- res_dim %>%
      dplyr::mutate(Propriete = dplyr::case_when(indicateur %in% indicateurs_ancrage ~ "Ancrage Territorial",
                                                 indicateur %in% indicateurs_autonomie ~ "Autonomie",
                                                 indicateur %in% indicateurs_robustesse ~ "Robustesse",
                                                 indicateur %in% indicateurs_responsabilite ~ "Responsabilité globale",
                                                 indicateur %in% indicateurs_capacite ~ "Capacité productive et reproductive \nde biens et de services"))  %>%
      dplyr::mutate(dimension = stringr::str_remove_all(dimension,"Durabilité ")) %>%
      dplyr::mutate(num_indic = readr::parse_number(indicateur)) %>%
      dplyr::arrange(dplyr::desc(dimension),num_indic) %>%
      dplyr::mutate(indicateur = factor(indicateur, levels = unique(indicateur)))

    plist <- list()

    for (i in unique(prop_radar$Propriete)) {

      # Get the name and the y position of each label
      label_data <- prop_radar %>% dplyr::filter(Propriete == i)
      label_data$id <- seq(1, nrow(label_data))
      number_of_bar <- nrow(label_data)
      angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
      label_data$hjust <- ifelse( angle < -90, 1, 0)
      label_data$angle <- ifelse(angle < -90, angle+180, angle)
      label_data = label_data %>% dplyr::filter(score_ind > 5)

      plist[[i]] <- ggplot2::ggplot(prop_radar %>% dplyr::filter(Propriete == i), ggplot2::aes(x = indicateur, y = score_ind, fill = dimension)) +
        ggplot2::geom_rect(xmin = -Inf, ymin = -20, xmax = Inf, ymax = 100, fill = "white", color = "white")+
        ggplot2::geom_col(ggplot2::aes(x = indicateur, y = 100, fill = dimension),alpha = 0.3, color = "black")+
        ggplot2::geom_col() +
        ggplot2::theme_bw()+
        ggplot2::geom_text(data=label_data, ggplot2::aes(x=id, y=score_ind+1, label=paste0(round(score_ind),"%"), hjust=hjust), color="black", fontface="bold",alpha=1, size=4.2, angle= label_data$angle, inherit.aes = FALSE) +
        # ggplot2::geom_hline(yintercept = c(100), color = "black", size = 1.5, linetype = 1)+
        ggplot2::scale_fill_manual(limits = c("Agroécologique","Socio-Territoriale","Economique"),values = c("Agroécologique" = "#2e9c15", "Socio-Territoriale" = "#5077FE", "Economique" = "#FE962B")) +
        ggplot2::theme(
          axis.title = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(size = 14,hjust = 0.5, face = "bold"),
          legend.text = ggplot2::element_text(size = 13),
          legend.title = ggplot2::element_text(size = 15),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank()

        ) +
        ggplot2::scale_y_continuous(limits = c(-20,130),breaks = c(0,20,40,60,80,100))+
        ggplot2::theme(axis.text = ggplot2::element_text(size = 13, color = "black", face = "bold"))  +
        ggplot2::labs(fill = "Dimension", title = glue::glue('Indicateurs de la propriété "{i}"'), caption = "NB : Les scores inférieurs à 5% ne sont pas indiqués")+
        ggplot2::theme(legend.position = "top")+
        ggplot2::coord_polar()
    }





    return(c(splotlist,list_indicators,plist))

      }

  if(IDEAdata$analysis.type == "single") {
    res_dim <- IDEAdata$dataset %>%  dplyr::mutate(dimension = stringr::str_remove_all(dimension,"Durabilité "))
    label_nodes <- label_nodes %>% dplyr::mutate(dimension = stringr::str_remove_all(dimension,"Durabilité "))
    return_plot <- list()
    nom <- unique(res_dim$nom_exploit)
    return_plot[[nom]] <- singleplots(res_dim)

  }

  if(IDEAdata$analysis.type == "group") {
    res_dim <- IDEAdata$dataset %>%  dplyr::mutate(dimension = stringr::str_remove_all(dimension,"Durabilité ")) %>% dplyr::group_by(nom_exploit) %>% tidyr::nest()
    label_nodes <- label_nodes %>% dplyr::mutate(dimension = stringr::str_remove_all(dimension,"Durabilité "))

    return_plot <- purrr::map(res_dim$data, singleplots)
    names(return_plot) <- res_dim$nom_exploit


  }

return_plot$analysis.type = IDEAdata$analysis.type
return_plot$plot.type <- "dim"
return(return_plot)


}



