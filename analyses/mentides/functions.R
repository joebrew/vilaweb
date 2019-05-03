commafy <- function(x){
  scales::comma(x, big.mark = '.', decimal.mark = ',')
}
point_replace <- function(x){
  gsub('.', ',', x, fixed = TRUE)
}


# uk_education
uk_education <- function(ca = FALSE){
  if(!ca){
    the_labs <- labs(x = '',
                     y = 'Percent in favor of Brexit',
                     title = 'Education and support for Brexit',
                     caption = 'Data source: "Understanding the Leave Vote" by Kirby Swales.\nChart by Joe Brew. www.vilaweb.cat')
    education <- factor(c('Secondary school not completed',
                          'Secondary schoool completed',
                          'Higher education below degree',
                          'Degree'))
    education <- factor(education, 
                        levels = education,
                        labels = gsub(' ', '\n', education))
  } else {
    the_labs <- labs(x = '',
                     y = 'Percentage a favor del Brexit',
                     title = 'Nivell educatiu i suport al Brexit',
                     caption = 'Font de dades: "Understanding the Leave Vote" de Kirby Swales.\nGràfic de Joe Brew. www.vilaweb.cat')
    education <- factor(c('Escola secundaria no acabada',
                          'Escola secundaria acabada',
                          'Estudis universitaris no acabats',
                          'Grau universitari'))
    education <- factor(education, 
                        levels = education,
                        labels = gsub(' ', '\n', education))
  }
  df <- tibble(education,
               y = c(78,61, 50, 26))
  ggplot(data = df,
         aes(x = education,
             y = y)) +
    geom_bar(stat = 'identity',
             fill = as.character(colors_vilaweb()[5])) +
    theme_vilaweb() +
    the_labs +
    theme(axis.text.x = element_text(size = 13),
          plot.caption = element_text(size = 10)) +
    geom_text(aes(label = point_replace(round(y, digits = 1))),
              nudge_y = -5,
              alpha = 0.8,
              size = 5,
              color = 'white')
}

# cat_education
cat_education <- function(ca = FALSE){
  
  
  education_dict <- tibble(
    C500 = c("No sap llegir i escriure",
             "Sap llegir i escriure però va anar menys de 5 anys a l'escola",
             "Va anar a l'escola 5 o mès anys però sense completar ESO, EGB o Batxillerat elemental",
             "ESO completa (Graduat escolar), EGB, Batxiller elemental o Certificat d' Escolaritat o d'Estudis Primaris",
             "Batxillerat, BUP, COU, Batxillerat superior, PREU",
             "Cicle formatiu de grau mitjà, FP1, Oficialia industrial o equivalent",
             "Cicle formatiu de grau superior, FP2, Mestratge industrial o equivalent",
             "Diplomatura, Enginyeria/arquitectura tècnica o equivalent",
             "Llicenciatura, Arquitectura, Enginyeria, Grau o equivalent (4 anys o mès)",
             "Cursos d'especialització professional, Postgrau, Màster (MIR, FIR...)",
             "Doctorat",
             "Altres estudis",
             "95",
             "No ho sap",
             "No contesta"),
    ca = c("Menys de 5 anys\na l'escola",
           "Menys de 5 anys\na l'escola",
           "5 anys a l'escola\nfins a Batxillerat/\nBUP/COU/PREU",
           "5 anys a l'escola\nfins a Batxillerat/\nBUP/COU/PREU",
           "5 anys a l'escola\nfins a Batxillerat/\nBUP/COU/PREU",
           "Grau, FP1, FP2,\nOficialia, Mestratge",
           "Grau, FP1, FP2,\nOficialia, Mestratge",
           "Llicenciatura,\nDiplomatura,\nEnginyeria tècnica",
           "Llicenciatura,\nDiplomatura,\nEnginyeria tècnica",
           "Postgrau, Màster,\nMIR, Doctorat",
           "Postgrau, Màster,\nMIR, Doctorat",
           rep(NA, 4)),
    en = c("<5 years\nof schooling",
           "<5 years\nof schooling",
           "5+ years\nof school to\nhigh school",
           "5+ years\nof school to\nhigh school",
           "5+ years\nof school to\nhigh school",
           "Post high school,\ntrade, technical\ndegree",
           "Post high school,\ntrade, technical\ndegree",
           "College or\ntechnical\nengineer degree",
           "College or\ntechnical\nengineer degree",
           "Graduate studies,\nMaster, Doctor,\nDoctorate",
           "Graduate studies,\nMaster, Doctor,\nDoctorate",
           rep(NA, 4))
  )
  education_dict$ca <- factor(education_dict$ca,
                              levels = unique(education_dict$ca))
  education_dict$en <- factor(education_dict$en,
                              levels = unique(education_dict$en))
  df <- vilaweb::ceo
  df <- df %>%
    mutate(indy = P31) %>%
    mutate(indy = as.character(indy)) %>%
    mutate(indy = ifelse(indy %in% c('No ho sap',
                                     'No contesta'),
                         'NS/NC',
                         indy)) %>%
    left_join(education_dict) %>%
    filter(!is.na(indy))
  if(ca){
    df$education <- df$ca
    legend_title <- 'Independentista'
  } else {
    df$education <- df$en
    legend_title <- 'Pro-\nindependence'
  }
  df <- df %>% filter(!is.na(education))
  pd <- df %>% group_by(education, indy) %>%
    tally %>%
    group_by(education) %>%
    mutate(p = n / sum(n) * 100) %>%
    arrange(indy)

  pd$indy <- factor(pd$indy,
                    levels = c('No', 'NS/NC', 'Sí'))
  if(!ca){
    pd$indy <- factor(pd$indy,
                      levels = c('No', 'NS/NC', 'Sí'),
                      labels = c('No', 'Not sure/\nno answer', 'Yes'))
  }
  
  # cols <- RColorBrewer::brewer.pal(n = 9,
  #                                  name = 'YlGnBu')
  # cols2 <- RColorBrewer::brewer.pal(n = 9,
  #                                   name = 'BrBG')
  # cols <- c(cols2[2], 
  #           grey(0.7),
  #           # grey(0.6),
  #           cols[6])
  cols <- databrew::make_colors(n = 10, categorical = TRUE)
  cols <- cols[c(2,3, 6)]
  cols <- rev(cols)
  # cols <- rev(cols)
  cols[2] <- grey(0.2)
  
  if(ca){
    the_labs <- labs(x = '',
                     y = 'Percentage',
                     title = 'Nivell educatiu i independentisme',
                     caption = paste0(
                       'Font de dades: Combinació d\'enquestes del Baròmetre d\'Opinió Política, Centre d\'Estudios d\'Opinió.\nDades recollides 2015-2018. Mostreig: ', commafy(sum(pd$n)),  ' residents de Catalunya amb ciutadania espanyola.\nGràfic de Joe Brew. www.vilaweb.cat'))
  } else {
    the_labs <- labs(x = '',
                     y = 'Percentage',
                     title = 'Education and support for Catalan independence',
                     caption = paste0(
                       'Data source: Combination of surveys from the Baròmetre d\'Opinió Política, Centre d\'Estudios d\'Opinió.\nData collected 2015-2018. Sample: ', commafy(sum(pd$n)),  ' residents of Catalonia with Spanish citizenship.\nChart: Joe Brew. www.vilaweb.cat'))
  }
  
  ggplot(data = pd,
         aes(x = education,
             y = p,
             fill = indy)) +
    geom_bar(stat = 'identity',
             position = position_stack()) +
    scale_fill_manual(name = legend_title,
                      values = cols) +
    theme_vilaweb() +
    theme(legend.position = 'right',
          legend.title = element_text(size = 7)) +
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust = 0.5,
                                     size = 12),
          plot.caption  = element_text(hjust = 0,
                                       size = 9)) +
    geom_text(aes(label = point_replace(round(p, digits = 1)),
                  y = p),
              position = position_stack(vjust = 0.5),
              color = 'white',
              alpha = 0.7) +
    the_labs
}


# uk_immigration
uk_immigration <- function(ca = FALSE){
  if(!ca){
    the_labs <- labs(x = '',
                     y = 'Percent in favor of Brexit',
                     title = 'Lowering immigration was a main issue in the Brexit vote',                     
                     caption = 'Data source: "Understanding the Leave Vote" by Kirby Swales.\nChart by Joe Brew. www.vilaweb.cat')
    immigration <- factor(c('EU membership\nundermines\nBritain\'s\nindependence',
                          'Leaving the\nEU would\nmake\nimmigration\nlower',
                          'Leaving the\nEU would\nmake Britain\'s\neconomy\nworse'))
    immigration <- factor(immigration, 
                        levels = immigration)
  } else {
    the_labs <- labs(x = '',
                     y = 'Percentage a favor del Brexit',
                     title = 'Reduir la immigració: una des les motivacions principals del Brexit',
                     caption = 'Font de dades: "Understanding the Leave Vote" de Kirby Swales.\nGràfic de Joe Brew. www.vilaweb.cat')
    immigration <- factor(c('Adhesió a la\nUE soscava la\nindependència de\nBretanya',
                            'Marxar de la\nUE reduiria\nla immigració',
                            'Marxar de la\nUE empitjoraria la\neconomia de\nBretanya'))
    immigration <- factor(immigration, 
                          levels = immigration)
  }
  df <- tibble(immigration,
               y = c(66,66,43))
  ggplot(data = df,
         aes(x = immigration,
             y = y)) +
    geom_bar(stat = 'identity',
             fill = as.character(colors_vilaweb()[5])) +
    theme_vilaweb() +
    the_labs +
    theme(axis.text.x = element_text(size = 10),
          plot.caption = element_text(size = 10),
          plot.title = element_text(size = 16)) +
    geom_text(aes(label = point_replace(round(y, digits = 1))),
              nudge_y = -5,
              alpha = 0.8,
              size = 5,
              color = 'white') 
}

cat_immigration <- function(ca = FALSE){
  pd <- vilaweb::ceo %>%
    mutate(indy = P31) %>%
    mutate(indy = as.character(indy)) %>%
    mutate(indy = ifelse(indy %in% c('No ho sap',
                                     'No contesta'),
                         'NS/NC',
                         indy)) %>%
    filter(!is.na(indy)) %>%
    filter(indy != 'NS/NC') %>%
    mutate(economy = P56I) %>%
    mutate(economy = as.character(economy)) %>%
    mutate(economy = ifelse(economy %in% c('No ho sap',
                                           'No contesta'),
                            'NS/NC',
                            economy)) %>%
    filter(!is.na(economy)) %>%
    filter(economy != 'NS/NC') %>%
    mutate(economy = 
             ifelse(economy %in% c("Molt d'acord",
                                   "D'acord"),
                    "D'acord o\nmolt d'acord",
                    ifelse(economy %in% c("En desacord",
                                          "Molt en desacord"),
                           "En desacord o\nmolt en desacord",
                           "Ni d'acord ni\nen desacord"))) %>% 
    group_by(economy, indy) %>%
    summarise(n = sum(PONDERA)) %>%
    ungroup %>%
    group_by(indy) %>%
    mutate(p = n / sum(n) * 100) %>%
    mutate(`Muestra` = n)
  
  pd$economy<- factor(pd$economy,
                      levels = c("D'acord o\nmolt d'acord",
                                 "Ni d'acord ni\nen desacord",
                                 "En desacord o\nmolt en desacord"))
  if(!ca){
    pd$economy<- factor(pd$economy,
                        levels = c("D'acord o\nmolt d'acord",
                                   "Ni d'acord ni\nen desacord",
                                   "En desacord o\nmolt en desacord"),
                        labels = c('Agree or\nstrongly agree',
                        'Neither agree\nnor disagree',
                        'Disagree or\nstrongly disagree'))
    pd$indy <- ifelse(pd$indy == 'No',
                      'Pro-Union Catalans',
                      'Pro-Independence Catalans')
    the_labs <- labs(x = '',
                     y = 'Percentage',
                     subtitle = "Agreement with above phrase",
                     title =  "'With so much immigration,\none no longer feels at home'",
                     caption = 'Sample: 3.143 residents of Catalonia with Spanish citizenship.\nCombination of BOP/CEO surveys, 2015 and 2018. Joe Brew | www.vilaweb.cat.')
  } else {
    pd$indy <- ifelse(pd$indy == 'No',
                      'Unionistes',
                      'Independentistes')
    the_labs <- labs(x = '',
                     y = 'Percentage',
                     subtitle = "Grau d'acord amb l'afirmació",
                     title =  "'Amb tanta immigració,\nun ja no se sent com a casa'",
                     caption = 'Mostra: 3.143 residents de Catalunya amb ciutadania espanyola.\nCombinació enquestes CEO. 2015 i 2018. Preguntes P56I i P31.\nJoe Brew | www.vilaweb.cat.')
  }
  n_cols <- length(unique(pd$economy))
  cols <- databrew::make_colors(n = n_cols, categorical = FALSE)
  # cols <- rev(cols)
  cols[2] <- 'darkgrey'

  ggplot(data = pd,
         aes(x = economy,
             y = p)) +
    geom_bar(stat = 'identity',
             position = position_dodge(width = 0.9),
             # color = 'black',
             alpha = 0.95,
             aes(fill = economy)) +
    facet_wrap(~indy) +
    theme_vilaweb() +
    # scale_fill_manual(name = '',
    #                   values = cols) +
    geom_text(aes(label = point_replace(round(p, digits = 2)),
                  y = p -5),
              alpha = 0.9,
              color = 'white',
              position = position_dodge(width = 0.9),
              size = 4) +
    scale_fill_manual(name = '',
                      values = as.character(vilaweb::colors_vilaweb()[c(1,5,3)])) +
    the_labs +
    theme(legend.position = 'none',
          plot.caption = element_text(size = 9,
                                      hjust = 0),
          axis.text.x = element_text(size = 5))
}

cat_immigration2 <- function(ca = FALSE){
  party_dict <- 
    tibble(P24 = c("PPC",
                   "CiU",
                   "ERC",
                   "PSC",
                   "ICV-EUiA",
                   "C's",
                   "Reagrupament.cat",
                   "SI",
                   "PxC",
                   "CUP",
                   "UPyD",
                   "Podemos",
                   "Barcelona en Comú",
                   "CDC",
                   "Junts pel Sí",
                   "Catalunya sí que es pot",
                   "Democràcia i Llibertat",
                   "En Comú Podem",
                   "PACMA",
                   "PDeCAT",
                   "Junts per Catalunya",
                   "Catalunya en Comú Podem",
                   "Altres partits",
                   "Cap",
                   "No ho sap",
                   "No contesta"),
           partit = c("PPC",
                      "PDCat/CiU/CDC/Junts",
                      "ERC",
                      "PSC",
                      "ICV-EUiA",
                      "C's",
                      "Reagrupament.cat",
                      "SI",
                      "PxC",
                      "CUP",
                      "UPyD",
                      "Podem(os)",
                      "Podem(os)",
                      "PDCat/CiU/CDC/Junts",
                      "PDCat/CiU/CDC/Junts",
                      "Podem(os)",
                      "Democràcia i Llibertat",
                      "Podem(os)",
                      "PACMA",
                      "PDCat/CiU/CDC/Junts",
                      "PDCat/CiU/CDC/Junts",
                      "Podem(os)",
                      "Altre/Cap/NS/NC",
                      "Altre/Cap/NS/NC",
                      "Altre/Cap/NS/NC",
                      "Altre/Cap/NS/NC"))
  
  spanish <- FALSE
  if(ca){
    catalan <- TRUE
    english <- FALSE
  } else {
    catalan <- FALSE
    english <- TRUE
  }
  
  pd <- vilaweb::ceo %>%
    left_join(party_dict) %>%
    group_by(partit) %>%
    mutate(size = n()) %>%
    filter(size >= 50) %>%
    ungroup %>%
    mutate(year = ANY) %>%
    mutate(axis = as.character(P25)) %>%
    mutate(axis = ifelse(axis == 'Extrema esquerra',
                         '1',
                         ifelse(axis == 'Extrema dreta',
                                '10',
                                as.character(axis)))) %>%
    mutate(axis = as.numeric(axis)) %>%
    filter(!partit %in% c('Altre/Cap/NS/NC',
                          'ICV-EUIA'))
  
  pd <- pd %>%
    mutate(indy = P31) %>%
    mutate(indy = as.character(indy)) %>%
    mutate(indy = ifelse(indy %in% c('No ho sap',
                                     'No contesta'),
                         'NS/NC',
                         indy)) %>%
    filter(!is.na(indy)) %>%
    filter(indy != 'NS/NC') %>%
    mutate(economy = P56I) %>%
    mutate(economy = as.character(economy)) %>%
    mutate(economy = ifelse(economy %in% c('No ho sap',
                                           'No contesta'),
                            'NS/NC',
                            economy)) %>%
    filter(!is.na(economy)) %>%
    filter(economy != 'NS/NC') %>%
    mutate(economy = 
             ifelse(economy %in% c("Molt d'acord",
                                   "D'acord"),
                    "D'acord o molt d'acord",
                    ifelse(economy %in% c("En desacord",
                                          "Molt en desacord"),
                           "En desacord o molt en desacord",
                           "Ni d'acord ni en desacord"))) %>% 
    group_by(economy, indy) %>%
    summarise(n = sum(PONDERA)) %>%
    ungroup %>%
    group_by(indy) %>%
    mutate(p = n / sum(n) * 100) %>%
    mutate(`Muestra` = n)
  
  original_levels <- c("D'acord o molt d'acord",
                       "Ni d'acord ni en desacord",
                       "En desacord o molt en desacord")
  cat_levels <- c("\nD'acord o\nmolt\nd'acord\n",
                  "\nNi d'acord\nni en\ndesacord\n",
                  "\nEn desacord\no molt en\ndesacord\n")
  en_levels <- c("\nAgree or\nstrongly agree\n",
                 "\nNeither agree\nnor disagree\n",
                 "\nDisagree or\nstrongly disagree\n")
  esp_levels <- c("\nDe acuerdo\no muy\nde acuerdo\n",
                  "\nNi de\nacuerdo ni\nen desacuerdo\n",
                  "\nEn desacuerdo\no muy\nen desacuerdo\n")
  
  if(catalan){
    the_subtitle <- "Grau d'acord amb l'afirmació"
    the_levels <- cat_levels
    the_title <- "\n'Amb tanta immigració, un ja no se sent com a casa'"
    the_caption <- 'Mostra: 3.143 residents de Catalunya amb ciutadania espanyola. Dades: Combinació enquestes CEO.\n2015 i 2018. Preguntes P56I i P31. Gràfic: Joe Brew | @joethebrew.'
    indies <- c('A favor\nde la\nindependència',
                'NS/NC',
                'En contra\nde la\nindependència')
  } else if(english){
    the_levels <- en_levels
    the_subtitle <- "Extent of agreement with the phrase"
    the_title <- '\n"With so much immigration, it doesn\'t feel like home any more"'
    the_caption <- 'Sample size: 3.143 residents of Catalonia with Spanish citizenship. Data: Aggregated CEO surveys,\n2015 and 2018. Questions P56I and P31. Chart: Joe Brew | @joethebrew | joe@databrew.cc\nCode for reproducibility: https://github.com/joebrew/vilaweb/tree/master/analyses/mentides'
    indies <- c('In favor of\nCatalan\nindependence',
                'Not sure/no answer',
                'Against\nCatalan\nindependence')
  } else {
    the_levels <- esp_levels
    the_subtitle <- "Grado de acuerdo con la afirmación"
    the_title <- '\n"Con tanta inmigración, uno ya no se siente como en casa"'
    the_caption <- 'Muestra: 3143 residentes de Cataluña con ciudadanía española. Datos: Combinación encuestas CEO.\n2015 y 2018. Preguntas P56I y P31. Gráfico: Joe Brew | @joethebrew.'
    indies <- c('A favor\nde la\nindependencia',
                'NS/NC',
                'En contra\nde la\nindependencia')
  }
  
  pd$indy <- 
    ifelse(pd$indy == 'Sí', indies[1],
           ifelse(pd$indy == 'No', indies[3],
                  indies[2]))
  
  pd$economy<- factor(pd$economy,
                      levels = rev(original_levels),
                      labels = rev(the_levels))
  n_cols <- length(unique(pd$economy))
  cols <- databrew::make_colors(n = 10, categorical = TRUE)
  cols <- cols[c(2,3, 6)]
  cols <- rev(cols)
  # cols <- rev(cols)
  cols[2] <- grey(0.2)
  
  pd <- pd %>%
    arrange(desc(economy), p)
  pd$indy <- factor(pd$indy, levels = unique(pd$indy))
  
  ggplot(data = pd,
         aes(x = indy,
             y = p)) +
    geom_bar(stat = 'identity',
             position = 'stack',
             # position = position_dodge(width = 0.9),
             # color = 'black',
             alpha = 0.85,
             aes(fill = economy)) +
    # facet_wrap(~partit) +
    theme_vilaweb() +
    # scale_fill_manual(name = '',
    #                   values = cols) +
    geom_text(aes(label = paste0(point_replace(round(p, digits = 1)), '%'),
                  y = p-1),
              alpha = 0.9,
              col = 'white',
              vjust = 1,
              position = 'stack',
              # position = position_dodge(width = 0.9),
              size = 5) +
    scale_fill_manual(name = '',
                      values = cols) +
    labs(x = '',
         y = '%',
         subtitle = the_subtitle,
         title =  the_title,
         caption = the_caption) +
    theme(legend.position = 'right',
          axis.text.x = element_text(size = 12),
          plot.caption = element_text(hjust = 0,
                                      color = cols[1]))
  
}




# uk_authority
uk_authority <- function(ca = FALSE){
  if(!ca){
    the_labs <- labs(x = '',
                     y = 'Percent in favor of Brexit',
                     title = 'Authoritarianism in the "leave" vote',
                     caption = 'Data source: "Understanding the Leave Vote" by Kirby Swales.\nChart by Joe Brew. www.vilaweb.cat')
    authority <- factor(c('Authoritarian', 'Neither', 'Libertarian'))
    authority <- factor(authority, 
                        levels = authority)
  } else {
    the_labs <- labs(x = '',
                     y = 'Percentage a favor del Brexit',
                     title = 'Nivell educatiu i suport al Brexit',
                     caption = 'Font de dades: "Understanding the Leave Vote" de Kirby Swales.\nGràfic de Joe Brew. www.vilaweb.cat')
    authority <- factor(c('Autoritari', 'Cap dels dos', 'Llibertari'))
    authority <- factor(authority, 
                        levels = authority)
  }
  df <- tibble(authority,
               y = c(66, 38, 18))
  ggplot(data = df,
         aes(x = authority,
             y = y)) +
    geom_bar(stat = 'identity',
             fill = as.character(colors_vilaweb()[5])) +
    theme_vilaweb() +
    the_labs +
    theme(axis.text.x = element_text(size = 13),
          plot.caption = element_text(size = 10)) +
    geom_text(aes(label = point_replace(round(y, digits = 1))),
              nudge_y = -5,
              alpha = 0.8,
              size = 5,
              color = 'white')
}

# cat_authority
var_cat <- function(ca = FALSE, var = 'P56I',
                    by_party = FALSE,
                    position = 'stack',
                    size = 4){

  party_dict <- 
    tibble(P24 = c("PPC",
                   "CiU",
                   "ERC",
                   "PSC",
                   "ICV-EUiA",
                   "C's",
                   "Reagrupament.cat",
                   "SI",
                   "PxC",
                   "CUP",
                   "UPyD",
                   "Podemos",
                   "Barcelona en Comú",
                   "CDC",
                   "Junts pel Sí",
                   "Catalunya sí que es pot",
                   "Democràcia i Llibertat",
                   "En Comú Podem",
                   "PACMA",
                   "PDeCAT",
                   "Junts per Catalunya",
                   "Catalunya en Comú Podem",
                   "Altres partits",
                   "Cap",
                   "No ho sap",
                   "No contesta"),
           partit = c("PPC",
                      "PDCat/CiU/CDC/Junts",
                      "ERC",
                      "PSC",
                      "ICV-EUiA",
                      "C's",
                      "Reagrupament.cat",
                      "SI",
                      "PxC",
                      "CUP",
                      "UPyD",
                      "Podem(os)",
                      "Podem(os)",
                      "PDCat/CiU/CDC/Junts",
                      "PDCat/CiU/CDC/Junts",
                      "Podem(os)",
                      "Democràcia i Llibertat",
                      "Podem(os)",
                      "PACMA",
                      "PDCat/CiU/CDC/Junts",
                      "PDCat/CiU/CDC/Junts",
                      "Podem(os)",
                      "Altre/Cap/NS/NC",
                      "Altre/Cap/NS/NC",
                      "Altre/Cap/NS/NC",
                      "Altre/Cap/NS/NC"))
  
  pd <- vilaweb::ceo %>%
    mutate(year = ANY) %>%
    mutate(indy = P31) %>%
    mutate(indy = as.character(indy)) %>%
    mutate(indy = ifelse(indy %in% c('No ho sap',
                                     'No contesta'),
                         'NS/NC',
                         indy)) %>%
    filter(!is.na(indy)) %>%
    filter(indy != 'NS/NC') %>%
    rename(variable = var)
  
  if(by_party){
    pd <- pd %>%
      left_join(party_dict) %>%
      group_by(partit) %>%
      mutate(size = n()) %>%
      mutate(remove = size < 50) %>%
      ungroup
  }
  
  pd <- pd %>%
    mutate(variable = as.character(variable)) %>%
    mutate(variable = ifelse(variable %in% c('No ho sap',
                                           'No contesta'),
                            'NS/NC',
                            variable)) %>%
    filter(!is.na(variable)) %>%
    filter(variable != 'NS/NC') %>%
    mutate(variable = as.character(variable)) %>%
    mutate(variable = 
             ifelse(variable %in% c("Molt d'acord",
                                   "D'acord"),
                    "D'acord o molt d'acord",
                    ifelse(variable %in% c("En desacord",
                                          "Molt en desacord"),
                           "En desacord o molt en desacord",
                           "Ni d'acord ni en desacord")))
  
  if(by_party){
    pd <- pd %>%
      filter(!remove) %>%
      group_by(variable, indy = partit) %>%
      summarise(n = sum(PONDERA),
                Muestra = n()) %>%
      ungroup %>%
      group_by(indy) %>%
      mutate(p = n / sum(n) * 100) 
  } else {
    pd <- pd %>%
      group_by(variable, indy) %>%
      summarise(n = sum(PONDERA),
                Muestra = n()) %>%
      ungroup %>%
      group_by(indy) %>%
      mutate(p = n / sum(n) * 100) 
  }
    
  
  original_levels <- c("D'acord o molt d'acord",
                       "Ni d'acord ni en desacord",
                       "En desacord o molt en desacord")
  cat_levels <- c("\nD'acord o\nmolt\nd'acord\n",
                  "\nNi d'acord\nni en\ndesacord\n",
                  "\nEn desacord\no molt en\ndesacord\n")
  en_levels <- c("\nAgree or\nstrongly agree\n",
                 "\nNeither agree\nnor disagree\n",
                 "\nDisagree or\nstrongly disagree\n")
  
  phrase_dict <- 
    tibble(var = paste0('P56',
                        c(LETTERS[1:11])),
           catalan = c("'Com menys intervingui el govern en l’economia, millor serà pel país'",
                       "'Cal abaixar els impostos, encara que això impliqui\nreduir serveis i prestacions públiques'",
                       "'El govern hauria de prendre mesures per a reduir les\ndiferències en els nivells d’ingressos'",
                       "'Les parelles de gais i lesbianes han de poder adoptar\nfills en les mateixes condicions que les parelles heterosexuals'",
                       "'L’escola ha d’ensenyar als nens a obeir l’autoritat'",
                       "'La religió no hauria de tenir cap influència en la política'",
                       "'En qualsevol circumstància, la llei sempre ha de ser obeïda'",
                       "'Algú amb plenes facultats hauria de poder decidir quan vol morir'",
                       "'Amb tanta immigració, un ja no se sent com a casa'",
                       "'El creixement econòmic ha de tenir prioritat sobre la protecció del medi ambient'",
                       "'Catalunya no té el dret de celebrar un referèndum d’autodeterminació'"),
           english = c("'The less the government interferes in the economy, the better off the country will be'",
                       "'Taxes must be lowered, even though it may\nmean reducing public services'",
                       "'The government should take measures to\nreduce differenes in income'",
                       "'Gay and lesbian couples should be able to adopt children\nunder the same conditions as heterosexual couples'",
                       "'School should teach children to obey authority'",
                       "'Religion should have no influence on politics'",
                       "'The law should always be obeyed in any circumstance'",
                       "'Someone with full abilities should be allowed to decide when (s)he wants to die'",
                       "'With so much immigration, one no longer feels at home'",
                       "'Economic growth should have priority over protection of the environment'",
                       "'Catalonia does not have a right to hold a self-determination referendum'"))
  
  if(ca){
    the_subtitle <- "Grau d'acord amb l'afirmació"
    the_levels <- cat_levels
    the_title <- phrase_dict$catalan[phrase_dict$var == var]
    the_caption <- paste0('Mostra: ',
                          commafy(sum(pd$Muestra)),
                          ' residents de Catalunya amb ciutadania espanyola. Dades: Combinació enquestes CEO.\n2015 i 2018. Preguntes ',
                          var,' i P31. Gràfic: Joe Brew | @joethebrew.')
    indies <- c('A favor\nde la\nindependència',
                'NS/NC',
                'En contra\nde la\nindependència')
  } else {
    the_levels <- en_levels
    the_subtitle <- "Extent of agreement with the phrase"
    the_title <- phrase_dict$english[phrase_dict$var == var]
    the_caption <- paste0('Sample: ',
                          commafy(sum(pd$Muestra)),
                          ' residents of Catalonia with Spanish citizenship. Data: CEO/BOP surveys.\n2015 and 2018. Chart: Joe Brew | @joethebrew. www.vilaweb.cat')
    indies <- c('In favor of\nCatalan independence',
                'Not sure/no answer',
                'Against\nCatalan independence')
  }
  
  if(!by_party){
    pd$indy <- 
      ifelse(pd$indy == 'Sí', indies[1],
             ifelse(pd$indy == 'No', indies[3],
                    indies[2]))
    pd$indy <- factor(pd$indy, levels = rev(unique(sort(pd$indy))))
    
  } else {
    party_levels <- c('CUP','ERC',
                      'PDCat/CiU/CDC/Junts',
                      'Podem(os)',
                      'PSC',
                      "C's",
                      'PPC')
    pd <- pd %>%
      filter(indy %in% party_levels)
    pd$indy <- factor(pd$indy,
                      levels = party_levels,
                      labels = gsub('/', '/\n', party_levels))
  }
  
  
  pd$variable<- factor(pd$variable,
                      levels = rev(original_levels),
                      labels = rev(the_levels))
  n_cols <- length(unique(pd$variable))
  cols <- vilaweb::colors_vilaweb()
  cols <- cols[c(3,2, 6)]
  cols <- as.character(cols)
  cols <- rev(cols)
  # cols <- rev(cols)
  cols[2] <- grey(0.2)
  
  pd <- pd %>%
    arrange(desc(variable), p)
  
  g <- 
    ggplot(data = pd,
         aes(x = indy,
             y = p,
             group = variable)) +
   
    # facet_wrap(~partit) +
    theme_vilaweb() 
  
  if(position == 'stack'){
    g <- g + 
      geom_bar(stat = 'identity',
               position = position_stack(),
               # position = position_dodge(width = 0.9),
               # color = 'black',
               alpha = 0.85,
               aes(fill = variable)) +
      geom_text(aes(label = paste0(point_replace(round(p, digits = 1)), '%'),
                    y = p-1),
                alpha = 0.9,
                col = 'white',
                vjust = 1,
                position = position_stack(),
                # position = position_dodge(width = 0.9),
                size = size)
  } else {
    g <- g +
      geom_bar(stat = 'identity',
               position = position_dodge(width = 0.9),
               # position = position_dodge(width = 0.9),
               # color = 'black',
               alpha = 0.85,
               aes(fill = variable)) +
      geom_text(aes(label = paste0(point_replace(round(p, digits = 1)), ''),
                    y = p+5),
                alpha = 0.8,
                col = 'black',
                # vjust = 1,
                position = position_dodge(width = 0.9),
                size = size) 
  }
  g <- g +
    scale_fill_manual(name = '',
                      values = cols) +
    labs(x = '',
         y = '%',
         subtitle = the_subtitle,
         title =  the_title,
         caption = the_caption) +
    theme(legend.position = 'right',
          axis.text.x = element_text(size = 12),
          plot.caption = element_text(hjust = 0,
                                      color = 'black'),
          plot.title = element_text(size = 17))
  return(g)
}


# uk_age
uk_age <- function(ca = FALSE){
  if(!ca){
    the_labs <- labs(x = '',
                     y = 'Percent in favor of Brexit',
                     title = 'Age and support for Brexit',
                     caption = 'Data source: "Understanding the Leave Vote" by Kirby Swales.\nChart by Joe Brew. www.vilaweb.cat')
  } else {
    the_labs <- labs(x = '',
                     y = 'Percentage a favor del Brexit',
                     title = 'Edat i suport al Brexit',
                     caption = 'Font de dades: "Understanding the Leave Vote" de Kirby Swales.\nGràfic de Joe Brew. www.vilaweb.cat')
  }
  education <- factor(c('18-34','35-44','45-54','55-64','65+'))
  education <- factor(education, 
                      levels = education)
  df <- tibble(education,
               y = c(40,50,52,51,61))
  ggplot(data = df,
         aes(x = education,
             y = y)) +
    geom_bar(stat = 'identity',
             fill = as.character(colors_vilaweb()[5])) +
    theme_vilaweb() +
    the_labs +
    theme(axis.text.x = element_text(size = 20),
          plot.caption = element_text(size = 10)) +
    geom_text(aes(label = paste0(point_replace(round(y, digits = 1)),'%')),
              nudge_y = -5,
              alpha = 0.8,
              size = 5,
              color = 'white')
}


# uk_welfare
uk_welfare <- function(ca = FALSE){
  if(!ca){
    the_labs <- labs(x = '',
                     y = 'Percent in favor of Brexit',
                     title = 'Welfarism among pro-Brexit voters',
                     caption = 'Data source: "Understanding the Leave Vote" by Kirby Swales.\nChart by Joe Brew. www.vilaweb.cat')
    education <- factor(c('Anti-welfare',
                          'Neither',
                          'Pro-welfare'))
    education <- factor(education, 
                        levels = education)
  } else {
    the_labs <- labs(x = '',
                     y = 'Percentage a favor del Brexit',
                     title = 'Suport a l\'estat del benestar i suport al Brexit',
                     caption = 'Font de dades: "Understanding the Leave Vote" de Kirby Swales.\nGràfic de Joe Brew. www.vilaweb.cat')
    education <- factor(c('En contra de\nl\'estat del benestar',
                          'Cap dels dos',
                          'A favor de\nl\'estat del benestar'))
    education <- factor(education, 
                        levels = education)
  }
  df <- tibble(education,
               y = c(75,54,30))
  ggplot(data = df,
         aes(x = education,
             y = y)) +
    geom_bar(stat = 'identity',
             fill = as.character(colors_vilaweb()[5])) +
    theme_vilaweb() +
    the_labs +
    theme(axis.text.x = element_text(size = 13),
          plot.caption = element_text(size = 10)) +
    geom_text(aes(label = point_replace(round(y, digits = 1))),
              nudge_y = -5,
              alpha = 0.8,
              size = 5,
              color = 'white')
}

# uk_income
uk_income <- function(ca = FALSE){
  if(!ca){
    the_labs <- labs(x = '',
                     y = 'Percent in favor of Brexit',
                     title = 'Monthly income and support for Brexit',
                     caption = 'Data source: "Understanding the Leave Vote" by Kirby Swales.\nChart by Joe Brew. www.vilaweb.cat')
  } else {
    the_labs <- labs(x = '',
                     y = 'Percentage a favor del Brexit',
                     title = 'Ingressos mensuals i suport al Brexit',
                     caption = 'Font de dades: "Understanding the Leave Vote" de Kirby Swales.\nGràfic de Joe Brew. www.vilaweb.cat')

  }
  education <- factor(c('< £1200',
                        '£1201-2200',
                        '£2201-3700',
                        '£3701 +'))
  education <- factor(education, 
                      levels = education)
  df <- tibble(education,
               y = c(66,57,51,38))
  ggplot(data = df,
         aes(x = education,
             y = y)) +
    geom_bar(stat = 'identity',
             fill = as.character(colors_vilaweb()[5])) +
    theme_vilaweb() +
    the_labs +
    theme(axis.text.x = element_text(size = 13),
          plot.caption = element_text(size = 10)) +
    geom_text(aes(label = point_replace(round(y, digits = 1))),
              nudge_y = -5,
              alpha = 0.8,
              size = 5,
              color = 'white')
}

cat_age <- function(ca = FALSE){
  df <- vilaweb::ceo
  df <- df %>%
    mutate(indy = P31) %>%
    mutate(indy = as.character(indy)) %>%
    mutate(indy = ifelse(indy %in% c('No ho sap',
                                     'No contesta'),
                         'NS/NC',
                         indy)) %>%
    filter(!is.na(indy)) %>%
    mutate(age = EDAT) %>%
    mutate(age = ifelse(age >= 75, 75, age)) %>%
    mutate(age = ifelse(age < 10, paste0('0', age), as.character(age))) %>%
    mutate(age = substr(age, 1, 1)) %>%
    mutate(age = paste0(age, '0', '-\n', age, '9 ')) %>%
    mutate(age = ifelse(substr(age, 1, 2) == '10',
                        '<20',
                        ifelse(substr(age, 1, 2) == '70',
                               '70+',
                               age)))

  pd <- df %>% group_by(age, indy) %>%
    summarise(n = sum(PONDERA),
              Muestra = n()) %>%
    group_by(age) %>%
    mutate(p = n / sum(n) * 100) %>%
    arrange(indy)
  
  pd$indy <- factor(pd$indy,
                    levels = c('No', 'NS/NC', 'Sí'))
  if(!ca){
    pd$indy <- factor(pd$indy,
                      levels = c('No', 'NS/NC', 'Sí'),
                      labels = c('No', 'Not sure/\nno answer', 'Yes'))
  }
  # cols <- RColorBrewer::brewer.pal(n = 9,
  #                                  name = 'YlGnBu')
  # cols2 <- RColorBrewer::brewer.pal(n = 9,
  #                                   name = 'BrBG')
  # cols <- c(cols2[2], 
  #           grey(0.7),
  #           # grey(0.6),
  #           cols[6])
  cols <- databrew::make_colors(n = 10, categorical = TRUE)
  cols <- cols[c(2,3, 6)]
  # cols <- rev(cols)
  # cols <- rev(cols)
  cols[2] <- grey(0.2)
  if(ca){
    legend_title <- 'Independentista'
    the_labs <- labs(x = '',
                     y = 'Percentage',
                     title = 'Independentisme i edat',
                     caption = paste0('Mostra: ',
                                      commafy(sum(pd$Muestra)),
                                      ' residents de Catalunya amb ciutadania espanyola.\nDades: Combinació enquestes CEO.\n2015 a 2018. Gràfic: Joe Brew | @joethebrew.'))
  } else {
    legend_title <- 'Pro-\nindependence'
    the_labs <- labs(x = '',
                     y = 'Percentage',
                     title = 'Age and support for Catalan independence',
                     caption = paste0('Sample: ',
                                      commafy(sum(pd$Muestra)),
                                      ' residents of Catalonia with Spanish citizeship.\nData: Combination of CEO/BOP surveys from 2015-2018Gràfic: Joe Brew | @joethebrew.'))
  }
  ggplot(data = pd,
         aes(x = age,
             y = p,
             fill = indy)) +
    geom_bar(stat = 'identity',
             position = position_stack()) +
    theme_vilaweb() +
    scale_fill_manual(name = legend_title,
                      values = cols) +
    geom_text(aes(label = paste0(point_replace(round(p, digits = 1)), ''),
                  y = p),
              position = position_stack(vjust = 0.5),
              color = 'white',
              alpha = 0.7) +
    the_labs +
    theme(legend.position = 'right',
          legend.title = element_text(size = 9),
          axis.text.x = element_text(size = 15,
                                     hjust = 0.5))
  
}

cat_income <- function(ca = FALSE){
  add_zero <- function(x, n){
    x <- as.character(x)
    adders <- n - nchar(x)
    adders <- ifelse(adders < 0, 0, adders)
    for (i in 1:length(x)){
      if(!is.na(x[i])){
        x[i] <- paste0(
          paste0(rep('0', adders[i]), collapse = ''),
          x[i],
          collapse = '')  
      } 
    }
    return(x)
  }
  extract_income <- function(zzz){
    
    paste0(add_zero(as.numeric(gsub('.', '', unlist(strsplit(zzz, ' '))[c(2,4)], fixed = TRUE)), n = 4), ' ', collapse = '-\n')
  }
  extract_income <- Vectorize(extract_income)
  df <- vilaweb::ceo
  x <- df %>%
    mutate(income = as.character(C900))
  x <- x %>%
    mutate(income = ifelse(income %in% c('No contesta', 'No ho sap'), NA,
                           ifelse(income %in% c("No tè cap tipus d'ingrés", "Menys o igual a 300 \u0080"),
                                  '<300',
                                  ifelse(income == 'Més de 6.000 \u0080', '6000+', extract_income(income))))) 
  x <- x %>%  
    mutate(indy = P31) %>%
    mutate(indy = as.character(indy)) %>%
    mutate(indy = ifelse(indy %in% c('No ho sap',
                                     'No contesta'),
                         'NS/NC',
                         indy)) %>%
    filter(!is.na(income), !is.na(indy)) %>%
    filter(!grepl('NA', income)) %>%
    group_by(indy, income) %>%
    summarise(n = sum(PONDERA),
              Muestra = n()) %>%
    ungroup %>%
    group_by(income) %>%
    mutate(p = n / sum(n) * 100) %>%
    ungroup
  
  
  # cols <- RColorBrewer::brewer.pal(n = 9,
  #                                  name = 'YlGnBu')
  # cols2 <- RColorBrewer::brewer.pal(n = 9,
  #                                   name = 'BrBG')
  # cols <- c(cols2[2], 
  #           grey(0.7),
  #           # grey(0.6),
  #           cols[6])
  cols <- databrew::make_colors(n = 10, categorical = TRUE)
  cols <- cols[c(2,3, 6)]
  cols <- rev(cols)
  # cols <- rev(cols)
  cols[2] <- grey(0.2)
  if(ca){
    legend_title <- 'Independentista'
    the_labs <- labs(y = 'Percentage',
         x = 'Ingressos mensuals familiars',
         title = paste0('Ingressos i independentisme'),
         caption = paste0('Mostra: ',
                          commafy(sum(x$Muestra)),
                          ' residents de Catalunya amb ciutadania espanyola.\nDades: Combinació enquestes CEO.\n2015 a 2018. Gràfic: Joe Brew | @joethebrew.'))
  } else {
    legend_title <- 'Pro-\nindependence'
    the_labs <- labs(y = 'Percentage',
                     x = 'Family income of respondent',
                     title = paste0('Household income and support for Catalan independence'),
                     caption = paste0('Sample: ',
                                      commafy(sum(x$Muestra)),
                                      ' residents of Catalonia with Spanish citizeship.\nData: Combination of CEO/BOP surveys from 2015-2018Gràfic: Joe Brew | @joethebrew.'))
  }
  
  ggplot(data = x,
         aes(x = income,
             y = p,
             fill = indy,
             group = indy)) +
    # geom_area(position = 'stack') +
    geom_bar(stat = 'identity', position = position_stack(),
             alpha = 0.9) +
    # scale_x_discrete(breaks = as.character(c(1, 3, 5, 7, 9)), labels=c('Extrema\nizquierda', 'Izquierda', 'Centro', 'Derecha', 'Extrema\nderecha'))  +
    the_labs +
    theme_vilaweb() +
    # scale_fill_manual(name = 'View on independence',
    #                   values = c('darkgrey', '#FF5733', '#1DA1F2')) +
    theme(legend.direction ="vertical",legend.position = "bottom", 
          axis.text.x = element_text(size = 8),
          plot.title = element_text(size = 16)) +
    geom_text(aes(label = point_replace(round(p, digits = 1)),
                  y = p),
              position = position_stack(vjust = 0.5),
              fill = NA,
              alpha = 0.8,
              color = 'white',
              size = 3) +
    scale_fill_manual(name = legend_title,
                      values = cols) +
    theme(legend.position = 'right',
          plot.caption = element_text(size = 8),
          legend.title = element_text(size = 10))
    # guides(fill=guide_legend(ncol=3,
    #                          title.position = 'top',
    #                          title.hjust = 0.5)) 
}

left_right <- function(ca = FALSE){
  df <- vilaweb::ceo %>%
    mutate(axis = P25,
           indy = P31) %>%
    mutate(indy = as.character(indy)) %>%
    mutate(indy = ifelse(indy %in% c('No ho sap',
                                     'No contesta'),
                         'NS/NC',
                         indy)) %>%
    filter(!axis %in% c('No ho sap', 'No contesta')) %>%
    filter(!is.na(indy)) %>%
    group_by(axis, indy) %>%
    tally %>%
    ungroup %>%
    group_by(axis) %>%
    mutate(p = n / sum(n) * 100) %>%
    mutate(`Muestra` = n)
  
  df$axis <- factor(df$axis,
                    levels = levels(df$axis),
                    labels = gsub(' ', '\n', levels(df$axis)))
  
  
  if(ca){
    df$indy <- factor(df$indy,
                      labels = c('Unionista',
                                 'NS/NC',
                                 'Independentista'))
  } else {
    df$indy <- factor(df$indy,
                      labels = c('Pro-Union',
                                 'No anwer/not sure',
                                 'Pro-independence'))
    df$axis <- factor(df$axis,
                      levels = levels(df$axis),
                      labels = gsub('dreta', 'Right', gsub('esquerra', 'Left', gsub('Extrema', 'Far', levels(df$axis)))))
  }
  
  
  cols <- RColorBrewer::brewer.pal(n = 9,
                                   name = 'YlGnBu')
  cols2 <- RColorBrewer::brewer.pal(n = 9,
                                    name = 'BrBG')
  cols <- c(cols2[2], cols[3], cols[6])
  
  if(ca){
    the_labs <- 
      labs(y = 'Percentatge',
           x = 'Ideologia (escala esquerra-dreta)',
           title = 'Escala ideològica (esquerra-dreta) i independentisme',
           caption = paste0(
             'Font de dades: Combinació d\'enquestes del Baròmetre d\'Opinió Política, Centre d\'Estudios d\'Opinió.\nDades recollides 2015-2018. Mostra: ', commafy(sum(df$Muestra)),  ' residents de Catalunya amb ciutadania espanyola.\nGràfic: Joe Brew | @joethebrew | www.vilaweb.cat'))
    
  } else {
    the_labs <- 
      labs(y = 'Percentage',
           x = 'Ideology (left-right scale)',
           title = 'Ideological scale (left-right) and independence',
           caption = paste0(
             'Data source: Combination of surveys from the Baròmetre d\'Opinió Política, Centre d\'Estudios d\'Opinió.\nData collected 2015-2018. Sample: ', commafy(sum(df$Muestra)),  ' residents of Catalonia with Spanish citizenship.\nChart: Joe Brew | @joethebrew | www.vilaweb.cat'))
    
  }
  
  ggplot(data = df,
         aes(x = axis,
             y = p,
             fill = indy,
             group = indy)) +
    # geom_area(position = position_stack(),
    #           alpha = 0.5) + 
    stat_smooth(
      position = position_stack(),
      # color = 'black',
      geom = 'area', method = 'loess', span = 0.7,
      alpha = 0.8) +
    the_labs +
    scale_fill_manual(name = '',
                      values = cols) +
    theme_vilaweb() +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 20))
}

franco <- function(ca = FALSE){
  party_dict <- 
    tibble(P24 = c("PPC",
                   "CiU",
                   "ERC",
                   "PSC",
                   "ICV-EUiA",
                   "C's",
                   "Reagrupament.cat",
                   "SI",
                   "PxC",
                   "CUP",
                   "UPyD",
                   "Podemos",
                   "Barcelona en Comú",
                   "CDC",
                   "Junts pel Sí",
                   "Catalunya sí que es pot",
                   "Democràcia i Llibertat",
                   "En Comú Podem",
                   "PACMA",
                   "PDeCAT",
                   "Junts per Catalunya",
                   "Catalunya en Comú Podem",
                   "Altres partits",
                   "Cap",
                   "No ho sap",
                   "No contesta"),
           partit = c("PPC",
                      "PDCat/CiU/CDC/Junts",
                      "ERC",
                      "PSC",
                      "ICV-EUiA",
                      "C's",
                      "Reagrupament.cat",
                      "SI",
                      "PxC",
                      "CUP",
                      "UPyD",
                      "Podem(os)",
                      "Podem(os)",
                      "PDCat/CiU/CDC/Junts",
                      "PDCat/CiU/CDC/Junts",
                      "Podem(os)",
                      "Democràcia i Llibertat",
                      "Podem(os)",
                      "PACMA",
                      "PDCat/CiU/CDC/Junts",
                      "PDCat/CiU/CDC/Junts",
                      "Podem(os)",
                      "Altre/Cap/NS/NC",
                      "Altre/Cap/NS/NC",
                      "Altre/Cap/NS/NC",
                      "Altre/Cap/NS/NC"))
  
  pd <- vilaweb::ceo %>%
    left_join(party_dict) %>%
    group_by(partit) %>%
    mutate(size = n()) %>%
    # filter(size >= 50) %>%
    ungroup %>%
    mutate(year = ANY) %>%
    mutate(axis = as.character(P25)) %>%
    mutate(axis = ifelse(axis == 'Extrema esquerra',
                         '1',
                         ifelse(axis == 'Extrema dreta',
                                '10',
                                as.character(axis)))) %>%
    mutate(axis = as.numeric(axis)) #%>%
  # filter(!partit %in% c('Altre/Cap/NS/NC',
  #                       'ICV-EUIA'))
  
  pd <- pd %>%
    mutate(indy = P31) %>%
    mutate(indy = as.character(indy)) %>%
    mutate(indy = ifelse(indy %in% c('No ho sap',
                                     'No contesta'),
                         'NS/NC',
                         indy)) 
  #   filter(!is.na(indy)) %>%
  # filter(indy != 'NS/NC') %>%
  
  # Get date
  pd <- pd %>%
    arrange(ANY, MES) %>%
    group_by(BOP_NUM) %>%
    mutate(bop_date = as.Date(paste0(dplyr::first(ANY),
                                     '-',
                                     dplyr::first(MES),
                                     '-01'))) 
  
  # # Get valoracio del rei 2018 onada 1 p21j
  # pd <- pd %>%
  #   mutate(valoracio_rei = P21J)
  # 
  # # Get val columns
  val_columns <- names(pd)[grepl('val_', names(pd))]
  
  # Franquism question: P102
  pd$franquisme <- pd$P102
  pd$constitucio <- pd$P95
  con_dict <- data_frame(
    constitucio = c('Votaria sí',
                    'Votaria no',
                    'Votaria en blanc',
                    'Votaria nul',
                    'No votaria',
                    'No ho sap',
                    'Contesta'),
    constitucio2 = c('Votaria sí',
                     'Votaria no',
                     'Votaria nul\nen blanc\nNo votaria\nNS/NC',
                     'Votaria nul\nen blanc\nNo votaria\nNS/NC',
                     'Votaria nul\nen blanc\nNo votaria\nNS/NC',
                     'Votaria nul\nen blanc\nNo votaria\nNS/NC',
                     'Votaria nul\nen blanc\nNo votaria\nNS/NC')
  )
  pd <- pd %>%
    left_join(con_dict) %>%
    dplyr::select(-constitucio) %>%
    dplyr::rename(constitucio = constitucio2)
  
  # Subset to only include columns of interest
  pd <- pd[,c('axis', 
              'bop_date',
              'partit',
              'indy',
              'franquisme',
              'constitucio',
              'PONDERA')]
  pd <- pd %>%
    filter(!is.na(franquisme)) %>%
    mutate(franquisme = as.character(franquisme)) %>%
    mutate(franquisme = ifelse(franquisme %in% c('No ho sap', 'No contesta'), 'NS/NC', franquisme)) %>%
    mutate(franquisme = ifelse(franquisme == 'Va tenir coses positives i negatives', 'Va tenir\ncoses positives\ni negatives', franquisme))
  pd$franquisme <- factor(pd$franquisme,
                          levels = c('Negatiu',
                                     'Va tenir\ncoses positives\ni negatives', 
                                     'Positiu',
                                     'NS/NC'))
  if(!ca){
    pd$franquisme <- factor(pd$franquisme,
                            levels = c('Negatiu',
                                       'Va tenir\ncoses positives\ni negatives', 
                                       'Positiu',
                                       'NS/NC'),
                            labels = c('Negative',
                                       'It had negative\nthings and\npositivethings',
                                       'Positive',
                                       'No answer\nnot sure'))
  }
  
  fran_dict <-
    data_frame(franquisme2 = c('Negatiu',
                               'Va tenir\ncoses positives\ni negatives',
                               'Positiu'))
  
 
    the_levels <- 
      paste0('Dels que diuen que\n"El franquisme ',
             c('va ser negatiu"',
               'va tenir coses\nnegatives i positives"',
               'va ser positiu"'), '...')
  
  
  fran_dict$franquisme <- factor(paste0('Dels que diuen que\n"El franquisme ',
                                        c('va ser negatiu"',
                                          'va tenir coses\nnegatives i positives"',
                                          'va ser positiu"'), '...'),
                                 levels = the_levels)
  
    plot_data <- pd %>%
      group_by(partit, franquisme) %>%
      summarise(n = sum(PONDERA),
                mostra = n()) %>%
      group_by(partit) %>%
      mutate(p = n / sum(n) * 100) %>%
      ungroup %>%
      filter(!partit %in% c('ICV-EUiA', 'PACMA',
                            'Altre/Cap/NS/NC'))
    
    plot_data <- plot_data %>%
      arrange(franquisme,p)
    plot_data$partit <- gsub('/', '/\n', plot_data$partit)
    plot_data$partit <- factor(plot_data$partit,
                               levels = unique(plot_data$partit))
    # plot_data$franquisme <- factor(plot_data$franquisme)
    # plot_data$franquisme <- factor(plot_data$franquisme,
    #                                levels = rev(levels(plot_data$franquisme)))
    
    cols <- RColorBrewer::brewer.pal(n = 8, 'Spectral' )
    cols <- cols[1:4]
    cols <- rev(cols)
    
    # cols <- rev(cols)
    cols[4] <- grey(0.6)
    cols[1] <- as.character(vilaweb::colors_vilaweb()[5])
    
    plot_data$label <- ifelse(plot_data$franquisme == 'NS/NC',
                              '',
                              paste0(round(plot_data$p, 1), '%'))
    
    if(ca){
      legend_name <- 'El franquisme\nva ser...'
      the_labs <- 
        labs(y = 'Percentatge',
             x = '',
             title = 'Valoració del franquisme per partit',
             subtitle = "Pregunta: 'Per a vostè, l’etapa del franquisme ha suposat a la història de Catalunya, \nen conjunt, un període positiu, negatiu o va tenir coses positives i negatives?'",
             caption = paste0(commafy(sum(plot_data$mostra)),
                              ' residents de Catalunya amb ciutadania espanyola. Dades del BOP/CEO 3a onada 2018.\nPreguntes 102 (franquisme) i 31 (independentisme). Gràfic de Joe Brew | @joethebrew. |  www.vilaweb.cat.'))
    } else {
      legend_name <- 'Francoism\nwas...'
      the_labs <- 
        labs(y = 'Percentage',
             x = '',
             title = 'Views on Franco by party',
             subtitle = "Question: 'For you, the Franco era meant what for Catalonia\'s history:\na positive period, a negative period, a period with positive and negative things?'",
             caption = paste0(commafy(sum(plot_data$mostra)),
                              ' residents of Catalonia with Spanish citizenship. Data from the BOP/CEO, 3rd wave, 2018.\nQuestions 102 (francoism) and 31 (independence). Chart by Joe Brew | @joethebrew. |  www.vilaweb.cat.'))
    }
    
    ggplot(data = plot_data,
           aes(x = partit,
               y = p,
               fill = franquisme)) +
      geom_bar(stat = 'identity',
               position = position_stack()) +
      scale_fill_manual(name = '',
                        values = cols) +
      geom_text(aes(label = point_replace(label)),
                # nudge_y = 4,
                size = 3,
                position = position_stack(),
                color = 'white',
                alpha = 0.8,
                vjust = 1) +
      the_labs +
      scale_fill_manual(name = legend_name,
                        values = cols) +
      theme_vilaweb() +
      theme(legend.position = 'right') +
      # theme(legend.position = 'none') +
      theme(axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 14),
            plot.title = element_text(hjust = 0.5, size = 24),        plot.subtitle = element_text(size = 10),
            
            strip.text = element_text(size = 21,
                                      color = grey(0.1)),
            plot.caption = element_text(hjust = 0, size = 7))
  
}


# Functions
mround <- function(x,base){ 
  base*round(x/base) 
} 

round_percent <- function(x) { 
  x <- x/sum(x)*100  # Standardize result
  res <- floor(x)    # Find integer bits
  rsum <- sum(res)   # Find out how much we are missing
  if(rsum<100) { 
    # Distribute points based on remainders and a random tie breaker
    o <- order(x%%1, sample(length(x)), decreasing=TRUE) 
    res[o[1:(100-rsum)]] <- res[o[1:(100-rsum)]]+1
  } 
  res 
}


val_pol <- function(df,
                    who = NULL,
                    know = FALSE,
                    group = NULL){
  
  # df <- vilaweb::ceo
  df <- df %>%
    dplyr::select(P24A,P24B,P24C,P24D,P24E,P24F,P24G,
                  P34,
                  P38_R,
                  EDAT,
                  EDAT_GR,
                  P29,
                  LLENGUA_ENQUESTA,
                  LLOC_NAIX,PROVÍNCIA,
                  contains("P49"),
                  contains("P26"),
                  P69) %>%
    dplyr::rename(provincia = PROVÍNCIA,
                  llengua = P69) %>%
    mutate(llengua = as_factor(llengua)) %>%
    mutate(axis = ifelse(P29 %in% 98:99,
                         NA, P29)) %>%
    mutate(axis_simple = ifelse(axis <=3, 
                                'Esquerra',
                                ifelse(axis <= 6,
                                       'Centre',
                                       'Dreta')))
  df$axis_simple <- factor(df$axis_simple,
                           levels = c('Esquerra', 'Centre', 'Dreta'))
  
  
  
  llengua_dict <- tibble(
    llengua = c("Català (valencià / balear)",
                "Castellà",
                "Totes dues igual: català (valencià / balear) i castellà",
                "Altres llengües o altres combinacions",
                "No ho sap"),
    language = c('Cat', 'Esp', 'Cat i\nEsp', 'Altres', 'Altres'))
  llengua_dict$language <- factor(llengua_dict$language,
                                  levels = c('Cat',  'Cat i\nEsp', 'Altres',  'Esp'))
  
  df <- left_join(df, llengua_dict)
  df <- df %>% dplyr::select(-llengua) %>%
    dplyr::rename(llengua = language)
  df$llengua <- factor(df$llengua,
                       levels = c('Cat',  'Cat i\nEsp', 'Altres',  'Esp'))
  # P34 is the indepenence question
  # df$indy <- df$P34
  indy_dict <- tibble(P34 = c(1, 2, 98,99),
                      indy = c('Independentistes',
                               'Unionistes',
                               'NS/NC',
                               'NS/NC'))
  
  # P38_R is party question
  party_dict <- tibble(P38_R = c(1, 3, 4, 6, 10, 21, 22, 80, 93, 96, 98),
                       partit = c('PPC','ERC', 'PSC', "C's", 'CUP', 'JxCat',
                                  'CECP', rep('Altre/NS/NC', 4)))
  
  
  df <- left_join(df, party_dict)
  df <- left_join(df, indy_dict)
  
  # # # P24 is the twitter question
  # 
  # a Pàgines de mitjans de comunicació.
  # b Pàgines de partits o candidats.
  # c Pàgines d’organitzacions ciutadanes o moviments cívics.
  # d Blogs o fòrums de debat.
  # e Facebook. 
  # f Twitter.
  # g Instagram.
  # 1. si
  # 2. no
  # 98/99. no sap / no contesta
  df$twitter <- as_factor(df$P24F)
  df$twitter <- as.character(df$twitter)
  df$twitter <- ifelse(is.na(df$twitter),
                       'No fa servir internet',
                       ifelse(df$twitter == 'Sí',
                              'Fa servir twitter\n(amb o sense altres mitjans)',
                              'No fa servir twitter,\nperò sí fa servir internet'))
  df$twitter <- factor(df$twitter,
                       levels = rev(c('Fa servir twitter\n(amb o sense altres mitjans)',
                                      'No fa servir twitter,\nperò sí fa servir internet',
                                      'No fa servir internet')))
  # df$twitter <- factor(df$twitter,
  #                      levels = rev(levels(df$twitter)))
  
  
  # Valoració de polítics
  politics <- gsub('P49A_', '', names(df)[grepl('P49A', names(df))])
  
  if(is.null(who)){
    who <- politics
  }
  pd <- df %>%
    dplyr::select(partit, axis, llengua, axis_simple, llengua, indy, contains('P49'))
  pd <- pd %>%
    gather(key, value, P49A_AFERNANDEZ:P49B_RIERA)
  pd$politic <- gsub('P49A_|P49B_', '', pd$key)
  pd$politic <- ifelse(pd$politic == 'AFERNANDEZ', 'FERNANDEZ', pd$politic)
  pd$key <- substr(pd$key, 4,4)
  key_dict <- tibble(key = c('A',
                             'B'),
                     know = c('Know', 'Score'))
  pd <- left_join(pd, key_dict)
  if(know){
    out <- pd %>%
      filter(know == 'Know') %>%
      mutate(value = ifelse(value == 2, 0, value))
  } else {
    out <- pd %>%
      filter(know == 'Score') %>%
      filter(! value %in% 98:99)
  }
  if(is.null(group)){
    out <- out %>%
      group_by(politic) %>%
      summarise(value = mean(value, na.rm = TRUE),
                n = n())
  } else {
    out <- out %>%
      group_by_('politic', group) %>%
      summarise(value = mean(value, na.rm = TRUE),
                n = n())
  }
  if(know){
    out$value <- out$value * 100
  }
  out <- out %>% arrange(desc(value))
  
  out <- out %>%
    filter(politic %in% who)
  return(out)
}

bp <- function(a,b){
  RColorBrewer::brewer.pal(n = 8, name = a)[b]
}

plot_val_pol <- function(ca = FALSE, who = NULL){
  pd <- val_pol(df = dfx,know = FALSE, group = 'axis_simple',
                who = who) %>%
    filter(!is.na(axis_simple))
  if(ca){
    the_labs <- labs(y = 'Valoració mitjana',
                     x = '',
                     title = 'Valoració de polítics per ideologia de l\'enquestat',
                     caption = "Dades del Baròmetre d'Opinió Política, març 2019. Mostra: 1.500 residents de CAT amb ciutadania ESP.\nIdeologia: escala 0-10 (esquerra-dreta). 0-3:Esquerra; 4-6:Centre; 7-10:Dreta. Gràfic de Joe Brew | @joethebrew.") 
  } else {
    the_labs <- labs(y = 'Average rating',
                     x = '',
                     title = 'Rating of politicians by ideology of respondent',
                     caption = "Data from the Baròmetre d'Opinió Política, March 2019. Sample: 1.500 residents of Catalonia with Spanish citizenship.\nIdeology: scale from 0-10 (left-right). 0-3:Left; 4-6:Centre; 7-10:Right. Chart by Joe Brew | @joethebrew | www.vilaweb.cat") 
  }
  ggplot(data = pd,
         aes(x = axis_simple,
             y = value)) +
    geom_area(aes(group = politic),
              fill = bp('Greys', 4),
              # color = bp('Greys', 5),
              alpha = 0.6) +
    geom_point(color = bp('Greys', 8),
               size = 0.5) +
    facet_wrap(~politic) +
    theme_vilaweb() +
    the_labs +
    geom_text(aes(label = point_replace(round(value, digits = 1))),
              alpha = 0.6,
              nudge_y = 1.5,
              size = 3) +
    geom_line(aes(group = 1),
              alpha = 0.8,
              size = 0.3)  +
    theme(plot.caption = element_text(size = 8))
}

democracy <- function(ca = FALSE){
  pd <- vilaweb::ceo %>%
    mutate(indy = P31) %>%
    mutate(indy = as.character(indy)) %>%
    mutate(indy = ifelse(indy %in% c('No ho sap',
                                     'No contesta'),
                         'NS/NC',
                         indy)) %>%
    filter(!is.na(indy)) %>%
    filter(indy != 'NS/NC') %>%
    mutate(democracy = P98) %>%
    mutate(democracy = as.character(democracy)) %>%
    mutate(democracy = ifelse(democracy %in% c('No ho sap',
                                           'No contesta'),
                            'NS/NC',
                            democracy)) %>%
    filter(!is.na(democracy)) %>%
    filter(democracy != 'NS/NC') %>%
    group_by(democracy, indy) %>%
    summarise(n = sum(PONDERA),
              Muestra = n(),
              date = paste0(unique(ANY), collapse = ',')) %>%
    ungroup %>%
    group_by(indy) %>%
    mutate(p = n / sum(n) * 100)
  pd$democracy <- ifelse(!is.na(pd$democracy),
                         paste0("'", pd$democracy, "'", collapse = NULL))
  
  pd$democracy<- factor(pd$democracy,
                      levels = c("'La democràcia és preferible a qualsevol altra forma de govern'",
                                 "'Per a persones com jo, m'és igual un règim democràtic o un règim no democràtic'",
                                 "'En algunes circumstàncies un règim autoritari és preferible a un sistema democràtic'"),
                      labels = c("'La democràcia és preferible\na qualsevol altra forma de govern'",
                                 "'Per a persones com jo,\nm'és igual un règim democràtic o\nun règim no democràtic'",
                                 "'En algunes circumstàncies\nun règim autoritari és preferible\na un sistema democràtic'"))
  if(!ca){
    pd$democracy<- factor(pd$democracy,
                          levels = levels(pd$democracy),
                          labels = c("'Democracy is always\npreferable to any\nother form of government'",
                                     "'People like me don't care\n whether it's a democratic\nor authoritarian regime'",
                                     "'In some circumstances an\nauthoritarian regime is\npreferable to a democratic system'"))
    pd$indy <- ifelse(pd$indy == 'No',
                      'Pro-Union Catalans',
                      'Pro-Independence Catalans')
    the_labs <- labs(x = '',
                     y = '',
                     title = "Preference for democracy",
                     caption = paste0('Sample: ', commafy(sum(pd$Muestra)),' residents of Catalonia with Spanish citizenship.\nBOP (CEO) survey, 2nd round, 2018. Joe Brew | @joethebrew\nwww.vilaweb.cat.'))
  } else {
    pd$indy <- ifelse(pd$indy == 'No',
                      'Unionistes',
                      'Independentistes')
    the_labs <- labs(x = '',
                     y = '',
                     title = "Preferència per la democràcia",
                     caption = paste0('Mostra: ', commafy(sum(pd$Muestra)),' residents de Catalunya amb ciutadania espanyola.\nEnquesta BOP (CEO), 2a onada, 2018. Joe Brew | @joethebrew\nwww.vilaweb.cat.'))
  }
  n_cols <- length(unique(pd$democracy))
  cols <- databrew::make_colors(n = n_cols, categorical = FALSE)
  # cols <- rev(cols)
  cols[2] <- 'darkgrey'
  
  ggplot(data = pd,
         aes(x = democracy,
             y = p)) +
    geom_bar(stat = 'identity',
             position = position_dodge(width = 0.9),
             # color = 'black',
             alpha = 0.95,
             aes(fill = democracy)) +
    facet_wrap(~indy) +
    theme_vilaweb() +
    # scale_fill_manual(name = '',
    #                   values = cols) +
    geom_text(aes(label = paste0(point_replace(round(p, digits = 1)), '%'),
                  y = ifelse(p > 50, p-10, p+10)),
              alpha = 0.9,
              color = 'black',
              position = position_dodge(width = 0.9),
              size = 4) +
    scale_fill_manual(name = '',
                      values = as.character(vilaweb::colors_vilaweb()[c(1,5,3)])) +
    the_labs +
    theme(legend.position = 'none',
          plot.caption = element_text(size = 9,
                                      hjust = 0),
          axis.text.y = element_text(size = 10),
          strip.text = element_text(size = 10)) +
    coord_flip()
}

indy_plot <- function(ca = FALSE, df = dfx, treure = FALSE){
  indy_dict <- tibble(P34 = c(1, 2, 98,99),
                      indy = c('Independentistes',
                               'Unionistes',
                               'NS/NC',
                               'NS/NC'))
  
  # P38_R is party question
  party_dict <- tibble(P38_R = c(1, 3, 4, 6, 10, 21, 22, 80, 93, 96, 98),
                       partit = c('PPC','ERC', 'PSC', "C's", 'CUP', 'JxCat',
                                  'CECP', rep('Altre/NS/NC', 4)))
  
  sov_dict <- tibble(P35 = c(1,2,3,4,5, 98, 99),
                     sov = factor(c('Molt d\'acord',
                                    'Bastant d\'acord',
                                    'Ni d\'acord ni en desacord/NS/NC',
                                    'Bastant en desacord',
                                    'Molt en desacord',
                                    'Ni d\'acord ni en desacord/NS/NC',
                                    'Ni d\'acord ni en desacord/NS/NC'),
                                  levels = c('Molt d\'acord',
                                             'Bastant d\'acord',
                                             'Ni d\'acord ni en desacord/NS/NC',
                                             'Bastant en desacord',
                                             'Molt en desacord')))
  
  if(!ca){
    sov_dict$sov <- factor(sov_dict$sov,
                           levels = levels(sov_dict$sov),
                           labels = c('Strongly\nagree',
                                      'Agree',
                                      'Neither agree\nnor disagree\nor not sure',
                                      'Disagree',
                                      'Strongly\ndisagree'))
    indy_dict$indy <- factor(indy_dict$indy,
                             levels = c('Independentistes',
                                        'NS/NC',
                                        'Unionistes'),
                             labels = c('Pro-independence',
                                        'Not sure /\nno answer',
                                        'Pro-union'))
    
  } else {
    sov_dict$sov <- factor(sov_dict$sov,
                           levels = levels(sov_dict$sov),
                           labels = c('Molt\nd\'acord',
                                      'Bastant\nd\'acord',
                                      'Ni d\'acord\nni en desacord\no NS/NC',
                                      'Bastant en\ndesacord',
                                      'Molt en\ndesacord'))
  }
  
  df <- left_join(df, party_dict)
  df <- left_join(df, indy_dict)
  df <- left_join(df, sov_dict)
  
  pd <- df %>%
    group_by(indy) %>%
    tally %>%
    ungroup
  if(treure){
    pd <- pd %>% filter(!indy %in% c('NS/NC', 'Not sure /\nno answer'))
  }
  pd <- pd %>%
    mutate(p = n / sum(n) * 100)
  
  
  if(ca){
    the_labs <- labs(title = 'Independentisme català, març 2019',
                     caption = 'Mostra: 1.500 residents de Catalunya amb ciutadania espanyola.\nEnquesta BOP/CEO, Ronda 1, març 2019. Joe Brew | @joethebrew | www.vilaweb.cat.')
  } else {
    the_labs <- labs(title = 'Support for Catalan independence, March 2019',
                     caption = 'Sample: 1.500 residents of Catalonia with Spanish citizenship.\nBOP/CEO survey, Round 1, March 2019. Joe Brew | @joethebrew | www.vilaweb.cat.')
  }
  
  ggplot(data = pd,
         aes(x = indy,
             y = p)) +
    geom_bar(stat = 'identity',
             fill = 'black',
             alpha = 0.7) +
    theme_vilaweb() +
    geom_text(aes(label = paste0(round(p, digits = 1), '%'),
                  y = p - 4),
              size = 6,
              color = 'white',
              alpha = 0.6) +
    labs(x = '',
         y = '%') +
    the_labs +
    theme(axis.text.x = element_text(size = 20))
}

ref_party_plot <- function(ca = FALSE, df = dfx, treure = FALSE){
  indy_dict <- tibble(P34 = c(1, 2, 98,99),
                      indy = c('Independentistes',
                               'Unionistes',
                               'NS/NC',
                               'NS/NC'))
  
  # P38_R is party question
  party_dict <- tibble(P38_R = c(1, 3, 4, 6, 10, 21, 22, 80, 93, 96, 98),
                       partit = c('PPC','ERC', 'PSC', "C's", 'CUP', 'JxCat',
                                  'CECP', rep('Altre/NS/NC', 4)))
  
  sov_dict <- tibble(P35 = c(1,2,3,4,5, 98, 99),
                     sov = factor(c('Molt d\'acord',
                                    'Bastant d\'acord',
                                    'Ni d\'acord ni en desacord/NS/NC',
                                    'Bastant en desacord',
                                    'Molt en desacord',
                                    'Ni d\'acord ni en desacord/NS/NC',
                                    'Ni d\'acord ni en desacord/NS/NC'),
                                  levels = c('Molt d\'acord',
                                             'Bastant d\'acord',
                                             'Ni d\'acord ni en desacord/NS/NC',
                                             'Bastant en desacord',
                                             'Molt en desacord')))
  
  if(!ca){
    sov_dict$sov <- factor(sov_dict$sov,
                           levels = levels(sov_dict$sov),
                           labels = c('Strongly\nagree',
                                      'Agree',
                                      'Neither agree\nnor disagree\nor not sure',
                                      'Disagree',
                                      'Strongly\ndisagree'))
  } else {
    sov_dict$sov <- factor(sov_dict$sov,
                           levels = levels(sov_dict$sov),
                           labels = c('Molt\nd\'acord',
                                      'Bastant\nd\'acord',
                                      'Ni d\'acord\nni en desacord\no NS/NC',
                                      'Bastant en\ndesacord',
                                      'Molt en\ndesacord'))
  }
  
  df <- left_join(df, party_dict)
  df <- left_join(df, indy_dict)
  df <- left_join(df, sov_dict)
  
  
  if(treure){
    pd <- df %>% filter(!indy %in% c("i d\'acord\nni en desacord\no NS/NC", 'Neither agree\nnor disagree\nor not sure'))
  } else {
    pd <- df
  }
  pd <- pd %>%
    group_by(indy = sov, partit) %>%
    tally %>%
    ungroup %>%
    group_by(partit) %>%
    mutate(p = n/ sum(n) * 100) %>%
    ungroup


  
  
  if(ca){
    the_labs <- labs(title = 'Grau d\'acord amb la frase següent',
                     subtitle = "'Els catalans i les catalanes tenen dret de decidir el\nseu futur com a país votant en un referèndum'",
                     caption = 'Mostra: 1.500 residents de Catalunya amb ciutadania espanyola.\nEnquesta BOP/CEO, Ronda 1, març 2019. Joe Brew | @joethebrew | www.vilaweb.cat.')
  } else {
    the_labs <- labs(title = 'Agreement with below phrase:',
                     subtitle = "'Catalans have a right to decide their future\nas a country through a referendum'",
                     
                     
                     caption = 'Sample: 1.500 residents of Catalonia with Spanish citizenship.\nBOP/CEO survey, Round 1, March 2019. Joe Brew | @joethebrew | www.vilaweb.cat.')
  }
  pd$indy <- factor(pd$indy,
                    levels = rev(levels(pd$indy)))
  pd <- pd %>% filter(!partit %in% c('Altre/NS/NC'))
  
  
  cols <- RColorBrewer::brewer.pal(n = 9,
                                   name = 'YlGnBu')
  cols2 <- RColorBrewer::brewer.pal(n = 9,
                                    name = 'BrBG')
  cols <- c(cols2[2], cols[3], cols[7])
  cols <- colorRampPalette(cols)(5)
  
  ggplot(data = pd,
         aes(x = partit,
             y = p,
             fill = indy,
             group = indy)) +
    geom_bar(stat = 'identity',
             position = position_stack(),
             alpha = 0.95) +
    theme_vilaweb() +
    geom_text(aes(label = paste0(round(p, digits = 1), '%'),
                  y = p),
              vjust = 1,
              position = position_stack(),
              size = 2,
              color = 'white',
              alpha = 0.6) +
    labs(x = '',
         y = '%') +
    the_labs +
    theme(axis.text.x = element_text(size = 14),
          legend.direction = 'vertical',
          legend.position = 'right',
          plot.caption = element_text(hjust = 1)) +
    scale_fill_manual(name = '',
                      values = cols) +
    geom_hline(yintercept = 50, lty = 2, alpha = 0.6)
}

ref_plot <- function(ca = FALSE, df = dfx, treure = FALSE, size = 3){
  indy_dict <- tibble(P34 = c(1, 2, 98,99),
                      indy = c('Independentistes',
                               'Unionistes',
                               'NS/NC',
                               'NS/NC'))
  
  # P38_R is party question
  party_dict <- tibble(P38_R = c(1, 3, 4, 6, 10, 21, 22, 80, 93, 96, 98),
                       partit = c('PPC','ERC', 'PSC', "C's", 'CUP', 'JxCat',
                                  'CECP', rep('Altre/NS/NC', 4)))
  
  sov_dict <- tibble(P35 = c(1,2,3,4,5, 98, 99),
                     sov = factor(c('Molt d\'acord',
                                    'Bastant d\'acord',
                                    'Ni d\'acord ni en desacord/NS/NC',
                                    'Bastant en desacord',
                                    'Molt en desacord',
                                    'Ni d\'acord ni en desacord/NS/NC',
                                    'Ni d\'acord ni en desacord/NS/NC'),
                                  levels = c('Molt d\'acord',
                                             'Bastant d\'acord',
                                             'Ni d\'acord ni en desacord/NS/NC',
                                             'Bastant en desacord',
                                             'Molt en desacord')))
  
  if(!ca){
    sov_dict$sov <- factor(sov_dict$sov,
                           levels = levels(sov_dict$sov),
                           labels = c('Strongly\nagree',
                                      'Agree',
                                      'Neither agree\nnor disagree\nor not sure',
                                      'Disagree',
                                      'Strongly\ndisagree'))
  } else {
    sov_dict$sov <- factor(sov_dict$sov,
                           levels = levels(sov_dict$sov),
                           labels = c('Molt\nd\'acord',
                                      'Bastant\nd\'acord',
                                      'Ni d\'acord\nni en desacord\no NS/NC',
                                      'Bastant en\ndesacord',
                                      'Molt en\ndesacord'))
  }
  
  df <- left_join(df, party_dict)
  df <- left_join(df, indy_dict)
  df <- left_join(df, sov_dict)
  
  pd <- df %>%
    group_by(indy = sov) %>%
    tally %>%
    ungroup
  if(treure){
    pd <- pd %>% filter(!indy %in% c("i d\'acord\nni en desacord\no NS/NC", 'Neither agree\nnor disagree\nor not sure'))
  }
  pd <- pd %>%
    mutate(p = n / sum(n) * 100)
  
  
  if(ca){
    the_labs <- labs(title = 'Grau d\'acord amb la frase següent',
                     subtitle = "'Els catalans i les catalanes tenen dret a decidir el\nseu futur com a país votant en un referèndum'",
                     caption = 'Mostra: 1.500 residents de Catalunya amb ciutadania espanyola.\nEnquesta BOP/CEO, Ronda 1, març 2019. Joe Brew | @joethebrew | www.vilaweb.cat.')
  } else {
    the_labs <- labs(title = 'Agreement with below phrase:',
                     subtitle = "'Catalans have a right to decide their future as a country through a referendum'",
                     
                     
                     caption = 'Sample: 1.500 residents of Catalonia with Spanish citizenship.\nBOP/CEO survey, Round 1, March 2019. Joe Brew | @joethebrew | www.vilaweb.cat.')
  }
  
  ggplot(data = pd,
         aes(x = indy,
             y = p)) +
    geom_bar(stat = 'identity',
             fill = 'black',
             alpha = 0.7) +
    theme_vilaweb() +
    geom_text(aes(label = paste0(round(p, digits = 1), '%'),
                  y = p - 4),
              size = size,
              color = 'white',
              alpha = 0.6) +
    labs(x = '',
         y = '%') +
    the_labs +
    theme(axis.text.x = element_text(size = 14),
          plot.caption = element_text(hjust = 1))
}

# Analysis of vote for 28a per CEO predictions
analyze <- function(){
  df <- dfx
  df$intencio <- df$P44
}
