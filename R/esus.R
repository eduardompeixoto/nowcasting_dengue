esus<-function(){

  install.packages("purrr")
  require(purrr)
  install.packages("ggplot2")
  require(ggplot2)
  devtools::install_github("https://github.com/covid19br/nowcaster")
  install.packages("sn")
  require(sn)
  require(readr)  
  require(foreign)
  require(dplyr)
  require(nowcaster)
  require(lubridate)
  require(dplyr)
  require(stringr)
  
  
  dengue <- read.csv2("http://sistemas.saude.rj.gov.br/tabnetbd/dash/dengue.csv",header = F)
  colnames<-c( 'dt_digita', 'dt_sin_pri', 'dt_notifica','muni_res', 'class_fin', 'evolucao')
  colnames(dengue)<-colnames
  
  
  dengue<-subset(dengue,dengue$class_fin!=5|is.na(dengue$class_fin))
  dengue<-subset(dengue, substr(dengue$muni_res,1,2)=='33')
  
  a<-dengue
  
  a$dt_sin_pri<- paste(substr(a$dt_sin_pri, 1, 4),
                       
                       substr(a$dt_sin_pri, 5, 6) ,
                       
                       substr(a$dt_sin_pri, 7, 8),sep="-" )
  
  
  
  a$reference_date=as.Date(a$dt_sin_pri)
  a$report_date=as.Date(a$dt_digita)
  
  banco<-a
  
  banco$municipio_residencia<-recode_factor(  banco$muni_res,
                                              "330010"="ANGRA DOS REIS-RJ",
                                              "330015"="APERIBE-RJ",
                                              "330020"="ARARUAMA-RJ",
                                              "330022"="AREAL-RJ",
                                              "330023"="ARMACAO DOS BUZIOS-RJ",
                                              "330025"="ARRAIAL DO CABO-RJ",
                                              "330030"="Barra do Piraí-RJ",
                                              "330040"="BARRA MANSA-RJ",
                                              "330045"="BELFORD ROXO-RJ",
                                              "330050"="BOM JARDIM-RJ",
                                              "330060"="BOM JESUS DO ITABAPOANA-RJ",
                                              "330070"="CABO FRIO-RJ",
                                              "330080"="CACHOEIRAS DE MACACU-RJ",
                                              "330090"="CAMBUCI-RJ",
                                              "330093"="CARAPEBUS-RJ",
                                              "330095"="COMENDADOR LEVY GASPARIAN-RJ",
                                              "330100"="CAMPOS DOS GOYTACAZES-RJ",
                                              "330110"="CANTAGALO-RJ",
                                              "330115"="CARDOSO MOREIRA-RJ",
                                              "330120"="CARMO-RJ",
                                              "330130"="CASIMIRO DE ABREU-RJ",
                                              "330140"="CONCEICAO DE MACABU-RJ",
                                              "330150"="CORDEIRO-RJ",
                                              "330160"="DUAS BARRAS-RJ",
                                              "330170"="DUQUE DE CAXIAS-RJ",
                                              "330180"="ENGENHEIRO PAULO DE FRONTIN-RJ",
                                              "330185"="GUAPIMIRIM-RJ",
                                              "330187"="IGUABA GRANDE-RJ",
                                              "330190"="Itaboraí-RJ",
                                              "330200"="ITAGUAI-RJ",
                                              "330205"="ITALVA-RJ",
                                              "330210"="ITAOCARA-RJ",
                                              "330220"="ITAPERUNA-RJ",
                                              "330225"="ITATIAIA-RJ",
                                              "330227"="JAPERI-RJ",
                                              "330230"="LAJE DO MURIAE-RJ",
                                              "330240"="MACAE-RJ",
                                              "330245"="MACUCO-RJ",
                                              "330250"="MAGE-RJ",
                                              "330260"="MANGARATIBA-RJ",
                                              "330270"="MARICA-RJ",
                                              "330280"="MENDES-RJ",
                                              "330285"="MESQUITA-RJ",
                                              "330290"="MIGUEL PEREIRA-RJ",
                                              "330300"="MIRACEMA-RJ",
                                              "330310"="NATIVIDADE-RJ",
                                              "330320"="NILOPOLIS-RJ",
                                              "330330"="NITEROI-RJ",
                                              "330340"="NOVA FRIBURGO-RJ",
                                              "330350"="área de Nova Iguaçu",
                                              "330360"="PARACAMBI-RJ",
                                              "330370"="PARAIBA DO SUL-RJ",
                                              "330380"="PARATY-RJ",
                                              "330385"="PATY DO ALFERES-RJ",
                                              "330390"="PETROPOLIS-RJ",
                                              "330395"="PINHEIRAL-RJ",
                                              "330400"="PIRAI-RJ",
                                              "330410"="PORCIUNCULA-RJ",
                                              "330411"="PORTO REAL-RJ",
                                              "330412"="QUATIS-RJ",
                                              "330414"="QUEIMADOS-RJ",
                                              "330415"="QUISSAMA-RJ",
                                              "330420"="RESENDE-RJ",
                                              "330430"="RIO BONITO-RJ",
                                              "330440"="RIO CLARO-RJ",
                                              "330450"="RIO DAS FLORES-RJ",
                                              "330452"="RIO DAS OSTRAS-RJ",
                                              "330455"="RIO DE JANEIRO-RJ",
                                              "330460"="SANTA MARIA MADALENA-RJ",
                                              "330470"="SANTO ANTONIO DE PADUA-RJ",
                                              "330475"="SAO FRANCISCO DE ITABAPOANA-RJ",
                                              "330480"="SAO FIDELIS-RJ",
                                              "330490"="SAO GONCALO-RJ",
                                              "330500"="SAO JOAO DA BARRA-RJ",
                                              "330510"="SAO JOAO DE MERITI-RJ",
                                              "330513"="SAO JOSE DE UBA-RJ",
                                              "330515"="SAO JOSE DO VALE DO RIO PRETO-RJ",
                                              "330520"="SAO PEDRO DA ALDEIA-RJ",
                                              "330530"="SAO SEBASTIAO DO ALTO-RJ",
                                              "330540"="SAPUCAIA-RJ",
                                              "330550"="SAQUAREMA-RJ",
                                              "330555"="SEROPEDICA-RJ",
                                              "330560"="SILVA JARDIM-RJ",
                                              "330570"="SUMIDOURO-RJ",
                                              "330575"="TANGUA-RJ",
                                              "330580"="TERESOPOLIS-RJ",
                                              "330590"="TRAJANO DE MORAES-RJ",
                                              "330600"="TRES RIOS-RJ",
                                              "330610"="VALENCA-RJ",
                                              "330615"="VARRE-SAI-RJ",
                                              "330620"="VASSOURAS-RJ",
                                              "330630"="VOLTA REDONDA-RJ")
  
  
  
  
  #tabela casos p ano dos munic
  #write.csv(table(d$MUNICIPIO_DE_RESI,d$ANO_EPI_SINTOMAS),"Anos.csv")
  
  #####Criando a variável Região de Residência
  
  banco$Regiao_residencia<-recode(  banco$municipio_residencia,
                                    'ANGRA DOS REIS-RJ'='Baía de Ilha Grande',
                                    'APERIBE-RJ'='Noroeste',
                                    'ARARUAMA-RJ'='Baixada Litorânea',
                                    'AREAL-RJ'='Centro Sul',
                                    'ARMACAO DOS BUZIOS-RJ'='Baixada Litorânea',
                                    'ARRAIAL DO CABO-RJ'='Baixada Litorânea',
                                    'Barra do Piraí-RJ'='Médio Paraíba',
                                    'BARRA MANSA-RJ'='Médio Paraíba',
                                    'BELFORD ROXO-RJ'='Metropolitana I',
                                    'BOM JARDIM-RJ'='Serrana',
                                    'BOM JESUS DO ITABAPOANA-RJ'='Noroeste',
                                    'CABO FRIO-RJ'='Baixada Litorânea',
                                    'CACHOEIRAS DE MACACU-RJ'='Serrana',
                                    'CAMBUCI-RJ'='Noroeste',
                                    'CARAPEBUS-RJ'='Norte',
                                    'COMENDADOR LEVY GASPARIAN-RJ'='Centro Sul',
                                    'CAMPOS DOS GOYTACAZES-RJ'='Norte',
                                    'CANTAGALO-RJ'='Serrana',
                                    'CARDOSO MOREIRA-RJ'='Noroeste',
                                    'CARMO-RJ'='Serrana',
                                    'CASIMIRO DE ABREU-RJ'='Baixada Litorânea',
                                    'CONCEICAO DE MACABU-RJ'='Norte',
                                    'CORDEIRO-RJ'='Serrana',
                                    'DUAS BARRAS-RJ'='Serrana',
                                    'DUQUE DE CAXIAS-RJ'='Metropolitana I',
                                    'ENGENHEIRO PAULO DE FRONTIN-RJ'='Centro Sul',
                                    'GUAPIMIRIM-RJ'='Serrana',
                                    'IGUABA GRANDE-RJ'='Baixada Litorânea',
                                    'Itaboraí-RJ'='Metropolitana II',
                                    'ITAGUAI-RJ'='Metropolitana I',
                                    'ITALVA-RJ'='Noroeste',
                                    'ITAOCARA-RJ'='Noroeste',
                                    'ITAPERUNA-RJ'='Noroeste',
                                    'ITATIAIA-RJ'='Médio Paraíba',
                                    'JAPERI-RJ'='Metropolitana I',
                                    'LAJE DO MURIAE-RJ'='Noroeste',
                                    'MACAE-RJ'='Norte',
                                    'MACUCO-RJ'='Serrana',
                                    'MAGE-RJ'='Metropolitana I',
                                    'MANGARATIBA-RJ'='Baía de Ilha Grande',
                                    'MARICA-RJ'='Metropolitana II',
                                    'MENDES-RJ'='Centro Sul',
                                    'MESQUITA-RJ'='Metropolitana I',
                                    'MIGUEL PEREIRA-RJ'='Centro Sul',
                                    'MIRACEMA-RJ'='Noroeste',
                                    'NATIVIDADE-RJ'='Noroeste',
                                    'NILOPOLIS-RJ'='Metropolitana I',
                                    'NITEROI-RJ'='Metropolitana II',
                                    'NOVA FRIBURGO-RJ'='Serrana',
                                    'área de Nova Iguaçu'='Metropolitana I',
                                    'PARACAMBI-RJ'='Centro Sul',
                                    'PARAIBA DO SUL-RJ'='Centro Sul',
                                    'PARATY-RJ'='Baía de Ilha Grande',
                                    'PATY DO ALFERES-RJ'='Centro Sul',
                                    'PETROPOLIS-RJ'='Serrana',
                                    'PINHEIRAL-RJ'='Médio Paraíba',
                                    'PIRAI-RJ'='Médio Paraíba',
                                    'PORCIUNCULA-RJ'='Noroeste',
                                    'PORTO REAL-RJ'='Médio Paraíba',
                                    'QUATIS-RJ'='Médio Paraíba',
                                    'QUEIMADOS-RJ'='Metropolitana I',
                                    'QUISSAMA-RJ'='Norte',
                                    'RESENDE-RJ'='Médio Paraíba',
                                    'RIO BONITO-RJ'='Metropolitana II',
                                    'RIO CLARO-RJ'='Médio Paraíba',
                                    'RIO DAS FLORES-RJ'='Médio Paraíba',
                                    'RIO DAS OSTRAS-RJ'='Baixada Litorânea',
                                    'RIO DE JANEIRO-RJ'='Metropolitana I',
                                    'SANTA MARIA MADALENA-RJ'='Serrana',
                                    'SANTO ANTONIO DE PADUA-RJ'='Noroeste',
                                    'SAO FRANCISCO DE ITABAPOANA-RJ'='Norte',
                                    'SAO FIDELIS-RJ'='Norte',
                                    'SAO GONCALO-RJ'='Metropolitana II',
                                    'SAO JOAO DA BARRA-RJ'='Norte',
                                    'SAO JOAO DE MERITI-RJ'='Metropolitana I',
                                    'SAO JOSE DE UBA-RJ'='Noroeste',
                                    'SAO JOSE DO VALE DO RIO PRETO-RJ'='Serrana',
                                    'SAO PEDRO DA ALDEIA-RJ'='Baixada Litorânea',
                                    'SAO SEBASTIAO DO ALTO-RJ'='Serrana',
                                    'SAPUCAIA-RJ'='Centro Sul',
                                    'SAQUAREMA-RJ'='Baixada Litorânea',
                                    'SEROPEDICA-RJ'='Metropolitana I',
                                    'SILVA JARDIM-RJ'='Metropolitana II',
                                    'SUMIDOURO-RJ'='Serrana',
                                    'TANGUA-RJ'='Metropolitana II',
                                    'TERESOPOLIS-RJ'='Serrana',
                                    'TRAJANO DE MORAES-RJ'='Serrana',
                                    'TRES RIOS-RJ'='Centro Sul',
                                    'VALENCA-RJ'='Médio Paraíba',
                                    'VARRE-SAI-RJ'='Noroeste',
                                    'VASSOURAS-RJ'='Centro Sul',
                                    'VOLTA REDONDA-RJ'='Médio Paraíba')
  
  banco$municipio_residencia<-droplevels(banco$municipio_residencia)
  banco$dt_notifica<-str_replace_all(banco$dt_notifica," ","")
  
  banco<-subset(banco, lubridate::epiweek(as.Date(banco$dt_notifica,format="%Y%m%d"))<lubridate::epiweek(Sys.Date()))    
  
  cast<-function(x){
    dengue <-
      nowcasting_inla(
        dataset = x,
        date_onset = "reference_date",
        date_report = "report_date",
        data.by.week = T  )
    
    
    dados_by_week_n <- dengue |>
      pluck("data") |>
      ungroup() |>
      dplyr::mutate(epiweek = epiweek(date_onset)) |>
      dplyr::filter(date_onset >= as.Date("2023-1-01"))
    
    dengue |>
      pluck("total")  |>
      mutate(epiweek = epiweek(dt_event)) |>
      filter(dt_event >= (max(dt_event)-60)) |>
      ggplot(aes(x = epiweek, y = Median,
                 col = 'Nowcasting (data de início de sintomas vs data de Digitação)')) +
      geom_line(data = dados_by_week_n,
                aes(x = epiweek,
                    y = observed,
                    col = 'Casos'),
                linewidth = 1.2) +
      geom_ribbon(aes(ymin = LI, ymax = LS, col = NA),
                  alpha = 0.2,
                  show.legend = F) +
      geom_line(linewidth = 1.2) +
      theme_bw() +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        
      ) +
      scale_color_manual(values = c('black','red'),
                         name = '') +
      labs(x = 'Semana Epidemiológica',
           y = 'Nº Casos',
           title = "Nowcasting de casos de Dengue  (data início de sintomas X data de digitação), Rio de Janeiro, 2023.")
    
  }
  
  split_data <- split(banco, banco$municipio_residencia)
  # Apply the cast function to each part of the split data

 
  



ANGRA_DOS_REIS<-cast(split_data[[1]])
APERIBE<-cast(split_data[[2]])
ARARUAMA<-cast(split_data[[3]])
# AREAL<-cast(split_data[[4]])
ARMACAO_DOS_BUZIOS<-cast(split_data[[5]])
ARRAIAL_DO_CABO<-cast(split_data[[6]])
Barra_do_Piraí<-cast(split_data[[7]])
BARRA_MANSA<-cast(split_data[[8]])
BELFORD_ROXO<-cast(split_data[[9]])
#BOM_JARDIM<-cast(split_data[[10]])
BOM_JESUS_DO_ITABAPOANA<-cast(split_data[[11]])
CABO_FRIO<-cast(split_data[[12]])
CACHOEIRAS_DE_MACACU<-cast(split_data[[13]])
CAMBUCI<-cast(split_data[[14]])
CARAPEBUS<-cast(split_data[[15]])
#COMENDADOR_LEVY_GASPARIAN<-cast(split_data[[16]])
CAMPOS_DOS_GOYTACAZES<-cast(split_data[[17]])
# CANTAGALO<-cast(split_data[[18]])
CARDOSO_MOREIRA<-cast(split_data[[19]])
CARMO<-cast(split_data[[20]])
CASIMIRO_DE_ABREU<-cast(split_data[[21]])
CONCEICAO_DE_MACABU<-cast(split_data[[22]])
CORDEIRO<-cast(split_data[[23]])
# DUAS_BARRAS<-cast(split_data[[24]])
DUQUE_DE_CAXIAS<-cast(split_data[[25]])
ENGENHEIRO_PAULO_DE_FRONTIN<-cast(split_data[[26]])
GUAPIMIRIM<-cast(split_data[[27]])
IGUABA_GRANDE<-cast(split_data[[28]])
Itaboraí<-cast(split_data[[29]])
ITAGUAI<-cast(split_data[[30]])
ITALVA<-cast(split_data[[31]])
ITAOCARA<-cast(split_data[[32]])
ITAPERUNA<-cast(split_data[[33]])
ITATIAIA<-cast(split_data[[34]])
JAPERI<-cast(split_data[[35]])
LAJE_DO_MURIAE<-cast(split_data[[36]])
MACAE<-cast(split_data[[37]])
MACUCO<-cast(split_data[[38]])
MAGE<-cast(split_data[[39]])
MANGARATIBA<-cast(split_data[[40]])
MARICA<-cast(split_data[[41]])
MENDES<-cast(split_data[[42]])
MESQUITA<-cast(split_data[[43]])
MIGUEL_PEREIRA<-cast(split_data[[44]])
#MIRACEMA<-cast(split_data[[45]])
NATIVIDADE<-cast(split_data[[46]])
NILOPOLIS<-cast(split_data[[47]])
NITEROI<-cast(split_data[[48]])
NOVA_FRIBURGO<-cast(split_data[[49]])
área_de_Nova_Iguaçu<-cast(split_data[[50]])
PARACAMBI<-cast(split_data[[51]])
#PARAIBA_DO_SUL<-cast(split_data[[52]])
PARATY<-cast(split_data[[53]])
PATY_DO_ALFERES<-cast(split_data[[54]])
PETROPOLIS<-cast(split_data[[55]])
PINHEIRAL<-cast(split_data[[56]])
PIRAI<-cast(split_data[[57]])
PORCIUNCULA<-cast(split_data[[58]])
PORTO_REAL<-cast(split_data[[59]])
QUATIS<-cast(split_data[[60]])
QUEIMADOS<-cast(split_data[[61]])
QUISSAMA<-cast(split_data[[62]])
RESENDE<-cast(split_data[[63]])
RIO_BONITO<-cast(split_data[[64]])
# RIO_CLARO<-cast(split_data[[65]])
#RIO_DAS_FLORES<-cast(split_data[[66]])
RIO_DAS_OSTRAS<-cast(split_data[[67]])
RIO_DE_JANEIRO<-cast(split_data[[68]])
# SANTA_MARIA_MADALENA<-cast(split_data[[69]])
SANTO_ANTONIO_DE_PADUA<-cast(split_data[[70]])
SAO_FRANCISCO_DE_ITABAPOANA<-cast(split_data[[71]])
SAO_FIDELIS<-cast(split_data[[72]])
SAO_GONCALO<-cast(split_data[[73]])
SAO_JOAO_DA_BARRA<-cast(split_data[[74]])
SAO_JOAO_DE_MERITI<-cast(split_data[[75]])
SAO_JOSE_DE_UBA<-cast(split_data[[76]])
# SAO_JOSE_DO_VALE_DO_RIO_PRETO<-cast(split_data[[77]])
SAO_PEDRO_DA_ALDEIA<-cast(split_data[[78]])
#  SAO_SEBASTIAO_DO_ALTO<-cast(split_data[[79]])
SAPUCAIA<-cast(split_data[[80]])
SAQUAREMA<-cast(split_data[[81]])
SEROPEDICA<-cast(split_data[[82]])
# SILVA_JARDIM<-cast(split_data[[83]])
SUMIDOURO<-cast(split_data[[84]])
TANGUA<-cast(split_data[[85]])
#  TERESOPOLIS<-cast(split_data[[86]])
#  TRAJANO_DE_MORAES<-cast(split_data[[87]])
TRES_RIOS<-cast(split_data[[88]])
VALENCA<-cast(split_data[[89]])
VARRE_SAI<-cast(split_data[[90]])
VASSOURAS<-cast(split_data[[91]])
VOLTA_REDONDA<-cast(split_data[[92]])

municipio<-list(
  ANGRA_DOS_REIS,
  APERIBE,
  ARARUAMA,
  #  AREAL,
  ARMACAO_DOS_BUZIOS,
  ARRAIAL_DO_CABO,
  Barra_do_Piraí,
  BARRA_MANSA,
  BELFORD_ROXO,
  # BOM_JARDIM,
  BOM_JESUS_DO_ITABAPOANA,
  CABO_FRIO,
  CACHOEIRAS_DE_MACACU,
  CAMBUCI,
  CARAPEBUS,
  # COMENDADOR_LEVY_GASPARIAN,
  CAMPOS_DOS_GOYTACAZES,
  # CANTAGALO,
  CARDOSO_MOREIRA,
  CARMO,
  CASIMIRO_DE_ABREU,
  CONCEICAO_DE_MACABU,
  CORDEIRO,
  #DUAS_BARRAS,
  DUQUE_DE_CAXIAS,
  ENGENHEIRO_PAULO_DE_FRONTIN,
  GUAPIMIRIM,
  IGUABA_GRANDE,
  Itaboraí,
  ITAGUAI,
  ITALVA,
  ITAOCARA,
  ITAPERUNA,
  ITATIAIA,
  JAPERI,
  LAJE_DO_MURIAE,
  MACAE,
  MACUCO,
  MAGE,
  MANGARATIBA,
  MARICA,
  MENDES,
  MESQUITA,
  MIGUEL_PEREIRA,
  # MIRACEMA,
  NATIVIDADE,
  NILOPOLIS,
  NITEROI,
  NOVA_FRIBURGO,
  área_de_Nova_Iguaçu,
  PARACAMBI,
  # PARAIBA_DO_SUL,
  PARATY,
  PATY_DO_ALFERES,
  PETROPOLIS,
  PINHEIRAL,
  PIRAI,
  PORCIUNCULA,
  PORTO_REAL,
  QUATIS,
  QUEIMADOS,
  QUISSAMA,
  RESENDE,
  RIO_BONITO,
  # RIO_CLARO,
  #  RIO_DAS_FLORES,
  RIO_DAS_OSTRAS,
  RIO_DE_JANEIRO,
 # SANTA_MARIA_MADALENA,
  SANTO_ANTONIO_DE_PADUA,
  SAO_FRANCISCO_DE_ITABAPOANA,
  SAO_FIDELIS,
  SAO_GONCALO,
  SAO_JOAO_DA_BARRA,
  SAO_JOAO_DE_MERITI,
  SAO_JOSE_DE_UBA,
  # SAO_JOSE_DO_VALE_DO_RIO_PRETO,
  SAO_PEDRO_DA_ALDEIA,
  #  SAO_SEBASTIAO_DO_ALTO,
  SAPUCAIA,
  SAQUAREMA,
  SEROPEDICA,
  #SILVA_JARDIM,
  SUMIDOURO,
  TANGUA,
  # TERESOPOLIS,
  #  TRAJANO_DE_MORAES,
  TRES_RIOS,
  VALENCA,
  VARRE_SAI,
  VASSOURAS,
  VOLTA_REDONDA
  
)


split_data <- split(banco, banco$Regiao_residencia)

regiao<-list(
  cast(banco),
  cast(split_data[["Baía de Ilha Grande"]]),
  cast(split_data[["Noroeste"]]),
  cast(split_data[["Baixada Litorânea"]]),
  cast(split_data[["Centro Sul"]]),
  cast(split_data[["Médio Paraíba"]]),
  cast(split_data[["Metropolitana I"]]),
  cast(split_data[["Serrana"]]),
  cast(split_data[["Norte"]]),
  cast(split_data[["Metropolitana II"]])
)



list(regiao, municipio)


}
