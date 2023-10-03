esus<-function(){

devtools::install_github("https://github.com/covid19br/nowcaster")
install.packages("sn")
require(sn)
require(readr)  
setwd("~/nowcasting_note")
require(foreign)
require(dplyr)
require(nowcaster)
require(tidyverse)
require(lubridate)


dengue <- read.csv2("http://sistemas.saude.rj.gov.br/tabnetbd/dash/dengue.csv",header = F)
colnames<-c( 'dt_digita', 'dt_sin_pri', 'dt_notifica','muni_res', 'class_fin', 'evolucao')
colnames(dengue)<-colnames

require(nowcasting)

dengue<-subset(dengue,dengue$class_fin!=5)


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

banco$Regiao_residencia<-droplevels(banco$Regiao_residencia)


cast<-function(x){
dengue <-
  nowcasting_inla(
    dataset = x,
    date_onset = "reference_date",
    date_report = "report_date",
    data.by.week = T  )

split_data <- split(banco, banco$Regiao_residencia)

estado<-cast(banco)
big<-cast(split_data[["Baía de Ilha Grande"]])
noroeste<-cast(split_data[["Noroeste"]])
bl<-cast(split_data[["Baixada Litorânea"]])
cs<-cast(split_data[["Centro Sul"]])
mp<-cast(split_data[["Médio Paraíba"]])
metro1<-cast(split_data[["Metropolitana I"]])
serrana<-cast(split_data[["Serrana"]])
norte<-cast(split_data[["Norte"]])
metro2<-cast(split_data[["Metropolitana II"]])

  list(big,noroeste,bl,cs,mp,metro1,metro2,serrana,norte)
}
  
