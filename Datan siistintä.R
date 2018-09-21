library(dplyr)
library(tidyr)
library(statfitools)
library(pxweb)

########################################################################################################

# Apufunktio 
# Funktio, joka lisää halutun kuntaluokituksen dataan

kuntaluokka <- function(kunta, luokittelu, vuosi = 2017) 
{  
  vuosi <- as.character(vuosi)
  url = paste("http://www.tilastokeskus.fi/static/media/uploads/meta/luokitukset/kooste_",
              vuosi,
              "_kaikki_kielet.xlsx", sep = "")
  
  kuntaluokitukset <- statfitools::sf_get_reg_keytable(url = url)
  keskuskunnat <- statfitools::keskuskunnat
  kuntaluokitukset <- dplyr::left_join(kuntaluokitukset, keskuskunnat, by = "Kunta")
  kuntaluokitukset$Keskusryhma[is.na(kuntaluokitukset$Keskusryhma)] <- "muu"
  
  if(!(luokittelu %in% names(kuntaluokitukset))) {
    stop("Desired luokittelu not in kuntaluokitukset.")
  }
  if(any(!(kunta %in% kuntaluokitukset$Kunta))) {
    not_found <- kunta[!(kunta %in% kuntaluokitukset$Kunta)]
    error_msg <- paste("The element(s)", 
                       paste(not_found, collapse = ", "),
                       "in input kunta not recognized.")
    stop(error_msg)
  }
  
  output <- sapply(kunta, 
                   function(y) do.call(`$`, list(kuntaluokitukset, luokittelu))[kuntaluokitukset$Kunta == y])
  names(output) <- NULL
  unlist(output)
}

###########################################################################################################

data0 <- 
  get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/tym/tyonv/statfin_tyonv_pxt_001.px",
                 dims = list(Kuukausi = c('*'),
                             Alue2018 = c('SSS', '005', '009', '010', '016', '018', '019', '020', '035', '043', '046', '047', '049', '050', '051', '052', '060', '061', '062', '065', '069', '071', '072', '074', '075', '076', '077', '078', '079', '081', '082', '086', '090', '091', '092', '097', '098', '099', '102', '103', '105', '106', '108', '109', '111', '139', '140', '142', '143', '145', '146', '148', '149', '151', '152', '153', '165', '167', '169', '170', '171', '172', '176', '177', '178', '179', '181', '182', '186', '202', '204', '205', '208', '211', '213', '214', '216', '217', '218', '224', '226', '230', '231', '232', '233', '235', '236', '239', '240', '241', '244', '245', '249', '250', '256', '257', '260', '261', '263', '265', '271', '272', '273', '275', '276', '280', '284', '285', '286', '287', '288', '290', '291', '295', '297', '300', '301', '304', '305', '309', '312', '316', '317', '318', '320', '322', '398', '399', '400', '402', '403', '405', '407', '408', '410', '416', '417', '418', '420', '421', '422', '423', '425', '426', '430', '433', '434', '435', '436', '438', '440', '441', '444', '445', '475', '478', '480', '481', '483', '484', '489', '491', '494', '495', '498', '499', '500', '503', '504', '505', '507', '508', '529', '531', '535', '536', '538', '541', '543', '545', '560', '561', '562', '563', '564', '576', '577', '578', '580', '581', '583', '584', '588', '592', '593', '595', '598', '599', '601', '604', '607', '608', '609', '611', '614', '615', '616', '619', '620', '623', '624', '625', '626', '630', '631', '635', '636', '638', '678', '680', '681', '683', '684', '686', '687', '689', '691', '694', '697', '698', '700', '702', '704', '707', '710', '729', '732', '734', '736', '738', '739', '740', '742', '743', '746', '747', '748', '749', '751', '753', '755', '758', '759', '761', '762', '765', '766', '768', '771', '777', '778', '781', '783', '785', '790', '791', '831', '832', '833', '834', '837', '844', '845', '846', '848', '849', '850', '851', '853', '854', '857', '858', '859', '886', '887', '889', '890', '892', '893', '895', '905', '908', '911', '915', '918', '921', '922', '924', '925', '927', '931', '934', '935', '936', '941', '946', '976', '977', '980', '981', '989', '992', 'MK01', 'MK02', 'MK04', 'MK05', 'MK06', 'MK07', 'MK08', 'MK09', 'MK10', 'MK11', 'MK12', 'MK13', 'MK14', 'MK15', 'MK16', 'MK17', 'MK18', 'MK19', 'MK21'),
                             Muuttujat = c('TYOTTOMAT', 'TYOVOIMA')),
                 clean = TRUE)

data0 <- data0 %>% mutate(kuukausi = paste(substring(Kuukausi, 6,7), "01", sep = "-"),
                          vuosi = substring(Kuukausi, 1,4)) %>%
                   mutate(time = as.Date(paste(vuosi, kuukausi, sep = "-"))) %>%
                   select(-Kuukausi, -kuukausi, -vuosi) %>%
                   spread(Muuttujat, values) %>%
                   mutate(Tyovoima = Työvoima,
                          Tyottomat = Työttömät) %>%
                   mutate(Tyolliset = Tyovoima - Tyottomat) %>%
                   mutate(Tyottomyysaste = Tyottomat/Tyovoima) %>%
                   gather(value_type, value, c("Tyottomat", "Tyovoima", "Tyolliset", "Tyottomyysaste")) %>%
                   mutate(alue_tyyppi = unlist(sapply(strsplit(as.character(Alue2018), split = " "), tail, 1))) %>%
                   mutate(alue = sapply(sapply(strsplit(as.character(Alue2018), split = " "), head, -1), 
                                        paste, sep= " ", collapse = "")) %>%
                   mutate(alue = ifelse(alue == "KOKO", "Koko maa", alue)) %>%
                   mutate(alue_tyyppi = ifelse(alue_tyyppi == "MAA", "Koko maa", alue_tyyppi)) %>%
                   select(time, alue, alue_tyyppi, value_type, value) 

#####################################################################

##############################################################################################################


# Laske trendit ja kausitasoitukset

data0 <- data0 %>% spread(value_type, value) %>%
                   group_by(alue, alue_tyyppi) %>%
                   mutate(Tyottomat_sa = sa_series(Tyottomat, time),
                          Tyottomat_trend = trend_series(Tyottomat, time),
                          Tyolliset_sa = sa_series(Tyolliset,time),
                          Tyolliset_trend = trend_series(Tyolliset, time),
                          Tyottomyysaste_sa = sa_series(Tyottomyysaste, time),
                          Tyottomyysaste_trend = trend_series(Tyottomyysaste, time)) %>%
                   ungroup() %>%
                   gather(value_type, value, c("Tyottomat", "Tyottomat_sa", "Tyottomat_trend",
                                               "Tyolliset", "Tyolliset_sa", "Tyolliset_trend",
                                               "Tyottomyysaste", "Tyottomyysaste_sa", "Tyottomyysaste_trend"))

# Luo datasetit kunnille, maakunnille ja koko maalle

data_maakunnat <- data0 %>% filter(alue_tyyppi %in% c("Maakunta")) %>% select(-alue_tyyppi) %>%
                          mutate(alue = ifelse(alue == "Ahvenanmaa", "Ahvenanmaa - Åland", as.character(alue)))
data_kunnat <- data0 %>% filter(alue_tyyppi %in% c("Kunta")) %>% 
                         select(-alue_tyyppi) %>%
                         mutate(alue = ifelse(alue == "Koskitl", "Koski Tl", alue)) %>%
                         mutate(alue = ifelse(alue == "Pedersörenkunta", "Pedersören kunta", alue)) %>%
                         mutate(alue = ifelse(alue == "Maarianhamina-Mariehamn", "Maarianhamina", alue)) %>%
                         mutate(Maakunta = kuntaluokka(alue, "Maakunta")) 
data_kokomaa <- data0 %>% filter(alue_tyyppi == "Koko maa") %>% select(-alue_tyyppi)
                    
setwd(paste(getwd(), "/App/data", sep = ""))
saveRDS(data_maakunnat, file = "maakuntadata.rds")
saveRDS(data_kunnat, file = "kuntadata.rds")
saveRDS(data_kokomaa, file = "kokomaadata.rds")




saveRDS(data0, file = "työllisyysdata.rds")


#######################################################################################################
# Väestön määrä

väestö_data <- 
  get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/vaerak/statfin_vaerak_pxt_004.px",
                 dims = list(Alue = c('020', '005', '009', '010', '016', '018', '019', '035', '043', '046', '047', '049', '050', '051', '052', '060', '061', '062', '065', '069', '071', '072', '074', '075', '076', '077', '078', '079', '081', '082', '086', '111', '090', '091', '097', '098', '099', '102', '103', '105', '106', '108', '109', '139', '140', '142', '143', '145', '146', '153', '148', '149', '151', '152', '165', '167', '169', '170', '171', '172', '176', '177', '178', '179', '181', '182', '186', '202', '204', '205', '208', '211', '213', '214', '216', '217', '218', '224', '226', '230', '231', '232', '233', '235', '236', '239', '240', '320', '241', '322', '244', '245', '249', '250', '256', '257', '260', '261', '263', '265', '271', '272', '273', '275', '276', '280', '284', '285', '286', '287', '288', '290', '291', '295', '297', '300', '301', '304', '305', '312', '316', '317', '318', '398', '399', '400', '407', '402', '403', '405', '408', '410', '416', '417', '418', '420', '421', '422', '423', '425', '426', '444', '430', '433', '434', '435', '436', '438', '440', '441', '475', '478', '480', '481', '483', '484', '489', '491', '494', '495', '498', '499', '500', '503', '504', '505', '508', '507', '529', '531', '535', '536', '538', '541', '543', '545', '560', '561', '562', '563', '564', '309', '576', '577', '578', '445', '580', '581', '599', '583', '854', '584', '588', '592', '593', '595', '598', '601', '604', '607', '608', '609', '611', '638', '614', '615', '616', '619', '620', '623', '624', '625', '626', '630', '631', '635', '636', '678', '710', '680', '681', '683', '684', '686', '687', '689', '691', '694', '697', '698', '700', '702', '704', '707', '729', '732', '734', '736', '790', '738', '739', '740', '742', '743', '746', '747', '748', '791', '749', '751', '753', '755', '758', '759', '761', '762', '765', '766', '768', '771', '777', '778', '781', '783', '831', '832', '833', '834', '837', '844', '845', '846', '848', '849', '850', '851', '853', '857', '858', '859', '886', '887', '889', '890', '892', '893', '895', '785', '905', '908', '911', '092', '915', '918', '921', '922', '924', '925', '927', '931', '934', '935', '936', '941', '946', '976', '977', '980', '981', '989', '992'),
                             Ikä = c('015', '016', '017', '018', '019', '020', '021', '022', '023', '024', '025', '026', '027', '028', '029', '030', '031', '032', '033', '034', '035', '036', '037', '038', '039', '040', '041', '042', '043', '044', '045', '046', '047', '048', '049', '050', '051', '052', '053', '054', '055', '056', '057', '058', '059', '060', '061', '062', '063', '064'),
                             Sukupuoli = c('S'),
                             Vuosi = c('*'),
                             Tiedot = c('*')),
                 clean = TRUE)

väestö_data <- väestö_data %>% group_by(Alue, Vuosi) %>%
  summarize(values = sum(values)) %>%
  ungroup() %>%
  mutate(time = as.Date(paste(Vuosi, "01-01", sep = "-"))) %>%
  mutate(kunta = Alue) %>%
  mutate(väestömäärä = values) %>%
  select(time, kunta, väestömäärä) %>%
  filter(time >= as.Date("2006-01-01"))

väestö_data <-väestö_data %>% mutate(maakunta = kuntaluokka(kunta, "Maakunta"))
m = 2
while(m < 13) {
  väestö_data_temp <- väestö_data %>% mutate(time = as.Date(paste(substring(as.character(time),1,4),
                                                                  ifelse(m < 10,"-0","-"), m, "-01",m, sep = "")))
  väestö_data <- rbind(väestö_data, väestö_data_temp)
  m = m +1
}



