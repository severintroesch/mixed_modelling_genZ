
#### -------------- ####
#### GEN Z ANALYSIS ####
#### -------------- ####

# "Wie gut ist die Generation Z? Im Vergleich zu den vorherigen? etc."

### Packages ---------------------------------------------------------------------- ####

library(tidyverse)
library(GGally)
library(xlsx) #data from excel
library(lubridate)
library(car)
library(haven) # data from spss 
library(readxl) # data from excel
library(lme4)
library(ggthemes)
library(readr)


### get ausdauer data ------------------------------------------------------------- ####

### mtb ---

# mtb directly from Excel Datenbank
mtb = read.xlsx("mtb_db_apr19.xlsx", sheetName = "Datenbank", startRow = 41, encoding="UTF-8")
mtb = mtb %>% mutate_each(funs(type.convert(as.character(.)))) %>% tbl_df() # reassigns all datatypes

# keep only rows with tests
mtb <- drop_na(mtb,Name)

# set empty cells to NA
#mtb[mtb == ""] <- NA #nescessary?

# Correct type for Verwerfen_variables
mtb$Verwerfen_submax = parse_character(mtb$Verwerfen_submax)
mtb$Verwerfen_MPO =    parse_character(mtb$Verwerfen_MPO)
mtb$Verwerfen_MMP5 =   parse_character(mtb$Verwerfen_MMP5)
mtb$Verwerfen_MMP1 =   parse_character(mtb$Verwerfen_MMP1)
mtb$Verwerfen_Kap =    parse_character(mtb$Verwerfen_Kap)

# # All possible age and sex categories, necessary to insure that no rows are missing in the export
# allAlter = data.frame(AlterCut = c('(15,19]','(19,21]', '(21,23]', '(23,50]', '(15,19]','(19,21]', '(21,23]', '(23,50]'),
#                       Geschlecht = c('f','f','f','f','m','m','m','m'))


# Set variables to NA if test was flagged with Verwerfen=ja 

#submax
mtb$Verwerfen_submax[is.na(mtb$Verwerfen_submax)] = 'nein'  # first verwerfen = NA zu nein (nicht verwerfen)
submax_vars = names(mtb %>% select(Lac_Vorb_PPT:Proz_Aes_MMP5)) # define variables associated with test
mtb[mtb$Verwerfen_submax =='ja', submax_vars] = NA # if verwerfen = ja set associated variables to NA

#MPO
mtb$Verwerfen_MPO[is.na(mtb$Verwerfen_MPO)] = 'nein'
MPO_vars = names(mtb %>% select(Gang_J_MPO:Proz_MMP5_MPO))
mtb[mtb$Verwerfen_MPO =='ja', MPO_vars] = NA

#MMP5 (inkl. VO2max)
mtb$Verwerfen_MMP5[is.na(mtb$Verwerfen_MMP5)] = 'nein'
MMP5_vars = names(mtb %>% select(P_30s_MMP5:Cad_avg_MMP5,HF_60s_MMP5:Lac_end_MMP5))
mtb[mtb$Verwerfen_MMP5 =='ja', MMP5_vars] = NA


#MMP1
mtb$Verwerfen_MMP1[is.na(mtb$Verwerfen_MMP1)] = 'nein'
MMP1_vars = names(mtb %>% select(Cad_isokinetic:Borg_Abr_MMP1))
mtb[mtb$Verwerfen_MMP1 =='ja', MMP1_vars] = NA

#Kap-Test
mtb$Verwerfen_Kap[is.na(mtb$Verwerfen_Kap)] = 'nein'
Kap_vars = names(mtb %>% select(P_avg_SL_Kap:Weg_tot_abs_Kap))
mtb[mtb$Verwerfen_Kap =='ja', Kap_vars] = NA


# Set Oxycon VO2max values to NA
old_VO2max_vars = names(mtb %>% select(VO2max_mL:RERmax, VO2max_mL_Hbm_g))
mtb[ymd(mtb$Testdatum_1) < ymd(20161001), old_VO2max_vars] = NA #ACHTUNG: for that to work all rows must contain values (no empty rows)



### langlauf --

# langlauf directly from Excel Datenbank
langlauf = read_delim(file = "langlauf_db_apr19.csv",delim = ";", col_names = T, skip = 48, locale = locale(encoding = "ISO-8859-1"))
langlauf <- langlauf[-1,] #drop first, empty row

# keep only rows with tests
langlauf <- drop_na(langlauf,Name)

# set "-" to NA
langlauf[langlauf == "-"] <- NA

# select interesting vars --------------------------------- tbd!!!
#langlauf2 <- langlauf %>% select()


# Correct type for vars


# Set Oxycon VO2max values to NA
old_VO2max_vars_ll = names(langlauf %>% select(91:105))
langlauf[dmy(langlauf$Testdatum) < ymd(20160701), old_VO2max_vars_ll] = NA #ACHTUNG: for that to work all rows must contain values (no empty rows) - wann auf DB gewechselt?



### create sensible "sub-datenbanken" for research questions --

## mtb

mtb2 <- mtb %>% select(1:11,13,30:44,starts_with("VO2max"), starts_with("Hbm"), starts_with("P_avg"), starts_with("Weg_tot"),"PV_ml")
mtb2 <- mtb2 %>% mutate_at(vars(-c(1:9)), funs(as.numeric(as.character(.))))
mtb2$Testdatum_1 <- ymd(mtb2$Testdatum_1)
mtb2$Geburtsdatum_1 <- ymd(mtb2$Geburtsdatum_1)
mtb2 #data types correct?

# filter kader / sport
mtb2 <- mtb2 %>% filter(Sportart_1 == "MTB XCO")
summary(mtb2) #sinnvoll? ja.

# avg power in kap test
mtb2 <- mtb2 %>% mutate(pkap_W = (P_avg_SL_Kap*4 + P_avg_R1_Kap*10 + P_avg_R2_Kap*10)/24)


## langlauf

langlauf2 <- langlauf %>% select(1:11,13,starts_with("VO2"), starts_with("Hb"), "Weg Tot", "Kader", "Sportart", starts_with("Vavg "), "PV [ml]", "Stufe_2") #Stufe_2 ist Ans-Stufe
langlauf2 <- langlauf2 %>% mutate_at(vars(-c(1:4,12,20, 21)), funs(as.numeric(as.character(.))))
langlauf2$Name <- factor(langlauf2$Name)
langlauf2$Sportart <- factor(langlauf2$Sportart)
langlauf2$Vorname <- factor(langlauf2$Vorname)
langlauf2$Kader <- factor(langlauf2$Kader)
langlauf2$Geschlecht <- factor(langlauf2$Geschlecht)
langlauf2$Testdatum <- dmy(langlauf2$Testdatum)
langlauf2$Geburtsdatum <- dmy(langlauf2$Geburtsdatum)
langlauf2 #data types correct?

# filter kader / sport
langlauf2 <- langlauf2 %>% filter(Sportart == "Langlauf")
langlauf2[langlauf2$Kader == "U-20" & !is.na(langlauf2$Kader), "Kader"] <- "U20" #U-20 durch U20 ersetzen
langlauf2[langlauf2$Kader == "U-23" & !is.na(langlauf2$Kader), "Kader"] <- "U23" #U-20 durch U20 ersetzen
langlauf2[((langlauf2$Kader == "A-Kader") | (langlauf2$Kader == "Elite (NM und A-Kader)") | (langlauf2$Kader == "NM und A-Kader")) & (!is.na(langlauf2$Kader)) , "Kader"] <- "NM und A-Kader" #elite, achtung: & vor | 

langlauf2[langlauf2$Geschlecht == "f " & !is.na(langlauf2$Geschlecht), "Geschlecht"] <- "f" #"f " durch f ersetzen

langlauf2 <- langlauf2 %>% filter(Kader != "Hobby")

langlauf2 %>% filter(Sportart == "Langlauf") %>% select(Kader) %>%  table() #
summary(langlauf2) #sinnvoll? ja.

# avg power in kap test
langlauf2 <- langlauf2 %>% mutate(pkap_W = ((5 * `Vavg 3°`*(((`Gewicht [kg]`+2.5)*9.81*sin(3/360*2*pi))+ # F Hangabtrieb - 2.5 kg angenommen für ski + schuhe + stöcke
                                                              ((`Gewicht [kg]`+2.5)*9.81*cos(3/360*2*pi)*0.0278)) + # F roll - 0.0278 angenommen als muR
                                               3 * `Vavg 5°`*(((`Gewicht [kg]`+2.5)*9.81*sin(3/360*2*pi))+ # F Hangabtrieb
                                                                ((`Gewicht [kg]`+2.5)*9.81*cos(3/360*2*pi)*0.0278)))/8)) # F roll



## combined mtb & ll for abs hbmass und vo2max, gewicht, kap perf, ... 

comb_ll <- langlauf2 %>% select("Name","Vorname", "Alter","Geburtsdatum", "Testdatum", "Grösse[cm]" ,"Gewicht [kg]" , "%Fett (iDXA)", "FFMI (kg/m2)", "Geschlecht"  , "VO2 [ml/min]","HbMasse [g]" , "Weg Tot", "Kader" , "Sportart","pkap_W" ,"PV [ml]" ,"Stufe_2")
colnames(comb_ll) <- c("last","first","age_y","dob","date_test","height_cm","weight_kg", "bodyfat_perc", "ffmi", "sex" ,  "vo2_abs", "hbmass_abs",  "perf_kap",      "kader" ,    "sport","pkap_W" ,"pv_ml","ans_stufe_ll")

comb_mtb <- mtb2 %>% select("Name","Vorname", "Alter_y_1","Geburtsdatum_1","Testdatum_1", "Grösse_cm_1" ,"Gewicht_kg_1" , "Bereich_ProzFett", "FFMIgesamt", "Geschlecht"  , "VO2max_mL","Hbm_g" , "Weg_tot_Kap", "Kader_1" , "Sportart_1", "pkap_W" ,"PV_ml" )
colnames(comb_mtb) <- c("last","first","age_y","dob","date_test","height_cm","weight_kg", "bodyfat_perc", "ffmi", "sex" ,  "vo2_abs", "hbmass_abs",  "perf_kap",      "kader" ,    "sport","pkap_W" ,"pv_ml")

# join
comb_ll_mtb <- full_join(comb_ll, comb_mtb)

# factorize
comb_ll_mtb$last <- factor(comb_ll_mtb$last)
comb_ll_mtb$first <- factor(comb_ll_mtb$first)
comb_ll_mtb$sex <- factor(comb_ll_mtb$sex)
comb_ll_mtb$kader <- factor(comb_ll_mtb$kader)
comb_ll_mtb$sport <- factor(comb_ll_mtb$sport)

# name with first und last
comb_ll_mtb <- comb_ll_mtb %>% mutate(name = factor(paste(last, first)))

# season-variable
comb_ll_mtb_seas <- comb_ll_mtb %>% mutate(season = if_else(sport == "Langlauf" & month(date_test) %in% c(10,11,12,1,2), true = "in", false = if_else(sport == "MTB XCO" & month(date_test) %in% 2:8, true = "in", false = "off")))


## lean body mass dazu
comb_ll_mtb_seas <- comb_ll_mtb_seas %>% mutate(lbm_kg = (100 - bodyfat_perc)/100 * weight_kg)



## final ausd df
ausd_final <- comb_ll_mtb_seas #%>% select("name","sex","dob","sport" ,"date_test","age_y","weight_kg","height_cm","vo2_abs","perf_kap" ) #select relevant stuff
ausd_final <- ausd_final %>% mutate(oe = factor("ausd"))
ausd_final <- ausd_final %>% mutate(generation = factor(ifelse(year(dob)>1996,"Z",ifelse(year(dob)>1980,"Y","X")))) #generation variable
ausd_final <- ausd_final %>% select("name"  ,     "sex"     ,   "dob"     ,   "sport"    ,  "date_test" , "age_y" , "oe"     ,    "generation"  , "weight_kg" , "vo2_abs"  ,  "perf_kap", "ans_stufe_ll", "hbmass_abs")

columns <- colnames(ausd_final)



### get kraft data (alt) ---------------------------------------------------------- ####

## ski alpin herren - from spss
kraft_ski_m <- read_sav("kraft_skialpin_herren_SPSS_2.sav")
kraft_ski_m <- kraft_ski_m %>% mutate(sex = factor("m"),
                                      name = factor(paste(Name, Vorname)),
                                      sport = factor("ski alpin"),
                                      oe = factor("kraft"),
                                      generation = factor(ifelse(year(Geburtsdatum)>1996,"Z",ifelse(year(Geburtsdatum)>1980,"Y","X"))))

#klaus will folgende vars: Fmax_iso, Fmax_iso_rel, Ej_Pmax_rel_100, Sj_Pmax_rel_100, Ej_Pmax_rel_200, Sj_Pmax_rel_200, Fv0_max 
colselect_ski_m <- c("name"       ,     "sex"       ,      "Geburtsdatum"  ,  "sport"        ,   "Testdat"      ,   "Alter","oe","generation",  "Gewicht"     ,    "Fmax_iso"      ,  "Fmax_iso_rel" ,  "Ej_Pmax_rel_100" ,"Sj_Pmax_rel_100" ,"Ej_Pmax_rel_200" ,"Sj_Pmax_rel_200" ,"Fv0_max")

kraft_ski_m <- kraft_ski_m %>% select(colselect_ski_m) #selection gleich wie ausd
#benennung gleich ausd
colnames(kraft_ski_m) <- c("name"    ,   "sex"    ,    "dob"    ,    "sport"    ,  "date_test" , "age_y"    ,  "oe"     ,    "generation" ,"weight_kg"  ,      "Fmax_iso"  ,      "Fmax_iso_rel"  ,  "Ej_Pmax_rel_100", "Sj_Pmax_rel_100" ,"Ej_Pmax_rel_200" ,"Sj_Pmax_rel_200", "Fv0_max")



## ski alpin damen - from excel
kraft_ski_f <- read_excel("kraft_Ski_alpin_Frauen.xlsx", 
                                     col_types = c("numeric", "numeric", "numeric", 
                                                   "numeric", "text", "text", "text", 
                                                   "date", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric"), 
                                     na = "NA")

#construct variables
kraft_ski_f <- kraft_ski_f %>% mutate(yob = round(year(date(kraft_ski_f$Testdatum))-kraft_ski_f$Alter,0),
                                      sex = factor("f"),
                                      name = CODE,
                                      sport = factor("ski alpin"),
                                      oe = factor("kraft"),
                                      dob = NA,
                                      generation = factor(ifelse(yob>1996,"Z",ifelse(yob>1980,"Y","X"))))

# klaus will: Excel (Frauen): Spalten I, J, M, N, P, Q - cols = (1,2,4,8,9,10,13,14,16,17)

# selection
colselect_ski_f <- c("name" , "sex", "dob","sport","Testdatum" ,"Alter" ,"oe","generation", "Gewicht"  , "IsoTest 100°\r\nFmax_iso\r\n[N]" , "IsoTest 100°\r\nFmax_iso_rel\r\n[N/kg]" , "Elastojump 100%\r\nPmax_rel\r\n[Watt/kg]","Statojump 100%\r\nPmax_rel\r\n[Watt/kg]" , "Elastojump 200%\r\nPmax_rel\r\n[Watt/kg]", "Statojump 200%\r\nPmax_rel\r\n[Watt/kg]" , "Fv0_max\r\n[N]")
kraft_ski_f <- kraft_ski_f %>% select(colselect_ski_f)

#benennung gleich colselect_ski_m
colnames(kraft_ski_f) <- colnames(kraft_ski_m)


## join ski alpin herren und damen - kraft final
kraft_final <- rbind(kraft_ski_f,kraft_ski_m)



### get kraft data (neu) ---------------------------------------------------------- ####

## ski alpin herren - from excel
kraft_ski_m <- read_excel("kraft_mld_neu_m.xlsx", 
                          col_types = c("text", "date", "date", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric"),
                          na = "NA")



#construct variables
kraft_ski_m <- kraft_ski_m %>% mutate(yob = year(date(kraft_ski_m$`Geburtsdatum\r\n[M.T.J]`)),
                                      sex = factor("m"),
                                      name = factor(`Name Vorname`),
                                      sport = factor("ski alpin"),
                                      oe = factor("kraft"),
                                      dob = `Geburtsdatum\r\n[M.T.J]`,
                                      generation = factor(ifelse(yob>1996,"Z",ifelse(yob>1980,"Y","X"))))

# klaus will: s. grüne spalten in excel vorlage:  "IsoTest 100°\r\nFmax_iso\r\n[N]" , "Elastojump 100%\r\nPmax_rel\r\n[Watt/kg]", "Statojump 100%\r\nPmax_rel\r\n[Watt/kg]"

# selection
colselect_ski_m <- c("name" , "sex", "dob","sport","Testdatum\r\n[M.T.J]" ,"Alter\r\n[J]" ,"oe","generation", "Körpermasse\r\n[kg]"  , "IsoTest 100°\r\nFmax_iso\r\n[N]" , "Elastojump 100%\r\nPmax_rel\r\n[Watt/kg]", "Statojump 100%\r\nPmax_rel\r\n[Watt/kg]", "Elastojump 200%\r\nPmax_rel\r\n[Watt/kg]", "Statojump 200%\r\nPmax_rel\r\n[Watt/kg]")
kraft_ski_m <- kraft_ski_m %>% select(colselect_ski_m)

#benennung gleich colselect_ski_m
colnames(kraft_ski_m) <- c("name"    ,   "sex"    ,    "dob"    ,    "sport"    ,  "date_test" , "age_y"    ,  "oe"     ,    "generation" ,"weight_kg"  ,      "Fmax_iso_100"  ,   "Ej_Pmax_rel_100", "Sj_Pmax_rel_100", "Ej_Pmax_rel_200", "Sj_Pmax_rel_200" )




## ski alpin damen - from excel
kraft_ski_f <- read_excel("kraft_mld_neu_f.xlsx", 
                          col_types = c("text", "date", "date", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric"),
                          na = "NA")



#construct variables
kraft_ski_f <- kraft_ski_f %>% mutate(yob = year(date(kraft_ski_f$`Geburtsdatum\r\n[M.T.J]`)),
                                      sex = factor("f"),
                                      name = factor(`Name Vorname`),
                                      sport = factor("ski alpin"),
                                      oe = factor("kraft"),
                                      dob = `Geburtsdatum\r\n[M.T.J]`,
                                      generation = factor(ifelse(yob>1996,"Z",ifelse(yob>1980,"Y","X"))))

# klaus will: s. grüne spalten in excel vorlage:  "IsoTest 100°\r\nFmax_iso\r\n[N]" , "Elastojump 100%\r\nPmax_rel\r\n[Watt/kg]", "Statojump 100%\r\nPmax_rel\r\n[Watt/kg]"


# selection
colselect_ski_f <- c("name" , "sex", "dob","sport","Testdatum\r\n[M.T.J]" ,"Alter\r\n[J]" ,"oe","generation", "Körpermasse\r\n[kg]"  , "IsoTest 100°\r\nFmax_iso\r\n[N]" , "Elastojump 100%\r\nPmax_rel\r\n[Watt/kg]", "Statojump 100%\r\nPmax_rel\r\n[Watt/kg]", "Elastojump 200%\r\nPmax_rel\r\n[Watt/kg]", "Statojump 200%\r\nPmax_rel\r\n[Watt/kg]")
kraft_ski_f <- kraft_ski_f %>% select(colselect_ski_f)

#benennung gleich colselect_ski_m
colnames(kraft_ski_f) <- c("name"    ,   "sex"    ,    "dob"    ,    "sport"    ,  "date_test" , "age_y"    ,  "oe"     ,    "generation" ,"weight_kg"  ,      "Fmax_iso_100"  ,   "Ej_Pmax_rel_100", "Sj_Pmax_rel_100", "Ej_Pmax_rel_200", "Sj_Pmax_rel_200" )




## join ski alpin herren und damen - kraft final
kraft_final <- rbind(kraft_ski_f,kraft_ski_m)

# check generation table
table(kraft_final$generation, kraft_final$sex)

## check if some guys (ans whick) have  multiple generations attached
kraft_final %>% group_by(name, generation) %>% tally() %>% group_by(name) %>% summarise(n = n()) %>% filter(n > 1)
# that is the case, therefore, make a new, combined name-factor for the modeling
kraft_final$name2 <- factor(paste(kraft_final$name, kraft_final$dob))

## aus interesse
ggpairs(kraft_final %>% select(2,8:14), aes(color = sex))

                                                                                                                                                                                                           

### get spiel data ---------------------------------------------------------------- ####
spiel_1 <- read_excel("spiel_13_Rohdaten_assortiert.xlsx", 
                                           col_types = c("text", "text", "text", 
                                                         "text", "date", "date", "numeric", 
                                                         "text", "text", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric", 
                                                         "numeric", "numeric", "text", "text"))
#relevant variables already selected in excel

#construct variables - and rename
spiel_1 <- spiel_1 %>% mutate(yob = year(`Geb.-Dat`),
                                      sex = factor(ifelse(Geschlecht == "weiblich","f", "m")),
                                      name = factor(paste(Name, Vorname)),
                                      sport = factor(Sportart),
                                      date_test = date(`Test-Datum`),
                                      oe = factor("spiel"),
                                      dob = date(`Geb.-Dat`),
                                      age_y = `Alter J.`,
                                      weight_kg = `Gewicht (kg)`,
                                      generation = factor(ifelse(yob>1996,"Z",ifelse(yob>1980,"Y","X"))),
                                      yoyo_m = `Yo-Yo Distanz (m)`,
                                      sprint10_s = `Sprint 10m (sec)`,
                                      sprint40_s = `Sprint 40m (sec)`,
                                      sprung_cmj_wkg = `Sprungkraft CMJ (Watt/kg)`)

#select
colselect_spiel <- c("name","sex", "dob","sport","date_test" , "age_y","oe" ,"generation" ,"weight_kg"  ,"yoyo_m" , "sprint10_s" , "sprint40_s"   , "sprung_cmj_wkg" )
spiel_final <- spiel_1 %>% select(colselect_spiel)
spiel_final


### get physio data --------------------------------------------------------------- ####

physio1 <- read_delim("P:/EHSM-LSP-AUSD/Persönliche_Ordner/Severin/Konferenzen/MTT/2019/analyse_generation_z_rls/physio_rumpf_okt19.csv", 
                      ";", escape_double = FALSE, col_types = cols(Geburtsdatum = col_date(format = "%d.%m.%Y"), 
                                                                   Testdatum = col_date(format = "%d.%m.%Y"),
                                                                   dorsal_resultat = col_number(),
                                                                   ventral_resultat = col_number(),
                                                                   lateral_resultat = col_number(),
                                                                   age_y = col_number()), 
                      trim_ws = TRUE)

#construct variables - and rename
physio <- physio1 %>% mutate(yob = year(Geburtsdatum),
                              sex = factor(ifelse(Geschlecht == "w","f", "m")),
                              name = factor(paste(Name, Vorname)),
                              sport = factor(Sportart),
                              date_test = date(Testdatum),
                              oe = factor("physio"),
                              dob = Geburtsdatum,
                              weight_kg = Gewicht,
                              generation = factor(ifelse(yob>1996,"Z",ifelse(yob>1980,"Y","X")))) %>% select(27:35,24,20,16,8) %>% drop_na(dorsal_resultat,
                                                                                                                                           ventral_resultat,
                                                                                                                                           lateral_resultat) %>% 
  filter(between(age_y,10,50))


## aus interesse
ggpairs(physio %>% select(2,9:12), aes(color = sex))


### combine ausdauer, kraft & spiel data ------------------------------------------ #### 

## write csvs for future simple access
write.csv(ausd_final,"ausd_df.csv")
write.csv(kraft_final,"kraft_df.csv")
write.csv(spiel_final,"spiel_df.csv")




### ANALYSIS - data load ---------------------------------------------------------- #### 

## "lazy loading" der daten (i.e. direkt aus csv)
ausd_final <- read_csv("ausd_df.csv", 
                       col_types = cols(generation = col_factor(levels = c("X", "Y", "Z")), 
                                        sex = col_factor(levels = c("f", "m")))) %>% select(2:14)
ausd_final$sport <- as.factor(ausd_final$sport)
ausd_final$name <- as.factor(ausd_final$name)


kraft_final <- read_csv("kraft_df.csv", 
                       col_types = cols(generation = col_factor(levels = c("X", "Y", "Z")), 
                                        sex = col_factor(levels = c("f", "m")))) %>% select(2:17)
kraft_final$sport <- as.factor(kraft_final$sport)
kraft_final$name <- as.factor(kraft_final$name)


spiel_final <- read_csv("spiel_df.csv", 
                        col_types = cols(generation = col_factor(levels = c("X", "Y", "Z")), 
                                         sex = col_factor(levels = c("f", "m")))) %>% select(2:14)
spiel_final$sport <- as.factor(spiel_final$sport)
spiel_final$name <- as.factor(spiel_final$name)



### Overview
table(ausd_final$generation, ausd_final$sex)
table(kraft_final$generation, kraft_final$sex)
table(spiel_final$generation, spiel_final$sex)

## explore ###

#ggpairs(ausd_final %>% select(...))



## AUSDAUER I (Kap LL) ------------------------------------------------------------ ####

# ## COMPARE GENERATIONS WITH ATHLETES AGGREGATED
# 
# ## aggregate date by athlete mean - ACHTUNG BIAS: GENERATIONEN HABEN VERSCHIEDENE ALTERSSTRUKTUREN!
# ausd_final_agg <- ausd_final %>% group_by(name) %>% summarise(sex = first(sex),
#                                                               dob = first(dob),
#                                                               sport = first(sport),
#                                                               age_y = mean(age_y, na.rm = T),
#                                                               oe = first(oe),
#                                                               generation = first(generation),
#                                                               weight_kg = mean(weight_kg, na.rm = T),
#                                                               vo2_abs = mean(vo2_abs, na.rm = T),
#                                                               perf_kap = mean(perf_kap, na.rm = T))
# 
# table(ausd_final_agg$generation, ausd_final_agg$sex)
# 
# 
# # perf vs generation - SCHEISSPLOT - UND BIASED
# ggplot(ausd_final_agg, aes(x = sex, y = perf_kap, fill = generation))+
#   geom_violin(alpha = 0.5, scale = "area", draw_quantiles = c(0.5))+
#   facet_wrap(.~sport)+
#   theme_bw()+
#   labs(title = "AUSDAUER: Performance by Generation")


## COMPARE GENERATIONS BY TIME EVOLUTION

## mixed model ausdauer - perf_kap

data_mod <- ausd_final  %>% drop_na(perf_kap) %>% filter(age_y > 15 & age_y < 25) #ages between 15 und 25

# modellwahl
# lm1 <-                 lm(perf_kap ~ age_y  + sport +  generation + name,           data = data_mod)
mm_ausd_base <-  lmer(perf_kap ~ generation +  age_y + sport  + sex + sex:age_y + sport:age_y + (1+age_y|name), data = data_mod, REML = F)
mm_ausd_vglc <-  lmer(perf_kap ~ generation +  age_y + sport  + sex + sex:age_y + sport:age_y + (1+age_y|name), data = data_mod, REML = F)

anova(mm_ausd_base,mm_ausd_vglc) 

#drop1 bessser:
drop1(mm_ausd_vglc, test = "Chisq") #interaktionen alle nicht signifikant

summary(mm_ausd_base)
coef(mm_ausd_base)


#bestes modell nach ausprobieren (für ages 15 bis 25): perf_kap ~ generation +  age_y + sport  + sex + sex:age_y + sport:age_y + (1+age_y|name)

# modell auf zu plottende daten anpassen
data_mod_end <- ausd_final  %>% drop_na(perf_kap) %>% filter(sport == "Langlauf") %>% filter(age_y > 15 & age_y < 25) #ages between 15 und 25
mm_ausd_end  <- lmer(perf_kap ~ generation +  age_y  + sex + sex:age_y + (1+age_y|name), data = data_mod_end, REML = F) #sport und age:sport aus modell weg weil nur für einen sport gefittet

#modellannahmen prüfen:
plot(mm_ausd_end) #ok
qqnorm(resid(mm_ausd_end)) #ok
qqPlot(resid(mm_ausd_end)) #ok
hist(resid(mm_ausd_end)) #ok

summary(mm_ausd_end)
#confint(mm_ausd_end) #dont run - takes forever

# und plotten:
# perf vs alter - noch mixed model rein!
(ausd1 <- ggplot(data_mod_end, aes(x = age_y, y = perf_kap,  color = sex))+
  geom_point(alpha = 0.25, aes(size = generation))+
  geom_line(aes(group = name), alpha = 0.25)+
  geom_line(aes(y = predict(mm_ausd_end, re.form = NA), group = name, size = generation))+ #re.form = form of random effect (plotted without random effects)
  scale_size_discrete(range = c(1, 2))+
  theme_bw()+
  #scale_color_colorblind()+
  labs(title = "LANGLAUF: Performance vs. Alter",
       subtitle = "Mixed effects model fit for ages 15 to 25:\nperf_kap ~ generation +  age_y  + sex + sex:age_y + (1+age_y|name)",
       y = "24 min TT performance (m)",
       x = "Age (y)"))

ggsave(filename = "ausd_langlauf_kaptest_vs_alter_300.jpeg", plot = ausd1)


## AUSDAUER II (rel vo2max LL) ---------------------------------------------------- ####

## COMPARE GENERATIONS BY TIME EVOLUTION

## mixed model ausdauer - vo2_abs/weight

data_mod <- ausd_final  %>% drop_na(vo2_abs) %>% filter(age_y > 15 & age_y < 25) #ages between 15 und 25

# modellwahl
# lfull: lmer(vo2_abs/weight_kg ~ generation + age_y  + sex + generation:age_y + sex:age_y + (1+age_y|name), data = data_mod, REML = F)
mm_ausd_base2 <-  lmer(vo2_abs/weight_kg ~ generation +  sex +   (1|name), data = data_mod, REML = F)
mm_ausd_vglc2 <-  lmer(vo2_abs/weight_kg ~  sex + generation +  (1|name), data = data_mod, REML = F)

anova(mm_ausd_base2,mm_ausd_vglc2) 

#drop1 bessser:
drop1(mm_ausd_vglc2, test = "Chisq") #interaktionen alle nicht signifikant

summary(mm_ausd_base2)
coef(mm_ausd_base2)


#bestes modell nach ausprobieren (für ages 15 bis 25): vo2_abs/weight_kg ~  sex +   (1|name) #note: fixed effect für age_y und random slope weggefallen!

# modell auf zu plottende daten anpassen
data_mod_end2 <- ausd_final  %>% drop_na(vo2_abs) %>% filter(sport == "Langlauf") %>% filter(age_y > 15 & age_y < 25) #ages between 15 und 25
mm_ausd_end2  <- lmer(vo2_abs/weight_kg ~   sex +   (1|name), data = data_mod_end2, REML = F) #sport und age:sport aus modell weg weil nur für einen sport gefittet

#modellannahmen prüfen:
plot(mm_ausd_end2) #ok
qqnorm(resid(mm_ausd_end2)) #ok
qqPlot(resid(mm_ausd_end2)) #ok
hist(resid(mm_ausd_end2)) #ok

summary(mm_ausd_end2)
#confint(mm_ausd_end) #dont run - takes forever

# und plotten:
# perf vs alter - noch mixed model rein!
ggplot(data_mod_end2, aes(x = age_y, y = vo2_abs/weight_kg,  color = sex))+
  geom_point(alpha = 0.25, aes(size = generation))+
  geom_line(aes(group = name), alpha = 0.25)+
  geom_line(aes(y = predict(mm_ausd_end2, re.form = NA), group = name, size = generation))+ #re.form = form of random effect (plotted without random effects)
  scale_size_discrete(range = c(1, 2))+
  theme_bw()+
  #scale_color_colorblind()+
  labs(title = "LANGLAUF: Rel. VO2max vs. Alter",
       subtitle = "Mixed effects model fit for ages 15 to 25:\nvo2_abs/weight_kg ~ sex + (1|name)",
       y = "Relative VO2max (ml/min/kg)",
       x = "Age (y)")




## AUSDAUER III (ANS LL) ---------------------------------------------------------- ####

## COMPARE GENERATIONS BY TIME EVOLUTION

## mixed model ausdauer - ans_stufe_ll

data_mod <- ausd_final  %>% drop_na(ans_stufe_ll) %>% filter(age_y > 15 & age_y < 25) #ages between 15 und 25

# modellwahl
# lfull: lmer(ans_stufe_ll ~ generation + age_y  + sex + generation:age_y + sex:age_y + (1+age_y|name), data = data_mod, REML = F)
mm_ausd_base3 <-  lmer(ans_stufe_ll ~ generation + age_y  + sex +  sex:age_y + (1+age_y|name), data = data_mod, REML = F)
mm_ausd_vglc3 <-  lmer(ans_stufe_ll ~ generation + age_y  + sex +  sex:age_y + (1+age_y|name), data = data_mod, REML = F)

anova(mm_ausd_base3,mm_ausd_vglc3) 

#drop1 bessser:
drop1(mm_ausd_vglc3, test = "Chisq") #interaktionen alle nicht signifikant

summary(mm_ausd_base2)
coef(mm_ausd_base2)


#bestes modell nach ausprobieren (für ages 15 bis 25): ans_stufe_ll ~ generation + age_y  + sex +  sex:age_y + (1+age_y|name) #note: fixed effect für age_y und random slope weggefallen!

# modell auf zu plottende daten anpassen
data_mod_end3 <- ausd_final  %>% drop_na(ans_stufe_ll) %>% filter(sport == "Langlauf") %>% filter(age_y > 15 & age_y < 25) #ages between 15 und 25
mm_ausd_end3  <- lmer(ans_stufe_ll ~ generation + age_y  + sex +  sex:age_y + (1+age_y|name), data = data_mod_end3, REML = F)

#modellannahmen prüfen:
plot(mm_ausd_end3) #ok
qqnorm(resid(mm_ausd_end3)) #too long tailed?
qqPlot(resid(mm_ausd_end3)) #too long tailed?
hist(resid(mm_ausd_end3)) #too long tailed?

summary(mm_ausd_end3)
#confint(mm_ausd_end) #dont run - takes forever

# und plotten:
# perf vs alter - noch mixed model rein!
ggplot(data_mod_end3, aes(x = age_y, y = ans_stufe_ll,  color = sex))+
  geom_point(alpha = 0.25, aes(size = generation))+
  geom_line(aes(group = name), alpha = 0.25)+
  geom_line(aes(y = predict(mm_ausd_end3, re.form = NA), group = name, size = generation))+ #re.form = form of random effect (plotted without random effects)
  scale_size_discrete(range = c(1, 2))+
  theme_bw()+
  #scale_color_colorblind()+
  labs(title = "LANGLAUF: Anaerobic threshold vs. Alter",
       subtitle = "Mixed effects model fit for ages 15 to 25:\nans_stufe_ll ~ generation + age_y  + sex +  sex:age_y + (1+age_y|name)",
       y = "Anaerobic threshold (Stufe)",
       x = "Age (y)")


## AUSDAUER IV (Kap MTB) ---------------------------------------------------------- ####

## COMPARE GENERATIONS BY TIME EVOLUTION

## mixed model ausdauer - ans_stufe_ll

data_mod <- ausd_final  %>% drop_na(perf_kap) %>% filter(age_y > 15 & age_y < 25) %>% filter(sport == "MTB XCO")#ages between 15 und 25

# modellwahl
# lfull: lmer(perf_kap ~ generation + age_y  + sex + generation:age_y + sex:age_y + (1+age_y|name), data = data_mod, REML = F)
mm_ausd_base4 <-  lmer(perf_kap ~ generation + age_y  + sex +  sex:age_y + (1|name), data = data_mod, REML = F)
mm_ausd_vglc4 <-  lmer(perf_kap ~ generation + age_y  + sex +  sex:age_y + (1|name), data = data_mod, REML = F)

anova(mm_ausd_base4,mm_ausd_vglc4) 

#drop1 bessser:
drop1(mm_ausd_vglc4, test = "Chisq")

summary(mm_ausd_base4)
coef(mm_ausd_base4)


#bestes modell nach ausprobieren (für ages 15 bis 25): perf_kap ~ generation + age_y  + sex +  sex:age_y + (1|name) #note: fixed effect für age_y und random slope weggefallen!

# modell auf zu plottende daten anpassen
data_mod_end4 <- ausd_final  %>% drop_na(perf_kap) %>% filter(sport == "MTB XCO") %>% filter(age_y > 15 & age_y < 25) #ages between 15 und 25
mm_ausd_end4  <- lmer(perf_kap ~ generation + age_y  + sex +  sex:age_y + (1|name), data = data_mod_end4, REML = F)

#modellannahmen prüfen:
plot(mm_ausd_end4) #ok
qqnorm(resid(mm_ausd_end4)) #too long tailed?
qqPlot(resid(mm_ausd_end4)) #too long tailed?
hist(resid(mm_ausd_end4)) #too long tailed?

summary(mm_ausd_end4)
#confint(mm_ausd_end) #dont run - takes forever

# und plotten:
# perf vs alter - noch mixed model rein!
ggplot(data_mod_end4, aes(x = age_y, y = perf_kap,  color = sex))+
  geom_point(alpha = 0.25, aes(size = generation))+
  geom_line(aes(group = name), alpha = 0.25)+
  geom_line(aes(y = predict(mm_ausd_end4, re.form = NA), group = name, size = generation))+ #re.form = form of random effect (plotted without random effects)
  scale_size_discrete(range = c(1, 2))+
  theme_bw()+
  #scale_color_colorblind()+
  labs(title = "MTB: Performance vs. Alter",
       subtitle = "Mixed effects model fit for ages 15 to 25:\nperf_kap ~ generation + age_y  + sex +  sex:age_y + (1|name)",
       y = "24 min TT performance (m)",
       x = "Age (y)")


## AUSDAUER V (Kap LL - 18 to 21) ------------------------------------------------- ####

#gemäss toms wunsch, nur die antersbereiche zu plotten, die bei beiden Generationen vorkommen

## COMPARE GENERATIONS BY TIME EVOLUTION

## mixed model ausdauer - perf_kap

data_mod <- ausd_final  %>% drop_na(perf_kap) %>% filter(age_y > 18 & age_y < 21) %>% filter(sport == "Langlauf")

# modellwahl
#basismodell: perf_kap ~ generation +  age_y  + sex + sex:age_y + generation:age_y + (1+age_y|name)
mm_ausd_base5 <-  lmer(perf_kap ~ generation +  age_y  + sex +   (1|name), data = data_mod, REML = F)
mm_ausd_vglc5 <-  lmer(perf_kap ~ generation +  age_y  + sex +   (1|name), data = data_mod, REML = F)

anova(mm_ausd_base5,mm_ausd_vglc5) 

#drop1 bessser:
drop1(mm_ausd_vglc5, test = "Chisq") #interaktionen alle nicht signifikant

summary(mm_ausd_bas5e)
coef(mm_ausd_base5)


#bestes modell nach ausprobieren (für ages 18 bis 21): perf_kap ~ generation +  age_y  + sex +   (1|name)

# modell auf zu plottende daten anpassen
data_mod_end5 <- ausd_final  %>% drop_na(perf_kap) %>% filter(sport == "Langlauf") %>% filter(age_y > 18 & age_y < 21) #ages between 15 und 25
mm_ausd_end5  <- lmer(perf_kap ~ generation +  age_y  + sex +   (1|name), data = data_mod_end5, REML = F) #sport und age:sport aus modell weg weil nur für einen sport gefittet

#modellannahmen prüfen:
plot(mm_ausd_end5) #ok
qqnorm(resid(mm_ausd_end5)) #ok
qqPlot(resid(mm_ausd_end5)) #ok
hist(resid(mm_ausd_end5)) #ok

summary(mm_ausd_end5)
#confint(mm_ausd_end) #dont run - takes forever

# und plotten:
# perf vs alter - noch mixed model rein!
(ausd5 <- ggplot(data_mod_end5, aes(x = age_y, y = perf_kap,  color = sex))+
    geom_point(alpha = 0.25, aes(size = generation))+
    geom_line(aes(group = name), alpha = 0.25)+
    geom_line(aes(y = predict(mm_ausd_end5, re.form = NA), group = name, size = generation))+ #re.form = form of random effect (plotted without random effects)
    scale_size_discrete(range = c(1, 2))+
    theme_bw()+
    #scale_color_colorblind()+
    labs(title = "LANGLAUF: Performance vs. Alter",
         subtitle = "Mixed effects model fit for ages 18 to 21:\nperf_kap ~ generation +  age_y  + sex +   (1|name)",
         y = "24 min TT performance (m)",
         x = "Age (y)"))

ggsave(filename = "ausd_langlauf_kaptest_vs_alter_300_18to21.jpeg", plot = ausd5)


## AUSDAUER VI (Kap MTB - 18 to 21) ----------------------------------------------- ####

#gemäss toms wunsch, nur die antersbereiche zu plotten, die bei beiden Generationen vorkommen

## COMPARE GENERATIONS BY TIME EVOLUTION

## mixed model ausdauer - perf_kap

data_mod <- ausd_final  %>% drop_na(perf_kap) %>% filter(age_y > 18 & age_y < 21) %>% filter(sport == "MTB XCO")

# modellwahl
#basismodell: perf_kap ~ generation +  age_y  + sex + sex:age_y + generation:age_y + (1+age_y|name)
mm_ausd_base6 <-  lmer(perf_kap ~ generation +  age_y  + sex + sex:age_y +  (1|name), data = data_mod, REML = F)
mm_ausd_vglc6 <-  lmer(perf_kap ~ generation +  age_y  + sex + sex:age_y +  (1|name), data = data_mod, REML = F)

anova(mm_ausd_base6,mm_ausd_vglc6) 

#drop1 bessser:
drop1(mm_ausd_vglc6, test = "Chisq") #interaktionen alle nicht signifikant

summary(mm_ausd_bas6e)
coef(mm_ausd_base6)


#bestes modell nach ausprobieren (für ages 18 bis 21): perf_kap ~ generation +  age_y  + sex + sex:age_y +  (1|name)

# modell auf zu plottende daten anpassen
data_mod_end6 <- ausd_final  %>% drop_na(perf_kap) %>% filter(sport == "MTB XCO") %>% filter(age_y > 18 & age_y < 21) 
mm_ausd_end6  <- lmer(perf_kap ~ generation +  age_y  + sex + sex:age_y +  (1|name), data = data_mod_end6, REML = F) #sport und age:sport aus modell weg weil nur für einen sport gefittet

#modellannahmen prüfen:
plot(mm_ausd_end6) #ok
qqnorm(resid(mm_ausd_end6)) #ok
qqPlot(resid(mm_ausd_end6)) #ok
hist(resid(mm_ausd_end6)) #ok

summary(mm_ausd_end6)
#confint(mm_ausd_end) #dont run - takes forever

# und plotten:
# perf vs alter - noch mixed model rein!
(ausd6 <- ggplot(data_mod_end6, aes(x = age_y, y = perf_kap,  color = sex))+
    geom_point(alpha = 0.25, aes(size = generation))+
    geom_line(aes(group = name), alpha = 0.25)+
    geom_line(aes(y = predict(mm_ausd_end6, re.form = NA), group = name, size = generation))+ #re.form = form of random effect (plotted without random effects)
    scale_size_discrete(range = c(1, 2))+
    theme_bw()+
    #scale_color_colorblind()+
    labs(title = "MTB: Performance vs. Alter",
         subtitle = "Mixed effects model fit for ages 18 to 21:\nperf_kap ~ generation +  age_y  + sex + sex:age_y +  (1|name)",
         y = "24 min TT performance (m)",
         x = "Age (y)"))

ggsave(filename = "ausd_mtb_kaptest_vs_alter_300_18to21.jpeg", plot = ausd6)


## SPIEL I (sprint 10 s Fussball) ------------------------------------------------- ####

## COMPARE GENERATIONS BY TIME EVOLUTION

## mixed model spiel - für sprint10_s, weil da am meisten datenpunkte

spiel_mod <- spiel_final  %>% drop_na(sprint10_s) %>% filter(age_y > 15 & age_y < 25) #ages between 15 und 25


# modellwahl
# ausgangsmodell: sprint10_s ~ generation +  age_y + sport  + sex + sex:age_y + sport:age_y + generation:age_y +(1+age_y|name)
mm_spiel_base <-  lmer(sprint10_s ~ generation +  age_y + sport  + sex + sex:age_y + sport:age_y + generation:age_y + (1+age_y|name), data = spiel_mod, REML = F)
mm_spiel_vglc <-  lmer(sprint10_s ~ generation +  age_y + sport  + sex + sex:age_y + sport:age_y + generation:age_y + (1+age_y|name), data = spiel_mod, REML = F)
anova(mm_spiel_base,mm_spiel_vglc) 

drop1(mm_spiel_vglc, test = "Chisq")

summary(mm_spiel_base)
coef(mm_spiel_base)

plot(mm_spiel_base)


#bestes modell nach ausprobieren (für ages 15 bis 25): sprint10_s ~ generation +  age_y + sport  + sex + sex:age_y + sport:age_y + generation:age_y + (1+age_y|name) #-- volles modell!

# modell auf zu plottende daten anpassen
data_mod_end_spiel <- spiel_final  %>% drop_na(sprint10_s, generation) %>% filter(sport == "Fussball") %>%   filter(generation != "X") %>% filter(age_y > 15 & age_y < 25) #ages between 15 und 25, am meisten Fussballer, wenige punkte aus gen X weg
mm_spiel_end  <- lmer(sprint10_s ~ generation +  age_y  + sex + sex:age_y + generation:age_y + (1+age_y|name), data = data_mod_end_spiel, REML = F) #sport und age:sport aus modell weg weil nur für einen sport gefittet

#modellannahmen prüfen:
plot(mm_spiel_end) #ok
qqnorm(resid(mm_spiel_end)) #hmm
qqPlot(resid(mm_spiel_end)) #hmm
hist(resid(mm_spiel_end)) #ok

summary(mm_spiel_end)

# und plotten:
# perf vs alter
ggplot(data_mod_end_spiel, aes(x = age_y, y = sprint10_s,  color = sex))+
  geom_point(alpha = 0.25, aes(size = generation))+
  geom_line(aes(group = name), alpha = 0.25)+
  geom_line(aes(y = predict(mm_spiel_end, re.form = NA), group = name, size = generation))+
  scale_size_discrete(range = c(1, 2))+
  theme_bw()+
  #scale_color_colorblind()+
  labs(title = "FUSSBALL: Sprint performance vs. Alter",
       subtitle = "Mixed effects model fit for ages 15 to 25:\nsprint10_s ~ generation +  age_y  + sex + sex:age_y + generation:age_y + (1+age_y|name)",
       y = "10 m sprint time (s)",
       x = "Age (y)")



## SPIEL II (cmj Fussball) -------------------------------------------------------- ####

## COMPARE GENERATIONS BY TIME EVOLUTION

## mixed model spiel - für "sprung_cmj_wkg"

spiel_mod <- spiel_final  %>% drop_na(sprung_cmj_wkg) %>% filter(age_y > 15 & age_y < 25) %>% filter(sport == "Fussball") #ages between 15 und 25

# modellwahl
# ausgangsmodell: sprung_cmj_wkg ~ generation +  age_y + sex + sex:age_y + generation:age_y +(1+age_y|name)
mm_spiel_base2 <-  lmer(sprung_cmj_wkg ~ generation +  age_y +  sex + sex:age_y +  generation:age_y +(1+age_y|name), data = spiel_mod, REML = F)
mm_spiel_vglc2 <-  lmer(sprung_cmj_wkg ~ generation +  age_y +  sex + sex:age_y +  generation:age_y +(1+age_y|name), data = spiel_mod, REML = F)
anova(mm_spiel_base2,mm_spiel_vglc2) 

drop1(mm_spiel_vglc2, test = "Chisq")

summary(mm_spiel_base2)
coef(mm_spiel_base2)

plot(mm_spiel_base2)


#bestes modell nach ausprobieren (für ages 15 bis 25): sprint10_s ~ generation +  age_y + sport  + sex + sex:age_y + sport:age_y + generation:age_y + (1+age_y|name) #-- volles modell!

# modell auf zu plottende daten anpassen
data_mod_end_spiel2 <- spiel_final  %>% drop_na(sprung_cmj_wkg, generation) %>% filter(sport == "Fussball") %>%   filter(generation != "X") %>% filter(age_y > 15 & age_y < 25) #ages between 15 und 25, am meisten Fussballer, wenige punkte aus gen X weg
mm_spiel_end2  <- lmer(sprung_cmj_wkg ~ generation +  age_y +  sex + sex:age_y +  generation:age_y +(1+age_y|name), data = data_mod_end_spiel, REML = F) #sport und age:sport aus modell weg weil nur für einen sport gefittet

#modellannahmen prüfen:
plot(mm_spiel_end2) #ok
qqnorm(resid(mm_spiel_end2)) #hmm
qqPlot(resid(mm_spiel_end2)) #hmm
hist(resid(mm_spiel_end2)) #ok

summary(mm_spiel_end2)

# und plotten:
# perf vs alter
ggplot(data_mod_end_spiel, aes(x = age_y, y = sprung_cmj_wkg,  color = sex))+
  geom_point(alpha = 0.25, aes(size = generation))+
  geom_line(aes(group = name), alpha = 0.25)+
  geom_line(aes(y = predict(mm_spiel_end2, re.form = NA), group = name, size = generation))+
  scale_size_discrete(range = c(1, 2))+
  theme_bw()+
  #scale_color_colorblind()+
  labs(title = "FUSSBALL: Rel. CMJ power vs. Alter",
       subtitle = "Mixed effects model fit for ages 15 to 25:\nsprung_cmj_wkg ~ generation +  age_y +  sex + sex:age_y +  generation:age_y +(1+age_y|name)",
       y = "CMJ Power rel (W/kg)",
       x = "Age (y)")

## SPIEL III (sprint 10 s Fussball - alter getrimmt 15 bis 21 y) ------------------ ####

## COMPARE GENERATIONS BY TIME EVOLUTION

## mixed model spiel - für sprint10_s, weil da am meisten datenpunkte

spiel_mod <- spiel_final  %>% drop_na(sprint10_s) %>% filter(age_y > 15 & age_y < 21) #ages between 15 und 25


# modellwahl
# ausgangsmodell: sprint10_s ~ generation +  age_y + sport  + sex + sex:age_y + sport:age_y + generation:age_y +(1+age_y|name)
mm_spiel_base3 <-  lmer(sprint10_s ~ generation +  age_y + sport  + sex + sex:age_y + sport:age_y + generation:age_y + (1+age_y|name), data = spiel_mod, REML = F)
mm_spiel_vglc3 <-  lmer(sprint10_s ~ generation +  age_y + sport  + sex + sex:age_y + sport:age_y + generation:age_y + (1+age_y|name), data = spiel_mod, REML = F)
anova(mm_spiel_base3,mm_spiel_vglc3) 

drop1(mm_spiel_vglc3, test = "Chisq")

summary(mm_spiel_base3)
coef(mm_spiel_base3)

plot(mm_spiel_base3)


#bestes modell nach ausprobieren (für ages 15 bis 25): sprint10_s ~ generation +  age_y + sport  + sex + sex:age_y + sport:age_y + generation:age_y + (1+age_y|name) #-- volles modell!

# modell auf zu plottende daten anpassen
data_mod_end_spiel3 <- spiel_final  %>% drop_na(sprint10_s, generation) %>% filter(sport == "Fussball") %>%   filter(generation != "X") %>% filter(age_y > 15 & age_y < 21) #ages between 15 und 21, am meisten Fussballer, wenige punkte aus gen X weg
mm_spiel_end3  <- lmer(sprint10_s ~ generation +  age_y  + sex + sex:age_y + generation:age_y + (1+age_y|name), data = data_mod_end_spiel3, REML = F) #sport und age:sport aus modell weg weil nur für einen sport gefittet
drop1(mm_spiel_end3, test = "Chisq")

#modellannahmen prüfen:
plot(mm_spiel_end3) #ok
qqnorm(resid(mm_spiel_end3)) #hmm
qqPlot(resid(mm_spiel_end3)) #hmm
hist(resid(mm_spiel_end3)) #ok

summary(mm_spiel_end3)

# und plotten:
# perf vs alter
(spiel3 <- ggplot(data_mod_end_spiel3, aes(x = age_y, y = sprint10_s,  color = sex))+
  geom_point(alpha = 0.25, aes(size = generation))+
  geom_line(aes(group = name), alpha = 0.25)+
  geom_line(aes(y = predict(mm_spiel_end3, re.form = NA), group = name, size = generation))+
  scale_size_discrete(range = c(1, 2))+
  theme_bw()+
  #scale_color_colorblind()+
  labs(title = "FUSSBALL: Sprint performance vs. Alter",
       subtitle = "Mixed effects model fit for ages 15 to 21:\nsprint10_s ~ generation +  age_y  + sex + sex:age_y + generation:age_y + (1+age_y|name)",
       y = "10 m sprint time (s)",
       x = "Age (y)"))

ggsave(filename = "spiel_fussball_sprint_vs_alter_300_15to21.jpeg",width = 30, height = 20, units = "cm", plot = spiel3)

## SPIEL IV (cmj Fussball - alter getrimmt 15 bis 21 y) --------------------------- ####

## COMPARE GENERATIONS BY TIME EVOLUTION

## mixed model spiel - für "sprung_cmj_wkg"

spiel_mod <- spiel_final  %>% drop_na(sprung_cmj_wkg) %>% filter(age_y > 15 & age_y < 21) %>% filter(sport == "Fussball") #ages between 15 und 21

# modellwahl
# ausgangsmodell: sprung_cmj_wkg ~ generation +  age_y + sex + sex:age_y + generation:age_y +(1+age_y|name)
mm_spiel_base4 <-  lmer(sprung_cmj_wkg ~ generation +  age_y + sex + sex:age_y + generation:age_y +(1+age_y|name), data = spiel_mod, REML = F)
mm_spiel_vglc4 <-  lmer(sprung_cmj_wkg ~ generation +  age_y + sex + sex:age_y + generation:age_y +(1+age_y|name), data = spiel_mod, REML = F)
anova(mm_spiel_base4,mm_spiel_vglc4) 

drop1(mm_spiel_vglc4, test = "Chisq")

summary(mm_spiel_base4)
coef(mm_spiel_base4)

plot(mm_spiel_base4)


#bestes modell nach ausprobieren (für ages 15 bis 21): sprung_cmj_wkg ~ generation +  age_y + sex + sex:age_y + generation:age_y +(1+age_y|name) #-- volles modell!

# modell auf zu plottende daten anpassen
data_mod_end_spiel4 <- spiel_final  %>% drop_na(sprung_cmj_wkg, generation) %>% filter(sport == "Fussball") %>%   filter(generation != "X") %>% filter(age_y > 15 & age_y < 21) #ages between 15 und 21, am meisten Fussballer, wenige punkte aus gen X weg
mm_spiel_end4  <- lmer(sprung_cmj_wkg ~ generation +  age_y + sex + sex:age_y + generation:age_y +(1+age_y|name), data = data_mod_end_spiel4, REML = F) #sport und age:sport aus modell weg weil nur für einen sport gefittet

#modellannahmen prüfen:
plot(mm_spiel_end4) #ok
qqnorm(resid(mm_spiel_end4)) #hmm
qqPlot(resid(mm_spiel_end4)) #hmm
hist(resid(mm_spiel_end4)) #ok

summary(mm_spiel_end4)

# und plotten:
# perf vs alter
ggplot(data_mod_end_spiel4, aes(x = age_y, y = sprung_cmj_wkg,  color = sex))+
  geom_point(alpha = 0.25, aes(size = generation))+
  geom_line(aes(group = name), alpha = 0.25)+
  geom_line(aes(y = predict(mm_spiel_end4, re.form = NA), group = name, size = generation))+
  scale_size_discrete(range = c(1, 2))+
  theme_bw()+
  #scale_color_colorblind()+
  labs(title = "FUSSBALL: Rel. CMJ power vs. Alter",
       subtitle = "Mixed effects model fit for ages 15 to 21:\nsprung_cmj_wkg ~ generation +  age_y +  sex + sex:age_y +  generation:age_y +(1+age_y|name)",
       y = "CMJ Power rel (W/kg)",
       x = "Age (y)")


## etwas anderes: correlation cmj und sprintleistung
ggplot(spiel_final, aes(x = sprung_cmj_wkg, y = sprint10_s, color = sex))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()+
  facet_wrap(.~sport)

ggplot(spiel_final, aes(x = sprint40_s, y = sprint10_s, color = sex))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()



## SPIEL V (yoyo Fussball - alter getrimmt 15 bis 21 y) --------------------------- ####

## COMPARE GENERATIONS BY TIME EVOLUTION

## mixed model spiel - für "yoyo_m"

spiel_mod <- spiel_final  %>% drop_na(yoyo_m) %>% filter(age_y > 15 & age_y < 21) %>% filter(sport == "Fussball") #ages between 15 und 21

# modellwahl
# ausgangsmodell: yoyo_m ~ generation +  age_y + sex + sex:age_y + generation:age_y +(1+age_y|name)
mm_spiel_base5 <-  lmer(yoyo_m ~ generation +  age_y + sex + sex:age_y +  (1|name), data = spiel_mod, REML = F) #random slope ingendwie unmöglich
mm_spiel_vglc5 <-  lmer(yoyo_m ~ generation +  age_y + sex + sex:age_y +  (1|name), data = spiel_mod, REML = F)
anova(mm_spiel_base5,mm_spiel_vglc5) 

drop1(mm_spiel_vglc5, test = "Chisq")

summary(mm_spiel_base5)
coef(mm_spiel_base5)

plot(mm_spiel_vglc5)
qqPlot(resid(mm_spiel_vglc5)) #hmm


#bestes modell nach ausprobieren (für ages 15 bis 21): yoyo_m ~ generation +  age_y + sex + sex:age_y + (1|name) #-- volles modell!

# modell auf zu plottende daten anpassen
data_mod_end_spiel5 <- spiel_final  %>% drop_na(yoyo_m, generation) %>% filter(sport == "Fussball") %>%   filter(generation != "X") %>% filter(age_y > 15 & age_y < 21) #ages between 15 und 21, am meisten Fussballer, wenige punkte aus gen X weg
mm_spiel_end5  <- lmer(yoyo_m ~ generation +  age_y + sex + sex:age_y + (1|name), data = data_mod_end_spiel5, REML = F) #sport und age:sport aus modell weg weil nur für einen sport gefittet

#modellannahmen prüfen:
plot(mm_spiel_end5) #ok
qqnorm(resid(mm_spiel_end5)) #hmm
qqPlot(resid(mm_spiel_end5)) #hmm
hist(resid(mm_spiel_end5)) #ok

summary(mm_spiel_end5)

# und plotten:
# perf vs alter
ggplot(data_mod_end_spiel5, aes(x = age_y, y = yoyo_m,  color = sex))+
  geom_point(alpha = 0.25, aes(size = generation))+
  geom_line(aes(group = name), alpha = 0.25)+
  geom_line(aes(y = predict(mm_spiel_end5, re.form = NA), group = name, size = generation))+
  scale_size_discrete(range = c(1, 2))+
  theme_bw()+
  #scale_color_colorblind()+
  labs(title = "FUSSBALL: Yoyo distance vs. Alter",
       subtitle = "Mixed effects model fit for ages 15 to 21:\nyoyo_m ~ generation +  age_y + sex + sex:age_y + (1|name)",
       y = "Yoyo test distance (m)",
       x = "Age (y)")

## SPIEL VI (sprint 10 s Eishockey - alter getrimmt 15 bis 18 y) ------------------ ####

## COMPARE GENERATIONS BY TIME EVOLUTION

## mixed model spiel - für sprint10_s, weil da am meisten datenpunkte

spiel_mod <- spiel_final  %>% drop_na(sprint10_s) %>% filter(age_y > 15 & age_y < 18) %>% filter(sport == "Eishockey")#ages between 15 und 25

# modellwahl
# ausgangsmodell: sprint10_s ~ generation +  age_y + sex + sex:age_y + generation:age_y + (1+age_y|name)
mm_spiel_base6 <-  lmer(sprint10_s ~ generation +  age_y + sex + sex:age_y + generation:age_y + (1+age_y|name), data = spiel_mod, REML = F)
mm_spiel_vglc6 <-  lmer(sprint10_s ~ generation +  age_y + sex + sex:age_y + generation:age_y + (1+age_y|name), data = spiel_mod, REML = F)
anova(mm_spiel_base6,mm_spiel_vglc6) 

drop1(mm_spiel_vglc6, test = "Chisq")

summary(mm_spiel_base6)
coef(mm_spiel_base6)

plot(mm_spiel_base6)


#bestes modell nach ausprobieren (für ages 15 bis 25): sprint10_s ~ generation +  age_y + sport  + sex + sex:age_y + sport:age_y + generation:age_y + (1+age_y|name) #-- volles modell!

# modell auf zu plottende daten anpassen
data_mod_end_spiel6 <- spiel_final  %>% drop_na(sprint10_s, generation) %>% filter(sport == "Eishockey") %>%   filter(generation != "X") %>% filter(age_y > 15 & age_y < 18) #ages between 15 und 18, am meisten Eishockeyer, wenige punkte aus gen X weg
mm_spiel_end6  <- lmer(sprint10_s ~ generation +  age_y + sex + sex:age_y + generation:age_y + (1+age_y|name), data = data_mod_end_spiel6, REML = F) #sport und age:sport aus modell weg weil nur für einen sport gefittet
drop1(mm_spiel_end6, test = "Chisq")

#modellannahmen prüfen:
plot(mm_spiel_end6) #ok
qqnorm(resid(mm_spiel_end6)) #hmm
qqPlot(resid(mm_spiel_end6)) #hmm
hist(resid(mm_spiel_end6)) #ok

summary(mm_spiel_end6)

# und plotten:
# perf vs alter
ggplot(data_mod_end_spiel6, aes(x = age_y, y = sprint10_s,  color = sex))+
  geom_point(alpha = 0.25, aes(size = generation))+
  geom_line(aes(group = name), alpha = 0.25)+
  geom_line(aes(y = predict(mm_spiel_end6, re.form = NA), group = name, size = generation))+
  scale_size_discrete(range = c(1, 2))+
  theme_bw()+
  #scale_color_colorblind()+
  labs(title = "Eishockey: Sprint performance vs. Alter",
       subtitle = "Mixed effects model fit for ages 15 to 18:\nsprint10_s ~ generation +  age_y  + sex + sex:age_y + generation:age_y + (1+age_y|name)",
       y = "10 m sprint time (s)",
       x = "Age (y)")


## SPIEL VII (cmj Eishockey - alter getrimmt 15 bis 18 y) ------------------------- ####

## COMPARE GENERATIONS BY TIME EVOLUTION

## mixed model spiel - für "sprung_cmj_wkg"

spiel_mod <- spiel_final  %>% drop_na(sprung_cmj_wkg) %>% filter(age_y > 15 & age_y < 18) %>% filter(sport == "Eishockey") #ages between 15 und 18

# modellwahl
# ausgangsmodell: sprung_cmj_wkg ~ generation +  age_y + sex + sex:age_y + generation:age_y +(1+age_y|name)
mm_spiel_base7 <-  lmer(sprung_cmj_wkg ~ generation +  age_y + sex + sex:age_y + generation:age_y +(1|name), data = spiel_mod, REML = F)
mm_spiel_vglc7 <-  lmer(sprung_cmj_wkg ~ generation +  age_y + sex + sex:age_y + generation:age_y +(1|name), data = spiel_mod, REML = F)
anova(mm_spiel_base7,mm_spiel_vglc7) 

drop1(mm_spiel_vglc7, test = "Chisq")

summary(mm_spiel_base7)
coef(mm_spiel_base7)

plot(mm_spiel_base7)


#bestes modell nach ausprobieren (für ages 15 bis 18): sprung_cmj_wkg ~ generation +  age_y + sex + sex:age_y + generation:age_y +(1|name)

# modell auf zu plottende daten anpassen
data_mod_end_spiel7 <- spiel_final  %>% drop_na(sprung_cmj_wkg, generation) %>% filter(sport == "Eishockey") %>%   filter(generation != "X") %>% filter(age_y > 15 & age_y < 18) #ages between 15 und 18, am meisten Eishockeyer, wenige punkte aus gen X weg
mm_spiel_end7  <- lmer(sprung_cmj_wkg ~ generation +  age_y + sex + sex:age_y + generation:age_y +(1|name), data = data_mod_end_spiel7, REML = F) #sport und age:sport aus modell weg weil nur für einen sport gefittet
drop1(mm_spiel_end7, test = "Chisq")

#modellannahmen prüfen:
plot(mm_spiel_end7) #ok
qqnorm(resid(mm_spiel_end7)) #hmm
qqPlot(resid(mm_spiel_end7)) #hmm
hist(resid(mm_spiel_end7)) #ok

summary(mm_spiel_end7)

# und plotten:
# perf vs alter
ggplot(data_mod_end_spiel7, aes(x = age_y, y = sprung_cmj_wkg,  color = sex))+
  geom_point(alpha = 0.25, aes(size = generation))+
  geom_line(aes(group = name), alpha = 0.25)+
  geom_line(aes(y = predict(mm_spiel_end7, re.form = NA), group = name, size = generation))+
  scale_size_discrete(range = c(1, 2))+
  theme_bw()+
  #scale_color_colorblind()+
  labs(title = "Eishockey: Rel. CMJ power vs. Alter",
       subtitle = "Mixed effects model fit for ages 15 to 18:\nsprung_cmj_wkg ~ generation +  age_y +  sex + sex:age_y +  generation:age_y +(1+age_y|name)",
       y = "CMJ Power rel (W/kg)",
       x = "Age (y)")



## SPIEL VIII (yoyo Eishockey - alter getrimmt 15 bis 18 y) ----------------------- ####

## COMPARE GENERATIONS BY TIME EVOLUTION

## mixed model spiel - für "yoyo_m"

spiel_mod <- spiel_final  %>% drop_na(yoyo_m) %>% filter(age_y > 15 & age_y < 18) %>% filter(sport == "Eishockey") #ages between 15 und 18

# modellwahl
# ausgangsmodell: yoyo_m ~ generation +  age_y + sex + sex:age_y + generation:age_y +(1+age_y|name)
mm_spiel_base8 <-  lmer(yoyo_m ~ generation +  age_y + sex + sex:age_y + (1|name), data = spiel_mod, REML = F) #random slope ingendwie unmöglich
mm_spiel_vglc8 <-  lmer(yoyo_m ~ generation +  age_y + sex + sex:age_y + (1|name), data = spiel_mod, REML = F)
anova(mm_spiel_base8,mm_spiel_vglc8) 

drop1(mm_spiel_vglc8, test = "Chisq")

summary(mm_spiel_base8)
coef(mm_spiel_base8)

plot(mm_spiel_vglc8)
qqPlot(resid(mm_spiel_vglc8)) #hmm


#bestes modell nach ausprobieren (für ages 15 bis 18): yoyo_m ~ generation +  age_y + sex + sex:age_y + (1|name) #-- volles modell!

# modell auf zu plottende daten anpassen
data_mod_end_spiel8 <- spiel_final  %>% drop_na(yoyo_m, generation) %>% filter(sport == "Eishockey") %>%   filter(generation != "X") %>% filter(age_y > 15 & age_y < 18) #ages between 15 und 18, am meisten Eishockeyer, wenige punkte aus gen X weg
mm_spiel_end8  <- lmer(yoyo_m ~ generation +  age_y + sex + sex:age_y + (1|name), data = data_mod_end_spiel8, REML = F) #sport und age:sport aus modell weg weil nur für einen sport gefittet
drop1(mm_spiel_end8, test = "Chisq")

#modellannahmen prüfen:
plot(mm_spiel_end8) #ok
qqnorm(resid(mm_spiel_end8)) #hmm
qqPlot(resid(mm_spiel_end8)) #hmm
hist(resid(mm_spiel_end8)) #ok

summary(mm_spiel_end8)

# und plotten:
# perf vs alter
ggplot(data_mod_end_spiel8, aes(x = age_y, y = yoyo_m,  color = sex))+
  geom_point(alpha = 0.25, aes(size = generation))+
  geom_line(aes(group = name), alpha = 0.25)+
  geom_line(aes(y = predict(mm_spiel_end8, re.form = NA), group = name, size = generation))+
  scale_size_discrete(range = c(1, 2))+
  theme_bw()+
  #scale_color_colorblind()+
  labs(title = "Eishockey: Yoyo distance vs. Alter",
       subtitle = "Mixed effects model fit for ages 15 to 18:\nyoyo_m ~ generation +  age_y + sex + sex:age_y + (1|name)",
       y = "Yoyo test distance (m)",
       x = "Age (y)")


# verifizierung:
spiel_final %>% filter(sport == "Eishockey") %>% drop_na(yoyo_m, generation, weight_kg) %>%  group_by(generation, sex) %>% summarise(meanyoyo = mean(yoyo_m),
                                                                                                                                     meanweight = mean(weight_kg))


spiel_final %>% filter(sport == "Fussball") %>% drop_na(yoyo_m, generation, weight_kg) %>%  group_by(generation, sex) %>% summarise(meanyoyo = mean(yoyo_m),
                                                                                                                                    meanweight = mean(weight_kg))

## KRAFT I (fmax iso) ------------------------------------------------------------- ####

## COMPARE GENERATIONS BY TIME EVOLUTION

## mixed model kraft - für Fmax_iso, weil das am besten tönt...

kraft_mod <- kraft_final  %>%  drop_na(Fmax_iso_100, generation) %>%   filter(generation != "X") %>% filter(age_y < 21)

# modellwahl
# ausgangsmodell (nur 1 sportart): Fmax_iso_100 ~ generation +  age_y  + sex + sex:age_y +  generation:age_y +(1+age_y|name)
mm_kraft_base <-  lmer(Fmax_iso_100 ~ generation +  age_y  + sex +   (1+age_y|name2), data = kraft_mod, REML = F)
mm_kraft_vglc <-  lmer(Fmax_iso_100 ~ age_y  + sex +  (1+age_y|name2), data = kraft_mod, REML = F)
anova(mm_kraft_base,mm_kraft_vglc) 

drop1(mm_kraft_vglc, test = "Chisq")

summary(mm_kraft_base)
coef(mm_kraft_base)

plot(mm_kraft_base)


#bestes modell nach ausprobieren: Fmax_iso_100 ~ age_y  + sex +  (1+age_y|name2) 

# modell auf zu plottende daten anpassen
data_mod_end_kraft <- kraft_final  %>% drop_na(Fmax_iso_100, generation) %>%   filter(generation != "X") %>% filter(age_y < 21)
mm_kraft_end  <- lmer(Fmax_iso_100 ~ age_y  + sex +  (1+age_y|name2), data = data_mod_end_kraft, REML = F) 

#modellannahmen prüfen:
plot(mm_kraft_end) #ok
qqnorm(resid(mm_kraft_end)) #ok
qqPlot(resid(mm_kraft_end)) #ok
hist(resid(mm_kraft_end)) #ok

summary(mm_kraft_end)

# und plotten:
# perf vs alter
(kraft1 <- ggplot(data_mod_end_kraft, aes(x = age_y, y = Fmax_iso_100,  color = sex))+
  geom_point(alpha = 0.25, aes(size = generation))+
  geom_line(aes(group = name), alpha = 0.25)+
  geom_line(aes(y = predict(mm_kraft_end, re.form = NA), group = name2, size = generation))+
  scale_size_discrete(range = c(1, 2))+
  theme_bw()+
  #scale_color_colorblind()+
  labs(title = "SKI ALPIN: Isometrische Maxkraft vs. Alter",
       subtitle = "Mixed effects model fit:\nFmax_iso_100 ~  age_y  + sex + (1+age_y|name)",
       y = "Fmax isometrisch (N)",
       x = "Age (y)"))

ggsave(filename = "kraft_skialpin_fmaxiso_vs_alter_300.jpeg", plot = kraft1, width = 25, height = 15, units = "cm")



## KRAFT II (Elastojump rel 100) -------------------------------------------------- ####

## COMPARE GENERATIONS BY TIME EVOLUTION

## mixed model kraft - für Fmax_iso, weil das am besten tönt...

kraft_mod2 <- kraft_final  %>%  drop_na(Ej_Pmax_rel_100, generation) %>%   filter(generation != "X") %>% filter(age_y < 21) #ages between 15 und 25

# modellwahl
# ausgangsmodell (nur 1 sportart): Ej_Pmax_rel_100 ~ generation +  age_y  + sex + sex:age_y +  generation:age_y +(1+age_y|name)
mm_kraft_base2 <-  lmer(Ej_Pmax_rel_100 ~ generation +  age_y  + sex + (1+age_y|name2), data = kraft_mod2, REML = F)
mm_kraft_vglc2 <-  lmer(Ej_Pmax_rel_100 ~ generation +  age_y  + sex +  sex:age_y + (1+age_y|name2), data = kraft_mod2, REML = F)
anova(mm_kraft_base2,mm_kraft_vglc2) 

drop1(mm_kraft_vglc2, test = "Chisq")

summary(mm_kraft_base2)
coef(mm_kraft_base2)

plot(mm_kraft_base2)


#bestes modell nach ausprobieren (für ages 15 bis 25): Ej_Pmax_rel_100 ~ generation +  age_y  + sex +  sex:age_y + (1+age_y|name2)

# modell auf zu plottende daten anpassen
data_mod_end_kraft2 <- kraft_final  %>% drop_na(Ej_Pmax_rel_100, generation) %>%   filter(generation != "X") %>% filter(age_y < 21)
mm_kraft_end2  <- lmer(Ej_Pmax_rel_100 ~ generation +  age_y  + sex +  sex:age_y + (1+age_y|name2), data = data_mod_end_kraft2, REML = F) 

#modellannahmen prüfen:
plot(mm_kraft_end2) #ok
qqnorm(resid(mm_kraft_end2)) #ok
qqPlot(resid(mm_kraft_end2)) #ok
hist(resid(mm_kraft_end2)) #ok

summary(mm_kraft_end2)

# und plotten:
# perf vs alter
(kraft2 <- ggplot(data_mod_end_kraft2, aes(x = age_y, y = Ej_Pmax_rel_100,  color = sex))+
    geom_point(alpha = 0.25, aes(size = generation))+
    geom_line(aes(group = name2), alpha = 0.25)+
    geom_line(aes(y = predict(mm_kraft_end2, re.form = NA), group = name2, size = generation))+
    scale_size_discrete(range = c(1, 2))+
    theme_bw()+
    #scale_color_colorblind()+
    labs(title = "SKI ALPIN: Elastojump 100% rel. vs. Alter",
         subtitle = "Mixed effects model fit:\nEj_Pmax_rel_100 ~ generation +  age_y  + sex + (1+age_y|name)",
         y = "Rel. Power Elastojump (W/kg)",
         x = "Age (y)"))

ggsave(filename = "kraft_skialpin_elasto100_vs_alter_300.jpeg", plot = kraft2, width = 25, height = 15, units = "cm")



## KRAFT III (Statojump rel 100) -------------------------------------------------- ####

## COMPARE GENERATIONS BY TIME EVOLUTION

## mixed model kraft - für Fmax_iso, weil das am besten tönt...

kraft_mod3 <- kraft_final  %>%  drop_na(Sj_Pmax_rel_100, generation) %>%   filter(generation != "X") %>% filter(age_y < 21) #ages between 15 und 25

# modellwahl
# ausgangsmodell (nur 1 sportart): Sj_Pmax_rel_100 ~ generation +  age_y  + sex + sex:age_y +  generation:age_y +(1+age_y|name)
mm_kraft_base3 <-  lmer(Sj_Pmax_rel_100 ~ generation +   sex +  (1+age_y|name2), data = kraft_mod2, REML = F)
mm_kraft_vglc3 <-  lmer(Sj_Pmax_rel_100 ~ generation +  sex + (1+age_y|name2), data = kraft_mod2, REML = F)
anova(mm_kraft_base3,mm_kraft_vglc3) 

drop1(mm_kraft_vglc3, test = "Chisq")

summary(mm_kraft_base3)
coef(mm_kraft_base3)

plot(mm_kraft_base3)


#bestes modell nach ausprobieren (für ages 15 bis 25): Sj_Pmax_rel_100 ~ generation +  sex + (1+age_y|name2)

# modell auf zu plottende daten anpassen
data_mod_end_kraft3 <- kraft_final  %>% drop_na(Sj_Pmax_rel_100, generation) %>%   filter(generation != "X") %>% filter(age_y < 21)
mm_kraft_end3  <- lmer(Sj_Pmax_rel_100 ~ generation +   sex +(1+age_y|name2), data = data_mod_end_kraft3, REML = F) 

#modellannahmen prüfen:
plot(mm_kraft_end3) #ok
qqnorm(resid(mm_kraft_end3)) #ok
qqPlot(resid(mm_kraft_end3)) #ok
hist(resid(mm_kraft_end3)) #ok

summary(mm_kraft_end3)

# und plotten:
# perf vs alter
(kraft3 <- ggplot(data_mod_end_kraft3, aes(x = age_y, y = Sj_Pmax_rel_100,  color = sex))+
    geom_point(alpha = 0.25, aes(size = generation))+
    geom_line(aes(group = name2), alpha = 0.25)+
    geom_line(aes(y = predict(mm_kraft_end3, re.form = NA), group = name2, size = generation))+
    scale_size_discrete(range = c(1, 2))+
    theme_bw()+
    #scale_color_colorblind()+
    labs(title = "SKI ALPIN: Statojump 100% rel. vs. Alter",
         subtitle = "Mixed effects model fit:\nSj_Pmax_rel_100 ~ generation +  sex + (1+age_y|name2)",
         y = "Rel. Power Statojump (W/kg)",
         x = "Age (y)"))

ggsave(filename = "kraft_skialpin_stato100_vs_alter_300_u21.jpeg", plot = kraft3, width = 25, height = 15, units = "cm")



## KRAFT IV (Elastojump rel 200) -------------------------------------------------- ####

## COMPARE GENERATIONS BY TIME EVOLUTION

## mixed model kraft - für Fmax_iso, weil das am besten tönt...

kraft_mod4 <- kraft_final  %>%  drop_na(Ej_Pmax_rel_200, generation) %>%   filter(generation != "X") %>% filter(age_y < 21) #ages between 15 und 25

# modellwahl
# ausgangsmodell (nur 1 sportart): Ej_Pmax_rel_200 ~ generation +  age_y  + sex + sex:age_y +  generation:age_y +(1+age_y|name2)
mm_kraft_base4 <-  lmer(Ej_Pmax_rel_200 ~ generation + sex +(1+age_y|name2), data = kraft_mod4, REML = F)
mm_kraft_vglc4 <-  lmer(Ej_Pmax_rel_200 ~ generation + sex +(1+age_y|name2), data = kraft_mod4, REML = F)
anova(mm_kraft_base4,mm_kraft_vglc4) 

drop1(mm_kraft_vglc4, test = "Chisq")

summary(mm_kraft_base4)
coef(mm_kraft_base4)

plot(mm_kraft_base4)


#bestes modell nach ausprobieren (für ages 15 bis 25): Ej_Pmax_rel_200 ~ generation + sex +(1+age_y|name2)

# modell auf zu plottende daten anpassen
data_mod_end_kraft4 <- kraft_final  %>% drop_na(Ej_Pmax_rel_200, generation) %>%   filter(generation != "X") %>% filter(age_y < 21)
mm_kraft_end4  <- lmer(Ej_Pmax_rel_200 ~ generation + sex +(1+age_y|name2), data = data_mod_end_kraft4, REML = F) 

#modellannahmen prüfen:
plot(mm_kraft_end4) #ok
qqnorm(resid(mm_kraft_end4)) #ok
qqPlot(resid(mm_kraft_end4)) #ok
hist(resid(mm_kraft_end4)) #ok

summary(mm_kraft_end4)

# und plotten:
# perf vs alter
(kraft4 <- ggplot(data_mod_end_kraft4, aes(x = age_y, y = Ej_Pmax_rel_200,  color = sex))+
    geom_point(alpha = 0.25, aes(size = generation))+
    geom_line(aes(group = name2), alpha = 0.25)+
    geom_line(aes(y = predict(mm_kraft_end4, re.form = NA), group = name2, size = generation))+
    scale_size_discrete(range = c(1, 2))+
    theme_bw()+
    #scale_color_colorblind()+
    labs(title = "SKI ALPIN: Elastojump 200% rel. vs. Alter",
         subtitle = "Mixed effects model fit:\nEj_Pmax_rel_200 ~ generation + sex +(1+age_y|name2)",
         y = "Rel. Power Elastojump (W/kg)",
         x = "Age (y)"))

ggsave(filename = "kraft_skialpin_elasto200_vs_alter_300_u21.jpeg", plot = kraft4, width = 25, height = 15, units = "cm")



## KRAFT V (Statojump rel 200) ---------------------------------------------------- ####

## COMPARE GENERATIONS BY TIME EVOLUTION


kraft_mod5 <- kraft_final  %>%  drop_na(Sj_Pmax_rel_200, generation) %>%   filter(generation != "X") %>% filter(age_y < 21) #ages between 15 und 25

# modellwahl
# ausgangsmodell (nur 1 sportart): Sj_Pmax_rel_200 ~ generation +  age_y  + sex + sex:age_y +  generation:age_y +(1+age_y|name2)
mm_kraft_base5 <-  lmer(Sj_Pmax_rel_200 ~ generation +  age_y  + sex + sex:age_y  +(1|name2), data = kraft_mod5, REML = F)
mm_kraft_vglc5 <-  lmer(Sj_Pmax_rel_200 ~ generation +  age_y  + sex + sex:age_y  +(1|name2), data = kraft_mod5, REML = F)
anova(mm_kraft_base5,mm_kraft_vglc5) 

drop1(mm_kraft_vglc5, test = "Chisq")

summary(mm_kraft_base5)
coef(mm_kraft_base5)

plot(mm_kraft_base5)


#bestes modell nach ausprobieren (für ages 15 bis 25): Sj_Pmax_rel_200 ~ generation +  age_y  + sex + sex:age_y  +(1|name2)

# modell auf zu plottende daten anpassen
data_mod_end_kraft5 <- kraft_final  %>% drop_na(Sj_Pmax_rel_200, generation) %>%   filter(generation != "X") %>% filter(age_y < 21)
mm_kraft_end5  <- lmer(Sj_Pmax_rel_200 ~ generation +  age_y  + sex + sex:age_y  +(1|name2), data = data_mod_end_kraft5, REML = F) 

#modellannahmen prüfen:
plot(mm_kraft_end5) #ok
qqnorm(resid(mm_kraft_end5)) #ok
qqPlot(resid(mm_kraft_end5)) #ok
hist(resid(mm_kraft_end5)) #ok

summary(mm_kraft_end5)

# und plotten:
# perf vs alter
(kraft5 <- ggplot(data_mod_end_kraft5, aes(x = age_y, y = Sj_Pmax_rel_200,  color = sex))+
    geom_point(alpha = 0.25, aes(size = generation))+
    geom_line(aes(group = name2), alpha = 0.25)+
    geom_line(aes(y = predict(mm_kraft_end5, re.form = NA), group = name2, size = generation))+
    scale_size_discrete(range = c(1, 2))+
    theme_bw()+
    #scale_color_colorblind()+
    labs(title = "SKI ALPIN: Statojump 200% rel. vs. Alter",
         subtitle = "Mixed effects model fit:\nSj_Pmax_rel_200 ~ generation +  age_y  + sex + sex:age_y  +(1|name2)",
         y = "Rel. Power Statojump (W/kg)",
         x = "Age (y)"))

ggsave(filename = "kraft_skialpin_stato200_vs_alter_300_u21.jpeg", plot = kraft5, width = 25, height = 15, units = "cm")



## PHYSIO (für nix) --------------------------------------------------------------- ####

ggpairs(physio %>% select(2,9:13), aes(color = sex), alpha = 0.5) #sport hat zu viele levels um anständig beurteilt zu werden

# perf vs alter ohne mm
ggplot(physio %>% filter(generation != "X"), aes(x = age_y, y = ventral_resultat,  color = generation))+
  geom_point(alpha = 0.25, aes(size = generation))+
  geom_line(aes(group = name), alpha = 0.25)+
  #geom_line(aes(y = predict(mm_spiel_end8, re.form = NA), group = name, size = generation))+
  scale_size_discrete(range = c(1, 2))+
  theme_bw()+
  facet_grid(sex~.)
  labs(title = "Eishockey: Yoyo distance vs. Alter",
       subtitle = "Mixed effects model fit for ages 15 to 18:\nyoyo_m ~ generation +  age_y + sex + sex:age_y + (1|name)",
       y = "Yoyo test distance (m)",
       x = "Age (y)")
