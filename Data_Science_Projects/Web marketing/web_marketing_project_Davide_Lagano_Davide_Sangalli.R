#### LIBRARIES ####
{
library(dplyr)
library(magrittr)
library(ggplot2)
library(forcats)
library(tidyr)
library(caret)
library(MLmetrics)
library(partykit)
library(rpart)
library(e1071)
library(keras)
library(tidyverse)
library(lubridate)
#library(FSelector)
}
set.seed(12345)

#### IMPORTIAMO I 7 DATASET CHE SERVIRANNO PER LA NOSTRA ANALISI ####

### settiamo la cartella di lavoro ###
dir = "C:/Users/david/Desktop/data_science_lezioni/primo_anno/secondo_semestre/web marketing and communication management/project/datasets"
setwd(dir)

### iscrizione dei clienti fedeli ###
df_1_cli_fid <- read.csv("raw_1_cli_fid.csv", na.strings = c("NA", ""), encoding='utf-8',
                         sep=";")

### dettagli account dei clienti ###
df_2_cli_account <- read.csv("raw_2_cli_account.csv", na.strings = c("NA", ""),
                              encoding='utf-8',sep=";")

### indirizzi dei clienti ###
df_3_cli_address <- read.csv2("raw_3_cli_address.csv", na.strings = c(""), stringsAsFactors = F,
                              encoding='utf-8', sep=";")

### privacy riguardante i clienti ###
df_4_cli_privacy <- read.csv2("raw_4_cli_privacy.csv" , na.strings = c("NA", ""),
                              encoding='utf-8',sep=";")

### caratteristiche della campagna e-mail ###
df_5_camp_cat <- read.csv2("raw_5_camp_cat.csv" , na.strings = c("NA", ""),
                           encoding='utf-8',sep=";")

### eventi e-mail ###
df_6_camp_event <- read.csv2("raw_6_camp_event.csv" , na.strings = c("NA", ""),
                             encoding='utf-8',sep=";")

### dataset degli scontrini ###
df_7_tic <- read.csv2("raw_7_tic.csv", na.strings = c("NA", ""),encoding='utf-8',sep=";")



#### DATA pulizia ####

### df_1_cli_fid ###

## analisi preliminare ##
str(df_1_cli_fid)
summary(df_1_cli_fid)

## pulizia dei dati ##
df_1_cli_fid_clean <- df_1_cli_fid

## formattazione dei dati ##
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(DT_ACTIVE = as.Date(DT_ACTIVE))

## trasformiamo le varaibili categoriche numeriche in factor ##
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(ID_NEG = as.factor(ID_NEG)) %>%
  mutate(TYP_CLI_FID = as.factor(TYP_CLI_FID)) %>%
  mutate(STATUS_FID = as.factor(STATUS_FID))


## Controllo di consistenza: numero di FID per cliente ##
num_fid_x_cli <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  summarize(NUM_FIDs =  n_distinct(ID_FID), NUM_DATEs = n_distinct(DT_ACTIVE))


dist_num_fid_x_cli <- num_fid_x_cli %>%
  group_by(NUM_FIDs, NUM_DATEs) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI))

dist_num_fid_x_cli

# ci sono clienti con più FIDs
num_fid_x_cli %>% filter(NUM_DATEs == 3)

df_1_cli_fid %>% filter(ID_CLI == 621814)

## manteniamo il primo e l'ultimo FID ##
# primo --> data di registrazione
# ultimo --> features
df_1_cli_fid_first <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == min(DT_ACTIVE)) %>%
  arrange(ID_FID) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()


#Con il codice sopra prendiamo il primo, se invece usiamo il massimo prendiamo l'ultimo.
df_1_cli_fid_last <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == max(DT_ACTIVE)) %>%
  arrange(desc(ID_FID)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()



df_1_cli_fid_clean <- df_1_cli_fid_last %>%
  left_join(df_1_cli_fid_first %>%
              select(ID_CLI, FIRST_ID_NEG = ID_NEG, FIRST_DT_ACTIVE = DT_ACTIVE)
            ,  by = 'ID_CLI') %>%
  left_join(num_fid_x_cli %>%
              select(ID_CLI, NUM_FIDs) %>%
              mutate(NUM_FIDs = as.factor(NUM_FIDs))
            ,  by = 'ID_CLI')

## ricontrolliamo ##
str(df_1_cli_fid_clean)
summary(df_1_cli_fid_clean)

## esplorazione delle distribuzioni e relativo istogramma di alcune variabili del dataset ##

# COD_FID
df_1_cli_fid_clean %>%
  group_by(COD_FID) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))
 

# Si nota che il valore "STANDARD" è particolarmente preponderante rispetto alle altre
ggplot(df_1_cli_fid_clean, aes(x=COD_FID)) + geom_bar()



# TYP_CLI_FID
df_1_cli_fid_clean %>%
  group_by(TYP_CLI_FID) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

# Si nota che c'è un particolare sbilanciamento della variabile binaria, la quasi 
# totalità delle osservazioni è pari a 1
ggplot(df_1_cli_fid_clean, aes(x=TYP_CLI_FID)) + geom_bar()

# STATUS_FID
df_1_cli_fid_clean %>%
  group_by(STATUS_FID) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

# Dall'istogramma si nota che la quasi totalità dei clienti sono attivi
ggplot(df_1_cli_fid_clean, aes(x=STATUS_FID)) + geom_bar()

# ID_NEG
df_1_cli_fid_clean %>%
  group_by(ID_NEG) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs))  %>%
  arrange(desc(PERCENT))

ggplot(df_1_cli_fid_clean, aes(x=ID_NEG)) + geom_bar()

#Si ripropone lo stesso procedimento per il resto dei dataset.

### df_2_cli_account ###

## analisi preliminare ##
str(df_2_cli_account)
summary(df_2_cli_account)

## pulizia ##
df_2_cli_account_clean <- df_2_cli_account


## formattiamo le variabili categoriali numeriche in factor ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(W_PHONE = as.factor(W_PHONE))  %>%
  mutate(TYP_CLI_ACCOUNT = as.factor(TYP_CLI_ACCOUNT))

## correzione degli NA all'interno delle diverse categorie, utilizzando il pacchetto
## forcats

df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(W_PHONE = fct_explicit_na(W_PHONE, "0")) %>% #questa funzione trasforma una categoria in un nuovo level
  mutate(EMAIL_PROVIDER = fct_explicit_na(EMAIL_PROVIDER, "(missing)")) %>%
  mutate(TYP_JOB = fct_explicit_na(TYP_JOB, "(missing)"))
# Uno degli approcci migliori per gestire questo problema è trasformare il dato missing
# in una categoria.

## analisi delle variabili ##

# W_PHONE
# La quasi totalità dei clienti ha aggiunto il numero telefonico
df_2_cli_account_clean %>%
  group_by(W_PHONE) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

ggplot(df_2_cli_account_clean, aes(x=W_PHONE)) + geom_bar()

# TYP_JOB
# C'è un numero sproporzionato di missing value, non ha quindi senso consoderare tale variabile
# al fine dell'analisi.
df_2_cli_account_clean %>%
  group_by(TYP_JOB) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df_2_cli_account_clean %>%
  summarize(TOT_TYP_JOB = n_distinct(TYP_JOB))

ggplot(df_2_cli_account_clean, aes(x=TYP_JOB)) + geom_bar()

## ricontrolliamo ##
str(df_2_cli_account_clean)
summary(df_2_cli_account_clean)

## EMAIL_PROVIDER ##

df_2_cli_account_clean %>%
  group_by(EMAIL_PROVIDER) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df_2_cli_account_clean %>%
  summarize(TOT_EMAIL_PROVIDER = n_distinct(EMAIL_PROVIDER))

# E' possibile notare che i provider delle mail sono numerosi.
# Una categoria con molti valori non è influente, di conseguenza, si prendono i valori più
# significativi. Si procede, quindi, guardando la frequenza con cui compare un determinato valore.

freq_email_providers <- df_2_cli_account_clean %>%
  group_by(EMAIL_PROVIDER) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs))

head(freq_email_providers, 20)

#Un dato significativo sono i provider Gmail (40%).
clean_email_providers <- freq_email_providers %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  mutate(AUX = if_else(PERCENT_COVERED < 0.85 | (PERCENT_COVERED > 0.85 & lag(PERCENT_COVERED) < 0.85), 1,0)) %>%
  mutate(EMAIL_PROVIDER_CLEAN = if_else(AUX | EMAIL_PROVIDER == "(missing)", EMAIL_PROVIDER, "others"))

head(clean_email_providers, 20)

df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  left_join(clean_email_providers %>%
              select(EMAIL_PROVIDER, EMAIL_PROVIDER_CLEAN)
            , by = "EMAIL_PROVIDER") %>%
  select(-EMAIL_PROVIDER) %>%
  mutate(EMAIL_PROVIDER_CLEAN = as.factor(EMAIL_PROVIDER_CLEAN))

# EMAIL_PROVIDER_CLEAN
df_2_cli_account_clean %>%
  group_by(EMAIL_PROVIDER_CLEAN) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

ggplot(df_2_cli_account_clean, aes(x=EMAIL_PROVIDER_CLEAN)) + geom_bar()

## Introducendo la categoria "others", è possibile notare che 
# risulta il secondo valore più significativo. ##

## Ricontroliamo ##
str(df_2_cli_account_clean)
summary(df_2_cli_account_clean)


### df_3_cli_address ###

## Analisi preliminare ##
str(df_3_cli_address)
summary(df_3_cli_address)

## Pulizia ##
df_3_cli_address_clean <- df_3_cli_address

## conversione di PRV e REGION in fattoriali ##
df_3_cli_address_clean <- df_3_cli_address_clean %>%
  mutate(PRV = as.factor(PRV)) %>%
  mutate(REGION = as.factor(REGION)) %>%
  arrange(desc(PRV)) %>%
  distinct()

# ispezione dettagliata sul df_3_cli_address
df_3_cli_address_clean %>%
  group_by(w_CAP = !is.na(CAP), w_PRV = !is.na(PRV), w_REGION = !is.na(REGION)) %>%
  summarize(TOT_ADDs = n_distinct(ID_ADDRESS))

df_3_cli_address_clean %>% select(PRV, REGION) %>% distinct() %>% arrange(desc(nchar(as.character(PRV))))

# eliminazione dei record senza CAP - PRV - REGION
df_3_cli_address_clean <- df_3_cli_address_clean %>%
  filter(!is.na(CAP) & !is.na(PRV) & !is.na(REGION))
unique(df_3_cli_address_clean$PRV)


df_3_cli_address_clean$PRV <- factor(df_3_cli_address_clean$PRV)

str(df_3_cli_address_clean)
levels(df_3_cli_address_clean$PRV)


## esplorazione della distribuzione ##
# PRV
df_3_cli_address_clean %>%
  group_by(PRV) %>%
  summarize(TOT_ADDs = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_ADDs/sum(TOT_ADDs)) %>%
  arrange(desc(PERCENT))

ggplot(df_3_cli_address_clean, aes(x=PRV)) + geom_bar()

# REGION
df_3_cli_address_clean %>%
  group_by(REGION) %>%
  summarize(TOT_ADDs = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_ADDs/sum(TOT_ADDs)) %>%
  arrange(desc(PERCENT))

ggplot(df_3_cli_address_clean, aes(x=REGION)) + geom_bar()


## ricontrolliamo ##
str(df_3_cli_address_clean)
summary(df_3_cli_address_clean)

# Nella variabile relativa agli indirizzi, ci sono alcuni valori in cui la provincia è palesemente sbagliata.
#Ci sono valori senza cap, potremmo tagliarli e fare un filtro tenendo solamente i valori sensibili.
#Dai dati si può notare che Milano e Roma sono fortemente significativi (dato non sorprendente).

### df_4_cli_privacy ###

## analisi preliminare ##
str(df_4_cli_privacy)
summary(df_4_cli_privacy)

## pulizia ##
df_4_cli_privacy_clean <- df_4_cli_privacy

# trasformazione dei booleani in facotr
df_4_cli_privacy_clean <- df_4_cli_privacy_clean %>%
  mutate(FLAG_PRIVACY_1 = as.factor(FLAG_PRIVACY_1)) %>%
  mutate(FLAG_PRIVACY_2 = as.factor(FLAG_PRIVACY_2)) %>%
  mutate(FLAG_DIRECT_MKT = as.factor(FLAG_DIRECT_MKT))

## esplorazione della distribuzione ##
# FLAG_PRIVACY_1
df_4_cli_privacy_clean %>%
  group_by(FLAG_PRIVACY_1) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

ggplot(df_4_cli_privacy_clean, aes(x=FLAG_PRIVACY_1)) + geom_bar()

## Si può notare che il valore preponderante è "1"

# FLAG_PRIVACY_2
df_4_cli_privacy_clean %>%
  group_by(FLAG_PRIVACY_2) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

ggplot(df_4_cli_privacy_clean, aes(x=FLAG_PRIVACY_2)) + geom_bar()

## Si può notare che il valore preponderante è "1"

# FLAG_DIRECT_MKT
df_4_cli_privacy_clean %>%
  group_by(FLAG_DIRECT_MKT) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

ggplot(df_4_cli_privacy_clean, aes(x=FLAG_DIRECT_MKT)) + geom_bar()

## Si può notare che il valore preponderante è "1"

df_4_cli_privacy_clean %>%
  group_by(FLAG_PRIVACY_1, FLAG_PRIVACY_2, FLAG_DIRECT_MKT) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

## ricontrolliamo ##
str(df_4_cli_privacy_clean)
summary(df_4_cli_privacy_clean)

# Anche in questo quarto dataset ci sono dei valori categorici che vanno convertiti.
# C'è una divisione abbastanza equa.

### df_5_camp_cat ###

## analisi preliminare ##
str(df_5_camp_cat)
summary(df_5_camp_cat)

## pulizia ##
df_5_camp_cat_clean <- df_5_camp_cat

# Il campo CHANNEL_CAMP ha un valore, non rilevante.
df_5_camp_cat_clean <- df_5_camp_cat_clean %>%
  select(-CHANNEL_CAMP)

## esplorazione della distribuzione ##
# FLAG_DIRECT_MKT
df_5_camp_cat_clean %>%
  group_by(TYP_CAMP) %>%
  summarize(TOT_CAMPs = n_distinct(ID_CAMP)) %>%
  mutate(PERCENT = TOT_CAMPs/sum(TOT_CAMPs)) %>%
  arrange(desc(PERCENT))

ggplot(df_5_camp_cat_clean, aes(x=TYP_CAMP)) + geom_bar()

# Categorizzazione delle campagne. C'è un campo che contiene solo email, quindi è stato rimosso.
# Vediamo che la comunicazione di tipologia prodotto è quella maggiormente utilizzata.
# La seconda più utilizzata, è quella personalizzata (20%). 

### df_6_camp_event ###

## analisi preliminare ##
str(df_6_camp_event)
summary(df_6_camp_event)

## pulizia ##
df_6_camp_event_clean <- df_6_camp_event

# nonostante il campo EVENT_TIME è "datetime", associamo le date corrispondenti.
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  mutate(EVENT_DATE = as.Date(EVENT_DATE, format="%Y-%m-%dT%H:%M:%S"))

# Ai fini dell'analisi non fa differenza distinguere fra "ERRORS" e "BOUNCE".
# Li combiniamo, quindi, in una categoria comune "FAILURE" con "F" come EVENT_CODE prima di trasformare il campo in factor.
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  mutate(TYP_EVENT = as.factor(if_else(TYP_EVENT == "E" | TYP_EVENT == "B", "F", as.character(TYP_EVENT))))

## esplorazione della distribuzione ##
# type event
df_6_camp_event_clean %>%
  group_by(TYP_EVENT) %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT), TOT_CLIs = n_distinct(ID_CLI), TOT_CAMPs = n_distinct(ID_CAMP)) %>%
  mutate(PERCENT_EVENT = TOT_EVENTs/sum(TOT_EVENTs), PERCENT_CLI = TOT_CLIs/sum(TOT_CLIs), PERCENT_CAMP = TOT_CAMPs/sum(TOT_CAMPs)) %>%
  arrange(desc(PERCENT_EVENT), desc(PERCENT_EVENT), desc(PERCENT_CAMP))

ggplot(df_6_camp_event_clean %>% select(TYP_EVENT, ID_EVENT) %>% distinct(), aes(x=TYP_EVENT)) + geom_bar()
ggplot(df_6_camp_event_clean %>% select(TYP_EVENT, ID_CLI) %>% distinct(), aes(x=TYP_EVENT)) + geom_bar()
ggplot(df_6_camp_event_clean %>% select(TYP_EVENT, ID_CAMP) %>% distinct(), aes(x=TYP_EVENT)) + geom_bar()

# min - max dates
df_6_camp_event_clean %>% summarize(MIN_DATE = min(EVENT_DATE), MAX_DATE = max(EVENT_DATE))
#In questo ultimo dataset ci sono più di 2 milioni di righe, troppe da analizzare computazionalmente.
#Abbiamo un campo interpretato come un fattore, anche se sono date espresse in un formato non standard.
#In una prima fase esplorativa alcune informazioni vengono tagliate.

#Aspetto interessante: la maggior parte degli eventi sono "send" o "view".
#I click, in relazione alla quantità di eventi, sono relativamente pochi.
#Anche gli eventi relativo al numero di fallimenti è abbastanza basso.

# Capiamo quali sono gli intervalli di comunicazione. In particolare queste comunicazioni sono state inviate
# fra il 1 gennaio e il 30 aprile. Saranno tenuti in considerazione nel momento in cui si andranno a
# creare variabili più complicate.

### dataset 7 ###
str(df_7_tic)

df_7_tic_clean <- df_7_tic
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(ID_CLI = as.factor(ID_CLI)) %>%
  mutate(ID_NEG = as.factor(ID_NEG)) %>%
  mutate(ID_ARTICOLO = as.factor(ID_ARTICOLO)) %>%
  mutate(COD_REPARTO = as.factor(COD_REPARTO)) %>%
  mutate(DIREZIONE = as.factor(DIREZIONE))

df_7_tic_clean$DATETIME
str(df_7_tic_clean)

#### DATA PREPARATION ####
# Lo scopo della fase di preparazione è quello di creare un dataset finale (master)
# che contenga tutte le informazioni di cui abbiamo bisogno al fine di applicare i nostri modelli.
# Stiamo considerando gli eventi categorizzati per tipologia: send, view, click e failure.
# Iniziamo facendo un left join fra il dataset 5 e 6. Stiamo quindi aggiungendo al dataset degli eventi (6) la
# categorizzazione delle campagne, che possono essere local, national, newsletter, personalized e product.

df_6_camp_event_clean_w_type <- df_6_camp_event_clean %>%
  left_join(df_5_camp_cat_clean
            , by = "ID_CAMP")

# Creiamo un dataframe contenente gli eventi identificati come invio. Quindi partendo
#dal dataframe appena creato, filtriamo per il tipo evento "S" e teniamo le colonne che
#ci interessano. Chiamiamo l'ID dell'evento ID_EVENT_S, mentre la data dell'evento
#SEND_DATE.
df_sents <- df_6_camp_event_clean_w_type %>%
  filter(TYP_EVENT == "S") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_S = ID_EVENT, ID_CLI, ID_CAMP, TYP_CAMP, ID_DELIVERY, SEND_DATE = EVENT_DATE)

# Ripetiamo il ragionamento per gli eventi di apertura delle mail. Inoltre raggruppiamo per 
#gli identificativi del cliente, della campagna e della consegna e teniamo solamente
#la prima apertura (quindi quella con data minore).
df_opens <- df_6_camp_event_clean_w_type %>%
  filter(TYP_EVENT == "V") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_O = ID_EVENT, ID_CLI, ID_CAMP, TYP_CAMP, ID_DELIVERY, OPEN_DATE = EVENT_DATE) %>%
  group_by(ID_CLI, ID_CAMP, ID_DELIVERY) %>%
  filter(OPEN_DATE == min(OPEN_DATE)) %>%
  filter(row_number() == 1) %>%
  ungroup()

# Analizziamo chi ha cliccato almeno una volta una comunicazione. 
df_clicks <- df_6_camp_event_clean_w_type %>%
  filter(TYP_EVENT == "C") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_C = ID_EVENT, ID_CLI, ID_CAMP, TYP_CAMP, ID_DELIVERY, CLICK_DATE = EVENT_DATE) %>%
  group_by(ID_CLI, ID_CAMP, ID_DELIVERY) %>%
  filter(CLICK_DATE == min(CLICK_DATE)) %>%
  filter(row_number() == 1) %>%
  ungroup()

# Stesso procedimento per il fallimento.
df_fails <- df_6_camp_event_clean_w_type %>%
  filter(TYP_EVENT == "F") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_F = ID_EVENT, ID_CLI, ID_CAMP, TYP_CAMP, ID_DELIVERY, FAIL_DATE = EVENT_DATE) %>%
  group_by(ID_CLI, ID_CAMP, ID_DELIVERY) %>%
  filter(FAIL_DATE == min(FAIL_DATE)) %>%
  filter(row_number() == 1) %>%
  ungroup()

# Generalmente sui failure, l'invio viene tentato una volta sola, quindi mi aspetto un unico feedback.

# A questo punto creiamo un nuovo dataframe in cui mettiamo assieme i dati sull'invio e sull'apertura delle mail.
# Creiamo anche una colonna DIFF che identifica la finestra di tempo fra il momento di invio e di apertura della
# mail. Abbiamo filtrato considerando quelle che hanno NA come valore di open_date(cosa possibile in quanto abbiamo
# fatto il left join partendo dal dataframe degli invii, quindi sono presenti anche in casi in cui non è avvenuta 
# apertura) e quelli in cui la data di invio era precedente alla data di apertura. Il caso contrario, infatti,
# risultava essere senza senso.
df_sents_w_open <- df_sents %>%
  left_join(df_opens
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(OPEN_DATE) | SEND_DATE <= OPEN_DATE) %>%
  mutate(DIFF = as.integer(OPEN_DATE - SEND_DATE))

# Raggruppiamo per mail rispetto ai valori che hanno un valore non nullo all'interno della colonna DIFF.
# Calcoliamo in seguito la percentuale.
#Si può notare che circa l'82% delle comunicazioni vengono inviate e non aperte (valore: FALSE).
df_sents_w_open %>%
  group_by(w_open = !is.na(DIFF)) %>%
  summarize(TOT_SENTs = n_distinct(ID_EVENT_S)) %>%
  mutate(PERCENT = TOT_SENTs/sum(TOT_SENTs)) %>%
  arrange(desc(PERCENT))

ggplot(df_sents_w_open, aes(x=!is.na(DIFF))) + geom_bar()

# Analizziamo ora la distribuzione delle aperture rispetto ai diversi giorni.
#Per prima cosa filtriamo rispetto ai valori che non hanno valori nulli in DIFF.
#Raggruppiamo sempre per DIFF, rappresentante lo span di tempo che ci mettono per aprire le mail.
df_sents_w_open %>% filter(!is.na(DIFF)) %>%
  group_by(DIFF) %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)) %>%
  arrange(DIFF) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_EVENTs)/sum(TOT_EVENTs))

# Vediamo in quanti aprono la mail nello stesso giorno in cui è stata inviata (64%).
ggplot(df_sents_w_open %>% filter(!is.na(DIFF)) %>%
         group_by(DIFF) %>%
         summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)) %>%
         arrange(DIFF) %>%
         mutate(PERCENT_COVERED = cumsum(TOT_EVENTs)/sum(TOT_EVENTs)) %>%
         filter(DIFF <= 14)
       , aes(y=PERCENT_COVERED, x=DIFF)) + geom_line() + geom_point() + scale_x_continuous(breaks=seq(0,14,2), minor_breaks=0:14)
#Dal grafico possiamo vedere l'andamento della funzione e possiamo quindi decidere di scegliere 
#due giorni come finestra dell'apertura mail.

window_days <- 2

#A questo punto cominciamo a preparare la variabile target. In particolare, sarà uguale a 1
# se una mail inviata sarà aperta entro il timespan definito
#dalla finestra di giorni, impostata a 2, altrimenti il valore assunto sarà 0.


target_event <- df_sents_w_open %>%
  mutate(TARGET = as.factor(if_else(!is.na(DIFF) & DIFF <= window_days, "1", "0"))) %>%
  select(ID_EVENT_S, ID_CLI, ID_CAMP, ID_DELIVERY, SEND_DATE, TARGET)

# Definiamo le variabili che vogliamo calcolare nel modello.
# Variabili fondamentali: open-rate e click through dei clienti.

# alcune variabili rilevanti da inserire:
# - average open rate (within 14 days) of the communications received by the client in the 30 days before the sent
# - average click-through (within 14 days) rate of the communications received by the client in the 30 days before the sent

# in order to have comparable situation we are considering:
# - targeted sent made after the 2019-02-01 and window_days before 2019-04-30
# - targeted sent to clients registered by at least 30 days

# Verrà calcolata la media degli open_rate per ciascun cliente, sulle comuicazioni 
# ricevute nel mese precedente.
# verranno costruite delle metriche collegate all'id del cliente per le mail ricevute nel mese precedente.
# Cerchiamo di utilizzare solo gli eventi per cui sono disponibili dati per calcolare questa metrica.
# Verranno valutate le comunicazione inviate dal 2 febbraio, in modo da avere un mese precedente per 
# costruire la metrica.
# Tutte le variabili che vengono costruite nel database master devono essere confrontabili all'interno
# di ciascun record. Non avrebbe senso avere una valutazione fatta su X giorni e una su meno di X giorni.

rate_window <- 14
prev_window <- 30

dt_start <- as.Date("2019-02-01")
dt_end <- as.Date("2019-04-30") - window_days

# Nella pipeline sottostante viene creato un dataframe contenente i dati sugli eventi rilevanti nella finestra precedente. 
# Primo left join: invii --> aperture
# Facciamo un filtro mantenendo i valori che hanno nell'open_date o nella data
# di invio precedente a quella di apertura, un valore nullo.
# Secondo left join: invii+aperture --> click
# Filtriamo mantenendo i campi che hanno nel click_date o nella data di
# invio precedente a quella di click, un valore nullo.
# Terzo left join: invii+aperture+click --> failure
# Utilizziamo la stessa metodologa di filtraggio, successivamente ci si crea due nuovi
# attributi: la differenza fra l'open_date e la send_date (diff_open) e fra clickdate e send date (diff_click). 
# In seguito filtriamo tenendo i valori che hanno i campi sopra indicati vuoti, oppure per quelli i cui valori sono
# inferiore alla window rate di 14 giorni.
relevant_event <- df_sents %>%
  left_join(df_opens
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(OPEN_DATE) | SEND_DATE <= OPEN_DATE) %>%
  left_join(df_clicks
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(CLICK_DATE) | SEND_DATE <= CLICK_DATE) %>%
  left_join(df_fails
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(FAIL_DATE) | SEND_DATE <= FAIL_DATE) %>%
  mutate(DIFF_OPEN = as.integer(OPEN_DATE - SEND_DATE)) %>%
  mutate(DIFF_CLICK = as.integer(CLICK_DATE - SEND_DATE)) %>%
  filter(is.na(DIFF_OPEN) | DIFF_OPEN < rate_window) %>%
  filter(is.na(DIFF_CLICK) | DIFF_CLICK < rate_window)

# Cambiamo i nomi delle colonne in modo da identificarli in modo diverso.
names(relevant_event) <- sapply(names(relevant_event), paste0, "_PREV")


target_event_w_prev <- target_event %>% filter(SEND_DATE >= dt_start & SEND_DATE <= dt_end) %>%
  left_join(relevant_event
            , by = c("ID_CLI" = "ID_CLI_PREV")
  ) %>%
  filter(is.na(SEND_DATE_PREV) | (SEND_DATE_PREV < SEND_DATE & SEND_DATE <= SEND_DATE_PREV + prev_window)) %>%
  mutate(OPENED = if_else(OPEN_DATE_PREV <= SEND_DATE & SEND_DATE <= OPEN_DATE_PREV + prev_window, 1, 0)) %>%
  mutate(CLICKED = if_else(CLICK_DATE_PREV <= SEND_DATE & SEND_DATE <= CLICK_DATE_PREV + prev_window, 1, 0)) %>%
  mutate(FAILED = if_else(!is.na(ID_EVENT_F_PREV), 1, 0)) %>%
  group_by(ID_EVENT_S, ID_CLI, ID_CAMP, ID_DELIVERY, SEND_DATE,  TARGET) %>%
  summarize(NUM_SEND_PREV = n_distinct(ID_EVENT_S_PREV, na.rm = T)
            , NUM_OPEN_PREV = sum(OPENED, na.rm = T)
            , NUM_CLICK_PREV = sum(CLICKED, na.rm = T)
            , NUM_FAIL_PREV = sum(FAILED, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(OPEN_RATE_PREV = NUM_OPEN_PREV/NUM_SEND_PREV) %>%
  mutate(CLICK_RATE_PREV = NUM_CLICK_PREV/NUM_OPEN_PREV) %>%
  mutate(W_SEND_PREV = as.factor(NUM_SEND_PREV > 0)) %>%
  mutate(W_FAIL_PREV = as.factor(NUM_FAIL_PREV > 0)) %>%
  mutate(SEND_WEEKDAY = as.factor(weekdays(SEND_DATE))) %>%
  mutate(OPEN_RATE_PREV = if_else(is.na(OPEN_RATE_PREV), 0, OPEN_RATE_PREV)) %>%
  mutate(CLICK_RATE_PREV = if_else(is.na(CLICK_RATE_PREV), 0, CLICK_RATE_PREV))

# Costruzione del dataset master terminata. Questo conterrà le info associate a open_rate e click_rate
# Ultimo step: aggiungere le info collegate al cliente, all'account all'indirizzo.
View(target_event_w_prev)

# add client data
df_master <- target_event_w_prev %>%
  left_join(df_1_cli_fid_clean %>%
              select(ID_CLI, ID_NEG, TYP_CLI_FID, COD_FID, STATUS_FID, FIRST_DT_ACTIVE, NUM_FIDs)
            , by = "ID_CLI") %>%
  filter(FIRST_DT_ACTIVE <= SEND_DATE) %>%
  mutate(AGE_FID = as.integer(SEND_DATE - FIRST_DT_ACTIVE)) %>%
  left_join(df_2_cli_account_clean
            , by = "ID_CLI") %>%
  left_join(df_3_cli_address_clean %>%
              select(ID_ADDRESS, PRV, REGION)
            , by = "ID_ADDRESS") %>%
  left_join(df_4_cli_privacy_clean
            , by = "ID_CLI") %>%
  mutate(PRV = fct_explicit_na(PRV)) %>%
  mutate(REGION = fct_explicit_na(REGION)) %>%
  select(-ID_ADDRESS, -ID_CAMP, -ID_DELIVERY, -SEND_DATE, -FIRST_DT_ACTIVE)

# E' stato aggiunto il dato di quando è stata inviata la mail.

View(df_master)

# Controlliamo se non ci sono duplicati

df_master %>%
  group_by(ID_EVENT_S) %>% 
  summarize(num = n()) %>% 
  group_by(num) %>%
  count()

#### DATA ESPLORATION ####
# Cerchiamo di capire la numerosità dei clienti con target positivo.

# Visualizziamo la frequenza dell'evento.
df_master %>%
  group_by(TARGET) %>%
  summarize(NUM_EVENTs = n_distinct(ID_EVENT_S)) %>%
  mutate(PERCENT = NUM_EVENTs/sum(NUM_EVENTs))

# Comportamento dei clienti nel mese precedente

df_master %>%
  group_by(TARGET,  W_SEND_PREV) %>%
  summarize(NUM_EVENTs = n_distinct(ID_EVENT_S), mean_OR = mean(OPEN_RATE_PREV, na.rm = T))
# Si nota che i valori dell'open_rate sono fortemente polarizzati, di cui solo il 9% è parte della classe negativa.
# Se guardo gli eventi con target 1 ho un open_rate del 58%.
# Questo è un indicatore rapido in fase di costruzione che ci permette di capire che c'è una netta divisione.

str(df_master)
summary(df_master)

### T-TEST ###
# Controlliamo se le variabili continue hanno una differenza significativa.

var <- c("NUM_SEND_PREV","NUM_OPEN_PREV", "NUM_CLICK_PREV", "NUM_FAIL_PREV", "OPEN_RATE_PREV", "CLICK_RATE_PREV", "AGE_FID")
cor(df_master[,var])

t.test(NUM_SEND_PREV ~ TARGET, data = df_master)
t.test(NUM_OPEN_PREV ~ TARGET, data = df_master)
t.test(NUM_CLICK_PREV ~ TARGET, data = df_master)
t.test(NUM_FAIL_PREV ~ TARGET, data = df_master)
t.test(OPEN_RATE_PREV ~ TARGET, data = df_master)
t.test(CLICK_RATE_PREV ~ TARGET, data = df_master)
t.test(AGE_FID ~ TARGET, data = df_master)
# In alcuni casi questi test preliminari tendono ad essere molto positivi.
# Quasi tutte le variabili risultano significative.


# Questa funzione ci permette di poter preparare le variabili categoriche per il test chi-quadro.
prepare_chisq <- function(df, x){
  y <- enquo(x)
  
  
  test_df <- df %>%
    mutate(KEY = if_else(TARGET == "1", "OK", "KO")) %>%
    select(UQ(y), KEY, ID_EVENT_S) %>%
    group_by(UQ(y), KEY) %>%
    summarize(n = n()) %>%
    spread(KEY, n) %>%
    ungroup() %>%
    as.data.frame()
  
  test_m <- test_df %>%
    select(OK, KO) %>%
    mutate(OK = if_else(is.na(OK), as.integer(0), OK)) %>%
    mutate(KO = if_else(is.na(KO), as.integer(0), KO)) %>%
    as.matrix() 
  row.names(test_m) <- as.character(test_df[,1])
  
  return(test_m)
}

plot_factor <- function(df, x, lab){
  y <- enquo(x)
  
  df_count_tot <- df %>%
    group_by(UQ(y)) %>%
    summarise(n_tot = n_distinct(ID_EVENT_S)) %>%
    ungroup()
  
  df_count <- df %>%
    group_by(UQ(y), TARGET) %>%
    summarise(n = n_distinct(ID_EVENT_S))
  
  df <- df_count %>%
    left_join(df_count_tot, by = lab) %>%
    mutate(frac = round(n / n_tot, 2))
  
  ggplot(data=df, aes(x=UQ(y), y=frac, fill=TARGET)) +
    geom_bar(stat="identity", position=position_dodge()) +
    geom_text(aes(x=UQ(y), y=frac, label = frac),
              position = position_dodge(width = 1),
              vjust = 2, size = 3, color = "white", fontface = "bold")
}

# Eseguiamo un test chi-qadro con un successivo plot per le variabili categoriche
# presenti all'interno del df_master.


chisq.test(prepare_chisq(df_master, W_SEND_PREV))
plot_factor(df_master, W_SEND_PREV, "W_SEND_PREV")

#p-value alto: ipotesi nulla accettata
#in altri casi per esempio se si guarda il giorno della settimana dell'invio le oscillazioni sono lievi
#ma il p-value è basso.

chisq.test(prepare_chisq(df_master, W_FAIL_PREV))
plot_factor(df_master, W_FAIL_PREV, "W_FAIL_PREV")

chisq.test(prepare_chisq(df_master, SEND_WEEKDAY))
plot_factor(df_master, SEND_WEEKDAY, "SEND_WEEKDAY")

chisq.test(prepare_chisq(df_master, ID_NEG))
plot_factor(df_master, ID_NEG, "ID_NEG")

chisq.test(prepare_chisq(df_master, TYP_CLI_FID))
plot_factor(df_master, TYP_CLI_FID, "TYP_CLI_FID")

chisq.test(prepare_chisq(df_master, COD_FID))
plot_factor(df_master, COD_FID, "COD_FID")

chisq.test(prepare_chisq(df_master, STATUS_FID))
plot_factor(df_master, STATUS_FID, "STATUS_FID")

chisq.test(prepare_chisq(df_master, NUM_FIDs))
plot_factor(df_master, NUM_FIDs, "NUM_FIDs")

chisq.test(prepare_chisq(df_master, W_PHONE))
plot_factor(df_master, W_PHONE, "W_PHONE")

chisq.test(prepare_chisq(df_master, TYP_JOB))
plot_factor(df_master, TYP_JOB, "TYP_JOB")

chisq.test(prepare_chisq(df_master, EMAIL_PROVIDER_CLEAN))
plot_factor(df_master, EMAIL_PROVIDER_CLEAN, "EMAIL_PROVIDER_CLEAN")

chisq.test(prepare_chisq(df_master, PRV))
plot_factor(df_master, PRV, "PRV")

chisq.test(prepare_chisq(df_master, REGION))
plot_factor(df_master, REGION, "REGION")

chisq.test(prepare_chisq(df_master, FLAG_PRIVACY_1))
plot_factor(df_master, FLAG_PRIVACY_1, "FLAG_PRIVACY_1")

chisq.test(prepare_chisq(df_master, FLAG_PRIVACY_2))
plot_factor(df_master, FLAG_PRIVACY_2, "FLAG_PRIVACY_2")

chisq.test(prepare_chisq(df_master, FLAG_DIRECT_MKT))
plot_factor(df_master, FLAG_DIRECT_MKT, "FLAG_DIRECT_MKT")

## COSTRUZIONE DEI MODELLI ##

#Elimino le righe che contengono dei valori nulli.
#Il numero di righe rimane uguale perché non abbiamo valori nulli nel dataset).

df_master <- df_master[complete.cases(df_master),]

# Analisi preliminari sul dataset: guardiamo chi è più propenso ad aderire alla
# campagna mail. L'analisi è fatta sul tipo di cliente (premium,standard ecc...).
# Si nota che coloro che sono più propensi ad aderire alla campagna mail sono i clienti
# con un account STANDARD, seguiti da quelli con account PREMIUM. 

subset <- subset(df_master,select=c(COD_FID,TARGET))
code_fidelity <- subset %>% filter(TARGET==1)
stats <- count(code_fidelity %>% group_by(COD_FID))
total <- sum(stats$n)
perc_premium <- ((stats %>% filter(COD_FID=="PREMIUM") %>% pull(n))/total) *100
perc_premium_biz <- ((stats %>% filter(COD_FID=="PREMIUM BIZ") %>% pull(n))/total) *100
perc_standard <- ((stats %>% filter(COD_FID=="STANDARD") %>% pull(n))/total) *100
perc_standard_biz <- ((stats %>% filter(COD_FID=="STANDARD BIZ") %>% pull(n))/total) *100


df_master <- dplyr::select(df_master,-c("ID_EVENT_S","ID_NEG"))

sample_fs <- df_master[sample(nrow(df_master),400000),]

ig <-information.gain(TARGET~., sample_fs)


# Facendo una feature selection, basata sull'importanza delle variabili, quella che risulta
# più significativa è OPEN_RATE_PREV, seguita da CLICK_RATE_PREV e NUM_SEND_PREV.
# Perciò costruiamo un dataset che contenga, oltre alla variabile TARGET, solo queste variabili esplicative.

df_master_new <- subset(df_master,select=-c(TYP_JOB,PRV,REGION,EMAIL_PROVIDER_CLEAN,
                                            W_SEND_PREV,FLAG_PRIVACY_1, FLAG_PRIVACY_2,
                                            FLAG_DIRECT_MKT,W_PHONE,AGE_FID,NUM_FIDs,
                                            STATUS_FID,STATUS_FID,SEND_WEEKDAY,
                                            TYP_CLI_FID,W_SEND_PREV,W_FAIL_PREV,
                                            NUM_FAIL_PREV,COD_FID,TYP_CLI_ACCOUNT,
                                            NUM_SEND_PREV,NUM_OPEN_PREV,NUM_CLICK_PREV))



plot(df_master_new$TARGET)

# possiamo notare che c'è una forte class imbalance: la classe positiva (1), è nettamente minore
# rispetto alla classe 0. Questo potrebbe portare ad una distorsione nella classificazione
# della variabile TARGET da parte dei vari modelli, perciò quello che si può fare,
# è bilanciare il training set in modo da poter addestrare meglio il modello e ottenere quindi
# misure più affidabili.

# TRAIN AND TEST SPLIT

# viene fatto uno stratified sampling rispetto alla variabile TARGET, dividendo il dataset
# in modo che il 75% delle osservazioni corrisponda al train set e il restante al test 
# set.

training_indices <- df_master_new$TARGET %>% createDataPartition(p=0.75)

training_set <- df_master_new[unlist(training_indices), ]
test_set <- df_master_new[-unlist(training_indices), ]

# poiché il dataset è sbilanciato, effettuiamo ora un undersampling della classe maggioritaria (0),
# estraendo dalle osservazioni la cui target è "0", un campione pari al numero delle osservazioni
# della classe con target "1".
# Abbiamo effettuato un undersampling in quanto la numerosità delle variabili ce lo permetteva,
# di conseguenza, non andiamo incontro ad una perdita consistente di dati.

ones_mail <- subset(training_set,TARGET==1)
zeros_mail <- subset(training_set,TARGET==0)

sample_zeros <- zeros_mail[sample(nrow(zeros_mail),126068),]

df_train_final <- rbind(sample_zeros,ones_mail)

df_train_final <- df_train_final[sample(nrow(df_train_final)),]

# "df_train_final" costituisce il nostro dataset di train, con le classe perfettamente bilanciate.
# Inoltre, per garantire un miglior processo di training viene anche effettuato lo shuffle dello
# stesso.

# Dopo aver provato numerosi modelli, è stato deciso di presentare solamente i 3 modelli che
# performavano meglio.
# Passiamo ora alla presentazione del primo modello: è stato scelto il decision_tree.

# Segue l'addestramento del modello:

decision_tree <- rpart(TARGET~OPEN_RATE_PREV+ CLICK_RATE_PREV, data=df_train_final)

# Viene di seguito testato il modello sul test set.

pred_decision_tree <- predict(decision_tree,test_set,type="class")
pred_proba_dt <- predict(decision_tree,test_set,type="prob")

# Vengono estratte le probabilità, per ogni osservazione, di appartenere alla classe "0" o "1"

pos_proba_dt <- pred_proba_dt[,2]
neg_proba_dt <- pred_proba_dt[,1]

# Eseguiamo la confusion matrix e i test Recall, Precision e F1_Score

MLmetrics::ConfusionMatrix(y_pred=pred_decision_tree,y_true=test_set$TARGET)

Recall(y_pred=pred_decision_tree,y_true=test_set$TARGET,positive="1")
Precision(y_pred=pred_decision_tree,y_true=test_set$TARGET,positive="1")
F1_Score(y_pred=pred_decision_tree,y_true=test_set$TARGET,positive="1")

# Calcoliamo la Roc Curve sul test set.
# Viene comunque tenuta poco in considerazione, in quanto con il problema di class imbalance, risulta
# molto più affidabile la "Precision-Recall Curve".

roc_test_dt <- PRROC::roc.curve(scores.class0 = pos_proba_dt,
                             weights.class0 = as.numeric(test_set$TARGET),curve=TRUE)

plot(roc_test_dt)

pr_test_dt <- PRROC:: pr.curve(scores.class0=pos_proba_dt,scores.class1=neg_proba_dt,
                            curve=TRUE)

plot(pr_test_dt)

# Viene ora presentato il modello "Naive Bayes" e analogamente a quanto fatto prima, vengono calcolate
# le relative misure

nb <- naiveBayes(TARGET ~ OPEN_RATE_PREV + CLICK_RATE_PREV, data = df_train_final)
pred_nb <- predict(nb,test_set)
pred_proba_nb <- predict(nb,test_set, type="raw")
pos_proba_nb <- pred_proba_nb[,2]
neg_proba_nb <- pred_proba_nb[,1]

MLmetrics::ConfusionMatrix(pred_nb,test_set$TARGET)
Recall(y_pred=pred_nb,y_true=test_set$TARGET,positive="1")
Precision(y_pred=pred_nb,y_true=test_set$TARGET,positive="1")
F1_Score(y_pred=pred_nb,y_true=test_set$TARGET,positive="1")

roc_test_nb <- PRROC::roc.curve(scores.class0 = pos_proba_nb,
                                weights.class0 = as.numeric(test_set$TARGET),curve=TRUE)

plot(roc_test_nb)

pr_test_nb <- PRROC:: pr.curve(scores.class0=pos_proba_nb,scores.class1=neg_proba_nb,
                               curve=TRUE)

plot(pr_test_nb)

# Viene di seguito presentato l'ultimo modello: una rete neurale. Per poterla addestrare,
# abbiamo dovuto togliere dal train e dal test set, la variabile di target, salvando quest'ultima
# in un nuovo dataframe. Per esigenze di architettura della rete, abbiamo dovuto poi
# successivamente trasformare le labels e entrambi i set in matrici.

df_train_final %>% pull(TARGET) -> training_labels
test_set %>% pull(TARGET) -> test_labels

training_labels <- as.matrix(training_labels)
test_labels <- as.matrix(test_labels)

#eliminiamo dal training set e dal test set la colonna TARGET, in quanto già contenuta
#nelle apposite variabili definita sopra.

df_train_final_nn <- subset(df_train_final,select=-c(TARGET))
test_set_nn <- subset(test_set,select=-c(TARGET))

df_train_final_nn <- as.matrix(df_train_final_nn)
test_set_nn <- as.matrix(test_set_nn)

# Viene di seguito presentata l'architettura della rete neurale. Successivamente viene
# utilizzata una funzione di perdita "sparse_categorical_crossentropy", come ottimizzatore
# viene utilizzato un "adam" otimizer, come metrica l'accuratezza, benchè ad essa non verrà
# data rilevanza nell'analisi succesiva del modello

df_train_nn <- subset(df_train_final_nn,select=-c(ID_CLI))
df_test_nn <- subset(test_set_nn,select=-c(ID_CLI))

df_train_nn <- as.matrix(df_train_nn)
df_test_nn <- as.matrix(df_test_nn)


model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 256, activation = 'relu',input_shape=ncol(df_train_nn)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 2, activation = 'softmax')


model %>% compile(
  loss = 'sparse_categorical_crossentropy',
  optimizer = 'adam',
  metrics=c('accuracy')
)

# i dati vengono processati in batch da 256 e vengono eseguite 50 epoche 
# (cicli di aggiornamento della rete). Inoltre il train set viene ulteriormente diviso e
# viene creato un validation test. Il test set, per la rete neurale, rappresenterà un 
# set di dati mai visto prima.

history <- model %>% fit(
  df_train_nn, training_labels, 
  epochs = 50, batch_size = 256,
  validation_split=0.2
)

#viene valutato il test set e si nota che l'accuratezza è molto buona e praticamente
#non si discosta da quella raggiunta sia dal train che dal validation test.

model %>% evaluate(df_test_nn, test_labels)
predictions_test_set <- model %>% predict_classes(df_test_nn)

#vengono poi calcolate le probabilità di ogni osservazione di appartenere alla classe 
#positiva oppure a quella negativa

class_prob <- model %>% predict_proba(df_test_nn)
pos_class_prob <- class_prob[,2]
neg_class_prob <- class_prob[,1]


#vengono poi calcolate le più comuni metriche, oltre all'accuratezza, già calcolata in 
#preccendeza

ConfusionMatrix(y_pred=predictions_test_set,y_true=test_labels)

Recall(y_pred=predictions_test_set,y_true=test_labels,positive="1")
Precision(y_pred=predictions_test_set,y_true=test_labels,positive="1")
F1_Score(y_pred=predictions_test_set,y_true=test_labels,positive="1")

#viene poi plottata la roc curve per la rete neurale, anche se non ne diamo molta
# importanza, per il motivo già esplicitato prima, da come risulta dal grafico,
#l'AUC è molto buona (0.825)

roc_test <- PRROC::roc.curve(scores.class0 = pos_class_prob,
                            weights.class0 = as.numeric(test_labels),curve=TRUE)

plot(roc_test)

#viene anche plottata la Precision-Recall Curve

pr_test <- PRROC:: pr.curve(scores.class0=pos_class_prob,scores.class1=neg_class_prob,
                            curve=TRUE)

plot(pr_test)

data <- df_7_tic %>%
  mutate(DATE = as.Date(DATETIME, format="%Y-%m-%dT%H%M%S")) %>% dplyr::select(-DATETIME)

data$IMPORTO_NETTO <- data$IMPORTO_LORDO - data$SCONTO
data$ID_CLI <- as.factor(data$ID_CLI)

scontrini <- data %>% filter(DATE < (as.Date("2019-04-30") - 50))
scontrini_rest <- scontrini  %>% filter(DIREZIONE == -1)
scontrini <- scontrini %>% filter(DIREZIONE == 1)


data_rest <- data %>% filter(DIREZIONE == -1)
data <- data %>% filter(DIREZIONE == 1)

df_cli <- inner_join(data %>% group_by(ID_CLI) %>% summarise(TOTALE_NETTO = sum(IMPORTO_NETTO)),
                     data %>% group_by(ID_CLI) %>% count(ID_CLI), by = "ID_CLI") %>%
  mutate(NUMERO_ACQUISTI_TOT = n) %>% dplyr::select(-n)


df_temp <- as.data.frame(table((data %>% group_by(ID_CLI, ID_SCONTRINO)%>% count(ID_CLI))$ID_CLI),
                         ID_CLI = as.factor())
df_temp$ID_CLI <- df_temp$Var1
df_temp$NUMERO_SCONTRINI <- df_temp$Freq
df_temp$Var1 <- NULL
df_temp$Freq <- NULL

df_cli <- inner_join(df_cli, df_temp, by = "ID_CLI")

df_rest <- inner_join(data_rest %>% group_by(ID_CLI) %>% count(ID_CLI),
                      data_rest %>% group_by(ID_CLI) %>% summarise(TOTALE_NETTO_REST = sum(IMPORTO_NETTO)),
                      by = "ID_CLI") %>% ungroup() %>% mutate(NUMERO_REST_ART = n) %>% dplyr::select(-n)


df_master_c <- left_join(df_cli, df_rest, by = "ID_CLI")
df_master_c$NUMERO_REST_ART[is.na(df_master_c$NUMERO_REST_ART)] <- 0
df_master_c$TOTALE_NETTO_REST[is.na(df_master_c$TOTALE_NETTO_REST)] <- 0

d1 <- data %>% group_by(ID_CLI) %>% filter(DATE == min(DATE)) %>% 
  dplyr::select(c(ID_CLI,DATE))
d1 <- unique(d1)

d2 <- data %>% group_by(ID_CLI) %>% filter(DATE == max(DATE)) %>% dplyr::select(c(ID_CLI,DATE))
d2 <- unique(d2)

d1$DATE_MIN <- d1$DATE
d1$DATE <- NULL
d2$DATE_MAX <- d2$DATE
d2$DATE <- NULL

d3 <- df_master_c %>% left_join(d1, by = "ID_CLI") %>% left_join(d2, by = "ID_CLI")

prova <- data %>% group_by(ID_CLI) %>% count(n_distinct(DATE))
prova <- prova %>% mutate(NUM_INGRESSI = `n_distinct(DATE)`) %>% dplyr::select(ID_CLI,NUM_INGRESSI)

d3 <- d3 %>% left_join(prova, by = "ID_CLI")

d3$DIFF <- d3$DATE_MAX - d3$DATE_MIN
d3$FREQ <- d3$DIFF / (d3$NUM_INGRESSI -1)
d3 %>% filter(DATE_MIN == DATE_MAX & NUMERO_SCONTRINI >= 2)
d3 <- d3 %>% filter(DATE_MIN != DATE_MAX) 

d4 <- d3 %>% filter(DATE_MIN < (as.Date("2019-04-30") - 50))

d5 <- d4 %>% mutate(CHURN = if_else(DATE_MAX > (as.Date("2019-04-30") - 50), "F","T"))
d5 %>% group_by(CHURN) %>% count(CHURN)


prova2 <- data %>% group_by(ID_CLI) %>% count(n_distinct(ID_NEG))

data %>% filter(ID_CLI == 344197)

d3$DIFF_2 <- as.Date("2019-04-30") - d3$DATE_MAX

data$IMPORTO_NETTO <- data$IMPORTO_LORDO - data$SCONTO
data$ID_CLI <- as.factor(data$ID_CLI)

scontrini <- data %>% filter(DATE < (as.Date("2019-04-30") - 50))
scontrini_rest <- scontrini  %>% filter(DIREZIONE == -1)
scontrini <- scontrini %>% filter(DIREZIONE == 1)


clienti <- inner_join(scontrini %>% group_by(ID_CLI) %>% summarise(TOTALE_NETTO = sum(IMPORTO_NETTO)),
                      scontrini %>% group_by(ID_CLI) %>% count(ID_CLI), by = "ID_CLI") %>%
  mutate(NUMERO_ACQUISTI_TOT = n) %>% dplyr::select(-n)


temp <- as.data.frame(table((scontrini %>% group_by(ID_CLI, ID_SCONTRINO)%>% count(ID_CLI))$ID_CLI),
                      ID_CLI = as.factor())
temp$ID_CLI <- temp$Var1
temp$NUMERO_SCONTRINI <- temp$Freq
temp$Var1 <- NULL
temp$Freq <- NULL

clienti <- inner_join(clienti, temp, by = "ID_CLI")

rest <- inner_join(scontrini_rest %>% group_by(ID_CLI) %>% count(ID_CLI),
                   scontrini_rest %>% group_by(ID_CLI) %>% summarise(TOTALE_NETTO_REST = sum(IMPORTO_NETTO)),
                   by = "ID_CLI") %>% ungroup() %>% mutate(NUMERO_REST_ART = n) %>% dplyr::select(-n)


master <- left_join(clienti, rest, by = "ID_CLI")
master$NUMERO_REST_ART[is.na(master$NUMERO_REST_ART)] <- 0
master$TOTALE_NETTO_REST[is.na(master$TOTALE_NETTO_REST)] <- 0


p <- scontrini %>% group_by(ID_CLI) %>% count(n_distinct(DATE))
p <- p %>% mutate(NUM_INGRESSI = `n_distinct(DATE)`) %>% dplyr::select(ID_CLI,NUM_INGRESSI)

master <- master %>% left_join(p, by = "ID_CLI")


s1 <- scontrini %>% group_by(ID_CLI) %>% filter(DATE == min(DATE)) %>% dplyr::select(c(ID_CLI,DATE))
s1 <- unique(s1)

s2 <- scontrini %>% group_by(ID_CLI) %>% filter(DATE == max(DATE)) %>% dplyr::select(c(ID_CLI,DATE))
s2 <- unique(s2)

s1$DATE_MIN <- s1$DATE
s1$DATE <- NULL
s2$DATE_MAX <- s2$DATE
s2$DATE <- NULL

master <- master %>% left_join(s1, by = "ID_CLI") %>% left_join(s2, by = "ID_CLI")

master$DIFF <- master$DATE_MAX - master$DATE_MIN
master$FREQ <- master$DIFF / (master$NUM_INGRESSI -1)

d6 <- d5 %>% dplyr::select(ID_CLI,CHURN)
master <- master %>% left_join(d6, by = "ID_CLI")

p1 <- scontrini %>% group_by(ID_CLI) %>% count(n_distinct(ID_NEG))
p1 <- p1 %>% mutate(NUM_NEG = `n_distinct(ID_NEG)`) %>% dplyr::select(ID_CLI,NUM_NEG)
master <- master %>% left_join(p1, by = "ID_CLI") 


master$REST_RATE <- master$NUMERO_REST_ART / master$NUMERO_ACQUISTI_TOT

dataset <- df_1_cli_fid_clean %>% filter(STATUS_FID == 1) %>% arrange(ID_CLI)

dataset$ID_CLI <- as.factor(dataset$ID_CLI)


df_2_cli_account_clean$ID_CLI <- as.factor(df_2_cli_account_clean$ID_CLI)
dataset <- dataset %>% inner_join(df_2_cli_account_clean, by = "ID_CLI")
dataset <- dataset  %>% inner_join(df_3_cli_address_clean, by = "ID_ADDRESS")

definitivo <- master %>% inner_join(dataset,by="ID_CLI")

definitivo$FREQ <- ifelse(is.na(definitivo$FREQ),0,definitivo$FREQ)

definitivo <- definitivo %>% mutate(CHURN=ifelse(CHURN=="T",1,0))

definitivo$FREQ <- if_else(is.nan(definitivo$FREQ),0,definitivo$FREQ)

## Classificazione Task 2 ##
# Teniamo solo le osservazioni che non hanno missing value

churn <- definitivo
churn <- churn[complete.cases(churn),]
churn$DATE_MIN <- as.factor(churn$DATE_MIN)
churn$DATE_MAX <- as.factor(churn$DATE_MAX)
churn$DT_ACTIVE <- as.factor(churn$DT_ACTIVE)
# Abbiamo eliminato le colonne corrispondente agli ID, non utili ai fini della nostra classificazione.

churn <- dplyr::select(churn,-c("ID_CLI","ID_NEG"))

# Facendo un'analisi dell'importanza delle variabili rispetto alla variabile di classe churn, abbiamo
# rilevato che gli attributi maggiormente significtivo per tale scopo sono: Date_min, Date_max, DIFF,
# DT_active, FREQ, rest_rate, numero_scontrini, numero_acquisti_totali.

ig_churn <-information.gain(CHURN~., churn)

# Abbiamo di seguito effettuato le opportune trasformazioni, al fine di utilizzare tali attributi per
# la successiva classificazione.

churn <- churn %>% mutate(DT_ACTIVE=as.numeric(DT_ACTIVE))
churn <- churn %>% mutate(DATE_MIN=as.numeric(DATE_MIN))
churn <- churn %>% mutate(DATE_MAX=as.numeric(DATE_MAX))
churn <- churn %>% mutate(DIFF=as.numeric(DIFF))

# Viene creato un nuovo dataframe "Churn_new" in cui vengono mantenute solo le variabili significative
# precedentemente descritte.

churn_new <- subset(churn,select=-c(TOTALE_NETTO,TOTALE_NETTO_REST,NUMERO_REST_ART,
                                    NUM_NEG,TYP_CLI_FID,COD_FID,STATUS_FID,FIRST_ID_NEG,
                                    TYP_JOB,EMAIL_PROVIDER_CLEAN,CAP,
                                    PRV,REGION,NUM_INGRESSI,W_PHONE,NUM_FIDs,FIRST_DT_ACTIVE,ID_FID,
                                    ID_ADDRESS,TYP_CLI_ACCOUNT))

# La fase di classificazione viene gestita esattamente come nel Task 1, a differenza del fatto che
# la classe maggioritaria, in questo task, è "1", ovvero i clienti churn.

training_indices_churn <- churn_new$CHURN %>% createDataPartition(p=0.75)

training_set_churn <- churn_new[unlist(training_indices_churn), ]
test_set_churn <- churn_new[-unlist(training_indices_churn), ]

ones <- subset(training_set_churn,CHURN==1)
zeros <- subset(training_set_churn,CHURN==0)

sample_ones <- ones[sample(nrow(ones),nrow(zeros)),]

churn_train_final <- rbind(zeros,sample_ones)

churn_train_final <- churn_train_final[sample(nrow(churn_train_final)),]


churn_train_final <- as.data.frame(churn_train_final)
test_set_churn <- as.data.frame(test_set_churn)

dt_churn <- rpart(CHURN ~ DATE_MIN + DATE_MAX + DIFF + DT_ACTIVE + FREQ + REST_RATE+
                    NUMERO_SCONTRINI + NUMERO_ACQUISTI_TOT,
                  data=churn_train_final, method="class")

pred_dt_churn <- predict(dt_churn,test_set_churn,type="class")
pred_proba_dt_churn <- predict(dt_churn, test_set_churn,type="prob")

pos_proba_dt_churn <- pred_proba_dt_churn[,2]
neg_proba_dt_churn <- pred_proba_dt_churn[,1]

MLmetrics::ConfusionMatrix(y_pred=pred_dt_churn,y_true=test_set_churn$CHURN)

Recall(y_pred=pred_dt_churn,y_true=test_set_churn$CHURN,positive="1")
Precision(y_pred=pred_dt_churn,y_true=test_set_churn$CHURN,positive="1")
F1_Score(y_pred=pred_dt_churn,y_true=test_set_churn$CHURN,positive="1")

roc_test_dt_churn <- PRROC::roc.curve(scores.class0 = pos_proba_dt_churn,
                                weights.class0 = as.numeric(test_set_churn$CHURN),
                                curve=TRUE)

plot(roc_test_dt_churn)

pr_test_dt_churn <- PRROC:: pr.curve(scores.class0=pos_proba_dt_churn,
                                     scores.class1=neg_proba_dt_churn,curve=TRUE)

plot(pr_test_dt_churn)

churn_train_final <- as.data.frame(churn_train_final)
test_set_churn <- as.data.frame(test_set_churn)

churn_train_final$CHURN <- as.factor(churn_train_final$CHURN)

nb_churn <- naiveBayes(CHURN ~ DATE_MIN + DATE_MAX + DIFF + DT_ACTIVE + FREQ + REST_RATE+
                         NUMERO_SCONTRINI + NUMERO_ACQUISTI_TOT, 
                       data = churn_train_final)

pred_nb_churn <- predict(nb_churn,test_set_churn)
pred_proba_nb_churn <- predict(nb_churn,test_set_churn, type="raw")
pos_proba_nb_churn <- pred_proba_nb_churn[,2]
neg_proba_nb_churn <- pred_proba_nb_churn[,1]


MLmetrics::ConfusionMatrix(y_pred=pred_nb_churn,y_true=test_set_churn$CHURN)
Recall(y_pred=pred_nb_churn,y_true=test_set_churn$CHURN,positive="1")
Precision(y_pred=pred_nb_churn,y_true=test_set_churn$CHURN,positive="1")
F1_Score(y_pred=pred_nb_churn,y_true=test_set_churn$CHURN,positive="1")

roc_test_nb_churn <- PRROC::roc.curve(scores.class0 = pos_proba_nb_churn,
                                weights.class0 = as.numeric(test_set_churn$CHURN),
                                curve=TRUE)

plot(roc_test_nb_churn)

pr_test_nb_churn <- PRROC:: pr.curve(scores.class0=pos_proba_nb_churn,
                                     scores.class1=neg_proba_nb_churn, curve=TRUE)

plot(pr_test_nb_churn)

churn_train_final %>% pull(CHURN) -> training_labels_churn
test_set_churn %>% pull(CHURN) -> test_labels_churn

training_labels_churn <- as.matrix(training_labels_churn)
test_labels_churn <- as.matrix(test_labels_churn)

churn_train_final_nn <- subset(churn_train_final,select=-c(CHURN))
test_set_churn_nn <- subset(test_set_churn,select=-c(CHURN))

churn_train_final_nn <- as.matrix(churn_train_final_nn)
test_set_churn_nn <- as.matrix(test_set_churn_nn)


model_churn <- keras_model_sequential() 
model_churn %>% 
  layer_dense(units = 256, activation = 'relu',input_shape = ncol(churn_train_final_nn)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 2, activation = 'softmax')


model_churn %>% compile(
  loss = 'sparse_categorical_crossentropy',
  optimizer = 'adam',
  metrics=c('accuracy')
)

history_churn <- model_churn %>% fit(
  churn_train_final_nn, training_labels_churn, 
  epochs = 50, batch_size = 256,
  validation_split=0.2
)

model_churn %>% evaluate(test_set_churn_nn, test_labels_churn)


predictions_test_set_churn_nn <- model_churn %>% predict_classes(test_set_churn_nn)

class_prob_churn_nn <- model_churn %>% predict_proba(test_set_churn_nn)

pos_class_prob_churn_nn <- class_prob_churn_nn[,2]
neg_class_prob_churn_nn <- class_prob_churn_nn[,1]


ConfusionMatrix(y_pred=predictions_test_set_churn_nn,y_true=test_labels_churn)

Recall(y_pred=predictions_test_set_churn_nn,y_true=test_labels_churn,positive="1")
Precision(y_pred=predictions_test_set_churn_nn,y_true=test_labels_churn,positive="1")
F1_Score(y_pred=predictions_test_set_churn_nn,y_true=test_labels_churn,positive="1")

roc_test <- PRROC::roc.curve(scores.class0 = pos_class_prob_churn_nn,
                             weights.class0 = as.numeric(test_labels_churn),curve=TRUE)

plot(roc_test)

pr_test <- PRROC:: pr.curve(scores.class0=pos_class_prob_churn_nn,scores.class1=neg_class_prob_churn_nn,
                            curve=TRUE)

plot(pr_test)

info <- sessionInfo()
out<-capture.output(info)
cat(out,file="sessionInfo.txt",sep="\n",append=TRUE)