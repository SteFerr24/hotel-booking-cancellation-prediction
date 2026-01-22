#librerie utilizzate

library(plyr)
library(mice)
library(dplyr)
library(psych)
library(VIM)
library(funModeling)
library(factoextra)
library(factoextra)
library(GGally)
library(rgl)
library(mctest)
library(gvlma)
library(MASS)
library(car)
library(lmtest)
library(sandwich)
library(mgcv)
library(ggeffects)
library(gratia)
library(stringr)
library(ppcor)
library(corrplot)
library(plyr)
library(ggplot2)
library(leaps)
library(robustbase)
library(forestmodel)
library(ggplot2)
library(MASS)
library(caret)
require(corrgram)
library(dplyr)
library(factorMerger)
library(caret)
library(Boruta)
library(pROC)
library(caret)
library(rpart)
library(rpart.plot)
library(gbm)

#FASE INIZIALE -----------------------------------------------------------------
options(scipen=999)
data <- read.csv("hotel_booking.csv", sep="," , dec = ".",stringsAsFactors=TRUE,na.strings=c("NA","N/A", "Unknown ", "Unknown", "?",""))
head(data)
summary(data)
colnames(data) 
str(data)

set.seed(123)

#METRICA SU CUI IMPOSTARE IL LAVORO --------------------------------------------
# Nel problema di previsione delle cancellazioni alberghiere, il modello viene 
# utilizzato come strumento di scoring del rischio di cancellazione. Pertanto, il 
# tuning è stato effettuato ottimizzando la ROC-AUC, in quanto consente di valutare
# la capacità discriminativa del modello su tutte le possibili soglie decisionali,
# senza assumere una specifica politica operativa

#Perché il modello è utilizzato come strumento di scoring del rischio e non 
#come classificatore a soglia fissa

# ------------------------------------------------------------------------------
# Rimozione colonne inutili / leakage
# ------------------------------------------------------------------------------

to_drop <- c(
  "reservation_status",
  "reservation_status_date",
  "name",
  "email",
  "phone.number",
  "credit_card",
  "agent",
  "company",
  "required_car_parking_spaces"
)

data <- data[, setdiff(names(data), to_drop)]

str(data)


# ------------------------------------------------------------------------------
# SISTEMO VARIABILI DATASET 
# ------------------------------------------------------------------------------

# Target: is_canceled -> factor No/Yes
data$is_canceled <- factor(data$is_canceled, levels = c(0, 1), labels = c("No", "Yes"))

# lead_time: binning -> OK!

# date: year/month come factor; week/day come integer
data$arrival_date_year <- factor(data$arrival_date_year)
data$arrival_date_month <- factor(data$arrival_date_month)
data$arrival_date_week_number <- as.integer(data$arrival_date_week_number)
data$arrival_date_day_of_month <- as.integer(data$arrival_date_day_of_month)

# guests: binning
data$adults <- factor(ifelse(data$adults >= 3, 3, data$adults),
                      levels = c(0,1,2,3), labels = c("0","1","2","3+"))

data$children <- factor(ifelse(data$children >= 2, 2, data$children),
                        levels = c(0,1,2), labels = c("0","1","2+"))

data$babies <- factor(ifelse(data$babies >= 1, 1, data$babies),
                      levels = c(0,1), labels = c("0","1+"))

# meal: unifico SC e Undefined
data$meal <- as.character(data$meal)
data$meal[data$meal %in% c("SC", "Undefined")] <- "NoMealPlan"
data$meal <- factor(data$meal)

# market_segment: tolgo Undefined e ricreo factor 
data <- data %>%
  filter(market_segment != "Undefined") %>%
  mutate(market_segment = factor(market_segment))

# repeated guest come factor
data$is_repeated_guest <- factor(data$is_repeated_guest, levels = c(0,1), labels = c("No","Yes"))

# previous cancellations / non-canceled: binning
data$previous_cancellations <- factor(ifelse(data$previous_cancellations >= 2, 2, data$previous_cancellations),
                                      levels = c(0,1,2), labels = c("0","1","2+"))

data$previous_bookings_not_canceled <- factor(ifelse(data$previous_bookings_not_canceled >= 2, 2, data$previous_bookings_not_canceled),
                                              levels = c(0,1,2), labels = c("0","1","2+"))

# reserved_room_type -> room_category
data <- data %>%
  filter(reserved_room_type != "P") %>%
  mutate(room_category = case_when(
    reserved_room_type %in% c("A", "B")           ~ "Economy",
    reserved_room_type %in% c("D", "E", "L")      ~ "Standard",
    reserved_room_type %in% c("C", "F", "G", "H") ~ "Premium",
    TRUE ~ "Other"
  ))

data$room_category <- factor(data$room_category,
                             levels = c("Economy", "Standard", "Premium", "Other"))
data$reserved_room_type <- NULL

# assigned_room_type -> room_assigned
data <- data %>%
  filter(!(assigned_room_type %in% c("P", "L"))) %>%
  mutate(room_assigned = case_when(
    assigned_room_type %in% c("A", "B", "K")      ~ "Economy",
    assigned_room_type %in% c("D", "E", "I")      ~ "Standard",
    assigned_room_type %in% c("C", "F", "G", "H") ~ "Premium",
    TRUE ~ "Other"
  ))

data$room_assigned <- factor(data$room_assigned,
                             levels = c("Economy", "Standard", "Premium", "Other"))
data$assigned_room_type <- NULL

# booking_changes: binning
data$booking_changes <- factor(ifelse(data$booking_changes >= 2, 2, data$booking_changes),
                               levels = c(0,1,2), labels = c("0","1","2+"))

# days_in_waiting_list: binning -> OK!!

# adr: rimuovo valori non plausibili
data <- data %>% filter(adr > 0, adr <= 500)

# total_of_special_requests: binning
data$total_of_special_requests <- factor(ifelse(data$total_of_special_requests >= 3, 3, data$total_of_special_requests),
                                         levels = c(0,1,2,3), labels = c("0","1","2","3+"))

# stays: binning (weekend e week) -> entrambi coerenti
data$stays_in_weekend_nights <- factor(ifelse(data$stays_in_weekend_nights >= 4, 4, data$stays_in_weekend_nights),
                                       levels = c(0,1,2,3,4), labels = c("0","1","2","3","4+"))

data$stays_in_week_nights <- factor(ifelse(data$stays_in_week_nights >= 4, 4, data$stays_in_week_nights),
                                    levels = c(0,1,2,3,4), labels = c("0","1","2","3","4+"))

# country: aggrego in macro-aree
library(countrycode)
data$country_clean <- toupper(trimws(as.character(data$country)))
data$country_clean[data$country_clean == "CN"]  <- "CHN"
data$country_clean[data$country_clean == "TMP"] <- "TLS"

data$continent <- countrycode(
  sourcevar   = data$country_clean,
  origin      = "iso3c",
  destination = "continent",
  warn        = FALSE
)

data$continent[data$country_clean %in% c("ATF", "UMI")] <- "Other"
data$continent[is.na(data$continent)] <- "Other"
data$region <- data$continent
data$region[data$region %in% c("Americas", "Oceania", "Other")] <- "Rest_of_World"

data$region <- factor(
  data$region,
  levels = c("Europe", "Asia", "Africa", "Rest_of_World")
)

# Check finale
data$continent <- NULL
data$country <- NULL
data$country_clean <- NULL
table(data$region)
prop.table(table(data$region))

str(data) # 27 variabili 

# ------------------------------------------------------------------------------
# SPLIT CON SCORE PRESO DAL DATASET COMPLETO + TRAIN/VALIDATION DAL DATASET DIMINUITO
# ------------------------------------------------------------------------------
set.seed(189898)

# 1) Creo SCORE (10%) dal dataset completo
idx_score <- createDataPartition(data$is_canceled, p = 0.10, list = FALSE)
score <- data[idx_score, ]          # 10% SCORE dal dataset grande
data_rest <- data[-idx_score, ]     # 90% rimanente (da cui estraggo data2)

# 2) Diminuire dataset SOLO dal rimanente (così nessuna riga può finire anche nello score)
set.seed(12123)

n_sample <- 10245
idx_sample <- createDataPartition(data_rest$is_canceled, p = n_sample / nrow(data_rest), list = FALSE)
data2 <- data_rest[idx_sample, ]

# Controllo proporzioni target (data_rest vs data2)
prop.table(table(data_rest$is_canceled))
prop.table(table(data2$is_canceled))


# 3) Split TRAIN / VALIDATION dal dataset ridotto (data2)
set.seed(189898)

# Split 70% Train e 30% Validation
idx_train <- createDataPartition(data2$is_canceled, p = 0.70, list = FALSE)
train <- data2[idx_train, ]
test  <- data2[-idx_train, ]   # VALIDATION

#“È stato creato un set di scoring completamente indipendente (10%) estratto dal dataset 
#originale e mai utilizzato durante addestramento e validazione, per simulare uno scenario 
#reale di dati futuri.”

# ------------------------------------------------------------------------------
######################### PRE-PROCESSING #######################################
# ------------------------------------------------------------------------------

# --- CHECK MISSING (TRAIN) ----------------------------------------------------
missingness <- aggr(train, col=c('navyblue','yellow'), numbers=TRUE,
                    sortVars=TRUE, labels=names(train), cex.axis=.7, gap=2)
aggr(train, numbers=TRUE, prop=FALSE, cex.axis=0.7,
     combined=TRUE, col=c('navyblue','yellow'),
     sortVars=TRUE, labels=names(train), gap=3,
     ylab=c("Missing data count", "Pattern"))

test <- test[!is.na(test$region), ] 
train <- train[!is.na(train$region), ] 

missingness <- aggr(test, col=c('navyblue','yellow'), numbers=TRUE,
                    sortVars=TRUE, labels=names(test), cex.axis=.7, gap=2)
aggr(test, numbers=TRUE, prop=FALSE, cex.axis=0.7,
     combined=TRUE, col=c('navyblue','yellow'),
     sortVars=TRUE, labels=names(test), gap=3,
     ylab=c("Missing data count", "Pattern"))

missingness <- aggr(train, col=c('navyblue','yellow'), numbers=TRUE,
                    sortVars=TRUE, labels=names(train), cex.axis=.7, gap=2)
aggr(train, numbers=TRUE, prop=FALSE, cex.axis=0.7,
     combined=TRUE, col=c('navyblue','yellow'),
     sortVars=TRUE, labels=names(train), gap=3,
     ylab=c("Missing data count", "Pattern"))
# OK !!

# --- COLLINEARITA' ------------------------------------------------------------
#VARIABILI NUMERICHE 
# 1) Creo dataset con sole variabili numeriche (numeric/integer) dal train
numdata <- train %>%
  dplyr::select(where(is.numeric) | where(is.integer))

# Controllo variabilità (varianza) delle numeriche rimaste
apply(numdata, 2, function(x) var(x, na.rm = TRUE))
names(numdata)

# 2) Corrgram (solo numeriche)
corrgram(numdata, lower.panel = panel.cor, cex = 1, cex.labels = 1)

# 3) VIF e TOL (serve un modello lineare)
# Target binario 0/1
y <- ifelse(train$is_canceled == "Yes", 1, 0)

# Modello su numeriche (uso complete.cases per evitare NA in lm/mctest)
df_vif <- data.frame(y = y, numdata)
df_vif <- df_vif[complete.cases(df_vif), ]

m <- lm(y ~ ., data = df_vif)

# Test collinearità: VIF e TOL
imcdiag(m, method = "VIF")
imcdiag(m, method = "TOL")

# Report completo (include più diagnostiche)
imcdiag(m)

#VARIABILI NUMERICHE
# vettore logico: quali colonne sono factor?
is_factor <- sapply(train, is.factor)
is_factor

# dataframe con sole variabili factor (escludo il target se non ti serve qui)
factordata <- train[, is_factor, drop = FALSE]
factordata <- factordata[, setdiff(names(factordata), "is_canceled"), drop = FALSE]
str(factordata)

# dataframe con sole variabili numeriche/integer (caret/cor lavora bene con entrambe)
is_num <- sapply(train, function(x) is.numeric(x) || is.integer(x))
numdata <- train[, is_num, drop = FALSE]
str(numdata)

# cor() richiede complete obs (oppure pairwise.complete.obs)
R <- cor(numdata, use = "pairwise.complete.obs")
R
correlatedPredictors <- caret::findCorrelation(R, cutoff = 0.95, names = TRUE)
correlatedPredictors #NESSUNA VARIABILE CORRELATA 

#VARIABILI FACTOR 
rm(factordata) 
sum(is.na(train))

library(dplyr)
factordata <- train %>% dplyr::select_if(is.factor)
colnames(factordata)
str(factordata)

#test chi quadro 
library(plyr)
combos <- combn(ncol(factordata), 2)
adply(combos, 2, function(x) {
  test <- chisq.test(factordata[, x[1]], factordata[, x[2]])
  tab <- table(factordata[, x[1]], factordata[, x[2]])
  out <- data.frame("Row" = colnames(factordata)[x[1]],
                    "Column" = colnames(factordata)[x[2]],
                    "Chi.Square" = round(test$statistic, 3),
                    "df" = test$parameter,
                    "p.value" = round(test$p.value, 3),
                    "n" = sum(tab),
                    "u1" = length(unique(factordata[, x[1]])) - 1,
                    "u2" = length(unique(factordata[, x[2]])) - 1,
                    "nMinu1u2" = sum(tab) * min(length(unique(factordata[, x[1]])) - 1, 
                                                length(unique(factordata[, x[2]])) - 1),
                    "Chi.Square norm" = test$statistic / (sum(tab) * min(length(unique(factordata[, x[1]])) - 1,
                                                                         length(unique(factordata[, x[2]])) - 1))
  )
  return(out)
})
#NESSUNA COLLINEARITA' SULLE VARIABILI FACTOR !!!

# --- NEAR ZERO VARIANCE -------------------------------------------------------
nzv = nearZeroVar(train, saveMetrics = TRUE)
nzv # 6 variabili che sono NZV [children / babies / is_reapited_guest / previous_cancellation / previous_bookings_not_canceled / days_in_waiting_list]
nearZeroVar(train, names = TRUE)

# --- MODEL SELECTION CON TREE -------------------------------------------------
set.seed(1)
cvCtrl <- trainControl(method = "cv", number=10, search="grid", classProbs = TRUE,
                       summaryFunction=twoClassSummary,verboseIter = TRUE)
rpartTune_selection <- train(is_canceled ~ ., data = train, method = "rpart",
                             tuneLength = 10,
                             trControl = cvCtrl,metric="ROC")
rpartTune_selection
getTrainPerf(rpartTune_selection)
print(rpartTune_selection)

Vimportance <- varImp(rpartTune_selection, scale = TRUE)
plot(Vimportance,
     top = 15,
     main = "train tuned - Variable Importance",)
# Estrazione Importanza ordinata
Vimportance_df <- as.data.frame(Vimportance$importance)
Vimportance_df <- Vimportance_df[order(-Vimportance_df$Overall), , drop = FALSE]

# Top 20 per comodità
print(head(Vimportance_df, 20))

variabili_top <- c(
  "is_canceled",                   # target
  
  # Top importance (dalla varImp)
  "deposit_type",
  "lead_time",
  "previous_cancellations",
  "distribution_channel",
  "total_of_special_requests",
  "market_segment",
  "customer_type",
  "booking_changes",
  "room_assigned",
  "adr",
  "stays_in_week_nights"
)

variabili_top <- intersect(variabili_top, colnames(train))

#sistemazione 
variabili_top <- intersect(variabili_top, colnames(train))

train_processed <- train[, variabili_top, drop = FALSE]
test_processed  <- test[,  variabili_top, drop = FALSE]
score_processed <- score[, variabili_top, drop = FALSE]

str(train_processed) #12 variabili !!

# --- MODEL SELECTION CON BORUTA -----------------------------------------------
set.seed(123)
boruta.train <- Boruta(is_canceled ~ ., data = train, doTrace = 1)

# Plot
plot(boruta.train,
     xlab = "Features",
     xaxt = "n",
     ylab = " importance Boruta")

print(boruta.train) 

boruta.metrics <- attStats(boruta.train)
print(head(boruta.metrics, 20))
print(table(boruta.metrics$decision))

# mi dice di togliere solo babies, tengo la model selection fatta con tree 
# anche tree aveva rimosso questa variabile !!

# ------------------------------------------------------------------------------
######################## TUNING DEI MODELLI ####################################
# ------------------------------------------------------------------------------

############################# LASSO ############################################
################################################################################
set.seed(1)
cvCtrl <- trainControl(method = "cv", number=10, search="grid", classProbs = TRUE,
                       summaryFunction=twoClassSummary,verboseIter = TRUE)
tune_lasso <- train(is_canceled ~ .,data = train,method = "glmnet",metric = "ROC",
                    trControl = cvCtrl)

# Output Risultati
tune_lasso
print(tune_lasso$results)
print(tune_lasso$bestTune)
# --- ROC 0.8538114 / SENS 0.9264099 / SPEC 0.6290795


######################### LOGISTIC #############################################
################################################################################
set.seed(123)
tune_logit <- train(is_canceled ~ .,data = train_processed,method = "glm",
                    family = binomial(),preProcess = c("center", "scale", "nzv", "corr"),
                    metric = "ROC",trControl = cvCtrl)

print(tune_logit$results)
# --- ROC 0.829998 / SENS 0.9328702 / SPEC 0.5650932


############################## PLS #############################################
################################################################################
set.seed(123)
tune_pls <- train(is_canceled ~ .,data = train_processed,
                  preProcess = c("center", "scale","nzv"),method = "pls",
                  tuneLength = 12,metric = "ROC",trControl = cvCtrl)

print(tune_pls$results)
print(tune_pls$bestTune)
# --- ROC 0.8282462 / SENS 0.9540586 / SPEC 0.5104131
best_row_pls <- tune_pls$results[
  tune_pls$results$ncomp == tune_pls$bestTune$ncomp, 
]

best_row_pls
plot(tune_pls)

############################## NAIVE BAYES #####################################
################################################################################
set.seed(123)
tune_nb <- train(is_canceled ~ .,data = train_processed,preProcess = c("nzv", "corr"), 
                 method = "naive_bayes", metric = "ROC", trControl = cvCtrl )

print(tune_nb$results)
# --- ROC 0.8061057 / SENS 0.9993309 / SPEC 0.3459690 con useKernel 
best_row_nb <- tune_nb$results[
  tune_nb$results$ncomp == tune_pls$bestTune$ncomp, 
]

best_row_pls

############################## LDA #############################################
################################################################################
set.seed(123)
tune_lda <- train(is_canceled ~ .,data = train_processed,method = "lda",
                  preProcess = c("center", "scale","nzv"),metric = "ROC",trControl = cvCtrl)

print(tune_lda$results)
# --- ROC 0.8281505 / SENS 0.951828 / SPEC 0.5163624  

print(tune_lda)

############################## KNN #############################################
################################################################################
set.seed(123)
tune_knn <- train(is_canceled ~ .,data = train_processed,
                  preProcess = c("center", "scale","nzv"),method = "knn",tuneLength=5,
                  metric = "ROC",trControl = cvCtrl)

print(tune_knn$results)
print(tune_knn$bestTune)
# --- ROC 0.8266345 / SENS 0.8878152 / SPEC 0.6153249 


############################## TREE ############################################
################################################################################
set.seed(123)
tree <- train(is_canceled ~ .,data = train,method = "rpart",tuneLength = 10,
              metric = "ROC",trControl = cvCtrl)

print(tree$results)
print(tree$bestTune)
# --- ROC 0.8253091 / SENS 0.9252834 / SPEC 0.6242551 
install.packages("rpart.plot")   # se non l'hai
library(rpart.plot)

rpart.plot(tree$finalModel, type = 4, extra = 101, split.font = 0.9, ycompress=FALSE, cex=.7)
############################ RANDOM FOREST #####################################
################################################################################
set.seed(123)
rf_grid <- expand.grid(mtry = c(2, 5, 8),splitrule = "extratrees",
                       min.node.size = c(5, 10))
tune_rf <- train(is_canceled ~ .,data = train,method = "ranger",
                 metric = "ROC",trControl = cvCtrl,tuneGrid = rf_grid,importance = "impurity")

print(tune_rf$results)
print(tune_rf$bestTune)
# --- ROC 0.8789908 / SENS 0.9326465 / SPEC 0.6406009 

############################ GRADIENT BOOST ####################################
################################################################################
set.seed(123)
gb <- train(is_canceled ~ .,data = train,method = "gbm",tuneLength = 10,
            metric = "ROC",trControl = cvCtrl,verbose = FALSE)

print(gb$results)
print(gb$bestTune)
# --- ROC 0.8749468 / SENS 0.9130165 / SPEC 0.6558800

############################# NEURAL NETWORK ###################################
################################################################################
set.seed(123)
nnet_fit <- train(is_canceled ~ .,data = train_processed,
                  method = "nnet",preProcess = c("center", "scale","nzv"),
                  metric = "ROC",trControl = cvCtrl,maxit = 300,trace = FALSE)                   

print(nnet_fit$results)
print(nnet_fit$bestTune)
# --- ROC 0.8445064 / SENS 0.9165894 / SPEC 0.6149351

plot(nnet_fit) 

# ------------------------------------------------------------------------------
# ########################### ASSESSMENT #######################################
# ------------------------------------------------------------------------------
#CONFRONTIAMO I RISULTATI
results <- resamples(list(
  lasso = tune_lasso,
  logit = tune_logit,
  pls = tune_pls,
  nb = tune_nb,
  lda = tune_lda,
  knn = tune_knn,
  tree = tree,
  rf = tune_rf,
  gb = gb,
  nnet = nnet_fit
))
# Riassumiamo le distribuzioni delle metriche sulle folds
summary(results)
# boxplots dei risultati
bwplot(results)
###################################### CURVE ROC ###############################
################################################################################
library(pROC)

# 1) Predizioni: prendo la probabilità della classe "Yes"
test_processed$logit <- predict(tune_logit, test_processed, type = "prob")[, "Yes"]
test$tree            <- predict(tree,       test,           type = "prob")[, "Yes"]
test$rf              <- predict(tune_rf,    test,           type = "prob")[, "Yes"]
test_processed$knn   <- predict(tune_knn,   test_processed, type = "prob")[, "Yes"]
test_processed$nb    <- predict(tune_nb,    test_processed, type = "prob")[, "Yes"]
test_processed$nnet  <- predict(nnet_fit,   test_processed, type = "prob")[, "Yes"]
test$gb              <- predict(gb,         test,           type = "prob")[, "Yes"]
test$lasso           <- predict(tune_lasso, test,           type = "prob")[, "Yes"]
test_processed$lda   <- predict(tune_lda,   test_processed, type = "prob")[, "Yes"]
test_processed$pls   <- predict(tune_pls,   test_processed, type = "prob")[, "Yes"]

# 2) ROC (coerenti con is_canceled)
roc_logit <- roc(is_canceled ~ logit, data = test_processed, levels = c("No","Yes"), direction = "<")
roc_tree  <- roc(is_canceled ~ tree,  data = test,           levels = c("No","Yes"), direction = "<")
roc_rf    <- roc(is_canceled ~ rf,    data = test,           levels = c("No","Yes"), direction = "<")
roc_knn   <- roc(is_canceled ~ knn,   data = test_processed, levels = c("No","Yes"), direction = "<")
roc_nb    <- roc(is_canceled ~ nb,    data = test_processed, levels = c("No","Yes"), direction = "<")
roc_nnet  <- roc(is_canceled ~ nnet,  data = test_processed, levels = c("No","Yes"), direction = "<")
roc_gb    <- roc(is_canceled ~ gb,    data = test,           levels = c("No","Yes"), direction = "<")
roc_lasso <- roc(is_canceled ~ lasso, data = test,           levels = c("No","Yes"), direction = "<")
roc_lda   <- roc(is_canceled ~ lda,   data = test_processed, levels = c("No","Yes"), direction = "<")
roc_pls   <- roc(is_canceled ~ pls,   data = test_processed, levels = c("No","Yes"), direction = "<")

# 3) AUC (stampa)
roc_logit$auc
roc_tree$auc
roc_rf$auc
roc_knn$auc
roc_nb$auc
roc_nnet$auc
roc_gb$auc
roc_lasso$auc
roc_lda$auc
roc_pls$auc

# 4) Plot ROC (stile prof: plot() + add=TRUE)
plot(roc_logit)
plot(roc_tree,  add = TRUE, col = "pink")
plot(roc_rf,    add = TRUE, col = "red")
plot(roc_knn,   add = TRUE, col = "blue")
plot(roc_nb,    add = TRUE, col = "yellow")
plot(roc_nnet,  add = TRUE, col = "green")
plot(roc_gb,    add = TRUE, col = "purple")
plot(roc_lasso, add = TRUE, col = "orange")
plot(roc_lda,   add = TRUE, col = "brown")
plot(roc_pls,   add = TRUE, col = "cyan")

# (opzionale) legenda
legend("bottomright",
       legend = c("logit","tree","rf","knn","nb","nnet","gb","lasso","lda","pls"),
       col    = c("black","pink","red","blue","yellow","green","purple","orange","brown","cyan"),
       lwd    = 2, cex = 0.85)
################################ CURVE LIFT  ###################################
################################################################################
# Gradient Boosting
gain_lift(
  data   = test,
  score  = "gb",
  target = "is_canceled"
)

# Random Forest
gain_lift(
  data   = test,
  score  = "rf",
  target = "is_canceled"
)

#VINCE RANDOM FOREST !!!!

# ------------------------------------------------------------------------------
# ######################## TUNING DELLA SOGLIA  ################################
# ------------------------------------------------------------------------------
library(dplyr)

# 1) Probabilità RF sul test set (classe positiva = "Yes")
test$rf <- predict(tune_rf, test, type = "prob")[, "Yes"]

# 2) Dataset per soglia
df_rf <- data.frame(
  Class = factor(test$is_canceled, levels = c("No","Yes")),
  ProbYes = test$rf
)

thresholds <- seq(0, 1, by = 0.001)

kappa_from_counts <- function(tp, tn, fp, fn) {
  n <- tp + tn + fp + fn
  if (n == 0) return(NA_real_)
  po <- (tp + tn) / n
  p_yes_true <- (tp + fn) / n
  p_no_true  <- (tn + fp) / n
  p_yes_pred <- (tp + fp) / n
  p_no_pred  <- (tn + fn) / n
  pe <- p_yes_true * p_yes_pred + p_no_true * p_no_pred
  if (isTRUE(all.equal(1, pe))) return(NA_real_)
  (po - pe) / (1 - pe)
}

prop_table <- data.frame(
  threshold = thresholds,
  sens = NA_real_, spec = NA_real_, acc = NA_real_,
  f1 = NA_real_, kappa = NA_real_,
  bal_acc = NA_real_, youdenJ = NA_real_
)

for (thr in thresholds) {
  
  pred <- ifelse(df_rf$ProbYes > thr, "Yes", "No")
  truth <- df_rf$Class
  
  tp <- sum(pred == "Yes" & truth == "Yes")
  tn <- sum(pred == "No"  & truth == "No")
  fp <- sum(pred == "Yes" & truth == "No")
  fn <- sum(pred == "No"  & truth == "Yes")
  
  sens <- ifelse(tp + fn == 0, NA_real_, tp / (tp + fn))
  spec <- ifelse(tn + fp == 0, NA_real_, tn / (tn + fp))
  acc  <- (tp + tn) / (tp + tn + fp + fn)
  
  prec <- ifelse(tp + fp == 0, NA_real_, tp / (tp + fp))
  f1   <- ifelse(is.na(prec) | is.na(sens) | (prec + sens) == 0, NA_real_,
                 2 * prec * sens / (prec + sens))
  
  kap <- kappa_from_counts(tp, tn, fp, fn)
  bal <- ifelse(is.na(sens) | is.na(spec), NA_real_, (sens + spec) / 2)
  j   <- ifelse(is.na(sens) | is.na(spec), NA_real_, sens + spec - 1)
  
  prop_table[prop_table$threshold == thr, c("sens","spec","acc","f1","kappa","bal_acc","youdenJ")] <-
    c(sens, spec, acc, f1, kap, bal, j)
}

# 3) Plot (come il grafico che mi hai mandato)
plot(prop_table$threshold, prop_table$sens, type="l", lwd=2, col="red",
     ylim=c(0,1), xlab="Threshold", ylab="Metric value",
     main="Random Forest (tune_rf): metriche vs soglia")
lines(prop_table$threshold, prop_table$spec,    lwd=2, col="dodgerblue")
lines(prop_table$threshold, prop_table$acc,     lwd=2, col="gray50")
lines(prop_table$threshold, prop_table$f1,      lwd=2, col="green3")
lines(prop_table$threshold, prop_table$kappa,   lwd=2, col="orange")


legend("bottom", bty="o",
       legend=c("Sensitivity","Specificity","Accuracy","F1 score","Kappa"),
       col=c("red","dodgerblue","gray50","green3","orange"),
       lwd=2)

# 4) Scelta migliore soglia (coerente con ROC): massimizza Balanced Accuracy (≃ Youden)
best_thr_f1 <- prop_table[which.max(prop_table$f1), ]
best_thr_f1

abline(v = best_thr$threshold, lty = 2)

prop_table %>%
  slice_max(f1, n = 1, with_ties = FALSE)

# MATRICE DI CONFUSIONE CON SOGLIA E RANDOM FOREST SUL TEST 
pred_y <- ifelse(test$rf > 0.376, "Yes", "No")
pred_y <- factor(pred_y, levels = c("No","Yes"))

confusionMatrix(
  pred_y,
  factor(test$is_canceled, levels = c("No","Yes")),
  positive = "Yes"
)

######################## SCORE SUI NUOVI DATI ##################################
################################################################################
# dataset di score (può contenere is_canceled, ma non lo usiamo per predire)
score$prob <- predict(tune_rf, score, type = "prob")      # 2 colonne: No/Yes
score$prob_yes <- score$prob[, "Yes"]                     # P(Yes) = cancellato

# soglia scelta
thr <- 0.38

# predizione finale
score$pred_y <- ifelse(score$prob_yes > thr, "cancellato", "non cancellato")
score$pred_y <- factor(score$pred_y, levels = c("non cancellato", "cancellato"))

# controllo distribuzione
table(score$pred_y)
prop.table(table(score$pred_y))

# preview
head(score[, c("prob_yes", "pred_y")])


# CONFRONTO CON SOGLIA NORMALE 
# soglia standard
pred_05 <- ifelse(score$prob_yes > 0.5, "cancellato","non cancellato")

# soglia ottimizzata
pred_038 <- ifelse(score$prob_yes > 0.38, "cancellato","non cancellato")

confusionMatrix(factor(pred_05,  levels=c("non cancellato","cancellato")),
                factor(ifelse(score$is_canceled=="Yes","cancellato","non cancellato"),
                       levels=c("non cancellato","cancellato")),
                positive="cancellato")

confusionMatrix(factor(pred_038, levels=c("non cancellato","cancellato")),
                factor(ifelse(score$is_canceled=="Yes","cancellato","non cancellato"),
                       levels=c("non cancellato","cancellato")),
                positive="cancellato")

library(caret)

# funzione per calcolare F1 a una data soglia
compute_f1 <- function(prob_yes, truth, threshold) {
  
  pred <- ifelse(prob_yes > threshold, "cancellato", "non cancellato")
  pred <- factor(pred, levels = c("non cancellato","cancellato"))
  
  truth <- factor(ifelse(truth == "Yes",
                         "cancellato","non cancellato"),
                  levels = c("non cancellato","cancellato"))
  
  cm <- confusionMatrix(pred, truth, positive = "cancellato")
  
  precision <- cm$byClass["Pos Pred Value"]
  recall    <- cm$byClass["Sensitivity"]
  
  f1 <- 2 * precision * recall / (precision + recall)
  
  data.frame(
    threshold   = threshold,
    precision   = precision,
    sensitivity = recall,
    f1          = f1
  )
}

# applicazione alle due soglie
f1_05  <- compute_f1(score$prob_yes, score$is_canceled, 0.5)
f1_038 <- compute_f1(score$prob_yes, score$is_canceled, 0.38)

# tabella finale di confronto
f1_comparison <- rbind(f1_05, f1_038)
f1_comparison

