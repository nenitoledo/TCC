pacotes <- c("plotly","tidyverse","knitr","kableExtra","fastDummies","rgl","car",
             "reshape2","jtools","lmtest","caret","pROC","ROCR","nnet","magick",
             "cowplot")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}




install.packages("readr")

library(readr)

base <- read_delim(file = "full_data.csv",
                   delim = ",",
                   escape_double = FALSE,
                   trim_ws = TRUE)

summary(base)

base$gender <- as.factor(base$gender)
base$hypertension<- as.factor(base$hypertension)
base$heart_disease<-as.factor(base$heart_disease)
base$ever_married<-as.factor(base$ever_married)
base$work_type<- as.factor(base$work_type)
base$Residence_type<-as.factor(base$Residence_type)
base$smoking_status<-as.factor(base$smoking_status)
base$stroke<-as.factor(base$stroke)


sapply(base, function(x) sum(is.na(x)))

table(base$stroke)

#DUMMY das variáveis categóricas
base_dummies <- dummy_columns(.data = base,
                              select_columns = c("gender",
                                                 "hypertension",
                                                 "heart_disease",
                                                 "ever_married",
                                                 "work_type",
                                                 "Residence_type",
                                                 "smoking_status"),
                              remove_selected_columns = T,
                              remove_first_dummy = T)

m_base_dummies <- glm(formula = stroke ~ . ,
                      data = base_dummies,
                      family = "binomial")

summ(m_base_dummies)
summary(m_base_dummies)

logLik(m_base_dummies) 
logLik

step_base_dummies <- step(object = m_base_dummies,
                          k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

summary(step_base_dummies)

summ(step_base_dummies, confint = T, digits = 3, ci.width = .95)
export_summs(step_base_dummies, scale = F, digits = 6)

logLik(step_base_dummies)

lrtest(m_base_dummies, step_base_dummies)

export_summs(m_base_dummies, step_base_dummies, scale = F,
             digits = 4)

#construção da matriz de confusão
base_dummies$phat <- step_base_dummies$fitted.values


confusionMatrix(
  table(predict(step_base_dummies, type = "response") >= 0.3, 
        base$stroke == "1")[2:1, 2:1]
)

#outras analises dos parametros da matriz
predicoes <- prediction(predictions = step_base_dummies$fitted.values, 
                        labels = base$stroke)

dados_curva_ROC <- performance(predicoes, measure = "sens")

sensitividade <- dados_curva_ROC@y.values[[1]]

especificidade <- performance(predicoes, measure = "spec")
especificidade <- especificidade@y.values[[1]]

cutoffs <- dados_curva_ROC@x.values

dados_plotagem <- cbind.data.frame(cutoffs, especificidade, sensitividade)

dados_plotagem %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)



#curva ROC
ROC <- roc(response = base$stroke, 
           predictor = step_base_dummies$fitted.values)
ROC

ggplotly(
  ggroc(ROC, color = "light blue", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(ROC$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((ROC$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)

############
install.packages("themis")
library("themis")

install.packages("recipe")
library(recipes)
library(modeldata)


base_dummies <- 
  recipe(stroke ~ . , data = base_dummies) %>%
  themis::step_upsample(stroke) %>%
  prep() %>%
  juice()
