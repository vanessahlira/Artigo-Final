################### Script for replication of the article ###########################

## Vanessa Horacio Lira ##

############################# Análise Exploratória dos Dados ####################################

## Carregando o banco
setwd("C:/Users/Dell/OneDrive/Academico/Doutorado/2. Analise de Dados/Trabalho Final/Twitter/Dados")
load("vanessa-lira-ad-ufpe-2019.RData")

## Carregando pacotes
library(tidyverse)
library(magrittr)
install.packages("ggridges")
library(ggridges)
install.packages("HH")
library(HH)
install.packages("effects")
library(effects)
install.packages("TITAN2")
library(TITAN2)
install.packages("jtools")
library(jtools)

## Visualizando o banco
View(twitter)
dim(twitter)
str(twitter)
# Temos 9986 observações e 18 variáveis

## Conhecendo as variáveis
names(twitter)

## Visualizando a distribuição das variáveis

## Variavéis numéricas: n_posts, n_followers,n_likes, n_followed.

## Vamos analisar cada uma delas levando em consideração: média, desvio-padrão

summary(twitter$n_posts)
sd(twitter$n_posts)

summary(twitter$n_followers)
sd(twitter$n_followers)

summary(twitter$n_followed)
sd(twitter$n_followed)

summary(twitter$n_likes)
sd(twitter$n_likes)

## Vemos que essas variáveis apresentam desvio-padrão muito alto.
## Por essa razão precisamos fazer transformação logarítima.

# log-log model

# transformacao log
lnPosts <- log(twitter$n_posts)

# transformacao log
lnFollowers <- log(twitter$n_followers)

# transformacao log
lnLikes <- log(twitter$n_likes)

# transformacao log
lnFollowed <- log(twitter$n_followed)


## Variaveis numericas

# Postagens

summary(twitter$n_posts)
mean(twitter$n_posts)
var(twitter$n_posts)
sd(twitter$n_posts)

ggplot(data = twitter) +
  geom_histogram(mapping = aes(x=lnPosts, fill = evento), binwidth = 0.5)+
  scale_x_continuous(name = "Number of Posts") +
  scale_y_continuous(name = "Frequency")+
  ggtitle("Frequency of Posts by Issue")+
  theme_bw()

# Curtidas

summary(twitter$n_likes)
mean(twitter$n_likes)
var(twitter$n_likes)
sd(twitter$n_likes)

ggplot(data = twitter) +
  geom_histogram(mapping = aes(x=lnLikes, fill = evento), binwidth = 0.5)+
  scale_x_continuous(name = "Number of likes") +
  scale_y_continuous(name = "Frequência")+
  ggtitle("Frequecy of Likes by Issue")+
  theme_bw()

# Seguidores

summary(twitter$n_followers)
mean(twitter$n_followers)
var(twitter$n_followers)
sd(twitter$n_followers)

ggplot(data = twitter) +
  geom_histogram(mapping = aes(x=lnFollowers, fill = gender), binwidth = 0.5)+
  scale_x_continuous(name = "Nº of followers") +
  scale_y_continuous(name = "Frequency")+
  ggtitle("Users' Followers by Gender")+
  theme_bw()

# Seguidos

summary(twitter$n_followed)
mean(twitter$n_followed)
var(twitter$n_followed)
sd(twitter$n_followed)

mean(lnFollowed)

ggplot(data = twitter) +
  geom_histogram(mapping = aes(x=lnFollowed, fill = gender), binwidth = 0.5)+
  scale_x_continuous(name = "Nº de perfis seguidos") +
  scale_y_continuous(name = "Frequência")+
  ggtitle("Frequência dos Perfis Seguidos pelos Usuários por Gênero")+
  theme_bw()

## Variaveis categoricas

# Primeiro, vamos nos certificar de que as variaveis categoricas sao mesmo categoricas
class(twitter$religion)
class(twitter$ideology)
class(twitter$gender)
class(twitter$evento)

# Segundo, vamos transformar em categoricas
twitter$religion <- as.character(twitter$religion)
twitter$ideology <- as.character(twitter$ideology)
twitter$evento <- as.character(twitter$evento)

# ideology
tab <- table(twitter$ideology)
prop.table(tab)

# Vemos que apenas 2.8% dos tweets sao de usuarios que apresentam ideology em suas descricoes.

ggplot(data = twitter)+
  geom_bar(mapping = aes(x = ideology, fill = evento))

ggplot(data = twitter)+
  geom_bar(mapping = aes(x = ideology, fill = gender))

ggplot(data = twitter)+
  geom_bar(mapping = aes(x = ideology, fill = religion))

# religion
tab_ <- table(twitter$religion)
prop.table(tab_)

# Vemos que apenas 0.8% dos tweets sao de usuarios que apresentam religion em suas descricoes.

ggplot(data = twitter)+
  geom_bar(mapping = aes(x = religion, fill = evento))

ggplot(data = twitter)+
  geom_bar(mapping = aes(x = religion, fill = gender))

ggplot(data = twitter)+
  geom_bar(mapping = aes(x = religion, fill = ideology))

# Vemos que 1) os poucos perfis com elementos religiosos publicaram no evento da
# reforma da previdencia; 2) nao foi possivel identificar os genders dos perfis com
# elementos religiosos.

# gender
tab__ <- table(twitter$gender)
prop.table(tab__)

# Vemos que na maioria dos tuites nao foi possivel identificar o gender. No entanto,
# entre os identificados vemos que o numero de homens (2) foi mais que o dobro (36%) do
# que o numero de mulheres (16%) que publicaram. 

ggplot(data = twitter)+
  geom_bar(mapping = aes(x = gender, fill = evento))

ggplot(data = twitter)+
  geom_bar(mapping = aes(x = gender, fill = religion))

ggplot(data = twitter)+
  geom_bar(mapping = aes(x = gender, fill = ideology))

# Vemos que: 1) a proporcao entre homens e mulheres nos dois eventos e semelhante nos dois 
# eventos, sendo o numero de homens macicamente superior em ambos; 2) e praticamente
# imperceptivel o numero de homens e mulheres com perfis religiosos; 3) quanto a ideology
# nos perfis, vemos que os homnes apresentam-se tambem em maior numero. Ou seja, nesses dados
# homens apresentam mais perfil ideologico. 

## Uso de palavras morais-emocionais

twitter %>% 
  ggplot() +
  geom_bar(aes(x = moral_emotional), color = "black", fill = "light blue") +
  theme_minimal() +
  ggtitle("Use of moral-emotional words")


## Relacao entre as variaveis

# Variaveis categoricas
xtabs(~gender + evento, data = twitter)

xtabs(~ideology + religion, data = twitter)

# Variaveis numericas

## Curtidas - Postagens
ggplot(data = twitter,
       mapping = aes(x = lnLikes,
                     y = lnPosts,
                     color = ideology)) +
  geom_point()+
  theme_minimal() +
  ggtitle("Relation Between Likes and Posts")


## Por evento, gender, ideology

ggplot(data = twitter,
       mapping = aes(x = lnLikes,
                     y = lnPosts)) +
  geom_point() +
  facet_wrap(facets = ~evento)+
  theme_minimal() +
  ggtitle("Relation Between Likes and Posts by Issue")


ggplot(data = twitter,
       mapping = aes(x = lnLikes,
                     y = lnPosts)) +
  geom_point() +
  facet_wrap(facets = ~gender)+
  theme_minimal() +
  ggtitle("Relation Between Likes and Posts by Gender")

ggplot(data = twitter,
       mapping = aes(x = lnLikes,
                     y = lnPosts)) +
  geom_point() +
  facet_wrap(facets = ~ideology)+
  theme_minimal() +
  ggtitle("Relation Between Likes and Posts by Ideology")

# Seguidos - Seguidores

ggplot(data = twitter,
       mapping = aes(x = lnFollowed,
                     y = lnFollowers,
                     color = gender)) +
  geom_point()+
  theme_minimal() +
  ggtitle("Relation Between Followed and Followed by Gender")


## Curtidas ao longo dos anos

twitter %>%
  mutate(ano = as.factor(tempo)) %>% 
  ggplot(aes(y = ano, x = lnPosts, fill = ano)) +
  geom_density_ridges(na.rm = TRUE, show.legend = FALSE)+
  theme_minimal() +
  ggtitle("Users' Posts Through the Years")


## Outras variaveis

## Location
twitter %>%
  count(location_mod, sort = TRUE) %>%
  mutate(location_mod = reorder(location_mod,n)) %>%
  na.omit() %>%
  top_n(10) %>%
  ggplot(aes(x = location_mod,y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Location",
       y = "Count",
       title = "Users' Location on Twitter")+
  theme_minimal()

## Hashtags
twitter %>%
  count(hashtags, sort = TRUE) %>%
  mutate(hashtags= reorder(hashtags,n)) %>%
  na.omit() %>%
  top_n(10) %>%
  ggplot(aes(x = hashtags,y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Hashtags",
       y = "Count",
       title = "Most Used Hashtags") +
  theme_bw()+
  theme_minimal()

## Tempo de Twitter
summary(twitter$tempo)

twitter %>% 
  ggplot(aes(x = tempo)) +
  geom_histogram(color = "black", fill = "dark blue", binwidth = 0.5)+
  ggtitle("How long users have their Twitter profiles")+
  theme_minimal()

# Vemos que os perfis que mais publicaram foram os mais antigos, ou seja, aqueles individuos
# que criaram sua conta na plataforma em 2009. Isso pode sugerir que individuos recem-chegados
# no Twitter nao sao os que participam mais sobre politica. 


######################## Modelos de Regressão Logística ###########################

## Primeiro, converter a variavel categorica em fator.

twitter$gender <- factor(twitter$gender)
twitter$religion <- factor(twitter$religion)
twitter$ideology <- factor(twitter$ideology)
twitter$evento <- factor(twitter$evento)

## Checando o vies da VD 

# Idealmente, a proporcao de issues e nao-issues em Y deve ser aproximadamente o mesmo.

table(twitter$moral_emotional)
# 0    1 
# 7430 2556 

# Claramente existe um vies na variavel "moral_emotional", uma condicao observada quando a proporcao
# de issues (1) e muito menor do que a proporcao de nao-issues (0). Por essa razao, devemos
# tirar uma amostra das observacao em aproximadamente proporcoes iguais para ter melhores modelos.

# Create Training Data
input_ones <- twitter[which(twitter$moral_emotional == 1), ]  # all 1's
input_zeros <- twitter[which(twitter$moral_emotional == 0), ]  # all 0's
set.seed(100)  # for repeatability of samples

input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_ones))  # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_ones))  # 0's for training. Pick as many 0's as 1's
training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's

# Create Test Data
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's 

## Build Logit Models and Predict
model_1 <- glm(moral_emotional ~ n_followers + n_followed + n_posts + 
                 n_likes, data = trainingData, family = "binomial")

model_2 <- glm(moral_emotional ~ n_followers + n_followed + n_posts + 
                 n_likes + evento + tempo, data = trainingData, family = "binomial")

model_3 <- glm(moral_emotional ~ n_followers + n_followed + n_posts + 
                 n_likes + ideology, data = trainingData, family = "binomial")

model_4 <- glm(moral_emotional ~ n_followers + n_followed + n_posts + 
                 n_likes + religion, data = trainingData, family = "binomial")

model_5 <- glm(moral_emotional ~ n_followers + n_followed + n_posts + 
                 n_likes + gender, data = trainingData, family = "binomial")

model_6 <- glm(moral_emotional ~ n_followers + n_followed + n_posts + 
                 n_likes + ideology + religion + gender + evento + tempo,
               data = trainingData, family = "binomial")

model_7 <- glm(moral_emotional ~  n_posts + evento + gender + n_posts*evento +
                 n_posts*gender + evento*gender, data = trainingData, family = "binomial")

## Apresentando
summ(model_1)
## Pseudo-R² = 0.0
summ(model_2)
## Pseudo-R² =0.02
summ(model_3)
## Pseudo-R² =0.00
summ(model_4)
## Pseudo-R² =0.00
summ(model_5)
## Pseudo-R² =0.01
summ(model_6)
## Pseudo-R² =0.02
summ(model_7)
## Pseudo-R² =0.02

## Predicting

predicted_1 <- plogis(predict(model_1, testData))  # predicted scores
predicted_2 <- plogis(predict(model_2, testData))
predicted_3 <- plogis(predict(model_3, testData))
predicted_4 <- plogis(predict(model_4, testData))
predicted_5 <- plogis(predict(model_5, testData))
predicted_6 <- plogis(predict(model_6, testData))
predicted_7 <- plogis(predict(model_7, testData))

## Obtendo os intervalos de confianca para os coeficientes estimados
confint(model_1)
confint(model_2)
confint(model_3)
confint(model_4)
confint(model_5)
confint(model_6)
confint(model_7)

## Teste de Significancia dos coeficientes

## Calculando a signicancia estatistica para a variavel categorica genero (1 e 2)
wald.test(b = coef(model_2), Sigma = vcov(model_2), Terms = 6:6)
# 0.000001

summary(model_2)

# A estatistica Wald prove a signficancia estatistica para cada coeficiente estimado
# para realizacao de testes de hiposes. Se o coeficiente logistico e estatisticamente
# significante, podemos interpreta-lo em termos de como o mesmo impacta a probabilidade 
# estimada e consequentemente a previsao de pertinencia a um grupo (HAIR et. al, 2009).
# Para genero, vemos que o parametro e diferente de 0, por essa razao, devemos inclui-lo
# no modelo. 

vif(model_1)
vif(model_2)
vif(model_3)
vif(model_4)
vif(model_5)
vif(model_6)
vif(model_7)


plotROC(testData$moral_emotional, predicted_1)
plotROC(testData$moral_emotional, predicted_2)
plotROC(testData$moral_emotional, predicted_3)
plotROC(testData$moral_emotional, predicted_4)
plotROC(testData$moral_emotional, predicted_5)
plotROC(testData$moral_emotional, predicted_6)
plotROC(testData$moral_emotional, predicted_7)


Concordance(testData$moral_emotional, predicted_1)
Concordance(testData$moral_emotional, predicted_2)
Concordance(testData$moral_emotional, predicted_3)
Concordance(testData$moral_emotional, predicted_4)
Concordance(testData$moral_emotional, predicted_5)
Concordance(testData$moral_emotional, predicted_6)
Concordance(testData$moral_emotional, predicted_7)


## Extraindo coeficientes e erros-padrao
logit.coefs.1 <- coef(model_1)
logit.coefs.2 <- coef(model_2)
logit.coefs.3 <- coef(model_3)
logit.coefs.4 <- coef(model_4)
logit.coefs.5 <- coef(model_5)
logit.coefs.6 <- coef(model_6)
logit.coefs.7 <- coef(model_7)

logit.ses.1 <- sqrt(diag(vcov(model_1)))
logit.ses.2 <- sqrt(diag(vcov(model_2)))
logit.ses.3 <- sqrt(diag(vcov(model_3)))
logit.ses.4 <- sqrt(diag(vcov(model_4)))
logit.ses.5 <- sqrt(diag(vcov(model_5)))
logit.ses.6 <- sqrt(diag(vcov(model_6)))
logit.ses.7 <- sqrt(diag(vcov(model_7)))

logit.coefs.1
logit.coefs.2
logit.coefs.3
logit.coefs.4
logit.coefs.5
logit.coefs.6
logit.coefs.7

logit.ses.1
logit.ses.2
logit.ses.3
logit.ses.4
logit.ses.5
logit.ses.6
logit.ses.7

## But how do we interpret these coefficients?
## Interpretacao dos coeficientes

# Exponenciar os coenficente e interpretarcomo razoes de chance
# odds ratios only
exp(coef(model_1))
exp(coef(model_2))
exp(coef(model_3))
exp(coef(model_4))
exp(coef(model_5))
exp(coef(model_6))
exp(coef(model_7))

# odds ratios and 95% CI
exp(cbind(OR = coef(model_1), confint(model_1)))
exp(cbind(OR = coef(model_2), confint(model_2)))
exp(cbind(OR = coef(model_3), confint(model_3)))
exp(cbind(OR = coef(model_4), confint(model_4)))
exp(cbind(OR = coef(model_5), confint(model_5)))
exp(cbind(OR = coef(model_6), confint(model_6)))
exp(cbind(OR = coef(model_7), confint(model_7)))

# Para cada uma unidade de aumento no numero de postagens, seguidores, perfis seguidos
# e curtidas, as chances de chance de usar palavaras morais-emocionais (versus nao utilizar)
# aumenta por um fator de 1,00.
## PREDICTED PROBABILITIES
# Atraves do pacote effects vamos calcular as probabilidades preditas para cada valor
# das nossas variaveis independentes

all.effects.1 <- allEffects(mod = model_1)

# ERRO: genero nao e factor nem numerica. Logo, vamos transforma-la:
trainingData$gender <- as.factor(trainingData$gender)

all.effects.1 <- allEffects(mod = model_1)
all.effects.2 <- allEffects(mod = model_2)
all.effects.3 <- allEffects(mod = model_3)
all.effects.4 <- allEffects(mod = model_4)
all.effects.5 <- allEffects(mod = model_5)
all.effects.6 <- allEffects(mod = model_6)
all.effects.7 <- allEffects(mod = model_7)

summary(all.effects.1)
summary(all.effects.2)
summary(all.effects.3)
summary(all.effects.4)
summary(all.effects.5)
summary(all.effects.6)
summary(all.effects.7)

plot(all.effects.1, type = "response", ylim = c(0, 1), main = "Predicted
Probabilities Model 1")

plot(all.effects.2, type = "response", ylim = c(0, 1), main = "Predicted
Probabilities Model 2")

plot(all.effects.3, type = "response", ylim = c(0, 1), main = "Predicted
Probabilities Model 3")

plot(all.effects.4, type = "response", ylim = c(0, 1), main = "Predicted
Probabilities Model 4")

plot(all.effects.5, type = "response", ylim = c(0, 1), main = "Predicted
Probabilities Model 5")

plot(all.effects.6, type = "response", ylim = c(0, 1), main = "Predicted
Probabilities Model 6")

plot(all.effects.7, type = "response", ylim = c(0, 1), main = "Predicted
Probabilities Model 7")

## We may also wish to see measures of how well our model fits. 
## This can be particularly useful when comparing competing models. 
## The output produced by summary(mylogit) included indices of fit (shown below 
## the coefficients), including the null and deviance residuals and the AIC. 
## One measure of model fit is the significance of the overall model. This test 
## asks whether the model with predictors fits significantly better than a model with 
## just an intercept (i.e., a null model). The test statistic is the difference between
## the residual deviance for the model with predictors and the null model. The test 
## statistic is distributed chi-squared with degrees of freedom equal to the differences
## in degrees of freedom between the current and the null model (i.e., the number of predictor 
##variables in the model). To find the difference in deviance for the two models 
##(i.e., the test statistic) we can use the command:

with(model_1, null.deviance - deviance)
with(model_2, null.deviance - deviance)
with(model_3, null.deviance - deviance)
with(model_4, null.deviance - deviance)
with(model_5, null.deviance - deviance)
with(model_6, null.deviance - deviance)
with(model_7, null.deviance - deviance)

## The degrees of freedom for the difference between the two models is equal to the 
## number of predictor variables in the mode, and can be obtained using:

with(model_1, df.null - df.residual)
with(model_2, df.null - df.residual)
with(model_3, df.null - df.residual)
with(model_4, df.null - df.residual)
with(model_5, df.null - df.residual)
with(model_6, df.null - df.residual)
with(model_7, df.null - df.residual)

## Finally, the p-value can be obtained using:
with(model_1, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
with(model_2, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
with(model_3, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
with(model_4, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
with(model_5, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
with(model_6, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
with(model_7, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))


## The chi-square of 41.46 with 5 degrees of freedom and an associated p-value of 
## less than 0.001 tells us that our model as a whole fits significantly better than
## an empty model. This is sometimes called a likelihood ratio test (the deviance
## residual is -2*log likelihood). To see the model's log likelihood, we type:

logLik(model_1)
logLik(model_2)
logLik(model_3)
logLik(model_4)
logLik(model_5)
logLik(model_6)
logLik(model_7)

## Comparing model coefficients visually

## Let's see how the uncertainty and magnitude of effect differs for these variables.

## Hyphotesis 1: Comparing models 1 and 2 
plot_summs(model_1, model_2, scale = TRUE)
plot_summs(model_1, model_2, scale = TRUE, plot.distributions = TRUE)

## The graph represents the estimated coefficients in the models for each variable over
## a 95% confidence interval. Although in the logistic regression we have named the models
## form 1 to 7, in graphics we will see in the legend the models named as: Model 1, Model 2,
## and Model 3. Thus, it is important to note which models the graphics are actually referring to.
## Only the coefficients of the variable "number of posts" 
## do not touch 0 for both models. That is, even if it has a small effect, it is possible
## to say that the number of posts has a negative association with the likelihood of using 
## moral-emotional words.
## Similarly, for model 2 the issue variable does not touch 0. In this case, having a 
## higher level of association comparatively and positively with respect to the variable
## number of posts.
## As our hypothesis 1 seeks to measure the association between the four variables that
## compose user activity on Twitter with the use of words, it is possible to say from the
## analysis of the models that it is more likely that there is no association.
## between users' activities and the use of moral-emotional words.
## Importantly, the use of controls improves the fit of the model, but with very little 
## impact.
## We see that the addition of variables makes the number of posts a bit far from 0.
## But practically, the addition of variables does not make changes in the coefficients estimates.

## Hyphotesis 2: Comparing models 3, 4, 5, 
plot_summs(model_3, model_4, model_5, scale = TRUE)

## When comparing the models concerning the hyphothesis 2, we observe that the variable number
## of posts is the only one that does not touchs zero, although it is negative. This means that
## this variable might have a negative association with the dependent variable (use of moral-emotional words),
## but it has a small effect, given its closeness to zero.
## As we can see in the graphic the variables ideology and religion present coefficients 
## with higher confidence intervals.
## Another relevant variable is gender2, presented in model 5. It does not touch zero and its coefficient is positive.
## This means the if the user is a man it is more likely that he uses moral-emotional 
## words on political tweets.

## Hypothesis 3: Comparing models 2, 6, and 7
plot_summs(model_2, model_6, model_7, scale = TRUE)

## We observe that the coefficient of variable number of posts does not touch 0, except
## in model 7. The variable issue is positive in all models, and does not touch 0, 
## which means that is relevant to predict that the tweets published on the issue, probably
## pension reform, are more likely to use moral-emotional words. Also, in model 6, being a man
## is positively associated with the use of moral-emotional words on political tweets.
## And being a woman and have higher number of posts is negativly associated with the 
#3 use of words. 

# Hyphotesis 4: Comparing models 2 and 6
plot_summs(model_2, model_6, scale = TRUE)

## The hyphotesis 4 expects that newer Twitter accounts use more moral-emotional words.
## As we can see from the graphic, the confidence interval from the time of Twitter coefficients
## touches zero. Therefore, it is very unlikely that exists association with the dependent variable.
## In other words, we have no evidence that users with more time of Twitter are more or less
## likely to use moral-emotional words on political tweets. 

# Hyphotesis 5: Model 7
plot_summs(model_7, scale = TRUE)

## Is the use of moral-emotional words associated with the interactive relation 
## between the variables gender, number of posts, and issue? These variables are the ones 
## that did not touched zero in the other models. Nevertheless they do not have explanatory power,
## because only two of them do not touch zero: issue, with a positive effect, and number of posts from women,
## which has a negative effect. This means that the higher the number of posts from female users,
## the lowest is the probability of using moral-emotional words. However, in both
## the effects are negative limiting the explanatory capacity of the model.

## In short, the analysis of the models shows us mixed results. While some variables, 
## such as number of posts, gender, and issue have some effect, others have several 
## problems and limitations, as shown above in the confidence intervals of the estimated 
## coefficients. With this, we note that it is, although we cannot immediately reject 
## the null hypothesis, but it is more likely to be true. Evidence indicates that the 
## association between the selected independent variables is weak or does not exist. 
## Therefore, they may not be the best predictors of whether or not an individual commenting 
## on politics on Twitter will use moral-emotional words. It may also be necessary to 
## repeat the models based on other random samples.

########################