#bibliotecas
install.package("corrplot")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("dplyr")

library(corrplot)
library(ggplot2)
library(tidyverse)
library(dplyr)

setwd("C:\\titanic")
df_titanic <- read.csv("titanic.csv")
View(df_titanic)

#excluindo as colunas PassengerID, Name, Ticket e Cabin
df_titanic <- subset(df_titanic, select = c(-PassengerId, -Name, -Ticket, -Cabin))
View(df_titanic)

#Verificando valores nulos
sum(is.na(df_titanic))

#excluindo valores nulos
df_titanic <- df_titanic[complete.cases(df_titanic), ]
View(df_titanic)

#Visualizações dos dados
#Quantas mulheres e homens sobreviveram?
ggplot(data = df_titanic, aes(x=Sex, y=Survived, fill=Sex)) +
  geom_col() +
  labs(x="Sexo", y = "Sobreviventes", title="Quantas mulheres e homes sobreviveram?") +
  scale_fill_manual(values=c("pink","light blue"))

#Proporção de homens e mulheres no dataset
sex_table <- round(prop.table(table(df_titanic$Sex)),2)

#tabela para dataframe
df_sex <- data.frame(Sex = rownames(sex_table), Proportion = as.numeric(sex_table))

# gráfico
ggplot(df_sex, aes(x = Sex, y = Proportion, fill = Sex)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proporção de homens e mulheres no Titanic",
       x = "Sexo", y = "Proporção") + 
  scale_fill_manual(values=c("pink","light blue"))

# Quantas pessoas de cada classe social sobreviveram?
ggplot(df_titanic, aes(x = Pclass, y = Survived, fill = Pclass)) +
  geom_bar(stat = "identity") +
  labs(title = "Quantas pessoas de cada classe social sobreviveram?",
       x = "Classe social", y = "Quantidade")

#Proporção de pessoas em cada classe social  no dataset
class_table <- round(prop.table(table(df_titanic$Pclass)),2)

#tabela para dataframe
df_class <- data.frame(Class = rownames(class_table), Proportion = as.numeric(class_table))

# gráfico
ggplot(df_class, aes(x = Class, y = Proportion, fill = Class)) +
  geom_bar(stat = "identity") +
  labs(title = "Proporção das classes sociais no Titanic",
       x = "Classe social", y = "Porcentagem") +
  scale_fill_manual(values=c("#152940", "#336a98", "#92d9fa"))

#Qual era a idade dos passageiros que estavam no navio?
ggplot(data = df_titanic, aes(x = Age)) +
  geom_histogram(fill = "gray", color = "black") +
  labs(title = "Idade dos passageiros do Titanic", x = "Idade", y = "Quantidade")

#Faixa etária das pessoas que sobreviveram
df_titanic$age_group = cut(df_titanic$Age, breaks = seq(round(min(df_titanic$Age)), 
                                                        round(max(df_titanic$Age)), 5))

df_survived <- subset(df_titanic, Survived == 1)

ggplot(data = df_survived, aes(x = age_group)) +
  geom_bar(fill = "light green", color = "black") +
  labs(title = "Faixa etária das pessoas que sobreviveram", 
       x = "Idade", y = "Quantidade") 

#Quantas mulheres sobreviveram?
df_survived_female <- subset(df_titanic, Survived == 1 & Sex == "female")

ggplot(data = df_survived_female, aes(x = age_group)) +
  geom_bar(fill = "pink", color = "black") +
  labs(title = "Faixa etária das mulheres que sobreviveram", 
       x = "Idade", y = "Quantidade") 

#Quantos homens sobreviveram?
df_survived_male <- subset(df_titanic, Survived == 1 & Sex == "male")

ggplot(data = df_survived_male, aes(x = age_group)) +
  geom_bar(fill = "light blue", color = "black") +
  labs(title = "Faixa etária dos homens que sobreviveram", 
       x = "Idade", y = "Quantidade") 