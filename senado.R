library(dplyr)
library(ggplot2)
senado<- read.csv("senado.csv")

#1
partidos<- senado %>% select(nomeCompleto, partido_sigla) %>% group_by(partido_sigla) %>% count()
maximo<- max(partidos[,2])
minimo<- min(partidos[,2])
partido_max_cand<- partidos %>% filter(n==maximo)
partido_min_cand<- partidos %>% filter(n==minimo)
max_minimo<- rbind(partido_max_cand, partido_min_cand)

#2
senadores<- senado %>% select(descricaoSexo) %>% group_by(descricaoSexo) %>% summarise(sexo= n())

#3
maior_gasto_campanha<- senado %>% select(nomeCompleto, gastoCampanha1T) %>% filter(gastoCampanha1T == max(senado[,18]))

#4
maior_gasto_partido<- senado %>% select(partido_sigla, gastoCampanha1T) %>% group_by(partido_sigla) %>% summarize(total_gasto = sum(gastoCampanha1T))
partido_maior_gasto<- maior_gasto_partido %>% select(partido_sigla, total_gasto) %>% filter(total_gasto == max(total_gasto))

#5
media_estado<- senado %>% select(localCandidatura, gastoCampanha1T) %>% group_by(localCandidatura) %>% summarise(gastos = sum(gastoCampanha1T))

#6
estado_senadores_nasc<- senado %>% select(sgUfNascimento) %>% group_by(sgUfNascimento) %>% count()

#7
canditados_eleitos<- senado %>% select(partido_sigla , descricaoTotalizacao ) %>% group_by(partido_sigla, descricaoTotalizacao) %>% summarise(cand_eleitos = n()) %>% arrange(desc(cand_eleitos))
tres_primeiros<- head(canditados_eleitos, n=3)

#8
por_raca<- senado %>% select(descricaoCorRaca) %>%  group_by(descricaoCorRaca) %>% summarise(senadores = n()) %>% arrange(desc(senadores))
por_profissao<- senado %>% select(ocupacao) %>% group_by(ocupacao) %>% summarise(senadores = n()) %>% arrange(desc(senadores))

#9
por_grau_instucao<- senado %>% select(grauInstrucao) %>% group_by(grauInstrucao) %>% summarise(senadores = n()) %>% arrange(desc(senadores))

#10
por_estado_civil<- senado %>% select(descricaoEstadoCivil) %>% group_by(descricaoEstadoCivil) %>% summarise(senadores = n()) %>% arrange(desc(senadores))





#gráfico 1
barplot(partidos$n, names=partidos$partido_sigla, xlab="Partidos", ylab="Quantidade de Candidatos", col="pink")

#gráfico 2
pie(senadores$sexo, senadores$descricaoSexo, col= c("pink","lightblue"))

#gráfico 3
barplot(maior_gasto_campanha$gastoCampanha1T, xlab="Candidatos", ylab="Gastos", names=maior_gasto_campanha$nomeCompleto, col="pink")

#grafico 4
barplot(maior_gasto_partido$total_gasto, names=maior_gasto_partido$partido_sigla, xlab="Partidos", ylab="Total Gasto", col="pink")

#grafico 5
barplot(media_estado$gastos, names=media_estado$localCandidatura, xlab="Estados", ylab="Média de Gastos", col="pink")

#grafico 6
barplot(estado_senadores_nasc$n, names=estado_senadores_nasc$sgUfNascimento, xlab="Estados", ylab="Quantidade de Candidatos",col="pink")

#grafico 7
pie(tres_primeiros$cand_eleitos, tres_primeiros$partido_sigla, col= c("blue", "darkgreen", "yellow"))

#grafico 8
pie(por_raca$senadores, por_raca$descricaoCorRaca, col= c("white", "brown", "black"))
barplot(por_profissao$senadores, names=por_profissao$ocupacao, xlab="Profissões", ylab="Quantidade", col="pink")

#grafico 9
pie(por_grau_instucao$senadores, por_grau_instucao$grauInstrucao, col= c("lightblue", "pink", "red", "lightgreen"))

#grafico 10
pie(por_estado_civil$senadores, por_estado_civil$descricaoEstadoCivil, col= c("darkred", "black", "grey", "white"))