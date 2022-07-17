dados = read.csv("C:\\Users\\jpafl\\Downloads\\Real estate.csv")
dados$No = dados$X1.transaction.date  = dados$X5.latitude = dados$X6.longitude = NULL

n = dim(dados)[1] #num de observações
k = dim(dados)[2]-1 #num de variáveis explicativas
colnames(dados) <- c('idade','distância','lojas','preço')
#idade da casa (em anos)
#distância até a estação de metrô mais próxima(em m)
#por unidade de área (em ping, onde 1 ping = 3,3m²)
#número de lojas de conveniência próximas a casa
dados

############ Modelo #################################

ajuste123=lm(dados$'preço' ~ dados$idade+dados$distância+dados$lojas,data=dados)
summary(ajuste123)
beta_chapeu = summary(ajuste123)$coefficient[1:(k+1)]
ep_beta_chapeu = summary(ajuste123)$coefficient[(k+2):(2*(k+1))]
SQreg_123 = sum(summary(aov(ajuste123))[[1]]$'Sum Sq'[1:(k)]) #calcula SQreg do modelo 123


############# Intervalos de confiança 95% ########################
t1 = qt(0.975,n-(k+1))
lim_max = beta_chapeu +t1*ep_beta_chapeu
lim_min = beta_chapeu -t1*ep_beta_chapeu
lim_min
lim_max


############## Tabela ANOVA ################################

summary.aov(ajuste123) # anova do modelo 123
SQres = summary(aov(ajuste123))[[1]]$'Sum Sq'[k+1]
QMres = SQres / ajuste123$df.residual  # Estimador não viesado para variância
s=sqrt(QMres)



############## Soma de Quadrados Extra ##########################

ajuste23=lm(dados$preço ~ dados$distância + dados$lojas,data=dados)
ajuste13=lm(dados$preço ~ dados$idade + dados$lojas,data=dados)
ajuste12=lm(dados$preço ~ dados$idade + dados$distância,data=dados)

aov(ajuste23) # anova do modelo 23
SQreg_23 = sum(summary(aov(ajuste23))[[1]]$'Sum Sq'[1:2]) #calcula SQreg do modelo 23
SQextra_b1 = SQreg_123 - SQreg_23 #calcula SQ Extra de beta1 dado que beta2 e beta3 já estão no MRL

aov(ajuste13) #anova do modelo 13
SQreg_13 = sum(summary(aov(ajuste13))[[1]]$'Sum Sq'[1:2]) #calcula SQreg do modelo 13
SQextra_b2 = SQreg_123 - SQreg_13 #calcula SQ Extra de beta2 dado que beta 1 e beta 3 já estão no modelo

aov(ajuste12) # anova do modelo 12
SQreg_12 = sum(summary(aov(ajuste12))[[1]]$'Sum Sq'[1:2]) #calcula SQReg do modelo 12
SQextra_b3 = SQreg_123 - SQreg_12 # calcula SQ Extra de beta3 dado que beta1 e beta2 já estão no modelo

#calcula estatísticas dos testes F parciais de cada coeficiente 
F_b1 = SQextra_b1/QMres
F_b2 = SQextra_b2/QMres
F_b3 = SQextra_b3/QMres
qf(.95,1,n-(k+1)) # limiar da região crítica dos testes F parciais

# calcula os p-valores dos testes F parciais
1-pf(F_b1,1,n-(k+1))
1-pf(F_b2,1,n-(k+1))
1-pf(F_b3,1,n-(k+1))


############## R² ##########################################

R_2 = summary(ajuste123)$r.squared


############## Multicolinearidade ##########################

round(cor(dados[,1:k]),digits=4)
library(car)
vif(ajuste123)


############ Resíduos padronizados ######################

e_i = ajuste123$residuals
d_i = e_i/s

table(abs(d_i) > 2)['TRUE']
18/n


########### Resíduo Studentizado ######################

r_i = rstandard(ajuste123)
for (i in 1:n) (if (abs(r_i[i]) > 3) {print (i)}) # quais amostras ficaram fora do intervalo (-3,3)


############ Resíduo PRESS #######################

h_ii = hatvalues(ajuste123)
press_i = e_i/(1-h_ii)
for (i in 1:n) (if (abs(press_i[i]) > 28) {print (i)})


############ Pontos de alavancagem ######################3

h_med = (k+1)/n
for (i in 1:n) (if (h_ii[i]>0.03) {print (i)}) #quais amostras apresentam pontos muito superiores a h_med


############ Resíduo studentizado externamente ###################################
t_i = rstudent(ajuste123)
S2_i = ((n-k-1)*QMres - e_i**2/(1-h_ii))/(n-k-2)

for (i in 1:n) (if (abs(QMres - S2_i[i]) > 1.7) {print (i)})


############ Distancia de Cook  ##################
Di =  cooks.distance(ajuste123)
for (i in 1:n) (if (Di[i]>0.2) {print (i)})
Di[271]


############ Predição ####################

X=matrix(1,n,k+1) 
X[,2]=dados$idade
X[,3]=dados$distância
X[,4]=dados$lojas
y = dados$preço

# calcula matriz X'X
XTX=t(X)%*%X

# calcula o determinante da matriz X'X
det(XTX)

# calcula a inversa de X'X
iXTX=solve(XTX)

xh = matrix(data=c(1.000000 , 17.712560, 1083.885689, 4.094203),k+1,1)
yh_chap = t(xh)%*%beta_chapeu
ep_yh_chap = s*sqrt(1 + t(xh)%*%iXTX%*%xh)

#Intervalo de confiança 95% para a observação futura:
obs_fut_min = yh_chap - t1*ep_yh_chap
obs_fut_max = yh_chap + t1*ep_yh_chap




