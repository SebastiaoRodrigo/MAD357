##1
idade <- c(15.5, 23.75, 8.00,17.00, 5.00,19.00,24.00,2.50,7.50,11.00,13.00,3.75,25.00,9.75,22.00,18.00,6.00,12.50,2.00,21.50)
tensao <- c(2158.70,1678.15,2316.00,2061.30,2207.50,1708.30,1784.70,2575.00,2357.90,2277.70,2165.20,2399.55,1779.80,2336.75,1765.30,2053.50,2414.40,2200.50,2654.20,1753.70)

ajuste_MRLS=lm(tensao~idade) # regressão linear
print(ajuste_MRLS) # valores de beta0 e beta1
print(summary(ajuste_MRLS))

##2
anova(ajuste_MRLS) #tabela anova

##3
n = 20
nivel_confianca = 0.95
alfa = 1 - nivel_confianca
est_beta0 = ajuste_MRLS$coefficients[1] #estimador para beta0 obtido
est_beta1 = ajuste_MRLS$coefficients[2] #estimador para beta1 obtido
x_bar = mean(idade) #média dos xi's 
sxx = sum((idade-x_bar)^2) #soma dos quadrados das diferenças entre cada xi e xbar
t1 = qt(1 - alfa/2,n-2) #quantil de (1 - alfa/2) da t-Student com n-2 graus de liberdade 
resid = residuals.lm(ajuste_MRLS) #resíduos do modelo 
est_var = sum(resid^2)/(n-2) #estimador para a variância obtido 

ep_beta0 = sqrt(est_var*(1/n + x_bar^2/sxx)) #erro padrão para o estimador de beta0
IC_min_beta0 =  est_beta0 - t1*ep_beta0 #limite inferior para beta0
IC_max_beta0 =  est_beta0 + t1*ep_beta0 #limite superior para beta0

ep_beta1 = sqrt(est_var/sxx) #erro padrão para o estimador de beta1
IC_min_beta1 =  est_beta1 - t1*ep_beta1 #limite inferior para beta1
IC_max_beta1 =  est_beta1 + t1*ep_beta1 #limite superior para beta1

chisq_max = qchisq(1-alfa/2, n-2) #quantil de (1 - alfa/2) da qui-quadrada com n-2 graus de liberdade
chisq_min = qchisq(alfa/2, n-2) #quantil de (1 - alfa/2) da qui-quadrada com n-2 graus de liberdade
IC_min_var = (n-2)*est_var/chisq_max #limite inferior para variância
IC_max_var = (n-2)*est_var/chisq_min #limite superior para variância



##4

#Intervalos de confiança conjuntos usando a abordagem de Bonferroni:
t2 = qt(1 - alfa/4,n-2) #quantil de (1 - alfa/4) da t-Student com n-2 graus de liberdade

IC_conj_min_beta0 =  est_beta0 - t2*ep_beta0 #limite inferior para beta0
IC_conj_max_beta0 =  est_beta0 + t2*ep_beta0 #limite superior para beta0

IC_conj_min_beta1 =  est_beta1 - t2*ep_beta1 #limite inferior para beta1
IC_conj_max_beta1 =  est_beta1 + t2*ep_beta1 #limite superior para beta1



##5
x0 = 20
est_resp_med = est_beta0 + est_beta1*x0 #resposta média para x0
est_var_resp_med = est_var*(1/n + (x0 - x_bar)^2/sxx) #variância para a resposta média

#intervalo de confiança para resposta média:
resp_med_min = est_resp_med - t1*sqrt(est_var_resp_med) #limite inferior para resposta média
resp_med_max = est_resp_med + t1*sqrt(est_var_resp_med) #limite superior para resposta média



##6
est_var_obs_fut = est_var*(1 +1/n + (x0 - x_bar)^2/sxx) 
#intervalo de confiança para resposta média:
obs_fut_min = est_resp_med - t1*sqrt(est_var_obs_fut) #limite inferior para previsão de observação futura
obs_fut_max = est_resp_med + t1*sqrt(est_var_obs_fut) #limite superior para previsão de observação futura


##7
resid_pad = resid/sqrt(est_var) #residuos padronizados