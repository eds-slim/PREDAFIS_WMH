## uses dataframe datamrs_fu


datamrs_fu$fu_6_mrs <- as.factor(datamrs_fu$fu_6_mrs)

data <- datamrs_fu%>% 
  mutate(logwmh = log(wmh_volume)) %>% 
  mutate(age = (age > 60) %>% as.numeric()) %>% 
  mutate(fu_6_mrs = factor(fu_6_mrs, ordered = TRUE)) %>% 
  dplyr::select(age, logwmh, mrs0_2_fu6, fu_6_mrs, nihss_score, mrs_pre_stroke) %>% 
  mutate(mrs_pre_stroke = as.numeric(mrs_pre_stroke)) %>% 
  na.omit()

covs <- c('nihss_score', 'mrs_pre_stroke')

data
nrow(data)

# group by age and see differences between mean WMH Vol
res1 <- compareGroups(age ~logwmh, data= data, method= c('logwmh'=2))
restab <- createTable(res1, show.all= TRUE)
print(restab)

#or
data %>% group_by(age) %>% summarise(m = mean(exp(logwmh)))



require(mediation)
require(MASS)

# path c
model.sim <- glm(as.formula(paste0('mrs0_2_fu6 ~ age +', paste(covs, collapse = '+'))), data = data, family = binomial(link = 'logit'))
broom::tidy(model.sim, exponentiate = TRUE, conf.int = TRUE)

#path a
#model.m <- lm(as.formula(paste0('logwmh' ~ age, data = data)))
#broom::tidy(model.m)

model.m.2 <- lm(as.formula(paste0('logwmh ~ age +', paste(covs, collapse = '+'))), data=data)
broom::tidy(model.m.2)

#path b


model.y.3 <- glm(as.formula(paste0('mrs0_2_fu6 ~ logwmh + ', paste(covs, collapse = '+'))), data= data, famil = binomial)
broom::tidy(model.y.3, exponentiate= TRUE, conf.int = TRUE)

#path c'
#model.y <- glm(as.formula(paste0('mrs0_1_fu6 ~ age + logwmh +', paste(covs, collapse = '+'))), data = data, family = binomial(link = 'probit'))
#broom::tidy(model.y)

model.y.2 <- glm(as.formula(paste0('mrs0_2_fu6 ~ age + logwmh + ', paste(covs, collapse = '+'))), data= data, famil = binomial)
broom::tidy(model.y.2, exponentiate= TRUE, conf.int = TRUE)

medi <- mediate(model.m = model.m.2, model.y = model.y.2, treat = 'age', mediator = 'logwmh', boot= T, boot.ci.type = 'perc', sims = 1e3)
summary(medi)

####### ordinal mrs
model.y.polr <- polr(as.formula(paste0('fu_6_mrs ~ age + logwmh +', paste(covs, collapse = '+'))), data = data)
broom::tidy(model.y.polr, conf.int = TRUE)

mm <- mediate(model.m.2, model.y.polr, treat = 'age', mediator = 'logwmh', boot = TRUE, sims = 1000)
summary(mm)
plot(mm)

Q <- mm$d0^2%>% sum()
deltaI <- t(mm$d0.sims %>% t() - mm$d0)
QQ <- apply(deltaI, 1, function(x)(sum(x^2)))
p <- mean(QQ > Q)
quantile(QQ-Q, probs = c(0.025, 0.975))
mm$d0/mm$tau.coef


## medflex = Natural effect model
require(medflex)
expData <- neImpute(as.formula(paste0('mrs0_2_fu6 ~ age + logwmh +', paste(covs, collapse = '+'))), family = binomial("logit"), data = data, nRep = 3)
neMod1 <- neModel(as.formula(paste0('mrs0_2_fu6 ~ age0 + age1 +', paste(covs, collapse = '+'))), family = binomial("logit"), expData = expData, se = "robust")
summary(neMod1)

## sobel test
library(multilevel)

sobel(data$age, data$logwmh, data$mrs0_1_fu6)
pnorm(save$z.value, lower.tail = T)*2

