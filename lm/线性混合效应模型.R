getwd()
setwd("/Users/panquan/rstudio/lm")
library(lme4) 
data(cbpp)

#gm_all <- allFit(gm1) 
gm1 <- glmer(cbind(incidence, size - incidence) 
             ~ period + (1 | herd), data = cbpp, family = binomial) ## show available methods
# (1 | herd) 或
allFit(show.meth.tab=TRUE) #显示使用哪些优化方法
gm_all <- allFit(gm1) 
ss <- summary(gm_all) 
ss$fixef ## extract fixed effects 提取固定效果
ss$llik ## log-likelihoods 
ss$sdcor ## SDs and correlations SD和相关性
ss$theta ## Cholesky factors Cholesky因素
ss$which.OK ## which fits worked 哪个适合

#data(Arabidopsis)
data(Arabidopsis)
summary(Arabidopsis[,"total.fruits"]) 
table(gsub("[0-9].","",levels(Arabidopsis[,"popu"])))
#table制表统计个数和出现的次数
#gsub：替换但保留原数据,把"[0-9]."替换为空值; 
#lecels：统计一列分类数据的水平
library(lattice) 
stripplot(log(total.fruits+1) ~ amd|nutrient, data = Arabidopsis,
          groups = gen,
          strip=strip.custom(strip.names=c(TRUE,TRUE)),
          type=c('p','a'), ## points and panel-average value --
          ## see ?panel.xyplot 
          scales=list(x=list(rot=90)), 
          main="Panel: nutrient, Color: genotype")

#bootMer 为混合模型执行基于模型的（半）参数化引导程序。
fm01ML <- lmer(Yield ~ 1|Batch, Dyestuff, REML = FALSE) 
## see ?"profile-methods" 
mySumm <- function(.) { 
  s <- sigma(.)
  c(beta =getME(., "beta"), sigma = s, sig01 = unname(s * getME(., "theta")))
}
(t0 <- mySumm(fm01ML)) # just three parameters
## alternatively:
mySumm2 <- function(.) {
  c(beta=fixef(.),sigma=sigma(.), sig01=sqrt(unlist(VarCorr(.)))) 
}

set.seed(101) ## 3.8s (on a 5600 MIPS 64bit fast(year 2009) desktop "AMD Phenom(tm) II X4 925"): 
system.time( boo01 <- bootMer(fm01ML, mySumm, nsim = 100) )

## to "look" at it 
if (requireNamespace("boot")) {
  boo01 
  ## note large estimated bias for sig01 
  ## (~30% low, decreases _slightly_ for nsim = 1000)
  ## extract the bootstrapped values as a data frame ... 
  head(as.data.frame(boo01))
  ## ------ Bootstrap-based confidence intervals ------------
  ## warnings about "Some ... intervals may be unstable" go away 
  ## for larger bootstrap samples, e.g. nsim=500
  ## intercept 
  (bCI.1 <- boot::boot.ci(boo01, index=1, type=c("norm", "basic", "perc")))
  # beta
 
  ## Residual standard deviation - original scale:
  (bCI.2 <- boot::boot.ci(boo01, index=2, type=c("norm", "basic", "perc"))) 
  ## Residual SD - transform to log scale:
  (bCI.2L <- boot::boot.ci(boo01, index=2, type=c("norm", "basic", "perc"), 
                           h = log, hdot = function(.) 1/., hinv = exp))

  ## Among-batch variance:
  (bCI.3 <- boot::boot.ci(boo01, index=3, type=c("norm", "basic", "perc"))) # sig01
  confint(boo01) 
  confint(boo01,type="norm") 
  confint(boo01,type="basic")
  
  ## Graphical examination: 
  plot(boo01,index=3)
  
  ## Check stored values from a longer (1000-replicate) run: 
  (load(system.file("testdata","boo01L.RData", package="lme4")))# "boo01L" 
  plot(boo01L, index=3) 
  mean(boo01L$t[,"sig01"]==0) ## note point mass at zero!
}
## if boot package available

#cake Breakage Angle of Chocolate Cakes巧克力蛋糕的破损角度
#关于巧克力蛋糕破损角的数据，用三种不同的配方制成，并在六种不同温度下烘烤。 
#这是一种裂区设计，配方是整体单元，不同的温度应用于子单元（重复内部）。 
#实验说明表明重复编号代表时间排序。
#replicate a factor with levels 1 to 15 
#recipe a factor with levels A, B and C
#temperature an ordered factor with levels 175 < 185 < 195 < 205 < 215 < 225  温度因子型
#angle a numeric vector giving the angle at which the cake broke.
#temp numeric value of the baking temperature (degrees F)温度数值型

str(cake) #即structure，紧凑的显示对象内部结构，即对象里有什么
## 'temp' is continuous, 'temperature' an ordered factor with 6 levels
(fm1 <- lmer(angle ~ recipe * temperature + (1|recipe:replicate), cake, REML= FALSE)) 
(fm2 <- lmer(angle ~ recipe + temperature + (1|recipe:replicate), cake, REML= FALSE)) 
(fm3 <- lmer(angle ~ recipe + temp + (1|recipe:replicate), cake, REML= FALSE))
## and now "choose" : 
anova(fm3, fm2, fm1)

#Contagious bovine pleuropneumonia 传染性牛胸膜肺炎
#该数据集描述了在位于埃塞俄比亚Boji区的15个商业畜群进行的后续调查中，瘤牛中CBPP的血清学发生率。
#该调查的目的是研究CBPP在新感染牛群中的内部传播。 
#从这些畜群的所有动物中每季度收集血样以确定其CBPP状态。 
#这些数据用于计算CBPP的血清学发生率（在给定时间段内发生的新病例）。 
#一些数据丢失（失去跟进）。
#A data frame with 56 observations on the following 4 variables.
#herd A factor identifying the herd (1 to 15).
#incidence The number of new serological cases for a given herd and time period.
#size A numeric vector describing herd size at the beginning of a given time period. 
#period A factor with levels 1 to 4.

## response as a matrix 
(m1 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd), family = binomial, data = cbpp)) 
## response as a vector of probabilities and usage of argument "weights" 
m1p <- glmer(incidence / size ~ period + (1 | herd), weights = size, family = binomial, data = cbpp) 
## Confirm that these are equivalent:
stopifnot(all.equal(fixef(m1), fixef(m1p), tolerance = 1e-5), 
          all.equal(ranef(m1), ranef(m1p), tolerance = 1e-5))
## GLMM with individual-level variability (accounting for overdispersion) 
cbpp$obs <- 1:nrow(cbpp) 
(m2 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd) + (1|obs), family = binomial, data = cbpp))

