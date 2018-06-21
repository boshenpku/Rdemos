library(lme4)
library(MASS)
#library(languageR)


#for instructions from a cool Russian scientist: https://www.youtube.com/watch?v=Od8gfNOOS9o



setwd("/Users/boshen/Box/Experiments/Self Blame & Guilt/Slf_blm_Shanghai1611/analysis/factoranalysis")


r <- read.table('/Users/boshen/Box/Experiments/Self Blame & Guilt/Slf_blm_Shanghai1611/data/calculated/ReasonLong.txt',head=T)
r$reason <- factor(r$reason)

r <- r[order(r$context),]
r <- r[order(r$agent),]
r <- r[order(r$subid),]

str(r)

# all variance = (1|subj) + condition + subj:condition
rtmp1 <- r[r$reason=='1',]
rtmp2 <- r[r$reason=='2',]
rtmp3 <- r[r$reason=='3',]
rtmp4 <- r[r$reason=='4',]
rtmp5 <- r[r$reason=='5',]
rtmp6 <- r[r$reason=='6',]
rtmp7 <- r[r$reason=='7',]
data2 <- cbind(rtmp1$rating,rtmp2$rating,rtmp3$rating,rtmp4$rating,rtmp5$rating,rtmp6$rating,rtmp7$rating)

# variance = (1|subj)
rrm <- resid(lm(rating ~ context*agent+reason,data=r))
# boxplot(rrm~r$context*r$agent)
rrm1 <- rrm[r$reason=='1']
rrm2 <- rrm[r$reason=='2']
rrm3 <- rrm[r$reason=='3']
rrm4 <- rrm[r$reason=='4']
rrm5 <- rrm[r$reason=='5']
rrm6 <- rrm[r$reason=='6']
rrm7 <- rrm[r$reason=='7']
tmp1 <- aggregate(rrm1,by=list(rtmp1$subid),FUN=mean)
tmp2 <- aggregate(rrm2,by=list(rtmp2$subid),FUN=mean)
tmp3 <- aggregate(rrm3,by=list(rtmp3$subid),FUN=mean)
tmp4 <- aggregate(rrm4,by=list(rtmp4$subid),FUN=mean)
tmp5 <- aggregate(rrm5,by=list(rtmp5$subid),FUN=mean)
tmp6 <- aggregate(rrm6,by=list(rtmp6$subid),FUN=mean)
tmp7 <- aggregate(rrm7,by=list(rtmp7$subid),FUN=mean)
data2 <- cbind(tmp1$x,tmp2$x,tmp3$x,tmp4$x,tmp5$x,tmp6$x,tmp7$x)


# variance = condition
rrm <- resid(lm(rating ~ subid+reason,data=r))
rrm1 <- rrm[r$reason=='1']
rrm2 <- rrm[r$reason=='2']
rrm3 <- rrm[r$reason=='3']
rrm4 <- rrm[r$reason=='4']
rrm5 <- rrm[r$reason=='5']
rrm6 <- rrm[r$reason=='6']
rrm7 <- rrm[r$reason=='7']
tmp1 <- aggregate(rrm1,by=list(rtmp1$context,rtmp1$agent),FUN=mean)
tmp2 <- aggregate(rrm2,by=list(rtmp2$context,rtmp2$agent),FUN=mean)
tmp3 <- aggregate(rrm3,by=list(rtmp3$context,rtmp3$agent),FUN=mean)
tmp4 <- aggregate(rrm4,by=list(rtmp4$context,rtmp4$agent),FUN=mean)
tmp5 <- aggregate(rrm5,by=list(rtmp5$context,rtmp5$agent),FUN=mean)
tmp6 <- aggregate(rrm6,by=list(rtmp6$context,rtmp6$agent),FUN=mean)
tmp7 <- aggregate(rrm7,by=list(rtmp7$context,rtmp7$agent),FUN=mean)
data2 <- cbind(tmp1$x,tmp2$x,tmp3$x,tmp4$x,tmp5$x,tmp6$x,tmp7$x)
'''''''''''''''''''''''FAILED'''''''''''''''''''''''''''

# variance = subj:condition
rrm <- resid(lm(rating ~ subid+reason+agent*context,data=r))
rrm1 <- rrm[r$reason=='1']
rrm2 <- rrm[r$reason=='2']
rrm3 <- rrm[r$reason=='3']
rrm4 <- rrm[r$reason=='4']
rrm5 <- rrm[r$reason=='5']
rrm6 <- rrm[r$reason=='6']
rrm7 <- rrm[r$reason=='7']
data2 <- cbind(rrm1,rrm2,rrm3,rrm4,rrm5,rrm6,rrm7)

# variance = （1|subj) + subj:condition
rrm <- resid(lm(rating ~ reason+agent*context,data=r))
rrm1 <- rrm[r$reason=='1']
rrm2 <- rrm[r$reason=='2']
rrm3 <- rrm[r$reason=='3']
rrm4 <- rrm[r$reason=='4']
rrm5 <- rrm[r$reason=='5']
rrm6 <- rrm[r$reason=='6']
rrm7 <- rrm[r$reason=='7']
data2 <- cbind(rrm1,rrm2,rrm3,rrm4,rrm5,rrm6,rrm7)


# variance = condition + subj:condition
rrm <- resid(lm(rating ~ subid+reason,data=r))
rrm1 <- rrm[r$reason=='1']
rrm2 <- rrm[r$reason=='2']
rrm3 <- rrm[r$reason=='3']
rrm4 <- rrm[r$reason=='4']
rrm5 <- rrm[r$reason=='5']
rrm6 <- rrm[r$reason=='6']
rrm7 <- rrm[r$reason=='7']
data2 <- cbind(rrm1,rrm2,rrm3,rrm4,rrm5,rrm6,rrm7)

# only one condition
rtmp1 <- r[r$reason=='1'&r$agent=='self'&r$context=='reparable',]
rtmp2 <- r[r$reason=='2'&r$agent=='self'&r$context=='reparable',]
rtmp3 <- r[r$reason=='3'&r$agent=='self'&r$context=='reparable',]
rtmp4 <- r[r$reason=='4'&r$agent=='self'&r$context=='reparable',]
rtmp5 <- r[r$reason=='5'&r$agent=='self'&r$context=='reparable',]
rtmp6 <- r[r$reason=='6'&r$agent=='self'&r$context=='reparable',]
rtmp7 <- r[r$reason=='7'&r$agent=='self'&r$context=='reparable',]
data2 <- cbind(rtmp1$x,rtmp2$x,rtmp3$x,rtmp4$x,rtmp5$x,rtmp6$x,rtmp7$x)



# row.names(rwidemat) <- c(1,2,3,4,5,6,7)



#descriptive statistics
summary(data2)
cor(data2)


##plot the pairwise scatterplots
pairs(data2) 



#2 ways to do PCA: prcomp and princomp

##perform the PCA
mypca <- princomp(data2, scores=TRUE, cor=TRUE)  #scores transforms the data into the transformed space; cor = correlation matrix instead of covariance (however, I think we can use covariance bc all are using 1-7 likert scale)
summary(mypca)

#look at eigenvalues, which is the "Standard Deviation"--if above one, then we can use that component

#could also try cov (the same when scale is the same)
#mypca <- princomp(data2, scores=TRUE, cov=TRUE)  #scores transforms the data into the transformed space; cor = correlation matrix instead of covariance (however, I think we can use covariance bc all are using 1-7 likert scale)
#summary(mypca)

##print the scree plot
# we can retain anything with variances (y-value; eignevalues) above 1!
plot(mypca)
#line scree plot (the way we usually visualize a scree plot)
screeplot(mypca, type = "line", main ="Scree Plot")




##biplot: correlation of components (how the components agree/disagree with eachother)
biplot(mypca)


## print the loadings associated with the principal components (if less than 0.1, it doesn't show up)
#the loadings are telling you the correlation b/w the component and the original variable
#the loadings: we want to see as few variables as possible loading very strongly on the components 
mypca$loadings

##print the scores (projections): these are the rotated or transformed data set
## we can use these scores as the new data that we can use instead of the original variables (we are combining variables into smaller more representative components
mypca$scores


#rotation  the teacher did not get these to work and neither did i :(
#why do we want to rotate the loadings? 
#varimax(mypca$rotation)
#promax(mypca$rotation)
#If I can run this, this should not be called PCA; should be called "PCA followed by a varimax rotation"(see: http://stats.stackexchange.com/questions/612/is-pca-followed-by-a-rotation-such-as-varimax-still-pca)





#######################################

#Factor analysis (# of factors taken from PCA)

myfa <- factanal(data2, factor=2, rotation="none") #no rotation (should rotate dude)
myfa

myfa2 <- factanal(data2, factor=3, rotation="varimax") #use this one i think because it should be rotated, as the assumption is that everything is orthogonal, but it's usually not
myfa2

myfa3 <- factanal(data2, factor=2, rotation="varimax", scores="regression")
myfa3

##uniqueness high means communality low, or the variance explained by the facotrs is low
##loadings: the variables' loading/correlation with the factors
	##NOTE: you can name your factors/components based on the items/variables that load/correlate with the factors/components! :)


######################################
# EFA
library(psych)
fa.parallel(data2,fa='both')
dev.copy(pdf,'FA.ParallelAllVari.pdf',width=8,height=8)
dev.copy(pdf,'FA.ParallelRmSubjRandom.pdf',width=8,height=8)
dev.off()
fa <- fa(data2,nfactors=3,rotate='none',fm='pa')
fa.varimax <- fa(data2,nfactors=3,rotate='varimax',fm='pa')
fa.promax <- fa(data2,nfactors=2,rotate='promax',fm='pa')
factor.plot(fa.promax,labels=rownames(fa.promax$loading))
dev.copy(pdf,'FA.PlotAllVari2Factors.pdf',width=8,height=8)
dev.copy(pdf,'FA.PlotRmSubjRandom2Factors.pdf',width=8,height=8)
dev.off()
fa.diagram(fa.promax,simple=F)
dev.copy(pdf,'FA.DiagramAllVari2Factors.pdf',width=8,height=8)
dev.copy(pdf,'FA.DiagramRmSubjRandom2Factors.pdf',width=8,height=8)
dev.off()
fsm(fa.promax)




