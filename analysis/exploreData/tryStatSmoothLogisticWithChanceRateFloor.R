library(ggplot2)

#See method 2, glm, here for custom link function with a parameter  so could do the chanceRate
#https://www.rpubs.com/bbolker/logregexp  , it doesn't have geom_smooth embedding however, have to take that from an example like the below





######custom geom_smoth embedding
#Show custom geom_smoth embedding  using dose-response modeling library, but then wont' have custom link functions, just its link functions
#https://stackoverflow.com/questions/36780357/plotting-dose-response-curves-with-ggplot2-and-drc

library(drc)

df <- structure(list(iv = c(1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6, 1, 
                            2, 3, 4, 5, 6), dv = c(9.2, 8.5, 13.5, 15.8, 18.3, 17.7, 8.7, 
                                                   10.8, 14.3, 15, 18, 15.3, 8.7, 14.6, 14.8, 16.8, 15.8, 15.8)), row.names = c(NA, 
                                                                                                                                -18L), class = c("tbl_df", "tbl", "data.frame"))

drm_asymp <- drc::drm(
  dv ~ iv,
  data = df,,
  fct = AR.2(names = c("Asym", "lrc"))
)
ggplot(data = df, aes(x = iv, y = dv)) +
  geom_point() +
  geom_smooth(
    method = nls,
    se = F,
    method.args = list(formula = y ~ SSasympOrig(x, Asym, lrc), start=list(Asym=17, lrc=-.5))
  )

####

df <- structure(list(iv = c(1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6, 1, 
                            2, 3, 4, 5, 6), dv = c(9.2, 8.5, 13.5, 15.8, 18.3, 17.7, 8.7, 
                                                   10.8, 14.3, 15, 18, 15.3, 8.7, 14.6, 14.8, 16.8, 15.8, 15.8)), row.names = c(NA, 
                                                                                                                                -18L), class = c("tbl_df", "tbl", "data.frame"))

drm_asymp <- drc::drm(
  dv ~ iv,
  data = df,,
  fct = AR.2(names = c("Asym", "lrc"))
)
ggplot(data = df, aes(x = iv, y = dv)) +
  geom_point() +
  geom_smooth(
    method = nls,
    se = F,
    method.args = list(formula = y ~ drc::SSasympOrig(x, Asym, lrc), start=list(Asym=17, lrc=-.5))
  )



###

##Weird this doesn't work
d<-data.frame(n=c(100, 80, 60, 55, 50, 102, 78, 61, 42, 18),
              year=rep(2000:2004, 2), 
              cat=rep(c("a", "b"), each=5))

lm.mod<-function(df){
  m1<-lm(n~year, data=df)
  m2<-lm(n~year+I(year^2), data=df)
  p <- ifelse(AIC(m2)<AIC(m1), "y~x", "y~poly(x, 2)")
  return(p) 
}
# I only made the return here explicit out of personal preference

ggplot(d, aes(year, n, color=cat)) + geom_line() + geom_point() +
  stat_smooth(method=lm, formula=lm.mod(d), se=F)

ggplot(d, aes(year, n, group=cat)) + geom_line() + geom_point() +
  facet_wrap(~cat, ncol=1)+
  stat_smooth(method=lm, formula=lm.mod(d))