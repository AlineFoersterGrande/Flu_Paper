##############################################################################################################################
################################## DESCRIPTIVE ANALYSIS ##############################################################################
#########################################################################################################################

# Data
library(readxl)
matriz_ncont_analise <- read_excel("matriz_ncont_analise.xlsx")
matriz_ncont_analise=as.matrix(matriz_ncont_analise)

matriz_ncont_analise_genetica <- read_excel("matriz_ncont_analise_genetica.xlsx")
matriz_ncont_analise_genetica=as.matrix(matriz_ncont_analise_genetica)

# Monthly average influenza incidence in each region:
medias_janeiro=medias_fevereiro=medias_marco=medias_abril=medias_maio=medias_junho=medias_julho=medias_agosto=medias_setembro=medias_outubro=medias_novembro=medias_dezembro=numeric(7)

for (i in 1:7){
  medias_janeiro[i]=mean(t(matriz_ncont_analise)[i,c(4,16,28,40,52,64,76,88,100,112)])
  medias_fevereiro[i]=mean(t(matriz_ncont_analise)[i,c(5,17,29,41,53,65,77,89,101,113)])
  medias_marco[i]=mean(t(matriz_ncont_analise)[i,c(6,18,30,42,54,66,78,90,102,114)])
  medias_abril[i]=mean(t(matriz_ncont_analise)[i,c(7,19,31,43,55,67,79,91,103,115)])
  medias_maio[i]=mean(t(matriz_ncont_analise)[i,c(8,20,32,44,56,68,80,92,104,116)])
  medias_junho[i]=mean(t(matriz_ncont_analise)[i,c(9,21,33,45,57,69,81,93,105,117)])
  medias_julho[i]=mean(t(matriz_ncont_analise)[i,c(10,22,34,46,58,70,82,94,106,118)])
  medias_agosto[i]=mean(t(matriz_ncont_analise)[i,c(11,23,35,47,59,71,83,95,107,119)])
  medias_setembro[i]=mean(t(matriz_ncont_analise)[i,c(12,24,36,48,60,72,84,96,108,120)])
  medias_outubro[i]=mean(t(matriz_ncont_analise)[i,c(1,13,25,37,49,61,73,85,97,109,121)])
  medias_novembro[i]=mean(t(matriz_ncont_analise)[i,c(2,14,26,38,50,62,74,86,98,110,122)])
  medias_dezembro[i]=mean(t(matriz_ncont_analise)[i,c(3,15,27,39,51,63,75,87,99,111,123)])
}
names(medias_janeiro)=names(medias_fevereiro)=names(medias_marco)=names(medias_abril)=names(medias_maio)=names(medias_junho)=names(medias_julho)=names(medias_agosto)=names(medias_setembro)=names(medias_outubro)=names(medias_novembro)=names(medias_dezembro)=rownames(t(matriz_ncont_analise))
###################

## Incidence Data

# Brazil (Monthly average)
library(ggplot2)
medias_BR_grafico=c(medias_janeiro["Brasil"],medias_fevereiro["Brasil"],medias_marco["Brasil"],medias_abril["Brasil"],medias_maio["Brasil"],medias_junho["Brasil"],medias_julho["Brasil"],medias_agosto["Brasil"],medias_setembro["Brasil"],medias_outubro["Brasil"],medias_novembro["Brasil"],medias_dezembro["Brasil"])
medias_BR_grafico=cbind(medias_BR_grafico, meses=c(1:12))
medias_BR_grafico=as.data.frame(medias_BR_grafico)
ggplot(medias_BR_grafico, aes(x=meses)) +
  geom_line(aes(y=medias_BR_grafico), color = "#00AFBB", size = 1) +
  labs(x = "Months", y = "Number of positive cases", title = "Brazil") + scale_x_discrete(limits=c("jan", "feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))

# North America Region (Monthly average)
medias_AmericaNorte_grafico=c(medias_janeiro["RegiaoAmericaNorte"],medias_fevereiro["RegiaoAmericaNorte"],medias_marco["RegiaoAmericaNorte"],medias_abril["RegiaoAmericaNorte"],medias_maio["RegiaoAmericaNorte"],medias_junho["RegiaoAmericaNorte"],medias_julho["RegiaoAmericaNorte"],medias_agosto["RegiaoAmericaNorte"],medias_setembro["RegiaoAmericaNorte"],medias_outubro["RegiaoAmericaNorte"],medias_novembro["RegiaoAmericaNorte"],medias_dezembro["RegiaoAmericaNorte"])
medias_AmericaNorte_grafico=cbind(medias_AmericaNorte_grafico, meses=c(1:12))
medias_AmericaNorte_grafico=as.data.frame(medias_AmericaNorte_grafico)
ggplot(medias_AmericaNorte_grafico, aes(x=meses)) +
  geom_line(aes(y=medias_AmericaNorte_grafico), color = "hotpink", size = 1) +
  labs(x = "Months", y = "Number of positive cases", title = "North America Region") + scale_x_discrete(limits=c("jan", "feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))

# South America Region (Monthly average)
medias_AmericaSul_grafico=c(medias_janeiro["RegiaoAmericaSul"],medias_fevereiro["RegiaoAmericaSul"],medias_marco["RegiaoAmericaSul"],medias_abril["RegiaoAmericaSul"],medias_maio["RegiaoAmericaSul"],medias_junho["RegiaoAmericaSul"],medias_julho["RegiaoAmericaSul"],medias_agosto["RegiaoAmericaSul"],medias_setembro["RegiaoAmericaSul"],medias_outubro["RegiaoAmericaSul"],medias_novembro["RegiaoAmericaSul"],medias_dezembro["RegiaoAmericaSul"])
medias_AmericaSul_grafico=cbind(medias_AmericaSul_grafico, meses=c(1:12))
medias_AmericaSul_grafico=as.data.frame(medias_AmericaSul_grafico)
ggplot(medias_AmericaSul_grafico, aes(x=meses)) +
  geom_line(aes(y=medias_AmericaSul_grafico), color = "mediumorchid1", size = 1) +
  labs(x = "Months", y = "Number of positive cases", title = "South America Region") + scale_x_discrete(limits=c("jan", "feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))

# Central America Region (Monthly average)
medias_AmericaCentral_grafico=c(medias_janeiro["RegiaoAmericaCentral"],medias_fevereiro["RegiaoAmericaCentral"],medias_marco["RegiaoAmericaCentral"],medias_abril["RegiaoAmericaCentral"],medias_maio["RegiaoAmericaCentral"],medias_junho["RegiaoAmericaCentral"],medias_julho["RegiaoAmericaCentral"],medias_agosto["RegiaoAmericaCentral"],medias_setembro["RegiaoAmericaCentral"],medias_outubro["RegiaoAmericaCentral"],medias_novembro["RegiaoAmericaCentral"],medias_dezembro["RegiaoAmericaCentral"])
medias_AmericaCentral_grafico=cbind(medias_AmericaCentral_grafico, meses=c(1:12))
medias_AmericaCentral_grafico=as.data.frame(medias_AmericaCentral_grafico)
ggplot(medias_AmericaCentral_grafico, aes(x=meses)) +
  geom_line(aes(y=medias_AmericaCentral_grafico), color = "tomato", size = 1) +
  labs(x = "Months", y = "Number of positive cases", title = "Central America Region") + scale_x_discrete(limits=c("jan", "feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))

# European Region (Monthly average)
medias_Europa_grafico=c(medias_janeiro["RegiaoEuropeia"],medias_fevereiro["RegiaoEuropeia"],medias_marco["RegiaoEuropeia"],medias_abril["RegiaoEuropeia"],medias_maio["RegiaoEuropeia"],medias_junho["RegiaoEuropeia"],medias_julho["RegiaoEuropeia"],medias_agosto["RegiaoEuropeia"],medias_setembro["RegiaoEuropeia"],medias_outubro["RegiaoEuropeia"],medias_novembro["RegiaoEuropeia"],medias_dezembro["RegiaoEuropeia"])
medias_Europa_grafico=cbind(medias_Europa_grafico, meses=c(1:12))
medias_Europa_grafico=as.data.frame(medias_Europa_grafico)
ggplot(medias_Europa_grafico, aes(x=meses)) +
  geom_line(aes(y=medias_Europa_grafico), color = "chartreuse4", size = 1) +
  labs(x = "Months", y = "Number of positive cases", title = "European Region") + scale_x_discrete(limits=c("jan", "feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))

# South Asia Region (Monthly average)
medias_SouthAsia_grafico=c(medias_janeiro["RegiaoSouthAsia"],medias_fevereiro["RegiaoSouthAsia"],medias_marco["RegiaoSouthAsia"],medias_abril["RegiaoSouthAsia"],medias_maio["RegiaoSouthAsia"],medias_junho["RegiaoSouthAsia"],medias_julho["RegiaoSouthAsia"],medias_agosto["RegiaoSouthAsia"],medias_setembro["RegiaoSouthAsia"],medias_outubro["RegiaoSouthAsia"],medias_novembro["RegiaoSouthAsia"],medias_dezembro["RegiaoSouthAsia"])
medias_SouthAsia_grafico=cbind(medias_SouthAsia_grafico, meses=c(1:12))
medias_SouthAsia_grafico=as.data.frame(medias_SouthAsia_grafico)
ggplot(medias_SouthAsia_grafico, aes(x=meses)) +
  geom_line(aes(y=medias_SouthAsia_grafico), color = "royalblue", size = 1) +
  labs(x = "Months", y = "Number of positive cases", title = "South Asia Region") + scale_x_discrete(limits=c("jan", "feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))

# Western Pacific Region (Monthly average)
medias_WesternPacific_grafico=c(medias_janeiro["RegiaoWesternPacific"],medias_fevereiro["RegiaoWesternPacific"],medias_marco["RegiaoWesternPacific"],medias_abril["RegiaoWesternPacific"],medias_maio["RegiaoWesternPacific"],medias_junho["RegiaoWesternPacific"],medias_julho["RegiaoWesternPacific"],medias_agosto["RegiaoWesternPacific"],medias_setembro["RegiaoWesternPacific"],medias_outubro["RegiaoWesternPacific"],medias_novembro["RegiaoWesternPacific"],medias_dezembro["RegiaoWesternPacific"])
medias_WesternPacific_grafico=cbind(medias_WesternPacific_grafico, meses=c(1:12))
medias_WesternPacific_grafico=as.data.frame(medias_WesternPacific_grafico)
ggplot(medias_WesternPacific_grafico, aes(x=meses)) +
  geom_line(aes(y=medias_WesternPacific_grafico), color = "darkgoldenrod1", size = 1) +
  labs(x = "Months", y = "Number of positive cases", title = "Western Pacific Region") + scale_x_discrete(limits=c("jan", "feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))
##########################################################

# Correlation matrix of influenza incidence in the different regions considered
library(RColorBrewer)
M=cor(matriz_ncont_analise)
colnames(M)=c("NorthAmerica", "Brazil", "SouthAmerica","CentralAmerica", "Europe","SouthAsia","WesternPacific")
rownames(M)=colnames(M)
library(ggcorrplot)
ggcorrplot(M,hc.order = TRUE, type = "lower",lab = TRUE)
##########################################################

# Observation: Comparing time series
matriz_ncont_analise_graficoseries_=matriz_ncont_analise
matriz_ncont_analise_graficoseries_=as.data.frame(matriz_ncont_analise_graficoseries_)
rownames(matriz_ncont_analise_graficoseries_)=c(1:123)
colors <- c("NorthAmerica" = "orange1", "Europe" = "springgreen2","SouthAmerica" = "brown1")

ggplot(matriz_ncont_analise_graficoseries_, aes(x = c(1:123))) +
  geom_line(aes(y = RegiaoAmericaNorte, color = "NorthAmerica"), size = 0.75) +
  geom_line(aes(y = RegiaoEuropeia, color = "Europe"), size = 0.75) +
  geom_line(aes(y = RegiaoAmericaSul, color = "SouthAmerica"), size = 0.75) +
  labs(x = "Years", y = "Number of positive cases", title = "Comparing time series",color = "Regions") +
  scale_color_manual(values = colors) +
  scale_x_continuous(limit = c(0,123),
                     breaks = c(4,16,28,40,52,64,76,88,100,112),
                     labels = c("2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"))
##########################################################

# Observation: Time series of flu incidence (with years on the x axis)

# Incidence Graphs:
matriz_ncont_analise_graficoseries_2008_2018=matriz_ncont_analise_graficoseries_
rownames(matriz_ncont_analise_graficoseries_2008_2018)=c(1:123)

# Brazil
ggplot() +
  geom_line(data = matriz_ncont_analise_graficoseries_2008_2018, aes(x=c(1:123), y = matriz_ncont_analise_graficoseries_2008_2018[,"Brasil"]), color = "#00AFBB", size=1) +
  labs(x = "Years", y = "Number of positive cases", title = "Brazil")+
  scale_x_continuous(limit = c(0,123),
                     breaks = c(4,16,28,40,52,64,76,88,100,112),
                     labels = c("2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"))

# North America Region
ggplot() +
  geom_line(data = matriz_ncont_analise_graficoseries_2008_2018, aes(x=c(1:123), y = matriz_ncont_analise_graficoseries_2008_2018[,"RegiaoAmericaNorte"]), color = "hotpink", size=1) +
  labs(x = "Years", y = "Number of positive cases", title = "North America Region")+
  scale_x_continuous(limit = c(0,123),
                     breaks = c(4,16,28,40,52,64,76,88,100,112),
                     labels = c("2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"))

# South America Region
ggplot() +
  geom_line(data = matriz_ncont_analise_graficoseries_2008_2018, aes(x=c(1:123), y = matriz_ncont_analise_graficoseries_2008_2018[,"RegiaoAmericaSul"]), color = "mediumorchid1", size=1) +
  labs(x = "Years", y = "Number of positive cases", title = "South America Region")+
  scale_x_continuous(limit = c(0,123),
                     breaks = c(4,16,28,40,52,64,76,88,100,112),
                     labels = c("2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"))

# Central America Region
ggplot() +
  geom_line(data = matriz_ncont_analise_graficoseries_2008_2018, aes(x=c(1:123), y = matriz_ncont_analise_graficoseries_2008_2018[,"RegiaoAmericaCentral"]), color = "tomato", size=1) +
  labs(x = "Years", y = "Number of positive cases", title = "Central America Region")+
  scale_x_continuous(limit = c(0,123),
                     breaks = c(4,16,28,40,52,64,76,88,100,112),
                     labels = c("2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"))

# European Region
ggplot() +
  geom_line(data = matriz_ncont_analise_graficoseries_2008_2018, aes(x=c(1:123), y = matriz_ncont_analise_graficoseries_2008_2018[,"RegiaoEuropeia"]), color = "chartreuse4", size=1) +
  labs(x = "Years", y = "Number of positive cases", title = "European Region")+
  scale_x_continuous(limit = c(0,123),
                     breaks = c(4,16,28,40,52,64,76,88,100,112),
                     labels = c("2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"))

# South Asia Region
ggplot() +
  geom_line(data = matriz_ncont_analise_graficoseries_2008_2018, aes(x=c(1:123), y = matriz_ncont_analise_graficoseries_2008_2018[,"RegiaoSouthAsia"]), color = "royalblue", size=1) +
  labs(x = "Years", y = "Number of positive cases", title = "South Asia Region")+
  scale_x_continuous(limit = c(0,123),
                     breaks = c(4,16,28,40,52,64,76,88,100,112),
                     labels = c("2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"))

# Western Pacific Region
ggplot() +
  geom_line(data = matriz_ncont_analise_graficoseries_2008_2018, aes(x=c(1:123), y = matriz_ncont_analise_graficoseries_2008_2018[,"RegiaoWesternPacific"]), color = "darkgoldenrod1", size=1) +
  labs(x = "Years", y = "Number of positive cases", title = "Western Pacific Region")+
  scale_x_continuous(limit = c(0,123),
                     breaks = c(4,16,28,40,52,64,76,88,100,112),
                     labels = c("2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"))
#######################################################################################################################

## Genetic Data

# Genetic Graphs:
matriz_ncont_analise_graficoseries_2008_2018_genetica=matriz_ncont_analise_genetica
rownames(matriz_ncont_analise_graficoseries_2008_2018_genetica)=c(1:123)
matriz_ncont_analise_graficoseries_2008_2018_genetica=as.data.frame(matriz_ncont_analise_graficoseries_2008_2018_genetica)

# AllH1
ggplot() +
  geom_line(data = matriz_ncont_analise_graficoseries_2008_2018_genetica, aes(x=c(1:123), y = matriz_ncont_analise_graficoseries_2008_2018_genetica[,"AllH1"]), color = "darkgoldenrod2", size=1) +
  labs(x = "Years", y = "Genetic Diversity", title = "Global Diversity (H1N1)")+
  scale_x_continuous(limit = c(0,123),
                     breaks = c(4,16,28,40,52,64,76,88,100,112),
                     labels = c("2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"))

# AllH3
ggplot() +
  geom_line(data = matriz_ncont_analise_graficoseries_2008_2018_genetica, aes(x=c(1:123), y = matriz_ncont_analise_graficoseries_2008_2018_genetica[,"AllH3"]), color = "chartreuse3", size=1) +
  labs(x = "Years", y = "Genetic Diversity", title = "Global Diversity (H3N2)")+
  scale_x_continuous(limit = c(0,123),
                     breaks = c(4,16,28,40,52,64,76,88,100,112),
                     labels = c("2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"))

# NorthAmericaH1
ggplot() +
  geom_line(data = matriz_ncont_analise_graficoseries_2008_2018_genetica, aes(x=c(1:123), y = matriz_ncont_analise_graficoseries_2008_2018_genetica[,"NorthAmericaH1"]), color = "darkgoldenrod1", size=1) +
  labs(x = "Years", y = "Genetic Diversity", title = "North America Diversity (H1N1)")+
  scale_x_continuous(limit = c(0,123),
                     breaks = c(4,16,28,40,52,64,76,88,100,112),
                     labels = c("2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"))

# NorthAmericaH3
ggplot() +
  geom_line(data = matriz_ncont_analise_graficoseries_2008_2018_genetica, aes(x=c(1:123), y = matriz_ncont_analise_graficoseries_2008_2018_genetica[,"NorthAmericaH3"]), color = "chartreuse2", size=1) +
  labs(x = "Years", y = "Genetic Diversity", title = "North America Diversity (H3N2)")+
  scale_x_continuous(limit = c(0,123),
                     breaks = c(4,16,28,40,52,64,76,88,100,112),
                     labels = c("2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"))

# AsiaH1
ggplot() +
  geom_line(data = matriz_ncont_analise_graficoseries_2008_2018_genetica, aes(x=c(1:123), y = matriz_ncont_analise_graficoseries_2008_2018_genetica[,"AsiaH1"]), color = "darkgoldenrod3", size=1) +
  labs(x = "Years", y = "Genetic Diversity", title = "Asia Diversity (H1N1)")+
  scale_x_continuous(limit = c(0,123),
                     breaks = c(4,16,28,40,52,64,76,88,100,112),
                     labels = c("2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"))

# AsiaH3
ggplot() +
  geom_line(data = matriz_ncont_analise_graficoseries_2008_2018_genetica, aes(x=c(1:123), y = matriz_ncont_analise_graficoseries_2008_2018_genetica[,"AsiaH3"]), color = "chartreuse4", size=1) +
  labs(x = "Years", y = "Genetic Diversity", title = "Asia Diversity (H3N2)")+
  scale_x_continuous(limit = c(0,123),
                     breaks = c(4,16,28,40,52,64,76,88,100,112),
                     labels = c("2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"))

##############################################################################################################################
################################## MODELING 1 ##############################################################################
#########################################################################################################################

# Data
library(readxl)
matriz_ncasos_defasagens_mediasBR <- read_excel("matriz_ncasos_defasagens_mediasBR.xlsx")
matriz_ncasos_defasagens_mediasBR=as.matrix(matriz_ncasos_defasagens_mediasBR)

matrix_pred <- read_excel("matrix_pred.xlsx")
matrix_pred=as.matrix(matrix_pred)

matriz_ncont_analise <- read_excel("matriz_ncont_analise.xlsx")
matriz_ncont_analise=as.matrix(t(matriz_ncont_analise))

matriz_ncont_2019 <- read_excel("matriz_ncont_2019.xlsx")
matriz_ncont_2019=as.matrix(t(matriz_ncont_2019))

## Stepwise Function:
# This function performs p-value based stepwise regression. Originally obtained from https://msu.edu/~rubin/code/stepwise_demo.nb.html# (currently extinct)

#'
#' Perform a stepwise linear regression using F tests of significance.
#'
#' @param full.model the model containing all possible terms
#' @param initial.model the first model to consider
#' @param alpha.to.enter the significance level above which a variable may enter the model
#' @param alpha.to.leave the significance level below which a variable may be deleted from the model
#' @param data the data frame to use (optional, as with lm)
#'
#' @return the final model
#'
stepwise <-
  function(full.model, initial.model, alpha.to.enter = 0.0, alpha.to.leave = 1.0, data = NULL) {
    # Sanity check: alpha.to.enter should not be greater than alpha.to.leave.
    if (alpha.to.enter > alpha.to.leave) {
      warning("Your alpha-to-enter is greater than your alpha-to-leave, which could throw the function into an infinite loop.\n")
      return(NA)
    }
    # Warning: horrible kludge coming!
    # Acquire the full and initial models as formulas. If they are
    # entered as formulas, convert them to get their environments
    # squared away.
    # Note: "showEnv = F" is necessary to avoid having an
    # environment identifier break things if the model is
    # defined inside a function.
    if (is.character(full.model)) {
      fm <- as.formula(full.model)
    } else {
      fm <- as.formula(capture.output(print(full.model, showEnv = F)))
    }
    if (is.character(initial.model)) {
      im <- as.formula(initial.model)
    } else {
      im <- as.formula(capture.output(print(initial.model, showEnv = F)))
    }
    # Deal with a missing data argument.
    if (is.null(data)) {
      # Catch the use of "." in a formula when the data argument is null.
      if ("." %in% all.vars(fm) | "." %in% all.vars(im)) {
        warning("In order to use the shortcut '.' in a formula, you must explicitly specify the data source via the 'data' argument.\n")
        return(NA)
      } else {
        # Use the parent environment.
        data <- parent.frame()
      }
    }
    # Fit the full model.
    full <- lm(fm, data);
    # Sanity check: do not allow an overspecified full model.
    if (full$df.residual < 1) {
      warning("Your full model does not have enough observations to properly estimate it.\n")
      return(NA)
    }
    msef <- (summary(full)$sigma)^2;  # MSE of full model
    n <- length(full$residuals);  # sample size
    # Fit the initial model.
    current <- lm(im, data);
    # Process consecutive models until we break out of the loop.
    while (TRUE) {
      # Summarize the current model.
      temp <- summary(current);
      # Print the model description.
      print(temp$coefficients);
      # Get the size, MSE and Mallow's cp of the current model.
      p <- dim(temp$coefficients)[1]; # size
      mse <- (temp$sigma)^2; # MSE
      cp <- (n - p)*mse/msef - (n - 2*p);  # Mallow's cp
      # Show the fit statistics.
      fit <- sprintf("\nS = %f, R-sq = %f, R-sq(adj) = %f, C-p = %f",
                     temp$sigma, temp$r.squared, temp$adj.r.squared, cp);
      # Show the fit itself.
      write(fit, file = "");
      write("=====", file = "");
      # Try to drop a term (but only if more than one is left).
      if (p > 1) {
        # Look for terms that can be dropped based on F tests.
        d <- drop1(current, test = "F");
        # Find the term with largest p-value.
        pmax <- suppressWarnings(max(d[, 6], na.rm = TRUE));
        # If the term qualifies, drop the variable.
        if (pmax > alpha.to.leave) {
          # We have a candidate for deletion.
          # Get the name of the variable to delete.
          var <- rownames(d)[d[,6] == pmax];
          # If an intercept is present, it will be the first name in the list.
          # There also could be ties for worst p-value.
          # Taking the second entry if there is more than one is a safe solution to both issues.
          if (length(var) > 1) {
            var <- var[2];
          }
          # Print out the variable to be dropped.
          write(paste("--- Dropping", var, "\n"), file = "");
          # Modify the formulat to drop the chosen variable (by subtracting it from the current formula).
          f <- formula(current);
          f <- as.formula(paste(f[2], "~", paste(f[3], var, sep = " - ")), env = environment(f));
          # Fit the modified model and loop.
          current <- lm(f, data);
          next;
        }
      }
      # If we get here, we failed to drop a term; try adding one.
      # Note: add1 throws an error if nothing can be added (current == full), which we trap with tryCatch.
      a <- tryCatch(
        add1(current, full, test = "F"),
        error = function(e) NULL
      );
      if (is.null(a)) {
        # There are no unused variables (or something went splat), so we bail out.
        break;
      }
      # Find the minimum p-value of any term (skipping the terms with no p-value). In case none of the remaining terms have a p-value (true of the intercept and any linearly dependent predictors), suppress warnings about an empty list. The test for a suitable candidate to drop will fail since pmin will be set to infinity.
      pmin <- suppressWarnings(min(a[, 6], na.rm = TRUE));
      if (pmin < alpha.to.enter) {
        # We have a candidate for addition to the model. Get the variable's name.
        var <- rownames(a)[a[,6] == pmin];
        # We have the same issue with ties and the presence of an intercept term, and the same solution, as above.
        if (length(var) > 1) {
          var <- var[2];
        }
        # Print the variable being added.
        write(paste("+++ Adding", var, "\n"), file = "");
        # Add it to the current formula.
        f <- formula(current);
        f <- as.formula(paste(f[2], "~", paste(f[3], var, sep = " + ")), env = environment(f));
        # Fit the modified model and loop.
        current <- lm(f, data = data);
        next;
      }
      # If we get here, we failed to make any changes to the model; time to declare victory and exit.
      break;
    }
    current
  }


## Incidence Models:

# Stepwise 1
matriz_ncasos_defasagens_mediasBR=as.data.frame(matriz_ncasos_defasagens_mediasBR)
attach(matriz_ncasos_defasagens_mediasBR)
aToEnter <- 0.10
aToLeave <- 0.10
result <- stepwise(Brasil_Bt ~ 1 + Brasil_Bt1 + Brasil_Bt2 + Brasil_Bt3 + Brasil_Bt4 + Brasil_Bt5 + Brasil_Bt6 + Brasil_Bt7 + Brasil_Bt8 + Brasil_Bt9 + Brasil_Bt10 + Brasil_Bt11 + RegiaoAmericaNorte_At1 + RegiaoAmericaNorte_At2 + RegiaoAmericaNorte_At3 + RegiaoAmericaNorte_At4 + RegiaoAmericaNorte_At5 + RegiaoAmericaNorte_At6 + RegiaoAmericaNorte_At7 + RegiaoAmericaNorte_At8 + RegiaoAmericaNorte_At9 + RegiaoAmericaNorte_At10 + RegiaoAmericaNorte_At11 + RegiaoAmericaSul_St1 + RegiaoAmericaSul_St2 + RegiaoAmericaSul_St3 + RegiaoAmericaSul_St4 + RegiaoAmericaSul_St5 + RegiaoAmericaSul_St6 + RegiaoAmericaSul_St7 + RegiaoAmericaSul_St8 + RegiaoAmericaSul_St9 + RegiaoAmericaSul_St10 + RegiaoAmericaSul_St11 + RegiaoAmericaCentral_Ct1 + RegiaoAmericaCentral_Ct2 + RegiaoAmericaCentral_Ct3 + RegiaoAmericaCentral_Ct4 + RegiaoAmericaCentral_Ct5 + RegiaoAmericaCentral_Ct6 + RegiaoAmericaCentral_Ct7 + RegiaoAmericaCentral_Ct8 + RegiaoAmericaCentral_Ct9 + RegiaoAmericaCentral_Ct10 + RegiaoAmericaCentral_Ct11 + RegiaoEuropeia_Et1 + RegiaoEuropeia_Et2 + RegiaoEuropeia_Et3 + RegiaoEuropeia_Et4 + RegiaoEuropeia_Et5 + RegiaoEuropeia_Et6 + RegiaoEuropeia_Et7 + RegiaoEuropeia_Et8 + RegiaoEuropeia_Et9 + RegiaoEuropeia_Et10 + RegiaoEuropeia_Et11 + RegiaoSouthAsia_st1 + RegiaoSouthAsia_st2 + RegiaoSouthAsia_st3 + RegiaoSouthAsia_st4 + RegiaoSouthAsia_st5 + RegiaoSouthAsia_st6 + RegiaoSouthAsia_st7 + RegiaoSouthAsia_st8 + RegiaoSouthAsia_st9 + RegiaoSouthAsia_st10 + RegiaoSouthAsia_st11 + RegiaoWesternPacific_Wt1 + RegiaoWesternPacific_Wt2 + RegiaoWesternPacific_Wt3 + RegiaoWesternPacific_Wt4 + RegiaoWesternPacific_Wt5 + RegiaoWesternPacific_Wt6 + RegiaoWesternPacific_Wt7 + RegiaoWesternPacific_Wt8 + RegiaoWesternPacific_Wt9 + RegiaoWesternPacific_Wt10 + RegiaoWesternPacific_Wt11 + mediasBR_formatodefasagem, Brasil_Bt ~ 1, aToEnter, aToLeave)

# Model LASSO 5 
library(glmnet)
treinox=as.matrix(matriz_ncasos_defasagens_mediasBR[,-13])
treinoy=as.matrix(matriz_ncasos_defasagens_mediasBR[,13])
fit = glmnet(x=treinox, y=treinoy, alpha=1, nlambda=100)
coefs.ajuste = coef(fit)
coefs.ajuste[which(abs(coefs.ajuste[,20])>0.0000000001),20] # Model 5 variables Lasso. Vemos que ele seleciona novamente as vari??veis: RegiaoAmericaNorte_At4,Brasil_Bt1,RegiaoEuropeia_Et2 e RegiaoEuropeia_Et3. O que muda ?? que ao inv??s de selecionar RegiaoAmericaCentral_Ct3 ele seleciona mediasBR_formatodefasagem (vari??vel que representa as m??dias dos Brasil nos diferentes 12 meses)
##########################

# Prediction (incidence data only)
# Prediction 11 steps ahead:

# Matrices:

# Creating matrix_pred: Nessa matriz vamos preencher os dados de 2019 de acordo com a m??dia do m??s nos anos anteriores (exemplo, o n??mero de casos de janeiro de 2019 da europa ser?? a m??dia do meses de janeiro da europa dos anos anteriores). Al??m disso vamos colocar os dados existentes de 2018 (que ser??o necess??rios por causa das defasagens), por exemplo, na coluna da vari??vel "Brasil_Bt1" teremos as 11 linhas indo desde dezembro de 2018 at?? outubro de 2010 (pois ?? apenas 1 defasagem), j?? a vari??vel "Brasil_Bt2" teremos as 11 linhas indo desde novembro de 2018 at?? setembro de 2010 (pois s??o 2 defasagens) e assim por diante.
matrix_pred_mediasBR=matrix(ncol=10, nrow=11) # A mesma ideia da matrix_pred, mas dessa vez tem uma vari??vel a mais, a "mediasBR_formatodefasagem"
colnames(matrix_pred_mediasBR)=c("Brasil_Bt1","RegiaoEuropeia_Et1","Brasil_Bt2","RegiaoEuropeia_Et2","Brasil_Bt3","RegiaoEuropeia_Et3","RegiaoAmericaCentral_Ct3","RegiaoEuropeia_Et4","RegiaoAmericaNorte_At4","mediasBR_formatodefasagem")
matrix_pred_mediasBR[,"mediasBR_formatodefasagem"]=c(111.3,133.9,333.3,551.5,559,559.8,348.3,168.5,142.6,127,85.90909) #m??dia dos meses de janeiro at?? novembro do n??mero de casos do Brasil
matrix_pred_mediasBR[,"Brasil_Bt1"]=matrix_pred[,"Brasil_Bt1"]
matrix_pred_mediasBR[,"Brasil_Bt2"]=matrix_pred[,"Brasil_Bt2"]
matrix_pred_mediasBR[,"Brasil_Bt3"]=matrix_pred[,"Brasil_Bt3"]
matrix_pred_mediasBR[,"RegiaoEuropeia_Et1"]=matrix_pred[,"RegiaoEuropeia_Et1"]
matrix_pred_mediasBR[,"RegiaoEuropeia_Et2"]=matrix_pred[,"RegiaoEuropeia_Et2"]
matrix_pred_mediasBR[,"RegiaoEuropeia_Et3"]=matrix_pred[,"RegiaoEuropeia_Et3"]
matrix_pred_mediasBR[,"RegiaoEuropeia_Et4"]=matrix_pred[,"RegiaoEuropeia_Et4"]
matrix_pred_mediasBR[,"RegiaoAmericaCentral_Ct3"]=matrix_pred[,"RegiaoAmericaCentral_Ct3"]
matrix_pred_mediasBR[,"RegiaoAmericaNorte_At4"]=matrix_pred[,"RegiaoAmericaNorte_At4"]
matrix_pred_mediasBR

# Creating matrix_pred: Nessa matriz vamos preencher os dados de 2019 de acordo com a m??dia do m??s nos anos anteriores (exemplo, o n??mero de casos de janeiro de 2019 da europa ser?? a m??dia do meses de janeiro da europa dos anos anteriores). Al??m disso vamos colocar os dados existentes de 2018 (que ser??o necess??rios por causa das defasagens), por exemplo, na coluna da vari??vel "Brasil_Bt1" teremos as 11 linhas indo desde dezembro de 2018 at?? outubro de 2010 (pois ?? apenas 1 defasagem), j?? a vari??vel "Brasil_Bt2" teremos as 11 linhas indo desde novembro de 2018 at?? setembro de 2010 (pois s??o 2 defasagens) e assim por diante.
matrix_pred_dummy=matrix(ncol=14, nrow=11) # A mesma ideia da matrix_pred, mas dessa vez tem vari??veis categ??ricas
colnames(matrix_pred_dummy)=c("Brasil_Bt1","RegiaoEuropeia_Et1","RegiaoAmericaCentral_Ct1","Brasil_Bt2","RegiaoEuropeia_Et2","RegiaoAmericaNorte_At2","Brasil_Bt3","RegiaoEuropeia_Et3","RegiaoAmericaCentral_Ct3","RegiaoEuropeia_Et4","RegiaoAmericaNorte_At4","RegiaoAmericaCentral_Ct4","fevereiro","agosto")
matrix_pred_dummy[,"fevereiro"]=c(0,1,0,0,0,0,0,0,0,0,0)
matrix_pred_dummy[,"agosto"]=c(0,0,0,0,0,0,0,1,0,0,0)
matrix_pred_dummy[,"Brasil_Bt1"]=matrix_pred[,"Brasil_Bt1"]
matrix_pred_dummy[,"Brasil_Bt2"]=matrix_pred[,"Brasil_Bt2"]
matrix_pred_dummy[,"Brasil_Bt3"]=matrix_pred[,"Brasil_Bt3"]
matrix_pred_dummy[,"RegiaoEuropeia_Et1"]=matrix_pred[,"RegiaoEuropeia_Et1"]
matrix_pred_dummy[,"RegiaoEuropeia_Et2"]=matrix_pred[,"RegiaoEuropeia_Et2"]
matrix_pred_dummy[,"RegiaoEuropeia_Et3"]=matrix_pred[,"RegiaoEuropeia_Et3"]
matrix_pred_dummy[,"RegiaoEuropeia_Et4"]=matrix_pred[,"RegiaoEuropeia_Et4"]
matrix_pred_dummy[,"RegiaoAmericaCentral_Ct3"]=matrix_pred[,"RegiaoAmericaCentral_Ct3"]
matrix_pred_dummy[,"RegiaoAmericaNorte_At2"]=matrix_pred[,"RegiaoAmericaNorte_At2"]
matrix_pred_dummy[,"RegiaoAmericaNorte_At4"]=matrix_pred[,"RegiaoAmericaNorte_At4"]
matrix_pred_dummy[1,"RegiaoAmericaCentral_Ct1"]=matriz_ncont_analise["RegiaoAmericaCentral",123] #dezembro de 2018 da Regiao da America Central
matrix_pred_dummy[1:4,"RegiaoAmericaCentral_Ct4"]=matriz_ncont_analise["RegiaoAmericaCentral",120:123] # setembro até dezembro de 2018 da Regiao da America Central

# Agora vamos preencher os dados de 2019 de acordo com a m??dia do m??s nos anos anteriores:
matrix_pred_dummy[2:11,"RegiaoAmericaCentral_Ct1"]=c(medias_janeiro["RegiaoAmericaCentral"],medias_fevereiro["RegiaoAmericaCentral"],medias_marco["RegiaoAmericaCentral"],medias_abril["RegiaoAmericaCentral"],medias_maio["RegiaoAmericaCentral"],medias_junho["RegiaoAmericaCentral"],medias_julho["RegiaoAmericaCentral"],medias_agosto["RegiaoAmericaCentral"],medias_setembro["RegiaoAmericaCentral"],medias_outubro["RegiaoAmericaCentral"])
matrix_pred_dummy[5:11,"RegiaoAmericaCentral_Ct4"]=c(medias_janeiro["RegiaoAmericaCentral"],medias_fevereiro["RegiaoAmericaCentral"],medias_marco["RegiaoAmericaCentral"],medias_abril["RegiaoAmericaCentral"],medias_maio["RegiaoAmericaCentral"],medias_junho["RegiaoAmericaCentral"],medias_julho["RegiaoAmericaCentral"])
matrix_pred_dummy

# Creating matrix_pred_mediasBR_step (ou seja, matrix_pred com os dados do modelo Stepwise)
matrix_pred_mediasBR_step=matrix(nrow=11,ncol=13)
matrix_pred_mediasBR_step[,1:12]=cbind(matrix_pred_mediasBR,matrix_pred[,"RegiaoAmericaNorte_At2"],matrix_pred_dummy[,"RegiaoAmericaCentral_Ct1"])
colnames(matrix_pred_mediasBR_step)=c("Brasil_Bt1","RegiaoEuropeia_Et1","Brasil_Bt2","RegiaoEuropeia_Et2","Brasil_Bt3","RegiaoEuropeia_Et3","RegiaoAmericaCentral_Ct3","RegiaoEuropeia_Et4","RegiaoAmericaNorte_At4","mediasBR_formatodefasagem","RegiaoAmericaNorte_At2","RegiaoAmericaCentral_Ct1","RegiaoEuropeia_Et8")
# Known 2018 data
matrix_pred_mediasBR_step[1:8,"RegiaoEuropeia_Et8"]=matriz_ncont_analise["RegiaoEuropeia",116:123] # maio até dezembro de 2018 da Regiao Europeia
# 2019 unknown data and their averages
matrix_pred_mediasBR_step[9:11,"RegiaoEuropeia_Et8"]=c(medias_janeiro["RegiaoEuropeia"],medias_fevereiro["RegiaoEuropeia"],medias_marco["RegiaoEuropeia"])
matrix_pred_mediasBR_step

# LASSO 5 Model:
coefs.ajuste[which(abs(coefs.ajuste[,20])>0.0000000001),20] # Modelo de 5 vari??veis usando Lasso. Vemos que ele seleciona novamente as vari??veis: RegiaoAmericaNorte_At4,Brasil_Bt1,RegiaoEuropeia_Et2 e RegiaoEuropeia_Et3. O que muda ?? que ao inv??s de selecionar RegiaoAmericaCentral_Ct3 ele seleciona mediasBR_formatodefasagem (vari??vel que representa as m??dias dos Brasil nos diferentes 12 meses)
yhat_lasso5_mediasBR=vector()
for (i in 1:11){
  yhat_lasso5_mediasBR[i]=60.8213391570+(0.0143072450*matrix_pred_mediasBR[i,"mediasBR_formatodefasagem"]) + (0.0001854181*matrix_pred_mediasBR[i,"RegiaoAmericaNorte_At4"]) + (0.5284269137*matrix_pred_mediasBR[i,"Brasil_Bt1"]) + (0.0054782468*matrix_pred_mediasBR[i,"RegiaoEuropeia_Et2"]) + (0.0024194189*matrix_pred_mediasBR[i,"RegiaoEuropeia_Et3"])
}

# Stepwise 1 Model:
aToEnter <- 0.10
aToLeave <- 0.10
result <- stepwise(Brasil_Bt ~ 1 + Brasil_Bt1 + Brasil_Bt2 + Brasil_Bt3 + Brasil_Bt4 + Brasil_Bt5 + Brasil_Bt6 + Brasil_Bt7 + Brasil_Bt8 + Brasil_Bt9 + Brasil_Bt10 + Brasil_Bt11 + RegiaoAmericaNorte_At1 + RegiaoAmericaNorte_At2 + RegiaoAmericaNorte_At3 + RegiaoAmericaNorte_At4 + RegiaoAmericaNorte_At5 + RegiaoAmericaNorte_At6 + RegiaoAmericaNorte_At7 + RegiaoAmericaNorte_At8 + RegiaoAmericaNorte_At9 + RegiaoAmericaNorte_At10 + RegiaoAmericaNorte_At11 + RegiaoAmericaSul_St1 + RegiaoAmericaSul_St2 + RegiaoAmericaSul_St3 + RegiaoAmericaSul_St4 + RegiaoAmericaSul_St5 + RegiaoAmericaSul_St6 + RegiaoAmericaSul_St7 + RegiaoAmericaSul_St8 + RegiaoAmericaSul_St9 + RegiaoAmericaSul_St10 + RegiaoAmericaSul_St11 + RegiaoAmericaCentral_Ct1 + RegiaoAmericaCentral_Ct2 + RegiaoAmericaCentral_Ct3 + RegiaoAmericaCentral_Ct4 + RegiaoAmericaCentral_Ct5 + RegiaoAmericaCentral_Ct6 + RegiaoAmericaCentral_Ct7 + RegiaoAmericaCentral_Ct8 + RegiaoAmericaCentral_Ct9 + RegiaoAmericaCentral_Ct10 + RegiaoAmericaCentral_Ct11 + RegiaoEuropeia_Et1 + RegiaoEuropeia_Et2 + RegiaoEuropeia_Et3 + RegiaoEuropeia_Et4 + RegiaoEuropeia_Et5 + RegiaoEuropeia_Et6 + RegiaoEuropeia_Et7 + RegiaoEuropeia_Et8 + RegiaoEuropeia_Et9 + RegiaoEuropeia_Et10 + RegiaoEuropeia_Et11 + RegiaoSouthAsia_st1 + RegiaoSouthAsia_st2 + RegiaoSouthAsia_st3 + RegiaoSouthAsia_st4 + RegiaoSouthAsia_st5 + RegiaoSouthAsia_st6 + RegiaoSouthAsia_st7 + RegiaoSouthAsia_st8 + RegiaoSouthAsia_st9 + RegiaoSouthAsia_st10 + RegiaoSouthAsia_st11 + RegiaoWesternPacific_Wt1 + RegiaoWesternPacific_Wt2 + RegiaoWesternPacific_Wt3 + RegiaoWesternPacific_Wt4 + RegiaoWesternPacific_Wt5 + RegiaoWesternPacific_Wt6 + RegiaoWesternPacific_Wt7 + RegiaoWesternPacific_Wt8 + RegiaoWesternPacific_Wt9 + RegiaoWesternPacific_Wt10 + RegiaoWesternPacific_Wt11 + mediasBR_formatodefasagem, Brasil_Bt ~ 1, aToEnter, aToLeave)
yhat_stepwise1_mediasBR=vector()
for (i in 1:11){
  yhat_stepwise1_mediasBR[i]=-0.843385381 + (0.847002309*matrix_pred_mediasBR_step[i,"Brasil_Bt1"]) + (0.011173204*matrix_pred_mediasBR_step[i,"RegiaoEuropeia_Et2"]) + (-0.342484879*matrix_pred_mediasBR_step[i,"Brasil_Bt2"]) + (-0.024900896*matrix_pred_mediasBR_step[i,"RegiaoAmericaCentral_Ct3"]) + (0.006418213*matrix_pred_mediasBR_step[i,"RegiaoEuropeia_Et4"]) + (0.015150757*matrix_pred_mediasBR_step[i,"RegiaoAmericaCentral_Ct1"]) + (-0.004137434*matrix_pred_mediasBR_step[i,"RegiaoAmericaNorte_At2"]) + (0.005347344*matrix_pred_mediasBR_step[i,"RegiaoEuropeia_Et1"]) + (0.002548655*matrix_pred_mediasBR_step[i,"RegiaoEuropeia_Et8"])
}

# EQM:
EQM_Lasso5 = EQM_Stepwise1 = numeric(11)
for(i in 1:11){
  EQM_Lasso5[i] = mean((yhat_lasso5_mediasBR[1:i]-matriz_ncont_2019["Brasil",1:i])^2)  
  EQM_Stepwise1[i] = mean((yhat_stepwise1_mediasBR[1:i]-matriz_ncont_2019["Brasil",1:i])^2)  
}
EQM_Lasso5;EQM_Stepwise1 # EQM values in all steps and in all models
#############################

# Prediction (incidence data only)
# Prediction 1 steps ahead:

# In-sample Models
preditos_lasso5_insample=predict(fit,s=50.390,type="response",newx=treinox) # Model LASSO 5

# Stepwise 1 Model
aToEnter <- 0.10
aToLeave <- 0.10
result1 <- stepwise(Brasil_Bt ~ 1 + Brasil_Bt1 + Brasil_Bt2 + Brasil_Bt3 + Brasil_Bt4 + Brasil_Bt5 + Brasil_Bt6 + Brasil_Bt7 + Brasil_Bt8 + Brasil_Bt9 + Brasil_Bt10 + Brasil_Bt11 + RegiaoAmericaNorte_At1 + RegiaoAmericaNorte_At2 + RegiaoAmericaNorte_At3 + RegiaoAmericaNorte_At4 + RegiaoAmericaNorte_At5 + RegiaoAmericaNorte_At6 + RegiaoAmericaNorte_At7 + RegiaoAmericaNorte_At8 + RegiaoAmericaNorte_At9 + RegiaoAmericaNorte_At10 + RegiaoAmericaNorte_At11 + RegiaoAmericaSul_St1 + RegiaoAmericaSul_St2 + RegiaoAmericaSul_St3 + RegiaoAmericaSul_St4 + RegiaoAmericaSul_St5 + RegiaoAmericaSul_St6 + RegiaoAmericaSul_St7 + RegiaoAmericaSul_St8 + RegiaoAmericaSul_St9 + RegiaoAmericaSul_St10 + RegiaoAmericaSul_St11 + RegiaoAmericaCentral_Ct1 + RegiaoAmericaCentral_Ct2 + RegiaoAmericaCentral_Ct3 + RegiaoAmericaCentral_Ct4 + RegiaoAmericaCentral_Ct5 + RegiaoAmericaCentral_Ct6 + RegiaoAmericaCentral_Ct7 + RegiaoAmericaCentral_Ct8 + RegiaoAmericaCentral_Ct9 + RegiaoAmericaCentral_Ct10 + RegiaoAmericaCentral_Ct11 + RegiaoEuropeia_Et1 + RegiaoEuropeia_Et2 + RegiaoEuropeia_Et3 + RegiaoEuropeia_Et4 + RegiaoEuropeia_Et5 + RegiaoEuropeia_Et6 + RegiaoEuropeia_Et7 + RegiaoEuropeia_Et8 + RegiaoEuropeia_Et9 + RegiaoEuropeia_Et10 + RegiaoEuropeia_Et11 + RegiaoSouthAsia_st1 + RegiaoSouthAsia_st2 + RegiaoSouthAsia_st3 + RegiaoSouthAsia_st4 + RegiaoSouthAsia_st5 + RegiaoSouthAsia_st6 + RegiaoSouthAsia_st7 + RegiaoSouthAsia_st8 + RegiaoSouthAsia_st9 + RegiaoSouthAsia_st10 + RegiaoSouthAsia_st11 + RegiaoWesternPacific_Wt1 + RegiaoWesternPacific_Wt2 + RegiaoWesternPacific_Wt3 + RegiaoWesternPacific_Wt4 + RegiaoWesternPacific_Wt5 + RegiaoWesternPacific_Wt6 + RegiaoWesternPacific_Wt7 + RegiaoWesternPacific_Wt8 + RegiaoWesternPacific_Wt9 + RegiaoWesternPacific_Wt10 + RegiaoWesternPacific_Wt11 + mediasBR_formatodefasagem, Brasil_Bt ~ 1, aToEnter, aToLeave)
treinox_stepwise=as.data.frame(treinox)
preditos_stepwise1_insample=predict(result1,newdata=treinox_stepwise,type="response") # type="response" se refere a fazer a predi????o dentro da amostra, result1 ?? o modelo stepwise ajustado, e newdata s??o as vari??veis explicativas da amostra (2008 at?? 2018)

# EQM In-sample
mean((predict(fit,s=50.390,type="response",newx=treinox) - treinoy)^2) # EQM Model lasso p=5
mean((predict(result1,newdata=treinox_stepwise,type="response") - treinoy)^2) # EQM Model Stepwise 1

# Mean absolute percentage error (MAPE) In-sample
(sum((abs((treinoy-preditos_lasso5_insample)/treinoy)))/111)*100 # mape model lasso com p=5
(sum((abs((treinoy-preditos_stepwise1_insample)/treinoy)))/111)*100 # mape model Stepwise 1
######

# Models Out-of-sample

matriz_ncasos_defasagens_mediasBR_2019=matrix(nrow=11,ncol=79)
colnames(matriz_ncasos_defasagens_mediasBR_2019)=colnames(matriz_ncasos_defasagens_mediasBR)
# Preenchendo matriz nova com dados de 2019:
matriz_ncasos_defasagens_mediasBR_2019[,"mediasBR_formatodefasagem"]=c(111.3,133.9,333.3,551.5,559,559.8,348.3,168.5,142.6,127,85.90909)
matriz_ncasos_defasagens_mediasBR_2019[,"Brasil_Bt"]=t(matriz_ncont_2019["Brasil",])
for(i in 1:10){ 
  matriz_ncasos_defasagens_mediasBR_2019[(i+1):11,paste("Brasil_Bt",i,sep="")]=t(matriz_ncont_2019["Brasil",1:(11-i)])
  matriz_ncasos_defasagens_mediasBR_2019[(i+1):11,paste("RegiaoAmericaNorte_At",i,sep="")]=t(matriz_ncont_2019["RegiaoAmericaNorte",1:(11-i)])
  matriz_ncasos_defasagens_mediasBR_2019[(i+1):11,paste("RegiaoAmericaSul_St",i,sep="")]=t(matriz_ncont_2019["RegiaoAmericaSul",1:(11-i)])
  matriz_ncasos_defasagens_mediasBR_2019[(i+1):11,paste("RegiaoAmericaCentral_Ct",i,sep="")]=t(matriz_ncont_2019["RegiaoAmericaCentral",1:(11-i)])
  matriz_ncasos_defasagens_mediasBR_2019[(i+1):11,paste("RegiaoEuropeia_Et",i,sep="")]=t(matriz_ncont_2019["RegiaoEuropeia",1:(11-i)])
  matriz_ncasos_defasagens_mediasBR_2019[(i+1):11,paste("RegiaoSouthAsia_st",i,sep="")]=t(matriz_ncont_2019["RegiaoSouthAsia",1:(11-i)])
  matriz_ncasos_defasagens_mediasBR_2019[(i+1):11,paste("RegiaoWesternPacific_Wt",i,sep="")]=t(matriz_ncont_2019["RegiaoWesternPacific",1:(11-i)])
}

# Preenchendo matriz nova com dados de 2018:
for(i in 1:11){ 
  matriz_ncasos_defasagens_mediasBR_2019[1:i,paste("Brasil_Bt",i,sep="")]=t(matriz_ncont_analise["Brasil",(124-i):123])
  matriz_ncasos_defasagens_mediasBR_2019[1:i,paste("RegiaoAmericaNorte_At",i,sep="")]=t(matriz_ncont_analise["RegiaoAmericaNorte",(124-i):123])
  matriz_ncasos_defasagens_mediasBR_2019[1:i,paste("RegiaoAmericaSul_St",i,sep="")]=t(matriz_ncont_analise["RegiaoAmericaSul",(124-i):123])
  matriz_ncasos_defasagens_mediasBR_2019[1:i,paste("RegiaoAmericaCentral_Ct",i,sep="")]=t(matriz_ncont_analise["RegiaoAmericaCentral",(124-i):123])
  matriz_ncasos_defasagens_mediasBR_2019[1:i,paste("RegiaoEuropeia_Et",i,sep="")]=t(matriz_ncont_analise["RegiaoEuropeia",(124-i):123])
  matriz_ncasos_defasagens_mediasBR_2019[1:i,paste("RegiaoSouthAsia_st",i,sep="")]=t(matriz_ncont_analise["RegiaoSouthAsia",(124-i):123])
  matriz_ncasos_defasagens_mediasBR_2019[1:i,paste("RegiaoWesternPacific_Wt",i,sep="")]=t(matriz_ncont_analise["RegiaoWesternPacific",(124-i):123])
}
matriz_ncasos_defasagens_mediasBR_2019 #matriz com as 11 defasagens com os novos dados. Ou, seja, para o Brasil_Bt (e medias_BR) os novos dados s??o de janeiro de 2019 at?? novembro de 2019, j?? para o Brasil_Bt1 os novos dados s??o de dezembro de 2018 at?? outubro de 2019, j?? para o Brasil_Bt2 os novos dados s??o de novembro de 2018 at?? setembro de 2019,...,j?? para o Brasil_Bt11 os novos dados s??o de fevereiro de 2018 at?? dezembro de 2018.

# Putting the matrices together:
matriz_ncasos_defasagens_mediasBR_2008_2019=rbind(matriz_ncasos_defasagens_mediasBR,matriz_ncasos_defasagens_mediasBR_2019) # Juntando as matrizes matriz_ncasos_defasagens_mediasBR (tem dados com defasagens desde outubro de 2008 at?? dezembro de 2018) e matriz_ncasos_defasagens_mediasBR_2019 (tem dados com defasagens desde janeiro de 2019 at?? novembro de 2019)
rownames(matriz_ncasos_defasagens_mediasBR_2008_2019)=c(1:122)
View(matriz_ncasos_defasagens_mediasBR_2008_2019)

# Models (remembering):

# Stepwise 1
matriz_ncasos_defasagens_mediasBR=as.data.frame(matriz_ncasos_defasagens_mediasBR)
attach(matriz_ncasos_defasagens_mediasBR)
aToEnter <- 0.10
aToLeave <- 0.10
result <- stepwise(Brasil_Bt ~ 1 + Brasil_Bt1 + Brasil_Bt2 + Brasil_Bt3 + Brasil_Bt4 + Brasil_Bt5 + Brasil_Bt6 + Brasil_Bt7 + Brasil_Bt8 + Brasil_Bt9 + Brasil_Bt10 + Brasil_Bt11 + RegiaoAmericaNorte_At1 + RegiaoAmericaNorte_At2 + RegiaoAmericaNorte_At3 + RegiaoAmericaNorte_At4 + RegiaoAmericaNorte_At5 + RegiaoAmericaNorte_At6 + RegiaoAmericaNorte_At7 + RegiaoAmericaNorte_At8 + RegiaoAmericaNorte_At9 + RegiaoAmericaNorte_At10 + RegiaoAmericaNorte_At11 + RegiaoAmericaSul_St1 + RegiaoAmericaSul_St2 + RegiaoAmericaSul_St3 + RegiaoAmericaSul_St4 + RegiaoAmericaSul_St5 + RegiaoAmericaSul_St6 + RegiaoAmericaSul_St7 + RegiaoAmericaSul_St8 + RegiaoAmericaSul_St9 + RegiaoAmericaSul_St10 + RegiaoAmericaSul_St11 + RegiaoAmericaCentral_Ct1 + RegiaoAmericaCentral_Ct2 + RegiaoAmericaCentral_Ct3 + RegiaoAmericaCentral_Ct4 + RegiaoAmericaCentral_Ct5 + RegiaoAmericaCentral_Ct6 + RegiaoAmericaCentral_Ct7 + RegiaoAmericaCentral_Ct8 + RegiaoAmericaCentral_Ct9 + RegiaoAmericaCentral_Ct10 + RegiaoAmericaCentral_Ct11 + RegiaoEuropeia_Et1 + RegiaoEuropeia_Et2 + RegiaoEuropeia_Et3 + RegiaoEuropeia_Et4 + RegiaoEuropeia_Et5 + RegiaoEuropeia_Et6 + RegiaoEuropeia_Et7 + RegiaoEuropeia_Et8 + RegiaoEuropeia_Et9 + RegiaoEuropeia_Et10 + RegiaoEuropeia_Et11 + RegiaoSouthAsia_st1 + RegiaoSouthAsia_st2 + RegiaoSouthAsia_st3 + RegiaoSouthAsia_st4 + RegiaoSouthAsia_st5 + RegiaoSouthAsia_st6 + RegiaoSouthAsia_st7 + RegiaoSouthAsia_st8 + RegiaoSouthAsia_st9 + RegiaoSouthAsia_st10 + RegiaoSouthAsia_st11 + RegiaoWesternPacific_Wt1 + RegiaoWesternPacific_Wt2 + RegiaoWesternPacific_Wt3 + RegiaoWesternPacific_Wt4 + RegiaoWesternPacific_Wt5 + RegiaoWesternPacific_Wt6 + RegiaoWesternPacific_Wt7 + RegiaoWesternPacific_Wt8 + RegiaoWesternPacific_Wt9 + RegiaoWesternPacific_Wt10 + RegiaoWesternPacific_Wt11 + mediasBR_formatodefasagem, Brasil_Bt ~ 1, aToEnter, aToLeave)

# Lasso p=5 
library(glmnet)
treinox=as.matrix(matriz_ncasos_defasagens_mediasBR[,-13]) #vari??veis independentes at?? a defasagem 11
treinoy=as.matrix(matriz_ncasos_defasagens_mediasBR[,13])
fit = glmnet(x=treinox, y=treinoy, alpha=1, nlambda=100)
coefs.ajuste = coef(fit)
coefs.ajuste[which(abs(coefs.ajuste[,20])>0.0000000001),20] # Modelo de 5 vari??veis usando Lasso. Vemos que ele seleciona novamente as vari??veis: RegiaoAmericaNorte_At4,Brasil_Bt1,RegiaoEuropeia_Et2 e RegiaoEuropeia_Et3. O que muda ?? que ao inv??s de selecionar RegiaoAmericaCentral_Ct3 ele seleciona mediasBR_formatodefasagem (vari??vel que representa as m??dias dos Brasil nos diferentes 12 meses)

treinox_2019=as.matrix(matriz_ncasos_defasagens_mediasBR_2008_2019[,-13])
treinoy_2019=as.matrix(matriz_ncasos_defasagens_mediasBR_2008_2019[,13])

# Models Out-of-sample
preditos_lasso5_insample2019=predict(fit,s=50.390,type="response",newx=treinox_2019) # LASSO 5

# Model Stepwise 1:
aToEnter <- 0.10
aToLeave <- 0.10
result1 <- stepwise(Brasil_Bt ~ 1 + Brasil_Bt1 + Brasil_Bt2 + Brasil_Bt3 + Brasil_Bt4 + Brasil_Bt5 + Brasil_Bt6 + Brasil_Bt7 + Brasil_Bt8 + Brasil_Bt9 + Brasil_Bt10 + Brasil_Bt11 + RegiaoAmericaNorte_At1 + RegiaoAmericaNorte_At2 + RegiaoAmericaNorte_At3 + RegiaoAmericaNorte_At4 + RegiaoAmericaNorte_At5 + RegiaoAmericaNorte_At6 + RegiaoAmericaNorte_At7 + RegiaoAmericaNorte_At8 + RegiaoAmericaNorte_At9 + RegiaoAmericaNorte_At10 + RegiaoAmericaNorte_At11 + RegiaoAmericaSul_St1 + RegiaoAmericaSul_St2 + RegiaoAmericaSul_St3 + RegiaoAmericaSul_St4 + RegiaoAmericaSul_St5 + RegiaoAmericaSul_St6 + RegiaoAmericaSul_St7 + RegiaoAmericaSul_St8 + RegiaoAmericaSul_St9 + RegiaoAmericaSul_St10 + RegiaoAmericaSul_St11 + RegiaoAmericaCentral_Ct1 + RegiaoAmericaCentral_Ct2 + RegiaoAmericaCentral_Ct3 + RegiaoAmericaCentral_Ct4 + RegiaoAmericaCentral_Ct5 + RegiaoAmericaCentral_Ct6 + RegiaoAmericaCentral_Ct7 + RegiaoAmericaCentral_Ct8 + RegiaoAmericaCentral_Ct9 + RegiaoAmericaCentral_Ct10 + RegiaoAmericaCentral_Ct11 + RegiaoEuropeia_Et1 + RegiaoEuropeia_Et2 + RegiaoEuropeia_Et3 + RegiaoEuropeia_Et4 + RegiaoEuropeia_Et5 + RegiaoEuropeia_Et6 + RegiaoEuropeia_Et7 + RegiaoEuropeia_Et8 + RegiaoEuropeia_Et9 + RegiaoEuropeia_Et10 + RegiaoEuropeia_Et11 + RegiaoSouthAsia_st1 + RegiaoSouthAsia_st2 + RegiaoSouthAsia_st3 + RegiaoSouthAsia_st4 + RegiaoSouthAsia_st5 + RegiaoSouthAsia_st6 + RegiaoSouthAsia_st7 + RegiaoSouthAsia_st8 + RegiaoSouthAsia_st9 + RegiaoSouthAsia_st10 + RegiaoSouthAsia_st11 + RegiaoWesternPacific_Wt1 + RegiaoWesternPacific_Wt2 + RegiaoWesternPacific_Wt3 + RegiaoWesternPacific_Wt4 + RegiaoWesternPacific_Wt5 + RegiaoWesternPacific_Wt6 + RegiaoWesternPacific_Wt7 + RegiaoWesternPacific_Wt8 + RegiaoWesternPacific_Wt9 + RegiaoWesternPacific_Wt10 + RegiaoWesternPacific_Wt11 + mediasBR_formatodefasagem, Brasil_Bt ~ 1, aToEnter, aToLeave)
treinox_stepwise_2019=as.data.frame(as.matrix(matriz_ncasos_defasagens_mediasBR_2008_2019[,-13]))
preditos_stepwise1_insample_2019=predict(result1,newdata=treinox_stepwise_2019,type="response")

# EQM Out-Sample of each model:
mean((predict(fit,s=50.390,type="response",newx=treinox_2019)[112:122] - treinoy_2019[112:122])^2) # EQM Model lasso p=5
mean((predict(result1,newdata=treinox_stepwise_2019,type="response")[112:122] - treinoy_2019[112:122])^2) # EQM Model Stepwise 1

# Mean absolute percentage error (MAPE) Out-Sample
(sum((abs((treinoy_2019[112:122]-preditos_lasso5_insample2019[112:122])/treinoy_2019[112:122])))/11)*100 # mape model lasso p=5
(sum((abs((treinoy_2019[112:122]-preditos_stepwise1_insample_2019[112:122])/treinoy_2019[112:122])))/11)*100 # mape model Stepwise 1
##################################

# Prediction charts 11 steps ahead (no genetics):

valores_verdadeiros=treinoy_2019 # valores verdadeiros do Brasil_Bt de outubro de 2009 at?? novembro de 2019
valores_verdadeiros_2015=valores_verdadeiros[64:122,1]
valores_verdadeiros_2018=valores_verdadeiros_2015[44:59] # dados de agosto de 2018 at?? novembro de 2019

# Gr??fico que tem valores verdadeiros de outubro de 2009 at?? novembro de 2019 (valores_verdadeiros), e valores preditos de janeiro de 2019 at?? novembro de 2019
ts<- ts(valores_verdadeiros, start=c(2009,10), end=c(2019,11), frequency=12)
plot.ts(window(ts, start=start(ts), end=2020), col="black", xlim=range(time(ts)), ylim=range(ts),ylab='Number of positive cases',xlab='Years')
par(new=TRUE)
ts2<- ts(yhat_lasso5_mediasBR, start=c(2019,1), end=c(2019,11), frequency=12)
plot.ts(window(ts2, start=2019), col="firebrick1", axes=F, xlab="", ylab="", xlim=range(time(ts)), ylim=range(ts),lwd=2)
par(new=TRUE)
ts4<- ts(yhat_stepwise1_mediasBR, start=c(2019,1), end=c(2019,11), frequency=12)
plot.ts(window(ts4, start=2019), col="chartreuse3", axes=F, xlab="", ylab="", xlim=range(time(ts)), ylim=range(ts),lwd=2)
par(new=TRUE)
abline(v=2019,lty=2)
legend("topleft",
       bty="n",
       c("LASSO 5","Stepwise 1"),
       lty=c(1, 1),
       lwd=c(2, 2),
       col=c("firebrick1",
             "chartreuse3"),cex=0.9,inset=-.015)
title("Forecasts of the different models")

# Gr??fico que tem valores verdadeiros de abril de 2009 at?? novembro de 2019 (valores_verdadeiros_genetica), e valores preditos de janeiro de 2019 at?? agosto de 2019
ts<- ts(valores_verdadeiros_2018, start=c(2018,8), end=c(2019,11), frequency=12)
plot.ts(window(ts, start=start(ts), end=2020), col="black", xlim=range(time(ts)), ylim=range(ts),ylab='Number of positive cases',xlab='Time',lwd=1.1)
par(new=TRUE)
ts2<- ts(yhat_lasso5_mediasBR, start=c(2019,1), end=c(2019,11), frequency=12)
plot.ts(window(ts2, start=2019), col="firebrick1", axes=F, xlab="", ylab="", xlim=range(time(ts)), ylim=range(ts),lwd=2)
par(new=TRUE)
ts4<- ts(yhat_stepwise1_mediasBR, start=c(2019,1), end=c(2019,11), frequency=12)
plot.ts(window(ts4, start=2019), col="chartreuse3", axes=F, xlab="", ylab="", xlim=range(time(ts)), ylim=range(ts),lwd=2)
plot.ts(window(ts5, start=2019), col="darkorchid2", axes=F, xlab="", ylab="", xlim=range(time(ts)), ylim=range(ts),lwd=1.6)
abline(v=2019,lty=2)
legend("topleft",
       bty="n",
       c("LASSO 5","Stepwise 1"),
       lty=c(1, 1),
       lwd=c(2, 2),
       col=c("firebrick1",
             "chartreuse3"),cex=0.9,inset=-.015)
title("Forecasts of the different models")
#########

# Prediction 11 steps forward (no genetics) - starting in early 2019 (January 2019)

valores_verdadeiros_2019=valores_verdadeiros_2018[6:16] # dados de janeiro de 2019 at?? novembro de 2019

valores_verdadeiros_2019=as.data.frame(valores_verdadeiros_2019)
yhat_lasso5_mediasBR=as.data.frame(yhat_lasso5_mediasBR)
yhat_stepwise1_mediasBR=as.data.frame(yhat_stepwise1_mediasBR)
dados_1passo_semgen=c(valores_verdadeiros_2019,yhat_lasso5_mediasBR,yhat_stepwise1_mediasBR)
dados_1passo_semgen=as.data.frame(dados_1passo_semgen)

colors <- c("Observed" = "black", "LASSO 5" = "firebrick1","Stepwise 1" = "chartreuse3")

ggplot(dados_1passo_semgen, aes(x = c(1:11))) +
  geom_line(aes(y = valores_verdadeiros_2019, color = "Observed"), size = 0.4) +
  geom_line(aes(y = yhat_lasso5_mediasBR, color = "LASSO 5"), size = 0.5) +
  geom_line(aes(y = yhat_stepwise1_mediasBR, color = "Stepwise 1"), size = 0.5) +
  labs(x = "2019", y = "Number of positive cases", title = "Out-of-sample forecasts",color = "Models") +
  scale_x_continuous(limit = c(1,11),
                     breaks = c(1,2,3,4,5,6,7,8,9,10,11),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov")) +
  scale_color_manual(values = colors)

#################

# Prediction charts 1 steps ahead (incidence data only):

# Prediction Lasso p=5
colnames(preditos_lasso5_insample2019)="preditos_lasso5_insample2019"
preditos_lasso5_insample2019=data.frame(preditos_lasso5_insample2019)
treinoy_2019=data.frame(treinoy_2019)

ggplot(preditos_lasso5_insample2019, aes(x=c(1:122))) +
  geom_line(aes(y=preditos_lasso5_insample2019), color = "firebrick1", size = 1)+
  geom_line(data = treinoy_2019, aes(x=c(1:122), y = treinoy_2019), color = "black", size=0.4) +
  labs(x = "Years", y = "Number of positive cases", title = "Predicted values (LASSO p=5)")+
  scale_x_continuous(limit = c(0,122),
                     breaks = c(4,16,28,40,52,64,76,88,100,112),
                     labels = c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019")) +
  geom_vline(xintercept = 112, linetype='dashed', color = "black", size=0.5)

# Prediction Stepwise 1
colnames(preditos_stepwise1_insample_2019)="preditos_stepwise1_insample_2019"
preditos_stepwise1_insample_2019=data.frame(preditos_stepwise1_insample_2019)

ggplot(preditos_stepwise1_insample_2019, aes(x=c(1:122))) +
  geom_line(aes(y=preditos_stepwise1_insample_2019), color = "chartreuse3", size = 1)+
  geom_line(data = treinoy_2019, aes(x=c(1:122), y = treinoy_2019), color = "black", size=0.4) +
  labs(x = "Years", y = "Number of positive cases", title = "Predicted values (Stepwise 1 Model)")+
  scale_x_continuous(limit = c(0,122),
                     breaks = c(4,16,28,40,52,64,76,88,100,112),
                     labels = c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019")) +
  geom_vline(xintercept = 112, linetype='dashed', color = "black", size=0.5)

##############################################################################################################################
################################## MODELING 2 ##############################################################################
#########################################################################################################################

# Data
library(readxl)
matriz_ncont_2019_genetica <- read_excel("matriz_ncont_2019_genetica.xlsx")
matriz_ncont_2019_genetica=as.matrix(t(matriz_ncont_2019_genetica))

matriz_ncasos_defasagens_genetica <- read_excel("matriz_ncasos_defasagens_genetica.xlsx")
matriz_ncasos_defasagens_genetica=as.matrix(matriz_ncasos_defasagens_genetica)
###################

# Models with Incidence and Genetics data

# Models LASSO 5 and 10 variables  (6 lags in the number of cases and 6 lags in genetics)
library(glmnet)
treinox=as.matrix(matriz_ncasos_defasagens_genetica[,-1]) #vari??veis independentes at?? a defasagem 6
treinoy=as.matrix(matriz_ncasos_defasagens_genetica[,1])
fit = glmnet(x=treinox, y=treinoy, alpha=1, nlambda=200)
coefs.ajuste = coef(fit)
coefs.ajuste[which(abs(coefs.ajuste[,40])>0.0000000001),40] # Model 5 variables Lasso (Brasil_Bt1,RegiaoEuropeia_Et2,RegiaoEuropeia_Et3,RegiaoAmericaNorte_At4,mediasBR)

# Stepwise 1
matriz_ncasos_defasagens_genetica=as.data.frame(matriz_ncasos_defasagens_genetica)
attach(matriz_ncasos_defasagens_genetica)
aToEnter <- 0.10
aToLeave <- 0.10
result <- stepwise(Brasil_Bt ~ 1 + Brasil_Bt1 + Brasil_Bt2 + Brasil_Bt3 + Brasil_Bt4 + Brasil_Bt5 + Brasil_Bt6 + RegiaoAmericaNorte_At1 + RegiaoAmericaNorte_At2 + RegiaoAmericaNorte_At3 + RegiaoAmericaNorte_At4 + RegiaoAmericaNorte_At5 + RegiaoAmericaNorte_At6 + RegiaoAmericaSul_St1 + RegiaoAmericaSul_St2 + RegiaoAmericaSul_St3 + RegiaoAmericaSul_St4 + RegiaoAmericaSul_St5 + RegiaoAmericaSul_St6 + RegiaoAmericaCentral_Ct1 + RegiaoAmericaCentral_Ct2 + RegiaoAmericaCentral_Ct3 + RegiaoAmericaCentral_Ct4 + RegiaoAmericaCentral_Ct5 + RegiaoAmericaCentral_Ct6 + RegiaoEuropeia_Et1 + RegiaoEuropeia_Et2 + RegiaoEuropeia_Et3 + RegiaoEuropeia_Et4 + RegiaoEuropeia_Et5 + RegiaoEuropeia_Et6 + RegiaoSouthAsia_st1 + RegiaoSouthAsia_st2 + RegiaoSouthAsia_st3 + RegiaoSouthAsia_st4 + RegiaoSouthAsia_st5 + RegiaoSouthAsia_st6 + RegiaoWesternPacific_Wt1 + RegiaoWesternPacific_Wt2 + RegiaoWesternPacific_Wt3 + RegiaoWesternPacific_Wt4 + RegiaoWesternPacific_Wt5 + RegiaoWesternPacific_Wt6 + mediasBR + AllH1_1 + AllH1_2 + AllH1_3 + AllH1_4 + AllH1_5 + AllH1_6 + AllH3_1 + AllH3_2 + AllH3_3 + AllH3_4 + AllH3_5 + AllH3_6 + NorthAmericaH1_1 + NorthAmericaH1_2 + NorthAmericaH1_3 + NorthAmericaH1_4 + NorthAmericaH1_5 + NorthAmericaH1_6 + NorthAmericaH3_1 + NorthAmericaH3_2 + NorthAmericaH3_3 + NorthAmericaH3_4 + NorthAmericaH3_5 + NorthAmericaH3_6 + AsiaH1_1 + AsiaH1_2 + AsiaH1_3 + AsiaH1_4 + AsiaH1_5 + AsiaH1_6 + AsiaH3_1+ AsiaH3_2 + AsiaH3_3 + AsiaH3_4 + AsiaH3_5 + AsiaH3_6, Brasil_Bt ~ 1, aToEnter, aToLeave)
#############################

# Prediction (incidence and genetics data)
# Prediction 1 step ahead:

# Matrices with number of positive cases and genetics data
matriz_ncont_analise_genetica # outubro de 2008 at?? dezembro de 2018
matriz_ncont_2019_genetica # janeiro de 2019 at?? setembro de 2019
matriz_ncasos_defasagens_genetica # matriz com 6 defasagens com dados de outubro de 2008 at?? dezembro de 2018 e o Brasil no tempo atual vai de abril de 2009 at?? dezembro de 2018.

# Creating lags matrix with 2019 genetics data
matriz_ncasos_defasagens_genetica_2019=matrix(nrow=8,ncol=80) # Nessa matriz os dados que est??o no tempo atual v??o de janeiro 2019 at?? agosto de 2019, os com 1 defasagem v??o de dezembro de 2018 at?? julho de 2019, os com 2 defasagens v??o de novembro de 2018 at?? junho de 2019, os com 3 defasagens v??o de outubro de 2018 at?? maio de 2019,..., os com 6 defasagens v??o de julho de 2018 at?? fevereiro de 2019.
colnames(matriz_ncasos_defasagens_genetica_2019)=colnames(matriz_ncasos_defasagens_genetica)
# Preenchendo matriz nova com dados de 2019:
matriz_ncasos_defasagens_genetica_2019[,"mediasBR"]=c(111.3,133.9,333.3,551.5,559,559.8,348.3,168.5) # dados de janeiro 2019 at?? agosto de 2019 (ou seja, a m??dia do Brasil no m??s de janeiro, no m??s de fevereiro,..., no m??s de agosto).
matriz_ncasos_defasagens_genetica_2019[,"Brasil_Bt"]=t(matriz_ncont_2019["Brasil",1:8]) # dados de janeiro 2019 at?? agosto de 2019

for(i in 1:6){ 
  matriz_ncasos_defasagens_genetica_2019[(i+1):8,paste("Brasil_Bt",i,sep="")]=t(matriz_ncont_2019["Brasil",1:(8-i)])
  matriz_ncasos_defasagens_genetica_2019[(i+1):8,paste("RegiaoAmericaNorte_At",i,sep="")]=t(matriz_ncont_2019["RegiaoAmericaNorte",1:(8-i)])
  matriz_ncasos_defasagens_genetica_2019[(i+1):8,paste("RegiaoAmericaSul_St",i,sep="")]=t(matriz_ncont_2019["RegiaoAmericaSul",1:(8-i)])
  matriz_ncasos_defasagens_genetica_2019[(i+1):8,paste("RegiaoAmericaCentral_Ct",i,sep="")]=t(matriz_ncont_2019["RegiaoAmericaCentral",1:(8-i)])
  matriz_ncasos_defasagens_genetica_2019[(i+1):8,paste("RegiaoEuropeia_Et",i,sep="")]=t(matriz_ncont_2019["RegiaoEuropeia",1:(8-i)])
  matriz_ncasos_defasagens_genetica_2019[(i+1):8,paste("RegiaoSouthAsia_st",i,sep="")]=t(matriz_ncont_2019["RegiaoSouthAsia",1:(8-i)])
  matriz_ncasos_defasagens_genetica_2019[(i+1):8,paste("RegiaoWesternPacific_Wt",i,sep="")]=t(matriz_ncont_2019["RegiaoWesternPacific",1:(8-i)])
  matriz_ncasos_defasagens_genetica_2019[(i+1):8,paste("AllH1_",i,sep="")]=t(matriz_ncont_2019_genetica["AllH1",1:(8-i)])
  matriz_ncasos_defasagens_genetica_2019[(i+1):8,paste("AllH3_",i,sep="")]=t(matriz_ncont_2019_genetica["AllH3",1:(8-i)])
  matriz_ncasos_defasagens_genetica_2019[(i+1):8,paste("NorthAmericaH1_",i,sep="")]=t(matriz_ncont_2019_genetica["NorthAmericaH1",1:(8-i)])
  matriz_ncasos_defasagens_genetica_2019[(i+1):8,paste("NorthAmericaH3_",i,sep="")]=t(matriz_ncont_2019_genetica["NorthAmericaH3",1:(8-i)])
  matriz_ncasos_defasagens_genetica_2019[(i+1):8,paste("AsiaH1_",i,sep="")]=t(matriz_ncont_2019_genetica["AsiaH1",1:(8-i)])
  matriz_ncasos_defasagens_genetica_2019[(i+1):8,paste("AsiaH3_",i,sep="")]=t(matriz_ncont_2019_genetica["AsiaH3",1:(8-i)])
}

# Preenchendo matriz nova com dados de 2018:
for(i in 1:6){ 
  matriz_ncasos_defasagens_genetica_2019[1:i,paste("Brasil_Bt",i,sep="")]=t(matriz_ncont_analise["Brasil",(124-i):123])
  matriz_ncasos_defasagens_genetica_2019[1:i,paste("RegiaoAmericaNorte_At",i,sep="")]=t(matriz_ncont_analise["RegiaoAmericaNorte",(124-i):123])
  matriz_ncasos_defasagens_genetica_2019[1:i,paste("RegiaoAmericaSul_St",i,sep="")]=t(matriz_ncont_analise["RegiaoAmericaSul",(124-i):123])
  matriz_ncasos_defasagens_genetica_2019[1:i,paste("RegiaoAmericaCentral_Ct",i,sep="")]=t(matriz_ncont_analise["RegiaoAmericaCentral",(124-i):123])
  matriz_ncasos_defasagens_genetica_2019[1:i,paste("RegiaoEuropeia_Et",i,sep="")]=t(matriz_ncont_analise["RegiaoEuropeia",(124-i):123])
  matriz_ncasos_defasagens_genetica_2019[1:i,paste("RegiaoSouthAsia_st",i,sep="")]=t(matriz_ncont_analise["RegiaoSouthAsia",(124-i):123])
  matriz_ncasos_defasagens_genetica_2019[1:i,paste("RegiaoWesternPacific_Wt",i,sep="")]=t(matriz_ncont_analise["RegiaoWesternPacific",(124-i):123])
  matriz_ncasos_defasagens_genetica_2019[1:i,paste("AllH1_",i,sep="")]=t(matriz_ncont_analise_genetica[(124-i):123,"AllH1"])
  matriz_ncasos_defasagens_genetica_2019[1:i,paste("AllH3_",i,sep="")]=t(matriz_ncont_analise_genetica[(124-i):123,"AllH3"])
  matriz_ncasos_defasagens_genetica_2019[1:i,paste("NorthAmericaH1_",i,sep="")]=t(matriz_ncont_analise_genetica[(124-i):123,"NorthAmericaH1"])
  matriz_ncasos_defasagens_genetica_2019[1:i,paste("NorthAmericaH3_",i,sep="")]=t(matriz_ncont_analise_genetica[(124-i):123,"NorthAmericaH3"])
  matriz_ncasos_defasagens_genetica_2019[1:i,paste("AsiaH1_",i,sep="")]=t(matriz_ncont_analise_genetica[(124-i):123,"AsiaH1"])
  matriz_ncasos_defasagens_genetica_2019[1:i,paste("AsiaH3_",i,sep="")]=t(matriz_ncont_analise_genetica[(124-i):123,"AsiaH3"])
}
matriz_ncasos_defasagens_genetica_2019

# Putting the matrices together:
matriz_ncasos_defasagens_genetica_2008_2019=rbind(matriz_ncasos_defasagens_genetica,matriz_ncasos_defasagens_genetica_2019) # Juntando as matrizes matriz_ncasos_defasagens_genetica (tem dados com defasagens desde outubro de 2008 at?? dezembro de 2018) e matriz_ncasos_defasagens_genetica_2019 (tem dados com defasagens desde janeiro de 2019 at?? agosto de 2019)
rownames(matriz_ncasos_defasagens_genetica_2008_2019)=c(1:125)
View(matriz_ncasos_defasagens_genetica_2008_2019)

# Models and predictions
treinox_genetica_2019=as.matrix(matriz_ncasos_defasagens_genetica_2008_2019[,-1])
treinoy_genetica_2019=as.matrix(matriz_ncasos_defasagens_genetica_2008_2019[,1])

# Models LASSO 5 and 10
preditos_lasso5_insample2019_genetica=predict(fit,s=46.420,type="response",newx=treinox_genetica_2019) # s=46.420 se refere ao lambda com p=5, type="response" se refere a fazer a predi????o dentro da amostra, fit ?? o modelo do lasso ajustado, e newx s??o as vari??veis explicativas da amostra (2008 at?? 2019)

# Model Stepwise 1:
aToEnter <- 0.10
aToLeave <- 0.10
result1 <- stepwise(Brasil_Bt ~ 1 + Brasil_Bt1 + Brasil_Bt2 + Brasil_Bt3 + Brasil_Bt4 + Brasil_Bt5 + Brasil_Bt6 + RegiaoAmericaNorte_At1 + RegiaoAmericaNorte_At2 + RegiaoAmericaNorte_At3 + RegiaoAmericaNorte_At4 + RegiaoAmericaNorte_At5 + RegiaoAmericaNorte_At6 + RegiaoAmericaSul_St1 + RegiaoAmericaSul_St2 + RegiaoAmericaSul_St3 + RegiaoAmericaSul_St4 + RegiaoAmericaSul_St5 + RegiaoAmericaSul_St6 + RegiaoAmericaCentral_Ct1 + RegiaoAmericaCentral_Ct2 + RegiaoAmericaCentral_Ct3 + RegiaoAmericaCentral_Ct4 + RegiaoAmericaCentral_Ct5 + RegiaoAmericaCentral_Ct6 + RegiaoEuropeia_Et1 + RegiaoEuropeia_Et2 + RegiaoEuropeia_Et3 + RegiaoEuropeia_Et4 + RegiaoEuropeia_Et5 + RegiaoEuropeia_Et6 + RegiaoSouthAsia_st1 + RegiaoSouthAsia_st2 + RegiaoSouthAsia_st3 + RegiaoSouthAsia_st4 + RegiaoSouthAsia_st5 + RegiaoSouthAsia_st6 + RegiaoWesternPacific_Wt1 + RegiaoWesternPacific_Wt2 + RegiaoWesternPacific_Wt3 + RegiaoWesternPacific_Wt4 + RegiaoWesternPacific_Wt5 + RegiaoWesternPacific_Wt6 + mediasBR + AllH1_1 + AllH1_2 + AllH1_3 + AllH1_4 + AllH1_5 + AllH1_6 + AllH3_1 + AllH3_2 + AllH3_3 + AllH3_4 + AllH3_5 + AllH3_6 + NorthAmericaH1_1 + NorthAmericaH1_2 + NorthAmericaH1_3 + NorthAmericaH1_4 + NorthAmericaH1_5 + NorthAmericaH1_6 + NorthAmericaH3_1 + NorthAmericaH3_2 + NorthAmericaH3_3 + NorthAmericaH3_4 + NorthAmericaH3_5 + NorthAmericaH3_6 + AsiaH1_1 + AsiaH1_2 + AsiaH1_3 + AsiaH1_4 + AsiaH1_5 + AsiaH1_6 + AsiaH3_1+ AsiaH3_2 + AsiaH3_3 + AsiaH3_4 + AsiaH3_5 + AsiaH3_6, Brasil_Bt ~ 1, aToEnter, aToLeave)
treinox_genetica_stepwise_2019=as.data.frame(as.matrix(matriz_ncasos_defasagens_genetica_2008_2019[,-1]))
preditos_stepwise1_insample_2019_genetica=predict(result1,newdata=treinox_genetica_stepwise_2019,type="response") # type="response" se refere a fazer a predi????o dentro da amostra, result1 ?? o modelo stepwise ajustado, e newdata s??o as vari??veis explicativas da amostra (2008 at?? 2019)

# EQM In-Sample of each model:
mean((predict(fit,s=46.420,type="response",newx=treinox_genetica_2019)[1:117] - treinoy_genetica_2019[1:117])^2) # EQM Modelo lasso p=5
mean((predict(result1,newdata=treinox_genetica_stepwise_2019,type="response")[1:117] - treinoy_genetica_2019[1:117])^2) # EQM Modelo Stepwise 1

# EQM Out-Sample of each model:
mean((predict(fit,s=46.420,type="response",newx=treinox_genetica_2019)[118:125] - treinoy_genetica_2019[118:125])^2) # EQM Modelo lasso p=5
mean((predict(result1,newdata=treinox_genetica_stepwise_2019,type="response")[118:125] - treinoy_genetica_2019[118:125])^2) # EQM Modelo Stepwise 1

# Mean absolute percentage error of In-sample models:
(sum((abs((treinoy_genetica_2019[1:117]-predict(fit,s=46.420,type="response",newx=treinox_genetica_2019)[1:117])/treinoy_genetica_2019[1:117])))/117)*100 # mape do modelo lasso com p=5
(sum((abs((treinoy_genetica_2019[1:117]-predict(result1,newdata=treinox_genetica_stepwise_2019,type="response")[1:117])/treinoy_genetica_2019[1:117])))/117)*100 # mape do modelo Stepwise 1

# Mean absolute percentage error of Out-Sample models
(sum((abs((treinoy_genetica_2019[118:125]-predict(fit,s=46.420,type="response",newx=treinox_genetica_2019)[118:125])/treinoy_genetica_2019[118:125])))/8)*100 # mape do modelo lasso com p=5
(sum((abs((treinoy_genetica_2019[118:125]-predict(result1,newdata=treinox_genetica_stepwise_2019,type="response")[118:125])/treinoy_genetica_2019[118:125])))/8)*100 # mape do modelo Stepwise 1

# Prediction charts 1 step ahead

# Transformando os dados necess??rios em dataframe:
colnames(treinoy_genetica_2019)="treinoy_genetica_2019"
treinoy_genetica_2019=data.frame(treinoy_genetica_2019)
matriz_mediasBR_genetica_2008_2019=matriz_ncasos_defasagens_genetica_2008_2019[,"mediasBR"]
names(matriz_mediasBR_genetica_2008_2019)="matriz_mediasBR_genetica_2008_2019"
matriz_mediasBR_genetica_2008_2019=data.frame(matriz_mediasBR_genetica_2008_2019)
colnames(preditos_lasso5_insample2019_genetica)="preditos_lasso5_insample2019_genetica"
preditos_lasso5_insample2019_genetica=data.frame(preditos_lasso5_insample2019_genetica)
colnames(preditos_stepwise1_insample_2019_genetica)="preditos_stepwise1_insample_2019_genetica"
preditos_stepwise1_insample_2019_genetica=data.frame(preditos_stepwise1_insample_2019_genetica)

# Lasso 5
ggplot() +
  geom_line(data = preditos_lasso5_insample2019_genetica, aes(x=c(1:125), y = preditos_lasso5_insample2019_genetica), color = "firebrick1", size = 1) +
  geom_line(data = treinoy_genetica_2019, aes(x=c(1:125), y = treinoy_genetica_2019), color = "black", size=0.4) +
  #geom_line(data = matriz_mediasBR_genetica_2008_2019, aes(x=c(1:125), y = matriz_mediasBR_genetica_2008_2019), color = "darkorange3", size=0.4) +
  labs(x = "Years", y = "Number of positive cases", title = "Predicted values (LASSO p=5)")+
  scale_x_continuous(limit = c(0,125),
                     breaks = c(10,22,34,46,58,70,82,94,106,118),
                     labels = c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019")) +
  geom_vline(xintercept = 118, linetype='dashed', color = "black", size=0.5)

# Stepwise 1
ggplot() +
  geom_line(data = preditos_stepwise1_insample_2019_genetica, aes(x=c(1:125), y = preditos_stepwise1_insample_2019_genetica), color = "chartreuse3", size = 1) +
  geom_line(data = treinoy_genetica_2019, aes(x=c(1:125), y = treinoy_genetica_2019), color = "black", size=0.4) +
  #geom_line(data = matriz_mediasBR_genetica_2008_2019, aes(x=c(1:125), y = matriz_mediasBR_genetica_2008_2019), color = "darkorange3", size=0.4) +
  labs(x = "Years", y = "Number of positive cases", title = "Predicted values (Stepwise 1 Model)")+
  scale_x_continuous(limit = c(0,125),
                     breaks = c(10,22,34,46,58,70,82,94,106,118),
                     labels = c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019")) +
  geom_vline(xintercept = 118, linetype='dashed', color = "black", size=0.5)
#############################

# Prediction (incidence and genetics data)
# Prediction 11 steps ahead:

# Matrix
# Creating matrix_pred_genetica: Nessa matriz vamos preencher os dados de incid??ncia de 2019 de acordo com a m??dia do m??s nos anos anteriores (exemplo, o n??mero de casos de janeiro de 2019 da europa ser?? a m??dia do meses de janeiro da europa dos anos anteriores). Al??m disso vamos colocar os dados de incid??ncia existentes de 2018 (que ser??o necess??rios por causa das defasagens), por exemplo, na coluna da vari??vel "Brasil_Bt1" teremos as 11 linhas indo desde dezembro de 2018 at?? outubro de 2019 (pois ?? apenas 1 defasagem), j?? a vari??vel "Brasil_Bt2" teremos as 11 linhas indo desde novembro de 2018 at?? setembro de 2010 (pois s??o 2 defasagens) e assim por diante.
matrix_pred_genetica=matrix(ncol=13, nrow=11)  # Preenchendo as colunas com as vari??veis que foram significativas: Brasil_Bt1, Brasil_Bt2, Brasil_Bt3, RegiaoEuropeia_Et1, RegiaoEuropeia_Et2, RegiaoEuropeia_Et3, RegiaoAmericaNorte_At4, RegiaoAmericaCentral_Ct2, RegiaoAmericaCentral_Ct3, RegiaoAmericaSul_St2, medias_BR, AsiaH1_5 e AsiaH1_4, e nas linhas com os meses de janeiro de 2019 at?? novembro de 2019.
rownames(matrix_pred_genetica)<-c(1:11)
colnames(matrix_pred_genetica)<-c("Brasil_Bt1", "Brasil_Bt2", "Brasil_Bt3", "RegiaoEuropeia_Et1", "RegiaoEuropeia_Et2", "RegiaoEuropeia_Et3", "RegiaoAmericaNorte_At4", "RegiaoAmericaCentral_Ct2", "RegiaoAmericaCentral_Ct3", "RegiaoAmericaSul_St2", "medias_BR", "AsiaH1_5","AsiaH1_4")
matrix_pred_genetica[,"Brasil_Bt1"]=matrix_pred_mediasBR[,"Brasil_Bt1"]
matrix_pred_genetica[,"Brasil_Bt2"]=matrix_pred_mediasBR[,"Brasil_Bt2"]
matrix_pred_genetica[,"Brasil_Bt3"]=matrix_pred_mediasBR[,"Brasil_Bt3"]
matrix_pred_genetica[,"medias_BR"]=matrix_pred_mediasBR[,"mediasBR_formatodefasagem"]
matrix_pred_genetica[,"RegiaoEuropeia_Et1"]=matrix_pred_mediasBR[,"RegiaoEuropeia_Et1"]
matrix_pred_genetica[,"RegiaoEuropeia_Et2"]=matrix_pred_mediasBR[,"RegiaoEuropeia_Et2"]
matrix_pred_genetica[,"RegiaoEuropeia_Et3"]=matrix_pred_mediasBR[,"RegiaoEuropeia_Et3"]
matrix_pred_genetica[,"RegiaoAmericaNorte_At4"]=matrix_pred_mediasBR[,"RegiaoAmericaNorte_At4"]
matrix_pred_genetica[,"RegiaoAmericaCentral_Ct3"]=matrix_pred_mediasBR[,"RegiaoAmericaCentral_Ct3"]
matrix_pred_genetica[1:2,"RegiaoAmericaCentral_Ct2"]=matriz_ncont_analise["RegiaoAmericaCentral",122:123] # novembro e dezembro de 2018 do Brasil
matrix_pred_genetica[3:11,"RegiaoAmericaCentral_Ct2"]=c(medias_janeiro["RegiaoAmericaCentral"],medias_fevereiro["RegiaoAmericaCentral"],medias_marco["RegiaoAmericaCentral"],medias_abril["RegiaoAmericaCentral"],medias_maio["RegiaoAmericaCentral"],medias_junho["RegiaoAmericaCentral"],medias_julho["RegiaoAmericaCentral"],medias_agosto["RegiaoAmericaCentral"],medias_setembro["RegiaoAmericaCentral"])
matrix_pred_genetica[1:2,"RegiaoAmericaSul_St2"]=matriz_ncont_analise["RegiaoAmericaSul",122:123] # novembro e dezembro de 2018 do Brasil
matrix_pred_genetica[3:11,"RegiaoAmericaSul_St2"]=c(medias_janeiro["RegiaoAmericaSul"],medias_fevereiro["RegiaoAmericaSul"],medias_marco["RegiaoAmericaSul"],medias_abril["RegiaoAmericaSul"],medias_maio["RegiaoAmericaSul"],medias_junho["RegiaoAmericaSul"],medias_julho["RegiaoAmericaSul"],medias_agosto["RegiaoAmericaSul"],medias_setembro["RegiaoAmericaSul"])
matrix_pred_genetica[1:4,"AsiaH1_4"]=matriz_ncont_analise_genetica[120:123,"AsiaH1"] # AsiaH1_4 tem dados desde setembro de 2018 at?? julho de 2019 (assim como todas as defasagens 4). Por??m a matriz matriz_ncont_analise_genetica vai at?? dezembro de 2018.
matrix_pred_genetica[5:11,"AsiaH1_4"]=matriz_ncont_2019_genetica["AsiaH1",1:7] # AsiaH1_4 tem dados desde setembro de 2018 at?? julho de 2019 (assim como todas as defasagens 4). Por??m a matriz matriz_ncont_2019_genetica vai de janeiro de 2019 at?? setembro de 2019, mas devido a missing vai at?? agosto de 2019.
matrix_pred_genetica[1:5,"AsiaH1_5"]=matriz_ncont_analise_genetica[119:123,"AsiaH1"] # AsiaH1_5 tem dados desde agosto de 2018 at?? junho de 2019 (assim como todas as defasagens 5). Por??m a matriz matriz_ncont_analise_genetica vai at?? dezembro de 2018.
matrix_pred_genetica[6:11,"AsiaH1_5"]=matriz_ncont_2019_genetica["AsiaH1",1:6] # AsiaH1_5 tem dados desde agosto de 2018 at?? junho de 2019 (assim como todas as defasagens 5). Por??m a matriz matriz_ncont_2019_genetica vai de janeiro de 2019 at?? setembro de 2019, mas devido a missing vai at?? agosto de 2019.
matrix_pred_genetica

# Models (remembering)
# LASSO 5
coefs.ajuste[which(abs(coefs.ajuste[,40])>0.0000000001),40]
yhat_lasso5_genetica=vector()
for (i in 1:11){
  yhat_lasso5_genetica[i]=54.8723711541+(0.0758551673*matrix_pred_genetica[i,"medias_BR"]) + (0.0003659098*matrix_pred_genetica[i,"RegiaoAmericaNorte_At4"]) + (0.5049880226*matrix_pred_genetica[i,"Brasil_Bt1"]) + (0.0053546236*matrix_pred_genetica[i,"RegiaoEuropeia_Et2"]) + (0.0021883807*matrix_pred_genetica[i,"RegiaoEuropeia_Et3"])
}

# Stepwise 1
matriz_ncasos_defasagens_genetica=as.data.frame(matriz_ncasos_defasagens_genetica)
attach(matriz_ncasos_defasagens_genetica)
aToEnter <- 0.10
aToLeave <- 0.10
result <- stepwise(Brasil_Bt ~ 1 + Brasil_Bt1 + Brasil_Bt2 + Brasil_Bt3 + Brasil_Bt4 + Brasil_Bt5 + Brasil_Bt6 + RegiaoAmericaNorte_At1 + RegiaoAmericaNorte_At2 + RegiaoAmericaNorte_At3 + RegiaoAmericaNorte_At4 + RegiaoAmericaNorte_At5 + RegiaoAmericaNorte_At6 + RegiaoAmericaSul_St1 + RegiaoAmericaSul_St2 + RegiaoAmericaSul_St3 + RegiaoAmericaSul_St4 + RegiaoAmericaSul_St5 + RegiaoAmericaSul_St6 + RegiaoAmericaCentral_Ct1 + RegiaoAmericaCentral_Ct2 + RegiaoAmericaCentral_Ct3 + RegiaoAmericaCentral_Ct4 + RegiaoAmericaCentral_Ct5 + RegiaoAmericaCentral_Ct6 + RegiaoEuropeia_Et1 + RegiaoEuropeia_Et2 + RegiaoEuropeia_Et3 + RegiaoEuropeia_Et4 + RegiaoEuropeia_Et5 + RegiaoEuropeia_Et6 + RegiaoSouthAsia_st1 + RegiaoSouthAsia_st2 + RegiaoSouthAsia_st3 + RegiaoSouthAsia_st4 + RegiaoSouthAsia_st5 + RegiaoSouthAsia_st6 + RegiaoWesternPacific_Wt1 + RegiaoWesternPacific_Wt2 + RegiaoWesternPacific_Wt3 + RegiaoWesternPacific_Wt4 + RegiaoWesternPacific_Wt5 + RegiaoWesternPacific_Wt6 + mediasBR + AllH1_1 + AllH1_2 + AllH1_3 + AllH1_4 + AllH1_5 + AllH1_6 + AllH3_1 + AllH3_2 + AllH3_3 + AllH3_4 + AllH3_5 + AllH3_6 + NorthAmericaH1_1 + NorthAmericaH1_2 + NorthAmericaH1_3 + NorthAmericaH1_4 + NorthAmericaH1_5 + NorthAmericaH1_6 + NorthAmericaH3_1 + NorthAmericaH3_2 + NorthAmericaH3_3 + NorthAmericaH3_4 + NorthAmericaH3_5 + NorthAmericaH3_6 + AsiaH1_1 + AsiaH1_2 + AsiaH1_3 + AsiaH1_4 + AsiaH1_5 + AsiaH1_6 + AsiaH3_1+ AsiaH3_2 + AsiaH3_3 + AsiaH3_4 + AsiaH3_5 + AsiaH3_6, Brasil_Bt ~ 1, aToEnter, aToLeave)
yhat_stepwise1_genetica=vector()
for (i in 1:11){
  yhat_stepwise1_genetica[i]=38.789439989 + (0.857165240*matrix_pred_genetica[i,"Brasil_Bt1"]) + (0.008931684*matrix_pred_genetica[i,"RegiaoEuropeia_Et2"]) + (-0.342930108*matrix_pred_genetica[i,"Brasil_Bt2"]) + (-0.035450084*matrix_pred_genetica[i,"RegiaoAmericaCentral_Ct3"]) + (-9221.790023588*matrix_pred_genetica[i,"AsiaH1_5"]) + (0.022042397*matrix_pred_genetica[i,"RegiaoAmericaCentral_Ct2"]) + (0.002855475*matrix_pred_genetica[i,"RegiaoAmericaNorte_At4"]) + (6482.736416212*matrix_pred_genetica[i,"AsiaH1_4"]) + (0.026039381*matrix_pred_genetica[i,"RegiaoAmericaSul_St2"])
}

# EQM
EQM_Lasso5_genetica = EQM_Stepwise1_genetica = numeric(11)
for(i in 1:11){
  EQM_Lasso5_genetica[i] = mean((yhat_lasso5_genetica[1:i]-matriz_ncont_2019["Brasil",1:i])^2)  
  EQM_Stepwise1_genetica[i] = mean((yhat_stepwise1_genetica[1:i]-matriz_ncont_2019["Brasil",1:i])^2)  
}
EQM_Lasso5_genetica;EQM_Stepwise1_genetica # EQM values in all steps and in all models


# Forecast charts 11 steps ahead (with genetics)

# Genetics 11 steps forward- starting in 2009
valores_verdadeiros_genetica=rbind(treinoy_genetica_2019,209,120,39) # valores verdadeiros do n??mero de casos positivos do Brasil no tempo atual, de abril de 2009 at?? novembro de 2019
# Gr??fico que tem valores verdadeiros de abril de 2009 at?? novembro de 2019 (valores_verdadeiros_genetica), e valores preditos de janeiro de 2019 at?? novembro de 2019
ts<- ts(valores_verdadeiros_genetica, start=c(2009,4), end=c(2019,11), frequency=12)
plot.ts(window(ts, start=start(ts), end=2020), col="black", xlim=range(time(ts)), ylim=range(ts),ylab='Number of positive cases',xlab='Years')
par(new=TRUE)
ts2<- ts(yhat_lasso5_genetica, start=c(2019,1), end=c(2019,11), frequency=12)
plot.ts(window(ts2, start=2019), col="firebrick1", axes=F, xlab="", ylab="", xlim=range(time(ts)), ylim=range(ts),lwd=2)
par(new=TRUE)
ts4<- ts(yhat_stepwise1_genetica, start=c(2019,1), end=c(2019,11), frequency=12)
plot.ts(window(ts4, start=2019), col="chartreuse3", axes=F, xlab="", ylab="", xlim=range(time(ts)), ylim=range(ts),lwd=2)
abline(v=2019,lty=2)
legend("topleft",
       bty="n",
       c("LASSO 5", "Stepwise 1"),
       lty=c(1, 1),
       lwd=c(2, 2),
       col=c("firebrick1",
             "chartreuse3"),cex=0.9,inset=-.015)
title("Forecasts of the different models")
#####

# Genetics 11 steps forward - beginning late 2018 (August 2018)
valores_verdadeiros_genetica_2015=valores_verdadeiros_genetica[70:128,1] # dados de janeiro de 2015 at?? novembro de 2019
valores_verdadeiros_genetica_2018=valores_verdadeiros_genetica_2015[44:59] # dados de agosto de 2018 at?? novembro de 2019
# Gr??fico que tem valores verdadeiros de agosto de 2018 at?? novembro de 2019 (valores_verdadeiros_genetica), e valores preditos de janeiro de 2019 at?? novembro de 2019
ts<- ts(valores_verdadeiros_genetica_2018, start=c(2018,8), end=c(2019,11), frequency=12)
plot.ts(window(ts, start=start(ts), end=2020), col="black", xlim=range(time(ts)), ylim=range(ts),ylab='Number of positive cases',xlab='Time',lwd=1.1)
par(new=TRUE)
ts2<- ts(yhat_lasso5_genetica, start=c(2019,1), end=c(2019,11), frequency=12)
plot.ts(window(ts2, start=2019), col="firebrick1", axes=F, xlab="", ylab="", xlim=range(time(ts)), ylim=range(ts),lwd=2)
par(new=TRUE)
ts4<- ts(yhat_stepwise1_genetica, start=c(2019,1), end=c(2019,11), frequency=12)
plot.ts(window(ts4, start=2019), col="chartreuse3", axes=F, xlab="", ylab="", xlim=range(time(ts)), ylim=range(ts),lwd=2)
abline(v=2019,lty=2)
legend("topleft",
       bty="n",
       c("LASSO 5", "Stepwise 1"),
       lty=c(1, 1),
       lwd=c(2, 2),
       col=c("firebrick1",
             "chartreuse3"),cex=0.9,inset=-.015)
title("Forecasts of the different models")
#############

# Genetics 11 steps forward - starting in early 2019 (January 2019)

valores_verdadeiros_2019_genetica=valores_verdadeiros_genetica_2018[6:16] # dados de janeiro de 2019 at?? novembro de 2019

valores_verdadeiros_2019_genetica=as.data.frame(valores_verdadeiros_2019_genetica)
yhat_lasso5_genetica=as.data.frame(yhat_lasso5_genetica)
yhat_stepwise1_genetica=as.data.frame(yhat_stepwise1_genetica)
dados_1passo_comgen=c(valores_verdadeiros_2019_genetica,yhat_lasso5_genetica,yhat_stepwise1_genetica)
dados_1passo_comgen=as.data.frame(dados_1passo_comgen)

colors <- c("Observed" = "black", "LASSO 5" = "firebrick1","Stepwise 1" = "chartreuse3")

ggplot(dados_1passo_comgen, aes(x = c(1:11))) +
  geom_line(aes(y = valores_verdadeiros_2019_genetica, color = "Observed"), size = 0.4) +
  geom_line(aes(y = yhat_lasso5_genetica, color = "LASSO 5"), size = 0.5) +
  geom_line(aes(y = yhat_stepwise1_genetica, color = "Stepwise 1"), size = 0.5) +
  labs(x = "2019", y = "Number of positive cases", title = "Out-of-sample forecasts",color = "Models") +
  scale_x_continuous(limit = c(1,11),
                     breaks = c(1,2,3,4,5,6,7,8,9,10,11),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov")) +
  scale_color_manual(values = colors)

##############################################################################################################################
######################### COMPARISON FORECASTS ##############################################################################
#########################################################################################################################

# Prediction comparison charts

# i) Comparing 11 steps forward

# Gráfico comparando 11 passos à frente: Montar 2 gráficos de séries temporais, um para cada modelo (Lasso 5 e Stepwise 1, onde o eixo x tem 11 pontos (tempos), indo de 1 passo a frente (janeiro de 2019) até 11 passos à frente (novembro de 2019) e o eixo y representa o EQM encontrado. Logo colocar em cada um dos 2 gráficos o EQM encontrado em cada passo nos dados não genéticos e nos dados genéticos, assim teremos 2 retas por gráfico.
eqm_lasso5=c(6346.406,5712.861,8895.711,7305.046,6349.039,21599.59,23092.44,20206.98,18254.56,16485.37,15798.62)
eqm_lasso5=as.data.frame(eqm_lasso5)
eqm_lasso5_genetica=c(5701.445,5255.093,9153.062,8076.482,6721.953,20228.37,21693.13,18985.19,17174.71,15509.01,14851.2)
eqm_lasso5_genetica=as.data.frame(eqm_lasso5_genetica)
dados_lasso5=cbind(eqm_lasso5,eqm_lasso5_genetica)
dados_lasso5=as.data.frame(dados_lasso5)

# Comparing EQM Lasso 5
colors <- c("Incidence" = "orange1", "Genetic" = "springgreen2")

ggplot(dados_lasso5, aes(x = c(1:11))) +
  geom_line(aes(y = eqm_lasso5, color = "Incidence"), size = 1) +
  geom_line(aes(y = eqm_lasso5_genetica, color = "Genetic"), size = 1) +
  labs(x = "Number of steps forward", y = "Mean squared error (MSE)", title = "Comparing MSE (LASSO p=5)",color = "Data") +
  scale_x_continuous(limit = c(1,11),
                     breaks = c(1,2,3,4,5,6,7,8,9,10,11),
                     labels = c("1","2","3","4","5","6","7","8","9","10","11")) +
  scale_color_manual(values = colors)
#####

eqm_stepwise1=c(5904.132,2958.375,8736.857,12323.75,10129.28,21543.97,23130.21,21381.31,20962.64,18934.49,17653.39)
eqm_stepwise1=as.data.frame(eqm_stepwise1)
eqm_stepwise1_genetica=c(3512.231,3428.675,9823.776,17825.10,14600.88,25108.41,25466.23,22385.60,21318.42,19235.41,18353.35)
eqm_stepwise1_genetica=as.data.frame(eqm_stepwise1_genetica)
dados_stepwise1=cbind(eqm_stepwise1,eqm_stepwise1_genetica)
dados_stepwise1=as.data.frame(dados_stepwise1)

# Comparing EQM Stepwise 1
colors <- c("Incidence" = "orange1", "Genetic" = "springgreen2")

ggplot(dados_stepwise1, aes(x = c(1:11))) +
  geom_line(aes(y = eqm_stepwise1, color = "Incidence"), size = 1) +
  geom_line(aes(y = eqm_stepwise1_genetica, color = "Genetic"), size = 1) +
  labs(x = "Number of steps forward", y = "Mean squared error (MSE)", title = "Comparing MSE (Stepwise 1 Model)",color = "Data") +
  scale_x_continuous(limit = c(1,11),
                     breaks = c(1,2,3,4,5,6,7,8,9,10,11),
                     labels = c("1","2","3","4","5","6","7","8","9","10","11")) +
  scale_color_manual(values = colors)

#######################

# ii) Comparing 1 step forward

df2=data.frame()
df2[c(1,3),"Data"]="Incidence"
df2[c(2,4),"Data"]="Genetic"
df2[c(1,2),"Models"]="LASSO 5"
df2[c(3,4),"Models"]="Stepwise 1"
df2[1:4,"MSEx10000"]=c(21805.29/10000,27781.20/10000,43228.16/10000,47623.36/10000)

library(scales)
ggplot(data=df2,aes(x=Models, y=MSEx10000, fill=Data)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_y_continuous(limit=c(0,5)) +
  scale_fill_manual(values=c('dodgerblue','firebrick2'))

##############################################################################################################################
######################### RESIDUAL ANALYSIS ##############################################################################
#########################################################################################################################

### Residual Analysis:

# Incidence

# Stepwise 1
matriz_ncasos_defasagens_mediasBR=as.data.frame(matriz_ncasos_defasagens_mediasBR)
attach(matriz_ncasos_defasagens_mediasBR)
aToEnter <- 0.10
aToLeave <- 0.10
result <- stepwise(Brasil_Bt ~ 1 + Brasil_Bt1 + Brasil_Bt2 + Brasil_Bt3 + Brasil_Bt4 + Brasil_Bt5 + Brasil_Bt6 + Brasil_Bt7 + Brasil_Bt8 + Brasil_Bt9 + Brasil_Bt10 + Brasil_Bt11 + RegiaoAmericaNorte_At1 + RegiaoAmericaNorte_At2 + RegiaoAmericaNorte_At3 + RegiaoAmericaNorte_At4 + RegiaoAmericaNorte_At5 + RegiaoAmericaNorte_At6 + RegiaoAmericaNorte_At7 + RegiaoAmericaNorte_At8 + RegiaoAmericaNorte_At9 + RegiaoAmericaNorte_At10 + RegiaoAmericaNorte_At11 + RegiaoAmericaSul_St1 + RegiaoAmericaSul_St2 + RegiaoAmericaSul_St3 + RegiaoAmericaSul_St4 + RegiaoAmericaSul_St5 + RegiaoAmericaSul_St6 + RegiaoAmericaSul_St7 + RegiaoAmericaSul_St8 + RegiaoAmericaSul_St9 + RegiaoAmericaSul_St10 + RegiaoAmericaSul_St11 + RegiaoAmericaCentral_Ct1 + RegiaoAmericaCentral_Ct2 + RegiaoAmericaCentral_Ct3 + RegiaoAmericaCentral_Ct4 + RegiaoAmericaCentral_Ct5 + RegiaoAmericaCentral_Ct6 + RegiaoAmericaCentral_Ct7 + RegiaoAmericaCentral_Ct8 + RegiaoAmericaCentral_Ct9 + RegiaoAmericaCentral_Ct10 + RegiaoAmericaCentral_Ct11 + RegiaoEuropeia_Et1 + RegiaoEuropeia_Et2 + RegiaoEuropeia_Et3 + RegiaoEuropeia_Et4 + RegiaoEuropeia_Et5 + RegiaoEuropeia_Et6 + RegiaoEuropeia_Et7 + RegiaoEuropeia_Et8 + RegiaoEuropeia_Et9 + RegiaoEuropeia_Et10 + RegiaoEuropeia_Et11 + RegiaoSouthAsia_st1 + RegiaoSouthAsia_st2 + RegiaoSouthAsia_st3 + RegiaoSouthAsia_st4 + RegiaoSouthAsia_st5 + RegiaoSouthAsia_st6 + RegiaoSouthAsia_st7 + RegiaoSouthAsia_st8 + RegiaoSouthAsia_st9 + RegiaoSouthAsia_st10 + RegiaoSouthAsia_st11 + RegiaoWesternPacific_Wt1 + RegiaoWesternPacific_Wt2 + RegiaoWesternPacific_Wt3 + RegiaoWesternPacific_Wt4 + RegiaoWesternPacific_Wt5 + RegiaoWesternPacific_Wt6 + RegiaoWesternPacific_Wt7 + RegiaoWesternPacific_Wt8 + RegiaoWesternPacific_Wt9 + RegiaoWesternPacific_Wt10 + RegiaoWesternPacific_Wt11 + mediasBR_formatodefasagem, Brasil_Bt ~ 1, aToEnter, aToLeave)
# Residuals
s1r = result$residuals
plot.ts(s1r)
Box.test(s1r, lag=20, type="L") # p-value = 0.9561
shapiro.test(s1r)
hist(s1r, breaks=20)

s1_ind= which(abs(s1r)<400)
shapiro.test(s1r[s1_ind]) # teste de normalidade sem os 5 outliers
hist(s1r[s1_ind], breaks=20) # histograma sem os 5 outliers


# Model 5 variables Lasso (11 lags)
library(glmnet)
treinox=as.matrix(matriz_ncasos_defasagens_mediasBR[,-13]) #vari??veis independentes at?? a defasagem 11
treinoy=as.matrix(matriz_ncasos_defasagens_mediasBR[,13])
fit = glmnet(x=treinox, y=treinoy, alpha=1, nlambda=100)
coefs.ajuste = coef(fit)
coefs.ajuste[which(abs(coefs.ajuste[,20])>0.0000000001),20] # Model 5 variables Lasso.

# Residuals LASSO 5:
preditos_lasso5_insample=predict(fit,s=50.390,type="response",newx=treinox) # Modelo LASSO 5
res_l5=treinoy-preditos_lasso5_insample
plot.ts(res_l5)
Box.test(res_l5, lag=20, type="L") # p-value = 0.2096
shapiro.test(res_l5)
hist(res_l5, breaks=20)
boxplot(res_l5)

l5_ind= which(abs(res_l5)<400)
shapiro.test(res_l5[l5_ind])
hist(res_l5[l5_ind], breaks=30)
which(abs(res_l5)>400)
##########################################
# Models with genetic:

# Stepwise 1
matriz_ncasos_defasagens_genetica=as.data.frame(matriz_ncasos_defasagens_genetica)
attach(matriz_ncasos_defasagens_genetica)
aToEnter <- 0.10
aToLeave <- 0.10
result <- stepwise(Brasil_Bt ~ 1 + Brasil_Bt1 + Brasil_Bt2 + Brasil_Bt3 + Brasil_Bt4 + Brasil_Bt5 + Brasil_Bt6 + RegiaoAmericaNorte_At1 + RegiaoAmericaNorte_At2 + RegiaoAmericaNorte_At3 + RegiaoAmericaNorte_At4 + RegiaoAmericaNorte_At5 + RegiaoAmericaNorte_At6 + RegiaoAmericaSul_St1 + RegiaoAmericaSul_St2 + RegiaoAmericaSul_St3 + RegiaoAmericaSul_St4 + RegiaoAmericaSul_St5 + RegiaoAmericaSul_St6 + RegiaoAmericaCentral_Ct1 + RegiaoAmericaCentral_Ct2 + RegiaoAmericaCentral_Ct3 + RegiaoAmericaCentral_Ct4 + RegiaoAmericaCentral_Ct5 + RegiaoAmericaCentral_Ct6 + RegiaoEuropeia_Et1 + RegiaoEuropeia_Et2 + RegiaoEuropeia_Et3 + RegiaoEuropeia_Et4 + RegiaoEuropeia_Et5 + RegiaoEuropeia_Et6 + RegiaoSouthAsia_st1 + RegiaoSouthAsia_st2 + RegiaoSouthAsia_st3 + RegiaoSouthAsia_st4 + RegiaoSouthAsia_st5 + RegiaoSouthAsia_st6 + RegiaoWesternPacific_Wt1 + RegiaoWesternPacific_Wt2 + RegiaoWesternPacific_Wt3 + RegiaoWesternPacific_Wt4 + RegiaoWesternPacific_Wt5 + RegiaoWesternPacific_Wt6 + mediasBR + AllH1_1 + AllH1_2 + AllH1_3 + AllH1_4 + AllH1_5 + AllH1_6 + AllH3_1 + AllH3_2 + AllH3_3 + AllH3_4 + AllH3_5 + AllH3_6 + NorthAmericaH1_1 + NorthAmericaH1_2 + NorthAmericaH1_3 + NorthAmericaH1_4 + NorthAmericaH1_5 + NorthAmericaH1_6 + NorthAmericaH3_1 + NorthAmericaH3_2 + NorthAmericaH3_3 + NorthAmericaH3_4 + NorthAmericaH3_5 + NorthAmericaH3_6 + AsiaH1_1 + AsiaH1_2 + AsiaH1_3 + AsiaH1_4 + AsiaH1_5 + AsiaH1_6 + AsiaH3_1+ AsiaH3_2 + AsiaH3_3 + AsiaH3_4 + AsiaH3_5 + AsiaH3_6, Brasil_Bt ~ 1, aToEnter, aToLeave)
# Residuals
s1r_gen = result$residuals
plot.ts(s1r_gen)
abline(h=400,col="red")
abline(h=-400,col="red")
Box.test(s1r_gen, lag=20, type="L") # p-value = 0.9922
shapiro.test(s1r_gen)
hist(s1r_gen, breaks=20)

s1_gen_ind= which(abs(s1r_gen)<400)
shapiro.test(s1r_gen[s1_gen_ind])
hist(s1r_gen[s1_gen_ind], breaks=30)
which(abs(s1r_gen)>400)


# Model LASSO 5 variables (6 lags of the number of cases and 6 lags of genetics)
library(glmnet)
treinox=as.matrix(matriz_ncasos_defasagens_genetica[,-1]) #vari??veis independentes at?? a defasagem 6
treinoy=as.matrix(matriz_ncasos_defasagens_genetica[,1])
fit = glmnet(x=treinox, y=treinoy, alpha=1, nlambda=200)
coefs.ajuste = coef(fit)
coefs.ajuste[which(abs(coefs.ajuste[,40])>0.0000000001),40] # Model 5 variables Lasso (Brasil_Bt1,RegiaoEuropeia_Et2,RegiaoEuropeia_Et3,RegiaoAmericaNorte_At4,mediasBR)

# Residuals LASSO 5
res_l5_gen=treinoy_genetica_2019-preditos_lasso5_insample2019_genetica
res_l5_gen=as.matrix(res_l5_gen)
plot.ts(res_l5_gen)
abline(h=400,col="red")
abline(h=-400,col="red")
Box.test(res_l5_gen, lag=20, type="L") # p-value = 0.3805
shapiro.test(res_l5_gen)
hist(res_l5_gen, breaks=20)
a=boxplot(res_l5_gen)
mean(res_l5_gen)
median(res_l5_gen)

l5_ind_gen= which(abs(res_l5_gen)<400)
shapiro.test(res_l5_gen[l5_ind_gen])
hist(res_l5_gen[l5_ind_gen], breaks=30)
which(abs(res_l5_gen)>400)

##############################################################################################################################
######################### LASSO with Cross Validation ########################################################################
##############################################################################################################################

# LASSO with Cross-Validation - moving averages for Brazil

### Genetic data
treinoy=as.matrix(matriz_ncasos_defasagens_genetica[,1]) # variável resposta
treinox=as.matrix(matriz_ncasos_defasagens_genetica[,-1]) #variáveis independentes até a defasagem 6

set.seed(26787) # seed
library(glmnet)
library(ggplot2)

### LASSO

## Perform lasso
lasso1 <- glmnet(x = treinox, y = treinoy,
                 alpha = 1) # ‘alpha = 1’ is the lasso penalty, and ‘alpha = 0’ the ridge penalty.
plot(lasso1, xvar = "lambda")


# Let's implement lasso with 117-fold CV below (leave one out cross-validation since we have 117 observations)
lasso1_cv <- cv.glmnet(x = treinox, y = treinoy, # O comando cv.glmnet implementa validação cruzada k-fold
                       type.measure = "mse", # type.measure: loss to use for cross-validation.
                       nfold = 117, # método leave one out 
                       alpha = 1) # ‘alpha = 1’ is the lasso penalty, and ‘alpha = 0’ the ridge penalty.
plot(lasso1_cv) # Penalty vs CV MSE plot


## Extract coefficients at the error-minimizing lambda
lasso1_cv$lambda.min

## s: Value(s) of the penalty parameter ‘lambda’ at which predictions are required. 
# Default is the entire sequence used to create the model.
best_lasso_coef <- coef(lasso1_cv, s = lasso1_cv$lambda.min)
best_lasso_coef # Genetic LASSO model - 15 covariates

#########################################################################

##### Data Analysis - LASSO - Forecast

# Forecast 11 steps ahead:

# Forecast Matrix:
# Criando matrix_pred_genetica_lasso: Nessa matriz vamos preencher os dados de incidência 
# de 2019 de acordo com a média do mês nos anos anteriores (exemplo, o número de casos de janeiro 
# de 2019 da europa será a média do meses de janeiro da europa dos anos anteriores). Além disso 
# vamos colocar os dados de incidência existentes de 2018 (que serão necessários por causa das defasagens), 
# por exemplo, na coluna da variável "Brasil_Bt1" teremos as 11 linhas indo desde dezembro de 2018 até outubro de 2019 
# (pois é apenas 1 defasagem), já a variável "Brasil_Bt2" teremos as 11 linhas indo desde novembro de 2018 até 
# setembro de 2010 (pois são 2 defasagens) e assim por diante. Já com as variáveis de diversidade não vamos
# preencher com as médias dos anos anteriores, mas sim apenas com a média do ano de 2018.

# In setting up this matrix, we will have to consider the variables that were significant in the lasso model, 
# which are the following: Brasil_Bt1, Brasil_Bt2, Brasil_Bt3, RegiaoEuropeia_Et1,
# RegiaoEuropeia_Et2, RegiaoEuropeia_Et3, RegiaoEuropeia_Et4, RegiaoAmericaNorte_At4,
# RegiaoAmericaCentral_Ct3, RegiaoSouthAsia_st4, RegiaoWesternPacific_Wt6, mediasBR,
# AllH1_5, AsiaH1_4 e AsiaH1_5.

matrix_pred_genetica_lasso=matrix(ncol=15, nrow=11)  # Preenchendo as colunas com as variáveis que foram significativas citadas acima, e nas linhas com os meses de janeiro de 2019 até novembro de 2019.
rownames(matrix_pred_genetica_lasso)<-c(1:11)
colnames(matrix_pred_genetica_lasso)<-c("Brasil_Bt1", "Brasil_Bt2", "Brasil_Bt3", "RegiaoEuropeia_Et1", 
                                        "RegiaoEuropeia_Et2", "RegiaoEuropeia_Et3", "RegiaoEuropeia_Et4", 
                                        "RegiaoAmericaNorte_At4","RegiaoAmericaCentral_Ct3","RegiaoSouthAsia_st4",
                                        "RegiaoWesternPacific_Wt6","medias_BR","AllH1_5","AsiaH1_4","AsiaH1_5")
matrix_pred_genetica_lasso[,"Brasil_Bt1"]=matrix_pred_genetica[,"Brasil_Bt1"]   
matrix_pred_genetica_lasso[,"Brasil_Bt2"]=matrix_pred_genetica[,"Brasil_Bt2"]  
matrix_pred_genetica_lasso[,"Brasil_Bt3"]=matrix_pred_genetica[,"Brasil_Bt3"]  
matrix_pred_genetica_lasso[,"RegiaoEuropeia_Et1"]=matrix_pred_genetica[,"RegiaoEuropeia_Et1"]  
matrix_pred_genetica_lasso[,"RegiaoEuropeia_Et2"]=matrix_pred_genetica[,"RegiaoEuropeia_Et2"]  
matrix_pred_genetica_lasso[,"RegiaoEuropeia_Et3"]=matrix_pred_genetica[,"RegiaoEuropeia_Et3"]  
matrix_pred_genetica_lasso[,"RegiaoAmericaNorte_At4"]=matrix_pred_genetica[,"RegiaoAmericaNorte_At4"]  
matrix_pred_genetica_lasso[,"RegiaoAmericaCentral_Ct3"]=matrix_pred_genetica[,"RegiaoAmericaCentral_Ct3"]  
matrix_pred_genetica_lasso[,"medias_BR"]=matrix_pred_genetica[,"medias_BR"]  
matrix_pred_genetica_lasso[1:4,"RegiaoEuropeia_Et4"]=matriz_ncont_analise["RegiaoEuropeia",120:123] # setembro até dezembro de 2018 do Brasil
matrix_pred_genetica_lasso[5:11,"RegiaoEuropeia_Et4"]=c(medias_janeiro["RegiaoEuropeia"],medias_fevereiro["RegiaoEuropeia"],medias_marco["RegiaoEuropeia"],medias_abril["RegiaoEuropeia"],medias_maio["RegiaoEuropeia"],medias_junho["RegiaoEuropeia"],medias_julho["RegiaoEuropeia"])  
matrix_pred_genetica_lasso[1:4,"RegiaoSouthAsia_st4"]=matriz_ncont_analise["RegiaoSouthAsia",120:123]
matrix_pred_genetica_lasso[5:11,"RegiaoSouthAsia_st4"]=c(medias_janeiro["RegiaoSouthAsia"],medias_fevereiro["RegiaoSouthAsia"],medias_marco["RegiaoSouthAsia"],medias_abril["RegiaoSouthAsia"],medias_maio["RegiaoSouthAsia"],medias_junho["RegiaoSouthAsia"],medias_julho["RegiaoSouthAsia"])  
matrix_pred_genetica_lasso[1:6,"RegiaoWesternPacific_Wt6"]=matriz_ncont_analise["RegiaoWesternPacific",118:123]
matrix_pred_genetica_lasso[7:11,"RegiaoWesternPacific_Wt6"]=c(medias_janeiro["RegiaoWesternPacific"],medias_fevereiro["RegiaoWesternPacific"],medias_marco["RegiaoWesternPacific"],medias_abril["RegiaoWesternPacific"],medias_maio["RegiaoWesternPacific"])  
matrix_pred_genetica_lasso[1:5,"AllH1_5"]=matriz_ncont_analise_genetica[119:123,"AllH1"]
matrix_pred_genetica_lasso[6:11,"AllH1_5"]=mean(matriz_ncont_analise_genetica[112:123,"AllH1"]) # preenchendo os dados de 2019 com a média de AllH1 do ano anterior (2018-112:123)
matrix_pred_genetica_lasso[1:4,"AsiaH1_4"]=matriz_ncont_analise_genetica[120:123,"AsiaH1"]
matrix_pred_genetica_lasso[5:11,"AsiaH1_4"]=mean(matriz_ncont_analise_genetica[112:123,"AsiaH1"])
matrix_pred_genetica_lasso[1:5,"AsiaH1_5"]=matriz_ncont_analise_genetica[119:123,"AsiaH1"]
matrix_pred_genetica_lasso[6:11,"AsiaH1_5"]=mean(matriz_ncont_analise_genetica[112:123,"AsiaH1"]) # preenchendo os dados de 2019 com a média de AsiaH1 do ano anterior (2018-112:123)
matrix_pred_genetica_lasso # matriz final que vamos utilizar para previsão


## LASSO Genetic Model
yhat_modelo_lassocv_genetica=numeric(11)
for (i in 1:11){
  yhat_modelo_lassocv_genetica[i]=55.45976 + (0.60024*matrix_pred_genetica_lasso[i,"Brasil_Bt1"]) + (-0.03383*matrix_pred_genetica_lasso[i,"Brasil_Bt2"]) + (-0.04411*matrix_pred_genetica_lasso[i,"Brasil_Bt3"]) + (0.00161*matrix_pred_genetica_lasso[i,"RegiaoEuropeia_Et1"]) + (0.00609*matrix_pred_genetica_lasso[i,"RegiaoEuropeia_Et2"])  + (0.00086*matrix_pred_genetica_lasso[i,"RegiaoEuropeia_Et3"]) + (0.00053*matrix_pred_genetica_lasso[i,"RegiaoEuropeia_Et4"]) + (0.00166*matrix_pred_genetica_lasso[i,"RegiaoAmericaNorte_At4"])  + (-0.00946*matrix_pred_genetica_lasso[i,"RegiaoAmericaCentral_Ct3"]) + (-0.00957*matrix_pred_genetica_lasso[i,"RegiaoSouthAsia_st4"]) + (-0.00094*matrix_pred_genetica_lasso[i,"RegiaoWesternPacific_Wt6"]) + (0.05940*matrix_pred_genetica_lasso[i,"medias_BR"]) + (-837.79934*matrix_pred_genetica_lasso[i,"AllH1_5"]) + (3827.27813*matrix_pred_genetica_lasso[i,"AsiaH1_4"]) +  (-3156.13603*matrix_pred_genetica_lasso[i,"AsiaH1_5"])        
}

##### Forecasts

# Graphic 11 steps forward - starting in 2009
valores_verdadeiros_genetica=rbind(treinoy_genetica_2019,209,120,39) # valores verdadeiros do número de casos positivos do Brasil no tempo atual, de abril de 2009 até novembro de 2019
# Gráfico que tem valores verdadeiros de abril de 2009 até novembro de 2019 (valores_verdadeiros_genetica), e valores preditos de janeiro de 2019 até novembro de 2019
ts<- ts(valores_verdadeiros_genetica, start=c(2009,4), end=c(2019,11), frequency=12)
plot.ts(window(ts, start=start(ts), end=2020), col="black", xlim=range(time(ts)), ylim=range(ts),ylab='Number of positive cases',xlab='Years')
par(new=TRUE)
ts2<- ts(yhat_modelo_lassocv_genetica, start=c(2019,1), end=c(2019,11), frequency=12)
plot.ts(window(ts2, start=2019), col="firebrick1", axes=F, xlab="", ylab="", xlim=range(time(ts)), ylim=range(ts),lwd=2)


# Graphic 11 steps forward ggplot2 - starting in early 2019 (January 2019)

valores_verdadeiros_2019_genetica=valores_verdadeiros_genetica_2018[6:16] # dados de janeiro de 2019 até novembro de 2019
valores_verdadeiros_2019_genetica=as.data.frame(valores_verdadeiros_2019_genetica)
yhat_modelo_lassocv_genetica=as.data.frame(yhat_modelo_lassocv_genetica)
dados_lassocv_comgen=c(valores_verdadeiros_2019_genetica,yhat_modelo_lassocv_genetica)
dados_lassocv_comgen=as.data.frame(dados_lassocv_comgen)

colors <- c("Observed" = "black", "LASSO" = "firebrick1")

ggplot(dados_lassocv_comgen, aes(x = c(1:11))) +
  geom_line(aes(y = valores_verdadeiros_2019_genetica, color = "Observed"), size = 0.6) +
  geom_line(aes(y = yhat_modelo_lassocv_genetica, color = "LASSO"), size = 0.8) +
  labs(x = "2019", y = "Number of positive cases", title = "Out-of-sample forecasts",color = "Models") +
  scale_x_continuous(limit = c(1,11),
                     breaks = c(1,2,3,4,5,6,7,8,9,10,11),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov")) +
  scale_color_manual(values = colors)


##### Forecasting Errors 11 steps

# Mean square error (MSE)
EQM_Modelo_LASSOCV_genetica = numeric(11)
for(i in 1:11){
  EQM_Modelo_LASSOCV_genetica[i] = mean((yhat_modelo_lassocv_genetica[1:i,]-matriz_ncont_2019["Brasil",1:i])^2)  
}

# MAPE 11 steps forward
(sum((abs((matriz_ncont_2019["Brasil",]-yhat_modelo_lassocv_genetica)/matriz_ncont_2019["Brasil",])))/11)*100 # mape modelo lasso com cv

# MAE 11 steps forward
(sum((abs(matriz_ncont_2019["Brasil",]-yhat_modelo_lassocv_genetica)))/11) # mae modelo lasso com cv

####################################################################################################
####################################################################################################

### Incidence Data
treinoy=as.matrix(matriz_ncasos_defasagens_mediasBR[,13]) # variável resposta
treinox=as.matrix(matriz_ncasos_defasagens_mediasBR[,-13]) # variáveis independentes até a defasagem 11

set.seed(26787) # semente

### LASSO

## Perform lasso
lasso1 <- glmnet(x = treinox, y = treinoy,
                 alpha = 1) # ‘alpha = 1’ is the lasso penalty, and ‘alpha = 0’ the ridge penalty.
plot(lasso1, xvar = "lambda")

# Let's implement lasso with 111-fold CV below (leave one out cross-validation since we have 111 observations)
lasso1_cv <- cv.glmnet(x = treinox, y = treinoy, # O comando cv.glmnet implementa validação cruzada k-fold
                       type.measure = "mse", # type.measure: loss to use for cross-validation.
                       nfold = 111, # método leave one out 
                       alpha = 1) # ‘alpha = 1’ is the lasso penalty, and ‘alpha = 0’ the ridge penalty.
plot(lasso1_cv) # Penalty vs CV MSE plot


## Extract coefficients at the error-minimizing lambda
lasso1_cv$lambda.min

## s: Value(s) of the penalty parameter ‘lambda’ at which predictions are required. 
# Default is the entire sequence used to create the model.
best_lasso_coef <- coef(lasso1_cv, s = lasso1_cv$lambda.min)
best_lasso_coef # LASSO incidence model - 13 covariates

#########################################################################

##### Data Analysis - LASSO - Forecast

# Forecast 11 steps ahead:

# Forecast Matrix:
# Criando matrix_pred_lasso: Nessa matriz vamos preencher os dados de incidência 
# de 2019 de acordo com a média do mês nos anos anteriores (exemplo, o número de casos de janeiro 
# de 2019 da europa será a média do meses de janeiro da europa dos anos anteriores). Além disso 
# vamos colocar os dados de incidência existentes de 2018 (que serão necessários por causa das defasagens), 
# por exemplo, na coluna da variável "Brasil_Bt1" teremos as 11 linhas indo desde dezembro de 2018 até outubro de 2019 
# (pois é apenas 1 defasagem), já a variável "Brasil_Bt2" teremos as 11 linhas indo desde novembro de 2018 até 
# setembro de 2010 (pois são 2 defasagens) e assim por diante.

# In setting up this matrix, we will have to consider the variables that were significant in the lasso model, 
# which are the following: Brasil_Bt1, Brasil_Bt2, RegiaoEuropeia_Et1,
# RegiaoEuropeia_Et2, RegiaoEuropeia_Et3, RegiaoEuropeia_Et4, RegiaoAmericaNorte_At4,
# RegiaoAmericaCentral_Ct1, RegiaoAmericaCentral_Ct3, RegiaoSouthAsia_st4, RegiaoSouthAsia_st9,
# RegiaoWesternPacific_Wt7 e RegiaoWesternPacific_Wt9.

matrix_pred_lasso=matrix(ncol=13, nrow=11)  # Preenchendo as colunas com as variáveis que foram significativas citadas acima, e nas linhas com os meses de janeiro de 2019 até novembro de 2019.
rownames(matrix_pred_lasso)<-c(1:11)
colnames(matrix_pred_lasso)<-c("Brasil_Bt1","Brasil_Bt2","RegiaoEuropeia_Et1","RegiaoEuropeia_Et2",
                               "RegiaoEuropeia_Et3", "RegiaoEuropeia_Et4","RegiaoAmericaNorte_At4",
                               "RegiaoAmericaCentral_Ct1","RegiaoAmericaCentral_Ct3","RegiaoSouthAsia_st4",
                               "RegiaoSouthAsia_st9","RegiaoWesternPacific_Wt7","RegiaoWesternPacific_Wt9")
matrix_pred_lasso[,"Brasil_Bt1"]=matrix_pred[,"Brasil_Bt1"]    
matrix_pred_lasso[,"Brasil_Bt2"]=matrix_pred[,"Brasil_Bt2"]    
matrix_pred_lasso[,"RegiaoEuropeia_Et1"]=matrix_pred[,"RegiaoEuropeia_Et1"]    
matrix_pred_lasso[,"RegiaoEuropeia_Et2"]=matrix_pred[,"RegiaoEuropeia_Et2"]    
matrix_pred_lasso[,"RegiaoEuropeia_Et3"]=matrix_pred[,"RegiaoEuropeia_Et3"]    
matrix_pred_lasso[,"RegiaoEuropeia_Et4"]=matrix_pred[,"RegiaoEuropeia_Et4"]    
matrix_pred_lasso[,"RegiaoAmericaNorte_At4"]=matrix_pred[,"RegiaoAmericaNorte_At4"]    
matrix_pred_lasso[,"RegiaoAmericaCentral_Ct1"]=c(matriz_ncont_analise["RegiaoAmericaCentral",123], medias_janeiro["RegiaoAmericaCentral"],medias_fevereiro["RegiaoAmericaCentral"],medias_marco["RegiaoAmericaCentral"],medias_abril["RegiaoAmericaCentral"],medias_maio["RegiaoAmericaCentral"],medias_junho["RegiaoAmericaCentral"],medias_julho["RegiaoAmericaCentral"],medias_agosto["RegiaoAmericaCentral"],medias_setembro["RegiaoAmericaCentral"],medias_outubro["RegiaoAmericaCentral"])  
matrix_pred_lasso[,"RegiaoAmericaCentral_Ct3"]=matrix_pred[,"RegiaoAmericaCentral_Ct3"]    
matrix_pred_lasso[,"RegiaoSouthAsia_st4"]=c(matriz_ncont_analise["RegiaoSouthAsia",120:123],medias_janeiro["RegiaoSouthAsia"],medias_fevereiro["RegiaoSouthAsia"],medias_marco["RegiaoSouthAsia"],medias_abril["RegiaoSouthAsia"],medias_maio["RegiaoSouthAsia"],medias_junho["RegiaoSouthAsia"],medias_julho["RegiaoSouthAsia"]) 
matrix_pred_lasso[,"RegiaoSouthAsia_st9"]=c(matriz_ncont_analise["RegiaoSouthAsia",115:123], medias_janeiro["RegiaoSouthAsia"],medias_fevereiro["RegiaoSouthAsia"]) # como temos 9 defasagens então a matriz vai-se de abril de 2018 até fevereiro de 2019 (onde até dezembro de 2018 são os dados verdadeiros e depois em janeiro e fevereiro tem-se as médias mensais até 2018)
matrix_pred_lasso[,"RegiaoWesternPacific_Wt7"]=matrix_pred[,"RegiaoWesternPacific_Wt7"]    
matrix_pred_lasso[,"RegiaoWesternPacific_Wt9"]=c(matriz_ncont_analise["RegiaoWesternPacific",115:123], medias_janeiro["RegiaoWesternPacific"],medias_fevereiro["RegiaoWesternPacific"])
matrix_pred_lasso # matriz final que vamos utilizar para previsão

## LASSO Model
yhat_modelo_lassocv=numeric(11)
for (i in 1:11){
  yhat_modelo_lassocv[i]=44.08853 + (0.00086*matrix_pred_lasso[i,"RegiaoAmericaNorte_At4"]) + (0.68871*matrix_pred_lasso[i,"Brasil_Bt1"]) + (-0.15364*matrix_pred_lasso[i,"Brasil_Bt2"]) + (0.00095*matrix_pred_lasso[i,"RegiaoAmericaCentral_Ct1"]) + (-0.01308*matrix_pred_lasso[i,"RegiaoAmericaCentral_Ct3"])  + (0.00180*matrix_pred_lasso[i,"RegiaoEuropeia_Et1"]) + (0.00650*matrix_pred_lasso[i,"RegiaoEuropeia_Et2"]) + (0.00084*matrix_pred_lasso[i,"RegiaoEuropeia_Et3"])  + (0.00230*matrix_pred_lasso[i,"RegiaoEuropeia_Et4"]) + (-0.00173*matrix_pred_lasso[i,"RegiaoSouthAsia_st4"]) + (0.00066*matrix_pred_lasso[i,"RegiaoSouthAsia_st9"]) + (-0.00158*matrix_pred_lasso[i,"RegiaoWesternPacific_Wt7"]) + (0.00138*matrix_pred_lasso[i,"RegiaoWesternPacific_Wt9"])
}

##### Forecasts

# Prediction 11 steps forward (no genetics) - starting in early 2019 (January 2019)

valores_verdadeiros_2019=valores_verdadeiros_2018[6:16] # dados de janeiro de 2019 até novembro de 2019

valores_verdadeiros_2019=as.data.frame(valores_verdadeiros_2019)
yhat_modelo_lassocv=as.data.frame(yhat_modelo_lassocv)
dados_lassocv_semgen=c(valores_verdadeiros_2019,yhat_modelo_lassocv)
dados_lassocv_semgen=as.data.frame(dados_lassocv_semgen)

colors <- c("Observed" = "black","LASSO" = "dodgerblue")

ggplot(dados_lassocv_semgen, aes(x = c(1:11))) +
  geom_line(aes(y = valores_verdadeiros_2019, color = "Observed"), size = 0.6) +
  geom_line(aes(y = yhat_modelo_lassocv, color = "LASSO"), size = 0.8) +
  labs(x = "2019", y = "Number of positive cases", title = "Out-of-sample forecasts",color = "Models") +
  scale_x_continuous(limit = c(1,11),
                     breaks = c(1,2,3,4,5,6,7,8,9,10,11),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov")) +
  scale_color_manual(values = colors)

##### Forecasting Errors 11 steps

# Mean square error (MSE)
EQM_Modelo_LASSOCV = numeric(11)
for(i in 1:11){
  EQM_Modelo_LASSOCV[i] = mean((yhat_modelo_lassocv[1:i,]-matriz_ncont_2019["Brasil",1:i])^2)  
}

# MAPE 11 steps forward
(sum((abs((matriz_ncont_2019["Brasil",]-yhat_modelo_lassocv)/matriz_ncont_2019["Brasil",])))/11)*100 # mape modelo lasso com cv

# MAE 11 steps forward
(sum((abs(matriz_ncont_2019["Brasil",]-yhat_modelo_lassocv)))/11) # mae modelo lasso com cv
##########################

# Comparing MSE LASSO CV (Incidence vs. Genetic) 11 steps

EQM_Modelo_LASSOCV=as.data.frame(EQM_Modelo_LASSOCV)
EQM_Modelo_LASSOCV_genetica=as.data.frame(EQM_Modelo_LASSOCV_genetica)
dados_lassocv=cbind(EQM_Modelo_LASSOCV,EQM_Modelo_LASSOCV_genetica)
dados_lassocv=as.data.frame(dados_lassocv)

colors <- c("Incidence" = "orange1", "Genetic" = "springgreen2")

ggplot(dados_lassocv, aes(x = c(1:11))) +
  geom_line(aes(y = EQM_Modelo_LASSOCV, color = "Incidence"), size = 1) +
  geom_line(aes(y = EQM_Modelo_LASSOCV_genetica, color = "Genetic"), size = 1) +
  labs(x = "Number of steps forward", y = "Mean squared error (MSE)", title = "Comparing MSE (LASSO CV)",color = "Data") +
  scale_x_continuous(limit = c(1,11),
                     breaks = c(1,2,3,4,5,6,7,8,9,10,11),
                     labels = c("1","2","3","4","5","6","7","8","9","10","11")) +
  scale_color_manual(values = colors)

#########################################################################
#########################################################################

# Stepwise 1 corrections (just for genetic)

matrix_pred_genetica_step1=cbind(matrix_pred_genetica[,-c(12,13)],matrix_pred_genetica_lasso[,c(14,15)])

# Stepwise 1
matriz_ncasos_defasagens_genetica=as.data.frame(matriz_ncasos_defasagens_genetica)
attach(matriz_ncasos_defasagens_genetica)
aToEnter <- 0.10
aToLeave <- 0.10
result <- stepwise(Brasil_Bt ~ 1 + Brasil_Bt1 + Brasil_Bt2 + Brasil_Bt3 + Brasil_Bt4 + Brasil_Bt5 + Brasil_Bt6 + RegiaoAmericaNorte_At1 + RegiaoAmericaNorte_At2 + RegiaoAmericaNorte_At3 + RegiaoAmericaNorte_At4 + RegiaoAmericaNorte_At5 + RegiaoAmericaNorte_At6 + RegiaoAmericaSul_St1 + RegiaoAmericaSul_St2 + RegiaoAmericaSul_St3 + RegiaoAmericaSul_St4 + RegiaoAmericaSul_St5 + RegiaoAmericaSul_St6 + RegiaoAmericaCentral_Ct1 + RegiaoAmericaCentral_Ct2 + RegiaoAmericaCentral_Ct3 + RegiaoAmericaCentral_Ct4 + RegiaoAmericaCentral_Ct5 + RegiaoAmericaCentral_Ct6 + RegiaoEuropeia_Et1 + RegiaoEuropeia_Et2 + RegiaoEuropeia_Et3 + RegiaoEuropeia_Et4 + RegiaoEuropeia_Et5 + RegiaoEuropeia_Et6 + RegiaoSouthAsia_st1 + RegiaoSouthAsia_st2 + RegiaoSouthAsia_st3 + RegiaoSouthAsia_st4 + RegiaoSouthAsia_st5 + RegiaoSouthAsia_st6 + RegiaoWesternPacific_Wt1 + RegiaoWesternPacific_Wt2 + RegiaoWesternPacific_Wt3 + RegiaoWesternPacific_Wt4 + RegiaoWesternPacific_Wt5 + RegiaoWesternPacific_Wt6 + mediasBR + AllH1_1 + AllH1_2 + AllH1_3 + AllH1_4 + AllH1_5 + AllH1_6 + AllH3_1 + AllH3_2 + AllH3_3 + AllH3_4 + AllH3_5 + AllH3_6 + NorthAmericaH1_1 + NorthAmericaH1_2 + NorthAmericaH1_3 + NorthAmericaH1_4 + NorthAmericaH1_5 + NorthAmericaH1_6 + NorthAmericaH3_1 + NorthAmericaH3_2 + NorthAmericaH3_3 + NorthAmericaH3_4 + NorthAmericaH3_5 + NorthAmericaH3_6 + AsiaH1_1 + AsiaH1_2 + AsiaH1_3 + AsiaH1_4 + AsiaH1_5 + AsiaH1_6 + AsiaH3_1+ AsiaH3_2 + AsiaH3_3 + AsiaH3_4 + AsiaH3_5 + AsiaH3_6, Brasil_Bt ~ 1, aToEnter, aToLeave)

yhat_stepwise1_genetica=vector()

# Model
for (i in 1:11){
  yhat_stepwise1_genetica[i]=38.789439989 + (0.857165240*matrix_pred_genetica_step1[i,"Brasil_Bt1"]) + (0.008931684*matrix_pred_genetica_step1[i,"RegiaoEuropeia_Et2"]) + (-0.342930108*matrix_pred_genetica_step1[i,"Brasil_Bt2"]) + (-0.035450084*matrix_pred_genetica_step1[i,"RegiaoAmericaCentral_Ct3"]) + (-9221.790023588*matrix_pred_genetica_step1[i,"AsiaH1_5"]) + (0.022042397*matrix_pred_genetica_step1[i,"RegiaoAmericaCentral_Ct2"]) + (0.002855475*matrix_pred_genetica_step1[i,"RegiaoAmericaNorte_At4"]) + (6482.736416212*matrix_pred_genetica_step1[i,"AsiaH1_4"]) + (0.026039381*matrix_pred_genetica_step1[i,"RegiaoAmericaSul_St2"])
}

# MSE
EQM_Stepwise1_genetica=numeric(11)
for(i in 1:11){
  EQM_Stepwise1_genetica[i] = mean((yhat_stepwise1_genetica[1:i]-matriz_ncont_2019["Brasil",1:i])^2)  
}
EQM_Stepwise1_genetica 

# Comparing MSE Stepwise 1 (Incidence vs. Genetic) 11 steps
EQM_Stepwise1=as.data.frame(EQM_Stepwise1)
EQM_Stepwise1_genetica=as.data.frame(EQM_Stepwise1_genetica)
dados_steps1=cbind(EQM_Stepwise1,EQM_Stepwise1_genetica)
dados_steps1=as.data.frame(dados_steps1)

colors <- c("Incidence" = "orange1", "Genetic" = "springgreen2")

ggplot(dados_steps1, aes(x = c(1:11))) +
  geom_line(aes(y = EQM_Stepwise1, color = "Incidence"), size = 1) +
  geom_line(aes(y = EQM_Stepwise1_genetica, color = "Genetic"), size = 1) +
  labs(x = "Number of steps forward", y = "Mean squared error (MSE)", title = "Comparing MSE (Stepwise)",color = "Data") +
  scale_x_continuous(limit = c(1,11),
                     breaks = c(1,2,3,4,5,6,7,8,9,10,11),
                     labels = c("1","2","3","4","5","6","7","8","9","10","11")) +
  scale_color_manual(values = colors)

#########################################################################
#########################################################################

### Forecast plot with all three final models - LASSO 5, LASSO with CV and Stepwise 1


# Prediction 11 steps forward (no genetics) - starting in early 2019 (January 2019)

valores_verdadeiros_2019=valores_verdadeiros_2018[6:16] # dados de janeiro de 2019 at?? novembro de 2019

valores_verdadeiros_2019=as.data.frame(valores_verdadeiros_2019)
yhat_lasso5_mediasBR=as.data.frame(yhat_lasso5_mediasBR)
yhat_modelo_lassocv=as.data.frame(yhat_modelo_lassocv)
yhat_stepwise1_mediasBR=as.data.frame(yhat_stepwise1_mediasBR)
dados_1passo_semgen_correcao=c(valores_verdadeiros_2019,yhat_lasso5_mediasBR,yhat_modelo_lassocv,yhat_stepwise1_mediasBR)
dados_1passo_semgen_correcao=as.data.frame(dados_1passo_semgen_correcao)

colors <- c("Observed" = "black", "LASSO 5" = "firebrick1","LASSO CV" = "dodgerblue","Stepwise" = "chartreuse3")

ggplot(dados_1passo_semgen_correcao, aes(x = c(1:11))) +
  geom_line(aes(y = valores_verdadeiros_2019, color = "Observed"), size = 0.4) +
  geom_line(aes(y = yhat_lasso5_mediasBR, color = "LASSO 5"), size = 0.5) +
  geom_line(aes(y = yhat_modelo_lassocv, color = "LASSO CV"), size = 0.5) +
  geom_line(aes(y = yhat_stepwise1_mediasBR, color = "Stepwise"), size = 0.5) +
  labs(x = "2019", y = "Number of positive cases", title = "Out-of-sample forecasts",color = "Models") +
  scale_x_continuous(limit = c(1,11),
                     breaks = c(1,2,3,4,5,6,7,8,9,10,11),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov")) +
  scale_color_manual(values = colors)
##############################

# Genetics 11 steps forward - starting in early 2019 (January 2019)

valores_verdadeiros_2019_genetica=valores_verdadeiros_genetica_2018[6:16] # dados de janeiro de 2019 at?? novembro de 2019

valores_verdadeiros_2019_genetica=as.data.frame(valores_verdadeiros_2019_genetica)
yhat_lasso5_genetica=as.data.frame(yhat_lasso5_genetica)
yhat_modelo_lassocv_genetica=as.data.frame(yhat_modelo_lassocv_genetica)
yhat_stepwise1_genetica=as.data.frame(yhat_stepwise1_genetica)
dados_1passo_comgen_correcao=c(valores_verdadeiros_2019_genetica,yhat_lasso5_genetica,yhat_modelo_lassocv_genetica,yhat_stepwise1_genetica)
dados_1passo_comgen_correcao=as.data.frame(dados_1passo_comgen_correcao)

colors <- c("Observed" = "black", "LASSO 5" = "firebrick1","LASSO CV" = "dodgerblue","Stepwise" = "chartreuse3")

ggplot(dados_1passo_comgen_correcao, aes(x = c(1:11))) +
  geom_line(aes(y = valores_verdadeiros_2019_genetica, color = "Observed"), size = 0.4) +
  geom_line(aes(y = yhat_lasso5_genetica, color = "LASSO 5"), size = 0.5) +
  geom_line(aes(y = yhat_modelo_lassocv_genetica, color = "LASSO CV"), size = 0.5) +
  geom_line(aes(y = yhat_stepwise1_genetica, color = "Stepwise"), size = 0.5) +
  labs(x = "2019", y = "Number of positive cases", title = "Out-of-sample forecasts",color = "Models") +
  scale_x_continuous(limit = c(1,11),
                     breaks = c(1,2,3,4,5,6,7,8,9,10,11),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov")) +
  scale_color_manual(values = colors)
#########################################################################

# Prediction 1 step forward (Incidence)

# Running the model again

treinoy=as.matrix(matriz_ncasos_defasagens_mediasBR[,13]) # variável resposta
treinox=as.matrix(matriz_ncasos_defasagens_mediasBR[,-13]) # variáveis independentes até a defasagem 11

set.seed(26787) # semente

## Perform lasso
lasso1 <- glmnet(x = treinox, y = treinoy,
                 alpha = 1) # ‘alpha = 1’ is the lasso penalty, and ‘alpha = 0’ the ridge penalty.
plot(lasso1, xvar = "lambda")

# Let's implement lasso with 111-fold CV below 
lasso1_cv <- cv.glmnet(x = treinox, y = treinoy, 
                       type.measure = "mse", 
                       nfold = 111,
                       alpha = 1) 
plot(lasso1_cv) # Penalty vs CV MSE plot

## Extract coefficients at the error-minimizing lambda
lasso1_cv$lambda.min
best_lasso_coef <- coef(lasso1_cv, s = lasso1_cv$lambda.min)
best_lasso_coef # LASSO incidence model - 13 covariates

# Models Out-of-sample
treinox_2019=as.matrix(matriz_ncasos_defasagens_mediasBR_2008_2019[,-13]) # covariáveis 
treinoy_2019=as.matrix(matriz_ncasos_defasagens_mediasBR_2008_2019[,13]) # variável resposta
preditos_lassocv_insample2019=predict(lasso1,s=18.110490,type="response",newx=treinox_2019) # LASSO CV, onde s= 18.110490 é o lambda mínimo encontrado pelo comando lasso1_cv$lambda.min


# Prediction charts 1 steps ahead (incidence data only):

# Prediction Lasso CV Incidence
colnames(preditos_lassocv_insample2019)="preditos_lassocv_insample2019"
preditos_lassocv_insample2019=data.frame(preditos_lassocv_insample2019)
treinoy_2019=data.frame(treinoy_2019)

ggplot(preditos_lassocv_insample2019, aes(x=c(1:122))) +
  geom_line(aes(y=preditos_lassocv_insample2019), color = "dodgerblue", size = 1)+
  geom_line(data = treinoy_2019, aes(x=c(1:122), y = treinoy_2019), color = "black", size=0.4) +
  labs(x = "Years", y = "Number of positive cases", title = "Predicted values (LASSO CV)")+
  scale_x_continuous(limit = c(0,122),
                     breaks = c(4,16,28,40,52,64,76,88,100,112),
                     labels = c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019")) +
  geom_vline(xintercept = 112, linetype='dashed', color = "black", size=0.5)
########

# MSE and MAPE (in-sample and out-of-sample)

# MSE In-sample
preditos_lassocv_insample=predict(lasso1,s=18.110490,type="response",newx=treinox) # Model LASSO CV - In-sample Models
mean((predict(lasso1,s=18.110490,type="response",newx=treinox) - treinoy)^2) # EQM Model lasso CV

# Mean absolute percentage error (MAPE) In-sample
(sum((abs((treinoy-preditos_lassocv_insample)/treinoy)))/111)*100 # mape model lasso CV

# MSE Out-of-sample
mean((predict(lasso1,s=18.110490,type="response",newx=treinox_2019)[112:122] - treinoy_2019[112:122,])^2) # EQM Model lasso cv

# Mean absolute percentage error (MAPE) Out-Sample
(sum((abs((treinoy_2019[112:122,]-preditos_lassocv_insample2019[112:122,])/treinoy_2019[112:122,])))/11)*100 # mape model lasso cv

##########################################

# Prediction 1 step forward (Genetic)

# Running the model again

treinoy=as.matrix(matriz_ncasos_defasagens_genetica[,1]) # variável resposta
treinox=as.matrix(matriz_ncasos_defasagens_genetica[,-1]) #variáveis independentes até a defasagem 6

set.seed(26787) # seed

## Perform lasso
lasso1 <- glmnet(x = treinox, y = treinoy,
                 alpha = 1) # ‘alpha = 1’ is the lasso penalty, and ‘alpha = 0’ the ridge penalty.
plot(lasso1, xvar = "lambda")

# Let's implement lasso with 117-fold CV below 
lasso1_cv <- cv.glmnet(x = treinox, y = treinoy, 
                       type.measure = "mse", 
                       nfold = 117, 
                       alpha = 1) 
plot(lasso1_cv) # Penalty vs CV MSE plot

# Extract coefficients at the error-minimizing lambda
lasso1_cv$lambda.min
best_lasso_coef <- coef(lasso1_cv, s = lasso1_cv$lambda.min)
best_lasso_coef # Genetic LASSO model - 15 covariates


# Models Out-of-sample
treinox_genetica_2019=as.matrix(matriz_ncasos_defasagens_genetica_2008_2019[,-1])
treinoy_genetica_2019=as.matrix(matriz_ncasos_defasagens_genetica_2008_2019[,1])
preditos_lassocv_insample2019_genetica=predict(lasso1,s=20.86147709,type="response",newx=treinox_genetica_2019) # LASSO CV, onde s= 20.86147709 é o lambda mínimo encontrado pelo comando lasso1_cv$lambda.min e nele vemos que temos 15 variáveis explicativas. Ademais type="response" se refere a fazer a previsão dentro da amostra, lasso1 é o modelo do lasso ajustado, e newx são as variáveis explicativas da amostra (2008 até 2019)


# Prediction charts 1 steps ahead (incidence and genetic data):

# Prediction Lasso CV Incidence
colnames(preditos_lassocv_insample2019_genetica)="preditos_lassocv_insample2019_genetica"
preditos_lassocv_insample2019_genetica=data.frame(preditos_lassocv_insample2019_genetica)
treinoy_genetica_2019=data.frame(treinoy_genetica_2019)

ggplot() +
  geom_line(data = preditos_lassocv_insample2019_genetica, aes(x=c(1:125), y = preditos_lassocv_insample2019_genetica), color = "dodgerblue", size = 1) +
  geom_line(data = treinoy_genetica_2019, aes(x=c(1:125), y = treinoy_genetica_2019), color = "black", size=0.4) +
  labs(x = "Years", y = "Number of positive cases", title = "Predicted values (LASSO CV)")+
  scale_x_continuous(limit = c(0,125),
                     breaks = c(10,22,34,46,58,70,82,94,106,118),
                     labels = c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019")) +
  geom_vline(xintercept = 118, linetype='dashed', color = "black", size=0.5)
########

# MSE and MAPE (in-sample and out-of-sample)

# MSE In-sample
mean((predict(lasso1,s=20.86147709,type="response",newx=treinox_genetica_2019)[1:117] - treinoy_genetica_2019[1:117,])^2) # EQM Modelo lasso cv

# MSE Out-of-sample
mean((predict(lasso1,s=20.86147709,type="response",newx=treinox_genetica_2019)[118:125] - treinoy_genetica_2019[118:125,])^2) # EQM Modelo lasso cv

# Mean absolute percentage error (MAPE) In-sample
(sum((abs((treinoy_genetica_2019[1:117,]-predict(lasso1,s=20.86147709,type="response",newx=treinox_genetica_2019)[1:117])/treinoy_genetica_2019[1:117,])))/117)*100 # mape do modelo lasso cv

# Mean absolute percentage error (MAPE) Out-Sample
(sum((abs((treinoy_genetica_2019[118:125,]-predict(lasso1,s=20.86147709,type="response",newx=treinox_genetica_2019)[118:125])/treinoy_genetica_2019[118:125,])))/8)*100 # mape do modelo lasso cv
########

# Graph comparing 1 step forward

df3=data.frame()
df3[c(1,3,5),"Data"]="Incidence"
df3[c(2,4,6),"Data"]="Genetic"
df3[c(1,2),"Models"]="LASSO 5"
df3[c(3,4),"Models"]="LASSO CV"
df3[c(5,6),"Models"]="Stepwise"
df3[1:6,"MSEx10000"]=c(21805.29/10000,27781.20/10000,25860.88/10000, 36757.65/10000,43228.16/10000,47623.36/10000)

library(scales)
ggplot(data=df3,aes(x=Models, y=MSEx10000, fill=Data)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_y_continuous(limit=c(0,5)) +
  scale_fill_manual(values=c('dodgerblue','firebrick2'))
#########################################################################
#########################################################################

### Residual Analysis


# Incidence

# Running the model again

treinoy=as.matrix(matriz_ncasos_defasagens_mediasBR[,13]) # variável resposta
treinox=as.matrix(matriz_ncasos_defasagens_mediasBR[,-13]) # variáveis independentes até a defasagem 11

set.seed(26787) # semente

## Perform lasso
lasso1 <- glmnet(x = treinox, y = treinoy,
                 alpha = 1) # ‘alpha = 1’ is the lasso penalty, and ‘alpha = 0’ the ridge penalty.
plot(lasso1, xvar = "lambda")

# Let's implement lasso with 111-fold CV below 
lasso1_cv <- cv.glmnet(x = treinox, y = treinoy, 
                       type.measure = "mse", 
                       nfold = 111,
                       alpha = 1) 
plot(lasso1_cv) # Penalty vs CV MSE plot

## Extract coefficients at the error-minimizing lambda
lasso1_cv$lambda.min
best_lasso_coef <- coef(lasso1_cv, s = lasso1_cv$lambda.min)
best_lasso_coef # LASSO incidence model - 13 covariates

# Residuals LASSO CV
preditos_lassocv_insample=predict(lasso1,s=18.110490,type="response",newx=treinox) # Model LASSO CV - In-sample Models
res_lassocv=treinoy-preditos_lassocv_insample
plot.ts(res_lassocv)
Box.test(res_lassocv, lag=20, type="L") # p-value = 0.8212156
shapiro.test(res_lassocv) # p-value = 0.0000000002679416
hist(res_lassocv, breaks=20)
boxplot(res_lassocv)

lassocv_ind= which(abs(res_lassocv)<400)
shapiro.test(res_lassocv[lassocv_ind]) # teste de normalidade sem os 5 outliers
hist(res_lassocv[lassocv_ind], breaks=20) # histograma sem os 5 outliers
which(abs(res_lassocv)>400) # esses são os 5 outliers (44,  78,  79,  80 , 103)
##########################################

# Models with genetic:

# Running the model again

treinoy=as.matrix(matriz_ncasos_defasagens_genetica[,1]) # variável resposta
treinox=as.matrix(matriz_ncasos_defasagens_genetica[,-1]) #variáveis independentes até a defasagem 6

set.seed(26787) # seed

## Perform lasso
lasso1 <- glmnet(x = treinox, y = treinoy,
                 alpha = 1) # ‘alpha = 1’ is the lasso penalty, and ‘alpha = 0’ the ridge penalty.
plot(lasso1, xvar = "lambda")

# Let's implement lasso with 117-fold CV below 
lasso1_cv <- cv.glmnet(x = treinox, y = treinoy, 
                       type.measure = "mse", 
                       nfold = 117, 
                       alpha = 1) 
plot(lasso1_cv) # Penalty vs CV MSE plot

## Extract coefficients at the error-minimizing lambda
lasso1_cv$lambda.min
best_lasso_coef <- coef(lasso1_cv, s = lasso1_cv$lambda.min)
best_lasso_coef # Genetic LASSO model - 15 covariates

# Residuals LASSO CV Genetic
res_lassocv_gen=treinoy_genetica_2019-preditos_lassocv_insample2019_genetica
res_lassocv_gen=as.matrix(res_lassocv_gen)
plot.ts(res_lassocv_gen)
abline(h=400,col="red")
abline(h=-400,col="red")
Box.test(res_lassocv_gen, lag=20, type="L") # p-value = 0.4770
shapiro.test(res_lassocv_gen) # p-value = 0.0000000001384
hist(res_lassocv_gen, breaks=20)
boxplot(res_lassocv_gen)
mean(res_lassocv_gen)
median(res_lassocv_gen)

lassocv_ind_gen= which(abs(res_lassocv_gen)<400)
shapiro.test(res_lassocv_gen[lassocv_ind_gen]) # teste de normalidade sem os 6 outliers
hist(res_lassocv_gen[lassocv_ind_gen], breaks=30) # histograma sem os 5 outliers
which(abs(res_lassocv_gen)>400) # esses são os 6 outliers (50,  51, 84,  85,  86 , 109)
####################################################################################


## Residuals Graphs for the Stepwise Model

library(nortsTest)
library(lmtest)

# i) Incidence

matriz_ncasos_defasagens_mediasBR=as.data.frame(matriz_ncasos_defasagens_mediasBR)
attach(matriz_ncasos_defasagens_mediasBR)
aToEnter <- 0.10
aToLeave <- 0.10
result <- stepwise(Brasil_Bt ~ 1 + Brasil_Bt1 + Brasil_Bt2 + Brasil_Bt3 + Brasil_Bt4 + Brasil_Bt5 + Brasil_Bt6 + Brasil_Bt7 + Brasil_Bt8 + Brasil_Bt9 + Brasil_Bt10 + Brasil_Bt11 + RegiaoAmericaNorte_At1 + RegiaoAmericaNorte_At2 + RegiaoAmericaNorte_At3 + RegiaoAmericaNorte_At4 + RegiaoAmericaNorte_At5 + RegiaoAmericaNorte_At6 + RegiaoAmericaNorte_At7 + RegiaoAmericaNorte_At8 + RegiaoAmericaNorte_At9 + RegiaoAmericaNorte_At10 + RegiaoAmericaNorte_At11 + RegiaoAmericaSul_St1 + RegiaoAmericaSul_St2 + RegiaoAmericaSul_St3 + RegiaoAmericaSul_St4 + RegiaoAmericaSul_St5 + RegiaoAmericaSul_St6 + RegiaoAmericaSul_St7 + RegiaoAmericaSul_St8 + RegiaoAmericaSul_St9 + RegiaoAmericaSul_St10 + RegiaoAmericaSul_St11 + RegiaoAmericaCentral_Ct1 + RegiaoAmericaCentral_Ct2 + RegiaoAmericaCentral_Ct3 + RegiaoAmericaCentral_Ct4 + RegiaoAmericaCentral_Ct5 + RegiaoAmericaCentral_Ct6 + RegiaoAmericaCentral_Ct7 + RegiaoAmericaCentral_Ct8 + RegiaoAmericaCentral_Ct9 + RegiaoAmericaCentral_Ct10 + RegiaoAmericaCentral_Ct11 + RegiaoEuropeia_Et1 + RegiaoEuropeia_Et2 + RegiaoEuropeia_Et3 + RegiaoEuropeia_Et4 + RegiaoEuropeia_Et5 + RegiaoEuropeia_Et6 + RegiaoEuropeia_Et7 + RegiaoEuropeia_Et8 + RegiaoEuropeia_Et9 + RegiaoEuropeia_Et10 + RegiaoEuropeia_Et11 + RegiaoSouthAsia_st1 + RegiaoSouthAsia_st2 + RegiaoSouthAsia_st3 + RegiaoSouthAsia_st4 + RegiaoSouthAsia_st5 + RegiaoSouthAsia_st6 + RegiaoSouthAsia_st7 + RegiaoSouthAsia_st8 + RegiaoSouthAsia_st9 + RegiaoSouthAsia_st10 + RegiaoSouthAsia_st11 + RegiaoWesternPacific_Wt1 + RegiaoWesternPacific_Wt2 + RegiaoWesternPacific_Wt3 + RegiaoWesternPacific_Wt4 + RegiaoWesternPacific_Wt5 + RegiaoWesternPacific_Wt6 + RegiaoWesternPacific_Wt7 + RegiaoWesternPacific_Wt8 + RegiaoWesternPacific_Wt9 + RegiaoWesternPacific_Wt10 + RegiaoWesternPacific_Wt11 + mediasBR_formatodefasagem, Brasil_Bt ~ 1, aToEnter, aToLeave)

# Residuals plot
plot.ts(result$residuals, ylab="Residuals", main="Incidence",font.main=1)

# Fitted values vs observed plot
plot(fitted(result),Brasil_Bt,xlab="Fitted Values",ylab="Observed", main="Incidence",font.main=1)
lines(c(-100,2000),c(-100,2000),col="red")

# Heteroscedastic tests
Lm.test(y=result$residuals,lag.max=2,alpha=0.05)
bptest(result)

# ii) Genetic

matriz_ncasos_defasagens_genetica=as.data.frame(matriz_ncasos_defasagens_genetica)
attach(matriz_ncasos_defasagens_genetica)
aToEnter <- 0.10
aToLeave <- 0.10
result <- stepwise(Brasil_Bt ~ 1 + Brasil_Bt1 + Brasil_Bt2 + Brasil_Bt3 + Brasil_Bt4 + Brasil_Bt5 + Brasil_Bt6 + RegiaoAmericaNorte_At1 + RegiaoAmericaNorte_At2 + RegiaoAmericaNorte_At3 + RegiaoAmericaNorte_At4 + RegiaoAmericaNorte_At5 + RegiaoAmericaNorte_At6 + RegiaoAmericaSul_St1 + RegiaoAmericaSul_St2 + RegiaoAmericaSul_St3 + RegiaoAmericaSul_St4 + RegiaoAmericaSul_St5 + RegiaoAmericaSul_St6 + RegiaoAmericaCentral_Ct1 + RegiaoAmericaCentral_Ct2 + RegiaoAmericaCentral_Ct3 + RegiaoAmericaCentral_Ct4 + RegiaoAmericaCentral_Ct5 + RegiaoAmericaCentral_Ct6 + RegiaoEuropeia_Et1 + RegiaoEuropeia_Et2 + RegiaoEuropeia_Et3 + RegiaoEuropeia_Et4 + RegiaoEuropeia_Et5 + RegiaoEuropeia_Et6 + RegiaoSouthAsia_st1 + RegiaoSouthAsia_st2 + RegiaoSouthAsia_st3 + RegiaoSouthAsia_st4 + RegiaoSouthAsia_st5 + RegiaoSouthAsia_st6 + RegiaoWesternPacific_Wt1 + RegiaoWesternPacific_Wt2 + RegiaoWesternPacific_Wt3 + RegiaoWesternPacific_Wt4 + RegiaoWesternPacific_Wt5 + RegiaoWesternPacific_Wt6 + mediasBR + AllH1_1 + AllH1_2 + AllH1_3 + AllH1_4 + AllH1_5 + AllH1_6 + AllH3_1 + AllH3_2 + AllH3_3 + AllH3_4 + AllH3_5 + AllH3_6 + NorthAmericaH1_1 + NorthAmericaH1_2 + NorthAmericaH1_3 + NorthAmericaH1_4 + NorthAmericaH1_5 + NorthAmericaH1_6 + NorthAmericaH3_1 + NorthAmericaH3_2 + NorthAmericaH3_3 + NorthAmericaH3_4 + NorthAmericaH3_5 + NorthAmericaH3_6 + AsiaH1_1 + AsiaH1_2 + AsiaH1_3 + AsiaH1_4 + AsiaH1_5 + AsiaH1_6 + AsiaH3_1+ AsiaH3_2 + AsiaH3_3 + AsiaH3_4 + AsiaH3_5 + AsiaH3_6, Brasil_Bt ~ 1, aToEnter, aToLeave)

# Residuals plot
plot.ts(result$residuals,ylab="Residuals", main="Genetic",font.main=1)

# Fitted values vs observed plot
plot(fitted(result),Brasil_Bt,xlab="Fitted Values",ylab="Observed", xlim=c(-200,1600), main="Genetic",font.main=1)
lines(c(-100,2000),c(-100,2000),col="red")

# Heteroscedastic tests
Lm.test(y=result$residuals,lag.max=2,alpha=0.05)
bptest(result)

##############################################################################################################################
######################## AR MODEL ############################################################################################
##############################################################################################################################
 
# Autoregressive model

Bt=matriz_ncont_analise_genetica[,2]
mod=arima(Bt,order=c(2,0,0))
mod
lmtest::coeftest(mod)
Box.test(mod$residuals, lag=20, fitdf = 2)
resid = as.numeric(mod$residuals)
Box.test(resid^2, lag=20, fitdf = 2)
pred = as.numeric(predict(mod,n.ahead = 11)$pred)
to.forecast = t(matriz_ncont_2019)[,2]
mean((pred-to.forecast)^2)
plot.ts(to.forecast)
lines(pred)
### Genetic data
names(mod)
fitted = Bt - resid
plot.ts(Bt)
lines(fitted, col="red")
mean((fitted-Bt)^2)

library(ggplot2)

mbt = cbind(as.numeric(to.forecast),pred)
colnames(mbt)=c("Observed", "Forecasted")
row.names(mbt)=NULL

mbt=as.data.frame(mbt)

colors = c("Observed"="black","Forecasted"="red") 
s = ggplot(mbt,aes(x=1:11)) +
  geom_line(aes(y = Observed,color = "Observed"), size = 0.4) +
  geom_line(aes(y = Forecasted, color = "Forecasted"), size = 0.5) +
  labs(x = "2019", y = "Number of positive cases",color="Legend", 
       title = "Out-of-sample forecasts for the AR(2) model", size=2) +
  scale_x_continuous(limit = c(1,11),
                     breaks = c(1,2,3,4,5,6,7,8,9,10,11),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov")) +
  scale_color_manual(values = colors) 
s

w2<-6
h2<-3.5

pdf(file = "forecast_AR2.pdf",width = w2, height = h2)
{
  s
}
