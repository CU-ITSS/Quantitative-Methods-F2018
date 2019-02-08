##########################################################################
#	R & statnet (brief) Tutorial for IBS 2018
#	author: jimi adams
#	last updated: 2018-11-28
#
#	For a more complete tutorial, see the enclosed EPIC tutorial.
##########################################################################

##########################################################################
#	0.1 	Setting up your R environment
##########################################################################
# This is the main package (along w/ its dependencies) we're using for analyses and visualizations here.
#install.packages("statnet")			# The first time you install a package
library(statnet)						      # Attach the library to your session

# These 2 are merely to present some of the results in more readable graphics (code works w/o)
#install.packages("coda")
library(coda)
#install.packages("latticeExtra")
library(latticeExtra)

##########################################################################
#	0.2 	Loading our data
##########################################################################
# Chris Marcum produced a single R environment w/ a # of datasets
# https://github.com/cmarcum/TVHookupNetworks/blob/master/TvContacts.Rdata
load("tvrels.Rdata")
ls()  # what's in here?

# Greys Anatomy data come from: Lind, B. 2012. Bad Hessian Blog. https://goo.gl/oJsZpH
# Glee data come from: adams, j. 2015. Network Science 3(2): 293-295.
# L-Word data come from: Marcum, CS et al. 2016. Network Science 4(3): 400-405.

##########################################################################
#	1. 	Exploring the data
##########################################################################
la <- plot(greys)		#fix plot coordinates, so they don't jump around if we replot later
plot(greys, coord=la, vertex.col=get.vertex.attribute(greys, "nrace"), 		# Numeric converstion of race variable
     label=get.vertex.attribute(greys, "name"), label.cex=.75, 		# Names
     vertex.sides=(50*(get.vertex.attribute(greys, "sex")=="F")+4), 	# circles=women, squares=men
     vertex.cex=.05*(2013-get.vertex.attribute(greys, "birthyear")))	# size proportional to age

##########################################################################
# 2.	Model building and fitting in a (somewhat) systematic way
#     See lecture slides for the logic of these model building steps.
##########################################################################
# Bernoulli
gr.bern <- ergm(greys~edges) 				# Bernoulli 
summary(gr.bern)
exp(gr.bern$coef)       # These are log-odds so we can exponentiate for OR interpretation
plot(simulate(gr.bern))			# What does a simulated network of that model look like?
gr.bern.gof <- gof(gr.bern)					# Estimating Goodness of Fit with default set of diagnostics
summary(gr.bern.gof) 						# Summarize the goodness of fit
par(mfrow=c(1,3)); plot(gr.bern.gof)  # just adjusting the layout to allow for 3 side-by-side images
par(mfrow=c(1,1))   # returning the graphics handler to normal

# Sex heterophily
gr.het <- ergm(greys~edges+nodematch("sex")) 	# Estimate the model, w/ sex-match estimation
plot(simulate(gr.het), vertex.cex=1.5,			# Take a look
     vertex.sides=(50*(get.vertex.attribute(greys, "sex")=="F")+3))						
gr.het.gof <- gof(gr.het)						# Estimating Goodness of Fit with default set of diagnostics
summary(gr.het.gof) 							# Summarize the goodness of fit
par(mfrow=c(1,3)); plot(gr.het.gof)
par(mfrow=c(1,1))
summary(gr.het) 								# Summarize the model

# No Isolates
gr.het_noisol <- ergm(greys~edges+nodematch("sex")+degree(1)) 	# Estimate the model, adding isolation constraint
summary(gr.het_noisol)
plot(simulate(gr.het_noisol), vertex.cex=1.5,	# Take a look
     vertex.sides=(50*(get.vertex.attribute(greys, "sex")=="F")+4))						
gr.het_noisol.gof<-gof(gr.het_noisol)			# Estimating Goodness of Fit with default set of diagnostics
summary(gr.het_noisol.gof) 						# Summarize the goodness of fit
par(mfrow=c(1,3)); plot(gr.het_noisol.gof)
par(mfrow=c(1,1))
summary(gr.het_noisol) 							# Summarize the model
mcmc.diagnostics(gr.het_noisol)

# Respecifying the same model, with some additional details fed into the MCMCMLE
gr.het_noisol<-ergm(greys~edges+nodematch("sex")+degree(1), control=control.ergm(MCMC.burnin=50000, MCMC.interval=5000))
summary(gr.het_noisol)							# no real change here
mcmc.diagnostics(gr.het_noisol)					# some here

# Age Assortativity
gr.het_noisol_age<-ergm(greys~edges+nodematch("sex")+degree(1)+absdiff("birthyear"), control=control.ergm(MCMC.burnin=50000, MCMC.interval=5000))
plot(simulate(gr.het_noisol_age), vertex.sides=(50*(get.vertex.attribute(greys, "sex")=="F")+4), 
     vertex.cex=.05*(2013-get.vertex.attribute(greys, "birthyear")))	
gr.het_noisol_age.gof<-gof(gr.het_noisol_age)			# Estimating Goodness of Fit with default set of diagnostics
summary(gr.het_noisol_age.gof) 						# Summarize the goodness of fit
par(mfrow=c(1,3)); plot(gr.het_noisol_age.gof)
par(mfrow=c(1,1))
summary(gr.het_noisol_age) 							# Summarize the model
mcmc.diagnostics(gr.het_noisol_age)

# Race Homophily
gr.het_noisol_age_race<-ergm(greys~edges+nodematch("sex")+degree(1)+absdiff("birthyear")+nodefactor("race")+nodematch("race"), control=control.ergm(MCMC.burnin=50000, MCMC.interval=5000))
plot(simulate(gr.het_noisol_age_race), vertex.col=get.vertex.attribute(greys, "nrace"), 
     vertex.sides=(50*(get.vertex.attribute(greys, "sex")=="F")+4), 
     vertex.cex=.05*(2013-get.vertex.attribute(greys, "birthyear")))	
gr.het_noisol_age_race.gof<-gof(gr.het_noisol_age_race)			# Estimating Goodness of Fit with default set of diagnostics
summary(gr.het_noisol_age_race.gof) 						# Summarize the goodness of fit
par(mfrow=c(1,3)); plot(gr.het_noisol_age_race.gof)
par(mfrow=c(1,1))
summary(gr.het_noisol_age_race) 							# Summarize the model
mcmc.diagnostics(gr.het_noisol_age_race)

# Race Homophily, allowing it to differ by race
gr.het_noisol_age_race1<-ergm(greys~edges+nodematch("sex")+degree(1)+absdiff("birthyear")+nodematch("race", diff=TRUE), control=control.ergm(MCMC.burnin=50000, MCMC.interval=5000))
plot(simulate(gr.het_noisol_age_race1), vertex.col=get.vertex.attribute(greys, "nrace"), 
     vertex.sides=(50*(get.vertex.attribute(greys, "sex")=="F")+4), 
     vertex.cex=.05*(2013-get.vertex.attribute(greys, "birthyear")))	
gr.het_noisol_age_race1.gof<-gof(gr.het_noisol_age_race1)			# Estimating Goodness of Fit with default set of diagnostics
summary(gr.het_noisol_age_race1.gof) 						# Summarize the goodness of fit
par(mfrow=c(1,3)); plot(gr.het_noisol_age_race1.gof)
par(mfrow=c(1,1))
summary(gr.het_noisol_age_race1) 							# Summarize the model
mcmc.diagnostics(gr.het_noisol_age_race1)

# Above, dropping agediff
gr.het_noisol_race<-ergm(greys~edges+nodematch("sex")+degree(1)+nodematch("race"), control=control.ergm(MCMC.burnin=50000, MCMC.interval=5000))
plot(simulate(gr.het_noisol_race), vertex.col=get.vertex.attribute(greys, "nrace"), 
     vertex.sides=(50*(get.vertex.attribute(greys, "sex")=="F")+4))	
gr.het_noisol_race.gof<-gof(gr.het_noisol_race)
summary(gr.het_noisol_race.gof) 				
par(mfrow=c(1,3)); plot(gr.het_noisol_race.gof)
par(mfrow=c(1,1))
summary(gr.het_noisol_race) 							
mcmc.diagnostics(gr.het_noisol_race)

##########################################################################
# 3.	Model presented in Glee paper
#  model.4 is what's presented in the paper (though they've since corrected a bug in the code)
##########################################################################
model.4<- ergm(glee ~ edges + nodematch("sex") + degree(1) + degree(2) + gwnsp(decay = 0, fixed = TRUE) + threepath)
summary(model.4)
plot(gof(model.4))  # These are a really informative example
mcmc.diagnostics(model.4)
