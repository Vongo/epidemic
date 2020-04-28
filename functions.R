#### DISCLAIMER ####
# Those functions were taken from https://gitlab.com/ChristopheGen/simucovid19/-/tree/master
# This content was unlicensed at the time I copied it. Please check if it's still the case before you use it.

library(animation)
### J'appelle "incubes" un patient qui a le virus (et donc il est potentiellement contagieux)
###   mais qui n'est pas encore malade.
### Un malade est une personne qui dévellope la maladie.
### terminees sont les personnes sortiesd'affaire, qu'elles soient gueries ou mortes.
### deces est le nombre de décédé.
### rQuotidienMalades est le r pour les malades, par jour.
### rContagieuxest le r pour les personnes qui sont contagieuses, mais pas encore malade, par jour
### total est le nombre de personnes qui ne peuvent plus être contaminées
###    Autrement dit, Total = Contagieux + malades + termines
covid19simu <-function(
	r0=5.7,
	tauxMortalite=0.01,
	ratioIncubationMaladie=0.5,
	maladesNonImmunises=0.01,
	dureeIncubation=c(1,9),
	dureeMaladie=c(5,14),
	datesConfines1,
	effetConfines1,
	datesConfines2,
	effetConfines2,
	dateComportement,
	effetComportement,
	pourcentMaladesCritiques=0.05,
	nombreDePlaces=15000,
	tauxMortaliteNonSoignes=0.04,
	populationTotale=67000000,
	dureeSimulation=500
){
	#############
	### Création du vecteur confines. Une valeur pour chaque jour
	### Un confinement de 1 indique une absence de confinement
	### 0.25 indique que les interactions sociale ont été réduite de 75% (et donc 25% de contamination)
	confines <-rep(1,dureeSimulation)
	if(!missing(datesConfines1)){
		confines[datesConfines1] <-effetConfines1
	}
	if(!missing(datesConfines2)){
		confines[datesConfines2] <-effetConfines2
	}
	#############
	### Création du vecteur comportement. Une valeur pour chaque jour
	### Un comportement de 1 indique qu'on ne change rien
	### 0.25 indique qu'on a 4 fois moins de chance de contaminer qu'avant
	comportement <-rep(1,dureeSimulation)
	if(!missing(dateComportement)){
		comportement[dateComportement:dureeSimulation] <-effetComportement
	}
	#############
	### Calcul du r0 quotidien des malades et celui des incubés
	### Le système d'équation à résoudre est :
	###   r0Malade + r0Incubes = r0
	###   r0Malade = r0quotidien*dureeMalade
	###   r0Incubes = r0quotidien*dureeIncubes
	###   r0Incubes / r0Malades = ratioIncubationMaladie
	(r0quotidienMalades <-r0/(mean(dureeIncubation)*ratioIncubationMaladie + mean(dureeMaladie) ))
	(r0quotidienIncubes <-ratioIncubationMaladie*r0quotidienMalades)
	#############
	### Création des différents états qui vont évoluer.
	sains <- incubes <- malades <- termines <- rQuotidienIncubes <-rQuotidienMalades <-maladesCritiques <-decesJour <- r <-numeric(dureeSimulation)

	#############
	#### Pour simplifier, on considère que
	### -chaque jour, un incubé a 1/(duree de l'incubation) chance de basculer à malade
	### -chaque jour, un malade a 1/(duree de la maladie) chance de basculer à terminé
	### -chaque jour, un malade a une probabilité de (taux de mortalité)/(duree de la maladie) de déceder
	### -cette dernière probabilité varie selon que le malade est correctement soigné ou pas.
	probaIncubes_Malades <-1/mean(dureeIncubation)
	probaMalades_Termines <-1/mean(dureeMaladie)
	probaDeces <-tauxMortalite/mean(dureeMaladie)
	probaDecesNonSoignes <-tauxMortaliteNonSoignes/mean(dureeMaladie)
	#############
	### Calcul de la probabilité de deces :
	###    Si lamortalité est de x% et que la probabilité d'être malade critique est de y%,
	###    alors la mortalité des malades critique est de y/x
	probaDecesMaladesCritiques <-probaDeces / pourcentMaladesCritiques
	probaDecesMaladesCritiquesNonSoignes <-probaDecesNonSoignes / pourcentMaladesCritiques
		# # # # # # # # # # # # # # # # # # # #
	## # # Initialisation de l'algorithme  # # ##
		# # # # # # # # # # # # # # # # # # #

	#############
	#### On commence avec un unique incubé, pas de malade, pas d'immunisé.
	incubes[1] <-1
	malades[1] <-0
	termines[1] <-0
	sains[1] <-populationTotale  -incubes[1] -malades[1] -termines[1]

	#############
	#### rQuotidienMalades et rQuotidienIncubes vont évoluer avec le temps, ils sont pondérés par la taille de la population
	### Ils prennent également en compte le confinement et l'évolution du comportement
	rQuotidienMalades[1] <-r0quotidienMalades * sains[1]/populationTotale * confines[1]*comportement[1]
	rQuotidienIncubes[1] <-r0quotidienIncubes * sains[1]/populationTotale * confines[1]*comportement[1]

	#############
	#### r0 (global, pas quotidien) va évoluer avec le temps. On le calcule ici (pour affichage uniquement)
	r[1] <-rQuotidienIncubes[1]*mean(dureeIncubation)+rQuotidienMalades[1]*mean(dureeMaladie)
		# # # # # # # # # #
	## # # Algorithme  # # ##
		# # # # # # # # #
	#### i=2 et suivant
	for(i in 2:dureeSimulation){
		### Le nombre d'incubés total au jour i, c'est
		###    Les incubés de la veille
		###    plus ceux qui sont contaminés par des incubés
		###    plus ceux qui sont contaminés par des malades
		###    moins ceux qui tombent malades
		(incubes[i] <-incubes[i-1] + incubes[i-1]*rQuotidienIncubes[i-1] +malades[i-1]*rQuotidienMalades[i-1] -incubes[i-1]*probaIncubes_Malades)
		### Le nombre de malade total au jour i, c'est
		###Les malades de la veille
		###    plus ceux qui étaient incubés et qui sont tombés malades
		###    moins ceux qui passent terminé
		(malades[i] <-malades[i-1] +incubes[i-1]*probaIncubes_Malades -malades[i-1]*probaMalades_Termines)
		### Le nombre de terminés total au jour i, c'est
		###    Les terminés de la veille
		###    plus ceux qui étaient malades et qui passent terminé
		###    moins ceux qui étaient malades, qui passent terminé MAIS qui ne sont pas immunisés
		(termines[i] <-termines[i-1] +malades[i-1]*probaMalades_Termines -malades[i-1]*probaMalades_Termines*maladesNonImmunises)
		### Mise a jour de rQuotidienMalades et rQuotidienIncubes avec les données du jour
		(rQuotidienMalades[i] <-r0quotidienMalades * sains[i-1]/populationTotale * comportement[i]*confines[i])
		(rQuotidienIncubes[i] <-r0quotidienIncubes * sains[i-1]/populationTotale * comportement[i]*confines[i])
		(r[i] <-rQuotidienIncubes[i]*mean(dureeIncubation)+rQuotidienMalades[i]*mean(dureeMaladie))
		(sains[i] <-populationTotale -incubes[i] -malades[i] -termines[i])
		### Le nombre de malades critiques est un pourcentage du nombre de malademaladesCritiques[i] <-malades[i]*pourcentMaladesCritiques
		### Le nombre de deces est un pourcentage du nombre de malade pour les malades correctement soigné,
		### Le nombre de deces est un pourcentage du nombre de malade pour les malades correctement soigné,
		decesJour[i] <-ifelse(maladesCritiques[i-1]<=nombreDePlaces,
			maladesCritiques[i-1]*probaDecesMaladesCritiques,
			nombreDePlaces*probaDecesMaladesCritiques + (maladesCritiques[i-1]-nombreDePlaces)*probaDecesMaladesCritiquesNonSoignes)
	}
	deces <-cumsum(decesJour)
	df <-data.frame(incubes,malades,termines,sains,rQuotidienMalades,rQuotidienIncubes,deces,r)
	return(df)
}
	# # # # # # # # # # # # # #
## # # Affichage classique # # ##
	# # # # # # # # # # # # # #
affich <-function(df,intervalle){
	populationTotale <-sum(df[1,1:4])
	minIntervalle<-intervalle[1]
	maxIntervalle <-intervalle[2]
	xPolygon <-c(minIntervalle,minIntervalle:maxIntervalle,maxIntervalle)
	y <-c(0,df$malades[minIntervalle:maxIntervalle],0)
	i <-maxIntervalle
	plot(
		y=populationTotale[c(1,1)],
		x=c(minIntervalle,maxIntervalle),
		ylim=c(0,populationTotale),
		type="n",
		axes=FALSE,
		xlab="Jours",
		ylab="Population (en million)",
		main=paste("R0 =",round(df$r[1],2))
	)
	axis(1)
	axis(2,las=1,label=c(0,10,20,30,40,50,60),at=c(0,10,20,30,40,50,60)*1000000)
	box()
	xPolygon <-c(minIntervalle,minIntervalle:i,i)
	y=c(0,df$maladesl[minIntervalle:i],0)
	polygon(
		x=xPolygon,
		y=c(populationTotale,populationTotale-df$termines[minIntervalle:i],populationTotale),
		col=rgb(153,170,217,
		maxColorValue=255)
	)
	polygon(
		x=xPolygon,
		y=c(populationTotale,populationTotale-df$deces[minIntervalle:i],populationTotale),
		col=1
	)
	polygon(
		x=xPolygon,
		y=c(0,df$incubes[minIntervalle:i]+df$malades[minIntervalle:i],0),
		col=rgb(169,206,151,
		maxColorValue=255)
	)
	polygon(
		x=xPolygon,
		y=c(0,df$malades[minIntervalle:i],0),
		col=rgb(246,174,144,
		maxColorValue=255)
	)
}
	# # # # # # # # # # # #
## # # Affichage animé # # ##
	# # # # # # # # # # # #
graph.ani <-function(df,intervalle,nbFrame=10){
	populationTotale <-sum(df[1,1:4])
	minIntervalle <-intervalle[1]
	maxIntervalle <-intervalle[2]
	for(i in seq(minIntervalle,maxIntervalle,nbFrame)){
		p <- plot(
			y=populationTotale[c(1,1)],
			x=c(minIntervalle,maxIntervalle),
			ylim=c(0,populationTotale),
			type="n",
			axes=FALSE,
			xlab="Jours",
			ylab="Population (en million)",
			main=paste("R =",round(df$r[i],2),sep="")
		)
		axis(1)
		axis(2,las=1,label=c(0,10,20,30,40,50,60),at=c(0,10,20,30,40,50,60)*1000000)
		axis(2,las=1,label=c(0,25,50),at=c(0,25,50))
		axis(2,las=1,label=c(0,25,50,75,100),at=c(0,25000,50000,75000,100000))
		box()
		xPolygon <-c(minIntervalle,minIntervalle:i,i)
		polygon(
			x=xPolygon,
			y=c(populationTotale,populationTotale-df$termines[minIntervalle:i],populationTotale),
			col=rgb(153,170,217,
			maxColorValue=255)
		)
		polygon(x=xPolygon,y=c(populationTotale,populationTotale-df$deces[minIntervalle:i],populationTotale),col=1)
		polygon(x=xPolygon,y=c(0,df$incubes[minIntervalle:i]+df$malades[minIntervalle:i],0),col=rgb(169,206,151,maxColorValue=255))
		polygon(x=xPolygon,y=c(0,df$malades[minIntervalle:i],0),col=rgb(246,174,144,maxColorValue=255))
		p
	}
}
	# # # # # # # #
## # # Exemple # # ##
	# # # # # # # #
# df1 <-covid19simu(tauxMortalite=0.01,tauxMortaliteNonSoigne=0.01,r0=2.5,ratioIncubationMaladie=0.5)
# affich(df=df1,intervalle=c(120,250))
# saveGIF(graph.ani(df1, intervalle=c(120,250)), interval = 0.1 ,movie.name = "graphAnima1.gif",ani.width=776,ani.height=480,loop=1)
#
# df2 <-covid19simu(tauxMortalite=0.01,tauxMortaliteNonSoigne=0.01,r0=5.7,ratioIncubationMaladie=0.5)
# affich(df=df2,intervalle=c(50,130))
# saveGIF(graph.ani(df2, intervalle=c(50,130)),  interval = 0.1 ,movie.name = "graphAnima2.gif",ani.width=776,ani.height=480,loop=1)
#
# df3 <-covid19simu(tauxMortalite=0.01,tauxMortaliteNonSoigne=0.01,r0=1.4,ratioIncubationMaladie=0.5,dureeSimulation=1000)
# affich(df=df3,intervalle=c(400,700))
# saveGIF(graph.ani(df3, intervalle=c(400,700)), interval = 0.1 ,movie.name = "graphAnima3.gif",ani.width=776,ani.height=480,loop=1)
#
# df4 <-covid19simu(tauxMortalite=0.01,tauxMortaliteNonSoigne=0.01,r0=5.7,ratioIncubationMaladie=0.5,datesConfines1=65:125,effetConfines1=0.15)
# affich(df=df4,intervalle=c(50,220))
# saveGIF(graph.ani(df4, intervalle=c(50,220)),  interval = 0.1 ,movie.name = "graphAnima4.gif",ani.width=776,ani.height=480,loop=1)
#
# df5 <-covid19simu(tauxMortalite=0.01,tauxMortaliteNonSoigne=0.01,r0=5.7,ratioIncubationMaladie=0.5,datesConfines1=65:125,effetConfines1=0.15,dateComportement=125,effetComportement=0.3)
# affich(df=df5,intervalle=c(50,350))
# saveGIF(graph.ani(df5, intervalle=c(50,350)),  interval = 0.1 , movie.name = "graphAnima5.gif",ani.width=776,ani.height=480,loop=1)
