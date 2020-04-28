library(shinydashboard, quietly=TRUE, warn.conflicts=FALSE)
library(logging, quietly=TRUE, warn.conflicts=FALSE)
library(magrittr, quietly=TRUE, warn.conflicts=FALSE)
basicConfig(level='DEBUG')
today=Sys.Date()
addHandler(writeToFile, file=paste0(getwd(), "/", format(today, "%y%m%d"), ".log"), level='FINEST')
LOGGER.SHINY_SERVER="com.vongo.suriv.webview.sever"
source("./functions.R", chdir=T)

loginfo("Loading server code…", logger=LOGGER.SHINY_SERVER)

shinyServer(function(input, output, session) {
	##########
	#   UX   #
	##########
	# TUTORIAL
	steps <- reactive(
		data.frame(
		    element=c("#ft_plot", "#ft_box_p1", "#ft_box_p2", "#ft_box_p3"),
		    intro=c(
				"En haut de votre écran, s'affiche le résulat de vos manipulations. Commme le graphique animé est plus long à générer, par souci d'efficacité, je vous conseille de faire votre paramétrage en mode statique, et de n'activer le mode dynamique que pour les résultats que vous voulez sauvegarder.",
				"Ici, on règle les principaux paramètres épidémiologiques. Si le sens de certains paramètres vous échappe, je vous conseille de retourner voir l'excellente vidéo de la chaîne 'Les Stats ? Même pas mal !', en suivant le lien : https://www.youtube.com/watch?v=KDG6dm9bSyw",
				"Vous pouvez également paramétrer les mesures prises pour répondre à l'épidémie, et leur efficacité. Toutes les dates sont exprimées en nombre de jours depuis l'origine. Les 'effets' sont exprimés entre 0 (efficacité totale) et 1 (aucune incidence).",
				"Enfin, vous pouvez paramétrer diverses choses concernant l'environnement (population, capacité de soin, etc.), la maladie, ou votre simulation. C'est notamment ici que vous pouvez basculer entre les modes statique et dynamique ('graphique animé')."
			),
		    position=c("bottom", "right", "top", "left")
		)
 	)
	observeEvent(input$mh.help,
       introjs(session,
			options=list(steps=steps(), "nextLabel"="Suivant", "prevLabel"="Précédent", "skipLabel"="Passer"),
        	events=list("oncomplete"=I('alert("Bonne exploration !")'))
		)
  	)

	########################
	#    FT : FirstTab     #
	########################
	df <- reactive(
		covid19simu(
			r0=input$ft_r0,
			tauxMortalite=input$ft_tauxMortalite,
			ratioIncubationMaladie=input$ft_ratioIncubationMaladie,
			maladesNonImmunises=input$ft_maladesNonImmunises,
			dureeIncubation=input$ft_dureeIncubation,
			dureeMaladie=input$ft_dureeMaladie,
			datesConfines1=input$ft_datesConfines1 %>% {seq(.[1], .[2])},
			effetConfines1=input$ft_effetConfines1,
			datesConfines2=input$ft_datesConfines2 %>% {seq(.[1], .[2])},
			effetConfines2=input$ft_effetConfines2,
			dateComportement=input$ft_dateComportement,
			effetComportement=input$ft_effetComportement,
			pourcentMaladesCritiques=input$ft_pourcentMaladesCritiques,
			nombreDePlaces=input$ft_nombreDePlaces,
			tauxMortaliteNonSoignes=input$ft_tauxMortaliteNonSoignes,
			populationTotale=input$ft_populationTotale,
			dureeSimulation=input$ft_dureeSimulation
		)
	)
	output$ft_main_plot_ui <- renderUI({
		if (input$ft_animate=="Oui") {
			imageOutput("ft_main_plot_gif", width="auto", height="auto", inline=TRUE) %>% withSpinner()
		} else {
			plotOutput("ft_main_plot") %>% withSpinner()
		}
	})
	output$ft_main_plot <- renderPlot({
		affich(df=df(), intervalle=c(50, input$ft_dureeSimulation))
	})
	output$ft_main_plot_gif <- renderImage({
		filename <- paste0(getwd(), "/", session$token, ".gif")
		saveGIF(
			graph.ani(df(), intervalle=c(50, input$ft_dureeSimulation)),
			interval=0.1,
 			movie.name=filename,
			ani.width=776,
			ani.height=480,
			loop=1
		)
		list(src=filename,
			contentType='image/gif',
			width=776,
			height=480
		)
	})

	##########
	# OTHERS #
	##########
	output$about <- renderUI({
		tagList(
			"Cet outil est une couche de présentation entièrement basée sur le travail de Christophe Genolini, à qui tout le crédit revient.",
			br(),
			"Pour plus de détails, reportez-vous à cette ",
			a("vidéo", href="https://www.youtube.com/watch?v=KDG6dm9bSyw")
		)
	})
	loginfo("Session started [%s]…", session$token, logger=LOGGER.SHINY_SERVER)
})
