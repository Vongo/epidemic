library(shinydashboard, quietly=TRUE, warn.conflicts=FALSE)
library(logging, quietly=TRUE, warn.conflicts=FALSE)
library(shinyjqui, quietly=TRUE, warn.conflicts=FALSE)
library(rintrojs, quietly=TRUE, warn.conflicts=FALSE)
library(shinyWidgets, quietly=TRUE, warn.conflicts=FALSE)
library(shinycssloaders, quietly=TRUE, warn.conflicts=FALSE)
basicConfig(level='DEBUG')
today=Sys.Date()
addHandler(writeToFile, file=paste0(getwd(), "/", format(today, "%y%m%d"), ".log"), level='FINEST')
LOGGER.SHINY_UI="com.vongo.suriv.webview.ui"

loginfo("Loading ui code…", logger=LOGGER.SHINY_UI)

addResourcePath("static", paste0(getwd()))
title <- "Epidemic Simulator 0.1"

header <- dashboardHeader(titleWidth=370)
header$children[[2]]$children <- tagList(
	tags$a(href='https://www.youtube.com/watch?v=KDG6dm9bSyw', tags$img(src='static/virus.png', height='50', width='50')),
	title
)

shinyUI(dashboardPage(title=title,
	header,
	dashboardSidebar(
		introjsUI(),
		sidebarMenu(id="main",
			menuItem("Simulateur", tabName="ft", icon=icon("columns")),
			menuItem("À propos", tabName="ma", icon=icon("info-circle")),
			actionButton("mh.help", "Besoin d'aide ?")
		)
	),
	dashboardBody(
		tags$head(tags$link(rel="shortcut icon", href='static/virus.png')),
		tabItems(
			tabItem(tabName="ft",
				jqui_sortable(div(id="ft_options",
					box(id="ft_plot",
						width=12,
						uiOutput("ft_main_plot_ui")
					),
					box(title="Paramètres du virus", collapsible=FALSE, collapsed=FALSE, width=4, id="ft_box_p1",
						sliderInput("ft_r0", "r0", min=0.1, max=10, step=0.05, value=5.7),
						sliderInput("ft_tauxMortalite", "Taux de mortalité", min=0.005, max=0.1, step=0.005, value=0.01),
						sliderInput("ft_ratioIncubationMaladie", "Ratio de l'incubation de la maladie", min=0, max=1, step=, value=0.5),
						sliderInput("ft_maladesNonImmunises", "Malades non-immunisés", min=0, max=1, step=, value=0.01),
						sliderInput("ft_dureeIncubation", "Durée de l'incubation", min=1, max=15, step=, value=c(1,9)),
						sliderInput("ft_dureeMaladie", "Durée de la maladie", min=1, max=30, step=, value=c(5,14))
					),
					box(title="Réaction société", collapsible=FALSE, collapsed=FALSE, width=4, id="ft_box_p2",
						sliderInput("ft_datesConfines1", "Dates du premier confinement", min=1, max=500, step=1, value=c(1, 350)),
						sliderInput("ft_effetConfines1", "Effet du premier confinement", min=0.05, max=1, step=0.05, value=1),
						sliderInput("ft_datesConfines2", "Dates du second confinement", min=1, max=500, step=1, value=c(1, 350)),
						sliderInput("ft_effetConfines2", "Effet du second confinement", min=0.05, max=1, step=0.05, value=1),
						sliderInput("ft_dateComportement", "Date du changement de comportement", min=1, max=500, step=1, value=250),
						sliderInput("ft_effetComportement", "Effet du changement de comportement", min=0.05, max=1, step=0.05, value=1)
					),
					box(title="Options diverses", collapsible=FALSE, collapsed=FALSE, width=4, id="ft_box_p3",
						radioGroupButtons("ft_animate", "Graphique animé", c("Oui", "Non"), "Non", status="primary",
							checkIcon=list(yes=icon("ok", lib="glyphicon"), no=icon("remove", lib="glyphicon"))),
						sliderInput("ft_pourcentMaladesCritiques", "Part des malades critiques", min=0, max=0.2, step=0.01, value=0.05),
						sliderInput("ft_nombreDePlaces", "Nombre de places en réanimation", min=0, max=50000, step=500, value=15000),
						sliderInput("ft_tauxMortaliteNonSoignes", "Taux de mortalité des non-soignés", min=0.005, max=0.2, step=0.005, value=0.04),
						sliderInput("ft_populationTotale", "Population totale", min=1e6, max=15e7, step=1e6, value=67e6),
						sliderInput("ft_dureeSimulation", "Durée de la simulation", min=10, max=500, step=10, value=350)
					)
				))
			),
			tabItem(tabName="ma", htmlOutput("about"))
		)
	)
))
