## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(choroplethr)
library(choroplethrMaps)
library(maps)

ui <- dashboardPage( skin = "red",
	dashboardHeader(title = "FTIAC Analysis"),
	dashboardSidebar(
		sidebarMenu(
			menuItem("Introduction", tabName = "introduction", icon = icon("info-circle")),
			hr(),
			h4("Data Filters"),
			selectInput("college", "College:",
				    c("All Colleges" = "ALL",
				      "Arts & Sciences" = "AS",
				      "Business" = "BU",
				      "Education & Human Services" = "ED",
				      "Engineering Technology" = "TE",
				      "Health Professions" = "HP",
				      "Kendall" = "KE",
				      "University College" = "UN"
				      )),
			sliderInput("years", "Years:", min=2010, max=2018, value=c(2010,2018), step = 1, sep=NULL, round = FALSE,  ticks = TRUE, animate = FALSE),
			hr(),
			h4("County Maps"),
			menuItem("Students", tabName = "student1", icon = icon("file-image")),
			menuItem("Students/100,000", tabName = "student2", icon = icon("file-image")),
			menuItem("Retention", tabName = "retention", icon = icon("file-image")),
			menuItem("Mean HS GPA", tabName = "GPA", icon = icon("file-image")),
			menuItem("Mean ACT, Composite", tabName = "comp", icon = icon("file-image")),
			menuItem("Mean ACT, English", tabName = "engl", icon = icon("file-image")),
			menuItem("Mean ACT, Math", tabName = "math", icon = icon("file-image")),
			menuItem("Mean ACT, Reading", tabName = "read", icon = icon("file-image")),
			menuItem("Mean ACT, Science", tabName = "sci", icon = icon("file-image"))
		)
	),
	dashboardBody(
		# Setting the CSS of the dashboard to look prettier
		tags$head(
			tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
		),

		img(src = 'boxflame.png', align = "left"),
		titlePanel("  Ferris State University"),
		tabItems(
			# INTRODUCTION TAB
			tabItem(tabName = "introduction",
				fluidRow(
					p(strong("Overview -"),"This dashboard has been created using Shiny and the R programming language. Its purpose is to explore possible patterns in the recruitment and retention of full time, freshman, FTIAC students at Ferris State University from 2010 to 2018."),
					p(strong("Data -"),"The data used for this site were obtained from Institutional Research and Testing at Ferris State University. A special thanks to Jerome Forbes, Kathy Fisher, and Mitzi Day for providing the raw data. In compliance with FERPA, all personally identifiable information has been removed from the dataset."),
					p(strong("Code -"),"All of the working data and code for this project can be found", a("online", href="http://www.ferris.edu"), "here."),
					p(strong("License -"),"This work is released under the MIT license. © Clifton Franklund, 2019. Please feel free to use, modify, and share these materials. However, I would appreciate it if you would cite my work.")
				)
			),

			# STUDENTS TAB
			tabItem(tabName = "student1",
				p("Student recruitment by county is shown. Total students included =", textOutput('totalStudents', inline = TRUE)),
				plotOutput("recruitPlot", height=600)
			),
			# STUDENTS/100,000 TAB
			tabItem(tabName = "student2",
				p("Student recruitment by county per 100,000 population is shown. Total students included =", textOutput('totalStudents2', inline = TRUE)),
				plotOutput("recruitPlot2", height=600)
			),
			# RETENTION TAB
			tabItem(tabName = "retention",
				p("Percent student retention (fall to fall) is coded by color. The numbers indicate students per county. Total students included =", textOutput('totalStudentsRetained', inline = TRUE)),
				plotOutput("retentionPlot", height=600)
			),
			# HS GPA TAB
			tabItem(tabName = "GPA",
				p("The mean HS GPA is shown by color. Numbers indicate students per county. Total students included =", textOutput('totalStudentsGPA', inline = TRUE)),
				plotOutput("gpaPlot", height=600)
			),
			# ACT COMP TAB
			tabItem(tabName = "comp",
				p("This is just a place holder")
			),
			# ACT ENGL TAB
			tabItem(tabName = "engl",
				p("This is just a place holder")
			),
			# ACT READ TAB
			tabItem(tabName = "read",
				p("This is just a place holder")
			),
			# ACT MATH TAB
			tabItem(tabName = "math",
				p("This is just a place holder")
			),
			# ACT SCI TAB
			tabItem(tabName = "sci",
				p("This is just a place holder")
			)

		),

		div(id="disqus_thread",
		    HTML(
		    	"<script>
		    	(function() {
		    	var d = document, s = d.createElement('script');
		    	s.src = 'https://ftiac.disqus.com/embed.js';
		    	s.setAttribute('data-timestamp', +new Date());
		    	(d.head || d.body).appendChild(s);
		    	})();
		    	</script>
		    	<noscript>Please enable JavaScript to view the <a href='https://disqus.com/?ref_noscript' rel='nofollow'>comments powered by Disqus.</a></noscript>"
		    )
		),
		div(HTML('<script id="dsq-count-scr" src="//ftiac.disqus.com/count.js" async></script>'))
))

server = function(input, output) {
	collegeVector <- c("All Colleges" = "ALL",
			   "Arts & Sciences" = "AS",
			   "Business" = "BU",
			   "Education & Human Services" = "ED",
			   "Engineering Technology" = "TE",
			   "Health Professions" = "HP",
			   "Kendall" = "KE",
			   "University College" = "UN"
	)

	census <- c(10942, 9601, 111408, 29598, 23580, 15899, 8860, 59173, 107771, 17525, 156813, 45248, 136146, 52293, 25949, 26152, 38520, 30926, 75382, 14074, 37069, 26168, 107759, 32694, 425790, 25692, 16427, 88141, 42476, 46688, 36628, 33118, 280895, 63905, 25887, 11817, 70311, 160248, 250331, 17153, 602622, 2156, 11539, 88319, 21708, 99892, 180967, 6631, 11113, 840978, 24733, 67077, 28705, 42798, 24029, 83629, 14849, 152021, 63342, 9765, 172188, 48460, 1202362, 26570, 21699, 6780, 23528, 8640, 24164, 263801, 13181, 24449, 200169, 163040, 61295, 43114, 8485, 70648, 55729, 75907, 344791, 1820584, 32735)

	FTIAC_data <- read_csv("data_processed/FTIAC_data.csv")
	data(county.map)
	df.mi.county.center =
		county.map %>%
		filter(STATE == "26") %>%
		group_by(region) %>%
		summarize(avg.long = (min(long)+max(long))/2,
			  avg.lat  = (min(lat)+max(lat))/2,
			  label    = as.numeric(as.character(COUNTY[1]))+26000) # numeric form of county part of fips

	mi_county_labels = data.frame(long = df.mi.county.center$avg.long,
				      lat = df.mi.county.center$avg.lat,
				      region = df.mi.county.center$label,
				      pop = census)

	output$college <- renderText({names(collegeVector)[collegeVector == input$college]})
	output$years <- renderText({input$years})
	output$retentionPlot <- renderPlot({
		ifelse(input$college == "ALL",
				   filtered <- FTIAC_data %>% filter(
				   	TERM >= input$years[1],
				   	TERM <= input$years[2]
				        ) %>%
				   	group_by(COUNTY_CODE_ORIG) %>%
				   	summarize(value=mean(RETAINED)*100,
				   		  N=length(RETAINED),
				   		  GPA=mean(FERRIS_GPA, na.rm=TRUE)),
				   filtered <- FTIAC_data %>% filter(
				   	TERM >= input$years[1],
				   	TERM <= input$years[2],
				   	COLLEGE == input$college
				        ) %>%
				   	group_by(COUNTY_CODE_ORIG) %>%
				   	summarize(value=mean(RETAINED)*100,
				   		  N=length(RETAINED))
				)

		colnames(filtered) <- c("region","value","N")
		filtered$region <- paste(26, substr(filtered$region, 3, 5), sep="")
		filtered$region <- as.numeric(filtered$region)
		filtered <- as.data.frame(filtered)
		MIdata <- merge(filtered, mi_county_labels)

		county_choropleth(filtered, state_zoom = "michigan") +
		scale_fill_brewer("Percent\nRetention",palette="Reds") +
		geom_text(data = MIdata, aes(long, lat, label = paste(round(value,0),"%",sep=""), group = NULL), color = 'black')
	})


	output$recruitPlot <- renderPlot({
		ifelse(input$college == "ALL",
		       filtered <- FTIAC_data %>% filter(
		       	TERM >= input$years[1],
		       	TERM <= input$years[2]
		       ) %>%
		       	group_by(COUNTY_CODE_ORIG) %>%
		       	summarize(value=length(TERM)),
		       filtered <- FTIAC_data %>% filter(
		       	TERM >= input$years[1],
		       	TERM <= input$years[2],
		       	COLLEGE == input$college
		       ) %>%
		       	group_by(COUNTY_CODE_ORIG) %>%
		       	summarize(value=length(TERM))
		)
		colnames(filtered) <- c("region","value")
		filtered$region <- paste(26, substr(filtered$region, 3, 5), sep="")
		filtered$region <- as.numeric(filtered$region)
		filtered <- as.data.frame(filtered)
		MIdata <- merge(filtered, mi_county_labels)

		county_choropleth(filtered, state_zoom = "michigan") +
			scale_fill_brewer("Students\nRecruited",palette="Reds") +
			geom_text(data = MIdata, aes(long, lat, label = value, group = NULL), color = 'black')
	})


	output$recruitPlot2 <- renderPlot({
		ifelse(input$college == "ALL",
		       filtered <- FTIAC_data %>% filter(
		       	TERM >= input$years[1],
		       	TERM <= input$years[2]
		       ) %>%
		       	group_by(COUNTY_CODE_ORIG) %>%
		       	summarize(value=length(TERM)),
		       filtered <- FTIAC_data %>% filter(
		       	TERM >= input$years[1],
		       	TERM <= input$years[2],
		       	COLLEGE == input$college
		       ) %>%
		       	group_by(COUNTY_CODE_ORIG) %>%
		       	summarize(value=length(TERM))
		)
		filtered$value <- round(filtered$value*100000/census,0)
		colnames(filtered) <- c("region","value")
		filtered$region <- paste(26, substr(filtered$region, 3, 5), sep="")
		filtered$region <- as.numeric(filtered$region)
		filtered <- as.data.frame(filtered)
		MIdata <- merge(filtered, mi_county_labels)

		county_choropleth(filtered, state_zoom = "michigan") +
			scale_fill_brewer("Recruitment\nper 100,000",palette="Reds") +
			geom_text(data = MIdata, aes(long, lat, label = value, group = NULL), color = 'black')
	})


	output$gpaPlot <- renderPlot({
		ifelse(input$college == "ALL",
		       filtered <- FTIAC_data %>% filter(
		       	TERM >= input$years[1],
		       	TERM <= input$years[2]
		       ) %>%
		       	group_by(COUNTY_CODE_ORIG) %>%
		       	summarize(value=mean(FERRIS_GPA, na.rm=TRUE),
		       		  N=length(RETAINED)),
		       filtered <- FTIAC_data %>% filter(
		       	TERM >= input$years[1],
		       	TERM <= input$years[2],
		       	COLLEGE == input$college
		       ) %>%
		       	group_by(COUNTY_CODE_ORIG) %>%
		       	summarize(value=mean(FERRIS_GPA, na.rm=TRUE),
		       		  N=length(RETAINED))
		)

		colnames(filtered) <- c("region","value","N")
		filtered$region <- paste(26, substr(filtered$region, 3, 5), sep="")
		filtered$region <- as.numeric(filtered$region)
		filtered <- as.data.frame(filtered)
		MIdata <- merge(filtered, mi_county_labels)

		county_choropleth(filtered, state_zoom = "michigan") +
			scale_fill_brewer("Mean\nHS GPA",palette="Reds") +
			geom_text(data = MIdata, aes(long, lat, label = round(value,2), group = NULL), color = 'black')
	})


	output$totalStudentsRetained <- renderText({ifelse(input$college == "ALL",
				nrow(FTIAC_data %>%
					filter(TERM >= input$years[1],
					       TERM <= input$years[2]
					       )
				),
				nrow(FTIAC_data %>%
				     	filter(TERM >= input$years[1],
				     	       TERM <= input$years[2],
				     	       COLLEGE == input$college
				     	)
				))})
	output$totalStudents <- renderText({ifelse(input$college == "ALL",
				nrow(FTIAC_data %>%
				       	filter(TERM >= input$years[1],
				       	       TERM <= input$years[2]
					       	)
				   ),
				nrow(FTIAC_data %>%
				       	filter(TERM >= input$years[1],
				       	       TERM <= input$years[2],
				       	       COLLEGE == input$college
				       	)
				))})
	output$totalStudents2 <- renderText({ifelse(input$college == "ALL",
						   nrow(FTIAC_data %>%
						        	filter(TERM >= input$years[1],
						        	       TERM <= input$years[2]
						        	)
						   ),
						   nrow(FTIAC_data %>%
						        	filter(TERM >= input$years[1],
						        	       TERM <= input$years[2],
						        	       COLLEGE == input$college
						        	)
						   ))})
	output$totalStudentsGPA <- renderText({ifelse(input$college == "ALL",
						    nrow(FTIAC_data %>%
						         	filter(TERM >= input$years[1],
						         	       TERM <= input$years[2]
						         	)
						    ),
						    nrow(FTIAC_data %>%
						         	filter(TERM >= input$years[1],
						         	       TERM <= input$years[2],
						         	       COLLEGE == input$college
						         	)
						    ))})

}

shinyApp(ui, server)
