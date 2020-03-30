library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(dplyr)
library(RColorBrewer)
library(raster)
library(htmltools)
library(DT)
library(readr)
#library(rmapshaper)




#loading data table
var_data<-read_csv("./www/data.csv")

#loading base map
base_map<-readOGR("./www/216district_population1.shp") #%>% ms_simplify()


#loading ghana variable shape file
shp<-readOGR("./www/ghn3.shp") #%>% ms_simplify()

#setting default crs
crs(shp)<- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


#loading water utilities
gwcl<-read_csv("./www/gwlc_locations.csv")

#loading minerals
minerals<-read_csv("./www/minerals.csv")
#mineral<-readOGR("./www/Minerals_points.shp")


#loading health sites
health<-readOGR("./www/healthsites.shp")

#loading laboratories
laboratory<-read_csv("./www/Labs.csv")

#airports
airport<-readOGR("./www/hotosm_gha_airports_points.shp")

#railway
railway<-readOGR("./www/hotosm_gha_railways_lines.shp")

#roads
roads<-readOGR("./www/ghana_road_networks.shp")

#seaports
seaports<-readOGR("./www/hotosm_gha_sea_ports_points.shp")

#financial services
finance<-readOGR("./www/hotosm_gha_financial_services_points.shp")

#water ways
waterways<-readOGR("./www/hotosm_gha_waterways_lines.shp")


#education facilities
education<-readOGR("./www/hotosm_gha_education_facilities_points.shp")

#landuse
landcover<-readOGR("./www/ghana_landuse.shp") #%>% ms_simplify()


#crops
crops<-readOGR("./www/_2003.shp") #%>% ms_simplify()



ui <-dashboardPage(
  dashboardHeader(
    title = "GHANA DASHBOARD", titleWidth = 400
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "About",
        tabName = "intro",
        icon = icon("info")
      ),
      
      menuItem(
        "Map",
        tabName = "map",
        icon = icon("map")
      )
      
    )             
    
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML('
                              .main-header .logo {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 24px;
                              }
                              '))),
    tabItems(
      
      tabItem(
        tabName = "intro",
        fluidRow(
          column(6,
                 box(
                   title = HTML("<strong>Background.</strong>"),
                   #status = "info",
                   collapsible = TRUE,width = NULL,
                   h4("This dashboard displays the distribution of water and demographic 
                      explanatory variables for each of the 216 districts in Ghana."),
                   h4("It can be used  for both technical and non-technical stakeholders 
                      such as national policy makers, NGOs, district administrators, 
                      development partners and consultants as an advocacy tool to support 
                      informed decision making for planning."),
                   h4("The dashboard can also be used as a diagnostic tool that helps to 
                      easily identify districts in Ghana where service improvements is 
                      needed based on the selected variable.")
                 )
          ),
          
          column(6,
                 box(
                   title = HTML("<strong>Dataset.</strong>"),
                   #status = "info",
                   collapsible = TRUE,width = NULL,
                  h4("The Ghana data is obtained from different sources as described in the table. 
                      It is then grouped into water and demographic variables."),
                  p(h4("Water variables include:Distance to water body (Lake, River) in metres,
                        Flood frequency rates,Water coverage levels,Water scarcity levels
                        ")),
                  p(h4("Demographic variables include:% of poor persons per district,Cholera cases per 100,000 people,
                        Distance to the main road in metres,Men and Women literacy levels,Population density,Time to access urban centre in minutes
                      "))
                 )
          )
          
        ),
       fluidRow(
         column(12,
                box(
                  title = HTML("<strong>The table below shows the various explanatory variables, 
                               their description, and their source.</strong>"),
                  #status = "info",
                  collapsible = TRUE,width = NULL,
         dataTableOutput("table")
                )
         )
       )#end row
        
        ), #end tabname
      
      tabItem(
        tabName = "map",
        
        fluidRow(
          column(width = 6,
                 selectInput(
                   inputId = "stats",
                   label = h4("Select Indicator"),
                   choices =c(
                     "Cholera cases per 100,000 people"=1,
                     "Men literacy levels(%)"=2,
                     "Women literacy levels(%)"=3,
                     "Time to access urban centre(minutes)"=4,
                     "Distance from the main road(metres)"=5,
                     "Water scarcity levels"=6,
                     "Flood frequency rates"=7,
                     "Population density per district"=9,
                     "GDP per km2"=10,
                     "Water coverage(%)"=11,
                     "% of poor persons"=12,
                     "District League Table Score"=13,
                     "Vegetation Land Cover"=14,
                     "Maize production in 2003 per district(tonnes)"=15,
                     "Rice production in 2003 per district(tonnes)"=16,
                     "Millet production in 2003 per district(tonnes)"=17,
                     "Sorghum production in 2003 per district(tonnes)"=18,
                     "Cassava production in 2003 per district(tonnes)"=19,
                     "Yam production in 2003 per district(tonnes)"=20,
                     "Cocoyam production in 2003 per district(tonnes)"=21,
                     "Plantain production in 2003 per district(tonnes)"=22,
                     "Groundnut production in 2003 per district(tonnes)"=23,
                     "ODF % community coverage"=24
                     
                   ),selected = 1
                   
                 ) 
                 
          )
        #end column
        
        
        ),
        
      fluidRow(
        column(8,
               box(
                 title = HTML("<strong>Map</strong>"),
                 #status = "info",
                 collapsible = TRUE,width = NULL,height = 750,
                 leafletOutput("map",height = 600)
               )
               ),#end col
        column(4,
               box(
                 #status = "info",
                 collapsible = TRUE,width = NULL,
                  height = 750,

                 #slider input for cholera cases 
                 sliderInput(inputId = "cholera",
                             label = "Cholera cases per 100,000 people",
                             min = min(shp@data$ChlrCss,na.rm =T),
                             max = max(shp@data$ChlrCss,na.rm =T),
                             value = c(min(shp@data$ChlrCss,na.rm =T),
                                       max(shp@data$ChlrCss,na.rm =T))
                 ),
                 #slider input for men literacy levels
                 sliderInput(inputId = "men",
                             label = "Men literacy levels(%)",
                             min = min(shp@data$prcmnlt,na.rm =T),
                             max = max(shp@data$prcmnlt,na.rm =T),
                             value = c(min(shp@data$prcmnlt,na.rm =T),
                                       max(shp@data$prcmnlt,na.rm =T))
                 ),
                 
                 #slider input for women literacy levels
                 sliderInput(inputId = "women",
                             label = "Women literacy levels(%)",
                             min = min(shp@data$prcwmnl,na.rm =T),
                             max = max(shp@data$prcwmnl,na.rm =T),
                             value = c(min(shp@data$prcwmnl,na.rm =T),
                                       max(shp@data$prcwmnl,na.rm =T))
                 ),
                 #slider input for time to access urban centre
                 sliderInput(inputId = "time",
                             label = "Time to access urban centre(minutes)",
                             min = min(shp@data$TmTAcUC,na.rm =T),
                             max = max(shp@data$TmTAcUC,na.rm =T),
                             value = c(min(shp@data$TmTAcUC,na.rm =T),
                                       max(shp@data$TmTAcUC,na.rm =T))
                 ),
                 #slider input for distance from the main road
                 sliderInput(inputId = "road",
                             label = "Distance from the main road(metres)",
                             min = min(shp@data$DstnFMR,na.rm =T),
                             max = max(shp@data$DstnFMR,na.rm =T),
                             value = c(min(shp@data$DstnFMR,na.rm =T),
                                       max(shp@data$DstnFMR,na.rm =T))
                 ),
                 #slider input for population density per district
                 # sliderInput(inputId = "density",
                 #             label = "Population density per district",
                 #             min = min(shp@data$PpDnspd,na.rm =T),
                 #             max = max(shp@data$PpDnspd,na.rm =T),
                 #             value = c(min(shp@data$PpDnspd,na.rm =T),
                 #                       max(shp@data$PpDnspd,na.rm =T))
                 # ),
                 #slider input for GDP per km2
                 # sliderInput(inputId = "gdp",
                 #             label = "GDP per km2",
                 #             min = min(shp@data$Gdpprsq,na.rm =T),
                 #             max = max(shp@data$Gdpprsq,na.rm =T),
                 #             value = c(min(shp@data$Gdpprsq,na.rm =T),
                 #                       max(shp@data$Gdpprsq,na.rm =T))
                 # ),
                 #slider input for water coverage(%)
                 sliderInput(inputId = "wtr",
                             label = "Water coverage(%)",
                             min = min(shp@data$prcwtrc,na.rm =T),
                             max = max(shp@data$prcwtrc,na.rm =T),
                             value = c(min(shp@data$prcwtrc,na.rm =T),
                                       max(shp@data$prcwtrc,na.rm =T))
                 ),
                 #slider input for % of poor persons
                 sliderInput(inputId = "poor",
                             label = "% of poor persons",
                             min = min(shp@data$prcprpr,na.rm =T),
                             max = max(shp@data$prcprpr,na.rm =T),
                             value = c(min(shp@data$prcprpr,na.rm =T),
                                       max(shp@data$prcprpr,na.rm =T))
                 )
               )
               
               )#end column
      )
        
      ) #end tab item
      )#end tab items
    
    )
  
  
    ) 

server<-function(input,output,session){
  
  
  #cholera legend
  brk1<-seq(0,300,50)
  chl<-colorBin("YlOrRd", shp$ChlrCss,na.color = "transparent",bins = brk1)
  
  #district league score
  br<-seq(0,100,20)
  dlt<-colorBin("YlOrRd", shp$dlt_Scr,na.color = "transparent",bins = br)
  
  
  #men lit legend
  brk2<-seq(0,100,20)
  mn<-colorBin("YlOrRd", shp$prcmnlt,na.color = "transparent",bins = brk2)
  
  #wmn lit legend
  brk3<-seq(0,100,20)
  wmn<-colorBin("YlOrRd", shp$prcwmnl,na.color = "transparent",bins = brk3)
  
  #time access urban centre
  brk4<-seq(0,350,50)
  tm<-colorBin("YlOrRd", shp$TmTAcUC,na.color = "transparent",bins = brk4)
  
  #distance from the main road
  brk5<-seq(0,18000,3000)
  dst<-colorBin("YlOrRd", shp$DstnFMR,na.color = "transparent",bins = brk5)
  
  #water scarcity levels
  wsc<-colorFactor(topo.colors(4), shp$wtr_scr)
  wsc2<-colorFactor("YlOrRd", shp$wtr_scr)
  
  #flood frequency
  fld<-colorFactor("YlOrRd", shp$fld_frq)
  
  
  
  #population density
  brk6<-seq(0,15000,3000)
  ppd<-colorBin("YlOrRd", shp$PpDnspd,na.color = "transparent",bins = brk6)
  
  #GDP per km2
  brk7<-seq(0,10000,2000)
  gd<-colorBin("YlOrRd", shp$Gdpprsq,na.color = "transparent",bins = brk7)
  
  #water coverage
  brk8<-seq(0,100,20)
  watr<-colorBin("YlOrRd", shp$prcwtrc,na.color = "transparent",bins = brk8)
  
  #poor persons
  brk8<-seq(0,100,20)
  pr<-colorBin("YlOrRd", shp$prcprpr,na.color = "transparent",bins = brk8)
  
  
  #sliderinput reactive function for all numeric input options
  sld<-reactive({
    subset(shp,shp@data$ChlrCss>=input$cholera[1]&
             shp@data$ChlrCss<=input$cholera[2]&
             shp@data$prcmnlt>=input$men[1]&
             shp@data$prcmnlt<=input$men[2]&
             shp@data$prcwmnl>=input$women[1]&
             shp@data$prcwmnl<=input$women[2]& 
             shp@data$TmTAcUC>=input$time[1]&
             shp@data$TmTAcUC<=input$time[2]&
             shp@data$DstnFMR>=input$road[1]&
             shp@data$DstnFMR<=input$road[2]&
             # shp@data$PpDnspd>=input$density[1]&
             # shp@data$PpDnspd<=input$density[2]&
             # shp@data$Gdpprsq>=input$gdp[1]&
             # shp@data$Gdpprsq<=input$gdp[2]&
             shp@data$prcwtrc>=input$wtr[1]&
             shp@data$prcwtrc<=input$wtr[2]&
             shp@data$prcprpr>=input$poor[1]&
             shp@data$prcprpr<=input$poor[2]
           
    )
    
  })
  
  ##########rendering the data table###################
  output$table<-renderDataTable(
    var_data,
    class = 'cell-border stripe',
    editable = TRUE,
    options = list(scrollX = T)
  )
  
  
  #gwcl awesome marker
  ikon<-awesomeIcons(
    icon = "tint",
    iconColor = "#FFFFFF",
    lib = 'fa',
    markerColor = "green"
  )
  
  
  
  
  #minerals awesome marker
  ikon2<-awesomeIcons(
    icon = "gavel",
    iconColor = "#FFFFFF",
    lib = "fa",
    markerColor = "gray"
  )
  
  
  #healthsites awesome marker
  ikon3<-awesomeIcons(
    icon = "h-square",
    iconColor = "#FFFFFF",
    lib = "fa",
    markerColor = "purple"
  )
  
  #labs awesome marker
  ikon4<-awesomeIcons(
    icon = "flask",
    iconColor = "#FFFFFF",
    lib = "fa",
    markerColor = "pink"
  )
  
  #airport awesome marker
  ikon5<-awesomeIcons(
    icon = "plane",
    iconColor = "#FFFFFF",
    lib = "fa",
    markerColor = "orange"
  )
  
  
  #seaports
  ikon6<-awesomeIcons(
    icon = "anchor",
    iconColor = "#FFFFFF",
    lib = "fa",
    markerColor = "lavender"
  )
  
  #finance
  ikon7<-awesomeIcons(
    icon = "dollar-sign",
    iconColor = "#FFFFFF",
    lib = "fa",
    markerColor = "navy"
  )
  
  
  #education
  ikon8<-awesomeIcons(
    icon = "graduation-cap",
    iconColor = "#FFFFFF",
    lib = "fa",
    markerColor = "navy"
  )
  
  
  #rendering the basemap
  output$map<-renderLeaflet(
    leaflet() %>%
      setView(lng=-0.990341,lat=8.213941,zoom = 6.5) %>%
      
      #water utility
      addAwesomeMarkers(
        data = gwcl,
        lat = ~lat,
        lng = ~lon,
        icon = ikon,
        group = "Water Utilities",
        label = paste(
          gwcl$Facility
        )

      ) %>%
      
      #minerals
      addAwesomeMarkers(
        data = minerals,
        # lat = ~lat,
        # lng = ~lon,
        icon = ikon2,
        group = "Minerals",
        label = paste(
          minerals$MINERALS
        )
        #labelOptions = labelOptions(noHide = T)
      ) %>%
      
      #laboratories
      addAwesomeMarkers(
        data = laboratory,
        lat = ~lat,
        lng = ~lon,
        icon = ikon4,
        group = "Laboratory",
        label = paste(
         laboratory$Lab
        )
        
      ) %>%
      
    #health sites
    addAwesomeMarkers(
      data = health,
      icon = ikon3,
      group = "Health sites",
      label = paste(
        health$amenity
      )
      #labelOptions = labelOptions(noHide = T)
    ) %>%
    
    #airport
    addAwesomeMarkers(
      data = airport,
      icon = ikon5,
      group = "Airports",
      label = paste(
        airport$name
      )
      #labelOptions = labelOptions(noHide = T)
    ) %>%
      
  
    #railway
    addPolylines(
      data = railway,
      color = "yellow",
      smoothFactor = 0.5,
      weight = 2, 
      opacity = 1.0,
      fillOpacity = 1.0,
      group = "Railway",
      label = paste(
        railway$name
      )
    )%>%
      
      #seaports
      addAwesomeMarkers(
        data = seaports,
        icon = ikon6,
        group = "Seaports",
        label = paste(
          seaports$name
        )
        #labelOptions = labelOptions(noHide = T)
      ) %>%
      
      #finance
      addAwesomeMarkers(
        data = finance,
        icon = ikon7,
        group = "Financial Institutions",
        label = paste(
          finance$name
        )
        #labelOptions = labelOptions(noHide = T)
      )%>%
      
      #waterway
      addPolylines(
        data = waterways,
        color = "blue",
        smoothFactor = 0.5,
        weight = 2, 
        opacity = 1.0,
        fillOpacity = 1.0,
        group = "Waterways",
        label = paste(
          waterways$waterway
        )
      )%>%
      
      #education
      addAwesomeMarkers(
        data = education,
        icon = ikon8,
        group = "Education Institutions",
        label = paste(
          education$name
        )
        #labelOptions = labelOptions(noHide = T)
      )%>%
      
      #roads
      addPolylines(
        data = roads,
        color = "magenta",
        smoothFactor = 0.5,
        weight = 2, 
        opacity = 1.0,
        fillOpacity = 1.0,
        group = "Roads",
        label = paste(
          roads$DXF_LAYER
        )
      )%>%
      
      
      addPolygons(
        data = sld(),
        color = ~chl(ChlrCss),
        smoothFactor = 0.5,
        weight = 2, 
        opacity = 1.0,
        fillOpacity = 0.9,
        highlightOptions = highlightOptions(
          weight = 1,
          color = "black",
          fillOpacity = 0.7,
          bringToFront = FALSE
        ),
        label = paste(
          "<strong>Region:</strong>",sld()$REGION,
          "<br>",
          "<strong>District:</strong>",sld()$DISTRIC,
          "<br>",
          "<strong>Cholera Cases:</strong>",sld()$ChlrCss
        ) %>% lapply(htmltools::HTML),
        labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                  padding = "3px 8px"), 
                                     textsize = "11px", direction = "auto")
      ) %>% addLegend(
        pal = chl, values = sld()$ChlrCss, title = "Cholera cases per 100,000 people"
      ) %>%
      
      addPolygons(
        data = base_map,
        color = "dimgrey",
        weight = 1,
        fill = F,
        group = "base_map"
      ) %>%
      
      # Layers control
      addLayersControl(
        position = "bottomleft",
        overlayGroups = c("Water Utilities","Minerals","Laboratory","Health sites","Airports","Railway","Seaports",
                          "Financial Institutions", "Waterways","Education Institutions","Roads"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% hideGroup(c("Water Utilities","Minerals","Laboratory","Health sites","Airports","Railway","Seaports",
                        "Financial Institutions", "Waterways","Education Institutions","Roads"))
   
  ) #end leaflet
  
  observe({
    clear<-leafletProxy("map") %>% clearControls()
    
    if ("1" %in% input$stats){
      clear %>% clearShapes() %>%
      addPolygons(
        data = sld(),
        color = ~chl(ChlrCss),
        smoothFactor = 0.5,
        weight = 2, opacity = 1.0,
        fillOpacity = 0.9,
        highlightOptions = highlightOptions(
          weight = 1,
          color = "black",
          fillOpacity = 0.7,
          bringToFront = FALSE
        ),
        label = paste(
          "<strong>Region:</strong>",sld()$REGION,
          "<br>",
          "<strong>District:</strong>",sld()$DISTRIC,
          "<br>",
          "<strong>Cholera Cases:</strong>",sld()$ChlrCss
        ) %>% lapply(htmltools::HTML),
        labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                  padding = "3px 8px"), 
                                     textsize = "11px", direction = "auto")
      ) %>% addLegend(
        pal = chl, values = shp$ChlrCss, title = "Cholera cases per 100,000 people"
      ) %>%
        addPolygons(
          data = base_map,
          color = "dimgrey",
          weight = 1, 
          fill = F,
          group = "base_map"
        )
    }
    ######end#####
    if ("2" %in% input$stats){
      
      clear %>% 
        addPolygons(
          data = sld(),
          color = ~mn(prcmnlt),
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 0.9,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label = paste(
            "<strong>Region:</strong>",sld()$REGION,
            "<br>",
            "<strong>District:</strong>",sld()$DISTRIC,
            "<br>",
            "<strong>Men literacy levels(%):</strong>",sld()$prcmnlt
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "11px", direction = "auto")
        ) %>% addLegend(
          pal = mn, values = shp$prcmnlt, title = "Men literacy levels(%)"
        ) %>%
        addPolygons(
          data = base_map,
          color = "dimgrey",
          weight = 1, 
          fill = F,
          group = "base_map"
        )
        
    }
    
    ########end#####
    
    if ("3" %in% input$stats){
      
      clear %>% 
        
        addPolygons(
          data = sld(),
          color = ~wmn(prcwmnl),
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 0.9,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label = paste(
            "<strong>Region:</strong>",sld()$REGION,
            "<br>",
            "<strong>District:</strong>",sld()$DISTRIC,
            "<br>",
            "<strong>Women literacy(%):</strong>",sld()$prcwmnl
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "11px", direction = "auto")
        ) %>% addLegend(
          pal = wmn, values = shp$prcwmnl, title = "Women literacy levels(%)"
        ) %>%
        addPolygons(
          data = base_map,
          color = "dimgrey",
          weight = 1, 
          fill = F,
          group = "base_map"
        )
    }
    
    #######end######
    
    if ("4" %in% input$stats){
      
      clear %>% 
        
        addPolygons(
          data = sld(),
          color = ~tm(TmTAcUC),
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 0.9,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label = paste(
            "<strong>Region:</strong>",sld()$REGION,
            "<br>",
            "<strong>District:</strong>",sld()$DISTRIC,
            "<br>",
            "<strong>Time to access urban centre(minutes):</strong>",sld()$TmTAcUC
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "11px", direction = "auto")
        ) %>% addLegend(
          pal = tm, values = shp$TmTAcUC, title = "Time to access urban centre(minutes)"
        )%>%
        addPolygons(
          data = base_map,
          color = "dimgrey",
          weight = 1, 
          fill = F,
          group = "base_map"
        )
    }
    
    #########end########
    
    if ("5" %in% input$stats){
      
      clear %>% 
        
        addPolygons(
          data = sld(),
          color = ~dst(DstnFMR),
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 0.9,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label = paste(
            "<strong>Region:</strong>",sld()$REGION,
            "<br>",
            "<strong>District:</strong>",sld()$DISTRIC,
            "<br>",
            "<strong>Distance from main road(metres):</strong>",sld()$DstnFMR
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "11px", direction = "auto")
        ) %>% addLegend(
          pal = dst, values = shp$DstnFMR, title = "Distance from main road(metres)"
        ) %>%
        addPolygons(
          data = base_map,
          color = "dimgrey",
          weight = 1, 
          fill = F,
          group = "base_map"
        )
    }
    
    ###########end#########
    
    if ("6" %in% input$stats){
      
      clear %>% 
        
        addPolygons(
          data = sld(),
          color = ~wsc2(wtr_scr),
          smoothFactor = 0.5,
          weight = 2, 
          opacity = 1.0,
          fillOpacity = 0.9,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label = paste(
            "<strong>Region:</strong>",sld()$REGION,
            "<br>",
            "<strong>District:</strong>",sld()$DISTRIC,
            "<br>",
            "<strong>Water scarcity levels:</strong>",sld()$wtr_scr
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "11px", direction = "auto")
        ) %>% addLegend(
          pal = wsc2, values = shp$wtr_scr, title = "Water scarcity levels"
        )  %>%
        addPolygons(
          data = base_map,
          color = "dimgrey",
          weight = 1, 
          fill = F,
          group = "base_map"
        )
    }
    
    ############end#########
    
    if ("7" %in% input$stats){
      
      clear %>% 
        
        addPolygons(
          data = sld(),
          color = ~fld(fld_frq),
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 0.9,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label = paste(
            "<strong>Region:</strong>",sld()$REGION,
            "<br>",
            "<strong>District:</strong>",sld()$DISTRIC,
            "<br>",
            "<strong>Flood frequency rates:</strong>",sld()$fld_frq
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "11px", direction = "auto")
        ) %>% addLegend(
          pal = fld, values = shp$fld_frq, title = "Flood frequency rates"
        )  %>%
        addPolygons(
          data = base_map,
          color = "dimgrey",
          weight = 1, 
          fill = F,
          group = "base_map"
        )
    }
    
    
    ##########end################
    
    if ("9" %in% input$stats){
      
      clear %>% 
        
        addPolygons(
          data = sld(),
          color = ~ppd(PpDnspd),
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 0.9,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label = paste(
            "<strong>Region:</strong>",sld()$REGION,
            "<br>",
            "<strong>District:</strong>",sld()$DISTRIC,
            "<br>",
            "<strong>Population density per district:</strong>",sld()$PpDnspd
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "11px", direction = "auto")
        ) %>% addLegend(
          pal = ppd, values = shp$PpDnspd, title = "Population density per district"
        ) %>%
        addPolygons(
          data = base_map,
          color = "dimgrey",
          weight = 1, 
          fill = F,
          group = "base_map"
        )
    }
    ##########end#############
    
    if ("10" %in% input$stats){
      
      clear %>% 
        
        addPolygons(
          data = sld(),
          color = ~gd(Gdpprsq),
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 0.9,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label = paste(
            "<strong>Region:</strong>",sld()$REGION,
            "<br>",
            "<strong>District:</strong>",sld()$DISTRIC,
            "<br>",
            "<strong>GDP per km2:</strong>",sld()$Gdpprsq
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "11px", direction = "auto")
        ) %>% addLegend(
          pal = gd, values = shp$Gdpprsq, title = "GDP per km2"
        ) %>%
        addPolygons(
          data = base_map,
          color = "dimgrey",
          weight = 1, 
          fill = F,
          group = "base_map"
        )
    }
    
    ############end#########3
    if ("11" %in% input$stats){
      
      clear %>% 
        
        addPolygons(
          data = sld(),
          color = ~watr(prcwtrc),
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 0.9,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label = paste(
            "<strong>Region:</strong>",sld()$REGION,
            "<br>",
            "<strong>District:</strong>",sld()$DISTRIC,
            "<br>",
            "<strong>Water coverage(%):</strong>",sld()$prcwtrc
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "11px", direction = "auto")
        ) %>% addLegend(
          pal = watr, values = shp$prcwtrc, title = "Water coverage(%)"
        ) %>%
        addPolygons(
          data = base_map,
          color = "dimgrey",
          weight = 1, 
          fill = F,
          group = "base_map"
        )
    }
    
    ##################end###############
    
    if ("12" %in% input$stats){
      
      clear %>% 
        
        addPolygons(
          data = sld(),
          color = ~pr(prcprpr),
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 0.9,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label = paste(
            "<strong>Region:</strong>",sld()$REGION,
            "<br>",
            "<strong>District:</strong>",sld()$DISTRIC,
            "<br>",
            "<strong>% of poor persons:</strong>",sld()$prcprpr
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "11px", direction = "auto")
        ) %>% addLegend(
          pal = pr, values = shp$prcprpr, title = "% of poor persons"
        ) %>%
        addPolygons(
          data = base_map,
          color = "dimgrey",
          weight = 1, 
          fill = F,
          group = "base_map"
        )
    }
    
    ##############end###########
   
    if ("13" %in% input$stats){
      
      clear %>% 
        
        addPolygons(
          data = sld(),
          color = ~dlt(dlt_Scr),
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 0.9,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label = paste(
            "<strong>Region:</strong>",sld()$REGION,
            "<br>",
            "<strong>District:</strong>",sld()$DISTRIC,
            "<br>",
            "<strong>District League Score:</strong>",sld()$dlt_Scr
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "11px", direction = "auto")
        ) %>% addLegend(
          pal = dlt, values = shp$dlt_Scr, title = "District League Score"
        ) %>%
        addPolygons(
          data = base_map,
          color = "dimgrey",
          weight = 1, 
          fill = F,
          group = "base_map"
        )
    }
    
    #landuse color function
    ldv<-colorFactor(topo.colors(25),landcover$LANDCOV)
    ##############end###############
    if ("14" %in% input$stats){
      
      clear %>% 
        
        addPolygons(
          data = landcover,
          color = ~ldv(LANDCOV),
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 0.9,
          label = paste(
            landcover$LANDCOV
          ) 
          
        ) %>% addLegend(
          pal = ldv, values =landcover$LANDCOV, title = "Vegetation Landcover"
        ) %>%
        addPolygons(
          data = base_map,
          color = "dimgrey",
          weight = 1, 
          fill = F,
          group = "base_map",
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label = paste(
            "<strong>Region:</strong>",base_map$REGION,
            "<br>",
            "<strong>District:</strong>",base_map$DISTRICT
          ) %>% lapply(htmltools::HTML)
        )
    }
    
    ####################end########################
    
    #maize color fxn
    maiz<-colorNumeric("YlOrRd",crops$MAIZE)
    
    if ("15" %in% input$stats){
      
      clear %>% 
        
        addPolygons(
          data = crops,
          color = ~maiz(MAIZE),
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 0.9,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label = paste(
            "<strong>Region:</strong>",crops$REGION,
            "<br>",
            "<strong>District:</strong>",crops$DISTRICT,
            "<br>",
            "<strong>Maize production(tonnes):</strong>",crops$MAIZE
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "11px", direction = "auto")
        ) %>% addLegend(
          pal = maiz, values =crops$MAIZE , title = "Maize production(tonnes)"
        ) %>%
        addPolygons(
          data = crops,
          color = "dimgrey",
          weight = 1, 
          fill = F,
          group = "base_map"
        )
        
    }
    
    ###################end##############
    
    #rice color fxn
    rice<-colorNumeric("YlOrRd",crops$RICE)
    
    if ("16" %in% input$stats){
      
      clear %>% 
        
        addPolygons(
          data = crops,
          color = ~rice(RICE),
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 0.9,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label = paste(
            "<strong>Region:</strong>",crops$REGION,
            "<br>",
            "<strong>District:</strong>",crops$DISTRICT,
            "<br>",
            "<strong>Rice production(tonnes):</strong>",crops$RICE
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "11px", direction = "auto")
        ) %>% addLegend(
          pal = rice, values =crops$RICE , title = "Rice production(tonnes)"
        ) %>%
        addPolygons(
          data = crops,
          color = "dimgrey",
          weight = 1, 
          fill = F,
          group = "base_map"
        )
      
    }
    
    ##################end#############
    #rice color fxn
    mill<-colorNumeric("YlOrRd",crops$MILLET)
    
    if ("17" %in% input$stats){
      
      clear %>% 
        
        addPolygons(
          data = crops,
          color = ~mill(MILLET),
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 0.9,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label = paste(
            "<strong>Region:</strong>",crops$REGION,
            "<br>",
            "<strong>District:</strong>",crops$DISTRICT,
            "<br>",
            "<strong>Millet production(tonnes):</strong>",crops$MILLET
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "11px", direction = "auto")
        ) %>% addLegend(
          pal = mill, values =crops$MILLET , title = "Millet production(tonnes)"
        ) %>%
        addPolygons(
          data = crops,
          color = "dimgrey",
          weight = 1, 
          fill = F,
          group = "base_map"
        )
      
    }
    
    ################end##############
    #sorghum color fxn
    sorg<-colorNumeric("YlOrRd",crops$SORGHUM)
    
    if ("18" %in% input$stats){
      
      clear %>% 
        
        addPolygons(
          data = crops,
          color = ~sorg(SORGHUM),
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 0.9,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label = paste(
            "<strong>Region:</strong>",crops$REGION,
            "<br>",
            "<strong>District:</strong>",crops$DISTRICT,
            "<br>",
            "<strong>Sorghum production(tonnes):</strong>",crops$SORGHUM
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "11px", direction = "auto")
        ) %>% addLegend(
          pal = sorg, values =crops$SORGHUM , title = "Sorghum production(tonnes)"
        ) %>%
        addPolygons(
          data = crops,
          color = "dimgrey",
          weight = 1, 
          fill = F,
          group = "base_map"
        )
      
    }
    
    ##############end###########
    #cassava color fxn
    cass<-colorNumeric("YlOrRd",crops$CASSAVA)
    
    if ("19" %in% input$stats){
      
      clear %>% 
        
        addPolygons(
          data = crops,
          color = ~cass(CASSAVA),
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 0.9,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label = paste(
            "<strong>Region:</strong>",crops$REGION,
            "<br>",
            "<strong>District:</strong>",crops$DISTRICT,
            "<br>",
            "<strong>Cassava production(tonnes):</strong>",crops$CASSAVA
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "11px", direction = "auto")
        ) %>% addLegend(
          pal = cass, values =crops$CASSAVA , title = "Cassava production(tonnes)"
        ) %>%
        addPolygons(
          data = crops,
          color = "dimgrey",
          weight = 1, 
          fill = F,
          group = "base_map"
        )
      
    }
    
    ##############end###########
    
    #yam color fxn
    yam<-colorNumeric("YlOrRd",crops$YAM)
    
    if ("20" %in% input$stats){
      
      clear %>% 
        
        addPolygons(
          data = crops,
          color = ~yam(YAM),
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 0.9,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label = paste(
            "<strong>Region:</strong>",crops$REGION,
            "<br>",
            "<strong>District:</strong>",crops$DISTRICT,
            "<br>",
            "<strong>Yam production(tonnes):</strong>",crops$YAM
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "11px", direction = "auto")
        ) %>% addLegend(
          pal = yam, values =crops$YAM , title = "Yam production(tonnes)"
        ) %>%
        addPolygons(
          data = crops,
          color = "dimgrey",
          weight = 1, 
          fill = F,
          group = "base_map"
        )
      
    }
    
    ########end############
    #cocoyam color fxn
    coyam<-colorNumeric("YlOrRd",crops$COCOYAM)
    
    if ("21" %in% input$stats){
      
      clear %>% 
        
        addPolygons(
          data = crops,
          color = ~coyam(COCOYAM),
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 0.9,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label = paste(
            "<strong>Region:</strong>",crops$REGION,
            "<br>",
            "<strong>District:</strong>",crops$DISTRICT,
            "<br>",
            "<strong>Cocoyam production(tonnes):</strong>",crops$COCOYAM
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "11px", direction = "auto")
        ) %>% addLegend(
          pal = coyam, values =crops$COCOYAM, title = "Cocoyam production(tonnes)"
        ) %>%
        addPolygons(
          data = crops,
          color = "dimgrey",
          weight = 1, 
          fill = F,
          group = "base_map"
        )
      
    }
    
    ##########end########
    #cocoyam color fxn
    plant<-colorNumeric("YlOrRd",crops$PLANTAIN)
    
    if ("22" %in% input$stats){
      
      clear %>% 
        
        addPolygons(
          data = crops,
          color = ~plant(PLANTAIN),
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 0.9,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label = paste(
            "<strong>Region:</strong>",crops$REGION,
            "<br>",
            "<strong>District:</strong>",crops$DISTRICT,
            "<br>",
            "<strong>Plantain production(tonnes):</strong>",crops$PLANTAIN
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "11px", direction = "auto")
        ) %>% addLegend(
          pal = plant, values =crops$PLANTAIN, title = "Plantain production(tonnes)"
        ) %>%
        addPolygons(
          data = crops,
          color = "dimgrey",
          weight = 1, 
          fill = F,
          group = "base_map"
        )
      
    }
    ############end########
    #groundnuts color fxn
    grd<-colorNumeric("YlOrRd",crops$GROUNDNUT)
    
    if ("23" %in% input$stats){
      
      clear %>% 
        
        addPolygons(
          data = crops,
          color = ~grd(GROUNDNUT),
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 0.9,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label = paste(
            "<strong>Region:</strong>",crops$REGION,
            "<br>",
            "<strong>District:</strong>",crops$DISTRICT,
            "<br>",
            "<strong>Groundnut production(tonnes):</strong>",crops$GROUNDNUT
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "11px", direction = "auto")
        ) %>% addLegend(
          pal = grd, values =crops$GROUNDNUT, title = "Groundnut production(tonnes)"
        ) %>%
        addPolygons(
          data = crops,
          color = "dimgrey",
          weight = 1, 
          fill = F,
          group = "base_map"
        )
      
    }
    
    #################end######################
    #odf color fxn
    bk<-seq(0,100,20)
    od<-colorBin("YlOrRd",shp$ODF,bins = bk)
    
    
    if ("24" %in% input$stats){
      
      clear %>% 
        
        addPolygons(
          data = sld(),
          color = ~od(ODF),
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 0.9,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label = paste(
            "<strong>Region:</strong>",sld()$REGION,
            "<br>",
            "<strong>District:</strong>",sld()$DISTRIC,
            "<br>",
            "<strong>% ODF community coverage:</strong>",sld()$ODF
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "11px", direction = "auto")
        ) %>% addLegend(
          pal = od, values = shp$ODF, title = "% ODF community coverage"
        ) %>%
        addPolygons(
          data = base_map,
          color = "dimgrey",
          weight = 1, 
          fill = F,
          group = "base_map"
        )
    }
    
    
  }) ###end of observer fxn
  
  
  
  
}

shinyApp(ui,server)


