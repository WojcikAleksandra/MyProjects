library(shiny)
library(ggplot2)
library(dplyr)

ProductionYear = c("1960", "1970", "1980", "1990", "2000")

ui <- fluidPage(
  titlePanel("Opóźnienia samolotów"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "typ_samolotu",
        "Wybierz rok produkcji samolotu:",
        choices = ProductionYear,
        multiple = TRUE
      )
    ),
    mainPanel(
      plotOutput("wykres")
    )
  )
)


server <- function(input, output) {
  
  output$wykres <- renderPlot({
    dane_do_wykresu <- ramki_danych %>%
      filter(TypSamolotu %in% input$typ_samolotu)  
    
    if (nrow(dane_do_wykresu) > 0) {
      kolory <- rainbow(nrow(dane_do_wykresu))
      
      ggplot() +
        lapply(1:nrow(dane_do_wykresu), function(i) {
          Data <- MainData[ProductionYear %in% input$typ_samolotu, ]
          dane <- data.frame(Rok = Data[[FlightYear]], SrednieOpoznienie = Data[[MeanDelay]])
            geom_point(data = dane, aes(x = Rok, y = SrednieOpoznienie), size = 3, color = kolory[i])
        }) +
        labs(title = "Średnie opóźnienie dla wybranych typów samolotów",
             x = "Rok", y = "Średnie opóźnienie") +
        theme(legend.position = "right")
    } else {
      ggplot() + labs(title = "Brak danych dla wybranych typów samolotów")
    }
  })
  
}


shinyApp(ui = ui, server = server)
