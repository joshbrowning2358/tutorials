library(shiny)
library(ggplot2)
library(plotly)

gg_color_hue = function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}

get_n_bins = function(data){
    return(1 + 3.322*log(nrow(data)))
}

ui = shinyUI(fluidPage(
   
    titlePanel("Population vs. Mean Distribution"),
   
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId='n_samples', label='# Samples', min=1, max=1000, value=10, step=1),
            sliderInput(inputId='mean', label='Mean', min=-20, max=20, value=0, step=1),
            sliderInput(inputId='sd', label='Standard Deviation', min=0.1, max=10, value=1, step=0.1),
            fluidRow(
                actionButton(inputId='resimulate', label='Simulate'),
                actionButton(inputId='resimulate10', label='x10'),
                actionButton(inputId='resimulate100', label='x100'),
                actionButton(inputId='resimulate1000', label='x1000')
            ),
            fluidRow(
                actionButton(inputId='clear', label='Clear')
            )
        ),
    
        mainPanel(
            plotlyOutput("population_distribution", height=450),
            plotlyOutput("mean_distribution", height=450)
        )
   )
))

server = shinyServer(function(input, output) {
    cols = gg_color_hue(3)
    
    mean_points = reactiveValues(x=c())

    observeEvent({
        input$clear
        # Clear mean if parameters are changed
        input$n_samples
        input$mean
        input$sd}, {
            mean_points$x = c()
    })
    
    observeEvent(input$resimulate10, {
        # Generate 9 new means as data gets re-simulated and adds one point as well
        new_means = sapply(1:9, function(i) mean(rnorm(input$n_samples, input$mean, input$sd)))
        mean_points$x = c(mean_points$x, new_means)
    })

    observeEvent(input$resimulate100, {
        # Generate 99 new means as data gets re-simulated and adds one point as well
        new_means = sapply(1:99, function(i) mean(rnorm(input$n_samples, input$mean, input$sd)))
        mean_points$x = c(mean_points$x, new_means)
    })
    
    observeEvent(input$resimulate1000, {
        # Generate 999 new means as data gets re-simulated and adds one point as well
        new_means = sapply(1:999, function(i) mean(rnorm(input$n_samples, input$mean, input$sd)))
        mean_points$x = c(mean_points$x, new_means)
    })
    
    data = reactive({
        input$resimulate
        input$resimulate10
        input$resimulate100
        input$resimulate1000
        isolate({
            obs = rnorm(input$n_samples, mean=input$mean, sd=input$sd)
            mean_points$x = c(mean_points$x, mean(obs))
        })
        return(obs)
    })
    
    output$population_distribution = renderPlotly({
        d = data.frame(x=c(mean(data()), data()))
        d$name = c('Mean', rep('Data', nrow(d) - 1))
        isolate({
            p = ggplot(d) +
                geom_histogram(aes(x=x, fill=name, color=name), bins=get_n_bins(d)) +
                coord_cartesian(xlim=input$mean + 4*input$sd*c(-1, 1)) +
                scale_color_manual('', values=c('Data'=cols[1], 'Mean'=cols[2])) +
                scale_fill_manual('', values=c('Data'=cols[1], 'Mean'=cols[2]))
        })
        return(ggplotly(p))
    })

    output$mean_distribution = renderPlotly({
        d = data.frame(x=mean_points$x)
        plt = ggplot(d)
        if(nrow(d) > 0){
            isolate({
                plt = plt +
                    geom_histogram(data=d, aes(x=x, fill='Mean', color='Mean'), bins=get_n_bins(d)) +
                    scale_color_manual(values=c('Truth'=cols[3], 'Mean'=cols[2])) +
                    scale_fill_manual('', values=c('Truth'=cols[3], 'Mean'=cols[2])) +
                    coord_cartesian(xlim=input$mean + 4*input$sd*c(-1, 1)) +
                    guides(color=FALSE)
            })
        }
        return(ggplotly(plt))
    })
})

shinyApp(ui = ui, server = server)
