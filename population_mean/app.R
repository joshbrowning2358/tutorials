library(shiny)
library(ggplot2)

gg_color_hue = function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}

get_binwidth = function(data){
    potential_values = exp(seq(log(0.3), log(0.00001), length.out=100))
    max_counts = sapply(potential_values, function(binwidth){
        hist_counts = hist(data, breaks=seq(min(data), max(data) + binwidth, by=binwidth), plot=FALSE)$counts
        return(max(hist_counts))
    })
    return(max(potential_values[potential_values * max_counts < 12 * 0.3]))
}

ui = shinyUI(fluidPage(
   
    titlePanel("Population vs. Mean Distribution"),
   
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId='n_samples', label='# Samples', min=1, max=100, value=10, step=1),
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
            plotOutput("population_distribution", height=300),
            plotOutput("mean_distribution", height=300)
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
        isolate({
            obs = rnorm(input$n_samples, mean=input$mean, sd=input$sd)
            mean_points$x = c(mean_points$x, mean(obs))
        })
        return(obs)
    })
    
    output$population_distribution = renderPlot({
        d = data.frame(x=c(mean(data()), data()))
        d$name = c('Mean', rep('Data', nrow(d) - 1))
        binwidth = get_binwidth(d$x)
        isolate({
            x_vals = seq(input$mean - 4 * input$sd, input$mean + 4 * input$sd, length.out=1000)
            pdf = data.frame(x=x_vals, pdf=dnorm(x_vals, input$mean, input$sd))
            ggplot(d) +
                geom_dotplot(aes(x=x, fill=name, color=name), method='histodot', binwidth=binwidth*input$sd) +
                # geom_line(data=pdf, aes(x=x_vals, y=pdf, color='Truth', fill='Truth')) +
                scale_y_continuous(NULL, breaks=NULL) +
                coord_cartesian(xlim=input$mean + 4*input$sd*c(-1, 1)) +
                scale_color_manual('', values=c('Data'=cols[1], 'Mean'=cols[2])) +
                scale_fill_manual('', values=c('Data'=cols[1], 'Mean'=cols[2]))
        })
    })

    output$mean_distribution = renderPlot({
        d = data.frame(x=mean_points$x)
        x_vals = isolate(seq(input$mean - 4 * input$sd, input$mean + 4 * input$sd, length.out=1000))
        pdf = data.frame(x=x_vals, pdf=dnorm(x_vals, isolate(input$mean), isolate(input$sd/sqrt(input$n_samples))))
        #plt = ggplot(pdf) +
        #    geom_line(aes(x=x_vals, y=pdf, color='Truth', fill='Truth'))
        plt = ggplot(d)
        if(nrow(d) > 0){
            isolate({
                binwidth = get_binwidth(mean_points$x) * input$sd
                plt = plt + geom_dotplot(data=d, aes(x=x, fill='Mean', color='Mean'), binwidth=binwidth,
                                         method='histodot') +
                    scale_color_manual(values=c('Truth'=cols[3], 'Mean'=cols[2])) +
                    scale_y_continuous(NULL, breaks=NULL) +
                    scale_fill_manual('', values=c('Truth'=cols[3], 'Mean'=cols[2])) +
                    coord_cartesian(xlim=input$mean + 4*input$sd*c(-1, 1)) +
                    guides(color=FALSE)
            })
        }
        plt
    })
})

shinyApp(ui = ui, server = server)
