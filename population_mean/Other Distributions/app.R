library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(sn)

gg_color_hue = function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}

get_n_bins = function(data){
    return(1 + 3.322*log(nrow(data)))
}

ui = dashboardPage(
    dashboardHeader(title="Population vs. Mean Distribution"),
   
    dashboardSidebar(disable=TRUE),
    
    dashboardBody(
        fluidRow(
            column(width=4,
                tabBox(
                    width=NULL,
                    id='distribution',
                    tabPanel(
                        title='Normal',
                        sliderInput(inputId='normal_mean', label='Mean', min=-20, max=20, value=0, step=1),
                        sliderInput(inputId='normal_sd', label='Standard Deviation', min=0.1, max=10, value=1, step=0.1)
                    ),
                    tabPanel(
                        title='Bernoulli',
                        sliderInput(inputId='bernoulli_p', label='Probability', min=0, max=1, value=0.5, step=0.01)
                    ),
                    tabPanel(
                        title='Binomial',
                        sliderInput(inputId='binomial_p', label='Probability', min=0, max=1, value=0.5, step=0.01),
                        sliderInput(inputId='binomial_n', label='Draws', min=1, max=100, value=10, step=1)
                    ),
                    tabPanel(
                        title='Uniform',
                        sliderInput(inputId='uniform_range', label='Range', min=-20, max=20, value=c(0, 1), step=0.01)
                    ),
                    tabPanel(
                        title='Poisson',
                        sliderInput(inputId='poisson_lambda', label='Lambda', min=0.01, max=10, value=1, step=0.01)
                    ),
                    tabPanel(
                        title='Geometric',
                        sliderInput(inputId='geometric_p', label='Probability', min=0.01, max=1, value=0.5, step=0.01)
                    ),
                    tabPanel(
                        title='Log-Normal',
                        sliderInput(inputId='log_normal_mean', label='log(Mean)', min=-10, max=10, value=0, step=0.01),
                        sliderInput(inputId='log_normal_sd', label='log(sd)', min=0, max=10, value=1, step=0.01)
                    ),
                    tabPanel(
                        title="Student's t",
                        sliderInput(inputId='t_df', label='Degrees of Freedom', min=1, max=500, value=30, step=1),
                        sliderInput(inputId='t_mean', label='Mean', min=-20, max=20, value=0, step=0.1)
                    ),
                    tabPanel(
                        title="Skew Normal",
                        sliderInput(inputId='skew_normal_mean', label='Mean', min=-20, max=20, value=0, step=0.01),
                        sliderInput(inputId='skew_normal_sd', label='Standard Deviation', min=0, max=10, value=1, step=0.01),
                        sliderInput(inputId='skew_normal_skew', label='Skew', min=-10, max=10, value=3, step=0.01)
                    ),
                    tabPanel(
                        title="Cauchy",
                        sliderInput(inputId='cauchy_location', label='Location', min=-10, max=10, value=0, step=0.01),
                        sliderInput(inputId='cauchy_scale', label='Scale', min=0.01, max=10, value=1, step=0.01)
                    )
                ),
                sliderInput(inputId='n_samples', label='# Samples', min=1, max=1000, value=10, step=1),
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
            column(width=8,
                id='outputs',
                plotlyOutput("population_distribution", height=450),
                plotlyOutput("mean_distribution", height=450)
            )
        )
    )
)

server = shinyServer(function(input, output) {
    n_simulations <<- 1
    
    cols = gg_color_hue(3)
    
    mean_points = reactiveValues(x=c())

    data = reactive({
        input$resimulate
        input$resimulate10
        input$resimulate100
        input$resimulate1000
        if(input$distribution == 'Normal')
            result = rnorm(n_simulations * input$n_samples, input$normal_mean, input$normal_sd)
        else if(input$distribution == 'Bernoulli')
            result = rbinom(n_simulations * input$n_samples, size=1, prob=input$bernoulli_p)
        else if(input$distribution == 'Binomial')
            result = rbinom(n_simulations * input$n_samples, size=input$binomial_n, prob=input$binomial_p)
        else if(input$distribution == 'Uniform')
            result = runif(n_simulations * input$n_samples, min=input$uniform_range[1], max=input$uniform_range[2])
        else if(input$distribution == 'Poisson')
            result = rpois(n_simulations * input$n_samples, lambda=input$poisson_lambda)
        else if(input$distribution == 'Geometric')
            result = rgeom(n_simulations * input$n_samples, prob=input$geometric_p)
        else if(input$distribution == 'Log-Normal')
            result = rlnorm(n_simulations * input$n_samples, meanlog=input$log_normal_mean, sdlog=input$log_normal_sd)
        else if(input$distribution == "Student's t")
            result = rt(n_simulations * input$n_samples, df=input$t_df, ncp=input$t_mean)
        else if(input$distribution == 'Skew Normal')
            result = rsn(n_simulations * input$n_samples, xi=input$skew_normal_mean, omega=input$skew_normal_sd, alpha=input$skew_normal_skew)
        else if(input$distribution == 'Cauchy')
            result = rcauchy(n_simulations * input$n_samples, location=input$cauchy_location, scale=input$cauchy_scale)
        else
            stop('Distribution not implemented!')
        new_means = tapply(result, rep(1:n_simulations, each=input$n_samples), mean)
        isolate({mean_points$x = c(mean_points$x, new_means)})
        return(result[1:input$n_samples])
    })
    
    observeEvent({
            input$clear
            # Clear mean distribution if parameters are changed
            input$n_samples
            input$normal_mean; input$normal_sd
            input$bernoulli_p
            input$binomial_p; input$binomial_n
            input$uniform_range
            input$poisson_lambda
            input$geometric_p
            input$log_normal_mean; input$log_normal_sd
            input$t_df; input$t_mean
            input$skew_normal_mean; input$skew_normal_sd; input$skew_normal_skew
            input$cauchy_location; input$cauchy_scale
            input$distribution
        }, {
            mean_points$x = c()
    })
    
    observeEvent(input$resimulate10, {
        n_simulations <<- 10
        data()
        n_simulations <<- 1
    })

    observeEvent(input$resimulate100, {
        n_simulations <<- 100
        data()
        n_simulations <<- 1
    })
    
    observeEvent(input$resimulate1000, {
        n_simulations <<- 1000
        data()
        n_simulations <<- 1
    })
    
    plot_range = reactive({
        if(input$distribution == 'Normal')
            return(input$normal_mean + 4 * input$normal_sd * c(-1, 1))
        else if(input$distribution == 'Bernoulli')
            return(c(-.05, 1.05))
        else if(input$distribution == 'Binomial')
            return(c(-.05, input$binomial_n + 0.05))
        else if(input$distribution == 'Uniform')
            return(input$uniform_range + c(-0.5, 0.5))
        else if(input$distribution == 'Poisson')
            return(c(-0.05, max(3*input$poisson_lambda, 1)))
        else if(input$distribution == 'Geometric')
            return(c(-0.05, 15 * sqrt((1 - input$geometric_p) / input$geometric_p^2)))
        else if(input$distribution == 'Log-Normal')
            return(NULL)
        else if(input$distribution == "Student's t")
            return(NULL)
        else if(input$distribution == 'Skew Normal')
            return(NULL)
        else if(input$distribution == 'Cauchy')
            return(NULL)
        else
            stop('Distribution range not implemented!')
    })
    
    output$population_distribution = renderPlotly({
        d = data.frame(x=c(mean(data()), data()))
        d$name = c('Mean', rep('Data', nrow(d) - 1))
        isolate({
            p = ggplot(d) +
                geom_histogram(aes(x=x, fill=name, color=name), bins=get_n_bins(d)) +
                coord_cartesian(xlim=plot_range()) +
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
                    coord_cartesian(xlim=plot_range()) +
                    guides(color=FALSE)
            })
        }
        return(ggplotly(plt))
    })
})

shinyApp(ui = ui, server = server)
