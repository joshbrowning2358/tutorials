library(shiny)
library(ggplot2)

gg_color_hue = function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}

X_MIN = -20
X_MAX = 20

ui = shinyUI(fluidPage(
    
    titlePanel("Functions and Derivatives"),
    
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId='x', label='x', min=X_MIN, max=X_MAX, value=0, step=0.25),
            sliderInput(inputId='h', label='h', min=0.001, max=10, value=5, step=0.1),
            textInput('func', label='Function', value='x*(x-15)*(x+15)'),
            fluidRow(
                actionButton(inputId='compute', label='Compute'),
                actionButton(inputId='compute_all', label='All')
            ),
            fluidRow(
                actionButton(inputId='clear', label='Clear')
            )
        ),
        
        mainPanel(
            plotOutput("pairplot", height=600)
        )
    )
))

server = shinyServer(function(input, output) {
    cols = gg_color_hue(3)
    
    derivative_points = reactiveValues(x=c(), y=c())
    
    observeEvent({
        input$clear
        # Clear mean if parameters are changed
        input$func}, {
            derivative_points$x = c()
            derivative_points$y = c()
        })
    
    f = reactive({
        eval(parse(text=input$func))
    })
    
    data = reactive({
        x = seq(-20, 20, length.out=1000)
        y = eval(parse(text=input$func))
        return(data.frame(x=x, y=y))
    })
    
    observeEvent(input$compute, {
        derivative_points$x = c(derivative_points$x, input$x)
        x = input$x + epsilon
        y_x_plus_h = eval(parse(text=input$func))
        x = input$x - epsilon
        y_x_minus_h = eval(parse(text=input$func))
        new_y = (y_x_plus_h - y_x_minus_h) / (2*epsilon)
        derivative_points$y = c(derivative_points$y, new_y)
    })
    
    observeEvent(input$compute_all, {
        sapply(seq(-20, 20, 0.25), function(x_0){
            derivative_points$x = c(derivative_points$x, x_0)
            x = x_0 + epsilon
            y_x_plus_h = eval(parse(text=input$func))
            x = x_0 - epsilon
            y_x_minus_h = eval(parse(text=input$func))
            new_y = (y_x_plus_h - y_x_minus_h) / (2*epsilon)
            derivative_points$y = c(derivative_points$y, new_y)
        })
    })
    
    secant_data = reactive({
        x = input$x + c(0, input$h)
        y = eval(parse(text=input$func))
        plot_x = c(-20, 20)
        m = (y[2] - y[1]) / (x[2] - x[1])
        plot_y = m * (plot_x - x[1]) + y[1]
        return(data.frame(x=plot_x, y=plot_y))
    })
    
    output$pairplot = renderPlot({
        input$compute
        d = data()
        secant = secant_data()
        derivative = data.frame(x=derivative_points$x, y=derivative_points$y)
        colors = gg_color_hue(3)
        out = ggplot(d, aes(x=x, y=y)) +
            geom_line(aes(color='Function')) +
            geom_line(data=secant, aes(color='Secant'))
        if(nrow(derivative) > 0)
            out = out + geom_point(data=derivative, aes(color='Derivative'))
        out + scale_color_manual('', values=c('Function'=colors[1], 'Derivative'=colors[2], 'Secant'=colors[3]))
    })
})

shinyApp(ui = ui, server = server)
