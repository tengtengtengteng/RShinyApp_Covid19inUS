select <- dplyr::select
count <- dplyr::count

# calculates and returns a CI table
calc_CI <- function(df, var1, var2){
  summary_df <- df %>%
    select(sym(var1), sym(var2)) %>%
    group_by(!!sym(var2)) %>%
    summarize(Max = max(!!sym(var1), na.rm = TRUE), 
              Mean = mean(!!sym(var1), na.rm = TRUE),
              Sd = sd(!!sym(var1), na.rm = TRUE),
              Median = median(!!sym(var1), na.rm = TRUE),
              n = n())
  summary_df$lower_CI <- summary_df$Mean - 
    qnorm(0.975) * summary_df$Sd/sqrt(summary_df$n)
  summary_df$upper_CI <- summary_df$Mean + 
    qnorm(0.975) * summary_df$Sd/sqrt(summary_df$n)
  
  return(summary_df)
}

# plot CI
plot_CI <- function(df, var1, var2){
  summary_df <- df %>%
    select(sym(var1), sym(var2)) %>%
    group_by(!!sym(var2)) %>%
    summarize(Mean = mean(!!sym(var1), na.rm = TRUE),
              Sd = sd(!!sym(var1), na.rm = TRUE),
              n = n())
  summary_df$lower_CI <- summary_df$Mean - 
    qnorm(0.975)* summary_df$Sd/sqrt(summary_df$n)
  summary_df$upper_CI <- summary_df$Mean + 
    qnorm(0.975)* summary_df$Sd/sqrt(summary_df$n)
  
  ci_plot <- ggplot(summary_df, aes(Mean, !!sym(var2)))
  ci_plot <- ci_plot + geom_point() +
    geom_errorbarh(aes(xmax = upper_CI, xmin = lower_CI, height = .2))+
    labs(x = var1)
  
  return(ci_plot)
}

# plot boxplot
plot_boxplot <- function(df, y, x, group){
  if(missing(group)){
    bplot <- ggplot(df, aes(y = !!sym(y), x = !!sym(x)))+
      geom_boxplot()+
      labs(title = paste('Boxplots of', y, 'by', x),
           x = gsub('_', ' ', x), y = y)
  } else {
    bplot <- ggplot(df, aes(y = !!sym(y), x = !!sym(x), color = !!sym(group)))+
      geom_boxplot()+
      labs(title = paste('Boxplots of', y, 'by', x),
           x = gsub('_', ' ', x), y = y)
  }
  
  return (bplot)
}

# anova table (can be both 1 way or 2 way)
print_anova_table <- function(df, y, x1, x2){
  if(missing(x2)){
    aov.model <- aov(as.formula(paste(y, '~', x1)), data = df)
  } else {
    aov.model<-aov(as.formula(paste(y, '~', x1, '*', x2)), data = df)
  }
  cat('-------------------    ANOVA     --------------------\n')
  print(summary(aov.model))
  cat("\n")
  
  cat('---------------Tukey HSD for Factor A----------------\n')
  tukey <- TukeyHSD(aov.model, which = x1)
  print(tukey)
  cat("\n")
  if(!missing(x2)){
    cat('---------------Tukey HSD for Factor B----------------\n')
    tukey <- TukeyHSD(aov.model, which = x2)
    print(tukey)
  }
}

# MLR any direction
print_stepwise_MLR <- function(df, y, X, strategy){
  Rdata <- df[,c(y, X)]
  Rdata <- na.omit(Rdata)

  fit1 <- lm(as.formula(paste(y, '~ .')), data = Rdata)
  fit2 <- lm(as.formula(paste(y, '~ 1')), data = Rdata)

  if(strategy=="Backward Selection"){
    step <- stepAIC(fit1, direction = "backward")
  } else if(strategy=="Forward Selection"){
    step <- stepAIC(fit2, direction = "forward", scope = list(upper = fit1, lower = fit2))
  } else {
    step <- stepAIC(fit2, direction = "both", scope = list(upper = fit1, lower = fit2))
  }
  return(summary(step))
}

# correlation between 2 variable only
print_correlation <- function(df, y, x){
  cor_table <- df %>% 
    select(c(sym(y), sym(x))) %>% 
    drop_na() %>% 
    cor()
  cor_table[1, 2]
}

# ggplot scatterplot with option of line
plot_scatterplot <- function(df, y, x, line){

  g <- ggplot(df, aes_string(x=sym(x), y=y)) + geom_point() + theme_bw()
  if(missing(line)){
    g
  } else {
    g + geom_smooth(method='lm', se=FALSE) + theme_bw()
  }
}

ggplotly_scatterplot <- function(df, y, x){
  ggplotly(plot_scatterplot(df, y, x, line=TRUE))
}

# plotly scatterplot with line
plotly_scatterplot <- function(df, x, y){
    g <- plot_ly(df, x=as.formula(paste('~',x)) , y=as.formula(paste('~',y))) %>%
      add_markers(y = as.formula(paste('~',y)), name = x) %>%
      add_lines(x=as.formula(paste('~',x)),
                y = fitted(lm(as.formula(paste(y, '~', x)), data=df)),
                name = 'Best Fit Line'
                ) #%>%
      #layout(title = paste("Correlation between", y, "and", x))
    
    g
}

# predict based on user inputs and best MLR model
predict_MLR <- function(inputs){
  
  totaltest <- as.numeric(inputs[1])
  pci <- as.numeric(inputs[2])
  political <- as.numeric(inputs[3])
  popdens <- as.numeric(inputs[4])
  pop <- as.numeric(inputs[5])
  temp <- as.numeric(inputs[6])
  stayhome <- inputs[7]
  quarantine <- inputs[8]
  facecover <- inputs[9]
  
  prediction <- 5.580e+05 +
    2.456e+00 * totaltest +
    -2.766e+00 * pci +
    -2.096e+03 * political +
    -9.218e+01 * popdens +
    2.124e-02 * pop +
    -2.317e+03 * temp

  if(stayhome == 'High Risk Groups'){
    prediction = prediction + -6.142e+04
  } else if(stayhome == 'No Restrictions'){
    prediction = prediction + -1.827e+05
  } else if (stayhome == 'Statewide'){
    prediction = prediction + -1.762e+05
  }

  if(quarantine == 'From Certain States'){
    prediction = prediction + 7.837e+03
  } else if(quarantine == 'International Travelers'){
    prediction = prediction + 1.232e+05
  } else if (quarantine == 'No Restrictions'){
    prediction = prediction + 4.929e+04
  }
  
  if(facecover == 'Required For Certain Employees'){
    prediction = prediction + 3.756e+03
  } else if (facecover == 'Required For General Public'){
    prediction = prediction + 4.231e+04
  }

  prediction
}

# summary statistics of x
print_summary <- function(df, x){
  summary(df[,x])
}

# plot distribution of variable
plot_descriptive <- function(df, var){
  never_changing <- c('Income_per_Capita', 'Democratic_Advantage', 'Population_Density', 'Population', 'Temp')
  continuous <- c('Death', 'Recovered', 'TotalTestResultsIncrease')
  categorical <- c('Stay_at_Home_Order', 'Mandatory_Quarantine_for_Travelers', 'Face_Covering_Requirement')
  if(var %in% never_changing){
    ggplotly(plot_distribution(df, var))
  } else if (var %in% continuous) {
    ggplotly(plot_line_continuous(df, 'Date', var, 'State'))
  } else if (var %in% categorical) {
    ggplotly(plot_line_categorical(df, 'Date', var))
  }
}

# plot the never changing ones
plot_distribution <- function(df, x){
  df <- df %>% group_by(State) %>% summarize(max(!!sym(x)))
  colnames(df) <- c('State', x)
  g <- ggplot(data=df, aes(x=!!sym(x))) + 
    geom_histogram(bins=15, color='darkblue', fill='lightblue', alpha=0.5) + 
    theme_bw()
  g
}

# continuous plot vs time
plot_line_continuous <- function(df, x, y, group){
  if(missing(group)){
    ggplot(df, aes_string(x=sym(x), y=sym(y)), color=sym(group)) + 
      #geom_point() +
      geom_line() +
      theme_bw()
  } else {
    ggplot(df, aes_string(x=sym(x), y=sym(y), color=sym(group)))+#, label=sym(group))) + 
      #geom_point() + 
      geom_line() +
      #geom_text(aes(label=ifelse(Date>=ymd('2020-12-13'), as.character(!!sym(group)),'')), hjust='outwards' ) +
      theme_bw() +
      #theme(legend.position = 'none') +
      scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
      theme(legend.title = element_blank())
  }
}

# categorical plot vs time (count of category vs time)
plot_line_categorical <- function(df, x, y){
  len_x <- length(unique(df[[x]]))
  len_y <- length(unique(df[[y]]))
  categorical_df <- data.frame(rep(unique(df[[x]]), len_y), rep(unique(df[[y]]), len_x))
  colnames(categorical_df) <- c(x, y)
  temp_df <- df %>% group_by(!!sym(x), !!sym(y)) %>% count()
  plot_df <- categorical_df %>% left_join(temp_df) %>% mutate_if(is.numeric, replace_na, replace=0)
  
  ggplot(plot_df, aes_string(x=sym(x), y='n', color=sym(y))) + geom_line(size=1.5) + theme_bw()
}

plot_main <- function(df, state_name, measure, group){
  if (state_name=='All'){
    df_plot <- df %>% group_by(Date) %>% summarize(sum(!!sym(measure)))
    colnames(df_plot) <- c('Date', measure)
    ggplotly(
      plot_line_continuous(df_plot,
                           x = 'Date',
                           y = measure)
    )
  } else {
    df_plot <- df %>% filter(State==state_name)
    ggplotly(
      plot_line_continuous(df_plot,
                           x = 'Date',
                           y = measure,
                           group = group)
    )
  }
}

plotly_map <- function(map_df){
  # specify map projection/options
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    lakecolor = toRGB('white')
  )
  
  plot_geo() %>%
    add_trace(
      z = ~map_df,
      locations = names(map_df),
      locationmode = 'USA-states',
      colorscale = 'Blue'
    ) %>%
    colorbar(title = 'Number of Positive Cases') %>%
    layout(geo = g)
    #layout(title = 'Number of Positive COVID-19 Cases in US States', geo = g)
}