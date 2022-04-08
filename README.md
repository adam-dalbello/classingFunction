# segmentedDistributions Function
Groups a `dimension` by its own quartiles (the thresholds for each group being p25, p50 and p75) then outputs an aggregate of a `metric`. `dimension` must be either an integer or numeric in class.

Matrices, data.tables and dataframes will be cast as tibbles. Variables passed through `dimension` and `metric` must be numeric or integers.
<br>
<br>

### Languages and Tools
<div>
  <img src="https://github.com/devicons/devicon/blob/master/icons/r/r-original.svg" title = "r" alt = "r" width = "60" height = "60"/>&nbsp;
  <img src="https://github.com/devicons/devicon/blob/master/icons/rstudio/rstudio-original.svg" title = "RStudio" alt = "RStudio" width = "60" height = "60"/>&nbsp;
</div>

### Packages
<div>
  <img src="https://github.com/tidyverse/dplyr/raw/main/man/figures/logo.png" height = "100" style = "max-width: 100%;"/>&nbsp;
  <img src="https://github.com/tidyverse/rlang/raw/main/man/figures/logo.png" height = "100" style = "max-width: 100%;"/>&nbsp;
  <img src="https://raw.githubusercontent.com/tidyverse/tibble/main/man/figures/logo.png" height = "100" style = "max-width: 100%;"/>&nbsp;
  <img src="https://github.com/tidyverse/ggplot2/raw/main/man/figures/logo.png" height = "100" style = "max-width: 100%;"/>&nbsp;
  <img src="https://github.com/tidyverse/tidyr/raw/main/man/figures/logo.png"  height = "100" style = "max-width: 100%;"/>&nbsp;
</div>
<br>
<br>

### Arguments
| Argument | Description |
| --- | --- |
| `.data` | A data frame, tibble, matrix, or data table. Will be cast as a tibble internally. |
| `dimension` | The variable/column/field to segment. |
| `metric` | The variable/column/field distributions will be printed out for. |
| `na.rm` | Determines if NA values are to be included. By default ignores NA values. |

# Function
```r
segmentedDistributions <- function(.data, dimension, metric, date = NULL, na.rm = TRUE) {
  require(dplyr)
  require(rlang)
  
  .data <- as_tibble(.data)
  
  if (is.numeric(.data %>% select( {{ dimension }}, {{ metric }} ) %>% as.matrix() )  )   {
    vector1 <- .data %>% pull( {{ dimension }} )
    thresholds <- quantile(vector1, probs = c(0.25, 0.5, 0.75), na.rm = na.rm)
    vector2 <- if_else(vector1 <= thresholds[[1]], paste0('p0 < ', as_label(enquo(dimension)), ' <= p25'),
                       if_else(vector1 <= thresholds[[2]], paste0('p25 < ', as_label(enquo(dimension)), ' <= p50'),
                               if_else(vector1 <= thresholds[[3]], paste0('p50 < ', as_label(enquo(dimension)), ' <= p75'),
                                       paste0('p75 < ', as_label(enquo(dimension)), ' <= p100')
                               )
                       )
               ) %>% 
      as_tibble()
    
    lhsNames <- function(variable, measure) {
      paste0(as_name(enquo(variable)), '_', measure)
    }
    
    bind_cols(.data, vector2) %>% 
      group_by(value) %>% 
      summarise(
        observations = n(),
        !!lhsNames( {{ metric }}, 'min') := min( {{ metric }}, na.rm = na.rm),
        !!lhsNames( {{ metric }}, 'p25') := quantile( {{ metric }}, prob = 0.25, na.rm = na.rm),
        !!lhsNames( {{ metric }}, 'p50') := quantile( {{ metric }}, prob = 0.50, na.rm = na.rm),
        !!lhsNames( {{ metric }}, 'mean') := mean( {{ metric }}, .groups = 'drop', na.rm = na.rm),
        !!lhsNames( {{ metric }}, 'p75') := quantile( {{ metric }}, prob = 0.75, na.rm = na.rm),
        !!lhsNames( {{ metric }}, 'max') := max( {{ metric }}, na.rm = na.rm)
      ) %>% 
      rename(!!lhsNames( {{ dimension }}, 'class') := 'value') %>%
      print()
      
  } else {
    stop('Pass numeric dimension and metric variables. Only numeric data permissable.')
  }
  
  if (length(.data %>% pull( {{ date }} )) > 0) {
    bind_cols(.data, vector2) %>%
      group_by( {{ date }}, value) %>% 
      summarise(
        p25 = quantile( {{ metric }}, prob = 0.25, na.rm = na.rm),
        mean = mean( {{ metric }}, na.rm = na.rm),
        p50 = quantile( {{ metric }}, prob = 0.50, na.rm = na.rm),
        p75 = quantile( {{ metric }}, prob = 0.75, na.rm = na.rm)
      ) %>%
      rename('class' = 'value' ) %>% 
      tidyr::pivot_longer(cols = c('p25', 'mean', 'p50', 'p75'), names_to = 'measure', values_to = as_name(enquo(metric))) %>% 
      mutate(measure = factor(measure, levels = c('p25', 'mean', 'p50', 'p75'))) %>% 
      ggplot2::ggplot(aes( {{ date }}, {{ metric }}, col = measure, group = measure)) +
      geom_line() +
      geom_point(alpha = 0.5, size = 1) +
      facet_grid(. ~ class) +
      theme(
        panel.background = element_rect(fill = 'grey94'),
        text = element_text(family = 'Segoe UI'),
        strip.background.x = element_rect(fill = 'white')
      ) +
      ggtitle(paste(as_label(enquo(dimension)), 'Segmented', as_label(enquo(metric)), 'Distribution', by = ' '))
  }
  
}

#> # A tibble: 3 x 8
#>   levels_completed_class observations levels_completed_min levels_completed_p25 levels_completed_p50 levels_completed_mean levels_completed_p75 levels_completed_max
#>   <chr>                         <int>                <dbl>                <dbl>                <dbl>                 <dbl>                <dbl>                <dbl>
#> 1 > p0, <= p25                   6317                    0                    0                    0                     0                    0                    0
#> 2 > p50, <= p75                  4185                    1                    1                    2                  1.65                    2                    2
#> 3 > p75, <= p100                 1213                    3                    3                    3                     3                    3                    3
```

![segmented line plot white](https://user-images.githubusercontent.com/25012294/162504843-5a4615c2-0fd3-40c9-9eea-a0fd29b998f2.png)
