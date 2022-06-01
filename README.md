# decileSegmentedDistributions
Groups a `dimension` by its own deciles then outputs an aggregate of a `metric`. Matrices, data.tables and dataframes will be cast as tibbles. Variables passed through `dimension` and `metric` must be numeric or integer.
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
<br>

# Arguments
| Argument | Description |
| --- | --- |
| `.data` | A data frame, tibble, matrix, or data table. Will be cast as a tibble internally. |
| `dimension` | The variable/column/field to segment. |
| `metric` | The variable/column/field distributions will be printed out for. |
| `na.rm` | Determines if NA values are to be included. By default ignores NA values. |

# Function
```r
decileSegmentedDistributions <- function(.data, dimension, metric, na.rm = TRUE) {
  require(dplyr)
  require(rlang)
  
  .data <- as_tibble(.data)
  
  if (is.numeric(.data %>% select( {{ dimension }}, {{ metric }} ) %>% as.matrix() )  )   {
    vector1 <- .data %>% pull( {{ dimension }} )
    thresholds <- quantile(vector1, probs = c(0.10, 0.20, 0.30, 0.40, 0.5, 0.60, 0.70, 0.80, 0.90), na.rm = na.rm)
    vector2 <- if_else(vector1 <= thresholds[[1]], paste0('min <= ', as_label(enquo(dimension)), ' <= p10'),
               if_else(vector1 <= thresholds[[2]], paste0('p10 < ', as_label(enquo(dimension)), ' <= p20'),
               if_else(vector1 <= thresholds[[3]], paste0('p20 < ', as_label(enquo(dimension)), ' <= p30'),
               if_else(vector1 <= thresholds[[4]], paste0('p30 < ', as_label(enquo(dimension)), ' <= p40'),
               if_else(vector1 <= thresholds[[5]], paste0('p40 < ', as_label(enquo(dimension)), ' <= p50'),
               if_else(vector1 <= thresholds[[6]], paste0('p50 < ', as_label(enquo(dimension)), ' <= p60'),
               if_else(vector1 <= thresholds[[7]], paste0('p60 < ', as_label(enquo(dimension)), ' <= p70'),
               if_else(vector1 <= thresholds[[8]], paste0('p70 < ', as_label(enquo(dimension)), ' <= p80'),
               if_else(vector1 <= thresholds[[9]], paste0('p80 < ', as_label(enquo(dimension)), ' <= p90'),
                                                   paste0('p90 < ', as_label(enquo(dimension)), ' <= max')
                    )
                  )
                )
              )
            )
          )
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
        !!lhsNames( {{ metric }}, 'mean') := paste0(round((mean( {{ metric }}, .groups = 'drop', na.rm = na.rm) * 100), 2), '%')
      ) %>% 
      rename(!!lhsNames( {{ dimension }}, 'range (pxx representing percentiles)') := 'value') %>%
      print()
      print(paste0(as_label(enquo(dimension)), ' deciles'))
      print(thresholds)
    
  } else {
    stop('Pass numeric dimension and metric variables. Only numeric data permissable.')
  }
}
```
