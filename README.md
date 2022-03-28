# classingFunction
Groups a ```dimension``` by its own quartiles (the thresholds for each group being p25, p50 and p75) then outputs an aggregate of a ```metric```. ```dimension``` must be either an integer or numeric in class.

### Languages and Tools
<div>
  <img src="https://github.com/devicons/devicon/blob/master/icons/r/r-original.svg" title="r" alt="r" width="70" height="70"/>&nbsp;
  <img src="https://github.com/devicons/devicon/blob/master/icons/rstudio/rstudio-original.svg" title="RStudio" alt="RStudio" width="70" height="70"/>&nbsp;
</div>

### Packages
<div>
  <img src="https://github.com/tidyverse/dplyr/raw/main/man/figures/logo.png" height="100" style="max-width: 100%;"/>&nbsp;
  <img src="https://github.com/tidyverse/rlang/raw/main/man/figures/logo.png" height="100" style="max-width: 100%;"/>&nbsp;
</div>
<br>
<br>

# Function
```r
classingFunction <- function(.data, dimension, metric) {
  require(dplyr)
  require(rlang)
  
  if (is.matrix(.data)) {
    .data <- as_tibble(.data) %>% 
      mutate({{ dimension }} := as.numeric({{ dimension }}) )
  }
  
  if (is.data.frame(.data)) {
    .data <- as_tibble(.data)
  }
  
  if (is.numeric(.data %>% select({{ dimension }}) %>% as.matrix() )  )   {
    vector1 <- .data %>% select({{ dimension }}) %>% as.matrix()
    thresholds <- quantile(vector1, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
    vector2 <- if_else(vector1 <= thresholds[[1]], '> p0, <= p25',
                       if_else(vector1 <= thresholds[[2]], '> p25, <= p50',
                               if_else(vector1 <= thresholds[[3]], '> p50, <= p75', '> p75, <= p100'
                                       )
                               )
                       ) %>%
      as_tibble() %>%
      rename(dimension_class := 'value') 
    
    bind_cols(.data, vector2) %>% 
      group_by(dimension_class) %>% 
      summarise(mean = mean({{ metric }}), .groups = 'drop')
  } else {
    stop('Cast the dimension variable to either a numeric or integer. Only numerical data is allowed')
  }
  
}
```
