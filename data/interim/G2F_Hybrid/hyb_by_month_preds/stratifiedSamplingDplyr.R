by_cyl <- mtcars %>% group_by(cyl)
# Sample fixed number per group
sample_n(mtcars, 10)
sample_n(mtcars, 50, replace = TRUE)
sample_n(mtcars, 26, weight = mpg)
sample_n(by_cyl, 3)
sample_n(by_cyl, 10, replace = TRUE)
sample_n(by_cyl, 3, weight = mpg / mean(mpg))
# Sample fixed fraction per group
# Default is to sample all data = randomly resample rows
dim(sample_frac(mtcars))
dim(sample_frac(mtcars, 0.1))
dim(sample_frac(mtcars, 1.5, replace = TRUE))
sample_frac(mtcars, 0.1, weight = vs)
sample_frac(by_cyl, 0.2)
sample_frac(by_cyl, 1, replace = TRUE)


sample_frac(df, .2, weight = Pedi)
test %>% 
    group_by(Pedi) %>%
    summarise(count = n()) %>% table()

print( arrange( df, category ) )

