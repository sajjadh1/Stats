qnorm(0.05, mean = 145, 14.491)

2*pt(-abs(-0.939), df = 20)

qt(0.025, 20)

1 - pt(2.52, 20)


male_data <- c(2.382, 1.726, 2.056, 2.751, 3.042, 2.635, 3.001)
female_data <- c(2.988, 3.111, 2.076, 2.798, 2.352, 3.350, 2.571, 4.324, 1.611, 1.991, 1.873, 1.895, 3.977)

n_male <- length(male_data)
n_female <- length(female_data)

mean_male <- mean(male_data)
mean_female <-mean(female_data)

var_male = var(male_data)
var_female = var(female_data)

pooled_var = ((n_male-1)*var_male) +((n_female-1)* var_female)
ans = pooled_var /(n_male +n_female -2)

pt(0.488, df =13+7-2)

