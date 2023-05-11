# Import the data
data <- read.csv("~/Documents/webpage/world-happiness-report-2021.csv")

# show first few lines of data
head(data[, c(1, 2, 3, 7, 8, 9, 10, 11, 12)])

###################################################################################################
# Global F-Test.
# See if data is useful.
###################################################################################################

# F-test for 6 variables
basic <- lm(Ladder.score ~ Logged.GDP.per.capita + Social.support + Healthy.life.expectancy + Freedom.to.make.life.choices + Generosity + Perceptions.of.corruption, data = data)
summary(basic)

# F-test for 7 variables
basic <- lm(Ladder.score ~ Regional.indicator + Logged.GDP.per.capita + Social.support + Healthy.life.expectancy + Freedom.to.make.life.choices + Generosity + Perceptions.of.corruption, data = data)
summary(basic)

###################################################################################################
# Stepwise.
# Weedout insignificant variables.
###################################################################################################

# Stepwise for 6 variables
min.model = lm(Ladder.score ~ 1, data = data)
step(min.model, 
     scope = (~ Logged.GDP.per.capita + Social.support + Healthy.life.expectancy + Freedom.to.make.life.choices + 
                Generosity + Perceptions.of.corruption),
     data = data,
     direction = 'forward')

# Stepwise for 7 variables
min.model = lm(Ladder.score ~ 1, data = data)
step(min.model, 
     scope = (~ Regional.indicator + Logged.GDP.per.capita + Social.support + Healthy.life.expectancy + Freedom.to.make.life.choices + 
                Generosity + Perceptions.of.corruption),
     data = data,
     direction = 'forward')

# TEST: Did not include in final model.
# Stepwise with all interaction terms
step(min.model, 
     scope = (~ (Regional.indicator + Logged.GDP.per.capita + I(Logged.GDP.per.capita^2) + Social.support + Healthy.life.expectancy + Freedom.to.make.life.choices + 
                      Generosity + I(Generosity^2) + Perceptions.of.corruption)^2),
     data = data,
     direction = 'forward')

# Since stepwise with 7 variables result in lower AIC, 
# we will consider the result of stepwise including Regional Indicator.

###################################################################################################
# Check the conclusion made with the stepwise regression
###################################################################################################

# Complete Model: Includes the regional indicator
# Reduced Model: Without Reginal Indicator
complete <- lm(Ladder.score ~ Regional.indicator + Logged.GDP.per.capita + Social.support + Healthy.life.expectancy + Freedom.to.make.life.choices + Generosity + Perceptions.of.corruption, data = data)
reduced <- lm(Ladder.score ~ Logged.GDP.per.capita + Social.support + Healthy.life.expectancy + Freedom.to.make.life.choices + Generosity + Perceptions.of.corruption, data = data)
anova(complete, reduced)

# The anova shows that including Regional Indicator will be a better model

summary(complete)
summary(reduced)

# Reduced Model: Without the Perceptions of Corruption 
complete <- lm(Ladder.score ~ Regional.indicator + Logged.GDP.per.capita + Social.support + Healthy.life.expectancy + Freedom.to.make.life.choices + Generosity + Perceptions.of.corruption, data = data)
reduced <- lm(Ladder.score ~ Regional.indicator + Logged.GDP.per.capita + Social.support + Healthy.life.expectancy + Freedom.to.make.life.choices + Generosity, data = data)
anova(complete, reduced)

summary(reduced)

# The anova shows that when Regional Indicator is included, Perceptions of Corruption is not a significant variable.

# Reduced Model: Without the Healthy Life Expectancy and Perceptions of Corruption data
complete <- lm(Ladder.score ~ Regional.indicator + Logged.GDP.per.capita + Social.support + Healthy.life.expectancy + Freedom.to.make.life.choices + Generosity + Perceptions.of.corruption, data = data)
reduced <- lm(Ladder.score ~ Regional.indicator + Logged.GDP.per.capita + Social.support + Freedom.to.make.life.choices + Generosity, data = data)
anova(complete, reduced)

summary(reduced)

# The anova shows that when Regional Indicator is included, 
# Perceptions of Corruption and Healthy Life Expectancy is not a significant variable.

# TEST. Did not include in final model.
# Reduced Model: missing the Healthy Life Expectancy, Perceptions of Corruption, and Generosity data
complete <- lm(Ladder.score ~ Regional.indicator + Logged.GDP.per.capita + Social.support + Healthy.life.expectancy + Freedom.to.make.life.choices + Generosity + Perceptions.of.corruption, data = data)
reduced <- lm(Ladder.score ~ Regional.indicator + Logged.GDP.per.capita + Social.support + Freedom.to.make.life.choices, data = data)
anova(complete, reduced)

summary(reduced)

# The anova shows that when Regional Indicator is included, 
# Perceptions of Corruption, Healthy Life Expectancy, and Generosity is not a significant variable.
# However, there's no big difference in the p-value or the adjusted R-square for all three models.

###################################################################################################
# Check Assumptions
###################################################################################################

# Plot residual for residual analysis
plot(data$Logged.GDP.per.capita, reduced$residuals)

# Plot qqplot to get better when you reduce the variables in the model though.
qqnorm(reduced$residuals)
qqline(reduced$residuals)

# Plot the data vs. residual to see if there's any weird trend in the residual
plot(data$Logged.GDP.per.capita, basic$residual)
plot(data$Regional.indicator, data$Ladder.score)
plot(data$Logged.GDP.per.capita, data$Ladder.score)
plot(data$Social.support, data$Ladder.score)
#plot(data$Healthy.life.expectancy, data$Ladder.score)
plot(data$Freedom.to.make.life.choices, data$Ladder.score)
plot(data$Generosity, data$Ladder.score)
#plot(data$Perceptions.of.corruption, data$Ladder.score)

###################################################################################################
# Second-Order Terms
###################################################################################################

# Reduced model to compare to. Without healthy life expectancy and perception of corruption
reduced <- lm(Ladder.score ~ Regional.indicator + Logged.GDP.per.capita + Social.support + Freedom.to.make.life.choices + Generosity, data = data)

# Squared Logged GDP per capita
squared <- lm(Ladder.score ~ Regional.indicator + Logged.GDP.per.capita + Social.support + Freedom.to.make.life.choices + Generosity 
              + I(Logged.GDP.per.capita^2), data = data)

# Test. Did not include in Final Model.
# Squared Social support
squared <- lm(Ladder.score ~ Regional.indicator + Logged.GDP.per.capita + Social.support + Freedom.to.make.life.choices + Generosity 
          + I(Social.support^2), data = data)

# Test. Did not include in Final Model.
# Squared Freedom to make life choices
squared <- lm(Ladder.score ~ Regional.indicator + Logged.GDP.per.capita + Social.support + Freedom.to.make.life.choices + Generosity 
          + I(Freedom.to.make.life.choices^2), data = data)

# Squared Generosity
squared <- lm(Ladder.score ~ Regional.indicator + Logged.GDP.per.capita ++ Social.support + Freedom.to.make.life.choices + Generosity 
          + I(Generosity^2), data = data)

# Use summary and anova to see if the squared term is affective.
summary(squared)
anova(squared, reduced)

# Only Logged GDP percapita and Generosity increased the R^2

# Squared Logged GDP per capita and Squared Generosity
squared <- lm(Ladder.score ~ Regional.indicator + Logged.GDP.per.capita + I(Logged.GDP.per.capita^2) 
              + Social.support + Freedom.to.make.life.choices 
              + Generosity + I(Generosity^2), data = data)
summary(squared)

# Test. Did not include in Final Model.
# Squared every quantitative variable
squared <- lm(Ladder.score ~ Regional.indicator + Logged.GDP.per.capita + I(Logged.GDP.per.capita^2) 
              + Social.support + I(Social.support^2) + Freedom.to.make.life.choices + I(Freedom.to.make.life.choices^2)
              + Generosity + I(Generosity^2), data = data)
summary(squared)

# Only including the squared Logged GDP per capita and squared Generosity were effective enough.
# Thus, we only include the quared Logged GDP per capita and squared Generosity in the final model

###################################################################################################
# Test. Did not include in Final Model.
# Reorganize Regional Indicator
###################################################################################################

# Tried to remove some of the levels within the qualitative variable.
# Wasn't effective. 
data$Regional.indicator.organized <- factor(data$Regional.indicator, levels = c('Central and Eastern Europe', 'Commonwealth of Independent States', 'East Asia', 'Latin America and Caribbean', 'Middle East and North Africa', 'North America and ANZ', 'South Asia', 'Southeast Asia', 'Sub-Saharan Africa', 'Western Europe'),
                                            labels = c('World', 'World', 'World', 'World', 'Middle East and North Africa', 'World', 'South Asia', 'Southeast Asia', 'Sub-Saharan Africa', 'Western Europe'))
regionalIndicator <- lm(Ladder.score ~ Regional.indicator.organized + Logged.GDP.per.capita + Social.support + Freedom.to.make.life.choices + Generosity, data = data)
summary(regionalIndicator)
qqnorm(regionalIndicator$residuals)
qqline(regionalIndicator$residuals)

# The plot above shows that social support might have a degree higher than 1?

square <- lm(Ladder.score ~ Regional.indicator.organized + Logged.GDP.per.capita + I(Logged.GDP.per.capita^2) + Social.support + Freedom.to.make.life.choices + Generosity + I(Generosity^2), data = data)
summary(square)
qqnorm(square$residuals)
qqline(square$residuals)

square <- lm(Ladder.score ~ Regional.indicator.organized + Logged.GDP.per.capita + I(Logged.GDP.per.capita^2) + Social.support + I(Social.support^2) + Freedom.to.make.life.choices + I(Freedom.to.make.life.choices^2) + Generosity, data = data)
summary(square)

anova(square, reduced)

###################################################################################################
# Interaction Terms
###################################################################################################

# include all possible interaction terms without second-order terms
interact <- lm(Ladder.score ~ (Regional.indicator + Logged.GDP.per.capita + Social.support + Freedom.to.make.life.choices + Generosity)^2, data = data)
summary(interact)

# Test. Did not include in Final Model.
# include all possible interaction terms without squared social support
interact <- lm(Ladder.score ~ (Regional.indicator + Logged.GDP.per.capita + Social.support + I(Social.support^2) + Freedom.to.make.life.choices + Generosity)^2, data = data)
summary(interact)

# include all possible interaction terms without squared logged GDP per capita and generosity
interact <- lm(Ladder.score ~ (Regional.indicator + Logged.GDP.per.capita + I(Logged.GDP.per.capita^2) + Social.support + Freedom.to.make.life.choices + Generosity + I(Generosity^2))^2, data = data)
summary(interact)

# Test. Did not include in Final Model.
# include all significant interaction terms that has interaction with regional indicator
interactSummary <- lm(Ladder.score ~ Regional.indicator + Logged.GDP.per.capita + Social.support + I(Social.support^2) + Freedom.to.make.life.choices + Generosity + Regional.indicator*Social.support + Regional.indicator.organized*I(Social.support^2) + Logged.GDP.per.capita*Freedom.to.make.life.choices + Social.support*Generosity + I(Social.support^2)*Generosity, data = data)
summary(interactSummary)

# include all significant interaction terms that does not have interactino with regional indicator
interactSummary <- lm(Ladder.score ~ Regional.indicator + Logged.GDP.per.capita + I(Logged.GDP.per.capita^2)  + Social.support + Freedom.to.make.life.choices + Generosity + I(Generosity^2) + Logged.GDP.per.capita*Freedom.to.make.life.choices + Social.support*Freedom.to.make.life.choices + Social.support*Generosity + I(Social.support^2)*Generosity, data = data)
summary(interactSummary)


anova(interact, interactSummary)

plot(data$Social.support, interact$residuals)
 
qqnorm(interact$residuals)
qqline(interact$residuals)

###################################################################################################
# Cross Fold Validation
###################################################################################################

# install paackages need to be run only once
install.packages("DAAG")
library("DAAG")

# Test. Did not include in Final Model.
# Cross fold validation with all variables
CVlm(data = data, form.lm = formula(Ladder.score ~ Regional.indicator + Logged.GDP.per.capita + 
                                            Social.support + Healthy.life.expectancy +
                                            Freedom.to.make.life.choices + Perceptions.of.corruption + Generosity),
     m = 3, dots = FALSE, seed = 29, plotit = c("Observed"),
     #main="Small symbols show cross-validation predicted values",
     #legend.pos="topleft", printit = TRUE
)

### FINAL MODEL
# Cross fold validation with all variables and significant second-order term
CVlm(data = data, form.lm = formula(Ladder.score ~ Regional.indicator + Logged.GDP.per.capita + 
                                            I(Logged.GDP.per.capita^2) + Social.support + 
                                            Freedom.to.make.life.choices + Generosity + I(Generosity^2)),
        m = 3, dots = FALSE, seed = 29, plotit = c("Observed"),
)

# Test. Did not include in Final Model.
# Cross fold validation with all variables and significant second-order term and all possible interaction terms
CVlm(data = data, form.lm = formula(Ladder.score ~ (Regional.indicator + Logged.GDP.per.capita + 
                                            I(Logged.GDP.per.capita^2) + Social.support + 
                                            Freedom.to.make.life.choices + Generosity + I(Generosity^2))^2),
     m = 3, dots = FALSE, seed = 29, plotit = c("Observed"),
     #main="Small symbols show cross-validation predicted values",
     #legend.pos="topleft", printit = TRUE
)

# Test. Did not include in Final Model.
# Cross fold validation with all variables and second-order term and all significant interaction terms with Regional Indicator
CVlm(data = data, form.lm = formula(Ladder.score ~ Regional.indicator + Logged.GDP.per.capita + 
                                            I(Logged.GDP.per.capita^2) + Social.support + 
                                            Freedom.to.make.life.choices + Generosity + I(Generosity^2) + 
                                            Regional.indicator*Logged.GDP.per.capita + Regional.indicator*Social.support + 
                                            Logged.GDP.per.capita*I(Generosity^2) + I(Logged.GDP.per.capita^2)*Social.support + 
                                            I(Logged.GDP.per.capita^2)*Generosity + Social.support*Generosity +
                                            Generosity*I(Generosity^2)),
     m = 3, dots = FALSE, seed = 29, plotit = c("Observed"),
     #main="Small symbols show cross-validation predicted values",
     #legend.pos="topleft", printit = TRUE
)

# Test. Did not include in Final Model.
# Cross fold validation with all variables and second-order term and all significant interaction terms with Regional Indicator
CVlm(data = data, form.lm = formula(Ladder.score ~ Regional.indicator + Logged.GDP.per.capita + 
                                            I(Logged.GDP.per.capita^2) + Social.support + 
                                            Freedom.to.make.life.choices + Generosity + I(Generosity^2) + 
                                            Logged.GDP.per.capita*Freedom.to.make.life.choices + 
                                            Social.support*Freedom.to.make.life.choices + 
                                            Social.support*Generosity + I(Social.support^2)*Generosity),
     m = 3, dots = FALSE, seed = 29, plotit = c("Observed"),
     #main="Small symbols show cross-validation predicted values",
     #legend.pos="topleft", printit = TRUE
)

###################################################################################################
# Final Model and Prediction
###################################################################################################

final <- lm(Ladder.score ~ Regional.indicator + Logged.GDP.per.capita + I(Logged.GDP.per.capita^2) + Social.support + 
                    Freedom.to.make.life.choices + Generosity + I(Generosity^2), data = data)
summary(final)

# Predict the happiness or ladder score of South Africa
new <- data.frame(Regional.indicator = 'Sub-Saharan Africa', Logged.GDP.per.capita = 9.40, Social.support = 0.860, 
                  Freedom.to.make.life.choices = 0.749, Generosity = -0.067)
predict(final, new, interval = "prediction")