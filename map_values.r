data.processed <- mutate(data.raw,
                         Question = mapvalues(measurement_name,
                            c("I know that I have enough money in savings, retirement, or assets to cover the costs of my treatment.",
                              "I worry about the financial problems I will have in the future as a result of my illness or treatment."),
                            c('Confident','Worry')),
                         Response = mapvalues(meas_value,
                            c("1 - Strongly Disagree", "2 - Disagree", "3 - Neutral",
                              "4 - Agree", "5 - Strongly Agree"),
                            c("Not at all", "A little bit", "Somewhat",
                              "Quite a bit", "Very much")),
