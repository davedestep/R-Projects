mutate(race = factor(race, 
                       levels = 1:4,
                       labels = c('White',
                                  'Black',
                                  'Hispanic',
                                  'Other')), 
         income_cat = case_when(
           income < 31914 ~ 1,
           income >= 31914 & income < 38729 ~ 2,
           income >= 38730 & income < 48029 ~ 3,
           income >= 48029 ~ 4))
