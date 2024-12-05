model <- lm(Quarterly_Average ~ Detached_Absorption_Quarterly_Avg + 
              Detached_Unabsorbed_Quarterly_Avg + 
              Starting_Detached_Construction + 
              Completed_Construction_Detached + 
              rates, 
            data = cost_data_quarterly)
model

