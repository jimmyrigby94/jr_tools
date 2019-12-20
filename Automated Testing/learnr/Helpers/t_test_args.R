# Generates fully crossed args given case specific variable, groups, and parameter scopes
t_test_args<-function(var, s1_name, s2_name, s1_m_l, s1_m_u, s2_m_l, s2_m_u, s1_sd_l, s1_sd_u, s2_sd_l, s2_sd_u){
  args<-expand_grid(chapter = "sampling_dist",
                    topic = "hypothesis_testing",
                    subtopic = "t_test",
                    sample1 = s1_name,
                    sample2 = s2_name,
                    equal_var = c("Assume", "Don't assume"),
                    directional = c("less than", "different", "greater than"),
                    var = var,
                    xbar1 = paste0("round(runif(1,", s1_m_l, ", ", s1_m_u, "),2)"), 
                    xbar2 = paste0("round(runif(1,", s2_m_l, ", ", s2_m_u, "),2)"),
                    sd1 = paste0("round(runif(1,", s1_sd_l, ", ", s1_sd_u, "),2)"),
                    sd2 = paste0("round(runif(1,", s2_sd_l, ", ", s2_sd_u, "),2)"),
                    n1 = c("round(runif(1, 12, 100))"),
                    n2 = c("round(runif(1, 12, 100))"),
                    ask = c("probability of observing the mean differences given your null",
                            "t-value for the two samples",
                            "the 95% confidence interval of mean difference"))
}
