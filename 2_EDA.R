######### EDA ############



# Function to create aggregated EDA data
create_EDA_agg <- function(ft = 1,
                           y = 1,
                           weight = 1, 
                           interaction = 1, 
                           ft_nbreaks = 30, 
                           interaction_nbreaks = 30,
                           ft_band_type = "equal",
                           interaction_band_type = "quantile") {
    gc()
    
    # Banding the feature if it is numeric and has more than 5 unique values
    ft <- if (is.numeric(ft) & length(unique(ft)) > 5) {
      KT_band_data(x = ft, nbreaks = ft_nbreaks, method = ft_band_type)
    } else {
      ft
    }
    
    # Banding the interaction if it is numeric and has more than 4 unique values
    interaction <- if (is.numeric(interaction) & length(unique(interaction)) > 4) {
      KT_band_data(x = interaction, nbreaks = interaction_nbreaks, method = interaction_band_type)
    } else {
      interaction
    }
    
    # Calculate total weight
    tot_weight <- sum(weight)
    
    # Create aggregated data table
    agg_data <- data.table(ft = ft, interaction = interaction, y = y, weight = weight) %>%
      group_by(ft, interaction) %>%
      summarise(y = sum(y) / sum(weight), weight = sum(weight) / tot_weight) %>%
      ungroup()
    
    # Garbage collection to free up memory
    gc()
    
    return(agg_data)
}

# Function to plot EDA data
EDA_plot <- function(agg_df,
                     bar_alpha = 0.5,
                     lwd = 1,
                     point_size = 2.5,
                     line_alpha = 1,
                     point_alpha = 1,
                     ft_name = "x",
                     interaction_name = "y",
                     smooth_strength = 0.7) {

    gc()
    
    # Create the plot
    suppressMessages(
      p <- ggplot(agg_df, aes(x = ft, group = interaction, fill = interaction, weight = weight)) +
        theme_light(base_size = 15) +
        geom_bar(aes(y = weight / mean(agg_df$weight) * (sum(agg_df$y * agg_df$weight) / 10)), stat = "identity", alpha = bar_alpha) +
        geom_line(aes(y = y, color = interaction), lwd = lwd, alpha = line_alpha) +
        geom_point(aes(y = y, color = interaction), size = point_size, alpha = point_alpha, shape = 4) +
        scale_y_continuous(name = "", sec.axis = sec_axis(~ . * mean(agg_df$weight), name = "weight")) +
        theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 0.9)) +
        labs(fill = interaction_name, color = interaction_name, x = ft_name)
    )
    
    # Add smooth line if smooth_strength is greater than 0
    if (smooth_strength > 0) {
      p <- p + geom_smooth(aes(y = y, color = interaction), method = "loess", span = smooth_strength, se = FALSE)
    }

    gc()
    
    return(p)

}

# Custom rounding function
custom_round <- function(x, digits = 2) {
 
    sapply(x, function(val) {
      if (abs(val) < 1) {
        return(signif(val, digits))
      } else {
        return(round(val, digits))
      }
    })

}
