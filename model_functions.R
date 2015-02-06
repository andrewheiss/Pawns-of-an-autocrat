library(dplyr)
library(ggplot2)

# Stargazer doesn't support clmm models, so this modifies a clmm object 
# directly so it works with stargazer. Probably horribly evil and bad 
# to do, but it works.
fake.clm <- function(x) {
  model.fake <- x
  model.fake$nobs <- x$dims$nobs
  model.fake$call[1] <- call("clm")
  model.fake
}

# Clean up the interval labels created by cut()
clean.levels <- function(x) {
  separated <- unlist(strsplit(x, ","))
  
  clean.numbers <- function(y) {
    if(grepl("\\(", y)) {
      cleaned <- gsub("\\(", "", y)
      return(as.numeric(cleaned) + 1)
    } else {
      cleaned <- gsub("\\[|\\]", "", y)
      return(as.numeric(cleaned))
    }
  }
  
  character.label <- paste(sapply(separated, FUN=clean.numbers), collapse="-")
  return(gsub("-Inf", "+", character.label))
}


# predict.clmm doesn't exist yet, so this replicates it
# Arguments:
#  - model = a clmm model
#  - newdata = a dataframe of new data to apply the model to
# Returns a dataframe of predicted probabilities for each row and response level
fake.predict.clmm <- function(model, newdata) {
  # Actual prediction function
  pred <- function(eta, theta, cat = 1:(length(theta) + 1), inv.link = plogis) {
    Theta <- c(-1000, theta, 1000)
    sapply(cat, function(j) inv.link(Theta[j + 1] - eta) - inv.link(Theta[j] - eta))
  }
  
  # Multiply each row by the coefficients
  coefs <- c(model$beta, unlist(model$ST))
  xbetas <- sweep(newdata, MARGIN=2, coefs, `*`)
  
  # Make predictions
  pred.mat <- data.frame(pred(eta=rowSums(xbetas), theta=model$Theta))
  colnames(pred.mat) <- levels(model$model[,1])
  pred.mat
}


# Draw random coefficients instead of mucking around with SEs and confints
ologit.predict <- function(i, coefs, newdata, mod) {
  pred <- function(eta, theta, cat = 1:(length(theta) + 1), inv.link = plogis) {
    Theta <- c(-1000, theta, 1000)
    sapply(cat, function(j) inv.link(Theta[j + 1] - eta) - inv.link(Theta[j] - eta))
  }
  # Pass the row number instead of the actual row so it can be 
  # included in final data frame
  x <- coefs[i,]
  n.thetas <- length(mod$Theta)
  theta.draw <- x[1:n.thetas]
  coefs.draw <- x[(n.thetas+1):length(x)]
  
  xbetas1 <- sweep(newdata, MARGIN=2, coefs.draw, `*`)
  pred.mat1 <- data.frame(pred(eta=rowSums(xbetas1), theta=theta.draw)) %>%
    set_colnames(levels(pawns.data$assn)) %>%
    mutate(sim.round = i)
}


# Extract coefficients and calculate 95% confidence interval for each
extract.coef.plot <- function(x, models, names) {
  multiplier <- qnorm(1 - 0.05 / 2)
  current.model <- models[[x]]
  
  coefs <- current.model$coefficients
  
  if(current.model$call[1] == "clm()") {
    errs <- sqrt(diag(vcov(current.model)))
  } else {
    n.coef <- length(current.model$coefficients)
    errs <- sqrt(diag(vcov(current.model)[1:n.coef, 1:n.coef]))
  }
  
  data_frame(estimate = current.model$coefficients, 
             stderr = errs) %>%
    mutate(IV = row.names(.)) %>% tail(-2) %>%
    mutate(ymin = estimate - (multiplier * stderr),
           ymax = estimate + (multiplier * stderr),
           model.name = names[x])
}


# Create a separation plot for an ordered logistic regression model using ggplot
# Arguments:
#  - pred.mat = a dataframe of predicted probabilities for each response level
#  - actual.char = a character list of the actual response outcomes
#  - actual.levels = the levels of the ordered factor (since the function 
#    requires de-leveled responses)
# Returns a ggplot plot
sep.plot <- function(pred.mat, actual.char, actual.levels) {
  # Returns a dataframe of predicted probabilities with actual = 1 
  # when the response outcome happens
  match.actual <- function(x, pred, actual) {
    data_frame(fitted=pred[,x],
               actual=as.numeric(actual==colnames(pred[x])),
               plot.level=colnames(pred[x]))
  }
  
  # Match each column in pred.mat (predicted probabilities for each response level)
  pred.long.list <- lapply(1:ncol(pred.mat), FUN=match.actual, pred=pred.mat, 
                           actual=actual.char)
  
  # Convert list of dataframes to long and clean up resultant dataframe, 
  # adding a sequential index for plotting
  plot.all <- rbind_all(pred.long.list) %>%
    arrange(plot.level, fitted) %>%
    mutate(plot.level = factor(plot.level, levels=actual.levels, ordered=TRUE)) %>%
    group_by(plot.level) %>%
    mutate(x = seq_along(actual))
  
  # Only select the rows where `actual` changes
  diffs.condensed <- plot.all %>%
    group_by(plot.level) %>%
    mutate(change = c(0, diff(actual))) %>% 
    subset(change != 0 ) %>% select(-change)
  
  # Extract the first row of each plot level since those get ignored in diffs.condensed
  first.rows <- plot.all %>% group_by(plot.level) %>% slice(1) %>% mutate(x = 0)
  
  # Get various extra data: the number of observations in each group + 
  # the expected number of events (the sum of probabilities) + 
  # the marker location (1 - epxected)
  metadata <- plot.all %>% group_by(plot.level) %>%
    summarize(obs = n(), expected = sum(fitted),
              marker = obs - expected + 1)
  
  # Get a single number for the number of observations in each response level
  group.size <- (metadata %>% slice(1))$obs
  
  # Add first rows back in, calculate ranges for geom_rect
  condensed.plot.data <- bind_rows(diffs.condensed, first.rows) %>%
    arrange(plot.level, fitted) %>%
    group_by(plot.level) %>%
    mutate(xmax = lead(x),
           xmax = ifelse(is.na(xmax), group.size + 1, xmax)) %>%
    rename(xmin = x)
  
  # Plot everything finally
  ggplot(condensed.plot.data) + 
    geom_rect(aes(xmin=xmin, xmax=xmax, ymin=0, ymax=1, fill=factor(actual))) + 
    geom_line(data=plot.all, aes(y=fitted, x = x), lwd = 0.8) + 
    geom_point(data=metadata, aes(x=marker, y=-0.05), shape=17, size=3) + 
    scale_fill_manual(values = c("grey90", "#00A0B0"), guide="none") +
    theme_bw() + 
    theme(line = element_blank(), axis.text = element_blank(), 
          axis.ticks = element_blank(), axis.title = element_blank(),
          panel.border = element_blank(),
          strip.text=element_text(size=rel(1), family="Source Sans Pro Semibold"),
          strip.background=element_rect(fill=NA, colour=NA)) + 
    facet_wrap(~ plot.level, ncol=1)
}


theme_clean <- function(base_size=12, base_family="Source Sans Pro Light", 
                        legend.bottom=FALSE) {
  ret <- theme_bw(base_size, base_family) + 
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          axis.title.x=element_text(vjust=-0.2), axis.title.y=element_text(vjust=1.5),
          title=element_text(vjust=1.2, family="Source Sans Pro Semibold"),
          panel.border = element_blank(), axis.line=element_blank(),
          panel.grid.minor=element_blank(), 
          axis.ticks=element_blank(), 
          axis.title=element_text(size=rel(0.8), family="Source Sans Pro Semibold"),
          strip.text=element_text(size=rel(1), family="Source Sans Pro Semibold"),
          strip.background=element_rect(fill="#ffffff", colour=NA),
          panel.margin.y=unit(1.5, "lines"))
  
  if (legend.bottom) {
    ret <- ret + theme(legend.position="bottom", legend.margin=unit(0, "lines"),
                       plot.margin=unit(c(1, 1, -0.5, 0.5), "lines"))
  }
  
  ret
}
