#' Train a Forecasting Model with Regression Models
#' @export
#' @param input A tsibble or ts object
#' @param y A character, the column name of the depended variable of the input object, required (and applicable) only when the input is tsibble object
#' @param x A character, the column names of the independed variable of the input object, applicable when using tsibble object with regressors
#' @param seasonal A character, optional, create categorical variable/s to model the single or multiple seasonal components. Supporting the
#' following frequencies structure c("quarter", "month", "week", "yday", "wday", "hour") for quarterly,
#' monthly, weekly, day of the year, day of the week, and hourly respectively
#' @param trend A list, define the trend structure. Possible arguments -
#' "linear", a boolean variable, if set to TRUE defines a linear trend (e.g., index of 1,2,3,...,t, for a series with t observations)
#' "power", an numeric value, defines the polynomial degree of the series index (for example a power = 0.5 define a square root index and power = 2 defines a squared index). By default set to NULL
#' "exponential" - a boolean variable, if set to TRUE defines an exponential trend.
#' "log" - a boolean variable, if set to TRUE defines a log transformation for the trend.
#' By default, the trend argument is set to a linear trend (i.e., power = 1)
#' @param lags A positive integer, defines the series lags to be used as input to the model (equivalent to AR process)
#' @param events A list, an optional, create hot encoding variables based on date/time objects,
#' where the date/time objects must align with the input object index class (may not work when the input object is 'ts'). For more information please see details
#' @param method A character, defines the regression method to be used, currently only "lm" method is available
#' @param method_arg A list, defines the argument of the selected method
#' @param scale A character, scaling options of the series, methods available -
#' c("log", "normal", "standard") for log transformation, normalization, or standardization of the series, respectively.
#' If set to NULL (default), no transformation will occur
#' @description Train a forecasting model with regression models
#' @details XXXXXXXXXXX TBD XXXXXXXXXXXXXXXXXXX
#' @examples



trainML <- function(input,
                    y = NULL,
                    x = NULL,
                    seasonal = NULL,
                    trend = list(linear = TRUE, exponential = FALSE, log = FALSE, power = FALSE),
                    lags = NULL,
                    events = NULL,
                    method =  "lm",
                    method_arg = list(step = FALSE, direction = "both"),
                    scale = NULL){

  `%>%` <- magrittr::`%>%`

  freq <- md <- time_stamp <- new_features <- residuals <- NULL

  # Error handling

  # Check the trend argument

  if(!base::is.list(trend) || !all(base::names(trend) %in% c("linear", "exponential", "log", "power"))){
    stop("The 'trend' argument is not valid")
  } else{

    if(!"linear" %in% base::names(trend)){
      trend$linear <- FALSE
    } else if(!base::is.logical(trend$linear)){
      stop("The 'linear' argument of the trend must be either TRUE or FALSE")
    }

    if(!"exponential" %in% base::names(trend)){
      trend$exponential <- FALSE
    } else if(!base::is.logical(trend$exponential)){
      stop("The 'exponential' argument of the trend must be either TRUE or FALSE")
    }

    if(!"log" %in% base::names(trend)){
      trend$log <- FALSE
    } else if(!base::is.logical(trend$log)){
      stop("The 'log' argument of the trend must be either TRUE or FALSE")
    }

    if(!"power" %in%  base::names(trend)){
      trend$power <- FALSE
    } else if(!base::is.null(trend$power) && !base::is.numeric(trend$power) && trend$power != FALSE){
      stop("The value of the 'power' argument is not valid, can be either a numeric ",
           "(e.g., 2 for square, 0.5 for square root, etc.), or FALSE for disable")
    }

    if(trend$linear && !base::is.null(trend$power) && trend$power == 1){
      warning("Setting both the 'power' argument to 1 and the 'linear' argument to TRUE is equivalent. ",
              "To avoid redundancy in the variables, setting 'linear' to FALSE")
    }
  }


  # Check if the x variables are in the input obj
  if(!base::is.null(x)){
    if(!base::all(x %in% names(input))){
      stop("Some or all of the variables names in the 'x' argument do not align with the column names of the input object")
    }
  }


  # Checking the lags argument
  if(!base::is.null(lags)){
    if(!base::is.numeric(lags) || base::any(lags %% 1 != 0 ) || base::any(lags <= 0)){
      stop("The value of the 'ar' argument is not valid. Must be a positive integer")
    }
  }

  # Checking the scale argument
  if(!is.null(scale)){
    if(base::length(scale) > 1 || !base::any(c("log", "normal", "standard") %in% scale)){
      stop("The value of the 'scale' argument are not valid")
    }
  }

  # Setting the input table
  if(base::any(base::class(input) == "tbl_ts")){
    df <- input
  } else if(any(class(input) == "ts")){
    y <- base::deparse(base::substitute(input))
    df <- tsibble::as_tsibble(input) %>% setNames(c("index", y))
    if(!base::is.null(x)){
      warning("The 'x' argument cannot be used when input is a 'ts' class")
    }
    x <- NULL
  }

  time_stamp <- base::attributes(df)$index2

  freq <- base::list(unit = base::names(base::which(purrr::map(tsibble::interval(df), ~.x) > 0)),
                     value = tsibble::interval(df)[which(tsibble::interval(df) != 0)] %>% base::as.numeric(),
                     frequency = stats::frequency(df),
                     class = base::class(df[,time_stamp, drop = TRUE]))

  # Checking the event argument
  if(!base::is.null(events) && !base::is.list(events)){
    stop("The 'events' argument is not valid, please use list")
  } else if(!base::is.null(events) && base::is.list(events)){
    for(n in base::names(events)){
      if(!base::any(freq$class %in% base::class(events[[n]]))){
        stop("The date/time object of the 'events' argument does not align with the ones of the input object")
      } else {
        df[n] <- 0
        df[base::which(df[,time_stamp , drop = TRUE] %in% events[[n]]), n] <- 1
        new_features <- c(new_features, n)
      }
    }
  }
  # Scaling the series
  if(!base::is.null(scale)){
    y_temp <- NULL
    if(scale == "log"){
      df[[base::paste(y,"log", sep = "_")]] <- base::log(df[[y]])
      y_temp <- y
      y <- base::paste(y,"log", sep = "_")
    } else if(scale == "normal"){
      df$y_normal <- (df$y - base::min(df$y)) / (base::max(df$y) - base::min(df$y))
      y_temp <- y
      y <- "y_normal"
    } else if(scale == "standard"){
      df$y_standard <- (df$y - base::mean(df$y)) / stats::sd(df$y)
      y_temp <- y
      y <- "y_standard"
    }
  }



  # Setting the frequency component
  if(!base::is.null(seasonal)){

    # Case series frequency is quarterly

    if(freq$unit == "quarter"){
      if(base::length(seasonal) == 1 & seasonal == "quarter"){
        df$quarter <- lubridate::quarter(df[[time_stamp]]) %>% base::factor(ordered = FALSE)
        new_features <- c(new_features, "quarter")
      } else if(base::length(seasonal) > 1 & "quarter" %in% seasonal){
        warning("Only quarter seasonal component can be used with quarterly frequency")
        df$quarter <- lubridate::quarter(df[[time_stamp]]) %>% base::factor(ordered = FALSE)
        new_features <- c(new_features, "quarter")
      } else {
        stop("The seasonal component is not valid")
      }

      # Case series frequency is monthly

    } else if(freq$unit == "month"){
      if(base::length(seasonal) == 1 && seasonal == "month"){
        df$month <- lubridate::month(df[[time_stamp]], label = TRUE) %>% base::factor(ordered = FALSE)
        new_features <- c(new_features, "month")
      } else if(all(seasonal %in% c("month", "quarter"))){
        df$quarter <- lubridate::quarter(df[[time_stamp]]) %>% base::factor(ordered = FALSE)
        df$month <- lubridate::month(df[[time_stamp]], label = TRUE) %>% base::factor(ordered = FALSE)
        new_features <- c(new_features, "month", "quarter")
      } else if(any(seasonal %in% c("month", "quarter"))){
        if("month" %in% seasonal){
          df$month <- lubridate::month(df[[time_stamp]], label = TRUE) %>% base::factor(ordered = FALSE)
          new_features <- c(new_features, "month")
        }

        if("quarter" %in% seasonal){
          df$quarter <- lubridate::quarter(df[[time_stamp]]) %>% base::factor(ordered = FALSE)
          new_features <- c(new_features, "quarter")
        }

      } else {stop("The seasonal component is not valid")}

      # Case series frequency is weekly

    } else if(freq$unit == "week"){
      if(base::length(seasonal) == 1 && seasonal == "week"){
        df$week <- lubridate::week(df[[time_stamp]]) %>% base::factor(ordered = FALSE)
        new_features <- c(new_features, "week")
      } else if(all(c("week", "month", "quarter") %in% seasonal)){
        df$quarter <- lubridate::quarter(df[[time_stamp]]) %>% base::factor(ordered = FALSE)
        df$month <- lubridate::month(df[[time_stamp]], label = TRUE) %>% base::factor(ordered = FALSE)
        df$week <- lubridate::week(df[[time_stamp]]) %>% base::factor(ordered = FALSE)
        new_features <- c(new_features, "week","month", "quarter")
      } else if(any(c("week", "month", "quarter") %in% seasonal)){
        if("week" %in% seasonal){
          df$week <- lubridate::week(df[[time_stamp]]) %>% base::factor(ordered = FALSE)
          new_features <- c(new_features, "week")
        }

        if("month" %in% seasonal){
          df$month <- lubridate::month(df[[time_stamp]], label = TRUE) %>% base::factor(ordered = FALSE)
          new_features <- c(new_features, "month")
        }

        if("quarter" %in% seasonal){
          df$quarter <- lubridate::quarter(df[[time_stamp]]) %>% base::factor(ordered = FALSE)
          new_features <- c(new_features, "quarter")
        }
      } else {stop("The seasonal component is not valid")}

      # Case series frequency is daily

    } else if(freq$unit == "day"){
      if(base::length(seasonal) == 1 && seasonal == "wday"){
        df$wday <- lubridate::wday(df[[time_stamp]], label = TRUE) %>% base::factor(ordered = FALSE)
        new_features <- c(new_features, "wday")
      } else if(base::length(seasonal) == 1 && seasonal == "yday"){
        df$yday <- lubridate::yday(df[[time_stamp]]) %>% base::factor(ordered = FALSE)
        new_features <- c(new_features, "yday")
      } else if(all(c("wday", "yday","week", "month", "quarter") %in% seasonal)){
        df$quarter <- lubridate::quarter(df[[time_stamp]]) %>% base::factor(ordered = FALSE)
        df$month <- lubridate::month(df[[time_stamp]], label = TRUE) %>% base::factor(ordered = FALSE)
        df$week <- lubridate::week(df[[time_stamp]]) %>% base::factor(ordered = FALSE)
        df$wday <- lubridate::wday(df[[time_stamp]], label = TRUE) %>% base::factor(ordered = FALSE)
        df$yday <- lubridate::yday(df[[time_stamp]]) %>% base::factor(ordered = FALSE)
        new_features <- c(new_features, "wday", "yday", "week","month", "quarter")
      } else if(any(c("wday", "yday","week", "month", "quarter") %in% seasonal)){
        if("wday" %in% seasonal){
          df$wday <- lubridate::wday(df[[time_stamp]], label = TRUE) %>% base::factor(ordered = FALSE)
          new_features <- c(new_features, "wday")
        }

        if("yday" %in% seasonal){
          df$yday <- lubridate::yday(df[[time_stamp]]) %>% base::factor(ordered = FALSE)
          new_features <- c(new_features, "yday")
        }

        if("week" %in% seasonal){
          df$week <- lubridate::week(df[[time_stamp]]) %>% base::factor(ordered = FALSE)
          new_features <- c(new_features, "week")
        }


        if("month" %in% seasonal){
          df$month <- lubridate::month(df[[time_stamp]], label = TRUE) %>% base::factor(ordered = FALSE)
          new_features <- c(new_features, "month")
        }

        if("quarter" %in% seasonal){
          df$quarter <- lubridate::quarter(df[[time_stamp]]) %>% base::factor(ordered = FALSE)
          new_features <- c(new_features, "quarter")
        }

      } else {stop("The seasonal component is not valid")}

      # Case series frequency is hourly

    } else if(freq$unit == "hour"){
      if(base::length(seasonal) == 1 && seasonal == "hour"){
        df$hour <- lubridate::hour(df[[time_stamp]]) %>% base::factor(ordered = FALSE)
        new_features <- c(new_features, "hour")
      } else if(all(c("hour", "wday", "yday","week", "month", "quarter") %in% seasonal)){
        df$quarter <- lubridate::quarter(df[[time_stamp]]) %>% base::factor(ordered = FALSE)
        df$month <- lubridate::month(df[[time_stamp]], label = TRUE) %>% base::factor(ordered = FALSE)
        df$week <- lubridate::week(df[[time_stamp]]) %>% base::factor(ordered = FALSE)
        df$wday <- lubridate::wday(df[[time_stamp]], label = TRUE) %>% base::factor(ordered = FALSE)
        df$yday <- lubridate::yday(df[[time_stamp]]) %>% base::factor(ordered = FALSE)
        df$hour <- (lubridate::hour(df[[time_stamp]]) + 1) %>% base::factor(ordered = FALSE)
        new_features <- c(new_features, "hour","wday", "yday", "week","month", "quarter")
      } else if(any(c("hour","wday", "yday","week", "month", "quarter") %in% seasonal)){
        if("hour" %in% seasonal){
          df$hour <- (lubridate::hour(df[[time_stamp]]) + 1) %>% base::factor(ordered = FALSE)
          new_features <- c(new_features, "hour")
        }

        if("wday" %in% seasonal){
          df$wday <- lubridate::wday(df[[time_stamp]], label = TRUE) %>% base::factor(ordered = FALSE)
          new_features <- c(new_features, "wday")
        }

        if("yday" %in% seasonal){
          df$yday <- lubridate::yday(df[[time_stamp]]) %>% base::factor(ordered = FALSE)
          new_features <- c(new_features, "yday")
        }

        if("week" %in% seasonal){
          df$week <- lubridate::week(df[[time_stamp]]) %>% base::factor(ordered = FALSE)
          new_features <- c(new_features, "week")
        }

        if("month" %in% seasonal){
          df$month <- lubridate::month(df[[time_stamp]], label = TRUE) %>% base::factor(ordered = FALSE)
          new_features <- c(new_features, "month")
        }

        if("quarter" %in% seasonal){
          df$quarter <- lubridate::quarter(df[[time_stamp]]) %>% base::factor(ordered = FALSE)
          new_features <- c(new_features, "quarter")
        }

      } else {stop("The seasonal component is not valid")}
    } else if(freq$unit == "minute"){
      if(base::length(seasonal) == 1 && seasonal == "minute"){
        df$minute <- (lubridate::hour(df[[time_stamp]]) * 2 + (lubridate::minute(df[[time_stamp]]) + freq$value )/ freq$value )%>%
          factor(ordered = FALSE)
        new_features <- c(new_features, "minute")
      } else if(all(c("minute", "hour", "wday", "yday","week", "month", "quarter") %in% seasonal)){
        df$quarter <- lubridate::quarter(df[[time_stamp]]) %>% base::factor(ordered = FALSE)
        df$month <- lubridate::month(df[[time_stamp]], label = TRUE) %>% base::factor(ordered = FALSE)
        df$week <- lubridate::week(df[[time_stamp]]) %>% base::factor(ordered = FALSE)
        df$wday <- lubridate::wday(df[[time_stamp]], label = TRUE) %>% base::factor(ordered = FALSE)
        df$yday <- lubridate::yday(df[[time_stamp]]) %>% base::factor(ordered = FALSE)
        df$hour <- (lubridate::hour(df[[time_stamp]]) + 1) %>% base::factor(ordered = FALSE)
        df$minute <- (lubridate::hour(df[[time_stamp]]) * 2 + (lubridate::minute(df[[time_stamp]]) + freq$value )/ freq$value) %>%
          base::factor(ordered = FALSE)
        new_features <- c(new_features, "minute", "hour","wday", "yday", "week","month", "quarter")
      } else if(any(c("minute", "hour","wday", "yday","week", "month", "quarter") %in% seasonal)){
        if("minute" %in% seasonal){
          df$minute <- (lubridate::hour(df[[time_stamp]]) * 2 + (lubridate::minute(df[[time_stamp]]) + freq$value )/ freq$value) %>%
            base::factor(ordered = FALSE)
          new_features <- c(new_features, "minute")
        }

        if("hour" %in% seasonal){
          df$hour <- (lubridate::hour(df[[time_stamp]]) + 1) %>% base::factor(ordered = FALSE)
          new_features <- c(new_features, "hour")
        }

        if("wday" %in% seasonal){
          df$wday <- lubridate::wday(df[[time_stamp]], label = TRUE) %>% base::factor(ordered = FALSE)
          new_features <- c(new_features, "wday")
        }

        if("yday" %in% seasonal){
          df$yday <- lubridate::yday(df[[time_stamp]]) %>% base::factor(ordered = FALSE)
          new_features <- c(new_features, "yday")
        }

        if("week" %in% seasonal){
          df$week <- lubridate::week(df[[time_stamp]]) %>% base::factor(ordered = FALSE)
          new_features <- c(new_features, "week")
        }

        if("month" %in% seasonal){
          df$month <- lubridate::month(df[[time_stamp]], label = TRUE) %>% base::factor(ordered = FALSE)
          new_features <- c(new_features, "month")
        }

        if("quarter" %in% seasonal){
          df$quarter <- lubridate::quarter(df[[time_stamp]]) %>% base::factor(ordered = FALSE)
          new_features <- c(new_features, "quarter")
        }

      } else {stop("The seasonal component is not valid")}
    }
  }


  # Setting the trend
  if(base::is.numeric(trend$power)){
    for(i in trend$power){
      df[[base::paste("trend_power_", i, sep = "")]] <- c(1:base::nrow(df)) ^ i
      new_features <- c(new_features, base::paste("trend_power_", i, sep = ""))
    }
  }

  if(trend$exponential){
    df$exp_trend <- base::exp(1:base::nrow(df))
    new_features <- c(new_features, "exp_trend")
  }

  if(trend$log){
    df$log_trend <- base::log(1:base::nrow(df))
    new_features <- c(new_features, "log_trend")
  }

  if(trend$linear){
    df$linear_trend <- 1:base::nrow(df)
    new_features <- c(new_features, "linear_trend")
  }



  # Setting the lags variables

  if(!base::is.null(lags) && base::is.null(scale)){
    for(i in lags){
      df[base::paste("lag_", i, sep = "")] <- df[[y]] %>% dplyr::lag( i)
      new_features <- c(new_features, base::paste("lag_", i, sep = ""))
    }
    df1 <- df[(max(lags)+ 1):base::nrow(df),]
  } else if(!base::is.null(lags) && !base::is.null(scale)){
    for(i in lags){
      df[base::paste("lag_scale", i, sep = "")] <- df[[y]] %>% dplyr::lag(i)
      new_features <- c(new_features, base::paste("lag_scale", i, sep = ""))
    }
    df1 <- df[(max(lags)+ 1):base::nrow(df),]
  } else {
    df1 <- df
  }


  if(method == "lm"){
    if(!base::is.null(x)){
      f <- stats::as.formula(paste(y, "~ ", paste0(x, collapse = " + "), "+", paste0(new_features, collapse = " + ")))
    } else{
      f <- stats::as.formula(paste(y, "~ ", paste0(new_features, collapse = " + ")))
    }

    if(!base::is.null(scale)){
      y <- y_temp
    }


    if(method_arg$step){
      md_init <- NULL
      md_init <- stats::lm(f, data = df1)
      md <- step(md_init, direction = method_arg$direction)
    } else(
      md <- stats::lm(f, data = df1)
    )

  }
  if(base::is.null(scale)){
    fitted <- base::data.frame(index = df1[[base::attributes(df1)$index2]],
                               fitted = stats::predict(md, newdata = df1))

    residuals <- base::data.frame(index = df1[[base::attributes(df1)$index2]],
                                  residuals =  df1[[y]] -  fitted$fitted) %>%
      tsibble::as_tsibble(index = "index")
  } else if(!base::is.null(scale) && scale == "log"){
    fitted <- base::data.frame(index = df1[[base::attributes(df1)$index2]],
                               fitted = base::exp(stats::predict(md, newdata = df1)))

    residuals <- base::data.frame(index = df1[[base::attributes(df1)$index2]],
                                  residuals =  df1[[y]] -  fitted$fitted) %>%
      tsibble::as_tsibble(index = "index")
  }





  output <- list(model = md,
                 fitted = fitted,
                 residuals = residuals,
                 parameters = list(y = y,
                                   x = x,
                                   index = time_stamp,
                                   new_features = new_features,
                                   seasonal = seasonal,
                                   trend = trend,
                                   lags = lags,
                                   events = events,
                                   method = method,
                                   method_arg = method_arg,
                                   scale = scale,
                                   frequency = freq),
                 series = df)

  final_output <- base::structure(output, class = "trainML")

  return(final_output)

}
