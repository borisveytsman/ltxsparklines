# Process data and output the dataframe with x, y and
# optional color data
.process_input <- function(x=NULL, y=NULL, color=NULL, name=NA) {
    if (is.null(x) && is.null(y)) {
        return(NULL)
    }
    if (!is.null(x) && !is.null(y)) { # Two objects
        x <- as.numeric(x)
        y <- as.numeric(y)
        if (length(x) != length(y)) {
            warning(paste0("Vectors for ", name,
                           " have different lengths.",
                           " Skipping these data."),
                    call. = FALSE)
            return (NULL)
        }
        df <- data.frame(x=x,y=y)
    } else { # The case of one object
        if(is.null(x)) {
            x <- y
        }
        if (is.matrix(x)) {
            if (dim(x)[2]<2) {
                warning(paste0("Matrix for ", name,
                               "should have at least two columns. ",
                               "Skipping these data."))
                return(NULL)
            }
            df <- data.frame(x=x[,1], y=x[,2])
        } else if (is.data.frame(x))  {
            if (ncol(x)<2) {
                warning(paste0("Data frame for ", name,
                               "should have at least two columns. ",
                               "Skipping these data."))
                return(NULL)
            }
            df <- data.frame(x=x[,1], y=x[,2])
            if (ncol(x)>2 && is.null(color)) {
                df$color <- x[,3]
            }
        } else if (is.ts(x)) {
            df <- data.frame(x=as.numeric(time(x)),
                             y=as.numeric(x))
        } else  if (requireNamespace('zoo', quietly=TRUE) &&
                    zoo::is.zoo(x)) {
            df <- data.frame(x=as.numeric(time(x)),
                             y=as.numeric(x))            
        } else { # If everything else fails, treat x as a vector
            x <- as.numeric(x)
            df <- data.frame(x=1:length(x),
                             y=as.numeric(x))                        
        }
    }

    # Adding color
    if (!is.null(color)) {
        df$color <- rep(color,nrow(df))[1:nrow(df)]
    }
    return(df)
}

# Normalize a value
.range_normalize <- function(x,range) {
    (x-range[1])/(range[2]-range[1])
}

# Plotting routines
.plot_line <- function (df) {
    df <- df[complete.cases(df),]
    if (nrow(df) <2) {
        return('')
    }
    paste0("\\spark ",
           paste0(as.numeric(t(as.matrix(df))),
                  collapse = ' '),
           " /\n")
}

.plot_dots <- function (df) {
    if (ncol(df) < 3) {
        df[,3] <- rep(getOption('ltxsparklines.dotcolor'),
                      nrow(df))
    }
    df <- df[complete.cases(df),]
    if (!nrow(df) || ncol(df)<3) {
        return ("")
    }
    dots <- sapply(1:nrow(df), function(i) {
        paste("\\sparkdot", df[i,1], df[i,2], df[i,3], "\n",
              sep=" ")})
    paste0(dots, collapse="")
}

.plot_spikes <- function (df) {
    df <- df[complete.cases(df),]
    if (!nrow(df)) {
        return ("")
    }
    spikes <- sapply(1:nrow(df), function(i) {
        paste("\\sparkspike", df[i,1], df[i,2], "\n",
              sep=" ")})
    paste0(spikes, collapse="")
}


# Setting up the defaults;  following Hadley Wickham
.onLoad <- function (libname, pkgname) {
    opts <- list(
        ltxsparklines.width = 10,
        ltxsparklines.clip = FALSE,
        ltxsparklines.na.rm = TRUE,
        ltxsparklines.bottomline = FALSE,
        ltxsparklines.bottomlinex = c(NA, NA),
        ltxsparklines.startdotcolor = NA,
        ltxsparklines.enddotcolor = NA,
        ltxsparklines.dotcolor='blue',
        ltxsparklines.output='knitr')
    new.opts <-  !(names (opts) %in% names(options()))
    if (any(new.opts)) {
        options(opts[new.opts])
    }
    invisible()
}

    
# The main function
sparkline <- function(x=NULL, y=NULL,
                      xspikes=NULL, yspikes=NULL,
                      xdots=NULL, ydots=NULL,dotcolor=NULL,
                      width=getOption('ltxsparklines.width'),
                      rectangle=c(NA,NA),
                      xlim=c(NA,NA),
                      ylim=c(NA,NA),
                      clip=getOption('ltxsparklines.clip'),
                      na.rm=getOption('ltxsparklines.na.rm'),
                      bottomline=getOption('ltxsparklines.bottomline'),
                      bottomlinelength=NA,
                      bottomlinex=getOption('ltxsparklines.bottomlinex'),
                      startdotcolor=getOption('ltxsparklines.startdotcolor'),
                      enddotcolor=getOption('ltxsparklines.enddotcolor'),
                      output=getOption('ltxsparklines.output')) {

    if(is.null(width) || is.na(width)) {
        width=10
    }

    if (clip) {
        result <- paste0("\\begin{sparkline*}{", width, "}\n");
    } else {
        result <- paste0("\\begin{sparkline}{", width, "}\n");
    }
    line <- .process_input(x,y, name='line')[,c(1,2)]
    if (na.rm && !is.null(line) && is.data.frame(line)) {
        line <- line[complete.cases(line),]
    }
    spikes <- .process_input(xspikes, yspikes, name='spikes')[,c(1,2)]
    dots <- .process_input(xdots, ydots, name='dots',
                           color=dotcolor)

    # Calculating ranges
    if (is.na(xlim[1])) {
        xlim[1] <- min(line[,1], spikes[,1], dots[,1], na.rm=TRUE)
    }
    if (is.na(ylim[1])) {
        ylim[1] <- min(line[,2], spikes[,2], dots[,2], na.rm=TRUE)
    }
    if (is.na(xlim[2])) {
        xlim[2] <- max(line[,1], spikes[,1], dots[,1], na.rm=TRUE)
    }
    if (is.na(ylim[2])) {
        ylim[2] <- max(line[,2], spikes[,2], dots[,2], na.rm=TRUE)
    }
    if (is.na(xlim[1]) || is.na(xlim[2]) || xlim[1]==xlim[2] || 
        is.na(ylim[1]) || is.na(ylim[2]) || ylim[1]==ylim[2]) {
        warning("Cannot calculate sparkline size",
                call. = FALSE)
        return(NULL)
    }

    line[,1] <- .range_normalize(line[,1], xlim)
    line[,2] <- .range_normalize(line[,2], ylim)
    spikes[,1] <- .range_normalize(spikes[,1], xlim)
    spikes[,2] <- .range_normalize(spikes[,2], ylim)
    dots[,1] <- .range_normalize(dots[,1], xlim)
    dots[,2] <- .range_normalize(dots[,2], ylim)
    rectangle <- .range_normalize(rectangle, ylim)
    bottomlinelength <- .range_normalize(bottomlinelength, xlim)
    bottomlinex <- .range_normalize(bottomlinex, xlim)


    
    if (!is.na(rectangle[1]) && !is.na(rectangle[2])) {
        result <- paste0(result, "\\sparkrectangle ",
                         rectangle[1], " ",
                         rectangle[2], "\n")
    }

    if (!is.null(spikes) && is.data.frame(spikes) && nrow(spikes)) {
        result <- paste0(result,
                         paste0(.plot_spikes(spikes),
                                collapse=""))
    }

        
    if (!is.null(line) && is.data.frame(line) && nrow(line)) {
        line.splits <- c(1,which(!complete.cases(line)),nrow(line))
        sparks <- sapply(1:(length(line.splits)-1), function (i) {
            start <- line.splits[i]
            end <- line.splits[i+1]
            .plot_line(line[start:end,])
        })
        result <- paste0(result, paste0(sparks, collapse=""))

        if (!is.null(startdotcolor) && !is.na(startdotcolor)) {
            result <-
                paste0(result,
                       .plot_dots (data.frame(x=line[1,1],
                                             y=line[1,2],
                                             color=startdotcolor)))
        }

        if (!is.null(enddotcolor) && !is.na(enddotcolor)) {
            n <- nrow(line)
            result <-
                paste0(result,
                       .plot_dots (data.frame(x=line[n,1],
                                             y=line[n,2],
                                             color=enddotcolor)))
        }

    }

    if (!is.null(dots) && is.data.frame(dots) && nrow(dots)) {
        result <- paste0(result,
                         paste0(.plot_dots(dots),
                                collapse=""))
    }

    if(!is.null(bottomline) && !is.na(bottomline) && bottomline) {
        if(is.na(bottomlinelength)) {
            bottomlinelength=1
        }
        result <- paste0(result,"\\sparkbottomline[",
                         bottomlinelength,
                         "]\n");
    }
    if(!is.null(bottomlinex) && !is.na(bottomlinex[1]) &&
       !is.na(bottomlinex[x])) {
        result <- paste0(result,"\\sparkbottomlinex",
                         bottomlinex[1], " ",
                         bottomlinex[2],
                         " \n");
    }
    if (clip) {
        result <- paste0(result,"\\end{sparkline*}");
    } else {
        result <- paste0(result,"\\end{sparkline}");
    }
    # Sweave bug/feature
    if (output == 'inlineSweave') {
        result <- gsub('([\\])', '\\1\\1\\1', result)
    }
    return(result)
}
                                            
                      
