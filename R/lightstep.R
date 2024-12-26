#' Fast and light stepwise selection of regressors
#'
#' Function selects variables that give linear regression with the lowest
#' information criteria. The selection is done stepwise (forward) based on
#' partial correlations. This should be a simpler and faster implementation
#' than step() function from the `stats' package.
#'
#' The algorithm uses alm() to fit different models and cor() to select the next
#' regressor in the sequence.
#'
#'
#' @template author
#' @template keywords
#'
#' @param data Data frame containing dependant variable in the first column and
#' the others in the rest. The variables need to be numeric, categorical ones are not
#' supported and should be expanded via the \link[stats]{model.matrix} in advance.
#' @param ic Information criterion to use.
#' @param silent If \code{silent=FALSE}, then nothing is silent, everything is
#' printed out. \code{silent=TRUE} means that nothing is produced.
#' @param df Number of degrees of freedom to add (should be used if stepwise is
#' used on residuals).
#' @param subset an optional vector specifying a subset of observations to be
#' used in the fitting process.
#' @param distribution Distribution to pass to \code{alm()}. See \link[greybox]{alm}
#' for details.
#' @param occurrence what distribution to use for occurrence part. See
#' \link[greybox]{alm} for details.
#' @param ... This is temporary and is needed in order to capture "silent"
#' parameter if it is provided.
#'
#' @return Function returns \code{model} - the final model of the class "alm".
#' See \link[greybox]{alm} for details of the output.
#'
#' @seealso \code{\link[stats]{step}, \link[greybox]{xregExpander},
#' \link[greybox]{lmCombine}}
#'
#' @examples
#'
#' ### Simple example
#' obs <- 100000
#' xreg <- matrix(rnorm(200*obs,10,3),obs,200)
#' xreg <- cbind(100+0.5*xreg[,1]-0.75*xreg[,2]+rnorm(obs,0,3),xreg,rnorm(obs,300,10))
#' colnames(xreg) <- c("y",paste0("x",c(1:200)),"Noise")
#' xreg <- data.table(xreg)
#' lightstep(xreg)
#'
#' @importFrom stats .lm.fit
#' @importFrom greybox is.occurrence AICc BICc
#' @export lightstep
lightstep <- function(data, ic=c("AICc","AIC","BIC","BICc"), silent=TRUE, df=NULL,
                      subset=NULL,
                      distribution=c("dnorm","dlaplace","ds","dgnorm","dlogis","dt","dalaplace",
                                     "dlnorm","dllaplace","dls","dlgnorm","dbcnorm",
                                     "dinvgauss","dgamma","dexp",
                                     "dfnorm","drectnorm",
                                     "dpois","dnbinom",
                                     "dbeta","dlogitnorm",
                                     "plogis","pnorm"),
                      occurrence=c("none","plogis","pnorm"), ...){
    ##### Function that selects variables based on IC and using partial correlations

    # Start measuring the time of calculations
    startTime <- Sys.time();

    if(is.null(df)){
        df <- 0;
    }
    # Create substitute and remove the original data
    dataSubstitute <- substitute(data);

    # Use formula to form the data frame for further selection
    if(!is.null(formula) || !is.null(subset)){
        # If subset is provided, but not formula, generate one
        if(is.null(formula)){
            formula <- as.formula(paste0(colnames(data)[1],"~."));
        }

        # Do model.frame manipulations
        mf <- match.call(expand.dots = FALSE);
        mf <- mf[c(1L, match(c("formula", "data", "subset"), names(mf), 0L))];
        mf$drop.unused.levels <- TRUE;
        mf[[1L]] <- quote(stats::model.frame);

        if(!is.data.frame(data)){
            mf$data <- as.data.frame(data);
        }
        # Evaluate data frame to do transformations of variables
        data <- eval(mf, parent.frame());
        responseName <- colnames(data)[1];

        # Remove variables that have "-x" in the formula
        dataTerms <- terms(data);
        data <- data[,c(responseName, colnames(attr(dataTerms,"factors")))];
        ## We do it this way to avoid factors expansion into dummies at this stage
    }

    # Get rid of data.table class
    if(is.data.table(data)){
        # Check, whether the response is numeric
        if(!is.numeric(data[[1]])){
            warning(paste0("The response variable is not numeric! ",
                           "We will make it numeric, but we cannot promise anything."),
                    call.=FALSE);
            data[[1]] <- as.numeric(data[[1]]);
        }
        data <- as.matrix(data);
    }

    distribution <- match.arg(distribution);
    if(distribution=="dnorm"){
        useALM <- FALSE;
    }
    else{
        useALM <- TRUE;
        if(any(distribution==c("plogis","pnorm"))){
            data[,1] <- (data[,1]!=0)*1;
        }
    }
    # Only likelihood is supported by the function
    loss <- "likelihood";

    # Check the data for NAs
    if(any(is.na(data))){
        rowsSelected <- apply(!is.na(data),1,all);
    }
    else{
        rowsSelected <- rep(TRUE,nrow(data));
    }

    # If occurrence is not provideded, then set it to "none"
    if(is.null(occurrence)){
        occurrence <- "none";
    }
    # Check occurrence. If it is not "none" then use alm().
    if(is.occurrence(occurrence)){
        useALM[] <- TRUE;
        rowsSelected[] <- rowsSelected & (data[,1]!=0);
    }
    else{
        occurrence <- match.arg(occurrence);
        if(any(occurrence==c("plogis","pnorm"))){
            useALM[] <- TRUE;
            rowsSelected[] <- rowsSelected & (data[,1]!=0);
        }
    }

    # If something is passed in the ellipsis, use alm()
    if(length(list(...))>0){
        useALM[] <- TRUE;
    }

    listToCall <- list(...);
    # Define what function to use in the estimation
    if(useALM){
        lmCall <- alm;
        listToCall$distribution <- distribution;
        listToCall$loss <- loss;
        listToCall$fast <- TRUE;
    }
    else{
        # if(!is.data.frame(data)){
            lmCall <- function(formula, ...){
                varsToUse <- c("(Intercept)",all.vars(formula)[-1]);
                model <- .lm.fit(data[, varsToUse, drop=FALSE], y);
                # colnames(model$qr) <- varsToUse;
                return(structure(model,class="lm"));
            }
        # }
        # else{
        #     lmCall <- function(formula, data){
        #         model <- .lm.fit(model.matrix(formula, data=data),
        #                          as.matrix(data[,all.vars(formula)[1]]));
        #         return(structure(model,class="lm"));
        #     }
        # }
        listToCall <- vector("list");
    }

    # The gsub is needed in order to remove accidental special characters
    # colnames(data) <- gsub("\`","",colnames(data),ignore.case=TRUE);
    colnames(data) <- make.names(colnames(data), unique=TRUE);
    # Names of the variables
    variablesNames <- colnames(data);

    # The number of explanatory variables and the number of observations
    nVariables <- ncol(data)-1;
    obsInsample <- sum(rowsSelected);

    # Create data to work with
    if(any(!rowsSelected)){
        data <- data[rowsSelected,];
    }
    y <- data[,1, drop=FALSE];
    data[,1] <- 1;
    colnames(data)[1] <- "(Intercept)";
    # listToCall$data <- data[rowsSelected,];
    # listToCall$y <- listToCall$data[,1];
    # listToCall$data[,1] <- 1;
    # colnames(listToCall$data)[1] <- "(Intercept)";
    errors <- matrix(0,obsInsample,1);

    # Record the names of the response and the explanatory variables
    responseName <- variablesNames[1];
    variablesNames <- variablesNames[-1];

    assocValues <- vector("numeric",nVariables);
    names(assocValues) <- variablesNames;
    #### The function that works similar to association(), but faster ####
    # assocFast <- function(){
    #     # Measures of association with numeric data
    #     # assocValues[] <- suppressWarnings(cor(errors,data[,-1],
    #     #                                       use="complete.obs",method=method));
    #     return(corCpp(errors,data[,-1]));
    # }

    # Select IC
    ic <- match.arg(ic);
    IC <- switch(ic,"AIC"=AIC,"BIC"=BIC,"BICc"=BICc,AICc);

    method <- method[1];

    bestICNotFound <- TRUE;
    allICs <- list(NA);
    # Run the simplest model y = const
    testFormula <- paste0("`",responseName,"`~ 1");
    listToCall$formula <- as.formula(testFormula);
    testModel <- do.call(lmCall,listToCall);
    # Write down the logLik and take df into account
    logLikValue <- logLik(testModel);
    attributes(logLikValue)$df <- nparam(logLikValue) + df;
    # Write down the IC. This one needs to be calculated from the logLik
    # in order to take the additional df into account.
    currentIC <- bestIC <- IC(logLikValue);
    names(currentIC) <- "Intercept";
    allICs[[1]] <- currentIC;
    # Add residuals to the ourData
    errors[] <- residuals(testModel);

    bestFormula <- testFormula;
    if(!silent){
        cat("Formula: "); cat(testFormula);
        cat(", IC: "); cat(currentIC); cat("\n\n");
    }

    m <- 2;
    # Start the loop
    while(bestICNotFound){
        assocValues[] <- corCpp(errors,data[,-1]);

        newElement <- variablesNames[which(abs(assocValues)==
                                               max(abs(assocValues[!(variablesNames %in% all.vars(as.formula(bestFormula)))]),
                                                                    na.rm=TRUE))[1]];
        # If the newElement is the same as before, stop
        if(is.na(newElement) || any(newElement==all.vars(as.formula(bestFormula)))){
            bestICNotFound <- FALSE;
            break;
        }
        # Include the new element in the original model
        testFormula <- paste0(testFormula,"+`",newElement,"`");
        listToCall$formula <- as.formula(testFormula);
        testModel[] <- do.call(lmCall,listToCall);
        # Modify logLik
        logLikValue <- logLik(testModel);
        attributes(logLikValue)$df <- nparam(logLikValue) + df;
        if(attributes(logLikValue)$df >= (obsInsample+1)){
            if(!silent){
                warning("Number of degrees of freedom is greater than number of observations. Cannot proceed.");
            }
            bestICNotFound <- FALSE;
            break;
        }

        # Calculate the IC
        currentIC <- IC(logLikValue);
        if(!silent){
            cat(paste0("Step ",m-1,". "));
            cat("Formula: "); cat(testFormula);
            cat(", IC: "); cat(currentIC);
            cat("\nCorrelations: \n"); print(round(assocValues,3)); cat("\n");
        }
        # If IC is greater than the previous, then the previous model is the best
        if(currentIC >= bestIC){
            bestICNotFound <- FALSE;
        }
        else{
            bestIC <- currentIC;
            bestFormula <- testFormula;
            errors[] <- residuals(testModel);
        }
        names(currentIC) <- newElement;
        allICs[[m]] <- currentIC;
        m <- m+1;
    }

    # Remove "1+" from the best formula
    bestFormula <- sub(" 1+", "", bestFormula,fixed=TRUE);
    bestFormula <- as.formula(bestFormula);

    # If this is a big data just wrap up the stuff using lmCall
    if(distribution=="dnorm" && !useALM){
        varsNames <- all.vars(bestFormula)[-1];

        listToCall$formula <- bestFormula;

        bestModel <- do.call(lmCall,listToCall);
        # Expand the data from the final model
        bestModel$data <- cbind(y,listToCall$data[,all.vars(bestFormula)[-1]]);
        if(is.null(colnames(bestModel$data))){
            colnames(bestModel$data) <- c(responseName,varsNames);
        }
        else{
            colnames(bestModel$data)[1] <- responseName;
        }
        rm(listToCall);

        bestModel$distribution <- distribution;
        bestModel$logLik <- bestModel$lossValue <- logLik(bestModel);
        bestModel$mu <- bestModel$fitted <- bestModel$data[,1] - c(bestModel$residuals);
        # This is number of variables + constant + variance
        bestModel$df <- length(bestModel$coefficients) + 1;
        bestModel$df.residual <- obsInsample - bestModel$df;
        names(bestModel$coefficients) <- c("(Intercept)",varsNames);
        # Remove redundant bits
        bestModel$effects <- NULL;
        bestModel$qr <- NULL;
        bestModel$qraux <- NULL;
        bestModel$pivot <- NULL;
        bestModel$tol <- NULL;
        # Form the pseudocall to alm
        bestModel$call <- quote(alm(formula=bestFormula, data=data, distribution="dnorm"));
        bestModel$call$formula <- bestFormula;
        bestModel$subset <- rep(TRUE, obsInsample);
        bestModel$scale <- sqrt(sum(bestModel$residuals^2) / obsInsample);
        bestModel$loss <- loss;
        class(bestModel) <- c("alm","greybox");
    }
    else{
        listToCall$formula <- bestFormula;
        listToCall$data <- dataSubstitute;
        listToCall$distribution <- distribution;
        listToCall$loss <- loss;
        listToCall$occurrence <- occurrence;
        listToCall$fast <- TRUE;
        bestModel <- do.call("alm", listToCall,
                             envir = parent.frame());
        bestModel$call$occurrence <- substitute(occurrence);
        class(bestModel) <- c("alm","greybox");
        if(any(distribution==c("plogis","pnorm"))){
            class(bestModel) <- c(class(bestModel),"occurrence");
        }
    }

    bestModel$ICs <- unlist(allICs);
    bestModel$timeElapsed <- Sys.time()-startTime;

    return(bestModel);
}
