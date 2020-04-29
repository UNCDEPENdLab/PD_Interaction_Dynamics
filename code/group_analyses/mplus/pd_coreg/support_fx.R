
univariateoutiers_min <- function(vec) {
  min <- as.numeric(quantile(vec, .25))
  max <- as.numeric(quantile(vec, .75))
  iqr = IQR(vec)
  min_cutoff <- min - 1.5 * (iqr)
  return(min_cutoff)
  
  
  
}
univariateoutiers_max <- function(vec) {
  min <- as.numeric(quantile(vec, .25))
  max <- as.numeric(quantile(vec, .75))
  iqr = IQR(vec)
  max_cutoff <- max + 1.5 * (iqr)
  return(max_cutoff)
  
  
  
}

corwithtarget <-
  function(df,
           omit = NULL,
           target,
           withvars = NULL,
           pmin = NULL,
           partial = NULL,
           absrmin = NULL,
           digits = 3,
           prewhiten = FALSE,
           orderbyr = FALSE) {
    require(forecast)
    
    if (!is.null(omit)) {
      dnames <- which(names(df) %in% omit)
      df <- df[, -1 * dnames]
    }
    
    if (is.null(withvars)) {
      withvars <- names(df)[which(!names(df) %in% target)]
    }
    
    if (!is.null(partial)) {
      df <- as.data.frame(lapply(df, function(col) {
        residuals(lm(col ~ as.matrix(df[, partial])))
      }))
    }
    
    res <- sapply(target, function(tv) {
      cvec <- sapply(withvars, function(wv) {
        #prewhiten?
        if (prewhiten) {
          r <- residuals(lm(df[, tv] ~ df[, wv]))
          a <- auto.arima(r)
          x <- Arima(df[, wv], model = a)$residuals
          y <- Arima(df[, tv], model = a)$residuals
        } else {
          x <- df[, wv]
          y <- df[, tv]
        }
        
        tryCatch(
          rc <-
            Hmisc::rcorr(x, y),
          error = function(e) {
            print(e)
            
          }
        )
        list(r = round(rc$r[1, 2], 3), p = round(rc$P[1, 2], 3))
      })
      
      if (!is.null(pmin)) {
        sigr <- which(unlist(cvec["p", ]) <= pmin)
        if (length(sigr) == 0L) {
          cvec <- c()
        } else {
          cvec <- cvec[, sigr, drop = FALSE]
        }
      }
      
      if (!is.null(absrmin)) {
        goodr <- which(abs(unlist(cvec["r", ])) >= absrmin)
        if (length(goodr) == 0L) {
          cvec <- c()
        } else {
          cvec <- cvec[, goodr, drop = FALSE]
        }
      }
      
      #be sure that we never include the correlation of the variable with itself
      selfmatch <- dimnames(cvec)[[2]] == tv
      cvec <- cvec[, !selfmatch, drop = FALSE]
      
      #reorder by correlation size if requested
      if (orderbyr == TRUE) {
        cvec <- cvec[, order(unlist(cvec[1, ]), decreasing = TRUE)]
      }
      
      return(cvec)
      
      #print(cvec)
    }, simplify = FALSE)
    
    return(res)
  }

runBSEM <- function(modelsyntaxobj) {
  mobj = mplusObject(
    TITLE = "Testing",
    usevariables = as.vector(modelsyntaxobj[[1]]),
    ANALYSIS = "ESTIMATOR=BAYES; PROCESSORS = 2;BITERATIONS = (30000);",
    MODEL = modelsyntaxobj[[2]],
    rdata = modelsyntaxobj[[4]] ,
    OUTPUT = "STANDARDIZED"
  )
  mbin <- "/Users/alisonmarie526/Applications/Mplus/mplus"
  fitted <-
    mplusModeler(
      mobj,
      modelout = paste0(modelsyntaxobj[[5]], ".inp"),
      run = TRUE,
      Mplus_command = mbin,
      hashfilename = FALSE
    )
}

modsyntax <- function(dat, model) {
  div10 <- function(x) {
    return(x / 10)
  }
  mult10 <- function(x) {
    return(x * 10)
  }
  
  mname = deparse(substitute(model))
  d <- sem(
    model,
    dat,
    missing = "ML",
    estimator = "MLR",
    meanstructure = TRUE,
    mimic = "Mplus"
  )
  paramest <- parameterestimates(d)
  allvarnames = ""
  modsyntax = ""
  allvarsyntax = ""
  lavmodsyntax = ""
  for (i in 1:length(paramest$lhs)) {
    lhs = paramest$lhs[[i]]
    op = paramest$op[[i]]
    rhs = paramest$rhs[[i]]
    label = paramest$label[[i]]
    param = paramest$est[[i]]
    lavlinesyntax = ifelse((label == "" ||
                              op == ":=" ||
                              is.null(label)),
                           paste0(lhs, op, rhs, "\n"),
                           paste0(lhs, op, label, "*", rhs, "\n")
    )
    lavlinesyntax2 = ifelse(param == 0, paste0(lhs, op, "0*", rhs, "\n"), lavlinesyntax)
    lavmodsyntax = paste0(lavmodsyntax, lavlinesyntax2)
    
    if (op == "~~") {
      opr = " WITH "
    } else if (op == "~") {
      opr = " ON "
    } else if (op == "~1") {
      lhs = paste0("[", lhs, "]")
      
      opr = ""
      
    } else {
      opr = op
    }
    if (label == "" ||
        (op == ":=") || is.null(label)) {
      lab = ""
    } else {
      lab = paste0("* (", label, ")")
    }
    if (op == ":=") {
      linesyntax = ""
      newvarname = label
      allvarnames = paste(allvarnames, newvarname, sep = " ")
      newvarsyntax = paste0(lhs, " = ", rhs, lab, ";\n")
      allvarsyntax = paste0(allvarsyntax, newvarsyntax)
      modsyntax = paste0(modsyntax, linesyntax)
    } else if (param == 0) {
      linesyntax = paste0(lhs, opr, rhs, "@0;\n")
      modsyntax = paste0(modsyntax, linesyntax)
    } else {
      linesyntax = paste0(lhs, opr, rhs, lab, "; \n")
      modsyntax = paste0(modsyntax, linesyntax)
    }
  }
  if (allvarsyntax != "") {
    modsyntax = paste0(modsyntax,
                       "\n",
                       "MODEL CONSTRAINT: \n",
                       "NEW (",
                       allvarnames,
                       " ); \n",
                       allvarsyntax)
  }
  
  usevardf <-
    data.frame(
      var_lhs = as.vector(as.character(paramest$lhs)),
      opr = as.vector(paramest$op),
      var_rhs = as.vector(as.character(paramest$rhs))
    )
  usevardf <- dplyr::filter(usevardf,  opr != ":=", opr != "~1")
  usevardf_long <-
    tidyr::gather(usevardf, key = "var_side", value = "var",-opr) %>% dplyr::filter(var != "")
  
  usevar_asvector <- unique(as.vector(usevardf_long$var))
  fname <- paste0(bsem_dir, "/", mname, "_df.dat")
  print(fname)
  datObj <- prepareMplusData(dat, fname)
  return(list(usevar_asvector, modsyntax, datObj, dat, mname, d))
}

pull_fitmeasures <-
  function(m, y, data_frame = posnegint_personality) {
    if (is.null(m) || is.null(y)) {
      new_fits <- c(0, 0, 0, 0,
                    0,0,0, 0)
      return(new_fits)
    }
    m_name <- y
    bsem_out <- runBSEM(modsyntax(dat = data_frame, m_name))
    dic <- bsem_out$results$summaries$DIC
    new_fits <-
      as.vector(fitmeasures(
        m,
        fit.measures = c(
          "unrestricted.logl",
          "aic",
          "bic",
          "baseline.df",
          "cfi.scaled",
          "rmsea.scaled",
          "srmr"
        )
      ))
    new_fits <- c(new_fits, dic)
    return(new_fits)
    
  }
