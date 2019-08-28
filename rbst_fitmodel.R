#Estimates all 5 chosen robust SEs and returns list of fits (if output - "fit") or 
#estimates (if output = "estimates"):
# All use lavaan function, estimator ML, and se = "robust.sem"
# 
# 1.EXP.STR0: information= "expected", h1.information= "structured" 
# 2.EXP.UNS0: information= "expected", h1.information= "unstructured"
# 3.OBS.STR1: information= "observed", h1.information= "structured",   observed.information = "h1"
# 4.OBS.UNS1: information= "observed", h1.information= "unstructured"  observed.information = "h1"
# 5.OBS.HESS: information= "observed", observed.information = "hessian"

robust.fitmodel <- function(Timepoint = timepoint, 
                            d         = data,
                            m         = model,
                            output    = "estimates") {
  #Fit all 5 models:
  ress <- list(
    exp.str0 = try(lavaan(data = d, model = m, estimator = "ML",se = "robust.sem", information = "expected",h1.information="structured")),
    exp.uns0 = try(lavaan(data = d, model = m, estimator = "ML",se = "robust.sem", information = "expected",h1.information="unstructured")),
    obs.str1 = try(lavaan(data = d, model = m, estimator = "ML",se = "robust.sem", information = "observed",h1.information="structured",observed.information = "h1")),
    obs.uns1 = try(lavaan(data = d, model = m, estimator = "ML",se = "robust.sem", information = "observed",h1.information="unstructured",observed.information = "h1")),  
    obs.hess = try(lavaan(data = d, model = m, estimator = "ML",se = "robust.sem", information = "observed",observed.information = "hessian"))
  )
  
  for(i in 1:length(ress)){
    print(i)
    if(class(ress[[i]])=="try-error"){ 
      
    }else{
      insp            <- inspect(ress[[i]], what = "optim")
      converge        <- insp$converged
      
      if (converge == FALSE){ 
      }else{#Everything worked out
        
        if(output == "fit"){ #For remove function
          next
        }else if (output == "estimates"){ #For estimating
          print("here")
          estimates <- data.frame(parameterEstimates(ress[[i]]))
          z         <- estimates[estimates$label == "sint", "z"] #slope intercept
          LL        <- fitMeasures(ress[[i]], c("logl"))
          
          f.estims  <- c(z,LL)
          names(f.estims) <- c("z","logl")
          print(f.estims)
          ress[[i]] <- f.estims
        }else{"output param in modelfit wrong!"}
      }
    }
  } 
  return(ress)
}
