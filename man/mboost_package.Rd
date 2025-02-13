\name{mboost-package}
\alias{mboost-package}
\alias{mboost_package}
\alias{package_mboost}
\alias{package-mboost}
\docType{package}
\title{
  mboost: Model-Based Boosting
}
\description{
  Functional gradient descent algorithm
  (boosting) for optimizing general risk functions utilizing
  component-wise (penalized) least squares estimates or regression
  trees as base-learners for fitting generalized linear, additive
  and interaction models to potentially high-dimensional data.
}
\details{
\tabular{ll}{
Package: \tab mboost\cr
Version: \tab 2.9-3\cr
Date: \tab 2020-07-29\cr
License: \tab GPL-2\cr
}

  This package is intended for modern regression modeling and stands
  in-between classical generalized linear and additive models, as for example
  implemented by \code{\link{lm}}, \code{\link{glm}}, or \code{\link[mgcv]{gam}},
  and machine-learning approaches for complex interactions models,
  most prominently represented by \code{\link[gbm]{gbm}} and
  \code{\link[randomForest]{randomForest}}.

  All functionality in this package is based on the generic
  implementation of the optimization algorithm  (function
  \code{\link{mboost_fit}}) that allows for fitting linear, additive,
  and interaction models (and mixtures of those) in low and
  high dimensions. The response may be numeric, binary, ordered,
  censored or count data.

  Both theory and applications are discussed by Buehlmann and Hothorn (2007).
  UseRs without a basic knowledge of boosting methods are asked
  to read this introduction before analyzing data using this package.
  The examples presented in this paper are available as package vignette
  \code{mboost_illustrations}.

  Note that the model fitting procedures in this package DO NOT automatically
  determine an appropriate model complexity. This task is the responsibility
  of the data analyst.

  A description of novel features that were introduced in version 2.0 is
  given in Hothorn et. al (2010).

  Hofner et al. (2014) present a comprehensive hands-on tutorial for using the
  package \code{mboost}, which is also available as
  \code{vignette(package = "mboost", "mboost_tutorial")}.

  Ben Taieba and Hyndman (2013) used this package for fitting their model in the
  Kaggle Global Energy Forecasting Competition 2012. The corresponding research
  paper is a good starting point when you plan to analyze your data using
  \code{mboost}.

}

\section{NEWS in 2.9-series}{

  Series 2.9 provides a new family (\code{RCG}), uses \code{partykit::ctree} 
  instead of \code{party::ctree} to be more flexible, allows for multivariate 
  negative gradients, and leave-one-out crossvalidation. Further minor changes were 
  introduces and quite some bugs were fixed. 

  \bold{For more details and other changes see}\cr\code{news(Version >= "2.9-0", package  = "mboost")}
  
}

\section{NEWS in 2.8-series}{

  Series 2.8 allows to fit models with zero boosting steps (i.e., models containing 
  only the offset). Furthermore, cross-validation can now also select a model
  without base-learners. In a \code{\link{Binomial}} family one can now specifiy
  links via \code{make.link}. With \code{Binomial(type = "glm")} an alternative 
  implementation of \code{Binomial} models is now existing and defines the model
  along the lines of the \code{glm} implementation. Additionally, it works not only with a 
  two-level factor but also with a two-column matrix containing the number of 
  successes and number of failures. Finally, a new base-learner \code{\link{bkernel}} for
  kernel boosting was added. The references were updated and a lot of bugs fixed. 

  \bold{For more details and other changes see}\cr\code{news(Version >= "2.8-0", package  = "mboost")}
  
}

\section{NEWS in 2.7-series}{

  Series 2.7 provides a new family (\code{Cindex}), variable importance measures
  (\code{varimp}) and improved plotting facilities. The manual was updated in 
  various places, vignettes were improved and a lot of bugs were fixed. 

  \bold{For more details and other changes see}\cr\code{news(Version >= "2.7-0", package  = "mboost")}
  
}

\section{NEWS in 2.6-series}{

  Series 2.6 includes a lot of bug fixes and improvements. Most notably,
  the development of the package is now hosted entirely on github in the
  project \href{https://github.com/boost-R/mboost/}{boost-R/mboost}.
  Furthermore, the package is now maintained by Benjamin Hofner.

  \bold{For more details and other changes see}\cr\code{news(Version >= "2.6-0", package  = "mboost")}
  
}

\section{NEWS in 2.5-series}{

  Crossvaliation does not stop on errors in single folds anymore an was
  sped up by setting \code{mc.preschedule = FALSE} if parallel
  computations via \code{\link{mclapply}} are used. The
  \code{\link{plot.mboost}} function is now documented. Values outside
  the boundary knots are now better handeled (forbidden during fitting,
  while linear extrapolation is used for prediction). Further perfomance
  improvements and a lot of bug fixes have been added.

  \bold{For more details and other changes see}\cr\code{news(Version >= "2.5-0", package  = "mboost")}
}

\section{NEWS in 2.4-series}{

  Bootstrap confidence intervals have been implemented in the novel
  \code{\link[=confint.mboost]{confint}} function. The stability
  selection procedure has now been moved to a stand-alone package called
  \pkg{stabs}, which now also implements an iterface to use stability
  selection with other fitting functions. A generic function for
  \code{"mboost"} models is implemented in \pkg{mboost}.

  \bold{For more details and other changes see}\cr\code{news(Version >= "2.4-0", package  = "mboost")}
}

\section{NEWS in 2.3-series}{

  The stability selection procedure has been completely rewritten and
  improved. The code base is now extensively tested. New options allow
  for a less conservative error control.

  Constrained effects can now be fitted using quadratic programming
  methods using the option \code{type = "quad.prog"} (default) for
  highly improved speed. Additionally, new constraints have been added.

  Other important changes include:
  \itemize{
    \item A new replacement function \code{mstop(mod) <- i} as an alternative to
    \code{mod[i]} was added (as suggested by Achim Zeileis).

    \item We added new families \code{Hurdle} and \code{Multinomial}.

    \item We added a new argument \code{stopintern} for internal stopping
     (based on out-of-bag data) during fitting to \code{boost_control}.
  }

  \bold{For more details and other changes see}\cr\code{news(Version >= "2.3-0", package  = "mboost")}
}

\section{NEWS in 2.2-series}{
  Starting from version 2.2, the default for the degrees of freedom has
  changed. Now the degrees of freedom are (per default) defined as
  \deqn{\mathrm{df}(\lambda) = \mathrm{trace}(2S -
  S^{\top}S),}{df(\lambda) = trace(2S - S'S),} with smoother matrix
  \eqn{S = X(X^{\top}X + \lambda K)^{-1} X}{S = X(X'X + \lambda K)^(-1)
  X} (see Hofner et al., 2011). Earlier versions used the trace of the
  smoother matrix \eqn{\mathrm{df}(\lambda) = \mathrm{trace}(S)} as
  degrees of freedom. One can change the old definition using
  \code{options(mboost_dftraceS = TRUE)} (see also B. Hofner et al.,
  2011 and \code{\link{bols}}).

  Other important changes include:
  \itemize{
    \item We switched from packages \code{multicore} and \code{snow} to
    \code{parallel}

    \item We changed the behavior of \code{bols(x, intercept = FALSE)}
    when \code{x} is a factor: now the intercept is simply dropped from
    the design matrix and the coding can be specified as usually for
    factors. Additionally, a new contrast is introduced:
    \code{"contr.dummy"} (see \code{\link{bols}} for details).

    \item We changed the computation of B-spline basis at the
    boundaries; B-splines now also use equidistant knots in the
    boundaries (per default).
  }

  \bold{For more details and other changes see}\cr\code{news(Version >= "2.2-0" & Version < "2.3-0", package  = "mboost")}
}
\section{NEWS in 2.1-series}{

  In the 2.1 series, we added multiple new base-learners including
  \code{\link{bmono}} (monotonic effects), \code{\link{brad}} (radial
  basis functions) and \code{\link{bmrf}} (Markov random fields), and
  extended \code{\link{bbs}} to incorporate cyclic splines (via argument
  \code{cyclic = TRUE}). We also changed the default \code{df} for
  \code{\link{bspatial}} to \code{6}.

  Starting from this version, we now also automatically center the
  variables in \code{\link{glmboost}} (argument \code{center = TRUE}).

  \bold{For more details and other changes see}\cr\code{news(Version >= "2.1-0" & Version < "2.2-0", package  = "mboost")}
}
\section{NEWS in 2.0-series}{

  Version 2.0 comes with new features, is faster and more accurate
  in some aspects. In addition, some changes to the user interface
  were necessary: Subsetting \code{mboost} objects changes the object.
  At each time, a model is associated with a number of boosting iterations
  which can be changed (increased or decreased) using the subset operator.

  The \code{center} argument in \code{\link{bols}} was renamed
  to \code{intercept}. Argument \code{z} renamed to \code{by}.

  The base-learners \code{bns} and \code{bss} are deprecated
  and replaced by \code{bbs} (which results in qualitatively the
  same models but is computationally much more attractive).

  New features include new families (for example for ordinal regression)
  and the \code{which} argument to the \code{coef} and \code{predict}
  methods for selecting interesting base-learners. Predict
  methods are much faster now.

  The memory consumption could be reduced considerably,
  thanks to sparse matrix technology in package \code{Matrix}.
  Resampling procedures run automatically in parallel
  on OSes where parallelization via package \code{parallel} is available.

  The most important advancement is a generic implementation
  of the optimizer in function \code{\link{mboost_fit}}.

  \bold{For more details and other changes see}\cr\code{news(Version >= "2.0-0" & Version < "2.1-0", package  = "mboost")}

}
\author{
  Torsten Hothorn,Peter Buehlmann, Thomas Kneib, Matthias Schmid and
  Benjamin Hofner <\email{Benjamin.Hofner@pei.de}>
}

\references{
  Peter Buehlmann and Torsten Hothorn (2007),
  Boosting algorithms: regularization, prediction and model fitting.
  \emph{Statistical Science}, \bold{22}(4), 477--505. \cr
  \doi{10.1214/07-STS242}

  Torsten Hothorn, Peter Buehlmann, Thomas Kneib, Matthias Schmid and
  Benjamin Hofner (2010), Model-based Boosting 2.0. \emph{Journal of
  Machine Learning Research}, \bold{11}, 2109--2113. \cr
  \url{https://jmlr.csail.mit.edu/papers/v11/hothorn10a.html}

  Benjamin Hofner, Torsten Hothorn, Thomas Kneib, and Matthias Schmid (2011),
  A framework for unbiased model selection based on boosting.
  \emph{Journal of Computational and Graphical Statistics}, \bold{20}, 956--971. \cr
  \doi{10.1198/jcgs.2011.09220}

  Benjamin Hofner, Andreas Mayr, Nikolay Robinzonov and Matthias Schmid
  (2014). Model-based Boosting in R: A Hands-on Tutorial Using the R
  Package mboost. \emph{Computational Statistics}, \bold{29}, 3--35.\cr
  \doi{10.1007/s00180-012-0382-5}

  Available as vignette via: \code{vignette(package = "mboost",
    "mboost_tutorial")}

  Souhaib Ben Taieba and Rob J. Hyndman (2014),
  A gradient boosting approach to the Kaggle load forecasting competition.
  \emph{International Journal of Forecasting}, \bold{30}, 382--394.\cr
  \doi{10.1016/j.ijforecast.2013.07.005}

}

\seealso{
  The main fitting functions include:\cr
  \itemize{
    \item \code{\link{gamboost}} for boosted (generalized) additive models,
    \item \code{\link{glmboost}} for boosted linear models and
    \item \code{\link{blackboost}} for boosted trees.
  }
  Model tuning is done via cross-validation as implemented in \code{\link{cvrisk}}.\cr
  See there for more details and further links.
}

\examples{

  data("bodyfat", package = "TH.data")
  set.seed(290875)

  ### model conditional expectation of DEXfat given
  model <- mboost(DEXfat ~
      bols(age) +                 ### a linear function of age
      btree(hipcirc, waistcirc) + ### a smooth non-linear interaction of
                                  ### hip and waist circumference
      bbs(kneebreadth),           ### a smooth function of kneebreadth
      data = bodyfat, control = boost_control(mstop = 100))

  ### 10-fold cv for assessing `optimal' number of boosting iterations
  cvm <- cvrisk(model, papply = lapply, 
                folds = cv(model.weights(model), type = "kfold"))
  ### probably needs larger initial mstop but the
  ### CRAN team is picky about running times for examples
  plot(cvm)

  ### restrict model to mstop(cvm)
  model[mstop(cvm), return = FALSE]
  mstop(model)

  ### plot age and kneebreadth
  layout(matrix(1:2, nc = 2))
  plot(model, which = c("age", "kneebreadth"))

  ### plot interaction of hip and waist circumference
  attach(bodyfat)
  nd <- expand.grid(hipcirc = h <- seq(from = min(hipcirc),
                                  to = max(hipcirc),
                                  length = 100),
                    waistcirc = w <- seq(from = min(waistcirc),
                                  to = max(waistcirc),
                                  length = 100))
  plot(model, which = 2, newdata = nd)
  detach(bodyfat)

  ### customized plot
  layout(1)
  pr <- predict(model, which = "hip", newdata = nd)
  persp(x = h, y = w, z = matrix(pr, nrow = 100, ncol = 100))

}

\keyword{ package }
\keyword{ smooth }
\keyword{ nonparametric }
\keyword{ models }
