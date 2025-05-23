% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/McmcSettings.R
\name{McmcSettings}
\alias{McmcSettings}
\title{MCMC Settings Class}
\description{
Helper class to manage Markov Chain parameters consistently across models
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{iterations}}{Number of total iterations per chain}

\item{\code{warmup}}{Number of warmup iterations}

\item{\code{chains}}{Number of Markov chains}

\item{\code{cores}}{Number of cores for parallel processing}

\item{\code{adapt_delta}}{Target acceptance rate}

\item{\code{max_treedepth}}{Maximum tree depth for NUTS sampler}

\item{\code{seed}}{Random seed for reproducibility}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-McmcSettings-new}{\code{McmcSettings$new()}}
\item \href{#method-McmcSettings-get_brms_settings}{\code{McmcSettings$get_brms_settings()}}
\item \href{#method-McmcSettings-clone}{\code{McmcSettings$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-McmcSettings-new"></a>}}
\if{latex}{\out{\hypertarget{method-McmcSettings-new}{}}}
\subsection{Method \code{new()}}{
Initialize MCMC settings
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{McmcSettings$new(
  iterations = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  adapt_delta = 0.999,
  max_treedepth = 15,
  seed = 123
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{iterations}}{Number of total iterations per chain}

\item{\code{warmup}}{Number of warmup iterations}

\item{\code{chains}}{Number of Markov chains}

\item{\code{cores}}{Number of cores for parallel processing}

\item{\code{adapt_delta}}{Target acceptance rate}

\item{\code{max_treedepth}}{Maximum tree depth for NUTS sampler}

\item{\code{seed}}{Random seed for reproducibility}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-McmcSettings-get_brms_settings"></a>}}
\if{latex}{\out{\hypertarget{method-McmcSettings-get_brms_settings}{}}}
\subsection{Method \code{get_brms_settings()}}{
Get settings as list for brms
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{McmcSettings$get_brms_settings()}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{iterations}}{Number of total iterations per chain}

\item{\code{warmup}}{Number of warmup iterations}

\item{\code{chains}}{Number of Markov chains}

\item{\code{cores}}{Number of cores for parallel processing}

\item{\code{adapt_delta}}{Target acceptance rate}

\item{\code{max_treedepth}}{Maximum tree depth for NUTS sampler}

\item{\code{seed}}{Random seed for reproducibility}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-McmcSettings-clone"></a>}}
\if{latex}{\out{\hypertarget{method-McmcSettings-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{McmcSettings$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
