


#' Training data
#'
#' Generates training data for testing the GAN. Function generates two sine waves with different periods and amplitudes
#'
#' @param n Number of waves
#' @param m Number of points to evaluate the sine waves
#' @param shape List of shape parameters for the two sine waves. \code{a} amplitude; \code{p} period factor i.e. how many cycles
#'
#' @return
#' @export
#'
#' @examples
get_training_data <- function(n = 200, m = 250, shape = list(a = c(1, 3), p = c(2, 10))) {
  mat <- matrix(NA, nrow = n, ncol = m)
  y <- rep(NA, n)
  for(k in 1:n){
    ak <- shape$a[k %% 2 + 1]
    pk <- shape$p[k %% 2 + 1]
    mat[k,] <- ak*sin(2*pi*(1:m)/m*pk) + rnorm(m, 0, 0.05)
  }
  mat
}




#' Plot a set of curves
#'
#' For inspecting the output
#'
#' @param data x_train
#' @param id which rows to plot. Defaults to 1:2
#' @param nrow
#' @param k
#'
#' @return
#' @export
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom tidyr gather
#' @importFrom stringr str_extract
#'
#' @examples
plot_curves <- function(data, id = 1:2, nrow = NULL, k = NULL) {

  if(is.list(data)) data = data[[length(data)]]

  if(!is.null(k)) km <- kmeans(data, k)

  data[id,] %>%
    t() %>%
    as_tibble() %>%
    mutate(x = 1:n()) %>%
    gather("series", "y", -x) %>%
    mutate(id = as.numeric(str_extract(series, "[[:digit:]]+"))) %>%
    {if(!is.null(k)) left_join(., tibble(id = id, cluster = as.factor((km$cluster[id]))), by = "id") else mutate(., cluster = 1)} %>%
    as.data.frame() %>%
    ggplot(aes(x = x, y = y, col = cluster)) +
    geom_line() +
    facet_wrap(~series, nrow = nrow) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      strip.text = element_blank(),
      legend.position = "none"
      )
}


#' Title
#'
#' @param data
#' @param id
#' @param nrow
#' @param k
#'
#' @return
#' @export
#'
#' @examples
get_curves <- function(data, id = 1:2, nrow = NULL, k = NULL) {

  if(is.list(data)) data = data[[length(data)]]

  if(!is.null(k)) km <- kmeans(data, k)

  data[id,] %>%
    t() %>%
    as_tibble() %>%
    mutate(x = 1:n()) %>%
    gather("series", "y", -x) %>%
    mutate(id = as.numeric(str_extract(series, "[[:digit:]]+"))) %>%
    {if(!is.null(k)) left_join(., tibble(id = id, cluster = as.factor((km$cluster[id]))), by = "id") else mutate(., cluster = 1)} %>%
    as.data.frame()
}



#' GAN
#'
#' Runs the GAN in Python and returns the results
#'
#' @param data
#' @param epochs
#' @param batch_size
#'
#' @return
#' @export
#'
#' @importFrom reticulate py_run_file py_run_string
#' @importFrom glue glue
#'
#' @examples
gan <- function(data, epochs = 500, batch_size = 32, model_name = "gan_r", continue_training = FALSE, trace = 100) {
  x_train <<- data
  py_run_file("./Python/gan.py")
  if(!continue_training) message("python GAN object saved as 'gan_r'\naccess by py$gan_r e.g. py$gan_r$details()")
  py_str <- glue("
  {model_name} = GAN()
  {model_name}.trace = {trace}
  {model_name}.train(epochs = {epochs}, batch_size = {batch_size})
  ")
  if(continue_training) {
    py_str <- glue("gan_r.train(epochs = {epochs}, batch_size = {batch_size})")
    message("training existing model")
  }
  print(py_str)
  py_run_string(py_str)
}







#' Joint function
#'
#' @param m Number of points to evaluate the function. Default 200
#' @param shape Shape of the sine waves. Default \code{list(a = c(1, 3), p = c(2, 10))}
#'
#' @importFrom purrr map_dfr
#'
#' @return
#' @export
#'
#' @examples
y_joint <- function(m = 200, shape = list(a = c(1, 3), p = c(2, 10))){

  df <- map_dfr(seq(0, 1, length = 12), ~data.frame(
    x = 1:m,
    y = .x*sin(2*pi*(1:m)/(m/shape$p[1])) +
      (1-.x)*sin(2*pi*(1:m)/(m/shape$p[2])) +
      rnorm(m, 0, 0.05),
    a = round(.x, 2)))

  ggplot(df, aes(x = x, y = y)) + geom_line() + facet_wrap(~a)
}

