


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








#' GAN
#'
#' Runs the GAN in Python and returns the results
#'
#' @param data Training data in the form of a matrix. Each row is an observation, each column is a time point.
#' @param epochs Number of epochs. Default 500
#' @param batch_size Batch size for each training iteration
#' @param file Location of the python file containing the GAN functions
#' @param model_name Name of the model to submit to Python. Default 'gan_r'.
#' @param continue_training Logical. If you want to continue to train a model set to TRUE.
#' @param trace Frequency at which to write to the log. Convenient to monitor progress. Logs saved at './log/' in package directory
#'
#' @return
#' @export
#'
#' @importFrom reticulate py_run_file py_run_string
#' @importFrom glue glue
#'
#' @examples
gan <- function(data, file = "./Python/gan.py", epochs = 500, batch_size = 32, model_name = "gan_r", continue_training = FALSE, trace = 100) {
  x_train <<- data
  py_run_file(file)
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







