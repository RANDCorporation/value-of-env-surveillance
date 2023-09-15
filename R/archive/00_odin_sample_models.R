
library(odin)

# Logistic group
path_logistic <- system.file("examples/logistic.R", package = "odin")

# Compile and construct model class
modelclass <- odin(path_logistic)

mod <- modelclass$new()
mod

# RhS is the "Right-hand side of the model"
# This executes one time-step in the model
mod$rhs(t = 1, y = 0)

# Model initial conditions
# Compute model derivatives
mod$deriv(0, 1)

mod$deriv(0, 50)

# Content of al variables, intermediates and parameters
# We can think of this as the state-space of the model
mod$contents()

# Set times

tt <- seq(0, 30, length.out = 101)

# Can pass other initial conditions
y <- mod$run(t = tt, y = 50)
plot(y, xlab = "Time", ylab = "N", las = 1, main = "")


# Parameters are specified within the model file with the user() function:


generator <- odin::odin(x = "./R/odin_models/logitic_growth.R")


generator$new(r = 1)


# A Delay model:

gen <- odin::odin(workdir = "cpp/",{
  # This first line specifies the delay
  ylag <- delay(y, tau)
  initial(y) <- 0.5
  deriv(y) <- 0.2 * ylag * 1 / (1 + ylag^10) - 0.1 * y
  tau <- user(10)
  output(ylag) <- ylag
})
dde <- gen$new()

# This model uses the deSolve::dede function.

t <- seq(0, 300, length.out = 301)
y1 <- dde$run(t)
plot(y1, ylab = "y", mfrow = c(1, 2), which = 1)
plot(y1[, -1L], xlab = "y", ylab = "ylag", mfrow = NULL, type = "l")

# Lags introduce some interesting dynamics:

dde$set_user(tau = 20)
y2 <- dde$run(t)
plot(y2, ylab = "y", mfrow = c(1, 2), which = 1)
plot(y2[, -1L], xlab = "y", ylab = "ylag", mfrow = NULL, type = "l")

# ODIN supports arrays:

# Generalized lotka-volterra model:
gen <- odin::odin({
  deriv(y[]) <- r[i] * y[i] * (1 - sum(ay[i, ]))
  initial(y[]) <- y0[i]

  y0[] <- user()
  r[] <- user()
  a[, ] <- user()
  ay[, ] <- a[i, j] * y[j]

  dim(r) <- user()
  n_spp <- length(r)

  dim(y) <- n_spp
  dim(y0) <- n_spp
  dim(a) <- c(n_spp, n_spp)
  dim(ay) <- c(n_spp, n_spp)

  # WHat is this config thing?
  config(base) <- "lv4"
})

pars <- list(r = c(1.00, 0.72, 1.53, 1.27),
             a = rbind(c(1.00, 1.09, 1.52, 0.00),
                       c(0.00, 1.00, 0.44, 1.36),
                       c(2.33, 0.00, 1.00, 0.47),
                       c(1.21, 0.51, 0.35, 1.00)),
             y0 = c(0.3013, 0.4586, 0.1307, 0.3557))


mod <- gen$new(user = pars)

t <- seq(0, 2000, length.out = 10001)
y <- mod$run(t)
pairs(y[, -1], panel = lines, col = "#00000055", lwd = 0.2)


# Interpolating functions:
# Use this to drive model inputs with data.
# Step, linear and cubic.

# interpolate()
# t is a reserved variable.

# Flux Example
# As an example, here is the flux model from the deSolve compiledCode vignette (see vignette("compiledCode"), section 6).

# dC/dt = flux(t) - k * C

flux_model <- odin::odin(workdir = "cpp/",{
  deriv(C) <- flux - kk * C
  initial(C) <- C0
  # Can also use "constant" or "spline" interpolation
  flux <- interpolate(flux_t, flux_y, "spline")
  C0 <- user()
  kk <- user()
  output(deposition) <- kk * C
  ## Fair bit of boilerplate here that may be removed in future
  ## versions:
  #
  flux_t[] <- user()
  flux_y[] <- user()
  dim(flux_t) <- user()
  dim(flux_y) <- user()
})

# Time-series inputs

flux_t <- c(1, 11, 21, 41, 73, 83, 93, 103, 113, 123, 133, 143, 153,
            163, 173, 183, 194, 204, 214, 224, 234, 244, 254, 264,
            274, 284, 294, 304, 315, 325, 335, 345, 355, 365)

flux_y <- c(0.654, 0.167, 0.06, 0.07, 0.277, 0.186, 0.14, 0.255, 0.231,
            0.309, 1.127, 1.923, 1.091, 1.001, 1.691, 1.404, 1.226, 0.767,
            0.893, 0.737, 0.772, 0.726, 0.624, 0.439, 0.168, 0.28, 0.202,
            0.193, 0.286, 0.599, 1.889, 0.996, 0.681, 1.135)

plot(flux_t, flux_y, type = "l", ylab = "Flux", xlab = "Time")

k <- 0.01
C0 <- mean(approx(flux_t, flux_y, xout = 1:365)$y) / k

mod <- flux_model$new(kk = k, C0 = C0, flux_t = flux_t, flux_y = flux_y)

t <- seq(1, 365)
y <- mod$run(t, tcrit = 365)

plot(flux_t, flux_y, type = "l", col = "red", ylab = "Flux & deposition")
lines(t, y[, 3], col = "blue")



# List of parameters
coef(flux_model)

# Sending model to JSON
model_ir <- odin_ir(flux_model)

# Can you run from the text alone?
model_ir <- structure("{\"version\":\"1.4.5\",\"config\":{\"base\":\"odin\",\"include\":null,\"custom\":null},\"meta\":{\"internal\":\"internal\",\"user\":\"user\",\"state\":\"state\",\"result\":\"dstatedt\",\"output\":\"output\",\"time\":\"t\",\"initial_time\":\"initial_t\"},\"features\":{\"continuous\":true,\"discrete\":false,\"mixed\":false,\"has_array\":true,\"has_output\":true,\"has_user\":true,\"has_delay\":false,\"has_interpolate\":true,\"has_stochastic\":false,\"has_include\":false,\"has_debug\":false,\"initial_time_dependent\":false},\"data\":{\"elements\":[{\"name\":\"C\",\"location\":\"variable\",\"storage_type\":\"double\",\"rank\":0,\"dimnames\":null,\"stage\":\"time\"},{\"name\":\"C0\",\"location\":\"internal\",\"storage_type\":\"double\",\"rank\":0,\"dimnames\":null,\"stage\":\"user\"},{\"name\":\"deposition\",\"location\":\"output\",\"storage_type\":\"double\",\"rank\":0,\"dimnames\":null,\"stage\":\"time\"},{\"name\":\"dim_flux_t\",\"location\":\"internal\",\"storage_type\":\"int\",\"rank\":0,\"dimnames\":null,\"stage\":\"null\"},{\"name\":\"dim_flux_y\",\"location\":\"internal\",\"storage_type\":\"int\",\"rank\":0,\"dimnames\":null,\"stage\":\"null\"},{\"name\":\"flux\",\"location\":\"transient\",\"storage_type\":\"double\",\"rank\":0,\"dimnames\":null,\"stage\":\"time\"},{\"name\":\"flux_t\",\"location\":\"internal\",\"storage_type\":\"double\",\"rank\":1,\"dimnames\":{\"length\":\"dim_flux_t\",\"dim\":[],\"mult\":[]},\"stage\":\"user\"},{\"name\":\"flux_y\",\"location\":\"internal\",\"storage_type\":\"double\",\"rank\":1,\"dimnames\":{\"length\":\"dim_flux_y\",\"dim\":[],\"mult\":[]},\"stage\":\"user\"},{\"name\":\"initial_C\",\"location\":\"internal\",\"storage_type\":\"double\",\"rank\":0,\"dimnames\":null,\"stage\":\"user\"},{\"name\":\"interpolate_flux\",\"location\":\"internal\",\"storage_type\":\"interpolate_data\",\"rank\":0,\"dimnames\":null,\"stage\":\"user\"},{\"name\":\"kk\",\"location\":\"internal\",\"storage_type\":\"double\",\"rank\":0,\"dimnames\":null,\"stage\":\"user\"}],\"variable\":{\"length\":1,\"contents\":[{\"name\":\"C\",\"offset\":0,\"initial\":\"initial_C\"}]},\"output\":{\"length\":1,\"contents\":[{\"name\":\"deposition\",\"offset\":0}]}},\"equations\":[{\"name\":\"C0\",\"type\":\"user\",\"source\":[4],\"depends\":null,\"lhs\":\"C0\",\"user\":{\"default\":null,\"dim\":false,\"min\":null,\"max\":null}},{\"name\":\"deriv_C\",\"type\":\"expression_scalar\",\"source\":[1],\"depends\":{\"functions\":[\"-\",\"*\"],\"variables\":[\"flux\",\"kk\",\"C\"]},\"lhs\":\"C\",\"rhs\":{\"value\":[\"-\",\"flux\",[\"*\",\"kk\",\"C\"]]}},{\"name\":\"flux\",\"type\":\"interpolate\",\"source\":[3],\"depends\":{\"functions\":[],\"variables\":[\"t\",\"interpolate_flux\"]},\"lhs\":\"flux\",\"interpolate\":\"interpolate_flux\"},{\"name\":\"flux_t\",\"type\":\"user\",\"source\":[7],\"depends\":null,\"lhs\":\"flux_t\",\"user\":{\"default\":null,\"dim\":true,\"min\":null,\"max\":null}},{\"name\":\"flux_y\",\"type\":\"user\",\"source\":[8],\"depends\":null,\"lhs\":\"flux_y\",\"user\":{\"default\":null,\"dim\":true,\"min\":null,\"max\":null}},{\"name\":\"initial_C\",\"type\":\"expression_scalar\",\"source\":[2],\"depends\":{\"functions\":[],\"variables\":[\"C0\"]},\"lhs\":\"initial_C\",\"rhs\":{\"value\":\"C0\"}},{\"name\":\"interpolate_flux\",\"type\":\"alloc_interpolate\",\"source\":[3],\"depends\":{\"functions\":[],\"variables\":[\"flux_t\",\"flux_y\"]},\"lhs\":\"interpolate_flux\",\"interpolate\":{\"t\":\"flux_t\",\"y\":\"flux_y\",\"type\":\"spline\",\"equation\":\"flux\"}},{\"name\":\"kk\",\"type\":\"user\",\"source\":[5],\"depends\":null,\"lhs\":\"kk\",\"user\":{\"default\":null,\"dim\":false,\"min\":null,\"max\":null}},{\"name\":\"output_deposition\",\"type\":\"expression_scalar\",\"source\":[6],\"depends\":{\"functions\":[\"*\"],\"variables\":[\"kk\",\"C\"]},\"lhs\":\"deposition\",\"rhs\":{\"value\":[\"*\",\"kk\",\"C\"]}}],\"debug\":[],\"components\":{\"create\":{\"variables\":[],\"equations\":[]},\"user\":{\"variables\":[],\"equations\":[\"C0\",\"flux_t\",\"flux_y\",\"kk\",\"initial_C\",\"interpolate_flux\"]},\"initial\":{\"variables\":[],\"equations\":[]},\"rhs\":{\"variables\":[\"C\"],\"equations\":[\"flux\",\"deriv_C\"]},\"update_stochastic\":{\"variables\":[],\"equations\":[]},\"output\":{\"variables\":[\"C\"],\"equations\":[\"output_deposition\"]}},\"user\":[{\"name\":\"C0\",\"has_default\":false},{\"name\":\"flux_t\",\"has_default\":false},{\"name\":\"flux_y\",\"has_default\":false},{\"name\":\"kk\",\"has_default\":false}],\"interpolate\":{\"min\":[\"flux_t\"],\"max\":[\"flux_t\"],\"critical\":[]},\"source\":[\"deriv(C) <- flux - kk * C\",\"initial(C) <- C0\",\"flux <- interpolate(flux_t, flux_y, \\\"spline\\\")\",\"C0 <- user()\",\"kk <- user()\",\"output(deposition) <- kk * C\",\"flux_t[] <- user()\",\"flux_y[] <- user()\",\"dim(flux_t) <- user()\",\"dim(flux_y) <- user()\"]}", class = "json")

dput(model_ir)

# Building model from json:
re <- odin_build(model_ir)

model <- re$model$new(kk = k, C0 = C0, flux_t = flux_t, flux_y = flux_y)

t <- seq(1, 365)
y <- mod$run(t, tcrit = 365)

plot(flux_t, flux_y, type = "l", col = "red", ylab = "Flux & deposition")
lines(t, y[, 3], col = "blue")




# Build Odin model:

