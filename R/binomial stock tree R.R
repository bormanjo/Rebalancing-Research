library(data.tree)



up.factor <- 2
down.factor <- 0.5

P_0 <- 16
n_steps <- 3
prob.up <- 0.5


t_0 <- Node$new(name = 0, )

root <- CreateRegularTree(height = 3, branchingFactor = 2)
root[["1.1"]]
CreateRegularTree

new_tree <- function (height = 2, parent = Node$new("x")){
  if (height <= 1) 
    return()
  for (i in c("up", "down")) {
    child <- parent$AddChild(paste(parent$name, i, sep = "_"), 
                             check = FALSE)
    new_tree(height - 1, child)
  }
  return(parent)
}

name_to_prob <- function(name, prob_up){
  name <- gsub("up", as.character(prob_up), name)
  name <- gsub("down", as.character(1 - prob_up), name)
  name <- gsub("x", "1", name)
  prod(as.numeric(unlist(strsplit(name, "_", fixed = T))))
}

name_to_state <- function(name, P_0, up.factor, down.factor){
  name <- gsub("up", as.character(up.factor), name)
  name <- gsub("down", as.character(down.factor), name)
  name <- gsub("x", as.character(P_0), name)
  prod(as.numeric(unlist(strsplit(name, "_", fixed = T))))
}

name_to_height <- function(name){
  length(unlist(strsplit(name, "_", fixed = T)))
}

set_cumProb <- function(node){
  if(is.null(node$parent)){
    node$cumProb <- node$prob
  } else {
    node$cumProb <- node$prob * node$parent$prob
  }
  
  children <- node$children
  
  if(!is.null(children)){
    for(i in 1:length(children)){
      set_cumProb(children[[i]])
    } 
  }
}

node_to_ExpVal <- function(node){
  node$EV <- (node$Get("prob", filterFun = isLeaf) %*% node$Get("state", filterFun = isLeaf)) / node$prob
}



name_to_prob(a$name, 0.5)
name_to_state(a$name, P_0, up.factor, down.factor)

name_to_prob(a$`x_up`$name, 0.5)
name_to_state(a$`x_up`$name, P_0, up.factor, down.factor)

a$Do(
  function(node){
    node$state <- name_to_state(node$name, P_0, up.factor, down.factor)
    node$prob <- name_to_prob(node$name, prob_up)
    node$EV <- node_to_ExpVal(node)
  }, traversal = "post-order"
)

print(a, "state", "prob", "EV", "height")

node_to_ExpVal(a, a$height)


binomial_stock_price <- function(P_0, n_steps, prob.up, factor.up, factor.down){
  a <- new_tree(height = n_steps)
  a$Do(
    function(node){
      node$state <- name_to_state(node$name, P_0, up.factor, down.factor)
      node$prob <- name_to_prob(node$name, prob_up)
      node$EV <- node_to_ExpVal(node)
    }, traversal = "post-order"
  )
  return(a)
}

system.time(
  root <- binomial_stock_price(16, 10, 0.3, 2, 0.5)
)

system.time(
  root <- binomial_stock_price(16, 20, 0.3, 2, 0.5)
)

system.time(
  root <- binomial_stock_price(16, 40, 0.3, 2, 0.5)
)

system.time(
  root <- binomial_stock_price(16, 80, 0.3, 2, 0.5)
)

system.time(
  root <- binomial_stock_price(16, 160, 0.3, 2, 0.5)
)

system.time(
  root <- binomial_stock_price(16, 240, 0.3, 2, 0.5)
)
print(root, "state", "prob", "EV", "height")





