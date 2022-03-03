
# Library
install.packages("plotly")
library(plotly)

###############
## Pair Pool ##
###############

pair_pool <- function(w, x_start, x_end, color, first_plot = T){
  if (length(w)!=2){
    print("Input must have length of two for pair pool! ")
  }
  
  pc1 = seq(x_start, x_end, by=0.01)
  pc2 = rep(1, length(pc1))
  
  pv = (1+pc1)^w[1]*(1+pc2)^w[2]
  av = (1+pc1)*w[1]+(1+pc2)*w[2]
  il = pv/av-1
  
  if (first_plot){
    plot(pc1, il, type = 'l', col = color, ylab = "Impermanent Loss", 
         xlab = "Relative price change (current vs. initial)")
    
  }else{
    lines(pc1, il, type = 'l', col = color)
  }

}


pair_pool(w = c(0.5, 0.5), x_start = -1, x_end = 5, color = 'blue', first_plot = T)
pair_pool(w = c(0.2, 0.8), x_start = -1, x_end = 5, color = 'green', first_plot = F)
pair_pool(w = c(0.05, 0.95), x_start = -1, x_end = 5, color = 'orange', first_plot = F)
legend("bottomright", legend=c("50%-50%", "20%-80%", "5%-95%"), col = c('blue', 'green', 'orange'), lty = 1:1, cex=0.8)

##############
## Tri-pool ##
##############
set.seed(111)
w = c(0.333333,0.333333,0.333333)
pc1 = seq(-1, 5, by=0.01)
pc2 = rev(round(runif(length(pc1), 0, 1)*pc1,2))
pc3 = rep(1, length(pc1))

pv = (1+pc1)^w[1]*(1+pc2)^w[2]*(1+pc3)^w[3]
av = (1+pc1)*w[1]+(1+pc2)*w[2]+(1+pc3)*w[3]
il = round(pv/av-1,2)

fig <- plot_ly(x = ~pc1, y = ~pc2, z = ~il, type = 'mesh3d') 
fig <- fig %>% layout(scene = list(xaxis=list(title = "x: Token 1 price change"),
                                   yaxis=list(title = "y: Token 2 price change"),
                                   zaxis=list(title = "z: Impermanent Loss")))

fig

