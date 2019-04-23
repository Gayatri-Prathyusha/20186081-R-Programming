edges = read.csv("edges.csv")

users = read.csv("users.csv")

str(users)

str(edges) 

table(users$locale, users$school)
table(users$gender, users$school)
install.packages("igraph")

library(igraph)
g = graph.data.frame(edges, FALSE, users)
plot(g, vertex.size=5, vertex.label=NA)
table(degree(g) >= 10)
V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)
 summary(degree(g))
V(g)$color = "black"

V(g)$color[V(g)$gender == "A"] = "red"

V(g)$color[V(g)$gender == "B"] = "gray"

plot(g, vertex.label=NA)
V(g)$color = "black"

V(g)$color[V(g)$school == "A"] = "red"

V(g)$color[V(g)$school == "AB"] = "gray"

plot(g, vertex.label=NA)
V(g)$color = "black"

V(g)$color[V(g)$locale == "A"] = "red"

V(g)$color[V(g)$locale == "B"] = "gray"

plot(g, vertex.label=NA)
rglplot(g, vertex.label=NA)
plot(g, edge.width=2, vertex.label=NA)

