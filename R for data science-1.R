# R for data science - Chapter

mpg

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

# clase con colores
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

# clase con tamaños (no recomendado al ser una variable continua)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

# shape para transparencias, alpha para formas

# documentación del dataset
?mpg

# facets, un plot por cada clase (para variables discretas)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~ class, nrow = 2)

# hasta pg 16
