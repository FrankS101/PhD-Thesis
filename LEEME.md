New Techniques and Algorithms for Multiobjective and Lexicographic Goal-Based Shortest Path Problems
============================

Tesis Doctoral, Francisco J. Pulido, defendida en la Universidad de Málaga el 7 de Julio de 2015.

_Enlaces:_ 
- http://riuma.uma.es/xmlui/handle/10630/10204
- http://www.lcc.uma.es/~francis/

_Licencia:_ Creative Commons Attribution-NonCommercial-NoDerivs

_Contacto:_ Francisco J. Pulido (@FrankS101, <francis@lcc.uma.es>)

Citar usando el siguiente BibTex:

```
@phdthesis{pulido2015phd,
  title  = {New Techniques and Algorithms for Multiobjective and Lexicographic Goal-Based Shortest Path Problems},
  author = {Pulido, Francisco-Javier},
  school = {University of M\'{a}laga, Spain},
  year   = 2015,
  Url    = {http://riuma.uma.es/xmlui/handle/10630/10204}
}
```

---
# Resumen

El problema del camino más corto (SPP) es uno de los más antiguos y extensamente estudiados en los campos de Inteligencia Artificial (AI) e Investigación Operativa (OR), el cual consiste en encontrar el camino entre dos nodos de un grafo tal que la suma de los pesos de los arcos que lo componen sea mínima. Sin embargo, los problemas en la vida real suelen implicar múltiples, y normalmente contradictorios, criterios. Cuando múltiples objetivos deben ser optimizados simultáneamente el concepto de una sola solución óptima pierde su validez, en su lugar, un conjunto de soluciones eficientes o Pareto-óptimas definen el equilibrio óptimo entre los objetivos bajo consideración.

El problema de búsqueda multicriterio es la extensión natural del problema del camino más corto cuando se consideran múltiples criterios. El problema de búsqueda multicriterio es computacionalmente más complejo que el que involucra un solo criterio. El número de expansiones de etiquetas puede crecer exponencialmente con la profundidad de la solución, incluso para el caso de dos objetivos (Hansen, 1980). Con el supuesto de costes enteros acotados y un número fijo de objetivos el problema se convierte en tratable para grafos de tamaño polinomial (por ejemplo, véase (Mandow & Pérez de la Cruz, 2009, Müller-Hannemann & Weihe, 2006)).

Un gran variedad de aplicaciones prácticas en diferentes campos pueden ser abordadas como problemas de búsqueda multicriterio, como la planificación de la trayectoria de robots (Wu et al., 2011), el transporte de materiales peligrosos (Caramia et al., 2010), la planificación de rutas en diferentes contextos (Jozefowiez et al., 2008), el
transporte público (Raith, 2009) o la calidad del servicio en redes (Craveirinha et al., 2009).

La programación por metas es una de las técnicas de decisión multicriterio más exitosas utilizadas en la optimización de metas multiobjetivo. En esta tesis aplicamos una de sus variantes al problema de búsqueda multicriterio. Así, nuestro objetivo es resolver el problema de búsqueda multicriterio con preferencias lexicográficas basadas en metas. Para ello, proponemos un nuevo algoritmo llamado LEXGO\*, un algoritmo exacto de etiquetado que devuelve el subconjunto de caminos óptimos de Pareto que satisfacen un conjunto de metas lexicográficas, o el subconjunto de mínima desviación con respecto a las metas si estas no se pueden satisfacer completamente. Adicionalmente, se demuestran la admisibilidad de LEXGO\* y la propiedad de expandir sólo un subconjunto de las etiquetas expandidas por un algoritmo de búsqueda completa multicriterio.

Puesto que los requisitos temporales en lugar de los espaciales son el factor limitante para el rendimiento de los algoritmos de búsqueda multicriterio, proponemos una nueva técnica, llamada _t-discarding_, para disminuir el número y dimensionalidad de las comprobaciones de dominancia durante la búsqueda. La aplicación del _t-discarding_ a los algoritmos previamente estudiados, NAMOA\* y LEXGO\* , da lugar a dos nuevos algoritmos eficientes en tiempo, NAMOA\*<sub>dr</sub> y LEXGO\*<sub>dr</sub>, respectivamente.

Todas las alternativas algorítmicas han sido testadas en dos escenarios, mallas aleatorias y mapas de carreteras. La evaluación experimental muestra la efectividad de LEXGO\* en ambos bancos de prueba, así como reducciones espectaculares en los requisitos temporales de ambos algoritmos con respecto a sus contrapartidas que utilizan las técnicas de comprobación de dominancia estándar.
