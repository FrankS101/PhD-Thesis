New Techniques and Algorithms for Multiobjective and Lexicographic Goal-Based Shortest Path Problems
============================

PhD dissertation, Francisco J. Pulido, defended on July 7, 2015, in the Department of Computer Science and Programming Languages, University of Málaga (Spain). Supervised by Dr. Lawrence Mandow.

_Mirrors:_ 
- https://riuma.uma.es/xmlui/bitstream/handle/10630/10204/TD_Pulido_Arrebola.pdf?sequence=3&isAllowed=y
- http://www.lcc.uma.es/~francis/

## Why this can be useful to you?

Become a PhD in Computer Science (or any other field) is never an easy path. I believe Open Research will make science more transparent and reproducible. Have you ever seen a new algorithm or technique and wanted to compare it to your own approach? Did you have to implement that algorithm yourself? and what about LaTeX? You will end up loving it but it will not be an easy path either.

The aim of publishing the source code, LaTeX sources of my dissertation, and even the presentation I used to defend my PhD has a twofold purpose: on one hand, provide an example of a finished PhD to those who can benefit from it, and on the other hand, pave the way for further improvements to the papers that have resulted in this thesis. Having already the source code may help you to extend, improve, criticize or learn from this work.

## License and contact

_License:_ Creative Commons Attribution-NonCommercial-NoDerivs License

_Contact:_ Francisco J. Pulido (@FrankS101, <francis@lcc.uma.es>)

Please cite using the following BibTex entry:

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
# Abstract
Shortest Path Problems (SPP) are one of the most extensively studied problems in the fields of Artificial Intelligence (AI) and Operations Research (OR). It consists in finding the shortest path between two given nodes in a graph such that the sum of the weights of its constituent arcs is minimized. However, real life problems frequently involve the consideration of multiple, and often conflicting, criteria. When multiple objectives must be simultaneously optimized, the concept of a single optimal solution is no longer valid. Instead, a set of efficient or Pareto-optimal solutions define the optimal trade-off between the objectives under consideration.  

The Multicriteria Search Problem (MSP), or Multiobjective Shortest Path Problem, is the natural extension to the SPP when more than one criterion are considered. The MSP is computationally harder than the single objective one. The number of label expansions can grow exponentially with solution depth, even for the two objective case (Hansen, 1980). However, with the assumption of bounded integer costs and a fixed number of objectives the problem becomes tractable for polynomially sized graphs (e.g. see (Mandow & Pérez de la Cruz, 2009; Müller-Hannemann & Weihe, 2006)).

A wide variety of practical application in different fields can be identified for the MSP, like robot path planning (Wu et al., 2011), hazardous material transportation (Caramia et al., 2010), route planning (Jozefowiez et al., 2008), optimization of public transportation (Raith, 2009), QoS in networks (Craveirinha et al., 2009), or routing in  multimedia networks (Climaco et al., 2003).

Goal programming is one of the most successful Multicriteria Decision Making (MCDM) techniques used in Multicriteria Optimization. In this thesis we explore one of its variants in the MSP. Thus, we aim to solve the Multicriteria Search Problem with lexicographic goal-based preferences. To do so, we build on previous work on algorithm NAMOA\*, a successful extension of the A\* algorithm to the multiobjective case. More precisely, we provide a new algorithm called LEXGO\*, an exact label-setting algorithm that returns the subset of Pareto optimal paths that satisfy a set of lexicographic goals, or the subset that minimizes deviation from goals if these cannot be fully satisfied. Moreover, LEXGO\* is proved to be admissible and expands only a subset of the labels expanded by an optimal algorithm like NAMOA*, which performs a full Multiobjective Search.

Since time rather than memory is the limiting factor in the performance of multicriteria search algorithms, we also propose a new technique called _t-discarding_ to speed up dominance checks in the process of discarding new alternatives during the search. The application of _t-discarding_ to the algorithms studied previously, NAMOA\* and LEXGO\*, leads to the introduction of two new time-efficient algorithms named NAMOA\*<sub>dr</sub> and LEXGO\*<sub>dr</sub>, respectively.

All the algorithmic alternatives are tested in two scenarios, random grids and realistic road maps problems. The experimental evaluation shows the effectiveness of LEXGO\* in both benchmarks, as well as the dramatic reductions of time requirements experienced by the _t-discarding_ versions of the algorithms, with respect to the ones with traditional pruning.
