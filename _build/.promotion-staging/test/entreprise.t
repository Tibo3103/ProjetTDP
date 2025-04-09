  $ dune exec Proj_GraphDB f entreprise.q
  Parsing error in file: entreprise.q on line: 9 column: 15 token: -
  rest: [:emp]-> (ab: E),
     (pierre: P) -[:emp]-> (pp: E)
  set
     marie.nom = "Marie Dubois",    marie.age = 25,
     pierre.nom = "Pierre Dupont", pierre.age = 24,
     ab.nom = "Airbus", ab.pme = false,
     pp.nom = "Petit Pain", pp.pme = true
  create
     (pp) -[:f]-> (ab),
     (marie) -[:emp]-> (ab),
     (pierre) -[:emp]-> (pp),
     (marie) -[:ami]-> (pierre)
  
  match (p: P) -[:emp]-> (e: E)
  where p.age < 25
  
  return p, e
  
  Fatal error: exception Failure("Stopped execution.")
  [2]
