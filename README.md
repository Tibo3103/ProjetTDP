PROJET TYPE DE DONNEES ET PREUVES :
BIBANG BI ASSEKO - RAILLIA

Ce projet propose une partie de base de données orientée graphe en ocaml.

Nous n'avons pas ajouté de bibliothèque supplémentaire à celles étant présentes de base.

Pour exécuter notre projet, il suffit d'exécuter le fichier setup.sh afin de mettre en place opam et dune, de lancer un dune build, puis de lancer la commande de lancement des tests:

- sh setup.sh
- dune build
- dune exec Proj_GraphDB f test/tiny.q

✅ Fonctionnalités implémentées

- ✔️ Parsing de programmes MiniGQL (`parser.mly`)
- ✔️ Traduction des `pattern` complexes en instructions élémentaires (`instr.ml`)
- ✔️ Début de vérification des types (`typing.ml`)
- ✔️ Structure du graphe avec nœuds et relations typés (`graphstruct.ml`)
- ✔️ Requête `create` fonctionnelle sur les nœuds
- ✔️ Vérification des types déclarés (`check_graph_types`)

⚠️ Fonctionnalités partiellement ou non implémentées

- ❌ Les instructions `match`, `where`, `set`, `return` ne sont pas encore complètement vérifiées au niveau du typage
- ❌ Le typage des expressions (`tp_expr`) est partiel : tous les `Const`, `AttribAcc`, `BinOp` sont considérés `IntT` par défaut
- ❌ L'évaluation (`sem.ml`) n'a pas encore été testée en bout-en-bout
- ❌ Pas de tests `.q` robustes ou vérifiés par `dune runtest`

##  Algorithmes ou choix notables

- Utilisation d’un environnement typé (`bindings`) qui évolue à chaque instruction
- Traduction récursive des `pattern` en instructions élémentaires
- Respect du typage structurel des graphes et relations

##  Limitations actuelles

- Pas de typage réel sur les expressions complexes
- Impossible pour l’instant d’exécuter un programme MiniGQL complet
- Pas encore de visualisation générée (`graph.pdf`, `table.pdf`)


En réalisant ce projet, nous avons rencontré beaucoup de problèmes techniques et de problèmes de compréhension, donc nous avons utilisé l'IA Chat GPT afin de nous débloquer et de nous éclairer sur le fonctionnement du code. nous avons également beaucoup travaillé avec le groupe de Benoît CONNE et de Damien CARRIE, qui nous ont aidé pour beaucoup de nos problèmes.
