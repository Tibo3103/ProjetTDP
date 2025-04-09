#!/bin/bash

# Initialisation d'opam
echo "Initialisation d'opam..."
opam init -y --disable-sandboxing

# Mise à jour de l'environnement shell
echo "Mise à jour de l'environnement shell..."
eval $(opam env)

# Installation des dépendances
echo "Installation de dune..."
opam install -y dune

# Compilation du projet
echo "Compilation du projet avec dune..."
dune build

echo "Installation de menhir..."
opam install -y menhir

echo "Installation de ppx_deriving..."
opam install -y ppx_deriving

echo "Installation de ocamlgraph..."
opam install -y ocamlgraph

echo "Installation de utop..."
opam install -y utop

echo "votre  Environnement OCaml est  prêt pour le projet MiniGQL."
