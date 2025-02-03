#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import re
import os

"""
Usage :
    gen_deps.py file1.f90 file2.F90 ...
    
Ce script génère des lignes de dépendances pour Makefile, de la forme :
    build/file1.o: src/file1.f90 build/module1.mod ...
    build/file2.o: src/file2.F90 ...
    
Il repère les "module <nom>" et "use <nom>" en début de ligne
(éventuellement précédés d'espaces). Il gère la casse (option IGNORECASE),
mais reste très basique (n'analyse pas les lignes scindées, submodule, etc.).
"""

# Expressions régulières pour repérer modules et utilisations
module_regex = re.compile(r'^\s*module\s+(\w+)', re.IGNORECASE)
use_regex    = re.compile(r'^\s*use\s+(\w+)',    re.IGNORECASE)

def source_to_obj(source_path):
    """
    Convertit un chemin de fichier source (ex: src/foo.f90)
    en chemin d'objet correspondant (ex: build/foo.o).
    """
    base = os.path.basename(source_path)        # foo.f90
    root, _ext = os.path.splitext(base)         # foo, .f90
    return os.path.join("build", root + ".o")   # build/foo.o

def module_to_mod_file(module_name):
    """
    Convertit un nom de module (ex: "myMod") en fichier .mod correspondant (ex: build/mymod.mod).
    On met le nom en minuscules pour s'aligner sur le comportement courant de gfortran.
    """
    return os.path.join("build", module_name.lower() + ".mod")

def main(sources):
    # Dictionnaire : module -> fichier source dans lequel il est défini
    module_definitions = {}
    # Dictionnaire : fichier source -> ensemble des modules utilisés
    file_uses = {}

    # 1) Repérage des modules définis
    for f in sources:
        file_uses[f] = set()  # Initialement vide
        # Lecture du contenu
        try:
            with open(f, 'r') as ff:
                for line in ff:
                    # On ignore les lignes commençant par '!' (commentaires)
                    # ou si on veut être strict, on peut faire plus compliqué.
                    m = module_regex.match(line)
                    if m:
                        mod = m.group(1).lower()
                        module_definitions[mod] = f
        except Exception as e:
            # En cas d'erreur d'ouverture/lecture
            sys.stderr.write(f"Erreur de lecture du fichier {f}: {e}\n")
            continue

    # 2) Repérage des "use"
    for f in sources:
        try:
            with open(f, 'r') as ff:
                for line in ff:
                    u = use_regex.match(line)
                    if u:
                        used_module = u.group(1).lower()
                        file_uses[f].add(used_module)
        except Exception as e:
            sys.stderr.write(f"Erreur de lecture du fichier {f}: {e}\n")
            continue

    # 3) Génération des dépendances
    #
    # Pour chaque fichier source :
    #   build/<fichier>.o : <fichier_source> [ + build/<module>.mod pour chaque "use" trouvé ]
    #
    # Même s'il n'y a pas de modules, on génère la dépendance "objet : source".
    for f in sources:
        obj  = source_to_obj(f)
        deps = [f]  # La dépendance de base : l'objet dépend du fichier source
        for used_mod in file_uses[f]:
            if used_mod in module_definitions:
                # On ajoute build/used_mod.mod
                deps.append(module_to_mod_file(used_mod))

        # Format de sortie : build/foo.o : src/foo.f90 build/mymod.mod ...
        print(f"{obj}: {' '.join(deps)}")

if __name__ == "__main__":
    # On récupère la liste des fichiers passés en argument
    all_sources = sys.argv[1:]
    if not all_sources:
        sys.stderr.write("Aucun fichier source spécifié à gen_deps.py\n")
        sys.exit(0)

    main(all_sources)
