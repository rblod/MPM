#!/usr/bin/env python3
"""
generate_dependencies.py

This script scans all Fortran source files with extension .F90 and .f90 in the given
source directory (including the "preprocessed" subdirectory) and extracts module definitions
and "use" statements. It then generates a unified dependency file (deps.mk) containing
dependency rules for all source files in relative paths.

Usage:
    python3 generate_dependencies.py <src_dir> <build_dir>

- <src_dir> is typically "." (the current directory)
- <build_dir> is where the object files will be placed (typically also ".")
"""

import sys, os, re, glob

def extract_defined_modules(file_path):
    """Extracts modules defined in a Fortran source file (returns names in lower case)."""
    modules = set()
    try:
        with open(file_path, 'r') as f:
            for line in f:
                # Remove comments
                line = line.split("!")[0]
                # Ignore "end module" lines
                if re.search(r'^\s*end\s+module', line, re.IGNORECASE):
                    continue
                # Find lines beginning with "module" (but not "module procedure")
                matches = re.findall(r'^\s*module\s+([A-Za-z0-9_]+)\b', line, re.IGNORECASE)
                for mod in matches:
                    if "procedure" not in line.lower():
                        modules.add(mod.lower())
    except Exception as e:
        print(f"Error reading {file_path}: {e}", file=sys.stderr)
    return modules

def extract_used_modules(file_path):
    """Extracts modules used in a Fortran source file (via 'use' statements)."""
    used = set()
    try:
        with open(file_path, 'r') as f:
            for line in f:
                line = line.split("!")[0]
                matches = re.findall(r'^\s*use\s+([A-Za-z0-9_]+)\b', line, re.IGNORECASE)
                for mod in matches:
                    used.add(mod.lower())
    except Exception as e:
        print(f"Error reading {file_path}: {e}", file=sys.stderr)
    return used

def main():
    if len(sys.argv) != 3:
        print("Usage: python3 generate_dependencies.py <src_dir> <build_dir>")
        sys.exit(1)
    src_dir = sys.argv[1]
    build_dir = sys.argv[2]

    # Rechercher tous les fichiers Fortran (.F90 et .f90) dans src_dir et src_dir/preprocessed
    sources = glob.glob(os.path.join(src_dir, "*.F90")) + glob.glob(os.path.join(src_dir, "*.f90"))
    sources += glob.glob(os.path.join(src_dir, "preprocessed", "*.f90"))
    sources = list(set(sources))  # Éliminer les doublons

    # Conserver des chemins relatifs par rapport au répertoire courant
    sources = [os.path.relpath(s, os.getcwd()) for s in sources]

    # Construire un dictionnaire associant chaque module défini à son fichier source (base name)
    mod_to_file = {}
    for src in sources:
        mods = extract_defined_modules(src)
        base = os.path.basename(src)
        for mod in mods:
            # On suppose qu'un module est défini une seule fois
            mod_to_file[mod] = base

    dep_rules = []
    for src in sources:
        base = os.path.splitext(os.path.basename(src))[0]
        obj = os.path.join(build_dir, base + ".o")
        used = extract_used_modules(src)
        deps = []
        for mod in used:
            if mod in mod_to_file and mod_to_file[mod] != os.path.basename(src):
                dep_obj = os.path.join(build_dir, os.path.splitext(mod_to_file[mod])[0] + ".o")
                deps.append(dep_obj)
        rule = f"{obj}: {src}"
        if deps:
            rule += " " + " ".join(sorted(set(deps)))
        dep_rules.append(rule)

    # Écrire toutes les règles de dépendance dans un fichier unique "deps.mk"
    dep_text = "\n".join(dep_rules)
    print(dep_text)
    return 0

if __name__ == "__main__":
    sys.exit(main())
