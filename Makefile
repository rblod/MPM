# ============================================================================
# Paramètres de compilation
# ============================================================================
FC = mpif90
# -J build => place les .mod dans build/
# -I build => recherche les .mod dans build/
FFLAGS = -O2 -Wall -J build -I build  -fcoarray=single -I/opt/homebrew/Cellar/netcdf-fortran/4.6.1_1/include -I/opt/homebrew/Cellar/netcdf-fortran/4.6.1_1/include

# ============================================================================
# Répertoires et noms
# ============================================================================
SRC_DIR   = src
BUILD_DIR = build
EXEC      = shallow_water

# ============================================================================
# Détection automatique des sources .f90 et .F90
# ============================================================================
SOURCES_F90      = $(wildcard $(SRC_DIR)/*.f90)
SOURCES_F90_CPP  = $(wildcard $(SRC_DIR)/*.F90)
SOURCES          = $(SOURCES_F90) $(SOURCES_F90_CPP)

# ============================================================================
# OBJ => liste des .o correspondants
# ============================================================================
# Exemple : src/foo.f90    -> build/foo.o
#           src/bar.F90    -> build/bar.o
OBJ = $(patsubst $(SRC_DIR)/%.f90, $(BUILD_DIR)/%.o, $(SOURCES_F90)) \
      $(patsubst $(SRC_DIR)/%.F90, $(BUILD_DIR)/%.o, $(SOURCES_F90_CPP))

# ============================================================================
# Cible principale
# ============================================================================
all: $(BUILD_DIR)/$(EXEC)

# ============================================================================
# Génération/réutilisation du fichier de dépendances
# ============================================================================
# 1) On force la création du répertoire build/
$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

# 2) On génère le fichier build/.depend
#    On y envoie la sortie de gen_deps.py
$(BUILD_DIR)/.depend: $(SOURCES) | $(BUILD_DIR)
	@echo ">> Génération des dépendances dans $(BUILD_DIR)/.depend"
	@./gen_deps.py $(SOURCES) > $(BUILD_DIR)/.depend

# 3) On inclut build/.depend s'il existe
-include $(BUILD_DIR)/.depend

# ============================================================================
# Règles de compilation
# ============================================================================
# Remarque : 
#  - Pour les .F90, on active le préprocesseur (-cpp).
#  - Pour les .f90, pas de -cpp.
#  - Les .mod sont créés automatiquement dans $(BUILD_DIR) via -J.

$(BUILD_DIR)/%.o: $(SRC_DIR)/%.f90
	$(FC) $(FFLAGS) -c $< -o $@

$(BUILD_DIR)/%.o: $(SRC_DIR)/%.F90
	$(FC) $(FFLAGS) -cpp -c $< -o $@

# ============================================================================
# Édition de liens (création de l'exécutable)
# ============================================================================
$(BUILD_DIR)/$(EXEC): $(OBJ)
	$(FC) $(FFLAGS) -o $@ $^ -L/opt/homebrew/Cellar/netcdf-fortran/4.6.1_1/lib -lnetcdff -L/opt/homebrew/Cellar/netcdf/4.9.2_2/lib -lnetcdf -lnetcdf 

# ============================================================================
# Nettoyage
# ============================================================================
clean:
	rm -rf $(BUILD_DIR)

# ============================================================================
# Cible 'dep' pour forcer la régénération de .depend
# ============================================================================
dep: $(BUILD_DIR)/.depend


$(BUILD_DIR)/%.mod: $(BUILD_DIR)/%.o
	@true

# ============================================================================
# Affichage debug des variables
# ============================================================================
print-sources:
	@echo "SOURCES_F90     = $(SOURCES_F90)"
	@echo "SOURCES_F90_CPP = $(SOURCES_F90_CPP)"
	@echo "SOURCES         = $(SOURCES)"
	@echo "OBJ             = $(OBJ)"

# ============================================================================
# Fin
# ============================================================================
.PHONY: all clean dep print-sources
