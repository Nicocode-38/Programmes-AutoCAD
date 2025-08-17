calques_dialog : dialog {
  label = "Gestionnaire de calques Pro";
  : column {
    : boxed_column {
      label = "Actions";
      : button {
        key = "btn_suppr_vides";
        label = " Supprimer calques vides";
      }
    }
    : boxed_column {
      label = " Filtrer les calques";
      : row {
        : edit_box {
          key = "filtre_nom";
          label = "Nom contient :";
          width = 20;
        }
        : button {
          key = "btn_filtrer_nom";
          label = "Filtrer";
        }
      }
      : row {
        : edit_box {
          key = "filtre_couleur";
          label = "Couleur ACI :";
          width = 5;
        }
        : button {
          key = "btn_filtrer_couleur";
          label = "Filtrer";
        }
      }
      : button {
        key = "btn_reset";
        label = "Réinitialiser les filtres";
      }
    }

        : boxed_column {
          label = "Liste des calques";
          : list_box {
            key = "layers_list";
            height = 12;
            width = 40;
            multiple_select = true;
          }
        }

        : boxed_column {
          label = " Renommer les calques";
          : row {
            : edit_box {
              key = "prefixe";
              label = "Préfixe :";
              width = 20;
            }
            : button {
              key = "btn_renommer";
              label = "Renommer sélection";
            }
          }
        }

        : boxed_column {
          label = "Solides 3D";
          : button {
            key = "btn_deplacer_solides";
            label = " Déplacer solides 3D";
          }
          : button {
            key = "btn_restaurer";
            label = " Restaurer calques d'origine";
          }
        }
: boxed_column {
  label = " Créer calques numérotés";
  : row {
    : edit_box {
      key = "prefixe_creation";
      label = "Préfixe :";
      width = 15;
    }
    : edit_box {
      key = "nb_calques";
      label = "Nombre :";
      width = 5;
    }
    : edit_box {
      key = "couleur_creation";
      label = "Couleur ACI :";
      width = 5;
    }
    : button {
        key = "btn_creer_num";
        label = "Créer";
        is_default = true;
      }
    }
  }
}
    : row {
      : spacer { width = 1; }
      : button {
        key = "cancel";
        label = "Fermer";
        is_cancel = true;
      }
    }
  }
