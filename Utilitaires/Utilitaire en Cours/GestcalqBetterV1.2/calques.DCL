calques_dialog : dialog {
  label = "Gestion des calques";
  width = 50;
  height = 22;

  : column {
    : boxed_column {
      label = "Liste des calques";
      : list_box {
        key = "layers_list";
        height = 10;
        width = 40;
        multiple_select = true;
      }
    }

    : boxed_row {
      label = "Préfixe de renommage";
      : edit_box {
        key = "prefixe";
        label = "Préfixe :";
        width = 20;
      }
      : button {
        key = "btn_prefixe";
        label = "Renommer";
        is_default = true;
      }
    }

    : boxed_row {
      label = "Filtrer par couleur ACI";
      : edit_box {
        key = "filtre_couleur";
        label = "ACI :";
        width = 5;
      }
      : button {
        key = "btn_filtrer";
        label = "Geler par couleur";
      }
    }

    : button {
      key = "btn_degele";
      label = "Dégeler tous les calques";
    }

    : spacer {}

    : button {
      key = "btn_deplacer";
      label = "Déplacer solides 3D";
    }

    : button {
      key = "btn_restaurer";
      label = "Restaurer calques des solides 3D";
    }

    : spacer {}

    : button {
      key = "cancel";
      label = "Fermer";
      is_cancel = true;
    }
  }
}
