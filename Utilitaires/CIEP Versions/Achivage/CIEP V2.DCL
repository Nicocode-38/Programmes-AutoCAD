ciep_dialog : dialog {
    label = "Sélection des présentations";
    : boxed_column {
        label = "Choisissez les layouts où copier les objets :";
        : list_box {
            key = "layout_list";
            height = 15;
            width = 40;
            multiple_select = true;
        }
    }
    spacer;
    : row {
        : button {
            key = "accept";
            label = "Valider";
            is_default = true;
        }
        : button {
            key = "cancel";
            label = "Annuler";
            is_cancel = true;
        }
    }
}
