ExporterLongueursDialog : dialog {
    label = "Exporter longueurs par calque";

    : row {
        : label {
            label = "Sélectionnez les calques à exporter :";
        }
    }

    : list_box {
        key = "calques";
        label = "Liste des calques";
        height = 10;
        width = 40;
        multiple_select = true;
    }

    : row {
        : label {
            label = "Nom du fichier CSV (chemin complet) :";
        }
        : edit_box {
            key = "nom_fichier";
            label = "";
            width = 40;
        }
    }

    spacer;

    : row {
        : button {
            key = "ok";
            label = "Exporter";
            is_default = true;
        }
        : button {
            key = "cancel";
            label = "Annuler";
            is_cancel = true;
        }
    }
}
