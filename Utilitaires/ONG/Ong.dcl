// =================================================================
//
//  ONG.DCL V3.21
//
//  Copyright (C) Patrick_35
//
// =================================================================

ong : dialog {
  key = "total";
  is_cancel = true;
  allow_accept = true;
  : row {
    : list_box {key = "present"; height = 20; width = 20;multiple_select = true;}
    : boxed_column {
      width = 25;
      label = "Onglets";
      : button {key = "choisir";  label = "Choisir";}
      : button {key = "renommer"; label = "Renommer";}
      : button {key = "effacer";  label = "Effacer";}
      : button {key = "nouveau";  label = "Nouveau";}
      : button {key = "gabarit";  label = "Gabarit";}
      : button {key = "copier";   label = "Copier";}
      spacer;
    }
    : column {
      : boxed_column {
        width = 25;
      label = "Mise en page / Impression";
        : button {key = "page";   label = "Mise en page";}
        : button {key = "tracer"; label = "Imprimer";}
        : toggle {key = "inv"; 	  label = "Inverser impression";}
      }
      : boxed_column {
      label = "Lisps externes";
        : button {key = "oog";	  label = "Organiser Onglets";}
        : button {key = "mim";	  label = "Propager Imprimante";}
        : button {key = "mpl"; 	  label = "Propager Config.Imp";}
      spacer;
      }
    }
  }
  spacer;
  ok_cancel;
//  ok_button;
}

edi : dialog{
  key = texte;
  : row {
    : edit_box {key = select; width = 20; allow_accept = true;}
  }
  spacer;
  ok_cancel;
}