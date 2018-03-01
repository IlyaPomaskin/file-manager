open Types;

type t =
  | SetPanelFocus(PanelSide.t)
  | CopyFiles(list(string), string);