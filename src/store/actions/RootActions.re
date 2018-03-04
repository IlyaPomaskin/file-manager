open Types;

type t =
  | SetPanelFocus(PanelSide.t)
  | CopyFiles(list(string), string)
  | MoveFiles(list(string), string);