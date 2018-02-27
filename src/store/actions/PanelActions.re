open Types;

type t =
  | SetPath(string)
  | SetItemFocus(FileInfo.t)
  | SetPanelItemsPerColumnCount(int)
  | SelectItems(FileInfo.t);