module Offset = {
  type t =
    | Left
    | Right
    | Up
    | Down;
};

module PanelSide = {
  type t =
    | Left
    | Right;
};

module FileInfo = {
  type t = {
    name: string,
    fullPath: string,
    isFile: bool
  };
};

module PanelType = {
  type t = {
    focusedItem: FileInfo.t,
    path: string,
    files: list(FileInfo.t),
    itemsPerColumn: int,
    selectedFiles: list(FileInfo.t)
  };
};