open Rationale;

open Types;

open Types.FileInfo;

module Action = {
  type t =
    | SetPath(string)
    | SetItemFocus(FileInfo.t)
    | SetPanelItemsPerColumnCount(int)
    | SelectItems(FileInfo.t);
};

let reducer = (panel: PanelType.t, action: Action.t) =>
  switch action {
  | SetPath(relativePath) =>
    let nextFiles =
      FsUtils.getFilesList(Node_path.resolve(panel.path, relativePath));
    let nextFocusedItemIndex =
      switch relativePath {
      | ".." =>
        let currentDirectoryName = Node_path.basename(panel.path);
        let idx =
          RList.findIndex(
            info => info.name === currentDirectoryName,
            nextFiles
          );
        Option.default(0, idx);
      | _ => 0
      };
    {
      ...panel,
      path: Node_path.resolve(panel.path, relativePath),
      files: nextFiles,
      focusedItem: List.nth(nextFiles, nextFocusedItemIndex),
      selectedFiles: []
    };
  | SetItemFocus(info) => {...panel, focusedItem: info}
  | SetPanelItemsPerColumnCount(itemsPerColumn) => {...panel, itemsPerColumn}
  | SelectItems(toFile) =>
    let fromIndex = RList.indexOf(panel.focusedItem, panel.files);
    let toIndex = RList.indexOf(toFile, panel.files);
    let indeces =
      if (fromIndex < toIndex) {
        (fromIndex, toIndex);
      } else {
        (toIndex, fromIndex);
      };
    let selectedFiles =
      switch indeces {
      | (Some(fromIndex), Some(toIndex)) =>
        panel.files
        |> Rationale.RList.slice(fromIndex, toIndex - fromIndex)
        |> Rationale.RList.concat(panel.selectedFiles)
      | _ => panel.selectedFiles
      };
    {...panel, selectedFiles};
  };