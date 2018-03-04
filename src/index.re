open Rationale.Option;

open Types;

open Types.FileInfo;

ReactDOMRe.renderToElementWithId(<App />, "index");

let arrowKeyNameToOffset = keyName =>
  switch keyName {
  | "ArrowLeft" => Types.Offset.Left
  | "ArrowRight" => Types.Offset.Right
  | "ArrowUp" => Types.Offset.Up
  | "ArrowDown" => Types.Offset.Down
  | _ => Types.Offset.Down
  };

let keyPressHandler = event => {
  let state = Reductive.Store.getState(Store.store);
  let dispatch = Reductive.Store.dispatch(Store.store);
  let keyName = Webapi.Dom.KeyboardEvent.key(event);
  let shiftKey = Webapi.Dom.KeyboardEvent.shiftKey(event);
  let panel = Rationale.Lens.view(Store.getLensBySide(state.focused), state);
  switch (shiftKey, keyName) {
  | (true, "ArrowLeft")
  | (true, "ArrowRight")
  | (true, "ArrowUp")
  | (true, "ArrowDown") =>
    keyName
    |> arrowKeyNameToOffset
    |> PanelUtils.getItemByOffset(panel)
    <$> (
      item => {
        dispatch(PanelActions(state.focused, SelectItems(item)));
        dispatch(PanelActions(state.focused, SetItemFocus(item)));
      }
    )
    |> ignore
  | (false, "ArrowLeft")
  | (false, "ArrowRight")
  | (false, "ArrowUp")
  | (false, "ArrowDown") =>
    keyName
    |> arrowKeyNameToOffset
    |> PanelUtils.getItemByOffset(panel)
    <$> (item => dispatch(PanelActions(state.focused, SetItemFocus(item))))
    |> ignore
  | (_, "Enter") =>
    dispatch(PanelActions(state.focused, SetPath(panel.focusedItem.name)))
  | (_, "Backspace") => dispatch(PanelActions(state.focused, SetPath("..")))
  | (_, "Tab") =>
    dispatch(
      RootActions(
        SetPanelFocus(
          state.focused === Types.PanelSide.Left ?
            Types.PanelSide.Right : Types.PanelSide.Left
        )
      )
    )
  | (_, "F5") =>
  | (_, "F5")
  | (_, "F6") =>
    let srcFiles =
      switch panel {
      | {selectedFiles: []} => [panel.focusedItem.fullPath]
      | {focusedItem: {name: ".."}} => []
      | _ => List.map(info => info.fullPath, panel.selectedFiles)
      };
    let dstSide =
      switch state.focused {
      | PanelSide.Left => PanelSide.Right
      | PanelSide.Right => PanelSide.Left
      };
    let dstPanel = Rationale.Lens.view(Store.getLensBySide(dstSide), state);
    let dst = dstPanel.path;
    switch keyName {
    | "F5" => dispatch(RootActions(CopyFiles(srcFiles, dst)))
    | "F6" => dispatch(RootActions(MoveFiles(srcFiles, dst)))
    | _ => ()
    };
    dispatch(PanelActions(dstSide, SetPath(dst)));
  | _ => ()
  };
};

Webapi.Dom.Document.addKeyDownEventListener(
  keyPressHandler,
  Webapi.Dom.document
);