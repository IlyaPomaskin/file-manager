open Rationale.Option;

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
        dispatch(PanelAction(state.focused, SelectItems(item)));
        dispatch(PanelAction(state.focused, SetItemFocus(item)));
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
    <$> (item => dispatch(PanelAction(state.focused, SetItemFocus(item))))
    |> ignore
  | (_, "Enter") =>
    dispatch(PanelAction(state.focused, SetPath(panel.focusedItem.name)))
  | (_, "Backspace") => dispatch(PanelAction(state.focused, SetPath("..")))
  | (_, "Tab") =>
    dispatch(
      RootAction(
        SetPanelFocus(
          state.focused === Types.PanelSide.Left ?
            Types.PanelSide.Right : Types.PanelSide.Left
        )
      )
    )
  | _ => ()
  };
};

Webapi.Dom.Document.addKeyDownEventListener(
  keyPressHandler,
  Webapi.Dom.document
);