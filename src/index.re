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
  let state = Reductive.Store.getState(State.store);
  let dispatch = Reductive.Store.dispatch(State.store);
  let keyName = Webapi.Dom.KeyboardEvent.key(event);
  let shiftKey = Webapi.Dom.KeyboardEvent.shiftKey(event);
  let panel = Rationale.Lens.view(State.getLensBySide(state.focused), state);
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
        dispatch(SelectItems(state.focused, item));
        dispatch(SetItemFocus(state.focused, item));
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
    <$> (item => dispatch(SetItemFocus(state.focused, item)))
    |> ignore
  | (_, "Enter") => dispatch(SetPath(state.focused, panel.focusedItem.name))
  | (_, "Backspace") => dispatch(SetPath(state.focused, ".."))
  | (_, "Tab") =>
    dispatch(
      SetPanelFocus(
        state.focused === Types.PanelSide.Left ?
          Types.PanelSide.Right : Types.PanelSide.Left
      )
    )
  | _ => ()
  };
};

Webapi.Dom.Document.addKeyDownEventListener(
  keyPressHandler,
  Webapi.Dom.document
);