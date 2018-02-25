open Rationale.Option;

ReactDOMRe.renderToElementWithId(<App />, "index");

let arrowKeyNameToOffset = keyName =>
  switch keyName {
  | "ArrowLeft" => State.Offset.Left
  | "ArrowRight" => State.Offset.Right
  | "ArrowUp" => State.Offset.Up
  | "ArrowDown" => State.Offset.Down
  | _ => State.Offset.Down
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
    |> Panel_utils.getItemByOffset(panel)
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
    |> Panel_utils.getItemByOffset(panel)
    <$> (item => dispatch(SetItemFocus(state.focused, item)))
    |> ignore
  | (_, "Enter") => dispatch(SetPath(state.focused, panel.focusedItem.name))
  | (_, "Backspace") => dispatch(SetPath(state.focused, ".."))
  | (_, "Tab") =>
    dispatch(
      SetPanelFocus(
        state.focused === State.PanelSide.Left ?
          State.PanelSide.Right : State.PanelSide.Left
      )
    )
  | _ => ()
  };
};

Webapi.Dom.Document.addKeyDownEventListener(
  keyPressHandler,
  Webapi.Dom.document
);