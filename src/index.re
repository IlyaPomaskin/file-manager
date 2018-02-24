ReactDOMRe.renderToElementWithId(<App />, "index");

let keyPressHandler = event => {
  let state = Reductive.Store.getState(State.store);
  let dispatch = Reductive.Store.dispatch(State.store);
  let keyName = Webapi.Dom.KeyboardEvent.key(event);
  let panel = Rationale.Lens.view(State.getLensBySide(state.focused), state);
  switch keyName {
  | "ArrowLeft" => dispatch(SetItemFocusOffset(State.Offset.Left))
  | "ArrowRight" => dispatch(SetItemFocusOffset(State.Offset.Right))
  | "ArrowUp" => dispatch(SetItemFocusOffset(State.Offset.Up))
  | "ArrowDown" => dispatch(SetItemFocusOffset(State.Offset.Down))
  | "Enter" => dispatch(SetPath(state.focused, panel.focusedItem.name))
  | "Backspace" => dispatch(SetPath(state.focused, ".."))
  | "Tab" =>
    dispatch(
      SetPanelFocus(
        state.focused === State.PanelSide.Left ?
          State.PanelSide.Right : State.PanelSide.Left
      )
    )
  | "Space" =>
    dispatch(SelectItem(panel.focusedItem));
    dispatch(SetItemFocusOffset(State.Offset.Down));
  | _ => ()
  };
};

Webapi.Dom.Document.addKeyDownEventListener(
  keyPressHandler,
  Webapi.Dom.document
);