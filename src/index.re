ReactDOMRe.renderToElementWithId(<App />, "index");

let keyPressHandler = event => {
  let state = Reductive.Store.getState(State.store);
  let dispatch = Reductive.Store.dispatch(State.store);
  let keyName = Webapi.Dom.KeyboardEvent.key(event);
  let panel = Rationale.Lens.view(State.getLensBySide(state.focused), state);
  switch keyName {
  | "ArrowLeft"
  | "ArrowRight"
  | "ArrowUp"
  | "ArrowDown" => dispatch(SetItemFocusOffset(keyName))
  | "Enter" => dispatch(SetPath(state.focused, panel.focusedItem.name))
  | "Backspace" => dispatch(SetPath(state.focused, ".."))
  | "Tab" => dispatch(SetPanelFocus(state.focused === Left ? Right : Left))
  | _ => ()
  };
};

Webapi.Dom.Document.addKeyDownEventListener(
  keyPressHandler,
  Webapi.Dom.document
);