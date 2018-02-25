open Rationale;

open Store;

open PanelReducer.Action;

let renderPanel = (~side, ~state, ~rootDispatch, ~panelDispatch) => {
  let panel = Lens.view(Store.getLensBySide(side), state);
  <div className="o-grid__cell grid">
    <input
      className="path"
      value=panel.path
      onChange=(
        event =>
          panelDispatch(
            SetPath(
              ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value
            )
          )
      )
      tabIndex=(-1)
    />
    <Panel
      panel
      isFocused=(state.focused === side)
      onFocusItem=(info => panelDispatch(SetItemFocus(info)))
      onPathChange=(info => panelDispatch(SetPath(info.name)))
      onClick=(_e => rootDispatch(Store.Action.SetPanelFocus(side)))
      onItemsPerColumnChange=(
        itemsPerColumn =>
          panelDispatch(SetPanelItemsPerColumnCount(itemsPerColumn))
      )
    />
  </div>;
};

module ContentComponent = {
  let component = ReasonReact.statelessComponent("Content");
  let make = (~state, ~dispatch: Store.Actions.t => unit, _children) => {
    ...component,
    render: _self =>
      <div className="content">
        <div className="o-grid o-grid--no-gutter">
          (
            renderPanel(
              ~side=Left,
              ~state,
              ~rootDispatch=a => dispatch(RootAction(a)),
              ~panelDispatch=a => dispatch(PanelAction(Left, a))
            )
          )
          (
            renderPanel(
              ~side=Right,
              ~state,
              ~rootDispatch=a => dispatch(RootAction(a)),
              ~panelDispatch=a => dispatch(PanelAction(Right, a))
            )
          )
        </div>
      </div>
  };
};

let make =
  Reductive.Provider.createMake(
    ~name="ContentConnect",
    ~component=ContentComponent.make,
    Store.store
  );