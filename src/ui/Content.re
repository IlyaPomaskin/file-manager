open Rationale;

open Store;

open PanelActions;

open RootActions;

let renderPanel = (~side, ~state, ~rootDispatch, ~panelDispatch) => {
  let panel = Lens.view(Store.getLensBySide(side), state);
  <div className="o-grid__cell grid">
    <input className="path" value=panel.path tabIndex=(-1) />
    <Panel
      panel
      isFocused=(state.focused === side)
      onFocusItem=(info => panelDispatch(SetItemFocus(info)))
      onPathChange=(info => panelDispatch(SetPath(info.name)))
      onClick=(_e => rootDispatch(SetPanelFocus(side)))
      onItemsPerColumnChange=(
        itemsPerColumn =>
          panelDispatch(SetPanelItemsPerColumnCount(itemsPerColumn))
      )
    />
  </div>;
};

module ContentComponent = {
  let component = ReasonReact.statelessComponent("Content");
  let make = (~state, ~dispatch: AppActions.t => unit, _children) => {
    ...component,
    render: _self =>
      <div className="content">
        <div className="o-grid o-grid--no-gutter">
          (
            renderPanel(
              ~side=Left,
              ~state,
              ~rootDispatch=a => dispatch(RootActions(a)),
              ~panelDispatch=a => dispatch(PanelActions(Left, a))
            )
          )
          (
            renderPanel(
              ~side=Right,
              ~state,
              ~rootDispatch=a => dispatch(RootActions(a)),
              ~panelDispatch=a => dispatch(PanelActions(Right, a))
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