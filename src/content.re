open Rationale;

open State;

let renderPanel = (~side, ~state, ~dispatch) => {
  let panel = Lens.view(State.getLensBySide(side), state);
  <div className="o-grid__cell grid">
    <input
      className="path"
      value=panel.focusedItem.name
      onChange=(
        event =>
          dispatch(
            SetPath(
              side,
              ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value
            )
          )
      )
      tabIndex=(-1)
    />
    <Panel
      isFocused=(state.focused === side)
      path=panel.path
      files=panel.files
      focusedItem=panel.focusedItem
      itemsPerColumn=panel.itemsPerColumn
      onFocusItem=(info => dispatch(SetItemFocus(side, info)))
      onPathChange=(info => dispatch(SetPath(side, info.name)))
      onClick=(_e => dispatch(SetPanelFocus(side)))
      onItemsPerColumnChange=(
        itemsPerColumn =>
          dispatch(SetPanelItemsPerColumnCount(side, itemsPerColumn))
      )
    />
  </div>;
};

module ContentComponent = {
  let component = ReasonReact.statelessComponent("Content");
  let make = (~state, ~dispatch, _children) => {
    ...component,
    render: _self =>
      <div className="content">
        <div className="o-grid o-grid--no-gutter">
          (renderPanel(~side=Left, ~state, ~dispatch))
          (renderPanel(~side=Right, ~state, ~dispatch))
        </div>
      </div>
  };
};

let make =
  Reductive.Provider.createMake(
    ~name="ContentConnect",
    ~component=ContentComponent.make,
    State.store
  );