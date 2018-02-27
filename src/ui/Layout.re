let component = ReasonReact.statelessComponent("Layout");

let make = _children => {
  ...component,
  render: _self =>
    ReasonReact.createDomElement(
      "div",
      ~props={
        "className": "o-container o-container--super c-text c-text--mono layout"
      },
      _children
    )
};