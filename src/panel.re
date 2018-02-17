open Fs_utils;

type action =
  | SetFocus(string);

type retainedProps = {
  path: string,
  files: list(fileInfo)
};

type state = {
  focused: string,
  itemRef: ref(option(Dom.element))
};

let fileImage = {js|📄|js};

let folderImage = {js|📁|js};

let getItemHeight = () => 24;

let getPanelHeight = () => 363;

let splitByColumns = (~itemHeight: int, ~panelHeight: int, items) => {
  let itemsInColumn = panelHeight / itemHeight;
  items |> Rationale.RList.splitEvery(itemsInColumn);
  /* |> List.map(Array.of_list)
     |> Array.of_list; */
};

let renderColumnItems = (~self, ~onPathChange, info) =>
  <div
    key=info.name
    style=(
      ReactDOMRe.Style.make(~height=string_of_int(getItemHeight()) ++ "px", ())
    )
    className=(
      Cn.make([
        "panel-item",
        "u-color-brand-lighter" |> Cn.ifBool(info.isFile),
        "u-color-brand-darker" |> Cn.ifBool(! info.isFile),
        "u-bg-grey" |> Cn.ifBool(self.ReasonReact.state.focused === info.name)
      ])
    )
    onClick=(_e => info.isFile ? Js.log(info) : onPathChange(info.name))
    onMouseOver=(_e => self.ReasonReact.send(SetFocus(info.name)))>
    (ReasonReact.stringToElement(info.isFile ? fileImage : folderImage))
    (ReasonReact.stringToElement(info.name))
  </div>;

let renderColumn = (renderItem, index, columnItems) =>
  <div key=(string_of_int(index)) className="panel-column">
    (
      columnItems
      |> List.map(renderItem)
      |> Array.of_list
      |> ReasonReact.arrayToElement
    )
  </div>;

let component = ReasonReact.reducerComponentWithRetainedProps("Panel");

let make = (~path, ~onPathChange, ~files, _children) => {
  ...component,
  retainedProps: {
    path,
    files
  },
  willReceiveProps: self => {
    if (self.retainedProps.path !== path) {
      self.send(SetFocus(".."));
    };
    self.state;
  },
  initialState: () => {focused: "..", itemRef: ref(None)},
  reducer: (action, _state) =>
    switch action {
    | SetFocus(name) => ReasonReact.Update({..._state, focused: name})
    },
  render: self =>
    <div className="panel">
      (
        files
        |> splitByColumns(
             ~itemHeight=getItemHeight(),
             ~panelHeight=getPanelHeight()
           )
        |> List.mapi(renderColumn(renderColumnItems(~self, ~onPathChange)))
        |> Array.of_list
        |> ReasonReact.arrayToElement
      )
    </div>
};