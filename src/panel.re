open Fs_utils;

type action =
  | SetFocus(string)
  | SetPanelHeight(int);

type retainedProps = {
  path: string,
  files: list(fileInfo)
};

type state = {
  focused: string,
  panelHeight: int,
  panelRef: ref(option(Dom.element)),
  itemRef: ref(option(Dom.element))
};

let fileImage = {js|📄|js};

let folderImage = {js|📁|js};

let getItemHeight = () => 24;

let getPanelHeight = () => 363;

let splitByColumns = (~itemHeight: int, ~panelHeight: int, items) => {
  let itemsInColumn = panelHeight / itemHeight;
  Rationale.RList.splitEvery(itemsInColumn, items);
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

let updatePanelHeight = self =>
  switch self.ReasonReact.state.panelRef^ {
  | Some(node) =>
    self.ReasonReact.send(
      SetPanelHeight(
        node |> ElementRe.getBoundingClientRect |> DomRectRe.height
      )
    )
  | _ => ()
  };

let resizeEventListener = (_evt, self) => updatePanelHeight(self);

let setPanelRef = (node, self) =>
  self.ReasonReact.state.panelRef := Js.Nullable.to_opt(node);

let make = (~path, ~onPathChange, ~files, _children) => {
  ...component,
  retainedProps: {
    path,
    files
  },
  subscriptions: self => [
    Sub(
      () => {
        WindowRe.addEventListener(
          "resize",
          self.handle(resizeEventListener),
          Webapi.Dom.window
        );
        "resize";
      },
      _token =>
        WindowRe.removeEventListener(
          "resize",
          self.handle(resizeEventListener),
          Webapi.Dom.window
        )
    )
  ],
  didMount: self => {
    updatePanelHeight(self);
    ReasonReact.NoUpdate;
  },
  willReceiveProps: self => {
    if (self.retainedProps.path !== path) {
      self.send(SetFocus(".."));
    };
    self.state;
  },
  initialState: () => {
    focused: "..",
    itemRef: ref(None),
    panelRef: ref(None),
    panelHeight: 333
  },
  reducer: (action, state) =>
    switch action {
    | SetFocus(name) => ReasonReact.Update({...state, focused: name})
    | SetPanelHeight(height) =>
      ReasonReact.Update({...state, panelHeight: height})
    },
  render: self =>
    <div className="panel" ref=(self.handle(setPanelRef))>
      (
        files
        |> splitByColumns(
             ~itemHeight=getItemHeight(),
             ~panelHeight=self.state.panelHeight
           )
        |> List.mapi(renderColumn(renderColumnItems(~self, ~onPathChange)))
        |> Array.of_list
        |> ReasonReact.arrayToElement
      )
    </div>
};