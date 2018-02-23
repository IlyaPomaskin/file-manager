open Fs_utils;

type action =
  | SetPanelHeight(int);

type retainedProps = {
  onPathChange: fileInfo => unit,
  onFocusItem: fileInfo => unit,
  onItemsPerColumnChange: int => unit,
  focusedItem: fileInfo,
  path: string,
  files: list(fileInfo)
};

type state = {panelRef: ref(option(Dom.element))};

let fileImage = {js|📄|js};

let folderImage = {js|📁|js};

let getItemHeight = () => 24;

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
    let panelHeight =
      node |> ElementRe.getBoundingClientRect |> DomRectRe.height;
    let itemHeight = getItemHeight();
    self.ReasonReact.retainedProps.onItemsPerColumnChange(
      max(panelHeight / itemHeight, 1)
    );
    self.ReasonReact.send(SetPanelHeight(panelHeight));
  | _ => ()
  };

let resizeEventListener = (_evt, self) => updatePanelHeight(self);

let setPanelRef = (node, self) =>
  self.ReasonReact.state.panelRef := Js.Nullable.to_opt(node);

let scrollToNode = node => {
  let optNode = Js.Nullable.to_opt(node);
  switch optNode {
  | Some(n) => Webapi.Dom.Element.scrollIntoView(n)
  | _ => ()
  };
};

let renderColumnItems = (retainedProps, info) =>
  <div
    key=info.name
    style=(
      ReactDOMRe.Style.make(~height=string_of_int(getItemHeight()) ++ "px", ())
    )
    className=(
      Cn.make([
        "panel-item",
        "panel-item--focused" |> Cn.ifBool(retainedProps.focusedItem === info),
        "u-color-brand-lighter" |> Cn.ifBool(info.isFile),
        "u-color-brand-darker" |> Cn.ifBool(! info.isFile)
      ])
    )
    ref=(
      node =>
        if (retainedProps.focusedItem === info) {
          scrollToNode(node);
        }
    )
    onDoubleClick=(_e => retainedProps.onPathChange(info))
    onClick=(_e => retainedProps.onFocusItem(info))>
    (ReasonReact.stringToElement(info.isFile ? fileImage : folderImage))
    (ReasonReact.stringToElement(info.name))
  </div>;

let make =
    (
      ~isFocused,
      ~focusedItem,
      ~onFocusItem,
      ~onPathChange,
      ~onClick,
      ~onItemsPerColumnChange,
      ~path,
      ~files,
      ~itemsPerColumn,
      _children
    ) => {
  ...component,
  retainedProps: {
    onFocusItem,
    onPathChange,
    onItemsPerColumnChange,
    focusedItem,
    path,
    files
  },
  subscriptions: self => [
    Sub(
      () => {
        Webapi.Dom.Window.addEventListener(
          "resize",
          self.handle(resizeEventListener),
          Webapi.Dom.window
        );
        "resize";
      },
      _token =>
        Webapi.Dom.Window.removeEventListener(
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
    if (self.retainedProps.files !== files) {
      updatePanelHeight(self);
    };
    self.state;
  },
  initialState: () => {panelRef: ref(None)},
  render: self =>
    <div
      className=("panel " ++ (isFocused ? "panel--focused" : ""))
      ref=(self.handle(setPanelRef))
      onClick>
      (
        files
        |> Rationale.RList.splitEvery(itemsPerColumn)
        |> List.mapi(renderColumn(renderColumnItems(self.retainedProps)))
        |> Array.of_list
        |> ReasonReact.arrayToElement
      )
    </div>
};