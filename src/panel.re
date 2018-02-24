open Fs_utils;

type action =
  | SetPanelHeight(int);

type retainedProps = {
  onPathChange: fileInfo => unit,
  onFocusItem: fileInfo => unit,
  onItemsPerColumnChange: int => unit,
  focusedItem: fileInfo,
  path: string,
  files: list(fileInfo),
  isFocused: bool
};

type state = {panelRef: ref(option(Dom.element))};

let fileImage = {js|📄|js};

let folderImage = {js|📁|js};

let renderColumn = (renderItem, index, columnItems) =>
  <div key=(string_of_int(index)) className="panel-column">
    (
      columnItems
      |> List.map(renderItem)
      |> Array.of_list
      |> ReasonReact.arrayToElement
    )
  </div>;

let updatePanelHeight = self =>
  switch self.ReasonReact.state.panelRef^ {
  | Some(node) =>
    let panelHeight = Panel_utils.getPanelHeight(node);
    self.ReasonReact.retainedProps.onItemsPerColumnChange(
      Panel_utils.getColumnsCount(panelHeight)
    );
    self.ReasonReact.send(SetPanelHeight(panelHeight));
  | _ => ()
  };

let resizeEventListener = (_evt, self) => updatePanelHeight(self);

let setPanelRef = (node, self) =>
  self.ReasonReact.state.panelRef := Js.Nullable.to_opt(node);

let renderColumnItems = (panelRef, retainedProps, info) =>
  <div
    key=info.name
    className=(
      Cn.make([
        "panel-item",
        "panel-item--focused" |> Cn.ifBool(retainedProps.focusedItem === info),
        "panel-item--active-focused"
        |> Cn.ifBool(
             retainedProps.focusedItem === info && retainedProps.isFocused
           ),
        "u-color-brand-lighter" |> Cn.ifBool(info.isFile),
        "u-color-brand-darker" |> Cn.ifBool(! info.isFile)
      ])
    )
    ref=(
      Panel_utils.scrollToNode(
        retainedProps.isFocused && retainedProps.focusedItem.name === info.name,
        panelRef
      )
    )
    onDoubleClick=(_e => retainedProps.onPathChange(info))
    onClick=(_e => retainedProps.onFocusItem(info))>
    (ReasonReact.stringToElement(info.isFile ? fileImage : folderImage))
    (ReasonReact.stringToElement(info.name))
  </div>;

let component = ReasonReact.reducerComponentWithRetainedProps("Panel");

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
    files,
    isFocused
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
        |> List.mapi(
             renderColumn(
               renderColumnItems(self.state.panelRef^, self.retainedProps)
             )
           )
        |> Array.of_list
        |> ReasonReact.arrayToElement
      )
    </div>
};