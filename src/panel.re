open Fs_utils;

type action =
  | SetPanelHeight(int);

type retainedProps = {
  onPathChange: fileInfo => unit,
  onFocusItem: fileInfo => unit,
  focusedItem: fileInfo,
  path: string,
  files: list(fileInfo)
};

type state = {
  panelHeight: int,
  panelRef: ref(option(Dom.element)),
  itemRef: ref(option(Dom.element))
};

let fileImage = {js|📄|js};

let folderImage = {js|📁|js};

let getItemHeight = () => 24;

let splitByColumns = (~itemHeight: int, ~panelHeight: int, items) => {
  let itemsInColumn = max(panelHeight / itemHeight, 1);
  Rationale.RList.splitEvery(itemsInColumn, items);
};

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

let make = {
  let renderColumnItems = (retainedProps, info) =>
    <div
      key=info.name
      style=(
        ReactDOMRe.Style.make(
          ~height=string_of_int(getItemHeight()) ++ "px",
          ()
        )
      )
      className=(
        Cn.make([
          "panel-item",
          "u-color-brand-lighter" |> Cn.ifBool(info.isFile),
          "u-color-brand-darker" |> Cn.ifBool(! info.isFile),
          "u-bg-grey"
        ])
        |> Cn.ifBool(retainedProps.focusedItem.name === info.name)
      )
      onDoubleClick=(_e => retainedProps.onPathChange(info))
      onClick=(_e => retainedProps.onFocusItem(info))>
      (ReasonReact.stringToElement(info.isFile ? fileImage : folderImage))
      (ReasonReact.stringToElement(info.name))
    </div>;
  (
    ~isFocused,
    ~focusedItem,
    ~onFocusItem,
    ~onPathChange,
    ~onClick,
    ~path,
    ~files,
    _children
  ) => {
    ...component,
    retainedProps: {
      onFocusItem,
      onPathChange,
      focusedItem,
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
      if (self.retainedProps.files !== files) {
        updatePanelHeight(self);
      };
      self.state;
    },
    initialState: () => {
      itemRef: ref(None),
      panelRef: ref(None),
      panelHeight: 0
    },
    reducer: (action, state) =>
      switch action {
      | SetPanelHeight(height) =>
        ReasonReact.Update({...state, panelHeight: height})
      },
    render: self =>
      <div
        className=("panel " ++ (isFocused ? "panel--focused" : ""))
        ref=(self.handle(setPanelRef))
        onClick>
        (
          files
          |> splitByColumns(
               ~itemHeight=getItemHeight(),
               ~panelHeight=self.state.panelHeight
             )
          |> List.mapi(renderColumn(renderColumnItems(self.retainedProps)))
          |> Array.of_list
          |> ReasonReact.arrayToElement
        )
      </div>
  };
};