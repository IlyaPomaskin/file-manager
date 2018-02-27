open Types;

open Types.FileInfo;

open Types.PanelType;

type action =
  | SetColumnWidth(int)
  | SetPanelHeight(int);

type retainedProps = {
  panel: PanelType.t,
  isFocused: bool,
  onPathChange: FileInfo.t => unit,
  onFocusItem: FileInfo.t => unit,
  onItemsPerColumnChange: int => unit
};

type state = {
  panelRef: ref(option(Dom.element)),
  columnWidth: int
};

let fileImage = {js|📄|js};

let folderImage = {js|📁|js};

let updatePanelHeight = self =>
  switch self.ReasonReact.state.panelRef^ {
  | Some(node) =>
    let panelHeight = PanelUtils.getPanelHeight(node);
    self.ReasonReact.retainedProps.onItemsPerColumnChange(
      PanelUtils.getColumnsCount(panelHeight)
    );
    self.ReasonReact.send(SetPanelHeight(panelHeight));
  | _ => ()
  };

let updateColumnsWidth = self =>
  self.ReasonReact.send(
    SetColumnWidth(
      PanelUtils.getMaxColumnWidth(self.ReasonReact.state.panelRef^)
    )
  );

let resizeEventListener = (_evt, self) => updatePanelHeight(self);

let setPanelRef = (node, self) =>
  self.ReasonReact.state.panelRef := Js.Nullable.to_opt(node);

let renderColumn = (columnWidth, renderItem, columnItems) =>
  <div
    key=List.nth(columnItems, 0).name
    className="panel-column"
    style=(
      ReactDOMRe.Style.make(
        ~width=columnWidth === 0 ? "auto" : string_of_int(columnWidth) ++ "px",
        ()
      )
    )>
    (
      columnItems
      |> List.map(renderItem)
      |> Array.of_list
      |> ReasonReact.arrayToElement
    )
  </div>;

let renderColumnItems = (panelRef, retainedProps, info) =>
  <div
    key=info.name
    className=(
      Cn.make([
        "panel-item",
        "panel-item--selected"
        |> Cn.ifBool(
             Rationale.RList.contains(info, retainedProps.panel.selectedFiles)
           ),
        "panel-item--focused"
        |> Cn.ifBool(retainedProps.panel.focusedItem === info),
        "panel-item--active-focused"
        |> Cn.ifBool(
             retainedProps.panel.focusedItem === info
             && retainedProps.isFocused
           ),
        "u-color-brand-lighter" |> Cn.ifBool(info.isFile),
        "u-color-brand-darker" |> Cn.ifBool(! info.isFile)
      ])
    )
    ref=(
      PanelUtils.scrollToNode(
        retainedProps.isFocused
        && retainedProps.panel.focusedItem.name === info.name,
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
      ~panel,
      ~isFocused,
      ~onFocusItem,
      ~onPathChange,
      ~onClick,
      ~onItemsPerColumnChange,
      _children
    ) => {
  ...component,
  retainedProps: {
    panel,
    isFocused,
    onFocusItem,
    onPathChange,
    onItemsPerColumnChange
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
  willReceiveProps: self =>
    if (self.retainedProps.panel.files !== panel.files) {
      updatePanelHeight(self);
      {...self.state, columnWidth: 0};
    } else {
      self.state;
    },
  didUpdate: ({oldSelf, newSelf}) =>
    if (oldSelf.retainedProps.panel.files !== newSelf.retainedProps.panel.files) {
      updateColumnsWidth(newSelf);
    },
  initialState: () => {panelRef: ref(None), columnWidth: 0},
  reducer: (action, state) =>
    switch action {
    | SetColumnWidth(width) =>
      ReasonReact.Update({...state, columnWidth: width})
    | _ => ReasonReact.NoUpdate
    },
  render: self =>
    <div
      className=("panel " ++ (isFocused ? "panel--focused" : ""))
      ref=(self.handle(setPanelRef))
      onClick>
      (
        panel.files
        |> Rationale.RList.splitEvery(panel.itemsPerColumn)
        |> List.map(
             renderColumn(
               self.state.columnWidth,
               renderColumnItems(self.state.panelRef^, self.retainedProps)
             )
           )
        |> Array.of_list
        |> ReasonReact.arrayToElement
      )
    </div>
};