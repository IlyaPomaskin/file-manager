open Types;

open Types.FileInfo;

open Types.PanelType;

type action =
  | SetColumnWidth(int)
  | SetPanelHeight(int);

type retainedProps = {
  panel: PanelType.t,
  onItemsPerColumnChange: int => unit
};

type state = {
  panelRef: ref(option(Dom.element)),
  columnWidth: int
};

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
  self.ReasonReact.state.panelRef := Js.Nullable.toOption(node);

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
        |> List.map(columnItems =>
             <Column
               key=List.nth(columnItems, 0).name
               columnWidth=self.state.columnWidth
               columnItems
               panelRef=self.state.panelRef^
               panel=self.retainedProps.panel
               isFocused
               onPathChange
               onFocusItem
             />
           )
        |> Array.of_list
        |> ReasonReact.arrayToElement
      )
    </div>
};