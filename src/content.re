open Rationale;

open Fs_utils;

type panelSides =
  | Left
  | Right;

type action =
  | SetPath(panelSides, fileInfo)
  | SetPanelFocus(panelSides)
  | SetItemFocus(panelSides, fileInfo);

type panelProps = {
  isFocused: bool,
  focusedItem: fileInfo,
  path: string,
  files: list(Fs_utils.fileInfo)
};

type state = {
  left: panelProps,
  right: panelProps
};

let component = ReasonReact.reducerComponent("Content");

let leftLens =
  Lens.make(
    (state: state) => state.left,
    (value: panelProps, state: state) => {...state, left: value}
  );

let rightLens =
  Lens.make(
    (state: state) => state.right,
    (value: panelProps, state: state) => {...state, right: value}
  );

let getLensBySide = side =>
  switch side {
  | Left => leftLens
  | Right => rightLens
  };

let getPath = (path, relative) => Node_path.resolve(path, relative);

let getFiles = path => Fs_utils.getFilesList(path);

let renderPanel = (side: panelSides, self) => {
  let state =
    switch side {
    | Left => self.ReasonReact.state.left
    | Right => self.ReasonReact.state.right
    };
  <div className="o-grid__cell grid">
    <input
      className="path"
      value=state.path
      onChange=(
        event =>
          self.ReasonReact.send(
            SetPath(
              side,
              ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value
            )
          )
      )
      tabIndex=(-1)
    />
    <Panel
      isFocused=state.isFocused
      path=state.path
      files=state.files
      focusedItem=state.focusedItem
      onFocusItem=(info => self.ReasonReact.send(SetItemFocus(side, info)))
      onPathChange=(info => self.ReasonReact.send(SetPath(side, info)))
      onClick=(_e => self.send(SetPanelFocus(side)))
    />
  </div>;
};

let make = _children => {
  ...component,
  initialState: () => {
    left: {
      isFocused: true,
      focusedItem: makeFileInfo("/Users/i.pomaskin", ".."),
      path: "/Users/i.pomaskin",
      files: getFiles("/Users/i.pomaskin")
    },
    right: {
      isFocused: false,
      focusedItem: makeFileInfo("/Users/i.pomaskin", ".."),
      path: "/Users/i.pomaskin",
      files: getFiles("/Users/i.pomaskin")
    }
  },
  subscriptions: _self => [
    Sub(
      () =>
        DocumentRe.addEventListener(
          "keydown",
          _evt => Js.log(_evt),
          Webapi.Dom.document
        ),
      _token =>
        DocumentRe.removeEventListener(
          "keydown",
          _evt => Js.log(_evt),
          Webapi.Dom.document
        )
    )
  ],
  reducer: (action, state) =>
    switch action {
    | SetPath(side, info) =>
      ReasonReact.Update(
        Lens.over(
          getLensBySide(side),
          panel => {
            ...panel,
            path: getPath(panel.path, info.name),
            files: getFiles(getPath(panel.path, info.name))
          },
          state
        )
      )
    | SetPanelFocus(side) =>
      ReasonReact.Update(
        state
        |> Lens.over(leftLens, panel => {...panel, isFocused: side === Left})
        |> Lens.over(rightLens, panel => {...panel, isFocused: side === Right})
      )
    | SetItemFocus(side, info) =>
      ReasonReact.Update(
        Lens.over(
          getLensBySide(side),
          panel => {...panel, focusedItem: info},
          state
        )
      )
    },
  render: self =>
    <div className="content">
      <div className="o-grid o-grid--no-gutter">
        (renderPanel(Left, self))
        (renderPanel(Right, self))
      </div>
    </div>
};