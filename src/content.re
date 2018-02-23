open Rationale;

open Fs_utils;

type panelSide =
  | Left
  | Right;

type action =
  | SetPath(panelSide, string)
  | SetPanelFocus(panelSide)
  | SetItemFocus(panelSide, fileInfo)
  | SetItemFocusOffset(string)
  | SetPanelItemsPerColumnCount(panelSide, int)
  | DispatchKeyPress(string);

type panelProps = {
  focusedItem: fileInfo,
  path: string,
  files: list(Fs_utils.fileInfo),
  itemsPerColumn: int
};

type state = {
  focused: panelSide,
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

let keyPressHandler = (self, event) =>
  self.ReasonReact.send(DispatchKeyPress(Webapi.Dom.KeyboardEvent.key(event)));

let renderPanel = (side: panelSide, self) => {
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
          self.send(
            SetPath(
              side,
              ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value
            )
          )
      )
      tabIndex=(-1)
    />
    <Panel
      isFocused=(self.state.focused === side)
      path=state.path
      files=state.files
      focusedItem=state.focusedItem
      itemsPerColumn=state.itemsPerColumn
      onFocusItem=(info => self.send(SetItemFocus(side, info)))
      onPathChange=(info => self.send(SetPath(side, info.name)))
      onClick=(_e => self.send(SetPanelFocus(side)))
      onItemsPerColumnChange=(
        itemsPerColumn =>
          self.send(SetPanelItemsPerColumnCount(side, itemsPerColumn))
      )
    />
  </div>;
};

let make = _children => {
  ...component,
  initialState: () => {
    focused: Left,
    left: {
      let files = getFiles("/Users/i.pomaskin");
      {
        focusedItem: List.nth(files, 0),
        path: "/Users/i.pomaskin",
        files,
        itemsPerColumn: 1
      };
    },
    right: {
      let files = getFiles("/Users/i.pomaskin");
      {
        focusedItem: List.nth(files, 0),
        path: "/Users/i.pomaskin",
        files,
        itemsPerColumn: 1
      };
    }
  },
  subscriptions: self => [
    Sub(
      () =>
        Webapi.Dom.Document.addKeyDownEventListener(
          keyPressHandler(self),
          Webapi.Dom.document
        ),
      _token =>
        Webapi.Dom.Document.addKeyDownEventListener(
          keyPressHandler(self),
          Webapi.Dom.document
        )
    )
  ],
  reducer: (action, state) =>
    switch action {
    | SetPath(side, relativePath) =>
      ReasonReact.Update(
        Lens.over(
          getLensBySide(side),
          panel => {
            let nextFiles = getFiles(getPath(panel.path, relativePath));
            let nextFocusedItemIndex =
              switch relativePath {
              | ".." =>
                let currentDirectoryName = Node_path.basename(panel.path);
                let idx =
                  RList.findIndex(
                    info => info.name === currentDirectoryName,
                    nextFiles
                  );
                Option.default(0, idx);
              | _ => 0
              };
            {
              ...panel,
              path: getPath(panel.path, relativePath),
              files: nextFiles,
              focusedItem: List.nth(nextFiles, nextFocusedItemIndex)
            };
          },
          state
        )
      )
    | SetPanelFocus(side) => ReasonReact.Update({...state, focused: side})
    | SetItemFocus(side, info) =>
      ReasonReact.Update(
        Lens.over(
          getLensBySide(side),
          panel => {...panel, focusedItem: info},
          state
        )
      )
    | SetPanelItemsPerColumnCount(side, itemsPerColumn) =>
      ReasonReact.Update(
        Lens.over(
          getLensBySide(side),
          panel => {...panel, itemsPerColumn},
          state
        )
      )
    | SetItemFocusOffset(keyName) =>
      ReasonReact.Update(
        Lens.over(
          getLensBySide(state.focused),
          panel => {
            let currentIndex = RList.indexOf(panel.focusedItem, panel.files);
            let idx = Option.default(0, currentIndex);
            let idxOffset =
              switch keyName {
              | "ArrowLeft" => idx - panel.itemsPerColumn
              | "ArrowRight" => idx + panel.itemsPerColumn
              | "ArrowUp" => idx - 1
              | "ArrowDown" => idx + 1
              | _ => 0
              };
            let nextIdx = {
              let lastIndex = List.length(panel.files) - 1;
              if (idxOffset > lastIndex) {
                min(lastIndex, idxOffset);
              } else {
                max(0, idxOffset);
              };
            };
            let focusedItem =
              Option.default(
                panel.focusedItem,
                Rationale.RList.nth(nextIdx, panel.files)
              );
            {...panel, focusedItem};
          },
          state
        )
      )
    | DispatchKeyPress(keyName) =>
      ReasonReact.SideEffects(
        (
          self => {
            let panel = Lens.view(getLensBySide(state.focused), state);
            switch keyName {
            | "ArrowLeft"
            | "ArrowRight"
            | "ArrowUp"
            | "ArrowDown" => self.send(SetItemFocusOffset(keyName))
            | "Enter" =>
              self.send(SetPath(state.focused, panel.focusedItem.name))
            | "Backspace" => self.send(SetPath(state.focused, ".."))
            | _ => ()
            };
          }
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