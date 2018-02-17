open Rationale;

type panelSides =
  | Left
  | Right;

type action =
  | SetPath(panelSides, string);

type panelProps = {
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

let handlePathInput = (side: panelSides, event, self) =>
  self.ReasonReact.send(
    SetPath(
      side,
      ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value
    )
  );

let handlePathChange = (side: panelSides, path, self) =>
  self.ReasonReact.send(SetPath(side, path));

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
      onChange=(self.handle(handlePathInput(side)))
      tabIndex=(-1)
    />
    <Panel
      path=state.path
      files=state.files
      onPathChange=(self.handle(handlePathChange(side)))
    />
  </div>;
};

let make = _children => {
  ...component,
  initialState: () => {
    left: {
      path: "/Users/i.pomaskin",
      files: getFiles("/Users/i.pomaskin")
    },
    right: {
      path: "/Users/i.pomaskin",
      files: getFiles("/Users/i.pomaskin")
    }
  },
  reducer: (action, state) =>
    switch action {
    | SetPath(side, relativePath) =>
      ReasonReact.Update(
        Lens.over(
          getLensBySide(side),
          panel => {
            ...panel,
            path: getPath(panel.path, relativePath),
            files: getFiles(getPath(panel.path, relativePath))
          },
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