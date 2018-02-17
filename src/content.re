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
          side === Left ? leftLens : rightLens,
          panel => {
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
        <div className="o-grid__cell grid">
          <input
            className="path"
            value=self.state.left.path
            onChange=(self.handle(handlePathInput(Left)))
            tabIndex=(-1)
          />
          <Panel
            path=self.state.left.path
            files=self.state.left.files
            onPathChange=(self.handle(handlePathChange(Left)))
          />
        </div>
        <div className="o-grid__cell grid">
          <input
            className="path"
            value=self.state.right.path
            onChange=(self.handle(handlePathInput(Right)))
            tabIndex=(-1)
          />
          <Panel
            path=self.state.right.path
            files=self.state.right.files
            onPathChange=(self.handle(handlePathChange(Right)))
          />
        </div>
      </div>
    </div>
};