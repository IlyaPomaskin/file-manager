open Rationale;

type panelSides =
  | Left
  | Right;

type action =
  | SetPath(panelSides, string);

type panelProps = {
  path: string,
  files: array(Fs_utils.fileInfo)
};

type state = {
  left: panelProps,
  right: panelProps
};

let component = ReasonReact.reducerComponent("Content");

let make = _children => {
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
  let makePanel = (~path, ~relativePath="", ()) => {
    let path = Node_path.resolve(path, relativePath);
    {path, files: Fs_utils.getFilesList(path)};
  };
  let handlePathInput = (side: panelSides, event, self) =>
    self.ReasonReact.send(
      SetPath(
        side,
        ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value
      )
    );
  let handlePathChange = (side: panelSides, path, self) => self.ReasonReact.send(SetPath(side, path));
  {
    ...component,
    initialState: () => {
      left: makePanel(~path="/Users/i.pomaskin", ()),
      right: makePanel(~path="/Users/i.pomaskin", ())
    },
    reducer: (action, state) =>
      switch action {
      | SetPath(side, relativePath) =>
        ReasonReact.Update(
          Lens.over(
            side === Left ? leftLens : rightLens,
            panel => makePanel(~path=panel.path, ~relativePath, ()),
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
};