open Fs_utils;

type action =
  | SetFocus(string);

type retainedProps = {
  path: string,
  files: array(fileInfo)
};

type state = {focused: string};

let component = ReasonReact.reducerComponentWithRetainedProps("Panel");

let make = (~path, ~onPathChange, ~files, _children) => {
  ...component,
  retainedProps: {
    path,
    files
  },
  willReceiveProps: self => {
    if (self.retainedProps.path !== path) {
      self.send(SetFocus(".."));
    };
    self.state;
  },
  initialState: () => {focused: ".."},
  reducer: (action, _state) =>
    switch action {
    | SetFocus(name) => ReasonReact.Update({focused: name})
    },
  render: self =>
    <div className="panel">
      (
        ReasonReact.arrayToElement(
          Array.map(
            info =>
              <div
                key=info.name
                className=(
                  Cn.make([
                    "panel-item",
                    "u-color-brand-lighter" |> Cn.ifBool(info.isFile),
                    "u-color-brand-darker" |> Cn.ifBool(! info.isFile),
                    "u-bg-grey" |> Cn.ifBool(self.state.focused === info.name)
                  ])
                )
                onClick=(
                  _e => info.isFile ? Js.log(info) : onPathChange(info.name)
                )
                onMouseOver=(_e => self.send(SetFocus(info.name)))>
                (
                  ReasonReact.stringToElement(
                    info.isFile ? {js|📄|js} : {js|📁|js}
                  )
                )
                (ReasonReact.stringToElement(info.name))
              </div>,
            files
          )
        )
      )
    </div>
};