open Rationale;

open Types;

type appState = {
  focused: PanelSide.t,
  left: PanelType.t,
  right: PanelType.t
};

let leftLens =
  Lens.make(state => state.left, (value, state) => {...state, left: value});

let rightLens =
  Lens.make(state => state.right, (value, state) => {...state, right: value});

let getLensBySide = side =>
  switch side {
  | PanelSide.Left => leftLens
  | PanelSide.Right => rightLens
  };

let appReducer = (state, action: AppActions.t) =>
  switch action {
  | RootActions(SetPanelFocus(side)) => {...state, focused: side}
  | RootActions(CopyFiles(files, dst)) =>
    List.iter(
      path => {
        let dstFileName = Node_path.resolve(dst, Node_path.basename(path));
        NodeFsLocal.copyFileSync(path, dstFileName, 0);
      },
      files
    );
    state;
  | PanelActions(side, action) =>
    Lens.over(
      getLensBySide(side),
      prevState => PanelReducer.reducer(prevState, action),
      state
    )
  };

let store =
  Reductive.Store.create(
    ~reducer=appReducer,
    ~preloadedState={
      focused: PanelSide.Left,
      left: {
        let path = NodeFsLocal.homedir();
        let files = FsUtils.getFilesList(path);
        {
          focusedItem: List.nth(files, 0),
          path,
          files,
          itemsPerColumn: 1,
          selectedFiles: []
        };
      },
      right: {
        let path = NodeFsLocal.homedir();
        let files = FsUtils.getFilesList(path);
        {
          focusedItem: List.nth(files, 0),
          path,
          files,
          itemsPerColumn: 1,
          selectedFiles: []
        };
      }
    },
    ()
  );
/*
 module Connect = {
   type state('reductiveState) = {
     reductiveState: option('reductiveState),
     unsubscribe: option(unit => unit)
   };
   type action =
     | UpdateState;
   let innerComponent = ReasonReact.reducerComponent("Connect");
   let make = (~render, _children) => {
     ...innerComponent,
     initialState: () => {
       reductiveState: Some(Store.getState(store)),
       unsubscribe: None
     },
     reducer: (action, state) =>
       switch action {
       | UpdateState =>
         ReasonReact.Update({
           ...state,
           reductiveState: Some(Store.getState(store))
         })
       },
     didMount: ({reduce}) =>
       ReasonReact.Update({
         unsubscribe: Some(Store.subscribe(store, reduce((_) => UpdateState))),
         reductiveState: Some(Store.getState(store))
       }),
     willUnmount: ({state}) =>
       switch state.unsubscribe {
       | Some(unsubscribe) => unsubscribe()
       | None => ()
       },
     render: ({state}) =>
       switch state.reductiveState {
       | None => ReasonReact.nullElement
       | Some(state) => render(~state, ~dispatch=Store.dispatch(store))
       }
   };
 }; */