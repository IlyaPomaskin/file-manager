open Rationale;

open Types;

open Types.FileInfo;

type appState = {
  focused: PanelSide.t,
  left: PanelType.t,
  right: PanelType.t
};

type actions =
  | SetPath(PanelSide.t, string)
  | SetPanelFocus(PanelSide.t)
  | SetItemFocus(PanelSide.t, FileInfo.t)
  | SetPanelItemsPerColumnCount(PanelSide.t, int)
  | SelectItems(PanelSide.t, FileInfo.t);

let leftLens =
  Lens.make(state => state.left, (value, state) => {...state, left: value});

let rightLens =
  Lens.make(state => state.right, (value, state) => {...state, right: value});

let getLensBySide = side =>
  switch side {
  | PanelSide.Left => leftLens
  | PanelSide.Right => rightLens
  };

let appReducer = (state, action) =>
  switch action {
  | SetPath(side, relativePath) =>
    Lens.over(
      getLensBySide(side),
      panel => {
        let nextFiles =
          Fs_utils.getFilesList(Node_path.resolve(panel.path, relativePath));
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
          path: Node_path.resolve(panel.path, relativePath),
          files: nextFiles,
          focusedItem: List.nth(nextFiles, nextFocusedItemIndex),
          selectedFiles: []
        };
      },
      state
    )
  | SetPanelFocus(side) => {...state, focused: side}
  | SetItemFocus(side, info) =>
    Lens.over(
      getLensBySide(side),
      panel => {...panel, focusedItem: info},
      state
    )
  | SetPanelItemsPerColumnCount(side, itemsPerColumn) =>
    Lens.over(getLensBySide(side), panel => {...panel, itemsPerColumn}, state)
  | SelectItems(side, toFile) =>
    Lens.over(
      getLensBySide(side),
      panel => {
        let fromIndex = RList.indexOf(panel.focusedItem, panel.files);
        let toIndex = RList.indexOf(toFile, panel.files);
        let indeces =
          if (fromIndex < toIndex) {
            (fromIndex, toIndex);
          } else {
            (toIndex, fromIndex);
          };
        let selectedFiles =
          switch indeces {
          | (Some(fromIndex), Some(toIndex)) =>
            panel.files
            |> Rationale.RList.slice(fromIndex, toIndex - fromIndex)
            |> Rationale.RList.concat(panel.selectedFiles)
          | _ => panel.selectedFiles
          };
        {...panel, selectedFiles};
      },
      state
    )
  | _ => state
  };

let store =
  Reductive.Store.create(
    ~reducer=appReducer,
    ~preloadedState={
      focused: PanelSide.Left,
      left: {
        let path = ".";
        let files = Fs_utils.getFilesList(path);
        {
          focusedItem: List.nth(files, 0),
          path,
          files,
          itemsPerColumn: 1,
          selectedFiles: []
        };
      },
      right: {
        let path = ".";
        let files = Fs_utils.getFilesList(path);
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