open Rationale;

open Fs_utils;

type panelType = {
  focusedItem: fileInfo,
  path: string,
  files: list(fileInfo),
  itemsPerColumn: int
};

type side =
  | Left
  | Right;

type appState = {
  focused: side,
  left: panelType,
  right: panelType
};

type actions =
  | SetPath(side, string)
  | SetPanelFocus(side)
  | SetItemFocus(side, fileInfo)
  | SetItemFocusOffset(string)
  | SetPanelItemsPerColumnCount(side, int);

let leftLens =
  Lens.make(state => state.left, (value, state) => {...state, left: value});

let rightLens =
  Lens.make(state => state.right, (value, state) => {...state, right: value});

let getLensBySide = side =>
  switch side {
  | Left => leftLens
  | Right => rightLens
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
          focusedItem: List.nth(nextFiles, nextFocusedItemIndex)
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
  | SetItemFocusOffset(keyName) =>
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
  | _ => state
  };

let store =
  Reductive.Store.create(
    ~reducer=appReducer,
    ~preloadedState={
      focused: Left,
      left: {
        let path = ".";
        let files = Fs_utils.getFilesList(path);
        {focusedItem: List.nth(files, 0), path, files, itemsPerColumn: 1};
      },
      right: {
        let path = ".";
        let files = Fs_utils.getFilesList(path);
        {focusedItem: List.nth(files, 0), path, files, itemsPerColumn: 1};
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