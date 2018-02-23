open Reductive;

type panelType = {path: string};

type appState = {leftPanel: panelType};

type side =
  | Left
  | Right;

type actions =
  | SetPath(side, string);

let appReducer = (state: appState, action: actions) =>
  switch action {
  | SetPath(_side, path) => {
      leftPanel: {
        path: path
      }
    }
  | _ => state
  };

let store =
  Reductive.Store.create(
    ~reducer=appReducer,
    ~preloadedState={
      leftPanel: {
        path: "123"
      }
    },
    ()
  );

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
};
/* module ConnectExample = {
     let component = ReasonReact.statelessComponent("ConnectExample");
     let make = _children => {
       ...component,
       render: _self =>
         <Provider
           render=(
             (~state, ~dispatch) =>
               <button
                 onClick=(
                   (_) =>
                     dispatch(
                       SetPath(Left, string_of_int(Js.Math.random_int(1, 100)))
                     )
                 )>
                 (ReasonReact.stringToElement("path" ++ state.leftPanel.path))
               </button>
           )
         />
     };
   }; */