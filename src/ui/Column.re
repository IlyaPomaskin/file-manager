open Types.FileInfo;

let component = ReasonReact.statelessComponent("Column");

let make =
    (
      ~columnWidth,
      ~columnItems,
      ~panelRef,
      ~panel,
      ~isFocused,
      ~onPathChange,
      ~onFocusItem,
      _children
    ) => {
  ...component,
  render: _self =>
    <div
      className="panel-column"
      style=(
        ReactDOMRe.Style.make(
          ~width=
            columnWidth === 0 ? "auto" : string_of_int(columnWidth) ++ "px",
          ()
        )
      )>
      (
        columnItems
        |> List.map(info =>
             <ColumnItem
               key=info.name
               panelRef
               panel
               isFocused
               onPathChange
               onFocusItem
               info
             />
           )
        |> Array.of_list
        |> ReasonReact.arrayToElement
      )
    </div>
};