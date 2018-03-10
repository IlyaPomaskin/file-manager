open Types.FileInfo;

open Types.PanelType;

let fileImage = {js|📄|js};

let folderImage = {js|📁|js};

let component = ReasonReact.statelessComponent("ColumnItem");

let make =
    (
      ~panelRef,
      ~panel,
      ~isFocused,
      ~onPathChange,
      ~onFocusItem,
      ~info,
      _children
    ) => {
  ...component,
  render: _self =>
    <div
      className=(
        Cn.make([
          "panel-item",
          "panel-item--selected"
          |> Cn.ifBool(Rationale.RList.contains(info, panel.selectedFiles)),
          "panel-item--focused" |> Cn.ifBool(panel.focusedItem === info),
          "panel-item--active-focused"
          |> Cn.ifBool(panel.focusedItem === info && isFocused),
          "u-color-brand-lighter" |> Cn.ifBool(info.isFile),
          "u-color-brand-darker" |> Cn.ifBool(! info.isFile)
        ])
      )
      ref=(
        PanelUtils.scrollToNode(
          isFocused && panel.focusedItem.name === info.name,
          panelRef
        )
      )
      onDoubleClick=(_e => onPathChange(info))
      onClick=(_e => onFocusItem(info))>
      <div className="panel-item__icon">
        (ReasonReact.stringToElement(info.isFile ? fileImage : folderImage))
      </div>
      (ReasonReact.stringToElement(info.name))
    </div>
};