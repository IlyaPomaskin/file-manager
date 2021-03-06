open Rationale;

open Rationale.Option;

open Types;

let getItemHeight = () => 24;

let getPanelHeight = node =>
  node |> ElementRe.getBoundingClientRect |> DomRectRe.height;

let getColumnsCount = panelHeight => {
  let itemHeight = getItemHeight();
  max(panelHeight / itemHeight, 1);
};

let scrollToNode = (shouldScroll, panelRef, node) => {
  let optPanelItemNode = Js.toOption(node);
  switch (shouldScroll, panelRef, optPanelItemNode) {
  | (true, Some(panelNode), Some(panelItemNode)) =>
    let itemOffsetX1 =
      panelItemNode
      |> ElementRe.unsafeAsHtmlElement
      |> HtmlElementRe.offsetLeft;
    let itemOffsetX2 =
      panelItemNode
      |> ElementRe.unsafeAsHtmlElement
      |> HtmlElementRe.clientWidth
      |> (width => width + itemOffsetX1);
    let panelOffset =
      panelNode |> ElementRe.unsafeAsHtmlElement |> HtmlElementRe.offsetLeft;
    let panelScrollX1 =
      panelNode
      |> ElementRe.unsafeAsHtmlElement
      |> HtmlElementRe.scrollLeft
      |> (x => x + panelOffset);
    let panelScrollX2 =
      panelNode
      |> ElementRe.unsafeAsHtmlElement
      |> HtmlElementRe.clientWidth
      |> (width => width + panelScrollX1 + 5);
    if (itemOffsetX1 !== panelScrollX1
        && (itemOffsetX1 < panelScrollX1 || itemOffsetX2 > panelScrollX2)) {
      ElementRe.setScrollLeft(panelNode, itemOffsetX1 - panelOffset);
    };
  | _ => ()
  };
};

let getMaxColumnWidth = panelRef =>
  panelRef
  <$> ElementRe.querySelectorAll(".panel-column")
  <$> NodeListRe.toArray
  <$> Array.to_list
  <$> List.map(node =>
        node |> ElementRe.ofNode <$> ElementRe.clientWidth |> Option.default(0)
      )
  <$> List.fold_left(max, 0)
  <$> (maxWidth => maxWidth + 5)
  |> Option.default(120);

let getItemByOffset = (panel: PanelType.t, offsetType: Offset.t) =>
  panel.files
  |> RList.indexOf(panel.focusedItem)
  <$> (
    focusedItemIndex =>
      switch offsetType {
      | Offset.Left => focusedItemIndex - panel.itemsPerColumn
      | Offset.Right => focusedItemIndex + panel.itemsPerColumn
      | Offset.Up => focusedItemIndex - 1
      | Offset.Down => focusedItemIndex + 1
      }
  )
  <$> (
    targetIndex => {
      let lastIndex = List.length(panel.files) - 1;
      if (targetIndex > lastIndex) {
        min(lastIndex, targetIndex);
      } else {
        max(0, targetIndex);
      };
    }
  )
  >>= (targetIndex => RList.nth(targetIndex, panel.files));