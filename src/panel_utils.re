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
  switch panelRef {
  | Some(node) =>
    node
    |> ElementRe.querySelectorAll(".panel-column")
    |> NodeListRe.toArray
    |> Array.to_list
    |> List.map(ElementRe.ofNode)
    |> List.filter(Rationale.Option.isSome)
    |> List.fold_left(
         (maxWidth, optNode) =>
           switch optNode {
           | Some(node) => max(maxWidth, ElementRe.clientWidth(node))
           | _ => maxWidth
           },
         0
       )
  | _ => 0
  };