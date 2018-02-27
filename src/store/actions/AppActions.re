open Types;

type t =
  | RootActions(RootActions.t)
  | PanelActions(PanelSide.t, PanelActions.t);