// Generated by BUCKLESCRIPT VERSION 2.2.0, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var React = require("react");
var Reductive = require("reductive/src/reductive.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var Lens$Rationale = require("rationale/src/Lens.js");
var Panel$ReactTemplate = require("./Panel.bs.js");
var State$ReactTemplate = require("../State.bs.js");

function renderPanel(side, state, dispatch) {
  var panel = Lens$Rationale.view(State$ReactTemplate.getLensBySide(side), state);
  return React.createElement("div", {
              className: "o-grid__cell grid"
            }, React.createElement("input", {
                  className: "path",
                  tabIndex: -1,
                  value: panel[/* path */1],
                  onChange: (function ($$event) {
                      return Curry._1(dispatch, /* SetPath */Block.__(0, [
                                    side,
                                    $$event.target.value
                                  ]));
                    })
                }), ReasonReact.element(/* None */0, /* None */0, Panel$ReactTemplate.make(panel, +(state[/* focused */0] === side), (function (info) {
                        return Curry._1(dispatch, /* SetItemFocus */Block.__(2, [
                                      side,
                                      info
                                    ]));
                      }), (function (info) {
                        return Curry._1(dispatch, /* SetPath */Block.__(0, [
                                      side,
                                      info[/* name */0]
                                    ]));
                      }), (function () {
                        return Curry._1(dispatch, /* SetPanelFocus */Block.__(1, [side]));
                      }), (function (itemsPerColumn) {
                        return Curry._1(dispatch, /* SetPanelItemsPerColumnCount */Block.__(3, [
                                      side,
                                      itemsPerColumn
                                    ]));
                      }), /* array */[])));
}

var component = ReasonReact.statelessComponent("Content");

function make(state, dispatch, _) {
  var newrecord = component.slice();
  newrecord[/* render */9] = (function () {
      return React.createElement("div", {
                  className: "content"
                }, React.createElement("div", {
                      className: "o-grid o-grid--no-gutter"
                    }, renderPanel(/* Left */0, state, dispatch), renderPanel(/* Right */1, state, dispatch)));
    });
  return newrecord;
}

var ContentComponent = /* module */[
  /* component */component,
  /* make */make
];

var partial_arg = Reductive.Provider[/* createMake */0](/* Some */["ContentConnect"], State$ReactTemplate.store);

function make$1(param) {
  return partial_arg(make, param);
}

exports.renderPanel = renderPanel;
exports.ContentComponent = ContentComponent;
exports.make = make$1;
/* component Not a pure module */
