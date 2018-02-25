// Generated by BUCKLESCRIPT VERSION 2.2.0, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Reductive = require("reductive/src/reductive.js");
var Lens$Rationale = require("rationale/src/Lens.js");
var FsUtils$ReactTemplate = require("../utils/FsUtils.bs.js");
var PanelReducer$ReactTemplate = require("./panelReducer.bs.js");

var Action = /* module */[];

var Actions = /* module */[];

var leftLens = Lens$Rationale.make((function (state) {
        return state[/* left */1];
      }), (function (value, state) {
        return /* record */[
                /* focused */state[/* focused */0],
                /* left */value,
                /* right */state[/* right */2]
              ];
      }));

var rightLens = Lens$Rationale.make((function (state) {
        return state[/* right */2];
      }), (function (value, state) {
        return /* record */[
                /* focused */state[/* focused */0],
                /* left */state[/* left */1],
                /* right */value
              ];
      }));

function getLensBySide(side) {
  if (side !== 0) {
    return rightLens;
  } else {
    return leftLens;
  }
}

function appReducer(state, action) {
  if (action.tag) {
    return /* record */[
            /* focused */action[0][0],
            /* left */state[/* left */1],
            /* right */state[/* right */2]
          ];
  } else {
    var action$1 = action[1];
    return Lens$Rationale.over(getLensBySide(action[0]), (function (prevState) {
                  return PanelReducer$ReactTemplate.reducer(prevState, action$1);
                }), state);
  }
}

var path = ".";

var files = FsUtils$ReactTemplate.getFilesList(path);

var path$1 = ".";

var files$1 = FsUtils$ReactTemplate.getFilesList(path$1);

var store = Reductive.Store[/* create */0](appReducer, /* record */[
      /* focused : Left */0,
      /* left : record */[
        /* focusedItem */List.nth(files, 0),
        /* path */path,
        /* files */files,
        /* itemsPerColumn */1,
        /* selectedFiles : [] */0
      ],
      /* right : record */[
        /* focusedItem */List.nth(files$1, 0),
        /* path */path$1,
        /* files */files$1,
        /* itemsPerColumn */1,
        /* selectedFiles : [] */0
      ]
    ], /* None */0, /* () */0);

exports.Action = Action;
exports.Actions = Actions;
exports.leftLens = leftLens;
exports.rightLens = rightLens;
exports.getLensBySide = getLensBySide;
exports.appReducer = appReducer;
exports.store = store;
/* leftLens Not a pure module */
