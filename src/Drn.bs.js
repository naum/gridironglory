// Generated by BUCKLESCRIPT VERSION 4.0.7, PLEASE EDIT WITH CARE
'use strict';

var Random = require("bs-platform/lib/js/random.js");

Random.self_init(/* () */0);

function roll(param) {
  var r = Random.$$int(6);
  if (r !== 5) {
    return r;
  } else {
    return 5 + roll(/* () */0) | 0;
  }
}

function drn(param) {
  return roll(/* () */0) + roll(/* () */0) | 0;
}

function randSkill(param) {
  return ((1 + roll(/* () */0) | 0) + roll(/* () */0) | 0) + roll(/* () */0) | 0;
}

exports.roll = roll;
exports.drn = drn;
exports.randSkill = randSkill;
/*  Not a pure module */
