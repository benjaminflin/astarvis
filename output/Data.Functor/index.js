// Generated by purs version 0.13.5
"use strict";
var $foreign = require("./foreign.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Functor = function (map) {
    this.map = map;
};
var map = function (dict) {
    return dict.map;
};
var mapFlipped = function (dictFunctor) {
    return function (fa) {
        return function (f) {
            return map(dictFunctor)(f)(fa);
        };
    };
};
var $$void = function (dictFunctor) {
    return map(dictFunctor)(Data_Function["const"](Data_Unit.unit));
};
var voidLeft = function (dictFunctor) {
    return function (f) {
        return function (x) {
            return map(dictFunctor)(Data_Function["const"](x))(f);
        };
    };
};
var voidRight = function (dictFunctor) {
    return function (x) {
        return map(dictFunctor)(Data_Function["const"](x));
    };
};
var functorFn = new Functor(Control_Semigroupoid.compose(Control_Semigroupoid.semigroupoidFn));
var functorArray = new Functor($foreign.arrayMap);
var flap = function (dictFunctor) {
    return function (ff) {
        return function (x) {
            return map(dictFunctor)(function (f) {
                return f(x);
            })(ff);
        };
    };
};
module.exports = {
    Functor: Functor,
    map: map,
    mapFlipped: mapFlipped,
    "void": $$void,
    voidRight: voidRight,
    voidLeft: voidLeft,
    flap: flap,
    functorFn: functorFn,
    functorArray: functorArray
};
