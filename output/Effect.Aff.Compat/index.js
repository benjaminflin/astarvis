// Generated by purs version 0.13.5
"use strict";
var Data_Either = require("../Data.Either/index.js");
var Effect_Aff = require("../Effect.Aff/index.js");
var EffectFnCanceler = function (x) {
    return x;
};
var EffectFnAff = function (x) {
    return x;
};
var fromEffectFnAff = function (v) {
    return Effect_Aff.makeAff(function (k) {
        return function __do() {
            var v1 = v(function ($4) {
                return k(Data_Either.Left.create($4))();
            }, function ($5) {
                return k(Data_Either.Right.create($5))();
            });
            return function (e) {
                return Effect_Aff.makeAff(function (k2) {
                    return function __do() {
                        v1(e, function ($6) {
                            return k2(Data_Either.Left.create($6))();
                        }, function ($7) {
                            return k2(Data_Either.Right.create($7))();
                        });
                        return Effect_Aff.nonCanceler;
                    };
                });
            };
        };
    });
};
module.exports = {
    EffectFnAff: EffectFnAff,
    EffectFnCanceler: EffectFnCanceler,
    fromEffectFnAff: fromEffectFnAff
};
