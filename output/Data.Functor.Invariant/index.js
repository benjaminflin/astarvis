// Generated by purs version 0.13.5
"use strict";
var Data_Functor = require("../Data.Functor/index.js");
var Invariant = function (imap) {
    this.imap = imap;
};
var invariantMultiplicative = new Invariant(function (f) {
    return function (v) {
        return function (v1) {
            return f(v1);
        };
    };
});
var invariantEndo = new Invariant(function (ab) {
    return function (ba) {
        return function (v) {
            return function ($31) {
                return ab(v(ba($31)));
            };
        };
    };
});
var invariantDual = new Invariant(function (f) {
    return function (v) {
        return function (v1) {
            return f(v1);
        };
    };
});
var invariantDisj = new Invariant(function (f) {
    return function (v) {
        return function (v1) {
            return f(v1);
        };
    };
});
var invariantConj = new Invariant(function (f) {
    return function (v) {
        return function (v1) {
            return f(v1);
        };
    };
});
var invariantAdditive = new Invariant(function (f) {
    return function (v) {
        return function (v1) {
            return f(v1);
        };
    };
});
var imapF = function (dictFunctor) {
    return function (f) {
        return function (v) {
            return Data_Functor.map(dictFunctor)(f);
        };
    };
};
var invariantArray = new Invariant(imapF(Data_Functor.functorArray));
var invariantFn = new Invariant(imapF(Data_Functor.functorFn));
var imap = function (dict) {
    return dict.imap;
};
module.exports = {
    imap: imap,
    Invariant: Invariant,
    imapF: imapF,
    invariantFn: invariantFn,
    invariantArray: invariantArray,
    invariantAdditive: invariantAdditive,
    invariantConj: invariantConj,
    invariantDisj: invariantDisj,
    invariantDual: invariantDual,
    invariantEndo: invariantEndo,
    invariantMultiplicative: invariantMultiplicative
};
