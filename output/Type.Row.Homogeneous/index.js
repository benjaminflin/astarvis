// Generated by purs version 0.13.5
"use strict";
var HomogeneousRowList = {};
var Homogeneous = {};
var homogeneousRowListNil = HomogeneousRowList;
var homogeneousRowListCons = function (dictHomogeneousRowList) {
    return function (dictTypeEquals) {
        return HomogeneousRowList;
    };
};
var homogeneous = function (dictRowToList) {
    return function (dictHomogeneousRowList) {
        return Homogeneous;
    };
};
module.exports = {
    Homogeneous: Homogeneous,
    HomogeneousRowList: HomogeneousRowList,
    homogeneous: homogeneous,
    homogeneousRowListCons: homogeneousRowListCons,
    homogeneousRowListNil: homogeneousRowListNil
};
