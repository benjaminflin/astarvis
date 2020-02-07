// Generated by purs version 0.13.5
"use strict";
var Data_List_Partial = require("../Data.List.Partial/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_PQueue = require("../Data.PQueue/index.js");
var tail = function (dictPartial) {
    return Data_Newtype.over(Data_PQueue.newtypePQueue)(Data_PQueue.newtypePQueue)(Data_PQueue.PQueue)(Data_List_Partial.tail());
};
var last = function (dictPartial) {
    var $4 = Data_List_Partial.last();
    var $5 = Data_Newtype.unwrap(Data_PQueue.newtypePQueue);
    return function ($6) {
        return $4($5($6));
    };
};
var init = function (dictPartial) {
    return Data_Newtype.over(Data_PQueue.newtypePQueue)(Data_PQueue.newtypePQueue)(Data_PQueue.PQueue)(Data_List_Partial.init());
};
var head = function (dictPartial) {
    var $7 = Data_List_Partial.head();
    var $8 = Data_Newtype.unwrap(Data_PQueue.newtypePQueue);
    return function ($9) {
        return $7($8($9));
    };
};
module.exports = {
    head: head,
    tail: tail,
    init: init,
    last: last
};
