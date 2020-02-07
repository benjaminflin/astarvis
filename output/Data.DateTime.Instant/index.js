// Generated by purs version 0.13.5
"use strict";
var $foreign = require("./foreign.js");
var Data_Boolean = require("../Data.Boolean/index.js");
var Data_Bounded = require("../Data.Bounded/index.js");
var Data_Date = require("../Data.Date/index.js");
var Data_Date_Component = require("../Data.Date.Component/index.js");
var Data_DateTime = require("../Data.DateTime/index.js");
var Data_Enum = require("../Data.Enum/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Time = require("../Data.Time/index.js");
var Data_Time_Component = require("../Data.Time.Component/index.js");
var Data_Time_Duration = require("../Data.Time.Duration/index.js");
var Instant = function (x) {
    return x;
};
var unInstant = function (v) {
    return v;
};
var toDateTime = (function () {
    var mkDateTime = function (y) {
        return function (mo) {
            return function (d) {
                return function (h) {
                    return function (mi) {
                        return function (s) {
                            return function (ms) {
                                return new Data_DateTime.DateTime(Data_Date.canonicalDate(y)(Data_Maybe.fromJust()(Data_Enum.toEnum(Data_Date_Component.boundedEnumMonth)(mo)))(d), new Data_Time.Time(h, mi, s, ms));
                            };
                        };
                    };
                };
            };
        };
    };
    return $foreign.toDateTimeImpl(mkDateTime);
})();
var showInstant = new Data_Show.Show(function (v) {
    return "(Instant " + (Data_Show.show(Data_Time_Duration.showMilliseconds)(v) + ")");
});
var ordDateTime = Data_Time_Duration.ordMilliseconds;
var instant = function (v) {
    if (v >= -8.6399778816e15 && v <= 8.639977881599999e15) {
        return new Data_Maybe.Just(v);
    };
    if (Data_Boolean.otherwise) {
        return Data_Maybe.Nothing.value;
    };
    throw new Error("Failed pattern match at Data.DateTime.Instant (line 44, column 1 - line 44, column 41): " + [ v.constructor.name ]);
};
var fromDateTime = function (v) {
    return $foreign.fromDateTimeImpl(Data_Date.year(v.value0), Data_Enum.fromEnum(Data_Date_Component.boundedEnumMonth)(Data_Date.month(v.value0)), Data_Date.day(v.value0), Data_Time.hour(v.value1), Data_Time.minute(v.value1), Data_Time.second(v.value1), Data_Time.millisecond(v.value1));
};
var fromDate = function (d) {
    return $foreign.fromDateTimeImpl(Data_Date.year(d), Data_Enum.fromEnum(Data_Date_Component.boundedEnumMonth)(Data_Date.month(d)), Data_Date.day(d), Data_Bounded.bottom(Data_Time_Component.boundedHour), Data_Bounded.bottom(Data_Time_Component.boundedMinute), Data_Bounded.bottom(Data_Time_Component.boundedSecond), Data_Bounded.bottom(Data_Time_Component.boundedMillisecond));
};
var eqDateTime = Data_Time_Duration.eqMilliseconds;
var boundedInstant = new Data_Bounded.Bounded(function () {
    return ordDateTime;
}, -8.6399778816e15, 8.639977881599999e15);
module.exports = {
    instant: instant,
    unInstant: unInstant,
    fromDateTime: fromDateTime,
    fromDate: fromDate,
    toDateTime: toDateTime,
    eqDateTime: eqDateTime,
    ordDateTime: ordDateTime,
    boundedInstant: boundedInstant,
    showInstant: showInstant
};
