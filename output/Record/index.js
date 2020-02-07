// Generated by purs version 0.13.5
"use strict";
var Data_Eq = require("../Data.Eq/index.js");
var Data_Symbol = require("../Data.Symbol/index.js");
var Record_Unsafe = require("../Record.Unsafe/index.js");
var Record_Unsafe_Union = require("../Record.Unsafe.Union/index.js");
var Type_Data_RowList = require("../Type.Data.RowList/index.js");
var Unsafe_Coerce = require("../Unsafe.Coerce/index.js");
var EqualFields = function (equalFields) {
    this.equalFields = equalFields;
};
var union = function (dictUnion) {
    return function (l) {
        return function (r) {
            return Record_Unsafe_Union.unsafeUnionFn(l, r);
        };
    };
};
var set = function (dictIsSymbol) {
    return function (dictCons) {
        return function (dictCons1) {
            return function (l) {
                return function (b) {
                    return function (r) {
                        return Record_Unsafe.unsafeSet(Data_Symbol.reflectSymbol(dictIsSymbol)(l))(b)(r);
                    };
                };
            };
        };
    };
};
var nub = function (dictNub) {
    return Unsafe_Coerce.unsafeCoerce;
};
var merge = function (dictUnion) {
    return function (dictNub) {
        return function (l) {
            return function (r) {
                return Record_Unsafe_Union.unsafeUnionFn(l, r);
            };
        };
    };
};
var insert = function (dictIsSymbol) {
    return function (dictLacks) {
        return function (dictCons) {
            return function (l) {
                return function (a) {
                    return function (r) {
                        return Record_Unsafe.unsafeSet(Data_Symbol.reflectSymbol(dictIsSymbol)(l))(a)(r);
                    };
                };
            };
        };
    };
};
var get = function (dictIsSymbol) {
    return function (dictCons) {
        return function (l) {
            return function (r) {
                return Record_Unsafe.unsafeGet(Data_Symbol.reflectSymbol(dictIsSymbol)(l))(r);
            };
        };
    };
};
var modify = function (dictIsSymbol) {
    return function (dictCons) {
        return function (dictCons1) {
            return function (l) {
                return function (f) {
                    return function (r) {
                        return set(dictIsSymbol)()()(l)(f(get(dictIsSymbol)()(l)(r)))(r);
                    };
                };
            };
        };
    };
};
var equalFieldsNil = new EqualFields(function (v) {
    return function (v1) {
        return function (v2) {
            return true;
        };
    };
});
var equalFields = function (dict) {
    return dict.equalFields;
};
var equalFieldsCons = function (dictIsSymbol) {
    return function (dictEq) {
        return function (dictCons) {
            return function (dictEqualFields) {
                return new EqualFields(function (v) {
                    return function (a) {
                        return function (b) {
                            var get$prime = get(dictIsSymbol)()(Data_Symbol.SProxy.value);
                            var equalRest = equalFields(dictEqualFields)(Type_Data_RowList.RLProxy.value);
                            return Data_Eq.eq(dictEq)(get$prime(a))(get$prime(b)) && equalRest(a)(b);
                        };
                    };
                });
            };
        };
    };
};
var equal = function (dictRowToList) {
    return function (dictEqualFields) {
        return function (a) {
            return function (b) {
                return equalFields(dictEqualFields)(Type_Data_RowList.RLProxy.value)(a)(b);
            };
        };
    };
};
var disjointUnion = function (dictUnion) {
    return function (dictNub) {
        return function (l) {
            return function (r) {
                return Record_Unsafe_Union.unsafeUnionFn(l, r);
            };
        };
    };
};
var $$delete = function (dictIsSymbol) {
    return function (dictLacks) {
        return function (dictCons) {
            return function (l) {
                return function (r) {
                    return Record_Unsafe.unsafeDelete(Data_Symbol.reflectSymbol(dictIsSymbol)(l))(r);
                };
            };
        };
    };
};
var rename = function (dictIsSymbol) {
    return function (dictIsSymbol1) {
        return function (dictCons) {
            return function (dictLacks) {
                return function (dictCons1) {
                    return function (dictLacks1) {
                        return function (prev) {
                            return function (next) {
                                return function (record) {
                                    return insert(dictIsSymbol1)()()(next)(get(dictIsSymbol)()(prev)(record))($$delete(dictIsSymbol)()()(prev)(record));
                                };
                            };
                        };
                    };
                };
            };
        };
    };
};
module.exports = {
    get: get,
    set: set,
    modify: modify,
    insert: insert,
    "delete": $$delete,
    rename: rename,
    equal: equal,
    merge: merge,
    union: union,
    disjointUnion: disjointUnion,
    nub: nub,
    EqualFields: EqualFields,
    equalFields: equalFields,
    equalFieldsCons: equalFieldsCons,
    equalFieldsNil: equalFieldsNil
};
