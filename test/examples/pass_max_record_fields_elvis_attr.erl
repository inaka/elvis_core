-module(pass_max_record_fields_elvis_attr).

-export([records/0]).

-elvis([{elvis_style, max_record_fields, #{ max_fields => 30 }}]).

-record(r1, {one :: field}).

-record(r2, {one :: field, two :: fields}).

-record(r3, {one :: field, two :: fields, three = fields :: fields}).

-record(r5, {f1, f2, f3, f4, f5}).

-record(r25, {
    f01,
    f02,
    f03,
    f04,
    f05,
    f06,
    f07,
    f08,
    f09,
    f10,
    f11,
    f12,
    f13,
    f14,
    f15,
    f16,
    f17,
    f18,
    f19,
    f20,
    f21,
    f22,
    f23,
    f24,
    f25
}).

-record(r26, {
    f01,
    f02,
    f03,
    f04,
    f05,
    f06,
    f07,
    f08,
    f09,
    f10,
    f11,
    f12,
    f13,
    f14,
    f15,
    f16,
    f17,
    f18,
    f19,
    f20,
    f21,
    f22,
    f23,
    f24,
    f25,
    f26
}).

records() -> [#r1{}, #r2{}, #r3{}, #r5{}, #r25{}, #r26{}].
