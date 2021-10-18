# Changelog

## [1.3.1](https://github.com/inaka/elvis_core/tree/1.3.1) (2021-10-18)

[Full Changelog](https://github.com/inaka/elvis_core/compare/1.3.0...1.3.1)

**Fixed bugs:**

- badarg when applying numeric\_format [\#213](https://github.com/inaka/elvis_core/issues/213)

**Merged pull requests:**

- Exit with `1` if `escriptize` \(from CI\) fails [\#215](https://github.com/inaka/elvis_core/pull/215) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Fix \#213: Handle no-text numbers in numeric\_format [\#214](https://github.com/inaka/elvis_core/pull/214) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [1.3.0](https://github.com/inaka/elvis_core/tree/1.3.0) (2021-10-06)

[Full Changelog](https://github.com/inaka/elvis_core/compare/1.2.0...1.3.0)

**Implemented enhancements:**

- Run CI on a Windows container [\#180](https://github.com/inaka/elvis_core/issues/180)

**Closed issues:**

- Document numeric\_format [\#211](https://github.com/inaka/elvis_core/issues/211)
- New rule \(?\) behavior vs. behaviour [\#210](https://github.com/inaka/elvis_core/issues/210)
- \[New Rule\] Disallow numbers with underscores in them [\#208](https://github.com/inaka/elvis_core/issues/208)
- Handle Windows newlines gracefully [\#205](https://github.com/inaka/elvis_core/issues/205)

**Merged pull requests:**

- Fix 210: New rule for behavior vs behaviour. [\#212](https://github.com/inaka/elvis_core/pull/212) ([jackyhui96](https://github.com/jackyhui96))
- Fix \#208: New rule for numeric format [\#209](https://github.com/inaka/elvis_core/pull/209) ([elbrujohalcon](https://github.com/elbrujohalcon))
- CI over Windows [\#207](https://github.com/inaka/elvis_core/pull/207) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Handle Windows line endings gracefully [\#206](https://github.com/inaka/elvis_core/pull/206) ([g-andrade](https://github.com/g-andrade))

## [1.2.0](https://github.com/inaka/elvis_core/tree/1.2.0) (2021-07-28)

[Full Changelog](https://github.com/inaka/elvis_core/compare/1.1.2...1.2.0)

**Implemented enhancements:**

- Print initial message, upon starting analysis [\#193](https://github.com/inaka/elvis_core/issues/193)

**Closed issues:**

- Fetch more options from elvis.config + rebar.config [\#197](https://github.com/inaka/elvis_core/issues/197)
- Release request [\#188](https://github.com/inaka/elvis_core/issues/188)

**Merged pull requests:**

- Don't crash upon trying to handle unicode that Erlang accepts [\#203](https://github.com/inaka/elvis_core/pull/203) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Remove error-prone \_meta\_SUITE while creating alias \_test\_ [\#202](https://github.com/inaka/elvis_core/pull/202) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Fix function/arity filtering [\#201](https://github.com/inaka/elvis_core/pull/201) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Allow for analysis under rebar3\_hank [\#200](https://github.com/inaka/elvis_core/pull/200) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Brush up README.md [\#199](https://github.com/inaka/elvis_core/pull/199) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- elvis.config options for application:get\_env/2,3 [\#198](https://github.com/inaka/elvis_core/pull/198) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Increase consumer confidence [\#195](https://github.com/inaka/elvis_core/pull/195) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- chore\(README\): fix link to wiki [\#194](https://github.com/inaka/elvis_core/pull/194) ([z8674558](https://github.com/z8674558))
- Move to a GitHub action \(instead of container-based\) CI approach [\#192](https://github.com/inaka/elvis_core/pull/192) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))

## [1.1.2](https://github.com/inaka/elvis_core/tree/1.1.2) (2021-03-24)

[Full Changelog](https://github.com/inaka/elvis_core/compare/1.1.1...1.1.2)

**Fixed bugs:**

- -\> at the beginning of the line makes the linter crash [\#183](https://github.com/inaka/elvis_core/issues/183)

**Closed issues:**

- Analysis not working over umbrella apps [\#178](https://github.com/inaka/elvis_core/issues/178)

**Merged pull requests:**

- Bump Version to 1.1.2 [\#189](https://github.com/inaka/elvis_core/pull/189) ([elbrujohalcon](https://github.com/elbrujohalcon))
- allow checking spaces around | and || [\#187](https://github.com/inaka/elvis_core/pull/187) ([z8674558](https://github.com/z8674558))
- Further adapt to OTP 24 while bumping some dep.s [\#185](https://github.com/inaka/elvis_core/pull/185) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Prevent crash for operators being analyzed at the left-most line position [\#184](https://github.com/inaka/elvis_core/pull/184) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Fix a crash happening when a macro is also an atom \(as a function\) [\#181](https://github.com/inaka/elvis_core/pull/181) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Fix analysis over umbrella apps [\#179](https://github.com/inaka/elvis_core/pull/179) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Ease consumption of warning [\#176](https://github.com/inaka/elvis_core/pull/176) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))

## [1.1.1](https://github.com/inaka/elvis_core/tree/1.1.1) (2021-02-08)

[Full Changelog](https://github.com/inaka/elvis_core/compare/1.1.0...1.1.1)

**Merged pull requests:**

- Fix unwarranted match as found by OTP 24 [\#175](https://github.com/inaka/elvis_core/pull/175) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))

## [1.1.0](https://github.com/inaka/elvis_core/tree/1.1.0) (2021-01-28)

[Full Changelog](https://github.com/inaka/elvis_core/compare/1.0.0...1.1.0)

**Closed issues:**

- Release request [\#173](https://github.com/inaka/elvis_core/issues/173)
- Warn on empty analysis [\#146](https://github.com/inaka/elvis_core/issues/146)
- Execution without elvis.config shows no issues [\#163](https://github.com/inaka/elvis_core/issues/163)
- provide a hex package [\#7](https://github.com/inaka/elvis_core/issues/7)

**Merged pull requests:**

- Fix \#173: New release \(1.1.0\) [\#174](https://github.com/inaka/elvis_core/pull/174) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Issue warnings on empty folders [\#169](https://github.com/inaka/elvis_core/pull/169) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Fix broken yml for GitHub Actions [\#143](https://github.com/inaka/elvis_core/pull/143) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Replace Travis CI with GitHub Actions [\#142](https://github.com/inaka/elvis_core/pull/142) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Allow for custom rules in elvis configuration [\#141](https://github.com/inaka/elvis_core/pull/141) ([cnasten](https://github.com/cnasten))
- Analyze abstract code \(beam files\) [\#138](https://github.com/inaka/elvis_core/pull/138) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))

## [1.0.0](https://github.com/inaka/elvis_core/tree/1.0.0) (2020-11-23)

[Full Changelog](https://github.com/inaka/elvis_core/compare/0.7.0...1.0.0)

**Merged pull requests:**

- Bump Version to 1.0.0 [\#140](https://github.com/inaka/elvis_core/pull/140) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Accept British spelling for behaviour. [\#139](https://github.com/inaka/elvis_core/pull/139) ([NAR](https://github.com/NAR))
- Feature option ignore in all rules [\#137](https://github.com/inaka/elvis_core/pull/137) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Allow module-level configuration via attribute `-elvis\(\_\)` [\#136](https://github.com/inaka/elvis_core/pull/136) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Decrease chance of error and improve maintenance [\#135](https://github.com/inaka/elvis_core/pull/135) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Complete removal of `seqbind` references [\#134](https://github.com/inaka/elvis_core/pull/134) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Catch invalid dynamic call inside `try ... of`. [\#133](https://github.com/inaka/elvis_core/pull/133) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Add new rule `atom\_naming\_convention` [\#132](https://github.com/inaka/elvis_core/pull/132) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Add options `regex` and `ignore` to rule `macro\_names` [\#131](https://github.com/inaka/elvis_core/pull/131) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))

## [0.7.0](https://github.com/inaka/elvis_core/tree/0.7.0) (2020-09-09)

[Full Changelog](https://github.com/inaka/elvis_core/compare/0.6.1...0.7.0)

**Merged pull requests:**

- Bump Version to 0.7.0 [\#130](https://github.com/inaka/elvis_core/pull/130) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Simplify by removal of seemingly unused rules no\_seqbind and no\_useless\_seqbind [\#129](https://github.com/inaka/elvis_core/pull/129) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Make sure gen\_statem follows the same rule as gen\_server [\#128](https://github.com/inaka/elvis_core/pull/128) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- More visible defaults \(part 2\) [\#127](https://github.com/inaka/elvis_core/pull/127) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- New default nesting level [\#125](https://github.com/inaka/elvis_core/pull/125) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Prevent sub-folder files from being analyzed twice [\#124](https://github.com/inaka/elvis_core/pull/124) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))

## [0.6.1](https://github.com/inaka/elvis_core/tree/0.6.1) (2020-08-11)

[Full Changelog](https://github.com/inaka/elvis_core/compare/0.6.0...0.6.1)

**Merged pull requests:**

- Update changelog [\#123](https://github.com/inaka/elvis_core/pull/123) ([jfacorro](https://github.com/jfacorro))
- Allow use of rebar\_raw\_resource in rebar3 -oriented projects [\#122](https://github.com/inaka/elvis_core/pull/122) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Excess comma removed from example config [\#121](https://github.com/inaka/elvis_core/pull/121) ([roman-mazhut](https://github.com/roman-mazhut))
- Fix module name in README.md [\#120](https://github.com/inaka/elvis_core/pull/120) ([robertoaloi](https://github.com/robertoaloi))

## [0.6.0](https://github.com/inaka/elvis_core/tree/0.6.0) (2020-02-28)

[Full Changelog](https://github.com/inaka/elvis_core/compare/0.5.0...0.6.0)

**Merged pull requests:**

- Prepare release 0.6.0 [\#119](https://github.com/inaka/elvis_core/pull/119) ([jfacorro](https://github.com/jfacorro))
- Allow {Module, Function} ignores in function\_naming\_convention/3 [\#118](https://github.com/inaka/elvis_core/pull/118) ([onno-vos-dev](https://github.com/onno-vos-dev))
- \[inaka/elvis\#425\] Update README after renaming the application [\#117](https://github.com/inaka/elvis_core/pull/117) ([jfacorro](https://github.com/jfacorro))
- \[inaka/elvis\#425\] Rename application to elvis\_core [\#116](https://github.com/inaka/elvis_core/pull/116) ([jfacorro](https://github.com/jfacorro))

## [0.5.0](https://github.com/inaka/elvis_core/tree/0.5.0) (2019-12-14)

[Full Changelog](https://github.com/inaka/elvis_core/compare/0.4.3...0.5.0)

**Merged pull requests:**

- Prepare release 0.5.0 [\#115](https://github.com/inaka/elvis_core/pull/115) ([jfacorro](https://github.com/jfacorro))
- Update katana code [\#113](https://github.com/inaka/elvis_core/pull/113) ([jfacorro](https://github.com/jfacorro))

## [0.4.3](https://github.com/inaka/elvis_core/tree/0.4.3) (2019-12-04)

[Full Changelog](https://github.com/inaka/elvis_core/compare/0.4.2...0.4.3)

**Merged pull requests:**

- Prepare release 0.4.3 [\#114](https://github.com/inaka/elvis_core/pull/114) ([jfacorro](https://github.com/jfacorro))
- \[inaka/elvis\#483\] Operator spaces false positives [\#112](https://github.com/inaka/elvis_core/pull/112) ([jfacorro](https://github.com/jfacorro))
- Prevent duplicate linting of files [\#111](https://github.com/inaka/elvis_core/pull/111) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.4.2](https://github.com/inaka/elvis_core/tree/0.4.2) (2019-05-23)

[Full Changelog](https://github.com/inaka/elvis_core/compare/0.4.1...0.4.2)

**Merged pull requests:**

- Version bump to 0.4.2 [\#109](https://github.com/inaka/elvis_core/pull/109) ([igaray](https://github.com/igaray))
- Rock in parallel [\#107](https://github.com/inaka/elvis_core/pull/107) ([define-null](https://github.com/define-null))
- Add parsable option in order to output results in the dialyzer-like format [\#106](https://github.com/inaka/elvis_core/pull/106) ([define-null](https://github.com/define-null))
- Fix app name in README.md [\#105](https://github.com/inaka/elvis_core/pull/105) ([elbrujohalcon](https://github.com/elbrujohalcon))
- update erlang test [\#103](https://github.com/inaka/elvis_core/pull/103) ([getong](https://github.com/getong))

## [0.4.1](https://github.com/inaka/elvis_core/tree/0.4.1) (2018-07-03)

[Full Changelog](https://github.com/inaka/elvis_core/compare/0.4.0...0.4.1)

**Merged pull requests:**

- Bump Version to 0.4.1 [\#102](https://github.com/inaka/elvis_core/pull/102) ([elbrujohalcon](https://github.com/elbrujohalcon))
- fix OTP 21 dialyzer warnings [\#101](https://github.com/inaka/elvis_core/pull/101) ([f3c0](https://github.com/f3c0))

## [0.4.0](https://github.com/inaka/elvis_core/tree/0.4.0) (2018-06-29)

[Full Changelog](https://github.com/inaka/elvis_core/compare/0.3.9...0.4.0)

**Merged pull requests:**

- Bump Version to 0.4.0 [\#100](https://github.com/inaka/elvis_core/pull/100) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix inaka/elvis\#481\] Update dependencies [\#99](https://github.com/inaka/elvis_core/pull/99) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Support for no\_call and no\_common\_caveats [\#98](https://github.com/inaka/elvis_core/pull/98) ([adrianroe](https://github.com/adrianroe))
- Support ignore for ignored variable used [\#97](https://github.com/inaka/elvis_core/pull/97) ([cypherfox](https://github.com/cypherfox))
- Support the option to ignore no\_nested\_try\_catch rule on module basis [\#96](https://github.com/inaka/elvis_core/pull/96) ([frms-](https://github.com/frms-))
- Update README.md [\#95](https://github.com/inaka/elvis_core/pull/95) ([igaray](https://github.com/igaray))

## [0.3.9](https://github.com/inaka/elvis_core/tree/0.3.9) (2017-08-01)

[Full Changelog](https://github.com/inaka/elvis_core/compare/0.3.8...0.3.9)

**Merged pull requests:**

- \[Fix \#469\] Bump elvis\_core version to 0.3.9 [\#94](https://github.com/inaka/elvis_core/pull/94) ([harenson](https://github.com/harenson))
- \[Fix \#468\] Add include\_dirs/1 to elvis\_config [\#93](https://github.com/inaka/elvis_core/pull/93) ([harenson](https://github.com/harenson))
- \[\#456\] Adding Travis CI [\#92](https://github.com/inaka/elvis_core/pull/92) ([ferigis](https://github.com/ferigis))

## [0.3.8](https://github.com/inaka/elvis_core/tree/0.3.8) (2017-06-15)

[Full Changelog](https://github.com/inaka/elvis_core/compare/0.3.7...0.3.8)

**Merged pull requests:**

- Bump version to 0.3.8 [\#91](https://github.com/inaka/elvis_core/pull/91) ([harenson](https://github.com/harenson))

## [0.3.7](https://github.com/inaka/elvis_core/tree/0.3.7) (2017-06-15)

[Full Changelog](https://github.com/inaka/elvis_core/compare/0.3.6...0.3.7)

**Merged pull requests:**

- \[Fix inaka/elvis\#451\] Bump elvis\_core version to 0.3.7 [\#90](https://github.com/inaka/elvis_core/pull/90) ([harenson](https://github.com/harenson))
- \[Fix \#450\] Fix encoding issue for elvis\_file:src/1 [\#89](https://github.com/inaka/elvis_core/pull/89) ([harenson](https://github.com/harenson))
- add verbosity config option, output only errors unless set [\#88](https://github.com/inaka/elvis_core/pull/88) ([srenatus](https://github.com/srenatus))
- max\_function\_length: report violations including function arity [\#87](https://github.com/inaka/elvis_core/pull/87) ([srenatus](https://github.com/srenatus))
- Support ignore option in function\_naming\_convention [\#86](https://github.com/inaka/elvis_core/pull/86) ([define-null](https://github.com/define-null))

## [0.3.6](https://github.com/inaka/elvis_core/tree/0.3.6) (2017-04-25)

[Full Changelog](https://github.com/inaka/elvis_core/compare/0.3.5...0.3.6)

**Merged pull requests:**

- \[Close inaka/Elvis\#442\] bump version to 0.3.6 [\#85](https://github.com/inaka/elvis_core/pull/85) ([Euen](https://github.com/Euen))
- \[Close inaka/Elvis\#411\] unpdate line length to 100 [\#84](https://github.com/inaka/elvis_core/pull/84) ([Euen](https://github.com/Euen))
- Handle Hex deps with package name [\#83](https://github.com/inaka/elvis_core/pull/83) ([varnerac](https://github.com/varnerac))
- \[inaka/elvis\#342\] function references shouldn't count as a level [\#82](https://github.com/inaka/elvis_core/pull/82) ([jfacorro](https://github.com/jfacorro))
- Fix broken support for dashes in operator\_spaces rule [\#81](https://github.com/inaka/elvis_core/pull/81) ([tjarvstrand](https://github.com/tjarvstrand))
- Adds latin1 support for line\_length and operator\_spaces rules [\#80](https://github.com/inaka/elvis_core/pull/80) ([tjarvstrand](https://github.com/tjarvstrand))
- Remove Lager as a direct dependency [\#65](https://github.com/inaka/elvis_core/pull/65) ([waisbrot](https://github.com/waisbrot))

## [0.3.5](https://github.com/inaka/elvis_core/tree/0.3.5) (2017-02-09)

[Full Changelog](https://github.com/inaka/elvis_core/compare/0.3.5-pre...0.3.5)

**Merged pull requests:**

- \[inaka/elvis\#423\] Version Bump to 0.3.5 [\#79](https://github.com/inaka/elvis_core/pull/79) ([ferigis](https://github.com/ferigis))
- \[inaka/elvis\#402\] ct:print added to no\_debug\_call rule [\#77](https://github.com/inaka/elvis_core/pull/77) ([ferigis](https://github.com/ferigis))
- \[inaka/elvis\#406\] dialyzer warnings fixed [\#76](https://github.com/inaka/elvis_core/pull/76) ([ferigis](https://github.com/ferigis))
- \[inaka/elvis\#410\] no\_nested\_try\_catch rule added [\#75](https://github.com/inaka/elvis_core/pull/75) ([ferigis](https://github.com/ferigis))

## [0.3.5-pre](https://github.com/inaka/elvis_core/tree/0.3.5-pre) (2017-02-08)

[Full Changelog](https://github.com/inaka/elvis_core/compare/0.3.4...0.3.5-pre)

**Merged pull requests:**

- \[inaka/elvis\#424\] updating dependencies [\#74](https://github.com/inaka/elvis_core/pull/74) ([ferigis](https://github.com/ferigis))
- Fix unnecessary loading of files caused by git-branch option \(\#421\) [\#72](https://github.com/inaka/elvis_core/pull/72) ([onno-vos-dev](https://github.com/onno-vos-dev))

## [0.3.4](https://github.com/inaka/elvis_core/tree/0.3.4) (2016-11-15)

[Full Changelog](https://github.com/inaka/elvis_core/compare/0.3.3...0.3.4)

**Merged pull requests:**

- \[Close inaka/elvis\#417\] bump version to 0.3.4 - elvis\_core [\#71](https://github.com/inaka/elvis_core/pull/71) ([Euen](https://github.com/Euen))
- \[Close inaka/elvis\#416\] update wrong file type [\#70](https://github.com/inaka/elvis_core/pull/70) ([Euen](https://github.com/Euen))

## [0.3.3](https://github.com/inaka/elvis_core/tree/0.3.3) (2016-11-11)

[Full Changelog](https://github.com/inaka/elvis_core/compare/0.3.2...0.3.3)

**Merged pull requests:**

- Add zipper to rebar.lock [\#69](https://github.com/inaka/elvis_core/pull/69) ([Euen](https://github.com/Euen))
- \[Close inaka/elvis\#414\] bump version to 0.3.3 [\#68](https://github.com/inaka/elvis_core/pull/68) ([Euen](https://github.com/Euen))
- Rules for warning about seqbind [\#67](https://github.com/inaka/elvis_core/pull/67) ([gnuvince](https://github.com/gnuvince))
- \[Fix \#407\] Fix bug in max\_function\_length rule [\#64](https://github.com/inaka/elvis_core/pull/64) ([harenson](https://github.com/harenson))
- Don't automatically strip leading underscore, allow it by regex [\#63](https://github.com/inaka/elvis_core/pull/63) ([bartekgorny](https://github.com/bartekgorny))
- Make it so that the max\_function\_length style checker can ignore func… [\#62](https://github.com/inaka/elvis_core/pull/62) ([sargun](https://github.com/sargun))

## [0.3.2](https://github.com/inaka/elvis_core/tree/0.3.2) (2016-08-03)

[Full Changelog](https://github.com/inaka/elvis_core/compare/0.3.1...0.3.2)

**Merged pull requests:**

- \[Close inaka/elvis\#389\] version bump 0.3.2 [\#61](https://github.com/inaka/elvis_core/pull/61) ([Euen](https://github.com/Euen))
- Euen.fix dialyzer [\#60](https://github.com/inaka/elvis_core/pull/60) ([Euen](https://github.com/Euen))

## [0.3.1](https://github.com/inaka/elvis_core/tree/0.3.1) (2016-07-27)

[Full Changelog](https://github.com/inaka/elvis_core/compare/0.3.0...0.3.1)

**Merged pull requests:**

- \[close inaka/elvis\#388\] version bump 0.3.1 [\#59](https://github.com/inaka/elvis_core/pull/59) ([Euen](https://github.com/Euen))
- fix spec for dialyzer [\#58](https://github.com/inaka/elvis_core/pull/58) ([Euen](https://github.com/Euen))
- fix application name [\#57](https://github.com/inaka/elvis_core/pull/57) ([Euen](https://github.com/Euen))
- fix application name [\#56](https://github.com/inaka/elvis_core/pull/56) ([Euen](https://github.com/Euen))

## [0.3.0](https://github.com/inaka/elvis_core/tree/0.3.0) (2016-07-25)

[Full Changelog](https://github.com/inaka/elvis_core/compare/0.2.11...0.3.0)

**Merged pull requests:**

- \[Close inaka/elvis\#386\] version bump to 0.3.0 [\#55](https://github.com/inaka/elvis_core/pull/55) ([Euen](https://github.com/Euen))
- \[Close inaka/elvis\#381\] Euen.381.rebar3 [\#54](https://github.com/inaka/elvis_core/pull/54) ([Euen](https://github.com/Euen))
- \[\#384\] improve documentation and error [\#53](https://github.com/inaka/elvis_core/pull/53) ([Euen](https://github.com/Euen))
- Don't crash on rebar3's pkg aliases [\#52](https://github.com/inaka/elvis_core/pull/52) ([sstrigler](https://github.com/sstrigler))
- \[Fix inaka/elvis\#343\] Use unicode module to convert utf8 binary to list [\#51](https://github.com/inaka/elvis_core/pull/51) ([jfacorro](https://github.com/jfacorro))
- \[Fix inaka/elvis\#362\] Correctly distinguish macros from variables [\#50](https://github.com/inaka/elvis_core/pull/50) ([elbrujohalcon](https://github.com/elbrujohalcon))
- exclude '.' to be prepended to a filter [\#49](https://github.com/inaka/elvis_core/pull/49) ([bwegh](https://github.com/bwegh))

## [0.2.11](https://github.com/inaka/elvis_core/tree/0.2.11) (2016-04-07)

[Full Changelog](https://github.com/inaka/elvis_core/compare/0.2.10...0.2.11)

**Merged pull requests:**

- \[inaka/elvis\#372\] Version Bump to 0.2.11 [\#48](https://github.com/inaka/elvis_core/pull/48) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[\#371\] \[Fix inaka/elvis\#371\] Move deps to app.src [\#47](https://github.com/inaka/elvis_core/pull/47) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.2.10](https://github.com/inaka/elvis_core/tree/0.2.10) (2016-03-30)

[Full Changelog](https://github.com/inaka/elvis_core/compare/0.2.9...0.2.10)

**Merged pull requests:**

- \[Fix inaka/elvis\#369\] Version Bump to 0.2.10 [\#46](https://github.com/inaka/elvis_core/pull/46) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix inaka/elvis\#368\] Upgrade katana\_code dep [\#45](https://github.com/inaka/elvis_core/pull/45) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.2.9](https://github.com/inaka/elvis_core/tree/0.2.9) (2016-03-28)

[Full Changelog](https://github.com/inaka/elvis_core/compare/0.2.8-2...0.2.9)

**Merged pull requests:**

- \[Fix \#365\] Bump version to 0.2.9 [\#44](https://github.com/inaka/elvis_core/pull/44) ([harenson](https://github.com/harenson))
- \[Fix \#364\] Fix specs [\#43](https://github.com/inaka/elvis_core/pull/43) ([harenson](https://github.com/harenson))
- Refactor discussed in \#40 [\#42](https://github.com/inaka/elvis_core/pull/42) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#360\] Update dependencies; Update erlang.mk; Add meta suite [\#41](https://github.com/inaka/elvis_core/pull/41) ([elbrujohalcon](https://github.com/elbrujohalcon))
- don't ignore ignore \(pun\) after \#6 [\#40](https://github.com/inaka/elvis_core/pull/40) ([andreineculau](https://github.com/andreineculau))
- \[Fix \#353\] Update katana [\#39](https://github.com/inaka/elvis_core/pull/39) ([harenson](https://github.com/harenson))

## [0.2.8-2](https://github.com/inaka/elvis_core/tree/0.2.8-2) (2016-03-07)

[Full Changelog](https://github.com/inaka/elvis_core/compare/0.2.8...0.2.8-2)

**Merged pull requests:**

- \[Fix \#349\] Bump version to 0.2.8-2 [\#38](https://github.com/inaka/elvis_core/pull/38) ([harenson](https://github.com/harenson))
- \[Fix \#347\] Remove unused application [\#37](https://github.com/inaka/elvis_core/pull/37) ([harenson](https://github.com/harenson))

## [0.2.8](https://github.com/inaka/elvis_core/tree/0.2.8) (2016-03-07)

[Full Changelog](https://github.com/inaka/elvis_core/compare/0.2.7...0.2.8)

**Merged pull requests:**

- \[Fix \#345\] Bump version to 0.2.8 [\#36](https://github.com/inaka/elvis_core/pull/36) ([harenson](https://github.com/harenson))
- \[\#29\] More ignore [\#34](https://github.com/inaka/elvis_core/pull/34) ([Euen](https://github.com/Euen))
- \[Fix \#41\] rebar3 support [\#33](https://github.com/inaka/elvis_core/pull/33) ([Euen](https://github.com/Euen))

## [0.2.7](https://github.com/inaka/elvis_core/tree/0.2.7) (2016-01-22)

[Full Changelog](https://github.com/inaka/elvis_core/compare/0.2.6...0.2.7)

**Merged pull requests:**

- \[inaka/elvis\#335\] Version bump to 0.2.7 [\#32](https://github.com/inaka/elvis_core/pull/32) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#335\] Update katana to 0.2.22 [\#31](https://github.com/inaka/elvis_core/pull/31) ([jfacorro](https://github.com/jfacorro))

## [0.2.6](https://github.com/inaka/elvis_core/tree/0.2.6) (2016-01-21)

[Full Changelog](https://github.com/inaka/elvis_core/compare/0.2.6-alpha2...0.2.6)

**Merged pull requests:**

- \[inaka/elvis\#334\] Update katana [\#30](https://github.com/inaka/elvis_core/pull/30) ([jfacorro](https://github.com/jfacorro))
- update IGNORE\_DEPS in makefile [\#28](https://github.com/inaka/elvis_core/pull/28) ([Euen](https://github.com/Euen))
- \[Fix inaka/elvis\#332\] update katana version [\#27](https://github.com/inaka/elvis_core/pull/27) ([Euen](https://github.com/Euen))
- \[Fix inaka/elvis\#327\] update release 0.2.6 [\#26](https://github.com/inaka/elvis_core/pull/26) ([Euen](https://github.com/Euen))
- \[\#325\] old configuration format rule [\#25](https://github.com/inaka/elvis_core/pull/25) ([Euen](https://github.com/Euen))
- Euen.280.update for rebar3 [\#24](https://github.com/inaka/elvis_core/pull/24) ([Euen](https://github.com/Euen))
- \[Closes inaka/elvis\#326\] Remove calls to io:format [\#23](https://github.com/inaka/elvis_core/pull/23) ([jfacorro](https://github.com/jfacorro))
- \[Fix \#296\] Add test to check issue \#296 is already fixed [\#22](https://github.com/inaka/elvis_core/pull/22) ([harenson](https://github.com/harenson))
- \[Fix \#323\] Add documentation for default configuration feature [\#21](https://github.com/inaka/elvis_core/pull/21) ([harenson](https://github.com/harenson))
- \[Fix \#319\] Fix operator\_spaces rule check [\#20](https://github.com/inaka/elvis_core/pull/20) ([harenson](https://github.com/harenson))
- Handle erlang.mk dep\_depname\_commit format [\#19](https://github.com/inaka/elvis_core/pull/19) ([jonasrichard](https://github.com/jonasrichard))
- \[Fix \#166\] Extend default configuration [\#18](https://github.com/inaka/elvis_core/pull/18) ([harenson](https://github.com/harenson))
- Version Bump to 0.2.6-alpha3 [\#17](https://github.com/inaka/elvis_core/pull/17) ([davecaos](https://github.com/davecaos))
- \[\#300\] Updated for Hexer Hex.pm app [\#16](https://github.com/inaka/elvis_core/pull/16) ([davecaos](https://github.com/davecaos))

## [0.2.6-alpha2](https://github.com/inaka/elvis_core/tree/0.2.6-alpha2) (2015-12-23)

[Full Changelog](https://github.com/inaka/elvis_core/compare/8fe81e68eadc791b3f465f5c8c6b8481b6e31cc4...0.2.6-alpha2)

**Closed issues:**

- Update README with apropiate content from inaka/elvis [\#10](https://github.com/inaka/elvis_core/issues/10)
- Remove rebar binary [\#8](https://github.com/inaka/elvis_core/issues/8)
- add option for global ignore [\#5](https://github.com/inaka/elvis_core/issues/5)

**Merged pull requests:**

- \[Closes inaka/elvis\#317\] Update deps to avoid compiler warnings [\#15](https://github.com/inaka/elvis_core/pull/15) ([jfacorro](https://github.com/jfacorro))
- \[Closes inaka/elvis\#315\] Create release [\#14](https://github.com/inaka/elvis_core/pull/14) ([jfacorro](https://github.com/jfacorro))
- \[Closes inaka/elvis\#313\] Rename application [\#13](https://github.com/inaka/elvis_core/pull/13) ([jfacorro](https://github.com/jfacorro))
- \[Closes inaka/elvis\#312\] Update readme [\#12](https://github.com/inaka/elvis_core/pull/12) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#10\] Update readme [\#11](https://github.com/inaka/elvis_core/pull/11) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#8\] Remove rebar [\#9](https://github.com/inaka/elvis_core/pull/9) ([jfacorro](https://github.com/jfacorro))
- Add group level ignore [\#6](https://github.com/inaka/elvis_core/pull/6) ([Licenser](https://github.com/Licenser))
- Copy in the elvis files [\#2](https://github.com/inaka/elvis_core/pull/2) ([Licenser](https://github.com/Licenser))
- Update LICENSE [\#1](https://github.com/inaka/elvis_core/pull/1) ([elbrujohalcon](https://github.com/elbrujohalcon))



\* *This Changelog was automatically generated by [github_changelog_generator](https://github.com/github-changelog-generator/github-changelog-generator)*
