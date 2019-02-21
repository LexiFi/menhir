# Changes

## 2019/02/XX

* The Coq development is now free of any axiom (it used to use axiom
  K), and the parsers can now be executed directly within Coq, without
  using extraction.

* The parser interpreter is now written using dependent types, so that
  no dynamic checks are needed anymore at parsing time. When running
  the extracted code, this should give a performance boost. Moreover,
  efficient extraction of int31 is no longer needed. This required
  some refactoring of the type of parse trees.

## 2018/08/27

* Avoid an undocumented mode of use of the `fix` tactic,
  which would cause an incompatibility with Coq > 8.8.1.
  (Reported and corrected by Michael Soegtrop.)

## 2018/05/30

* Initial release.
