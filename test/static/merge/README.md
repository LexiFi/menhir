Each test consists of two files `lhsXX.messages` and `rhsXX.messages` that
must be merged and a file `mergedXX.exp` that contains the expected result of
the merge operation.

Here is a table of the tests:

Test number | Situation
----------- | ---------
01          | Two identical files.
02          | Empty left-hand file.
03          | Empty right-hand file.
04          | The left-hand file provides a message for a sentence that already appears in a group in the right-hand file.
05          | The left-hand file provides a message for a state that already appears in a group in the right-hand file.
06          | The left-hand file provides messages for two sentences that already appear in a group in the right-hand file.
07          | The right-hand file provides messages for two sentences that already appear in a group in the left-hand file (so the group must be split).
08          | Different groups, different default messages, different missing messages on either side.
