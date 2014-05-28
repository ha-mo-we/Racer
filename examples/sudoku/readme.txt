The Sudoku demo demonstrates a number of advanced RacerPro features, such as
programmatic knowledge base creation via MiniLisp programs (which are
efficiently executed on the RacerPro server and are termination safe), as well
as server extensibility. Please ensure that RacerPro is running in unsafe mode,
i.e., start RacerPro with the command line option "-u" (as "RacerPro -- -u"),
or simply launch RacerPorter from the RacerPro 2.0 directory which
automatically launches RacerPro in unsafe mode.

Please load the file "sudoko.racer" using the "Load..." button of RacerPorter,
or "Open..."  the file in the RacerEditor and evaluate the whole buffer using
Ctrl-Shift-h (or Cmd-h on a Mac). After that, evaluate the last two expressions
(time (sudoko...)) and (time (sudoko-web ...)) from that file individually,
using Ctrl-Shift-e (or Cmd-e on a Mac), and watch the results in the
RacerPorter Shell.

Two new RacerPro server functions are defined in MiniLisp, "sudoko" and
"sudoku-web", which can be called in order to solve a 8x8 Sudoku puzzle by
means of reasoning. Both functions take a Sudoku puzzle as input, where a 0
entry represents an empty field.  The "sudoku-web" function presents the
Sudoku solution as a HTML page which is served ("published") by RacerPro under
http://localhost:8080/sudoku.html, whereas the "sudoku" function presents the
solution in a tabular ASCII form in RacerPorter.

