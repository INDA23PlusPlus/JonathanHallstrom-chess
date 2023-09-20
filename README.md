# johalls-chess

## Basic usage
```
let mut b = Board::default(); // default starting position, use Board::from_fen("[some FEN string]") if you want a different starting position

b.play_move(b.get_legal_moves()[0]); // plays some move, the order of moves is unspecified
b.undo_last_move(); // gets you back to the starting position

println!("{}", b.to_fen());
```
