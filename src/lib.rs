#![allow(unused)]
use std::{collections::HashSet, convert::identity};

use itertools::*;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PieceType {
    #[default]
    None,
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

impl PieceType {
    fn get_repr(&self) -> char {
        match self {
            PieceType::None => panic!(),
            PieceType::Pawn => 'p',
            PieceType::Knight => 'n',
            PieceType::Bishop => 'b',
            PieceType::Rook => 'r',
            PieceType::Queen => 'q',
            PieceType::King => 'k',
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
struct Bitboard(u64);

impl Bitboard {
    fn get(&self, pos: (u8, u8)) -> bool {
        self.0 >> pos_to_idx(pos) & 1 != 0
    }

    fn set(&mut self, pos: (u8, u8)) {
        self.0 |= 1u64 << pos_to_idx(pos);
    }

    fn clear(&mut self, pos: (u8, u8)) {
        self.0 &= !(1u64 << pos_to_idx(pos));
    }

    fn combine(&self, other: Bitboard) -> Bitboard {
        Bitboard(self.0 | other.0)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Color {
    #[default]
    White,
    Black,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Piece {
    tp: PieceType,
    pos: (u8, u8),
    col: Color,
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Move {
    start_piece: Piece,
    end_piece: Piece,
    captured_piece: Option<Piece>,
}

impl Move {
    fn unknown_move() -> Move {
        Move {
            start_piece: Piece {
                tp: PieceType::None,
                pos: (0, 0),
                col: Color::White,
            },
            end_piece: Piece {
                tp: PieceType::None,
                pos: (0, 0),
                col: Color::White,
            },
            captured_piece: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Board {
    white_pieces: HashSet<Piece>,
    black_pieces: HashSet<Piece>,

    white_piece_bitboard: Bitboard,
    black_piece_bitboard: Bitboard,

    played_moves: Vec<Move>,
}

impl Default for Board {
    /// returns standard default position for chess
    ///
    /// ```
    /// # use jonathan_hallstrom_chess::*;
    ///
    /// let b = Board::default();
    /// assert_eq!(b.get_legal_moves().len(), 20);
    /// ```
    fn default() -> Self {
        Self::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap()
    }
}

fn append_pos_to_string(s: &mut String, pos: (u8, u8)) {
    s.push((b'a' + pos.1) as char);
    s.push((b'1' + pos.0) as char);
}

impl Board {
    fn empty() -> Board {
        Board {
            white_pieces: HashSet::new(),
            black_pieces: HashSet::new(),
            white_piece_bitboard: Bitboard(0),
            black_piece_bitboard: Bitboard(0),
            played_moves: vec![],
        }
    }

    fn add_piece(&mut self, p: Piece) {
        match p.col {
            Color::White => {
                self.white_pieces.insert(p);
                self.white_piece_bitboard.set(p.pos);
            }
            Color::Black => {
                self.black_pieces.insert(p);
                self.black_piece_bitboard.set(p.pos);
            }
        }
    }

    pub fn get_all_pieces(&self) -> HashSet<Piece> {
        self.white_pieces
            .iter()
            .chain(self.black_pieces.iter())
            .copied()
            .collect()
    }

    pub fn from_fen(fen_str: &str) -> Option<Board> {
        let mut board = Board::empty();
        let mut sections = fen_str.split_terminator(' ');
        let mut table_data_section = sections.next()?;

        let rows = dbg!(table_data_section
            .split_terminator('/')
            .filter(|x| !x.is_empty())
            .collect::<Vec<&str>>());

        if rows.len() != 8 {
            return None;
        }

        for (row, i) in rows.iter().zip(0..) {
            let mut j = 0;
            for c in row.chars() {
                match c {
                    'p' => board.add_piece(Piece {
                        tp: PieceType::Pawn,
                        pos: (7 - i, j),
                        col: Color::Black,
                    }),
                    'P' => board.add_piece(Piece {
                        tp: PieceType::Pawn,
                        pos: (7 - i, j),
                        col: Color::White,
                    }),
                    'n' => board.add_piece(Piece {
                        tp: PieceType::Knight,
                        pos: (7 - i, j),
                        col: Color::Black,
                    }),
                    'N' => board.add_piece(Piece {
                        tp: PieceType::Knight,
                        pos: (7 - i, j),
                        col: Color::White,
                    }),
                    'b' => board.add_piece(Piece {
                        tp: PieceType::Bishop,
                        pos: (7 - i, j),
                        col: Color::Black,
                    }),
                    'B' => board.add_piece(Piece {
                        tp: PieceType::Bishop,
                        pos: (7 - i, j),
                        col: Color::White,
                    }),
                    'r' => board.add_piece(Piece {
                        tp: PieceType::Rook,
                        pos: (7 - i, j),
                        col: Color::Black,
                    }),
                    'R' => board.add_piece(Piece {
                        tp: PieceType::Rook,
                        pos: (7 - i, j),
                        col: Color::White,
                    }),
                    'q' => board.add_piece(Piece {
                        tp: PieceType::Queen,
                        pos: (7 - i, j),
                        col: Color::Black,
                    }),
                    'Q' => board.add_piece(Piece {
                        tp: PieceType::Queen,
                        pos: (7 - i, j),
                        col: Color::White,
                    }),
                    'k' => board.add_piece(Piece {
                        tp: PieceType::King,
                        pos: (7 - i, j),
                        col: Color::Black,
                    }),
                    'K' => board.add_piece(Piece {
                        tp: PieceType::King,
                        pos: (7 - i, j),
                        col: Color::White,
                    }),
                    '1'..='8' => j += (c as u8) - b'1',
                    _ => return None,
                }
                j += 1;
            }
            if j != 8 {
                return None;
            }
        }

        let turn = dbg!(sections.next()?.chars()).next()?;

        let castling_rights = dbg!(sections.next())?;
        let en_passant_target = dbg!(sections.next())?;

        let elapsed_half_moves = dbg!(sections.next())?.parse::<u8>().ok()?;
        let moves = dbg!(sections.next())?.parse::<u32>().ok()?;

        board.played_moves = (0..((moves - 1) / 2 * 2 + (turn == 'b') as u32))
            .map(|_| Move::unknown_move())
            .collect();

        if en_passant_target != "-" {
            if !(b'a'..=b'h').contains(&en_passant_target.bytes().nth(0)?) {
                return None;
            }
            if !(b'1'..=b'8').contains(&en_passant_target.bytes().nth(1)?) {
                return None;
            }
            let pos = (
                en_passant_target.bytes().nth(1).unwrap() - b'1',
                en_passant_target.bytes().nth(0).unwrap() - b'a',
            );

            if board.played_moves.is_empty() {
                board.played_moves.push(Move::unknown_move());
                board.played_moves.push(Move::unknown_move());
            }
            if pos.0 != 2 && pos.0 != 5 {
                return None;
            }
            let col = if pos.0 == 3 {
                Color::White
            } else {
                Color::Black
            };
            let start_row = if pos.0 == 2 { 1 } else { 6 };
            let end_row = if pos.0 == 2 { 3 } else { 4 };

            let en_passant_move = Move {
                start_piece: Piece {
                    tp: PieceType::Pawn,
                    pos: (start_row, pos.1),
                    col,
                },
                end_piece: Piece {
                    tp: PieceType::Pawn,
                    pos: (end_row, pos.1),
                    col,
                },
                captured_piece: None,
            };

            *board.played_moves.last_mut().unwrap() = en_passant_move;
        }

        Some(board)
    }

    pub fn to_fen(&self) -> String {
        let mut res = String::new();

        let get_repr = |p: Piece| {
            let ch = p.tp.get_repr();
            if p.col == Color::White {
                ch.to_ascii_uppercase()
            } else {
                ch
            }
        };

        for i in 0..8 {
            let r = 7 - i;
            let mut last: i8 = -1;
            for j in 0..8 {
                let c = j;

                if self.white_piece_bitboard.get((r, c)) {
                    let p = self.white_pieces.iter().find(|p| p.pos == (r, c)).unwrap();
                    let empty_to_add = ((c as i8) - last - 1) as u8;
                    if empty_to_add >= 1 {
                        res.push((empty_to_add + b'0') as char);
                    }
                    res.push(get_repr(*p));
                    last = c as i8;
                } else if self.black_piece_bitboard.get((r, c)) {
                    let p = self.black_pieces.iter().find(|p| p.pos == (r, c)).unwrap();
                    let empty_to_add = ((c as i8) - last - 1) as u8;
                    if empty_to_add >= 1 {
                        res.push((empty_to_add + b'0') as char);
                    }
                    res.push(get_repr(*p));
                    last = c as i8;
                }
            }
            if last != 7 {
                res.push((b'0' as i8 + 7 as i8 - last) as u8 as char);
            }
            if i < 7 {
                res += "/";
            }
        }

        if self.is_white_to_play() {
            res += " w";
        } else {
            res += " b";
        }

        res += " -"; // castling rights

        if self.played_moves.last().is_some_and(|m| {
            m.end_piece.tp == PieceType::Pawn && m.end_piece.pos.0.abs_diff(m.end_piece.pos.0) == 2
        }) {
            let m = self.played_moves.last().unwrap();
            res += " ";
            append_pos_to_string(&mut res, m.end_piece.pos);
        } else {
            res += " -";
        }

        let mut num_plys = 0;

        for m in &self.played_moves {
            if m.start_piece.tp == PieceType::Pawn || m.captured_piece.is_some() {
                num_plys = 0;
            } else {
                num_plys += 1;
            }
        }

        res += " ";
        res += &num_plys.to_string();
        res += " ";
        res += &(1 + self.played_moves.len() / 2).to_string();

        res
    }
}

#[derive(Debug, Clone, Copy)]
pub enum MoveError {
    OutOfBounds,
    SelfCapture,
    SelfCheck,
    NoStartingPiece,
    CapturingEmptySquare,
}

impl std::fmt::Display for MoveError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            MoveError::OutOfBounds => write!(f, "Move is out of bounds"),
            MoveError::SelfCapture => write!(f, "Can't capture your own piece"),
            MoveError::SelfCheck => write!(f, "Move would result in a check on self"),
            MoveError::NoStartingPiece => write!(f, "No piece at the starting position"),
            MoveError::CapturingEmptySquare => write!(f, "Attempting to capture empty square"),
        }
    }
}

impl std::error::Error for MoveError {}

fn pos_to_idx(pos: (u8, u8)) -> u8 {
    assert!(pos.0 < 8 && pos.1 < 8);
    8 * pos.0 + pos.1
}

impl Board {
    pub fn is_legal_move(&self, m: Move) -> bool {
        self.get_legal_moves().iter().contains(&m)
    }

    /// If the provided move is valid, the state of the board will be the result of playing the
    /// given move, otherwise it will return an error corresponding to the way in which the move is
    /// invalid
    pub fn play_move(&mut self, m: Move) -> Result<(), MoveError> {
        if self.is_out_of_bounds(m) {
            return Err(MoveError::OutOfBounds);
        }

        if self.is_self_capture(m) {
            return Err(MoveError::SelfCapture);
        }

        if self.is_self_check(m) {
            return Err(MoveError::SelfCheck);
        }

        if self.is_capturing_empty_square(m) {
            return Err(MoveError::CapturingEmptySquare);
        };

        self.play_unchecked(m);
        Ok(())
    }

    // Undoes last played move, if there is no such move it returns `None`
    pub fn undo_last_move(&mut self) -> Option<()> {
        let mv = *self.played_moves.last()?;
        if mv == Move::unknown_move() {
            return None;
        }
        self.played_moves.pop();

        // have to do this horribleness to deal with ownership
        let curr_player = self.get_curr_player();
        let is_white_to_play = curr_player == Color::White;

        let (curr_player_pieces, opponent_pieces) = if is_white_to_play {
            (&mut self.white_pieces, &mut self.black_pieces)
        } else {
            (&mut self.black_pieces, &mut self.white_pieces)
        };

        let (curr_player_bitboard, opponent_bitboard) = if is_white_to_play {
            (
                &mut self.white_piece_bitboard,
                &mut self.black_piece_bitboard,
            )
        } else {
            (
                &mut self.black_piece_bitboard,
                &mut self.white_piece_bitboard,
            )
        };

        if let Some(captured) = mv.captured_piece {
            opponent_bitboard.set(captured.pos);
            opponent_pieces.insert(captured);
        }

        curr_player_bitboard.clear(mv.end_piece.pos);
        curr_player_bitboard.set(mv.start_piece.pos);

        curr_player_pieces.remove(&mv.end_piece);
        curr_player_pieces.insert(mv.start_piece);

        Some(())
    }

    // Returns an enum representing the current player, that is, White or Black
    pub fn get_curr_player(&self) -> Color {
        match self.played_moves.len() % 2 {
            0 => Color::White,
            1 => Color::Black,
            _ => unreachable!(),
        }
    }

    pub fn is_in_check(&self) -> bool {
        self.get_all_moves()
            .iter()
            .any(|x| x.captured_piece.is_some_and(|c| c.tp == PieceType::King))
    }

    pub fn get_legal_moves(&self) -> Vec<Move> {
        let mut cp = self.clone();
        let (curr_player_pieces, _) = self.get_pieces();
        curr_player_pieces
            .iter()
            .flat_map(|p| self.get_all_moves_for_piece(*p))
            .filter(|m| {
                cp.play_unchecked(*m);
                let res = !cp.is_in_check() && !self.is_self_capture(*m);
                cp.undo_last_move();
                res
            })
            .collect()
    }

    fn is_white_to_play(&self) -> bool {
        self.get_curr_player() == Color::White
    }

    fn is_black_to_play(&self) -> bool {
        self.get_curr_player() == Color::Black
    }

    fn is_self_capture(&self, m: Move) -> bool {
        if let Some(captured_piece) = m.captured_piece {
            let (curr_player_bitboard, _) = self.get_bitboards();
            curr_player_bitboard.get(captured_piece.pos)
        } else {
            false
        }
    }

    fn is_self_check(&self, m: Move) -> bool {
        let mut c: Board = self.clone();

        c.play_unchecked(m);

        c.is_in_check()
    }

    fn is_out_of_bounds(&self, m: Move) -> bool {
        !(m.end_piece.pos.0 < 64 && m.end_piece.pos.1 < 64)
    }

    fn is_capturing_empty_square(&self, m: Move) -> bool {
        if let Some(captured_piece) = m.captured_piece {
            let all_piece_bitboard = self.white_piece_bitboard.combine(self.black_piece_bitboard);
            !all_piece_bitboard.get(captured_piece.pos)
        } else {
            false
        }
    }

    fn get_bitboards(&self) -> (&Bitboard, &Bitboard) {
        let is_white_to_play = self.is_white_to_play();
        if is_white_to_play {
            (&self.white_piece_bitboard, &self.black_piece_bitboard)
        } else {
            (&self.black_piece_bitboard, &self.white_piece_bitboard)
        }
    }

    fn get_bitboards_mut(&mut self) -> (&mut Bitboard, &mut Bitboard) {
        let is_white_to_play = self.is_white_to_play();
        if is_white_to_play {
            (
                &mut self.white_piece_bitboard,
                &mut self.black_piece_bitboard,
            )
        } else {
            (
                &mut self.black_piece_bitboard,
                &mut self.white_piece_bitboard,
            )
        }
    }

    fn get_pieces(&self) -> (&HashSet<Piece>, &HashSet<Piece>) {
        let is_white_to_play = self.is_white_to_play();
        if is_white_to_play {
            (&self.white_pieces, &self.black_pieces)
        } else {
            (&self.black_pieces, &self.white_pieces)
        }
    }

    fn get_pieces_mut(&mut self) -> (&mut HashSet<Piece>, &mut HashSet<Piece>) {
        let is_white_to_play = self.is_white_to_play();
        if is_white_to_play {
            (&mut self.white_pieces, &mut self.black_pieces)
        } else {
            (&mut self.black_pieces, &mut self.white_pieces)
        }
    }

    fn play_unchecked(&mut self, m: Move) {
        // have to do this horribleness to deal with ownership
        let curr_player = self.get_curr_player();
        let is_white_to_play = curr_player == Color::White;

        let (curr_player_pieces, opponent_pieces) = if is_white_to_play {
            (&mut self.white_pieces, &mut self.black_pieces)
        } else {
            (&mut self.black_pieces, &mut self.white_pieces)
        };

        let (curr_player_bitboard, opponent_bitboard) = if is_white_to_play {
            (
                &mut self.white_piece_bitboard,
                &mut self.black_piece_bitboard,
            )
        } else {
            (
                &mut self.black_piece_bitboard,
                &mut self.white_piece_bitboard,
            )
        };

        let moved_piece = *curr_player_pieces
            .iter()
            .find(|x| **x == m.start_piece)
            .unwrap();

        if let Some(captured_piece) = m.captured_piece {
            if let Some(captured_piece) = opponent_pieces.iter().find(|x| **x == captured_piece) {
                let captured_piece = *captured_piece;

                opponent_pieces.retain(|x| *x != captured_piece);
                opponent_bitboard.clear(captured_piece.pos);
            }
        }

        curr_player_pieces.retain(|x| *x != moved_piece);
        curr_player_pieces.insert(m.end_piece);

        curr_player_bitboard.clear(m.start_piece.pos);
        curr_player_bitboard.set(m.end_piece.pos);

        self.played_moves.push(m);
    }

    // helper for regular moves, doesn't work for promotions or castling
    fn get_move(&self, p: Piece, pos: (u8, u8)) -> Option<Move> {
        let (curr_player_bitboard, opponent_bitboard) = self.get_bitboards();
        let (_, opponent_pieces) = self.get_pieces();

        if curr_player_bitboard.get(pos) {
            None
        } else if opponent_bitboard.get(pos) {
            Some(Move {
                start_piece: p,
                end_piece: Piece { pos, ..p },
                captured_piece: Some(*opponent_pieces.iter().find(|x| x.pos == pos).unwrap()),
            })
        } else {
            Some(Move {
                start_piece: p,
                end_piece: Piece { pos, ..p },
                captured_piece: None,
            })
        }
    }

    fn get_rook_moves(&self, p: Piece) -> Vec<Move> {
        let mut res = Vec::new();

        for i in 1..(8 - p.pos.1) {
            let pos = (p.pos.0, p.pos.1 + i);
            if let Some(m) = self.get_move(p, pos) {
                res.push(m);
                if (m.captured_piece.is_some()) {
                    break;
                }
            } else {
                break;
            }
        }
        for i in 1..=p.pos.1 {
            let pos = (p.pos.0, p.pos.1 - i);
            if let Some(m) = self.get_move(p, pos) {
                res.push(m);
                if (m.captured_piece.is_some()) {
                    break;
                }
            } else {
                break;
            }
        }
        for i in 1..(8 - p.pos.0) {
            let pos = (p.pos.0 + i, p.pos.1);
            if let Some(m) = self.get_move(p, pos) {
                res.push(m);
                if (m.captured_piece.is_some()) {
                    break;
                }
            } else {
                break;
            }
        }
        for i in 1..=p.pos.0 {
            let pos = (p.pos.0 - i, p.pos.1);
            if let Some(m) = self.get_move(p, pos) {
                res.push(m);
                if (m.captured_piece.is_some()) {
                    break;
                }
            } else {
                break;
            }
        }

        res
    }

    fn get_bishop_moves(&self, p: Piece) -> Vec<Move> {
        let mut res = Vec::new();

        let left = p.pos.1;
        let right = 8 - p.pos.1;
        let up = 8 - p.pos.0;
        let down = p.pos.0;

        for i in 1..8 {
            if p.pos.0 + i == 8 || p.pos.1 + i == 8 {
                break;
            }
            let pos = (p.pos.0 + i, p.pos.1 + i);
            if let Some(m) = self.get_move(p, pos) {
                res.push(m);
                if m.captured_piece.is_some() {
                    break;
                }
            } else {
                break;
            }
        }

        for i in 1..8 {
            if p.pos.0 + i == 8 || p.pos.1 < i {
                break;
            }
            let pos = (p.pos.0 + i, p.pos.1 - i);
            if let Some(m) = self.get_move(p, pos) {
                res.push(m);
                if m.captured_piece.is_some() {
                    break;
                }
            } else {
                break;
            }
        }

        for i in 1..8 {
            if p.pos.0 < i || p.pos.1 + i == 8 {
                break;
            }
            let pos = (p.pos.0 - i, p.pos.1 + i);
            if let Some(m) = self.get_move(p, pos) {
                res.push(m);
                if m.captured_piece.is_some() {
                    break;
                }
            } else {
                break;
            }
        }

        for i in 1..8 {
            if p.pos.0 < i || p.pos.1 < i {
                break;
            }
            let pos = (p.pos.0 - i, p.pos.1 - i);
            if let Some(m) = self.get_move(p, pos) {
                res.push(m);
                if m.captured_piece.is_some() {
                    break;
                }
            } else {
                break;
            }
        }

        res
    }

    fn get_king_moves(&self, p: Piece) -> Vec<Move> {
        let in_range = |x| (0..8).contains(&x);
        iproduct!(-1..=1, -1..=1)
            // filters out moves that end up outside the board
            .filter(|(dr, dc)| in_range((p.pos.0 as i8) + dr) && in_range((p.pos.1 as i8) + dc))
            .flat_map(|(dr, dc)| {
                self.get_move(
                    p,
                    (((p.pos.0 as i8) + dr) as u8, ((p.pos.1 as i8) + dc) as u8),
                )
            })
            .collect()
    }

    fn get_knight_moves(&self, p: Piece) -> Vec<Move> {
        let in_range = |x| (0..8).contains(&x);
        iproduct!(-2i8..=2i8, -2i8..=2i8)
            .filter(|(dr, dc)| dr.abs() + dc.abs() == 3)
            // filters out moves that end up outside the board
            .filter(|(dr, dc)| in_range((p.pos.0 as i8) + dr) && in_range((p.pos.1 as i8) + dc))
            .flat_map(|(dr, dc)| {
                self.get_move(
                    p,
                    (((p.pos.0 as i8) + dr) as u8, ((p.pos.1 as i8) + dc) as u8),
                )
            })
            .collect()
    }

    fn get_pawn_moves(&self, p: Piece) -> Vec<Move> {
        let in_range = |x| (0..8).contains(&x);
        let dr = if p.col == Color::White { 1 } else { -1 };
        let mut res = Vec::new();
        // if this is our first pawn move, check if one then if two moves are possible
        if (p.pos.0 == 1 && p.col == Color::White) || (p.pos.0 == 6 && p.col == Color::Black) {
            if let Some(m) = self.get_move(p, ((p.pos.0 as i8 + dr) as u8, p.pos.1)) {
                if m.captured_piece.is_none() {
                    res.push(m);
                    if let Some(m) = self.get_move(p, ((p.pos.0 as i8 + 2 * dr) as u8, p.pos.1)) {
                        if m.captured_piece.is_none() {
                            res.push(m);
                        }
                    }
                }
            }
        } else if in_range(p.pos.0 as i8 + dr) {
            if let Some(mut m) = self.get_move(p, ((p.pos.0 as i8 + dr) as u8, p.pos.1)) {
                if m.captured_piece.is_none() {
                    if m.end_piece.pos.0 == 0 || m.end_piece.pos.0 == 7 {
                        // promotion
                        m.end_piece.tp = PieceType::Knight;
                        res.push(m);
                        m.end_piece.tp = PieceType::Bishop;
                        res.push(m);
                        m.end_piece.tp = PieceType::Rook;
                        res.push(m);
                        m.end_piece.tp = PieceType::Queen;
                        res.push(m);
                    } else {
                        res.push(m);
                    }
                }
            }
        }

        if in_range(p.pos.0 as i8 + dr) && in_range(p.pos.1 as i8 + 1) {
            if let Some(mut m) =
                self.get_move(p, ((p.pos.0 as i8 + dr) as u8, (p.pos.1 as i8 + 1) as u8))
            {
                if m.captured_piece.is_some() {
                    if m.end_piece.pos.0 == 0 || m.end_piece.pos.0 == 7 {
                        // promotion
                        m.end_piece.tp = PieceType::Knight;
                        res.push(m);
                        m.end_piece.tp = PieceType::Bishop;
                        res.push(m);
                        m.end_piece.tp = PieceType::Rook;
                        res.push(m);
                        m.end_piece.tp = PieceType::Queen;
                        res.push(m);
                    } else {
                        res.push(m);
                    }
                }
            }
        }

        if in_range(p.pos.0 as i8 + dr) && in_range(p.pos.1 as i8 - 1) {
            if let Some(mut m) =
                self.get_move(p, ((p.pos.0 as i8 + dr) as u8, (p.pos.1 as i8 - 1) as u8))
            {
                if m.captured_piece.is_some() {
                    if m.end_piece.pos.0 == 0 || m.end_piece.pos.0 == 7 {
                        // promotion
                        m.end_piece.tp = PieceType::Knight;
                        res.push(m);
                        m.end_piece.tp = PieceType::Bishop;
                        res.push(m);
                        m.end_piece.tp = PieceType::Rook;
                        res.push(m);
                        m.end_piece.tp = PieceType::Queen;
                        res.push(m);
                    } else {
                        res.push(m);
                    }
                }
            }
        }

        if let Some(last_move) = self.played_moves.last() {
            if last_move.end_piece.tp == PieceType::Pawn
                && last_move
                    .end_piece
                    .pos
                    .0
                    .abs_diff(last_move.start_piece.pos.0)
                    == 2
            {
                if last_move.end_piece.pos.0 == p.pos.0
                    && last_move.end_piece.pos.1.abs_diff(p.pos.1) == 1
                {
                    res.push(Move {
                        start_piece: p,
                        end_piece: Piece {
                            pos: ((p.pos.0 as i8 + dr) as u8, last_move.end_piece.pos.1),
                            ..p
                        },
                        captured_piece: Some(last_move.end_piece),
                    });
                }
            }
        }

        res
    }

    fn get_all_moves_for_piece(&self, p: Piece) -> Vec<Move> {
        match p.tp {
            PieceType::None => panic!(),
            PieceType::Pawn => self.get_pawn_moves(p),
            PieceType::Knight => self.get_knight_moves(p),
            PieceType::Bishop => self.get_bishop_moves(p),
            PieceType::Rook => self.get_rook_moves(p),
            PieceType::Queen => {
                let mut res = self.get_bishop_moves(p);
                res.append(&mut self.get_rook_moves(p));
                res
            }
            PieceType::King => self.get_king_moves(p),
        }
    }

    fn get_legal_moves_for_piece(&self, p: Piece) -> Vec<Move> {
        self.get_all_moves_for_piece(p)
            .iter()
            .filter(|m| !self.is_self_capture(**m))
            .filter(|m| !self.is_self_check(**m))
            .copied()
            .collect()
    }

    fn get_all_moves(&self) -> Vec<Move> {
        let (curr_player_pieces, _) = self.get_pieces();
        curr_player_pieces
            .iter()
            .flat_map(|p| self.get_all_moves_for_piece(*p))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bitboard() {
        let mut b = Bitboard::default();
        b.set((0, 0));
        b.clear((0, 0));
        assert_eq!(b, Bitboard::default());
    }

    #[test]
    fn generating_correct_rook_moves() {
        let mut b = Board::empty();

        let p1 = Piece {
            pos: (0, 0),
            tp: PieceType::Rook,
            col: Color::White,
        };
        b.add_piece(p1);
        assert_eq!(b.get_rook_moves(p1).len(), 14);
        b.add_piece(Piece {
            tp: PieceType::Knight,
            pos: (0, 1),
            col: Color::White,
        });
        assert_eq!(b.get_rook_moves(p1).len(), 7);
        b.add_piece(Piece {
            tp: PieceType::Knight,
            pos: (2, 0),
            col: Color::White,
        });
        assert_eq!(b.get_rook_moves(p1).len(), 1);
        b.add_piece(Piece {
            tp: PieceType::Knight,
            pos: (1, 0),
            col: Color::White,
        });
        assert_eq!(b.get_rook_moves(p1).len(), 0);
    }

    #[test]
    fn generating_correct_bishop_moves() {
        let mut b = Board::empty();

        let white_bishop = Piece {
            pos: (0, 0),
            tp: PieceType::Bishop,
            col: Color::White,
        };
        b.add_piece(white_bishop);
        assert_eq!(b.get_bishop_moves(white_bishop).len(), 7);
        b.add_piece(Piece {
            tp: PieceType::Knight,
            pos: (3, 3),
            col: Color::Black,
        });
        assert_eq!(b.get_bishop_moves(white_bishop).len(), 3);
        assert!(b
            .get_bishop_moves(white_bishop)
            .iter()
            .any(|x| x.captured_piece.is_some()));
        b.add_piece(Piece {
            tp: PieceType::Knight,
            pos: (1, 1),
            col: Color::White,
        });
        assert_eq!(b.get_bishop_moves(white_bishop).len(), 0);
        b = Board::empty();

        let white_bishop = Piece {
            pos: (1, 2),
            tp: PieceType::Bishop,
            col: Color::White,
        };
        b.add_piece(white_bishop);
        dbg!(b.get_bishop_moves(white_bishop));
        assert_eq!(b.get_bishop_moves(white_bishop).len(), 9);
    }

    #[test]
    fn generating_correct_king_moves() {
        let mut b = Board::empty();
        let p1 = Piece {
            tp: PieceType::King,
            pos: (1, 1),
            col: Color::White,
        };
        b.add_piece(p1);
        assert_eq!(b.get_king_moves(p1).len(), 8);
        b.add_piece(Piece {
            tp: PieceType::Bishop,
            pos: (1, 2),
            col: Color::Black,
        });
        assert_eq!(b.get_king_moves(p1).len(), 8);
        assert_eq!(b.get_legal_moves().len(), 6); // the bishop blocks two squares
        b.add_piece(Piece {
            tp: PieceType::Bishop,
            pos: (2, 1),
            col: Color::White,
        });
        assert_eq!(b.get_king_moves(p1).len(), 7);
    }

    #[test]
    fn generating_correct_knight_moves() {
        let mut b = Board::empty();
        let p1 = Piece {
            tp: PieceType::Knight,
            pos: (4, 4),
            col: Color::White,
        };
        b.add_piece(p1);
        assert_eq!(b.get_all_moves_for_piece(p1).len(), 8);
        b.add_piece(Piece {
            tp: PieceType::Bishop,
            pos: (5, 6),
            col: Color::White,
        });
        assert_eq!(b.get_all_moves_for_piece(p1).len(), 7);
        b.add_piece(Piece {
            tp: PieceType::Bishop,
            pos: (6, 5),
            col: Color::Black,
        });
        assert_eq!(b.get_all_moves_for_piece(p1).len(), 7);
        assert!(b
            .get_all_moves_for_piece(p1)
            .iter()
            .any(|m| m.captured_piece.is_some()));
    }

    #[test]
    fn generating_correct_pawn_moves() {
        let mut b = Board::empty();
        let p1 = Piece {
            tp: PieceType::Pawn,
            pos: (1, 0),
            col: Color::White,
        };
        b.add_piece(p1);
        assert_eq!(dbg!(b.get_all_moves_for_piece(p1)).len(), 2);
        for m in b.get_all_moves_for_piece(p1) {
            b.play_move(m);
            dbg!(&b.white_pieces);
            assert_eq!(
                dbg!(b.get_all_moves_for_piece(*b.white_pieces.iter().next().unwrap())).len(),
                1
            );
            b.undo_last_move();
        }
        b.add_piece(Piece {
            tp: PieceType::Pawn,
            pos: (3, 1),
            col: Color::Black,
        });
        b.play_move(
            *b.get_legal_moves()
                .iter()
                .find(|m| m.end_piece.pos.0.abs_diff(m.start_piece.pos.0) == 2)
                .unwrap(),
        );
        assert_eq!(b.get_legal_moves().len(), 2);
        assert!(b
            .get_legal_moves()
            .iter()
            .any(|m| m.captured_piece.is_some()));
    }

    #[test]
    fn basic_undo() {
        let mut b = Board::empty();
        b.add_piece(Piece {
            tp: PieceType::King,
            pos: (0, 0),
            col: Color::White,
        });
        b.add_piece(Piece {
            tp: PieceType::Bishop,
            pos: (0, 1),
            col: Color::White,
        });

        let mut cp = b.clone();
        for mv in b.get_legal_moves() {
            cp.play_move(mv);
            cp.undo_last_move().unwrap();
            assert_eq!(cp, b);
        }
    }

    #[test]
    fn undo_possible_capture() {
        let mut b = Board::empty();
        b.add_piece(Piece {
            tp: PieceType::King,
            pos: (0, 0),
            col: Color::White,
        });
        b.add_piece(Piece {
            tp: PieceType::Bishop,
            pos: (0, 1),
            col: Color::Black,
        });

        let mut cp = b.clone();
        for mv in b.get_legal_moves() {
            cp.play_move(mv).unwrap();
            cp.undo_last_move().unwrap();
            assert_eq!(cp, b);
        }
    }

    #[test]
    fn basic_discoveries() {
        let mut b = Board::empty();
        let white_king = Piece {
            tp: PieceType::King,
            pos: (0, 0),
            col: Color::White,
        };
        b.add_piece(white_king);
        let white_rook = Piece {
            tp: PieceType::Rook,
            pos: (0, 1),
            col: Color::White,
        };
        b.add_piece(white_rook);
        b.add_piece(Piece {
            tp: PieceType::Rook,
            pos: (0, 2),
            col: Color::Black,
        });
        b.add_piece(Piece {
            tp: PieceType::King,
            pos: (0, 3),
            col: Color::Black,
        });

        assert_eq!(b.get_king_moves(white_king).len(), 2);
        assert_eq!(b.get_rook_moves(white_rook).len(), 8);
        assert_eq!(dbg!(b.get_legal_moves()).len(), 3);
    }

    #[test]
    fn rejecting_obviously_wrong_fen() {
        assert!(Board::from_fen("").is_none());
        assert!(Board::from_fen("a").is_none());
        assert!(Board::from_fen("/////// - - 0 1").is_none());
        assert!(Board::from_fen("8/8/8/8/8/8/8/ w - 0 1").is_none());
    }

    #[test]
    fn accepting_correct_fen() {
        assert_eq!(
            Board::from_fen("8/8/8/8/8/8/8/8 w - - 0 1").unwrap(),
            Board::empty()
        );
    }

    #[test]
    fn correct_default_position() {
        let mut b = Board::empty();
        b.add_piece(Piece {
            tp: PieceType::Rook,
            pos: (0, 0),
            col: Color::White,
        });
        b.add_piece(Piece {
            tp: PieceType::Rook,
            pos: (0, 7),
            col: Color::White,
        });
        b.add_piece(Piece {
            tp: PieceType::Knight,
            pos: (0, 1),
            col: Color::White,
        });
        b.add_piece(Piece {
            tp: PieceType::Knight,
            pos: (0, 6),
            col: Color::White,
        });
        b.add_piece(Piece {
            tp: PieceType::Bishop,
            pos: (0, 2),
            col: Color::White,
        });
        b.add_piece(Piece {
            tp: PieceType::Bishop,
            pos: (0, 5),
            col: Color::White,
        });
        b.add_piece(Piece {
            tp: PieceType::Queen,
            pos: (0, 3),
            col: Color::White,
        });
        b.add_piece(Piece {
            tp: PieceType::King,
            pos: (0, 4),
            col: Color::White,
        });

        b.add_piece(Piece {
            tp: PieceType::Rook,
            pos: (7, 0),
            col: Color::Black,
        });
        b.add_piece(Piece {
            tp: PieceType::Rook,
            pos: (7, 7),
            col: Color::Black,
        });
        b.add_piece(Piece {
            tp: PieceType::Knight,
            pos: (7, 1),
            col: Color::Black,
        });
        b.add_piece(Piece {
            tp: PieceType::Knight,
            pos: (7, 6),
            col: Color::Black,
        });
        b.add_piece(Piece {
            tp: PieceType::Bishop,
            pos: (7, 2),
            col: Color::Black,
        });
        b.add_piece(Piece {
            tp: PieceType::Bishop,
            pos: (7, 5),
            col: Color::Black,
        });
        b.add_piece(Piece {
            tp: PieceType::Queen,
            pos: (7, 3),
            col: Color::Black,
        });
        b.add_piece(Piece {
            tp: PieceType::King,
            pos: (7, 4),
            col: Color::Black,
        });

        for i in 0..8 {
            b.add_piece(Piece {
                tp: PieceType::Pawn,
                pos: (1, i),
                col: Color::White,
            });
            b.add_piece(Piece {
                tp: PieceType::Pawn,
                pos: (6, i),
                col: Color::Black,
            });
        }

        assert_eq!(b, Board::default());
        assert_eq!(b.get_legal_moves().len(), 20);
    }

    #[test]
    fn generating_correct_fen() {
        assert_eq!(Board::empty().to_fen(), "8/8/8/8/8/8/8/8 w - - 0 1");
        assert_eq!(
            Board::from_fen(&Board::default().to_fen()).unwrap(),
            Board::default()
        );
    }

    #[test]
    fn fen_en_passant() {
        let mut b =
            Board::from_fen("rnbqkbnr/ppp2ppp/4p3/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3")
                .unwrap();
        b.undo_last_move();
        assert_eq!(
            b,
            Board::from_fen("rnbqkbnr/pppp1ppp/4p3/4P3/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2")
                .unwrap()
        );
    }

    fn perft(b: &mut Board, depth: usize) -> u64 {
        if depth == 0 {
            1
        } else {
            let mut res = 0;
            for mv in b.get_legal_moves() {
                b.play_unchecked(mv);
                res += perft(b, depth - 1);
                b.undo_last_move();
            }
            res
        }
    }

    fn perft_dbg(b: &mut Board, depth: usize) -> u64 {
        if depth == 0 {
            1
        } else {
            let mut res = 0;
            let mut dbg_out = Vec::new();
            for mv in b.get_legal_moves() {
                b.play_unchecked(mv);
                let o = perft(b, depth - 1);
                let mut mv_str = String::with_capacity(5);
                append_pos_to_string(&mut mv_str, mv.start_piece.pos);
                append_pos_to_string(&mut mv_str, mv.end_piece.pos);
                if mv.end_piece.tp != mv.start_piece.tp {
                    mv_str.push(mv.end_piece.tp.get_repr());
                }

                dbg_out.push(mv_str + ": " + &o.to_string());
                b.undo_last_move();
            }
            dbg_out.sort();
            dbg!(dbg_out.iter().join(" "));
            res
        }
    }

    #[test]
    fn perft_starting_pos() {
        let mut b = Board::default();
        assert_eq!(perft(&mut b, 1), 20);
        assert_eq!(perft(&mut b, 2), 400);
        assert_eq!(perft(&mut b, 3), 8902);
        assert_eq!(perft(&mut b, 4), 197281);

        // no castling support yet
        // assert_eq!(perft(&mut b, 5), 4865609);
    }

    #[test]
    fn perft_1() {
        // values taken from stockfish
        let mut b = Board::from_fen("8/KR4rk/8/8/8/8/8/8 w - - 0 1").unwrap();
        assert_eq!(perft(&mut b, 1), 9);
        assert_eq!(perft(&mut b, 2), 65);
        assert_eq!(perft(&mut b, 3), 717);
        assert_eq!(perft(&mut b, 4), 8504);
        assert_eq!(perft(&mut b, 5), 113900);
        assert_eq!(perft(&mut b, 6), 1628738);
    }

    #[test]
    fn perft_2() {
        let mut b = Board::from_fen("8/KR4rk/8/5N2/8/8/8/8 w - - 0 1").unwrap();
        assert_eq!(perft(&mut b, 1), 17);
        assert_eq!(perft(&mut b, 2), 122);
        assert_eq!(perft(&mut b, 3), 1909);
        assert_eq!(perft(&mut b, 4), 19978);
        assert_eq!(perft(&mut b, 5), 355397);
    }

    #[test]
    fn perft_3() {
        let mut b = Board::from_fen("8/KR4rk/3n4/2N5/2B5/2n1bQ2/8/4R3 w - - 0 1").unwrap();
        assert_eq!(perft(&mut b, 1), 48);
        assert_eq!(perft(&mut b, 2), 1277);
        assert_eq!(perft(&mut b, 3), 53702);
        assert_eq!(perft(&mut b, 4), 1373576);
    }

    #[test]
    fn perft_4() {
        let mut b = Board::from_fen("8/KR4rk/3n4/2N5/2B5/2n1bQ2/5P2/4R3 w - - 0 1").unwrap();
        assert_eq!(perft(&mut b, 1), 47);
        assert_eq!(perft(&mut b, 2), 1201);
        assert_eq!(perft(&mut b, 3), 50014);
        assert_eq!(perft(&mut b, 4), 1239643);
    }

    #[test]
    fn perft_5() {
        let mut b = Board::from_fen("8/KR4rk/3n4/2N5/2B5/2n1PQ2/8/4R3 b - - 0 2").unwrap();
        assert_eq!(perft(&mut b, 1), 24);
        assert_eq!(perft(&mut b, 2), 1053);
        assert_eq!(perft(&mut b, 3), 20940);
        assert_eq!(perft(&mut b, 4), 928281);
    }

    #[test]
    fn perft_6() {
        let mut b = Board::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1").unwrap();
        assert_eq!(perft(&mut b, 1), 14);
        assert_eq!(perft(&mut b, 2), 191);
        assert_eq!(perft(&mut b, 3), 2812);
        assert_eq!(perft(&mut b, 4), 43238);
        assert_eq!(perft(&mut b, 5), 674624);
    }

    #[test]
    fn perft_7() {
        let mut b = Board::from_fen("8/2p5/3p4/1P5r/KR3p1k/8/4P1P1/8 b - - 1 2").unwrap();
        assert_eq!(perft(&mut b, 1), 15);
        assert_eq!(perft(&mut b, 2), 224);
        assert_eq!(perft(&mut b, 3), 3394);
        assert_eq!(perft(&mut b, 4), 52943);
        assert_eq!(perft(&mut b, 5), 868162);
    }

    #[test]
    fn perft_8() {
        let mut b = Board::from_fen("8/2p5/3p4/1P5r/KR3p2/6k1/4P1P1/8 w - - 2 3").unwrap();
        dbg!(b.get_legal_moves());
        assert_eq!(perft(&mut b, 1), 13);
        assert_eq!(perft(&mut b, 2), 271);
        assert_eq!(perft(&mut b, 3), 4084);
        assert_eq!(perft(&mut b, 4), 76169);
        assert_eq!(perft(&mut b, 5), 1233754);
    }

    #[test]
    fn perft_9() {
        let mut b = Board::from_fen("2r5/1Pp5/3p4/8/KR3p2/6k1/4P1P1/8 w - - 1 5").unwrap();
        dbg!(b.get_legal_moves());
        assert_eq!(perft(&mut b, 1), 23);
        assert_eq!(perft(&mut b, 2), 324);
        assert_eq!(perft(&mut b, 3), 7164);
        assert_eq!(perft(&mut b, 4), 106510);
        assert_eq!(perft(&mut b, 5), 2372502);
    }
}
