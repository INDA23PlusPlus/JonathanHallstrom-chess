#![allow(unused)]
use std::convert::identity;

use itertools::*;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum Color {
    #[default]
    White,
    Black,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Piece {
    tp: PieceType,
    pos: (u8, u8),
    col: Color,
}

#[derive(Debug, Default, Clone, Copy)]
pub struct Move {
    start_piece: Piece,
    end_piece: Piece,
    captured_piece: Option<Piece>,
}

#[derive(Debug, Clone)]
pub struct Board {
    white_pieces: Vec<Piece>,
    black_pieces: Vec<Piece>,

    white_piece_bitboard: Bitboard,
    black_piece_bitboard: Bitboard,

    played_moves: Vec<Move>,
}

impl Board {
    pub fn empty() -> Board {
        Board {
            white_pieces: vec![],
            black_pieces: vec![],
            white_piece_bitboard: Bitboard(0),
            black_piece_bitboard: Bitboard(0),
            played_moves: vec![],
        }
    }

    fn add_piece(&mut self, p: Piece) {
        match p.col {
            Color::White => {
                self.white_pieces.push(p);
                self.white_piece_bitboard.set(p.pos);
            }
            Color::Black => {
                self.black_pieces.push(p);
                self.black_piece_bitboard.set(p.pos);
            }
        }
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
    // Returns an enum representing the current player, that is, White or Black
    pub fn get_curr_player(&self) -> Color {
        match self.played_moves.len() % 2 {
            0 => Color::White,
            1 => Color::Black,
            _ => unreachable!(),
        }
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

        if c.get_all_moves().iter().any(|x| {
            if let Some(c) = x.captured_piece {
                c.tp == PieceType::King
            } else {
                false
            }
        }) {
            true
        } else {
            false
        }
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

    fn get_pieces(&self) -> (&Vec<Piece>, &Vec<Piece>) {
        let is_white_to_play = self.is_white_to_play();
        if is_white_to_play {
            (&self.white_pieces, &self.black_pieces)
        } else {
            (&self.black_pieces, &self.white_pieces)
        }
    }

    fn get_pieces_mut(&mut self) -> (&mut Vec<Piece>, &mut Vec<Piece>) {
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
        curr_player_pieces.push(Piece {
            tp: moved_piece.tp,
            pos: m.end_piece.pos,
            col: curr_player,
        });

        curr_player_bitboard.clear(m.start_piece.pos);
        curr_player_bitboard.set(m.end_piece.pos);

        self.played_moves.push(m);
    }

    // If the provided move is valid, the state of the board will be the result of playing the
    // given move, otherwise it will return an error corresponding to the way in which the move is
    // invalid
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

        Ok(self.play_unchecked(m))
    }

    // Undoes last played move, if there is no such move it returns `None`
    pub fn undo_last_move(&mut self) -> Option<()> {
        let mv = self.played_moves.last()?;

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
            opponent_pieces.push(captured);
        }

        curr_player_bitboard.clear(mv.end_piece.pos);
        curr_player_bitboard.set(mv.start_piece.pos);

        *curr_player_pieces
            .iter_mut()
            .find(|p| **p == mv.start_piece)
            .unwrap() = mv.end_piece;

        Some(())
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
                end_piece: Piece {
                    tp: p.tp,
                    pos,
                    col: p.col,
                },
                captured_piece: Some(*opponent_pieces.iter().find(|x| x.pos == pos).unwrap()),
            })
        } else {
            Some(Move {
                start_piece: p,
                end_piece: Piece {
                    tp: p.tp,
                    pos,
                    col: p.col,
                },
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
        iproduct!(-1..=1, -1..=1)
            .filter(|(dr, dc)| {
                0 <= (p.pos.0 as i8) + dr
                    && (p.pos.0 as i8) + dr < 8
                    && 0 <= (p.pos.1 as i8) + dc
                    && (p.pos.1 as i8) + dc < 8
            })
            .map(|(dr, dc)| {
                self.get_move(
                    p,
                    (((p.pos.0 as i8) + dr) as u8, ((p.pos.1 as i8) + dc) as u8),
                )
            })
            .filter_map(identity)
            .collect()
    }

    fn get_all_moves_for_piece(&self, p: Piece) -> Vec<Move> {
        match p.tp {
            PieceType::None => panic!(),
            PieceType::Pawn => unimplemented!(),
            PieceType::Knight => unimplemented!(),
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

    pub fn get_legal_moves(&self) -> Vec<Move> {
        let (curr_player_pieces, _) = self.get_pieces();
        curr_player_pieces
            .iter()
            .flat_map(|p| self.get_legal_moves_for_piece(*p))
            .collect()
    }
}

// this is terrible testing but I wan't to get this done ASAP
// and it's just gonna have to do for now
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

        b.add_piece(Piece {
            pos: (0, 0),
            tp: PieceType::Rook,
            col: Color::White,
        });
        assert_eq!(b.get_rook_moves(b.white_pieces[0]).len(), 14);
        b.add_piece(Piece {
            tp: PieceType::Knight,
            pos: (0, 1),
            col: Color::White,
        });
        assert_eq!(b.get_rook_moves(b.white_pieces[0]).len(), 7);
        b.add_piece(Piece {
            tp: PieceType::Knight,
            pos: (2, 0),
            col: Color::White,
        });
        assert_eq!(b.get_rook_moves(b.white_pieces[0]).len(), 1);
        b.add_piece(Piece {
            tp: PieceType::Knight,
            pos: (1, 0),
            col: Color::White,
        });
        assert_eq!(b.get_rook_moves(b.white_pieces[0]).len(), 0);
    }

    #[test]
    fn generating_correct_bishop_moves() {
        let mut b = Board::empty();

        b.add_piece(Piece {
            pos: (0, 0),
            tp: PieceType::Bishop,
            col: Color::White,
        });
        assert_eq!(b.get_bishop_moves(b.white_pieces[0]).len(), 7);
        b.add_piece(Piece {
            tp: PieceType::Knight,
            pos: (3, 3),
            col: Color::Black,
        });
        // dbg!(b.get_bishop_moves(b.white_pieces[0]));
        assert_eq!(b.get_bishop_moves(b.white_pieces[0]).len(), 3);
        assert!(b
            .get_bishop_moves(b.white_pieces[0])
            .iter()
            .find(|x| x.captured_piece.is_some())
            .is_some());
        b.add_piece(Piece {
            tp: PieceType::Knight,
            pos: (1, 1),
            col: Color::White,
        });
        assert_eq!(b.get_bishop_moves(b.white_pieces[0]).len(), 0);
        b = Board::empty();

        b.add_piece(Piece {
            pos: (1, 2),
            tp: PieceType::Bishop,
            col: Color::White,
        });
        dbg!(b.get_bishop_moves(b.white_pieces[0]));
        assert_eq!(b.get_bishop_moves(b.white_pieces[0]).len(), 9);
    }

    #[test]
    fn basic_discoveries() {
        let mut b = Board::empty();
        b.add_piece(Piece {
            tp: PieceType::King,
            pos: (0, 0),
            col: Color::White,
        });
        b.add_piece(Piece {
            tp: PieceType::Rook,
            pos: (0, 1),
            col: Color::White,
        });
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

        /*
        k
        r
        R
        K
        */
        assert_eq!(b.get_king_moves(b.white_pieces[0]).len(), 2);
        assert_eq!(b.get_rook_moves(b.white_pieces[1]).len(), 8);
        assert_eq!(dbg!(b.get_legal_moves()).len(), 3);
    }
}
