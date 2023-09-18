#![allow(unused)]
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
    start_pos: (u8, u8),
    end_pos: (u8, u8),
    captured_piece: Option<Piece>,
}

#[derive(Debug)]
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
    pub fn get_curr_player(&self) -> Color {
        match self.played_moves.len() % 2 {
            0 => Color::White,
            1 => Color::Black,
            _ => unreachable!(),
        }
    }

    pub fn is_white_to_play(&self) -> bool {
        self.get_curr_player() == Color::White
    }

    pub fn is_black_to_play(&self) -> bool {
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
        unimplemented!()
    }

    fn is_out_of_bounds(&self, m: Move) -> bool {
        !(m.end_pos.0 < 64 && m.end_pos.1 < 64)
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
        }

        let curr_player = self.get_curr_player();
        let is_white_to_play = self.is_white_to_play();

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
            .find(|x| x.pos == m.start_pos)
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
            pos: m.end_pos,
            col: curr_player,
        });

        curr_player_bitboard.clear(m.start_pos);
        curr_player_bitboard.set(m.end_pos);

        self.played_moves.push(m);

        Ok(())
    }

    fn get_rook_moves(&self, p: Piece) -> Vec<Move> {
        let mut res = Vec::new();

        let (curr_player_bitboard, opponent_bitboard) = self.get_bitboards();
        let (_, opponent_pieces) = self.get_pieces();

        let check_pos = |pos| {
            if curr_player_bitboard.get(pos) {
                None
            } else if opponent_bitboard.get(pos) {
                Some(Move {
                    start_pos: p.pos,
                    end_pos: pos,
                    captured_piece: Some(*opponent_pieces.iter().find(|x| x.pos == pos).unwrap()),
                })
            } else {
                Some(Move {
                    start_pos: p.pos,
                    end_pos: pos,
                    captured_piece: None,
                })
            }
        };

        for i in 1..(8 - p.pos.1) {
            let pos = (p.pos.0, p.pos.1 + i);
            dbg!(pos);
            if let Some(m) = check_pos(pos) {
                res.push(m);
            } else {
                break;
            }
        }
        for i in 1..=p.pos.1 {
            let pos = (p.pos.0, p.pos.1 - i);
            dbg!(pos);
            if let Some(m) = check_pos(pos) {
                res.push(m);
            } else {
                break;
            }
        }
        for i in 1..(8 - p.pos.0) {
            let pos = (p.pos.0 + i, p.pos.1);
            dbg!(pos);
            if let Some(m) = check_pos(pos) {
                res.push(m);
            } else {
                break;
            }
        }
        for i in 1..=p.pos.0 {
            let pos = (p.pos.0 - i, p.pos.1);
            dbg!(pos);
            if let Some(m) = check_pos(pos) {
                res.push(m);
            } else {
                break;
            }
        }

        res
    }

    fn get_legal_moves_for_piece(&self, p: Piece) -> Vec<Move> {
        match p.tp {
            PieceType::None => panic!(),
            PieceType::Pawn => unimplemented!(),
            PieceType::Knight => unimplemented!(),
            PieceType::Bishop => unimplemented!(),
            PieceType::Rook => self.get_rook_moves(p),
            PieceType::Queen => unimplemented!(),
            PieceType::King => unimplemented!(),
        }
    }

    pub fn get_legal_moves(&self) -> Vec<Move> {
        let (curr_player_pieces, _) = self.get_pieces();
        curr_player_pieces
            .iter()
            .flat_map(|p| self.get_legal_moves_for_piece(*p))
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
        let mut b = Board {
            white_pieces: vec![Piece {
                pos: (0, 0),
                tp: PieceType::Rook,
                col: Color::White,
            }],
            black_pieces: vec![],
            white_piece_bitboard: Bitboard(1),
            black_piece_bitboard: Bitboard(0),
            played_moves: vec![],
        };

        assert_eq!(b.get_rook_moves(b.white_pieces[0]).len(), 14);
        b.add_piece(Piece {
            tp: PieceType::Knight,
            pos: (0, 1),
            col: Color::White,
        })
    }
}
