use std::fmt::write;

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

pub struct Row {}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Piece {
    tp: PieceType,
    pos: (u8, u8),
}

#[derive(Debug, Default, Clone, Copy)]
pub struct Move {
    start_pos: (u8, u8),
    end_pos: (u8, u8),
    captured_piece: Option<Piece>,
}

#[derive(Debug, Default)]
pub struct Board {
    white_pieces: Vec<Piece>,
    black_pieces: Vec<Piece>,

    white_piece_bitboard: u64,
    black_piece_bitboard: u64,

    played_moves: Vec<Move>,
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

impl Board {
    pub fn is_white_to_play(&self) -> bool {
        return self.played_moves.len() % 2 == 0;
    }

    pub fn is_black_to_play(&self) -> bool {
        return !self.is_white_to_play();
    }

    // only for debugging purposes, all moves played should come from get_legal_moves
    fn is_self_capture(&self, m: Move) -> bool {
        if let Some(captured_piece) = m.captured_piece {
            let target_square: u8 = 8 * captured_piece.pos.0 + captured_piece.pos.1;
            if self.is_white_to_play() {
                return (self.white_piece_bitboard >> target_square & 1) == 1;
            } else {
                return (self.black_piece_bitboard >> target_square & 1) == 1;
            }
        } else {
            return false;
        }
    }

    // only for debugging purposes, all moves played should come from get_legal_moves
    fn is_self_check(&self, m: Move) -> bool {
        unimplemented!()
    }

    // only for debugging purposes, all moves played should come from get_legal_moves
    fn is_out_of_bounds(&self, m: Move) -> bool {
        return !(m.end_pos.0 < 64 && m.end_pos.1 < 64);
    }

    // only for debugging purposes, all moves played should come from get_legal_moves
    fn is_capturing_empty_square(&self, m: Move) -> bool {
        if let Some(captured_piece) = m.captured_piece {
            let target_square: u8 = 8 * captured_piece.pos.0 + captured_piece.pos.1;
            let all_piece_bitboard = self.white_piece_bitboard | self.black_piece_bitboard;
            return (all_piece_bitboard >> target_square & 1) == 0;
        } else {
            return false;
        }
    }

    // if the given move is legal
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

        let (curr_player_pieces, opponent_pieces) = if self.is_white_to_play() {
            (&mut self.white_pieces, &mut self.black_pieces)
        } else {
            (&mut self.black_pieces, &mut self.white_pieces)
        };

        let moved_piece = *curr_player_pieces
            .iter()
            .find(|x| x.pos == m.start_pos)
            .unwrap();


        
        if let Some(captured_piece) = m.captured_piece {
            if let Some(captured_piece) = opponent_pieces.iter().find(|x| **x == captured_piece) {
                let captured_piece = *captured_piece;

                opponent_pieces.retain(|x| *x != captured_piece);
            }
        }

        curr_player_pieces.retain(|x| *x != moved_piece);
        curr_player_pieces.push(Piece {
            tp: moved_piece.tp,
            pos: m.end_pos,
        });

        self.played_moves.push(m);

        Ok(())
    }

    pub fn get_legal_moves(&self) -> Vec<Move> {
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {}
}
