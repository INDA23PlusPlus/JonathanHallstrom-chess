#[derive(Debug, Default, Clone, Copy)]
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

#[derive(Debug, Default, Clone, Copy)]
pub struct Piece {
    tp: PieceType,
    pos: (u8, u8),
}

#[derive(Debug, Default, Clone, Copy)]
pub struct Move {
    start_pos: (u8, u8),
    end_pos: (u8, u8),
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
}

impl Board {
    pub fn is_white_to_play(&self) -> bool {
        return self.played_moves.len() % 2 == 0;
    }

    pub fn is_black_to_play(&self) -> bool {
        return !self.is_white_to_play();
    }

    fn is_self_capture(&self, m: Move) -> bool {
        let target_square: u8 = 8 * m.end_pos.0 + m.end_pos.1;
        if self.is_white_to_play() {
            return (self.white_piece_bitboard >> target_square & 1) == 1;
        } else {
            return (self.black_piece_bitboard >> target_square & 1) == 1;
        }
    }

    fn is_self_check(&self, m: Move) -> bool {
        unimplemented!()
    }

    fn is_out_of_bounds(&self, m: Move) -> bool {
        return m.end_pos.0 < 64 && m.end_pos.1 < 64;
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

        unimplemented!()
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
