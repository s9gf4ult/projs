use std::io;
use std::time::{Duration, SystemTime};
use termion::{async_stdin, color, event::Key, raw::IntoRawMode};

const BOARD_WIDTH: usize = 10;
const BOARD_HEIGHT: usize = 20;

#[derive(Debug, Clone)]
struct Piece {
    x: i32,
    y: i32,
    shape: Vec<Vec<bool>>,
    color: (u8, u8, u8),
}

impl Piece {
    fn new(x: i32, y: i32, shape: Vec<Vec<bool>>, color: (u8, u8, u8)) -> Self {
        Piece { x, y, shape, color }
    }

    fn rotate(&mut self) {
        let mut new_shape = vec![];
        for col in 0..self.shape[0].len() {
            let mut new_row = vec![];
            for row in self.shape.iter().rev() {
                new_row.push(row[col]);
            }
            new_shape.push(new_row);
        }
        self.shape = new_shape;
    }

    fn bounding_box(&self) -> (i32, i32, i32, i32) {
        let min_x = self.x;
        let max_x = self.x + self.shape[0].len() as i32 - 1;
        let min_y = self.y;
        let max_y = self.y + self.shape.len() as i32 - 1;
        (min_x, max_x, min_y, max_y)
    }
}

struct Game {
    board: Vec<Vec<bool>>,
    current_piece: Option<Piece>,
    score: u32,
}

impl Game {
    fn new() -> Self {
        let board = vec![vec![false; BOARD_WIDTH]; BOARD_HEIGHT];
        Game {
            board,
            current_piece: None,
            score: 0,
        }
    }

    fn spawn_piece(&mut self) {
        let piece = match rand::random::<usize>() % 7 {
            0 => create_i(),
            1 => create_o(),
            2 => create_t(),
            3 => create_l(),
            4 => create_j(),
            5 => create_s(),
            6 => create_z(),
            _ => panic!("Unexpected piece index"),
        };
        self.current_piece = Some(piece);
    }

    fn create_new_board(&self) -> Vec<Vec<bool>> {
        let mut new_board = vec![vec![false; BOARD_WIDTH]; BOARD_HEIGHT];
        for (y, row) in self.board.iter().enumerate() {
            for (x, val) in row.iter().enumerate() {
                if *val {
                    new_board[y][x] = true;
                }
            }
        }
        new_board
    }

    fn merge_piece(&mut self) {
        if let Some(piece) = &self.current_piece {
            for y in 0..piece.shape.len() as i32 {
                for x in 0..piece.shape[0].len() as i32 {
                    if piece.shape[y as usize][x as usize] {
                        self.board[(y + piece.y) as usize][(x + piece.x) as usize] = true;
                    }
                }
            }
        }
    }

    fn clear_lines(&mut self) -> u32 {
        let mut lines_cleared = 0u32;
        for y in (0..BOARD_HEIGHT).rev() {
            if self.board[y].iter().all(|&cell| cell) {
                self.board.swap_remove(y);
                self.board.insert(0, vec![false; BOARD_WIDTH]);
                lines_cleared += 1;
            }
        }
        match lines_cleared {
            1 => 100,
            2 => 500,
            3 => 1200,
            4 => 2000,
            _ => 0,
        }
    }

    fn move_down(&mut self) -> bool {
        if let Some(piece) = &self.current_piece {
            let (min_y, max_y) = (piece.y, piece.y + piece.shape.len() as i32 - 1);
            if min_y > 0 && max_y < BOARD_HEIGHT as i32 - 1 {
                return true;
            }
        }
        false
    }

    fn move_left(&mut self) -> bool {
        if let Some(piece) = &self.current_piece {
            let (min_x, max_x, min_y, max_y) = {
                let bb = piece.bounding_box();
                (bb.0, bb.1, bb.2, bb.3)
            };
            for y in min_y..=max_y as i32 {
                for x in 0..=2 {
                    let new_x = piece.x - x;
                    if new_x < 0 || self.board[(y + piece.y) as usize][(new_x) as usize] {
                        return false;
                    }
                }
            }
            piece.x -= 1;
            true
        } else {
            false
        }
    }

    fn move_right(&mut self) -> bool {
        if let Some(piece) = &self.current_piece {
            let (max_x, min_y, max_y) = {
                let bb = piece.bounding_box();
                (bb.1, bb.2, bb.3)
            };
            for y in min_y..=max_y as i32 {
                for x in 0..=2 {
                    let new_x = piece.x + x;
                    if new_x >= BOARD_WIDTH as i32
                        || self.board[(y + piece.y) as usize][(new_x) as usize]
                    {
                        return false;
                    }
                }
            }
            piece.x += 1;
            true
        } else {
            false
        }
    }

    fn rotate(&mut self) -> bool {
        if let Some(piece) = &mut self.current_piece {
            let backup_shape = piece.shape.clone();
            piece.rotate();
            let (min_x, max_x, min_y, max_y) = piece.bounding_box();
            for y in min_y..=max_y as i32 {
                for x in min_x..=max_x as i32 {
                    if x < 0 || x >= BOARD_WIDTH as i32 || y >= BOARD_HEIGHT as i32 {
                        piece.shape = backup_shape;
                        return false;
                    }
                    if self.board[y as usize][x as usize]
                        && piece.shape[(y - piece.y) as usize][(x - piece.x) as usize]
                    {
                        piece.shape = backup_shape;
                        return false;
                    }
                }
            }
            true
        } else {
            false
        }
    }

    fn draw(&self, stdout: &mut io::StdoutLock) {
        // Clear the screen
        stdout.write(termion::clear::All.as_bytes()).unwrap();

        // Draw the board
        for y in 0..BOARD_HEIGHT as i32 {
            for x in 0..BOARD_WIDTH as i32 {
                if self.board[y as usize][x as usize] {
                    write!(
                        stdout,
                        "{}#{}",
                        color::Fg(color::Rgb(0, 255, 255)),
                        color::Reset
                    )
                    .unwrap();
                } else {
                    write!(stdout, " ").unwrap();
                }
                write!(stdout, " ").unwrap();
            }
            writeln!(stdout).unwrap();
        }

        // Draw the current piece
        if let Some(piece) = &self.current_piece {
            for y in 0..piece.shape.len() as i32 {
                for x in 0..piece.shape[0].len() as i32 {
                    if piece.shape[y as usize][x as usize] {
                        write!(
                            stdout,
                            "{}#{}",
                            color::Fg(color::Rgb(piece.color.0, piece.color.1, piece.color.2)),
                            color::Reset
                        )
                        .unwrap();
                    } else {
                        write!(stdout, " ").unwrap();
                    }
                    write!(stdout, " ").unwrap();
                }
                writeln!(stdout).unwrap();
            }
        }

        // Draw the score
        write!(stdout, "Score: {}\r", self.score).unwrap();
    }
}

fn create_i() -> Piece {
    let shape = vec![vec![true, true, true, true], vec![false; 4]];
    Piece::new(
        (BOARD_WIDTH as i32) / 2 - 1,
        BOARD_HEIGHT as i32 - 4,
        shape,
        (0, 255, 255),
    )
}

fn create_o() -> Piece {
    let shape = vec![vec![true, true], vec![true, true]];
    Piece::new(
        (BOARD_WIDTH as i32) / 2 - 1,
        BOARD_HEIGHT as i32 - 4,
        shape,
        (255, 255, 0),
    )
}

fn create_t() -> Piece {
    let shape = vec![
        vec![true, true, true],
        vec![false, true, false],
        vec![false; 3],
    ];
    Piece::new(
        BOARD_WIDTH as i32 / 2,
        BOARD_HEIGHT as i32 - 4,
        shape,
        (192, 0, 192),
    )
}

fn create_l() -> Piece {
    let shape = vec![vec![true], vec![true], vec![true], vec![true]];
    Piece::new(
        BOARD_WIDTH as i32 / 2,
        BOARD_HEIGHT as i32 - 4,
        shape,
        (0, 192, 0),
    )
}

fn create_j() -> Piece {
    let shape = vec![vec![true], vec![true], vec![true], vec![true]];
    Piece::new(
        BOARD_WIDTH as i32 / 2,
        BOARD_HEIGHT as i32 - 4,
        shape,
        (0, 0, 192),
    )
}

fn create_s() -> Piece {
    let shape = vec![
        vec![false, true, true],
        vec![true, true, false],
        vec![false; 3],
    ];
    Piece::new(
        (BOARD_WIDTH as i32) / 2 - 1,
        BOARD_HEIGHT as i32 - 4,
        shape,
        (0, 255, 0),
    )
}

fn create_z() -> Piece {
    let shape = vec![
        vec![true, true, false],
        vec![false, true, true],
        vec![false; 3],
    ];
    Piece::new(
        (BOARD_WIDTH as i32) / 2 - 1,
        BOARD_HEIGHT as i32 - 4,
        shape,
        (255, 0, 0),
    )
}

fn main() {
    let mut stdout = io::stdout().into_raw_mode().unwrap();
    let stdin = async_stdin();

    let mut game = Game::new();
    game.spawn_piece();

    let mut last_time = SystemTime::now();
    const UPDATE_RATE: Duration = Duration::from_millis(1000);

    loop {
        // Update
        let now = SystemTime::now();
        if (now.duration_since(last_time).unwrap()).as_secs() > 1 {
            game.move_down();
            if !game
                .current_piece
                .as_ref()
                .map(|p| p.y + p.shape.len() as i32 <= BOARD_HEIGHT as i32)
                .unwrap_or(false)
            {
                game.merge_piece();
                game.clear_lines();
                game.score += game.clear_lines();
                game.board = game.create_new_board();
                game.spawn_piece();
            }
            last_time = now;
        }

        // Draw
        stdout.lock().write(termion::clear::All.as_bytes()).unwrap();

        game.draw(&mut stdout.lock());

        // Input
        if let Some(key) = stdin.keys().next() {
            match key {
                Key::Char('q') => break,
                Key::Left => if game.move_left() {},
                Key::Right => if game.move_right() {},
                Key::Up => if game.rotate() {},
                Key::Down => while game.move_down() {},
                _ => {}
            }
        }
    }

    std::process::exit(0);
}
