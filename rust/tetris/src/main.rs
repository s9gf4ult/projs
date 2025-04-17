use rand::Rng;
use std::{
    io::{stdout, Write},
    thread,
    time::{Duration, Instant},
};

use crossterm::{
    cursor::{Hide, MoveTo, Show},
    event::{poll, read, Event, KeyCode},
    execute,
    style::{Color, Print, SetBackgroundColor},
    terminal::{Clear, ClearType, EnterAlternateScreen, LeaveAlternateScreen, SetSize},
    Result,
};

// Constants for the game
const BOARD_WIDTH: usize = 10;
const BOARD_HEIGHT: usize = 20;
const TICK_RATE_MS: u64 = 500; // Speed of falling pieces in milliseconds

// Define colors for different tetrominos
const COLORS: [Color; 7] = [
    Color::Red,        // I piece
    Color::Blue,       // J piece
    Color::Yellow,     // L piece
    Color::Green,      // O piece
    Color::Cyan,       // S piece
    Color::Magenta,    // T piece
    Color::DarkYellow, // Z piece
];

// Define the shapes of the tetrominos
const TETROMINOS: [&[&[i32]]; 7] = [
    // I
    &[&[1, 1, 1, 1]],
    // J
    &[&[1, 0, 0], &[1, 1, 1]],
    // L
    &[&[0, 0, 1], &[1, 1, 1]],
    // O
    &[&[1, 1], &[1, 1]],
    // S
    &[&[0, 1, 1], &[1, 1, 0]],
    // T
    &[&[0, 1, 0], &[1, 1, 1]],
    // Z
    &[&[1, 1, 0], &[0, 1, 1]],
];

// Tetromino structure
struct Tetromino {
    shape_type: usize,
    shape: Vec<Vec<i32>>,
    color: Color,
    x: i32,
    y: i32,
}

impl Tetromino {
    // Create a new random tetromino
    fn new() -> Self {
        let mut rng = rand::thread_rng();
        let shape_type = rng.gen_range(0..7);
        let shape = TETROMINOS[shape_type]
            .iter()
            .map(|row| row.to_vec())
            .collect();
        let color = COLORS[shape_type];

        // Start position (center top)
        let x = (BOARD_WIDTH as i32 / 2) - 1;
        let y = 0;

        Tetromino {
            shape_type,
            shape,
            color,
            x,
            y,
        }
    }

    // Rotate the tetromino 90 degrees clockwise
    fn rotate(&self) -> Vec<Vec<i32>> {
        let height = self.shape.len();
        let width = self.shape[0].len();
        let mut new_shape = vec![vec![0; height]; width];

        for y in 0..height {
            for x in 0..width {
                new_shape[x][height - 1 - y] = self.shape[y][x];
            }
        }

        new_shape
    }

    // Check if the tetromino can be moved to a new position
    fn can_move(&self, board: &[Vec<Option<Color>>], dx: i32, dy: i32) -> bool {
        let height = self.shape.len();
        let width = self.shape[0].len();

        for y in 0..height {
            for x in 0..width {
                if self.shape[y][x] == 0 {
                    continue;
                }

                let nx = self.x + x as i32 + dx;
                let ny = self.y + y as i32 + dy;

                // Check boundaries
                if nx < 0 || nx >= BOARD_WIDTH as i32 || ny >= BOARD_HEIGHT as i32 {
                    return false;
                }

                // Check collision with existing pieces, but only if we're inside the board
                if ny >= 0 {
                    if let Some(Some(_)) =
                        board.get(ny as usize).and_then(|row| row.get(nx as usize))
                    {
                        return false;
                    }
                }
            }
        }

        true
    }

    // Check if the tetromino can be rotated
    fn can_rotate(&mut self, board: &[Vec<Option<Color>>]) -> bool {
        let rotated = self.rotate();
        let height = rotated.len();
        let width = rotated[0].len();

        for y in 0..height {
            for x in 0..width {
                if rotated[y][x] == 0 {
                    continue;
                }

                let nx = self.x + x as i32;
                let ny = self.y + y as i32;

                // Check boundaries
                if nx < 0 || nx >= BOARD_WIDTH as i32 || ny >= BOARD_HEIGHT as i32 {
                    return false;
                }

                // Check collision with existing pieces, but only if we're inside the board
                if ny >= 0 {
                    if let Some(Some(_)) =
                        board.get(ny as usize).and_then(|row| row.get(nx as usize))
                    {
                        return false;
                    }
                }
            }
        }

        true
    }
}

// Game structure
struct Game {
    board: Vec<Vec<Option<Color>>>,
    current_piece: Tetromino,
    game_over: bool,
    score: u32,
    level: u32,
    lines_cleared: u32,
}

impl Game {
    // Initialize a new game
    fn new() -> Self {
        let board = vec![vec![None; BOARD_WIDTH]; BOARD_HEIGHT];
        let current_piece = Tetromino::new();

        Game {
            board,
            current_piece,
            game_over: false,
            score: 0,
            level: 1,
            lines_cleared: 0,
        }
    }

    // Move the current piece
    fn move_piece(&mut self, dx: i32, dy: i32) -> bool {
        if self.current_piece.can_move(&self.board, dx, dy) {
            self.current_piece.x += dx;
            self.current_piece.y += dy;
            true
        } else {
            false
        }
    }

    // Rotate the current piece
    fn rotate_piece(&mut self) {
        if self.current_piece.can_rotate(&mut self.board) {
            self.current_piece.shape = self.current_piece.rotate();
        }
    }

    // Drop the piece immediately
    fn hard_drop(&mut self) {
        while self.move_piece(0, 1) {}
        self.place_piece();
    }

    // Place the current piece on the board and create a new one
    fn place_piece(&mut self) {
        let height = self.current_piece.shape.len();
        let width = self.current_piece.shape[0].len();

        // Add the piece to the board
        for y in 0..height {
            for x in 0..width {
                if self.current_piece.shape[y][x] == 0 {
                    continue;
                }

                let board_x = self.current_piece.x + x as i32;
                let board_y = self.current_piece.y + y as i32;

                // Check if the piece is placed above the board
                if board_y < 0 {
                    self.game_over = true;
                    return;
                }

                self.board[board_y as usize][board_x as usize] = Some(self.current_piece.color);
            }
        }

        // Check for completed lines
        self.clear_lines();

        // Create a new piece
        self.current_piece = Tetromino::new();

        // Check if the new piece can be placed
        if !self.current_piece.can_move(&self.board, 0, 0) {
            self.game_over = true;
        }
    }

    // Clear completed lines
    fn clear_lines(&mut self) {
        let mut lines_cleared = 0;

        for y in (0..BOARD_HEIGHT).rev() {
            if self.board[y].iter().all(|cell| cell.is_some()) {
                lines_cleared += 1;

                // Move all lines above down
                for y2 in (1..=y).rev() {
                    for x in 0..BOARD_WIDTH {
                        self.board[y2][x] = self.board[y2 - 1][x];
                    }
                }

                // Clear the top line
                for _x in 0..BOARD_WIDTH {
                    self.board[0][_x] = None;
                }
            }
        }

        // Update score and level
        if lines_cleared > 0 {
            // Classic Tetris scoring
            let points = match lines_cleared {
                1 => 40,
                2 => 100,
                3 => 300,
                4 => 1200, // Tetris!
                _ => 0,
            } * self.level;

            self.score += points;
            self.lines_cleared += lines_cleared;
            self.level = (self.lines_cleared / 10) + 1;
        }
    }

    // Update the game state (called each tick)
    fn update(&mut self) {
        if !self.game_over {
            if !self.move_piece(0, 1) {
                self.place_piece();
            }
        }
    }

    // Draw the game board and current piece
    fn draw(&self) -> Result<()> {
        let mut stdout = stdout();

        // Clear the screen
        execute!(stdout, Clear(ClearType::All))?;

        // Draw the border and header
        execute!(stdout, MoveTo(0, 0))?;
        execute!(stdout, Print(format!("Score: {}", self.score)))?;
        execute!(stdout, MoveTo(0, 1))?;
        execute!(stdout, Print(format!("Level: {}", self.level)))?;
        execute!(stdout, MoveTo(0, 2))?;
        execute!(stdout, Print(format!("Lines: {}", self.lines_cleared)))?;

        // Draw game over message if needed
        if self.game_over {
            execute!(stdout, MoveTo(0, BOARD_HEIGHT as u16 + 5))?;
            execute!(stdout, Print("Game Over! Press 'q' to quit."))?;
        }

        // Draw the board border
        for y in 0..BOARD_HEIGHT as u16 + 2 {
            execute!(stdout, MoveTo(0, y + 4))?;
            execute!(stdout, Print("║"))?;
            execute!(stdout, MoveTo(BOARD_WIDTH as u16 * 2 + 1, y + 4))?;
            execute!(stdout, Print("║"))?;
        }

        execute!(stdout, MoveTo(0, 4))?;
        execute!(stdout, Print("╔"))?;
        for _x in 0..BOARD_WIDTH {
            execute!(stdout, Print("══"))?;
        }
        execute!(stdout, Print("╗"))?;

        execute!(stdout, MoveTo(0, BOARD_HEIGHT as u16 + 4 + 1))?;
        execute!(stdout, Print("╚"))?;
        for _x in 0..BOARD_WIDTH {
            execute!(stdout, Print("══"))?;
        }
        execute!(stdout, Print("╝"))?;

        // Draw the board
        for y in 0..BOARD_HEIGHT {
            for x in 0..BOARD_WIDTH {
                let cell = self.board[y][x];
                let cell_x = (x as u16) * 2 + 1;
                let cell_y = (y as u16) + 4 + 0;

                execute!(stdout, MoveTo(cell_x, cell_y))?;

                if let Some(color) = cell {
                    execute!(
                        stdout,
                        SetBackgroundColor(color),
                        Print("  "),
                        SetBackgroundColor(Color::Reset)
                    )?;
                }
            }
        }

        // Draw the current piece
        let piece_height = self.current_piece.shape.len();
        let piece_width = self.current_piece.shape[0].len();

        for y in 0..piece_height {
            for x in 0..piece_width {
                if self.current_piece.shape[y][x] == 0 {
                    continue;
                }

                let board_x = self.current_piece.x + x as i32;
                let board_y = self.current_piece.y + y as i32;

                // Only draw if inside the visible board
                if board_y >= 0 && board_y < BOARD_HEIGHT as i32 {
                    let cell_x = (board_x as u16) * 2 + 1;
                    let cell_y = (board_y as u16) + 4 + 0;

                    execute!(stdout, MoveTo(cell_x, cell_y))?;
                    execute!(
                        stdout,
                        SetBackgroundColor(self.current_piece.color),
                        Print("  "),
                        SetBackgroundColor(Color::Reset)
                    )?;
                }
            }
        }

        // Draw controls
        execute!(stdout, MoveTo(BOARD_WIDTH as u16 * 2 + 5, 4))?;
        execute!(stdout, Print("Controls:"))?;
        execute!(stdout, MoveTo(BOARD_WIDTH as u16 * 2 + 5, 5))?;
        execute!(stdout, Print("←/→: Move"))?;
        execute!(stdout, MoveTo(BOARD_WIDTH as u16 * 2 + 5, 6))?;
        execute!(stdout, Print("↑: Rotate"))?;
        execute!(stdout, MoveTo(BOARD_WIDTH as u16 * 2 + 5, 7))?;
        execute!(stdout, Print("↓: Soft Drop"))?;
        execute!(stdout, MoveTo(BOARD_WIDTH as u16 * 2 + 5, 8))?;
        execute!(stdout, Print("Space: Hard Drop"))?;
        execute!(stdout, MoveTo(BOARD_WIDTH as u16 * 2 + 5, 9))?;
        execute!(stdout, Print("q: Quit"))?;

        stdout.flush()?;
        Ok(())
    }
}

fn main() -> Result<()> {
    // Setup terminal
    execute!(
        stdout(),
        EnterAlternateScreen,
        Hide,
        SetSize(BOARD_WIDTH as u16 * 2 + 30, BOARD_HEIGHT as u16 + 10)
    )?;

    let mut game = Game::new();
    let mut last_tick = Instant::now();

    game.draw()?;

    loop {
        // Check for input
        if poll(Duration::from_millis(100))? {
            match read()? {
                Event::Key(key) => match key.code {
                    KeyCode::Left => {
                        game.move_piece(-1, 0);
                    }
                    KeyCode::Right => {
                        game.move_piece(1, 0);
                    }
                    KeyCode::Up => {
                        game.rotate_piece();
                    }
                    KeyCode::Down => {
                        game.move_piece(0, 1);
                    }
                    KeyCode::Char(' ') => {
                        game.hard_drop();
                    }
                    KeyCode::Char('q') => {
                        break;
                    }
                    _ => {}
                },
                _ => {}
            }

            game.draw()?;
        }

        // Update game state at regular intervals
        let now = Instant::now();
        let tick_rate = TICK_RATE_MS / game.level as u64;
        if now.duration_since(last_tick).as_millis() > tick_rate as u128 {
            game.update();
            game.draw()?;
            last_tick = now;
        }

        // Exit if game is over and player presses 'q'
        if game.game_over {
            thread::sleep(Duration::from_millis(100));
        }
    }

    // Restore terminal
    execute!(stdout(), Show, LeaveAlternateScreen)?;

    Ok(())
}
