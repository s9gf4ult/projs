// src/main.rs

use crossterm::{
    cursor::{Hide, MoveTo, Show},
    event::{self, Event, KeyCode, KeyEvent, KeyModifiers},
    execute, queue,
    style::{Color, Print, ResetColor, SetBackgroundColor, SetForegroundColor},
    terminal::{
        disable_raw_mode, enable_raw_mode, size, Clear, ClearType, EnterAlternateScreen,
        LeaveAlternateScreen,
    },
    Result,
};
use rand::{seq::SliceRandom, Rng}; // Added Rng trait
use std::io::{stdout, Stdout, Write};
use std::time::{Duration, Instant};

// --- Constants ---
const BOARD_WIDTH: usize = 10;
const BOARD_HEIGHT: usize = 20; // Visible area
const BOARD_HEIGHT_TOTAL: usize = BOARD_HEIGHT + 4; // Include hidden area above for spawning

const TICK_RATE: Duration = Duration::from_millis(1000); // Initial fall speed
const FAST_TICK_RATE: Duration = Duration::from_millis(50); // Speed when holding down
const ACCELERATION_FACTOR: f64 = 0.95; // How much faster it gets per level
const LEVEL_UP_LINES: u32 = 10; // Lines to clear for next level

// Tetromino shapes and their rotations
// Represents relative coordinates from a pivot point
const TETROMINOES: [(Color, [[(isize, isize); 4]; 4]); 7] = [
    // I piece (Cyan) - Special pivot handling for rotation
    (Color::Cyan, [
        [(0, 1), (1, 1), (2, 1), (3, 1)], // Rotation 0
        [(2, 0), (2, 1), (2, 2), (2, 3)], // Rotation 1
        [(0, 2), (1, 2), (2, 2), (3, 2)], // Rotation 2
        [(1, 0), (1, 1), (1, 2), (1, 3)], // Rotation 3
    ]),
    // O piece (Yellow) - Doesn't rotate
    (Color::Yellow, [
        [(1, 0), (2, 0), (1, 1), (2, 1)],
        [(1, 0), (2, 0), (1, 1), (2, 1)],
        [(1, 0), (2, 0), (1, 1), (2, 1)],
        [(1, 0), (2, 0), (1, 1), (2, 1)],
    ]),
    // T piece (Magenta)
    (Color::Magenta, [
        [(0, 1), (1, 1), (2, 1), (1, 0)], // Rotation 0
        [(1, 0), (1, 1), (1, 2), (2, 1)], // Rotation 1
        [(0, 1), (1, 1), (2, 1), (1, 2)], // Rotation 2
        [(1, 0), (1, 1), (1, 2), (0, 1)], // Rotation 3
    ]),
    // S piece (Green)
    (Color::Green, [
        [(1, 1), (2, 1), (0, 0), (1, 0)], // Rotation 0
        [(1, 0), (1, 1), (2, 1), (2, 2)], // Rotation 1
        [(0, 2), (1, 2), (1, 1), (2, 1)], // Rotation 2
        [(0, 0), (0, 1), (1, 1), (1, 2)], // Rotation 3
    ]),
    // Z piece (Red)
    (Color::Red, [
        [(0, 1), (1, 1), (1, 0), (2, 0)], // Rotation 0
        [(2, 0), (2, 1), (1, 1), (1, 2)], // Rotation 1
        [(0, 2), (1, 2), (1, 1), (2, 1)], // Rotation 2
        [(1, 0), (1, 1), (0, 1), (0, 2)], // Rotation 3
    ]),
    // J piece (Blue)
    (Color::Blue, [
        [(0, 0), (0, 1), (1, 1), (2, 1)], // Rotation 0
        [(1, 0), (2, 0), (1, 1), (1, 2)], // Rotation 1
        [(0, 1), (1, 1), (2, 1), (2, 2)], // Rotation 2
        [(1, 0), (1, 1), (1, 2), (0, 2)], // Rotation 3
    ]),
    // L piece (DarkYellow -> Orange approximation)
    (Color::DarkYellow, [
        [(2, 0), (0, 1), (1, 1), (2, 1)], // Rotation 0
        [(1, 0), (1, 1), (1, 2), (2, 2)], // Rotation 1
        [(0, 1), (1, 1), (2, 1), (0, 2)], // Rotation 2
        [(0, 0), (1, 0), (1, 1), (1, 2)], // Rotation 3
    ]),
];

// --- Structs and Enums ---

#[derive(Clone, Copy, PartialEq)]
enum Cell {
    Empty,
    Occupied(Color),
}

struct Piece {
    tetromino_index: usize,
    rotation: usize,
    x: isize,
    y: isize, // Top-left corner of the 4x4 grid containing the piece
}

impl Piece {
    fn new(index: usize) -> Self {
        Piece {
            tetromino_index: index,
            rotation: 0,
            x: (BOARD_WIDTH / 2) as isize - 2, // Start roughly centered
            // Start partially hidden above the visible board
            y: (BOARD_HEIGHT_TOTAL - BOARD_HEIGHT - 2) as isize,
        }
    }

    fn color(&self) -> Color {
        TETROMINOES[self.tetromino_index].0
    }

    // Returns the absolute coordinates of the blocks for the current piece state
    fn get_blocks(&self) -> [(isize, isize); 4] {
        let shape = TETROMINOES[self.tetromino_index].1[self.rotation];
        let mut blocks = [(0, 0); 4];
        for i in 0..4 {
            blocks[i] = (self.x + shape[i].0, self.y + shape[i].1);
        }
        blocks
    }

    // Returns the absolute coordinates for a potential rotated state
    fn get_rotated_blocks(&self) -> [(isize, isize); 4] {
        let next_rotation = (self.rotation + 1) % 4;
        let shape = TETROMINOES[self.tetromino_index].1[next_rotation];
        let mut blocks = [(0, 0); 4];
        for i in 0..4 {
            blocks[i] = (self.x + shape[i].0, self.y + shape[i].1);
        }
        blocks
    }
}

struct GameState {
    board: Vec<Vec<Cell>>,
    current_piece: Piece,
    next_piece_index: usize,
    score: u32,
    lines_cleared: u32,
    level: u32,
    game_over: bool,
    rng: rand::rngs::ThreadRng,
    tick_rate: Duration,
}

impl GameState {
    fn new() -> Self {
        let mut rng = rand::thread_rng();
        let initial_piece_index = rng.gen_range(0..TETROMINOES.len());
        let next_piece_index = rng.gen_range(0..TETROMINOES.len());

        GameState {
            board: vec![vec![Cell::Empty; BOARD_WIDTH]; BOARD_HEIGHT_TOTAL],
            current_piece: Piece::new(initial_piece_index),
            next_piece_index,
            score: 0,
            lines_cleared: 0,
            level: 1,
            game_over: false,
            rng,
            tick_rate: TICK_RATE,
        }
    }

    fn spawn_new_piece(&mut self) {
        self.current_piece = Piece::new(self.next_piece_index);
        self.next_piece_index = self.rng.gen_range(0..TETROMINOES.len());

        // Check for game over condition immediately after spawning
        if !self.is_valid_position(&self.current_piece.get_blocks()) {
            self.game_over = true;
        }
    }

    // Checks if a given set of block coordinates is valid (within bounds and not colliding)
    fn is_valid_position(&self, blocks: &[(isize, isize)]) -> bool {
        for &(x, y) in blocks {
            if x < 0 || x >= BOARD_WIDTH as isize || y < 0 || y >= BOARD_HEIGHT_TOTAL as isize {
                return false; // Out of bounds
            }
            if self.board[y as usize][x as usize] != Cell::Empty {
                return false; // Collision with existing blocks
            }
        }
        true
    }

    fn move_piece(&mut self, dx: isize, dy: isize) -> bool {
        let current_blocks = self.current_piece.get_blocks();
        let mut next_blocks = [(0, 0); 4];
        for i in 0..4 {
            next_blocks[i] = (current_blocks[i].0 + dx, current_blocks[i].1 + dy);
        }

        if self.is_valid_position(&next_blocks) {
            self.current_piece.x += dx;
            self.current_piece.y += dy;
            true
        } else {
            false
        }
    }

    fn rotate_piece(&mut self) {
        let rotated_blocks = self.current_piece.get_rotated_blocks();

        // Basic Wall Kick attempt (simple shifts)
        let kicks = [(0, 0), (-1, 0), (1, 0), (-2, 0), (2, 0), (0, -1), (0, -2)]; // Added vertical kicks

        for &(kick_x, kick_y) in &kicks {
            let mut potential_blocks = [(0,0); 4];
             for i in 0..4 {
                 potential_blocks[i] = (rotated_blocks[i].0 + kick_x, rotated_blocks[i].1 + kick_y);
             }

            if self.is_valid_position(&potential_blocks) {
                self.current_piece.x += kick_x;
                self.current_piece.y += kick_y;
                self.current_piece.rotation = (self.current_piece.rotation + 1) % 4;
                return; // Rotation successful
            }
        }
        // If no kick works, rotation fails
    }

    fn hard_drop(&mut self) {
        while self.move_piece(0, 1) {
            // Keep moving down until it hits something
            self.score += 2; // Small score bonus for hard drop
        }
        self.lock_piece();
    }

    fn lock_piece(&mut self) {
        let blocks = self.current_piece.get_blocks();
        let color = self.current_piece.color();
        for &(x, y) in &blocks {
            if y >= 0 && y < BOARD_HEIGHT_TOTAL as isize && x >= 0 && x < BOARD_WIDTH as isize {
                self.board[y as usize][x as usize] = Cell::Occupied(color);
            }
        }
        self.clear_lines();
        self.spawn_new_piece(); // Spawn next piece
    }

    fn clear_lines(&mut self) {
        let mut lines_to_clear = Vec::new();
        // Check from bottom up, but only within visible area
        for y in (0..BOARD_HEIGHT_TOTAL).rev() {
            if y < BOARD_HEIGHT_TOTAL - BOARD_HEIGHT { continue; } // Skip hidden area check

            if self.board[y].iter().all(|&cell| cell != Cell::Empty) {
                lines_to_clear.push(y);
            }
        }

        if !lines_to_clear.is_empty() {
            let num_cleared = lines_to_clear.len() as u32;
            self.lines_cleared += num_cleared;

            // Scoring: 1: 100, 2: 300, 3: 500, 4: 800 points per level
            let points = match num_cleared {
                1 => 100,
                2 => 300,
                3 => 500,
                4 => 800,
                _ => 0,
            } * self.level;
            self.score += points;

            // Update level and speed
            let new_level = 1 + self.lines_cleared / LEVEL_UP_LINES;
            if new_level > self.level {
                self.level = new_level;
                // Decrease tick rate, but don't let it become too fast
                let new_tick_millis = (self.tick_rate.as_millis() as f64 * ACCELERATION_FACTOR) as u64;
                self.tick_rate = Duration::from_millis(new_tick_millis.max(50)); // Minimum 50ms tick
            }


            // Remove cleared lines and shift down
            for &line_y in &lines_to_clear {
                self.board.remove(line_y);
            }
            // Add new empty lines at the top
            for _ in 0..num_cleared {
                self.board.insert(0, vec![Cell::Empty; BOARD_WIDTH]);
            }
        }
    }

    // Game tick: move piece down, lock if necessary
    fn tick(&mut self) {
        if self.game_over {
            return;
        }
        if !self.move_piece(0, 1) {
            // Could not move down, lock the piece
            self.lock_piece();
        }
    }
}

// --- Rendering ---

fn draw(stdout: &mut Stdout, state: &GameState) -> Result<()> {
    let (term_width, term_height) = size()?;

    // Calculate offsets to center the game board (roughly)
    let board_draw_width = BOARD_WIDTH * 2 + 2; // Each cell is 2 chars wide + borders
    let board_draw_height = BOARD_HEIGHT + 2; // Visible height + borders
    let info_width = 20; // Width for score/next piece info

    let total_width = board_draw_width + info_width;
    let start_x = if term_width > total_width as u16 { (term_width - total_width as u16) / 2 } else { 0 };
    let start_y = if term_height > board_draw_height as u16 { (term_height - board_draw_height as u16) / 2 } else { 0 };

    queue!(stdout, Clear(ClearType::All), Hide)?;

    // Draw board border
    for y in 0..=BOARD_HEIGHT {
        queue!(
            stdout,
            MoveTo(start_x, start_y + y as u16),
            SetForegroundColor(Color::White),
            Print("│"),
            MoveTo(start_x + (BOARD_WIDTH * 2 + 1) as u16, start_y + y as u16),
            Print("│")
        )?;
    }
     for x in 0..=BOARD_WIDTH * 2 {
        queue!(
            stdout,
            MoveTo(start_x + x as u16, start_y),
             Print(if x % 2 == 0 {"─"} else {"─"}), // Top border
             MoveTo(start_x + x as u16, start_y + (BOARD_HEIGHT + 1) as u16),
             Print(if x % 2 == 0 {"─"} else {"─"}) // Bottom border
        )?;
    }
    // Corner pieces
    queue!(stdout, MoveTo(start_x, start_y), Print("┌"))?;
    queue!(stdout, MoveTo(start_x + (BOARD_WIDTH * 2 + 1) as u16, start_y), Print("┐"))?;
    queue!(stdout, MoveTo(start_x, start_y + (BOARD_HEIGHT + 1) as u16), Print("└"))?;
    queue!(stdout, MoveTo(start_x + (BOARD_WIDTH * 2 + 1) as u16, start_y + (BOARD_HEIGHT + 1) as u16), Print("┘"))?;


    // Draw locked pieces on the board (only visible area)
    for y in 0..BOARD_HEIGHT {
        for x in 0..BOARD_WIDTH {
            let board_y = y + (BOARD_HEIGHT_TOTAL - BOARD_HEIGHT); // Adjust index for the full board
            let cell = state.board[board_y][x];
            let draw_x = start_x + 1 + (x * 2) as u16;
            let draw_y = start_y + 1 + y as u16;

            match cell {
                Cell::Occupied(color) => {
                    queue!(
                        stdout,
                        MoveTo(draw_x, draw_y),
                        SetBackgroundColor(color),
                        Print("  "), // Two spaces for a block look
                        ResetColor
                    )?;
                }
                Cell::Empty => {
                     queue!(
                        stdout,
                        MoveTo(draw_x, draw_y),
                        Print("  ") // Empty space
                    )?;
                }
            }
        }
    }

    // Draw current falling piece (only visible parts)
    if !state.game_over {
        let blocks = state.current_piece.get_blocks();
        let color = state.current_piece.color();
        for &(px, py) in &blocks {
            // Only draw if within the visible board area
            let visible_y = py - (BOARD_HEIGHT_TOTAL - BOARD_HEIGHT) as isize;
            if px >= 0 && px < BOARD_WIDTH as isize && visible_y >= 0 && visible_y < BOARD_HEIGHT as isize {
                let draw_x = start_x + 1 + (px * 2) as u16;
                let draw_y = start_y + 1 + visible_y as u16;
                 queue!(
                    stdout,
                    MoveTo(draw_x, draw_y),
                    SetBackgroundColor(color),
                    Print("  "),
                    ResetColor
                )?;
            }
        }
    }

    // Draw Info Panel (Score, Level, Next Piece)
    let info_x = start_x + board_draw_width as u16 + 3;
    let info_y = start_y + 1;

    queue!(stdout, MoveTo(info_x, info_y), SetForegroundColor(Color::White), Print("Score:"))?;
    queue!(stdout, MoveTo(info_x + 2, info_y + 1), Print(format!("{}", state.score)))?;

    queue!(stdout, MoveTo(info_x, info_y + 3), Print("Lines:"))?;
    queue!(stdout, MoveTo(info_x + 2, info_y + 4), Print(format!("{}", state.lines_cleared)))?;

    queue!(stdout, MoveTo(info_x, info_y + 6), Print("Level:"))?;
    queue!(stdout, MoveTo(info_x + 2, info_y + 7), Print(format!("{}", state.level)))?;

    queue!(stdout, MoveTo(info_x, info_y + 9), Print("Next:"))?;
    // Draw the next piece preview
    let next_piece_data = TETROMINOES[state.next_piece_index];
    let next_piece_color = next_piece_data.0;
    let next_piece_shape = next_piece_data.1[0]; // Use base rotation for preview

    // Clear previous preview area
    for y_off in 0..4 {
        queue!(stdout, MoveTo(info_x + 2, info_y + 10 + y_off), Print("        "))?; // 4 * 2 spaces
    }
    // Draw new preview
    for &(block_x, block_y) in &next_piece_shape {
        let draw_x = info_x + 2 + (block_x * 2) as u16;
        let draw_y = info_y + 10 + block_y as u16;
         queue!(
            stdout,
            MoveTo(draw_x, draw_y),
            SetBackgroundColor(next_piece_color),
            Print("  "),
            ResetColor
        )?;
    }


    // Draw Game Over message
    if state.game_over {
        let msg = "GAME OVER";
        let msg_len = msg.len() as u16;
        let msg_x = start_x + (board_draw_width as u16 - msg_len) / 2;
        let msg_y = start_y + (board_draw_height as u16) / 2;
        queue!(
            stdout,
            MoveTo(msg_x, msg_y),
            SetForegroundColor(Color::Red),
            Print(msg),
            MoveTo(msg_x - 2, msg_y + 1), // Slightly adjust for second line
            Print("Press Q to Quit")
        )?;
    }

    // --- Instructions ---
     let help_y = start_y + board_draw_height as u16 + 2; // Below the board
     if help_y < term_height {
        queue!(stdout, MoveTo(start_x, help_y), ResetColor, Print("Arrows: Move/Rotate | Space: Hard Drop | Q: Quit"))?;
     }


    stdout.flush()?; // Write all queued commands to the terminal
    Ok(())
}

// --- Main Game Loop ---

fn run_game(stdout: &mut Stdout) -> Result<()> {
    let mut state = GameState::new();
    let mut last_tick = Instant::now();
    let mut soft_dropping = false;

    loop {
        // --- Timing ---
        let current_tick_rate = if soft_dropping {
             FAST_TICK_RATE.min(state.tick_rate) // Use faster rate if soft dropping, but not faster than level speed
        } else {
            state.tick_rate
        };

        if last_tick.elapsed() >= current_tick_rate {
            state.tick();
            last_tick = Instant::now();
            if state.game_over {
                draw(stdout, &state)?; // Draw final state
            }
        }

        // --- Input Handling ---
        // Poll for events with a timeout based on remaining time until next tick
        let timeout = current_tick_rate.saturating_sub(last_tick.elapsed());
        if event::poll(timeout)? {
            match event::read()? {
                Event::Key(KeyEvent { code, modifiers: _, kind: event::KeyEventKind::Press, state: _ }) => { // Only react on Press
                    if state.game_over {
                        // Only allow quit when game is over
                        if code == KeyCode::Char('q') || code == KeyCode::Char('Q') {
                            break;
                        }
                    } else {
                        match code {
                            KeyCode::Left => {
                                state.move_piece(-1, 0);
                            }
                            KeyCode::Right => {
                                state.move_piece(1, 0);
                            }
                            KeyCode::Down => {
                                soft_dropping = true;
                                state.score += 1; // Small score bonus for soft drop
                                // Immediately tick down when pressed
                                state.tick();
                                last_tick = Instant::now(); // Reset tick timer after manual move
                            }
                            KeyCode::Up => {
                                state.rotate_piece();
                            }
                            KeyCode::Char(' ') => { // Spacebar for Hard Drop
                                state.hard_drop();
                                last_tick = Instant::now(); // Reset tick timer after hard drop
                            }
                            KeyCode::Char('q') | KeyCode::Char('Q') => {
                                break; // Quit
                            }
                            _ => {} // Ignore other keys
                        }
                    }
                },
                 Event::Key(KeyEvent { code: KeyCode::Down, kind: event::KeyEventKind::Release, .. }) => {
                    // Stop soft dropping when Down arrow is released
                     soft_dropping = false;
                 },
                Event::Resize(_, _) => {
                    // Redraw immediately on resize
                    draw(stdout, &state)?;
                }
                _ => {} // Ignore other events
            }
        }
         // Key release event for soft drop handled above

        // --- Rendering ---
        // Only redraw if the game isn't over yet, or if it just became over in this loop iteration
        if !state.game_over || last_tick.elapsed() < Duration::from_millis(10) { // Redraw once more right after game over
             draw(stdout, &state)?;
        }

        if state.game_over && (event::poll(Duration::from_millis(10))?) { // Check for quit input during game over screen
             if let Event::Key(KeyEvent { code, .. }) = event::read()? {
                  if code == KeyCode::Char('q') || code == KeyCode::Char('Q') {
                      break;
                  }
             }
        }


    } // End main loop

    Ok(())
}

// --- Entry Point ---

fn main() -> Result<()> {
    let mut stdout = stdout();
    enable_raw_mode()?; // Allow reading key presses directly
    execute!(stdout, EnterAlternateScreen, Clear(ClearType::All))?; // Use alternate screen buffer

    // Run the game logic, handle potential errors
    let game_result = run_game(&mut stdout);

    // --- Cleanup ---
    // Ensure cleanup happens even if run_game returns an error
    execute!(stdout, Show, LeaveAlternateScreen)?; // Show cursor, leave alternate screen
    disable_raw_mode()?;

    // Return the result from the game loop
    game_result
}
