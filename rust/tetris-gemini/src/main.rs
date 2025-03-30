// src/main.rs

use crossterm::{
    cursor::{Hide, MoveTo, Show},
    event::{self, Event, KeyCode, KeyEvent}, // KeyModifiers removed previously
    execute, queue,
    style::{Color, Print, ResetColor, SetBackgroundColor, SetForegroundColor},
    terminal::{
        disable_raw_mode, enable_raw_mode, size, Clear, ClearType, EnterAlternateScreen,
        LeaveAlternateScreen,
    },
    // crossterm::Result removed previously
};
use rand::Rng; // seq::SliceRandom removed previously
use std::io::{stdout, Stdout, Write, Result as IoResult}; // Using std::io::Result
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
const TETROMINOES: [(Color, [[(isize, isize); 4]; 4]); 7] = [
    // ... (Tetromino data remains the same) ...
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

#[derive(Clone)] // Added Clone trait here for ghost piece calculation
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
            y: (BOARD_HEIGHT_TOTAL - BOARD_HEIGHT - 2) as isize, // Start partially hidden
        }
    }

    fn color(&self) -> Color {
        TETROMINOES[self.tetromino_index].0
    }

    fn get_blocks(&self) -> [(isize, isize); 4] {
        let shape = TETROMINOES[self.tetromino_index].1[self.rotation];
        let mut blocks = [(0, 0); 4];
        for i in 0..4 {
            blocks[i] = (self.x + shape[i].0, self.y + shape[i].1);
        }
        blocks
    }

    // get_rotated_blocks removed as it wasn't the best approach for wall kicks
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

        if !self.is_valid_position(&self.current_piece.get_blocks()) {
            self.game_over = true;
        }
    }

    // Checks if a given set of *absolute* block coordinates is valid
    fn is_valid_position(&self, blocks: &[(isize, isize)]) -> bool {
        for &(x, y) in blocks {
            if x < 0 || x >= BOARD_WIDTH as isize || y < 0 || y >= BOARD_HEIGHT_TOTAL as isize {
                return false; // Out of bounds
            }
            if y >= 0 { // Only check board if y is non-negative
                if self.board[y as usize][x as usize] != Cell::Empty {
                    return false; // Collision with existing blocks
                }
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
        // **FIX:** Removed unused variable `rotated_blocks_relative`
        // let rotated_blocks_relative = self.current_piece.get_rotated_blocks();

        let kicks = [(0, 0), (-1, 0), (1, 0), (-2, 0), (2, 0), (0, -1), (0, 1)];

        let original_x = self.current_piece.x;
        let original_y = self.current_piece.y;
        let next_rotation_index = (self.current_piece.rotation + 1) % 4;

        for &(kick_x, kick_y) in &kicks {
            let potential_x = original_x + kick_x;
            let potential_y = original_y + kick_y;
            let shape = TETROMINOES[self.current_piece.tetromino_index].1[next_rotation_index];

            // Calculate absolute block positions for this potential rotation + kick
            let mut potential_blocks_absolute = [(0isize, 0isize); 4];
             for i in 0..4 {
                potential_blocks_absolute[i] = (potential_x + shape[i].0, potential_y + shape[i].1);
             }

            // Check validity using the GameState's method
            if self.is_valid_position(&potential_blocks_absolute) {
                // Apply rotation and kick offset
                self.current_piece.x = potential_x;
                self.current_piece.y = potential_y;
                self.current_piece.rotation = next_rotation_index;
                return; // Rotation successful
            }
        }
    }


    fn hard_drop(&mut self) {
        let mut distance = 0;
        while self.move_piece(0, 1) {
            distance += 1;
        }
        self.score += 2 * distance;
        self.lock_piece();
    }

    fn lock_piece(&mut self) {
        let blocks = self.current_piece.get_blocks();
        let color = self.current_piece.color();
        let mut locked_in_visible = false;

        for &(x, y) in &blocks {
            if y >= 0 && y < BOARD_HEIGHT_TOTAL as isize && x >= 0 && x < BOARD_WIDTH as isize {
                self.board[y as usize][x as usize] = Cell::Occupied(color);
                if y >= (BOARD_HEIGHT_TOTAL - BOARD_HEIGHT) as isize {
                    locked_in_visible = true; // Track if any part landed in visible area
                }
            } else if y < 0 { // Part of piece is out of bounds *above* the top
                self.game_over = true;
                return;
            }
        }

        // Game over if piece locked entirely above the visible area
         if !locked_in_visible && !self.game_over {
             // Check if the piece locked entirely within the hidden top rows
              let mut fully_hidden = true;
              for &(_, y) in &blocks {
                  if y >= (BOARD_HEIGHT_TOTAL - BOARD_HEIGHT) as isize {
                      fully_hidden = false;
                      break;
                  }
              }
              if fully_hidden {
                   self.game_over = true;
                   return;
              }
         }


        if !self.game_over {
            self.clear_lines();
            self.spawn_new_piece();
        }
    }


    fn clear_lines(&mut self) {
         let mut lines_to_clear = Vec::new();
        for y in (0..BOARD_HEIGHT_TOTAL).rev() {
             // A line is clearable only if it's within the visible area AND full
            if y >= (BOARD_HEIGHT_TOTAL - BOARD_HEIGHT) && self.board[y].iter().all(|&cell| cell != Cell::Empty) {
                 lines_to_clear.push(y);
             } else if self.board[y].iter().all(|&cell| cell != Cell::Empty) {
                 // If a line *above* the visible area is full, it's technically game over,
                 // but this condition is usually caught by lock_piece or spawn_new_piece.
                 // We don't clear lines in the hidden area.
            }
         }

        if !lines_to_clear.is_empty() {
            let num_cleared = lines_to_clear.len() as u32;
            self.lines_cleared += num_cleared;

            let base_points = match num_cleared {
                1 => 100, 2 => 300, 3 => 500, 4 => 800, _ => 0,
            };
            let points = base_points * self.level;
            self.score += points;

            let new_level = 1 + self.lines_cleared / LEVEL_UP_LINES;
            if new_level > self.level {
                self.level = new_level;
                let new_tick_millis = (self.tick_rate.as_millis() as f64 * ACCELERATION_FACTOR) as u64;
                self.tick_rate = Duration::from_millis(new_tick_millis.max(50));
            }

            lines_to_clear.sort_unstable_by(|a, b| b.cmp(a));
            for &line_y in &lines_to_clear {
                self.board.remove(line_y);
            }
            for _ in 0..num_cleared {
                self.board.insert(0, vec![Cell::Empty; BOARD_WIDTH]);
            }
        }
    }

    fn tick(&mut self) {
        if self.game_over { return; }
        if !self.move_piece(0, 1) {
            self.lock_piece();
        }
    }
}

// --- Rendering ---

fn draw(stdout: &mut Stdout, state: &GameState) -> IoResult<()> {
    let (term_width, term_height) = size()?;
    let board_draw_width = BOARD_WIDTH * 2 + 2;
    let board_draw_height = BOARD_HEIGHT + 2;
    let info_width = 20;
    let total_width = board_draw_width + info_width;
    let start_x = if term_width > total_width as u16 { (term_width - total_width as u16) / 2 } else { 1 };
    let start_y = if term_height > board_draw_height as u16 { (term_height - board_draw_height as u16) / 2 } else { 1 };

    queue!(stdout, Clear(ClearType::All), Hide)?;

    // --- Draw Board Area (Border and Locked Pieces) ---
    // (Border drawing code remains the same)
    let border_color = Color::DarkGrey;
    queue!(stdout, SetForegroundColor(border_color))?;
    for y in 0..=BOARD_HEIGHT {
        queue!(
            stdout,
            MoveTo(start_x, start_y + y as u16), Print("│"),
            MoveTo(start_x + (BOARD_WIDTH * 2 + 1) as u16, start_y + y as u16), Print("│")
        )?;
    }
     for x in 0..=BOARD_WIDTH * 2 {
        queue!(
            stdout,
            MoveTo(start_x + x as u16, start_y), Print("─"),
             MoveTo(start_x + x as u16, start_y + (BOARD_HEIGHT + 1) as u16), Print("─")
        )?;
    }
    queue!(stdout, MoveTo(start_x, start_y), Print("┌"))?;
    queue!(stdout, MoveTo(start_x + (BOARD_WIDTH * 2 + 1) as u16, start_y), Print("┐"))?;
    queue!(stdout, MoveTo(start_x, start_y + (BOARD_HEIGHT + 1) as u16), Print("└"))?;
    queue!(stdout, MoveTo(start_x + (BOARD_WIDTH * 2 + 1) as u16, start_y + (BOARD_HEIGHT + 1) as u16), Print("┘"))?;
    queue!(stdout, ResetColor)?;

    // Draw locked pieces
    for y in 0..BOARD_HEIGHT {
        for x in 0..BOARD_WIDTH {
            let board_y = y + (BOARD_HEIGHT_TOTAL - BOARD_HEIGHT);
            if board_y >= BOARD_HEIGHT_TOTAL { continue; }
            let cell = state.board[board_y][x];
            let draw_x = start_x + 1 + (x * 2) as u16;
            let draw_y = start_y + 1 + y as u16;
            match cell {
                Cell::Occupied(color) => {
                    queue!(stdout, MoveTo(draw_x, draw_y), SetBackgroundColor(color), Print("  "), ResetColor)?;
                }
                Cell::Empty => {} // Leave empty cells blank
            }
        }
    }

    // --- Draw Ghost Piece ---
    if !state.game_over {
        let mut ghost_piece = state.current_piece.clone(); // Clone the current piece
        let mut current_blocks = ghost_piece.get_blocks();

        // Move ghost down until invalid
        // **FIX:** Call is_valid_position on `state`, not `ghost_piece`
        while state.is_valid_position(&current_blocks) {
            ghost_piece.y += 1; // Update ghost piece's internal y
            // Calculate next potential block positions based on updated y
             for i in 0..4 {
                 current_blocks[i].1 += 1;
             }
        }
        // Step back one to the last valid position
        ghost_piece.y -= 1;

        let ghost_blocks = ghost_piece.get_blocks(); // Get blocks at final ghost position
        let ghost_color = Color::DarkGrey;

        for &(px, py) in &ghost_blocks {
            let visible_y = py - (BOARD_HEIGHT_TOTAL - BOARD_HEIGHT) as isize;
            if px >= 0 && px < BOARD_WIDTH as isize && visible_y >= 0 && visible_y < BOARD_HEIGHT as isize {
                // Check if the board cell is actually empty before drawing ghost
                if py >= 0 && state.board[py as usize][px as usize] == Cell::Empty {
                    let draw_x = start_x + 1 + (px * 2) as u16;
                    let draw_y = start_y + 1 + visible_y as u16;
                    queue!(stdout, MoveTo(draw_x, draw_y), SetForegroundColor(ghost_color), Print("::"), ResetColor)?;
                }
            }
        }
    }


    // --- Draw Current Piece ---
    if !state.game_over {
        let blocks = state.current_piece.get_blocks();
        let color = state.current_piece.color();
        for &(px, py) in &blocks {
            let visible_y = py - (BOARD_HEIGHT_TOTAL - BOARD_HEIGHT) as isize;
            if px >= 0 && px < BOARD_WIDTH as isize && visible_y >= 0 && visible_y < BOARD_HEIGHT as isize {
                let draw_x = start_x + 1 + (px * 2) as u16;
                let draw_y = start_y + 1 + visible_y as u16;
                queue!(stdout, MoveTo(draw_x, draw_y), SetBackgroundColor(color), Print("  "), ResetColor)?;
            }
        }
    }

    // --- Draw Info Panel ---
    // (Info panel drawing code remains the same)
    let info_x = start_x + board_draw_width as u16 + 3;
    let info_y = start_y + 1;
    if info_x + 10 < term_width {
        queue!(stdout, MoveTo(info_x, info_y), SetForegroundColor(Color::White), Print("Score:"))?;
        queue!(stdout, MoveTo(info_x + 2, info_y + 1), Print(format!("{}", state.score)))?;
        queue!(stdout, MoveTo(info_x, info_y + 3), Print("Lines:"))?;
        queue!(stdout, MoveTo(info_x + 2, info_y + 4), Print(format!("{}", state.lines_cleared)))?;
        queue!(stdout, MoveTo(info_x, info_y + 6), Print("Level:"))?;
        queue!(stdout, MoveTo(info_x + 2, info_y + 7), Print(format!("{}", state.level)))?;
        queue!(stdout, MoveTo(info_x, info_y + 9), Print("Next:"))?;
        let next_piece_data = TETROMINOES[state.next_piece_index];
        let next_piece_color = next_piece_data.0;
        let next_piece_shape = next_piece_data.1[0];
        for y_off in 0..4 {
            queue!(stdout, MoveTo(info_x + 2, info_y + 10 + y_off), Print("        "))?;
        }
        for &(block_x, block_y) in &next_piece_shape {
            let draw_x = info_x + 2 + (block_x * 2) as u16;
            let draw_y = info_y + 10 + block_y as u16;
            queue!(stdout, MoveTo(draw_x, draw_y), SetBackgroundColor(next_piece_color), Print("  "), ResetColor)?;
        }
    } else {
         queue!(stdout, MoveTo(1, term_height.saturating_sub(1)), Print("Terminal too small for stats"))?;
    }


    // --- Draw Game Over ---
    if state.game_over {
        let msg = "GAME OVER";
        let msg_len = msg.len() as u16;
        let msg_x = start_x + (board_draw_width as u16).saturating_sub(msg_len) / 2;
        let msg_y = start_y + (board_draw_height as u16) / 2;
        if msg_x > 0 && msg_y > 0 {
            queue!(
                stdout,
                MoveTo(msg_x, msg_y), SetForegroundColor(Color::White), SetBackgroundColor(Color::Red), Print(msg), ResetColor,
                MoveTo(msg_x.saturating_sub(2).max(1), msg_y + 1), SetForegroundColor(Color::White), Print("Press Q to Quit")
            )?;
        }
    }

    // --- Draw Instructions ---
    let help_y = start_y + board_draw_height as u16 + 2;
    if help_y < term_height && start_x + 50 < term_width {
        queue!(stdout, MoveTo(start_x, help_y), ResetColor, Print("Arrows: Move/Rotate | Space: Hard Drop | Q: Quit"))?;
    }

    stdout.flush()?;
    Ok(())
}


// --- Main Game Loop ---

fn run_game(stdout: &mut Stdout) -> IoResult<()> {
    let mut state = GameState::new();
    let mut last_tick = Instant::now();
    let mut soft_dropping = false;
    draw(stdout, &state)?; // Initial draw

    loop {
        if state.game_over {
            // Game over loop: only process Quit or Resize
            if event::poll(Duration::from_millis(100))? {
                match event::read()? {
                    Event::Key(KeyEvent { code, kind: event::KeyEventKind::Press, .. })
                        if code == KeyCode::Char('q') || code == KeyCode::Char('Q') => break,
                    Event::Resize(_, _) => draw(stdout, &state)?, // Redraw on resize
                    _ => {}
                }
            }
            continue;
        }

        let current_tick_rate = if soft_dropping { FAST_TICK_RATE.min(state.tick_rate) } else { state.tick_rate };
        let time_since_last_tick = last_tick.elapsed();
        let mut needs_redraw = false;

        // --- Game Tick ---
        if time_since_last_tick >= current_tick_rate {
            state.tick();
            last_tick = Instant::now();
            needs_redraw = true;
            if state.game_over { draw(stdout, &state)?; continue; } // Draw final state and enter game over loop
        }

        // --- Input Handling ---
        let timeout = current_tick_rate.saturating_sub(time_since_last_tick);
        if event::poll(timeout)? {
            match event::read()? {
                Event::Key(KeyEvent { code, kind: event::KeyEventKind::Press, .. }) => {
                    match code {
                        KeyCode::Left => needs_redraw = state.move_piece(-1, 0),
                        KeyCode::Right => needs_redraw = state.move_piece(1, 0),
                        KeyCode::Up => { state.rotate_piece(); needs_redraw = true; },
                        KeyCode::Char(' ') => { state.hard_drop(); last_tick = Instant::now(); needs_redraw = true; },
                        KeyCode::Char('q') | KeyCode::Char('Q') => break, // Quit
                        KeyCode::Down => {
                            if !soft_dropping {
                                soft_dropping = true;
                                // **FIX:** Removed redundant needs_redraw assignment here
                            }
                            state.score += 1;
                            state.tick(); // Tick immediately on soft drop press
                            last_tick = Instant::now();
                            needs_redraw = true; // Redraw after soft drop move/tick
                        }
                        _ => {}
                    }
                },
                Event::Key(KeyEvent { code: KeyCode::Down, kind: event::KeyEventKind::Release, .. }) => {
                    if soft_dropping { soft_dropping = false; /* Redraw not strictly needed on release */ }
                },
                Event::Resize(_, _) => needs_redraw = true,
                _ => {}
            }
        }

        // --- Rendering ---
        if needs_redraw { draw(stdout, &state)?; }

    } // End main loop

    Ok(())
}

// --- Entry Point ---

fn main() -> IoResult<()> {
    let mut stdout = stdout();
    enable_raw_mode()?;
    execute!(stdout, EnterAlternateScreen, Hide)?;

    let game_result = run_game(&mut stdout); // Run game in its own scope

    // --- Cleanup (always runs) ---
    execute!(stdout, Show, LeaveAlternateScreen)?;
    disable_raw_mode()?;

    game_result // Return result (Ok or Err)
}