// src/main.rs

use crossterm::{
    cursor::{Hide, MoveTo, Show},
    // Removed KeyModifiers from here as it wasn't used
    event::{self, Event, KeyCode, KeyEvent},
    execute, queue,
    style::{Color, Print, ResetColor, SetBackgroundColor, SetForegroundColor},
    terminal::{
        disable_raw_mode, enable_raw_mode, size, Clear, ClearType, EnterAlternateScreen,
        LeaveAlternateScreen,
    },
    // Removed the incorrect 'Result' import
};
// Removed 'seq::SliceRandom' as it wasn't used
use rand::Rng;
use std::io::{stdout, Stdout, Write, Result as IoResult}; // Import std::io::Result
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
            // Need to check y >= 0 before indexing board
            if y >= 0 && self.board[y as usize][x as usize] != Cell::Empty {
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
        let rotated_blocks_relative = self.current_piece.get_rotated_blocks(); // Gets absolute coords

        // Calculate blocks relative to the piece's new potential position (x,y)
        // This seems overly complex, let's simplify the wall kick logic.
        // get_rotated_blocks already gives the absolute positions if rotation happens *without* moving x,y.
        // We just need to test these absolute positions, potentially shifted by kicks.

        // Basic Wall Kick attempt (simple shifts)
        // Order: No kick, horizontal kicks, vertical kicks
        let kicks = [(0, 0), (-1, 0), (1, 0), (-2, 0), (2, 0), (0, -1), (0, 1)]; // Added (0,1) kick

        let original_x = self.current_piece.x;
        let original_y = self.current_piece.y;
        let next_rotation_index = (self.current_piece.rotation + 1) % 4;

        for &(kick_x, kick_y) in &kicks {
            let potential_x = original_x + kick_x;
            let potential_y = original_y + kick_y;

            // Get the shape definition for the next rotation
            let shape = TETROMINOES[self.current_piece.tetromino_index].1[next_rotation_index];
            let mut potential_blocks_absolute = [(0isize, 0isize); 4];
            let mut valid = true;

            // Calculate the absolute position of each block in the potential rotated & kicked position
            for i in 0..4 {
                let abs_x = potential_x + shape[i].0;
                let abs_y = potential_y + shape[i].1;
                potential_blocks_absolute[i] = (abs_x, abs_y);

                // Perform validation checks directly here for efficiency
                 if abs_x < 0 || abs_x >= BOARD_WIDTH as isize || abs_y < 0 || abs_y >= BOARD_HEIGHT_TOTAL as isize {
                    valid = false; // Out of bounds
                    break;
                }
                if abs_y >= 0 && self.board[abs_y as usize][abs_x as usize] != Cell::Empty {
                     valid = false; // Collision with existing blocks
                     break;
                 }
            }


            // If this kicked position is valid, apply the rotation and kick offset
            if valid {
                self.current_piece.x = potential_x;
                self.current_piece.y = potential_y;
                self.current_piece.rotation = next_rotation_index;
                return; // Rotation successful
            }
        }
        // If no kick works, rotation fails, piece remains unchanged.
    }

    fn hard_drop(&mut self) {
        let mut distance = 0;
        while self.move_piece(0, 1) {
            distance += 1;
        }
        self.score += 2 * distance; // Score based on distance dropped
        self.lock_piece();
    }

    fn lock_piece(&mut self) {
        let blocks = self.current_piece.get_blocks();
        let color = self.current_piece.color();
        for &(x, y) in &blocks {
             // Ensure coordinates are valid before accessing board
            if y >= 0 && y < BOARD_HEIGHT_TOTAL as isize && x >= 0 && x < BOARD_WIDTH as isize {
                // Check bounds again just to be safe, although is_valid should prevent out of bounds lock
                 self.board[y as usize][x as usize] = Cell::Occupied(color);
             } else if y < 0 {
                // If any part of the piece locks above the visible area, it's game over
                self.game_over = true;
                 return; // Exit immediately on game over condition during lock
             }
        }

        // Only clear lines and spawn if not game over
        if !self.game_over {
             self.clear_lines();
             // Spawn might trigger game over if the new piece immediately collides
             self.spawn_new_piece();
        }
    }


    fn clear_lines(&mut self) {
        let mut lines_to_clear = Vec::new();
        // Check from bottom up, including the top hidden row where pieces might lock partially
        for y in (0..BOARD_HEIGHT_TOTAL).rev() {
            // Check if the row is full
            if self.board[y].iter().all(|&cell| cell != Cell::Empty) {
                lines_to_clear.push(y);
            }
        }


        if !lines_to_clear.is_empty() {
            let num_cleared = lines_to_clear.len() as u32;
            self.lines_cleared += num_cleared;

            // Scoring: 1: 100, 2: 300, 3: 500, 4: 800 points per level
            let base_points = match num_cleared {
                1 => 100,
                2 => 300,
                3 => 500,
                4 => 800,
                _ => 0, // Should not happen with standard board width
            };
             let points = base_points * self.level;
            self.score += points;

            // --- Level Up Logic ---
            let new_level = 1 + self.lines_cleared / LEVEL_UP_LINES;
             if new_level > self.level {
                self.level = new_level;
                // Decrease tick rate, making the game faster
                let new_tick_millis = (self.tick_rate.as_millis() as f64 * ACCELERATION_FACTOR) as u64;
                // Ensure tick rate doesn't go below a minimum threshold
                self.tick_rate = Duration::from_millis(new_tick_millis.max(50)); // e.g., 50ms minimum tick
            }

            // --- Remove Cleared Lines and Shift ---
             // Sort indices in descending order to avoid index issues after removal
             lines_to_clear.sort_unstable_by(|a, b| b.cmp(a));

            for &line_y in &lines_to_clear {
                 self.board.remove(line_y);
             }

            // Add new empty lines at the top (index 0)
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
            // Could not move down, potential lock
             // Before locking, check if the piece is entirely above the visible board.
             // If so, it's game over. (This check is now partly in lock_piece)
             self.lock_piece();
        }
    }
}

// --- Rendering ---

// Use std::io::Result for functions interacting with the terminal
fn draw(stdout: &mut Stdout, state: &GameState) -> IoResult<()> {
    let (term_width, term_height) = size()?;

    // Calculate offsets to center the game board (roughly)
    let board_draw_width = BOARD_WIDTH * 2 + 2; // Each cell is 2 chars wide + borders
    let board_draw_height = BOARD_HEIGHT + 2; // Visible height + borders
    let info_width = 20; // Width for score/next piece info

    let total_width = board_draw_width + info_width;
    let start_x = if term_width > total_width as u16 { (term_width - total_width as u16) / 2 } else { 1 }; // Min 1 margin
    let start_y = if term_height > board_draw_height as u16 { (term_height - board_draw_height as u16) / 2 } else { 1 }; // Min 1 margin

    queue!(stdout, Clear(ClearType::All), Hide)?;

    // Draw board border
    let border_color = Color::DarkGrey;
    queue!(stdout, SetForegroundColor(border_color))?;
    for y in 0..=BOARD_HEIGHT {
        queue!(
            stdout,
            MoveTo(start_x, start_y + y as u16),
            Print("│"), // Left
            MoveTo(start_x + (BOARD_WIDTH * 2 + 1) as u16, start_y + y as u16),
            Print("│") // Right
        )?;
    }
     for x in 0..=BOARD_WIDTH * 2 {
        let char = if x % 2 == 0 { "─" } else {"─"}; // Can simplify to just "─"
        queue!(
            stdout,
            MoveTo(start_x + x as u16, start_y),
             Print(char), // Top border
             MoveTo(start_x + x as u16, start_y + (BOARD_HEIGHT + 1) as u16),
             Print(char) // Bottom border
        )?;
    }
    // Corner pieces
    queue!(stdout, MoveTo(start_x, start_y), Print("┌"))?;
    queue!(stdout, MoveTo(start_x + (BOARD_WIDTH * 2 + 1) as u16, start_y), Print("┐"))?;
    queue!(stdout, MoveTo(start_x, start_y + (BOARD_HEIGHT + 1) as u16), Print("└"))?;
    queue!(stdout, MoveTo(start_x + (BOARD_WIDTH * 2 + 1) as u16, start_y + (BOARD_HEIGHT + 1) as u16), Print("┘"))?;
    queue!(stdout, ResetColor)?; // Reset color after border


    // Draw locked pieces on the board (only visible area)
    for y in 0..BOARD_HEIGHT { // Iterate through visible height
        for x in 0..BOARD_WIDTH {
            let board_y = y + (BOARD_HEIGHT_TOTAL - BOARD_HEIGHT); // Adjust index to get from full board data
            if board_y >= BOARD_HEIGHT_TOTAL { continue; } // Should not happen, but safety check

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
                        ResetColor // Reset background color immediately after block
                    )?;
                }
                Cell::Empty => {
                     // Optionally draw empty cells with a background or just leave them empty
                     // queue!(
                     //    stdout,
                     //    MoveTo(draw_x, draw_y),
                     //    Print("..") // Example: light dots for empty cells
                    // )?;
                    // Current: Does nothing, leaves terminal background
                }
            }
        }
    }

    // Draw Ghost Piece (preview of where the piece will land)
    if !state.game_over {
        let mut ghost_piece = state.current_piece; // Copy current piece
        let mut ghost_y = ghost_piece.y;
        let mut current_blocks = ghost_piece.get_blocks();
        while ghost_piece.is_valid_position(&current_blocks) {
            ghost_y += 1;
             // Calculate next potential block positions
             for i in 0..4 {
                 current_blocks[i].1 += 1; // Move down one step for the check
             }
        }
        // The last valid position was one step *before* the loop condition failed
        ghost_y -= 1;


        // Get the blocks for the final ghost position
        let ghost_shape = TETROMINOES[ghost_piece.tetromino_index].1[ghost_piece.rotation];
        let ghost_color = Color::DarkGrey; // Color for ghost

         for i in 0..4 {
             let px = ghost_piece.x + ghost_shape[i].0;
             let py = ghost_y + ghost_shape[i].1; // Use the calculated final ghost_y

            // Only draw ghost if within the visible board area and cell is empty
            let visible_y = py - (BOARD_HEIGHT_TOTAL - BOARD_HEIGHT) as isize;
             if px >= 0 && px < BOARD_WIDTH as isize && visible_y >= 0 && visible_y < BOARD_HEIGHT as isize {
                 // Check if the target cell for the ghost is actually empty on the board
                 let board_check_y = py as usize; // Use absolute y for board check
                 if board_check_y < state.board.len() && state.board[board_check_y][px as usize] == Cell::Empty {
                    let draw_x = start_x + 1 + (px * 2) as u16;
                    let draw_y = start_y + 1 + visible_y as u16;
                    queue!(
                        stdout,
                        MoveTo(draw_x, draw_y),
                        SetForegroundColor(ghost_color),
                        Print("::"), // Use different pattern for ghost
                        ResetColor
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
                    ResetColor // Reset background color immediately after block
                )?;
            }
        }
    }

    // Draw Info Panel (Score, Level, Next Piece)
    let info_x = start_x + board_draw_width as u16 + 3;
    let info_y = start_y + 1;

    // Check if there's enough space to draw the info panel
    if info_x + 10 < term_width { // Rough check for info panel width
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

        // Clear previous preview area (4x2 grid approx)
        for y_off in 0..4 {
            queue!(stdout, MoveTo(info_x + 2, info_y + 10 + y_off), Print("        "))?; // 4 * 2 spaces wide
        }
        // Draw new preview blocks relative to the preview area
        for &(block_x, block_y) in &next_piece_shape {
            // Adjust coords to be relative to top-left of a 4x4 grid
            let draw_x = info_x + 2 + (block_x * 2) as u16;
            let draw_y = info_y + 10 + block_y as u16;
             queue!(
                stdout,
                MoveTo(draw_x, draw_y),
                SetBackgroundColor(next_piece_color),
                Print("  "),
                ResetColor // Reset background color immediately
            )?;
        }
    } else {
        // Terminal too small for info panel, maybe print a message?
        queue!(stdout, MoveTo(1, term_height -1), Print("Terminal too small for stats"))?;
    }


    // Draw Game Over message
    if state.game_over {
        let msg = "GAME OVER";
        let msg_len = msg.len() as u16;
        // Center message within the board area
        let msg_x = start_x + (board_draw_width as u16).saturating_sub(msg_len) / 2;
        let msg_y = start_y + (board_draw_height as u16) / 2;

         // Ensure message coordinates are valid
        if msg_x > 0 && msg_y > 0 {
            queue!(
                stdout,
                MoveTo(msg_x, msg_y),
                SetForegroundColor(Color::White),
                SetBackgroundColor(Color::Red), // Background for emphasis
                Print(msg),
                ResetColor, // Reset both fg and bg
                MoveTo(msg_x.saturating_sub(2).max(1), msg_y + 1), // Adjust for second line, ensure within bounds
                 SetForegroundColor(Color::White),
                Print("Press Q to Quit")
            )?;
        }
    }

    // --- Instructions ---
     let help_y = start_y + board_draw_height as u16 + 2; // Below the board
     // Check if there's space below the board for instructions
     if help_y < term_height && start_x + 50 < term_width { // Check width too
        queue!(stdout, MoveTo(start_x, help_y), ResetColor, Print("Arrows: Move/Rotate | Space: Hard Drop | Q: Quit"))?;
     }


    stdout.flush()?; // Write all queued commands to the terminal
    Ok(())
}

// --- Main Game Loop ---

// Use std::io::Result
fn run_game(stdout: &mut Stdout) -> IoResult<()> {
    let mut state = GameState::new();
    let mut last_tick = Instant::now();
    let mut soft_dropping = false;

    // Initial draw
    draw(stdout, &state)?;

    loop {
        if state.game_over {
            // If game is over, just wait for quit command
             if event::poll(Duration::from_millis(100))? { // Poll frequently for quit
                 match event::read()? {
                    Event::Key(KeyEvent { code, kind: event::KeyEventKind::Press, .. }) => {
                         if code == KeyCode::Char('q') || code == KeyCode::Char('Q') {
                             break;
                         }
                     }
                      Event::Resize(_, _) => { // Allow redraw on resize even when game over
                         draw(stdout, &state)?;
                     }
                     _ => {}
                 }
             }
             continue; // Skip rest of the loop if game is over
        }


        // --- Timing ---
        let current_tick_rate = if soft_dropping {
             FAST_TICK_RATE.min(state.tick_rate) // Use faster rate if soft dropping, but not faster than level speed
        } else {
            state.tick_rate
        };

        let time_since_last_tick = last_tick.elapsed();
        let mut needs_redraw = false;

        if time_since_last_tick >= current_tick_rate {
            state.tick(); // Attempt to move piece down
            last_tick = Instant::now(); // Reset timer *after* ticking
             needs_redraw = true; // Redraw after a game tick occurs
             if state.game_over {
                 draw(stdout, &state)?; // Draw final game over state immediately
                 continue; // Go to game over handling at the top of the loop
             }
        }


        // --- Input Handling ---
        // Poll for events with a timeout based on remaining time until next tick
        let timeout = current_tick_rate.saturating_sub(time_since_last_tick);
        if event::poll(timeout)? {
            match event::read()? {
                Event::Key(KeyEvent { code, kind: event::KeyEventKind::Press, .. }) => {
                     match code {
                        KeyCode::Left => {
                            needs_redraw = state.move_piece(-1, 0);
                        }
                        KeyCode::Right => {
                            needs_redraw = state.move_piece(1, 0);
                        }
                        KeyCode::Down => {
                            if !soft_dropping { // Start soft drop on first press
                                 soft_dropping = true;
                                 needs_redraw = true; // Need to redraw to reflect potential faster speed indicator?
                             }
                             state.score += 1; // Small score bonus for each step of soft drop
                             // Immediately tick down when pressed
                             state.tick(); // This might lock the piece
                             last_tick = Instant::now(); // Reset tick timer after manual move
                             needs_redraw = true; // Redraw after the move/tick
                         }
                        KeyCode::Up => {
                            state.rotate_piece(); // rotate_piece doesn't return bool, assume redraw needed
                            needs_redraw = true;
                        }
                        KeyCode::Char(' ') => { // Spacebar for Hard Drop
                            state.hard_drop(); // hard_drop locks and spawns, triggering redraw internally maybe?
                             last_tick = Instant::now(); // Reset tick timer after hard drop
                             needs_redraw = true; // Ensure redraw after hard drop
                         }
                        KeyCode::Char('q') | KeyCode::Char('Q') => {
                            break; // Quit
                        }
                        _ => {} // Ignore other keys
                    }
                 },
                 Event::Key(KeyEvent { code: KeyCode::Down, kind: event::KeyEventKind::Release, .. }) => {
                    // Stop soft dropping when Down arrow is released
                     if soft_dropping {
                         soft_dropping = false;
                         needs_redraw = true; // May need redraw if speed indicator changes
                     }
                 },
                Event::Resize(_, _) => {
                    // Redraw immediately on resize
                     needs_redraw = true;
                }
                _ => {} // Ignore other events
            }
        }

        // --- Rendering ---
        if needs_redraw {
             draw(stdout, &state)?;
        }


    } // End main loop

    Ok(())
}

// --- Entry Point ---

// Use std::io::Result
fn main() -> IoResult<()> {
    let mut stdout = stdout();
    // Setup terminal
    enable_raw_mode()?;
    // Use try! or ? for error handling during setup
    execute!(stdout, EnterAlternateScreen, Hide)?;

    // Run the game logic in a separate scope or function
    // This ensures that cleanup happens even if run_game panics (though explicit cleanup is better)
    let game_result = run_game(&mut stdout);

    // --- Cleanup ---
    // Ensure cleanup happens regardless of game_result outcome
    execute!(stdout, Show, LeaveAlternateScreen)?;
    disable_raw_mode()?;

    // Return the result from the game loop
    game_result
}