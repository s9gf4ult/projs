use rand::Rng;
use std::io;
use std::sync::mpsc;
use std::thread;
use std::time::Duration;

// Define the Tetris pieces
const PIECES: [&str; 7] = [
    "####",
    " # \n###",
    "###\n # ",
    "## \n ##",
    " ##\n## ",
    "# #\n## ",
    " # #\n###",
];

// Define the Tetris grid size
const GRID_SIZE: usize = 20;

// Define the Tetris grid
struct Grid {
    cells: [[char; GRID_SIZE]; GRID_SIZE],
}

impl Grid {
    fn new() -> Grid {
        Grid {
            cells: [[' '; GRID_SIZE]; GRID_SIZE],
        }
    }

    fn clear_row(&mut self, row: usize) {
        for i in (0..row).rev() {
            self.cells[i + 1] = self.cells[i];
        }
        self.cells[0] = [' '; GRID_SIZE];
    }

    fn is_row_full(&self, row: usize) -> bool {
        for cell in self.cells[row] {
            if cell == ' ' {
                return false;
            }
        }
        true
    }

    fn is_collision(&self, piece: &Piece, x: usize, y: usize) -> bool {
        for (i, row) in piece.shape.lines().enumerate() {
            for (j, cell) in row.chars().enumerate() {
                if cell == '#' {
                    let grid_x = x + j;
                    let grid_y = y + i;
                    if grid_x >= GRID_SIZE
                        || grid_y >= GRID_SIZE
                        || self.cells[grid_y][grid_x] == '#'
                    {
                        return true;
                    }
                }
            }
        }
        false
    }

    fn place_piece(&mut self, piece: &Piece, x: usize, y: usize) {
        for (i, row) in piece.shape.lines().enumerate() {
            for (j, cell) in row.chars().enumerate() {
                if cell == '#' {
                    let grid_x = x + j;
                    let grid_y = y + i;
                    self.cells[grid_y][grid_x] = '#';
                }
            }
        }
    }
}

// Define a Tetris piece
struct Piece {
    shape: String,
    x: usize,
    y: usize,
}

impl Piece {
    fn new(shape: &str) -> Piece {
        Piece {
            shape: shape.to_string(),
            x: GRID_SIZE / 2,
            y: 0,
        }
    }

    fn rotate(&mut self) {
        // Simple rotation, not perfect but works for the given pieces
        self.shape = match self.shape.as_str() {
            "####" => "####".to_string(),
            " # \n###" => "## \n # \n # ".to_string(),
            "###\n # " => "# \n# \n##".to_string(),
            "## \n ##" => "## \n ##".to_string(),
            " ##\n## " => " ##\n## ".to_string(),
            "# #\n## " => " # \n##".to_string(),
            " # #\n###" => " # \n##\n # ".to_string(),
            _ => self.shape.clone(),
        };
    }
}

fn main() {
    let mut grid = Grid::new();
    let mut rng = rand::thread_rng();
    let mut piece = Piece::new(PIECES[rng.gen_range(0..7)]);
    let mut score = 0;

    // Create a channel to send user input to the game loop
    let (tx, rx) = mpsc::channel();

    thread::spawn(move || loop {
        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");
        tx.send(input.trim().to_string()).unwrap();
    });

    loop {
        // Clear the terminal
        print!("\x1B[2J\x1B[1;1H");

        // Draw the grid
        for (_i, row) in grid.cells.iter().enumerate() {
            for (_j, cell) in row.iter().enumerate() {
                print!("{}", cell);
            }
            println!();
        }

        // Draw the piece
        for (i, row) in piece.shape.lines().enumerate() {
            for (j, cell) in row.chars().enumerate() {
                let grid_x = piece.x + j;
                let grid_y = piece.y + i;
                if grid_x < GRID_SIZE && grid_y < GRID_SIZE {
                    if cell == '#' {
                        print!("#");
                    } else {
                        print!(" ");
                    }
                }
            }
            println!();
        }

        // Display the score
        println!("Score: {}", score);

        // Handle user input
        if let Ok(input) = rx.try_recv() {
            match input.as_str() {
                "a" => {
                    if piece.x > 0 {
                        piece.x -= 1;
                    }
                }
                "d" => {
                    if piece.x + piece.shape.lines().next().unwrap().len() < GRID_SIZE {
                        piece.x += 1;
                    }
                }
                "s" => {
                    if piece.y + piece.shape.lines().count() < GRID_SIZE {
                        piece.y += 1;
                    }
                }
                "w" => piece.rotate(),
                "q" => break,
                _ => (),
            }
        }

        // Check for collision
        if grid.is_collision(&piece, piece.x, piece.y + 1) {
            grid.place_piece(&piece, piece.x, piece.y);
            for i in (0..GRID_SIZE).rev() {
                if grid.is_row_full(i) {
                    grid.clear_row(i);
                    score += 1;
                }
            }
            piece = Piece::new(PIECES[rng.gen_range(0..7)]);
        } else {
            piece.y += 1;
        }

        // Limit the game speed
        thread::sleep(Duration::from_millis(500));
    }
}
