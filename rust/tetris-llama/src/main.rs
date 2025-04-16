use crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use rand::Rng;
use std::io;

const GRID_WIDTH: usize = 10;
const GRID_HEIGHT: usize = 20;

struct Tetromino {
    x: i32,
    y: i32,
    cells: Vec<(i8, i8)>,
}

impl Tetromino {
    fn new(x: i32, y: i32, cells: Vec<(i8, i8)>) -> Self {
        Tetromino { x, y, cells }
    }

    fn rotate(&mut self) {
        let mut rotated_cells = vec![];
        for (dx, dy) in &self.cells {
            rotated_cells.push((-dy, *dx));
        }
        self.cells = rotated_cells;
    }
}

struct Grid {
    cells: Vec<Vec<bool>>,
}

impl Grid {
    fn new(width: usize, height: usize) -> Self {
        let mut grid = vec![];
        for _ in 0..height {
            let row = vec![false; width];
            grid.push(row);
        }
        Grid { cells: grid }
    }

    fn is_collision(&self, tetromino: &Tetromino) -> bool {
        for (dx, dy) in &tetromino.cells {
            let x = tetromino.x + *dx as i32;
            let y = tetromino.y + *dy as i32;

            if x < 0 || x >= GRID_WIDTH as i32 || y < 0 || y >= GRID_HEIGHT as i32 {
                return true;
            }

            if self.cells[y as usize][x as usize] {
                return true;
            }
        }

        false
    }

    fn remove_row(&mut self, row: usize) -> bool {
        for cell in &self.cells[row] {
            if !*cell {
                return false;
            }
        }

        self.cells.remove(row);
        let new_row = vec![false; GRID_WIDTH];
        self.cells.insert(0, new_row);

        true
    }

    fn add_tetromino(&mut self, tetromino: &Tetromino) -> usize {
        for (dx, dy) in &tetromino.cells {
            let x = tetromino.x + *dx as i32;
            let y = tetromino.y + *dy as i32;

            if y < 0 || y >= GRID_HEIGHT as i32 {
                return 0;
            }

            self.cells[y as usize][x as usize] = true;
        }

        let mut rows_removed = 0;
        for row in (0..GRID_HEIGHT).rev() {
            if self.remove_row(row) {
                rows_removed += 1;
            }
        }

        rows_removed
    }
}

fn get_random_tetromino(x: i32, y: i32) -> Tetromino {
    let mut rng = rand::thread_rng();

    let shape_type = rng.gen_range(0..7);

    match shape_type {
        0 => Tetromino::new(x, y, vec![(-1, 0), (0, 0), (1, 0), (2, 0)]), // I
        1 => Tetromino::new(x, y, vec![(0, -1), (0, 0), (0, 1), (1, 0)]), // J
        2 => Tetromino::new(x, y, vec![(-1, -1), (-1, 0), (0, 0), (1, 0)]), // L
        3 => Tetromino::new(x, y, vec![(0, -2), (0, -1), (0, 0), (0, 1)]), // O
        4 => Tetromino::new(x, y, vec![(-1, -1), (-1, 0), (0, 0), (1, -1)]), // S
        5 => Tetromino::new(x, y, vec![(0, -2), (0, -1), (0, 0), (1, -2)]), // T
        6 => Tetromino::new(x, y, vec![(-1, 0), (-1, 1), (0, 0), (0, 1)]), // Z
        _ => unreachable!(),
    }
}

fn main() -> io::Result<()> {
    enable_raw_mode()?;
    let mut stdout = std::io::stdout();
    execute!(stdout, EnterAlternateScreen)?;
    execute!(stdout, EnableMouseCapture)?;

    let mut grid = Grid::new(GRID_WIDTH, GRID_HEIGHT);
    let mut tetromino = get_random_tetromino(5, 0);

    loop {
        print_grid(&grid, &tetromino)?;
        handle_events(&mut tetromino, &grid);
        if grid.is_collision(&tetromino) {
            break;
        }
        std::thread::sleep(std::time::Duration::from_millis(500));
    }

    let score = grid.add_tetromino(&tetromino);

    print_grid(&grid, &Tetromino::new(0, 0, vec![]))?;

    println!("Game Over. Your score is: {}", score);
    execute!(stdout, LeaveAlternateScreen)?;
    disable_raw_mode()?;
    Ok(())
}

fn handle_events(tetromino: &mut Tetromino, grid: &Grid) {
    if let event::Event::Key(key) = event::read().unwrap() {
        match key.code {
            event::KeyCode::Left => {
                tetromino.x -= 1;
                if grid.is_collision(&tetromino) {
                    tetromino.x += 1;
                }
            }
            event::KeyCode::Right => {
                tetromino.x += 1;
                if grid.is_collision(&tetromino) {
                    tetromino.x -= 1;
                }
            }
            event::KeyCode::Down => {
                tetromino.y += 1;
                if grid.is_collision(&tetromino) {
                    tetromino.y -= 1;
                }
            }
            event::KeyCode::Up => {
                tetromino.rotate();
                if grid.is_collision(&tetromino) {
                    for _ in 0..3 {
                        tetromino.rotate();
                    }
                }
            }
            _ => (),
        }
    } else {
        tetromino.y += 1;
        if grid.is_collision(&tetromino) {
            tetromino.y -= 1;
        }
    }
}

fn print_grid(grid: &Grid, tetromino: &Tetromino) -> io::Result<()> {
    let mut stdout = std::io::stdout();

    execute!(stdout, crossterm::terminal::MoveTo(0, 0))?;

    for y in (0..GRID_HEIGHT).rev() {
        for x in 0..GRID_WIDTH {
            if grid.cells[y][x]
                || tetromino.cells.iter().any(|&(dx, dy)| {
                    let cell_x = tetromino.x + dx as i32;
                    let cell_y = tetromino.y + dy as i32;

                    cell_x == x as i32 && cell_y == y as i32
                })
            {
                print!("X");
            } else {
                print!(".");
            }
        }

        println!();
    }

    stdout.flush()?;
    Ok(())
}

fn print_tetromino(tetromino: &Tetromino) {
    for (dx, dy) in &tetromino.cells {
        let x = tetromino.x + dx as i32;
        let y = tetromino.y + dy as i32;

        println!("({}, {})", x, y);
    }
}
