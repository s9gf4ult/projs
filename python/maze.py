import random

# Define maze dimensions
levels = 5
chambers_per_level = 10
chamber_size = 5
corridor_width = 3

# Define maze symbols
wall = "X"
floor = " "
stairs = "S"
corridor = "-"

# Generate maze for each level
maze = []
for level in range(levels):
    # Generate chambers
    chambers = []
    for chamber in range(chambers_per_level):
        # Generate chamber layout
        chamber_layout = [[wall for i in range(chamber_size)] for j in range(chamber_size)]
        for i in range(1, chamber_size-1):
            for j in range(1, chamber_size-1):
                if random.random() < 0.7:
                    chamber_layout[i][j] = floor
        # Add stairs to first and last chambers
        if chamber == 0:
            stairs_pos = random.randint(1, chamber_size-2)
            chamber_layout[stairs_pos][0] = stairs
            last_stairs_pos = stairs_pos
        elif chamber == chambers_per_level-1:
            chamber_layout[last_stairs_pos][chamber_size-1] = stairs
        # Add chamber to list of chambers
        chambers.append(chamber_layout)
    # Connect chambers with corridors
    for i in range(chambers_per_level-1):
        # Determine corridor position
        if i == 0:
            max_pos = chamber_size-1-corridor_width
            if max_pos < corridor_width+1:
                corridor_pos = max_pos
            else:
                corridor_pos = random.randint(corridor_width+1, max_pos)
        else:
            max_pos = chamber_size-1-corridor_width
            if max_pos < corridor_width+1:
                corridor_pos = max_pos
            else:
                corridor_pos = random.randint(corridor_width+1, max_pos)
        # Connect chambers with corridor
        for j in range(corridor_width):
            chambers[i][corridor_pos+j][chamber_size-1] = corridor
            chambers[i+1][corridor_pos+j][0] = corridor
    # Add chambers to maze
    maze.append(chambers)

# Convert maze to list of strings
maze_str = []
for level in maze:
    level_str = []
    for chamber in level:
        chamber_str = ""
        for row in chamber:
            chamber_str += "".join(row) + "\n"
        level_str.append(chamber_str)
    maze_str.append(level_str)

# Print maze
for level in maze_str:
    for chamber in level:
        print(chamber)
