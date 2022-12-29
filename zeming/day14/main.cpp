#include "../utils.cpp"

const int offset = 475;

vector<pair<int, int>> parse(string line) {
    vector<pair<int, int>> coordinates;
    while (line.find("->") != string::npos) {
        auto index = line.find("->");
        auto before = line.substr(0, index - 1);

        auto comma_index = before.find(',');
        auto left = stoi(before.substr(0, comma_index));
        auto right = stoi(before.substr(comma_index + 1, before.size()));

        coordinates.push_back({left, right});
        line = line.substr(index + 3, line.size());
    }

    // Get the last pari
    auto comma_index = line.find(',');
    auto left = stoi(line.substr(0, comma_index));
    auto right = stoi(line.substr(comma_index + 1, line.size()));
    coordinates.push_back({left, right});

    // for (auto& [left, first] : coordinates) {
    //     left -= offset;
    // }

    return coordinates;
}

void s() {
    int height = 1000;
    int length = 1000;

    vector<string> lines = get_lines("in");
    vector<vector<char>> grid(height, vector<char>(length, '.'));

    int max_height = 0;
    for (auto line : lines) {
        cout << line << endl;
        vector<pair<int, int>> coordinates = parse(line);
        for (int i = 0; i < coordinates.size() - 1; i++) {
            auto first = coordinates[i];
            auto second = coordinates[i + 1];

            int min_x = min(first.first, second.first);
            int max_x = max(first.first, second.first);
            int min_y = min(first.second, second.second);
            int max_y = max(first.second, second.second);

            if (max_y > max_height)
                max_height = max_y;

            // Fill in vertically
            if (min_x == max_x) {
                for (int i = min_y; i <= max_y; i++)
                    grid[i][min_x] = '#';
            } else {  // Fill in horizontally
                for (int i = min_x; i <= max_x; i++)
                    grid[min_y][i] = '#';
            }
        }
    }

    int floor_height = max_height + 2;

    for (int i = 0; i < grid[0].size(); i++) {
        grid[floor_height][i] = '#';
    }

    // print_grid(grid);

    // Pour sand in
    auto sand_origin = make_pair(500, 0);
    // sand_origin.first -= offset;
    grid[sand_origin.second][sand_origin.first] = '@';

    bool all_at_rest = false;
    bool out_of_bounds = false;
    auto move_sand = [&](pair<int, int>& sand) {
        // Rock bottom (Part 1)
        // if (sand.second == grid.size() - 1) {
        //     out_of_bounds = true;
        //     return false;
        // }
        // Rock bottom (Part 2)
        if (sand.second == floor_height) {
            out_of_bounds = true;
            return false;
        }

        // Space below sand
        if (grid[sand.second + 1][sand.first] == '.') {
            sand.second++;
            return true;
        }

        // If space immediately below is blocked by sand
        if (grid[sand.second + 1][sand.first] == '#' ||
            grid[sand.second + 1][sand.first] == 'o') {
            if (sand.first != 0) {
                // If there's space to move it diagonally down left
                if (grid[sand.second + 1][sand.first - 1] == '.') {
                    sand.second++;
                    sand.first--;
                    return true;
                }
            } else {
                out_of_bounds = true;
                return false;
            }
            // Move it diagonally down right if there is no
            // space to move it diagonally down left
            if (sand.first != grid[0].size() - 1) {
                if (grid[sand.second + 1][sand.first + 1] == '.') {
                    sand.second++;
                    sand.first++;
                    return true;
                }
            } else {
                out_of_bounds = true;
                return false;
            }
        }

        // Comes to rest
        return false;
    };

    // Part 1
    // int count = 0;
    // vector<pair<int, int>> history;
    // while (!all_at_rest) {
    //     auto sand = sand_origin;
    //     while (move_sand(sand)) {
    //     }
    //     if (out_of_bounds) {
    //         if (history.size() > 0 && history.back().first == sand.first &&
    //             history.back().second == sand.second) {
    //             all_at_rest = true;
    //         } else {
    //             history.push_back(sand);
    //         }
    //     } else {
    //         grid[sand.second][sand.first] = 'o';
    //         count++;
    //     }
    // }
    // cout << "Part1: " << count << endl;

    int previous = 0;
    auto count_sand = [&]() {
        int count = 0;
        for (int i = 0; i < grid.size(); i++) {
            for (int j = 0; j < grid[i].size(); j++) {
                if (grid[i][j] == 'o')
                    count++;
            }
            previous = count;
        }
    };

    while (true) {
        auto sand = sand_origin;
        while (move_sand(sand)) {
        }

        grid[sand.second][sand.first] = 'o';

        int old = previous;
        count_sand();
        if (previous == old) {
            cout << "Part2: " << previous << endl;
            break;
        }

        // print_grid(grid);
        // cout << endl;
    }

    // print_grid(grid);
}

int main() {
    s();
    return 0;
}