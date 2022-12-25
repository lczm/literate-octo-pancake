#include "../utils.cpp"

void s() {
    vector<vector<char>> grid = get_grid_char("in");

    auto find = [&](char c) {
        for (int i = 0; i < grid.size(); i++) {
            for (int j = 0; j < grid[i].size(); j++) {
                if (grid[i][j] == c)
                    return make_pair(i, j);
            }
        }
        return make_pair(0, 0);
    };

    // Find S & E
    auto s = find('S');
    auto e = find('E');

    // Set start and end to be at 'a' and 'z'
    grid[s.first][s.second] = 'a';
    grid[e.first][e.second] = 'z';

    auto search = [&](pair<int, int> start, pair<int, int> end) {
        queue<vector<pair<int, int>>> q;
        q.push({ start });
        set<pair<int, int>> seen;

        while (!q.empty()) {
            vector<pair<int, int>> path = q.front();
            q.pop();
            auto current = path.back();

            if (seen.find(current) == seen.end()) {
                seen.insert(current);
                if (current == end)
                    return path.size() - 1;

                auto current_height = int(grid[current.first][current.second]);
                for (auto direction: directions_four) {
                    auto p = make_pair(current.first + direction.first,
                                       current.second + direction.second);
                    if (p.first >= 0 && p.first < grid.size() &&
                        p.second >= 0 && p.second < grid[0].size())  {
                        auto new_height = int(grid[p.first][p.second]);
                        if (new_height <= current_height + 1) {
                            // So that the other directions can use the same one
                            vector<pair<int, int>> path_copy = path;
                            path_copy.push_back(p);
                            q.push(path_copy);
                        }
                    }
                }
            }
        }
        return (unsigned long)0;
    };

    print_grid(grid);

    cout << "Part 1: " << search(s, e) << endl;

    vector<pair<int, int>> possible_starts;
    for (int i = 0; i < grid.size(); i++) {
        for (int j = 0; j < grid[i].size(); j++) {
            if (grid[i][j] == 'a') {
                possible_starts.push_back(make_pair(i, j));
            }
        }
    }

    vector<int> possible_path_lengths;
    for (auto possible_start : possible_starts) {
        auto length = search(possible_start, e);
        if (length != 0)
            possible_path_lengths.push_back(length);
    }

    int minimum_path_length = INT_MAX;
    for (auto possible_path_length : possible_path_lengths) {
        if (possible_path_length < minimum_path_length)
            minimum_path_length = possible_path_length;
    }

    cout << "Part 2: " << minimum_path_length << endl;
}

int main() {
    s();
    return 0;
}

