#include "../utils.cpp"

void s() {
    vector<vector<int>> grid = get_grid("in");
    print_grid(grid);

    int visible = 0;
    int length = grid.size();
    int height = grid[0].size();

    // Perimeter of the grid
    visible += length * 2;
    visible += (height - 2) * 2;

    vector<pair<int, int>> directions = {
        {1, 0}, {-1, 0}, {0, -1}, {0, 1},
    };

    auto is_edge = [length, height](int l, int h) {
        if (l == 0)
            return true;
        if (h == 0)
            return true;
        if (l == length - 1)
            return true;
        if (h == height - 1)
            return true;
        return false;
    };

    function<int(int, int, int, int, pair<int, int>)> traverse;
    traverse = [&](int ol, int oh, int l, int h, pair<int, int> direction) {
        if (grid[l + direction.first][h + direction.second] >= grid[ol][oh]) {
            return 0;
        }

        if (is_edge(l + direction.first, h + direction.second)) {
            if (grid[ol][oh] > grid[l + direction.first][h + direction.second]) {
                return 1;
            }
        }

        return traverse(ol, oh, l + direction.first, h + direction.second, direction);
    };

    function<int(int, int, int, int, pair<int, int>)> scenic;
    scenic = [&](int ol, int oh, int l, int h, pair<int, int> direction) {
        if (grid[l + direction.first][h + direction.second] >= grid[ol][oh]) {
            return max(
                    abs(l + direction.first - ol),
                    abs(h + direction.second - oh));
        }

        if (is_edge(l + direction.first, h + direction.second)) {
            if (grid[ol][oh] > grid[l + direction.first][h + direction.second]) {
                return max(
                        abs(l + direction.first - ol),
                        abs(h + direction.second - oh));
            }
        }

        return scenic(ol, oh, l + direction.first, h + direction.second, direction);
    };

    int max_scenic = 0;

    for (int i = 1; i < length - 1; i++) {
        for (int j = 1; j < height - 1; j++) {
            vector<int> v;
            for (auto p : directions) {
                auto interval = traverse(i, j, i, j, p);
                if (interval) {
                    visible += interval;
                    break;
                }
            }
            for (auto p : directions) {
                v.push_back(scenic(i, j, i, j, p));
            }
            int sum = 1;
            for (const auto e : v)
                sum *= e;
            if (max_scenic < sum)
                max_scenic = sum;
        }
    }

    cout << "Part1: " << visible << endl;
    cout << "Part2: " << max_scenic << endl;
}

int main() {
    s();
    return 0;
}
