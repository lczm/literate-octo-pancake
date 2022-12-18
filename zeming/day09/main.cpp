#include "../utils.cpp"

void s() {
    vector<string> lines = get_lines("in");

    const int max_length = 1000;
    const int start = max_length / 2;
    vector<vector<char>> grid(max_length, vector<char>(max_length, '.'));

    unordered_map<char, pair<int, int>> directions = {
        { 'R', { 0 , 1 } },
        { 'L', { 0 ,-1 } },
        { 'U', { -1, 0 } },
        { 'D', { 1 , 0 } },
    };

    auto head = make_pair<int, int>(start - 1, 0);
    auto tail = make_pair<int, int>(start - 1, 0);
    grid[head.first][head.second] = 'H';

    auto unset = [&](const pair<int, int> coordinate) {
        grid[coordinate.first][coordinate.second] = '.';
    };

    auto set_head = [&](const pair<int, int> coordinate) {
        grid[coordinate.first][coordinate.second] = 'H';
    };

    auto set_tail = [&](const pair<int, int> coordinate) {
        grid[coordinate.first][coordinate.second] = 'T';
    };

    auto too_far = [&](const pair<int, int> head, const pair<int, int> tail) {
        if (abs(head.first - tail.first) <= 1 && abs(head.second - tail.second) <= 1)
            return false;
        return true;
    };

    auto move_tail = [&](const pair<int, int> head, pair<int, int>& tail) {
        if (head.first == tail.first) {
            if (head.second > tail.second)
                tail.second++;
            if (head.second < tail.second)
                tail.second--;
        }
        if (head.second == tail.second) {
            if (head.first > tail.first)
                tail.first++;
            if (head.first < tail.first)
                tail.first--;
        }
        // Consider the diagonal movement
        if (head.first < tail.first && head.second > tail.second) {
            tail.first--;
            tail.second++;
        }
        if (head.first < tail.first && head.second < tail.second) {
            tail.first--;
            tail.second--;
        }
        if (head.first > tail.first && head.second < tail.second) {
            tail.first++;
            tail.second--;
        }
        if (head.first > tail.first && head.second > tail.second) {
            tail.first++;
            tail.second++;
        }
    };

    // print_grid(grid);
    // cout << endl;

    set<pair<int, int>> visited;
    visited.insert(tail);

    for (auto line : lines) {
        auto space_delimiter = line.find(" ");
        auto direction = directions[line.substr(0, space_delimiter)[0]];
        auto magnitude = stoi(line.substr(space_delimiter + 1, line.size()));

        for (int i = 0; i < magnitude; i++) {
            unset(head);
            unset(tail);

            head.first += direction.first;
            head.second += direction.second;

            // Calculate where the tail should go here
            if (too_far(head, tail)) {
                move_tail(head, tail);
                visited.insert(tail);
            }

            set_tail(tail);
            set_head(head);
            // print_grid(grid);
            // cout << endl;
        }
    }

    cout << "Part1: " << visited.size() << endl;
}

void ss() {
    vector<string> lines = get_lines("in");

    const int max_length = 1000;
    const int start = max_length / 2;
    vector<vector<char>> grid(max_length, vector<char>(max_length, '.'));

    unordered_map<char, pair<int, int>> directions = {
        { 'R', { 0 , 1 } },
        { 'L', { 0 ,-1 } },
        { 'U', { -1, 0 } },
        { 'D', { 1 , 0 } },
    };

    vector<pair<int, int>> nodes;
    for (int i = 0; i < 10; i++) {
        nodes.push_back(make_pair(start - 1, 0));
    }

    pair<int, int>* head = &nodes[0];
    pair<int, int>* tail = &nodes[nodes.size() - 1];

    grid[head->first][head->second] = 'H';

    auto too_far = [&](const pair<int, int> head, const pair<int, int> tail) {
        if (abs(head.first - tail.first) <= 1 && abs(head.second - tail.second) <= 1)
            return false;
        return true;
    };

    auto move_tail = [&](const pair<int, int> head, pair<int, int>& tail) {
        if (head.first == tail.first) {
            if (head.second > tail.second)
                tail.second++;
            if (head.second < tail.second)
                tail.second--;
        }
        if (head.second == tail.second) {
            if (head.first > tail.first)
                tail.first++;
            if (head.first < tail.first)
                tail.first--;
        }
        // Consider the diagonal movement
        if (head.first < tail.first && head.second > tail.second) {
            tail.first--;
            tail.second++;
        }
        if (head.first < tail.first && head.second < tail.second) {
            tail.first--;
            tail.second--;
        }
        if (head.first > tail.first && head.second < tail.second) {
            tail.first++;
            tail.second--;
        }
        if (head.first > tail.first && head.second > tail.second) {
            tail.first++;
            tail.second++;
        }
    };

    set<pair<int, int>> visited;
    visited.insert(*tail);

    for (auto line : lines) {
        auto space_delimiter = line.find(" ");
        auto direction = directions[line.substr(0, space_delimiter)[0]];
        auto magnitude = stoi(line.substr(space_delimiter + 1, line.size()));

        for (int i = 0; i < magnitude; i++) {
            head->first += direction.first;
            head->second += direction.second;
            for (int i = 1; i < nodes.size(); i++) {
                if (too_far(nodes[i-1], nodes[i])) {
                    move_tail(nodes[i-1], nodes[i]);
                }
            }
            visited.insert(nodes[nodes.size() - 1]);
        }
    }

    cout << "Part2: " << visited.size() << endl;
}

int main() {
    s();
    ss();
    return 0;
}
