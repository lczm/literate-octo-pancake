#include "../utils.cpp"

const bool part1 = false;
const bool part2 = true;

void get_hamiltonian_area(set<int>& not_possible,
                          vector<int>& row_count,
                          pair<int, int> sensor,
                          pair<int, int> beacon,
                          int y_key) {
    int max_distance =
        abs(sensor.first - beacon.first) + abs(sensor.second - beacon.second);

    // If the row we are checking for is out of range, just skip the computation
    if (y_key > sensor.second && sensor.second + max_distance < y_key) {
        return;
    }
    if (y_key < sensor.second && sensor.second - max_distance > y_key) {
        return;
    }

    int x_distance = 0;
    if (y_key < sensor.second) {
        x_distance = sensor.second - max_distance - y_key;
    }
    if (y_key > sensor.second) {
        x_distance = sensor.second + max_distance - y_key;
    }

    int start = min(sensor.first - x_distance, sensor.first + x_distance);
    int end = max(sensor.first - x_distance, sensor.first + x_distance);

    if (part1) {
        for (int i = start; i <= end; i++) {
            if (beacon.second == y_key)
                continue;
            not_possible.insert(i);
        }
    }
}

void test_hamiltonian_distance() {
    auto sensor = make_pair(12, 14);
    auto beacon = make_pair(10, 16);

    int padding = 5;
    sensor.first += 5;
    sensor.second += 5;
    beacon.first += 5;
    beacon.second += 5;

    vector<vector<char>> grid(30, vector<char>(30, '.'));

    int max_distance =
        abs(sensor.first - beacon.first) + abs(sensor.second - beacon.second);

    cout << max_distance << endl;

    // Spread out horizontally + the quadrants
    for (int i = 0; i < max_distance; i++) {
        for (int j = max_distance - i; j != 0; j--) {
            grid[sensor.second - i][sensor.first - j] = '#';
            grid[sensor.second + i][sensor.first + j] = '#';
            grid[sensor.second - i][sensor.first + j] = '#';
            grid[sensor.second + i][sensor.first - j] = '#';
        }
        grid[sensor.second + i][sensor.first] = '#';
        grid[sensor.second - i][sensor.first] = '#';
    }
    // The top and bottom
    grid[sensor.second + max_distance][sensor.first] = '#';
    grid[sensor.second - max_distance][sensor.first] = '#';

    grid[sensor.second][sensor.first] = 's';
    grid[beacon.second][beacon.first] = 'b';

    print_grid(grid);
}

void s() {
    vector<string> lines = get_lines("in");

    int row = 2000000;
    unordered_map<int, bool> not_possible;
    set<int> xs;

    int distress_min = 0;
    int distress_max = 4000000;
    vector<int> row_count(distress_max, 0);

    vector<pair<int, int>> sensors;
    vector<pair<int, int>> beacons;

    for (auto line : lines) {
        line = line.substr(line.find("=") + 1, line.size());
        auto sensor_x = stoi(line.substr(0, line.find(",")));

        line = line.substr(line.find("=") + 1, line.size());
        auto sensor_y = stoi(line.substr(0, line.find(":")));

        line = line.substr(line.find("=") + 1, line.size());
        auto beacon_x = stoi(line.substr(0, line.find(",")));

        line = line.substr(line.find("=") + 1, line.size());
        auto beacon_y = stoi(line.substr(0, line.size()));

        sensors.push_back(make_pair(sensor_x, sensor_y));
        beacons.push_back(make_pair(beacon_x, beacon_y));

        // cout << sensor_x << " " << sensor_y << " " << beacon_x << " "
        //      << beacon_y << endl;

        if (part1) {
            get_hamiltonian_area(xs, row_count, make_pair(sensor_x, sensor_y),
                                 make_pair(beacon_x, beacon_y), row);
        }
    }

    if (part1) {
        cout << xs.size() << endl;
    }

    auto available = [&](int x, int y) {
        for (int i = 0; i < sensors.size(); i++) {
            auto sensor = sensors[i];
            auto beacon = beacons[i];
            int max_distance = abs(sensor.first - beacon.first) +
                               abs(sensor.second - beacon.second);
            if (abs(x - sensor.first) + abs(y - sensor.second) <=
                max_distance) {
                if (find(beacons.begin(), beacons.end(), make_pair(x, y)) ==
                    beacons.end()) {
                    return false;
                }
            }
        }
        return true;
    };

    const vector<pair<int, int>> directions = {
        {1, 1},
        {-1, -1},
        {-1, 1},
        {1, -1},
    };

    if (part2) {
        for (int i = 0; i < sensors.size(); i++) {
            auto sensor = sensors[i];
            auto beacon = beacons[i];
            long max_distance = abs(sensor.first - beacon.first) +
                                abs(sensor.second - beacon.second);
            // keep rounding off the perimeter
            for (int dx = 0; dx < max_distance; dx++) {
                long dy = max_distance + 1 - dx;
                for (auto [x, y] : directions) {
                    long xx = sensor.first + (dx * x);
                    long yy = sensor.second + (dy * y);
                    if (xx >= distress_min && xx <= distress_max &&
                        yy >= distress_min && yy <= distress_max)
                        if (available(xx, yy))
                            cout << (long)(xx * 4000000 + yy) << endl;
                }
            }
        }
    }
}

int main() {
    s();
    return 0;
}