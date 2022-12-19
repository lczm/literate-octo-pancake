#include "../utils.cpp"

struct Instruction {
    int remaining_cycle;
    int value;
};

void s() {
    vector<string> lines = get_lines("in");

    int cycle = 1;
    int register_value = 1;
    int marking = 20;
    deque<Instruction> q;
    vector<pair<int, int>> signals;
    vector<vector<char>> screen(6, vector<char>(41, '.'));
    Instruction ip;

    for (auto line : lines) {
        // During first cycle
        if (line[0] == 'n') { // noop
            ip.remaining_cycle = 1;
            ip.value = 0;
        } else {
            vector<string> splits = split_strings(line);
            int magnitude = stoi(splits[1]);
            ip.remaining_cycle = 2;
            ip.value = magnitude;
        }

        int remaining_cycle = ip.remaining_cycle;
        for (int i = 0; i < remaining_cycle; i++) {
            cout << "Cycle : " << cycle << " " << "| Register value : " << register_value << endl;

            // Cuts out the -1 edge case
            int position = register_value + 1;
            int cycle_row = floor(cycle / 40);
            int cycle_col = cycle % 40;

            cout << cycle_row << " | " << cycle_col << endl;

            if (abs(position - cycle_col) <= 1) {
                screen[cycle_row][cycle_col] = '#';
            }

            cycle++;
            ip.remaining_cycle--;
            if (ip.remaining_cycle == 0) {
                register_value += ip.value;
            }
            if (cycle == marking) {
                signals.push_back(make_pair(cycle, cycle * register_value));
                marking += 40;
            }
        }

        // cout << "cycle : " << cycle << " " << register_value << endl;
    }

    int total_sum = 0;
    for (auto signal : signals) {
        cout << signal.first << " " << signal.second << endl;
        total_sum += signal.second;
    }

    cout << "Part1: " << total_sum << endl;

    print_grid(screen);
}

int main() {
    s();
    return 0;
}
