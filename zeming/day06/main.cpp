#include "../utils.cpp"

using namespace std;

#define PART1 4
#define PART2 14

void s() {
    vector<string> lines = get_lines("in");
    string line = lines[0];

    deque<int> v;
    for (int i = 0; i < line.size() - PART2; i++) {
        char c = line[i];
        int ci = int(c);
        v.push_back(ci);

        if (v.size() == PART2) {
            for (int j = 0; j < v.size(); j++){
                for (int k = 0; k < v.size(); k++) {
                    if (j == k) continue;
                    if (v[j] == v[k]){
                        v.pop_front();
                    }
                }
            }
        }

        // Pad back the zero index
        if (v.size() == PART2) {
            cout << i+1 << endl;
            break;
        }
    }
}

int main() {
    s();
    return 0;
}
