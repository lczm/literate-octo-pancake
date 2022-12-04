#include <bits/stdc++.h>

using namespace std;

vector<string> get_lines(string file_name) {
    string line;
    ifstream file(file_name);
    vector<string> lines;

    while (getline(file, line)) {
        lines.push_back(line);
    }

    return lines;
}
