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

string get_string_from_vector(vector<string> v) {
    string s;
    for (const auto e : v)
        s += e;
    return s;
}

// Assumes the input is all integers
vector<vector<int>> get_grid(string file_name) {
    string line;
    ifstream file(file_name);
    vector<vector<int>> grid;

    while (getline(file, line)) {
        vector<int> g;
        for (char c : line)
            g.push_back(c - '0');
        grid.push_back(g);
    }
    return grid;
}

template <typename T>
void print_grid(vector<vector<T>> grid) {
    for (auto g : grid) {
        for (auto gg : g) {
            cout << gg;
        }
        cout << endl;
    }
}

vector<string> split_strings(string str) {
    vector<string> v;
    istringstream iss(str);
    for (string s; iss >> s;)
        v.push_back(s);
    return v;
}
