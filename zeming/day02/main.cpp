#include <bits/stdc++.h>

using namespace std;

void s() {
    string line;
    ifstream file("in");

    int score = 0;
    unordered_map<char, int> ll = {
        {'X', 1},
        {'Y', 2},
        {'Z', 3},
    };
    unordered_map<char,unordered_map<char, int>> lll = {
        {'A', {{'X', 3},
               {'Y', 6},
               {'Z', 0}}},
        {'B', {{'X', 0},
               {'Y', 3},
               {'Z', 6}}},
        {'C', {{'X', 6},
               {'Y', 0},
               {'Z', 3}}},
    };

    while (getline(file, line)) {
        char o = line[0];
        char m = line[2];

        score += ll[m];
        score += lll[o][m];
    }

    cout << score << endl;
}

void ss() {
    string line;
    ifstream file("in");

    int score = 0;
    unordered_map<char,unordered_map<char, int>> lll = {
        {'A', {{'X', 3},
               {'Y', 4},
               {'Z', 8}}},
        {'B', {{'X', 1},
               {'Y', 5},
               {'Z', 9}}},
        {'C', {{'X', 2},
               {'Y', 6},
               {'Z', 7}}},
    };

    while (getline(file, line)) {
        char o = line[0];
        char m = line[2];

        score += lll[o][m];
    }

    cout << score << endl;
}

int main() {
    s();
    ss();
}
