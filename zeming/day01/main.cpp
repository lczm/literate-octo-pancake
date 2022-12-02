#include <bits/stdc++.h>

using namespace std;

void s() {
    string line;
    ifstream file("in");

    int current = 0;
    int max = 0;

    while (getline(file, line)) {
        if (line == "") {
            max = std::max(max, current);
            current = 0;
            continue;
        }
        current += stoi(line);
    }

    cout << max << endl;
}

void ss() {
    string line;
    ifstream file("in");

    int current = 0;
    vector<int> v;

    while (getline(file, line)) {
        if (line == "") {
            v.push_back(current);
            current = 0;
            continue;
        }
        current += stoi(line);
    }

    sort(v.begin(), v.end(), greater<int>());

    cout << v[0] + v[1] + v[2] << endl;
}

int main() {
    s();
    ss();
}
