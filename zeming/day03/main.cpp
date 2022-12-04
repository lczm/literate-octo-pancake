#include "../utils.cpp"

using namespace std;

int c_val(char c) {
    // let 'a' be the start from the ascii table
    int v;
    if (c >= 97)
        v = int(c) - 96;
    else
        v = int(c) - 38;
    return v;
}

void s() {
    vector<string> lines = get_lines("in");
    int sum = 0;
    for (auto s : lines) {
        auto length = s.length();

        set<char> first; set<char> second;
        set<char> intersects;

        for (char c : s.substr(0, length / 2))
            first.insert(c);
        for (char c : s.substr(length / 2, length))
            second.insert(c);

        set_intersection(first.begin(), first.end(),
                         second.begin(), second.end(),
                         inserter(intersects, intersects.begin()));

        for (char c : intersects)
            sum += c_val(c);
    }
    cout << sum << endl;
}

void ss() {
    vector<string> lines = get_lines("in");
    int sum = 0;
    for (int i = 0; i < lines.size(); i+=3) {
        set<char> first; set<char> second; set<char> third;
        set<char> fs; set<char> es;

        for (int j = 0; j < lines[i].size(); j++)
            first.insert(lines[i][j]);
        for (int j = 0; j < lines[i+1].size(); j++)
            second.insert(lines[i+1][j]);
        for (int j = 0; j < lines[i+2].size(); j++)
            third.insert(lines[i+2][j]);

        set_intersection(first.begin(), first.end(),
                         second.begin(), second.end(),
                         inserter(fs, fs.begin()));
        set_intersection(fs.begin(),fs.end(),
                         third.begin(),third.end(),
                         inserter(es, es.begin()));

        for (char c : es) {
            sum += c_val(c);
        }
    }
    cout << sum << endl;
}

int main() {
    s();
    ss();
}
