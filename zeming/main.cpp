#include "./utils.cpp"

int main() {
    vector<vector<int>> a;
    for (int i = 0; i < 10; i++) {
        vector<int> v;
        for (int j = 0; j < 20; j++) {
            v.push_back(j);
        }
        a.push_back(v);
    }

    vector<vector<string>> b;
    for (int i = 0; i < 10; i++) {
        vector<string> v;
        for (int j = 0; j < 20; j++) {
            v.push_back("a");
        }
        b.push_back(v);
    }

    print_grid(a);
    print_grid(b);
}


