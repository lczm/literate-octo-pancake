#include "../utils.cpp"

using namespace std;

void s() {
    vector<string> lines = get_lines("in");
    int pairs = 0;
    int pairs2 = 0;
    for (auto s : lines) {
        auto index = s.find(",");
        string first = s.substr(0, index);
        string second = s.substr(index+1, s.length());

        auto f_index = first.find("-");
        string ff = first.substr(0, f_index);
        string fe = first.substr(f_index+1, first.length());
        int ffi = stoi(ff);
        int fei = stoi(fe);
        int min_f = min(ffi, fei);
        int max_f = max(ffi, fei);

        auto s_index = second.find("-");
        string sf = second.substr(0, s_index);
        string se = second.substr(s_index+1, second.length());
        int sfi = stoi(sf);
        int sei = stoi(se);
        int min_s = min(sfi, sei);
        int max_s = max(sfi, sei);

        int min_lower_bound = min(min_f, min_s);
        int max_lower_bound = max(min_f, min_s);
        int min_upper_bound = min(max_f, max_s);
        int max_upper_bound = max(max_f, max_s);

        // Try first contain second
        if (max_f >= max_s && min_f <= min_s)
            pairs++;
        // Try second contain first
        else if (max_s >= max_f && min_s <= min_f)
            pairs++;

        if (max_lower_bound >= min_lower_bound && max_lower_bound <= min_upper_bound) {
            pairs2++;
        }

    }

    cout << pairs << endl;
    cout << pairs2 << endl;
}

int main() {
    s();
    return 0;
}
