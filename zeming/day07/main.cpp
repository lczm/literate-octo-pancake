#include "../utils.cpp"

typedef struct {
    long long size;
    string name;
} File;

typedef struct Directory Directory;
struct Directory {
    string name;
    vector<File*> files;
    vector<Directory*> directories;
    Directory* parent;
    long long size = 0;
};

const long long total_size = 70000000;
const long long minimum_unused = 30000000;

vector<string> split(string str) {
    vector<string> v;
    istringstream iss(str);
    for (string s; iss >> s;)
        v.push_back(s);
    return v;
}

void traverse(Directory* root, int& index, vector<string>& lines,
              long long& sum_lt_100000, long long& total_sum) {
    while (index < lines.size()) {
        vector<string> line = split(lines[index]);
        if (line[0] == "$") {
            if (line[1] == "cd") { // cd
                string destination = line[2];
                // Move back to the parent
                if (destination == "..") {
                    index++;
                    root->parent->size += root->size;
                    if (root->size <= 100000)
                        sum_lt_100000 += root->size;
                    traverse(root->parent, index, lines, sum_lt_100000, total_sum);
                } else {
                    for (Directory* dir : root->directories) {
                        if (dir->name == destination) {
                            index++;
                            traverse(dir, index, lines, sum_lt_100000, total_sum);
                            break;
                        }
                    }
                }
            } else if (line[1] == "ls") { // ls
                while (index != lines.size() - 1 && split(lines[index+1])[0] != "$") {
                    vector<string> line = split(lines[index+1]);
                    if (line[0] == "dir") {
                        Directory* sub_directory = new Directory;
                        sub_directory->name = line[1];
                        sub_directory->parent = root;
                        root->directories.push_back(sub_directory);
                    } else {
                        File* sub_file = new File;
                        sub_file->size = stoll(line[0]);
                        sub_file->name = line[1];
                        root->files.push_back(sub_file);
                        root->size += sub_file->size;
                        total_sum += sub_file->size;
                    }
                    index++;
                }
            }
        }
        index++;
    }

    // Cd all the way back up
    while (index == lines.size() && root->name != "/") {
        lines.push_back("$ cd ..");
        traverse(root, index, lines, sum_lt_100000, total_sum);
    }
}

void traverse_for_min(Directory* root, const long long minimum_delete, long long& current) {
    if (root->size >= minimum_delete)
        if (root->size < current)
            current = root->size;
    for (Directory* directory : root->directories) {
        traverse_for_min(directory, minimum_delete, current);
    }
}

void s() {
    vector<string> lines = get_lines("in");

    // Guaranteed to be a root node
    int index = 1;
    long long sum_lt_100000 = 0;
    long long total_sum = 0;
    unordered_map<string, long long> m;

    Directory* root = new Directory;
    root->name = "/";
    root->parent = nullptr;
    traverse(root, index, lines, sum_lt_100000, total_sum);

    cout << "part1: " << sum_lt_100000 << endl;

    long long current_unused = total_size - total_sum;
    // always the case
    if (current_unused < minimum_unused) {
        long long minimum_delete = minimum_unused - current_unused;
        long long current = LONG_MAX;
        traverse_for_min(root, minimum_delete, current);
        cout << "part2: " << current << endl;
    }
}

int main() {
    s();
    return 0;
}
