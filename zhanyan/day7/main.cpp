/*
 * Advent of Code
 * Day 7
 *
 */

#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <exception>
#include <execution>
#include <fstream>
#include <functional>
#include <iostream>
#include <iterator>
#include <list>
#include <memory>
#include <numeric>
#include <sstream>
#include <stack>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

using namespace std;
using line_iter = istream_iterator<string>;

/** @class FSNode
 *  @brief Inode on a filesystem
 */
class FSNode {
public:
  /** @brief Calculate the size of the FSNode & all its children */
  virtual int size() const = 0;
};

using FSNodes = vector<shared_ptr<FSNode>>;

/** @class File
 *  @brief File on a filesystem
 */
class File : public FSNode {
public:
  /** @brief Construct a file with the given name & size */
  File(string name, int size) : name_(name), size_(size){};

  int size() const { return this->size_; }

protected:
  string name_;
  int size_;
};

/** @class Dir
 *  @brief Directory on a filesystem
 */
class Dir : public FSNode {
public:
  /** @brief Construct a new Directory with name and children residing within.
   *
   * @param name Nameof the directory.
   **/
  Dir(string name) : name_(name), children_(){};

  int size() const {
    return transform_reduce(
        execution::seq, this->children_.begin(), this->children_.end(), 0,
        plus<int>(),
        [](const shared_ptr<FSNode> &node) { return node->size(); });
  }

  void add(shared_ptr<FSNode> child) { this->children_.push_back(child); }

protected:
  string name_;
  FSNodes children_;
};

string to_path(vector<string> components) {
  if (components.size() > 0) {
    return accumulate(
        components.begin(), components.end(), string(),
        [](string acc, const string &next) { return acc + "/" + next; });
  }
  return "/";
}

int main(int argc, char *argv[]) {
  if (argc < 2) {
    cout << "Expected input file to be passed as first argument." << endl;
    return 1;
  }
  // read input from file passed as cmd arg
  unordered_map<string, shared_ptr<Dir>> directories;
  vector<string> current_dir;
  ifstream input{(argv[1])};
  string line;

  while (!input.eof()) {
    getline(input, line);
    // parse input line
    istringstream tokens(line);
    string token;
    tokens >> token;
    if (line.empty()) {
      // skip empty line
      continue;
    } else if (token == "$") {
      // parse new command
      tokens >> token;
      if (token == "cd") {
        // change directory command
        tokens >> token;
        if (token == "/") {
          // cd root: reset current path
          current_dir = vector<string>();
          directories["/"] = make_shared<Dir>("/");
        } else if (token == "..") {
          // cd to parent directory
          current_dir.pop_back();
        } else {
          // assume cd into directory
          current_dir.push_back(token);
          // check that we are aware of such a directory
          string path = to_path(current_dir);
          if (directories.count(path) <= 0) {
            cout << "Fatal: tried to cd into an unknown directory: "
                 << to_path(current_dir) << endl;
          }
        }
      } else if (token == "ls") {
        // continue parsing listing into current path
        continue;
      } else {
        cout << "Error: unsupported command: " << token << endl;
        return 1;
      }
    } else {
      // assume that we are parsing a directory listing
      auto dir = directories.at(to_path(current_dir));
      if (isdigit(token[0])) {
        // parsing a file listing
        int size = stoi(token);
        tokens >> token;
        dir->add(make_shared<File>(token, size));
      } else if (token == "dir") {
        // parsing a directory listing
        tokens >> token;
        auto subdir = make_shared<Dir>(token);
        dir->add(subdir);
        // register subdir in seen directories
        vector subdir_path = current_dir;
        subdir_path.push_back(token);
        directories[to_path(subdir_path)] = subdir;
      } else {
        cout << "Error: unparsable listing : " << token << endl;
        return 1;
      }
    }
  }

  // part 2: smallest directory that leaves 30M of free space
  int already_free = 70000000 - directories.at("/")->size();
  vector<int> dir_sizes;
  transform(directories.begin(), directories.end(), back_inserter(dir_sizes),
            [](auto entry) { return entry.second->size(); });
  vector<int> large_dir_sizes;
  copy_if(dir_sizes.begin(), dir_sizes.end(), back_inserter(large_dir_sizes),
          [already_free](int size) { return size + already_free >= 30000000; });
  cout << *min_element(large_dir_sizes.begin(), large_dir_sizes.end()) << endl;
}
