/*
 * Advent of Code
 * Day 14
 */

#include <algorithm>
#include <asm-generic/errno.h>
#include <fstream>
#include <iostream>
#include <iterator>
#include <limits>
#include <string>
#include <vector>

using namespace std;

struct Point {
  int x;
  int y;

  Point(int x, int y) : x(x), y(y) {}

  // Parses a point from the format X,Y
  Point(string text) {
    auto pivot = text.find(',');
    this->x = stoi(text.substr(0, pivot));
    this->y = stoi(text.substr(pivot + 1));
  }

  friend bool operator==(const Point &l, const Point &r) {
    return l.x == r.x && l.y == r.y;
  }
  friend bool operator!=(const Point &l, const Point &r) { return !(l == r); }
};

// Parse points from a line in the format X,Y -> X1,Y1 -> ....
vector<Point> parse(string line) {
  vector<Point> points;
  int end = 0;

  while (end != string::npos) {
    end = line.find(" -> ", end);
    points.emplace_back(Point(line.substr(0, end)));
    // 4 - length of ' -> '
    line = line.substr(end + 4);
  }

  return points;
}

// Simulate one step of sand given obstacle map & floor
// Returns new position of sand
Point simuate_sand(const Point &sand, const vector<vector<bool>> &obstacle,
                   int floor) {
  // sand hit the floor and is stuck
  if (sand.y + 1 >= floor) {
    return sand;
  }
  // attempt to drop down
  if (!obstacle[sand.x][sand.y + 1]) {
    return Point(sand.x, sand.y + 1);
  }
  // attempt to drop diagonal left
  if (!obstacle[sand.x - 1][sand.y + 1]) {
    return Point((sand.x - 1), sand.y + 1);
  }
  // attempt to drop diagonal right
  if (!obstacle[sand.x + 1][sand.y + 1]) {
    return Point((sand.x + 1), sand.y + 1);
  }
  // blocked: stay put
  return sand;
}

int main(int argc, char *argv[]) {
  if (argc < 2) {
    cout << "Expected input path to passed as command line argument." << endl;
    return 1;
  }

  // parse input into a set of points
  ifstream input(argv[1]);
  string line;
  vector<vector<Point>> lines;
  // record max dimensions of the cave
  int min_x = numeric_limits<int>::max();
  int max_x = numeric_limits<int>::min();
  int max_y = numeric_limits<int>::min();

  while (getline(input, line)) {
    auto points = parse(line);
    for (auto pt : points) {
      min_x = min(min_x, pt.x);
      max_x = max(max_x, pt.x);
      max_y = max(max_y, pt.y);
    }
    lines.emplace_back(parse(line));
  }

  // adjust max_x, max_y for floor
  // floor is +2 of max y of point
  max_y += 2;
  // assuming that sand moves diagonal right every drop start_x + max_y
  max_x = 500 + max_y + 1;

  // obstacle map tracking whether a x * y cell is blocked, either by rock or
  vector<vector<bool>> obstacle(max_x + 1, vector<bool>(max_y + 1, false));
  // fill obstacle map with rock lines
  for (vector<Point> line : lines) {
    for (int i = 0; i + 1 < line.size(); i++) {
      // draw a line of blockage in obstacle map
      Point begin = line[i];
      Point end = line[i + 1];
      if (begin.x == end.x) {
        // vertical line
        for (int y = min(begin.y, end.y); y <= max(begin.y, end.y); y++) {
          obstacle[begin.x][y] = true;
        }
      } else if (begin.y == end.y) {
        // horizontal line
        for (int x = min(begin.x, end.x); x <= max(begin.x, end.x); x++) {
          obstacle[x][begin.y] = true;
        }
      } else {
        cout << "Unsupported diagonal line." << endl;
        return 1;
      }
    }
  }

  // drop grains of sand until they exceeding the bounds of the rock lines
  int nSand = 0;
  while (true) {
    nSand++;
    // simulate a grain of sand until it gets stuck
    Point sand(-1, -1);
    Point sand_next(500, 0);
    while (sand != sand_next) {
      sand = sand_next;
      sand_next = simuate_sand(sand, obstacle, max_y);

      if (sand_next.x == 500 && sand_next.y == 0) {
        cout << "No. of sand grains get stuck: " << nSand << endl;
        return 0;
      }
    }
    sand = sand_next;

    // stuck sand becomes an new obstacle
    obstacle[sand.x][sand.y] = true;
  }
}
