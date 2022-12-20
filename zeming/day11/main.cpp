#include "../utils.cpp"

struct Monkey {
	int               test_magnitude;
	int          	  true_target;
	int 			  false_target;
	deque<unsigned long long>  items;
	string      	  operator_string;
	string      	  operation_magnitude;
};

void print_monkey_items(int i, Monkey m) {
	cout << i << " ";
	for (auto item : m.items)
		cout << item << " ";
	cout << endl;
}

void s() {
	auto part1 = false;
	auto part2 = true;
	vector<string> lines = get_lines("in");
	vector<Monkey> monkeys;
	unordered_map<int, int> count;
	// "gcd"
	int mod_mul = 1;

	for (int i = 0; i < lines.size(); i+=6) {
		Monkey m;

		auto starting_items = lines[i+1];
		auto operation = lines[i+2];
		auto test = lines[i+3];
		auto if_true = lines[i+4];
		auto if_false = lines[i+5];

		// Parse items
		starting_items = starting_items.substr(starting_items.find(":") + 2, starting_items.size());
		replace(starting_items.begin(), starting_items.end(), ',', ' ');
		unsigned long long item;
		stringstream iss(starting_items);
		while (iss >> item)
			m.items.push_back(item);

		// Parse operation_string and operation_magnitude
		operation = operation.substr(operation.find("old ") + 4, operation.size());
		string operator_string = string(1, operation[0]);
		string operation_magnitude = operation.substr(2, operation.size());
		m.operator_string = operator_string;
		m.operation_magnitude = operation_magnitude;

		// Parse test
		test = test.substr(test.find("by ") + 3, test.size());
		int test_magnitude = stoi(test);
		m.test_magnitude = test_magnitude;

		mod_mul *= test_magnitude;

		// Parse if true
		if_true = if_true.substr(if_true.find("monkey") + 7, if_true.size());
		int true_target = stoi(if_true);
		m.true_target = true_target;

		// Parse if false
		if_false = if_false.substr(if_false.find("monkey") + 7, if_false.size());
		int false_target = stoi(if_false);
		m.false_target = false_target;

		monkeys.push_back(m);

		// Skip the blank line
		i++;
	}

	int max_iterations = 10000;
	for (int iteration = 0; iteration < max_iterations; iteration++) {
		for (int i = 0; i < monkeys.size(); i++) {
			while (monkeys[i].items.size() != 0) {
				// Inspect the item here
				unsigned long long item = monkeys[i].items.front();
				count[i]++;
				// Check the operator
				unsigned long long worry_level;
				if (monkeys[i].operation_magnitude == "old") {
					if (monkeys[i].operator_string == "*") {
						worry_level = item * item;
					} else if (monkeys[i].operator_string == "+") {
						worry_level = item + item;
					}
				} else {
					int operation_magnitude = stoi(monkeys[i].operation_magnitude);
					if (monkeys[i].operator_string == "*") {
						worry_level = item * operation_magnitude;
					} else if (monkeys[i].operator_string == "+") {
						worry_level = item + operation_magnitude;
					}
				}
				// The monkey decides to get bored here
				if (part1)
					worry_level = floor(worry_level / 3);
				if (part2)
					worry_level %= mod_mul;
				// cout << "monkey : " << i << " worry_level : " << worry_level 
				// 	 << " item : " << item << " test_magnitude : " << monkeys[i].test_magnitude << endl;
				// The true/false branch
				if (worry_level % monkeys[i].test_magnitude == 0) {
					monkeys[monkeys[i].true_target].items.push_back(worry_level);
					// cout << "(true) monkey " << i << " throw to monkey "  << monkeys[i].true_target
					//      << " worry_level : " << worry_level << endl;
				} else {
					monkeys[monkeys[i].false_target].items.push_back(worry_level);
					// cout << "(false) monkey " << i << " throw to monkey "  << monkeys[i].false_target
					//      << " worry_level : " << worry_level << endl;
				}
				monkeys[i].items.pop_front();
			}
		}
	}

	for (int i = 0; i < monkeys.size(); i++)
		print_monkey_items(i, monkeys[i]);

	for (auto [key, value] : count)
		cout << key << " " << value << endl;

	unsigned long long upper_bound = 0;
	unsigned long long lower_bound = 0;
	for (auto [key, value] : count)
		if (value > upper_bound)
			upper_bound = value;
	for (auto [key, value] : count)
		if (value > lower_bound && value < upper_bound)
			lower_bound = value;

	cout << "lower_bound : " << lower_bound << endl;
	cout << "upper_bound : " << upper_bound << endl;
	cout << "part1 : " << lower_bound * upper_bound << endl;
}

int main() {
	s();
	return 0;
}