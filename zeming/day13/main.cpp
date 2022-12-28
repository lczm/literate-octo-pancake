#include "../utils.cpp"

enum class ItemType {
    Int,
    Vec,
};

struct Item;
using Packet = vector<Item>;
struct Item {
    ItemType type;

    int integer;
    Packet packet;

    static Item from(int integer) {
        return {.type = ItemType::Int, .integer = integer};
    }

    static Item from(Packet packet) {
        return {.type = ItemType::Vec, .packet = packet};
    }

    Packet to_packet() const {
        Packet p;
        p.push_back(*this);
        return p;
    }

    friend bool operator==(const Item& left, const Item& right) {
        if (left.type != right.type)
            return false;

        switch (left.type) {
            case ItemType::Int:
                return left.integer == right.integer;
            case ItemType::Vec:
                return left.packet == right.packet;
        }
    }
};

Packet parse_packet(istream& is);
Item parse_item(istream& is) {
    if (is.peek() == '[')
        return Item::from(parse_packet(is));
    else if (int num; is >> num)
        return Item::from(num);
    return {};
}

Packet parse_packet(istream& is) {
    Packet p;
    if (is.get() != '[')
        return p;

    while (is && is.peek() != ']') {
        p.push_back(parse_item(is));
        if (is.peek() == ',')
            is.get();
    }

    if (is.get() != ']')
        return p;
    if (is.peek() == '\n')
        is.get();

    return p;
}

vector<pair<Packet, Packet>> parse(istream&& is) {
    vector<pair<Packet, Packet>> packet_pairs;
    for (string line; is;) {
        auto left = parse_packet(is);
        auto right = parse_packet(is);
        packet_pairs.push_back({left, right});
        getline(is, line);
    }
    return packet_pairs;
}

int compare(const Item& left, const Item& right);
int compare(const Packet& left, const Packet& right) {
    int i;
    for (i = 0; i < left.size() && i < right.size(); i++) {
        if (int d = compare(left[i], right[i]); d < 0)
            return -1;
        else if (d > 0)
            return 1;
    }
    if (i == left.size() && i < right.size())
        return -1;
    if (i == right.size() && i < left.size())
        return 1;
    return 0;
}

int compare(const Item& left, const Item& right) {
    if (left.type == right.type && right.type == ItemType::Int) {
        return left.integer - right.integer;
    } else if (left.type == right.type && left.type == ItemType::Vec) {
        return compare(left.packet, right.packet);
    } else if (left.type == ItemType::Int) {
        return compare(left.to_packet(), right.packet);
    } else {
        return compare(left.packet, right.to_packet());
    }
}

vector<Packet> combine_packets(const vector<pair<Packet, Packet>>& pairs) {
    vector<Packet> packets;
    for (const auto& [left, right] : pairs) {
        packets.push_back(left);
        packets.push_back(right);
    }
    return packets;
}

void s() {
    auto packet_pairs = parse(ifstream("in"));

    int sum = 0;
    for (int i = 0; i < packet_pairs.size(); i++) {
        auto& [left, right] = packet_pairs[i];
        if (compare(left, right) < 0)
            sum += i + 1;
    }

    cout << sum << endl;

    auto all = combine_packets(packet_pairs);
    auto div1 = Item::from(Item::from(2).to_packet()).to_packet();
    auto div2 = Item::from(Item::from(6).to_packet()).to_packet();

    all.push_back(div1);
    all.push_back(div2);

    sort(all.begin(), all.end(),
         [](auto& l, auto& r) { return compare(l, r) < 0; });

    auto i = find(all.begin(), all.end(), div1) - all.begin() + 1;
    auto j = find(all.begin(), all.end(), div2) - all.begin() + 1;

    cout << i * j << endl;
}

int main() {
    s();
    return 0;
}