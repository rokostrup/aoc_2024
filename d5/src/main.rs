use std::fs;

type Rule = Vec<u32>;
type Rules = Vec<Rule>;
type Update = Vec<u32>;
type Updates = Vec<Update>;

fn parse_rule(rule: &str) -> Rule {
    let v = rule
        .split_terminator('|')
        .map(|x| x.parse::<u32>().unwrap())
        .collect::<Vec<u32>>();

    v
}

fn parse_update(rule: &str) -> Update {
    let v = rule
        .split_terminator(',')
        .map(|x| x.parse::<u32>().unwrap())
        .collect::<Vec<u32>>();

    v
}

fn get_middle_num(update: &Update) -> u32 {
    // we are guaranteed an uneven length,
    // so we can just divide by two to get
    // the correct rounded number (fx len = 3 -> 1)
    update[update.len() / 2]
}

fn parse_txt() -> (Rules, Updates) {
    let s = fs::read_to_string("input.txt").expect("Should have been able to read the file");

    let rules: Vec<_> = s
        .split_ascii_whitespace()
        .filter(|l| l.contains('|'))
        .map(|l| parse_rule(l))
        .collect();

    let updates: Vec<_> = s
        .split_ascii_whitespace()
        .filter(|l| l.contains(','))
        .map(|l| parse_update(l))
        .collect();

    (rules, updates)
}

// get a vector of numbers that must come before the given page.
// for a given page p, all pages x must come before it => x|p
fn numbers_before_page(page: u32, rules: &Rules) -> Vec<u32> {
    rules
        .iter()
        .filter(|rule| rule[1] == page)
        .map(|rule| rule[0])
        .collect()
}

fn numbers_after_page(page: u32, rules: &Rules) -> Vec<u32> {
    rules
        .iter()
        .filter(|rule| rule[0] == page)
        .map(|rule| rule[1])
        .collect()
}

fn is_update_ok(update: &Update, rules: &Rules) -> bool {
    for (i, entry) in update.iter().enumerate() {
        // we make a slice of the remaning entries
        let pages_before = &update[0..i];
        let pages_after = &update[i..];

        if numbers_after_page(*entry, rules)
            .iter()
            .any(|p| pages_before.contains(p))
        {
            return false;
        }
        if numbers_before_page(*entry, rules)
            .iter()
            .any(|p| pages_after.contains(p))
        {
            return false;
        }
    }

    true
}

fn main() {
    let (rules, updates) = parse_txt();

    let total: u32 = updates
        .iter()
        .filter(|u| is_update_ok(u, &rules))
        .map(|u| get_middle_num(&u))
        .sum();
    println!("{total}");
}
