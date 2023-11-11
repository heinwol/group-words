#![allow(dead_code, unused_variables, unused_imports)]

use itertools::{iproduct, Itertools};
use std::collections::HashSet;
use std::{
    fmt::{self, Debug, Display, Formatter},
    hash::Hash,
    iter::once,
    ops::Mul,
};

/// Splits string `s` at indices, right inclusive, like:
/// (aaab^-12ca^4b^3, [3, 9, 12]) ->
///     ["aaa", "b^-12c", "a^4", "b^3"]
fn split_at_indices(s: &str, idcs: &[usize]) -> Vec<String> {
    let mut res = Vec::<String>::with_capacity(idcs.len());
    let mut left: &str;
    let mut right: &str = s;
    let mut prev_idx = 0;

    for &idx in idcs.iter() {
        (left, right) = right.split_at(idx - prev_idx);
        prev_idx = idx;
        res.push(left.to_string());
    }
    res.push(right.to_string());
    res
}

type WordLetter = (char, i32);

#[derive(Eq)]
pub struct Word {
    elements: Vec<WordLetter>,
}

impl Word {
    pub fn new(elts: &[WordLetter]) -> Self {
        Word {
            elements: Vec::from(elts),
        }
        .free_reduction()
    }

    pub fn from_singleton(chr: char) -> Self {
        Word::from(&chr.to_string()[..])
    }

    // pub fn new(elts: Vec<WordLetter>) -> Self {
    //     Word { elements: elts }.free_reduction()
    // }

    pub fn empty() -> Self {
        Word::new(&[])
    }

    /*---*/

    pub fn len(&self) -> usize {
        self.elements.len()
    }

    pub fn is_empty(&self) -> bool {
        self.elements.len() == 0
    }

    pub fn free_reduction(&self) -> Self {
        let inner_vec = &self.elements;
        let mut res = Vec::with_capacity(inner_vec.capacity());

        for &x in inner_vec.iter() {
            match (res.pop(), x) {
                (None, _) => res.push(x),
                (Some((a, power_a)), (b, power_b)) => {
                    if power_b != 0 {
                        if a == b {
                            let res_power = power_a + power_b;
                            if res_power != 0 {
                                res.push((a, res_power));
                            }
                        } else {
                            res.push((a, power_a));
                            res.push(x);
                        }
                    } else {
                        res.push((a, power_a));
                    }
                }
            }
        }
        Word { elements: res }
    }

    pub fn to_string_with_separator(&self, sep: &str) -> String {
        let s: Vec<_> = self
            .elements
            .iter()
            .map(|(chr, pow)| {
                if *pow != 1 {
                    format!("{chr}^{pow}")
                } else {
                    format!("{chr}")
                }
            })
            .collect();
        s.join(sep)
    }

    pub fn literal_form(&self) -> Vec<WordLetter> {
        self.elements
            .iter()
            .map(|(chr, pow)| (0..pow.abs()).map(|i| (*chr, if *pow >= 0 { 1i32 } else { -1i32 })))
            .flatten()
            .collect()
    }

    pub fn inv(&self) -> Self {
        Self {
            elements: self
                .elements
                .iter()
                .rev()
                .map(|(chr, pow)| (*chr, -*pow))
                .collect(),
        }
    }

    pub fn symbols(&self) -> HashSet<char> {
        self.elements
            .iter()
            .map(|(chr, pow)| *chr)
            .collect::<HashSet<char>>()
    }
}

impl Clone for Word {
    fn clone(&self) -> Self {
        Word {
            elements: self.elements.clone(),
        }
    }
}

impl From<&str> for Word {
    fn from(s: &str) -> Self {
        let mut s_mut: String = s.into();
        // first, remove all whitespaces
        s_mut.retain(|chr| !chr.is_whitespace());
        let int_res: Vec<_> = s_mut.match_indices("^").map(|(i, _)| i - 1).collect();
        let s_split = split_at_indices(&s_mut, &int_res);

        let maped: Vec<_> = s_split
            .iter()
            .map(|s: &String| {
                s.as_str().rfind(|c: char| c.is_ascii_digit()).map_or_else(
                    || s.chars().map(|x| (x, 1i32)).collect::<Vec<WordLetter>>(),
                    |idx| {
                        let (l, r) = s.split_at(idx + 1);
                        let left_char = l.chars().nth(0).unwrap();
                        let left_power: i32 = (&l[2..]).to_string().parse().unwrap();
                        let left_res = (left_char, left_power);
                        let right_res = r.chars().map(|x| (x, 1i32));
                        let res: Vec<_> = once(left_res).chain(right_res).collect();
                        res
                    },
                )
            })
            .collect();
        let flattened = maped.concat();
        let res = Word::new(&flattened);
        res
    }
}

impl Display for Word {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = self.to_string_with_separator("");
        write!(f, "{s}")
    }
}

impl Debug for Word {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // let s = self.to_string_with_separator(" * ");
        let s = self
            .elements
            .iter()
            .map(|(chr, pow)| {
                if *pow != 1 {
                    format!("{chr}^{pow}")
                } else {
                    format!("{chr}")
                }
            })
            .collect::<Vec<_>>()
            .join(" ");
        if self.is_empty() {
            write!(f, "0")
        } else {
            write!(f, "{s}")
        }
    }
}

impl PartialEq for Word {
    fn eq(&self, other: &Self) -> bool {
        self.elements == other.elements
    }
}

impl Hash for Word {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.elements.hash(state)
    }
}

impl Mul for &Word {
    type Output = Word;
    fn mul(self, rhs: Self) -> Self::Output {
        let res_vec = [self.elements.as_slice(), rhs.elements.as_slice()].concat();
        return Word::new(&res_vec);
    }
}

pub fn cyclically_permute_vec<T>(v: &Vec<T>, shift: usize) -> Vec<T>
where
    T: Clone,
{
    let n = v.len();
    assert!(shift < n);
    let mut res = Vec::<T>::with_capacity(n);
    res.extend_from_slice(&v[n - shift..n]);
    res.extend_from_slice(&v[0..n - shift]);
    res
}

pub fn all_cyclic_permutations<T>(v: &Vec<T>) -> impl Iterator<Item = Vec<T>> + '_
where
    T: Clone,
{
    let n = v.len();
    (0..n).map(move |i| cyclically_permute_vec(&v, i))
}

// ------------------------------------------------------------

pub fn common_piece(wa: &Word, wb: &Word) -> Word {
    let mut res = Vec::<WordLetter>::new();
    for (a, b) in wa.literal_form().iter().zip(wb.literal_form().iter()) {
        if a == b {
            res.push(*a);
        } else {
            break;
        }
    }
    Word::new(&res)
}

pub fn inv_set(r: &HashSet<Word>) -> HashSet<Word> {
    r.iter().map(|w| w.inv()).collect::<HashSet<_>>()
}

pub fn all_cyclic_permutations_of_relator_set(r: &HashSet<Word>) -> HashSet<Word> {
    let mut r1 = r.clone();
    for word in r.iter() {
        for perm in all_cyclic_permutations(&word.literal_form()) {
            r1.insert(Word::new(&perm));
        }
    }
    r1
}

pub fn symmetrize(r: &HashSet<Word>) -> HashSet<Word> {
    let mut r1 = r.clone();
    r1 = &r1 | &inv_set(&r);
    let r2 = all_cyclic_permutations_of_relator_set(&r1);
    r2
}

pub fn pieces(r: &HashSet<Word>, r_is_symmetrized: bool) -> HashSet<Word> {
    let mut pcs = r
        .iter()
        .flat_map(|w| w.symbols())
        .collect::<HashSet<char>>()
        .iter()
        .flat_map(|chr| [Word::new(&[(*chr, 1)]), Word::new(&[(*chr, -1)])])
        .collect::<HashSet<Word>>();

    let temp;
    let r_sym = if !r_is_symmetrized {
        temp = symmetrize(r);
        &temp
    } else {
        r
    };
    let r_list: Vec<&Word> = r_sym.iter().collect();
    for i in 0..r_list.len() {
        for j in 0..i {
            pcs.insert(common_piece(r_list[i], r_list[j]));
        }
    }
    pcs.remove(&Word::empty());
    pcs
}

pub fn maximum_word_partitioning(pcs: &HashSet<Word>, w: &Word) -> Vec<Word> {
    let literal_form = w.literal_form();
    if literal_form.len() < 2 {
        return literal_form
            .iter()
            .map(|(chr, pow)| Word::new(&Vec::from([(*chr, *pow)])))
            .collect();
    }
    let mut subpartition = Vec::<Word>::with_capacity(w.elements.capacity());
    let mut j = 0;
    for i in 1..(literal_form.len() + 1) {
        let check = Word::new(&literal_form[j..i]);
        if !pcs.contains(&check) {
            let to_add = Word::new(&literal_form[j..(i - 1)]);
            subpartition.push(to_add);
            j = i - 1;
        }
    }
    subpartition.push(Word::new(&literal_form[j..literal_form.len()]));
    subpartition
    // TODO add assertions
}

pub fn all_c6_single_relator_groups<T>(seq: T) -> HashSet<Word>
where
    T: Iterator<Item = Word>,
{
    let mut res = HashSet::<Word>::new();
    for relator in seq {
        let r = HashSet::<Word>::from([relator]);
        let cycl_perms = all_cyclic_permutations_of_relator_set(&r);
        let r_sym = &cycl_perms | &inv_set(&cycl_perms);
        let pcs = pieces(&r_sym, true);
        let mut lenghts = Vec::<usize>::with_capacity(cycl_perms.len());
        for w_permuted in cycl_perms {
            let part = maximum_word_partitioning(&pcs, &w_permuted);
            lenghts.push(part.len());
        }
        if lenghts.iter().min().map_or(false, |len| *len >= 6) {
            res.insert(r.into_iter().next().unwrap());
        }
    }
    res
}

pub fn multiply_many(seq: &Vec<&Word>) -> Word {
    let res_len = seq.iter().map(|w| w.len()).sum();
    let mut res_vec = Vec::<WordLetter>::with_capacity(res_len);
    for v in seq.iter() {
        res_vec.extend(v.elements.iter())
    }
    Word::new(&res_vec)
}

fn iterate_sth() {
    let vars = vec!['a', 'b'];
    let vars_word = vars
        .iter()
        .map(|chr| Word::from_singleton(*chr))
        .collect::<HashSet<Word>>();

    let letters_with_inverses = (&vars_word | &inv_set(&vars_word))
        .into_iter()
        .collect::<Vec<Word>>();

    let mut combinations = vec![letters_with_inverses; 6];
    combinations[0] = vec![Word::from("a")];
    // combinations[1] = vec![Word::from("b")];
    // println!("{:?}", combinations);
    let multiprod = combinations.iter().multi_cartesian_product();

    let res = all_c6_single_relator_groups(multiprod.map(|w_vec| multiply_many(&w_vec)));
    println!("{:?}", res.len());
}

// ------------------------------------------------------------

fn main() {
    // let w  = Word::from("a a a b^-1 a b b");

    // let w2 = Word::from("a a a b^-1 a b b");
    // let mut set = HashSet::<Word>::new();
    // println!("{:?}", set.insert(w.clone()));
    // println!("{:?}", set.contains(&w2));

    // let w = Word::new(&vec![('a', 1), ('a', 1)]);
    // println!("{:?}", w.elements);

    // println!("{:?}", w);
    // let sym = symmetrize(&(vec![w.clone()].into_iter().collect()));
    // let pcs = pieces(&sym, true);
    // let partit = maximum_word_partitioning(&pcs, &w);
    // println!("{:?}", partit);

    // println!("{:?}", pcs.iter().map(|w| &w.elements).collect::<Vec<_>>());

    iterate_sth();
}
