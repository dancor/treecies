import math
import postgresql
import sys

taxons = [
    "kingdom",
    "phylum",
    "class",
    "order",
    "family",
    "genus",
    "species",
    ]

max_depth = 5

usage_str = "usage: py catalogue_of_life.py [species-count-cutoff]\n" + \
    "A cutoff of 10k gets about 100 taxons; cutoff of 500 gets about 1k; etc."

if len(sys.argv) != 2:
    raise Exception(usage_str)
min_count = int(sys.argv[1])
if not min_count:
    raise Exception(usage_str)

db = postgresql.open("pq://localhost/catalogue_of_life")

get_rows = [
    db.prepare("SELECT * FROM (\
SELECT COUNT(1), n.name_element FROM col.__taxon_cache AS t \
INNER JOIN col.scientific_name_element AS n " +
"ON (t.t_kingdom_scientific_name_element_id = n.id) " +
"WHERE t.t_infraspecies IS NULL AND t.t_species IS NOT NULL \
GROUP BY n.name_element \
ORDER BY -COUNT(1)) AS foo WHERE count > $1")]

for i in range(0, max_depth):
    get_rows.append(db.prepare("SELECT * FROM (SELECT COUNT(1), n.name_element FROM col.__taxon_cache AS t INNER JOIN col.scientific_name_element AS n ON (t.t_" + taxons[i + 1] + "_scientific_name_element_id = n.id) WHERE t.t_infraspecies IS NULL AND t.t_species IS NOT NULL AND t.t_" + taxons[i] + " = $1 GROUP BY n.name_element ORDER BY -COUNT(1)) AS foo WHERE count >= $2"))

def pretty_count(x):
    exp_val = int(math.log10(x))
    coeff = x / 10 ** exp_val
   
    sig_dig = 1
    more_sig_dig = sig_dig - 1
    
    sig_exp_val = exp_val - more_sig_dig
    sig_coeff = round(coeff * 10 ** more_sig_dig)
    
    if sig_coeff == 10:
        sig_exp_val += 1
        sig_coeff = 1
    
    res = str(sig_coeff)
    if sig_exp_val >= 6:
        res += '0' * (sig_exp_val - 6) + 'M'
    elif sig_exp_val >= 3:
        res += '0' * (sig_exp_val - 3) + 'k'
    else:
        res += '0' * sig_exp_val
    return res

def subtree(depth, parent):
    for (count, name) in get_rows[depth](parent, min_count):
        if name == "not assigned":
            continue
        print("  " * (depth - 1) + "- " + pretty_count(count) + " " + name)
        sys.stdout.flush()
        if depth < max_depth:
            subtree(depth + 1, name)

for (count, name) in get_rows[0](min_count):
    print(pretty_count(count) + " " + name)
    sys.stdout.flush()
    subtree(1, name)
