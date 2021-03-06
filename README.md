## ueberfoo

    $ ./ueberfoo.sh test-db new
    created new db test-db.

    $ ./ueberfoo.sh test-db add "artificial intelligence - a modern approach #ai #book #programming #link=http://aima.cs.berkeley.edu/"
    {:id 1, :link "http://aima.cs.berkeley.edu/", :tags #{:ai :programming :book}, :created "2012-08-05 19:14:53", :text "artificial intelligence - a modern approach"}

    $ ./ueberfoo.sh test-db list
    artificial intelligence - a modern approach

    $ ./ueberfoo.sh test-db list --all
    artificial intelligence - a modern approach
    http://aima.cs.berkeley.edu/
    2012-08-05 19:14:53
    1
    #ai #programming #book

    $ ./ueberfoo.sh test-db list --display kv --all
    #text: artificial intelligence - a modern approach
    #link: http://aima.cs.berkeley.edu/
    #created: 2012-08-05 19:14:53
    #id: 1
    #tags: #ai #programming #book

    $ ./ueberfoo.sh test-db list -d kv --select text tags link
    #text: artificial intelligence - a modern approach
    #tags: #ai #programming #book
    #link: http://aima.cs.berkeley.edu/

    $ ./ueberfoo.sh test-db list -d kv -s text tags link --clj
    {:text artificial intelligence - a modern approach, :tags #{:ai :programming :book}, :link http://aima.cs.berkeley.edu/}

    $ ./ueberfoo.sh test-db add "structure and interpretation of computer programs #programming #scheme #book #link=http://mitpress.mit.edu/sicp/"
    {:id 2, :link "http://mitpress.mit.edu/sicp/", :tags #{:scheme :programming :book}, :created "2012-08-05 19:15:04", :text "structure and interpretation of computer programs"}

    $ ./ueberfoo.sh test-db add "the joy of clojure #link=http://joyofclojure.com/ #clojure #programming #book"
    {:id 3, :link "http://joyofclojure.com/", :tags #{:programming :clojure :book}, :created "2012-08-05 19:15:06", :text "the joy of clojure"}

    $ ./ueberfoo.sh test-db add "buy milk #todo #due=1999-07-01"
    {:id 4, :due "1999-07-01", :tags #{:todo}, :created "2012-08-05 19:15:08", :text "buy milk"}

    $ ./ueberfoo.sh test-db list --filter "art"
    artificial intelligence - a modern approach

    $ ./ueberfoo.sh test-db list -f "of"
    the joy of clojure
    structure and interpretation of computer programs

    $ ./ueberfoo.sh test-db list -f "#todo"
    buy milk

    $ ./ueberfoo.sh test-db list -f "#link" --verbose
    0/4 entries.

    $ ./ueberfoo.sh test-db list -f "#link=" -v -s link
    http://joyofclojure.com/
    http://aima.cs.berkeley.edu/
    http://mitpress.mit.edu/sicp/
    3/4 entries.

    $ ./ueberfoo.sh test-db list -f "#link=edu" -v -s link
    http://aima.cs.berkeley.edu/
    http://mitpress.mit.edu/sicp/
    2/4 entries.

    $ ./ueberfoo.sh test-db list -f "#link=edu" "art" -v -s link
    http://aima.cs.berkeley.edu/
    1/4 entries.

    $ ./ueberfoo.sh test-db list -f "#link=edu art" -v -s link
    http://aima.cs.berkeley.edu/
    1/4 entries.

    $ ./ueberfoo.sh test-db list -f "programming" -v
    0/4 entries.

    $ ./ueberfoo.sh test-db list -f "#programming"
    the joy of clojure
    artificial intelligence - a modern approach
    structure and interpretation of computer programs

    $ ./ueberfoo.sh test-db list -f "#programming" --reverse
    structure and interpretation of computer programs
    artificial intelligence - a modern approach
    the joy of clojure

    $ ./ueberfoo.sh test-db list -f "#programming" --reverse --limit 1
    structure and interpretation of computer programs

    $ ./ueberfoo.sh test-db list -r -l 1 --offset 1
    artificial intelligence - a modern approach

    $ ./ueberfoo.sh test-db list -r -l 1
    structure and interpretation of computer programs

    $ ./ueberfoo.sh test-db list -r -l 1 -s text tags --display kv
    #text: structure and interpretation of computer programs
    #tags: #scheme #programming #book

    $ ./ueberfoo.sh test-db list -r -l 1 -s text tags --clj
    {:text structure and interpretation of computer programs, :tags #{:scheme :programming :book}}

    $ ./ueberfoo.sh test-db delete -r -l 1 -s text due --clj
    1 entry deleted:
    {:text structure and interpretation of computer programs}

    $ ./ueberfoo.sh test-db list
    buy milk
    the joy of clojure
    artificial intelligence - a modern approach
