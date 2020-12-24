(ns advent-2020.day-24
  (:require [clojure.string :as string]
            [common.util :refer :all]))


(defn parse-paths
  [input]
  ;; Split after any "e" or "w"
  (map #(string/split % #"(?<=[ew])") (string/split-lines input)))


(def dir->diff {"e" [1 0] "w" [-1 0] "ne" [1/2 1] "nw" [-1/2 1] "se" [1/2 -1] "sw" [-1/2 -1]})


(defn initial-black-tiles
  [paths]
  (->> (map (fn [path] (reduce (partial map +) [0 0] (map dir->diff path))) paths)
       (frequencies)
       (filter (comp odd? val))
       (map first)
       (set)))


(defn part-1
  [input]
  (count (initial-black-tiles (parse-paths input))))


(defn neighbours
  [tile]
  (map (fn [diffs] (map (partial apply +) tile diffs)) (vals dir->diff)))


(defn will-be-black
  [black-tiles tile]
  (let [black-adjacent-count (count (filter (partial contains? black-tiles) (neighbours tile)))]
    (if (contains? black-tiles tile) (#{1 2} black-adjacent-count) (= 2 black-adjacent-count))))


(defn update-black
  [black-tiles]
  (->> (mapcat neighbours black-tiles)
       (concat black-tiles)
       (distinct)
       (filter (partial will-be-black black-tiles))
       (set)))


(defn part-2
  [input]
  (count (nth (iterate update-black (initial-black-tiles (parse-paths input))) 100)))


(def small-input
  "sesenwnenenewseeswwswswwnenewsewsw\nneeenesenwnwwswnenewnwwsewnenwseswesw\nseswneswswsenwwnwse\nnwnwneseeswswnenewneswwnewseswneseene\nswweswneswnenwsewnwneneseenw\neesenwseswswnenwswnwnwsewwnwsene\nsewnenenenesenwsewnenwwwse\nwenwwweseeeweswwwnwwe\nwsweesenenewnwwnwsenewsenwwsesesenwne\nneeswseenwwswnwswswnw\nnenwswwsewswnenenewsenwsenwnesesenew\nenewnwewneswsewnwswenweswnenwsenwsw\nsweneswneswneneenwnewenewwneswswnese\nswwesenesewenwneswnwwneseswwne\nenesenwswwswneneswsenwnewswseenwsese\nwnwnesenesenenwwnenwsewesewsesesew\nnenewswnwewswnenesenwnesewesw\neneswnwswnwsenenwnwnwwseeswneewsenese\nneswnwewnwnwseenwseesewsenwsweewe\nwseweeenwnesenwwwswnew")


(def large-input
  "neswsesewswswswswswneeswswswswwwnwsw\nnenesenenwneenwnwweneswnenenenwnw\nwewnesesweswsenwnwnwsww\neeneswneewneseneeeseeweeesenwe\nnwnenenenenenewnwnesenwnwnenenenenenwnese\nnwwenwswwnwwwenwnwwnwnwwwnwnwsew\nnwnwnenwnwnwnwnwnwswnwnenwswnwnwnenenwne\nswsesenweseswseseswswnwswswswseesesesw\nnweneneswnenenenenwnenwnwswswnwnwwswsenwne\nwnwnwenwnwnwnwnwsewnwnenwnwwwsw\nsenwewwsewswswnwseswsenwswswsewnenww\nnenwnesenwnenwnwnwnenwenwswnwnewnwswnw\nneneswwsesenwsesewnenenenenenewenwne\nweneneeneneneneeene\newnwsenwwwwnwnewwneswnwsenwwnwsesww\nneenwnwnenenenwwnwnwnenenenenwswnwnenwnwe\nwwnwswwwnwwnwwwwwwnewnw\nneneneneeneneneneneneswnenenenenenenene\nnwwnenwnenwnenenenenesenenwnenenwnwnwne\nnenenwsenwsesesenwwnwneenesewnenwnene\nnenenewneneeneneneneeseneswnenenenwnene\nseseenwewseswsese\nseseeseseseseswsesesesesesesesenesesese\nsewswswnwswswswswwew\nwseswnwnwnwnenewnwnwwnwwnwnwnewnwsw\nneeeenenwneeeeeeswneneneeneee\nswnwnwnwnwnwnenenwnwewnwnwnenenwnwneenwne\nswswsweswseeswseswneswwswseswnwsesww\nswnewwnewwweewwwwnwsewwwww\neneswswwewwwswswesewwswswnwwe\neswnweeeeeneeeeeeesweeee\nwswswnweswwswnewswseswswswswswswswe\nnwewnwwwwnwwwwneswswwwwwnew\nnwnwnwnwnwwnwnwwenwnwnwnwsenwnwnwswnwnw\nswswnewswswswswwswswwwwweseswswswnww\nswneewswswswwswnwnwswwnwwsenwswese\nnenenenwswnenenwnenenwnwnwnenenwnenenwne\nnwnwnwnwwneswswenwenwenwnwnwnww\nneneseswnenwneneneneneenesw\nnweswswnwsewswnwwseeseneswnwneene\nwwwwswwwnwswwwswsewewwswwsw\nwewswwewswswswnweswwswswsweswwsw\nsesesesesesewseswsesesesesesenesenesesw\nwsweeneeeeneeeene\neseeeeeeeeeeeenweseseseee\nwesesewswnesewneeswseewsewnwesesw\nseweeneneeweeeneneeeneneeseew\nwswseswswwenwswwwswwwsw\neweweeewseneswseenweswnweswene\nwwswwwwwwwsewnewswwwwswwww\nswseewseneeneswswnewneesewsenwese\nsewwwwwwwnwwwnwwww\nseseneeseseeenwsesweseeeseswsesesese\nneenewnwneneeeneseneneeneseeneneene\nnwenwwwswnwwwwwnw\nnwseswseseswneseeswneswswneswswsewswse\nswswswwswwswwswnewwswwswwseswswe\nwnwwnwwnwnwsenwnwnwwnww\nswswnwseseseswsesesesewenesewenwsese\neseseesenweeseswseseenwee\nnenwseseneeeeeenweseenweweee\nswsweseswnwswswswswswswseswswswneseswsw\nwnewenesenesweswwnwwseesesesee\nwsesesesewseneseseesesesesesesesesesese\nseeseeseeeseeswsweesesenwneenwnw\nnwwnwnwnwsenwnwwswnwnwsenweswnwnwnwnwnee\nesenenenenenenenenenewnewnenenenenene\neswswswswswwswswswwnwswswswswswwswsw\nnwnenwnwenwesenwsewsenwwsewswnwsenw\nnwnwnwwnwnwnwwnewwwnenwsenwnwwnwwsw\nweeseswwenwnwwewwwwnwseenesw\nwwswswswwswwwnwwwswsewswwsw\newsewweswwwewwwwwwnwenwnw\neenwnwnewneweneswseenewseswwsesw\nneenenenenenenewwneneseneneneeneene\nseswnwswswseswswnwnwseseseeswseswswseswsesw\nseseneswseewsesweseseseseseswnwsesesese\nnwnwnwnwnwwnwnwnwnwnwenwnwwnwnwwnwnw\nseswswsenwwseneseseseswswneseswnesesesesese\neseeseeeeeenweenwesweneenee\nnwneswnwnwnwnwnwswneeseswenwswnwnwnww\nswsesweswseswswnwswswswswswswnweswswsw\nneseseesweseeeseeesewneeewsene\nneneswnewneneswseswneswnenw\nseweeweswwwnenesewwwnwnewsew\nswwweswswswwswswwswswnwswswwswswsw\nwneneenenesenewsenenwneswnewneenenw\nsenwnewnwnwnwnwnwnwwnwswwenwsesenwnwne\nwwwwwwnwswsewwwwwwwsw\nneneweneeeeenenweneeeeeenwsese\nswswsweswswswswswswswswnwseseswswseseswsw\nswswswswseswswnwswswseswswnwswneswsee\nnwewnenwseneneswsenenenewnenwnesenwnenw\neseswnwswswseneswseseswswseeswswswwsw\nseseeeseseseweneeseeeesesesesesese\neewnweswswsweeenenwwnwse\nwwswwwswwsewwswnwenwnwswswwwesw\nwwewwwwwwwswwwnewwwwww\nnwenwwnwnwnwnwneseswnwnwnwnwnwnwnene\nnwwnewnwnwwnwnwnwwnwnwsenwwwnwnwww\nsewswneswseeswseneseswswswsewneseswnw\nnewneswswwwwsweswswenesesewneswsw\nnwnewwwwwnwswwseswwwwnwwseese\nwwwwnewwwwwwenewswwwwwsww\neseeeeeewseneswswseweeneesesee\nnwnwnwswnenwnewnwenwnwnwsenwenenwnwwnwne\nswseswseswsenesewswseseswswswseswsesese\nswswsesesesweswnesesesewsese\neeweeeeseseeesee\nswnwnwnwnenwnwnwnwnwnwnwnwnwnwwnwsenenw\nnwwnwwwnwswwwwwwnwwewwewww\nswnenenenenenenenenenenenenenenenenene\nswswneswswswswswseswswnwswswswswswewsw\nnwneswnwnenwnwnwnenwnenenenenwnwnenwnwenw\nnwnwnwnenwnwnwnenwnwnwnwnenwsenwnwnwnwwnw\nwnwnwwnwswwwsenwnwnwnwnwwnewseeww\nnwenwnwnwenwnwwnwnwwsenwnwswwwnwnwnw\neeweeseseeeseseeseseseseseese\nneeenesewswsweeneenweesenenwswesw\nnwwnwnwnwewwnwnwnwwnwwnwnwnwewnwnw\nwnwneswwwswwwewwwwswwsewwswne\nnwnwsesenenwnenwswseswnenwwnwnenwnwnwnene\nnenenwnwnewnwswnwnwnenwneneneeneenww\nwwnwswnwnwnwnwnwswswsenwnwenwneesee\nnenwnwnwnwnenenwnenwnesenenenenenwnwsenw\nswseswswseswseseseswneseswswswseswswswsw\nswswsweneswnwwsweswswswswswswswnesw\nwnwwwwwwwwswwwewnewwnwwwwse\nnwnenenenenenwnwnwwnwenwnwwsenesesenwnw\nnenwneeeeeswswneeeeeneeeneswnee\nnenwnwnwnwnwnwsenwwnwnwnwwnwnwnwsenesenw\nnwnwwnwwswwwnwwnwwnwsewwnwnewnw\nwnwwwwenwseswnwswewnww\newwnwwwswwnwwnww\nnwnenwnenenenenenenenewnesenene\nsesesesweenwsenwenwsenweesenwenwnw\nneneeeeswenwneeswsenwenenenenenenee\nseeesenwswwesenwsesenwseseswesenesee\nnenwnwnenwswnenwnwsenwnesenwnwnwnwnenenw\neeeeeeeeeeesweenwswne\nesesweswnwsenweeesesesenweeesww\neseseeseseseseseweeseeeseesee\nswnenenenwnwnesenenewnwneeneneneenenesw\nneneneenenenenewneneneseneneneneeewse\nwnwnwnwnwenwnwwewswwwwwnwsewwwe\nneneeneneneneneneneneneswne\nseswswseseeswswnwseseneswswseseseseswsese\nwswnwswweneweenwwswswneswswwseswne\neswwwswswneswwswwnwwwenwwwswsewsw\nwswswswswenwnwnwswswswswswswseswneeswe\nwwwwwswswwwenenwnwnwwnenwsesww\nwseweeenwnewneneneneseeneneenwswse\nsenwwnenwnwnewseswwwsewswnewneene\nnenwnwnweneneenesesweneneneneneneswsw\nswseswswswswwswswswneseswseswsewswswnesesw\nenewnenwnenenenwsenenwnwnwnenenwseswene\nwwnewwwswwwwwwwwwswsewwww\nwsenwnwnewnwnwnwnwnwnwnwnwnwnwnwwnwnw\nwenwnwwwwnwwsenwneswsewwsewnwnw\nneenewneneeeneeee\nneseswnwswswnwnwseseswseewseseneseswseswsw\nnenenewwnenenewneeenenenweswsene\nseeeeesesesweeenweesenwseeseswe\nswesenweseewseeseeswswesenwenenewne\nseseseseseseeeswnwswne\nwwwwwwwwwwnwnwsewnwwwwsewe\nnenwnwneeswnwnwenwswswnwnwnwswnwnwnwnwnwnw\nswswswesewswswseswswswseswswseswswsw\nneneenenenenenenenenenewnenenenenenene\nwneseenenwnwwswseswswwnewnwwwww\nnwnwnwwnwnwweseneswwnwseenwnwnwwnwsw\nnwnwsenwnwnenwnwnwnwsenwnwnwnwsenwnwnewnwnw\nwsewneeneneneneneneeeneswe\nnenwnwnweweswneswne\neseeeeeseeeeeeeeeweewee\nnewnwswnenwneneneneeneweswnenwnee\nwwnwwwwwwnwwnwnwnwsewnwwwww\nwwnwwnwwnwswwnwwwwnwwwnweww\nneseseseseseswsesesesesesesesesesesesesw\nnenwnwnwnwswwnwwnwnwwnwswnwnwnwwnwnwe\nneenweeeeeeeeeeneeswseeeswe\nswswswswwswswnwswswsesweswswnwswwsw\nwnwsenwwnwwnwnwnwnwnwnenwnwnwnwnwnwnw\nseswnwswenesesewneswnwsewsesesenenesw\neneewnweseswweeenee\nneeneswneswsweswwnenweee\neeseesenewneeeneneswenewne\nswnwsewnwnwneweswnenwneneseesenwwsesw\nwseewswwnenwswswseesweseneswnwswese\nseseseseseswsesesewswseseswseseseseswnene\nseseswsesesewnenewseneswnesew\nwswseswnwwwwewwwwwswwwwswww\nnwnwwnwnwnwnwnwnwnwnwwnwnwnwnwnwenwe\nnenwsesenenenenenwnwnwnenwsenenwnwnenwnewne\nnwnenenwnenwnwswnewnwnwenenwnwnwnwnene\nneseswswnwnwnwneswsweneseseswswnwnewesw\nswwsewwwewswswnwwswswwnwsw\nneweeswneeeneeneneenwswneseewnw\nsewswwweseenwneesewwsewswneee\nwwwnwnwswwwwwwnenwnwnwnwnwnwwnw\nwwwswwwwwwwwnewwwwneswwsew\nnwseswswswneswenesewseeswswswswswnenw\nenwswseeeseseswesesesesesenesenwseese\nnenenwneswnwnwneneenwnwnweswnenwnwnwnwnw\nnwswweneeeeeeeeeeneeeswnese\nsweseseseseeeswenwneeeeese\nseswswseeswsesewesweswswsesesesenwnwsesw\neenwnewesesewseneeswseewsweenwe\nseseswnwwwnewwswweswnwwnewwesw\neeeeeeseseseeseweeeeeeseenew\nweseeneswenwewe\nswswsesenwseswseseswseneesewswsesesesese\nseswseseswswswseseswswswneswswswsesesw\nswwswwsewswwswnwwsesweswnwsenwnwnee\neneweeneneneeeeeeee\nsesesesenesewsesese\nnwnwnenwnewnenwnwnwsenenwnenwnwnwnwnwne\nswneswseseswswnwseseseseseswwseswswsese\nswswsewnewswwswswswswnenwwwneswsewsw\nnwswswsweneseseswseneswseseswnweswew\nswseswswswseswswswswseseswseswnesesw\nnenwnenenwenwnwnwnwnenenwwwenwnenwne\nswwnwnwwwwnwewwnwnwnwnwnwwnw\nesenwneswnenwneeswwswenwseneseeenw\neeeswsenwweneenenenesenenewsenenesew\nwwswsenwwwnewwewwswwswwwwne\nwswesweenewswswnwswsweswnwnwswswe\nswswseneswswswswsewswsw\nwnenenenewneenesenwwseeneneswne\nseeseseseeseenweewseseneseseeseese\nseseesesewseseseseseesweseneseesese\nswnwnwneseswswswswwe\nsenwnwnwnwnwnwswnwwnwnenenwwnwnwnwwnww\nwwwsenwewwswswwnwsewwewswsww\nswwswswswneseseseswsewseswswneswswsesw\nseseeeseseseeseseseseseesewesesese\nseseeeseweeneseewseeeeeseee\nnesenwnwnenenenenwnwnwnwnwnwnenenwsenw\nseseseeesesesenweseseeswseseseenese\nswswswswswswswseswswswswswswswnenwswswswsw\nsenwwswwswswwswswwswswswneswsweswsw\nswesenwswseswnwnwseseswseswswsweseesesw\nwwswswwwswwweswwswswwswswwswsw\nswewsweswswewnwwnewewwsewnwswww\nwnewseseswswnwswwnwnwwnwswseswseeew\nswsenwseseswswseneswswseseseswseseseswsw\nswswwwewswswswwwweswwwwesww\nswswswneeswnwswswswswswweswswswswswnwsw\nenweneeseeneneswswsewneneneeeene\nwswwswnwwwewwnwswesewswseenwe\nsesesesweeeseeewsenwseseeeseeee\nswswswswnwswswswswswswswsweswswseswswswsw\nseseewseseeseesesesesesesesesenwsese\neweeseweeneeeeneeeeeeeeese\neeeeeeeeeeeeeeenweeeesw\nnwswswswwwwwwwswswweswwwswswsw\nweswswnwswweswnwnwneswsenesewwenesw\nenweeseeseeneseseseewseeseeesesee\nnwnwnwnwnenwwnwnwnwnwsesenwnwwwnwnwe\nseseseeneseeseseeseeseeseeeweese\nsesesweeswseswsenweswseswnwsenwswswsw\nsesweseeeeeeenwseswnewse\nswwnwwnewwwwwwnwwwwwnwnwww\nswswswneswswswswswswwswwwswwswwesw\nwnewneneweneeneseswsenewnesweneene\neseeeseeeseenesw\nsesenwseneseseseseswswswsesesewsesesesesw\nnwneneneneswnenwnwnenenwneenenwnwnenwnene\nneneenewnenenenenenenesweeeeeesee\nnenwwswnwenwnwnwsesenenww\neenweswsweeeeneeneeweneene\nnenewwneseenwnenwnwsenwnenwwnw\nwwwnenewwsenesenwnwsewewwswswswnew\nseswwswneswswswwnwswwswwswswswwswwsw\newneneeeeenweesweswenweneneene\nsweswsenwswswwswnewnenewswenweenw\nsweweseesesesesewsenwseeweenesenwse\nswnwnwwwwwwwne\nnwnwswswseswswswswwswswswswswswse\nseswseseswswseseseeseswnwswsenweswseneswse\neeswnweseeneeeeseewseseseseesesee\nswswswswswswseswwswsweswswneswswwnenw\nwnwenwwwwnwwwwwnwwnwwww\nswwswwswswswswswwswwnwswswswswswswse\nnwwwwwswnwwnwwnwwwwnwnwneww\neeneswenwnwswnwsweswswnwswnenenwnwse\nswswseswswseswswswswneswswseneswseswswse\nneeeeeeeeeeneneweweeeene\nnwnwneenwnwnwnwswneswnenesenwswewnww\nweswwswswnesewsewswswwneneseeese\nwswswswseswswnesesesw\nenwneswwnenwnewnenwnweenwnenwneswnesene\neswwswswswswswwswswswwswswswnweswswsw\neswsenweswswnwswswswswsewswwwnwswswsw\nnenwnenenenenwnenenewneenenenenwnenene\nwnesesenewwwsewwwnwnwwnenwwswnewsw\nwwswswwswwswswwswswwnwswseswswswsw\nneneneneneswnenwnenenenwnwswnenwswnenee\nsesesesenesesenesewsesenewseswsesesese\nwneewseswwwnewswswswswenwnewwww\nswwnwswswswswnwswswswwsesweswswwesw\nseseswseswseseswseswnweseseseseswseswse\nswswseseswswswswsesweswnwseeseswnwsese\nneswswnwnwsewnwenwwnwwweenwsewnw\newneeneneenesweneeeneeenenenene\nnwnwwnenwnwwnwnwnwsewnw\neeneneeseenweseswneesweneenenenw\nneneneneeneneeneswnwnwneswnenenenenenenwne\nnwneneswsewswnwsewwnenwewnw\nnenwsenenwnenenenwnesenenenenenenenenwse\nseeeesesenwseeesesewsewseeseese\nnenenenenenenesesewnenwnenenenwnwnwnenw\nneeneneeewneeneneneneeneenenenee\nwswnwswseewnwnweneewwneww\nseseenwseweswnwneenenenenwnwseeneswse\nseswsenwswseeseseswswswseswswseswsesenwsw\nneseeseswseswsesewswswnwswwseswnenwsesw\nnwnenwswnenenwnwnwnwnwsenwnw\nnwnwwswnwwenwnwwnewnwnwnwwnwnwsenwnw\nseseeswseenwnwseneseeeesesewswsesese\nneneswnwenwnenenenenenenwnenwnwnwnwnwnesw\nswswswseeswneswswswswswsweswwneswswswnw\nswseswswswswswswneeswwswwswswswseswsesw\nsesesesewnwseseseseseseseeseseseseswsese\nneenenwwenwswnwneswsweenwnw\nwwnwwwnwwnwwnwnwnwseenwwnwnwnwnwnw\nswneswsesweswswswswswswswswwwswwsw\nesweeneeneeeneenesweeee\neeeeeenweesweeeeeeweew\nseseswswewseswwseeswenwnwswseswswse\nnenesesesweswnenwewseewnwseswewne\neseweseseseseeesesesesesesesesesee\nsesenwswseeswswsesesesenwseseseeneseese\nswnwswswnwnwsenwneenwwnwnenwwneenwe\nnwwwwnwnwnwnwnwwnwnwnwswewwwnwwnw\nseseswsewesesewseseseseseseeseswsesese\nsenwswwnwnenweesewswenenwneneswswwne\neseweneswsesewwnwwseseswswnesenesew\nswwswswswswseswswswswsweswswsw\neesenweweseseeeeeeeseeseseese\nseeeeneenewweeene\nnenwswswseeewsewwnwwnwwwswwsenw\nswswswswwwnewwswnwwnewwewewsww\nsewwswswneeswwnwswwwswnewswwswswsw\nswwewwnwswswswnwnwweswneeeeww\nswswwswswseeseswneeweswswnwnwswene\nnewwnwseesewsesenwsewseewesenwene\nwswwwnwwsenwwnewwnwsewwnwwne\nswswseseswnweswnwneeswneswsesesenwwswnesw\nnwnwnwnwwnwnwnwsenwnwwnwsenwnwnwwnenwnw\nnwswwenwswsweswwswswwseswswswnwsesw\nnwewwwnwwsenwwwnwesw\nwnwnwnwnwnwwnwsenwnwneswnwenwwsenwnw\nswnenwnwwnwnenwnenenwnenwnwenwnwenwnwsw\nswswnwswsweswwseswnwwswswnwsenwswew\neeeeneeswseeeeeeeeeenenwene\nneeeneneneeeneeeeneswneneenenee\nnwnwnwwnwnwneenwnwnwnwsenenwnwnwnwnwnw\neeeseeeswseenwenenwseeseeeesw\nneeneneeesenweeeene\nneneenenenesenenenenenenenenenenenenwne\nsenwwswswwswswsewneseeseswneswswswswene\nneeeeneeeseseeeeseeseeseesesww\neeeneneenewneneseneneneeeeneee\nswnwwnwsenenwnenwnwsenwnwnenewnwenwne\nneneneneneenenenenwnenenwwsenenenenenenw\nwwnewwwseswwnewww\nnwenesenwnwnwnwnew\nswseseswseswseswswswenwswswseswseneswse\nsenwswsweseneeeeneeneeswnesewswwnw\neenenwswswnwswewwneenwwwwsewsenw\nseeeenenwneneeneeeneeswe\nnewenweeneswneneswne\nswswswseswswsenewwnwwwnwnesweswwww\nnwneneeeneswneneneneeneeneeneeene\neeeeeeesweeeeewnweseeenw\nswseswswswswswsewneswnwswswsesesesesesese\nnwnwsenenenwswnwnenenwwsenenwnenewseswse\nwsewswwswwwswwnewseenenenewwwse\nnwnewnenenenenenwnenenenenenenwnenwsenw\nwnwwwsenewwnwwwwwwwwwwww\nnenwnenewseeseeswswenwwsesewsw\nenwnenesweneneneneswenenwswneneeenenene\nseeeeeenwnweeseeseeswseseeeee\nwwnwwnwswwwwnwwnenesww\nseswswswneseseseseswseseseswsesesesewswse\nwnenwseseeseswswwseseswswswswseswswseswsw\nwnwnwnwneenesenwnwnwnwnwnwnw\nnenenenwneeneneneneneneswnenwnenenenenwne\nsewwswswswwswswwswwswnwseswwswneswsw\neeeneeeswsenwwseseeeeeeeseese\nswswswswswswneswswswswswwswswswsw\nnewswwswwnenwsewwwwwwwwwswsew\nsesesesenwseseseseseseeseseswsenwsesese\nswswsesweswwseswwswnwswswswswswwnwnw\nnenenwneneswnenwsenewnesesewnwnewnee\nsesesesesewneseseseneseseseseseseswsesee\nnenenenenwwnesesenenenesesenewneenwene\nwnwwwwsewnwswneenwwenwswwwwww\nswseswswnwswswseswswswnwswswseseeswswswswsw\nwnwwnwwnwwwnewwwnwwswwwwwsewe\nneeeeneneenenenesewneneeneeenene\nseneenweeneeeeeneenweeeeseee\nenewnenwsenenwnenenenenesenwnenenwnenenw\nneeneneswneneweenwneneeneeenenee\nwwwswwswsweswneswswwswsewswswww\nswwwswswseswwwswswwsenenwswwwwnw\nneenwneneneeseswswewnwswnenwnenenwnenwne\nseeseewwweeseweseeseeesesenwe\nswneneneneneneneneneneneeneneneneenene\nwwswseesweswneenenwnwseesewswnwene\nseseseswseswswseswseneseseswswseswsesesew\nneeneneseenewenenwswnweeneeswneswne\nseswwneswsewnwnewsenwswswenwww\nwwsewwwwwwwnewswwewwwwwww\nnenenenwnenenenenenwnesenenenenenenenene\neseswswswwnwswneswneseeswswswswswswswww\nneeesweenewenenweneneewnenenenene\nnwswwwnwwewswwswwwswseswwwwswswsw\nwwenwnwewswwwnenwwwnewnwswww\nnwneenenwswwwsewewwsesewwsewswnw\nseswseneswseeenenwseeseswneswsesesesenw\neeseswweseeseseesesesesenw\nneseseseewwwneseseenwseseeseseeee\nseswnwsweseseeswseseswnwnwsesesenweseswe\nneseseswswswswsesesw\nnenenenewnenwnwnesenenenenwnenwwneneenene\nsweseseeswsesenweseesesewsesenwenesee\nsenenenenwwnwwnesenenesenenwnwnwnewnwe\nswwnwseneswseseseneseseneseswnwnesewwswse\nwnwnwnewnwnwwnwnwnwwsenwswwnwnenwwww\nwnewwwnwwwwwsewwwwwwwnwwnw\neeesenwenesenweenwesweneeeeee\nwsesenewnenenenenw\neeeeeseneeweneeeeeeeeee\neneseneweneneeeeswneeneeneneww\nnwnwnwnwnwnwnwenwnwnwwewnwnw\nseseseswswseseseswsesesewseseseseseswne\nswwwsweswswswswwswswswswswneww\nseeenweseweseesesweeeeeenwe\nnweneneeneeeeeeeeneseeeneene\nwswnewwwwwwwwswswwwswewwww\nswswswseswswswwseseswneswseswseseswswsesw\nswswswswseswswswsenwnweswswswseswsewesw\nnwswnwwnwswnwnwnenwnwenenwsenwne\nnwwnwesenwnwnwnenwwwnwnwewnwnwnwnwswnw\nwswswswnwwwwwneswwswwsewwswwsw\nnenewneneneeneewnenenenenenenwnenenene\nnwseseswswseseseeeseeseneseseseseese\neswseeseseeewnwseeseseeeseseesese\nnenwnwnwneneeneneneneneneneneneneneswnw\nnwnenwneneneneenwnenenenenenenwnewnene\nnwneneenenenwseeseneeneeenenenewnene\nwneseswsweswswsenwswswswsweeswnwswnw\nswneeswsewswswenweneswnwnesewew\nnwnwnwnwnwnenwnwnwnwsenwsenwwnwnwnwnwnwne\nneswneswnewneswseseswnwnwswnewseswsee\neenenwseweeneeneneeweneweeene\nnenwwwnwnwneenenenwswnwnwswseenwswnwenw\nsweswnwswsweeswwswnenw\nwsewsewsenwneswnwnwwewswnwweenw\nnenwnenwswnwnenwnwnenenwnenwnenwnenenene\nswwneswwewwswneswwwswswwswwwse\nswsewswwsewwswneswswwseneswswswnesw\nseenenwsweesenwseswnweseeeeenwe\nswwswwswswswweswnwsweswswswswnenwwwsw\nwnwnwwswenwweswenwwwwwnwnwswnw\nswswwswwwswwwwsewwwwwswswwnesw\nneswswswswswswswwswswswswwswswswwwe\nwenwenwwseswwwnewnwseseswwneswneww\nnwswswswneswswwwsewwswswswswswsesww")
