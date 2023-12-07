function main
    # read in all the games as dynamic variable
    # games_i_j
    set -l i 0
    set -l game_lengths
    while read line
        set i (math $i + 1)
        set rounds (string split -f2 ':' $line | string split '; ')

        set -l j 0
        for r in $rounds
            set j (math $j + 1)
            set -a 'games_'$i'_'$j (string trim $r)
        end
        set -a game_lengths $j
    end

    # ===============================
    # Part one
    # ===============================
    set -l maxr 12
    set -l maxg 13
    set -l maxb 14

    set -l total 0
    set -l gid 0
    for gamelen in $game_lengths
        set gid (math $gid + 1)
        set -l i $gid

        # echo Game $gid =========
        set -l possible 1
        for j in (seq 1 $gamelen)
            # echo "(round $j)"
            set -l round (eval echo \$'games_'$i'_'$j)[1]
            for dice in (string split ',' $round)
                set dice (string trim $dice)
                set amount (string split -f1 " " $dice)
                set color (string split -f2 " " $dice)
                if [ $color = "red" -a $amount -gt $maxr ]
                    set possible 0
                    break
                end
                if [ $color = "green" -a $amount -gt $maxg ]
                    set possible 0
                    break
                end
                if [ $color = "blue" -a $amount -gt $maxb ]
                    set possible 0
                    break
                end
            end
            if [ $possible -eq 0 ]
                break
            end
        end
        if [ $possible -eq 1 ]
            set total (math $total + $gid)
        end
    end
    echo $total

    # ===============================
    # Part two
    # ===============================
    set -l total 0
    set -l gid 0
    for gamelen in $game_lengths
        set gid (math $gid + 1)
        set -l i $gid

        set -l maxr 0
        set -l maxg 0
        set -l maxb 0

        for j in (seq 1 $gamelen)
            set -l round (eval echo \$'games_'$i'_'$j)[1]
            for dice in (string split ',' $round)
                set dice (string trim $dice)
                set amount (string split -f1 " " $dice)
                set color (string split -f2 " " $dice)
                if [ $color = "red" -a $amount -gt $maxr ]
                    set maxr $amount
                end
                if [ $color = "green" -a $amount -gt $maxg ]
                    set maxg $amount
                end
                if [ $color = "blue" -a $amount -gt $maxb ]
                    set maxb $amount
                end
            end
        end
        set total (math $total + $maxr x $maxg x $maxb)
    end
    echo $total
end

main
