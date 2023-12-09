BEGIN {
    total_score = 0;
}
{
    split($0, parts, ":");
    split(parts[2], nums, "|");
    split(nums[1], hand);
    split(nums[2], winning);
    count = 0;
    delete wset;
    for (i in winning) {
        wset[winning[i]] = 1;
    }
    for (i in hand) {
        if (hand[i] in wset) {
            count++;
        }
    }
    if (count > 0) {
        total_score += 2^(count-1);
    }
    cards[NR-1] = count
}
END {
    # part one
    print(total_score);
    # part two
    n = length(cards);
    sum = 0
    for (i = 0; i < n; i++) {
        val = cards[n - i - 1];
        totals[i] = 1;
        for (j = i - val; j < i; j++) {
            totals[i] += totals[j];
        }
        sum += totals[i];
    }
    print(sum);
}
