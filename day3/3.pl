use strict;
use warnings;

my @lines;
chomp(@lines = <>);

sub add_neighbors {
    my $i = shift;
    my $j = shift;
    my $adjref = shift;
    my @arr = @_;

    my $height = (scalar @arr);
    my $width = (length $arr[0]);

    for my $di (-1 .. 1) {
        for my $dj ( -1 .. 1 ) {
            if ($di == 0 && $dj == 0) {
                next;
            }
            my $ni = $i + $di;
            my $nj = $j + $dj;
            if ($ni < 0 || $nj < 0 || $ni >= $width || $nj >= $height) {
                next;
            }
            my $n = substr($arr[$nj], $ni, 1);
            if ($n !~ /^[.0-9]$/) {
                my $key = "$n $ni $nj";
                $adjref->{$key} = 0;
            }
        }
    }
}

my $height = (scalar @lines);
my $width = (length $lines[0]);
my %d_to_adj;
my %digits;

while (my ($j, $line) = each @lines) {
    while ($line =~ /(\d+)/g) {
        my %adj;
        my $start = (pos $line) - (length $1);
        my $end = (pos $line);

        for my $i ( $start .. ($end-1) ) {
            add_neighbors($i, $j, \%adj, @lines);
        }
        my $key = "$start $j";
        $digits{$key} = $1;
        $d_to_adj{$key} = \%adj;
    }
}

my $total = 0;
foreach my $key (keys %digits) {
    my $digit = $digits{$key};
    my $symbols = $d_to_adj{$key};
    if ((keys %{$symbols}) > 0) {
        $total += $digit;
    }
}
print "$total\n";

# Part two
my %inverted;
foreach my $key (keys %digits) {
    my $digit = $digits{$key};
    my $symbols = $d_to_adj{$key};
    foreach my $sym (keys %{$symbols})  {
        if (exists $inverted{$sym}) {
            push(@{$inverted{$sym}}, $digit);
        } else {
            $inverted{$sym} = [$digit];
        }
    }
}

$total = 0;
for my $sym (keys %inverted) {
    if ($sym !~ /^\*/) {
        next;
    }
    if ((scalar @{$inverted{$sym}}) == 1) {
        next;
    }
    my $ratio = 1;
    for (@{$inverted{$sym}}) {
        $ratio *= $_;
    }
    $total += $ratio;
}
print "$total\n";
