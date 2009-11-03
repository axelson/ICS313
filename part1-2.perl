#!/usr/bin/perl

change_param(\50);

sub change_param {
    my $scalar_ref = shift;
    print 'sub: $$scalar=' . $$scalar_ref . "\n";
    $$scalar_ref = "bob";
}

# In perl the sub program tries to change the literal but fails:
# Modification of a read-only value attempted at part1-2.perl line 9.
# This makes sense given the overall design philosophy of Perl because
# it should be impossible to change the value of a literal.
